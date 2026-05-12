;;; beads-agent-ralph-stream.el --- Single-process stream for Ralph loop -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module owns ONE `claude --print --output-format stream-json'
;; subprocess and the NDJSON parse state needed to render its output.
;; It is the inner layer of the Ralph Wiggum integration: it knows
;; nothing about iteration loops, cost budgets, epics, or the
;; controller state machine.  Those concerns live in
;; `beads-agent-ralph.el'.
;;
;; The public seam is `beads-agent-ralph--stream-spawn'.  Despite the
;; double-dash prefix (house style for internals), this is the function
;; tests rebind via `cl-letf' and the function the controller calls.
;; Its contract is documented in its docstring and kept stable.
;;
;; This file implements only the basic NDJSON line buffering scaffold.
;; UTF-8 boundary handling, reentrancy, drain-on-sentinel races,
;; `--include-partial-messages' reassembly, and deferred dispatch are
;; layered on top in the parser-hardening task (bde-2qha).

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'json)

;;; Customization

(defgroup beads-agent-ralph nil
  "Ralph Wiggum iteration loop for beads-agent."
  :group 'beads
  :prefix "beads-agent-ralph-")

(defcustom beads-agent-ralph-stop-grace-ms 2000
  "Grace period (ms) before SIGKILL after SIGINT.
Used by `beads-agent-ralph--stream-stop'.  If the `claude' subprocess
has not exited this many milliseconds after `interrupt-process', the
stream escalates to `kill-process'.  Tune up for slow-to-quit prompts;
tune down for snappier stop-then-resume cycles."
  :type 'integer
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-stderr-tail-max 50
  "Maximum number of lines retained in a stream's `stderr-tail' ring.
Older lines are dropped once this bound is exceeded.  The dashboard
shows the tail in failure banners; 50 lines is plenty for diagnosis
without unbounded memory growth."
  :type 'integer
  :group 'beads-agent-ralph)

;;; Stream Class

(defclass beads-agent-ralph--stream ()
  ((process
    :initarg :process
    :initform nil
    :documentation "The `claude' subprocess for this stream, or nil before spawn.
Created by `beads-agent-ralph--stream-spawn' via `make-process'.
Filters update slots on this object; the sentinel transitions `status'.")
   (events
    :initarg :events
    :initform nil
    :type list
    :documentation "Vector of parsed event plists, in receive order.
Each entry is a plist describing one NDJSON line (e.g. system_init,
assistant, tool_use, tool_result, result).  Stored as a list with
`push'; reverse before display.  The hardening task adds bounded
truncation when the list grows past a configured limit.")
   (partial-line
    :initarg :partial-line
    :initform ""
    :type string
    :documentation "Buffer of bytes received but not yet terminated by newline.
The process filter appends raw output here, splits on \\n, and keeps
the trailing fragment for the next filter call.  This is the minimum
line-reassembly state; UTF-8 boundary safety is added in hardening.")
   (partial-messages
    :initarg :partial-messages
    :initform nil
    :documentation "Hash table keyed by message id for stream_event assembly.
Populated when `--include-partial-messages' is in effect; remains
empty in the skeleton.  Value shape is defined by the parser-hardening
task; reserved here so callers can hold a reference.")
   (status
    :initarg :status
    :initform 'starting
    :type symbol
    :documentation "Lifecycle state of the stream.
One of: `starting' (before any output observed), `running' (filter has
seen at least one event), `finished' (process exited 0 and result
event observed), `failed' (process exited non-zero or protocol error),
`stopped' (controller-initiated interrupt).  The sentinel is the only
writer for terminal states.")
   (started-at
    :initarg :started-at
    :initform nil
    :documentation "Wall-clock time when `make-process' returned.
Stored as the value returned by `current-time'.  Used together with
`finished-at' to derive duration when the result event is missing.")
   (finished-at
    :initarg :finished-at
    :initform nil
    :documentation "Wall-clock time when the sentinel observed a terminal status.
Stored as the value returned by `current-time', or nil while running.")
   (cost-usd
    :initarg :cost-usd
    :initform nil
    :documentation "Total cost in USD reported by the final result event.
Nil until the result event is parsed.  Plan reads `cost-usd' from the
NDJSON payload; the empirical-TODO harness (bde-vuz2) verifies the
actual schema field name before this is finalized.")
   (duration-ms
    :initarg :duration-ms
    :initform nil
    :documentation "Wall-clock duration in milliseconds from the result event.
Nil until the result event is parsed.  Plan reads `duration-ms';
verified empirically alongside `cost-usd'.")
   (session-id
    :initarg :session-id
    :initform nil
    :documentation "Claude session id from the system_init event, if any.
Useful for log correlation; not used to resume since Ralph runs with
`--no-session-persistence'.")
   (sentinel-hit
    :initarg :sentinel-hit
    :initform nil
    :type boolean
    :documentation "Non-nil once the agent emitted the completion sentinel.
The controller polls this to decide whether the iteration is done by
agent declaration vs. by external termination.  Set by the parser
hardening pass; remains nil in the skeleton.")
   (bd-updates
    :initarg :bd-updates
    :initform nil
    :type list
    :documentation "List of (ISSUE-ID . ACTION) pairs observed in tool_use events.
Captures any `bd update'/`bd close'/`bd dep' the agent ran via its
tool harness.  Populated by the parser; empty in the skeleton.")
   (files-touched
    :initarg :files-touched
    :initform nil
    :documentation "Set of filesystem paths the agent edited this iteration.
Stored as a list deduplicated on insertion (small N expected).  Used by
the iteration record for the dashboard files-touched column.")
   (last-text
    :initarg :last-text
    :initform nil
    :documentation "Last non-empty assistant text content seen on the stream.
Surfaced in the dashboard footer and used as a fallback summary when
no explicit summary tag is emitted.")
   (summary-tag
    :initarg :summary-tag
    :initform nil
    :documentation "Contents of the agent's <summary>...</summary> tag, if emitted.
Preferred over `last-text' for the per-iteration summary column when
present.")
   (stderr-tail
    :initarg :stderr-tail
    :initform nil
    :type list
    :documentation "Bounded ring of the most recent stderr lines (max 50).
Stored newest-first.  The hardening pass enforces the bound; the
skeleton appends without trimming.  Used in failure banners.")
   (verify-result
    :initarg :verify-result
    :initform nil
    :documentation "Plist (:exit N :stdout S :stderr S) from the verify-command run.
Set by the controller after the iteration if a verify command is
configured; the stream itself does not invoke verify.  Reserved here
so the iteration record can copy it from the active stream snapshot.")
   (in-flight-guard
    :initarg :in-flight-guard
    :initform nil
    :type boolean
    :documentation "Non-nil while a flush is currently dispatching subscribers.
The flush function checks this on entry: if it is already set, the
inner call sets `pending-render' instead of re-entering dispatch.
This protects against re-entrancy when a subscriber callback runs
`accept-process-output' (or `sit-for', or anything else that pumps
the event loop) while flush is on the stack.")
   (pending-render
    :initarg :pending-render
    :initform nil
    :type boolean
    :documentation "Non-nil when a flush is scheduled or was deferred.
Set by the filter to mark new events; cleared by the flush after a
successful dispatch.  If a re-entrant flush is blocked by
`in-flight-guard', it sets `pending-render' so the outer flush
re-runs once before returning.")
   (flush-timer
    :initarg :flush-timer
    :initform nil
    :documentation "Idle timer scheduling the next subscriber flush, or nil.
The filter schedules at most one timer; subsequent filter calls
flip `pending-render' but do not re-schedule.  The flush function
clears this slot before running, so a subscriber that schedules
work via the filter loop can rearm the timer cleanly.")
   (subscribers
    :initarg :subscribers
    :initform nil
    :type list
    :documentation "List of (LABEL . CALLBACK) cells notified on stream change.
CALLBACK is invoked with the stream object after each parsed event or
status transition.  LABEL is an interned symbol used by callers (the
vui dashboard, persistence) to remove their subscription on teardown."))
  :documentation "One running `claude --print' stream and its parse state.

This object is the unit of concurrency for the Ralph loop: exactly
one is alive per running iteration.  The controller spawns it via
`beads-agent-ralph--stream-spawn', subscribes to status/event
changes, and treats it as opaque otherwise.

Concurrency model: filters mutate slots in place and SCHEDULE a
deferred flush via `run-at-time' — they never dispatch subscribers
synchronously.  The sentinel forces a synchronous drain
(`accept-process-output' + final flush) before setting terminal
status so subscribers observe the result event before the process
is declared done.  Re-entrancy from subscribers that pump the event
loop is mitigated by `in-flight-guard'.

Partial-message reassembly (when `--include-partial-messages' is
in argv) is reserved on `partial-messages' but not yet wired,
pending empirical TODO #3 in the plan.")

;;; Subscriber Helpers

(defun beads-agent-ralph--stream-subscribe (stream label callback)
  "Subscribe CALLBACK under LABEL to STREAM events.
LABEL is a symbol identifying the subscriber so it can be removed by
`beads-agent-ralph--stream-unsubscribe'.  CALLBACK receives the
stream object after each parsed event and on status transitions."
  (oset stream subscribers
        (cons (cons label callback)
              (cl-remove label (oref stream subscribers)
                         :key #'car))))

(defun beads-agent-ralph--stream-unsubscribe (stream label)
  "Remove the subscriber identified by LABEL from STREAM."
  (oset stream subscribers
        (cl-remove label (oref stream subscribers) :key #'car)))

(defun beads-agent-ralph--stream-dispatch (stream)
  "Dispatch all subscribers of STREAM synchronously, ONCE.

Internal helper used by the flush.  Subscriber errors are caught per
callback so a bad one cannot break the others or break the loop.

CONTRACT: subscribers MUST NOT call `accept-process-output',
`sit-for', or anything else that pumps the Emacs event loop
synchronously.  Doing so risks a re-entrant filter call between
events; the `in-flight-guard' check in `beads-agent-ralph--stream-flush'
mitigates the damage but cannot make it free."
  (dolist (entry (oref stream subscribers))
    (condition-case err
        (funcall (cdr entry) stream)
      (error
       (message "beads-agent-ralph stream subscriber %s errored: %S"
                (car entry) err)))))

(defun beads-agent-ralph--stream-flush (stream)
  "Run pending subscriber dispatch for STREAM.

If a flush is already running (`in-flight-guard' is set), this call
flips `pending-render' so the outer flush re-runs once before
returning and exits without re-entering dispatch.  This protects
against re-entrancy from subscribers that pump the event loop.

Clears `flush-timer' so the next filter call can re-arm cleanly."
  (when (timerp (oref stream flush-timer))
    (cancel-timer (oref stream flush-timer)))
  (oset stream flush-timer nil)
  (cond
   ((oref stream in-flight-guard)
    ;; Re-entered while an outer flush is running.  Mark pending and
    ;; let the outer flush re-run.
    (oset stream pending-render t))
   (t
    (unwind-protect
        (progn
          (oset stream in-flight-guard t)
          ;; Loop so that re-entrant pending-render flips during
          ;; dispatch are absorbed without dropping notifications.
          ;; Cap at 8 iterations to guard against pathological
          ;; subscriber loops.
          (let ((iterations 0)
                (continue t))
            (while continue
              (oset stream pending-render nil)
              (beads-agent-ralph--stream-dispatch stream)
              (cl-incf iterations)
              (setq continue (and (oref stream pending-render)
                                  (< iterations 8))))))
      (oset stream in-flight-guard nil)))))

(defun beads-agent-ralph--stream-schedule-flush (stream)
  "Mark STREAM dirty and schedule a deferred flush if not already scheduled.

The filter never dispatches synchronously: it parses bytes into events
and calls this function.  The flush runs on the next event-loop pump
via `run-at-time' (not `run-with-idle-timer', which is unreliable in
batch tests) so the filter returns to Emacs immediately, keeping the
UI responsive even under high event rates."
  (oset stream pending-render t)
  (unless (timerp (oref stream flush-timer))
    (oset stream flush-timer
          (run-at-time
           0 nil
           #'beads-agent-ralph--stream-flush stream))))

;; Compatibility shim: the controller and tests still call -notify.
;; Route through the deferred flush so all dispatch goes through one
;; surface (avoids the two-paths-to-subscribers footgun called out in
;; the spec).
(defalias 'beads-agent-ralph--stream-notify
  #'beads-agent-ralph--stream-schedule-flush
  "Alias kept for callers that expected the synchronous-sounding name.
Dispatch is deferred via `beads-agent-ralph--stream-schedule-flush'.")

;;; NDJSON Filter

(defun beads-agent-ralph--stream-parse-line (stream line)
  "Parse a single NDJSON LINE and update STREAM in place.
Returns the parsed plist on success, or nil if LINE was empty or
malformed.  Malformed lines are recorded as events of type `error'
so they remain visible in the dashboard rather than being silently
dropped."
  (when (and line (not (string-empty-p line)))
    (let ((parsed
           (condition-case _err
               (let ((json-object-type 'plist)
                     (json-array-type 'list)
                     (json-key-type 'keyword)
                     (json-false nil)
                     (json-null nil))
                 (json-read-from-string line))
             (error
              (list :type 'error :raw line)))))
      (oset stream events (cons parsed (oref stream events)))
      ;; Cheap status transition: as soon as any event arrives we are
      ;; no longer in `starting'.  Terminal transitions remain the
      ;; sentinel's responsibility.
      (when (eq (oref stream status) 'starting)
        (oset stream status 'running))
      parsed)))

(defun beads-agent-ralph--stream-filter (stream chunk)
  "Process filter body for STREAM, given decoded CHUNK from stdout.

Splits CHUNK on newline boundaries and parses each complete NDJSON
line.  Bytes received but not terminated by a newline are preserved
in `partial-line' for the next call.  Because `make-process' was
called with `:coding \\='utf-8-unix', CHUNK is an Emacs string with
correct UTF-8 boundaries; multibyte glyphs split across kernel reads
are reassembled by Emacs's coding system, not by us.

Schedules a deferred flush; never dispatches subscribers
synchronously."
  (let* ((buffered (concat (oref stream partial-line) chunk))
         (parts (split-string buffered "\n"))
         (trailing (car (last parts)))
         (complete-lines (butlast parts)))
    (oset stream partial-line (or trailing ""))
    (dolist (line complete-lines)
      (beads-agent-ralph--stream-parse-line stream line))
    (beads-agent-ralph--stream-schedule-flush stream)))

(defun beads-agent-ralph--stream-stderr-filter (stream chunk)
  "Append CHUNK (split on newlines) to STREAM's stderr-tail.
The tail is bounded by `beads-agent-ralph-stderr-tail-max'; older
lines are trimmed once the bound is exceeded.  Schedules a deferred
flush so banner subscribers see the new line."
  (dolist (line (split-string chunk "\n" t))
    (let* ((current (oref stream stderr-tail))
           (next (cons line current))
           (bound beads-agent-ralph-stderr-tail-max))
      (oset stream stderr-tail
            (if (> (length next) bound)
                (cl-subseq next 0 bound)
              next))))
  (beads-agent-ralph--stream-schedule-flush stream))

(defun beads-agent-ralph--stream-sentinel (stream event)
  "Process sentinel body for STREAM, given EVENT string from Emacs.

The sentinel handles the drain-vs-status race carefully:

1. Pump `accept-process-output' so any pending stdout chunks reach
   the filter before we declare the process done.
2. Parse any residual bytes left in `partial-line' (a final
   newline-less line that the filter has buffered).
3. Set the terminal `status' and `finished-at'.
4. Force a synchronous flush so subscribers observe the terminal
   transition without waiting on the idle timer.

Without step 1 we'd risk setting `status' = `finished' before the
last result event is parsed, which would hide cost data from the
controller and the dashboard."
  (let ((proc (oref stream process)))
    (when (processp proc)
      ;; Drain any pending stdout into the filter.
      (when (process-live-p proc)
        (accept-process-output proc 0 0 t))))
  ;; A final partial-line without a trailing newline is unusual but
  ;; possible if claude flushes mid-line on exit.  Parse it now.
  (let ((residual (oref stream partial-line)))
    (when (and residual (not (string-empty-p residual)))
      (beads-agent-ralph--stream-parse-line stream residual)
      (oset stream partial-line "")))
  (cond
   ((string-prefix-p "finished" event)
    (oset stream status 'finished))
   ((or (string-prefix-p "exited abnormally" event)
        (string-prefix-p "failed" event))
    (oset stream status 'failed))
   ((or (string-prefix-p "interrupt" event)
        (string-prefix-p "killed" event))
    (oset stream status 'stopped)))
  (oset stream finished-at (current-time))
  ;; Final flush is synchronous: dashboards and the controller need
  ;; the terminal transition immediately, not after idle.
  (beads-agent-ralph--stream-flush stream))

;;; Public Seam

(defun beads-agent-ralph--stream-spawn
    (&rest args)
  "Spawn a `claude --print' subprocess and return its stream object.

ARGS is a plist with the following keys (all but :prompt optional):

  :prompt             STRING — the prompt to pass to claude (required).
  :project-dir        STRING — passed as `--add-dir'; defaults to
                      `default-directory'.
  :max-budget-usd     NUMBER — passed as `--max-budget-usd'.
  :max-turns          NUMBER — passed as `--max-turns'.
  :permission-mode    STRING — passed as `--permission-mode'; defaults
                      to \"bypassPermissions\".
  :mcp-config         STRING — path passed as `--mcp-config' if set.
  :extra-args         LIST   — additional argv strings appended before
                      the prompt terminator.
  :program            STRING — override the `claude' executable
                      (defaults to \"claude\"); tests pass a stub here.
  :name               STRING — name for the Emacs process buffer;
                      defaults to \"beads-agent-ralph\".

Returns a `beads-agent-ralph--stream' whose `process' slot holds the
live subprocess.  The caller subscribes via
`beads-agent-ralph--stream-subscribe' to be notified of events.

CONTRACT: this is the test seam.  Tests rebind it with `cl-letf' to
return a pre-populated stream without launching a real process.  Keep
the signature and return shape stable across refactors."
  (let* ((prompt (or (plist-get args :prompt)
                     (error "Beads-agent-ralph--stream-spawn requires :prompt")))
         (project-dir (or (plist-get args :project-dir) default-directory))
         (program (or (plist-get args :program) "claude"))
         (name (or (plist-get args :name) "beads-agent-ralph"))
         (permission-mode (or (plist-get args :permission-mode)
                              "bypassPermissions"))
         (max-budget (plist-get args :max-budget-usd))
         (max-turns (plist-get args :max-turns))
         (mcp-config (plist-get args :mcp-config))
         (extra-args (plist-get args :extra-args))
         (argv
          (append
           (list program
                 "--print"
                 "--output-format" "stream-json"
                 "--verbose"
                 "--include-partial-messages"
                 "--no-session-persistence"
                 "--permission-mode" permission-mode
                 "--add-dir" (expand-file-name project-dir))
           (when max-budget
             (list "--max-budget-usd" (format "%s" max-budget)))
           (when max-turns
             (list "--max-turns" (format "%s" max-turns)))
           (when mcp-config
             (list "--mcp-config" mcp-config))
           extra-args
           (list "--" prompt)))
         (stream (beads-agent-ralph--stream
                  :partial-messages (make-hash-table :test #'equal)
                  :started-at (current-time)
                  :status 'starting))
         (proc
          (make-process
           :name name
           :command argv
           :buffer nil
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :stderr (make-pipe-process
                    :name (concat name "-stderr")
                    :buffer nil
                    :noquery t
                    :filter (lambda (_p chunk)
                              (beads-agent-ralph--stream-stderr-filter
                               stream chunk)))
           :filter (lambda (_p chunk)
                     (beads-agent-ralph--stream-filter stream chunk))
           :sentinel (lambda (_p event)
                       (beads-agent-ralph--stream-sentinel stream event)))))
    (oset stream process proc)
    ;; Close stdin immediately: claude --print reads its prompt from
    ;; argv, not stdin, and leaving it open masks parent-process leaks.
    (when (process-live-p proc)
      (process-send-eof proc))
    stream))

;;; Stop

(defun beads-agent-ralph--stream-stop (stream)
  "Interrupt STREAM's subprocess and let the sentinel mark it stopped.

Uses `interrupt-process' (portable on Windows where raw SIGINT is
unreliable).  If the process is still alive after
`beads-agent-ralph-stop-grace-ms' milliseconds, escalates to
`kill-process'.  The sentinel handles the terminal status transition;
this function never sets `status' directly."
  (let ((proc (oref stream process)))
    (when (process-live-p proc)
      (interrupt-process proc)
      (run-at-time (/ beads-agent-ralph-stop-grace-ms 1000.0)
                   nil
                   (lambda ()
                     (when (process-live-p proc)
                       (kill-process proc)))))))

(provide 'beads-agent-ralph-stream)

;;; beads-agent-ralph-stream.el ends here
