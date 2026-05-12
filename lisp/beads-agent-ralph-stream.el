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
    :documentation "Non-nil while the filter is actively parsing a chunk.
Used by the parser-hardening pass to defer re-entrant render
notifications.  Always nil in the skeleton.")
   (pending-render
    :initarg :pending-render
    :initform nil
    :type boolean
    :documentation "Non-nil when a render was deferred while `in-flight-guard' held.
Cleared once the deferred render is dispatched.  Reserved for the
hardening pass.")
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
changes, and treats it as opaque otherwise.  Filters mutate slots in
place; the sentinel is the sole writer of terminal `status' values.

The skeleton implementation parses one NDJSON line per newline,
appends each parsed plist to `events', and notifies subscribers.
UTF-8 boundary safety, partial-message reassembly, and bounded
ring trimming are layered on by `bde-2qha'.")

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

(defun beads-agent-ralph--stream-notify (stream)
  "Notify every subscriber of STREAM.
Errors in one callback do not stop the others; they are caught and
written to *Messages* so a bad subscriber cannot kill the loop."
  (dolist (entry (oref stream subscribers))
    (condition-case err
        (funcall (cdr entry) stream)
      (error
       (message "beads-agent-ralph stream subscriber %s errored: %S"
                (car entry) err)))))

;;; NDJSON Filter (skeleton)

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
  "Process filter body for STREAM, given raw CHUNK from stdout.
Splits CHUNK on newline boundaries, preserves any trailing partial
line in `partial-line', and parses each complete line.  Notifies
subscribers once after the whole chunk is consumed."
  (let* ((buffered (concat (oref stream partial-line) chunk))
         (parts (split-string buffered "\n"))
         (trailing (car (last parts)))
         (complete-lines (butlast parts)))
    (oset stream partial-line (or trailing ""))
    (dolist (line complete-lines)
      (beads-agent-ralph--stream-parse-line stream line))
    (beads-agent-ralph--stream-notify stream)))

(defun beads-agent-ralph--stream-stderr-filter (stream chunk)
  "Append CHUNK (split on newlines) to STREAM's stderr-tail."
  (dolist (line (split-string chunk "\n" t))
    (oset stream stderr-tail
          (cons line (oref stream stderr-tail))))
  (beads-agent-ralph--stream-notify stream))

(defun beads-agent-ralph--stream-sentinel (stream event)
  "Process sentinel body for STREAM, given EVENT string from Emacs.
Transitions `status' to a terminal value and stamps `finished-at'.
The string-based dispatch mirrors what Emacs delivers: `\"finished\\n\"',
`\"exited abnormally...\"', `\"interrupt\"', etc."
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
  (beads-agent-ralph--stream-notify stream))

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
  "Interrupt STREAM's subprocess and mark it stopped.
Sends SIGINT once, then SIGKILL if the process is still alive after a
brief grace period.  The sentinel will set `status' to `stopped'."
  (let ((proc (oref stream process)))
    (when (process-live-p proc)
      (interrupt-process proc)
      (run-at-time 1.0 nil
                   (lambda ()
                     (when (process-live-p proc)
                       (kill-process proc)))))))

(provide 'beads-agent-ralph-stream)

;;; beads-agent-ralph-stream.el ends here
