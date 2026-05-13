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
;; The parser is hardened beyond plain line buffering: it handles
;; UTF-8 boundary splits across chunks, reentrant subscriber dispatch
;; (events queued while a subscriber is running flush after it returns),
;; the drain-on-sentinel race (final NDJSON lines that arrive between
;; sentinel and process exit are flushed before `on-finish' fires), the
;; `--include-partial-messages' SSE shape (assistant / tool-use deltas
;; reassembled into single coalesced events), and deferred dispatch via
;; `run-at-time' so subscribers always run from a known scheduler tick
;; rather than directly from the process filter.  Empirical verification
;; for each behaviour lives under `lisp/test/ralph-empirical/'.

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
    :documentation "List of parsed event plists, newest-first (push order).
Each entry is a plist describing one NDJSON line (e.g. system_init,
assistant, tool_use, tool_result, result).  Consumers `reverse' for
chronological order; the bde-u9b7 persist subscriber walks an
`eq'-cached head cell to stay O(new-events-per-flush) without paying
for a full reverse.  This list is intentionally unbounded -- the
persistence subscriber writes every event to NDJSON on disk and the
dashboard renders from the in-memory list, so capping here would
silently drop history the dashboard's expand affordance promises.
A chatty overnight iter can grow this to tens of thousands of plists;
that is by design, bounded only by per-iter cost / max-turns guards.")
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
Populated when `--include-partial-messages' is in effect.  Keys are
the message ids from `message_start' events; values are per-message
buckets (themselves hash tables) holding accumulating blocks indexed
by their `index' field.  The reserved keys `:current-message-id'
\(the last id seen on a `message_start') and `:meta' (message-level
fields: role, stop-reason, stopped) are also stored.

On `message_stop' the bucket is finalised: an `assistant'-shaped
event is synthesised and pushed onto `events' so downstream
extractors do not need a separate path.  The bucket itself is kept
in the hash with `:stopped t' for forensic queries (replay,
inspection).")
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
Nil until the result event is parsed.  Read from the `total_cost_usd'
field on the NDJSON `result' event; the schema is verified by
`lisp/test/ralph-empirical/06-cost-schema.sh'.")
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
when an assistant text block matches `beads-agent-ralph-stream-sentinel-regexp'.")
   (bd-updates
    :initarg :bd-updates
    :initform nil
    :type list
    :documentation "List of (ISSUE-ID . ACTION) pairs observed in tool_use events.
Captures any `bd update'/`bd close'/`bd dep' the agent ran via its
tool harness.  Populated by the parser as `Bash' tool_use events are
observed and parsed for bd subcommands.")
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
Stored newest-first; the parser trims to `beads-agent-ralph-stderr-tail-max'
on every append.  Used in failure banners.")
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
in argv) buckets `stream_event' envelopes by message id into
`partial-messages', accumulates content blocks, and synthesises a
canonical `assistant'-shaped event on `message_stop' so downstream
code is mode-agnostic.  See `beads-agent-ralph--stream-handle-partial'.")

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

;;; Partial-message reassembly
;;
;; When the controller spawns `claude --include-partial-messages', the
;; CLI emits `stream_event' envelopes that wrap the same Anthropic
;; Messages-API streaming events the SDK consumes:
;;
;;   message_start         carries message.id and role
;;   content_block_start   indexed block, type=text|tool_use|thinking
;;   content_block_delta   delta.type=text_delta|input_json_delta|...
;;   content_block_stop    marks a block complete
;;   message_delta         carries stop_reason
;;   message_stop          terminates the message
;;
;; The empirical harness `lisp/test/ralph-empirical/03-partial-messages-shape.sh'
;; documents the exact field shape against a real claude binary.  This
;; module assembles those partials back into a synthesised `assistant'
;; event with the same plist shape the non-partial path produces, so
;; downstream extractors (`-extract-summary-tag', `-extract-last-text',
;; `-extract-tool-uses') keep working uniformly across both modes.
;;
;; State lives in `partial-messages', a hash table keyed by the message
;; id from `message_start'.  Each value is a plist of accumulating
;; blocks indexed by their `index' field.  A block is itself a plist
;; with :type, :text (for text/thinking), :input-json (for tool_use
;; partial JSON), :name and :id (for tool_use), and :stopped.

(defun beads-agent-ralph--stream-event-payload (envelope)
  "Return the inner SSE event plist for a top-level stream_event ENVELOPE.
ENVELOPE is the parsed NDJSON line.  When ENVELOPE's :type is
\"stream_event\", returns the nested :event plist (or ENVELOPE itself
when the inner event is hoisted to the envelope's keys).  Returns nil
for non-partial envelopes so the caller can short-circuit."
  (when (listp envelope)
    (let ((type (plist-get envelope :type)))
      (when (or (equal type "stream_event") (eq type 'stream_event))
        (or (plist-get envelope :event)
            envelope)))))

(defun beads-agent-ralph--stream-event-type-eq (event tag)
  "Return non-nil when EVENT's :type slot matches TAG (a string)."
  (let ((type (and (listp event) (plist-get event :type))))
    (or (equal type tag)
        (eq type (intern tag)))))

(defun beads-agent-ralph--stream-partial-key (stream envelope inner)
  "Return the partial-messages hash key for ENVELOPE / INNER on STREAM.
INNER is the unwrapped SSE event from
`beads-agent-ralph--stream-event-payload'.  Resolution order:

  1. message_start carries message.id — use it and record on STREAM
     so subsequent partials in this message can recover the key.
  2. Otherwise fall back to STREAM's most recently observed id, which
     covers the common case where Anthropic's SSE stream does not
     repeat the id on every event.
  3. Failing that, fall back to the envelope's parent_tool_use_id or
     session_id so events still bucket somewhere rather than being
     silently dropped.

The fallbacks are defensive: if the empirical harness reveals a
different field, the dispatcher still finds a stable key."
  (let* ((inner-msg (plist-get inner :message))
         (msg-id (and (listp inner-msg) (plist-get inner-msg :id))))
    (cond
     ((and msg-id (stringp msg-id))
      ;; Stash on the stream as the "current" id so later events
      ;; without an explicit id route here.
      (puthash :current-message-id msg-id (oref stream partial-messages))
      msg-id)
     (t
      (or (gethash :current-message-id (oref stream partial-messages))
          (plist-get envelope :parent_tool_use_id)
          (plist-get envelope :session_id))))))

(defun beads-agent-ralph--stream-ensure-bucket (stream key)
  "Return STREAM's bucket plist for partial KEY, creating it if absent.
The bucket is itself a hash table keyed by block :index (so out-of-
order delta arrivals still land in the right block) plus a `:meta'
key holding message-level fields (role, stop_reason)."
  (let ((bucket (gethash key (oref stream partial-messages))))
    (unless bucket
      (setq bucket (make-hash-table :test #'equal))
      (puthash :meta (list :role nil :stop-reason nil :stopped nil) bucket)
      (puthash key bucket (oref stream partial-messages)))
    bucket))

(defun beads-agent-ralph--stream-handle-content-block-start
    (bucket inner)
  "Initialise BUCKET's slot for the block opened by INNER (content_block_start)."
  (let* ((index (plist-get inner :index))
         (block (or (plist-get inner :content_block)
                    (plist-get inner :content-block)))
         (type (and (listp block) (plist-get block :type)))
         (state
          (list :type type
                :text (or (and (listp block) (plist-get block :text)) "")
                :input-json ""
                :input (and (listp block) (plist-get block :input))
                :name (and (listp block) (plist-get block :name))
                :id (and (listp block) (plist-get block :id))
                :tool-use-id (and (listp block)
                                  (plist-get block :tool_use_id))
                :stopped nil)))
    (when (numberp index)
      (puthash index state bucket))))

(defun beads-agent-ralph--stream-handle-content-block-delta
    (bucket inner)
  "Append INNER's delta into BUCKET at INNER's index.
Handles text_delta (concatenated into :text), input_json_delta
\(concatenated into :input-json for later parsing once the block stops),
and thinking_delta (treated as text).  Unknown delta types are stored
verbatim under :unknown-deltas so a future taxonomy change does not
silently lose data."
  (let* ((index (plist-get inner :index))
         (delta (plist-get inner :delta))
         (delta-type (and (listp delta) (plist-get delta :type)))
         (state (and (numberp index) (gethash index bucket))))
    (unless state
      ;; Some streams omit content_block_start (or it arrived in a
      ;; separate flush before our hash existed); initialise lazily so
      ;; deltas are not dropped.
      (setq state (list :type nil :text "" :input-json "" :stopped nil))
      (when (numberp index)
        (puthash index state bucket)))
    (cond
     ((or (equal delta-type "text_delta") (eq delta-type 'text_delta))
      (let ((piece (or (plist-get delta :text) "")))
        (plist-put state :text (concat (plist-get state :text) piece))))
     ((or (equal delta-type "thinking_delta") (eq delta-type 'thinking_delta))
      (let ((piece (or (plist-get delta :thinking)
                       (plist-get delta :text)
                       "")))
        (plist-put state :text (concat (plist-get state :text) piece))
        (plist-put state :type (or (plist-get state :type) "thinking"))))
     ((or (equal delta-type "input_json_delta")
          (eq delta-type 'input_json_delta))
      (let ((piece (or (plist-get delta :partial_json)
                       (plist-get delta :partial-json)
                       "")))
        (plist-put state :input-json
                   (concat (plist-get state :input-json) piece))))
     (t
      ;; Unknown delta shape — keep for forensic visibility.
      (plist-put state :unknown-deltas
                 (cons delta (plist-get state :unknown-deltas)))))
    (when (numberp index)
      (puthash index state bucket))))

(defun beads-agent-ralph--stream-handle-content-block-stop
    (bucket inner)
  "Mark BUCKET's block at INNER's index as stopped and finalise input JSON."
  (let* ((index (plist-get inner :index))
         (state (and (numberp index) (gethash index bucket))))
    (when state
      (plist-put state :stopped t)
      (let ((raw (plist-get state :input-json)))
        (when (and (stringp raw) (not (string-empty-p raw)))
          (let ((parsed
                 (condition-case _err
                     (let ((json-object-type 'plist)
                           (json-array-type 'list)
                           (json-key-type 'keyword)
                           (json-false nil)
                           (json-null nil))
                       (json-read-from-string raw))
                   (error nil))))
            (when parsed
              (plist-put state :input parsed)))))
      (puthash index state bucket))))

(defun beads-agent-ralph--stream-handle-message-delta
    (bucket inner)
  "Capture stop_reason from INNER into BUCKET's :meta plist."
  (let* ((delta (plist-get inner :delta))
         (stop (and (listp delta) (plist-get delta :stop_reason)))
         (meta (gethash :meta bucket)))
    (when stop
      (plist-put meta :stop-reason stop)
      (puthash :meta meta bucket))))

(defun beads-agent-ralph--stream-bucket-blocks (bucket)
  "Return BUCKET's accumulated content blocks as a list ordered by index.
Skips the :meta key and any synthetic state; produces blocks in the
shape the non-partial assistant event uses (`(:type STR :text STR …)')
so downstream extractors do not need a separate path."
  (let (entries)
    (maphash
     (lambda (key value)
       (when (numberp key)
         (push (cons key value) entries)))
     bucket)
    (setq entries (sort entries (lambda (a b) (< (car a) (car b)))))
    (mapcar
     (lambda (entry)
       (let* ((state (cdr entry))
              (type (or (plist-get state :type) "text")))
         (cond
          ((or (equal type "text") (eq type 'text)
               (equal type "thinking") (eq type 'thinking))
           (list :type type :text (or (plist-get state :text) "")))
          ((or (equal type "tool_use") (eq type 'tool_use))
           (list :type type
                 :name (plist-get state :name)
                 :id (plist-get state :id)
                 :input (or (plist-get state :input)
                            (and (stringp (plist-get state :input-json))
                                 (not (string-empty-p
                                       (plist-get state :input-json)))
                                 (list :__raw (plist-get state :input-json))))))
          ((or (equal type "tool_result") (eq type 'tool_result))
           (list :type type
                 :tool_use_id (plist-get state :tool-use-id)
                 :content (plist-get state :text)))
          (t
           ;; Unknown block type — preserve raw shape.
           (append (list :type type) state)))))
     entries)))

(defun beads-agent-ralph--stream-finalize-bucket (stream key)
  "Synthesise an `assistant'-shaped event from STREAM's bucket KEY.
Returns the synthesised event plist (suitable for pushing into the
events vector) without mutating the events list.  The caller decides
whether to insert it (the parser inserts on message_stop)."
  (let* ((bucket (gethash key (oref stream partial-messages)))
         (meta (and bucket (gethash :meta bucket)))
         (blocks (and bucket (beads-agent-ralph--stream-bucket-blocks
                              bucket))))
    (when bucket
      (list :type "assistant"
            :message (list :id (and (stringp key) key)
                           :role (or (plist-get meta :role) "assistant")
                           :stop_reason (plist-get meta :stop-reason)
                           :content blocks)
            :__synthesized-from-partials t))))

(defun beads-agent-ralph--stream-handle-partial (stream envelope)
  "Process ENVELOPE against STREAM's partial-messages hash.

Returns one of:

  nil          — ENVELOPE is not a partial-message event.  The caller
                 stores it normally.
  `:absorbed'  — ENVELOPE updated partial state.  The caller stores
                 the envelope for forensics but should not re-emit it
                 as the canonical assistant event.
  PLIST        — assembly just finalised: returned plist is a
                 synthesised assistant event.  The caller pushes it
                 into `events' so extractors observe it once.

This function never raises on malformed inner shapes: missing keys
short-circuit to `:absorbed'."
  (let ((inner (beads-agent-ralph--stream-event-payload envelope)))
    (when inner
      (let* ((key (beads-agent-ralph--stream-partial-key
                   stream envelope inner)))
        (cond
         ((null key)
          ;; No id to bucket under.  Record as absorbed so the envelope
          ;; still lands in `events' for inspection but does not trigger
          ;; further partial state.
          :absorbed)
         (t
          (let ((bucket (beads-agent-ralph--stream-ensure-bucket stream key)))
            (cond
             ((beads-agent-ralph--stream-event-type-eq inner "message_start")
              (let* ((msg (plist-get inner :message))
                     (role (and (listp msg) (plist-get msg :role)))
                     (meta (gethash :meta bucket)))
                (plist-put meta :role (or role "assistant"))
                (puthash :meta meta bucket))
              :absorbed)
             ((beads-agent-ralph--stream-event-type-eq
               inner "content_block_start")
              (beads-agent-ralph--stream-handle-content-block-start
               bucket inner)
              :absorbed)
             ((beads-agent-ralph--stream-event-type-eq
               inner "content_block_delta")
              (beads-agent-ralph--stream-handle-content-block-delta
               bucket inner)
              :absorbed)
             ((beads-agent-ralph--stream-event-type-eq
               inner "content_block_stop")
              (beads-agent-ralph--stream-handle-content-block-stop
               bucket inner)
              :absorbed)
             ((beads-agent-ralph--stream-event-type-eq inner "message_delta")
              (beads-agent-ralph--stream-handle-message-delta bucket inner)
              :absorbed)
             ((beads-agent-ralph--stream-event-type-eq inner "message_stop")
              ;; Finalise: synthesise the assistant event and clear
              ;; :current-message-id so the next message starts fresh.
              (let ((synth (beads-agent-ralph--stream-finalize-bucket
                            stream key)))
                (remhash :current-message-id (oref stream partial-messages))
                ;; Mark the bucket itself stopped but keep it in the
                ;; hash for late-arriving forensic queries.
                (let ((meta (gethash :meta bucket)))
                  (plist-put meta :stopped t)
                  (puthash :meta meta bucket))
                synth))
             (t
              ;; Unknown inner type — absorb so we do not re-emit.
              :absorbed)))))))))

;;; NDJSON Filter

(defun beads-agent-ralph--stream-parse-line (stream line)
  "Parse a single NDJSON LINE and update STREAM in place.
Returns the parsed plist on success, or nil if LINE was empty or
malformed.  Malformed lines are recorded as events of type `error'
so they remain visible in the dashboard rather than being silently
dropped.

When LINE is a partial-message envelope (stream_event wrapping
content_block_delta, message_start, etc.), the envelope is recorded
verbatim AND routed through
`beads-agent-ralph--stream-handle-partial' to update the assembly
hash; on message_stop the synthesised `assistant'-shaped event is
pushed onto `events' so the same extractor reads partial and
non-partial runs uniformly."
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
      ;; Partial-message handling: absorbed events stay on `events'
      ;; (for forensics) but do not duplicate; on finalisation we
      ;; insert the synthesised assistant event before returning.
      (let ((synth (beads-agent-ralph--stream-handle-partial stream parsed)))
        (when (and synth (listp synth))
          (oset stream events (cons synth (oref stream events)))))
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
         ;; Create the stderr pipe first and hold a handle.  If
         ;; `make-process' below raises (e.g. claude binary missing,
         ;; fork failure, EAGAIN), the pipe-process would otherwise be
         ;; orphaned with no caller-visible handle to delete.  Mirror
         ;; the bde-km3r fix shape for `run-shell-async'.
         (stderr-proc (make-pipe-process
                       :name (concat name "-stderr")
                       :buffer nil
                       :noquery t
                       :filter (lambda (_p chunk)
                                 (beads-agent-ralph--stream-stderr-filter
                                  stream chunk))))
         (spawn-ok nil)
         (proc nil))
    (unwind-protect
        (progn
          (setq proc
                (make-process
                 :name name
                 :command argv
                 :buffer nil
                 :connection-type 'pipe
                 :coding 'utf-8-unix
                 :noquery t
                 :stderr stderr-proc
                 :filter (lambda (_p chunk)
                           (beads-agent-ralph--stream-filter stream chunk))
                 :sentinel (lambda (_p event)
                             (beads-agent-ralph--stream-sentinel stream event))))
          (setq spawn-ok t))
      (unless spawn-ok
        (when (process-live-p stderr-proc)
          (delete-process stderr-proc))))
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
