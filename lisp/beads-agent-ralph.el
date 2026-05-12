;;; beads-agent-ralph.el --- Ralph Wiggum loop controller for bd issues -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module is the OUTER layer of the Ralph integration.  It owns
;; the iteration state machine, the per-iteration history, and the
;; async chaining primitive (`beads-agent-ralph--then') used by the
;; per-iteration sequence (bde-q3b4).  It is deliberately
;; backend-agnostic about transport: any subprocess work goes through
;; `beads-agent-ralph-stream'.
;;
;; This file ships the SKELETON only:
;;
;;   - `beads-agent-ralph--controller' defclass
;;   - `beads-agent-ralph--iteration' defclass
;;   - `beads-agent-ralph--then' async chaining helper
;;   - `beads-agent-ralph--schedule-next-iteration', the reentry guard
;;
;; The per-iteration step list, sentinel/stall/lying-agent detectors,
;; cost guards, worktree sharing, verify sandbox, persistence, dashboard
;; rendering, mode-line indicator, notifications, and backend protocol
;; wiring all live in follow-up tasks.  This skeleton is what they
;; build on top of.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'subr-x)

(require 'beads-agent-ralph-stream)
(require 'beads-command)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-command-update)
(require 'beads-types)

;;; Iteration Record

(defclass beads-agent-ralph--iteration ()
  ((issue-id
    :initarg :issue-id
    :initform nil
    :documentation "Beads issue id the agent was asked to work on.
For epic mode this is the sub-issue picked from the ready queue at the
start of this iteration, not the root epic id.")
   (status
    :initarg :status
    :initform nil
    :type symbol
    :documentation "Outcome symbol for this iteration.
One of: `finished' (sentinel hit or status moved to closed),
`failed' (verify failed, process crashed, lying-agent detected), or
`stalled' (no diff + no bd updates + no sentinel within budget).")
   (duration-ms
    :initarg :duration-ms
    :initform nil
    :documentation "Wall-clock duration of this iteration in milliseconds.
Sourced from the stream's result event when present; falls back to
finished-at - started-at otherwise.")
   (cost-usd
    :initarg :cost-usd
    :initform nil
    :documentation "Cost of this iteration in USD as reported by the result event.
Nil if the iteration ended before a result event was emitted.")
   (acceptance-before
    :initarg :acceptance-before
    :initform nil
    :documentation "Acceptance-criteria block extracted from the bd issue pre-iter.
Captured so the dashboard can diff against `acceptance-after' and so
the lying-agent detector has a baseline.")
   (acceptance-after
    :initarg :acceptance-after
    :initform nil
    :documentation "Acceptance-criteria block re-extracted after the iteration.
Compared against `acceptance-before' to flag silent regressions.")
   (files-touched
    :initarg :files-touched
    :initform nil
    :type list
    :documentation "Deduplicated list of filesystem paths edited this iteration.
Copied from the stream when the iteration finishes.")
   (tool-call-count
    :initarg :tool-call-count
    :initform 0
    :type integer
    :documentation "Number of tool_use events observed on the stream.
Used as a coarse proxy for activity level when detecting stalls.")
   (summary
    :initarg :summary
    :initform nil
    :documentation "Short summary string for the dashboard row.
Prefers the agent's <summary> tag; falls back to the last assistant
text or a generic synthesized string when neither is present.")
   (sentinel-hit
    :initarg :sentinel-hit
    :initform nil
    :type boolean
    :documentation "Non-nil when the agent emitted the completion sentinel.
Distinct from `status' = `finished' so we can tell agent-declared
completion apart from externally-detected completion (e.g. closed bd
issue, budget exhausted).")
   (bd-updates-count
    :initarg :bd-updates-count
    :initform 0
    :type integer
    :documentation "Count of bd-mutating tool_use events observed.
Higher counts loosen stall detection: an agent that wrote notes did
useful work even if no files changed.")
   (stderr-tail
    :initarg :stderr-tail
    :initform nil
    :type list
    :documentation "Last lines of the stream's stderr (newest first), copied at end.
Surfaced in failure banners so the user does not need to scrape the
agent buffer to see why the iteration failed.")
   (verify-result
    :initarg :verify-result
    :initform nil
    :documentation "Plist (:exit N :stdout S :stderr S) from the verify command.
Nil if no verify command was configured for the run."))
  :documentation "One completed Ralph iteration.

The controller pushes one of these into `history' when an iteration
finishes (in any terminal state).  The dashboard reads from this
record directly; it should contain everything needed to render a row
without re-querying the stream or bd.")

;;; Controller

(defclass beads-agent-ralph--controller ()
  ((root-id
    :initarg :root-id
    :initform nil
    :documentation "Beads issue or epic id the loop was launched against.
For issue mode this is also `current-issue-id' every iteration.  For
epic mode the controller walks the ready sub-issues of this root.")
   (root-kind
    :initarg :root-kind
    :initform 'issue
    :type symbol
    :documentation "Either `issue' or `epic'.
Controls iteration selection: issue mode keeps `current-issue-id' fixed
until done; epic mode picks the next ready sub-issue each iteration.")
   (project-dir
    :initarg :project-dir
    :initform nil
    :documentation "Project root the loop is bound to.
This is the directory the user launched the loop from; the worktree
(if any) is derived from it.")
   (worktree-dir
    :initarg :worktree-dir
    :initform nil
    :documentation "Working directory for `claude --add-dir', or nil for the project root.
Set by the worktree-sharing step (bde-e8lj); the skeleton leaves it nil.")
   (iteration
    :initarg :iteration
    :initform 0
    :type integer
    :documentation "Zero-based counter of iterations attempted (failed + ok).
Incremented by `beads-agent-ralph--schedule-next-iteration' before each
new spawn so dashboard rows line up with their iteration index.")
   (max-iterations
    :initarg :max-iterations
    :initform 50
    :type integer
    :documentation "Hard ceiling on iterations regardless of outcome.
Reaching this number transitions the controller to `done' with
`done-reason' = `budget'.")
   (cumulative-cost-usd
    :initarg :cumulative-cost-usd
    :initform 0.0
    :documentation "Running sum of per-iteration `cost-usd' values.
Compared against the total-cost guard each iteration.")
   (permission-mode
    :initarg :permission-mode
    :initform "bypassPermissions"
    :type string
    :documentation "Passed straight through to `claude --permission-mode'.
Override only when running against a sandboxed harness.")
   (prompt-template
    :initarg :prompt-template
    :initform nil
    :documentation "String template with %-placeholders for prompt construction.
Rendered each iteration by the prompt-construction task (bde-aqu1);
left nil in the skeleton, which forces tests to supply a template.")
   (prompt-file
    :initarg :prompt-file
    :initform nil
    :documentation "Optional path persisted with the rendered prompt for the iteration.
Reserved for the persistence task (bde-1p2c).")
   (verify-command
    :initarg :verify-command
    :initform nil
    :documentation "Shell string OR function invoked between iterations to verify.
Nil disables verification.  The sandboxing task (bde-e8lj) wraps shell
strings in a protected-paths jail; functions are called as `(funcall
fn controller)' and must return a plist matching `verify-result'.")
   (current-stream
    :initarg :current-stream
    :initform nil
    :documentation "Active `beads-agent-ralph--stream' for the in-flight iteration.
Nil between iterations.  The dashboard reads this for live rendering;
the stop command kills its process.")
   (current-issue-id
    :initarg :current-issue-id
    :initform nil
    :documentation "Issue the in-flight or next iteration targets.
Updated by the resolve-target step at the start of each iteration.")
   (history
    :initarg :history
    :initform nil
    :type list
    :documentation "List of finished `beads-agent-ralph--iteration', newest first.
Pushed to once per iteration in any terminal state.")
   (false-claim-count
    :initarg :false-claim-count
    :initform 0
    :type integer
    :documentation "Count of iterations where the agent claimed done but evidence disagreed.
Three in a row trips the lying-agent detector (bde-q3b4) and pauses
the loop.")
   (last-bd-note
    :initarg :last-bd-note
    :initform nil
    :documentation "Most recent bd notes payload authored by the agent.
Threaded into the next iteration's PLAN-VIEW so the agent sees its own
prior reasoning without depending on long context.")
   (last-verify
    :initarg :last-verify
    :initform nil
    :documentation "Verify-command result from the previous iteration.
Used by stall detection (a green verify after a red one is not a
stall even if no files changed).")
   (last-git-diff-stat
    :initarg :last-git-diff-stat
    :initform nil
    :documentation "Output of `git diff --stat HEAD' captured at iteration end.
Used by stall detection: an unchanged stat across iterations counts
toward `consecutive-stalls'.")
   (consecutive-stalls
    :initarg :consecutive-stalls
    :initform 0
    :type integer
    :documentation "Number of stalled iterations in a row.
Two in a row trips the stall detector and pauses the loop.")
   (status
    :initarg :status
    :initform 'idle
    :type symbol
    :documentation "Top-level controller state.
One of: `idle' (no iteration scheduled), `running' (iteration in
flight), `cooling-down' (between iterations, waiting on iteration-delay),
`done' (terminal success), `stopped' (terminal user-initiated),
`failed' (terminal failure), `auto-paused' (recoverable pause from
stall or lying-agent detector; resume keeps state).")
   (done-reason
    :initarg :done-reason
    :initform nil
    :type symbol
    :documentation "Why the controller left the running family of states.
One of: `sentinel' (agent declared done), `closed' (bd issue moved to
closed externally), `epic-empty' (epic mode ran out of ready
children), `budget' (max-iterations or cost cap), `stop' (user),
`failed' (terminal failure).")
   (iteration-delay
    :initarg :iteration-delay
    :initform 0.0
    :documentation "Seconds to wait between iterations.
Even when zero, the next iteration is dispatched via `run-at-time' so
the call stack does not grow across sentinels.  Configurable so the
user can rate-limit overnight runs.")
   (banner-log
    :initarg :banner-log
    :initform nil
    :type list
    :documentation "Bounded ring of past banner records, newest first.
Each entry is a plist (:severity SYM :text STR :time TIME).  The
dashboard renders the most severe recent banner; the ring keeps a
short history for the action bar's history toggle."))
  :documentation "Ralph loop controller: iteration state machine.

One controller per running loop.  The dashboard buffer holds a
reference to this; the backend protocol subclass (bde-k0pj) wraps it
so the rest of beads.el can talk to a Ralph session via the existing
`beads-agent-backend' protocol.

Status transitions to `auto-paused' (recoverable with one keystroke)
rather than to terminal `failed' for stall and lying-agent detection.
Resume preserves `iteration', `cumulative-cost-usd', and `history' so
the recovery is genuinely transparent.")

;;; Async chaining helper

(defun beads-agent-ralph--then (&rest spec)
  "Step through a chain of async tasks, with cancellation polling between each.

SPEC is a plist:

  :steps      LIST of (lambda (ACC K) ...) — each step does its async
              work, then calls (funcall K ERR RESULT) exactly once.
              ERR is nil on success, or a symbol naming the failure.
              RESULT is merged into the accumulator on success.
  :on-error   FN (ERR INDEX) — invoked on the first failure or on
              cancellation.  INDEX is the zero-based position of the
              failing step.  Subsequent steps are NOT run.
  :cancelled  PRED () — checked before each step.  If non-nil, the
              chain aborts with ERR = `cancelled', via :on-error.
  :acc        PLIST — initial accumulator, defaults to nil.

This is a function, not a macro: the accumulator is plain data and
there is no special binding form to remember.  One step error
short-circuits to :on-error; on-error is never called for subsequent
steps.  This is the `then' of the per-iteration sequence.

The function returns nil; results are delivered via callbacks."
  (let* ((steps (plist-get spec :steps))
         (on-error (plist-get spec :on-error))
         (cancelled (plist-get spec :cancelled))
         (acc (plist-get spec :acc))
         (idx 0))
    (cl-labels
        ((run-next
           (current-acc)
           (cond
            ((and cancelled (funcall cancelled))
             (when on-error
               (funcall on-error 'cancelled idx)))
            ((null steps)
             ;; Chain completed; nothing else to do.
             nil)
            (t
             (let ((step (car steps)))
               (setq steps (cdr steps))
               (let ((this-idx idx))
                 (cl-incf idx)
                 (condition-case err
                     (funcall
                      step current-acc
                      (lambda (step-err result)
                        (cond
                         (step-err
                          (when on-error
                            (funcall on-error step-err this-idx)))
                         (t
                          (run-next
                           (if (and (listp result)
                                    (or (null result)
                                        (keywordp (car result))))
                               (append current-acc result)
                             current-acc))))))
                   (error
                    (when on-error
                      (funcall on-error
                               (or (car-safe err) 'step-signal)
                               this-idx))))))))))
      (run-next acc)
      nil)))

;;; Reentry guard

(defun beads-agent-ralph--schedule-next-iteration (controller thunk)
  "Schedule THUNK as the next iteration body for CONTROLLER.

THUNK is invoked via `run-at-time' with delay
`(oref controller iteration-delay)' — even when zero — so the call
stack is broken across the process sentinel that triggered scheduling.
This prevents the controller from re-entering itself mid-callback,
which would otherwise corrupt `current-stream' and confuse subscribers.

Increments `iteration' before the thunk runs so the new iteration's
records line up with their dashboard row."
  (let ((delay (or (oref controller iteration-delay) 0.0)))
    (run-at-time
     delay nil
     (lambda ()
       (cl-incf (oref controller iteration))
       (funcall thunk)))))

;;; Customization

(defcustom beads-agent-ralph-stop-on-failed nil
  "Non-nil to terminate the loop on the first failed iteration.
A `failed' iteration is one where the `claude' subprocess exited
non-zero or the verify command, if configured, returned non-zero.
The default nil matches the Ralph essay's spirit: keep iterating
through transient errors so a single hiccup doesn't abort an
overnight run.  Set to t when you want CI-style fail-fast behaviour."
  :type 'boolean
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-max-budget-usd nil
  "Cumulative cost cap (USD) across all iterations of a single loop.
Nil disables the cap.  When set, the controller halves the per-iter
budget passed to `claude --max-budget-usd' to whatever remains under
the total before each spawn; reaching the cap transitions the
controller to `done' with `done-reason' = `budget'."
  :type '(choice (const :tag "Unlimited" nil) number)
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-max-budget-usd-per-iter nil
  "Per-iteration cost cap (USD) passed to `claude --max-budget-usd'.
Nil omits the flag.  When both per-iter and total caps are set, the
controller passes the smaller of (per-iter, total - cumulative) to
the next spawn so the total cap is never exceeded."
  :type '(choice (const :tag "Unlimited" nil) number)
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-max-turns nil
  "Maximum agent turns per iteration, passed to `claude --max-turns'.
Nil omits the flag.  This is a coarser guard than the cost budget
but works even when the agent is calling cheap tools in a tight loop."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-summary-max-len 100
  "Maximum characters retained for an iteration's `summary' slot.
Longer summaries are truncated with an ellipsis.  Keeps the dashboard
row legible without expanding."
  :type 'integer
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-stall-threshold 2
  "Number of consecutive stalled iterations that trip `auto-paused'.
A stalled iteration is one with no bd-mutating tool calls, no
files-touched, and no completion sentinel.  Two in a row is the
default per the plan: one stall is noise, two is a pattern."
  :type 'integer
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-banner-log-max 64
  "Maximum number of banner records retained on the controller.
Older entries are dropped from `banner-log' once this bound is
exceeded.  The dashboard renders only the most severe recent banner;
the rest accumulate for the history toggle."
  :type 'integer
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-sentinel "<promise>COMPLETE</promise>"
  "Literal string the agent must emit to signal root-level completion.
Substituted for the <SENTINEL> placeholder in the prompt template.
Must match `beads-agent-ralph--sentinel-regexp' so the stream parser
recognises the emitted token; change both together."
  :type 'string
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-verify-tail-bytes 4000
  "Maximum bytes of previous verify stdout/stderr included in <VERIFY-TAIL>.
Long tails crowd out the rest of the prompt context window without
helping the agent.  4 KB is enough for a typical failing test trace."
  :type 'integer
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-prompt
  "You are an autonomous agent driving a Beads-tracked codebase.
The orchestrator launches you one iteration at a time; each
iteration is a fresh process with no memory of the prior one, so
every fact you need lives in bd state or in this prompt.

ITERATION       <ITERATION> / <MAX-ITERATIONS>
ROOT            <ROOT-ID>
CURRENT TARGET  <ISSUE-ID> -- <ISSUE-TITLE>

DESCRIPTION
<ISSUE-DESCRIPTION>

ACCEPTANCE CRITERIA
<ACCEPTANCE>

PLAN VIEW (children of <ROOT-ID>)
<PLAN-VIEW>

PREVIOUS VERIFY
<VERIFY-TAIL>

DIFF STAT SINCE LAST ITER
<GIT-DIFF-STAT>

<PRIOR-FALSE-CLAIMS>
<PRIOR-STALL-COUNT>

WHAT TO DO THIS ITERATION

1. Re-read bd state fresh.  Run `bd show <ROOT-ID> --json` and
   `bd show <ISSUE-ID> --json` BEFORE acting.  The PLAN VIEW above
   is a stale summary; trust the live bd output.

2. Make ONE focused unit of progress on <ISSUE-ID>.  Do not start
   side quests.  If you notice work for a different issue listed
   in PLAN VIEW, append a short note to it with
   `bd update <other-id> --notes \"…\"` and keep moving.  Those
   notes flow back into the next iteration's PLAN VIEW.

3. Record your progress with `bd update <ISSUE-ID> --notes \"…\"`.
   Move the issue to `bd close <ISSUE-ID>` only when its
   acceptance criteria are demonstrably satisfied; otherwise
   leave it open for the next iteration.

4. Before exiting, emit exactly one line of the form

       <summary>one short paragraph recapping this iteration</summary>

   This is the dashboard's summary column for this iteration.

5. Emit the completion sentinel <SENTINEL> ONLY when both of the
   following hold in THIS iteration:

     a) you just ran `bd show <ROOT-ID> --json`, and
     b) the parsed `status` field of that output equals \"closed\".

   The loop will detect a false claim and pause itself; do not
   emit the sentinel speculatively."
  "Prompt template fed to `claude' each iteration.
The template is interpolated with the placeholders documented in
`beads-agent-ralph--render-prompt'.  Strings that look like
placeholders but are not in that set are left untouched, so user-
supplied issue content is safe to interpolate.

Override per-repo by setting `beads-agent-ralph-prompt-file' to a
file path that holds the desired template, or by binding this
defcustom in a `dir-locals.el'.

The default explicitly implements the (a)-(g) instructions on the
parent issue: fresh re-read, single focused unit, summary tag,
bd updates, cross-issue notes, sentinel precondition check."
  :type 'string
  :group 'beads-agent-ralph)

(defcustom beads-agent-ralph-prompt-file nil
  "Optional path to a file whose contents override `beads-agent-ralph-prompt'.
The path may contain the literal substring `<ROOT-ID>'; it is
substituted with the controller's root id before reading so each
loop can have its own in-tree prompt.  When the file does not
exist the defcustom value is used instead, without error -- this
makes it safe to leave configured for repos that opt in via a
checked-in `.beads/scratch/ralph/<id>.prompt.md'."
  :type '(choice (const :tag "Use defcustom only" nil) file)
  :group 'beads-agent-ralph)

;;; Hooks

(defvar beads-agent-ralph-dashboard-rerender-function nil
  "Function invoked with the controller whenever its state changes.
Set by the dashboard module (bde-9b00); nil during the skeleton.
Called with one argument, the `beads-agent-ralph--controller', and
is responsible for refreshing whatever buffer represents the loop.")

(defvar beads-agent-ralph-state-change-functions nil
  "Abnormal hook run after `status' transitions on a controller.
Each function is called with two arguments: the controller and the
new status symbol.  Used by the mode-line indicator (bde-vjfa) and
the notification module to react to terminal transitions.")

;;; Host buffer
;;
;; All async bd calls are issued from a hidden, never-killed scratch
;; buffer so the documented `beads-command--spawn-async' drop bug
;; (callbacks vanish when the caller-buffer is killed before the
;; process completes) cannot strand the controller.  The dashboard
;; buffer is short-lived from the controller's perspective.

(defconst beads-agent-ralph--host-buffer-name " *beads-agent-ralph-host*"
  "Name of the hidden buffer used to anchor async bd callbacks.
Leading-space name keeps the buffer out of `buffer-list' UI.")

(defun beads-agent-ralph--host-buffer ()
  "Return the live host buffer, creating it lazily."
  (or (get-buffer beads-agent-ralph--host-buffer-name)
      (let ((buf (get-buffer-create beads-agent-ralph--host-buffer-name)))
        (with-current-buffer buf
          (setq-local buffer-read-only t))
        buf)))

(defmacro beads-agent-ralph--in-host (&rest body)
  "Run BODY with the host buffer current, anchoring async callbacks."
  (declare (indent 0) (debug (body)))
  `(with-current-buffer (beads-agent-ralph--host-buffer)
     ,@body))

;;; Event extraction
;;
;; The stream parser hardening (bde-2qha) stores raw NDJSON events
;; on the stream.  Iteration-record fields derived from those events
;; are computed here at iteration end so the rendering task (bde-4re1)
;; can change its taxonomy without breaking the controller.

(defun beads-agent-ralph--events-in-order (stream)
  "Return STREAM's events in receive order (oldest first)."
  (reverse (oref stream events)))

(defun beads-agent-ralph--tool-uses (stream)
  "Return tool_use blocks from STREAM's assistant events as plists.
Each entry includes :name (the tool name) and :input (the tool input
as a plist).  Returned in receive order."
  (let (result)
    (dolist (event (beads-agent-ralph--events-in-order stream))
      (let ((type (plist-get event :type))
            (message (plist-get event :message)))
        (when (and (or (equal type "assistant") (eq type 'assistant))
                   (listp message))
          (let ((content (plist-get message :content)))
            (when (listp content)
              (dolist (block content)
                (when (and (listp block)
                           (let ((bt (plist-get block :type)))
                             (or (equal bt "tool_use") (eq bt 'tool_use))))
                  (push (list :name (plist-get block :name)
                              :input (plist-get block :input))
                        result))))))))
    (nreverse result)))

(defun beads-agent-ralph--assistant-text-blocks (stream)
  "Return all assistant `text' block bodies from STREAM, in order."
  (let (result)
    (dolist (event (beads-agent-ralph--events-in-order stream))
      (let ((type (plist-get event :type))
            (message (plist-get event :message)))
        (when (and (or (equal type "assistant") (eq type 'assistant))
                   (listp message))
          (let ((content (plist-get message :content)))
            (when (listp content)
              (dolist (block content)
                (when (and (listp block)
                           (let ((bt (plist-get block :type)))
                             (or (equal bt "text") (eq bt 'text))))
                  (let ((text (plist-get block :text)))
                    (when (and (stringp text)
                               (not (string-empty-p text)))
                      (push text result))))))))))
    (nreverse result)))

(defconst beads-agent-ralph--fs-edit-tools
  '("Edit" "Write" "MultiEdit" "NotebookEdit" "str_replace_editor"
    "str_replace_based_edit_tool")
  "Tool names that mutate files.
Used by `beads-agent-ralph--extract-files-touched' to classify
tool_use entries.  Conservative list; new tools fall through silently
and are picked up once added here.")

(defun beads-agent-ralph--extract-files-touched (stream)
  "Return STREAM's deduplicated list of file paths touched by the agent.
Inspect tool_use blocks whose `:name' is in
`beads-agent-ralph--fs-edit-tools' and pull the first plausible path
field (`:file_path' or `:path').  Return receive-order, dedup'd."
  (let (seen result)
    (dolist (tu (beads-agent-ralph--tool-uses stream))
      (when (member (plist-get tu :name) beads-agent-ralph--fs-edit-tools)
        (let* ((input (plist-get tu :input))
               (path (and (listp input)
                          (or (plist-get input :file_path)
                              (plist-get input :path)))))
          (when (and (stringp path) (not (member path seen)))
            (push path seen)
            (push path result)))))
    (nreverse result)))

(defconst beads-agent-ralph--bd-mutate-subcommands
  '("update" "close" "create" "dep" "comment" "remember" "forget"
    "supersede" "defer" "label" "human")
  "Bd subcommands that mutate state.
The classifier tags a Bash tool_use whose command begins with `bd
<sub>' (for one of these subs) as a bd-update.  Used by stall
detection: a stalled iter is one with neither file edits nor bd
mutations.")

(defun beads-agent-ralph--extract-bd-updates (stream)
  "Return a list of (ISSUE-ID . SUBCOMMAND) pairs observed in STREAM.
Inspects Bash tool_use commands of the form `bd <sub> [args] <id>'.
The ISSUE-ID is the first argument that looks like a beads id; if
none is found, the pair is (nil . SUBCOMMAND).  Receive-order; not
deduplicated."
  (let (result)
    (dolist (tu (beads-agent-ralph--tool-uses stream))
      (when (member (plist-get tu :name) '("Bash" "shell"))
        (let* ((input (plist-get tu :input))
               (cmd (and (listp input) (plist-get input :command))))
          (when (stringp cmd)
            (let* ((tokens (split-string cmd nil t))
                   (head (car tokens))
                   (sub (cadr tokens)))
              (when (and (member head '("bd" "beads"))
                         (member sub beads-agent-ralph--bd-mutate-subcommands))
                (let ((id (seq-find
                           (lambda (tok)
                             (string-match-p "\\`[a-z0-9]+-[a-z0-9]+\\'" tok))
                           (cddr tokens))))
                  (push (cons id sub) result))))))))
    (nreverse result)))

(defun beads-agent-ralph--extract-tool-call-count (stream)
  "Return the count of tool_use blocks observed in STREAM."
  (length (beads-agent-ralph--tool-uses stream)))

(defun beads-agent-ralph--extract-summary-tag (stream)
  "Return the agent's last <summary>...</summary> tag body from STREAM.
Scans STREAM's assistant text blocks newest-first; returns nil when no
tag is present.  Multiple summary tags resolve to the last one
emitted on the theory that the agent's final word wins."
  (let ((texts (nreverse (beads-agent-ralph--assistant-text-blocks stream)))
        (re "<summary>\\([\0-\377[:nonascii:]]*?\\)</summary>")
        found)
    (catch 'done
      (dolist (text texts)
        (when (string-match re text)
          (setq found (string-trim (match-string 1 text)))
          (throw 'done nil))))
    found))

(defconst beads-agent-ralph--sentinel-regexp
  "<promise>\\s-*COMPLETE\\s-*</promise>"
  "Pattern marking the agent's completion sentinel.
Per the plan the sentinel is `<promise>COMPLETE</promise>'; whitespace
inside the tag is tolerated.  Detection only sets `sentinel-hit'; the
controller still demands a fresh bd-show confirmation of `closed'
before treating this as termination.")

(defun beads-agent-ralph--extract-sentinel-hit (stream)
  "Return non-nil if STREAM's assistant text emitted the completion sentinel."
  (let ((re beads-agent-ralph--sentinel-regexp))
    (cl-some (lambda (text) (string-match-p re text))
             (beads-agent-ralph--assistant-text-blocks stream))))

(defun beads-agent-ralph--extract-last-text (stream)
  "Return the last non-empty assistant text block from STREAM, or nil."
  (car (last (beads-agent-ralph--assistant-text-blocks stream))))

(defun beads-agent-ralph--extract-result-fields (stream)
  "Return (:cost-usd :duration-ms) from the final `result' event of STREAM.
Returns a plist; values may be nil if the result event was not seen
or lacks the field."
  (let ((events (beads-agent-ralph--events-in-order stream))
        cost duration)
    (dolist (event events)
      (let ((type (plist-get event :type)))
        (when (or (equal type "result") (eq type 'result))
          (setq cost (or (plist-get event :total_cost_usd)
                         (plist-get event :cost_usd)
                         (plist-get event :cost-usd)))
          (setq duration (or (plist-get event :duration_ms)
                             (plist-get event :duration-ms))))))
    (list :cost-usd cost :duration-ms duration)))

;;; Async wrappers

(defun beads-agent-ralph--coerce-single-issue (result)
  "Coerce a `bd show --json' RESULT into one `beads-issue' object.
The show command emits a list when multiple ids are passed and a
single instance when only one is.  Some bd versions wrap a single
result in a one-element list anyway.  This normalises all shapes."
  (cond
   ((and (eieio-object-p result) (beads-issue-p result)) result)
   ((and (listp result) (= 1 (length result))) (car result))
   ((listp result) (car result))
   (t result)))

(defun beads-agent-ralph--bd-show-async (issue-id callback)
  "Run `bd show ISSUE-ID --json' asynchronously.
CALLBACK receives (success ISSUE-OR-ERROR).  ISSUE is a `beads-issue'
on success, or the error condition on failure."
  (beads-agent-ralph--in-host
    (beads-command-execute-async
     (beads-command-show :issue-ids (list issue-id) :json t)
     (lambda (result)
       (funcall callback t (beads-agent-ralph--coerce-single-issue result)))
     (lambda (err)
       (funcall callback nil err)))))

(defun beads-agent-ralph--bd-claim-async (issue-id callback)
  "Run `bd update ISSUE-ID --claim' asynchronously.
CALLBACK receives (success RESULT-OR-ERROR).  The claim is idempotent
on bd; an already-claimed issue still succeeds."
  (beads-agent-ralph--in-host
    (beads-command-execute-async
     (beads-command-update :issue-ids (list issue-id) :claim t)
     (lambda (result) (funcall callback t result))
     (lambda (err) (funcall callback nil err)))))

(defun beads-agent-ralph--bd-ready-children-async (parent-id callback)
  "List ready children of PARENT-ID via `bd list --parent ... --ready --json'.
CALLBACK receives (success ISSUES-OR-ERROR).  ISSUES is a list of
`beads-issue' objects, possibly empty.  Local additional filtering
\(e.g. blocker-free, status=open) is performed by the controller;
bd's `--ready' already excludes blocked issues."
  (beads-agent-ralph--in-host
    (beads-command-execute-async
     (beads-command-list :parent parent-id :ready t :status "open" :json t)
     (lambda (result)
       (funcall callback t (if (listp result) result (list result))))
     (lambda (err)
       (funcall callback nil err)))))

(defun beads-agent-ralph--bd-list-children-async (parent-id callback)
  "List ALL children of PARENT-ID via `bd list --parent ... --json'.
CALLBACK receives (success ISSUES-OR-ERROR).  ISSUES is a list of
`beads-issue' objects across all statuses, in bd's default sort
order; used to build the PLAN VIEW which surfaces both done and
remaining work to the agent.  Distinct from
`beads-agent-ralph--bd-ready-children-async', which is the epic-mode
target picker."
  (beads-agent-ralph--in-host
    (beads-command-execute-async
     (beads-command-list :parent parent-id :json t)
     (lambda (result)
       (funcall callback t
                (cond
                 ((null result) nil)
                 ((listp result) result)
                 (t (list result)))))
     (lambda (err)
       (funcall callback nil err)))))

(defun beads-agent-ralph--run-shell-async (command project-dir callback)
  "Run shell COMMAND in PROJECT-DIR asynchronously.
COMMAND is a shell string evaluated under $SHELL -c.  CALLBACK
receives a plist (:exit N :stdout S :stderr S).  Stdout and stderr
buffers are killed in the sentinel; the caller never sees them."
  (let* ((default-directory (or project-dir default-directory))
         (stdout-buf (generate-new-buffer " *beads-agent-ralph-stdout*"))
         (stderr-buf (generate-new-buffer " *beads-agent-ralph-stderr*"))
         (shell (or (getenv "SHELL") "/bin/sh")))
    (make-process
     :name "beads-agent-ralph-verify"
     :command (list shell "-c" command)
     :buffer stdout-buf
     :stderr stderr-buf
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let* ((exit (process-exit-status proc))
                (stdout (if (buffer-live-p stdout-buf)
                            (with-current-buffer stdout-buf (buffer-string))
                          ""))
                (stderr (if (buffer-live-p stderr-buf)
                            (with-current-buffer stderr-buf (buffer-string))
                          "")))
           (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
           (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))
           (funcall callback (list :exit exit
                                   :stdout stdout
                                   :stderr stderr))))))))

(defun beads-agent-ralph--run-verify-async (controller callback)
  "Run CONTROLLER's verify command (if any) asynchronously.
CALLBACK is called with a plist (:exit :stdout :stderr) or nil when
no verify command is configured.  Function-valued verify commands
are invoked as `(funcall fn controller)' and must return the plist
synchronously; the proper sandboxing of shell strings is layered on
in bde-e8lj."
  (let ((cmd (oref controller verify-command)))
    (cond
     ((null cmd) (funcall callback nil))
     ((functionp cmd)
      (let ((result (funcall cmd controller)))
        (funcall callback result)))
     ((stringp cmd)
      (beads-agent-ralph--run-shell-async
       cmd (oref controller project-dir) callback))
     (t
      (funcall callback (list :exit -1
                              :stdout ""
                              :stderr (format "invalid verify-command: %S"
                                              cmd)))))))

;;; Banner

(defconst beads-agent-ralph--banner-severity-rank
  '((info . 0) (notice . 1) (warning . 2) (error . 3))
  "Severity ranking for banner records, lowest to highest.
The dashboard surfaces the highest-ranked banner of the recent
window; lower severities still appear in the history toggle.")

(defun beads-agent-ralph--push-banner (controller severity text)
  "Push a banner record onto CONTROLLER's `banner-log'.
SEVERITY is one of `info', `notice', `warning', `error'.  TEXT is the
human-readable string.  Trims the log to `beads-agent-ralph-banner-log-max'."
  (let* ((entry (list :severity severity :text text :time (current-time)))
         (log (cons entry (oref controller banner-log))))
    (oset controller banner-log
          (if (> (length log) beads-agent-ralph-banner-log-max)
              (cl-subseq log 0 beads-agent-ralph-banner-log-max)
            log))))

;;; Detectors

(defun beads-agent-ralph--iteration-stalled-p (iter sentinel-hit)
  "Return non-nil when ITER counts as a stalled iteration.
A stall is the conjunction of: no bd-mutating tool calls, no
files-touched, and no completion sentinel.  SENTINEL-HIT comes from
the stream's parser scan and is not re-derived here."
  (and (zerop (oref iter bd-updates-count))
       (null (oref iter files-touched))
       (not sentinel-hit)))

(defun beads-agent-ralph--lying-agent-p (sentinel-hit root-status)
  "Return non-nil if SENTINEL-HIT contradicts ROOT-STATUS.
ROOT-STATUS is the freshly-queried bd status of the loop's root
issue (a string).  Sentinel-hit without `closed' = false claim."
  (and sentinel-hit
       (not (equal root-status "closed"))))

(defun beads-agent-ralph--budget-exhausted-p (controller)
  "Return non-nil when CONTROLLER has hit iteration or cost ceilings."
  (or (>= (oref controller iteration) (oref controller max-iterations))
      (when-let ((cap beads-agent-ralph-max-budget-usd))
        (>= (oref controller cumulative-cost-usd) cap))))

(defun beads-agent-ralph--effective-per-iter-budget (controller)
  "Return the per-iter `--max-budget-usd' for CONTROLLER's next spawn.
Combines the per-iter cap with the remainder of the total cap so a
nearly-spent total ratchets the per-iter cap down rather than letting
one final iter overshoot."
  (let ((per beads-agent-ralph-max-budget-usd-per-iter)
        (total beads-agent-ralph-max-budget-usd)
        (spent (oref controller cumulative-cost-usd)))
    (cond
     ((and per total)
      (max 0.0 (min per (- total spent))))
     (per per)
     (total (max 0.0 (- total spent)))
     (t nil))))

;;; Prompt rendering

(defconst beads-agent-ralph--placeholder-regexp
  "<\\(ISSUE-ID\\|ISSUE-TITLE\\|ISSUE-DESCRIPTION\\|ACCEPTANCE\\|\
ROOT-ID\\|ITERATION\\|MAX-ITERATIONS\\|SENTINEL\\|\
PLAN-VIEW\\|PRIOR-FALSE-CLAIMS\\|PRIOR-STALL-COUNT\\|\
VERIFY-TAIL\\|GIT-DIFF-STAT\\)>"
  "Regexp matching the placeholder set understood by the prompt renderer.
Tokens outside this set survive verbatim, so issue descriptions that
happen to contain HTML-looking tags are not mangled.")

(defun beads-agent-ralph--effective-template (controller)
  "Return the prompt template CONTROLLER should render this iteration.
Resolution order: the controller's `prompt-template' slot (set
explicitly at start time) wins, then `beads-agent-ralph-prompt-file'
if its contents are readable, then `beads-agent-ralph-prompt'."
  (let ((slot (oref controller prompt-template))
        (path (when beads-agent-ralph-prompt-file
                (replace-regexp-in-string
                 (regexp-quote "<ROOT-ID>")
                 (or (oref controller root-id) "")
                 beads-agent-ralph-prompt-file t t))))
    (or slot
        (and path (file-readable-p path)
             (with-temp-buffer
               (insert-file-contents path)
               (buffer-string)))
        beads-agent-ralph-prompt)))

(defun beads-agent-ralph--tail-text (text bytes)
  "Return up to BYTES trailing characters of TEXT.
Prefixes a `...' marker when the result was truncated.  Counts
characters rather than literal bytes -- Emacs strings are multibyte
and we don't need byte-exact precision for prompt context."
  (cond
   ((null text) "")
   ((or (null bytes) (<= (length text) bytes)) text)
   (t (concat "...\n" (substring text (- (length text) bytes))))))

(defun beads-agent-ralph--render-verify-tail (verify)
  "Render VERIFY (a plist :exit :stdout :stderr) for the <VERIFY-TAIL> slot.
Returns the empty string when VERIFY is nil so the prompt doesn't
include a useless nil marker on the first iteration."
  (cond
   ((null verify) "")
   ((not (listp verify)) "")
   (t
    (let ((exit (plist-get verify :exit))
          (out (plist-get verify :stdout))
          (err (plist-get verify :stderr))
          (cap beads-agent-ralph-verify-tail-bytes))
      (format "exit: %s\nstdout:\n%s\nstderr:\n%s"
              (if (numberp exit) exit "n/a")
              (beads-agent-ralph--tail-text out cap)
              (beads-agent-ralph--tail-text err cap))))))

(defun beads-agent-ralph--render-prior-false-claims (controller)
  "Return the <PRIOR-FALSE-CLAIMS> body for CONTROLLER, possibly empty.
Empty string when the counter is zero so the prompt doesn't carry a
distracting `0 times' note before the first false claim happens."
  (let ((n (oref controller false-claim-count)))
    (if (or (null n) (zerop n)) ""
      (format
       "PRIOR FALSE CLAIMS\nYou have emitted %s %d time%s while %s remains open."
       (or beads-agent-ralph-sentinel "<promise>COMPLETE</promise>")
       n (if (= n 1) "" "s")
       (or (oref controller root-id) "the root")))))

(defun beads-agent-ralph--render-prior-stall-count (controller)
  "Return the <PRIOR-STALL-COUNT> body for CONTROLLER, possibly empty.
Surfaced the iteration after a resume from `auto-paused' so the
agent learns its prior pattern.  The body emits whenever the
`consecutive-stalls' counter is non-zero, regardless of whether
the prior iter was the one that paused -- the reader is the agent,
not the controller, and a non-zero stall counter is signal worth
showing."
  (let ((n (oref controller consecutive-stalls)))
    (if (or (null n) (zerop n)) ""
      (format
       "PRIOR STALL COUNT\nYou were auto-paused after %d consecutive iteration%s with no bd update, no file edit, and no completion sentinel."
       n (if (= n 1) "" "s")))))

(defun beads-agent-ralph--iteration-number-for-issue (controller target-id)
  "Return the iteration number whose record targeted TARGET-ID, or nil.
Walks CONTROLLER's `history' newest-first; returns the iter number of
the first match, computed from list position so it survives history
trimming.  Used to annotate PLAN VIEW rows with the agent's own
prior touch."
  (let* ((history (oref controller history))
         (len (length history))
         (i 0)
         hit)
    (catch 'done
      (dolist (entry history)
        (when (equal (oref entry issue-id) target-id)
          (setq hit (- len i))
          (throw 'done nil))
        (cl-incf i)))
    hit))

(defun beads-agent-ralph--summary-for-issue (controller target-id)
  "Return the most recent iter `summary' for TARGET-ID, or nil.
Reads CONTROLLER's `history' newest-first and returns the first
match.  Used to annotate PLAN VIEW rows."
  (catch 'done
    (dolist (entry (oref controller history))
      (when (equal (oref entry issue-id) target-id)
        (throw 'done (oref entry summary))))))

(defun beads-agent-ralph--row-blocked-by (row plan-issues)
  "Return a list of open-dependency ids for ROW given PLAN-ISSUES.
ROW is a `beads-issue'; PLAN-ISSUES is the same list of issues used
to build PLAN VIEW.  An open dep is one present in PLAN-ISSUES whose
status is not `closed'.  Returns nil when no dependencies are
populated -- bd list may or may not expand them depending on
schema."
  (let ((deps (and (eieio-object-p row) (oref row dependencies)))
        (open-ids nil))
    (when (listp deps)
      (dolist (dep deps)
        (let ((dep-id (and (eieio-object-p dep)
                           (oref dep depends-on-id))))
          (when dep-id
            (let ((target (cl-find dep-id plan-issues
                                   :key (lambda (i) (oref i id))
                                   :test #'equal)))
              (when (and target
                         (not (equal (oref target status) "closed")))
                (push dep-id open-ids)))))))
    (nreverse open-ids)))

(defun beads-agent-ralph--render-plan-row (controller row plan-issues current-id)
  "Render one PLAN VIEW row for ROW under CONTROLLER.
PLAN-ISSUES is the full list (used to detect open blockers).
CURRENT-ID is the id of this iteration's target so we can mark it
with the arrow annotation."
  (let* ((id (oref row id))
         (title (or (oref row title) ""))
         (status (or (oref row status) "open"))
         (closed (equal status "closed"))
         (blocked-by (beads-agent-ralph--row-blocked-by row plan-issues))
         (suffix (cond
                  ((equal id current-id) " <- current")
                  (blocked-by
                   (format " (blocked by %s)"
                           (mapconcat #'identity blocked-by ", ")))
                  (t "")))
         (iter-n (beads-agent-ralph--iteration-number-for-issue
                  controller id))
         (summary (and iter-n
                       (beads-agent-ralph--summary-for-issue controller id)))
         (line1 (format "- [%s] %s %s (%s)%s"
                        (if closed "x" " ")
                        id title status suffix)))
    (if (and iter-n summary (not (string-empty-p summary)))
        (concat line1
                (format "\n      last touched by iter %d: %S"
                        iter-n summary))
      line1)))

(defun beads-agent-ralph--render-plan-view (controller plan-issues current-id)
  "Render the <PLAN-VIEW> body for CONTROLLER.
PLAN-ISSUES is the list of `beads-issue' children of the root (may
be nil or contain only the root for a single-issue loop).  CURRENT-ID
is the id of this iteration's target so its row is marked with the
arrow annotation.  Empty PLAN-ISSUES renders a one-line placeholder
so the agent still sees a deterministic block."
  (cond
   ((null plan-issues)
    (format "- [ ] %s (no plan-view children available)"
            (or current-id (oref controller root-id) "?")))
   (t
    (mapconcat
     (lambda (row)
       (beads-agent-ralph--render-plan-row
        controller row plan-issues current-id))
     plan-issues
     "\n"))))

(defun beads-agent-ralph--prompt-substitutions
    (controller issue plan-issues)
  "Build the placeholder->replacement alist for the renderer.
CONTROLLER provides loop-level state.  ISSUE is the current target
`beads-issue' (may be nil if the bd show step failed).  PLAN-ISSUES
is the list of children rendered into <PLAN-VIEW>.  Values are plain
strings; literal substitution applies."
  (let* ((current-id (or (and issue (oref issue id))
                         (oref controller current-issue-id)
                         (oref controller root-id))))
    (list
     (cons "ISSUE-ID"          (or current-id ""))
     (cons "ISSUE-TITLE"       (or (and issue (oref issue title)) ""))
     (cons "ISSUE-DESCRIPTION" (or (and issue (oref issue description)) ""))
     (cons "ACCEPTANCE"        (or (and issue (oref issue acceptance-criteria))
                                   ""))
     (cons "ROOT-ID"           (or (oref controller root-id) ""))
     (cons "ITERATION"
           (number-to-string (or (oref controller iteration) 0)))
     (cons "MAX-ITERATIONS"
           (number-to-string (or (oref controller max-iterations) 0)))
     (cons "SENTINEL"
           (or beads-agent-ralph-sentinel "<promise>COMPLETE</promise>"))
     (cons "PLAN-VIEW"
           (beads-agent-ralph--render-plan-view
            controller plan-issues current-id))
     (cons "PRIOR-FALSE-CLAIMS"
           (beads-agent-ralph--render-prior-false-claims controller))
     (cons "PRIOR-STALL-COUNT"
           (beads-agent-ralph--render-prior-stall-count controller))
     (cons "VERIFY-TAIL"
           (beads-agent-ralph--render-verify-tail
            (oref controller last-verify)))
     (cons "GIT-DIFF-STAT"
           (or (oref controller last-git-diff-stat) "")))))

(defun beads-agent-ralph--apply-substitutions (template substitutions)
  "Apply SUBSTITUTIONS (an alist) to TEMPLATE.
Replaces each `<TAG>' (per `beads-agent-ralph--placeholder-regexp')
with the literal value from SUBSTITUTIONS.  Replacement is single-
pass and literal, so issue text that contains `<ISSUE-ID>'-like
strings is NOT re-expanded.  Unknown placeholders -- caught by the
regexp but absent from the alist -- collapse to the empty string;
non-placeholder angle-bracket runs are left alone."
  (replace-regexp-in-string
   beads-agent-ralph--placeholder-regexp
   (lambda (match)
     (let* ((tag (match-string 1 match))
            (cell (assoc tag substitutions)))
       (if cell (or (cdr cell) "") "")))
   template t t))

(defun beads-agent-ralph--render-prompt (controller issue plan-issues)
  "Render CONTROLLER's prompt template for ISSUE with PLAN-ISSUES.

ISSUE is the current target `beads-issue' object (full result from
`bd show ID --json'); when nil the placeholder fields for the issue
collapse to the empty string.  PLAN-ISSUES is the list of
`beads-issue' children of `root-id' used for PLAN VIEW; nil renders
a single placeholder row.

Placeholders supported (literal, single-pass substitution):

  <ISSUE-ID>, <ISSUE-TITLE>, <ISSUE-DESCRIPTION>, <ACCEPTANCE>
  <ROOT-ID>, <ITERATION>, <MAX-ITERATIONS>, <SENTINEL>
  <PLAN-VIEW>, <PRIOR-FALSE-CLAIMS>, <PRIOR-STALL-COUNT>
  <VERIFY-TAIL>, <GIT-DIFF-STAT>

Returns the rendered prompt as a single string."
  (let ((template (beads-agent-ralph--effective-template controller))
        (subs (beads-agent-ralph--prompt-substitutions
               controller issue plan-issues)))
    (beads-agent-ralph--apply-substitutions template subs)))

;;; Stub dashboard

(defun beads-agent-ralph--stub-dashboard-name (root-id)
  "Return the buffer name for ROOT-ID's stub dashboard."
  (format "*beads-agent-ralph: %s*" root-id))

(defun beads-agent-ralph--ensure-stub-dashboard (controller)
  "Create a minimal dashboard buffer for CONTROLLER if missing.
The buffer is a placeholder for the real vui dashboard (bde-9b00):
it shows controller status, iteration count, cumulative cost, and a
one-line summary per finished iteration.  Returns the buffer."
  (let* ((root (oref controller root-id))
         (name (beads-agent-ralph--stub-dashboard-name root))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Ralph loop for %s [%s]\n"
                        root (oref controller status))))
      (setq-local buffer-read-only t))
    buf))

(defun beads-agent-ralph--stub-dashboard-update (controller)
  "Refresh the stub dashboard buffer for CONTROLLER, if alive.
Renders header + iteration table compactly so users still see
progress before the real dashboard (bde-9b00) lands."
  (let ((buf (get-buffer
              (beads-agent-ralph--stub-dashboard-name
               (oref controller root-id)))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (point-was (point)))
          (erase-buffer)
          (insert (format "Ralph loop: %s\n" (oref controller root-id)))
          (insert (format "Status: %s%s\n"
                          (oref controller status)
                          (if (oref controller done-reason)
                              (format " (%s)" (oref controller done-reason))
                            "")))
          (insert (format "Iteration: %d / %d   Cost: $%.4f\n"
                          (oref controller iteration)
                          (oref controller max-iterations)
                          (oref controller cumulative-cost-usd)))
          (insert (format "Stalls: %d   False claims: %d\n\n"
                          (oref controller consecutive-stalls)
                          (oref controller false-claim-count)))
          (insert "Iterations (newest first):\n")
          (dolist (iter (oref controller history))
            (insert (format "  #%-3s %-12s $%-7s %s\n"
                            (or (oref iter issue-id) "?")
                            (or (oref iter status) "?")
                            (if (oref iter cost-usd)
                                (format "%.4f" (oref iter cost-usd))
                              "n/a")
                            (or (oref iter summary) ""))))
          (insert "\n")
          (when (oref controller banner-log)
            (insert "Recent banners:\n")
            (dolist (entry (seq-take (oref controller banner-log) 5))
              (insert (format "  [%s] %s\n"
                              (plist-get entry :severity)
                              (plist-get entry :text)))))
          (goto-char (min point-was (point-max))))))))

(defun beads-agent-ralph--dashboard-rerender (controller)
  "Refresh whatever view is bound to CONTROLLER.
Calls `beads-agent-ralph-dashboard-rerender-function' when set, then
the stub dashboard.  Both run inside a `condition-case' so a broken
renderer cannot break the controller."
  (when beads-agent-ralph-dashboard-rerender-function
    (condition-case err
        (funcall beads-agent-ralph-dashboard-rerender-function controller)
      (error
       (message "beads-agent-ralph: dashboard rerender errored: %S" err))))
  (condition-case err
      (beads-agent-ralph--stub-dashboard-update controller)
    (error
     (message "beads-agent-ralph: stub dashboard errored: %S" err))))

;;; State change

(defun beads-agent-ralph--set-status (controller new-status)
  "Set CONTROLLER's `status' to NEW-STATUS and run change hooks.
Idempotent: a transition to the current status is a no-op.  Always
follows with a dashboard re-render so observers see the new state."
  (unless (eq (oref controller status) new-status)
    (oset controller status new-status)
    (condition-case err
        (run-hook-with-args 'beads-agent-ralph-state-change-functions
                            controller new-status)
      (error
       (message "beads-agent-ralph: state-change hook errored: %S" err)))
    (beads-agent-ralph--dashboard-rerender controller)))

;;; Iteration sequence

(defun beads-agent-ralph--build-iteration (controller stream)
  "Construct a `beads-agent-ralph--iteration' from CONTROLLER + STREAM.
Captures derived fields (files-touched, bd-updates count, tool calls,
summary, cost, duration, stderr-tail, verify-result) so the dashboard
need only read this record to render its row."
  (let* ((result-fields (beads-agent-ralph--extract-result-fields stream))
         (cost (plist-get result-fields :cost-usd))
         (duration (plist-get result-fields :duration-ms))
         (files (beads-agent-ralph--extract-files-touched stream))
         (bd-updates (beads-agent-ralph--extract-bd-updates stream))
         (tool-count (beads-agent-ralph--extract-tool-call-count stream))
         (summary-tag (beads-agent-ralph--extract-summary-tag stream))
         (last-text (beads-agent-ralph--extract-last-text stream))
         (sentinel-hit (beads-agent-ralph--extract-sentinel-hit stream))
         (raw-summary (or summary-tag last-text ""))
         (summary
          (if (> (length raw-summary) beads-agent-ralph-summary-max-len)
              (concat (substring raw-summary 0
                                 beads-agent-ralph-summary-max-len)
                      "…")
            raw-summary))
         (status (pcase (oref stream status)
                   ('finished 'finished)
                   ('failed   'failed)
                   ('stopped  'stopped)
                   (_         'failed))))
    (beads-agent-ralph--iteration
     :issue-id (oref controller current-issue-id)
     :status status
     :duration-ms duration
     :cost-usd cost
     :files-touched files
     :tool-call-count tool-count
     :bd-updates-count (length bd-updates)
     :summary summary
     :sentinel-hit sentinel-hit
     :stderr-tail (oref stream stderr-tail)
     :verify-result (oref stream verify-result))))

(defun beads-agent-ralph--terminate (controller reason)
  "Terminate CONTROLLER's loop with REASON (a symbol).
Sets `done-reason' and drives status through the appropriate
terminal value; clears `current-stream' but leaves `history' intact
so the dashboard remains useful after stop.  Idempotent: a second
call with the same reason is a no-op."
  (when (memq (oref controller status) '(running cooling-down))
    (oset controller done-reason reason)
    (oset controller current-stream nil)
    (beads-agent-ralph--set-status
     controller
     (pcase reason
       ('stop    'stopped)
       ('failed  'failed)
       (_        'done)))))

(defun beads-agent-ralph--pause (controller reason)
  "Transition CONTROLLER to `auto-paused' with banner REASON (a string).
Auto-pause is recoverable: state, counter, and cumulative cost are
preserved.  The resume command (bde-9b00) un-pauses without rebuilding."
  (oset controller current-stream nil)
  (beads-agent-ralph--push-banner controller 'warning reason)
  (beads-agent-ralph--set-status controller 'auto-paused))

(defun beads-agent-ralph--continue-after-iteration (controller)
  "Schedule the next iteration body on CONTROLLER unless terminating.
Increments `iteration' before scheduling so dashboard rows align.
Honours `iteration-delay' even when zero so the call stack is broken
between iterations."
  (cond
   ((beads-agent-ralph--budget-exhausted-p controller)
    (beads-agent-ralph--terminate controller 'budget))
   (t
    (beads-agent-ralph--set-status controller 'cooling-down)
    (beads-agent-ralph--schedule-next-iteration
     controller
     (lambda ()
       (beads-agent-ralph--set-status controller 'running)
       (beads-agent-ralph--run-iteration controller))))))

(defun beads-agent-ralph--on-stream-finish (controller stream)
  "Handle CONTROLLER's iteration completion when STREAM reaches terminal.
Steps:
1. run verify command (async) if configured;
2. capture acceptance-after and root status via bd show (async);
3. build the iteration record and push it to history;
4. run stall / lying-agent / closed / failed / budget detectors;
5. dispatch to terminate, pause, or schedule next iteration."
  (beads-agent-ralph--run-verify-async
   controller
   (lambda (verify-result)
     (when verify-result
       (oset stream verify-result verify-result))
     (beads-agent-ralph--bd-show-async
      (oref controller root-id)
      (lambda (root-ok root-result)
        (let* ((root-status
                (when root-ok
                  (oref (beads-agent-ralph--coerce-single-issue root-result)
                        status)))
               (iter (beads-agent-ralph--build-iteration controller stream))
               (sentinel-hit (oref iter sentinel-hit))
               (stalled (beads-agent-ralph--iteration-stalled-p
                         iter sentinel-hit))
               (lying (beads-agent-ralph--lying-agent-p
                       sentinel-hit root-status)))
               ;; Apply derived per-iter state.
               (when (oref iter cost-usd)
                 (oset controller cumulative-cost-usd
                       (+ (oref controller cumulative-cost-usd)
                          (oref iter cost-usd))))
               (oset controller last-verify (oref iter verify-result))
               (push iter (oref controller history))
               (oset controller current-stream nil)
               (if stalled
                   (cl-incf (oref controller consecutive-stalls))
                 (oset controller consecutive-stalls 0))
               (when lying
                 (cl-incf (oref controller false-claim-count))
                 (beads-agent-ralph--push-banner
                  controller 'warning
                  (format "Agent claimed complete %d×; root still open"
                          (oref controller false-claim-count))))
               (let* ((root-closed (equal root-status "closed"))
                      (kind (oref controller root-kind))
                      (stream-failed (eq (oref stream status) 'failed))
                      (paused (and stalled
                                   (>= (oref controller consecutive-stalls)
                                       beads-agent-ralph-stall-threshold))))
                 (cond
                  ;; Stall cap → auto-pause; counter+cost preserved.
                  (paused
                   (beads-agent-ralph--pause
                    controller
                    (format "Stalled %d iters in a row"
                            (oref controller consecutive-stalls))))
                  ;; Single-issue loop ends when the root closes.
                  ((and (eq kind 'issue) root-closed)
                   (beads-agent-ralph--terminate controller 'closed))
                  ;; Failed iter with stop-on-failed honours the cap.
                  ((and stream-failed beads-agent-ralph-stop-on-failed)
                   (beads-agent-ralph--terminate controller 'failed))
                  ;; Epic loop: a child closed (or didn't) — advance.
                  ;; The next resolve-target step emits `epic-empty'
                  ;; when no more ready children remain.
                  ((eq kind 'epic)
                   (beads-agent-ralph--continue-after-iteration controller))
                  ;; Lying-agent or normal continuation: keep going.
                  (t
                   (beads-agent-ralph--continue-after-iteration
                    controller))))
               (beads-agent-ralph--dashboard-rerender controller)))))))

(defun beads-agent-ralph--make-stream-subscriber (controller)
  "Return a stream subscriber closure bound to CONTROLLER.
The closure is registered with the spawned stream; it forwards live
events to the dashboard renderer and, on the first terminal status
transition, schedules `beads-agent-ralph--on-stream-finish' via
`run-at-time' so the sentinel returns immediately."
  (let ((handled nil))
    (lambda (stream)
      (cond
       ((and (not handled)
             (memq (oref stream status) '(finished failed stopped)))
        (setq handled t)
        (run-at-time
         0 nil
         (lambda ()
           (beads-agent-ralph--on-stream-finish controller stream))))
       (t
        (beads-agent-ralph--dashboard-rerender controller))))))

(defun beads-agent-ralph--spawn-stream-for (controller issue-id rendered-prompt)
  "Spawn a stream on CONTROLLER for ISSUE-ID with RENDERED-PROMPT.
Plumbs the cost guard (`--max-budget-usd'), turn cap (`--max-turns'),
permission mode, and worktree (`--add-dir').  Subscribes a fresh
controller-bound subscriber so iteration completion is observed."
  (let* ((per-iter-budget
          (beads-agent-ralph--effective-per-iter-budget controller))
         (stream (beads-agent-ralph--stream-spawn
                  :prompt rendered-prompt
                  :project-dir (or (oref controller worktree-dir)
                                   (oref controller project-dir)
                                   default-directory)
                  :permission-mode (oref controller permission-mode)
                  :max-budget-usd per-iter-budget
                  :max-turns beads-agent-ralph-max-turns)))
    (oset controller current-stream stream)
    (oset controller current-issue-id issue-id)
    (beads-agent-ralph--stream-subscribe
     stream
     'controller
     (beads-agent-ralph--make-stream-subscriber controller))
    (beads-agent-ralph--dashboard-rerender controller)
    stream))

(defun beads-agent-ralph--run-iteration (controller)
  "Run one iteration body for CONTROLLER.
Resolves the target issue (issue or epic mode), claims it, snapshots
acceptance-before, renders the prompt, and spawns the stream.
Stream completion is handled by the subscriber installed in
`beads-agent-ralph--spawn-stream-for'."
  (beads-agent-ralph--set-status controller 'running)
  (beads-agent-ralph--dashboard-rerender controller)
  (beads-agent-ralph--then
   :cancelled (lambda ()
                (memq (oref controller status)
                      '(stopped done failed auto-paused)))
   :on-error
   (lambda (err _idx)
     (cond
      ((eq err 'epic-empty)
       (beads-agent-ralph--terminate controller 'epic-empty))
      ((eq err 'cancelled) nil)
      (t
       (beads-agent-ralph--push-banner
        controller 'error
        (format "Iteration setup failed: %S" err))
       (beads-agent-ralph--terminate controller 'failed))))
   :steps
   (list
    ;; Step 1: resolve target issue.
    (lambda (_acc k)
      (let ((kind (oref controller root-kind)))
        (pcase kind
          ('issue
           (let ((id (oref controller root-id)))
             (oset controller current-issue-id id)
             (funcall k nil (list :issue-id id))))
          ('epic
           (beads-agent-ralph--bd-ready-children-async
            (oref controller root-id)
            (lambda (ok result)
              (cond
               ((not ok) (funcall k 'bd-list-failed nil))
               ((null result) (funcall k 'epic-empty nil))
               (t
                (let ((first-id (oref (car result) id)))
                  (oset controller current-issue-id first-id)
                  (funcall k nil (list :issue-id first-id))))))))
          (_ (funcall k 'unknown-root-kind nil)))))
    ;; Step 2: claim.
    (lambda (acc k)
      (beads-agent-ralph--bd-claim-async
       (plist-get acc :issue-id)
       (lambda (ok _result)
         (if ok (funcall k nil nil)
           ;; Claim failure is non-fatal: bd update --claim on an
           ;; already-claimed-by-someone-else issue is rare but
           ;; recoverable.  Log a banner and continue.
           (beads-agent-ralph--push-banner
            controller 'notice
            (format "Claim failed for %s; continuing"
                    (plist-get acc :issue-id)))
           (funcall k nil nil)))))
    ;; Step 3: fetch the current target issue (full record).
    (lambda (acc k)
      (beads-agent-ralph--bd-show-async
       (plist-get acc :issue-id)
       (lambda (ok result)
         (if (not ok) (funcall k nil (list :issue nil
                                           :acceptance-before nil))
           (let ((issue (beads-agent-ralph--coerce-single-issue result)))
             (funcall k nil
                      (list :issue issue
                            :acceptance-before
                            (and issue
                                 (oref issue acceptance-criteria)))))))))
    ;; Step 3b: fetch plan-view children of root (best-effort).
    (lambda (_acc k)
      (beads-agent-ralph--bd-list-children-async
       (oref controller root-id)
       (lambda (ok result)
         ;; Non-fatal: an empty plan-view still renders.
         (funcall k nil (list :plan-view (if ok result nil))))))
    ;; Step 4: render prompt + spawn.
    (lambda (acc k)
      (condition-case err
          (let* ((id (plist-get acc :issue-id))
                 (issue (plist-get acc :issue))
                 (plan-view (plist-get acc :plan-view))
                 (prompt (beads-agent-ralph--render-prompt
                          controller issue plan-view)))
            (beads-agent-ralph--spawn-stream-for controller id prompt)
            (funcall k nil nil))
        (error
         (funcall k (or (car-safe err) 'spawn-failed) nil)))))))

;;; Entry points

;;;###autoload
(defun beads-agent-ralph-start (&rest args)
  "Start a Ralph loop for an issue or epic.
ARGS is a plist:

  :issue       ISSUE-ID-STRING-OR-OBJECT — required.  When an object
               is supplied, only its `id' is used; iterations re-fetch
               state from bd to stay live.
  :prompt      STRING — optional override for the iteration template.
               When omitted the renderer reads from
               `beads-agent-ralph-prompt' (or the file pointed at by
               `beads-agent-ralph-prompt-file').  Supports the
               placeholders documented in `beads-agent-ralph--render-prompt'.
  :kind        `issue' or `epic'.  Defaults to `issue'.
  :project-dir STRING — bd repo root; defaults to `default-directory'.
  :worktree-dir STRING — if a worktree is in use, the agent works here
               instead of `:project-dir'.
  :verify-command STRING-OR-FUNCTION — optional per-iter verification.
  :max-iterations INTEGER — defaults to the slot default (50).
  :iteration-delay NUMBER — seconds between iterations.

Returns the controller object (also stored on the global state so
the backend can pick it up).  The first iteration is dispatched via
`run-at-time' so this function returns immediately."
  (let* ((issue (plist-get args :issue))
         (issue-id (cond ((stringp issue) issue)
                         ((and issue (eieio-object-p issue)
                               (beads-issue-p issue))
                          (oref issue id))
                         (t (error "beads-agent-ralph-start: :issue required"))))
         (prompt (plist-get args :prompt))
         (kind (or (plist-get args :kind) 'issue))
         (project-dir (or (plist-get args :project-dir) default-directory))
         (worktree-dir (plist-get args :worktree-dir))
         (verify-command (plist-get args :verify-command))
         (max-iterations (or (plist-get args :max-iterations) 50))
         (iteration-delay (or (plist-get args :iteration-delay) 0.0))
         (controller
          (beads-agent-ralph--controller
           :root-id issue-id
           :root-kind kind
           :project-dir project-dir
           :worktree-dir worktree-dir
           :verify-command verify-command
           :prompt-template prompt
           :max-iterations max-iterations
           :iteration-delay iteration-delay
           :status 'idle)))
    (beads-agent-ralph--ensure-stub-dashboard controller)
    (beads-agent-ralph--set-status controller 'running)
    (beads-agent-ralph--schedule-next-iteration
     controller
     (lambda () (beads-agent-ralph--run-iteration controller)))
    (cons controller
          (get-buffer
           (beads-agent-ralph--stub-dashboard-name issue-id)))))

;;;###autoload
(defun beads-agent-ralph-stop (controller)
  "Stop CONTROLLER's loop after the current iteration's stream is killed.
Signals the in-flight stream with SIGINT (escalating to SIGKILL after
`beads-agent-ralph-stop-grace-ms') and transitions the controller to
`stopped'.  The currently-in-flight iteration is allowed to register
its iteration record via the existing sentinel path."
  (let ((stream (oref controller current-stream)))
    (oset controller done-reason 'stop)
    (when stream
      (beads-agent-ralph--stream-stop stream))
    (unless stream
      (beads-agent-ralph--set-status controller 'stopped))
    controller))

;;;###autoload
(defun beads-agent-ralph-show-history (controller)
  "Pop to CONTROLLER's dashboard buffer.
Skeleton implementation: the stub dashboard already shows the
iteration log.  The persistence-replay version (bde-1p2c) replaces
this body to read JSONL off disk and replay per-iter NDJSON files."
  (interactive)
  (let ((buf (or (get-buffer
                  (beads-agent-ralph--stub-dashboard-name
                   (oref controller root-id)))
                 (beads-agent-ralph--ensure-stub-dashboard controller))))
    (pop-to-buffer buf)))

(provide 'beads-agent-ralph)

;;; beads-agent-ralph.el ends here
