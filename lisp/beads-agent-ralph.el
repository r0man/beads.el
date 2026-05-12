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

(require 'beads-agent-ralph-stream)

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

(provide 'beads-agent-ralph)

;;; beads-agent-ralph.el ends here
