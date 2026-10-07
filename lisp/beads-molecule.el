;;; beads-molecule.el --- Molecule execution view for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `beads-molecule' is the standalone execution view for a molecule (a
;; `bd mol' instance): the root header, its progress, the step DAG with
;; per-step state, the gates attached to the molecule, and the closed
;; output.  It is the "heart" of the standalone-first formula ->
;; molecule workflow (WI-SF-01, requirements REQ-SF-020, REQ-SF-021,
;; REQ-SF-024 (render), REQ-SF-033 (render)).
;;
;; Data acquisition is asynchronous and read-only.  A single aggregate
;; loader fans out `bd mol current', `bd mol progress', `bd mol show'
;; and the molecule's gates through `beads-command-execute-async' with a
;; per-molecule `:cache-key', so opening a view never blocks Emacs and
;; concurrent opens coalesce onto one bd subprocess per command.  Each
;; rendered section is wrapped in `vui-error-boundary' so one failing
;; branch cannot blank the whole view.
;;
;; Rendering is a `vui' component mounted into a `beads-section-mode'
;; derived major mode.  Step rows carry a `beads-thing' property, so
;; TAB/S-TAB movement and SPC folding are identical to every other
;; beads.el view (see `beads-thing').
;;
;; The deliberate seams (design.md section 5.2) are
;; `beads-molecule-step-state', `beads-molecule-step-face',
;; `beads-molecule-sections', `beads-molecule-action' and
;; `beads-molecule-section-functions'.  Downstream packages (Gas City)
;; derive extra state glyphs and append a City/rig section without
;; reimplementing any of the parsing.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'vui)
(require 'beads-command)
(require 'beads-command-mol)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-section)
(require 'beads-thing)
(require 'beads-faces)
(require 'beads-buffer)
(require 'beads-util)
(require 'beads-prefix)

(declare-function beads-show "beads-command-show")
(declare-function beads-dispatch "beads")

;;; Glyphs

(defconst beads-molecule-step-glyphs
  '((done . "✓")
    (current . "◐")
    (ready . "○")
    (blocked . "·")
    (pending . "·")
    (blocked-gate . "⊘"))
  "Glyph rendered for each molecule step state.
A fresh molecule renders `ready' as an open circle, `current' as a
half-filled circle, `done' as a check, and `blocked-gate' as a slashed
circle so a gated step stands out from an ordinary blocked step.")

(defconst beads-molecule-phase-glyph "◆"
  "Glyph preceding the molecule phase in the header.")

(defconst beads-molecule-gate-glyph "⚙"
  "Glyph preceding a gate in the gates section.")

;;; Faces

(defface beads-molecule-step-done
  '((t :inherit beads-face-status-closed))
  "Face for a completed molecule step."
  :group 'beads)

(defface beads-molecule-step-current
  '((t :inherit beads-face-status-in-progress))
  "Face for the current (in-progress) molecule step."
  :group 'beads)

(defface beads-molecule-step-ready
  '((t :inherit beads-face-status-open))
  "Face for a ready molecule step."
  :group 'beads)

(defface beads-molecule-step-blocked
  '((t :inherit beads-face-status-blocked))
  "Face for a blocked molecule step."
  :group 'beads)

(defface beads-molecule-step-pending
  '((t :inherit shadow))
  "Face for a molecule step that is waiting its turn."
  :group 'beads)

(defface beads-molecule-step-blocked-gate
  '((t :inherit beads-face-warning))
  "Face for a molecule step blocked by an open gate."
  :group 'beads)

(defface beads-molecule-header
  '((t :inherit beads-face-header))
  "Face for the molecule view header."
  :group 'beads)

;;; Buffer-local state

(defvar-local beads-molecule--root nil
  "Root molecule ID this buffer is displaying, captured at open.")

(defvar-local beads-molecule--project-root nil
  "Project root this molecule view was opened for, captured at open.")

(defvar-local beads-molecule--collapsed nil
  "Alist mapping section key to t when the section is folded.")

(defvar-local beads-molecule--last-good-data nil
  "Hash table mapping the async cache key to the last good aggregate payload.
Powers stale-while-revalidate rendering during a soft refresh so the
step tree does not flash back to a loading skeleton while new data lands.")

(defvar-local beads-molecule--model nil
  "The render model built for the current buffer, or nil.
Set after each successful aggregate load so point commands can resolve
the step under point without re-fetching.")

(defcustom beads-molecule-range-size 50
  "Number of steps rendered per `]'/`[' window in the molecule view."
  :type 'integer
  :group 'beads)

;;; JSON helpers

(defun beads-molecule--as-list (value)
  "Return VALUE coerced to a list.
JSON arrays parsed by `beads-command-parse' are vectors; a single
object is returned as a one-element list; nil stays nil."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun beads-molecule--aget (alist key)
  "Return the value of KEY in ALIST, or nil.
ALIST uses symbol keys, as produced by the command JSON parser."
  (cdr (assq key alist)))

(defun beads-molecule--json-bool (value)
  "Return VALUE as an Elisp boolean.
The command JSON parser leaves JSON `false' as the truthy symbol
`:json-false', so every boolean read back from a payload goes through
here rather than a bare `plist-get'."
  (and value (not (eq value :json-false))))

(defun beads-molecule--object-field (object slot)
  "Return SLOT from OBJECT.
OBJECT is either an EIEIO instance or a JSON alist with symbol keys.
Returns nil for an unbound or absent slot."
  (cond
   ((eieio-object-p object)
    ;; `eieio-oref' (not the `oref' macro) so SLOT can be dynamic.
    (and (slot-boundp object slot) (eieio-oref object slot)))
   ((consp object) (beads-molecule--aget object slot))
   (t nil)))

;;; Normalisation

(defun beads-molecule--normalize-step (raw)
  "Return a canonical step plist from a `bd mol current' RAW step.
RAW is the per-step alist `{issue, status, is_current}'."
  (let ((issue (beads-molecule--aget raw 'issue)))
    (list :id (beads-molecule--aget issue 'id)
          :title (beads-molecule--aget issue 'title)
          :status (beads-molecule--aget raw 'status)
          :is-current (beads-molecule--json-bool
                       (beads-molecule--aget raw 'is_current))
          :issue-type (beads-molecule--aget issue 'issue_type)
          :issue-status (beads-molecule--aget issue 'status)
          :assignee (beads-molecule--aget issue 'assignee)
          :ephemeral (beads-molecule--json-bool
                      (beads-molecule--aget issue 'ephemeral))
          :closed-at (beads-molecule--aget issue 'closed_at)
          :close-reason (beads-molecule--aget issue 'close_reason)
          :needs nil
          :blocked nil
          :gate nil)))

(defun beads-molecule--normalize-current (raw)
  "Return a canonical molecule plist from `bd mol current' RAW JSON.
RAW is the one-element array `bd mol current --json' emits (or its
first element).  Returns nil when no molecule is present."
  (let ((entry (car (beads-molecule--as-list raw))))
    (when entry
      (list :id (beads-molecule--aget entry 'molecule_id)
            :title (beads-molecule--aget entry 'molecule_title)
            :steps (mapcar #'beads-molecule--normalize-step
                           (beads-molecule--as-list
                            (beads-molecule--aget entry 'steps)))
            :completed (or (beads-molecule--aget entry 'completed) 0)
            :total (or (beads-molecule--aget entry 'total) 0)))))

(defun beads-molecule--normalize-progress (raw)
  "Return a canonical progress plist from `bd mol progress' RAW JSON.
Returns nil when RAW is nil.  Rate and ETA keys are read when the
running bd emits them and left nil otherwise, so the header degrades to
`rate — · ETA —' rather than inventing numbers."
  (when (consp raw)
    (list :completed (beads-molecule--aget raw 'completed)
          :total (beads-molecule--aget raw 'total)
          :percent (beads-molecule--aget raw 'percent)
          :current-step-id (beads-molecule--aget raw 'current_step_id)
          :in-progress (beads-molecule--aget raw 'in_progress)
          :rate (or (beads-molecule--aget raw 'rate)
                    (beads-molecule--aget raw 'rate_per_hour))
          :eta (or (beads-molecule--aget raw 'eta)
                   (beads-molecule--aget raw 'eta_seconds)))))

(defun beads-molecule--normalize-show (raw)
  "Return a canonical structure plist from `bd mol show' RAW JSON.
RAW is the object `bd mol show --json' emits.  Returns nil when RAW is
nil."
  (when (consp raw)
    (list :root (beads-molecule--aget raw 'root)
          :variables (beads-molecule--as-list (beads-molecule--aget raw 'variables))
          :issues (beads-molecule--as-list (beads-molecule--aget raw 'issues))
          :dependencies (beads-molecule--as-list
                         (beads-molecule--aget raw 'dependencies)))))

(defun beads-molecule--dependency-type (dep)
  "Return the dependency type string for DEP, or nil."
  (or (beads-molecule--object-field dep 'type)
      (beads-molecule--object-field dep 'dependency_type)))

(defun beads-molecule--dependency-target (dep)
  "Return the issue id DEP points at, or nil.
For a `blocks' edge from `bd dep list --direction up' / `bd show
--include-dependents' the dependent issue is carried in `id' (and
mirrored into `depends-on-id' by `beads-dependency-from-json')."
  (or (beads-molecule--object-field dep 'id)
      (beads-molecule--object-field dep 'depends-on-id)))

(defun beads-molecule--normalize-gates (shown)
  "Return canonical gate plists from SHOWN.
SHOWN is the result of `bd show --include-dependents', one `beads-issue'
or a list of them.  Each returned plist is
`(:id :title :status :await-type :blocked)' where `:blocked' is the
list of issue ids the gate blocks."
  (mapcar
   (lambda (gate)
     (let* ((deps (beads-molecule--as-list
                   (beads-molecule--object-field gate 'dependents)))
            (blocked (delq nil
                           (mapcar (lambda (dep)
                                     (when (equal (beads-molecule--dependency-type dep)
                                                  "blocks")
                                       (beads-molecule--dependency-target dep)))
                                   deps))))
       (list :id (beads-molecule--object-field gate 'id)
             :title (beads-molecule--object-field gate 'title)
             :status (beads-molecule--object-field gate 'status)
             :await-type (beads-molecule--object-field gate 'await-type)
             :blocked blocked)))
   (beads-molecule--as-list shown)))

;;; Step state

(cl-defgeneric beads-molecule-step-state (step &optional _mol)
  "Return the state symbol for STEP in its molecule.
STATE is one of `done', `current', `ready', `blocked', `pending' or
`blocked-gate'.  The default derives it from the `bd mol current'
status letter plus the gate and dependency annotations attached by the
view; extensions may add `[agent]' or `[rig]' states."
  (or (plist-get step :state)
      (let ((status (plist-get step :status))
            (issue-status (or (plist-get step :issue-status)
                              (plist-get (plist-get step :issue) :status))))
        (cond
         ((or (equal status "done") (equal issue-status "closed")) 'done)
         ((or (plist-get step :is-current)
              (equal issue-status "in_progress")) 'current)
         ((plist-get step :gate) 'blocked-gate)
         ((plist-get step :blocked) 'blocked)
         ((equal status "blocked") 'blocked)
         ((equal status "ready") 'ready)
         (t 'pending)))))

(defun beads-molecule-step-face (state)
  "Return the face symbol for a molecule step STATE."
  (pcase state
    ('done 'beads-molecule-step-done)
    ('current 'beads-molecule-step-current)
    ('ready 'beads-molecule-step-ready)
    ('blocked 'beads-molecule-step-blocked)
    ('blocked-gate 'beads-molecule-step-blocked-gate)
    (_ 'beads-molecule-step-pending)))

(defun beads-molecule-step-glyph (state)
  "Return the glyph string for a molecule step STATE."
  (or (cdr (assq state beads-molecule-step-glyphs)) "·"))

(defun beads-molecule-step-label (state)
  "Return the short human label for a molecule step STATE."
  (or (and state (symbol-name state)) "pending"))

;;; Model construction

(defun beads-molecule--step-needs (step-id deps)
  "Return the step ids STEP-ID directly blocks on from DEPS.
DEPS is the `dependencies' list from `bd mol show'."
  (delq nil
        (mapcar (lambda (dep)
                  (when (and (equal (beads-molecule--aget dep 'issue_id) step-id)
                             (equal (beads-molecule--aget dep 'type) "blocks"))
                    (beads-molecule--aget dep 'depends_on_id)))
                deps)))

(defun beads-molecule--annotate-step (step deps gates done-ids)
  "Return STEP with `:needs', `:blocked' and `:gate' attached.
DEPS is the `bd mol show' dependency list, GATES the canonical gate
plists, and DONE-IDS the ids of already-closed steps.  A step is
`blocked' when it has a dependency that is not yet done; it is
gate-blocked when an open gate lists it."
  (let* ((id (plist-get step :id))
         (needs (beads-molecule--step-needs id deps))
         (blocked (seq-some (lambda (need) (not (member need done-ids))) needs))
         (gate (seq-find (lambda (g)
                           (and (not (equal (plist-get g :status) "closed"))
                                (member id (plist-get g :blocked))))
                         gates)))
    (plist-put (plist-put (plist-put step :needs needs) :blocked blocked)
               :gate gate)))

(defun beads-molecule--phase (steps)
  "Return the molecule phase string for STEPS.
Any ephemeral step marks the molecule as a wisp (\"vapor\"); otherwise
it is persistent.  bd does not emit the phase directly, so this is
derived from the step issues' `ephemeral' flag."
  (if (seq-some (lambda (step) (plist-get step :ephemeral)) steps)
      "vapor"
    "persistent"))

(defun beads-molecule--assignee (steps)
  "Return a display assignee for STEPS, or an empty string.
Prefers the current step's assignee, then the first assigned step."
  (let ((current (seq-find (lambda (step)
                             (eq (beads-molecule-step-state step) 'current))
                           steps)))
    (or (and current (plist-get current :assignee))
        (seq-some (lambda (step) (plist-get step :assignee)) steps)
        "")))

(defun beads-molecule--build-model (state)
  "Build the render model from aggregate STATE.
STATE is the plist resolved by `beads-molecule--load'.  Returns nil
when no molecule was found, otherwise a plist with the root header
fields, annotated `:steps', `:gates', `:progress', `:output' and the
derived frontier counts."
  (let* ((current (plist-get state :current))
         (progress (plist-get state :progress))
         (show (plist-get state :show))
         (gates (plist-get state :gates))
         (deps (or (plist-get show :dependencies) nil))
         (raw-steps (or (plist-get current :steps) nil)))
    (when current
      (let* ((done-ids (delq nil
                             (mapcar (lambda (step)
                                       (when (equal (plist-get step :status) "done")
                                         (plist-get step :id)))
                                     raw-steps)))
             (steps (mapcar (lambda (step)
                              (beads-molecule--annotate-step
                               step deps gates done-ids))
                            raw-steps))
             (states (mapcar #'beads-molecule-step-state steps))
             (progress (or progress
                           (list :completed (plist-get current :completed)
                                 :total (plist-get current :total)
                                 :percent nil)))
             (output (seq-filter (lambda (step)
                                   (eq (beads-molecule-step-state step) 'done))
                                 steps)))
        (list :id (plist-get current :id)
              :title (plist-get current :title)
              :steps steps
              :states states
              :output output
              :gates gates
              :progress progress
              :phase (beads-molecule--phase steps)
              :assignee (beads-molecule--assignee steps)
              :ready-count (seq-count (lambda (s) (eq s 'ready)) states)
              :blocked-count (seq-count (lambda (s) (eq s 'blocked)) states)
              :gated-count (seq-count (lambda (s) (eq s 'blocked-gate)) states))))))

(defun beads-molecule--percent (model)
  "Return the completion percentage for MODEL, or nil."
  (let* ((progress (plist-get model :progress))
         (percent (plist-get progress :percent))
         (completed (plist-get progress :completed))
         (total (plist-get progress :total)))
    (cond
     ((numberp percent) percent)
     ((and (numberp completed) (numberp total) (> total 0))
      (* 100.0 (/ (float completed) total)))
     (t nil))))

(defun beads-molecule--rate (model)
  "Return a formatted step rate for MODEL, or `—' when unavailable."
  (let ((rate (plist-get (plist-get model :progress) :rate)))
    (if (numberp rate) (format "%.1f/h" rate) "—")))

(defun beads-molecule--eta (model)
  "Return a formatted ETA for MODEL, or `—' when unavailable."
  (let ((eta (plist-get (plist-get model :progress) :eta)))
    (cond
     ((numberp eta) (format "%.2gh" (/ eta 3600.0)))
     ((stringp eta) eta)
     (t "—"))))

;;; Rendering helpers

(cl-defgeneric beads-molecule-sections (_mol)
  "Return the ordered section keys rendered for a molecule.
The default `(steps gates output)' matches the WI-SF-01 mockup;
downstream packages may derive to add a City/rig section."
  '(steps gates output))

(defvar beads-molecule-section-functions nil
  "Hook of functions contributing extra section vnodes to the molecule view.
Each function is called with the render model plist and returns either
nil or a list of vui vnodes.  An empty hook (the standalone default)
adds nothing.")

(defun beads-molecule--collapsed-p (key)
  "Return non-nil when section KEY is folded in the current buffer."
  (cdr (assq key beads-molecule--collapsed)))

(defun beads-molecule--thing-id-at-point ()
  "Return the molecule step id of the thing at point, or nil."
  (let ((thing (beads-thing-at)))
    (and (consp thing) (keywordp (car thing)) (plist-get thing :id))))

(defun beads-molecule--section-header-vnode (key title count)
  "Return a foldable vui vnode for section KEY TITLE with COUNT rows."
  (let* ((collapsed (beads-molecule--collapsed-p key))
         (label (concat (if collapsed
                            beads-section-glyph-collapsed
                          beads-section-glyph-expanded)
                        " " title
                        (if count (format " (%d)" count) ""))))
    (vui-button
     (beads-thing-propertize label (list :kind 'section :key key))
     :no-decoration t
     :face 'bold
     :help-echo (if collapsed
                    (format "Expand %s" title)
                  (format "Collapse %s" title))
     :on-click (lambda () (beads-molecule-toggle-section key)))))

(defun beads-molecule--step-line (step index)
  "Return a propertized display line for STEP at INDEX.
The line carries a `beads-thing' step marker so TAB/S-TAB stop on it."
  (let* ((state (beads-molecule-step-state step))
         (glyph (propertize (beads-molecule-step-glyph state)
                            'face (beads-molecule-step-face state)))
         (id (or (plist-get step :id) ""))
         (type (or (plist-get step :issue-type) ""))
         (label (beads-molecule-step-label state))
         (title (or (plist-get step :title) ""))
         (needs (plist-get step :needs))
         (gate (plist-get step :gate))
         (rest (format " %2d. %-16s %-10s %-12s %s"
                       index id type label title))
         (rest (if (and needs (not (eq state 'done)))
                   (concat rest (format "  needs %s"
                                        (string-join needs ", ")))
                 rest))
         (rest (if gate
                   (concat rest (format "  %s %s"
                                        beads-molecule-gate-glyph
                                        (or (plist-get gate :id) "")))
                 rest)))
    (beads-thing-propertize
     (concat "  " glyph (propertize rest 'face 'beads-issue-line))
     (list :kind 'step :id id))))

(defun beads-molecule--gate-line (gate)
  "Return a display line for GATE."
  (let ((await (or (plist-get gate :await-type) "gate"))
        (id (or (plist-get gate :id) ""))
        (status (or (plist-get gate :status) "open"))
        (blocked (plist-get gate :blocked)))
    (format "  %s %-8s %-16s %-8s blocks %s"
            beads-molecule-gate-glyph await id status
            (if blocked (string-join blocked ", ") "—"))))

(defun beads-molecule--output-line (step)
  "Return a display line for a closed STEP."
  (let ((id (or (plist-get step :id) ""))
        (title (or (plist-get step :title) ""))
        (closed (plist-get step :closed-at))
        (reason (plist-get step :close-reason)))
    (concat "  ✓ " id " " title
            (when closed (format "  (closed %s%s)"
                                 (beads-molecule--short-time closed)
                                 (if (and reason (not (string-empty-p reason)))
                                     (format ", reason \"%s\"" reason)
                                   ""))))))

(defun beads-molecule--short-time (timestamp)
  "Return a compact rendering of TIMESTAMP, an ISO 8601 string.
Falls back to the raw string when it does not match the expected shape."
  (if (and (stringp timestamp) (>= (length timestamp) 16))
      (substring timestamp 11 16)
    (or timestamp "—")))

(defun beads-molecule--join-lines (lines)
  "Return LINES joined with newlines into one `vui-text' string."
  (string-join lines "\n"))

(defun beads-molecule--header-vnodes (model)
  "Return the header vnodes for MODEL."
  (let* ((percent (beads-molecule--percent model))
         (progress (plist-get model :progress))
         (completed (plist-get progress :completed))
         (total (plist-get progress :total))
         (assignee (plist-get model :assignee))
         (ready (plist-get model :ready-count))
         (blocked (plist-get model :blocked-count))
         (gated (plist-get model :gated-count))
         (assignee (if (and assignee (not (string-empty-p assignee)))
                       assignee "(none)")))
    (list
     (vui-text (format "%s — %s"
                       (or (plist-get model :id) "molecule")
                       (or (plist-get model :title) ""))
               :face 'beads-molecule-header)
     (vui-text
      (format "  %s %s · assignee %s · %s/%s done (%s) · rate %s · ETA %s"
              beads-molecule-phase-glyph (plist-get model :phase)
              assignee
              (or completed 0) (or total 0)
              (if percent (format "%.0f%%" percent) "—")
              (beads-molecule--rate model)
              (beads-molecule--eta model))
      :face 'shadow)
     (vui-text
      (format "  Ready frontier: %d  ·  Blocked: %d  ·  Gated: %d"
              (or ready 0) (or blocked 0) (or gated 0))
      :face 'shadow))))

(defun beads-molecule--steps-vnode (model ready-only)
  "Return the steps section vnode for MODEL.
When READY-ONLY is non-nil only ready steps render."
  (let* ((steps (plist-get model :steps))
         (visible (if ready-only
                      (seq-filter (lambda (step)
                                    (eq (beads-molecule-step-state step) 'ready))
                                  steps)
                    steps))
         (title (if ready-only "Ready" "Steps"))
         (lines (cl-loop for step in visible
                         for i from 1
                         collect (beads-molecule--step-line step i)))
         (body (if lines
                   (beads-molecule--join-lines lines)
                 (if ready-only
                     "  (no ready steps)"
                   "  (no steps)"))))
    (vui-vstack
     (beads-molecule--section-header-vnode
      'steps title (length visible))
     (unless (beads-molecule--collapsed-p 'steps)
       (vui-text body)))))

(defun beads-molecule--gates-vnode (model)
  "Return the gates section vnode for MODEL."
  (let* ((gates (plist-get model :gates))
         (lines (mapcar #'beads-molecule--gate-line gates))
         (body (if lines
                   (beads-molecule--join-lines lines)
                 "  (no gates)")))
    (vui-vstack
     (beads-molecule--section-header-vnode 'gates "Gates" (length gates))
     (unless (beads-molecule--collapsed-p 'gates)
       (vui-text body)))))

(defun beads-molecule--output-vnode (model)
  "Return the output section vnode for MODEL."
  (let* ((output (plist-get model :output))
         (lines (mapcar #'beads-molecule--output-line output))
         (body (if lines
                   (beads-molecule--join-lines lines)
                 "  (no closed steps yet)")))
    (vui-vstack
     (beads-molecule--section-header-vnode 'output "Output" (length output))
     (unless (beads-molecule--collapsed-p 'output)
       (vui-text body)))))

(defun beads-molecule--extra-vnodes (model)
  "Return the vnodes contributed by `beads-molecule-section-functions' for MODEL.
Each hook function returns nil or a list of vnodes; a failing function
is isolated so it cannot blank the rest of the view."
  (delq nil
        (mapcar (lambda (fn)
                  (condition-case err
                      (funcall fn model)
                    (error
                     (vui-text (format "  Section error: %s"
                                       (error-message-string err))
                               :face 'error))))
                beads-molecule-section-functions)))

(defun beads-molecule--footer-vnode ()
  "Return the key-hint footer vnode."
  (vui-text
   (concat "  g refresh · r ready-only · SPC fold · TAB/S-TAB move · "
           "RET inspect · ] next range · [ prev range · q bury")
   :face 'shadow))

(defun beads-molecule--error-string (err)
  "Return a displayable string for error ERR."
  (cond
   ((stringp err) err)
   ((and (listp err) (stringp (car err))) (car err))
   (t (format "%S" err))))

;;; Async aggregate loader

(defun beads-molecule--load-gates (root resolve reject)
  "Asynchronously load the gates attached to ROOT's molecule.
Fetches the open gate issues, then their dependents in one batched
`bd show --include-dependents' call, and calls RESOLVE with the
canonical gate plists.  Calls REJECT with the error otherwise."
  (beads-command-execute-async
   (beads-command-list :issue-type "gate" :json t)
   (lambda (gates)
     (let ((ids (delq nil (mapcar (lambda (gate)
                                    (beads-molecule--object-field gate 'id))
                                  (beads-molecule--as-list gates)))))
       (if (null ids)
           (funcall resolve nil)
         (beads-command-execute-async
          (beads-command-show :issue-ids ids :include-dependents t :json t)
          (lambda (shown)
            (funcall resolve (beads-molecule--normalize-gates shown)))
          (lambda (err) (funcall reject err))
          :queue 'auto
          :cache-key (list 'molecule-gates-detail root ids)
          :timeout beads-command-async-timeout))))
   (lambda (err) (funcall reject err))
   :queue 'auto
   :cache-key (list 'molecule-gates root)
   :timeout beads-command-async-timeout))

(defun beads-molecule--load (root range resolve reject)
  "Asynchronously load the aggregate data for molecule ROOT.
RANGE, when non-nil, is a cons of 1-based inclusive step bounds and is
forwarded to `bd mol current --range' so a large molecule only fetches
its visible window.

Fans out `bd mol current', `bd mol progress', `bd mol show' and the
gate fetch through `beads-command-execute-async', each with its own
`bd subprocess.  Calls RESOLVE with a plist of the four canonical
payloads once all four settle; a failure of `bd mol current' (the only
load-bearing command) calls REJECT instead.  A soft failure of the
other three degrades the corresponding section rather than blanking
the view."
  (let* ((state (list :current nil :progress nil :show nil :gates nil))
         (pending 4)
         (failed nil))
    (cl-labels
        ((finish ()
           (if failed
               (funcall reject failed)
             (funcall resolve state)))
         (done (key value)
           (setq state (plist-put state key value))
           (when (zerop (cl-decf pending)) (finish)))
         (fail (err)
           (setq failed (or failed err))
           (when (zerop (cl-decf pending)) (finish))))
      (beads-command-execute-async
       (beads-command-mol-current :mol-id root :json t
                                  :range (when range
                                           (format "%d-%d" (car range) (cdr range))))
       (lambda (value) (done :current (beads-molecule--normalize-current value)))
       (lambda (err) (fail err))
       :queue 'auto
       :cache-key (list 'molecule-current root)
       :timeout beads-command-async-timeout)
      (beads-command-execute-async
       (beads-command-mol-progress :mol-id root :json t)
       (lambda (value) (done :progress (beads-molecule--normalize-progress value)))
       (lambda (_err) (done :progress nil))
       :queue 'auto
       :cache-key (list 'molecule-progress root)
       :timeout beads-command-async-timeout)
      (beads-command-execute-async
       (beads-command-mol-show :mol-id root :json t)
       (lambda (value) (done :show (beads-molecule--normalize-show value)))
       (lambda (_err) (done :show nil))
       :queue 'auto
       :cache-key (list 'molecule-show root)
       :timeout beads-command-async-timeout)
      (beads-molecule--load-gates
       root
       (lambda (gates) (done :gates gates))
       (lambda (_err) (done :gates nil))))))

;;; Root component

(defun beads-molecule--root-state (key)
  "Return the value of KEY in the molecule root vui state, or nil."
  (and (boundp 'vui--root-instance)
       vui--root-instance
       (plist-get (vui-instance-state vui--root-instance) key)))

(defun beads-molecule--bump (key value)
  "Apply VALUE to root state slot KEY and re-render the buffer.
Skips the rerender when VALUE equals the current slot."
  (when (and (boundp 'vui--root-instance) vui--root-instance)
    (let ((state (vui-instance-state vui--root-instance)))
      (unless (equal (plist-get state key) value)
        (setf (vui-instance-state vui--root-instance)
              (plist-put state key value))
        (vui--rerender-instance vui--root-instance)))))

(defun beads-molecule-toggle-section (key)
  "Toggle the folded state of section KEY in the current molecule buffer."
  (interactive "S")
  (let* ((current (beads-molecule--collapsed-p key))
         (next (if current
                   (assq-delete-all key beads-molecule--collapsed)
                 (cons (cons key t) beads-molecule--collapsed))))
    (setq beads-molecule--collapsed next)
    (beads-molecule--bump :collapsed next)))

(vui-defcomponent beads-molecule--root
    (root store)
  "Root vui component for the molecule execution view.

ROOT is the molecule id; STORE is the bd store directory the view was
opened for (nil for the default store)."
  :state ((collapsed (or beads-molecule--collapsed nil))
          (generation 0)
          (ready-only nil)
          (range nil))
  :render
  (let* ((async-key (list 'beads-molecule root generation ready-only range))
         (async (vui-use-async
                 async-key
                 (lambda (resolve reject)
                   (beads-molecule--load root range resolve reject))))
         (status (plist-get async :status))
         (data (plist-get async :data))
         (err (plist-get async :error))
         ;; Stale-while-revalidate: keep the previous payload on screen
         ;; during a soft refresh so the tree does not flash empty.
         (cache (and (boundp 'beads-molecule--last-good-data)
                     beads-molecule--last-good-data))
         (cached (and cache (gethash async-key cache 'absent)))
         (has-cache (not (eq cached 'absent)))
         (effective (cond ((eq status 'ready) data)
                          ((and (eq status 'pending) has-cache) cached)
                          (t nil)))
         (effective-status (cond ((eq status 'ready) 'ready)
                                 ((and (eq status 'pending) has-cache) 'ready)
                                 (t status))))
    (when (and cache (eq status 'ready))
      (puthash async-key data cache))
    (vui-use-effect (effective)
      (when effective
        (setq-local beads-molecule--model (beads-molecule--build-model effective)))
      nil)
    (vui-vstack
     :spacing 1
     (pcase effective-status
       ('pending
        (vui-text (format "  Loading molecule %s…" root) :face 'shadow))
       ('error
        (vui-error-boundary
         :id (list 'beads-molecule root)
         :fallback (lambda (e)
                     (vui-text (format "  Error: %s"
                                       (beads-molecule--error-string e))
                               :face 'error))
         :children
         (list (vui-text (format "  Error: %s"
                                 (beads-molecule--error-string err))
                         :face 'error))))
       ('ready
        (let ((model (beads-molecule--build-model effective)))
          (if (null model)
              (vui-text (format "  No molecule found: %s.  Open with \
M-x beads-molecule and an explicit id." root)
                        :face 'warning)
            (vui-vstack
             :spacing 0
             (apply #'vui-vstack (beads-molecule--header-vnodes model))
             (apply #'vui-vstack
                    (seq-mapcat
                     (lambda (key)
                       (let ((vnode (beads-molecule--section-vnode
                                     key model ready-only)))
                         (when vnode
                           (list (vui-text "")
                                 (vui-error-boundary
                                  :id (list 'beads-molecule root key)
                                  :fallback
                                  (lambda (e)
                                    (vui-text
                                     (format "  %s error: %s" key
                                             (beads-molecule--error-string e))
                                     :face 'error))
                                  :children (list vnode))))))
                     (beads-molecule-sections (plist-get model :id))))
             (when-let* ((extra (beads-molecule--extra-vnodes model)))
               (apply #'vui-vstack extra)))))))
     (vui-text "")
     (beads-molecule--footer-vnode))))

;;; Mode

(defvar-keymap beads-molecule-mode-map
  :parent beads-section-mode-map
  "RET" #'beads-molecule-inspect
  "<return>" #'beads-molecule-inspect
  "g" #'beads-molecule-refresh
  "r" #'beads-molecule-toggle-ready-only
  "]" #'beads-molecule-next-range
  "[" #'beads-molecule-previous-range
  "q" #'quit-window)

;; TAB/S-TAB/SPC follow the shared thing contract; `beads-section-mode'
;; already installs them but vui binds <tab> in a parent map, so the
;; install is repeated here.
(beads-thing-define-keys beads-molecule-mode-map)

(define-derived-mode beads-molecule-mode beads-section-mode "Beads-Molecule"
  "Major mode for the beads molecule execution view.

Derived from `beads-section-mode' so the `beads-section' text-property
contract and the `beads-thing' movement contract are inherited.  The
view is read-only; the work-loop actions (claim/close) land in
WI-SF-02.

\\{beads-molecule-mode-map}"
  :interactive nil
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'beads-molecule--revert)
  (setq-local beads-molecule--collapsed nil)
  (setq-local beads-molecule--last-good-data (make-hash-table :test 'equal)))

(defun beads-molecule--revert (_ignore _noconfirm)
  "Revert the molecule buffer (no-prompt revert hook)."
  (beads-molecule-refresh))

;;; Commands

(cl-defgeneric beads-molecule-action (action step)
  "Perform ACTION on molecule STEP and return the result.
ACTION is a symbol: `inspect' opens the step in the issue detail view.
WI-SF-02 adds `claim', `close' and `close-eligible'.")
(cl-defmethod beads-molecule-action (action step)
  "Default implementation: dispatch ACTION on STEP."
  (pcase action
    ('inspect (beads-show (plist-get step :id)))
    (_ (user-error "Unsupported molecule action: %s" action))))

(defun beads-molecule--step-at-point ()
  "Return the model step at point, or nil.
The step is resolved through the `beads-thing' property; the model is
read back from the root component's loaded payload."
  (when-let* ((id (beads-molecule--thing-id-at-point)))
    (when-let* ((model (beads-molecule--current-model)))
      (seq-find (lambda (step) (equal (plist-get step :id) id))
                (plist-get model :steps)))))

(defun beads-molecule--current-model ()
  "Return the render model of the current buffer, or nil."
  beads-molecule--model)

(defun beads-molecule-inspect ()
  "Inspect the molecule step at point in the issue detail view."
  (interactive)
  (if-let* ((step (beads-molecule--step-at-point)))
      (beads-molecule-action 'inspect step)
    (user-error "No molecule step at point")))

(defun beads-molecule-refresh ()
  "Refresh the molecule view by bumping the generation counter."
  (interactive)
  (beads-molecule--bump :generation
                        (1+ (or (beads-molecule--root-state :generation) 0))))

(defun beads-molecule-toggle-ready-only ()
  "Toggle the ready-only step filter."
  (interactive)
  (beads-molecule--bump :ready-only
                        (not (beads-molecule--root-state :ready-only))))

(defun beads-molecule-next-range ()
  "Window the step tree forward by one page."
  (interactive)
  (let ((range (beads-molecule--root-state :range)))
    (beads-molecule--bump :range
                          (beads-molecule--shift-range range 1))))

(defun beads-molecule-previous-range ()
  "Window the step tree backward by one page."
  (interactive)
  (beads-molecule--bump :range
                        (beads-molecule--shift-range
                         (beads-molecule--root-state :range) -1)))

(defun beads-molecule--shift-range (range delta)
  "Return RANGE shifted by DELTA pages, or nil.
RANGE is nil or a cons of the 1-based inclusive bounds; the page size
is `beads-molecule-range-size'."
  (let* ((size beads-molecule-range-size)
         (start (if range (max 1 (+ (car range) (* delta size))) 1)))
    (cons start (+ start size -1))))

(defun beads-molecule--section-vnode (key model ready-only)
  "Return the vnode for section KEY of MODEL, or nil.
READY-ONLY narrows the steps section to the ready frontier."
  (pcase key
    ('steps (beads-molecule--steps-vnode model ready-only))
    ('gates (beads-molecule--gates-vnode model))
    ('output (beads-molecule--output-vnode model))
    (_ nil)))

;;; Entry point

(defun beads-molecule--buffer-name-for (root store)
  "Return the molecule buffer name for molecule ROOT in STORE.
A remote STORE is qualified with its TRAMP prefix so a local and a
remote molecule with the same id get distinct buffers."
  (let* ((project (if store
                      (file-name-nondirectory (directory-file-name store))
                    (or (ignore-errors (beads--project-name)) "unknown"))))
    (beads-buffer-utility "molecule" root project)))

;;;###autoload
(cl-defun beads-molecule-open (root &optional directory)
  "Open the molecule execution view for molecule ROOT.

With DIRECTORY non-nil, scope the view to the bead store at DIRECTORY
instead of resolving from `default-directory'.  The view's async bd
calls run with `--directory' from the buffer-local
`beads-store-directory' (see `beads-meta-build-global-options')."
  (interactive (list (read-string "Molecule ID: ")))
  (require 'beads-command-mol)
  (beads-check-executable)
  (let* ((store (or (beads-store-resolve directory) beads-store-directory))
         (default-directory (or store default-directory))
         (project-root (if store
                           (beads-store-project-root store)
                         (or (beads--project-root) default-directory)))
         (buffer-name (beads-molecule--buffer-name-for root store))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-molecule-mode)
        (beads-molecule-mode))
      (setq-local beads-molecule--root root)
      (setq-local beads-molecule--project-root project-root)
      (setq-local beads-store-directory store))
    (with-current-buffer buffer
      (vui-mount
       (vui-component 'beads-molecule--root :root root :store store)
       (buffer-name)))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun beads-molecule (root &optional directory)
  "Open the molecule execution view for ROOT.
DIRECTORY, when non-nil, scopes the view to that bead store.
Interactive entry point; see `beads-molecule-open' for the contract."
  (interactive (list (read-string "Molecule ID: ")))
  (beads-molecule-open root directory))

(provide 'beads-molecule)
;;; beads-molecule.el ends here
