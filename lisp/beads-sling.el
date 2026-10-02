;;; beads-sling.el --- Standalone sling targets, shape and dispatch -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, agents

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The standalone sling abstraction (design.md §4.5 and §8, REQ-008,
;; REQ-021): a target value object, the discovery hooks that let
;; downstream packages contribute dispatch destinations, the pure shape
;; inference that decides whether a dispatch is a plain work item, a
;; targeted formula run (`--on') or a formula-only run, and the dispatch
;; generic that turns a target plus a work item into a started session.
;;
;; Everything here works with `bd' plus the local agent subsystem only:
;;
;; - `beads-sling-target'      value object for one dispatch destination.
;; - `beads-sling-target-functions'  hook of target providers.  Its
;;   default value is `beads-sling--default-targets', which contributes
;;   one target per local agent role, one per available agent backend
;;   and one per existing git worktree.
;; - `beads-sling-targets'     collect + dedupe the providers.
;; - `beads-sling-shape'       pure inference: plain | on | formula.
;; - `beads-sling-backend'     named dispatch backend value object.
;; - `beads-sling-backend-register'  registry for named backends.
;; - `beads-sling-dispatch'    cl-defgeneric: launch BEAD to TARGET.
;; - `beads-sling-validators'  hook of pre-launch validation functions.
;;
;; Gas City and other extensions add targets (`add-hook
;; beads-sling-target-functions') and register a named backend
;; (`beads-sling-backend-register') without changing core.  An empty
;; hook yields no targets, and the default dispatch method starts a
;; local agent through the existing agent subsystem (REQ-021).

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'subr-x)
(require 'beads-agent)
(require 'beads-agent-backend)
(require 'beads-agent-type)
(require 'beads-git)

(declare-function beads-agent--start-with-worktree "beads-agent"
                  (issue-id backend project-dir worktree-path &optional agent-type-name))
(declare-function beads-agent-start "beads-agent"
                  (&optional issue-id backend-name prompt agent-type-name))
(declare-function beads-git-get-project-name "beads-git" ())
(declare-function beads-git-list-worktrees "beads-git" ())

;;; Sling Target

(defclass beads-sling-target ()
  ((name
    :initarg :name
    :type string
    :documentation "Stable, human-readable name of the target.")
   (kind
    :initarg :kind
    :type (or null symbol string)
    :documentation "Target kind (for example `role', `agent' or `worktree').")
   (scope
    :initarg :scope
    :type (or null string)
    :documentation "Store or scope the target belongs to, or nil.")
   (backend
    :initarg :backend
    :initform nil
    :type (or null symbol string)
    :documentation "Dispatch backend name; nil or `local' means local.")
   (description
    :initarg :description
    :initform nil
    :type (or null string)
    :documentation "One-line description shown in the target list.")
   (metadata
    :initarg :metadata
    :initform nil
    :type list
    :documentation "Free-form plist carried by the extension."))
  "A dispatchable sling target.
Extensions build these and return them from
`beads-sling-target-functions'.  `beads-sling-dispatch' specializes on
this class; a downstream package can subclass it to override dispatch
for its own targets without changing the core.

The `metadata' plist is the extension carrier.  Core understands two
keys on worktree targets:

- `:path'      absolute worktree path the agent should run in.
- `:branch'    branch checked out in the worktree, for display.
- `:agent-type'  agent role name to start in that worktree.")

(defun beads-sling-worktree-target (name path &optional branch)
  "Build a local worktree `beads-sling-target' for PATH.
NAME is the worktree's short name, BRANCH its branch (or nil).  The
returned target dispatches through `beads-sling-dispatch' to a local
agent started in PATH."
  (beads-sling-target
   :name (format "worktree: %s" name)
   :kind 'worktree
   :scope path
   :backend "local"
   :description (if (and branch (not (string-empty-p branch)))
                    (format "branch %s" branch)
                  "detached")
   :metadata (list :path path :branch branch)))

;;; Target Discovery

(defun beads-sling--project-label ()
  "Return the display label for the current project.
Falls back to \"local\" when no project root can be resolved so target
collection never fails in a bare directory."
  (or (ignore-errors (beads-git-get-project-name))
      (ignore-errors (beads--project-name))
      "local"))

(defun beads-sling--role-targets ()
  "Return one sling target per registered local agent role.
Role names are namespaced under the project label (for example
\"beads.el/task\").  This is the standalone replacement for the old
per-role agent commands."
  (let ((project (beads-sling--project-label)))
    (mapcar
     (lambda (type)
       (let ((role (downcase (oref type name))))
         (beads-sling-target
          :name (format "%s/%s" project role)
          :kind 'role
          :scope project
          :backend "local"
          :description (oref type description)
          :metadata (list :agent-type role))))
     (ignore-errors (beads-agent-type-list)))))

(defun beads-sling--backend-targets ()
  "Return one sling target per available local agent backend.
Backend names are namespaced under the project label and carry the
backend name in the `backend' slot so dispatch selects it."
  (let ((project (beads-sling--project-label)))
    (mapcar
     (lambda (backend)
       (beads-sling-target
        :name (format "%s/%s" project (oref backend name))
        :kind 'agent
        :scope project
        :backend (oref backend name)
        :description (oref backend description)))
     (ignore-errors (beads-agent--get-available-backends)))))

(defun beads-sling--worktree-targets ()
  "Return one sling target per existing git worktree."
  (mapcar
   (lambda (worktree)
     (let* ((path (car worktree))
            (branch (cadr worktree))
            (name (file-name-nondirectory (directory-file-name path))))
       (beads-sling-worktree-target name path branch)))
   (ignore-errors (beads-git-list-worktrees))))

(defun beads-sling--default-targets ()
  "Return the standalone default sling targets.
One target per registered local agent role (kind `role'), one per
available agent backend (kind `agent') and one per existing git
worktree (kind `worktree').  Ordering is roles, backends, worktrees so
the Who picker leads with the common choices."
  (append (beads-sling--role-targets)
          (beads-sling--backend-targets)
          (beads-sling--worktree-targets)))

(defvar beads-sling-target-functions (list #'beads-sling--default-targets)
  "Hook of functions returning sling targets.
Each function is called with no arguments and returns a list of
`beads-sling-target' objects.  The default value is
`beads-sling--default-targets', which contributes the local role,
backend and worktree targets; extensions `add-hook' their own providers,
for example Gas City contributes city and rig agents.
`beads-sling-targets' consults providers in order and dedupes by target
name, so an extension cannot shadow a default target of the same name.
A hook bound to nil yields no targets.")

(defun beads-sling-targets (&optional bead)
  "Collect, dedupe and return the sling targets for BEAD.
Runs `beads-sling-target-functions', keeps only
`beads-sling-target' instances and dedupes by `name' (first provider
wins).  BEAD is accepted for API symmetry with the dispatch signature
but is not forwarded to providers, which take no arguments.  Returns
nil when the hook is empty."
  (ignore bead)
  (let ((seen (make-hash-table :test #'equal))
        (targets nil))
    (dolist (fn beads-sling-target-functions)
      (dolist (target (ignore-errors (funcall fn)))
        (when (beads-sling-target-p target)
          (let ((key (oref target name)))
            (unless (gethash key seen)
              (puthash key t seen)
              (push target targets))))))
    (nreverse targets)))

;;; Shape Inference

(defun beads-sling-shape (work formula)
  "Infer the sling shape from WORK and FORMULA.
WORK is a bead id, freeform payload or nil.  FORMULA is a formula name
or nil.  Return one of:

- `on'      WORK and FORMULA are both set (a formula run against a bead).
- `plain'   only WORK is set (dispatch the work to a target).
- `formula' only FORMULA is set (run the formula with no work).
- nil       neither is set (a cold, not-yet-ready dispatch).

Empty strings count as unset so an untouched minibuffer field does not
turn a formula-only run into an `on' run.  This is intentionally pure:
it never reads variables or state, so the adaptive transient (WI-11)
can call it on every stage update."
  (let ((has-work (and work
                       (not (and (stringp work) (string-empty-p work)))))
        (has-formula (and formula
                          (not (and (stringp formula)
                                    (string-empty-p formula))))))
    (cond
     ((and has-work has-formula) 'on)
     (has-work 'plain)
     (has-formula 'formula)
     (t nil))))

;;; Named Backends

(defclass beads-sling-backend ()
  ((name
    :initarg :name
    :type string
    :documentation "Stable name the target's `backend' slot selects.")
   (dispatch
    :initarg :dispatch
    :type function
    :documentation "Function called as (TARGET BEAD PROMPT) to launch.")
   (description
    :initarg :description
    :initform ""
    :type string
    :documentation "One-line description of the backend."))
  "A named sling dispatch backend.
The default local path needs no backend; extensions register one (for
example Gas City registers a `gc' backend that calls `gc sling').")

(defvar beads-sling--backends (make-hash-table :test #'equal)
  "Registry of named sling backends, keyed by backend name.")

(defun beads-sling-backend-register (backend)
  "Register BACKEND and return it.
BACKEND must be a `beads-sling-backend'.  A later registration with the
same name replaces the earlier one."
  (unless (object-of-class-p backend 'beads-sling-backend)
    (error "Backend must be a beads-sling-backend instance"))
  (puthash (oref backend name) backend beads-sling--backends)
  backend)

(defun beads-sling-backend-get (name)
  "Return the registered `beads-sling-backend' named NAME, or nil.
NAME may be a string or symbol."
  (let ((key (cond ((symbolp name) (symbol-name name))
                   ((stringp name) name)
                   (t nil))))
    (and key (gethash key beads-sling--backends))))

(defun beads-sling-backend-list ()
  "Return all registered sling backends, sorted by name."
  (let ((backends (cl-loop for backend being the hash-values
                           of beads-sling--backends
                           collect backend)))
    (sort backends
          (lambda (a b) (string< (oref a name) (oref b name))))))

(defun beads-sling--local-backend-name (backend)
  "Normalize BACKEND to a local agent backend name, or nil.
Nil, `local' and the string \"local\" all mean the configured default
agent backend."
  (cond
   ((null backend) nil)
   ((eq backend 'local) nil)
   ((symbolp backend)
    (let ((name (symbol-name backend)))
      (unless (or (string-empty-p name) (equal name "local")) name)))
   ((stringp backend)
    (unless (or (string-empty-p backend) (equal backend "local")) backend))
   (t nil)))

;;; Validators

(defvar beads-sling-validators nil
  "Hook of pre-launch sling validators.
Each function is called with a CONTEXT plist and returns a warning
string or nil.  `beads-sling-validate' runs them and returns the
non-nil warnings.  Gas City uses this to warn about cross-store routes
and the city-scoped `run_targets' trap before `gc' would refuse.")

(defun beads-sling-validate (context)
  "Run `beads-sling-validators' against CONTEXT and return warnings.
CONTEXT is a plist describing the pending dispatch (work, formula,
target, ...).  Returns a list of warning strings, empty when the hook is
empty or every validator is satisfied."
  (delq nil (mapcar (lambda (fn) (ignore-errors (funcall fn context)))
                    beads-sling-validators)))

;;; Dispatch

(cl-defgeneric beads-sling-dispatch (target bead prompt)
  "Dispatch BEAD with PROMPT to TARGET and return the started session.
TARGET is a `beads-sling-target', BEAD is a bead id or freeform
payload, and PROMPT is the optional freeform instruction.  The default
method starts a local agent; extension packages specialize this generic
on their own target subclasses to reach their own backend.")

(cl-defmethod beads-sling-dispatch ((target beads-sling-target) bead prompt)
  "Default local method: launch BEAD to TARGET with PROMPT.
Resolution order:

1. A registered `beads-sling-backend' whose name matches the target's
   `backend' slot wins (Gas City's `gc' backend).
2. A worktree target with a `:path' starts a local agent in that
   worktree.
3. Otherwise a local agent is started through `beads-agent-start'; a
   non-local backend name selects that registered agent backend."
  (let* ((backend (oref target backend))
         (sling-backend (beads-sling-backend-get backend)))
    (cond
     (sling-backend
      (funcall (oref sling-backend dispatch) target bead prompt))
     ((and (eq (oref target kind) 'worktree)
           (plist-get (oref target metadata) :path))
      (beads-agent--start-with-worktree
       bead nil (beads-git-find-project-root)
       (plist-get (oref target metadata) :path)
       (or (plist-get (oref target metadata) :agent-type) "Task")))
     (t
      (beads-agent-start
       bead
       (beads-sling--local-backend-name backend)
       prompt nil)))))

(provide 'beads-sling)
;;; beads-sling.el ends here
