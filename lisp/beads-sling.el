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
(require 'transient)
(require 'beads-prefix)
(require 'beads-command)
(require 'beads-command-formula)
(require 'beads-types)
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
(declare-function beads-git-find-project-root "beads-git" (&optional dir))
(declare-function beads-completion-read-issue "beads-completion"
                  (prompt &optional default require-match initial history))
(declare-function beads-completion-sling-target-table "beads-completion"
                  (&optional bead))


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

;;; ============================================================
;;; Sling flow — adaptive transient and preview (WI-11, REQ-009)
;;; ============================================================
;;
;; The adaptive sling surface: one transient (`beads-sling') whose
;; stages collapse as they are answered, a live footer that recomputes
;; on every redraw, typed How readers generated from a picked formula's
;; `beads-formula-var' definitions, and a `P' full preview that never
;; gates launch.  Shape is inferred (WI-10's `beads-sling-shape') and
;; rendered as one sentence; the stages are the signed-off mockups
;; (menu-mockups.md §6 and §7).  Everything here is client-side: it
;; works with `bd' plus the local agent subsystem alone.

;;; Constants and labels

(defconst beads-sling--no-work-hint "(no work — A or point at a bead)"
  "Header stand-in for a missing work selection (mockup §6b).")

(defconst beads-sling--no-target-hint "(no target — T or default)"
  "Header stand-in for a target with no derivable default (mockup §6b).")

(defconst beads-sling--reserved-keys
  '("A" "f" "T" "w" "b" "n" "s" "P" "r" "x" "q")
  "The single-letter keys the sling transient binds statically.
Generated formula-var infix keys avoid exactly this list so a var
never shadows a stage or action key.  It is the mockup §6 key set:
the What pickers (`A', `f'), the Who picker (`T'), the routing
flags (`w', `b', `n') and the Actions (`s', `P', `r', `x', `q').")

(defun beads-sling--blank (value)
  "Return non-nil when VALUE is nil or the empty string."
  (or (null value)
      (and (stringp value) (string-empty-p value))))

(defun beads-sling--work-id-p (work)
  "Return non-nil when WORK reads as a bead id.
A display heuristic only: `beads-sling--work-phrase' qualifies a bare
id with `bead'; freeform text is rendered as entered."
  (and (stringp work)
       (string-match-p "\\`[a-zA-Z][a-zA-Z0-9._-]*-[0-9a-z]+\\(\\.[0-9]+\\)*\\'"
                       work)))

(defun beads-sling--freeform-p (work)
  "Return non-nil when WORK is freeform text rather than a bead id."
  (and (not (beads-sling--blank work))
       (not (beads-sling--work-id-p work))))

(defun beads-sling--work-phrase (work)
  "Return WORK as the header sentence's work phrase.
`bead <id>' for a bare bead id, the freeform text as entered, or the
mockup §6b no-work hint when blank."
  (cond ((beads-sling--blank work) beads-sling--no-work-hint)
        ((beads-sling--work-id-p work) (format "bead %s" work))
        (t work)))

(defun beads-sling--target-phrase (target)
  "Return TARGET as the header sentence's target phrase."
  (if (beads-sling--blank target) beads-sling--no-target-hint target))

(defun beads-sling--header-sentence (work formula target &optional recipe)
  "Return the one-sentence header for the sling scope (REQ-009).
WORK is a bead id or freeform text, FORMULA the picked formula name,
TARGET the resolved target, and RECIPE the picked formula.  Shape is
inferred through `beads-sling-shape' and rendered as one sentence —
never a flag (menu-mockups.md §6):

  Sling bead be-abcd to agent beads.el/task
  Sling (no work — A or point at a bead) to (no target — T or default)
  Run pancakes (formula) locally
  Run build-basic against bead be-abcd, drained by beads.el/task"
  (ignore recipe)
  (pcase (beads-sling-shape work formula)
    ('formula
     (if (beads-sling--blank target)
         (format "Run %s (formula) locally" formula)
       (format "Run %s (formula) on %s" formula target)))
    ('on
     (format "Run %s against %s, drained by %s"
             formula (beads-sling--work-phrase work)
             (beads-sling--target-phrase target)))
    (_
     (format "Sling %s to %s"
             (beads-sling--work-phrase work)
             (beads-sling--target-phrase target)))))

(defun beads-sling--work-label (work &optional title)
  "Return the What stage's work line answer for WORK and TITLE."
  (cond ((beads-sling--blank work) "(none — A to pick an open bead)")
        (title (format "%s — %s" work title))
        (t work)))

(defun beads-sling--formula-label (formula &optional recipe)
  "Return the What stage's formula line answer for FORMULA and RECIPE."
  (if (beads-sling--blank formula)
      "(none — f to pick)"
    (let ((description (and recipe (oref recipe description))))
      (if (beads-sling--blank description)
          formula
        (format "%s — %s" formula description)))))

(defun beads-sling--target-label (target)
  "Return the Who stage's target line answer for TARGET."
  (if (beads-sling--blank target)
      "(no default derivable — T to choose)"
    (format "%s · local · available" target)))

;;; Target lookup and derivation

(defun beads-sling--target-get (name)
  "Return the collected `beads-sling-target' named NAME, or nil."
  (when (and name (not (beads-sling--blank name)))
    (cl-find name (beads-sling-targets)
             :key (lambda (target) (oref target name))
             :test #'equal)))

(defun beads-sling--derived-target-name ()
  "Return the default target for this rig, or nil.
The first local role target named `<project>/task' is the convention
default (the mockup §6a `derived for this rig' tag); any other role target
answers next; backends and worktrees are explicit choices, never the
derived default."
  (let* ((project (beads-sling--project-label))
         (preferred (format "%s/task" project))
         (roles (cl-remove-if-not (lambda (target)
                                    (eq (oref target kind) 'role))
                                  (beads-sling-targets))))
    (or (and (cl-find preferred roles
                      :key (lambda (target) (oref target name))
                      :test #'equal)
             preferred)
        (and roles (oref (car roles) name)))))

(defun beads-sling--target-worktree (target)
  "Return TARGET's worktree path when it is a worktree target, or nil."
  (and (eq (oref target kind) 'worktree)
       (plist-get (oref target metadata) :path)))

(defun beads-sling--read-target-name ()
  "Read a sling target name with completion, or nil when none exist."
  (require 'beads-completion)
  (beads-completion-read-sling-target "Target: "))

;;; Formula-var scope seeds

(defun beads-sling--slug (text)
  "Return TEXT's repo-practice slug: downcased, non-alnum runs to `-'."
  (string-trim (replace-regexp-in-string
                "[^[:alnum:]]+" "-" (downcase (or text "")))
               "-" "-"))

(defun beads-sling--title-slug (work formula &optional title)
  "Return the `artifact_root' seed for WORK/FORMULA: `plans/<slug>/'.
The slug prefers TITLE, then freeform WORK, then FORMULA; nothing
derivable yields nil and the var keeps its declared default."
  (let ((slug (cl-find-if (lambda (text)
                            (and text (not (string-empty-p text))))
                          (list (and title (beads-sling--slug title))
                                (and (beads-sling--freeform-p work)
                                     (beads-sling--slug work))
                                (and formula (beads-sling--slug formula))))))
    (and slug (format "plans/%s/" slug))))

(defun beads-sling--target-rig (target)
  "Return the rig prefix of TARGET, or nil.
`rig/agent' yields `rig'."
  (and (stringp target)
       (string-match "\\`\\([^/[:space:]]+\\)/[^/]+\\'" target)
       (match-string 1 target)))

(defun beads-sling--var-seed (var scope)
  "Return the scope-derived seed value for VAR in SCOPE, or nil.
Seeds the two conventions the mockups show: `artifact_root' from the
work bead's title slug and `*_target' vars from the chosen target."
  (let ((name (or (oref var name) ""))
        (target (plist-get scope :target))
        (work (plist-get scope :work))
        (formula (plist-get scope :formula))
        (title (plist-get scope :work-title)))
    (cond
     ((equal name "artifact_root")
      (beads-sling--title-slug work formula title))
     ((equal name "rig_name")
      (beads-sling--target-rig target))
     ((and (string-suffix-p "_target" name)
           (beads-sling--target-rig target))
      target))))

;;; Typed formula-var infixes (REQ-009)

(defclass beads-sling--var-option (transient-option)
  ((var-name
    :initarg :var-name :initform nil
    :documentation "Variable name — the `--var NAME=' key.")
   (var-description
    :initarg :var-description :initform nil
    :documentation "The var's description; the read prompt.")
   (var-default
    :initarg :var-default :initform nil
    :documentation "The var's declared default.")
   (var-required
    :initarg :var-required :initform nil
    :documentation "Non-nil when dispatch refuses to run without a value.")
   (var-pattern
    :initarg :var-pattern :initform nil
    :documentation "Regexp the value must match, when declared.")
   (var-choices
    :initarg :var-choices :initform nil
    :documentation "Declared choice list, when the var is an enum.")
   (var-seed
    :initarg :var-seed :initform nil
    :documentation "Scope-derived initial value, overriding the default."))
  :abstract t
  :documentation "Base class of the generated formula-var infixes.
One infix per declared var, class per shape: an enum restricted to its
choices, a `true'/`false' toggle, a typed file, directory, agent or
numeric option, or a plain string option.  An unrecognised var fails
soft to string entry.")

(defclass beads-sling--enum-option (beads-sling--var-option) ()
  :documentation "A var with declared choices; input is restricted to them.")

(defclass beads-sling--bool-option (beads-sling--var-option) ()
  :documentation "A var whose type is boolean; a true/false toggle.")

(defclass beads-sling--string-option (beads-sling--var-option) ()
  :documentation "A plain string var; the fail-soft class of the heuristic.")

(defclass beads-sling--file-option (beads-sling--var-option) ()
  :documentation "A var naming a file, read with `read-file-name'.")

(defclass beads-sling--directory-option (beads-sling--var-option) ()
  :documentation "A var naming a directory, read with `read-directory-name'.")

(defclass beads-sling--agent-option (beads-sling--var-option) ()
  :documentation "A var naming a sling target agent.")

(defclass beads-sling--numeric-option (beads-sling--var-option) ()
  :documentation "A numeric var; non-digit entry is refused.")

(defun beads-sling--var-class (var)
  "Return the infix class that reads VAR.
The declared shape wins — an enum list, a boolean or integer
`var-type' — then the naming conventions: `context_path' and any
`*_path' a file option, `artifact_root' a directory option, any
`*_target' an agent option, numeric-looking defaults or `max_'/`_iterations'
names a numeric option.  Anything unrecognised fails soft to the
plain string option."
  (let ((name (or (oref var name) ""))
        (default (oref var default))
        (type (oref var var-type)))
    (cond
     ((oref var enum) 'beads-sling--enum-option)
     ((equal type "bool") 'beads-sling--bool-option)
     ((equal type "int") 'beads-sling--numeric-option)
     ((or (equal name "context_path")
          (string-suffix-p "_path" name))
      'beads-sling--file-option)
     ((equal name "artifact_root") 'beads-sling--directory-option)
     ((string-suffix-p "_target" name) 'beads-sling--agent-option)
     ((or (and default (string-match-p "\\`[0-9]+\\'" default))
          (string-prefix-p "max_" name)
          (string-suffix-p "_iterations" name))
      'beads-sling--numeric-option)
     (t 'beads-sling--string-option))))

(defun beads-sling--class-tag (class)
  "Return CLASS's mockup type tag (`[file]' …), or nil."
  (pcase class
    ('beads-sling--file-option "[file]")
    ('beads-sling--directory-option "[dir]")
    ('beads-sling--agent-option "[agent]")
    ('beads-sling--numeric-option "[numeric]")
    (_ nil)))

(defun beads-sling--var-description (var)
  "Return the infix description for VAR, carrying its metadata."
  (concat (or (oref var name) "var")
          (and (oref var required) " (required)")
          (when-let* ((description (oref var description)))
            (concat " — " description))
          (when-let* ((default (oref var default))
                      ((not (string-empty-p default))))
            (concat " [default: " default "]"))))

(defun beads-sling--workdir ()
  "Return the directory file/directory reads complete against.
The chosen worktree target's path when there is one, else the
project root, else `default-directory'."
  (or (when-let* ((target (beads-sling--target-get
                           (plist-get (transient-scope) :target))))
        (beads-sling--target-worktree target))
      (ignore-errors (beads-git-find-project-root))
      default-directory))

(defun beads-sling--history (obj)
  "Return OBJ's per-(formula, var) minibuffer history variable."
  (let ((sym (intern (format "beads-sling-history-%s-%s"
                             (or (plist-get (transient-scope) :formula) "")
                             (or (oref obj var-name) "")))))
    (unless (boundp sym) (set sym nil))
    sym))

(defun beads-sling--var-initial (obj)
  "Return OBJ's read-time initial input: value, seed, then default."
  (or (oref obj value)
      (and (not (beads-sling--blank (oref obj var-seed)))
           (oref obj var-seed))
      (and (not (beads-sling--blank (oref obj var-default)))
           (oref obj var-default))))

(defun beads-sling--check-pattern (name pattern value)
  "Signal `user-error' when PATTERN rejects NAME's VALUE."
  (when (and pattern (not (beads-sling--blank value)))
    (condition-case nil
        (unless (string-match pattern value)
          (user-error "Var %s does not match pattern %s" name pattern))
      (invalid-regexp nil))))

(defun beads-sling--read-string (obj)
  "Read the string var of OBJ, validating its pattern on entry."
  (let ((value (read-from-minibuffer (transient-prompt obj)
                                     (beads-sling--var-initial obj)
                                     nil nil (beads-sling--history obj))))
    (when (and (stringp value) (not (string-empty-p value)))
      (beads-sling--check-pattern (oref obj var-name) (oref obj var-pattern) value)
      value)))

(defun beads-sling--read-file (obj)
  "Read the file var of OBJ against the target rig's workdir."
  (let* ((dir (beads-sling--workdir))
         (default-directory dir)
         (insert-default-directory nil)
         (value (file-local-name
                 (read-file-name (transient-prompt obj) dir nil nil
                                 (beads-sling--var-initial obj)))))
    (unless (and (stringp value) (string-empty-p value)) value)))

(defun beads-sling--read-directory (obj)
  "Read the directory var of OBJ against the target rig's workdir."
  (let* ((dir (beads-sling--workdir))
         (default-directory dir)
         (insert-default-directory nil)
         (value (file-local-name
                 (read-directory-name (transient-prompt obj) dir nil nil
                                      (beads-sling--var-initial obj)))))
    (unless (and (stringp value) (string-empty-p value)) value)))

(defun beads-sling--read-agent (obj)
  "Read the agent var of OBJ with the sling target completion."
  (require 'beads-completion)
  (let ((value (completing-read (transient-prompt obj)
                                (beads-completion-sling-target-table)
                                nil nil (beads-sling--var-initial obj)
                                (beads-sling--history obj))))
    (unless (string-empty-p value) value)))

(defun beads-sling--read-numeric (obj)
  "Read the numeric var of OBJ, refusing anything else."
  (let ((value (read-from-minibuffer (transient-prompt obj)
                                     (beads-sling--var-initial obj)
                                     nil nil (beads-sling--history obj))))
    (when (and (stringp value) (not (string-empty-p value)))
      (unless (string-match-p "\\`[0-9]+\\'" value)
        (user-error "Var %s must be numeric (got %s)"
                    (or (oref obj var-name) "var") value))
      value)))

(defun beads-sling--read-guarded (obj read)
  "Run infix READ for OBJ, surviving a refused or aborted entry."
  (condition-case err
      (funcall read)
    ((user-error quit)
     (message "%s" (error-message-string err))
     (oref obj value))))

(cl-defmethod transient-prompt ((obj beads-sling--var-option))
  "Return OBJ's var description as the read prompt."
  (format "%s: " (or (oref obj var-description)
                      (format "Formula var %s" (oref obj var-name)))))

(cl-defmethod transient-init-value ((obj beads-sling--var-option))
  "Seed OBJ's value from the transient value, seed, then default."
  (cl-call-next-method)
  (when (null (oref obj value))
    (oset obj value (or (oref obj var-seed) (oref obj var-default)))))

(cl-defmethod transient-infix-read ((obj beads-sling--enum-option))
  "Read one of OBJ's declared choices."
  (beads-sling--read-guarded
   obj
   (lambda ()
     (completing-read (transient-prompt obj) (oref obj var-choices) nil t
                      (beads-sling--var-initial obj)
                      (beads-sling--history obj)))))

(cl-defmethod transient-infix-read ((obj beads-sling--bool-option))
  "Cycle OBJ's boolean value true -> false -> true."
  (pcase (oref obj value)
    ("true" "false")
    ("false" "true")
    (_ (or (beads-sling--var-initial obj) "true"))))

(cl-defmethod transient-infix-read ((obj beads-sling--string-option))
  "Read OBJ's string var."
  (beads-sling--read-guarded obj (lambda () (beads-sling--read-string obj))))

(cl-defmethod transient-infix-read ((obj beads-sling--file-option))
  "Read OBJ's file var."
  (beads-sling--read-guarded obj (lambda () (beads-sling--read-file obj))))

(cl-defmethod transient-infix-read ((obj beads-sling--directory-option))
  "Read OBJ's directory var."
  (beads-sling--read-guarded obj (lambda () (beads-sling--read-directory obj))))

(cl-defmethod transient-infix-read ((obj beads-sling--agent-option))
  "Read OBJ's agent var."
  (beads-sling--read-guarded obj (lambda () (beads-sling--read-agent obj))))

(cl-defmethod transient-infix-read ((obj beads-sling--numeric-option))
  "Read OBJ's numeric var."
  (beads-sling--read-guarded obj (lambda () (beads-sling--read-numeric obj))))

(defalias 'beads-sling--set-var #'transient--default-infix-command
  "Infix command for the generated formula-var options.")
(put 'beads-sling--set-var 'interactive-only t)

;;; Deterministic var keys

(defun beads-sling--char-combos (chars)
  "Return two-character strings drawn from CHARS in positional order."
  (let (combos)
    (cl-dotimes (i (length chars))
      (cl-do ((j (1+ i) (1+ j)))
          ((>= j (length chars)))
        (push (format "%c%c" (nth i chars) (nth j chars)) combos)))
    (nreverse combos)))

(defun beads-sling--var-key-natural (name used)
  "Return NAME's natural key candidate avoiding USED, or nil."
  (let* ((chars (seq-filter
                 (lambda (char) (string-match-p "[[:alnum:]]" (string char)))
                 (append name nil)))
         (free-single
          (lambda (key)
            (and (not (member key used))
                 (not (seq-some (lambda (other)
                                  (and (> (length other) 1)
                                       (string-prefix-p key other)))
                                used)))))
         (free-combo
          (lambda (key)
            (and (not (member (substring key 0 1) used))
                 (not (member key used))))))
    (or (seq-find free-single (and chars (list (string (car chars)))))
        (seq-find free-combo (beads-sling--char-combos chars)))))

(defun beads-sling--var-key (name used)
  "Return an unused transient key for NAME, avoiding USED."
  (or (beads-sling--var-key-natural name used)
      (let* ((chars (seq-filter
                     (lambda (char) (string-match-p "[[:alnum:]]" (string char)))
                     (append name nil)))
             (prefix (or (seq-find (lambda (p) (not (member p used)))
                                   (append (mapcar #'string chars)
                                           '("v" "z" "k" "j" "h")))
                         "v")))
        (let ((n 1))
          (while (member (format "%s%d" prefix n) used)
            (setq n (1+ n)))
          (format "%s%d" prefix n)))))

(defun beads-sling--assign-var-keys (vars reserved)
  "Return one (KEY . NATURAL-P) per var of VARS, avoiding RESERVED."
  (let ((used (copy-sequence reserved))
        assigned)
    (dolist (var vars)
      (let* ((name (or (oref var name) ""))
             (key (beads-sling--var-key name used)))
        (push (cons key (and (beads-sling--var-key-natural name used) t))
              assigned)
        (cl-pushnew key used :test #'equal)))
    (nreverse assigned)))

(defun beads-sling--var-infix-spec (var key &optional scope)
  "Return the raw transient infix spec for VAR bound to KEY in SCOPE."
  (let* ((name (oref var name))
         (class (beads-sling--var-class var))
         (tag (beads-sling--class-tag class)))
    (list key
          (concat (beads-sling--var-description var) (and tag (concat "  " tag)))
          'beads-sling--set-var
          :class class
          :argument (format "--var %s=" name)
          :var-name name
          :var-description (oref var description)
          :var-default (oref var default)
          :var-required (oref var required)
          :var-pattern (oref var pattern)
          :var-choices (oref var enum)
          :var-seed (beads-sling--var-seed var scope))))

(defun beads-sling--var-infix-assignments (formula reserved &optional scope)
  "Return one (KEY INFIX-SPEC NATURAL-P) per var of FORMULA, or nil.
RESERVED is the statically bound key list and SCOPE the menu scope."
  (let ((vars (and formula (oref formula vars))))
    (when vars
      (let ((assigned (beads-sling--assign-var-keys vars reserved)))
        (cl-mapcar (lambda (var ass)
                     (list (car ass)
                           (beads-sling--var-infix-spec var (car ass) scope)
                           (cdr ass)))
                   vars assigned)))))

(defun beads-sling--var-children (formula reserved &optional scope)
  "Return the raw How group spec for FORMULA, or nil.
RESERVED is the statically bound key list and SCOPE the menu scope.
The group is titled `How — <formula> vars'; a formula without vars
renders no How section.  Vars whose natural key candidates run out
render in a grouped `…' overflow subgroup."
  (when-let* ((assignments
               (beads-sling--var-infix-assignments formula reserved scope)))
    (let ((main (delq nil (mapcar (lambda (a) (and (nth 2 a) (nth 1 a)))
                                  assignments)))
          (overflow (delq nil (mapcar (lambda (a) (and (null (nth 2 a))
                                                      (nth 1 a)))
                                      assignments))))
      (if (null overflow)
          (apply #'vector
                 (format "How — %s vars" (or (oref formula name) "formula"))
                 main)
        (apply #'vector
               (format "How — %s vars" (or (oref formula name) "formula"))
               :class 'transient-subgroups
               (list (apply #'vector main)
                     (apply #'vector
                            (format "…  (%d more vars)" (length overflow))
                            overflow)))))))

;;; Live values, context and footer

(defun beads-sling--current-values (recipe)
  "Return the set formula-var values as a (NAME . VALUE) alist.
Parses the transient's `--var name=value' args, keeping only values
of infixes generated from RECIPE's vars; an empty value counts as
unset.  A value for a formula the user has since re-picked never
leaks into the next dispatch."
  (let ((names (mapcar (lambda (var) (oref var name))
                       (or (and recipe (oref recipe vars)) '()))))
    (delq nil
          (mapcar
           (lambda (arg)
             (and (string-prefix-p "--var " arg)
                  (let* ((kv (substring arg (length "--var ")))
                         (split (string-search "=" kv)))
                    (and split
                         (member (substring kv 0 split) names)
                         (let ((value (substring kv (1+ split))))
                           (and (not (beads-sling--blank value))
                                (cons (substring kv 0 split) value)))))))
           (transient-args 'beads-sling--transient)))))

(defun beads-sling--scope-context (&optional scope)
  "Return the dispatch context plist describing SCOPE.
The context is the single value the header, footer, preview and
launch share: work, formula, recipe, target, resolved var values and
the inferred shape."
  (let* ((scope (or scope (transient-scope)))
         (work (plist-get scope :work))
         (formula (plist-get scope :formula))
         (recipe (plist-get scope :recipe))
         (target (or (plist-get scope :target)
                     (beads-sling--derived-target-name))))
    (list :work work
          :work-title (plist-get scope :work-title)
          :formula formula
          :recipe recipe
          :target target
          :values (beads-sling--current-values recipe)
          :shape (beads-sling-shape work formula))))

(defun beads-sling--missing-required-vars (recipe values)
  "Return the names of RECIPE's required vars missing from VALUES."
  (delq nil
        (mapcar (lambda (var)
                  (and (oref var required)
                       (beads-sling--blank (cdr (assoc (oref var name) values)))
                       (oref var name)))
                (or (and recipe (oref recipe vars)) '()))))

(defun beads-sling--warnings (context)
  "Return the client-side warnings for CONTEXT.
Built-in checks plus the `beads-sling-validators' hook.  Warnings
never refuse a dispatch; they feed the live footer and the preview."
  (let* ((shape (plist-get context :shape))
         (work (plist-get context :work))
         (formula (plist-get context :formula))
         (target (plist-get context :target))
         (recipe (plist-get context :recipe))
         (values (plist-get context :values))
         (missing (beads-sling--missing-required-vars recipe values))
         (warnings
          (delq nil
                (list
                 (when (and (memq shape '(plain on))
                            (beads-sling--blank target))
                   "No target chosen — pick one with T")
                 (when (and (eq shape 'on) (beads-sling--blank work))
                   (format "%s drains a bead — pick work with A (or point at one)"
                           formula))
                 (when missing
                   (format "Missing required vars: %s"
                           (mapconcat #'identity missing ", ")))))))
    (append warnings (beads-sling-validate context))))

(defun beads-sling--footer (context)
  "Return the live footer line(s) for CONTEXT.
`✓ Ready — <shape> · target <name> · <detail>' when every client-side
check is clean, else one `⚠ <reason>' line per warning (mockup §6f).
Recomputed on every redraw, never a blocker."
  (let ((warnings (beads-sling--warnings context)))
    (if warnings
        (mapconcat (lambda (warning) (concat "⚠ " warning)) warnings "\n")
      (let* ((shape (plist-get context :shape))
             (target (plist-get context :target))
             (recipe (plist-get context :recipe))
             (values (plist-get context :values))
             (target-word (if (beads-sling--blank target) "local" target))
             (word (pcase shape
                     ('plain "local route")
                     ('on "on run")
                     ('formula "formula run")
                     (_ "run")))
             (detail
              (cond
               ((and (eq shape 'plain) (beads-sling--freeform-p
                                        (plist-get context :work)))
                "freeform work")
               ((and (eq shape 'plain)
                     (string-prefix-p "worktree: " (or target "")))
                target)
               (recipe
                (let ((vars (oref recipe vars)))
                  (if vars
                      (format "%d of %d vars set" (length values) (length vars))
                    "0 vars")))
               (t "no vars"))))
        (format "✓ Ready — %s · target %s · %s" word target-word detail)))))

;;; Formula picking

(defun beads-sling--formula-choices ()
  "Return an alist of (NAME . DESCRIPTION) for every formula, or nil."
  (ignore-errors
    (mapcar (lambda (summary)
              (cons (oref summary name) (or (oref summary description) "")))
            (beads-command-execute (beads-command-formula-list :json t)))))

(defun beads-sling--read-formula ()
  "Read a formula name with completion, or signal when none exist."
  (let ((choices (beads-sling--formula-choices)))
    (unless choices
      (user-error "No formulas available"))
    (completing-read
     "Formula: " choices nil t nil 'beads-sling-formula-history)))

(defun beads-sling--fetch-formula (name)
  "Return the `beads-formula' named NAME, or nil on failure."
  (ignore-errors
    (beads-command-execute
     (beads-command-formula-show :formula-name name :json t))))

;;; Launch

(defun beads-sling--launch-prompt (context)
  "Return the user prompt for CONTEXT, or nil.
Freeform work is the prompt itself on the plain path; a formula path
renders the formula and its set vars so a local agent sees them."
  (let ((shape (plist-get context :shape))
        (work (plist-get context :work))
        (formula (plist-get context :formula))
        (values (plist-get context :values)))
    (cond
     ((memq shape '(on formula))
      (string-join
       (append (list (format "Run formula %s." formula))
               (when values
                 (list "" "Variables:")
                 (mapcar (lambda (pair)
                           (format "- %s=%s" (car pair) (cdr pair)))
                         values)))
       "\n"))
     ((beads-sling--freeform-p work) work)
     (t nil))))

(defun beads-sling--launch-formula (context target)
  "Launch a formula-only dispatch for CONTEXT locally in TARGET.
Without a work bead there is no issue to attach to; a local agent is
started in the target worktree (or the project root) instead."
  (let* ((backend-name (beads-sling--local-backend-name (oref target backend)))
         (backend (if backend-name
                      (beads-agent--get-backend backend-name)
                    (beads-agent--select-backend
                     (beads-agent-type-get "Task"))))
         (root (beads-git-find-project-root))
         (dir (or (beads-sling--target-worktree target) root)))
    (ignore context)
    (beads-agent--start-project-agent backend root dir)))

(defun beads-sling--launch (context)
  "Launch CONTEXT through the sling dispatch seam.
The plain and on shapes dispatch the work bead (and prompt) to the
resolved target; the formula-only shape starts a local agent in the
target worktree.  A missing target is read interactively."
  (let* ((shape (plist-get context :shape))
         (target-name (plist-get context :target))
         (target (beads-sling--target-get
                  (or target-name (beads-sling--read-target-name)))))
    (unless target (user-error "No sling target selected"))
    (let ((prompt (beads-sling--launch-prompt context)))
      (pcase shape
        ((or 'plain 'on)
         (beads-sling-dispatch target (plist-get context :work) prompt))
        ('formula (beads-sling--launch-formula context target))
        (_ (user-error "Nothing to sling — pick work or a formula"))))))

;;; Transient suffixes

(defun beads-sling--resetup (scope)
  "Re-setup the sling transient with SCOPE, carrying current values."
  (transient-setup 'beads-sling--transient nil nil
                   :scope scope
                   :value (transient-args 'beads-sling--transient)))

(transient-define-suffix beads-sling--pick-work (&optional freeform)
  "Pick the What work for the sling; FREEFORM enters free text."
  (interactive "P")
  (let* ((scope (transient-scope))
         (work (if freeform
                   (read-string "Work (freeform): ")
                 (beads-completion-read-issue "Work: "))))
    (beads-sling--resetup
     (plist-put (plist-put (copy-sequence scope) :work work)
                :work-title nil))))

(transient-define-suffix beads-sling--pick-formula ()
  "Pick the formula for the sling and rebuild the menu in place."
  (interactive)
  (let* ((scope (transient-scope))
         (name (beads-sling--read-formula))
         (recipe (beads-sling--fetch-formula name)))
    (beads-sling--resetup
     (plist-put (plist-put (copy-sequence scope) :formula name)
                :recipe recipe))))

(transient-define-suffix beads-sling--pick-target ()
  "Pick the Who target for the sling and rebuild the menu in place."
  (interactive)
  (let* ((scope (transient-scope))
         (name (beads-sling--read-target-name)))
    (beads-sling--resetup
     (plist-put (copy-sequence scope) :target name))))

(transient-define-suffix beads-sling--run ()
  "Launch the sling exactly as shown (never gated by the preview)."
  (interactive)
  (beads-sling--launch (beads-sling--scope-context)))

(transient-define-suffix beads-sling--reset ()
  "Clear the work, formula and target; rebuild the menu from scratch."
  (interactive)
  (let ((directory (plist-get (transient-scope) :directory)))
    (beads-sling--resetup (list :directory directory
                                :work nil :work-title nil
                                :formula nil :recipe nil :target nil))))

(transient-define-suffix beads-sling--recipe-preview ()
  "Open the picked formula's recipe in the formula show buffer."
  (interactive)
  (let ((formula (plist-get (transient-scope) :formula)))
    (unless formula (user-error "No formula chosen — pick one with f"))
    (beads-formula-show formula)))

(transient-define-suffix beads-sling--show-preview ()
  "Open the full sling preview in a special-mode buffer."
  (interactive)
  (beads-sling--full-preview (beads-sling--scope-context)))

;;; Adaptive layout

(defun beads-sling--children-specs (scope)
  "Return the stacked layout specs for SCOPE (mockup §6).
Stages collapse as they are answered: each stage's line carries its
answer, the How group appears only once a formula with vars is picked,
the routing flags render only on the settled plain shape."
  (let* ((work (plist-get scope :work))
         (formula (plist-get scope :formula))
         (recipe (plist-get scope :recipe))
         (target (or (plist-get scope :target)
                     (beads-sling--derived-target-name)))
         (shape (beads-sling-shape work formula)))
    (ignore target)
    (append
     (list
      (vector (format "Sling — %s" (beads-sling--project-label))
              (list :info
                    (lambda ()
                      (beads-sling--header-sentence
                       (plist-get scope :work)
                       (plist-get scope :formula)
                       (or (plist-get scope :target)
                           (beads-sling--derived-target-name))
                       (plist-get scope :recipe))))
              (list :info
                    (lambda ()
                      (beads-sling--footer
                       (beads-sling--scope-context scope))))))
     (list
      (vector "What"
              (list "A"
                    (lambda (_obj)
                      (concat "Work: "
                              (beads-sling--work-label
                               (plist-get scope :work)
                               (plist-get scope :work-title))))
                    'beads-sling--pick-work)
              (list "f"
                    (lambda (_obj)
                      (concat "Formula: "
                              (beads-sling--formula-label
                               (plist-get scope :formula)
                               (plist-get scope :recipe))))
                    'beads-sling--pick-formula)))
     (when-let* ((how (beads-sling--var-children
                       recipe beads-sling--reserved-keys scope)))
       (list how))
     (list
      (vector "Who"
              (list "T"
                    (lambda (_obj)
                      (concat "Target: "
                              (beads-sling--target-label
                               (or (plist-get scope :target)
                                   (beads-sling--derived-target-name)))))
                    'beads-sling--pick-target)))
     (when (eq shape 'plain)
       (list
        (vector "Routing flags"
                '("-w" "Use worktree" "--worktree")
                '("-b" "Branch" "--branch=")
                '("-n" "Nudge target after launch" "--nudge"))))
     (list
      (vector "Actions"
              '("s" "Launch" beads-sling--run)
              '("P" "Full preview" beads-sling--show-preview)
              (and formula '("r" "Recipe preview" beads-sling--recipe-preview))
              '("x" "Reset" beads-sling--reset)
              '("q" "Quit" transient-quit-one))))))

(defun beads-sling--setup-children (_children)
  "Parse `beads-sling--children-specs' for the live scope."
  (transient-parse-suffixes
   'beads-sling--transient
   (beads-sling--children-specs (transient-scope))))

(beads-define-prefix beads-sling--transient ()
  "Adaptive sling transient (REQ-009, mockup §6).
One entry point covers the plain, formula and `--on' shapes: shape is
inferred and shown as one sentence, stages collapse as answered, and
the live footer recomputes on every redraw.  `P' opens a fuller
preview and never gates `s'."
  [ :class transient-subgroups :setup-children beads-sling--setup-children ])

(defun beads-sling--work-at-point ()
  "Return the bead id at point, or nil."
  (ignore-errors (beads-agent--detect-issue-id)))

(defun beads-sling--initial-scope ()
  "Return the sling scope seeded from the buffer at point."
  (list :work (beads-sling--work-at-point)
        :work-title nil
        :formula nil
        :recipe nil
        :target nil))

;;;###autoload
(defun beads-sling ()
  "Sling a bead, freeform work or formula to an agent target.
The one entry point (REQ-009): shape is inferred from the What stage,
stages collapse as answered, and the live footer always shows the
pending dispatch.  `s' launches, `P' opens the full preview."
  (interactive)
  (transient-setup 'beads-sling--transient nil nil :scope
                   (beads-sling--initial-scope)))

;;; Full preview (`P', mockup §7)

(defconst beads-sling-preview-buffer-name "*beads-sling: preview*"
  "Base name of the full sling preview buffer.")

(defvar-local beads-sling-preview--context nil
  "The dispatch context this preview buffer previews.")

(defun beads-sling--preview-recipe-lines (context)
  "Return CONTEXT's recipe `step needs …' lines."
  (let ((recipe (plist-get context :recipe)))
    (if (null recipe)
        (list "  (no formula picked — this dispatch is plain)")
      (let ((steps (oref recipe steps)))
        (if (null steps)
            (list "  (no steps)")
          (mapcar (lambda (step)
                    (format "  %-24s needs %s"
                            (or (oref step title) (oref step id) "step")
                            (if-let* ((needs (oref step needs)))
                                (mapconcat #'identity needs ", ")
                              "nothing")))
                  steps))))))

(defun beads-sling--preview-validation-lines (context)
  "Return CONTEXT's Validation section lines."
  (let ((warnings (beads-sling--warnings context)))
    (if warnings
        (mapcar (lambda (warning) (concat "⚠ " warning)) warnings)
      (list (format "✓ target %s is available"
                    (or (plist-get context :target) "local"))
            "✓ no cross-store route"
            (let ((recipe (plist-get context :recipe)))
              (if recipe
                  (let ((required
                         (delq nil (mapcar
                                    (lambda (var)
                                      (and (oref var required) (oref var name)))
                                    (or (oref recipe vars) '())))))
                    (if required
                        (format "✓ required vars set: %s"
                                (mapconcat #'identity required ", "))
                      "✓ no required vars"))
                "✓ no vars"))))))

(defun beads-sling--preview-lines (context)
  "Return the full preview's content lines for CONTEXT (mockup §7)."
  (let ((values (or (plist-get context :values) '())))
    (append
     (list (concat "  " (beads-sling--header-sentence
                          (plist-get context :work)
                          (plist-get context :formula)
                          (plist-get context :target)
                          (plist-get context :recipe)))
           ""
           "Validation")
     (mapcar (lambda (line) (concat "  " line))
             (beads-sling--preview-validation-lines context))
     (list ""
           (format "Recipe — %s (steps → needs)"
                   (or (plist-get context :formula) "none picked (plain dispatch)")))
     (beads-sling--preview-recipe-lines context)
     (list ""
           "Routing plan (dry run)"
           (format "  Work:   %s" (or (plist-get context :work) "(none)"))
           (format "  Target: %s" (or (plist-get context :target) "(none)"))
           (format "  Vars:   %s"
                   (if values
                       (mapconcat (lambda (pair)
                                    (format "%s=%s" (car pair) (cdr pair)))
                                  values " ")
                     "(none)"))))))

(defun beads-sling--preview-paint (buffer context)
  "Paint BUFFER's client-side sections for CONTEXT; return BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Sling preview — %s  (s launch · q quit)\n"
                      (beads-sling--project-label))
              (make-string 72 ?─) "\n")
      (dolist (line (beads-sling--preview-lines context))
        (insert line "\n"))
      (goto-char (point-min))))
  buffer)

(defun beads-sling--full-preview (context)
  "Open the full preview buffer for CONTEXT (never a gate)."
  (let ((buffer (get-buffer-create beads-sling-preview-buffer-name)))
    (with-current-buffer buffer
      (beads-sling-preview-mode)
      (setq beads-sling-preview--context context)
      (beads-sling--preview-paint buffer context))
    (pop-to-buffer buffer)))

(defun beads-sling-preview-launch ()
  "Launch exactly the dispatch this preview buffer previews."
  (interactive)
  (unless beads-sling-preview--context
    (user-error "Nothing to launch from this preview"))
  (beads-sling--launch beads-sling-preview--context))

(defvar-keymap beads-sling-preview-mode-map
  :doc "Keymap of the full sling preview buffer.\n`s' launches, `q' quits."
  "s" #'beads-sling-preview-launch
  "q" #'quit-window)

(define-derived-mode beads-sling-preview-mode special-mode "Beads-Sling-Preview"
  "Read-only full preview of the pending sling (mockup §7).
The buffer shows the header sentence, the client-side validation
checks in full, the recipe's steps and the client-side routing plan.
`s' launches exactly what was previewed; `q' quits.  The preview is
never a gate: the menu's `s' works with or without it."
  (setq-local truncate-lines nil))

(provide 'beads-sling)
;;; beads-sling.el ends here
