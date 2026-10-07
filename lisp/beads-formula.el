;;; beads-formula.el --- Formula browser, detail and launch UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The formula browser/detail/instantiate UI and ABI (design.md §4.7,
;; §5.1, §8.1; REQ-013, REQ-014, REQ-SF-013..016).  The `bd formula'
;; command classes stay in `beads-command-formula.el'; this module owns
;; the presentation and the extension seams.
;;
;; ABI shared with the sling flow:
;;
;; - `beads-formula-var-reader' (in `beads-formula-var-reader.el') maps a
;;   formula variable's declared metadata to a reader kind, so a var is
;;   read the same way in the browser and in the sling How stage.
;; - `beads-formula-launch-context' is the resolved launch (shape, phase,
;;   var values, assignee, target, warnings) shared with the sling
;;   preview.
;; - `beads-formula-instantiate' is the phase-aware instantiate generic:
;;   `pour', `wisp' or `wisp-root-only', dispatched to `bd mol pour' /
;;   `bd mol wisp'.  `beads-formula-launch' delegates to it with `pour'.
;; - `beads-formula-follow' opens the created molecule run view.
;;
;; Standalone behaviour: with no extension package loaded, browsing,
;; inspecting and instantiating a formula all work with `bd' alone.
;;
;; Key bindings added to the existing formula buffers:
;;
;;   l  seed the sling flow with the formula at point (list/detail)
;;   s  instantiate the formula standalone (list/detail)

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)
(require 'transient)
(require 'beads-util)
(require 'beads-types)
(require 'beads-command)
(require 'beads-command-formula)
(require 'beads-command-mol)
(require 'beads-formula-var-reader)
(require 'beads-prefix)

;;; Forward Declarations

(declare-function beads-sling-shape "beads-sling" (work formula))
(declare-function beads-molecule-open "beads-molecule" (root &rest _args))

(defvar beads-formula-list-mode-map)
(defvar beads-formula-show-mode-map)

;;; ============================================================
;;; Launch Context
;;; ============================================================

(defclass beads-formula-launch-context ()
  ((shape
    :initarg :shape
    :initform nil
    :type (or null symbol)
    :documentation "Inferred shape: `formula' (standalone) or `on' (against a bead).")
   (vars
    :initarg :vars
    :initform nil
    :type list
    :documentation "Resolved variable values as a (NAME . VALUE) alist.")
   (target
    :initarg :target
    :initform nil
    :type (or null string)
    :documentation "Resolved sling target name, or nil for local-only.")
   (warnings
    :initarg :warnings
    :initform nil
    :type list
    :documentation "Human-readable warnings collected before launch.")
   (phase
    :initarg :phase
    :initform nil
    :type (or null symbol)
    :documentation "Explicit instantiate phase (`pour', `wisp' or `wisp-root-only'), or nil.")
   (assignee
    :initarg :assignee
    :initform nil
    :type (or null string)
    :documentation "Root assignee (`--assignee'), or nil for unassigned.")
   (for-agent
    :initarg :for
    :initform nil
    :type (or null string)
    :documentation "`--for' agent query scope passed to the run view, or nil."))
  "The resolved formula launch shared with the sling flow (design.md §4.7).")

;;; ============================================================
;;; Phase
;;; ============================================================

(defun beads-formula-phase (formula)
  "Return FORMULA's declared phase, or nil.
FORMULA may be a `beads-formula', a `beads-formula-summary' or a name.
The `phase' slot is owned by the provenance extension (WI-SF-05); when
the slot is absent this returns nil rather than signalling, so the
instantiate flow works before that extension lands."
  (when (and (eieio-object-p formula) (slot-exists-p formula 'phase))
    (oref formula phase)))

(defconst beads-formula-instantiate-phases '(pour wisp wisp-root-only)
  "The explicit instantiate phases accepted by `beads-formula-instantiate'.")

(defun beads-formula-phase-recommendation (formula)
  "Return the recommended instantiate phase symbol for FORMULA.
A `vapor'-phase formula recommends `wisp'; anything else recommends
`pour'.  The recommendation is only a default: the operator always
chooses the phase explicitly (REQ-SF-013)."
  (if (equal (beads-formula-phase formula) "vapor") 'wisp 'pour))

(defun beads-formula-phase-warning (formula phase)
  "Return the phase-conflict warning for FORMULA and PHASE, or nil.
Pouring a vapor-phase formula is allowed but warns, mirroring `bd'.
Phase is compared against the `wisp' family so both `wisp' and
`wisp-root-only' satisfy a vapor recommendation."
  (when (and (eq phase 'pour)
             (equal (beads-formula-phase formula) "vapor"))
    (format (concat "Vapor-phase formula %s: wisp is recommended. "
                    "Pour will warn: \"formula %s declares phase=vapor; "
                    "pour creates persistent work.\"")
            (beads-formula--name formula)
            (beads-formula--name formula))))

;;; ============================================================
;;; Grouping and Detail
;;; ============================================================

(defconst beads-formula-group-order '("workflow" "expansion" "aspect")
  "Preferred display order of formula types in the browser.")

(defconst beads-formula-browser-format
  (vector (list "Name" 24 t)
          (list "Type" 12 t)
          (list "Steps" 6 t :right-align t)
          (list "Vars" 5 t :right-align t)
          (list "Phase" 8 t)
          (list "Source" 45 t))
  "Column layout for the grouped formula browser.
Columns: Name, Type, Steps, Vars, Phase, Source (menu-mockups §1).")

;;; Scope and shadowing (REQ-SF-010)

(defun beads-formula--scope-dirs (&optional project-root)
  "Return the formula search directories as an alist of scope to dir.
PROJECT-ROOT defaults to the current project root.  The scopes are
`project', `user' and `gt', in `bd' priority order."
  (let* ((root (or project-root (beads--project-root) default-directory))
         (gt (getenv "GT_ROOT")))
    (list (cons 'project (expand-file-name ".beads/formulas" root))
          (cons 'user (expand-file-name "~/.beads/formulas"))
          (cons 'gt (when gt (expand-file-name ".beads/formulas" gt))))))

(defun beads-formula-scope-of (source &optional project-root)
  "Return the scope symbol for SOURCE.
One of `project', `user', `gt' or `other', determined by which
search-path directory SOURCE lives under (REQ-SF-010).  PROJECT-ROOT
locates the project search path."
  (let ((dirs (beads-formula--scope-dirs project-root))
        (path (and source (expand-file-name source))))
    (if (null path)
        'other
      (or (cl-loop for (scope . dir) in dirs
                   when (and dir
                             (string-prefix-p (file-name-as-directory dir) path))
                   return scope)
          'other))))

(defun beads-formula-filter-by-scope (formulas scope &optional project-root)
  "Filter FORMULAS to SCOPE (`project', `user', `gt' or `all').
PROJECT-ROOT locates the project search path.  `all' keeps every
formula, matching the browser default (REQ-SF-010)."
  (if (eq scope 'all)
      formulas
    (seq-filter (lambda (formula)
                  (eq (beads-formula-scope-of (oref formula source) project-root)
                      scope))
                formulas)))

(defun beads-formula--formula-files (dir)
  "Return the formula file names directly under DIR.
Only file names are inspected, never their contents."
  (when (file-directory-p dir)
    (seq-filter (lambda (name)
                  (or (string-suffix-p ".formula.toml" name)
                      (string-suffix-p ".formula.json" name)))
                (directory-files dir nil directory-files-no-dot-files-regexp))))

(defun beads-formula-shadow-index (&optional project-root)
  "Return a hash mapping formula name to its source paths in priority order.
The index is built from the search-path directories by file name only
\(no TOML is parsed); it powers the `shadowed' marker (REQ-SF-010).
PROJECT-ROOT locates the project search path."
  (let ((table (make-hash-table :test #'equal))
        (dirs (beads-formula--scope-dirs project-root)))
    (dolist (pair dirs)
      (let ((dir (cdr pair)))
        (when dir
          (dolist (file (beads-formula--formula-files dir))
            (let* ((name (file-name-sans-extension
                          (file-name-sans-extension file)))
                   (path (expand-file-name file dir)))
              (puthash name (append (gethash name table) (list path))
                       table))))))
    table))

(defun beads-formula-shadowed-by (formula &optional index)
  "Return the lower-priority source paths shadowed by FORMULA.
INDEX is a `beads-formula-shadow-index' for the current project.  A
formula shadows same-name files that appear later on the search path;
the result is nil when it is the only declaration of its name."
  (let* ((name (oref formula name))
         (source (and (oref formula source)
                      (expand-file-name (oref formula source))))
         (paths (and index (gethash name index))))
    (when (and source (> (length paths) 1))
      (cdr (member source paths)))))

;;; Browser entries

(defvar-local beads-formula-browser-scope 'all
  "Scope filter for the grouped formula browser.
One of `project', `user' or `all' (REQ-SF-010).")

(defvar-local beads-formula-browser-shadow-index nil
  "Shadow index for the grouped browser (see `beads-formula-shadow-index').")

(defun beads-formula-browser--entry (formula)
  "Return the browser tabulated row for FORMULA.
The Name column carries a `⧉shad' marker when FORMULA shadows a
same-name formula lower on the search path."
  (let* ((name (or (oref formula name) ""))
         (shadowed (beads-formula-shadowed-by
                    formula beads-formula-browser-shadow-index))
         (display (if shadowed
                      (concat name
                              " "
                              (propertize "⧉shad"
                                          'face 'beads-face-warning
                                          'help-echo
                                          (format "shadows %s"
                                                  (mapconcat #'identity
                                                             shadowed ", "))))
                    name))
         (type (oref formula formula-type))
         (phase (oref formula phase))
         (source (oref formula source))
         (steps (or (oref formula steps) 0))
         (vars (or (oref formula vars) 0)))
    (list name
          (vector display
                  (beads-formula-list--format-type type)
                  (number-to-string steps)
                  (number-to-string vars)
                  (or phase "—")
                  (if source (abbreviate-file-name source) "—")))))

(defun beads-formula-browser-entries (formulas)
  "Build scope-filtered, shadow-annotated browser entries from FORMULAS.
The buffer-local `beads-formula-browser-scope' selects the scope."
  (let* ((root (or beads-formula-list--project-dir default-directory))
         (scope (or beads-formula-browser-scope 'all))
         (index (beads-formula-shadow-index root)))
    (setq-local beads-formula-browser-shadow-index index)
    ;; Keep the type-group order; a Name sort key would scatter the headers.
    (setq-local tabulated-list-sort-key nil)
    (tabulated-list-init-header)
    (beads-formula-grouped-entries
     (beads-formula-filter-by-scope formulas scope root))))

(defun beads-formula-grouped-entries (formulas)
  "Return type-grouped tabulated entries for the summaries in FORMULAS.
Each type gets a header row whose id is the cons
`(beads-formula-group . TYPE)'; the formulas of that type follow,
sorted by name.  Types in `beads-formula-group-order' come first in the
declared order; unrecognised types follow alphabetically.  The result is
a drop-in value for `tabulated-list-entries'; rows use the browser
layout (Name, Type, Steps, Vars, Phase, Source)."
  (let ((groups (make-hash-table :test #'equal))
        (types nil))
    (dolist (formula formulas)
      (let ((type (or (oref formula formula-type) "")))
        (unless (gethash type groups)
          (push type types))
        (push formula (gethash type groups))))
    (let ((ordered (append
                    (cl-remove-if-not (lambda (type) (gethash type groups))
                                      beads-formula-group-order)
                    (sort (cl-remove-if (lambda (type)
                                          (member type beads-formula-group-order))
                                        types)
                          #'string<))))
      (cl-mapcan
       (lambda (type)
         (let* ((members (sort (copy-sequence (gethash type groups))
                               (lambda (a b)
                                 (string< (or (oref a name) "")
                                          (or (oref b name) "")))))
                (label (if (string-empty-p type) "(untyped)" type))
                (header (list (cons 'beads-formula-group type)
                              (vector (propertize (concat "▾ " label)
                                                  'face 'beads-formula-header-face)
                                      "" "" "" "" ""))))
           (cons header
                 (mapcar #'beads-formula-browser--entry members))))
       ordered))))

(defun beads-formula-group-header-p (id)
  "Return non-nil when ID is a type-group header row id."
  (and (consp id) (eq (car id) 'beads-formula-group)))

(defun beads-formula-detail-sections (formula)
  "Return the detail section descriptors for FORMULA, in display order.
Each descriptor is a plist with `:key', `:title' and `:count'.  Only
sections with content are returned, mirroring the rendered recipe:
Vars, Steps, Bond points, Composition, then Source.  This is the
section index used for navigation and by downstream consumers that
want the structure without parsing the buffer."
  (let ((sections nil)
        (vars (oref formula vars))
        (steps (oref formula steps))
        (bond-points (oref formula bond-points)))
    (when vars
      (push (list :key 'vars
                  :title (format "Vars (%d)" (length vars))
                  :count (length vars))
            sections))
    (when steps
      (push (list :key 'steps
                  :title (format "Steps (%d)" (length steps))
                  :count (length steps))
            sections))
    (when bond-points
      (push (list :key 'bond-points
                  :title (format "Bond points (%d)" (length bond-points))
                  :count (length bond-points))
            sections))
    (when (or (oref formula extends)
              (oref formula aspects)
              (oref formula expansions))
      (push (list :key 'composition :title "Composition" :count 1) sections))
    (when (oref formula source)
      (push (list :key 'source :title "Source" :count 1) sections))
    (nreverse sections)))

;;; ============================================================
;;; Formula Resolution
;;; ============================================================

(defun beads-formula--name (formula)
  "Return FORMULA's name for a `beads-formula' or `beads-formula-summary'.
Accepts any object carrying a `name' slot, so downstream subclasses of
`beads-formula' work without a special predicate."
  (cond
   ((stringp formula) formula)
   ((and (eieio-object-p formula) (slot-exists-p formula 'name))
    (oref formula name))
   (t (error "Not a formula: %S" formula))))

(defun beads-formula--resolve (formula)
  "Return a `beads-formula' object for FORMULA.
FORMULA may already be a full formula object, a `beads-formula-summary',
or a formula name string; summaries and names are fetched with
`bd formula show'."
  (cond
   ((and (eieio-object-p formula) (slot-exists-p formula 'vars)) formula)
   ((or (stringp formula) (beads-formula-summary-p formula))
    (beads-command-execute
     (beads-command-formula-show
      :formula-name (beads-formula--name formula)
      :json t)))
   (t (error "Not a formula: %S" formula))))

;;; ============================================================
;;; Validation
;;; ============================================================

(defun beads-formula-validate-var (var value)
  "Return a validation warning for VAR and VALUE, or nil when valid.
VAR is a `beads-formula-var'; VALUE is its string value or nil.  A var
with a declared default and no value is valid, because the formula's
own default applies.  Required-missing, pattern mismatch, enum-outside
and non-numeric values each produce one message naming the var, so the
instantiate flow can block on the returned list (REQ-SF-014)."
  (let* ((spec (beads-formula-var-reader var))
         (name (or (plist-get spec :name) (oref var name)))
         (kind (plist-get spec :kind))
         (default (plist-get spec :default))
         (blank (beads-formula--blank-p value)))
    (cond
     ((and blank default) nil)
     (blank
      (when (plist-get spec :required)
        (format "Missing required variable: %s" name)))
     ((and (eq kind 'numeric)
           (not (string-match-p "\\`-?[0-9]+\\(?:\\.[0-9]+\\)?\\'" value)))
      (format "%s must be a number (got %s)" name value))
     ((and (plist-get spec :choices)
           (not (member value (plist-get spec :choices))))
      (format "%s is not in enum: %s (got %s)"
              name
              (mapconcat (lambda (choice) (format "%s" choice))
                         (plist-get spec :choices) ", ")
              value))
     ((and (plist-get spec :pattern)
           (not (string-match-p (plist-get spec :pattern) value)))
      (format "%s must match %s (got %s)"
              name (plist-get spec :pattern) value))
     (t nil))))

(defun beads-formula-instantiate-validation (formula vars)
  "Return validation warnings for instantiating FORMULA with VARS.
VARS is a (NAME . VALUE) alist; each warning names the offending var.
An empty result means the instantiate may proceed."
  (let ((values (beads-formula--normalize-vars vars)))
    (delq nil
          (mapcar (lambda (var)
                    (beads-formula-validate-var
                     var (cdr (assoc-string (oref var name) values))))
                  (oref formula vars)))))

;;; ============================================================
;;; Launch
;;; ============================================================

(defun beads-formula--normalize-vars (vars)
  "Return VARS as a (NAME . VALUE) alist with string keys and values.
Accepts an alist keyed by symbol or string; nil stays nil."
  (delq nil
        (mapcar (lambda (cell)
                  (let ((name (format "%s" (car cell)))
                        (value (cdr cell)))
                    (unless (beads-formula--blank-p value)
                      (cons name (format "%s" value)))))
                (append vars nil))))

(defun beads-formula--shape (bead formula)
  "Return the launch shape for BEAD and formula name FORMULA.
Delegates to `beads-sling-shape' when sling is loaded so both flows
agree; otherwise returns `on' when BEAD is set and `formula' otherwise."
  (if (fboundp 'beads-sling-shape)
      (beads-sling-shape bead formula)
    (if (beads-formula--blank-p bead) 'formula 'on)))

(defun beads-formula-launch-context-build (formula bead &optional vars phase assignee for)
  "Return the resolved `beads-formula-launch-context' for FORMULA and BEAD.
FORMULA is a name, summary or full formula; BEAD is the work bead id
\(nil for a standalone launch); VARS is an optional variable alist.
PHASE, ASSIGNEE and FOR are the optional explicit instantiate choices.
Warnings collect every validation problem, not just missing required
vars, so the instantiate flow can block on them."
  (let ((recipe (beads-formula--resolve formula))
        (normalized (beads-formula--normalize-vars vars)))
    (beads-formula-launch-context
     :shape (beads-formula--shape bead (beads-formula--name recipe))
     :vars normalized
     :target nil
     :phase phase
     :assignee assignee
     :for for
     :warnings (beads-formula-instantiate-validation recipe normalized))))

(defun beads-formula--var-args (vars)
  "Return VARS as a list of \"name=value\" strings for `bd mol pour'/`wisp'."
  (mapcar (lambda (cell) (format "%s=%s" (car cell) (cdr cell)))
          (beads-formula--normalize-vars vars)))

(defun beads-formula-instantiate--command (formula phase vars &optional assignee dry-run json)
  "Build the `bd' command that instantiates FORMULA with PHASE and VARS.
PHASE is `pour', `wisp' or `wisp-root-only'.  ASSIGNEE applies to
`pour' only (the `bd mol wisp' surface has no assignee flag); DRY-RUN
adds `--dry-run'.  JSON defaults to non-nil (the structured surface the
parsers expect); pass `raw' to omit `--json' for human output."
  (let ((name (beads-formula--name formula))
        (args (beads-formula--var-args vars))
        (json (not (eq json 'raw))))
    (pcase phase
      ('pour
       (beads-command-mol-pour :proto-id name :var args
                               :assignee assignee :dry-run dry-run :json json))
      ((or 'wisp 'wisp-root-only)
       (beads-command-mol-wisp :proto-id name :var args
                               :root-only (eq phase 'wisp-root-only)
                               :dry-run dry-run :json json))
      (_ (error "Unknown instantiate phase: %S" phase)))))

(defvar beads-formula-instantiate-assignee nil
  "Dynamically bound assignee for `beads-formula-instantiate'.
When non-nil, the `pour' default method passes it to `bd mol pour' as
`--assignee'.  The interactive instantiate flow binds it around the
generic call; programmatic callers may `let'-bind it too.")

(cl-defgeneric beads-formula-instantiate (formula phase bead &optional vars)
  "Instantiate FORMULA using the explicit PHASE against BEAD with VARS.
PHASE is `pour', `wisp' or `wisp-root-only'.  BEAD is the work bead an
`on' launch attaches to (nil standalone); the default `bd'-only method
irons locally and leaves BEAD to extension methods.  VARS is an
optional (NAME . VALUE) alist.  Returns the decoded `bd' result; follow
it with `beads-formula-follow' to open its run view.  `pour' honours
the dynamically bound `beads-formula-instantiate-assignee'.")

(cl-defmethod beads-formula-instantiate ((formula beads-formula) phase bead &optional vars)
  "Instantiate FORMULA with PHASE against BEAD through `bd mol'.
VARS is the optional variable alist.  See `beads-formula-instantiate'."
  (ignore bead)
  (beads-command-execute
   (beads-formula-instantiate--command formula phase vars
                                       beads-formula-instantiate-assignee nil)))

(cl-defmethod beads-formula-instantiate ((formula string) phase bead &optional vars)
  "Resolve FORMULA, then instantiate it with PHASE against BEAD.
VARS is the optional variable alist.  See `beads-formula-instantiate'."
  (beads-formula-instantiate (beads-formula--resolve formula) phase bead vars))

(cl-defgeneric beads-formula-launch (formula bead &optional vars)
  "Launch FORMULA against BEAD, or standalone when BEAD is nil.
FORMULA is a `beads-formula' object or a formula name; VARS is an
optional (NAME . VALUE) alist of formula variable values.  Returns the
resolved `beads-formula-launch-context'.  The default method delegates
to `beads-formula-instantiate' with the `pour' phase; downstream
packages override this generic to launch through their own backend and
return their own run session object.")

(cl-defmethod beads-formula-launch ((formula beads-formula) bead &optional vars)
  "Launch FORMULA against BEAD with VARS through `bd mol pour' (see the generic)."
  (let* ((context (beads-formula-launch-context-build formula bead vars 'pour))
         (result (beads-formula-instantiate formula 'pour bead (oref context vars))))
    (beads-formula-follow result context)
    context))

(cl-defmethod beads-formula-launch ((formula string) bead &optional vars)
  "Resolve FORMULA, then launch it against BEAD with VARS.
See `beads-formula-launch'."
  (beads-formula-launch (beads-formula--resolve formula) bead vars))

(defun beads-formula--result-root-id (result)
  "Return a mol root id from RESULT, or nil.
RESULT is the decoded `bd mol pour --json' output; the first
recognisable id key anywhere in the structure wins."
  (cond
   ((null result) nil)
   ((vectorp result) (beads-formula--result-root-id (append result nil)))
   ((consp result)
    (or (and (consp (car result))
             (let ((key (format "%s" (car (car result)))))
               (and (member key '("root_id" "new_epic_id" "id" "issue_id"
                                  "molecule_id" "mol_id"))
                    (cdr (car result)))))
        (beads-formula--result-root-id (cdr (car result)))
        (beads-formula--result-root-id (cdr result))))
   (t nil)))

(cl-defgeneric beads-formula-follow (result context)
  "Open the run view for a finished launch of CONTEXT.
RESULT is the decoded `bd mol' output.  The default method opens
`beads-molecule-open' on the created root id when the molecule view is
loaded, and reports the molecule otherwise.  Extension packages
specialize on their own CONTEXT to open their own run view.  Returns
CONTEXT.")

(cl-defmethod beads-formula-follow (result context)
  "Default follow of RESULT: open `beads-molecule-open' on the created root id.
Returns CONTEXT unchanged."
  (let ((id (beads-formula--result-root-id result)))
    (cond
     ((null id) (message "Formula launched"))
     ((fboundp 'beads-molecule-open) (beads-molecule-open id))
     (t (message "Formula launched — molecule %s" id))))
  context)

;;; ============================================================
;;; Instantiate Flow
;;; ============================================================

(defun beads-formula-instantiate--initial-scope (recipe)
  "Return the instantiate scope seeded with RECIPE.
Phase explicitly defaults to the formula's recommendation (wisp for a
vapor formula, pour otherwise); the operator can still change it."
  (list :formula (beads-formula--name recipe)
        :recipe recipe
        :phase (beads-formula-phase-recommendation recipe)
        :vars nil
        :assignee nil
        :for nil))

(defun beads-formula-instantiate--scope-context (scope)
  "Return the `beads-formula-launch-context' described by SCOPE."
  (beads-formula-launch-context-build
   (plist-get scope :recipe)
   (plist-get scope :bead)
   (plist-get scope :vars)
   (or (plist-get scope :phase) 'pour)
   (plist-get scope :assignee)
   (plist-get scope :for)))

(defun beads-formula-instantiate--resetup (scope)
  "Re-setup the instantiate transient with SCOPE."
  (transient-setup 'beads-formula-instantiate--transient nil nil :scope scope))

(defun beads-formula-instantiate--set-phase (phase)
  "Select PHASE in the instantiate flow and rebuild the menu."
  (beads-formula-instantiate--resetup
   (plist-put (copy-sequence (transient-scope)) :phase phase)))

(defun beads-formula-instantiate--phase-label (scope phase)
  "Return the menu label for PHASE in SCOPE.
Shows selection and the formula's recommendation so the explicit choice
is always visible (REQ-SF-013)."
  (let* ((active (eq (or (plist-get scope :phase) 'pour) phase))
         (recommended (eq (beads-formula-phase-recommendation
                           (plist-get scope :recipe))
                          phase))
         (base (pcase phase
                 ('pour "Pour (persistent, liquid)")
                 ('wisp "Wisp (ephemeral, vapor)")
                 ('wisp-root-only "Wisp root only (no steps)"))))
    (concat base "     " (if active "◉ selected" "○")
            (if recommended " recommended" ""))))

(defun beads-formula-instantiate--header (scope)
  "Return the dynamic header for the instantiate transient SCOPE.
Recomputes readiness, phase and warnings from the live scope."
  (let* ((recipe (plist-get scope :recipe))
         (phase (or (plist-get scope :phase) 'pour))
         (errors (beads-formula-instantiate-validation
                  recipe (plist-get scope :vars)))
         (phase-warning (beads-formula-phase-warning recipe phase))
         (set-count (length (beads-formula--normalize-vars
                             (plist-get scope :vars))))
         (total (length (oref recipe vars)))
         (status
          (if errors
              (format "⚠ Blocked — %s" (mapconcat #'identity errors "; "))
            (format "✓ Ready — %s · %d/%d vars · %s"
                    (pcase phase
                      ('pour "pour (persistent)")
                      ('wisp "wisp (ephemeral)")
                      ('wisp-root-only "wisp root only"))
                    set-count total
                    (or (plist-get scope :assignee) "unassigned")))))
    (concat (format "Formula %s — %s"
                    (beads-formula--name recipe)
                    (or (oref recipe description) "no description"))
            "\n" status
            (if phase-warning (concat "\n⚠ " phase-warning) "")
            (let ((for-value (plist-get scope :for)))
              (if for-value (format "\n--for %s" for-value) "")))))

(transient-define-suffix beads-formula-instantiate--phase-pour ()
  "Choose pour (persistent) as the instantiate phase."
  :description (lambda (_obj)
                 (beads-formula-instantiate--phase-label
                  (transient-scope) 'pour))
  (interactive)
  (beads-formula-instantiate--set-phase 'pour))

(transient-define-suffix beads-formula-instantiate--phase-wisp ()
  "Choose wisp (ephemeral) as the instantiate phase."
  :description (lambda (_obj)
                 (beads-formula-instantiate--phase-label
                  (transient-scope) 'wisp))
  (interactive)
  (beads-formula-instantiate--set-phase 'wisp))

(transient-define-suffix beads-formula-instantiate--phase-wisp-root-only ()
  "Choose wisp with --root-only (no child steps) as the phase."
  :description (lambda (_obj)
                 (beads-formula-instantiate--phase-label
                  (transient-scope) 'wisp-root-only))
  (interactive)
  (beads-formula-instantiate--set-phase 'wisp-root-only))

(transient-define-suffix beads-formula-instantiate--edit-vars ()
  "Read or edit one formula variable with its typed reader."
  :description
  (lambda (_obj)
    (let* ((scope (transient-scope))
           (recipe (plist-get scope :recipe))
           (set-count (length (beads-formula--normalize-vars
                               (plist-get scope :vars))))
           (total (length (oref recipe vars))))
      (format "Edit variables (%d/%d set)" set-count total)))
  (interactive)
  (let* ((scope (transient-scope))
         (recipe (plist-get scope :recipe))
         (names (mapcar (lambda (var) (oref var name)) (oref recipe vars))))
    (unless names
      (user-error "Formula %s declares no variables"
                  (beads-formula--name recipe)))
    (let* ((name (completing-read "Variable: " names nil t))
           (var (cl-find name (oref recipe vars)
                         :key (lambda (item) (oref item name))
                         :test #'equal))
           (value (beads-formula-read-var var)))
      (beads-formula-instantiate--resetup
       (plist-put (copy-sequence scope)
                  :vars (cons (cons name (or value ""))
                              (cl-remove name
                                         (beads-formula--normalize-vars
                                          (plist-get scope :vars))
                                         :key #'car :test #'equal)))))))

(transient-define-suffix beads-formula-instantiate--set-assignee ()
  "Read the root assignee for the instantiate (`--assignee')."
  :description
  (lambda (_obj)
    (format "Assignee: %s" (or (plist-get (transient-scope) :assignee)
                                "(none)")))
  (interactive)
  (let ((value (read-string "Assignee (agent or user): "
                            (plist-get (transient-scope) :assignee))))
    (beads-formula-instantiate--resetup
     (plist-put (copy-sequence (transient-scope)) :assignee
                (and (not (beads-formula--blank-p value)) value)))))

(transient-define-suffix beads-formula-instantiate--set-for ()
  "Read the `--for' agent that scopes the run view's queries."
  :description
  (lambda (_obj)
    (format "--for: %s" (or (plist-get (transient-scope) :for)
                             "(none)")))
  (interactive)
  (let ((value (read-string "Scope run view to agent (--for): "
                            (plist-get (transient-scope) :for))))
    (beads-formula-instantiate--resetup
     (plist-put (copy-sequence (transient-scope)) :for
                (and (not (beads-formula--blank-p value)) value)))))

(transient-define-suffix beads-formula-instantiate--run ()
  "Instantiate the formula exactly as shown, blocking on hard errors."
  :description "Instantiate"
  (interactive)
  (let* ((scope (transient-scope))
         (recipe (plist-get scope :recipe))
         (phase (or (plist-get scope :phase) 'pour))
         (vars (plist-get scope :vars))
         (errors (beads-formula-instantiate-validation recipe vars)))
    (when errors
      (user-error "Cannot instantiate %s: %s"
                  (beads-formula--name recipe)
                  (mapconcat #'identity errors "; ")))
    (let* ((beads-formula-instantiate-assignee (plist-get scope :assignee))
           (context (beads-formula-instantiate--scope-context scope))
           (result (beads-formula-instantiate recipe phase
                                              (plist-get scope :bead) vars)))
      (beads-formula-follow result context))
    (transient-quit-one)))

(transient-define-suffix beads-formula-instantiate--reset ()
  "Clear vars, assignee and --for; rebuild the menu from scratch."
  :description "Reset choices"
  (interactive)
  (let* ((scope (transient-scope))
         (recipe (plist-get scope :recipe)))
    (beads-formula-instantiate--resetup
     (beads-formula-instantiate--initial-scope recipe))))

(defconst beads-formula-instantiate-preview-buffer-name
  "*beads-formula: instantiate preview*"
  "Buffer name of the instantiate dry-run preview.")

(defun beads-formula-instantiate--preview-header (scope)
  "Return the header lines of the instantiate preview for SCOPE."
  (let* ((recipe (plist-get scope :recipe))
         (phase (or (plist-get scope :phase) 'pour))
         (vars (plist-get scope :vars))
         (assignee (plist-get scope :assignee))
         (command (beads-formula-instantiate--command
                   recipe phase vars assignee t 'raw))
         (line (mapconcat #'shell-quote-argument (beads-command-line command) " "))
         (errors (beads-formula-instantiate-validation recipe vars)))
    (append
     (list
      (format "Instantiate preview — %s (%s, dry-run)"
              (beads-formula--name recipe)
              (pcase phase
                ('pour "pour")
                ('wisp "wisp")
                ('wisp-root-only "wisp --root-only")))
      ""
      (format "  Formula phase: %s" (or (beads-formula-phase recipe) "undeclared"))
      (format "  Assignee: %s" (or assignee "(none)"))
      (format "  Vars: %d/%d set"
              (length (beads-formula--normalize-vars vars))
              (length (oref recipe vars)))
      (format "  --for: %s" (or (plist-get scope :for) "(none)")))
     (when errors
       (list "" (format "  ⚠ %s" (mapconcat #'identity errors "; "))))
     (list "" "  Command:" (concat "    " line) ""))))

(defun beads-formula-instantiate-preview (scope)
  "Open a dry-run preview buffer for the instantiate SCOPE.
Runs the real `bd' command with `--dry-run' (no writes) and renders its
human-readable output; a failure is rendered in the buffer instead of
signalled.  Returns the preview buffer."
  (let* ((recipe (plist-get scope :recipe))
         (phase (or (plist-get scope :phase) 'pour))
         (vars (plist-get scope :vars))
         (assignee (plist-get scope :assignee))
         (command (beads-formula-instantiate--command
                   recipe phase vars assignee t 'raw))
         (lines (beads-formula-instantiate--preview-header scope))
         (output (condition-case err
                     (format "%s" (beads-command-execute command))
                   (error (format "Error: %s" (error-message-string err))))))
    (with-current-buffer (get-buffer-create
                          beads-formula-instantiate-preview-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (mapconcat #'identity lines "\n"))
        (insert "  Dry-run output:\n" (or output "") "\n")
        (special-mode)
        (goto-char (point-min))))
    (display-buffer beads-formula-instantiate-preview-buffer-name)))

(transient-define-suffix beads-formula-instantiate--preview ()
  "Open a dry-run preview of the instantiate."
  :description "Full preview (dry-run)"
  (interactive)
  (beads-formula-instantiate-preview (transient-scope)))

(beads-define-prefix beads-formula-instantiate--transient ()
  "Instantiate a formula with an explicit pour/wisp phase (mockup §4).
Phase is never implicit; the formula's declared phase supplies the
recommended default.  Warnings recompute as choices change and `s'
blocks on hard validation errors."
  [:description (lambda () (beads-formula-instantiate--header
                            (transient-scope)))
   :class transient-row
   ("" "" ignore :if (lambda () nil))]
  [["Phase"
    ("p" beads-formula-instantiate--phase-pour)
    ("w" beads-formula-instantiate--phase-wisp)
    ("W" beads-formula-instantiate--phase-wisp-root-only)]
   ["Vars"
    ("v" beads-formula-instantiate--edit-vars)]
   ["Assign"
    ("a" beads-formula-instantiate--set-assignee)
    ("f" beads-formula-instantiate--set-for)]]
  [["Actions"
    ("s" beads-formula-instantiate--run)
    ("P" beads-formula-instantiate--preview)
    ("x" beads-formula-instantiate--reset)
    ("q" "Quit" transient-quit-one)]])

;;;###autoload
(defun beads-formula-launch-standalone (formula)
  "Instantiate FORMULA with the explicit pour/wisp choice.
FORMULA is the formula at point in a browser/detail buffer, or a
formula name when called interactively.  Opens the instantiate
transient; the phase is never implicit (REQ-SF-013)."
  (interactive
   (list (or (beads-formula--name-at-point)
             (completing-read
              "Formula: "
              (mapcar (lambda (f) (oref f name))
                      (beads-command-execute
                       (beads-command-formula-list :json t)))
              nil t))))
  (let ((recipe (beads-formula--resolve formula)))
    (transient-setup 'beads-formula-instantiate--transient nil nil
                     :scope (beads-formula-instantiate--initial-scope recipe))))

;;; ============================================================
;;; Sling Seeding
;;; ============================================================

(defun beads-formula-sling-scope (formula)
  "Return the sling scope plist that seeds the flow with FORMULA.
The formula stage is pre-filled (name and recipe) and the work stage is
left empty so the sling prompts only for the work bead (mockup §10b)."
  (let ((recipe (beads-formula--resolve formula)))
    (list :work nil
          :work-title nil
          :formula (beads-formula--name recipe)
          :recipe recipe
          :target nil)))

;;;###autoload
(defun beads-formula-seed-sling (formula)
  "Seed the sling flow with FORMULA and prompt only for the work bead.
Requires the sling module (WI-10/WI-11); signals a `user-error' when it
is not available."
  (interactive
   (list (or (beads-formula--name-at-point)
             (completing-read
              "Formula: "
              (mapcar (lambda (f) (oref f name))
                      (beads-command-execute
                       (beads-command-formula-list :json t)))
              nil t))))
  (let ((scope (beads-formula-sling-scope formula)))
    (unless (require 'beads-sling nil t)
      (user-error "Sling is not available; load beads-sling first"))
    (transient-setup 'beads-sling--transient nil nil :scope scope)))

(defun beads-formula--name-at-point ()
  "Return the formula name at point in a formula browser or detail buffer."
  (cond
   ((derived-mode-p 'beads-formula-list-mode)
    (let ((id (tabulated-list-get-id)))
      (and (stringp id) id)))
   ((derived-mode-p 'beads-formula-show-mode)
    (bound-and-true-p beads-formula-show--formula-name))
   (t nil)))

;;; ============================================================
;;; Browser Entry Point
;;; ============================================================

;;;###autoload
(defun beads-formula-browse (&optional type scope)
  "Browse formulas grouped by type in a tabulated list.
Optional TYPE filters by formula type (workflow, expansion, aspect).
Optional SCOPE filters the search path: `project', `user' or `all'
\(default `all').  Rows carry the `l' (seed sling), `s' (instantiate),
`o' (open source) and `/` (scope) actions."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Filter by type: "
                            beads-formula-group-order nil t))
         nil))
  (let ((scope (or scope 'all)))
    (beads-formula-list
     type
     (lambda (formulas)
       (setq-local beads-formula-browser-scope scope)
       (beads-formula-browser-entries formulas)))))

(defun beads-formula-browse-set-scope (scope)
  "Set the browser SCOPE (`project', `user' or `all') and refresh."
  (interactive
   (list (intern (completing-read
                  "Scope: " '("project" "user" "all") nil t nil nil
                  (symbol-name (or beads-formula-browser-scope 'all))))))
  (let ((type (and (bound-and-true-p beads-formula-list--command-obj)
                   (oref beads-formula-list--command-obj formula-type))))
    (beads-formula-browse type scope)))

;;; ============================================================
;;; Key Bindings
;;; ============================================================

(define-key beads-formula-list-mode-map (kbd "l")
            #'beads-formula-seed-sling)
(define-key beads-formula-list-mode-map (kbd "s")
            #'beads-formula-launch-standalone)
(define-key beads-formula-list-mode-map (kbd "/")
            #'beads-formula-browse-set-scope)
(define-key beads-formula-show-mode-map (kbd "l")
            #'beads-formula-seed-sling)
(define-key beads-formula-show-mode-map (kbd "s")
            #'beads-formula-launch-standalone)

(provide 'beads-formula)
;;; beads-formula.el ends here
