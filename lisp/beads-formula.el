;;; beads-formula.el --- Formula browser, detail and launch UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The formula browser/detail/launch UI and ABI (design.md §4.7, §10;
;; REQ-013, REQ-014).  The `bd formula' command classes stay in
;; `beads-command-formula.el'; this module owns the presentation and the
;; extension seams.
;;
;; ABI shared with the sling flow:
;;
;; - `beads-formula-var-reader' maps a formula variable's declared
;;   metadata (type, enum, name convention) to a transient reader kind,
;;   so a var is read the same way in the browser and in the sling How
;;   stage.
;; - `beads-formula-launch-context' is the resolved launch (shape, var
;;   values, target, warnings) shared with the sling preview.
;; - `beads-formula-launch' is the launch generic.  The default method
;;   irons the formula locally through `bd mol pour'; downstream packages
;;   (for example Gas City) override it to run `gc sling --formula/--on'
;;   and expose their own run view.
;;
;; Standalone behaviour: with no extension package loaded, browsing,
;; inspecting and launching a formula all work with `bd' alone.
;;
;; Key bindings added to the existing formula buffers:
;;
;;   l  seed the sling flow with the formula at point (list/detail)
;;   s  launch the formula standalone (list/detail)

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'transient)
(require 'beads-util)
(require 'beads-types)
(require 'beads-command)
(require 'beads-command-formula)
(require 'beads-command-mol)

;;; Forward Declarations

(declare-function beads-sling-shape "beads-sling" (work formula))
(declare-function beads-sling-targets "beads-sling" (&optional bead))

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
    :documentation "Human-readable warnings collected before launch."))
  "The resolved formula launch shared with the sling flow (design.md §4.7).")

;;; ============================================================
;;; Variable Readers
;;; ============================================================

(cl-defgeneric beads-formula-var-reader (var)
  "Return the transient reader spec for formula variable VAR.
The spec is a plist; `:kind' is one of `enum', `bool', `numeric',
`file', `directory', `agent' or `string'.  Optional keys are `:name',
`:prompt', `:required', `:default', `:pattern' and `:choices'.

The declared shape wins (an enum list, a boolean or integer `var-type'),
then naming conventions (`context_path'/`*_path' is a file,
`artifact_root' a directory, `*_target' an agent, numeric defaults or
`max_'/`_iterations' names are numeric).  Anything unrecognised fails
soft to plain string entry.  Downstream packages may specialize this
generic on their own metadata.")

(cl-defmethod beads-formula-var-reader ((var beads-formula-var))
  "Return the reader spec for VAR (see `beads-formula-var-reader')."
  (let ((name (or (oref var name) ""))
        (type (oref var var-type))
        (enum (oref var enum))
        (default (oref var default)))
    (list :kind (cond
                 (enum 'enum)
                 ((equal type "bool") 'bool)
                 ((equal type "int") 'numeric)
                 ((or (equal name "context_path")
                      (string-suffix-p "_path" name))
                  'file)
                 ((equal name "artifact_root") 'directory)
                 ((string-suffix-p "_target" name) 'agent)
                 ((or (and default (string-match-p "\\`[0-9]+\\'" default))
                      (string-prefix-p "max_" name)
                      (string-suffix-p "_iterations" name))
                  'numeric)
                 (t 'string))
          :name name
          :prompt (or (oref var description) name)
          :required (oref var required)
          :default default
          :pattern (oref var pattern)
          :choices enum)))

(defun beads-formula-var-kind (var)
  "Return VAR's reader kind symbol (a `beads-formula-var-reader' shortcut)."
  (plist-get (beads-formula-var-reader var) :kind))

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
  "Return FORMULA's name for a `beads-formula' or `beads-formula-summary'."
  (cond
   ((stringp formula) formula)
   ((or (beads-formula-p formula) (beads-formula-summary-p formula))
    (oref formula name))
   (t (error "Not a formula: %S" formula))))

(defun beads-formula--resolve (formula)
  "Return a `beads-formula' object for FORMULA.
FORMULA may already be a `beads-formula' object, a
`beads-formula-summary', or a formula name string; summaries and names
are fetched with `bd formula show'."
  (cond
   ((beads-formula-p formula) formula)
   ((or (stringp formula) (beads-formula-summary-p formula))
    (beads-command-execute
     (beads-command-formula-show
      :formula-name (beads-formula--name formula)
      :json t)))
   (t (error "Not a formula: %S" formula))))

;;; ============================================================
;;; Launch
;;; ============================================================

(defun beads-formula--blank-p (value)
  "Return non-nil when VALUE is nil or the empty string."
  (or (null value) (and (stringp value) (string-empty-p value))))

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

(defun beads-formula--missing-required-vars (formula vars)
  "Return the names of required FORMULA vars missing from VARS.
VARS is a (NAME . VALUE) alist as returned by
`beads-formula--normalize-vars'."
  (let ((values (beads-formula--normalize-vars vars)))
    (delq nil
          (mapcar (lambda (var)
                    (let ((name (oref var name)))
                      (when (and (oref var required)
                                 (not (assoc-string name values)))
                        name)))
                  (oref formula vars)))))

(defun beads-formula-launch-context-build (formula bead &optional vars)
  "Return the resolved `beads-formula-launch-context' for FORMULA and BEAD.
FORMULA is a name, summary or full formula; BEAD is the work bead id
\(nil for a standalone launch); VARS is an optional variable alist.
Warnings list any required vars that are still missing."
  (let ((recipe (beads-formula--resolve formula))
        (normalized (beads-formula--normalize-vars vars)))
    (beads-formula-launch-context
     :shape (beads-formula--shape bead (beads-formula--name recipe))
     :vars normalized
     :target nil
     :warnings (mapcar (lambda (name)
                         (format "Missing required variable: %s" name))
                       (beads-formula--missing-required-vars recipe normalized)))))

(defun beads-formula--var-args (vars)
  "Return VARS as a list of \"name=value\" strings for `bd mol pour'."
  (mapcar (lambda (cell) (format "%s=%s" (car cell) (cdr cell)))
          (beads-formula--normalize-vars vars)))

(cl-defgeneric beads-formula-launch (formula bead &optional vars)
  "Launch FORMULA against BEAD, or standalone when BEAD is nil.
FORMULA is a `beads-formula' object or a formula name; VARS is an
optional (NAME . VALUE) alist of formula variable values.  Returns the
resolved `beads-formula-launch-context'.  The default method irons the
formula locally with `bd mol pour'; downstream packages override this
generic to launch through their own backend and return their own run
session object.")

(cl-defmethod beads-formula-launch ((formula beads-formula) bead &optional vars)
  "Launch FORMULA against BEAD with VARS through `bd mol pour' (see the generic)."
  (let* ((context (beads-formula-launch-context-build formula bead vars))
         (command (beads-command-mol-pour
                   :proto-id (beads-formula--name formula)
                   :var (beads-formula--var-args (oref context vars))
                   :json t))
         (result (beads-command-execute command)))
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
               (and (member key '("root_id" "id" "issue_id"
                                  "molecule_id" "mol_id"))
                    (cdr (car result)))))
        (beads-formula--result-root-id (cdr (car result)))
        (beads-formula--result-root-id (cdr result))))
   (t nil)))

(defun beads-formula-follow (result context)
  "Open the run view for a finished launch of CONTEXT.
RESULT is the decoded `bd mol pour --json' output.  Standalone this
just reports the created molecule; extension packages override the
`beads-formula-launch' generic to expose a real run view instead.
Returns CONTEXT."
  (if-let* ((id (beads-formula--result-root-id result)))
      (message "Formula launched — molecule %s" id)
    (message "Formula launched"))
  context)

;;; ============================================================
;;; Interactive Launch and Sling Seeding
;;; ============================================================

(defun beads-formula-read-var (var)
  "Read one value for VAR using its `beads-formula-var-reader' spec.
Returns the value as a string, or nil when the user leaves it blank."
  (let* ((spec (beads-formula-var-reader var))
         (kind (plist-get spec :kind))
         (prompt (concat (or (plist-get spec :prompt) "Value")
                         (when-let* ((default (plist-get spec :default)))
                           (format " [%s]" default))
                         ": ")))
    (pcase kind
      ('enum (completing-read prompt (plist-get spec :choices) nil t
                              nil nil (plist-get spec :default)))
      ('bool (if (y-or-n-p (concat prompt "yes/no? ")) "true" "false"))
      ('file (read-file-name prompt nil nil t nil))
      ('directory (read-directory-name prompt))
      ('agent (beads-formula--read-agent prompt))
      ('numeric (let ((n (read-number prompt
                                      (and (plist-get spec :default)
                                           (string-to-number
                                            (plist-get spec :default))))))
                 (and n (number-to-string n))))
      (_ (read-string prompt nil nil (plist-get spec :default))))))

(defun beads-formula--read-agent (prompt)
  "Read an agent target name with PROMPT.
Uses `beads-sling-targets' when sling is loaded, else plain entry."
  (if (fboundp 'beads-sling-targets)
      (let ((names (mapcar (lambda (target) (format "%s" (oref target name)))
                           (beads-sling-targets))))
        (if names
            (completing-read prompt names nil t)
          (read-string prompt)))
    (read-string prompt)))

(defun beads-formula-read-vars (formula)
  "Read values for FORMULA's required vars without a default.
Returns a (NAME . VALUE) alist; optional and defaulted vars are left to
the formula's own defaults."
  (let ((values nil))
    (dolist (var (oref formula vars))
      (when (and (oref var required)
                 (beads-formula--blank-p (oref var default)))
        (let ((value (beads-formula-read-var var)))
          (unless (beads-formula--blank-p value)
            (push (cons (oref var name) value) values)))))
    (nreverse values)))

;;;###autoload
(defun beads-formula-launch-standalone (formula)
  "Launch FORMULA standalone, prompting for its required vars.
FORMULA is the formula at point in a browser/detail buffer, or a
formula name when called interactively."
  (interactive
   (list (or (beads-formula--name-at-point)
             (completing-read
              "Formula: "
              (mapcar (lambda (f) (oref f name))
                      (beads-command-execute
                       (beads-command-formula-list :json t)))
              nil t))))
  (let* ((recipe (beads-formula--resolve formula))
         (vars (beads-formula-read-vars recipe)))
    (beads-formula-launch recipe nil vars)))

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
