;;; beads-formula-edit.el --- Formula authoring: TOML create/edit, validation, convert -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Formula authoring surfaces (design.md §10; REQ-SF-060 … REQ-SF-062,
;; WI-SF-09).  The distill flow (REQ-SF-063, WI-SF-10) lives in this
;; module too; only the create/edit/schema/convert section is implemented
;; here.
;;
;; - `beads-formula-new' scaffolds a minimal `.formula.toml' in the
;;   project or user search-path directory and opens it in TOML mode
;;   (`toml-mode' when installed, else `conf-mode'), associated with the
;;   formula name.
;; - `beads-formula-edit-source' opens an existing formula's source the
;;   same way.
;; - `beads-formula-edit-validate' checks the buffer against
;;   `bd formula schema' (`beads-formula-schema-struct' field names,
;;   types and required flags) and against the `bd' TOML parser, then
;;   renders inline diagnostics and a compile-style results buffer.
;; - `beads-formula-convert-at-point' runs `bd formula convert' on the
;;   JSON formula at point, with `--stdout' and `--delete' variants.
;;
;; Authoring is the one file-touching surface (design.md §11): paths are
;; localized through `beads-remote-localize-path' so a remote (TRAMP)
;; store opens the file on its host.  No new hard dependency is added —
;; completion and validation are driven by the `bd' schema, not a
;; bundled TOML parser.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'subr-x)
(require 'transient)
(require 'beads-util)
(require 'beads-remote)
(require 'beads-command)
(require 'beads-command-formula)
(require 'beads-types)
(require 'beads-faces)
(require 'beads-prefix)

;;; Forward Declarations

(defvar beads-store-directory)
(defvar beads-formula-list-mode-map)
(defvar beads-formula-show-mode-map)
(defvar beads-formula-show--formula-name)

(declare-function beads-formula-list--current-formula-name
                  "beads-command-formula" ())
(declare-function toml-mode "toml-mode" ())
(declare-function toml-ts-mode "toml-ts-mode" ())

;;; Customization

(defgroup beads-formula-edit nil
  "Formula authoring for Beads."
  :group 'beads
  :prefix "beads-formula-edit-")

(defcustom beads-formula-edit-project-subdir ".beads/formulas"
  "Project-relative directory that holds project formulas.
Resolved against the store root (`beads-store-directory', else the
project root).  Mirrors `bd formula' search path 2."
  :type 'string
  :group 'beads-formula-edit)

(defcustom beads-formula-edit-user-subdir "~/.beads/formulas"
  "User directory that holds user formulas.
Mirrors `bd formula' search path 3."
  :type 'string
  :group 'beads-formula-edit)

;;; Faces

(defface beads-formula-edit-error
  '((t :inherit beads-face-error :underline t))
  "Face for a formula diagnostic line, used for inline overlays."
  :group 'beads-formula-edit)

(defface beads-formula-edit-required
  '((t :inherit beads-face-warning))
  "Face for a required schema field in completion annotations."
  :group 'beads-formula-edit)

;;; ============================================================
;;; Search-path directories
;;; ============================================================

(defun beads-formula-edit--store-root ()
  "Return the store root to resolve project formula paths against.
Prefers the buffer-local `beads-store-directory'; falls back to the
project root and finally `default-directory'.  Never signals."
  (file-name-as-directory
   (expand-file-name
    (or (and (boundp 'beads-store-directory) beads-store-directory)
        (ignore-errors (beads--project-root))
        default-directory))))

(defun beads-formula-edit-project-dir (&optional root)
  "Return the project formula directory under ROOT (default store root)."
  (expand-file-name beads-formula-edit-project-subdir
                    (file-name-as-directory
                     (or root (beads-formula-edit--store-root)))))

(defun beads-formula-edit-user-dir ()
  "Return the user formula directory."
  (file-name-as-directory
   (expand-file-name beads-formula-edit-user-subdir)))

(defun beads-formula-edit-scope-dir (scope)
  "Return the formula directory for SCOPE, a `project' or `user' symbol/string."
  (if (member scope '(user "user"))
      (beads-formula-edit-user-dir)
    (beads-formula-edit-project-dir)))

(defun beads-formula-edit--path (name scope)
  "Return the absolute source path for formula NAME in SCOPE.
NAME is the formula identifier; SCOPE is `project' or `user'."
  (expand-file-name (format "%s.formula.toml" name)
                    (beads-formula-edit-scope-dir scope)))

(defun beads-formula-edit--localize (path)
  "Return PATH opened on the current store's host (TRAMP-aware)."
  (beads-remote-localize-path path))

;;; ============================================================
;;; Mode selection and buffer association
;;; ============================================================

(defvar-local beads-formula-edit--formula-name nil
  "Formula name associated with the current authoring buffer.")

(defun beads-formula-edit--major-mode ()
  "Return the TOML major mode to use for authoring buffers.
Prefers `toml-mode' when installed, then `toml-ts-mode' when the TOML
grammar is available, and falls back to `conf-mode'."
  (cond
   ((locate-library "toml-mode") 'toml-mode)
   ((and (fboundp 'toml-ts-mode)
         (fboundp 'treesit-language-available-p)
         (treesit-language-available-p 'toml))
    'toml-ts-mode)
   (t 'conf-mode)))

(defun beads-formula-edit--setup (&optional name)
  "Set up the current buffer as a formula authoring buffer.
Switches to the preferred TOML major mode, associates NAME, and enables
`beads-formula-edit-minor-mode'."
  (funcall (beads-formula-edit--major-mode))
  (setq-local beads-formula-edit--formula-name name)
  (beads-formula-edit-minor-mode 1))

;;; ============================================================
;;; Scaffold and creation (REQ-SF-060)
;;; ============================================================

(defun beads-formula-edit-scaffold (name type)
  "Return minimal `.formula.toml' scaffold text for NAME and TYPE.
TYPE is `workflow', `expansion' or `aspect'."
  (format "formula = %s\ndescription = \"\"\nversion = 1\ntype = %s\n\n[[steps]]\nid = %s\ntitle = \"First step\"\n"
          (format "%S" name)
          (format "%S" (or type "workflow"))
          (format "%S"
                  (concat (replace-regexp-in-string
                           "[^a-z0-9]+" "-" (downcase (or name "")))
                          "-step"))))

;;;###autoload
(defun beads-formula-new (name type scope)
  "Create a new formula source file and open it for editing.
NAME is the formula identifier, TYPE one of `workflow', `expansion' or
`aspect', and SCOPE `project' (under `.beads/formulas') or `user'
\(under `~/.beads/formulas').  The file is created from
`beads-formula-edit-scaffold' and opened in a TOML authoring buffer
associated with NAME (REQ-SF-060)."
  (interactive
   (list (string-trim (read-string "Formula name: "))
         (completing-read "Type: " '("workflow" "expansion" "aspect")
                          nil t nil nil "workflow")
         (completing-read "Scope: " '("project" "user")
                          nil t nil nil "project")))
  (when (or (null name) (string-empty-p name))
    (user-error "Formula name is required"))
  (when (string-match-p "[/\\]" name)
    (user-error "Formula name must not contain a path separator: %s" name))
  (let* ((path (beads-formula-edit--path name scope))
         (local (beads-formula-edit--localize path)))
    (when (file-exists-p local)
      (user-error "Formula already exists: %s" path))
    (make-directory (file-name-directory local) t)
    (with-temp-file local
      (insert (beads-formula-edit-scaffold name type)))
    (beads-formula-edit--visit local name)))

(defun beads-formula-edit--visit (path name)
  "Open PATH in a formula authoring buffer associated with NAME.
Returns the buffer."
  (let ((buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (beads-formula-edit--setup name))
    (pop-to-buffer buffer)
    buffer))

;;; ============================================================
;;; Open an existing source (REQ-SF-060)
;;; ============================================================

(defun beads-formula-edit--resolve-source (formula)
  "Return FORMULA's source path.
FORMULA may be a `beads-formula' object, a `beads-formula-summary',
or a name string (fetched with `bd formula show')."
  (let ((recipe
         (cond
          ((beads-formula-p formula) formula)
          ((or (stringp formula) (beads-formula-summary-p formula))
           (beads-command-execute
            (beads-command-formula-show
             :formula-name (if (stringp formula) formula (oref formula name))
             :json t)))
          (t (error "Not a formula: %S" formula)))))
    (oref recipe source)))

;;;###autoload
(defun beads-formula-edit-source (formula)
  "Open FORMULA's source in a TOML authoring buffer (REQ-SF-060).
FORMULA may be a name, a `beads-formula-summary' or a `beads-formula'
object; when called interactively it is read from `bd formula list'.
Signals a `user-error' when the formula has no source file."
  (interactive
   (list (completing-read
          "Formula: "
          (mapcar (lambda (f) (oref f name))
                  (beads-command-execute
                   (beads-command-formula-list :json t)))
          nil t)))
  (let* ((name (cond ((stringp formula) formula)
                     ((or (beads-formula-p formula)
                          (beads-formula-summary-p formula))
                      (oref formula name))
                     (t nil)))
         (source (beads-formula-edit--resolve-source formula)))
    (unless (and source (not (string-empty-p source)))
      (user-error "Formula %s has no source file" (or name "?")))
    (beads-formula-edit--visit (beads-formula-edit--localize source) name)))

(defun beads-formula-edit-source-at-point ()
  "Open the source of the formula at point in a formula view."
  (interactive)
  (let ((name (cond
               ((derived-mode-p 'beads-formula-list-mode)
                (and (fboundp 'beads-formula-list--current-formula-name)
                     (beads-formula-list--current-formula-name)))
               ((derived-mode-p 'beads-formula-show-mode)
                (bound-and-true-p beads-formula-show--formula-name))
               (t nil))))
    (unless name
      (user-error "No formula at point"))
    (beads-formula-edit-source name)))

;;; ============================================================
;;; Schema completions (REQ-SF-061)
;;; ============================================================

(defvar beads-formula-edit--schema-cache nil
  "Cached `beads-formula-schema-struct' list for the current store.
Cleared by `beads-formula-edit-forget-schema'.")

(defun beads-formula-edit-forget-schema ()
  "Forget the cached formula schema."
  (interactive)
  (setq beads-formula-edit--schema-cache nil))

(defun beads-formula-edit--schema ()
  "Return the formula schema structs, fetching `bd formula schema' once.
Returns nil when `bd' is unavailable or the fetch fails."
  (or beads-formula-edit--schema-cache
      (setq beads-formula-edit--schema-cache
            (condition-case nil
                (beads-execute 'beads-command-formula-schema :json t)
              (error nil)))))

(defun beads-formula-edit--section (text pos)
  "Return the TOML section name enclosing POS in TEXT.
The empty string means the document top level."
  (let ((section ""))
    (dolist (line (split-string (substring text 0 (min pos (length text)))
                                "\n"))
      (when (string-match "\\`[ \t]*\\[\\[?\\([^]]+\\)\\]\\]?[ \t]*\\'" line)
        (setq section (string-trim (match-string 1 line)))))
    section))

(defun beads-formula-edit--struct-for-section (section)
  "Return the schema struct name backing SECTION, or `Formula'."
  (let ((head (car (split-string (or section "") "\\."))))
    (or (cdr (assoc head '(("" . "Formula")
                           ("vars" . "VarDef")
                           ("steps" . "Step")
                           ("template" . "Step")
                           ("compose" . "ComposeRules")
                           ("advice" . "AdviceRule")
                           ("pointcut" . "Pointcut")
                           ("pointcuts" . "Pointcut")
                           ("gate" . "Gate")
                           ("rules" . "GateRule")
                           ("hooks" . "Hook"))))
        "Formula")))

(defun beads-formula--schema-completions (schema &optional prefix section)
  "Return schema field-name completions from SCHEMA.
PREFIX filters by prefix; SECTION selects the struct (top level when
nil).  Each candidate carries the source `beads-formula-schema-field'
under its `beads-formula-schema-field' text property."
  (when schema
    (let* ((struct-name (beads-formula-edit--struct-for-section section))
           (struct (cl-find struct-name schema
                            :key (lambda (s) (oref s name)) :test #'equal)))
      (delq nil
            (mapcar
             (lambda (field)
               (let ((name (oref field json-name)))
                 (when (and name
                            (or (null prefix) (string-empty-p prefix)
                                (string-prefix-p prefix name)))
                   (propertize name 'beads-formula-schema-field field))))
             (oref struct fields))))))

(defun beads-formula-edit-completion-at-point ()
  "Completion-at-point function for formula authoring buffers.
Completions are the schema fields of the enclosing TOML section."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point)))
         (prefix (buffer-substring-no-properties start end))
         (text (buffer-substring-no-properties (point-min) (point)))
         (section (beads-formula-edit--section text (point)))
         (schema (beads-formula-edit--schema)))
    (when schema
      (list start end
            (beads-formula--schema-completions schema prefix section)
            :annotation-function
            (lambda (candidate)
              (when-let* ((field (get-text-property
                                  0 'beads-formula-schema-field candidate)))
                (propertize (format " %s%s" (oref field type)
                                    (if (oref field required) " required" ""))
                            'face (if (oref field required)
                                      'beads-formula-edit-required
                                    'font-lock-comment-face))))))))

;;; ============================================================
;;; Validation (REQ-SF-061)
;;; ============================================================

(defconst beads-formula-edit-validate-buffer-name "*beads-formula-validate*"
  "Buffer name for formula validation results.")

(defconst beads-formula-edit--bd-line-regexp
  "line \\([0-9]+\\)\\(?: (last key \"[^\"]*\")?\\): \\(.*\\)$"
  "Regexp matching a `bd' TOML parse diagnostic line.")

(defun beads-formula-edit--toml-kind (value)
  "Classify the raw TOML VALUE string into a kind symbol.
Returns one of `string', `integer', `float', `boolean', `array',
`table', or nil when VALUE is not a recognisable scalar/container."
  (let ((v (string-trim (or value ""))))
    (cond
     ((string-empty-p v) nil)
     ((member v '("true" "false")) 'boolean)
     ((string-match-p "\\`[+-]?[0-9][0-9_]*\\'" v) 'integer)
     ((string-match-p "\\`[+-]?[0-9][0-9_]*\\.[0-9]" v) 'float)
     ((string-match-p "\\`[\"']" v) 'string)
     ((string-prefix-p "[" v) 'array)
     ((string-prefix-p "{" v) 'table)
     (t nil))))

(defun beads-formula-edit--schema-kind (go-type)
  "Return the expected TOML kind for the Go type string GO-TYPE, or nil."
  (let ((type (or go-type "")))
    (cond
     ((string-prefix-p "[]" type) 'array)
     ((or (string-prefix-p "map[" type) (string-prefix-p "*" type)) 'table)
     ((member type '("string" "FormulaType" "Phase")) 'string)
     ((member type '("int" "int32" "int64")) 'integer)
     ((member type '("float32" "float64")) 'float)
     ((string= type "bool") 'boolean)
     (t nil))))

(defun beads-formula-edit--scan-top-level (text)
  "Return top-level TOML assignments in TEXT as (KEY LINE VALUE) lists.
Only assignments before the first `[table]' header are returned."
  (let ((line-no 0)
        (section nil)
        (entries nil))
    (dolist (line (split-string (or text "") "\n"))
      (setq line-no (1+ line-no))
      (let ((trimmed (string-trim line)))
        (cond
         ((or (string-empty-p trimmed) (string-prefix-p "#" trimmed)) nil)
         ((string-match "\\`\\[\\[?\\(.+\\)\\]\\]?\\'" trimmed)
          (setq section (match-string 1 trimmed)))
         ((and (not section)
               (string-match
                "\\`\\([A-Za-z0-9_.-]+\\)[ \t]*=[ \t]*\\(.*\\)\\'" trimmed))
          (push (list (match-string 1 trimmed) line-no
                      (match-string 2 trimmed))
                entries)))))
    (nreverse entries)))

(defun beads-formula-edit--validate-text (text schema)
  "Return diagnostics for TEXT using SCHEMA structs.
Each diagnostic is a plist `(:line N :severity S :message M)'.  Checks
the required top-level `Formula' fields and the TOML kind of each
declared top-level field (REQ-SF-061).  Unknown keys are out of scope."
  (let ((formula
         (and schema
              (cl-find "Formula" schema
                       :key (lambda (s) (oref s name)) :test #'equal)))
        (entries (beads-formula-edit--scan-top-level text))
        (diagnostics nil))
    (when formula
      (dolist (field (oref formula fields))
        (when (and (oref field required)
                   (not (assoc-string (oref field json-name) entries)))
          (push (list :line 1 :severity 'error
                      :message (format "missing required field `%s'"
                                       (oref field json-name)))
                diagnostics)))
      (dolist (entry entries)
        (let* ((key (nth 0 entry))
               (line (nth 1 entry))
               (value (nth 2 entry))
               (field (cl-find key (oref formula fields)
                               :key (lambda (f) (oref f json-name))
                               :test #'equal)))
          (when field
            (let ((expected (beads-formula-edit--schema-kind (oref field type)))
                  (actual (beads-formula-edit--toml-kind value)))
              (when (and expected actual (not (eq expected actual)))
                (push (list :line line :severity 'error
                            :message
                            (format "field `%s' expects %s, got %s"
                                    key (oref field type) actual))
                      diagnostics)))))))
    (sort (nreverse diagnostics)
          (lambda (a b) (< (plist-get a :line) (plist-get b :line))))))

(defun beads-formula-edit--parse-bd-error (stderr)
  "Return diagnostics parsed from the `bd' STDERR text.
Recognises the `bd' TOML parser `line N: MESSAGE' shape and ignores
the later search-path lines."
  (let ((diagnostics nil))
    (dolist (line (split-string (or stderr "") "\n"))
      (when (string-match beads-formula-edit--bd-line-regexp line)
        (push (list :line (string-to-number (match-string 1 line))
                    :severity 'error
                    :message (string-trim (match-string 2 line)))
              diagnostics)))
    (nreverse diagnostics)))

(defun beads-formula-edit--bd-diagnostics (name)
  "Return parser diagnostics for the formula NAME from `bd formula show'.
Returns nil when the formula parses, is unknown, or `bd' is absent."
  (when (and name (stringp name) (not (string-empty-p name)))
    (condition-case err
        (progn
          (beads-command-execute
           (beads-command-formula-show :formula-name name :json t))
          nil)
      (beads-command-error
       (beads-formula-edit--parse-bd-error (plist-get (cddr err) :stderr)))
      (error nil))))

(defun beads-formula-edit--diagnostics (buffer)
  "Return the merged diagnostics for authoring BUFFER."
  (with-current-buffer buffer
    (let* ((text (buffer-string))
           (name (bound-and-true-p beads-formula-edit--formula-name))
           (schema (beads-formula-edit--schema)))
      (append (beads-formula-edit--validate-text text schema)
              (beads-formula-edit--bd-diagnostics name)))))

(defun beads-formula-edit--clear-diagnostics (buffer)
  "Remove inline diagnostic overlays from BUFFER."
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max)
                     'beads-formula-edit-diagnostic t)))

(defun beads-formula-edit--render-inline (buffer diagnostics)
  "Render DIAGNOSTICS as overlays in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (dolist (diagnostic diagnostics)
        (goto-char (point-min))
        (forward-line (max 0 (1- (or (plist-get diagnostic :line) 1))))
        (let ((overlay (make-overlay (line-beginning-position)
                                     (line-end-position))))
          (overlay-put overlay 'beads-formula-edit-diagnostic t)
          (overlay-put overlay 'face 'beads-formula-edit-error)
          (overlay-put overlay 'help-echo (plist-get diagnostic :message))
          (overlay-put overlay 'evaporate t))))))

(defvar beads-formula-edit-validate-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'beads-formula-edit-validate-jump)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `beads-formula-edit-validate-mode'.")

(define-derived-mode beads-formula-edit-validate-mode special-mode
  "Beads-Validate"
  "Major mode for formula validation results.
\\{beads-formula-edit-validate-mode-map}")

(defun beads-formula-edit--show-results (buffer diagnostics)
  "Show DIAGNOSTICS for BUFFER in the compile-style results buffer."
  (let ((out (get-buffer-create beads-formula-edit-validate-buffer-name)))
    (with-current-buffer out
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize
                 (format "%s — %d diagnostic%s\n\n"
                         (buffer-name buffer) (length diagnostics)
                         (if (= (length diagnostics) 1) "" "s"))
                 'face 'bold))
        (if (null diagnostics)
            (insert "✓ no diagnostics\n")
          (dolist (diagnostic diagnostics)
            (insert (propertize
                     (format "%s:%d: %s: %s\n"
                             (buffer-name buffer)
                             (or (plist-get diagnostic :line) 1)
                             (plist-get diagnostic :severity)
                             (plist-get diagnostic :message))
                     'beads-formula-edit-source buffer
                     'beads-formula-edit-line (or (plist-get diagnostic :line) 1)
                     'mouse-face 'highlight
                     'help-echo "RET: jump to diagnostic"))))
        (goto-char (point-min))
        (beads-formula-edit-validate-mode)))
    (display-buffer out)
    out))

(defun beads-formula-edit-validate-jump ()
  "Jump to the source of the diagnostic at point."
  (interactive)
  (let ((buffer (get-text-property (point) 'beads-formula-edit-source))
        (line (get-text-property (point) 'beads-formula-edit-line)))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "No diagnostic at point"))
    (pop-to-buffer buffer)
    (goto-char (point-min))
    (forward-line (max 0 (1- (or line 1))))))

;;;###autoload
(defun beads-formula-edit-validate (&optional buffer)
  "Validate formula authoring BUFFER against schema and `bd'.
Renders inline diagnostics and a compile-style results buffer, and
returns the diagnostic list (REQ-SF-061).  BUFFER defaults to the
current buffer."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (diagnostics (beads-formula-edit--diagnostics buffer)))
    (beads-formula-edit--clear-diagnostics buffer)
    (beads-formula-edit--render-inline buffer diagnostics)
    (beads-formula-edit--show-results buffer diagnostics)
    diagnostics))

(defvar-local beads-formula-edit--saving nil
  "Non-nil while `beads-formula-edit-save' is validating, to skip the hook.")

(defun beads-formula-edit-save ()
  "Validate the buffer, then save it.
Bound to \\[beads-formula-edit-save] in the authoring minor mode."
  (interactive)
  (let ((beads-formula-edit--saving t))
    (beads-formula-edit-validate)
    (save-buffer)))

(defun beads-formula-edit--after-save ()
  "Validate the buffer after every save (on-save validation)."
  (unless beads-formula-edit--saving
    (beads-formula-edit-validate (current-buffer))))

;;; ============================================================
;;; Convert JSON -> TOML (REQ-SF-062)
;;; ============================================================

(defvar beads-formula-edit-convert--state nil
  "Live state plist for `beads-formula-edit-convert--transient'.
Keys: `:source', `:stdout' and `:delete'.")

(defconst beads-formula-edit-convert-buffer-name "*beads-formula-convert*"
  "Buffer name for the `--stdout' conversion output.")

(defun beads-formula-edit-convert--get (key)
  "Return KEY from the live convert state, or nil."
  (plist-get beads-formula-edit-convert--state key))

(defun beads-formula-edit-convert--set (key value)
  "Set KEY to VALUE in the live convert state and return VALUE."
  (setq beads-formula-edit-convert--state
        (plist-put beads-formula-edit-convert--state key value))
  value)

(defun beads-formula-edit--json-source ()
  "Return the JSON formula path at point, or nil.
Recognises a buffer visiting a `*.formula.json'/`*.json' file."
  (let ((file (or (buffer-file-name) (bound-and-true-p beads-store-directory))))
    (when (and (stringp file)
               (string-match-p "\\.json\\'" file))
      file)))

(defun beads-formula-edit-convert--command (state)
  "Build the `beads-command-formula-convert' for STATE.
`:json nil' is required because `--stdout' emits TOML, not JSON."
  (beads-command-formula-convert
   :formula-name (plist-get state :source)
   :stdout (and (plist-get state :stdout) t)
   :delete (and (plist-get state :delete) t)
   :json nil))

(defun beads-formula-edit--show-converted (text)
  "Show converted TOML TEXT in the conversion output buffer."
  (let ((buffer (get-buffer-create beads-formula-edit-convert-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or text ""))
        (goto-char (point-min))
        (funcall (beads-formula-edit--major-mode))
        (setq buffer-read-only t)))
    (display-buffer buffer)
    buffer))

(defun beads-formula-edit-convert--apply (state)
  "Run the convert command for STATE and report the result.
Returns the command output; opens the stdout buffer for `--stdout'."
  (let* ((command (beads-formula-edit-convert--command state))
         (output (beads-command-execute command)))
    (if (plist-get state :stdout)
        (beads-formula-edit--show-converted output)
      (message "Converted %s to TOML%s"
               (plist-get state :source)
               (if (plist-get state :delete) " (source deleted)" "")))
    output))

;;;###autoload
(defun beads-formula-convert-at-point ()
  "Convert the JSON formula at point to TOML (REQ-SF-062).
Opens the conversion transient with `--stdout' and `--delete' toggles."
  (interactive)
  (let ((source (beads-formula-edit--json-source)))
    (unless source
      (user-error "Not a JSON formula buffer"))
    (setq beads-formula-edit-convert--state
          (list :source source :stdout nil :delete nil))
    (transient-setup 'beads-formula-edit-convert--transient)))

(transient-define-suffix beads-formula-edit-convert--pick-source ()
  "Set the JSON formula source path."
  :description (lambda ()
                 (format "Source: %s"
                         (or (beads-formula-edit-convert--get :source)
                             "(none)")))
  :transient t
  (interactive)
  (beads-formula-edit-convert--set
   :source (read-file-name "Formula JSON: "
                           (file-name-directory
                            (or (beads-formula-edit-convert--get :source)
                                default-directory))
                           nil t))
  (transient--redisplay))

(transient-define-suffix beads-formula-edit-convert--toggle-stdout ()
  "Toggle writing the TOML to stdout instead of a file."
  :description (lambda ()
                 (format "--stdout: %s"
                         (if (beads-formula-edit-convert--get :stdout)
                             "yes" "no")))
  :transient t
  (interactive)
  (beads-formula-edit-convert--set
   :stdout (not (beads-formula-edit-convert--get :stdout)))
  (transient--redisplay))

(transient-define-suffix beads-formula-edit-convert--toggle-delete ()
  "Toggle deleting the JSON source after conversion."
  :description (lambda ()
                 (format "Delete JSON after: %s"
                         (if (beads-formula-edit-convert--get :delete)
                             "yes" "no")))
  :transient t
  (interactive)
  (beads-formula-edit-convert--set
   :delete (not (beads-formula-edit-convert--get :delete)))
  (transient--redisplay))

(transient-define-suffix beads-formula-edit-convert--go ()
  "Run `bd formula convert'."
  :description "Convert"
  (interactive)
  (beads-formula-edit-convert--apply beads-formula-edit-convert--state)
  (transient-quit-one))

(beads-define-prefix beads-formula-edit-convert--transient ()
  "Convert a JSON formula to TOML (REQ-SF-062).

Chooses the JSON source, toggles `--stdout' (print instead of write)
and `--delete' (remove the JSON after conversion), then runs
`bd formula convert' with `C'.  Seeded by `beads-formula-convert-at-point'.
See menu-mockups.md §10d."
  ["Source"
   (beads-formula-edit-convert--pick-source)]
  ["Options"
   (beads-formula-edit-convert--toggle-stdout)
   (beads-formula-edit-convert--toggle-delete)]
  ["Actions"
   (beads-formula-edit-convert--go)
   ("q" "Quit" transient-quit-one)])

;;; ============================================================
;;; Authoring minor mode
;;; ============================================================

(defvar beads-formula-edit-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-formula-edit-save)
    (define-key map (kbd "C-c C-v") #'beads-formula-edit-validate)
    (define-key map (kbd "C-c C-x") #'beads-formula-convert-at-point)
    map)
  "Keymap for `beads-formula-edit-minor-mode'.")

(define-minor-mode beads-formula-edit-minor-mode
  "Minor mode for editing a formula `.formula.toml' source.

\\{beads-formula-edit-minor-mode-map}"
  :lighter " Formula"
  :keymap beads-formula-edit-minor-mode-map
  (if beads-formula-edit-minor-mode
      (progn
        (add-hook 'after-save-hook #'beads-formula-edit--after-save nil t)
        (add-hook 'completion-at-point-functions
                  #'beads-formula-edit-completion-at-point nil t))
    (remove-hook 'after-save-hook #'beads-formula-edit--after-save t)
    (remove-hook 'completion-at-point-functions
                 #'beads-formula-edit-completion-at-point t)))

;;; ============================================================
;;; Wiring into the formula views
;;; ============================================================

(when (boundp 'beads-formula-list-mode-map)
  (define-key beads-formula-list-mode-map (kbd "e")
              #'beads-formula-edit-source-at-point)
  (define-key beads-formula-list-mode-map (kbd "C")
              #'beads-formula-convert-at-point))
(when (boundp 'beads-formula-show-mode-map)
  (define-key beads-formula-show-mode-map (kbd "e")
              #'beads-formula-edit-source-at-point)
  (define-key beads-formula-show-mode-map (kbd "C")
              #'beads-formula-convert-at-point))

(provide 'beads-formula-edit)
;;; beads-formula-edit.el ends here
