;;; beads-cook.el --- Cook preview UI for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The `bd cook' porcelain (REQ-SF-012, mockup §3): the `beads-cook'
;; transient, the `--dry-run' step-tree parser and the preview buffer.
;;
;; `bd cook' compiles a formula into a proto.  Two modes exist:
;;
;; - compile: `{{variable}}' placeholders are kept (the default);
;; - runtime: variables are substituted, which `bd' selects when any
;;   `--var' is supplied or `--mode=runtime' is given.
;;
;; Nothing is written unless `--persist' is passed; without it `bd
;; cook' only prints the resolved formula.  `P' previews the exact
;; step/dependency tree with `bd cook --dry-run' (which never writes);
;; `s' cooks, writing a proto only when Persist is on.  Force requires
;; Persist, matching `bd'.
;;
;; The `beads-command-cook' EIEIO class lives in
;; `beads-command-cook.el'; this module owns the presentation.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'beads-util)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-command-cook)
(require 'beads-command-formula)
(require 'beads-prefix)

;;; ============================================================
;;; Dry-run step tree
;;; ============================================================

(defun beads-cook--split-csv (string)
  "Split STRING on commas, trimming whitespace; nil for an empty string."
  (when (and string (not (string-empty-p (string-trim string))))
    (mapcar #'string-trim (split-string string "," t "[ \t]+"))))

(defun beads-cook--parse-annotations (text)
  "Parse [key: value] annotations from TEXT into an alist.
Handles nested brackets in values (e.g. \"[from: f@steps[0]]\") and
several keys inside one bracket separated by commas (bd prints a step
with both as \"[depends: x, needs: y]\").  A comma that does not
introduce a known key continues the previous value."
  (let ((alist nil)
        (index 0)
        (length (length text)))
    (while (string-match "\\[\\([a-z_-]+\\):[ \t]*" text index)
      (let ((key (match-string 1 text))
            (start (match-end 0))
            (i nil)
            (depth 1)
            (end nil))
        (setq i start)
        (while (and (< i length) (not end))
          (let ((char (aref text i)))
            (cond ((eq char ?\[) (setq depth (1+ depth)))
                  ((eq char ?\])
                   (setq depth (1- depth))
                   (when (= depth 0) (setq end i)))))
          (setq i (1+ i)))
        (when end
          (let ((first t))
            (dolist (part (split-string (substring text start end) "," t))
              (let ((part (string-trim part)))
                (if (string-match "\\`\\([a-z_-]+\\):[ \t]*\\(.*\\)\\'" part)
                    (progn
                      (setq key (match-string 1 part))
                      (push (cons key (match-string 2 part)) alist))
                  (if first
                      (push (cons key part) alist)
                    (when alist
                      (setcdr (car alist)
                              (concat (cdr (car alist)) ", " part))))))
              (setq first nil)))
          (setq index (1+ end)))))
    (nreverse alist)))

(defun beads-cook--strip-brackets (text)
  "Remove the trailing [key: value] annotations from TEXT.
Annotations always start with a bracketed `key:', so everything from
the first annotation on is dropped."
  (let ((trimmed (string-trim text)))
    (if (string-match "\\s-*\\[[a-z_-]+:" trimmed)
        (string-trim (substring trimmed 0 (match-beginning 0)))
      trimmed)))

(defun beads-cook-parse-tree (output)
  "Parse the human-readable `bd cook --dry-run' OUTPUT into a step tree.

Returns a plist with these keys:
  :formula         the formula name being cooked (string or nil);
  :proto           the proto id `bd' would create (string or nil);
  :mode            \"compile\" or \"runtime\" (string or nil);
  :step-count      the declared step count (integer or nil);
  :variables-used  variable names `bd' reports as referenced (list);
  :steps           a list of step plists, each with :id, :title,
                   :needs, :depends-on and :from.

The parser is deliberately defensive: lines it does not recognise are
ignored, so a future `bd' format change degrades to a partial tree
instead of signalling."
  (when (and output (not (string-empty-p output)))
    (let ((formula nil) (proto nil) (mode nil) (step-count nil)
          (variables-used nil) (steps nil))
      (dolist (line (split-string output "\n"))
        (let ((trimmed (string-trim-left line)))
          (cond
           ;; "Dry run: would cook formula X as proto Y (compile-time mode)"
           ((string-match
             "would cook formula \\([^ \t]+\\) as proto \\([^ \t]+\\) (\\([a-z]+\\)\\(?:-time\\)? mode)"
             line)
            (setq formula (match-string 1 line)
                  proto (match-string 2 line)
                  mode (match-string 3 line)))
           ;; "Steps (4) [{{variables}} shown as placeholders]:"
           ((string-match "\\`Steps (\\([0-9]+\\))" trimmed)
            (setq step-count (string-to-number (match-string 1 trimmed))))
           ;; "  ├── prepare: Implement {{context_path}} [needs: ...] ..."
           ((string-match
             "\\`[ \t]*[│├└─`|+]+[ \t]+\\([^:]+\\):[ \t]*\\(.*\\)\\'"
             line)
            (let* ((id (string-trim (match-string 1 line)))
                   (rest (match-string 2 line))
                   (annotations (beads-cook--parse-annotations rest)))
              (push (list :id id
                          :title (beads-cook--strip-brackets rest)
                          :needs (beads-cook--split-csv
                                  (cdr (assoc "needs" annotations)))
                          :depends-on (beads-cook--split-csv
                                       (cdr (assoc "depends" annotations)))
                          :from (cdr (assoc "from" annotations)))
                    steps)))
           ;; "Variables used: context_path, implementation_target"
           ((string-match "\\`Variables used:[ \t]*\\(.*\\)\\'" trimmed)
            (setq variables-used
                  (beads-cook--split-csv (match-string 1 trimmed)))))))
      (list :formula formula
            :proto proto
            :mode mode
            :step-count step-count
            :variables-used variables-used
            :steps (nreverse steps)))))

;;; ============================================================
;;; Preview buffer
;;; ============================================================

(defvar-local beads-cook-preview--formula nil
  "Formula name rendered by the current cook preview buffer.")
(defvar-local beads-cook-preview--mode nil
  "Cooking mode rendered by the current cook preview buffer.")
(defvar-local beads-cook-preview--vars nil
  "Variable list rendered by the current cook preview buffer.")

(defun beads-cook--preview-buffer-name (formula)
  "Return the preview buffer name for FORMULA in the current project."
  (format "*beads-cook-preview[%s]/%s*"
          (beads--project-name-for-root
           (or (beads--project-root) default-directory))
          formula))

(defun beads-cook--render-step (step)
  "Insert one parsed STEP into the current preview buffer."
  (insert (format "  %-16s %s"
                  (or (plist-get step :id) "")
                  (or (plist-get step :title) "")))
  (when-let* ((needs (plist-get step :needs)))
    (insert (format "  [needs: %s]" (mapconcat #'identity needs ", "))))
  (when-let* ((depends (plist-get step :depends-on)))
    (insert (format "  [depends: %s]" (mapconcat #'identity depends ", "))))
  (insert "\n"))

(defun beads-cook--render-tree (tree)
  "Render parsed TREE into the current preview buffer."
  (let ((inhibit-read-only t)
        (steps (plist-get tree :steps)))
    (erase-buffer)
    (insert (propertize
             (format "Cook preview — %s (%s, dry-run)\n"
                     (or (plist-get tree :formula) "?")
                     (or (plist-get tree :mode) "compile"))
             'face 'font-lock-function-name-face))
    (insert (make-string 72 ?─) "\n")
    (insert (format "Would create proto %s (+ %d child step%s)\n"
                    (or (plist-get tree :proto)
                        (plist-get tree :formula)
                        "?")
                    (length steps)
                    (if (= (length steps) 1) "" "s")))
    (insert "\nSteps:\n")
    (dolist (step steps)
      (beads-cook--render-step step))
    (when-let* ((used (plist-get tree :variables-used)))
      (insert (format "\nVariables used: %s\n"
                      (mapconcat #'identity used ", "))))
    (insert "\nPersist: no (preview only)\n")
    (insert (make-string 72 ?─) "\n")
    (goto-char (point-min))))

(defun beads-cook-preview-refresh ()
  "Re-run the dry-run preview for the current cook preview buffer."
  (interactive)
  (unless beads-cook-preview--formula
    (user-error "No formula associated with this buffer"))
  (beads-cook-preview beads-cook-preview--formula
                      beads-cook-preview--mode
                      beads-cook-preview--vars))

(defvar beads-cook-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'beads-cook-preview-refresh)
    map)
  "Keymap for `beads-cook-preview-mode'.")

(define-derived-mode beads-cook-preview-mode special-mode "Beads-Cook"
  "Major mode for the `bd cook --dry-run' step-tree preview.

\\{beads-cook-preview-mode-map}"
  (setq truncate-lines nil)
  (visual-line-mode 1))

;;; ============================================================
;;; Preview / cook entry points
;;; ============================================================

(defun beads-cook--formula-at-point ()
  "Return the formula name at point in a formula browser/detail buffer."
  (cond
   ((derived-mode-p 'beads-formula-list-mode)
    (let ((id (tabulated-list-get-id)))
      (and (stringp id) id)))
   ((derived-mode-p 'beads-formula-show-mode)
    (bound-and-true-p beads-formula-show--formula-name))
   (t nil)))

(defun beads-cook--read-formula ()
  "Prompt for a formula name, defaulting to the one at point."
  (let ((names (mapcar (lambda (formula) (oref formula name))
                       (beads-command-execute
                        (beads-command-formula-list :json t)))))
    (completing-read "Formula: " names nil t nil nil
                     (beads-cook--formula-at-point))))

;;;###autoload
(defun beads-cook-preview (formula &optional mode vars)
  "Preview cooking FORMULA without writing anything.

Runs `bd cook --dry-run' in MODE (compile by default, runtime when
MODE is \"runtime\" or VARS are supplied), parses the step tree and
renders it in a `beads-cook-preview-mode' buffer.  VARS is a list of
\"key=value\" strings.  Returns the parsed tree; nothing is written."
  (interactive
   (list (or (beads-cook--formula-at-point)
             (beads-cook--read-formula))
         nil nil))
  (beads-check-executable)
  (let* ((command (beads-command-cook
                   :formula-id formula
                   :dry-run t
                   :mode mode
                   :var vars
                   :json nil))
         (tree (beads-cook-parse-tree (beads-command-execute command)))
         (buffer (get-buffer-create (beads-cook--preview-buffer-name formula))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-cook-preview-mode)
        (beads-cook-preview-mode))
      (setq-local beads-cook-preview--formula formula
                  beads-cook-preview--mode mode
                  beads-cook-preview--vars vars
                  default-directory (or (beads--project-root)
                                        default-directory))
      (beads-cook--render-tree tree))
    (beads-buffer-display-same-or-reuse buffer)
    tree))

(defun beads-cook--vars-from-args (args)
  "Return every \"key=value\" value of --var in transient ARGS."
  (delq nil
        (mapcar (lambda (arg)
                  (when (string-match "\\`--var=\\(.*\\)\\'" arg)
                    (match-string 1 arg)))
                args)))

(transient-define-suffix beads-cook--preview ()
  "Preview the cook step tree with `bd cook --dry-run'."
  :key "P"
  :description "Full preview (dry-run)"
  (interactive)
  (let* ((args (transient-args 'beads-cook))
         (formula (or (beads-cook--formula-at-point)
                      (beads-cook--read-formula))))
    (beads-cook-preview formula
                        (transient-arg-value "--mode=" args)
                        (beads-cook--vars-from-args args))))

(transient-define-suffix beads-cook--execute ()
  "Cook the formula, writing a proto only when Persist is enabled."
  :key "s"
  :description "Cook"
  (interactive)
  (let* ((args (transient-args 'beads-cook))
         (formula (or (beads-cook--formula-at-point)
                      (beads-cook--read-formula)))
         (persist (transient-arg-value "--persist" args))
         (force (transient-arg-value "--force" args)))
    (when (and force (not persist))
      (user-error "Force requires persist"))
    (beads-command-execute-interactive
     (beads-command-cook
      :formula-id formula
      :mode (transient-arg-value "--mode=" args)
      :var (beads-cook--vars-from-args args)
      :persist persist
      :force force
      :prefix (transient-arg-value "--prefix=" args)))))

;;;###autoload (autoload 'beads-cook "beads-cook" nil t)
(beads-define-prefix beads-cook ()
  "Cook a formula: compile or runtime, preview, optional persist.

`P' previews the exact step/dependency tree with `bd cook --dry-run'
and never writes.  `s' cooks the formula; a proto is written only when
Persist is on.  Force requires Persist.  Proto prefix sets the id
prefix (for example \"gt-\"), and Variable adds a key=value
substitution, which switches `bd' to runtime mode."
  ["Options"
   ("-m" "Mode" "--mode=" :choices ("compile" "runtime"))
   ("-p" "Persist" "--persist")
   ("-f" "Force (with persist)" "--force")
   ("-x" "Proto prefix" "--prefix=" :prompt "Proto ID prefix: ")
   ("-v" "Variable" "--var=" :prompt "Variable (key=value): "
    :multi-value t)]
  ["Actions"
   ("P" "Full preview (dry-run)" beads-cook--preview)
   ("s" "Cook" beads-cook--execute)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-cook)
;;; beads-cook.el ends here
