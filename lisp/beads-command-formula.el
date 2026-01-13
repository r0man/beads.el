;;; beads-command-formula.el --- Formula command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides EIEIO classes and UI for the `bd formula' commands.
;; It implements:
;;
;; - beads-command-formula-list: EIEIO class for `bd formula list'
;; - beads-command-formula-show: EIEIO class for `bd formula show'
;; - beads-formula-list-mode: tabulated-list-mode for displaying formulas
;; - beads-formula-show: Show individual formula details in TOML-like view
;;
;; Usage:
;;   M-x beads-formula-list    ; List available formulas
;;   RET on formula            ; Show formula details
;;   g                         ; Refresh list
;;   q                         ; Quit buffer

;;; Code:

(require 'beads)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'transient)

;;; Forward Declarations

(defvar beads-executable)

;;; Customization

(defgroup beads-formula nil
  "Formula management for Beads."
  :group 'beads
  :prefix "beads-formula-")

(defcustom beads-formula-list-name-width 25
  "Width of Name column in formula list."
  :type 'integer
  :group 'beads-formula)

(defcustom beads-formula-list-type-width 12
  "Width of Type column in formula list."
  :type 'integer
  :group 'beads-formula)

(defcustom beads-formula-list-steps-width 6
  "Width of Steps column in formula list."
  :type 'integer
  :group 'beads-formula)

(defcustom beads-formula-list-vars-width 5
  "Width of Vars column in formula list."
  :type 'integer
  :group 'beads-formula)

(defcustom beads-formula-list-description-width 50
  "Width of Description column in formula list."
  :type 'integer
  :group 'beads-formula)

;;; Faces

(defface beads-formula-type-workflow
  '((t :inherit font-lock-function-name-face))
  "Face for workflow formula type."
  :group 'beads-formula)

(defface beads-formula-type-expansion
  '((t :inherit font-lock-variable-name-face))
  "Face for expansion formula type."
  :group 'beads-formula)

(defface beads-formula-type-aspect
  '((t :inherit font-lock-constant-face))
  "Face for aspect formula type."
  :group 'beads-formula)

(defface beads-formula-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headers in formula show buffer."
  :group 'beads-formula)

(defface beads-formula-label-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for metadata labels in formula show buffer."
  :group 'beads-formula)

(defface beads-formula-value-face
  '((t :inherit default))
  "Face for metadata values in formula show buffer."
  :group 'beads-formula)

;;; ============================================================
;;; Formula List Command Class
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-formula-list (beads-command-json)
    ((formula-type
      :initarg :formula-type
      :type (or null string)
      :initform nil
      :documentation "Filter by formula type (--type).
Values: workflow, expansion, aspect."
      :long-option "--type"
      :option-type :string
      :transient-key "t"
      :transient-description "Filter by type"
      :transient-class transient-option
      :transient-argument "--type="
      :transient-choices ("workflow" "expansion" "aspect")
      :transient-group "Filters"
      :transient-level 2
      :transient-order 1))
    :documentation "Represents bd formula list command.
Lists available formulas from all search paths."))

(cl-defmethod beads-command-subcommand ((_command beads-command-formula-list))
  "Return subcommand name for formula list."
  "formula list")

(cl-defmethod beads-command-validate ((_command beads-command-formula-list))
  "Validate formula list command.  No required fields."
  nil)

(cl-defmethod beads-command-parse ((command beads-command-formula-list))
  "Parse COMMAND output into beads-formula-summary objects."
  (with-slots (stdout) command
    (when (and stdout (not (string-empty-p stdout)))
      (let* ((json-array-type 'list)
             (json-object-type 'alist)
             (json-data (json-read-from-string stdout)))
        (mapcar #'beads-formula-summary-from-json json-data)))))

;;; ============================================================
;;; Formula Show Command Class
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-formula-show (beads-command-json)
    ((formula-name
      :initarg :formula-name
      :type (or null string)
      :initform nil
      :documentation "Name of the formula to show (positional argument)."
      :positional 1))
    :documentation "Represents bd formula show command.
Shows detailed information about a formula."))

(cl-defmethod beads-command-subcommand ((_command beads-command-formula-show))
  "Return subcommand name for formula show."
  "formula show")

(cl-defmethod beads-command-validate ((command beads-command-formula-show))
  "Validate COMMAND.  Requires formula name."
  (with-slots (formula-name) command
    (unless (and formula-name (not (string-empty-p formula-name)))
      "Formula name is required")))

(cl-defmethod beads-command-parse ((command beads-command-formula-show))
  "Parse COMMAND output into beads-formula object."
  (with-slots (stdout) command
    (when (and stdout (not (string-empty-p stdout)))
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-data (json-read-from-string stdout)))
        (beads-formula-from-json json-data)))))

;;; ============================================================
;;; Formula List Buffer Variables
;;; ============================================================

(defvar-local beads-formula-list--formulas nil
  "List of beads-formula-summary objects for current buffer.")

(defvar-local beads-formula-list--command-obj nil
  "Command object used to populate this buffer.")

(defvar-local beads-formula-list--project-dir nil
  "Project directory for this buffer.")

;;; ============================================================
;;; Formula List Utilities
;;; ============================================================

(defun beads-formula-list--type-face (type)
  "Return face for formula TYPE."
  (pcase type
    ("workflow" 'beads-formula-type-workflow)
    ("expansion" 'beads-formula-type-expansion)
    ("aspect" 'beads-formula-type-aspect)
    (_ 'default)))

(defun beads-formula-list--format-type (type)
  "Format formula TYPE with appropriate face."
  (propertize (or type "") 'face (beads-formula-list--type-face type)))

(defun beads-formula-list--formula-to-entry (formula)
  "Convert FORMULA (beads-formula-summary) to tabulated-list entry."
  (let* ((name (or (oref formula name) ""))
         (type (oref formula formula-type))
         (desc (or (oref formula description) ""))
         (steps (or (oref formula steps) 0))
         (vars (or (oref formula vars) 0)))
    (list name
          (vector name
                  (beads-formula-list--format-type type)
                  (number-to-string steps)
                  (number-to-string vars)
                  desc))))

(defun beads-formula-list--populate-buffer (formulas &optional command-obj)
  "Populate current buffer with FORMULAS.
Optional COMMAND-OBJ is stored for refresh."
  (setq beads-formula-list--formulas formulas
        beads-formula-list--command-obj command-obj
        tabulated-list-entries
        (mapcar #'beads-formula-list--formula-to-entry formulas))
  (tabulated-list-print t))

(defun beads-formula-list--current-formula-name ()
  "Return the name of the formula at point, or nil."
  (tabulated-list-get-id))

(defun beads-formula-list--get-formula-by-name (name)
  "Return beads-formula-summary for NAME from current buffer."
  (seq-find (lambda (f) (string= (oref f name) name))
            beads-formula-list--formulas))

;;; ============================================================
;;; Formula List Commands
;;; ============================================================

(defun beads-formula-list-refresh ()
  "Refresh the formula list buffer."
  (interactive)
  (let* ((command (or beads-formula-list--command-obj
                      (beads-command-formula-list :json t)))
         (_ (beads-command-execute command))
         (formulas (oref command data)))
    (beads-formula-list--populate-buffer formulas command)
    (message "Found %d formula%s"
             (length formulas)
             (if (= (length formulas) 1) "" "s"))))

(defun beads-formula-list-show ()
  "Show details for the formula at point."
  (interactive)
  (if-let* ((name (beads-formula-list--current-formula-name)))
      (beads-formula-show name)
    (user-error "No formula at point")))

(defun beads-formula-list-quit ()
  "Quit the formula list buffer."
  (interactive)
  (quit-window t))

(defun beads-formula-list-next ()
  "Move to the next formula."
  (interactive)
  (forward-line 1))

(defun beads-formula-list-previous ()
  "Move to the previous formula."
  (interactive)
  (forward-line -1))

(defun beads-formula-list-open-source ()
  "Open the source file of the formula at point."
  (interactive)
  (if-let* ((name (beads-formula-list--current-formula-name))
            (formula (beads-formula-list--get-formula-by-name name))
            (source (oref formula source)))
      (find-file source)
    (user-error "No formula at point or source not available")))

;;; ============================================================
;;; Formula List Mode
;;; ============================================================

(defvar beads-formula-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "n") #'beads-formula-list-next)
    (define-key map (kbd "p") #'beads-formula-list-previous)
    (define-key map (kbd "RET") #'beads-formula-list-show)
    (define-key map (kbd "g") #'beads-formula-list-refresh)
    (define-key map (kbd "q") #'beads-formula-list-quit)
    (define-key map (kbd "o") #'beads-formula-list-open-source)
    map)
  "Keymap for `beads-formula-list-mode'.")

(define-derived-mode beads-formula-list-mode tabulated-list-mode "Beads-Formulas"
  "Major mode for displaying Beads formulas in a tabulated list.

\\{beads-formula-list-mode-map}"
  (setq tabulated-list-format
        (vector (list "Name" beads-formula-list-name-width t)
                (list "Type" beads-formula-list-type-width t)
                (list "Steps" beads-formula-list-steps-width t
                      :right-align t)
                (list "Vars" beads-formula-list-vars-width t
                      :right-align t)
                (list "Description" beads-formula-list-description-width t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header)
  (hl-line-mode 1))

;;; ============================================================
;;; Formula Show Buffer
;;; ============================================================

(defvar-local beads-formula-show--formula-name nil
  "Name of the formula displayed in current buffer.")

(defvar-local beads-formula-show--formula-data nil
  "Full beads-formula object for current buffer.")

(defvar-local beads-formula-show--project-dir nil
  "Project directory for this buffer.")

(defun beads-formula-show--render-header (label value)
  "Render a LABEL: VALUE header line."
  (insert (propertize (format "%-12s" (concat label ":"))
                      'face 'beads-formula-label-face)
          (propertize (or value "")
                      'face 'beads-formula-value-face)
          "\n"))

(defun beads-formula-show--render-section (title)
  "Render a section TITLE."
  (insert "\n"
          (propertize title 'face 'beads-formula-header-face)
          "\n"
          (make-string (length title) ?=)
          "\n\n"))

(defun beads-formula-show--render-var (name def)
  "Render variable NAME with definition DEF."
  (let ((desc (alist-get 'description def))
        (default (alist-get 'default def))
        (required (alist-get 'required def)))
    (insert (propertize (format "  %s" name) 'face 'font-lock-variable-name-face))
    (when required
      (insert (propertize " (required)" 'face 'font-lock-warning-face)))
    (insert "\n")
    (when desc
      (insert (format "    %s\n" desc)))
    (when default
      (insert (format "    Default: %s\n" default)))
    (insert "\n")))

(defun beads-formula-show--render-step (step index)
  "Render STEP at INDEX."
  (let ((id (alist-get 'id step))
        (title (alist-get 'title step))
        (description (alist-get 'description step))
        (needs (alist-get 'needs step)))
    (insert (propertize (format "%d. %s" (1+ index) (or title id))
                        'face 'font-lock-function-name-face)
            "\n")
    (when (and id title)
      (insert (format "   ID: %s\n" id)))
    (when needs
      (insert (format "   Needs: %s\n" (mapconcat #'identity needs ", "))))
    (when description
      (insert "\n"
              (replace-regexp-in-string "^" "   " description)
              "\n"))
    (insert "\n")))

(defun beads-formula-show--render (formula)
  "Render FORMULA in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Header
    (insert (propertize (format "Formula: %s\n" (oref formula name))
                        'face '(:inherit font-lock-keyword-face :height 1.3))
            "\n")
    ;; Metadata
    (beads-formula-show--render-header "Type" (oref formula formula-type))
    (beads-formula-show--render-header "Version"
                                       (when (oref formula version)
                                         (number-to-string (oref formula version))))
    (beads-formula-show--render-header "Source" (oref formula source))
    ;; Description
    (when-let ((desc (oref formula description)))
      (beads-formula-show--render-section "Description")
      (insert desc "\n"))
    ;; Variables
    (when-let ((vars (oref formula vars)))
      (beads-formula-show--render-section "Variables")
      (let ((var-names (mapcar #'car vars)))
        (dolist (name var-names)
          (beads-formula-show--render-var (symbol-name name)
                                          (alist-get name vars)))))
    ;; Steps
    (when-let ((steps (oref formula steps)))
      (beads-formula-show--render-section
       (format "Steps (%d)" (length steps)))
      (cl-loop for step in steps
               for idx from 0
               do (beads-formula-show--render-step step idx)))
    (goto-char (point-min))))

;;; ============================================================
;;; Formula Show Commands
;;; ============================================================

(defun beads-formula-show-refresh ()
  "Refresh the formula show buffer."
  (interactive)
  (unless beads-formula-show--formula-name
    (user-error "No formula associated with this buffer"))
  (let* ((command (beads-command-formula-show
                   :formula-name beads-formula-show--formula-name
                   :json t))
         (_ (beads-command-execute command))
         (formula (oref command data)))
    (setq beads-formula-show--formula-data formula)
    (beads-formula-show--render formula)
    (message "Refreshed formula: %s" beads-formula-show--formula-name)))

(defun beads-formula-show-quit ()
  "Quit the formula show buffer."
  (interactive)
  (quit-window t))

(defun beads-formula-show-open-source ()
  "Open the source file of the current formula."
  (interactive)
  (if-let* ((formula beads-formula-show--formula-data)
            (source (oref formula source)))
      (find-file source)
    (user-error "No source file available")))

;;; ============================================================
;;; Formula Show Mode
;;; ============================================================

(defvar beads-formula-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'beads-formula-show-refresh)
    (define-key map (kbd "q") #'beads-formula-show-quit)
    (define-key map (kbd "o") #'beads-formula-show-open-source)
    (define-key map (kbd "n") #'forward-paragraph)
    (define-key map (kbd "p") #'backward-paragraph)
    map)
  "Keymap for `beads-formula-show-mode'.")

(define-derived-mode beads-formula-show-mode special-mode "Beads-Formula"
  "Major mode for displaying Beads formula details.

\\{beads-formula-show-mode-map}"
  (setq truncate-lines nil)
  (visual-line-mode 1))

;;; ============================================================
;;; Public Entry Points
;;; ============================================================

;;;###autoload
(defun beads-formula-list (&optional type)
  "Display available formulas in a tabulated list.
Optional TYPE filters by formula type (workflow, expansion, aspect)."
  (interactive)
  (beads-check-executable)
  (let* ((project-dir (or (beads-git-find-project-root) default-directory))
         (command (beads-command-formula-list
                   :json t
                   :formula-type type))
         (_ (beads-command-execute command))
         (formulas (oref command data))
         (buf-name (format "*beads-formula-list[%s]*"
                          (beads-git-get-project-name)))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-formula-list-mode)
        (beads-formula-list-mode))
      (setq beads-formula-list--project-dir project-dir)
      (setq default-directory project-dir)
      (if (not formulas)
          (progn
            (setq tabulated-list-entries nil)
            (tabulated-list-print t)
            (message "No formulas found"))
        (beads-formula-list--populate-buffer formulas command)
        (message "Found %d formula%s"
                 (length formulas)
                 (if (= (length formulas) 1) "" "s"))))
    (beads-buffer-display-same-or-reuse buffer)))

;;;###autoload
(defun beads-formula-show (formula-name)
  "Show details for FORMULA-NAME."
  (interactive
   (list (completing-read "Formula: "
                          (mapcar (lambda (f) (oref f name))
                                  (beads-command-formula-list! :json t))
                          nil t)))
  (beads-check-executable)
  (let* ((project-dir (or (beads-git-find-project-root) default-directory))
         (command (beads-command-formula-show
                   :formula-name formula-name
                   :json t))
         (_ (beads-command-execute command))
         (formula (oref command data))
         (buf-name (format "*beads-formula[%s]/%s*"
                           (beads-git-get-project-name)
                           formula-name))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-formula-show-mode)
        (beads-formula-show-mode))
      (setq beads-formula-show--project-dir project-dir
            beads-formula-show--formula-name formula-name
            beads-formula-show--formula-data formula
            default-directory project-dir)
      (beads-formula-show--render formula))
    (beads-buffer-display-same-or-reuse buffer)))

;;; ============================================================
;;; Transient Menu
;;; ============================================================

;;;###autoload (autoload 'beads-formula-menu "beads-command-formula" nil t)
(transient-define-prefix beads-formula-menu ()
  "Manage workflow formulas."
  ["Actions"
   ("l" "List formulas" beads-formula-list)
   ("s" "Show formula" beads-formula-show)]
  ["Filters"
   ("-t" "Type" "--type="
    :choices ("workflow" "expansion" "aspect"))])

(provide 'beads-command-formula)
;;; beads-command-formula.el ends here
