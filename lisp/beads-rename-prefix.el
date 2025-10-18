;;; beads-rename-prefix.el --- Rename prefix command for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

;;; Commentary:

;; Provides transient menu for the bd rename-prefix command.
;; This is a DESTRUCTIVE operation that renames all issue IDs and
;; updates all text references across all fields.

;;; Code:

(require 'beads)
(require 'transient)

;;; Forward declarations
(declare-function beads-list-refresh "beads-list")

;;; Transient State Variables

(defvar beads-rename-prefix--new-prefix nil
  "New prefix for renaming all issues.")

(defvar beads-rename-prefix--dry-run nil
  "Whether to run in dry-run mode.")

;;; Utility Functions

(defun beads-rename-prefix--reset-state ()
  "Reset rename-prefix transient state."
  (setq beads-rename-prefix--new-prefix nil
        beads-rename-prefix--dry-run nil))

(defun beads-rename-prefix--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-rename-prefix--validate-prefix ()
  "Validate that new-prefix is set and valid.
Returns error message string if invalid, nil if valid.

Prefix validation rules:
- Max length: 8 characters
- Allowed: lowercase letters, numbers, hyphens
- Must start with a letter
- Must end with a hyphen"
  (cond
   ((or (null beads-rename-prefix--new-prefix)
        (string-empty-p (string-trim beads-rename-prefix--new-prefix)))
    "New prefix is required")
   ((> (length beads-rename-prefix--new-prefix) 8)
    "Prefix must be max 8 characters")
   ((not (string-match-p "^[a-z]" beads-rename-prefix--new-prefix))
    "Prefix must start with lowercase letter")
   ((not (string-match-p "-$" beads-rename-prefix--new-prefix))
    "Prefix must end with hyphen")
   ((not (string-match-p "^[a-z][a-z0-9-]*-$"
                         beads-rename-prefix--new-prefix))
    "Prefix must contain only lowercase letters, numbers, and hyphens")
   (t nil)))

(defun beads-rename-prefix--validate-all ()
  "Validate all parameters.
Returns list of error messages, or nil if all valid."
  (delq nil (list (beads-rename-prefix--validate-prefix))))

;;; Infix Commands

(transient-define-infix beads-rename-prefix--infix-new-prefix ()
  "Set the new prefix."
  :class 'transient-option
  :description (lambda ()
                 (concat "New prefix (required)"
                         (beads-rename-prefix--format-value
                          beads-rename-prefix--new-prefix)))
  :key "p"
  :argument "prefix="
  :prompt "New prefix (e.g., kw-): "
  :reader (lambda (_prompt _initial-input _history)
            (let ((prefix (read-string
                           "New prefix (e.g., kw-): "
                           beads-rename-prefix--new-prefix)))
              (setq beads-rename-prefix--new-prefix prefix)
              prefix)))

(transient-define-infix beads-rename-prefix--infix-dry-run ()
  "Toggle dry-run flag."
  :class 'transient-switch
  :description "Dry run (--dry-run)"
  :key "d"
  :argument "--dry-run"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-rename-prefix--dry-run
                  (not beads-rename-prefix--dry-run))
            beads-rename-prefix--dry-run))

;;; Suffix Commands

(defun beads-rename-prefix--execute-preview (new-prefix)
  "Preview rename-prefix operation for NEW-PREFIX.
Shows old->new ID mappings in a buffer."
  (condition-case err
      (let* ((output (with-temp-buffer
                       (let ((exit-code
                              (call-process beads-executable nil t nil
                                            "rename-prefix" new-prefix
                                            "--dry-run")))
                         (unless (zerop exit-code)
                           (error "Preview failed: %s" (buffer-string)))
                         (buffer-string))))
             (buf (get-buffer-create "*beads-rename-prefix-preview*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Preview: Rename to prefix '%s'\n" new-prefix))
            (insert "==========================================\n\n")
            (insert output)
            (goto-char (point-min))
            (special-mode)
            (local-set-key (kbd "q") 'quit-window)))
        (display-buffer buf)
        (message "Preview generated (see *beads-rename-prefix-preview*)")
        nil)
    (error
     (beads--error "Failed to generate preview: %s"
                   (error-message-string err)))))

(defun beads-rename-prefix--execute (new-prefix dry-run)
  "Execute bd rename-prefix with NEW-PREFIX.
DRY-RUN controls whether to actually rename or just preview."
  (condition-case err
      (let ((args (list "rename-prefix" new-prefix)))
        (when dry-run
          (setq args (append args (list "--dry-run"))))
        (let ((output (with-temp-buffer
                        (let ((exit-code
                               (apply #'call-process
                                      beads-executable nil t nil args)))
                          (unless (zerop exit-code)
                            (error "Rename failed: %s" (buffer-string)))
                          (buffer-string)))))
          ;; Display output in buffer
          (let ((buf (get-buffer-create "*beads-rename-prefix*")))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (format "Rename prefix to '%s'\n" new-prefix))
                (insert "==========================================\n\n")
                (insert output)
                (goto-char (point-min))
                (special-mode)
                (local-set-key (kbd "q") 'quit-window)))
            (display-buffer buf))
          (if dry-run
              (message "Dry run completed (see *beads-rename-prefix* buffer)")
            (progn
              (message "Prefix renamed to: %s" new-prefix)
              ;; Invalidate caches and refresh buffers
              (beads--invalidate-completion-cache)
              (beads--invalidate-project-cache)
              (when beads-auto-refresh
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    (when (and (fboundp 'beads-list-refresh)
                               (derived-mode-p 'beads-list-mode))
                      (beads-list-refresh)))))))
          nil))
    (error
     (beads--error "Failed to rename prefix: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-rename-prefix--preview-command ()
  "Preview the rename-prefix operation."
  :key "v"
  :description "Preview changes"
  :transient t
  (interactive)
  (let ((errors (beads-rename-prefix--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (beads-rename-prefix--execute-preview
       beads-rename-prefix--new-prefix))))

(defun beads-rename-prefix--confirm-rename (new-prefix)
  "Strong confirmation prompt for renaming to NEW-PREFIX.
Requires user to type the new prefix to confirm."
  (let ((confirmation (read-string
                       (format
                        "Type new prefix '%s' to confirm (destructive!): "
                        new-prefix))))
    (string= confirmation new-prefix)))

(transient-define-suffix beads-rename-prefix--execute-command ()
  "Execute the bd rename-prefix command."
  :key "r"
  :description "Rename prefix (DESTRUCTIVE!)"
  (interactive)
  (let ((errors (beads-rename-prefix--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (progn
        (message "WARNING: This will rename ALL issues and references!")
        (message "External references in GitHub, docs, commits will break!")
        (sit-for 2)
        (when (beads-rename-prefix--confirm-rename
               beads-rename-prefix--new-prefix)
          (beads-rename-prefix--execute beads-rename-prefix--new-prefix
                                        beads-rename-prefix--dry-run)
          (unless beads-rename-prefix--dry-run
            (beads-rename-prefix--reset-state)))))))

(transient-define-suffix beads-rename-prefix--reset ()
  "Reset all rename-prefix parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-rename-prefix--reset-state)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-rename-prefix "beads-rename-prefix" nil t)
(transient-define-prefix beads-rename-prefix ()
  "Rename the issue prefix for all issues in the database.

This is a DESTRUCTIVE operation that renames all issue IDs and
updates all text references across all fields. External references
in GitHub issues, documentation, and commit messages will not be
updated automatically.

Prefix validation rules:
- Max length: 8 characters
- Allowed characters: lowercase letters, numbers, hyphens
- Must start with a letter
- Must end with a hyphen (e.g., 'kw-', 'work-')

Use dry-run mode to preview changes before applying."
  :value (lambda () nil)
  (interactive)
  (beads-check-executable)
  ["Rename Prefix Parameters"
   (beads-rename-prefix--infix-new-prefix)]
  ["Options"
   (beads-rename-prefix--infix-dry-run)]
  ["Actions"
   ("v" "Preview changes" beads-rename-prefix--preview-command)
   ("r" "Rename prefix" beads-rename-prefix--execute-command)
   ("R" "Reset fields" beads-rename-prefix--reset)
   ("q" "Quit" transient-quit-one)])

;;; Footer

(provide 'beads-rename-prefix)
;;; beads-rename-prefix.el ends here
