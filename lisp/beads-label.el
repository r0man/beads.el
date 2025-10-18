;;; beads-label.el --- Label management for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

;;; Commentary:

;; Provides transient menu interfaces for Beads label commands:
;; - bd label add: Add a label to an issue
;; - bd label remove: Remove a label from an issue
;; - bd label list: List labels for a specific issue
;; - bd label list-all: List all unique labels in the database
;;
;; All menus follow the patterns established in beads-create.el and
;; beads-update.el. They provide context-aware issue detection,
;; validation, and integration with beads-list and beads-show buffers.

;;; Code:

(require 'beads)
(require 'transient)

;;; Forward declarations
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-show--issue-id "beads-show")
(declare-function beads-refresh-show "beads-show")

;;; ============================================================
;;; bd label add
;;; ============================================================

;;; Transient State Variables

(defvar beads-label--issue-id nil
  "Issue ID for label operations.")

(defvar beads-label--label nil
  "Label to add or remove.")

;;; Utility Functions

(defun beads-label--reset-state ()
  "Reset label transient state."
  (setq beads-label--issue-id nil
        beads-label--label nil))

(defun beads-label--detect-issue-id ()
  "Detect issue ID from current context.
Returns issue ID string or nil if not found."
  (or
   ;; From beads-list buffer
   (when (and (fboundp 'beads-list--current-issue-id)
              (derived-mode-p 'beads-list-mode))
     (beads-list--current-issue-id))
   ;; From beads-show buffer
   (when (and (boundp 'beads-show--issue-id)
              (derived-mode-p 'beads-show-mode))
     beads-show--issue-id)
   ;; From buffer name (*beads-show: bd-N*)
   (when (string-match "\\*beads-show: \\(bd-[0-9]+\\)\\*"
                       (buffer-name))
     (match-string 1 (buffer-name)))))

(defun beads-label--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-label--validate-issue-id ()
  "Validate that issue ID is set.
Returns error message string if invalid, nil if valid."
  (when (or (null beads-label--issue-id)
            (string-empty-p (string-trim beads-label--issue-id)))
    "Issue ID is required"))

(defun beads-label--validate-label ()
  "Validate that label is set.
Returns error message string if invalid, nil if valid."
  (when (or (null beads-label--label)
            (string-empty-p (string-trim beads-label--label)))
    "Label is required"))

(defun beads-label--validate-all ()
  "Validate all parameters.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-label--validate-issue-id)
              (beads-label--validate-label))))

(defun beads-label--get-available-issues ()
  "Get list of available issue IDs."
  (condition-case nil
      (mapcar (lambda (i) (alist-get 'id i))
              (beads--parse-issues (beads--run-command "list")))
    (error nil)))

(defun beads-label--get-all-labels ()
  "Get list of all labels in the database."
  (condition-case nil
      (let ((result (beads--run-command "label" "list-all")))
        (if (vectorp result)
            (append result nil)
          nil))
    (error nil)))

(defun beads-label--get-issue-labels (issue-id)
  "Get labels for ISSUE-ID."
  (condition-case nil
      (let ((result (beads--run-command "label" "list" issue-id)))
        (if (vectorp result)
            (append result nil)
          nil))
    (error nil)))

;;; Infix Commands

(transient-define-infix beads-label--infix-issue ()
  "Set the issue ID."
  :class 'transient-option
  :description (lambda ()
                 (concat "Issue ID"
                         (beads-label--format-value
                          beads-label--issue-id)))
  :key "i"
  :argument "issue="
  :prompt "Issue ID: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((id (completing-read
                       "Issue ID: "
                       (beads-label--get-available-issues)
                       nil nil beads-label--issue-id)))
              (setq beads-label--issue-id id)
              id)))

(transient-define-infix beads-label--infix-label ()
  "Set the label."
  :class 'transient-option
  :description (lambda ()
                 (concat "Label"
                         (beads-label--format-value
                          beads-label--label)))
  :key "l"
  :argument "label="
  :prompt "Label: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((label (completing-read
                          "Label: "
                          (beads-label--get-all-labels)
                          nil nil beads-label--label)))
              (setq beads-label--label label)
              label)))

;;; Suffix Commands - Add

(defun beads-label--execute-add (issue-id label)
  "Execute bd label add ISSUE-ID LABEL."
  (condition-case err
      (progn
        (beads--run-command "label" "add" issue-id label)
        (message "Added label '%s' to %s" label issue-id)
        ;; Invalidate completion cache
        (beads--invalidate-completion-cache)
        ;; Refresh buffers if auto-refresh is enabled
        (when beads-auto-refresh
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (cond
               ((and (fboundp 'beads-list-refresh)
                     (derived-mode-p 'beads-list-mode))
                (beads-list-refresh))
               ((and (fboundp 'beads-refresh-show)
                     (derived-mode-p 'beads-show-mode)
                     (boundp 'beads-show--issue-id)
                     (string= beads-show--issue-id issue-id))
                (beads-refresh-show))))))
        nil)
    (error
     (beads--error "Failed to add label: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-label--add-command ()
  "Execute bd label add command."
  :key "a"
  :description "Add label to issue"
  (interactive)
  (let ((errors (beads-label--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (beads-label--execute-add beads-label--issue-id
                                beads-label--label)
      (beads-label--reset-state))))

;;; Suffix Commands - Remove

(defun beads-label--execute-remove (issue-id label)
  "Execute bd label remove ISSUE-ID LABEL."
  (condition-case err
      (progn
        (beads--run-command "label" "remove" issue-id label)
        (message "Removed label '%s' from %s" label issue-id)
        ;; Invalidate completion cache
        (beads--invalidate-completion-cache)
        ;; Refresh buffers if auto-refresh is enabled
        (when beads-auto-refresh
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (cond
               ((and (fboundp 'beads-list-refresh)
                     (derived-mode-p 'beads-list-mode))
                (beads-list-refresh))
               ((and (fboundp 'beads-refresh-show)
                     (derived-mode-p 'beads-show-mode)
                     (boundp 'beads-show--issue-id)
                     (string= beads-show--issue-id issue-id))
                (beads-refresh-show))))))
        nil)
    (error
     (beads--error "Failed to remove label: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-label--remove-command ()
  "Execute bd label remove command."
  :key "r"
  :description "Remove label from issue"
  (interactive)
  (let ((errors (beads-label--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (when (y-or-n-p (format "Remove label '%s' from %s? "
                              beads-label--label
                              beads-label--issue-id))
        (beads-label--execute-remove beads-label--issue-id
                                     beads-label--label)
        (beads-label--reset-state)))))

;;; Suffix Commands - List

(defun beads-label--execute-list (issue-id)
  "Execute bd label list for ISSUE-ID and display in buffer."
  (condition-case err
      (let* ((labels (beads-label--get-issue-labels issue-id))
             (buf (get-buffer-create
                   (format "*beads-labels: %s*" issue-id))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Labels for %s\n" issue-id))
            (insert "==============================\n\n")
            (if (null labels)
                (insert "No labels found.\n")
              (dolist (label labels)
                (insert (format "- %s\n" label))))
            (goto-char (point-min))
            (special-mode)
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "g")
                           (lambda ()
                             (interactive)
                             (beads-label--execute-list issue-id)))))
        (display-buffer buf)
        (message "Labels for %s" issue-id)
        nil)
    (error
     (beads--error "Failed to list labels: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-label--list-command ()
  "List labels for an issue."
  :key "l"
  :description "List labels for issue"
  (interactive)
  (let* ((issue-id (or beads-label--issue-id
                       (beads-label--detect-issue-id)
                       (completing-read
                        "List labels for issue: "
                        (beads-label--get-available-issues)
                        nil t))))
    (when (string-empty-p (string-trim issue-id))
      (user-error "No issue ID specified"))
    (beads-label--execute-list issue-id)))

;;; Suffix Commands - List All

(defun beads-label--execute-list-all ()
  "Execute bd label list-all and display in buffer."
  (condition-case err
      (let* ((labels (beads-label--get-all-labels))
             (buf (get-buffer-create "*beads-labels-all*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "All Labels\n")
            (insert "==============================\n\n")
            (if (null labels)
                (insert "No labels found.\n")
              (dolist (label labels)
                (insert (format "- %s\n" label))))
            (goto-char (point-min))
            (special-mode)
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "g")
                           (lambda ()
                             (interactive)
                             (beads-label--execute-list-all)))))
        (display-buffer buf)
        (message "All labels")
        nil)
    (error
     (beads--error "Failed to list all labels: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-label--list-all-command ()
  "List all labels in database."
  :key "L"
  :description "List all labels"
  (interactive)
  (beads-label--execute-list-all))

;;; Suffix Commands - Reset

(transient-define-suffix beads-label--reset ()
  "Reset all label parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-label--reset-state)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-label "beads-label" nil t)
(transient-define-prefix beads-label ()
  "Manage issue labels in Beads.

This transient menu provides an interface for managing labels
on issues. It supports adding, removing, and listing labels."
  :value (lambda () nil)
  (interactive)
  (beads-check-executable)
  ;; Try to set issue-id from context
  (unless beads-label--issue-id
    (setq beads-label--issue-id (beads-label--detect-issue-id)))
  ["Label Parameters"
   (beads-label--infix-issue)
   (beads-label--infix-label)]
  ["Actions"
   ["Modify"
    ("a" "Add label" beads-label--add-command)
    ("r" "Remove label" beads-label--remove-command)]
   ["View"
    ("l" "List issue labels" beads-label--list-command)
    ("L" "List all labels" beads-label--list-all-command)]
   ["Other"
    ("R" "Reset fields" beads-label--reset)
    ("q" "Quit" transient-quit-one)]])

;;; Footer

(provide 'beads-label)
;;; beads-label.el ends here
