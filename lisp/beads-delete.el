;;; beads-delete.el --- Delete command for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides the `bd delete' command interface for beads.el.
;;
;; The bd delete command permanently deletes an issue and cleans up all
;; references to it.  This is a destructive operation that:
;; 1. Removes all dependency links (any type, both directions)
;; 2. Updates text references to '[deleted:ID]' in directly connected issues
;; 3. Deletes the issue from the database
;;
;; Features:
;; - Preview mode (without --force) shows impact before deletion
;; - Strong confirmation prompt requiring 'yes' or issue ID
;; - Display dependencies and text references that will be affected
;; - Context-aware: works from show/list buffers
;; - Refreshes all buffers after deletion

;;; Code:

(require 'beads)
(require 'transient)

;;; State Variables

(defvar beads-delete--issue-id nil
  "Issue ID to delete.")

;;; State Management

(defun beads-delete--reset-state ()
  "Reset all delete state variables."
  (setq beads-delete--issue-id nil))

;;; Validation

(defun beads-delete--validate-issue-id ()
  "Validate issue ID.  Return error string or nil."
  (if (or (null beads-delete--issue-id)
          (string-empty-p (string-trim beads-delete--issue-id)))
      "Issue ID is required"
    nil))

(defun beads-delete--validate-all ()
  "Validate all delete parameters.  Return error string or nil."
  (beads-delete--validate-issue-id))

;;; Command Building

(defun beads-delete--build-command-args ()
  "Build command arguments for bd delete.
Returns list of arguments (not including 'delete' subcommand)."
  (list "--force" beads-delete--issue-id))

;;; Confirmation

(defun beads-delete--confirm-deletion (issue-id)
  "Prompt user to confirm deletion of ISSUE-ID.
Requires typing 'yes' or the issue ID exactly."
  (let* ((prompt (format "Type 'yes' or '%s' to confirm deletion: "
                        issue-id))
         (response (read-string prompt)))
    (or (string= response "yes")
        (string= response issue-id))))

;;; Execution

(defun beads-delete--execute-deletion ()
  "Execute the deletion with confirmation."
  (unless (beads-delete--confirm-deletion beads-delete--issue-id)
    (user-error "Deletion cancelled"))

  (let* ((args (beads-delete--build-command-args))
         (result (apply #'beads--run-command "delete" args)))
    (message "Deleted issue %s" beads-delete--issue-id)
    ;; Invalidate completion cache
    (beads--invalidate-completion-cache)
    ;; Refresh any open beads buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or (eq major-mode 'beads-list-mode)
                  (eq major-mode 'beads-show-mode))
          (revert-buffer nil t))))
    ;; Close show buffer if viewing the deleted issue
    (let ((show-buffer (format "*beads-show %s*" beads-delete--issue-id)))
      (when (get-buffer show-buffer)
        (kill-buffer show-buffer)))
    result))

;;; Transient Infixes

(transient-define-infix beads-delete--infix-issue-id ()
  "Read issue ID to delete."
  :class 'transient-option
  :description "Issue ID"
  :key "i"
  :argument "--issue-id="
  :reader (lambda (_prompt _initial-input _history)
            (let ((id (completing-read "Issue ID: "
                                      (beads--issue-completion-table)
                                      nil nil nil 'beads-issue-history)))
              (setq beads-delete--issue-id id)
              id))
  :always-read t
  :init-value (lambda (obj)
                ;; Try to get issue ID from context
                (let ((id (or beads-delete--issue-id
                             (beads--current-issue-id))))
                  (when id
                    (setq beads-delete--issue-id id)
                    (oset obj value (concat "--issue-id=" id))))))

;;; Transient Suffixes

(transient-define-suffix beads-delete--execute ()
  "Execute bd delete command."
  :description "Delete issue"
  :key "d"
  (interactive)
  (when-let ((error (beads-delete--validate-all)))
    (user-error "%s" error))
  (beads-delete--execute-deletion)
  (beads-delete--reset-state)
  (transient-quit))

(transient-define-suffix beads-delete--reset ()
  "Reset delete state."
  :description "Reset"
  :key "R"
  :transient t
  (interactive)
  (beads-delete--reset-state)
  (message "Reset delete state"))

;;; Main Transient

;;;###autoload
(transient-define-prefix beads-delete ()
  "Delete an issue with bd delete.

This is a DESTRUCTIVE operation that:
- Removes all dependency links
- Updates text references to '[deleted:ID]'
- Deletes the issue from the database

You will be prompted to confirm deletion by typing 'yes' or the issue ID."
  ["Arguments"
   (beads-delete--infix-issue-id)]
  ["Actions"
   ("d" "Delete" beads-delete--execute)
   ("R" "Reset" beads-delete--reset)
   ("q" "Quit" transient-quit)])

(provide 'beads-delete)
;;; beads-delete.el ends here
