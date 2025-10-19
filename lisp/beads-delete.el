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
(require 'beads-list)
(require 'beads-show)
(require 'transient)

;;; State Variables

(defvar beads-delete--issue-id nil
  "Issue ID to delete.")

(defvar beads-delete--force nil
  "Whether to force deletion without preview.")

;;; State Management

(defun beads-delete--reset-state ()
  "Reset all delete state variables."
  (setq beads-delete--issue-id nil
        beads-delete--force nil))

;;; Utility Functions

(defun beads-delete--detect-issue-id ()
  "Detect issue ID from current context.
Returns issue ID string or nil if not found."
  (or
   ;; From beads-list buffer
   (when (derived-mode-p 'beads-list-mode)
     (beads-list--current-issue-id))
   ;; From beads-show buffer
   (when (derived-mode-p 'beads-show-mode)
     beads-show--issue-id)
   ;; From buffer name (*beads-show: bd-N*)
   (when (string-match "\\*beads-show: \\([^*]+\\)\\*"
                      (buffer-name))
     (match-string 1 (buffer-name)))))

(defun beads-delete--format-current-value (value)
  "Format VALUE for display in transient menu.
Returns a propertized string showing the current value."
  (if (and value (not (string-empty-p (string-trim value))))
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [not set]" 'face 'transient-inactive-value)))

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
  (let ((args (list beads-delete--issue-id)))
    (when beads-delete--force
      (push "--force" args))
    args))

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
    ;; Close show buffer if viewing the deleted issue
    (let ((show-buffer (format "*beads-show %s*" beads-delete--issue-id)))
      (when (get-buffer show-buffer)
        (kill-buffer show-buffer)))
    ;; Refresh any open beads buffers
    (when beads-auto-refresh
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'beads-list-mode)
            (beads-list-refresh)))))
    result))

;;; Transient Infixes

(transient-define-infix beads-delete--infix-issue-id ()
  "Read issue ID to delete."
  :class 'transient-option
  :description (lambda ()
                 (concat "Issue ID (required)"
                         (beads-delete--format-current-value
                          beads-delete--issue-id)))
  :key "i"
  :argument "id="
  :prompt "Issue ID to delete: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((id (completing-read
                      "Issue ID to delete: "
                      (beads--issue-completion-table)
                      nil t beads-delete--issue-id
                      'beads--issue-id-history)))
              (setq beads-delete--issue-id id)
              id)))

(transient-define-infix beads-delete--infix-force ()
  "Toggle force deletion flag."
  :class 'transient-switch
  :description "Force deletion (--force)"
  :key "-f"
  :argument "--force"
  :init-value (lambda (obj)
                (when beads-delete--force
                  (oset obj value t)))
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-delete--force
                  (not beads-delete--force))
            beads-delete--force))

;;; Transient Suffixes

(transient-define-suffix beads-delete--execute ()
  "Execute bd delete command."
  :description "Delete issue"
  :key "x"
  (interactive)
  (when-let ((error (beads-delete--validate-all)))
    (user-error "%s" error))
  (when beads-delete--force
    (beads-delete--execute-deletion))
  (beads-delete--reset-state))

(transient-define-suffix beads-delete--reset ()
  "Reset delete state."
  :description "Reset fields"
  :key "R"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-delete--reset-state)
    (message "All fields reset")))

;;; Main Transient

;;;###autoload
(defun beads-delete (&optional issue-id)
  "Delete an issue with bd delete.

This is a DESTRUCTIVE operation that:
- Removes all dependency links
- Updates text references to '[deleted:ID]'
- Deletes the issue from the database

If ISSUE-ID is not provided, prompts for completion.
If called from beads-list or beads-show buffer, detects issue from context."
  (interactive
   (list (or (beads-delete--detect-issue-id)
             (completing-read "Delete issue: "
                              (beads--issue-completion-table)
                              nil t nil 'beads--issue-id-history))))
  ;; Set the issue ID
  (when issue-id
    (setq beads-delete--issue-id issue-id))
  ;; Check executable
  (beads-check-executable)
  ;; Show the transient
  (beads-delete--menu))

;;;###autoload (autoload 'beads-delete--menu "beads-delete" nil t)
(transient-define-prefix beads-delete--menu ()
  "Transient menu for deleting issues."
  :value (lambda () nil)
  ["Delete Issue"
   (beads-delete--infix-issue-id)
   (beads-delete--infix-force)]
  ["Actions"
   ("x" "Delete issue" beads-delete--execute)
   ("R" "Reset fields" beads-delete--reset)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-delete)
;;; beads-delete.el ends here
