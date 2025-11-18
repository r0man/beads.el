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
;; Workflow:
;; 1. User calls `beads-delete', selects issue
;; 2. Preview is shown (bd delete without --force)
;; 3. User confirms yes/no
;; 4. If yes: execute deletion (bd delete --force)
;; 5. If no: cancel operation
;;
;; Features:
;; - Interactive preview before deletion
;; - Context-aware: works from show/list buffers
;; - Refreshes all buffers after deletion

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-list)
(require 'beads-show)

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

;;; Preview Functions

(defun beads-delete--get-preview (issue-id)
  "Get deletion preview for ISSUE-ID.
Runs bd delete without --force to show what will be affected.
Returns preview output as string."
  (let ((db (beads--get-database-path))
        (args nil))
    ;; Build arguments list: delete <issue-id> [--db PATH] [--actor ACTOR]
    (push "delete" args)
    (push issue-id args)
    (when db
      (push "--db" args)
      (push (file-local-name db) args))
    (when beads-actor
      (push "--actor" args)
      (push beads-actor args))
    ;; Reverse to get correct order
    (setq args (nreverse args))
    (with-temp-buffer
      (let ((exit-code (apply #'process-file
                              beads-executable nil t nil args)))
        (if (zerop exit-code)
            (buffer-string)
          (beads--error "Failed to get preview (exit %d): %s"
                        exit-code (buffer-string)))))))

(defun beads-delete--show-preview (issue-id preview-text)
  "Show deletion preview for ISSUE-ID in a buffer.
PREVIEW-TEXT is the output from bd delete without --force.
Returns the preview buffer."
  (let ((buffer (get-buffer-create
                 (format "*beads-delete-preview: %s*" issue-id))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert preview-text)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (special-mode))
    buffer))

;;; Execution

(defun beads-delete--execute-deletion (issue-id)
  "Execute the deletion of ISSUE-ID with --force flag."
  (let ((result (beads-command-delete! :issue-id issue-id :force t)))
    (message "Deleted issue %s" issue-id)
    ;; Invalidate completion cache
    (beads--invalidate-completion-cache)
    ;; Close show buffer if viewing the deleted issue
    (let ((show-buffer (format "*beads-show %s*" issue-id)))
      (when (get-buffer show-buffer)
        (kill-buffer show-buffer)))
    ;; Close preview buffer
    (let ((preview-buffer (format "*beads-delete-preview: %s*" issue-id)))
      (when (get-buffer preview-buffer)
        (kill-buffer preview-buffer)))
    ;; Refresh any open beads buffers
    (when beads-auto-refresh
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'beads-list-mode)
            (beads-list-refresh)))))
    result))

;;; Main Command

;;;###autoload
(defun beads-delete (&optional issue-id)
  "Delete an issue with preview and confirmation.

This is a DESTRUCTIVE operation that:
- Removes all dependency links
- Updates text references to '[deleted:ID]'
- Deletes the issue from the database

Workflow:
1. Prompts for issue ID (or uses ISSUE-ID if provided)
2. Shows preview of what will be deleted (bd delete without --force)
3. Asks for confirmation (yes/no)
4. If yes: executes deletion (bd delete --force)
5. If no: cancels operation

If called from beads-list or beads-show buffer, detects issue
from context."
  (interactive
   (list (or (beads-delete--detect-issue-id)
             (completing-read "Delete issue: "
                              (beads--issue-completion-table)
                              nil t nil 'beads--issue-id-history))))
  ;; Check executable
  (beads-check-executable)

  ;; Get issue ID if not provided
  (unless issue-id
    (setq issue-id (completing-read "Delete issue: "
                                    (beads--issue-completion-table)
                                    nil t nil
                                    'beads--issue-id-history)))

  ;; Get preview
  (message "Getting deletion preview for %s..." issue-id)
  (condition-case err
      (let* ((preview-text (beads-delete--get-preview issue-id))
             (preview-buffer (beads-delete--show-preview
                              issue-id preview-text)))
        ;; Show preview buffer
        (pop-to-buffer preview-buffer)

        ;; Ask for confirmation and close preview buffer
        (unwind-protect
            (when (yes-or-no-p
                   (format "Delete issue %s? " issue-id))
              ;; Execute deletion
              (beads-delete--execute-deletion issue-id))
          ;; Always close preview buffer after answering
          (when (buffer-live-p preview-buffer)
            (kill-buffer preview-buffer))))
    (error
     (message "Failed to get deletion preview: %s"
              (error-message-string err)))))

(provide 'beads-delete)
;;; beads-delete.el ends here
