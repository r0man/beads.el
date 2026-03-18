;;; beads-actions.el --- Context-aware one-key actions -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;;; Commentary:

;; Pattern 1 context-aware action commands for beads.el.
;;
;; These commands detect the issue at point (or from marks in
;; list-mode), prompt minimally, execute immediately, and refresh
;; buffers.  They work in both `beads-list-mode' and
;; `beads-show-mode'.
;;
;; Keybindings:
;;   d   - Close issue(s)
;;   C   - Claim issue(s)
;;   s   - Set status
;;   #   - Set priority
;;   o   - Reopen issue(s) (list-mode only; show-mode uses `o' for follow-reference)

;;; Code:

(require 'beads)
(require 'beads-command-close)
(require 'beads-command-update)
(require 'beads-command-reopen)

(declare-function beads-list-refresh "beads-command-list")
(declare-function beads-list-unmark-all "beads-command-list")
(declare-function beads-refresh-show "beads-command-show")

(defvar beads-list--marked-issues)

;;; Target Detection

(defun beads-actions--target-ids ()
  "Return list of target issue IDs for the current action.
If marks exist in a list buffer, return marked IDs.
Otherwise return the issue at point as a single-element list.
Signal `user-error' if no target found."
  (cond
   ((and (bound-and-true-p beads-list--marked-issues)
         (not (null beads-list--marked-issues)))
    beads-list--marked-issues)
   ((beads-issue-at-point)
    (list (beads-issue-at-point)))
   (t
    (user-error "No issue at point"))))

;;; Buffer Refresh

(defun beads-actions--refresh ()
  "Refresh the current beads buffer after a mutation."
  (cond
   ((derived-mode-p 'beads-list-mode)
    (beads-list-refresh))
   ((eq major-mode 'beads-show-mode)
    (beads-refresh-show))))

(defun beads-actions--after-mutation ()
  "Invalidate cache, clear mark state, and refresh the current buffer."
  (beads--invalidate-completion-cache)
  (when (and (derived-mode-p 'beads-list-mode)
             (bound-and-true-p beads-list--marked-issues)
             beads-list--marked-issues)
    (beads-list-unmark-all))
  (beads-actions--refresh))

;;; Action Commands

;;;###autoload
(defun beads-actions-close ()
  "Close the issue(s) at point or marked issues.
Prompts for a close reason, then executes immediately."
  (interactive)
  (let* ((ids (beads-actions--target-ids))
         (count (length ids))
         (reason (read-string
                  (if (= count 1)
                      (format "Close %s reason: " (car ids))
                    (format "Close %d issue(s) reason: " count)))))
    (dolist (id ids)
      (condition-case err
          (beads-command-close! :issue-ids (list id) :reason reason)
        (error (message "Failed to close %s: %s" id
                        (error-message-string err)))))
    (beads-actions--after-mutation)
    (message "Closed %d issue(s)" count)))

;;;###autoload
(defun beads-actions-claim ()
  "Claim the issue(s) at point or marked issues.
Executes immediately without prompting."
  (interactive)
  (let* ((ids (beads-actions--target-ids))
         (count (length ids)))
    (dolist (id ids)
      (condition-case err
          (beads-command-update! :issue-ids (list id) :claim t)
        (error (message "Failed to claim %s: %s" id
                        (error-message-string err)))))
    (beads-actions--after-mutation)
    (message "Claimed %d issue(s)" count)))

;;;###autoload
(defun beads-actions-reopen ()
  "Reopen the issue(s) at point or marked issues.
Prompts for an optional reason."
  (interactive)
  (let* ((ids (beads-actions--target-ids))
         (count (length ids))
         (reason (read-string
                  (if (= count 1)
                      (format "Reopen %s reason: " (car ids))
                    (format "Reopen %d issue(s) reason: " count)))))
    (dolist (id ids)
      (condition-case err
          (if (string-empty-p (string-trim reason))
              (beads-command-reopen! :issue-ids (list id))
            (beads-command-reopen! :issue-ids (list id) :reason reason))
        (error (message "Failed to reopen %s: %s" id
                        (error-message-string err)))))
    (beads-actions--after-mutation)
    (message "Reopened %d issue(s)" count)))

;;;###autoload
(defun beads-actions-set-status ()
  "Set status for the issue(s) at point or marked issues.
Prompts with `completing-read' for a valid status value."
  (interactive)
  (let* ((ids (beads-actions--target-ids))
         (count (length ids))
         (status (completing-read
                  (if (= count 1)
                      (format "Status for %s: " (car ids))
                    (format "Status for %d issue(s): " count))
                  '("open" "in_progress" "blocked")
                  nil t)))
    (dolist (id ids)
      (condition-case err
          (beads-command-update! :issue-ids (list id) :status status)
        (error (message "Failed to update %s: %s" id
                        (error-message-string err)))))
    (beads-actions--after-mutation)
    (message "Set status to '%s' for %d issue(s)" status count)))

;;;###autoload
(defun beads-actions-set-priority ()
  "Set priority for the issue(s) at point or marked issues.
Prompts with `completing-read' for a priority value (0-4)."
  (interactive)
  (let* ((ids (beads-actions--target-ids))
         (count (length ids))
         (choices '(("0 - Critical" . 0)
                    ("1 - High" . 1)
                    ("2 - Medium" . 2)
                    ("3 - Low" . 3)
                    ("4 - Backlog" . 4)))
         (selection (completing-read
                     (if (= count 1)
                         (format "Priority for %s: " (car ids))
                       (format "Priority for %d issue(s): " count))
                     choices nil t))
         (priority (cdr (assoc selection choices))))
    (dolist (id ids)
      (condition-case err
          (beads-command-update! :issue-ids (list id) :priority priority)
        (error (message "Failed to update %s: %s" id
                        (error-message-string err)))))
    (beads-actions--after-mutation)
    (message "Set priority to %d for %d issue(s)" priority count)))

(provide 'beads-actions)

;;; beads-actions.el ends here
