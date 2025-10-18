;;; beads-stats.el --- Display issue statistics -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides a simple interface for viewing issue statistics in Beads.
;; Displays statistics such as total issues, issues by status, issues
;; by priority, and average lead time.
;;
;; Usage:
;;   M-x beads-stats RET
;;
;; The command opens a buffer displaying:
;; - Total issues
;; - Issues by status (open, in_progress, blocked, closed)
;; - Ready issues (no blockers)
;; - Average lead time

;;; Code:

(require 'beads)

;;; Variables

(defvar beads-stats-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'beads-stats-refresh)
    map)
  "Keymap for `beads-stats-mode'.")

;;; Mode Definition

(define-derived-mode beads-stats-mode special-mode "Beads-Stats"
  "Major mode for displaying Beads issue statistics.

\\{beads-stats-mode-map}"
  :group 'beads
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;; Statistics Display

(defun beads-stats--parse-stats (json)
  "Parse statistics from JSON object.
Returns an alist with statistics fields."
  `((total . ,(alist-get 'total_issues json))
    (open . ,(alist-get 'open_issues json))
    (in-progress . ,(alist-get 'in_progress_issues json))
    (closed . ,(alist-get 'closed_issues json))
    (blocked . ,(alist-get 'blocked_issues json))
    (ready . ,(alist-get 'ready_issues json))
    (lead-time . ,(alist-get 'average_lead_time_hours json))))

(defun beads-stats--format-lead-time (hours)
  "Format HOURS as human-readable lead time."
  (cond
   ((zerop hours) "N/A")
   ((< hours 1) (format "%.1f minutes" (* hours 60)))
   ((< hours 24) (format "%.1f hours" hours))
   (t (format "%.1f days" (/ hours 24.0)))))

(defun beads-stats--render (stats)
  "Render STATS into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; Header
    (insert (propertize "Beads Issue Statistics"
                       'face 'bold)
            "\n")
    (insert (propertize (make-string 50 ?═) 'face 'shadow) "\n\n")

    ;; Total
    (insert (propertize "Total Issues: " 'face 'bold)
            (propertize (format "%d" (alist-get 'total stats))
                       'face 'font-lock-constant-face)
            "\n\n")

    ;; By Status
    (insert (propertize "By Status:" 'face 'bold) "\n")
    (insert (propertize (make-string 50 ?─) 'face 'shadow) "\n")
    (insert (format "  %-15s %s\n"
                   (propertize "Open:" 'face 'default)
                   (propertize (format "%d" (alist-get 'open stats))
                              'face 'success)))
    (insert (format "  %-15s %s\n"
                   (propertize "In Progress:" 'face 'default)
                   (propertize (format "%d" (alist-get 'in-progress stats))
                              'face 'warning)))
    (insert (format "  %-15s %s\n"
                   (propertize "Blocked:" 'face 'default)
                   (propertize (format "%d" (alist-get 'blocked stats))
                              'face 'error)))
    (insert (format "  %-15s %s\n"
                   (propertize "Closed:" 'face 'default)
                   (propertize (format "%d" (alist-get 'closed stats))
                              'face 'shadow)))
    (insert "\n")

    ;; Ready Issues
    (insert (propertize "Ready to Work:" 'face 'bold) "\n")
    (insert (propertize (make-string 50 ?─) 'face 'shadow) "\n")
    (insert (format "  %s issues with no blockers\n"
                   (propertize (format "%d" (alist-get 'ready stats))
                              'face 'success)))
    (insert "\n")

    ;; Lead Time
    (insert (propertize "Average Lead Time:" 'face 'bold) "\n")
    (insert (propertize (make-string 50 ?─) 'face 'shadow) "\n")
    (insert (format "  %s\n"
                   (beads-stats--format-lead-time
                    (alist-get 'lead-time stats))))
    (insert "\n\n")

    ;; Footer
    (insert (propertize "Commands:" 'face 'bold) "\n")
    (insert "  g - refresh\n")
    (insert "  q - quit\n"))

  (goto-char (point-min)))

(defun beads-stats--fetch-and-display ()
  "Fetch statistics and display them in the current buffer."
  (condition-case err
      (let* ((json (beads--run-command "stats"))
             (stats (beads-stats--parse-stats json)))
        (beads-stats--render stats))
    (error
     (let ((inhibit-read-only t))
       (erase-buffer)
       (insert (propertize "Error fetching statistics\n" 'face 'error)
               "\n"
               (error-message-string err))))))

;;; Interactive Commands

;;;###autoload
(defun beads-stats ()
  "Display Beads issue statistics in a buffer."
  (interactive)
  (beads-check-executable)
  (let ((buffer (get-buffer-create "*beads-stats*")))
    (with-current-buffer buffer
      (beads-stats-mode)
      (beads-stats--fetch-and-display))
    (pop-to-buffer buffer)))

(defun beads-stats-refresh ()
  "Refresh the statistics display."
  (interactive)
  (when (derived-mode-p 'beads-stats-mode)
    (message "Refreshing statistics...")
    (beads-stats--fetch-and-display)
    (message "Statistics refreshed")))

(provide 'beads-stats)
;;; beads-stats.el ends here
