;;; beads-stats.el --- Display issue statistics -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides an interactive interface for viewing issue statistics in Beads.
;; Displays statistics such as total issues, issues by status, issues
;; by priority, and average lead time.
;;
;; Usage:
;;   M-x beads-stats RET
;;
;; The command opens a buffer displaying:
;; - Total issues (clickable)
;; - Issues by status (open, in_progress, blocked, closed) - all clickable
;; - Ready issues (no blockers) - clickable
;; - Average lead time
;;
;; Interactive Features:
;; - Click on any statistic number to view those issues in beads-list
;; - Hover over numbers to see tooltip with action description
;; - Use keyboard shortcuts: g to refresh, q to quit

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-types)
(require 'button)

;;; Forward Declarations

(declare-function beads-list "beads-list")
(declare-function beads-ready "beads-list")
(declare-function beads-blocked "beads-list")
(declare-function beads-list-mode "beads-list")
(declare-function beads-list--populate-buffer "beads-list")
(declare-function beads-list--format-filter-string "beads-list")

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

;;; Button Infrastructure

(define-button-type 'beads-stats-button
  'follow-link t
  'action #'beads-stats--button-action
  'mouse-face 'highlight
  :supertype 'button)

(defun beads-stats--button-action (button)
  "Handle click on BUTTON in stats buffer.
Opens filtered issue list based on button's filter-type property."
  (let ((filter-type (button-get button 'filter-type))
        (count (button-get button 'count)))
    (beads-stats--open-filtered-list filter-type count)))

(defun beads-stats--open-filtered-list (filter-type _count)
  "Open filtered issue list for FILTER-TYPE.
FILTER-TYPE can be: total, open, in-progress, closed, blocked, or ready."
  (pcase filter-type
    ('total
     ;; Show all issues
     (beads-list))
    ('ready
     ;; Show ready issues (existing command)
     (beads-ready))
    ('blocked
     ;; Show blocked issues (existing command)
     (beads-blocked))
    ((or 'open 'in-progress 'closed)
     ;; Show issues filtered by status
     (beads-stats--list-by-status filter-type))
    (_
     (message "Unknown filter type: %s" filter-type))))

(defun beads-stats--list-by-status (status)
  "Display issues filtered by STATUS.
STATUS should be one of: open, in-progress, or closed."
  (beads-check-executable)
  (let* ((status-str (pcase status
                       ('open "open")
                       ('in-progress "in_progress")
                       ('closed "closed")
                       (_ (error "Invalid status: %s" status))))
         (buffer-name (format "*beads-list: %s*" status-str))
         (cmd (beads-command-list :status status-str))
         (issues (beads-command-execute cmd))
         (buffer (get-buffer-create buffer-name))
         (project-dir default-directory))
    (with-current-buffer buffer
      (beads-list-mode)
      ;; Preserve project context in list buffer
      (setq default-directory project-dir)
      (if (not issues)
          (progn
            (setq tabulated-list-entries nil)
            (tabulated-list-print t)
            (setq mode-line-format
                  `("%e" mode-line-front-space
                    mode-line-buffer-identification
                    ,(format "  No %s issues" status-str)))
            (message "No %s issues found" status-str))
        (beads-list--populate-buffer issues 'list)
        (setq mode-line-format
              `("%e" mode-line-front-space
                mode-line-buffer-identification
                (:eval (format ,(format "  %%d %s issue%%s%%s%%s" status-str)
                             (length tabulated-list-entries)
                             (if (= (length tabulated-list-entries) 1) "" "s")
                             (if beads-list--marked-issues
                                 (format " [%d marked]"
                                        (length beads-list--marked-issues))
                               "")
                             (beads-list--format-filter-string)))))))
    (pop-to-buffer buffer)))

(defun beads-stats--make-stat-button (text filter-type count face)
  "Create a clickable stat button with TEXT, FILTER-TYPE, COUNT, and FACE.
Returns a propertized string that can be inserted into the buffer."
  (let ((help-text (pcase filter-type
                     ('total (format "Click to view all %d issues" count))
                     ('open (format "Click to view %d open issues" count))
                     ('in-progress
                      (format "Click to view %d in-progress issues" count))
                     ('closed (format "Click to view %d closed issues" count))
                     ('blocked
                      (format "Click to view %d blocked issues" count))
                     ('ready (format "Click to view %d ready issues" count))
                     (_ (format "Click to view %d issues" count)))))
    (propertize text
                'face face
                'mouse-face 'highlight
                'help-echo help-text
                'button t
                'category 'beads-stats-button
                'filter-type filter-type
                'count count
                'follow-link t)))

;;; Statistics Display

(defun beads-stats--parse-stats (json)
  "Parse statistics from JSON object.
Returns a beads-statistics object.

This function now uses `beads-statistics-from-json' from beads-types.el
to create proper beads-statistics objects instead of ad-hoc alists.

Note: Ensures average_lead_time_hours is converted to float to satisfy
the beads-statistics class type constraint."
  ;; Ensure average_lead_time_hours is a float (not an integer)
  ;; bd may return 0 as an integer, but the class requires a float
  (let ((lead-time (alist-get 'average_lead_time_hours json)))
    (when (and lead-time (integerp lead-time))
      (setf (alist-get 'average_lead_time_hours json) (float lead-time))))
  (beads-statistics-from-json json))

(defun beads-stats--format-lead-time (hours)
  "Format HOURS as human-readable lead time."
  (cond
   ((zerop hours) "N/A")
   ((< hours 1) (format "%.1f minutes" (* hours 60)))
   ((< hours 24) (format "%.1f hours" hours))
   (t (format "%.1f days" (/ hours 24.0)))))

(defun beads-stats--render (stats)
  "Render STATS (beads-statistics object) into the current buffer.
Output matches the format of `bd stats' CLI command exactly, but with
interactive clickable numbers."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; Header - matches CLI: "📊 Beads Statistics:"
    (insert "📊 Beads Statistics:\n\n")

    ;; All stats in simple format matching CLI
    ;; Format: "Label:      Number" with right-aligned numbers at column 19
    (insert (format "%-18s %s\n"
                   "Total Issues:"
                   (beads-stats--make-stat-button
                    (format "%d" (oref stats total-issues))
                    'total
                    (oref stats total-issues)
                    'default)))
    (insert (format "%-18s %s\n"
                   "Open:"
                   (beads-stats--make-stat-button
                    (format "%d" (oref stats open-issues))
                    'open
                    (oref stats open-issues)
                    'default)))
    (insert (format "%-18s %s\n"
                   "In Progress:"
                   (beads-stats--make-stat-button
                    (format "%d" (oref stats in-progress-issues))
                    'in-progress
                    (oref stats in-progress-issues)
                    'default)))
    (insert (format "%-18s %s\n"
                   "Closed:"
                   (beads-stats--make-stat-button
                    (format "%d" (oref stats closed-issues))
                    'closed
                    (oref stats closed-issues)
                    'default)))
    (insert (format "%-18s %s\n"
                   "Blocked:"
                   (beads-stats--make-stat-button
                    (format "%d" (oref stats blocked-issues))
                    'blocked
                    (oref stats blocked-issues)
                    'default)))
    (insert (format "%-18s %s\n"
                   "Ready:"
                   (beads-stats--make-stat-button
                    (format "%d" (oref stats ready-issues))
                    'ready
                    (oref stats ready-issues)
                    'default))))

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
  "Display Beads issue statistics in an interactive buffer.

Shows statistics including:
- Total issues
- Issues by status (Open, In Progress, Blocked, Closed)
- Ready to work issues
- Average lead time

INTERACTIVE FEATURES:
All statistic numbers are clickable! Click on any number to view
those issues in a filtered list:

- Click Total Issues → View all issues
- Click Open → View open issues only
- Click In Progress → View in-progress issues only
- Click Blocked → View blocked issues
- Click Closed → View closed issues
- Click Ready → View ready-to-work issues

KEYBOARD SHORTCUTS:
  g - Refresh statistics
  q - Quit statistics buffer

The statistics buffer is read-only and updates can be triggered
with \\[beads-stats-refresh]."
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
