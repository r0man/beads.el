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
    (define-key map (kbd "n") #'beads-stats-next)
    (define-key map (kbd "p") #'beads-stats-previous)
    map)
  "Keymap for `beads-stats-mode'.")

;;; Mode Definition

(define-derived-mode beads-stats-mode special-mode "Beads-Stats"
  "Major mode for displaying Beads issue statistics.

\\{beads-stats-mode-map}"
  :group 'beads
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;; Navigation Commands

(defun beads-stats-next ()
  "Move to the next clickable statistic button."
  (interactive)
  (let ((start-pos (point)))
    (forward-button 1 t t)
    ;; If we didn't move, we're at the last button - wrap to first
    (when (= (point) start-pos)
      (goto-char (point-min))
      (forward-button 1 t t))))

(defun beads-stats-previous ()
  "Move to the previous clickable statistic button."
  (interactive)
  (let ((start-pos (point)))
    (backward-button 1 t t)
    ;; If we didn't move, we're at the first button - wrap to last
    (when (= (point) start-pos)
      (goto-char (point-max))
      (backward-button 1 t t))))

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
FILTER-TYPE can be: total, open, in-progress, closed, blocked, deferred,
or ready."
  (pcase filter-type
    ('total
     ;; Show all issues directly without transient menu
     (beads-stats--list-all-issues))
    ('ready
     ;; Show ready issues (existing command)
     (beads-ready))
    ('blocked
     ;; Show blocked issues (existing command)
     (beads-blocked))
    ((or 'open 'in-progress 'closed 'deferred)
     ;; Show issues filtered by status
     (beads-stats--list-by-status filter-type))
    (_
     (message "Unknown filter type: %s" filter-type))))

(defun beads-stats--list-all-issues ()
  "Display all issues without showing the transient menu.
Opens a beads-list buffer with all issues, bypassing the transient menu."
  (beads-check-executable)
  (let* ((cmd (beads-command-list))
         (issues (beads-command-execute cmd))
         (buffer (get-buffer-create "*beads-list*")))
    (with-current-buffer buffer
      (beads-list-mode)
      (if (not issues)
          (progn
            (setq tabulated-list-entries nil)
            (tabulated-list-print t)
            (setq mode-line-format
                  '("%e" mode-line-front-space
                    mode-line-buffer-identification
                    "  No issues"))
            (message "No issues found"))
        (beads-list--populate-buffer issues 'list cmd)
        (setq mode-line-format
              '("%e" mode-line-front-space
                mode-line-buffer-identification
                (:eval (format "  %d issue%s%s%s"
                             (length tabulated-list-entries)
                             (if (= (length tabulated-list-entries) 1) "" "s")
                             (if beads-list--marked-issues
                                 (format " [%d marked]"
                                        (length beads-list--marked-issues))
                               "")
                             (beads-list--format-filter-string)))))))
    (pop-to-buffer buffer)))

(defun beads-stats--list-by-status (status)
  "Display issues filtered by STATUS.
STATUS should be one of: open, in-progress, closed, or deferred."
  (beads-check-executable)
  (let* ((status-str (pcase status
                       ('open "open")
                       ('in-progress "in_progress")
                       ('closed "closed")
                       ('deferred "deferred")
                       (_ (error "Invalid status: %s" status))))
         (buffer-name (format "*beads-list: %s*" status-str))
         (cmd (beads-command-list :status status-str))
         (issues (beads-command-execute cmd))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (beads-list-mode)
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

(defun beads-stats--insert-stat-button (text filter-type count face)
  "Insert a clickable stat button with TEXT, FILTER-TYPE, COUNT, and FACE.
Inserts an actual button into the buffer at point."
  (let ((help-text (pcase filter-type
                     ('total (format "Click to view all %d issues" count))
                     ('open (format "Click to view %d open issues" count))
                     ('in-progress
                      (format "Click to view %d in-progress issues" count))
                     ('closed (format "Click to view %d closed issues" count))
                     ('blocked
                      (format "Click to view %d blocked issues" count))
                     ('ready (format "Click to view %d ready issues" count))
                     (_ (format "Click to view %d issues" count))))
        (start (point)))
    (insert text)
    (make-button start (point)
                 'type 'beads-stats-button
                 'face face
                 'help-echo help-text
                 'filter-type filter-type
                 'count count)))

;;; Statistics Display

(defun beads-stats--parse-stats (json)
  "Parse statistics from JSON object.
Returns a beads-stats-data object containing summary and recent-activity.

This function uses `beads-stats-data-from-json' from beads-types.el
to create proper beads-stats-data objects with nested structure."
  (beads-stats-data-from-json json))

(defun beads-stats--format-lead-time (hours)
  "Format HOURS as human-readable lead time."
  (cond
   ((zerop hours) "N/A")
   ((< hours 1) (format "%.1f minutes" (* hours 60)))
   ((< hours 24) (format "%.1f hours" hours))
   (t (format "%.1f days" (/ hours 24.0)))))

(defun beads-stats--render (stats-data)
  "Render STATS-DATA (beads-stats-data object) into the current buffer.
Output matches the format of `bd stats' CLI command exactly, but with
interactive clickable numbers."
  (let ((inhibit-read-only t)
        (stats (oref stats-data summary))
        (activity (oref stats-data recent-activity)))
    (erase-buffer)

    ;; Blank line at top for padding
    (insert "\n")

    ;; Header - matches CLI: " 📊 Issue Database Status"
    (insert " 📊 Issue Database Status\n\n")

    ;; Summary section
    (insert " Summary:\n")

    ;; Format: "  Label:              Number" with right-aligned numbers
    ;; CLI uses column 24 for right-aligned numbers (2 spaces + 22 chars label)
    (insert "   Total Issues:")
    (insert (make-string (- 24 (current-column)) ?\s))
    (beads-stats--insert-stat-button
     (format "%d" (oref stats total-issues))
     'total
     (oref stats total-issues)
     'default)
    (insert "\n")

    (insert "   Open:")
    (insert (make-string (- 24 (current-column)) ?\s))
    (beads-stats--insert-stat-button
     (format "%d" (oref stats open-issues))
     'open
     (oref stats open-issues)
     'default)
    (insert "\n")

    (insert "   In Progress:")
    (insert (make-string (- 24 (current-column)) ?\s))
    (beads-stats--insert-stat-button
     (format "%d" (oref stats in-progress-issues))
     'in-progress
     (oref stats in-progress-issues)
     'default)
    (insert "\n")

    (insert "   Blocked:")
    (insert (make-string (- 24 (current-column)) ?\s))
    (beads-stats--insert-stat-button
     (format "%d" (oref stats blocked-issues))
     'blocked
     (oref stats blocked-issues)
     'default)
    (insert "\n")

    (insert "   Closed:")
    (insert (make-string (- 24 (current-column)) ?\s))
    (beads-stats--insert-stat-button
     (format "%d" (oref stats closed-issues))
     'closed
     (oref stats closed-issues)
     'default)
    (insert "\n")

    (insert "   Ready to Work:")
    (insert (make-string (- 24 (current-column)) ?\s))
    (beads-stats--insert-stat-button
     (format "%d" (oref stats ready-issues))
     'ready
     (oref stats ready-issues)
     'default)
    (insert "\n")

    ;; Extended section - only show if tombstones > 0
    (when (> (oref stats tombstone-issues) 0)
      (insert "\n Extended:\n")
      (insert "   Deleted:")
      (insert (make-string (- 24 (current-column)) ?\s))
      (insert (format "%d (tombstones)\n" (oref stats tombstone-issues))))

    ;; Recent Activity section - only show if we have activity data
    (when activity
      (insert "\n Recent Activity (last ")
      (insert (format "%d" (oref activity hours-tracked)))
      (insert " hours):\n")

      (insert "   Commits:")
      (insert (make-string (- 24 (current-column)) ?\s))
      (insert (format "%d\n" (oref activity commit-count)))

      (insert "   Total Changes:")
      (insert (make-string (- 24 (current-column)) ?\s))
      (insert (format "%d\n" (oref activity total-changes)))

      (insert "   Issues Created:")
      (insert (make-string (- 24 (current-column)) ?\s))
      (insert (format "%d\n" (oref activity issues-created)))

      (insert "   Issues Closed:")
      (insert (make-string (- 24 (current-column)) ?\s))
      (insert (format "%d\n" (oref activity issues-closed)))

      (insert "   Issues Reopened:")
      (insert (make-string (- 24 (current-column)) ?\s))
      (insert (format "%d\n" (oref activity issues-reopened)))

      (insert "   Issues Updated:")
      (insert (make-string (- 24 (current-column)) ?\s))
      (insert (format "%d\n" (oref activity issues-updated))))

    ;; Footer
    (insert "\n For more details, use 'bd list' to see individual issues.\n"))

  ;; Position point on first button (Total Issues)
  (goto-char (point-min))
  (forward-button 1 t t))

(defun beads-stats--fetch-and-display ()
  "Fetch statistics and display them in the current buffer."
  (condition-case err
      (let* ((json (beads-command-stats!))
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
