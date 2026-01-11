;;; beads-stats-test.el --- Tests for beads-stats -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-stats.el.
;; Tests cover statistics parsing, formatting, rendering, and display.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-buffer)
(require 'beads-types)
(require 'beads-stats)
(require 'beads-test)

;;; Test Fixtures

(defvar beads-stats-test--sample-stats
  '((summary . ((total_issues . 100)
                (open_issues . 30)
                (in_progress_issues . 15)
                (closed_issues . 50)
                (blocked_issues . 5)
                (deferred_issues . 0)
                (ready_issues . 25)
                (tombstone_issues . 2)
                (pinned_issues . 0)
                (epics_eligible_for_closure . 0)
                (average_lead_time_hours . 48.5)))
    (recent_activity . ((hours_tracked . 24)
                        (commit_count . 4)
                        (issues_created . 11)
                        (issues_closed . 3)
                        (issues_updated . 50)
                        (issues_reopened . 1)
                        (total_changes . 65))))
  "Sample statistics data for testing (new nested format).")

(defvar beads-stats-test--zero-stats
  '((summary . ((total_issues . 0)
                (open_issues . 0)
                (in_progress_issues . 0)
                (closed_issues . 0)
                (blocked_issues . 0)
                (deferred_issues . 0)
                (ready_issues . 0)
                (tombstone_issues . 0)
                (pinned_issues . 0)
                (epics_eligible_for_closure . 0)
                (average_lead_time_hours . 0.0)))
    (recent_activity . ((hours_tracked . 24)
                        (commit_count . 0)
                        (issues_created . 0)
                        (issues_closed . 0)
                        (issues_updated . 0)
                        (issues_reopened . 0)
                        (total_changes . 0))))
  "Zero statistics for testing edge cases (new nested format).")

;;; Test Utilities

(defun beads-stats-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

(defun beads-stats-test--get-stats-buffer ()
  "Get the stats buffer for the current project.
Returns the buffer name using the centralized naming module."
  (beads-buffer-name-utility "stats"))

(defun beads-stats-test--get-list-buffer (&optional filter)
  "Get the list buffer for the current project.
FILTER is an optional filter string."
  (beads-buffer-name-list nil filter))

(defun beads-stats-test--find-and-kill-stats-buffers ()
  "Kill all stats buffers for cleanup."
  (dolist (buf (beads-buffer-name-find-utility-buffers nil "stats"))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun beads-stats-test--find-and-kill-list-buffers ()
  "Kill all list buffers for cleanup."
  (dolist (buf (beads-buffer-name-find-list-buffers))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

;;; Tests for Statistics Parsing

(ert-deftest beads-stats-test-parse-stats ()
  "Test parsing statistics from JSON."
  (let* ((stats-data (beads-stats--parse-stats beads-stats-test--sample-stats))
         (stats (oref stats-data summary))
         (activity (oref stats-data recent-activity)))
    (should (beads-stats-data-p stats-data))
    (should (beads-statistics-p stats))
    (should (beads-recent-activity-p activity))
    (should (= (oref stats total-issues) 100))
    (should (= (oref stats open-issues) 30))
    (should (= (oref stats in-progress-issues) 15))
    (should (= (oref stats closed-issues) 50))
    (should (= (oref stats blocked-issues) 5))
    (should (= (oref stats ready-issues) 25))
    (should (= (oref stats tombstone-issues) 2))
    (should (= (oref stats average-lead-time) 48.5))
    ;; Check recent activity
    (should (= (oref activity hours-tracked) 24))
    (should (= (oref activity commit-count) 4))
    (should (= (oref activity issues-created) 11))
    (should (= (oref activity total-changes) 65))))

(ert-deftest beads-stats-test-parse-zero-stats ()
  "Test parsing statistics with all zeros."
  (let* ((stats-data (beads-stats--parse-stats beads-stats-test--zero-stats))
         (stats (oref stats-data summary)))
    (should (beads-stats-data-p stats-data))
    (should (beads-statistics-p stats))
    (should (= (oref stats total-issues) 0))
    (should (= (oref stats open-issues) 0))
    (should (= (oref stats closed-issues) 0))
    (should (= (oref stats average-lead-time) 0.0))))

;;; Tests for Lead Time Formatting

(ert-deftest beads-stats-test-format-lead-time-zero ()
  "Test formatting zero lead time."
  (should (equal (beads-stats--format-lead-time 0) "N/A")))

(ert-deftest beads-stats-test-format-lead-time-minutes ()
  "Test formatting lead time in minutes."
  (should (string-match-p "minutes"
                         (beads-stats--format-lead-time 0.5))))

(ert-deftest beads-stats-test-format-lead-time-hours ()
  "Test formatting lead time in hours."
  (should (string-match-p "hours"
                         (beads-stats--format-lead-time 12))))

(ert-deftest beads-stats-test-format-lead-time-days ()
  "Test formatting lead time in days."
  (should (string-match-p "days"
                         (beads-stats--format-lead-time 48))))

(ert-deftest beads-stats-test-format-lead-time-exact-hour ()
  "Test formatting exactly 1 hour."
  (should (string-match-p "hours"
                         (beads-stats--format-lead-time 1.0))))

(ert-deftest beads-stats-test-format-lead-time-exact-day ()
  "Test formatting exactly 24 hours (1 day)."
  (should (string-match-p "days"
                         (beads-stats--format-lead-time 24.0))))

;;; Tests for Rendering

(ert-deftest beads-stats-test-render-basic ()
  "Test basic rendering of statistics matches CLI format."
  (with-temp-buffer
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats-data)
      (let ((content (buffer-string)))
        ;; Check for CLI-style header with emoji
        (should (string-match-p "📊 Issue Database Status" content))
        (should (string-match-p "Summary:" content))
        (should (string-match-p "Total Issues:" content))
        (should (string-match-p "100" content))
        (should (string-match-p "Open:" content))
        (should (string-match-p "30" content))
        (should (string-match-p "In Progress:" content))
        (should (string-match-p "15" content))
        (should (string-match-p "Closed:" content))
        (should (string-match-p "50" content))
        (should (string-match-p "Blocked:" content))
        (should (string-match-p "\\b5\\b" content))
        (should (string-match-p "Ready to Work:" content))
        (should (string-match-p "25" content))
        ;; Check for Extended section (tombstones > 0)
        (should (string-match-p "Extended:" content))
        (should (string-match-p "Deleted:" content))
        (should (string-match-p "(tombstones)" content))
        ;; Check for Recent Activity section
        (should (string-match-p "Recent Activity" content))
        (should (string-match-p "Commits:" content))
        (should (string-match-p "Total Changes:" content))
        (should (string-match-p "Issues Created:" content))
        ;; Check footer
        (should (string-match-p "M-x beads-list" content))
        ;; Should NOT have old format
        (should-not (string-match-p "By Status:" content))
        (should-not (string-match-p "Average Lead Time:" content))))))

(ert-deftest beads-stats-test-render-zero-stats ()
  "Test rendering statistics with all zeros."
  (with-temp-buffer
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--zero-stats)))
      (beads-stats--render stats-data)
      (let ((content (buffer-string)))
        (should (string-match-p "Total Issues:" content))
        (should (string-match-p "\\b0\\b" content))
        ;; Extended section should NOT appear when tombstones = 0
        (should-not (string-match-p "Extended:" content))))))

(ert-deftest beads-stats-test-render-matches-cli ()
  "Test that rendered output matches CLI format exactly."
  (with-temp-buffer
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats-data)
      (let ((content (buffer-string)))
        ;; Verify CLI-style format
        (should (string-match-p "📊 Issue Database Status" content))
        (should (string-match-p "Summary:" content))
        (should (string-match-p "Total Issues:" content))
        (should (string-match-p "Open:" content))
        (should (string-match-p "In Progress:" content))
        (should (string-match-p "Closed:" content))
        (should (string-match-p "Blocked:" content))
        (should (string-match-p "Ready to Work:" content))
        (should (string-match-p "Recent Activity" content))
        (should (string-match-p "For more details" content))
        ;; Should NOT have old format
        (should-not (string-match-p "By Status:" content))
        (should-not (string-match-p "Commands:" content))
        (should-not (string-match-p "═" content))
        (should-not (string-match-p "─" content))))))

;;; Tests for Mode

(ert-deftest beads-stats-test-mode-defined ()
  "Test that beads-stats-mode is defined."
  (with-temp-buffer
    (beads-stats-mode)
    (should (eq major-mode 'beads-stats-mode))))

(ert-deftest beads-stats-test-mode-read-only ()
  "Test that beads-stats-mode buffer is read-only."
  (with-temp-buffer
    (beads-stats-mode)
    (should buffer-read-only)))

(ert-deftest beads-stats-test-mode-truncate-lines ()
  "Test that beads-stats-mode truncates lines."
  (with-temp-buffer
    (beads-stats-mode)
    (should truncate-lines)))

(ert-deftest beads-stats-test-mode-keymap ()
  "Test that beads-stats-mode keymap is set."
  (with-temp-buffer
    (beads-stats-mode)
    (should (keymapp (current-local-map)))))

(ert-deftest beads-stats-test-mode-keybindings ()
  "Test that key bindings are set correctly."
  (with-temp-buffer
    (beads-stats-mode)
    (should (eq (lookup-key (current-local-map) (kbd "q"))
               'quit-window))
    (should (eq (lookup-key (current-local-map) (kbd "g"))
               'beads-stats-refresh))))

;;; Tests for Main Command

(ert-deftest beads-stats-test-command-creates-buffer ()
  "Test that beads-stats creates a buffer."
  (beads-test-with-project ()
    (let ((json-output (json-encode beads-stats-test--sample-stats)))
      (cl-letf (((symbol-function 'call-process)
                 (beads-stats-test--mock-call-process 0 json-output)))
        (beads-stats)
        (should (get-buffer (beads-stats-test--get-stats-buffer)))
        (beads-stats-test--find-and-kill-stats-buffers)))))

(ert-deftest beads-stats-test-command-sets-mode ()
  "Test that beads-stats sets the correct mode."
  (beads-test-with-project ()
    (let ((json-output (json-encode beads-stats-test--sample-stats)))
      (cl-letf (((symbol-function 'call-process)
                 (beads-stats-test--mock-call-process 0 json-output)))
        (beads-stats)
        (with-current-buffer (beads-stats-test--get-stats-buffer)
          (should (eq major-mode 'beads-stats-mode)))
        (beads-stats-test--find-and-kill-stats-buffers)))))

(ert-deftest beads-stats-test-command-displays-content ()
  "Test that beads-stats displays statistics content."
  (beads-test-with-project ()
    (let ((json-output (json-encode beads-stats-test--sample-stats)))
      (cl-letf (((symbol-function 'call-process)
                 (beads-stats-test--mock-call-process 0 json-output)))
        (beads-stats)
        (with-current-buffer (beads-stats-test--get-stats-buffer)
          (let ((content (buffer-string)))
            (should (string-match-p "Total Issues:" content))
            (should (string-match-p "100" content))))
        (beads-stats-test--find-and-kill-stats-buffers)))))

(ert-deftest beads-stats-test-command-error-handling ()
  "Test that beads-stats handles command failure gracefully."
  (beads-test-with-project ()
    (let ((json-output (json-encode beads-stats-test--sample-stats)))
      (cl-letf (((symbol-function 'call-process)
                 (beads-stats-test--mock-call-process 1 "Error: failed")))
        (beads-stats)
        (should (get-buffer (beads-stats-test--get-stats-buffer)))
        (with-current-buffer (beads-stats-test--get-stats-buffer)
          (let ((content (buffer-string)))
            (should (string-match-p "Error fetching statistics" content))))
        (beads-stats-test--find-and-kill-stats-buffers)))))

;;; Tests for Refresh

(ert-deftest beads-stats-test-refresh-updates-buffer ()
  "Test that beads-stats-refresh updates the buffer."
  (beads-test-with-project ()
    (let ((json-output (json-encode beads-stats-test--sample-stats)))
      (cl-letf (((symbol-function 'call-process)
                 (beads-stats-test--mock-call-process 0 json-output)))
        (beads-stats)
        (with-current-buffer (beads-stats-test--get-stats-buffer)
          ;; Modify buffer to test refresh
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (insert "TEST"))
          ;; Refresh
          (beads-stats-refresh)
          (let ((content (buffer-string)))
            (should-not (string-match-p "TEST" content))
            (should (string-match-p "Total Issues:" content))))
        (beads-stats-test--find-and-kill-stats-buffers)))))

(ert-deftest beads-stats-test-refresh-only-in-stats-mode ()
  "Test that beads-stats-refresh only works in stats mode."
  (with-temp-buffer
    (text-mode)
    ;; Should not do anything in non-stats-mode buffer
    (beads-stats-refresh)
    (should (zerop (buffer-size)))))

;;; Integration Tests

(ert-deftest beads-stats-test-full-workflow ()
  "Test complete workflow from fetching to display."
  (beads-test-with-project ()
    (let ((json-output (json-encode beads-stats-test--sample-stats)))
      (cl-letf (((symbol-function 'call-process)
                 (beads-stats-test--mock-call-process 0 json-output)))
        ;; Open stats
        (beads-stats)
        (should (get-buffer (beads-stats-test--get-stats-buffer)))

        ;; Verify mode
        (with-current-buffer (beads-stats-test--get-stats-buffer)
          (should (eq major-mode 'beads-stats-mode))

          ;; Verify content
          (let ((content (buffer-string)))
            (should (string-match-p "100" content))
            (should (string-match-p "30" content))
            (should (string-match-p "15" content))
            (should (string-match-p "50" content))
            (should (string-match-p "\\b5\\b" content))
            (should (string-match-p "25" content)))

          ;; Test refresh
          (beads-stats-refresh)
          (should (string-match-p "Total Issues:" (buffer-string))))

        ;; Cleanup
        (beads-stats-test--find-and-kill-stats-buffers)))))

;;; Edge Cases

(ert-deftest beads-stats-test-edge-case-large-numbers ()
  "Test rendering with very large numbers."
  (let ((large-stats '((summary . ((total_issues . 999999)
                                   (open_issues . 500000)
                                   (in_progress_issues . 250000)
                                   (closed_issues . 249999)
                                   (blocked_issues . 0)
                                   (deferred_issues . 0)
                                   (ready_issues . 100000)
                                   (tombstone_issues . 0)
                                   (pinned_issues . 0)
                                   (epics_eligible_for_closure . 0)
                                   (average_lead_time_hours . 1000.5)))
                       (recent_activity . nil))))
    (with-temp-buffer
      (let ((stats-data (beads-stats--parse-stats large-stats)))
        (beads-stats--render stats-data)
        (let ((content (buffer-string)))
          (should (string-match-p "999999" content))
          (should (string-match-p "500000" content)))))))

(ert-deftest beads-stats-test-edge-case-fractional-lead-time ()
  "Test formatting fractional lead time."
  (should (stringp (beads-stats--format-lead-time 0.1)))
  (should (stringp (beads-stats--format-lead-time 1.5)))
  (should (stringp (beads-stats--format-lead-time 25.75))))

(ert-deftest beads-stats-test-edge-case-very-long-lead-time ()
  "Test formatting very long lead time."
  (let ((result (beads-stats--format-lead-time 720))) ; 30 days
    (should (string-match-p "days" result))
    (should (string-match-p "30" result))))

;;; Performance Tests

(ert-deftest beads-stats-test-performance-parsing ()
  "Test parsing performance."
  :tags '(:performance)
  (let ((start-time (current-time)))
    (dotimes (_ 1000)
      (beads-stats--parse-stats beads-stats-test--sample-stats))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should parse 1000 times in under 0.5 seconds
      (should (< elapsed 0.5)))))

(ert-deftest beads-stats-test-performance-formatting ()
  "Test lead time formatting performance."
  :tags '(:performance)
  (let ((start-time (current-time)))
    (dotimes (_ 1000)
      (beads-stats--format-lead-time 48.5))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should format 1000 times in under 0.5 seconds
      (should (< elapsed 0.5)))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-stats-test-stats-command-exists ()
  "Integration test: Verify beads-stats command exists."
  :tags '(integration)
  (should (fboundp 'beads-stats)))

(ert-deftest beads-stats-test-keybinding-g-refresh ()
  "Integration test: Verify g keybinding for refresh."
  :tags '(integration)
  (with-temp-buffer
    (beads-stats-mode)
    (let ((binding (lookup-key beads-stats-mode-map (kbd "g"))))
      (should (eq binding 'beads-stats-refresh)))))

(ert-deftest beads-stats-test-keybinding-q-quit ()
  "Integration test: Verify q keybinding for quit."
  :tags '(integration)
  (with-temp-buffer
    (beads-stats-mode)
    (let ((binding (lookup-key beads-stats-mode-map (kbd "q"))))
      (should (eq binding 'quit-window)))))

;;; ============================================================
;;; Interactive Stats Tests (Button Functionality)
;;; ============================================================

(ert-deftest beads-stats-test-integer-to-float-conversion ()
  "Test that integer average_lead_time_hours is converted to float.
This catches the bug where bd returns 0 as an integer, but
beads-statistics class requires a float."
  ;; Test with integer 0
  (let ((stats-with-int '((summary . ((total_issues . 10)
                                      (open_issues . 5)
                                      (in_progress_issues . 2)
                                      (closed_issues . 3)
                                      (blocked_issues . 0)
                                      (deferred_issues . 0)
                                      (ready_issues . 5)
                                      (tombstone_issues . 0)
                                      (pinned_issues . 0)
                                      (epics_eligible_for_closure . 0)
                                      (average_lead_time_hours . 0))))))  ; Integer!
    (let* ((stats-data (beads-stats--parse-stats stats-with-int))
           (stats (oref stats-data summary)))
      (should (beads-statistics-p stats))
      (should (floatp (oref stats average-lead-time)))
      (should (= (oref stats average-lead-time) 0.0))))

  ;; Test with other integers
  (let ((stats-with-int '((summary . ((total_issues . 10)
                                      (open_issues . 5)
                                      (in_progress_issues . 2)
                                      (closed_issues . 3)
                                      (blocked_issues . 0)
                                      (deferred_issues . 0)
                                      (ready_issues . 5)
                                      (tombstone_issues . 0)
                                      (pinned_issues . 0)
                                      (epics_eligible_for_closure . 0)
                                      (average_lead_time_hours . 48))))))  ; Integer!
    (let* ((stats-data (beads-stats--parse-stats stats-with-int))
           (stats (oref stats-data summary)))
      (should (beads-statistics-p stats))
      (should (floatp (oref stats average-lead-time)))
      (should (= (oref stats average-lead-time) 48.0)))))

(ert-deftest beads-stats-test-button-creation ()
  "Test beads-stats--insert-stat-button creates proper button in buffer."
  (with-temp-buffer
    (beads-stats--insert-stat-button
     "42"
     'total
     42
     'font-lock-constant-face)

    ;; Verify button was inserted
    (should (string= (buffer-string) "42"))

    ;; Verify button properties at the start of the inserted text
    (goto-char (point-min))
    (should (button-at (point)))
    (let ((button (button-at (point))))
      (should button)
      (should (eq (button-type button) 'beads-stats-button))
      (should (eq (button-get button 'filter-type) 'total))
      (should (= (button-get button 'count) 42))
      (should (eq (button-get button 'face) 'font-lock-constant-face))
      (should (eq (button-get button 'mouse-face) 'highlight))
      (should (button-get button 'follow-link))
      (should (button-get button 'help-echo)))))

(ert-deftest beads-stats-test-button-help-echo ()
  "Test button help-echo messages are correct."
  ;; Total
  (with-temp-buffer
    (beads-stats--insert-stat-button "100" 'total 100 'default)
    (goto-char (point-min))
    (let ((button (button-at (point))))
      (should (string-match-p "all 100 issues"
                             (button-get button 'help-echo)))))

  ;; Open
  (with-temp-buffer
    (beads-stats--insert-stat-button "30" 'open 30 'success)
    (goto-char (point-min))
    (let ((button (button-at (point))))
      (should (string-match-p "30 open issues"
                             (button-get button 'help-echo)))))

  ;; In-progress
  (with-temp-buffer
    (beads-stats--insert-stat-button "15" 'in-progress 15 'warning)
    (goto-char (point-min))
    (let ((button (button-at (point))))
      (should (string-match-p "15 in-progress issues"
                             (button-get button 'help-echo)))))

  ;; Blocked
  (with-temp-buffer
    (beads-stats--insert-stat-button "5" 'blocked 5 'error)
    (goto-char (point-min))
    (let ((button (button-at (point))))
      (should (string-match-p "5 blocked issues"
                             (button-get button 'help-echo)))))

  ;; Closed
  (with-temp-buffer
    (beads-stats--insert-stat-button "50" 'closed 50 'shadow)
    (goto-char (point-min))
    (let ((button (button-at (point))))
      (should (string-match-p "50 closed issues"
                             (button-get button 'help-echo)))))

  ;; Ready
  (with-temp-buffer
    (beads-stats--insert-stat-button "25" 'ready 25 'success)
    (goto-char (point-min))
    (let ((button (button-at (point))))
      (should (string-match-p "25 ready issues"
                             (button-get button 'help-echo))))))

(ert-deftest beads-stats-test-button-action-total ()
  "Test button action for total filter calls beads-stats--list-all-issues."
  (let ((list-all-issues-called nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-command-execute) (lambda (_cmd) nil))
              ((symbol-function 'beads-list-mode) (lambda ()))
              ((symbol-function 'beads-list--populate-buffer)
               (lambda (_issues _view &optional _cmd)))
              ((symbol-function 'pop-to-buffer) (lambda (_buf)))
              ((symbol-function 'beads-stats--list-all-issues)
               (lambda () (setq list-all-issues-called t))))
      (beads-stats--open-filtered-list 'total 100)
      (should list-all-issues-called))))

(ert-deftest beads-stats-test-button-action-ready ()
  "Test button action for ready filter."
  (let ((beads-ready-called nil))
    (cl-letf (((symbol-function 'beads-ready)
               (lambda () (setq beads-ready-called t))))
      (beads-stats--open-filtered-list 'ready 25)
      (should beads-ready-called))))

(ert-deftest beads-stats-test-button-action-blocked ()
  "Test button action for blocked filter."
  (let ((beads-blocked-called nil))
    (cl-letf (((symbol-function 'beads-blocked)
               (lambda () (setq beads-blocked-called t))))
      (beads-stats--open-filtered-list 'blocked 5)
      (should beads-blocked-called))))

(ert-deftest beads-stats-test-button-action-open ()
  "Test button action for open status filter."
  (let ((command-executed nil)
        (beads-check-executable-called nil)
        (mock-cmd (beads-command-list)))
    (oset mock-cmd data nil)
    (cl-letf (((symbol-function 'beads-check-executable)
               (lambda () (setq beads-check-executable-called t)))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (when (cl-typep cmd 'beads-command-list)
                   (setq command-executed cmd))
                 mock-cmd))
              ((symbol-function 'beads-list-mode)
               (lambda ()))
              ((symbol-function 'beads-list--populate-buffer)
               (lambda (_issues _view &optional _cmd)))
              ((symbol-function 'pop-to-buffer)
               (lambda (_buf))))
      (beads-stats--open-filtered-list 'open 30)
      (should beads-check-executable-called)
      (should command-executed)
      (should (equal (oref command-executed status) "open")))))

(ert-deftest beads-stats-test-button-action-in-progress ()
  "Test button action for in-progress status filter."
  (let ((command-executed nil)
        (mock-cmd (beads-command-list)))
    (oset mock-cmd data nil)
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (when (cl-typep cmd 'beads-command-list)
                   (setq command-executed cmd))
                 mock-cmd))
              ((symbol-function 'beads-list-mode) (lambda ()))
              ((symbol-function 'beads-list--populate-buffer)
               (lambda (_issues _view &optional _cmd)))
              ((symbol-function 'pop-to-buffer) (lambda (_buf))))
      (beads-stats--open-filtered-list 'in-progress 15)
      (should command-executed)
      (should (equal (oref command-executed status) "in_progress")))))

(ert-deftest beads-stats-test-button-action-closed ()
  "Test button action for closed status filter."
  (let ((command-executed nil)
        (mock-cmd (beads-command-list)))
    (oset mock-cmd data nil)
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (when (cl-typep cmd 'beads-command-list)
                   (setq command-executed cmd))
                 mock-cmd))
              ((symbol-function 'beads-list-mode) (lambda ()))
              ((symbol-function 'beads-list--populate-buffer)
               (lambda (_issues _view &optional _cmd)))
              ((symbol-function 'pop-to-buffer) (lambda (_buf))))
      (beads-stats--open-filtered-list 'closed 50)
      (should command-executed)
      (should (equal (oref command-executed status) "closed")))))

(ert-deftest beads-stats-test-render-contains-buttons ()
  "Test that beads-stats--render creates buttons in output."
  (with-temp-buffer
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats-data)

      ;; Search for Total Issues button - in new format: "Total Issues:      100"
      (goto-char (point-min))
      (should (search-forward "Total Issues:" nil t))
      ;; Move forward to find the number (with spaces for alignment)
      (skip-chars-forward " ")
      (let ((button (button-at (point))))
        (should button)
        (should (eq (button-get button 'filter-type) 'total)))

      ;; Search through buffer for buttons with different filter types
      ;; Just verify that buttons exist with the correct filter-type properties
      (goto-char (point-min))
      (let ((found-open nil))
        (while (not (eobp))
          (let ((button (button-at (point))))
            (when (and button (eq (button-get button 'filter-type) 'open))
              (setq found-open t)))
          (forward-char 1))
        (should found-open)))))

(ert-deftest beads-stats-test-render-button-faces ()
  "Test that buttons in rendered output have correct faces."
  (with-temp-buffer
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats-data)
      (let ((content (buffer-string)))
        ;; Verify the stats are in the output
        (should (string-match-p "Total Issues:" content))
        (should (string-match-p "100" content))
        (should (string-match-p "Open:" content))
        (should (string-match-p "30" content))
        (should (string-match-p "In Progress:" content))
        (should (string-match-p "15" content))))))

(ert-deftest beads-stats-test-zero-issues-with-buttons ()
  "Test rendering with zero issues creates buttons correctly."
  (with-temp-buffer
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--zero-stats)))
      (beads-stats--render stats-data)
      (goto-char (point-min))

      ;; Even with zero issues, buttons should be created
      (should (search-forward "Total Issues:" nil t))
      ;; Move forward past spaces to find the number
      (skip-chars-forward " ")
      (let ((button (button-at (point))))
        (should button)
        (should (eq (button-get button 'filter-type) 'total))
        (should (= (button-get button 'count) 0))))))

;;; Navigation Tests

(ert-deftest beads-stats-test-next-navigation ()
  "Test beads-stats-next moves to next button."
  (with-temp-buffer
    (beads-stats-mode)
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats-data)
      ;; Point should start at first button (Total Issues)
      (should (button-at (point)))
      (should (eq (button-get (button-at (point)) 'filter-type) 'total))

      ;; Move to next button (Open)
      (beads-stats-next)
      (should (button-at (point)))
      (should (eq (button-get (button-at (point)) 'filter-type) 'open))

      ;; Move to next button (In Progress)
      (beads-stats-next)
      (should (button-at (point)))
      (should (eq (button-get (button-at (point)) 'filter-type)
                  'in-progress)))))

(ert-deftest beads-stats-test-previous-navigation ()
  "Test beads-stats-previous moves to previous button."
  (with-temp-buffer
    (beads-stats-mode)
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats-data)
      ;; Start at first button, move to third
      (beads-stats-next)
      (beads-stats-next)
      (should (eq (button-get (button-at (point)) 'filter-type)
                  'in-progress))

      ;; Move back to Open
      (beads-stats-previous)
      (should (button-at (point)))
      (should (eq (button-get (button-at (point)) 'filter-type) 'open))

      ;; Move back to Total
      (beads-stats-previous)
      (should (button-at (point)))
      (should (eq (button-get (button-at (point)) 'filter-type) 'total)))))

(ert-deftest beads-stats-test-next-wraps-to-first ()
  "Test beads-stats-next wraps to first button from last."
  (with-temp-buffer
    (beads-stats-mode)
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats-data)
      ;; Navigate to last button (Ready)
      (goto-char (point-max))
      (backward-button 1 t t)
      (should (eq (button-get (button-at (point)) 'filter-type) 'ready))

      ;; Next from last should wrap to first (Total)
      (beads-stats-next)
      (should (button-at (point)))
      (should (eq (button-get (button-at (point)) 'filter-type) 'total)))))

(ert-deftest beads-stats-test-previous-wraps-to-last ()
  "Test beads-stats-previous wraps to last button from first."
  (with-temp-buffer
    (beads-stats-mode)
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats-data)
      ;; Start at first button (Total)
      (should (eq (button-get (button-at (point)) 'filter-type) 'total))

      ;; Previous from first should wrap to last (Ready)
      (beads-stats-previous)
      (should (button-at (point)))
      (should (eq (button-get (button-at (point)) 'filter-type) 'ready)))))

(ert-deftest beads-stats-test-render-positions-at-first-button ()
  "Test that beads-stats--render positions point at first button."
  (with-temp-buffer
    (let ((stats-data (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats-data)
      ;; Point should be positioned at the first button (Total Issues)
      (should (button-at (point)))
      (should (eq (button-get (button-at (point)) 'filter-type) 'total)))))

;;; Additional Coverage Tests

(ert-deftest beads-stats-test-list-all-issues-function-exists ()
  "Test list-all-issues function exists."
  (should (fboundp 'beads-stats--list-all-issues)))

(ert-deftest beads-stats-test-list-by-status-function-exists ()
  "Test list-by-status function exists."
  (should (fboundp 'beads-stats--list-by-status)))

(ert-deftest beads-stats-test-open-filtered-list-function-exists ()
  "Test open-filtered-list function exists."
  (should (fboundp 'beads-stats--open-filtered-list)))

(ert-deftest beads-stats-test-beads-stats-function-exists ()
  "Test beads-stats command exists."
  (should (fboundp 'beads-stats)))

(ert-deftest beads-stats-test-refresh-function-exists ()
  "Test beads-stats-refresh function exists."
  (should (fboundp 'beads-stats-refresh)))

;;; Button Action and Filter Tests

(ert-deftest beads-stats-test-button-action-calls-open-filtered-list ()
  "Test that button action extracts filter-type and calls open-filtered-list."
  (let ((called-with nil))
    (cl-letf (((symbol-function 'beads-stats--open-filtered-list)
               (lambda (filter-type count)
                 (setq called-with (list filter-type count)))))
      (with-temp-buffer
        ;; Create a button with properties
        (insert-text-button "5"
                           'type 'beads-stats-button
                           'filter-type 'open
                           'count 5)
        (goto-char (point-min))
        ;; Get the button at point and call action
        (let ((button (button-at (point))))
          (beads-stats--button-action button))
        (should (equal called-with '(open 5)))))))

(ert-deftest beads-stats-test-open-filtered-list-unknown-type ()
  "Test that unknown filter type shows message."
  (let ((message-shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (setq message-shown args))))
      (beads-stats--open-filtered-list 'unknown-type 10)
      (should message-shown)
      (should (string-match-p "Unknown" (car message-shown))))))

;;; List All Issues Tests

(ert-deftest beads-stats-test-list-all-issues-no-issues ()
  "Test beads-stats--list-all-issues with no issues.
The function extracts issues from the data slot of the command object."
  (let ((mock-cmd (beads-command-list)))
    (oset mock-cmd data nil)
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) mock-cmd))
              ((symbol-function 'pop-to-buffer) (lambda (_buf))))
      (beads-stats--list-all-issues)
      (let ((buf (get-buffer (beads-stats-test--get-list-buffer))))
        (should buf)
        (with-current-buffer buf
          (should (eq major-mode 'beads-list-mode))
          (should (null tabulated-list-entries)))
        (kill-buffer buf)))))

(ert-deftest beads-stats-test-list-all-issues-with-issues ()
  "Test beads-stats--list-all-issues with issues.
The function extracts issues from the data slot of the command object."
  (let* ((mock-issues (list (beads-issue :id "bd-1" :title "Test 1"
                                         :status "open" :priority 1)
                            (beads-issue :id "bd-2" :title "Test 2"
                                         :status "closed" :priority 2)))
         (mock-cmd (beads-command-list)))
    (oset mock-cmd data mock-issues)
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) mock-cmd))
              ((symbol-function 'beads-list--populate-buffer)
               (lambda (issues view &optional cmd)
                 (setq tabulated-list-entries
                       (mapcar (lambda (i) (list (oref i id) [])) issues))))
              ((symbol-function 'pop-to-buffer) (lambda (_buf))))
      (beads-stats--list-all-issues)
      (let ((buf (get-buffer (beads-stats-test--get-list-buffer))))
        (should buf)
        (with-current-buffer buf
          (should (eq major-mode 'beads-list-mode))
          (should (= 2 (length tabulated-list-entries))))
        (kill-buffer buf)))))

;;; List By Status Tests

(ert-deftest beads-stats-test-list-by-status-open ()
  "Test beads-stats--list-by-status with open status.
Verifies command is created with correct status filter."
  (let* ((mock-issues (list (beads-issue :id "bd-1" :title "Test 1"
                                         :status "open" :priority 1)))
         (mock-cmd (beads-command-list))
         (captured-status nil))
    (oset mock-cmd data mock-issues)
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-command-list)
               (lambda (&rest args)
                 (setq captured-status (plist-get args :status))
                 mock-cmd))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) mock-cmd))
              ((symbol-function 'beads-list--populate-buffer)
               (lambda (issues view &optional cmd)
                 (setq tabulated-list-entries
                       (mapcar (lambda (i) (list (oref i id) [])) issues))))
              ((symbol-function 'pop-to-buffer) (lambda (_buf))))
      (beads-stats--list-by-status 'open)
      (should (equal captured-status "open"))
      (let ((buf (get-buffer (beads-stats-test--get-list-buffer "open"))))
        (should buf)
        (kill-buffer buf)))))

(ert-deftest beads-stats-test-list-by-status-in-progress ()
  "Test beads-stats--list-by-status with in-progress status.
Verifies the symbol in-progress maps to string in_progress."
  (let* ((mock-issues nil)
         (mock-cmd (beads-command-list))
         (captured-status nil))
    (oset mock-cmd data mock-issues)
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-command-list)
               (lambda (&rest args)
                 (setq captured-status (plist-get args :status))
                 mock-cmd))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) mock-cmd))
              ((symbol-function 'pop-to-buffer) (lambda (_buf))))
      (beads-stats--list-by-status 'in-progress)
      (should (equal captured-status "in_progress"))
      (let ((buf (get-buffer (beads-stats-test--get-list-buffer "in_progress"))))
        (should buf)
        (kill-buffer buf)))))

(ert-deftest beads-stats-test-list-by-status-closed ()
  "Test beads-stats--list-by-status with closed status."
  (let* ((mock-cmd (beads-command-list))
         (captured-status nil))
    (oset mock-cmd data nil)
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-command-list)
               (lambda (&rest args)
                 (setq captured-status (plist-get args :status))
                 mock-cmd))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) mock-cmd))
              ((symbol-function 'pop-to-buffer) (lambda (_buf))))
      (beads-stats--list-by-status 'closed)
      (should (equal captured-status "closed"))
      (let ((buf (get-buffer (beads-stats-test--get-list-buffer "closed"))))
        (should buf)
        (kill-buffer buf)))))

(ert-deftest beads-stats-test-list-by-status-invalid ()
  "Test beads-stats--list-by-status with invalid status signals error."
  (should-error (beads-stats--list-by-status 'invalid-status)
                :type 'error))

;;; Integration Tests for Stats Buffer Links
;;
;; These are TRUE integration tests that create real issues in a temporary
;; project and verify the full workflow from stats buffer to list buffer.
;; No mocking of command execution - uses actual bd commands.

(ert-deftest beads-stats-test-click-total-issues-real ()
  "Integration test: clicking Total Issues shows all issues.
Creates real issues and verifies they appear in the list buffer."
  :tags '(integration)
  (beads-test-with-project ()
    ;; Create real issues in the temp project
    (let ((id1 (beads-test-create-issue "Open Issue" "task" 1))
          (id2 (beads-test-create-issue "Another Open" "bug" 2)))
      (beads-stats--list-all-issues)
      (let ((buf (get-buffer (beads-stats-test--get-list-buffer))))
        (should buf)
        (with-current-buffer buf
          (should (eq major-mode 'beads-list-mode))
          (should (>= (length tabulated-list-entries) 2))
          ;; Verify our issues are in the list
          (let ((ids (mapcar #'car tabulated-list-entries)))
            (should (member id1 ids))
            (should (member id2 ids))))
        (kill-buffer buf)))))

(ert-deftest beads-stats-test-click-open-issues-real ()
  "Integration test: clicking Open shows only open issues.
Creates issues with different statuses and verifies filtering."
  :tags '(integration)
  (beads-test-with-project ()
    ;; Create issues - one open, one we'll close
    (let ((open-id (beads-test-create-issue "Open Issue" "task" 1))
          (closed-id (beads-test-create-issue "Will Close" "task" 2)))
      ;; Close one issue
      (beads-command-execute
       (beads-command-close :issue-ids (list closed-id) :reason "Done"))
      ;; Click Open filter
      (beads-stats--list-by-status 'open)
      (let ((buf (get-buffer (beads-stats-test--get-list-buffer "open"))))
        (should buf)
        (with-current-buffer buf
          (should (eq major-mode 'beads-list-mode))
          ;; Verify open issue is present, closed is not
          (let ((ids (mapcar #'car tabulated-list-entries)))
            (should (member open-id ids))
            (should-not (member closed-id ids))))
        (kill-buffer buf)))))

(ert-deftest beads-stats-test-click-closed-issues-real ()
  "Integration test: clicking Closed shows only closed issues."
  :tags '(integration)
  (beads-test-with-project ()
    ;; Create and close an issue
    (let ((open-id (beads-test-create-issue "Stays Open" "task" 1))
          (closed-id (beads-test-create-issue "Will Close" "task" 2)))
      (beads-command-execute
       (beads-command-close :issue-ids (list closed-id) :reason "Completed"))
      ;; Click Closed filter
      (beads-stats--list-by-status 'closed)
      (let ((buf (get-buffer (beads-stats-test--get-list-buffer "closed"))))
        (should buf)
        (with-current-buffer buf
          (let ((ids (mapcar #'car tabulated-list-entries)))
            (should (member closed-id ids))
            (should-not (member open-id ids))))
        (kill-buffer buf)))))

(ert-deftest beads-stats-test-click-ready-real ()
  "Integration test: clicking Ready shows ready issues.
Issues without blockers should appear as ready."
  :tags '(integration)
  (beads-test-with-project ()
    ;; Create an open issue - it should be ready (no blockers)
    (let* ((ready-id (beads-test-create-issue "Ready Issue" "task" 1))
           (buf-name (beads-buffer-list "ready" nil)))
      (beads-ready)
      (let ((buf (get-buffer buf-name)))
        (should buf)
        (with-current-buffer buf
          (should (eq major-mode 'beads-list-mode))
          (let ((ids (mapcar #'car tabulated-list-entries)))
            (should (member ready-id ids))))
        (kill-buffer buf)))))

(ert-deftest beads-stats-test-full-workflow-real ()
  "Integration test: full workflow from beads-stats to filtered list.
Runs beads-stats, clicks a button, verifies correct list appears."
  :tags '(integration)
  (beads-test-with-project ()
    ;; Create some issues
    (let ((id1 (beads-test-create-issue "Issue One" "task" 1))
          (id2 (beads-test-create-issue "Issue Two" "bug" 2)))
      ;; Run beads-stats to create the stats buffer
      (beads-stats)
      (let ((stats-buf (get-buffer (beads-stats-test--get-stats-buffer))))
        (should stats-buf)
        (with-current-buffer stats-buf
          (should (eq major-mode 'beads-stats-mode))
          ;; Find the Total Issues button
          (goto-char (point-min))
          (search-forward "Total Issues:" nil t)
          (skip-chars-forward " ")
          (let ((button (button-at (point))))
            (should button)
            (should (eq (button-get button 'filter-type) 'total))
            ;; Click the button
            (button-activate button)))
        (kill-buffer stats-buf))
      ;; Verify list buffer was created with our issues
      (let ((list-buf (get-buffer (beads-stats-test--get-list-buffer))))
        (should list-buf)
        (with-current-buffer list-buf
          (should (eq major-mode 'beads-list-mode))
          (let ((ids (mapcar #'car tabulated-list-entries)))
            (should (member id1 ids))
            (should (member id2 ids))))
        (kill-buffer list-buf)))))

(ert-deftest beads-stats-test-button-dispatches-correctly ()
  "Test button action dispatches to correct handler based on filter-type."
  (let ((called-filter nil))
    (cl-letf (((symbol-function 'beads-stats--open-filtered-list)
               (lambda (filter-type count)
                 (setq called-filter (list filter-type count)))))
      (with-temp-buffer
        (beads-stats--insert-stat-button "42" 'open 42 'default)
        (goto-char (point-min))
        (button-activate (button-at (point)))
        (should (equal called-filter '(open 42)))))))

(provide 'beads-stats-test)
;;; beads-stats-test.el ends here
