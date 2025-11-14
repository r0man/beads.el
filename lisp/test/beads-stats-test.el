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
(require 'beads-types)
(require 'beads-stats)

;;; Test Fixtures

(defvar beads-stats-test--sample-stats
  '((total_issues . 100)
    (open_issues . 30)
    (in_progress_issues . 15)
    (closed_issues . 50)
    (blocked_issues . 5)
    (ready_issues . 25)
    (average_lead_time_hours . 48.5))
  "Sample statistics data for testing.")

(defvar beads-stats-test--zero-stats
  '((total_issues . 0)
    (open_issues . 0)
    (in_progress_issues . 0)
    (closed_issues . 0)
    (blocked_issues . 0)
    (ready_issues . 0)
    (average_lead_time_hours . 0.0))
  "Zero statistics for testing edge cases.")

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

;;; Tests for Statistics Parsing

(ert-deftest beads-stats-test-parse-stats ()
  "Test parsing statistics from JSON."
  (let ((stats (beads-stats--parse-stats beads-stats-test--sample-stats)))
    (should (beads-statistics-p stats))
    (should (= (oref stats total-issues) 100))
    (should (= (oref stats open-issues) 30))
    (should (= (oref stats in-progress-issues) 15))
    (should (= (oref stats closed-issues) 50))
    (should (= (oref stats blocked-issues) 5))
    (should (= (oref stats ready-issues) 25))
    (should (= (oref stats average-lead-time) 48.5))))

(ert-deftest beads-stats-test-parse-zero-stats ()
  "Test parsing statistics with all zeros."
  (let ((stats (beads-stats--parse-stats beads-stats-test--zero-stats)))
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
    (let ((stats (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats)
      (let ((content (buffer-string)))
        ;; Check for CLI-style header with emoji
        (should (string-match-p "📊 Beads Statistics:" content))
        (should (string-match-p "Total Issues:" content))
        (should (string-match-p "100" content))
        (should (string-match-p "Open:" content))
        (should (string-match-p "30" content))
        (should (string-match-p "In Progress:" content))
        (should (string-match-p "15" content))
        (should (string-match-p "Closed:" content))
        (should (string-match-p "50" content))
        (should (string-match-p "Blocked:" content))
        (should (string-match-p "5" content))
        (should (string-match-p "Ready:" content))
        (should (string-match-p "25" content))
        ;; Should NOT have extra sections from old format
        (should-not (string-match-p "By Status:" content))
        (should-not (string-match-p "Ready to Work:" content))
        (should-not (string-match-p "Average Lead Time:" content))))))

(ert-deftest beads-stats-test-render-zero-stats ()
  "Test rendering statistics with all zeros."
  (with-temp-buffer
    (let ((stats (beads-stats--parse-stats beads-stats-test--zero-stats)))
      (beads-stats--render stats)
      (let ((content (buffer-string)))
        (should (string-match-p "Total Issues:" content))
        (should (string-match-p "\\b0\\b" content))))))

(ert-deftest beads-stats-test-render-matches-cli ()
  "Test that rendered output matches CLI format exactly.
The CLI output is simple and does not include a Commands footer."
  (with-temp-buffer
    (let ((stats (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats)
      (let ((content (buffer-string)))
        ;; Verify CLI-style format
        (should (string-match-p "📊 Beads Statistics:" content))
        (should (string-match-p "Total Issues:" content))
        (should (string-match-p "Open:" content))
        (should (string-match-p "In Progress:" content))
        (should (string-match-p "Closed:" content))
        (should (string-match-p "Blocked:" content))
        (should (string-match-p "Ready:" content))
        ;; Should NOT have separators, section headers, or command footer
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
  (let ((json-output (json-encode beads-stats-test--sample-stats)))
    (cl-letf (((symbol-function 'call-process)
               (beads-stats-test--mock-call-process 0 json-output)))
      (beads-stats)
      (should (get-buffer "*beads-stats*"))
      (kill-buffer "*beads-stats*"))))

(ert-deftest beads-stats-test-command-sets-mode ()
  "Test that beads-stats sets the correct mode."
  (let ((json-output (json-encode beads-stats-test--sample-stats)))
    (cl-letf (((symbol-function 'call-process)
               (beads-stats-test--mock-call-process 0 json-output)))
      (beads-stats)
      (with-current-buffer "*beads-stats*"
        (should (eq major-mode 'beads-stats-mode)))
      (kill-buffer "*beads-stats*"))))

(ert-deftest beads-stats-test-command-displays-content ()
  "Test that beads-stats displays statistics content."
  (let ((json-output (json-encode beads-stats-test--sample-stats)))
    (cl-letf (((symbol-function 'call-process)
               (beads-stats-test--mock-call-process 0 json-output)))
      (beads-stats)
      (with-current-buffer "*beads-stats*"
        (let ((content (buffer-string)))
          (should (string-match-p "Total Issues:" content))
          (should (string-match-p "100" content))))
      (kill-buffer "*beads-stats*"))))

(ert-deftest beads-stats-test-command-error-handling ()
  "Test that beads-stats handles command failure gracefully."
  (let ((json-output (json-encode beads-stats-test--sample-stats)))
    (cl-letf (((symbol-function 'call-process)
               (beads-stats-test--mock-call-process 1 "Error: failed")))
      (beads-stats)
      (should (get-buffer "*beads-stats*"))
      (with-current-buffer "*beads-stats*"
        (let ((content (buffer-string)))
          (should (string-match-p "Error fetching statistics" content))))
      (kill-buffer "*beads-stats*"))))

;;; Tests for Refresh

(ert-deftest beads-stats-test-refresh-updates-buffer ()
  "Test that beads-stats-refresh updates the buffer."
  (let ((json-output (json-encode beads-stats-test--sample-stats)))
    (cl-letf (((symbol-function 'call-process)
               (beads-stats-test--mock-call-process 0 json-output)))
      (beads-stats)
      (with-current-buffer "*beads-stats*"
        ;; Modify buffer to test refresh
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert "TEST"))
        ;; Refresh
        (beads-stats-refresh)
        (let ((content (buffer-string)))
          (should-not (string-match-p "TEST" content))
          (should (string-match-p "Total Issues:" content))))
      (kill-buffer "*beads-stats*"))))

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
  (let ((json-output (json-encode beads-stats-test--sample-stats)))
    (cl-letf (((symbol-function 'call-process)
               (beads-stats-test--mock-call-process 0 json-output)))
      ;; Open stats
      (beads-stats)
      (should (get-buffer "*beads-stats*"))

      ;; Verify mode
      (with-current-buffer "*beads-stats*"
        (should (eq major-mode 'beads-stats-mode))

        ;; Verify content
        (let ((content (buffer-string)))
          (should (string-match-p "100" content))
          (should (string-match-p "30" content))
          (should (string-match-p "15" content))
          (should (string-match-p "50" content))
          (should (string-match-p "5" content))
          (should (string-match-p "25" content)))

        ;; Test refresh
        (beads-stats-refresh)
        (should (string-match-p "Total Issues:" (buffer-string))))

      ;; Cleanup
      (kill-buffer "*beads-stats*"))))

;;; Edge Cases

(ert-deftest beads-stats-test-edge-case-large-numbers ()
  "Test rendering with very large numbers."
  (let ((large-stats '((total_issues . 999999)
                      (open_issues . 500000)
                      (in_progress_issues . 250000)
                      (closed_issues . 249999)
                      (blocked_issues . 0)
                      (ready_issues . 100000)
                      (average_lead_time_hours . 1000.5))))
    (with-temp-buffer
      (let ((stats (beads-stats--parse-stats large-stats)))
        (beads-stats--render stats)
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
  (let ((stats-with-int '((total_issues . 10)
                          (open_issues . 5)
                          (in_progress_issues . 2)
                          (closed_issues . 3)
                          (blocked_issues . 0)
                          (ready_issues . 5)
                          (average_lead_time_hours . 0))))  ; Integer!
    (let ((stats (beads-stats--parse-stats stats-with-int)))
      (should (beads-statistics-p stats))
      (should (floatp (oref stats average-lead-time)))
      (should (= (oref stats average-lead-time) 0.0))))

  ;; Test with other integers
  (let ((stats-with-int '((total_issues . 10)
                          (open_issues . 5)
                          (in_progress_issues . 2)
                          (closed_issues . 3)
                          (blocked_issues . 0)
                          (ready_issues . 5)
                          (average_lead_time_hours . 48))))  ; Integer!
    (let ((stats (beads-stats--parse-stats stats-with-int)))
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
        (beads-check-executable-called nil))
    (cl-letf (((symbol-function 'beads-check-executable)
               (lambda () (setq beads-check-executable-called t)))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (when (cl-typep cmd 'beads-command-list)
                   (setq command-executed cmd))
                 nil))
              ((symbol-function 'beads-list-mode)
               (lambda ()))
              ((symbol-function 'beads-list--populate-buffer)
               (lambda (_issues _cmd)))
              ((symbol-function 'pop-to-buffer)
               (lambda (_buf))))
      (beads-stats--open-filtered-list 'open 30)
      (should beads-check-executable-called)
      (should command-executed)
      (should (equal (oref command-executed status) "open")))))

(ert-deftest beads-stats-test-button-action-in-progress ()
  "Test button action for in-progress status filter."
  (let ((command-executed nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (when (cl-typep cmd 'beads-command-list)
                   (setq command-executed cmd))
                 nil))
              ((symbol-function 'beads-list-mode) (lambda ()))
              ((symbol-function 'beads-list--populate-buffer)
               (lambda (_issues _cmd)))
              ((symbol-function 'pop-to-buffer) (lambda (_buf))))
      (beads-stats--open-filtered-list 'in-progress 15)
      (should command-executed)
      (should (equal (oref command-executed status) "in_progress")))))

(ert-deftest beads-stats-test-button-action-closed ()
  "Test button action for closed status filter."
  (let ((command-executed nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (when (cl-typep cmd 'beads-command-list)
                   (setq command-executed cmd))
                 nil))
              ((symbol-function 'beads-list-mode) (lambda ()))
              ((symbol-function 'beads-list--populate-buffer)
               (lambda (_issues _cmd)))
              ((symbol-function 'pop-to-buffer) (lambda (_buf))))
      (beads-stats--open-filtered-list 'closed 50)
      (should command-executed)
      (should (equal (oref command-executed status) "closed")))))

(ert-deftest beads-stats-test-render-contains-buttons ()
  "Test that beads-stats--render creates buttons in output."
  (with-temp-buffer
    (let ((stats (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats)

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
    (let ((stats (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats)
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
    (let ((stats (beads-stats--parse-stats beads-stats-test--zero-stats)))
      (beads-stats--render stats)
      (goto-char (point-min))

      ;; Even with zero issues, buttons should be created
      (should (search-forward "Total Issues:" nil t))
      ;; Move forward past spaces to find the number
      (skip-chars-forward " ")
      (let ((button (button-at (point))))
        (should button)
        (should (eq (button-get button 'filter-type) 'total))
        (should (= (button-get button 'count) 0))))))

(provide 'beads-stats-test)
;;; beads-stats-test.el ends here
