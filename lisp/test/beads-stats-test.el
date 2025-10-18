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
    (average_lead_time_hours . 0))
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
    (should (= (alist-get 'total stats) 100))
    (should (= (alist-get 'open stats) 30))
    (should (= (alist-get 'in-progress stats) 15))
    (should (= (alist-get 'closed stats) 50))
    (should (= (alist-get 'blocked stats) 5))
    (should (= (alist-get 'ready stats) 25))
    (should (= (alist-get 'lead-time stats) 48.5))))

(ert-deftest beads-stats-test-parse-zero-stats ()
  "Test parsing statistics with all zeros."
  (let ((stats (beads-stats--parse-stats beads-stats-test--zero-stats)))
    (should (= (alist-get 'total stats) 0))
    (should (= (alist-get 'open stats) 0))
    (should (= (alist-get 'closed stats) 0))
    (should (= (alist-get 'lead-time stats) 0))))

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
  "Test basic rendering of statistics."
  (with-temp-buffer
    (let ((stats (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats)
      (let ((content (buffer-string)))
        (should (string-match-p "Beads Issue Statistics" content))
        (should (string-match-p "Total Issues:" content))
        (should (string-match-p "100" content))
        (should (string-match-p "By Status:" content))
        (should (string-match-p "Open:" content))
        (should (string-match-p "30" content))
        (should (string-match-p "In Progress:" content))
        (should (string-match-p "15" content))
        (should (string-match-p "Blocked:" content))
        (should (string-match-p "5" content))
        (should (string-match-p "Closed:" content))
        (should (string-match-p "50" content))
        (should (string-match-p "Ready to Work:" content))
        (should (string-match-p "25" content))
        (should (string-match-p "Average Lead Time:" content))))))

(ert-deftest beads-stats-test-render-zero-stats ()
  "Test rendering statistics with all zeros."
  (with-temp-buffer
    (let ((stats (beads-stats--parse-stats beads-stats-test--zero-stats)))
      (beads-stats--render stats)
      (let ((content (buffer-string)))
        (should (string-match-p "Total Issues:" content))
        (should (string-match-p "\\b0\\b" content))))))

(ert-deftest beads-stats-test-render-has-commands ()
  "Test that rendered output includes command help."
  (with-temp-buffer
    (let ((stats (beads-stats--parse-stats beads-stats-test--sample-stats)))
      (beads-stats--render stats)
      (let ((content (buffer-string)))
        (should (string-match-p "Commands:" content))
        (should (string-match-p "g - refresh" content))
        (should (string-match-p "q - quit" content))))))

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

(ert-deftest beads-stats-test-integration-full-workflow ()
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

(provide 'beads-stats-test)
;;; beads-stats-test.el ends here
