;;; beads-restore-test.el --- Tests for beads-restore -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-restore.el.
;; Tests cover the restore command for compacted issues.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-restore)

;;; Test Fixtures

(defvar beads-restore-test--sample-compacted-issue
  '((id . "bd-42")
    (title . "Test Compacted Issue")
    (status . "closed")
    (priority . 1)
    (issue-type . "task")
    (created-at . "2025-01-01T10:00:00Z")
    (updated-at . "2025-01-15T10:30:00Z")
    (closed-at . "2025-01-15T10:30:00Z")
    (compacted-at . "2025-01-20T12:00:00Z")
    (description . "This is the full description from git history.")
    (acceptance-criteria . "- Criterion 1\n- Criterion 2")
    (design . "## Design Overview\n\nFull design details.")
    (notes . "Historical notes about the issue.")
    (events . [((event_type . "created")
                (actor . "user1")
                (timestamp . "2025-01-01T10:00:00Z")
                (details . "Issue created"))
               ((event_type . "status_changed")
                (actor . "user2")
                (timestamp . "2025-01-10T15:00:00Z")
                (details . "Status changed to in_progress"))
               ((event_type . "closed")
                (actor . "user1")
                (timestamp . "2025-01-15T10:30:00Z")
                (details . "Issue closed"))]))
  "Sample compacted issue with full history for testing.")

(defvar beads-restore-test--sample-issue-no-events
  '((id . "bd-43")
    (title . "Issue Without Events")
    (status . "closed")
    (created-at . "2025-01-01T10:00:00Z")
    (updated-at . "2025-01-15T10:30:00Z")
    (description . "Simple issue without events."))
  "Sample issue without event history.")

;;; Test Utilities

(defun beads-restore-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(ert-deftest beads-restore-test-detect-issue-id-from-buffer-name ()
  "Test detecting issue ID from buffer name."
  (with-temp-buffer
    (rename-buffer "*beads-restore: bd-42*" t)
    (should (equal (beads-restore--detect-issue-id) "bd-42"))))

(ert-deftest beads-restore-test-detect-issue-id-from-show-buffer ()
  "Test detecting issue ID from show buffer name."
  (with-temp-buffer
    (rename-buffer "*beads-show: bd-99*" t)
    (should (equal (beads-restore--detect-issue-id) "bd-99"))))

(ert-deftest beads-restore-test-detect-issue-id-no-match ()
  "Test detecting issue ID returns nil when no match."
  (with-temp-buffer
    (rename-buffer "*random-buffer*" t)
    (should (null (beads-restore--detect-issue-id)))))

(ert-deftest beads-restore-test-format-date-valid ()
  "Test date formatting with valid ISO date."
  (let ((result (beads-restore--format-date
                 "2025-01-15T10:30:45.123456Z")))
    (should (string= result "2025-01-15 10:30:45"))))

(ert-deftest beads-restore-test-format-date-nil ()
  "Test date formatting with nil."
  (let ((result (beads-restore--format-date nil)))
    (should (string= result "N/A"))))

(ert-deftest beads-restore-test-format-date-no-fractional ()
  "Test date formatting without fractional seconds."
  (let ((result (beads-restore--format-date "2025-01-15T10:30:00Z")))
    (should (string= result "2025-01-15 10:30:00"))))

;;; ============================================================
;;; Rendering Tests
;;; ============================================================

(ert-deftest beads-restore-test-render-issue-basic ()
  "Test basic issue rendering."
  (with-temp-buffer
    (beads-restore--render-issue
     beads-restore-test--sample-compacted-issue)
    (let ((content (buffer-string)))
      (should (string-match-p "Test Compacted Issue" content))
      (should (string-match-p "bd-42" content))
      (should (string-match-p "HISTORICAL VIEW" content))
      (should (string-match-p "full description" content)))))

(ert-deftest beads-restore-test-render-issue-metadata ()
  "Test that metadata is rendered correctly."
  (with-temp-buffer
    (beads-restore--render-issue
     beads-restore-test--sample-compacted-issue)
    (let ((content (buffer-string)))
      (should (string-match-p "ID: bd-42" content))
      (should (string-match-p "Status: CLOSED" content))
      (should (string-match-p "Priority: 1" content))
      (should (string-match-p "Type: TASK" content)))))

(ert-deftest beads-restore-test-render-issue-dates ()
  "Test that dates are rendered correctly."
  (with-temp-buffer
    (beads-restore--render-issue
     beads-restore-test--sample-compacted-issue)
    (let ((content (buffer-string)))
      (should (string-match-p "Created: 2025-01-01" content))
      (should (string-match-p "Updated: 2025-01-15" content))
      (should (string-match-p "Closed: 2025-01-15" content))
      (should (string-match-p "Compacted: 2025-01-20" content)))))

(ert-deftest beads-restore-test-render-issue-sections ()
  "Test that text sections are rendered."
  (with-temp-buffer
    (beads-restore--render-issue
     beads-restore-test--sample-compacted-issue)
    (let ((content (buffer-string)))
      (should (string-match-p "Description" content))
      (should (string-match-p "Acceptance Criteria" content))
      (should (string-match-p "Design" content))
      (should (string-match-p "Notes" content)))))

(ert-deftest beads-restore-test-render-issue-events ()
  "Test that event history is rendered."
  (with-temp-buffer
    (beads-restore--render-issue
     beads-restore-test--sample-compacted-issue)
    (let ((content (buffer-string)))
      (should (string-match-p "Event History" content))
      (should (string-match-p "created" content))
      (should (string-match-p "status_changed" content))
      (should (string-match-p "user1" content))
      (should (string-match-p "user2" content)))))

(ert-deftest beads-restore-test-render-issue-no-events ()
  "Test rendering issue without events."
  (with-temp-buffer
    (beads-restore--render-issue
     beads-restore-test--sample-issue-no-events)
    (let ((content (buffer-string)))
      (should (string-match-p "bd-43" content))
      (should-not (string-match-p "Event History" content)))))

(ert-deftest beads-restore-test-render-issue-empty-events ()
  "Test rendering issue with empty events array."
  (with-temp-buffer
    (let ((issue (copy-alist beads-restore-test--sample-issue-no-events)))
      (push '(events . []) issue)
      (beads-restore--render-issue issue)
      (let ((content (buffer-string)))
        (should-not (string-match-p "Event History" content))))))

(ert-deftest beads-restore-test-render-issue-historical-banner ()
  "Test that historical view banner is present."
  (with-temp-buffer
    (beads-restore--render-issue
     beads-restore-test--sample-compacted-issue)
    (let ((content (buffer-string)))
      (should (string-match-p "HISTORICAL VIEW" content))
      (should (string-match-p "Restored from Git" content))
      (should (string-match-p "read-only historical snapshot" content)))))

;;; ============================================================
;;; Command Tests
;;; ============================================================

(ert-deftest beads-restore-test-restore-success ()
  "Test successful restore operation."
  (let* ((json-output (json-encode
                       beads-restore-test--sample-compacted-issue))
         (beads-executable "bd"))
    (cl-letf (((symbol-function 'call-process)
               (beads-restore-test--mock-call-process 0 json-output))
              ((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'switch-to-buffer) (lambda (buf) buf)))
      (beads-restore "bd-42")
      (should (get-buffer "*beads-restore: bd-42*"))
      (with-current-buffer "*beads-restore: bd-42*"
        (should (eq major-mode 'beads-restore-mode))
        (should (string= beads-restore--issue-id "bd-42"))
        (should beads-restore--issue-data)
        (should (string-match-p "Test Compacted Issue"
                               (buffer-string)))))))

(ert-deftest beads-restore-test-restore-failure ()
  "Test restore operation handles errors."
  (let ((beads-executable "bd"))
    (cl-letf (((symbol-function 'call-process)
               (beads-restore-test--mock-call-process 1
                "Error: Issue is not compacted"))
              ((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'switch-to-buffer) (lambda (buf) buf)))
      (beads-restore "bd-42")
      (should (get-buffer "*beads-restore: bd-42*"))
      (with-current-buffer "*beads-restore: bd-42*"
        (let ((content (buffer-string)))
          (should (string-match-p "Error restoring issue" content))
          (should (string-match-p "not compacted" content)))))))

(ert-deftest beads-restore-test-restore-buffer-name ()
  "Test that restore creates buffer with correct name."
  (let* ((json-output (json-encode
                       beads-restore-test--sample-compacted-issue))
         (beads-executable "bd"))
    (cl-letf (((symbol-function 'call-process)
               (beads-restore-test--mock-call-process 0 json-output))
              ((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'switch-to-buffer) (lambda (buf) buf)))
      (beads-restore "bd-42")
      (should (get-buffer "*beads-restore: bd-42*")))))

(ert-deftest beads-restore-test-restore-preserves-project-dir ()
  "Test that restore preserves project directory."
  (let* ((json-output (json-encode
                       beads-restore-test--sample-compacted-issue))
         (beads-executable "bd")
         (test-dir "/tmp/test-project/"))
    (cl-letf (((symbol-function 'call-process)
               (beads-restore-test--mock-call-process 0 json-output))
              ((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'switch-to-buffer) (lambda (buf) buf)))
      (let ((default-directory test-dir))
        (beads-restore "bd-42")
        (with-current-buffer "*beads-restore: bd-42*"
          (should (string= default-directory test-dir)))))))

(ert-deftest beads-restore-test-restore-read-only ()
  "Test that restore buffer is read-only."
  (let* ((json-output (json-encode
                       beads-restore-test--sample-compacted-issue))
         (beads-executable "bd"))
    (cl-letf (((symbol-function 'call-process)
               (beads-restore-test--mock-call-process 0 json-output))
              ((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'switch-to-buffer) (lambda (buf) buf)))
      (beads-restore "bd-42")
      (with-current-buffer "*beads-restore: bd-42*"
        (should buffer-read-only)))))

;;; ============================================================
;;; Refresh Tests
;;; ============================================================

(ert-deftest beads-restore-test-refresh-success ()
  "Test successful refresh operation."
  (let* ((json-output (json-encode
                       beads-restore-test--sample-compacted-issue))
         (beads-executable "bd"))
    (cl-letf (((symbol-function 'call-process)
               (beads-restore-test--mock-call-process 0 json-output))
              ((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'switch-to-buffer) (lambda (buf) buf)))
      (beads-restore "bd-42")
      (with-current-buffer "*beads-restore: bd-42*"
        (goto-char (point-max))
        (let ((pos (point)))
          (beads-refresh-restore)
          ;; Position should be preserved (or clamped to max)
          (should (<= (point) pos)))))))

(ert-deftest beads-restore-test-refresh-not-in-restore-mode ()
  "Test refresh errors when not in restore mode."
  (with-temp-buffer
    (should-error (beads-refresh-restore)
                  :type 'user-error)))

(ert-deftest beads-restore-test-refresh-no-issue-id ()
  "Test refresh errors when issue ID is missing."
  (with-temp-buffer
    (beads-restore-mode)
    (should-error (beads-refresh-restore)
                  :type 'user-error)))

(ert-deftest beads-restore-test-refresh-failure ()
  "Test refresh handles errors gracefully."
  (let* ((json-output (json-encode
                       beads-restore-test--sample-compacted-issue))
         (beads-executable "bd")
         (call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args)
                 (setq call-count (1+ call-count))
                 (if (= call-count 1)
                     (with-current-buffer (current-buffer)
                       (insert json-output)
                       0)
                   1)))  ; Second call fails
              ((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'switch-to-buffer) (lambda (buf) buf)))
      (beads-restore "bd-42")
      (with-current-buffer "*beads-restore: bd-42*"
        ;; Refresh should not signal error, just return nil and message
        (let ((result (beads-refresh-restore)))
          (should (null result)))
        ;; Buffer should still exist with original content
        (should (string-match-p "Test Compacted Issue"
                               (buffer-string)))))))

;;; ============================================================
;;; Mode Tests
;;; ============================================================

(ert-deftest beads-restore-test-mode-definition ()
  "Test that beads-restore-mode is defined."
  (should (fboundp 'beads-restore-mode)))

(ert-deftest beads-restore-test-mode-is-special-mode ()
  "Test that beads-restore-mode derives from special-mode."
  (with-temp-buffer
    (beads-restore-mode)
    (should (derived-mode-p 'special-mode))))

(ert-deftest beads-restore-test-mode-keybindings ()
  "Test that restore mode has proper keybindings."
  (with-temp-buffer
    (beads-restore-mode)
    (should (local-key-binding (kbd "q")))
    (should (local-key-binding (kbd "g")))
    (should (eq (local-key-binding (kbd "g")) 'beads-refresh-restore))))

(ert-deftest beads-restore-test-mode-buffer-read-only ()
  "Test that restore mode sets buffer as read-only."
  (with-temp-buffer
    (beads-restore-mode)
    (should buffer-read-only)))

;;; ============================================================
;;; Autoload Tests
;;; ============================================================

(ert-deftest beads-restore-test-restore-function-defined ()
  "Test that beads-restore function is defined."
  (should (fboundp 'beads-restore)))

(ert-deftest beads-restore-test-refresh-function-defined ()
  "Test that beads-refresh-restore function is defined."
  (should (fboundp 'beads-refresh-restore)))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-restore-test-multiple-restores ()
  "Test opening multiple restore buffers."
  (let* ((json-output-1 (json-encode
                         beads-restore-test--sample-compacted-issue))
         (json-output-2 (json-encode
                         beads-restore-test--sample-issue-no-events))
         (beads-executable "bd")
         (call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args)
                 (setq call-count (1+ call-count))
                 (with-current-buffer (current-buffer)
                   (insert (if (= call-count 1)
                              json-output-1
                            json-output-2)))
                 0))
              ((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'switch-to-buffer) (lambda (buf) buf)))
      (beads-restore "bd-42")
      (beads-restore "bd-43")
      (should (get-buffer "*beads-restore: bd-42*"))
      (should (get-buffer "*beads-restore: bd-43*"))
      (should (= call-count 2)))))

(ert-deftest beads-restore-test-restore-with-minimal-issue ()
  "Test restoring issue with minimal fields."
  (let* ((minimal-issue '((id . "bd-1")
                         (title . "Minimal")
                         (status . "open")))
         (json-output (json-encode minimal-issue))
         (beads-executable "bd"))
    (cl-letf (((symbol-function 'call-process)
               (beads-restore-test--mock-call-process 0 json-output))
              ((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'switch-to-buffer) (lambda (buf) buf)))
      (beads-restore "bd-1")
      (with-current-buffer "*beads-restore: bd-1*"
        (should (string-match-p "Minimal" (buffer-string)))
        (should (string-match-p "OPEN" (buffer-string)))))))

(ert-deftest beads-restore-test-insert-header ()
  "Test insert-header helper function."
  (with-temp-buffer
    (beads-restore--insert-header "Label" "Value")
    (should (string-match-p "Label: Value" (buffer-string)))))

(ert-deftest beads-restore-test-insert-header-nil-value ()
  "Test insert-header with nil value."
  (with-temp-buffer
    (beads-restore--insert-header "Label" nil)
    (should (string-match-p "Label:" (buffer-string)))))

(ert-deftest beads-restore-test-insert-section ()
  "Test insert-section helper function."
  (with-temp-buffer
    (beads-restore--insert-section "Section Title" "Section content here")
    (let ((content (buffer-string)))
      (should (string-match-p "Section Title" content))
      (should (string-match-p "Section content here" content))
      (should (string-match-p "─" content)))))  ; Underline

(ert-deftest beads-restore-test-insert-section-nil-content ()
  "Test insert-section skips nil content."
  (with-temp-buffer
    (beads-restore--insert-section "Section Title" nil)
    (should (string= "" (buffer-string)))))

(ert-deftest beads-restore-test-insert-section-empty-content ()
  "Test insert-section skips empty content."
  (with-temp-buffer
    (beads-restore--insert-section "Section Title" "")
    (should (string= "" (buffer-string)))))

(ert-deftest beads-restore-test-insert-section-whitespace-content ()
  "Test insert-section skips whitespace-only content."
  (with-temp-buffer
    (beads-restore--insert-section "Section Title" "   \n\t  ")
    (should (string= "" (buffer-string)))))

(provide 'beads-restore-test)
;;; beads-restore-test.el ends here
