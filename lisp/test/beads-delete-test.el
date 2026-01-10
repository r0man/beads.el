;;; beads-delete-test.el --- Tests for beads-delete -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-delete.el.
;; Tests the preview-based deletion workflow.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-buffer-name)
(require 'beads-delete)
(require 'beads-test)

;;; Test Fixtures

(defvar beads-delete-test--sample-issue
  '((id . "bd-42")
    (title . "Test Issue")
    (status . "open")
    (priority . 1))
  "Sample issue for testing.")

(defvar beads-delete-test--preview-output
  "Issue: bd-42 - Test Issue

Dependencies to be removed:
  bd-42 blocks bd-43
  bd-41 blocks bd-42

Text references to be updated:
  bd-41: Will update reference to [deleted:bd-42]
  bd-43: Will update reference to [deleted:bd-42]
"
  "Sample preview output.")

;;; ============================================================
;;; Preview Tests
;;; ============================================================

(ert-deftest beads-delete-test-get-preview-success ()
  "Test getting deletion preview successfully."
  (cl-letf (((symbol-function 'process-file)
             (beads-test--mock-call-process
              0 beads-delete-test--preview-output)))
    (let ((preview (beads-delete--get-preview "bd-42")))
      (should (stringp preview))
      (should (string-match-p "bd-42" preview)))))

(ert-deftest beads-delete-test-get-preview-failure ()
  "Test getting preview handles bd command failure."
  (cl-letf (((symbol-function 'process-file)
             (beads-test--mock-call-process 1 "Error: issue not found")))
    (should-error (beads-delete--get-preview "bd-999"))))

(ert-deftest beads-delete-test-show-preview-creates-buffer ()
  "Test that show-preview creates a buffer with preview text."
  (let* ((issue-id "bd-42")
         (preview-text "Test preview content")
         (buffer (beads-delete--show-preview issue-id preview-text)))
    (unwind-protect
        (progn
          (should (bufferp buffer))
          (should (string-match-p issue-id (buffer-name buffer)))
          (with-current-buffer buffer
            (should (string-match-p preview-text (buffer-string)))
            (should buffer-read-only)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest beads-delete-test-show-preview-content-only ()
  "Test that preview buffer shows only bd command output."
  (let* ((buffer (beads-delete--show-preview
                  "bd-42" "Test content from bd")))
    (unwind-protect
        (with-current-buffer buffer
          (should (string= "Test content from bd" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; ============================================================
;;; Execution Tests
;;; ============================================================

(ert-deftest beads-delete-test-execute-deletion-success ()
  "Test successful deletion execution."
  (let ((json-output (json-encode '((id . "bd-42")
                                    (deleted . t)))))
    (cl-letf (((symbol-function 'process-file)
               (beads-test--mock-call-process 0 json-output))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (let ((result (beads-delete--execute-deletion "bd-42")))
        (should result)))))

(ert-deftest beads-delete-test-execute-deletion-command-failure ()
  "Test deletion handles bd command failure."
  (cl-letf (((symbol-function 'process-file)
             (beads-test--mock-call-process 1 "Error")))
    (should-error (beads-delete--execute-deletion "bd-42"))))

(ert-deftest beads-delete-test-execute-invalidates-cache ()
  "Test that deletion invalidates completion cache."
  (let ((json-output (json-encode '((id . "bd-42") (deleted . t))))
        (cache-invalidated nil))
    (cl-letf (((symbol-function 'process-file)
               (beads-test--mock-call-process 0 json-output))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () (setq cache-invalidated t))))
      (beads-delete--execute-deletion "bd-42")
      (should cache-invalidated))))

(ert-deftest beads-delete-test-execute-closes-show-buffer ()
  "Test that deletion closes the show buffer for deleted issue."
  (let ((json-output (json-encode '((id . "bd-42") (deleted . t)))))
    ;; Mock git functions for consistent buffer naming
    (cl-letf (((symbol-function 'beads-git-get-project-name)
               (lambda () "test-proj"))
              ((symbol-function 'beads-git-in-worktree-p)
               (lambda () nil)))
      ;; Create a show buffer with correct naming
      (let ((show-buf-name (beads-buffer-name-show "bd-42" "Test Title")))
        (with-current-buffer (get-buffer-create show-buf-name)
          (special-mode))
        (cl-letf (((symbol-function 'process-file)
                   (beads-test--mock-call-process 0 json-output))
                  ((symbol-function 'beads--invalidate-completion-cache)
                   (lambda () nil)))
          (beads-delete--execute-deletion "bd-42")
          (should-not (get-buffer show-buf-name)))))))

(ert-deftest beads-delete-test-execute-closes-preview-buffer ()
  "Test that deletion closes the preview buffer."
  (let ((json-output (json-encode '((id . "bd-42") (deleted . t)))))
    ;; Mock git functions for consistent buffer naming
    (cl-letf (((symbol-function 'beads-git-get-project-name)
               (lambda () "test-proj"))
              ((symbol-function 'beads-git-in-worktree-p)
               (lambda () nil)))
      ;; Create a preview buffer with correct naming
      (let ((preview-buf-name (beads-buffer-name-utility "delete-preview"
                                                         "bd-42")))
        (with-current-buffer (get-buffer-create preview-buf-name)
          (special-mode))
        (cl-letf (((symbol-function 'process-file)
                   (beads-test--mock-call-process 0 json-output))
                  ((symbol-function 'beads--invalidate-completion-cache)
                   (lambda () nil)))
          (beads-delete--execute-deletion "bd-42")
          (should-not (get-buffer preview-buf-name)))))))

;;; ============================================================
;;; Main Command Tests
;;; ============================================================

(ert-deftest beads-delete-test-command-defined ()
  "Test that beads-delete function is defined."
  (should (fboundp 'beads-delete)))

;;; ============================================================
;;; Context Detection Tests
;;; ============================================================

(ert-deftest beads-delete-test-detect-issue-id-from-list-mode ()
  "Test detecting issue ID from beads-list-mode."
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-42")))
      (should (equal (beads-delete--detect-issue-id) "bd-42")))))

(ert-deftest beads-delete-test-detect-issue-id-from-show-mode ()
  "Test detecting issue ID from beads-show-mode."
  (require 'beads-show)
  (with-temp-buffer
    (beads-show-mode)
    (setq-local beads-show--issue-id "bd-99")
    (should (equal (beads-delete--detect-issue-id) "bd-99"))))

(ert-deftest beads-delete-test-detect-issue-id-from-buffer-name ()
  "Test detecting issue ID from buffer name."
  (with-current-buffer (get-buffer-create "*beads-show: bd-123*")
    (should (equal (beads-delete--detect-issue-id) "bd-123"))
    (kill-buffer)))

(ert-deftest beads-delete-test-detect-issue-id-no-context ()
  "Test detecting issue ID when no context available."
  (with-temp-buffer
    (should (null (beads-delete--detect-issue-id)))))

;;; ============================================================
;;; List View Integration Tests
;;; ============================================================

(ert-deftest beads-delete-test-list-delete-function-defined ()
  "Test that beads-list-delete function is defined."
  (require 'beads-list)
  (should (fboundp 'beads-list-delete)))

(ert-deftest beads-delete-test-list-delete-with-issue ()
  "Test deleting from list view with issue at point."
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (let ((beads-delete-called nil)
          (beads-delete-called-with nil))
      (cl-letf (((symbol-function 'beads-list--current-issue-id)
                 (lambda () "bd-42"))
                ((symbol-function 'beads-delete)
                 (lambda (id)
                   (setq beads-delete-called t)
                   (setq beads-delete-called-with id))))
        (beads-list-delete)
        (should beads-delete-called)
        (should (equal beads-delete-called-with "bd-42"))))))

(ert-deftest beads-delete-test-list-delete-no-issue ()
  "Test deleting from list view with no issue at point."
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () nil)))
      (should-error (beads-list-delete) :type 'user-error))))

(ert-deftest beads-delete-test-list-keybinding-exists ()
  "Test that D keybinding exists in beads-list-mode-map."
  (require 'beads-list)
  (let ((binding (lookup-key beads-list-mode-map (kbd "D"))))
    (should (eq binding 'beads-list-delete))))

(ert-deftest beads-delete-test-list-delete-passes-id ()
  "Test that beads-list-delete passes the issue ID to beads-delete."
  (require 'beads-list)
  (let ((received-id nil))
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "test-123"))
              ((symbol-function 'beads-delete)
               (lambda (id) (setq received-id id))))
      (beads-list-delete)
      (should (equal received-id "test-123")))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(ert-deftest beads-delete-test-different-issue-id-formats ()
  "Test deletion with different issue ID formats."
  (let ((issue-ids '("bd-1" "bd-999" "worker-42" "test-123")))
    (dolist (id issue-ids)
      (let ((json-output (json-encode (list (cons 'id id)
                                            (cons 'deleted t)))))
        (cl-letf (((symbol-function 'process-file)
                   (beads-test--mock-call-process 0 json-output))
                  ((symbol-function 'beads--invalidate-completion-cache)
                   (lambda () nil)))
          (should (beads-delete--execute-deletion id)))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-delete-test-keybinding-exists ()
  "Integration test: Verify D keybinding exists in list mode."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "D"))))
      (should (eq binding 'beads-list-delete)))))

(ert-deftest beads-delete-test-full-workflow-with-confirmation ()
  "Integration test: Full delete workflow with user confirmation.
Tests the complete flow: preview -> confirm -> execute -> cleanup."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (require 'beads-test)
  (beads-test-with-project ()
    ;; First create an issue to delete
    (let* ((issue (beads-command-create! :title "Issue to Delete"
                                         :issue-type "task"))
           (issue-id (oref issue id))
           (preview-buffer-name (format "*beads-delete-preview: %s*" issue-id))
           (preview-shown nil))
      ;; Track cache invalidation and user interaction
      (let ((result
             (beads-test-with-cache-tracking
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (prompt)
                            (should (string-match-p issue-id prompt))
                            t))  ; Confirm deletion
                         ((symbol-function 'pop-to-buffer)
                          (lambda (buffer)
                            (setq preview-shown t)
                            (should (bufferp buffer))
                            (should (string-match-p issue-id
                                                    (buffer-name buffer)))
                            nil)))
                 ;; Execute delete with confirmation
                 (beads-delete issue-id)))))

        ;; Verify preview was shown
        (should preview-shown)

        ;; Verify cache was invalidated
        (should (plist-get result :completion-cache-invalidated))

        ;; Verify issue no longer exists
        (let ((issues (beads-command-list!)))
          (should-not (seq-find
                       (lambda (i) (equal (oref i id) issue-id))
                       issues)))

        ;; Verify preview buffer was cleaned up
        (should-not (get-buffer preview-buffer-name))))))

(ert-deftest beads-delete-test-full-workflow-user-cancels ()
  "Integration test: Full delete workflow when user cancels.
Tests that issue is NOT deleted when user says no to confirmation."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (require 'beads-test)
  (beads-test-with-project ()
    ;; First create an issue
    (let* ((issue (beads-command-create! :title "Issue Not to Delete"
                                         :issue-type "task"))
           (issue-id (oref issue id))
           (preview-shown nil))
      ;; Try to delete but cancel
      (let ((result
             (beads-test-with-cache-tracking
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (prompt)
                            (should (string-match-p issue-id prompt))
                            nil))  ; Cancel deletion
                         ((symbol-function 'pop-to-buffer)
                          (lambda (buffer)
                            (setq preview-shown t)
                            (should (bufferp buffer))
                            (should (string-match-p issue-id
                                                    (buffer-name buffer)))
                            nil)))
                 (beads-delete issue-id)))))

        ;; Verify preview was shown even though user cancelled
        (should preview-shown)

        ;; Verify cache was NOT invalidated (no deletion occurred)
        (should-not (plist-get result :completion-cache-invalidated))

        ;; Verify issue still exists
        (let* ((issues (beads-command-list!))
               (found (seq-find
                       (lambda (i) (equal (oref i id) issue-id))
                       issues)))
          (should found)
          (should (equal (oref found title) "Issue Not to Delete")))))))

(ert-deftest beads-delete-test-context-from-list-buffer ()
  "Integration test: Delete from beads-list-mode buffer.
Tests that delete detects issue ID from list buffer context."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (require 'beads-test)
  (beads-test-with-project ()
    ;; Create an issue
    (let* ((issue (beads-command-create! :title "List Context Delete"
                                         :issue-type "task"))
           (issue-id (oref issue id)))
      ;; Simulate being in a list buffer
      (with-temp-buffer
        (beads-list-mode)
        (cl-letf (((symbol-function 'beads-list--current-issue-id)
                   (lambda () issue-id))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (_) t))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer) nil)))
          ;; Delete should use detected ID
          (beads-list-delete))

        ;; Verify issue was deleted
        (let ((issues (beads-command-list!)))
          (should-not (seq-find
                       (lambda (i) (equal (oref i id) issue-id))
                       issues)))))))

(ert-deftest beads-delete-test-context-from-show-buffer ()
  "Integration test: Delete from beads-show-mode buffer.
Tests that delete detects issue ID from show buffer context."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (require 'beads-test)
  (beads-test-with-project ()
    ;; Create an issue
    (let* ((issue (beads-command-create! :title "Show Context Delete"
                                         :issue-type "task"))
           (issue-id (oref issue id)))
      ;; Simulate being in a show buffer
      (with-temp-buffer
        (beads-show-mode)
        (setq-local beads-show--issue-id issue-id)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (_) t))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer) nil)))
          ;; Verify context detection works
          (should (equal (beads-delete--detect-issue-id) issue-id))
          ;; Delete using the detected ID directly (not interactively)
          (beads-delete issue-id))

        ;; Verify issue was deleted
        (let ((issues (beads-command-list!)))
          (should-not (seq-find
                       (lambda (i) (equal (oref i id) issue-id))
                       issues)))))))

(ert-deftest beads-delete-test-preview-buffer-displayed ()
  "Integration test: Verify preview buffer is displayed before confirmation.
Tests that preview content is shown to user."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (require 'beads-test)
  (beads-test-with-project ()
    (let* ((issue (beads-command-create! :title "Preview Test Issue"))
           (issue-id (oref issue id))
           (preview-buffer-shown nil))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer)
                   (setq preview-buffer-shown t)
                   (should (bufferp buffer))
                   (should (string-match-p issue-id (buffer-name buffer)))))
                ((symbol-function 'yes-or-no-p)
                 (lambda (_) t)))
        (beads-delete issue-id)
        (should preview-buffer-shown)))))

(ert-deftest beads-delete-test-cleanup-buffers-after-deletion ()
  "Integration test: Verify buffers are cleaned up after deletion.
Tests that show and preview buffers are killed."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (require 'beads-test)
  (beads-test-with-project ()
    (let* ((issue (beads-command-create! :title "Cleanup Test"))
           (issue-id (oref issue id))
           ;; Get buffer names using centralized naming
           (show-buf-name (beads-buffer-name-show issue-id "Cleanup Test"))
           (preview-buf-name (beads-buffer-name-utility "delete-preview"
                                                        issue-id)))
      ;; Create show buffer manually with correct naming
      (with-current-buffer (get-buffer-create show-buf-name)
        (special-mode))

      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_) t))
                ((symbol-function 'pop-to-buffer)
                 (lambda (buffer) nil)))
        (beads-delete issue-id))

      ;; Verify buffers were killed
      (should-not (get-buffer show-buf-name))
      (should-not (get-buffer preview-buf-name)))))

;;; ============================================================
;;; Error Recovery Tests
;;; ============================================================

(ert-deftest beads-delete-test-error-recovery-preview-failure ()
  "Error recovery test: Preview command fails.
Tests graceful error handling when preview command fails."
  :tags '(:integration :error-recovery)
  (require 'beads-test)
  (cl-letf (((symbol-function 'process-file)
             (beads-test--mock-call-process 1 "Error: issue not found")))
    (should-error (beads-delete--get-preview "bd-nonexistent"))))

(ert-deftest beads-delete-test-error-recovery-executable-not-found ()
  "Error recovery test: bd executable not found.
Tests error handling when bd command is not in PATH."
  :tags '(:integration :error-recovery)
  (require 'beads-test)
  (cl-letf (((symbol-function 'beads-check-executable)
             (lambda () (error "bd executable not found"))))
    (should-error (beads-delete "bd-42"))))

(ert-deftest beads-delete-test-error-recovery-permission-denied ()
  "Error recovery test: Permission denied during deletion.
Tests handling when user lacks permissions."
  :tags '(:integration :error-recovery)
  (require 'beads-test)
  (cl-letf (((symbol-function 'process-file)
             (beads-test--mock-call-process
              1 "Error: permission denied")))
    (should-error (beads-delete--execute-deletion "bd-42"))))

(ert-deftest beads-delete-test-error-recovery-database-locked ()
  "Error recovery test: Database locked during deletion.
Tests handling when database is locked."
  :tags '(:integration :error-recovery)
  (require 'beads-test)
  (cl-letf (((symbol-function 'process-file)
             (beads-test--mock-call-process
              1 "Error: database is locked")))
    (should-error (beads-delete--execute-deletion "bd-42"))))

(ert-deftest beads-delete-test-error-recovery-issue-not-found ()
  "Error recovery test: Issue not found.
Tests handling when trying to delete non-existent issue."
  :tags '(:integration :error-recovery)
  (require 'beads-test)
  (cl-letf (((symbol-function 'process-file)
             (beads-test--mock-call-process
              1 "Error: issue not found")))
    (should-error (beads-delete--get-preview "bd-nonexistent"))))

(ert-deftest beads-delete-test-error-recovery-invalid-json-response ()
  "Error recovery test: bd returns invalid JSON.
Tests handling when deletion returns malformed JSON."
  :tags '(:integration :error-recovery)
  (require 'beads-test)
  (cl-letf (((symbol-function 'process-file)
             (beads-test--mock-call-process 0 "not valid json")))
    (should-error (beads-delete--execute-deletion "bd-42"))))

(ert-deftest beads-delete-test-error-recovery-cache-not-invalidated-on-error ()
  "Error recovery test: Cache not invalidated on error.
Tests that cache is only invalidated on successful deletion."
  :tags '(:integration :error-recovery)
  (require 'beads-test)
  (let ((cache-invalidated nil))
    (cl-letf (((symbol-function 'process-file)
               (beads-test--mock-call-process 1 "Error"))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () (setq cache-invalidated t))))
      (should-error (beads-delete--execute-deletion "bd-42"))
      ;; Cache should not be invalidated on error
      (should-not cache-invalidated))))

(ert-deftest beads-delete-test-error-recovery-no-database ()
  "Error recovery test: No .beads directory.
Tests handling when beads is not initialized."
  :tags '(:integration :error-recovery)
  (require 'beads-test)
  (cl-letf (((symbol-function 'beads--get-database-path)
             (lambda () nil))
            ((symbol-function 'process-file)
             (beads-test--mock-call-process
              1 "Error: no .beads directory found")))
    (should-error (beads-delete--get-preview "bd-42"))))

;;; ============================================================
;;; Interactive Command Tests
;;; ============================================================

(ert-deftest beads-delete-test-interactive-with-confirmation ()
  "Test beads-delete interactive command when user confirms."
  :tags '(:unit)
  (let ((deleted nil)
        (cache-invalidated nil))
    (cl-letf (((symbol-function 'beads-check-executable)
               (lambda () nil))
              ((symbol-function 'beads-delete--get-preview)
               (lambda (_id) "Preview text"))
              ((symbol-function 'beads-delete--show-preview)
               (lambda (_id _text)
                 (let ((buf (generate-new-buffer "*test-preview*")))
                   (with-current-buffer buf
                     (insert "Preview"))
                   buf)))
              ((symbol-function 'pop-to-buffer)
               (lambda (_buf) nil))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t))
              ((symbol-function 'beads-delete--execute-deletion)
               (lambda (_id) (setq deleted t)))
              ((symbol-function 'buffer-live-p)
               (lambda (_buf) nil)))
      (beads-delete "bd-42")
      (should deleted))))

(ert-deftest beads-delete-test-interactive-user-declines ()
  "Test beads-delete when user declines confirmation."
  :tags '(:unit)
  (let ((deleted nil))
    (cl-letf (((symbol-function 'beads-check-executable)
               (lambda () nil))
              ((symbol-function 'beads-delete--get-preview)
               (lambda (_id) "Preview text"))
              ((symbol-function 'beads-delete--show-preview)
               (lambda (_id _text)
                 (let ((buf (generate-new-buffer "*test-preview*")))
                   (with-current-buffer buf
                     (insert "Preview"))
                   buf)))
              ((symbol-function 'pop-to-buffer)
               (lambda (_buf) nil))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) nil))  ; User says no
              ((symbol-function 'beads-delete--execute-deletion)
               (lambda (_id) (setq deleted t)))
              ((symbol-function 'buffer-live-p)
               (lambda (_buf) nil)))
      (beads-delete "bd-42")
      (should-not deleted))))

(ert-deftest beads-delete-test-interactive-prompts-for-id ()
  "Test beads-delete prompts for issue ID when not provided."
  :tags '(:unit)
  (let ((deleted nil)
        (read-issue-called nil))
    (cl-letf (((symbol-function 'beads-check-executable)
               (lambda () nil))
              ((symbol-function 'beads-delete--detect-issue-id)
               (lambda () nil))
              ((symbol-function 'beads-completion-read-issue)
               (lambda (&rest _args)
                 (setq read-issue-called t)
                 "bd-42"))
              ((symbol-function 'beads-delete--get-preview)
               (lambda (_id) "Preview text"))
              ((symbol-function 'beads-delete--show-preview)
               (lambda (_id _text)
                 (let ((buf (generate-new-buffer "*test-preview*")))
                   buf)))
              ((symbol-function 'pop-to-buffer)
               (lambda (_buf) nil))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t))
              ((symbol-function 'beads-delete--execute-deletion)
               (lambda (_id) (setq deleted t)))
              ((symbol-function 'buffer-live-p)
               (lambda (_buf) nil)))
      (beads-delete nil)
      (should read-issue-called)
      (should deleted))))

(ert-deftest beads-delete-test-interactive-preview-error ()
  "Test beads-delete handles preview errors gracefully."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'beads-check-executable)
               (lambda () nil))
              ((symbol-function 'beads-delete--get-preview)
               (lambda (_id) (error "Preview failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-delete "bd-42")
      (should message-output)
      (should (string-match-p "Failed" message-output)))))

(ert-deftest beads-delete-test-get-preview-with-actor ()
  "Test beads-delete--get-preview includes actor when set."
  :tags '(:unit)
  (let ((beads-actor "testuser")
        (args-used nil))
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _infile _buffer _display &rest args)
                 (setq args-used args)
                 0)))
      (beads-delete--get-preview "bd-42")
      (should (member "--actor" args-used))
      (should (member "testuser" args-used)))))

;;; Full Flow Tests with Mocking

(ert-deftest beads-delete-test-full-flow-user-confirms ()
  "Test beads-delete when user confirms deletion."
  :tags '(:unit)
  (let ((deleted-id nil)
        (preview-buffer nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda () nil))
              ((symbol-function 'beads-delete--get-preview)
               (lambda (_id) "Preview text"))
              ((symbol-function 'beads-delete--show-preview)
               (lambda (_id _text)
                 (setq preview-buffer (get-buffer-create "*test-preview*"))
                 preview-buffer))
              ((symbol-function 'pop-to-buffer) (lambda (_buf) nil))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'beads-delete--execute-deletion)
               (lambda (id) (setq deleted-id id))))
      (beads-delete "bd-42")
      (should (equal deleted-id "bd-42"))
      ;; Clean up
      (when (buffer-live-p preview-buffer)
        (kill-buffer preview-buffer)))))

(ert-deftest beads-delete-test-full-flow-user-cancels ()
  "Test beads-delete when user cancels deletion."
  :tags '(:unit)
  (let ((deleted-id nil)
        (preview-buffer nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda () nil))
              ((symbol-function 'beads-delete--get-preview)
               (lambda (_id) "Preview text"))
              ((symbol-function 'beads-delete--show-preview)
               (lambda (_id _text)
                 (setq preview-buffer (get-buffer-create "*test-preview*"))
                 preview-buffer))
              ((symbol-function 'pop-to-buffer) (lambda (_buf) nil))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) nil))
              ((symbol-function 'beads-delete--execute-deletion)
               (lambda (id) (setq deleted-id id))))
      (beads-delete "bd-42")
      ;; Should NOT have called execute-deletion
      (should (null deleted-id))
      ;; Clean up
      (when (buffer-live-p preview-buffer)
        (kill-buffer preview-buffer)))))

(ert-deftest beads-delete-test-full-flow-preview-error ()
  "Test beads-delete when preview fails."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda () nil))
              ((symbol-function 'beads-delete--get-preview)
               (lambda (_id) (error "Preview failed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-delete "bd-42")
      (should message-output)
      (should (string-match-p "Failed" message-output)))))

(ert-deftest beads-delete-test-nil-issue-id-prompts-for-id ()
  "Test beads-delete prompts for issue ID when nil is passed."
  :tags '(:unit)
  (let ((deleted-id nil)
        (preview-buffer nil)
        (completion-called nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda () nil))
              ((symbol-function 'beads-completion-read-issue)
               (lambda (&rest _args)
                 (setq completion-called t)
                 "bd-prompted"))
              ((symbol-function 'beads-delete--get-preview)
               (lambda (_id) "Preview text"))
              ((symbol-function 'beads-delete--show-preview)
               (lambda (_id _text)
                 (setq preview-buffer (get-buffer-create "*test-preview*"))
                 preview-buffer))
              ((symbol-function 'pop-to-buffer) (lambda (_buf) nil))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'beads-delete--execute-deletion)
               (lambda (id) (setq deleted-id id))))
      (beads-delete nil)
      (should completion-called)
      (should (equal deleted-id "bd-prompted"))
      (when (buffer-live-p preview-buffer)
        (kill-buffer preview-buffer)))))

(ert-deftest beads-delete-test-buffer-killed-after-confirm ()
  "Test that preview buffer is killed after confirmation."
  :tags '(:unit)
  (let ((preview-buffer nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda () nil))
              ((symbol-function 'beads-delete--get-preview)
               (lambda (_id) "Preview text"))
              ((symbol-function 'beads-delete--show-preview)
               (lambda (_id _text)
                 (setq preview-buffer (get-buffer-create "*test-preview*"))
                 preview-buffer))
              ((symbol-function 'pop-to-buffer) (lambda (_buf) nil))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'beads-delete--execute-deletion)
               (lambda (_id) nil)))
      (beads-delete "bd-42")
      ;; Buffer should be killed by unwind-protect
      (should-not (buffer-live-p preview-buffer)))))

(ert-deftest beads-delete-test-buffer-killed-after-cancel ()
  "Test that preview buffer is killed even when user cancels."
  :tags '(:unit)
  (let ((preview-buffer nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda () nil))
              ((symbol-function 'beads-delete--get-preview)
               (lambda (_id) "Preview text"))
              ((symbol-function 'beads-delete--show-preview)
               (lambda (_id _text)
                 (setq preview-buffer (get-buffer-create "*test-preview*"))
                 preview-buffer))
              ((symbol-function 'pop-to-buffer) (lambda (_buf) nil))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
      (beads-delete "bd-42")
      ;; Buffer should be killed by unwind-protect even on cancel
      (should-not (buffer-live-p preview-buffer)))))

(provide 'beads-delete-test)
;;; beads-delete-test.el ends here
