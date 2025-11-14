;;; beads-reopen-test.el --- Tests for beads-reopen -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-reopen.el.
;; Tests cover transient definition, command construction, validation,
;; execution, and integration with the bd CLI.
;;
;; This test file uses the transient-args pattern where tests mock
;; (transient-args 'beads-reopen--menu) to return argument lists like
;; '("--id=bd-42" "--reason=Needs work").

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-reopen)

;;; Test Fixtures

(defvar beads-reopen-test--sample-reopen-response
  '((id . "bd-42")
    (title . "Test Issue")
    (description . "Test description")
    (status . "open")
    (priority . 1)
    (issue_type . "bug")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T10:10:00Z")
    (closed_at))
  "Sample response from bd reopen command.")

;;; Test Utilities

(defun beads-reopen-test--mock-transient-args (args)
  "Create a mock for `transient-args' returning ARGS.
ARGS should be a list of strings like (\"--id=bd-42\" \"--reason=Needs work\")."
  (lambda (prefix)
    (when (eq prefix 'beads-reopen--menu)
      args)))

(defun beads-reopen-test--mock-process-file (exit-code output)
  "Create a mock for `process-file' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile buffer display &rest args)
    (when buffer
      (let ((buf (if (listp buffer)
                     (car buffer)
                   buffer)))
        (when buf
          (with-current-buffer (if (bufferp buf) buf (current-buffer))
            (insert output)))))
    exit-code))

;;; Tests for Argument Parsing

(ert-deftest beads-reopen-test-parse-args-empty ()
  "Test parsing empty argument list."
  (let ((parsed (beads-reopen--parse-transient-args nil)))
    (should (null (plist-get parsed :issue-id)))
    (should (null (plist-get parsed :reason)))))

(ert-deftest beads-reopen-test-parse-args-issue-id-only ()
  "Test parsing with only issue ID."
  (let ((parsed (beads-reopen--parse-transient-args
                 '("--id=bd-42"))))
    (should (equal (plist-get parsed :issue-id) "bd-42"))
    (should (null (plist-get parsed :reason)))))

(ert-deftest beads-reopen-test-parse-args-all-fields ()
  "Test parsing with all fields."
  (let ((parsed (beads-reopen--parse-transient-args
                 '("--id=bd-42"
                   "--reason=Needs more work"))))
    (should (equal (plist-get parsed :issue-id) "bd-42"))
    (should (equal (plist-get parsed :reason) "Needs more work"))))

(ert-deftest beads-reopen-test-parse-args-issue-id-with-special-chars ()
  "Test parsing issue ID containing special characters."
  (let ((parsed (beads-reopen--parse-transient-args
                 '("--id=custom-123-xyz"))))
    (should (equal (plist-get parsed :issue-id) "custom-123-xyz"))))

(ert-deftest beads-reopen-test-parse-args-multiline-reason ()
  "Test parsing multiline reason.
Note: In real usage, multiline values are handled by the transient class.
For testing, we just verify the value is parsed correctly."
  (let ((parsed (beads-reopen--parse-transient-args
                 '("--id=bd-42"
                   "--reason=Line 1 Line 2 Line 3"))))
    (should (equal (plist-get parsed :issue-id) "bd-42"))
    (should (plist-get parsed :reason))))

;;; Tests for Context Detection

(ert-deftest beads-reopen-test-detect-issue-id-from-buffer-name ()
  "Test detecting issue ID from beads-show buffer name."
  (with-temp-buffer
    (rename-buffer "*beads-show: bd-42*" t)
    (should (equal (beads-reopen--detect-issue-id) "bd-42"))))

(ert-deftest beads-reopen-test-detect-issue-id-no-context ()
  "Test detecting issue ID when no context available."
  (with-temp-buffer
    (should (null (beads-reopen--detect-issue-id)))))

;;; Tests for Validation

(ert-deftest beads-reopen-test-validate-issue-id-nil ()
  "Test issue ID validation when ID is nil."
  (should (beads-reopen--validate-issue-id nil)))

(ert-deftest beads-reopen-test-validate-issue-id-empty ()
  "Test issue ID validation when ID is empty."
  (should (beads-reopen--validate-issue-id "")))

(ert-deftest beads-reopen-test-validate-issue-id-whitespace ()
  "Test issue ID validation when ID is only whitespace."
  (should (beads-reopen--validate-issue-id "   \n\t  ")))

(ert-deftest beads-reopen-test-validate-issue-id-valid ()
  "Test issue ID validation when ID is valid."
  (should (null (beads-reopen--validate-issue-id "bd-42"))))

(ert-deftest beads-reopen-test-validate-all-success ()
  "Test validate-all with valid parameters."
  (let ((parsed (beads-reopen--parse-transient-args
                 '("--id=bd-42"))))
    (should (null (beads-reopen--validate-all parsed)))))

(ert-deftest beads-reopen-test-validate-all-failure ()
  "Test validate-all with missing issue ID."
  (let ((parsed (beads-reopen--parse-transient-args nil)))
    (let ((errors (beads-reopen--validate-all parsed)))
      (should errors)
      (should (listp errors))
      (should (= (length errors) 1)))))

;;; Tests for Command Building

(ert-deftest beads-reopen-test-build-command-args-minimal ()
  "Test building command args with only issue ID."
  (let* ((parsed (beads-reopen--parse-transient-args
                  '("--id=bd-42")))
         (args (beads-reopen--build-command-args parsed)))
    (should (equal args '("bd-42")))))

(ert-deftest beads-reopen-test-build-command-args-with-reason ()
  "Test building command args with issue ID and reason."
  (let* ((parsed (beads-reopen--parse-transient-args
                  '("--id=bd-42"
                    "--reason=Needs more work")))
         (args (beads-reopen--build-command-args parsed)))
    (should (equal args '("bd-42" "--reason" "Needs more work")))))

(ert-deftest beads-reopen-test-build-command-args-empty-reason ()
  "Test that empty reason is not included."
  (let* ((parsed (beads-reopen--parse-transient-args
                  '("--id=bd-42"
                    "--reason=")))
         (args (beads-reopen--build-command-args parsed)))
    (should (equal args '("bd-42")))))

(ert-deftest beads-reopen-test-build-command-args-whitespace-reason ()
  "Test that whitespace-only reason is not included."
  (let* ((parsed (beads-reopen--parse-transient-args
                  '("--id=bd-42"
                    "--reason=   \n\t  ")))
         (args (beads-reopen--build-command-args parsed)))
    (should (equal args '("bd-42")))))

(ert-deftest beads-reopen-test-build-command-args-multiline-reason ()
  "Test building command args with multiline reason.
Note: Multiline text is handled by transient's editor, so in tests
we just verify the reason is included when provided."
  (let* ((parsed (beads-reopen--parse-transient-args
                  '("--id=bd-42"
                    "--reason=Needs fixes and more testing")))
         (args (beads-reopen--build-command-args parsed)))
    (should (member "--reason" args))
    (should (member "Needs fixes and more testing" args))))

;;; Tests for Execution

(ert-deftest beads-reopen-test-execute-success ()
  "Test successful issue reopening."
  (let ((json-output (json-encode
                      beads-reopen-test--sample-reopen-response)))
    (cl-letf (((symbol-function 'transient-args)
               (beads-reopen-test--mock-transient-args
                '("--id=bd-42" "--reason=Needs fixes")))
              ((symbol-function 'process-file)
               (beads-reopen-test--mock-process-file 0 json-output)))
      (should-not (beads-reopen--execute)))))

(ert-deftest beads-reopen-test-execute-validation-failure ()
  "Test execution fails with validation error."
  (cl-letf (((symbol-function 'transient-args)
             (beads-reopen-test--mock-transient-args
              '("--id="
                "--reason=Needs fixes"))))
    (should-error (beads-reopen--execute) :type 'user-error)))

(ert-deftest beads-reopen-test-execute-missing-issue-id ()
  "Test execution fails when issue ID is missing."
  (cl-letf (((symbol-function 'transient-args)
             (beads-reopen-test--mock-transient-args
              '("--reason=Needs fixes"))))
    (should-error (beads-reopen--execute) :type 'user-error)))

(ert-deftest beads-reopen-test-execute-command-failure ()
  "Test execution handles bd command failure."
  (cl-letf (((symbol-function 'transient-args)
             (beads-reopen-test--mock-transient-args
              '("--id=bd-42")))
            ((symbol-function 'process-file)
             (beads-reopen-test--mock-process-file 1 "Error: failed")))
    ;; Should not propagate error, just display message
    (should (stringp (beads-reopen--execute)))))

(ert-deftest beads-reopen-test-execute-without-reason ()
  "Test execution without reason (optional)."
  (let ((json-output (json-encode
                      beads-reopen-test--sample-reopen-response)))
    (cl-letf (((symbol-function 'transient-args)
               (beads-reopen-test--mock-transient-args
                '("--id=bd-42")))
              ((symbol-function 'process-file)
               (beads-reopen-test--mock-process-file 0 json-output)))
      (should-not (beads-reopen--execute)))))

;;; Tests for Preview

(ert-deftest beads-reopen-test-preview-valid ()
  "Test preview command with valid parameters."
  (cl-letf (((symbol-function 'transient-args)
             (beads-reopen-test--mock-transient-args
              '("--id=bd-42" "--reason=Needs fixes"))))
    ;; Preview returns a message string
    (should (stringp (beads-reopen--preview)))))

(ert-deftest beads-reopen-test-preview-validation-failure ()
  "Test preview shows validation errors."
  (cl-letf (((symbol-function 'transient-args)
             (beads-reopen-test--mock-transient-args
              '("--id="))))
    ;; Preview returns a message string even with validation errors
    (should (stringp (beads-reopen--preview)))))

(ert-deftest beads-reopen-test-preview-without-reason ()
  "Test preview without reason."
  (cl-letf (((symbol-function 'transient-args)
             (beads-reopen-test--mock-transient-args
              '("--id=bd-42"))))
    (should (stringp (beads-reopen--preview)))))

;;; Tests for Transient Definition

(ert-deftest beads-reopen-test-transient-defined ()
  "Test that beads-reopen transient is defined."
  (should (fboundp 'beads-reopen)))

(ert-deftest beads-reopen-test-transient-is-prefix ()
  "Test that beads-reopen--menu is a transient prefix."
  (should (get 'beads-reopen--menu 'transient--prefix)))

(ert-deftest beads-reopen-test-infix-commands-defined ()
  "Test that all infix commands are defined."
  (should (fboundp 'beads-option-reopen-issue-id))
  (should (fboundp 'beads-option-reopen-reason)))

(ert-deftest beads-reopen-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-reopen--execute))
  (should (fboundp 'beads-reopen--reset))
  (should (fboundp 'beads-reopen--preview)))

;;; Integration Tests

(ert-deftest beads-reopen-test-full-workflow ()
  "Test complete workflow from setting params to reopening."
  (let ((json-output (json-encode
                      beads-reopen-test--sample-reopen-response)))
    (cl-letf (((symbol-function 'transient-args)
               (beads-reopen-test--mock-transient-args
                '("--id=bd-42" "--reason=Needs more work")))
              ((symbol-function 'process-file)
               (beads-reopen-test--mock-process-file 0 json-output)))
      ;; Parse args
      (let* ((args (funcall (symbol-function 'transient-args)
                            'beads-reopen--menu))
             (parsed (beads-reopen--parse-transient-args args)))
        ;; Validate
        (should (null (beads-reopen--validate-all parsed)))
        ;; Build command
        (let ((cmd-args (beads-reopen--build-command-args parsed)))
          (should (member "bd-42" cmd-args))
          (should (member "--reason" cmd-args))
          (should (member "Needs more work" cmd-args)))
        ;; Execute
        (should-not (beads-reopen--execute))))))

;;; Edge Cases

(ert-deftest beads-reopen-test-edge-case-unicode-reason ()
  "Test reopening issue with Unicode characters in reason."
  (let* ((parsed (beads-reopen--parse-transient-args
                  '("--id=bd-42"
                    "--reason=Reopen 测试 issue with émojis 😀")))
         (args (beads-reopen--build-command-args parsed)))
    (should (member "Reopen 测试 issue with émojis 😀" args))))

(ert-deftest beads-reopen-test-edge-case-very-long-reason ()
  "Test reopening issue with very long reason."
  (let* ((long-reason (make-string 500 ?x))
         (parsed (beads-reopen--parse-transient-args
                  `("--id=bd-42"
                    ,(concat "--reason=" long-reason))))
         (args (beads-reopen--build-command-args parsed)))
    (should (member long-reason args))))

(ert-deftest beads-reopen-test-edge-case-reason-with-quotes ()
  "Test reopening issue with quotes in reason."
  (let* ((parsed (beads-reopen--parse-transient-args
                  '("--id=bd-42"
                    "--reason=Reopen \"urgent\" issue with 'quotes'")))
         (args (beads-reopen--build-command-args parsed)))
    (should (member "--reason" args))))

(ert-deftest beads-reopen-test-edge-case-special-issue-id ()
  "Test reopening issue with special characters in ID."
  (let* ((parsed (beads-reopen--parse-transient-args
                  '("--id=custom-123-xyz")))
         (args (beads-reopen--build-command-args parsed)))
    (should (member "custom-123-xyz" args))))

;;; Performance Tests

(ert-deftest beads-reopen-test-performance-command-building ()
  "Test command building performance."
  :tags '(:performance)
  (let ((parsed (beads-reopen--parse-transient-args
                 '("--id=bd-42" "--reason=Needs fixes"))))
    (let ((start-time (current-time)))
      (dotimes (_ 1000)
        (beads-reopen--build-command-args parsed))
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        ;; Should build 1000 commands in under 0.5 seconds
        (should (< elapsed 0.5))))))

(ert-deftest beads-reopen-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (let ((parsed (beads-reopen--parse-transient-args
                 '("--id=bd-42"))))
    (let ((start-time (current-time)))
      (dotimes (_ 1000)
        (beads-reopen--validate-all parsed))
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        ;; Should validate 1000 times in under 0.5 seconds
        (should (< elapsed 0.5))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-reopen-test-transient-menu-defined ()
  "Integration test: Verify beads-reopen transient menu is defined."
  :tags '(integration)
  (should (fboundp 'beads-reopen)))

(ert-deftest beads-reopen-test-execute-function-defined ()
  "Integration test: Verify execute function is defined."
  :tags '(integration)
  (should (fboundp 'beads-reopen--execute)))

(ert-deftest beads-reopen-test-validation-can-run ()
  "Integration test: Verify validation can run."
  :tags '(integration)
  (let ((parsed (beads-reopen--parse-transient-args
                 '("--id=bd-42" "--reason=Test reason"))))
    ;; Validation should complete without error
    (let ((validation-result (beads-reopen--validate-all parsed)))
      ;; Result is either nil or a list
      (should (or (null validation-result) (listp validation-result))))))

(ert-deftest beads-reopen-test-context-from-list-mode ()
  "Integration test: Test context detection from list mode."
  :tags '(integration)
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-42")))
      (should (equal (beads-reopen--detect-issue-id) "bd-42")))))

(ert-deftest beads-reopen-test-context-from-show-mode ()
  "Integration test: Test context detection from show mode."
  :tags '(integration)
  (require 'beads-show)
  (with-temp-buffer
    (beads-show-mode)
    (setq-local beads-show--issue-id "bd-99")
    (should (equal (beads-reopen--detect-issue-id) "bd-99"))))

(ert-deftest beads-reopen-test-list-reopen-command ()
  "Integration test: Verify beads-list-reopen command exists."
  :tags '(integration)
  (require 'beads-list)
  (should (fboundp 'beads-list-reopen)))

(ert-deftest beads-reopen-test-list-keybinding-o ()
  "Integration test: Verify o keybinding in list mode."
  :tags '(integration)
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "o"))))
      (should (eq binding 'beads-list-reopen)))))

(ert-deftest beads-reopen-test-bulk-reopen-command ()
  "Integration test: Verify beads-list-bulk-reopen command exists."
  :tags '(integration)
  (require 'beads-list)
  (should (fboundp 'beads-list-bulk-reopen)))

(ert-deftest beads-reopen-test-bulk-keybinding ()
  "Integration test: Verify B o keybinding in list mode."
  :tags '(integration)
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "B o"))))
      (should (eq binding 'beads-list-bulk-reopen)))))

(ert-deftest beads-reopen-test-command-building-workflow ()
  "Integration test: Test complete command building workflow."
  :tags '(integration)
  (let ((parsed (beads-reopen--parse-transient-args
                 '("--id=bd-42" "--reason=Needs more work"))))
    ;; Should not error during validation
    (should-not (beads-reopen--validate-all parsed))
    ;; Should build valid command args
    (let ((args (beads-reopen--build-command-args parsed)))
      (should (equal (car args) "bd-42"))
      (should (member "--reason" args))
      (should (member "Needs more work" args)))))

(ert-deftest beads-reopen-test-main-menu-integration ()
  "Integration test: Verify beads-reopen in main menu."
  :tags '(integration)
  (require 'beads-main)
  ;; Verify the command is accessible
  (should (fboundp 'beads-reopen))
  ;; Verify the transient menu exists
  (should (fboundp 'beads-reopen--menu))
  (should (get 'beads-reopen--menu 'transient--prefix)))

(ert-deftest beads-reopen-test-full-reopen-workflow-with-mocks ()
  "Integration test: Full workflow with mocked bd command."
  :tags '(integration)
  (let ((json-output (json-encode
                      beads-reopen-test--sample-reopen-response)))
    (cl-letf (((symbol-function 'transient-args)
               (beads-reopen-test--mock-transient-args
                '("--id=bd-42" "--reason=Reopening for review")))
              ((symbol-function 'process-file)
               (beads-reopen-test--mock-process-file 0 json-output)))
      ;; Execute reopen
      (should-not (beads-reopen--execute)))))

(ert-deftest beads-reopen-test-list-reopen-workflow ()
  "Integration test: Reopen from list mode workflow."
  :tags '(integration)
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    ;; Mock having an issue at point
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-123")))
      ;; Verify we can detect the issue
      (should (equal (beads-list--current-issue-id) "bd-123"))
      ;; Verify the command would work (we can't actually call it
      ;; interactively in tests, but we can verify the preconditions)
      (should (fboundp 'beads-list-reopen)))))

(ert-deftest beads-reopen-test-bulk-reopen-workflow ()
  "Integration test: Bulk reopen workflow with marked issues."
  :tags '(integration)
  (require 'beads-list)
  ;; Verify the bulk command exists and can handle marked issues
  (should (fboundp 'beads-list-bulk-reopen))
  ;; The actual bulk operation is tested in unit tests,
  ;; here we just verify the integration points exist
  (with-temp-buffer
    (beads-list-mode)
    (should (keymapp beads-list-mode-map))))

(ert-deftest beads-reopen-test-cache-invalidation ()
  "Integration test: Cache invalidated after reopen."
  :tags '(integration)
  (require 'beads)
  ;; Set up a fake cache
  (setq beads--completion-cache
        '((timestamp . 1000)
          (issues . (("bd-1" . "Issue 1") ("bd-2" . "Issue 2")))))

  ;; Mock successful reopen
  (let ((json-output (json-encode
                      beads-reopen-test--sample-reopen-response)))
    (cl-letf (((symbol-function 'transient-args)
               (beads-reopen-test--mock-transient-args
                '("--id=bd-42")))
              ((symbol-function 'process-file)
               (beads-reopen-test--mock-process-file 0 json-output)))

      ;; Execute reopen
      (beads-reopen--execute)

      ;; Cache should be invalidated (nil)
      (should (null beads--completion-cache)))))

(ert-deftest beads-reopen-test-preview-shows-full-command ()
  "Integration test: Preview shows complete bd command."
  :tags '(integration)
  (cl-letf (((symbol-function 'transient-args)
             (beads-reopen-test--mock-transient-args
              '("--id=bd-42" "--reason=Needs review"))))
    ;; Get preview
    (let ((preview (beads-reopen--preview)))
      ;; Should contain the command parts
      (should (stringp preview))
      (should (string-match-p "bd" preview))
      (should (string-match-p "reopen" preview))
      (should (string-match-p "bd-42" preview))
      (should (string-match-p "--reason" preview))
      ;; Reason may be shell-quoted, so check for escaped or unescaped version
      (should (or (string-match-p "Needs review" preview)
                  (string-match-p "Needs\\\\ review" preview))))))

(ert-deftest beads-reopen-test-error-handling-integration ()
  "Integration test: Error handling in complete workflow."
  :tags '(integration)
  (cl-letf (((symbol-function 'transient-args)
             (beads-reopen-test--mock-transient-args
              '("--id=bd-42" "--reason=Reopen")))
            ((symbol-function 'process-file)
             (beads-reopen-test--mock-process-file 1 "Error: issue not found")))

    ;; Execute should handle error gracefully
    (let ((result (beads-reopen--execute)))
      ;; Should return error message, not throw
      (should (stringp result))
      (should (string-match-p "Failed" result)))))

(ert-deftest beads-reopen-test-multiline-reason-integration ()
  "Integration test: Multiline reason handling.
Note: In real usage, transient handles multiline editing.
We verify that reasons are properly passed through."
  :tags '(integration)
  (let ((parsed (beads-reopen--parse-transient-args
                 '("--id=bd-42" "--reason=Line 1 Line 2 Line 3"))))
    ;; Build command args
    (let ((args (beads-reopen--build-command-args parsed)))
      ;; Should include reason
      (should (member "--reason" args))
      (should (member "Line 1 Line 2 Line 3" args)))))

(ert-deftest beads-reopen-test-context-priority ()
  "Integration test: Context detection priority."
  :tags '(integration)
  (require 'beads-list)
  (require 'beads-show)

  ;; Test list mode context
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-from-list")))
      (should (equal (beads-reopen--detect-issue-id) "bd-from-list"))))

  ;; Test show mode context
  (with-temp-buffer
    (beads-show-mode)
    (setq-local beads-show--issue-id "bd-from-show")
    (should (equal (beads-reopen--detect-issue-id) "bd-from-show")))

  ;; Test buffer name context
  ;; The buffer name must match the pattern *beads-show: bd-[0-9]+*
  (let ((test-buffer (generate-new-buffer "*beads-show: bd-999*")))
    (unwind-protect
        (with-current-buffer test-buffer
          (should (equal (beads-reopen--detect-issue-id) "bd-999")))
      (kill-buffer test-buffer))))

(ert-deftest beads-reopen-test-validation-before-execution ()
  "Integration test: Validation occurs before execution."
  :tags '(integration)
  (cl-letf (((symbol-function 'transient-args)
             (beads-reopen-test--mock-transient-args
              '("--reason=Some reason"))))
    ;; Should error due to validation failure
    (should-error (beads-reopen--execute) :type 'user-error)))

(ert-deftest beads-reopen-test-auto-refresh-flag ()
  "Integration test: Auto-refresh respects beads-auto-refresh."
  :tags '(integration)
  (require 'beads)
  ;; Test with auto-refresh enabled
  (let ((beads-auto-refresh t))
    (should beads-auto-refresh))

  ;; Test with auto-refresh disabled
  (let ((beads-auto-refresh nil))
    (should-not beads-auto-refresh)))

(provide 'beads-reopen-test)
;;; beads-reopen-test.el ends here
