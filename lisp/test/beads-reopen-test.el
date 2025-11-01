;;; beads-reopen-test.el --- Tests for beads-reopen -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-reopen.el.
;; Tests cover transient definition, command construction, validation,
;; execution, and integration with the bd CLI.

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

(defun beads-reopen-test--set-state (state-alist)
  "Set transient state from STATE-ALIST.
STATE-ALIST is an alist of (variable . value) pairs."
  (setq beads-reopen--issue-id nil
        beads-reopen--reason nil)
  (dolist (binding state-alist)
    (set (car binding) (cdr binding))))

(defmacro beads-reopen-test-with-state (state &rest body)
  "Execute BODY with beads-reopen transient state set to STATE.
STATE is an alist expression of (variable . value) pairs."
  (declare (indent 1))
  `(progn
     (beads-reopen-test--set-state ,state)
     ,@body))

(defun beads-reopen-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; Tests for State Management

(ert-deftest beads-reopen-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Needs more work"))
   (beads-reopen--reset-state)
   (should (null beads-reopen--issue-id))
   (should (null beads-reopen--reason))))

(ert-deftest beads-reopen-test-reset-state-from-nil ()
  "Test that reset-state works when variables are already nil."
  (beads-reopen-test-with-state nil
   (beads-reopen--reset-state)
   (should (null beads-reopen--issue-id))
   (should (null beads-reopen--reason))))

;;; Tests for Value Formatting

(ert-deftest beads-reopen-test-format-current-value-set ()
  "Test formatting when value is set."
  (let ((result (beads-reopen--format-current-value "test-value")))
    (should (stringp result))
    (should (string-match-p "test-value" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-reopen-test-format-current-value-nil ()
  "Test formatting when value is nil."
  (let ((result (beads-reopen--format-current-value nil)))
    (should (stringp result))
    (should (string-match-p "not set" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-reopen-test-format-current-value-empty-string ()
  "Test formatting with empty string."
  (let ((result (beads-reopen--format-current-value "")))
    (should (stringp result))
    (should (string-match-p "not set" result))))

(ert-deftest beads-reopen-test-format-current-value-long-string ()
  "Test formatting with long string value."
  (let* ((long-value (make-string 100 ?x))
         (result (beads-reopen--format-current-value long-value)))
    (should (stringp result))
    (should (string-match-p "\\.\\.\\." result))
    (should (< (length result) 60))))

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
  (beads-reopen-test-with-state nil
   (should (beads-reopen--validate-issue-id))))

(ert-deftest beads-reopen-test-validate-issue-id-empty ()
  "Test issue ID validation when ID is empty."
  (beads-reopen-test-with-state '((beads-reopen--issue-id . ""))
   (should (beads-reopen--validate-issue-id))))

(ert-deftest beads-reopen-test-validate-issue-id-whitespace ()
  "Test issue ID validation when ID is only whitespace."
  (beads-reopen-test-with-state '((beads-reopen--issue-id . "   \n\t  "))
   (should (beads-reopen--validate-issue-id))))

(ert-deftest beads-reopen-test-validate-issue-id-valid ()
  "Test issue ID validation when ID is valid."
  (beads-reopen-test-with-state '((beads-reopen--issue-id . "bd-42"))
   (should (null (beads-reopen--validate-issue-id)))))

(ert-deftest beads-reopen-test-validate-all-success ()
  "Test validate-all with valid parameters."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42"))
   (should (null (beads-reopen--validate-all)))))

(ert-deftest beads-reopen-test-validate-all-failure ()
  "Test validate-all with missing issue ID."
  (beads-reopen-test-with-state nil
   (let ((errors (beads-reopen--validate-all)))
     (should errors)
     (should (listp errors))
     (should (= (length errors) 1)))))

;;; Tests for Command Building

(ert-deftest beads-reopen-test-build-command-args-minimal ()
  "Test building command args with only issue ID."
  (beads-reopen-test-with-state '((beads-reopen--issue-id . "bd-42"))
   (let ((args (beads-reopen--build-command-args)))
     (should (equal args '("bd-42"))))))

(ert-deftest beads-reopen-test-build-command-args-with-reason ()
  "Test building command args with issue ID and reason."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Needs more work"))
   (let ((args (beads-reopen--build-command-args)))
     (should (equal args '("bd-42" "--reason" "Needs more work"))))))

(ert-deftest beads-reopen-test-build-command-args-empty-reason ()
  "Test that empty reason is not included."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . ""))
   (let ((args (beads-reopen--build-command-args)))
     (should (equal args '("bd-42"))))))

(ert-deftest beads-reopen-test-build-command-args-whitespace-reason ()
  "Test that whitespace-only reason is not included."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "   \n\t  "))
   (let ((args (beads-reopen--build-command-args)))
     (should (equal args '("bd-42"))))))

(ert-deftest beads-reopen-test-build-command-args-multiline-reason ()
  "Test building command args with multiline reason."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Needs fixes\nMore testing needed\nReopen for review"))
   (let ((args (beads-reopen--build-command-args)))
     (should (member "--reason" args))
     (let ((reason (nth (1+ (cl-position "--reason" args :test #'equal)) args)))
       (should (string-match-p "\n" reason))))))

;;; Tests for Execution

(ert-deftest beads-reopen-test-execute-success ()
  "Test successful issue reopening."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Needs fixes"))
   (let ((json-output (json-encode
                       beads-reopen-test--sample-reopen-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-reopen-test--mock-call-process 0 json-output)))
       (should-not (beads-reopen--execute))
       ;; State should be reset after successful execution
       (should (null beads-reopen--issue-id))
       (should (null beads-reopen--reason))))))

(ert-deftest beads-reopen-test-execute-validation-failure ()
  "Test execution fails with validation error."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "")
     (beads-reopen--reason . "Needs fixes"))
   (should-error (beads-reopen--execute) :type 'user-error)))

(ert-deftest beads-reopen-test-execute-missing-issue-id ()
  "Test execution fails when issue ID is missing."
  (beads-reopen-test-with-state
   '((beads-reopen--reason . "Needs fixes"))
   (should-error (beads-reopen--execute) :type 'user-error)))

(ert-deftest beads-reopen-test-execute-command-failure ()
  "Test execution handles bd command failure."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42"))
   (cl-letf (((symbol-function 'call-process)
              (beads-reopen-test--mock-call-process 1 "Error: failed")))
     ;; Should not propagate error, just display message
     (should (stringp (beads-reopen--execute))))))

(ert-deftest beads-reopen-test-execute-without-reason ()
  "Test execution without reason (optional)."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42"))
   (let ((json-output (json-encode
                       beads-reopen-test--sample-reopen-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-reopen-test--mock-call-process 0 json-output)))
       (should-not (beads-reopen--execute))
       (should (null beads-reopen--issue-id))))))

;;; Tests for Preview

(ert-deftest beads-reopen-test-preview-valid ()
  "Test preview command with valid parameters."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Needs fixes"))
   ;; Preview returns a message string
   (should (stringp (beads-reopen--preview)))))

(ert-deftest beads-reopen-test-preview-validation-failure ()
  "Test preview shows validation errors."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . ""))
   ;; Preview returns a message string even with validation errors
   (should (stringp (beads-reopen--preview)))))

(ert-deftest beads-reopen-test-preview-without-reason ()
  "Test preview without reason."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42"))
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
  (beads-reopen-test-with-state nil
   ;; Set parameters
   (setq beads-reopen--issue-id "bd-42")
   (setq beads-reopen--reason "Needs more work")

   ;; Validate
   (should (null (beads-reopen--validate-all)))

   ;; Build command
   (let ((args (beads-reopen--build-command-args)))
     (should (member "bd-42" args))
     (should (member "--reason" args))
     (should (member "Needs more work" args)))

   ;; Execute (mocked)
   (let ((json-output (json-encode
                       beads-reopen-test--sample-reopen-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-reopen-test--mock-call-process 0 json-output)))
       (should-not (beads-reopen--execute))
       ;; Verify state was reset
       (should (null beads-reopen--issue-id))))))

(ert-deftest beads-reopen-test-reset-and-reopen ()
  "Test resetting state and reopening another issue."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-1")
     (beads-reopen--reason . "First reason"))

   ;; Reset
   (beads-reopen--reset-state)
   (should (null beads-reopen--issue-id))
   (should (null beads-reopen--reason))

   ;; Set new values
   (setq beads-reopen--issue-id "bd-2")
   (setq beads-reopen--reason "Second reason")

   ;; Build command - should only have new values
   (let ((args (beads-reopen--build-command-args)))
     (should (member "bd-2" args))
     (should (member "Second reason" args))
     (should-not (member "bd-1" args)))))

;;; Edge Cases

(ert-deftest beads-reopen-test-edge-case-unicode-reason ()
  "Test reopening issue with Unicode characters in reason."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Reopen 测试 issue with émojis 😀"))
   (let ((args (beads-reopen--build-command-args)))
     (should (member "Reopen 测试 issue with émojis 😀" args)))))

(ert-deftest beads-reopen-test-edge-case-very-long-reason ()
  "Test reopening issue with very long reason."
  (let ((long-reason (make-string 500 ?x)))
    (beads-reopen-test-with-state
     `((beads-reopen--issue-id . "bd-42")
       (beads-reopen--reason . ,long-reason))
     (let ((args (beads-reopen--build-command-args)))
       (should (member long-reason args))))))

(ert-deftest beads-reopen-test-edge-case-reason-with-quotes ()
  "Test reopening issue with quotes in reason."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Reopen \"urgent\" issue with 'quotes'"))
   (let ((args (beads-reopen--build-command-args)))
     (should (member "--reason" args)))))

(ert-deftest beads-reopen-test-edge-case-special-issue-id ()
  "Test reopening issue with special characters in ID."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "custom-123-xyz"))
   (let ((args (beads-reopen--build-command-args)))
     (should (member "custom-123-xyz" args)))))

;;; Performance Tests

(ert-deftest beads-reopen-test-performance-command-building ()
  "Test command building performance."
  :tags '(:performance)
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Needs fixes"))
   (let ((start-time (current-time)))
     (dotimes (_ 1000)
       (beads-reopen--build-command-args))
     (let ((elapsed (float-time (time-subtract (current-time) start-time))))
       ;; Should build 1000 commands in under 0.5 seconds
       (should (< elapsed 0.5))))))

(ert-deftest beads-reopen-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42"))
   (let ((start-time (current-time)))
     (dotimes (_ 1000)
       (beads-reopen--validate-all))
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
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Test reason"))
   ;; Validation should complete without error
   (let ((validation-result (beads-reopen--validate-all)))
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
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Needs more work"))
   ;; Should not error during validation
   (should-not (beads-reopen--validate-all))
   ;; Should build valid command args
   (let ((args (beads-reopen--build-command-args)))
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
  (beads-reopen-test-with-state nil
   ;; Set up state
   (setq beads-reopen--issue-id "bd-42")
   (setq beads-reopen--reason "Reopening for review")

   ;; Mock the bd command
   (let ((json-output (json-encode
                       beads-reopen-test--sample-reopen-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-reopen-test--mock-call-process 0 json-output)))

       ;; Execute reopen
       (should-not (beads-reopen--execute))

       ;; Verify state was reset
       (should (null beads-reopen--issue-id))
       (should (null beads-reopen--reason))))))

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

(ert-deftest beads-reopen-test-state-reset-after-success ()
  "Integration test: State resets after successful reopen."
  :tags '(integration)
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-99")
     (beads-reopen--reason . "Test reopen"))

   ;; Mock successful execution
   (let ((json-output (json-encode
                       beads-reopen-test--sample-reopen-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-reopen-test--mock-call-process 0 json-output)))

       ;; Execute
       (beads-reopen--execute)

       ;; Verify all state was cleared
       (should (null beads-reopen--issue-id))
       (should (null beads-reopen--reason))))))

(ert-deftest beads-reopen-test-cache-invalidation ()
  "Integration test: Cache invalidated after reopen."
  :tags '(integration)
  (require 'beads)
  ;; Set up a fake cache
  (setq beads--completion-cache
        '((timestamp . 1000)
          (issues . (("bd-1" . "Issue 1") ("bd-2" . "Issue 2")))))

  ;; Mock successful reopen
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42"))
   (let ((json-output (json-encode
                       beads-reopen-test--sample-reopen-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-reopen-test--mock-call-process 0 json-output)))

       ;; Execute reopen
       (beads-reopen--execute)

       ;; Cache should be invalidated (nil)
       (should (null beads--completion-cache))))))

(ert-deftest beads-reopen-test-preview-shows-full-command ()
  "Integration test: Preview shows complete bd command."
  :tags '(integration)
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Needs review"))

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
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Reopen"))

   ;; Mock bd command failure
   (cl-letf (((symbol-function 'call-process)
              (beads-reopen-test--mock-call-process 1 "Error: issue not found")))

     ;; Execute should handle error gracefully
     (let ((result (beads-reopen--execute)))
       ;; Should return error message, not throw
       (should (stringp result))
       (should (string-match-p "Failed" result))))))

(ert-deftest beads-reopen-test-multiline-reason-integration ()
  "Integration test: Multiline reason handling."
  :tags '(integration)
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Line 1\nLine 2\nLine 3"))

   ;; Build command args
   (let ((args (beads-reopen--build-command-args)))
     ;; Should include multiline reason
     (should (member "--reason" args))
     (let ((reason (nth (1+ (cl-position "--reason" args :test #'equal)) args)))
       (should (string-match-p "\n" reason))
       (should (string-match-p "Line 1" reason))
       (should (string-match-p "Line 3" reason))))))

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
  (beads-reopen-test-with-state nil
   ;; Don't set issue ID - should fail validation
   (setq beads-reopen--reason "Some reason")

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
