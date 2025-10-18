;;; beads-transient-reopen-test.el --- Tests for beads-reopen -*- lexical-binding: t; -*-

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
    (updated_at . "2025-01-15T10:05:00Z")
    (closed_at . nil))
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
     (beads-reopen--reason . "Need to fix more issues"))
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
     (beads-reopen--reason . "Need to address more concerns"))
   (let ((args (beads-reopen--build-command-args)))
     (should (equal args '("bd-42" "--reason" "Need to address more concerns"))))))

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
     (beads-reopen--reason . "Reopening because:\nMore work needed\nAdditional requirements found"))
   (let ((args (beads-reopen--build-command-args)))
     (should (member "--reason" args))
     (let ((reason (nth (1+ (cl-position "--reason" args :test #'equal)) args)))
       (should (string-match-p "\n" reason))))))

;;; Tests for Execution

(ert-deftest beads-reopen-test-execute-success ()
  "Test successful issue reopening."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Need more work"))
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
     (beads-reopen--reason . "Reopening"))
   (should-error (beads-reopen--execute) :type 'user-error)))

(ert-deftest beads-reopen-test-execute-missing-issue-id ()
  "Test execution fails when issue ID is missing."
  (beads-reopen-test-with-state
   '((beads-reopen--reason . "Reopening"))
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
     (beads-reopen--reason . "Need more work"))
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
  "Test that beads-reopen is a transient prefix."
  (should (get 'beads-reopen 'transient--prefix)))

(ert-deftest beads-reopen-test-infix-commands-defined ()
  "Test that all infix commands are defined."
  (should (fboundp 'beads-reopen--infix-issue-id))
  (should (fboundp 'beads-reopen--infix-reason)))

(ert-deftest beads-reopen-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-reopen--execute))
  (should (fboundp 'beads-reopen--reset))
  (should (fboundp 'beads-reopen--preview)))

;;; Integration Tests

(ert-deftest beads-reopen-test-integration-full-workflow ()
  "Test complete workflow from setting params to reopening."
  (beads-reopen-test-with-state nil
   ;; Set parameters
   (setq beads-reopen--issue-id "bd-42")
   (setq beads-reopen--reason "Additional work required")

   ;; Validate
   (should (null (beads-reopen--validate-all)))

   ;; Build command
   (let ((args (beads-reopen--build-command-args)))
     (should (member "bd-42" args))
     (should (member "--reason" args))
     (should (member "Additional work required" args)))

   ;; Execute (mocked)
   (let ((json-output (json-encode
                       beads-reopen-test--sample-reopen-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-reopen-test--mock-call-process 0 json-output)))
       (should-not (beads-reopen--execute))
       ;; Verify state was reset
       (should (null beads-reopen--issue-id))))))

(ert-deftest beads-reopen-test-integration-reset-and-reopen ()
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
     (beads-reopen--reason . "Reopening 测试 issue with émojis 😀"))
   (let ((args (beads-reopen--build-command-args)))
     (should (member "Reopening 测试 issue with émojis 😀" args)))))

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
     (beads-reopen--reason . "Reopening \"critical\" issue with 'quotes'"))
   (let ((args (beads-reopen--build-command-args)))
     (should (member "--reason" args)))))

(ert-deftest beads-reopen-test-edge-case-special-issue-id ()
  "Test reopening issue with special characters in ID."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "custom-123-xyz"))
   (let ((args (beads-reopen--build-command-args)))
     (should (member "custom-123-xyz" args)))))

(ert-deftest beads-reopen-test-edge-case-custom-prefix ()
  "Test reopening issue with custom prefix in ID."
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "beads.el-30"))
   (let ((args (beads-reopen--build-command-args)))
     (should (member "beads.el-30" args)))))

;;; Performance Tests

(ert-deftest beads-reopen-test-performance-command-building ()
  "Test command building performance."
  :tags '(:performance)
  (beads-reopen-test-with-state
   '((beads-reopen--issue-id . "bd-42")
     (beads-reopen--reason . "Reopening"))
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

(provide 'beads-transient-reopen-test)
;;; beads-transient-reopen-test.el ends here
