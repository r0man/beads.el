;;; beads-transient-close-test.el --- Tests for beads-close -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-close.el.
;; Tests cover transient definition, command construction, validation,
;; execution, and integration with the bd CLI.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-close)

;;; Test Fixtures

(defvar beads-close-test--sample-close-response
  '((id . "bd-42")
    (title . "Test Issue")
    (description . "Test description")
    (status . "closed")
    (priority . 1)
    (issue_type . "bug")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T10:00:00Z")
    (closed_at . "2025-01-15T10:05:00Z"))
  "Sample response from bd close command.")

;;; Test Utilities

(defun beads-close-test--set-state (state-alist)
  "Set transient state from STATE-ALIST.
STATE-ALIST is an alist of (variable . value) pairs."
  (setq beads-close--issue-id nil
        beads-close--reason nil)
  (dolist (binding state-alist)
    (set (car binding) (cdr binding))))

(defmacro beads-close-test-with-state (state &rest body)
  "Execute BODY with beads-close transient state set to STATE.
STATE is an alist expression of (variable . value) pairs."
  (declare (indent 1))
  `(progn
     (beads-close-test--set-state ,state)
     ,@body))

(defun beads-close-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; Tests for State Management

(ert-deftest beads-close-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed"))
   (beads-close--reset-state)
   (should (null beads-close--issue-id))
   (should (null beads-close--reason))))

(ert-deftest beads-close-test-reset-state-from-nil ()
  "Test that reset-state works when variables are already nil."
  (beads-close-test-with-state nil
   (beads-close--reset-state)
   (should (null beads-close--issue-id))
   (should (null beads-close--reason))))

;;; Tests for Value Formatting

(ert-deftest beads-close-test-format-current-value-set ()
  "Test formatting when value is set."
  (let ((result (beads-close--format-current-value "test-value")))
    (should (stringp result))
    (should (string-match-p "test-value" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-close-test-format-current-value-nil ()
  "Test formatting when value is nil."
  (let ((result (beads-close--format-current-value nil)))
    (should (stringp result))
    (should (string-match-p "not set" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-close-test-format-current-value-empty-string ()
  "Test formatting with empty string."
  (let ((result (beads-close--format-current-value "")))
    (should (stringp result))
    (should (string-match-p "not set" result))))

(ert-deftest beads-close-test-format-current-value-long-string ()
  "Test formatting with long string value."
  (let* ((long-value (make-string 100 ?x))
         (result (beads-close--format-current-value long-value)))
    (should (stringp result))
    (should (string-match-p "\\.\\.\\." result))
    (should (< (length result) 60))))

;;; Tests for Context Detection

(ert-deftest beads-close-test-detect-issue-id-from-buffer-name ()
  "Test detecting issue ID from beads-show buffer name."
  (with-temp-buffer
    (rename-buffer "*beads-show: bd-42*" t)
    (should (equal (beads-close--detect-issue-id) "bd-42"))))

(ert-deftest beads-close-test-detect-issue-id-no-context ()
  "Test detecting issue ID when no context available."
  (with-temp-buffer
    (should (null (beads-close--detect-issue-id)))))

;;; Tests for Validation

(ert-deftest beads-close-test-validate-issue-id-nil ()
  "Test issue ID validation when ID is nil."
  (beads-close-test-with-state nil
   (should (beads-close--validate-issue-id))))

(ert-deftest beads-close-test-validate-issue-id-empty ()
  "Test issue ID validation when ID is empty."
  (beads-close-test-with-state '((beads-close--issue-id . ""))
   (should (beads-close--validate-issue-id))))

(ert-deftest beads-close-test-validate-issue-id-whitespace ()
  "Test issue ID validation when ID is only whitespace."
  (beads-close-test-with-state '((beads-close--issue-id . "   \n\t  "))
   (should (beads-close--validate-issue-id))))

(ert-deftest beads-close-test-validate-issue-id-valid ()
  "Test issue ID validation when ID is valid."
  (beads-close-test-with-state '((beads-close--issue-id . "bd-42"))
   (should (null (beads-close--validate-issue-id)))))

(ert-deftest beads-close-test-validate-all-success ()
  "Test validate-all with valid parameters."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42"))
   (should (null (beads-close--validate-all)))))

(ert-deftest beads-close-test-validate-all-failure ()
  "Test validate-all with missing issue ID."
  (beads-close-test-with-state nil
   (let ((errors (beads-close--validate-all)))
     (should errors)
     (should (listp errors))
     (should (= (length errors) 1)))))

;;; Tests for Command Building

(ert-deftest beads-close-test-build-command-args-minimal ()
  "Test building command args with only issue ID."
  (beads-close-test-with-state '((beads-close--issue-id . "bd-42"))
   (let ((args (beads-close--build-command-args)))
     (should (equal args '("bd-42"))))))

(ert-deftest beads-close-test-build-command-args-with-reason ()
  "Test building command args with issue ID and reason."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed the bug"))
   (let ((args (beads-close--build-command-args)))
     (should (equal args '("bd-42" "--reason" "Fixed the bug"))))))

(ert-deftest beads-close-test-build-command-args-empty-reason ()
  "Test that empty reason is not included."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . ""))
   (let ((args (beads-close--build-command-args)))
     (should (equal args '("bd-42"))))))

(ert-deftest beads-close-test-build-command-args-whitespace-reason ()
  "Test that whitespace-only reason is not included."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "   \n\t  "))
   (let ((args (beads-close--build-command-args)))
     (should (equal args '("bd-42"))))))

(ert-deftest beads-close-test-build-command-args-multiline-reason ()
  "Test building command args with multiline reason."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed the issue\nTested thoroughly\nReady to ship"))
   (let ((args (beads-close--build-command-args)))
     (should (member "--reason" args))
     (let ((reason (nth (1+ (cl-position "--reason" args :test #'equal)) args)))
       (should (string-match-p "\n" reason))))))

;;; Tests for Execution

(ert-deftest beads-close-test-execute-success ()
  "Test successful issue closing."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed"))
   (let ((json-output (json-encode
                       beads-close-test--sample-close-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-close-test--mock-call-process 0 json-output)))
       (should-not (beads-close--execute))
       ;; State should be reset after successful execution
       (should (null beads-close--issue-id))
       (should (null beads-close--reason))))))

(ert-deftest beads-close-test-execute-validation-failure ()
  "Test execution fails with validation error."
  (beads-close-test-with-state
   '((beads-close--issue-id . "")
     (beads-close--reason . "Fixed"))
   (should-error (beads-close--execute) :type 'user-error)))

(ert-deftest beads-close-test-execute-missing-issue-id ()
  "Test execution fails when issue ID is missing."
  (beads-close-test-with-state
   '((beads-close--reason . "Fixed"))
   (should-error (beads-close--execute) :type 'user-error)))

(ert-deftest beads-close-test-execute-command-failure ()
  "Test execution handles bd command failure."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42"))
   (cl-letf (((symbol-function 'call-process)
              (beads-close-test--mock-call-process 1 "Error: failed")))
     ;; Should not propagate error, just display message
     (should (stringp (beads-close--execute))))))

(ert-deftest beads-close-test-execute-without-reason ()
  "Test execution without reason (optional)."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42"))
   (let ((json-output (json-encode
                       beads-close-test--sample-close-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-close-test--mock-call-process 0 json-output)))
       (should-not (beads-close--execute))
       (should (null beads-close--issue-id))))))

;;; Tests for Preview

(ert-deftest beads-close-test-preview-valid ()
  "Test preview command with valid parameters."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed"))
   ;; Preview returns a message string
   (should (stringp (beads-close--preview)))))

(ert-deftest beads-close-test-preview-validation-failure ()
  "Test preview shows validation errors."
  (beads-close-test-with-state
   '((beads-close--issue-id . ""))
   ;; Preview returns a message string even with validation errors
   (should (stringp (beads-close--preview)))))

(ert-deftest beads-close-test-preview-without-reason ()
  "Test preview without reason."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42"))
   (should (stringp (beads-close--preview)))))

;;; Tests for Transient Definition

(ert-deftest beads-close-test-transient-defined ()
  "Test that beads-close transient is defined."
  (should (fboundp 'beads-close)))

(ert-deftest beads-close-test-transient-is-prefix ()
  "Test that beads-close is a transient prefix."
  (should (get 'beads-close 'transient--prefix)))

(ert-deftest beads-close-test-infix-commands-defined ()
  "Test that all infix commands are defined."
  (should (fboundp 'beads-close--infix-issue-id))
  (should (fboundp 'beads-close--infix-reason)))

(ert-deftest beads-close-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-close--execute))
  (should (fboundp 'beads-close--reset))
  (should (fboundp 'beads-close--preview)))

;;; Integration Tests

(ert-deftest beads-close-test-integration-full-workflow ()
  "Test complete workflow from setting params to closing."
  (beads-close-test-with-state nil
   ;; Set parameters
   (setq beads-close--issue-id "bd-42")
   (setq beads-close--reason "Completed successfully")

   ;; Validate
   (should (null (beads-close--validate-all)))

   ;; Build command
   (let ((args (beads-close--build-command-args)))
     (should (member "bd-42" args))
     (should (member "--reason" args))
     (should (member "Completed successfully" args)))

   ;; Execute (mocked)
   (let ((json-output (json-encode
                       beads-close-test--sample-close-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-close-test--mock-call-process 0 json-output)))
       (should-not (beads-close--execute))
       ;; Verify state was reset
       (should (null beads-close--issue-id))))))

(ert-deftest beads-close-test-integration-reset-and-reclose ()
  "Test resetting state and closing another issue."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-1")
     (beads-close--reason . "First reason"))

   ;; Reset
   (beads-close--reset-state)
   (should (null beads-close--issue-id))
   (should (null beads-close--reason))

   ;; Set new values
   (setq beads-close--issue-id "bd-2")
   (setq beads-close--reason "Second reason")

   ;; Build command - should only have new values
   (let ((args (beads-close--build-command-args)))
     (should (member "bd-2" args))
     (should (member "Second reason" args))
     (should-not (member "bd-1" args)))))

;;; Edge Cases

(ert-deftest beads-close-test-edge-case-unicode-reason ()
  "Test closing issue with Unicode characters in reason."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed 测试 issue with émojis 😀"))
   (let ((args (beads-close--build-command-args)))
     (should (member "Fixed 测试 issue with émojis 😀" args)))))

(ert-deftest beads-close-test-edge-case-very-long-reason ()
  "Test closing issue with very long reason."
  (let ((long-reason (make-string 500 ?x)))
    (beads-close-test-with-state
     `((beads-close--issue-id . "bd-42")
       (beads-close--reason . ,long-reason))
     (let ((args (beads-close--build-command-args)))
       (should (member long-reason args))))))

(ert-deftest beads-close-test-edge-case-reason-with-quotes ()
  "Test closing issue with quotes in reason."
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed \"critical\" issue with 'quotes'"))
   (let ((args (beads-close--build-command-args)))
     (should (member "--reason" args)))))

(ert-deftest beads-close-test-edge-case-special-issue-id ()
  "Test closing issue with special characters in ID."
  (beads-close-test-with-state
   '((beads-close--issue-id . "custom-123-xyz"))
   (let ((args (beads-close--build-command-args)))
     (should (member "custom-123-xyz" args)))))

;;; Performance Tests

(ert-deftest beads-close-test-performance-command-building ()
  "Test command building performance."
  :tags '(:performance)
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42")
     (beads-close--reason . "Fixed"))
   (let ((start-time (current-time)))
     (dotimes (_ 1000)
       (beads-close--build-command-args))
     (let ((elapsed (float-time (time-subtract (current-time) start-time))))
       ;; Should build 1000 commands in under 0.5 seconds
       (should (< elapsed 0.5))))))

(ert-deftest beads-close-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (beads-close-test-with-state
   '((beads-close--issue-id . "bd-42"))
   (let ((start-time (current-time)))
     (dotimes (_ 1000)
       (beads-close--validate-all))
     (let ((elapsed (float-time (time-subtract (current-time) start-time))))
       ;; Should validate 1000 times in under 0.5 seconds
       (should (< elapsed 0.5))))))

(provide 'beads-transient-close-test)
;;; beads-transient-close-test.el ends here
