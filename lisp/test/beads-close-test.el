;;; beads-close-test.el --- Tests for beads-close -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-close.el using transient-args pattern.
;; Tests cover parsing, validation, command construction, and execution.

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

(defun beads-close-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; Tests for Argument Parsing

(ert-deftest beads-close-test-parse-args-empty ()
  "Test parsing empty arguments."
  (let ((cmd (beads-close--parse-transient-args '())))
    (should (beads-command-close-p cmd))
    (should (null (oref cmd issue-ids)))
    (should (null (oref cmd reason)))))

(ert-deftest beads-close-test-parse-args-issue-id ()
  "Test parsing issue ID argument."
  (let ((cmd (beads-close--parse-transient-args '("--id=bd-42"))))
    (should (beads-command-close-p cmd))
    (should (equal (oref cmd issue-ids) '("bd-42")))
    (should (null (oref cmd reason)))))

(ert-deftest beads-close-test-parse-args-reason ()
  "Test parsing reason argument."
  (let ((cmd (beads-close--parse-transient-args
              '("--id=bd-42" "--reason=Fixed the bug"))))
    (should (beads-command-close-p cmd))
    (should (equal (oref cmd issue-ids) '("bd-42")))
    (should (equal (oref cmd reason) "Fixed the bug"))))


;;; Tests for Context Detection

(ert-deftest beads-close-test-detect-issue-id-from-buffer-name ()
  "Test detecting issue ID from beads-show buffer name."
  (with-temp-buffer
    (rename-buffer "*beads-show[proj]/bd-42*" t)
    (should (equal (beads-close--detect-issue-id) "bd-42"))))

(ert-deftest beads-close-test-detect-issue-id-no-context ()
  "Test detecting issue ID when no context available."
  (with-temp-buffer
    (should (null (beads-close--detect-issue-id)))))

;;; Tests for Validation

(ert-deftest beads-close-test-validate-issue-id-nil ()
  "Test issue ID validation when ID is nil."
  (let ((parsed (list :issue-id nil :reason nil)))
    (should (beads-close--validate-issue-id (plist-get parsed :issue-id)))))

(ert-deftest beads-close-test-validate-issue-id-empty ()
  "Test issue ID validation when ID is empty."
  (let ((parsed (list :issue-id "" :reason nil)))
    (should (beads-close--validate-issue-id (plist-get parsed :issue-id)))))

(ert-deftest beads-close-test-validate-issue-id-whitespace ()
  "Test issue ID validation when ID is only whitespace."
  (let ((parsed (list :issue-id "   \n\t  " :reason nil)))
    (should (beads-close--validate-issue-id (plist-get parsed :issue-id)))))

(ert-deftest beads-close-test-validate-issue-id-valid ()
  "Test issue ID validation when ID is valid."
  (let ((parsed (list :issue-id "bd-42" :reason nil)))
    (should (null (beads-close--validate-issue-id
                   (plist-get parsed :issue-id))))))

(ert-deftest beads-close-test-validate-all-success ()
  "Test validate-all with valid parameters."
  (let ((parsed (beads-close--parse-transient-args
                 '("--id=bd-42" "--reason=Fixed"))))
    (should (null (beads-close--validate-all parsed)))))

(ert-deftest beads-close-test-validate-all-failure ()
  "Test validate-all with missing issue ID and reason."
  (let ((parsed (beads-close--parse-transient-args '())))
    (let ((errors (beads-close--validate-all parsed)))
      (should errors)
      (should (listp errors))
      (should (= (length errors) 2)))))


;;; Tests for Execution

(ert-deftest beads-close-test-execute-success ()
  "Test successful issue closing."
  ;; bd close returns an array in JSON mode, even for a single issue
  (let ((json-output (json-encode (vector beads-close-test--sample-close-response))))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--id=bd-42" "--reason=Fixed")))
              ((symbol-function 'call-process)
               (beads-close-test--mock-call-process 0 json-output)))
      (should-not (beads-close--execute)))))

(ert-deftest beads-close-test-execute-validation-failure ()
  "Test execution fails with validation error."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--id="))))
    (should-error (beads-close--execute) :type 'user-error)))

(ert-deftest beads-close-test-execute-missing-issue-id ()
  "Test execution fails when issue ID is missing."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--reason=Fixed"))))
    (should-error (beads-close--execute) :type 'user-error)))

(ert-deftest beads-close-test-execute-command-failure ()
  "Test execution handles bd command failure."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--id=bd-42" "--reason=Fixed")))
            ((symbol-function 'call-process)
             (beads-close-test--mock-call-process 1 "Error: failed")))
    ;; Should not propagate error, just display message
    (should (stringp (beads-close--execute)))))

(ert-deftest beads-close-test-execute-without-reason ()
  "Test execution without reason fails with validation error."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--id=bd-42"))))
    ;; Should fail with user-error because reason is required
    (should-error (beads-close--execute) :type 'user-error)))

;;; Tests for Preview

(ert-deftest beads-close-test-preview-valid ()
  "Test preview command with valid parameters."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--id=bd-42" "--reason=Fixed"))))
    ;; Preview returns a message string
    (should (stringp (beads-close--preview)))))

(ert-deftest beads-close-test-preview-validation-failure ()
  "Test preview shows validation errors."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--id="))))
    ;; Preview returns a message string even with validation errors
    (should (stringp (beads-close--preview)))))

(ert-deftest beads-close-test-preview-without-reason ()
  "Test preview without reason."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--id=bd-42"))))
    (should (stringp (beads-close--preview)))))

;;; Tests for Transient Definition

(ert-deftest beads-close-test-transient-defined ()
  "Test that beads-close transient is defined."
  (should (fboundp 'beads-close)))

(ert-deftest beads-close-test-transient-is-prefix ()
  "Test that beads-close--menu is a transient prefix."
  (should (get 'beads-close--menu 'transient--prefix)))

(ert-deftest beads-close-test-infix-commands-defined ()
  "Test that all infix commands are defined."
  (should (fboundp 'beads-option-close-issue-id))
  (should (fboundp 'beads-option-close-reason)))

(ert-deftest beads-close-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-close--execute))
  (should (fboundp 'beads-close--reset))
  (should (fboundp 'beads-close--preview)))

;;; Integration Tests

(ert-deftest beads-close-test-full-workflow ()
  "Test complete workflow from setting params to closing."
  (let ((cmd (beads-close--parse-transient-args
              '("--id=bd-42" "--reason=Completed successfully"))))
    ;; Validate
    (should (null (beads-close--validate-all cmd)))

    ;; Execute (mocked)
    ;; bd close returns an array in JSON mode, even for a single issue
    (let ((json-output (json-encode (vector beads-close-test--sample-close-response))))
      (cl-letf (((symbol-function 'transient-args)
                 (lambda (_prefix)
                   '("--id=bd-42" "--reason=Completed successfully")))
                ((symbol-function 'call-process)
                 (beads-close-test--mock-call-process 0 json-output)))
        (should-not (beads-close--execute))))))

(ert-deftest beads-close-test-context-from-list-mode ()
  "Integration test: Test context detection from list mode."
  :tags '(integration)
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-42")))
      (should (equal (beads-close--detect-issue-id) "bd-42")))))

(ert-deftest beads-close-test-context-from-show-mode ()
  "Integration test: Test context detection from show mode."
  :tags '(integration)
  (require 'beads-show)
  (with-temp-buffer
    (beads-show-mode)
    (setq-local beads-show--issue-id "bd-99")
    (should (equal (beads-close--detect-issue-id) "bd-99"))))


(ert-deftest beads-close-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (let ((cmd (beads-close--parse-transient-args '("--id=bd-42" "--reason=Fixed")))
        (start-time (current-time)))
    (dotimes (_ 1000)
      (beads-close--validate-all cmd))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should validate 1000 times in under 0.5 seconds
      (should (< elapsed 0.5)))))

;;; Tests for Reset Function

(ert-deftest beads-close-test-reset-confirmed ()
  "Test reset when user confirms."
  (let ((reset-called nil)
        (message-output nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t)))
              ((symbol-function 'transient--redisplay)
               (lambda ()))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-close--reset)
      (should reset-called)
      (should (string-match-p "reset" message-output)))))

(ert-deftest beads-close-test-reset-declined ()
  "Test reset when user declines."
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t))))
      (beads-close--reset)
      (should-not reset-called))))

;;; Main Command Tests

(ert-deftest beads-close-test-main-command-calls-menu ()
  "Test beads-close calls the transient menu."
  (let ((menu-called nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-close--menu)
               (lambda () (setq menu-called t))))
      (beads-close "bd-123")
      (should menu-called))))

(ert-deftest beads-close-test-main-command-ignores-issue-id ()
  "Test beads-close ignores issue-id argument (per design)."
  ;; The beads-close function explicitly ignores the issue-id argument
  ;; because users must manually enter it in the transient menu
  (let ((menu-called nil))
    (cl-letf (((symbol-function 'beads-check-executable) (lambda ()))
              ((symbol-function 'beads-close--menu)
               (lambda () (setq menu-called t))))
      ;; Call with nil - should still work
      (beads-close nil)
      (should menu-called))))

(provide 'beads-close-test)
;;; beads-close-test.el ends here
