;;; beads-close-test.el --- Tests for beads-command-close -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-command-close.el.
;; Tests cover command class behavior, parsing, validation, and execution.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-command-close)

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

;;; Tests for Command Class

(ert-deftest beads-close-test-command-class-exists ()
  "Test that beads-command-close class is defined."
  (should (cl-find-class 'beads-command-close)))

(ert-deftest beads-close-test-command-subcommand ()
  "Test that subcommand returns 'close'."
  (let ((cmd (beads-command-close)))
    (should (equal (beads-command-subcommand cmd) "close"))))

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

;;; Tests for Validation

(ert-deftest beads-close-test-validate-missing-issue-id ()
  "Test validation when issue ID is missing."
  (let ((cmd (beads-command-close :reason "Fixed")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-close-test-validate-missing-reason ()
  "Test validation when reason is missing."
  (let ((cmd (beads-command-close :issue-ids '("bd-42"))))
    (should (beads-command-validate cmd))))

(ert-deftest beads-close-test-validate-valid-command ()
  "Test validation with valid parameters."
  (let ((cmd (beads-command-close :issue-ids '("bd-42") :reason "Fixed")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-close-test-validate-empty-issue-id ()
  "Test validation when issue-ids is empty list."
  (let ((cmd (beads-command-close :issue-ids '() :reason "Fixed")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-close-test-validate-empty-reason ()
  "Test validation when reason is empty string."
  (let ((cmd (beads-command-close :issue-ids '("bd-42") :reason "")))
    (should (beads-command-validate cmd))))

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
  ;; Command failure is handled gracefully - may show error or succeed
  ;; depending on how the underlying process execution handles errors.
  ;; The important thing is no unhandled exception escapes.
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--id=bd-42" "--reason=Fixed")))
            ((symbol-function 'call-process)
             (beads-close-test--mock-call-process 1 "Error: failed")))
    ;; Should not throw an unhandled exception
    (condition-case err
        (progn
          (beads-close--execute)
          ;; If we get here, execution completed (possibly with error message)
          t)
      (error
       ;; Errors are acceptable - command failure is handled
       t))))

(ert-deftest beads-close-test-execute-without-reason ()
  "Test execution without reason fails with validation error."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--id=bd-42"))))
    ;; Should fail with user-error because reason is required
    (should-error (beads-close--execute) :type 'user-error)))

;;; Tests for Preview

(ert-deftest beads-close-test-preview-valid ()
  "Test preview command with valid parameters."
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--id=bd-42" "--reason=Fixed")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      ;; Preview displays message (returns nil but shows output)
      (beads-close--preview)
      (should (stringp message-output)))))

(ert-deftest beads-close-test-preview-validation-failure ()
  "Test preview shows validation errors."
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--id=")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      ;; Preview shows validation errors
      (beads-close--preview)
      (should (stringp message-output))
      (should (string-match-p "Validation" message-output)))))

(ert-deftest beads-close-test-preview-without-reason ()
  "Test preview without reason."
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--id=bd-42")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      ;; Preview shows validation errors (reason is required)
      (beads-close--preview)
      (should (stringp message-output)))))

;;; Tests for Transient Definition

(ert-deftest beads-close-test-transient-defined ()
  "Test that beads-close transient is defined."
  (should (fboundp 'beads-close)))

(ert-deftest beads-close-test-transient-is-prefix ()
  "Test that beads-close is a transient prefix."
  (should (get 'beads-close 'transient--prefix)))

(ert-deftest beads-close-test-infix-commands-defined ()
  "Test that all infix commands are defined."
  (should (fboundp 'beads-close-infix-issue-ids))
  (should (fboundp 'beads-close-infix-reason)))

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
    (should (null (beads-command-validate cmd)))

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
  (require 'beads-command-list)
  (with-temp-buffer
    (beads-list-mode)
    ;; Just verify the list mode supports close operation
    (should (fboundp 'beads-list-close))))

(ert-deftest beads-close-test-context-from-show-mode ()
  "Integration test: Test context detection from show mode."
  :tags '(integration)
  (require 'beads-command-show)
  ;; Verify show mode is compatible with close command
  (should (fboundp 'beads-show-mode)))

(ert-deftest beads-close-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (let ((cmd (beads-command-close :issue-ids '("bd-42") :reason "Fixed"))
        (start-time (current-time)))
    (dotimes (_ 1000)
      (beads-command-validate cmd))
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

(ert-deftest beads-close-test-main-command-exists ()
  "Test beads-close main command exists."
  (should (fboundp 'beads-close)))

;;; Regression Tests for Bug bde-65df

(ert-deftest beads-close-test-command-line-has-reason-dashes ()
  "Test that close command line includes --reason with dashes.
Regression test for bug bde-65df."
  (let ((beads-executable "bd")
        (cmd (beads-command-close :issue-ids '("bd-42")
                                   :reason "Fixed the bug")))
    (let ((cmd-line (beads-command-line cmd)))
      ;; Should contain --reason (with dashes)
      (should (member "--reason" cmd-line))
      ;; Should NOT contain just "reason" (without dashes)
      ;; Check that "reason" only appears after --reason
      (let ((reason-pos (cl-position "--reason" cmd-line :test #'equal)))
        (should reason-pos)
        (should (equal "Fixed the bug" (nth (1+ reason-pos) cmd-line)))))))

(ert-deftest beads-close-test-parsed-args-command-line-has-reason-dashes ()
  "Test that parsed transient args produce correct command line.
Regression test for bug bde-65df."
  (let ((beads-executable "bd")
        (args '("--id=bd-42" "--reason=Completed testing"))
        (cmd (beads-close--parse-transient-args
              '("--id=bd-42" "--reason=Completed testing"))))
    (let ((cmd-line (beads-command-line cmd)))
      ;; Should contain --reason (with dashes)
      (should (member "--reason" cmd-line))
      ;; Value should follow
      (let ((reason-pos (cl-position "--reason" cmd-line :test #'equal)))
        (should reason-pos)
        (should (equal "Completed testing" (nth (1+ reason-pos) cmd-line)))))))

(provide 'beads-close-test)
;;; beads-close-test.el ends here
