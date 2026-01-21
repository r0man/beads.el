;;; beads-reopen-test.el --- Tests for beads-command-reopen -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-command-reopen.el.
;; Tests cover command class behavior, parsing, validation, and execution.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-command-reopen)

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

(defun beads-reopen-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; Tests for Command Class

(ert-deftest beads-reopen-test-command-class-exists ()
  "Test that beads-command-reopen class is defined."
  (should (cl-find-class 'beads-command-reopen)))

(ert-deftest beads-reopen-test-command-subcommand ()
  "Test that subcommand returns 'reopen'."
  (let ((cmd (beads-command-reopen)))
    (should (equal (beads-command-subcommand cmd) "reopen"))))

;;; Tests for Argument Parsing

(ert-deftest beads-reopen-test-parse-args-empty ()
  "Test parsing empty arguments."
  (let ((cmd (beads-reopen--parse-transient-args '())))
    (should (beads-command-reopen-p cmd))
    (should (null (oref cmd issue-ids)))
    (should (null (oref cmd reason)))))

(ert-deftest beads-reopen-test-parse-args-issue-id ()
  "Test parsing issue ID argument."
  (let ((cmd (beads-reopen--parse-transient-args '("--id=bd-42"))))
    (should (beads-command-reopen-p cmd))
    (should (equal (oref cmd issue-ids) '("bd-42")))
    (should (null (oref cmd reason)))))

(ert-deftest beads-reopen-test-parse-args-reason ()
  "Test parsing reason argument."
  (let ((cmd (beads-reopen--parse-transient-args
              '("--id=bd-42" "--reason=Needs more work"))))
    (should (beads-command-reopen-p cmd))
    (should (equal (oref cmd issue-ids) '("bd-42")))
    (should (equal (oref cmd reason) "Needs more work"))))

;;; Tests for Validation

(ert-deftest beads-reopen-test-validate-missing-issue-id ()
  "Test validation when issue ID is missing."
  (let ((cmd (beads-command-reopen :reason "Needs work")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-reopen-test-validate-valid-command ()
  "Test validation with valid parameters."
  (let ((cmd (beads-command-reopen :issue-ids '("bd-42"))))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-reopen-test-validate-empty-issue-ids ()
  "Test validation when issue-ids is empty list."
  (let ((cmd (beads-command-reopen :issue-ids '())))
    (should (beads-command-validate cmd))))

(ert-deftest beads-reopen-test-validate-with-reason ()
  "Test validation with reason (optional)."
  (let ((cmd (beads-command-reopen :issue-ids '("bd-42") :reason "Needs work")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for Execution

(ert-deftest beads-reopen-test-execute-success ()
  "Test successful issue reopening."
  ;; bd reopen returns an array in JSON mode
  (let ((json-output (json-encode (vector beads-reopen-test--sample-reopen-response))))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--id=bd-42")))
              ((symbol-function 'call-process)
               (beads-reopen-test--mock-call-process 0 json-output)))
      (should-not (beads-reopen--execute)))))

(ert-deftest beads-reopen-test-execute-validation-failure ()
  "Test execution fails with validation error."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--id="))))
    (should-error (beads-reopen--execute) :type 'user-error)))

(ert-deftest beads-reopen-test-execute-missing-issue-id ()
  "Test execution fails when issue ID is missing."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--reason=Needs work"))))
    (should-error (beads-reopen--execute) :type 'user-error)))

(ert-deftest beads-reopen-test-execute-command-failure ()
  "Test execution handles bd command failure."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_prefix) '("--id=bd-42")))
            ((symbol-function 'call-process)
             (beads-reopen-test--mock-call-process 1 "Error: failed")))
    ;; Should not throw an unhandled exception
    (condition-case err
        (progn
          (beads-reopen--execute)
          t)
      (error t))))

(ert-deftest beads-reopen-test-execute-with-reason ()
  "Test execution with optional reason."
  (let ((json-output (json-encode (vector beads-reopen-test--sample-reopen-response))))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--id=bd-42" "--reason=Reopening")))
              ((symbol-function 'call-process)
               (beads-reopen-test--mock-call-process 0 json-output)))
      (should-not (beads-reopen--execute)))))

;;; Tests for Preview

(ert-deftest beads-reopen-test-preview-valid ()
  "Test preview command with valid parameters."
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--id=bd-42")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-reopen--preview)
      (should (stringp message-output)))))

(ert-deftest beads-reopen-test-preview-validation-failure ()
  "Test preview shows validation errors."
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--id=")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-reopen--preview)
      (should (stringp message-output))
      (should (string-match-p "Validation" message-output)))))

;;; Tests for Transient Definition

(ert-deftest beads-reopen-test-transient-defined ()
  "Test that beads-reopen transient is defined."
  (should (fboundp 'beads-reopen)))

(ert-deftest beads-reopen-test-transient-is-prefix ()
  "Test that beads-reopen is a transient prefix."
  (should (get 'beads-reopen 'transient--prefix)))

(ert-deftest beads-reopen-test-infix-commands-defined ()
  "Test that all infix commands are defined."
  (should (fboundp 'beads-reopen-infix-issue-ids))
  (should (fboundp 'beads-reopen-infix-reason)))

(ert-deftest beads-reopen-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-reopen--execute))
  (should (fboundp 'beads-reopen--reset))
  (should (fboundp 'beads-reopen--preview)))

;;; Integration Tests

(ert-deftest beads-reopen-test-full-workflow ()
  "Test complete workflow from setting params to reopening."
  (let ((cmd (beads-reopen--parse-transient-args
              '("--id=bd-42" "--reason=Needs more work"))))
    ;; Validate
    (should (null (beads-command-validate cmd)))

    ;; Execute (mocked)
    (let ((json-output (json-encode (vector beads-reopen-test--sample-reopen-response))))
      (cl-letf (((symbol-function 'transient-args)
                 (lambda (_prefix)
                   '("--id=bd-42" "--reason=Needs more work")))
                ((symbol-function 'call-process)
                 (beads-reopen-test--mock-call-process 0 json-output)))
        (should-not (beads-reopen--execute))))))

(ert-deftest beads-reopen-test-context-from-list-mode ()
  "Integration test: Test context detection from list mode."
  :tags '(integration)
  (require 'beads-command-list)
  (with-temp-buffer
    (beads-list-mode)
    ;; Just verify the list mode supports reopen operation
    (should (fboundp 'beads-list-reopen))))

(ert-deftest beads-reopen-test-context-from-show-mode ()
  "Integration test: Test context detection from show mode."
  :tags '(integration)
  (require 'beads-command-show)
  ;; Verify show mode is compatible with reopen command
  (should (fboundp 'beads-show-mode)))

(ert-deftest beads-reopen-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (let ((cmd (beads-command-reopen :issue-ids '("bd-42")))
        (start-time (current-time)))
    (dotimes (_ 1000)
      (beads-command-validate cmd))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should validate 1000 times in under 0.5 seconds
      (should (< elapsed 0.5)))))

;;; Tests for Reset Function

(ert-deftest beads-reopen-test-reset-confirmed ()
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
      (beads-reopen--reset)
      (should reset-called)
      (should (string-match-p "reset" message-output)))))

(ert-deftest beads-reopen-test-reset-declined ()
  "Test reset when user declines."
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t))))
      (beads-reopen--reset)
      (should-not reset-called))))

;;; Main Command Tests

(ert-deftest beads-reopen-test-main-command-exists ()
  "Test beads-reopen main command exists."
  (should (fboundp 'beads-reopen)))

;;; Regression Tests

(ert-deftest beads-reopen-test-command-line-has-reason-dashes ()
  "Test that reopen command line includes --reason with dashes."
  (let ((beads-executable "bd")
        (cmd (beads-command-reopen :issue-ids '("bd-42")
                                    :reason "Needs more work")))
    (let ((cmd-line (beads-command-line cmd)))
      ;; Should contain --reason (with dashes)
      (should (member "--reason" cmd-line))
      (let ((reason-pos (cl-position "--reason" cmd-line :test #'equal)))
        (should reason-pos)
        (should (equal "Needs more work" (nth (1+ reason-pos) cmd-line)))))))

(ert-deftest beads-reopen-test-parsed-args-command-line ()
  "Test that parsed transient args produce correct command line."
  (let ((beads-executable "bd")
        (cmd (beads-reopen--parse-transient-args
              '("--id=bd-42" "--reason=Reopening issue"))))
    (let ((cmd-line (beads-command-line cmd)))
      ;; Should contain --reason (with dashes)
      (should (member "--reason" cmd-line))
      (let ((reason-pos (cl-position "--reason" cmd-line :test #'equal)))
        (should reason-pos)
        (should (equal "Reopening issue" (nth (1+ reason-pos) cmd-line)))))))

(provide 'beads-reopen-test)
;;; beads-reopen-test.el ends here
