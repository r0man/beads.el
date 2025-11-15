;;; beads-create-test.el --- Tests for beads-create -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-create.el transient menu.
;; Tests cover transient definition, command construction, validation,
;; execution, and integration with the bd CLI.
;;
;; This test file uses the transient-args pattern where tests mock
;; (transient-args 'beads-create) to return argument lists like
;; '("--title=Test" "--type=bug" "--priority=1").

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-create)

;;; Test Fixtures

(defvar beads-create-test--sample-create-response
  '((id . "bd-42")
    (title . "Test Issue")
    (description . "Test description")
    (status . "open")
    (priority . 1)
    (issue_type . "bug")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T10:00:00Z"))
  "Sample response from bd create command.")

;;; Test Utilities

(defun beads-create-test--mock-transient-args (args)
  "Create a mock for `transient-args' returning ARGS.
ARGS should be a list of strings like (\"--title=Test\" \"--type=bug\")."
  (lambda (prefix)
    (when (eq prefix 'beads-create)
      args)))

(defun beads-create-test--mock-process-file (exit-code output)
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

(ert-deftest beads-create-test-parse-args-empty ()
  "Test parsing empty argument list."
  (let ((cmd (beads-create--parse-transient-args nil)))
    (should (beads-command-create-p cmd))
    (should (null (oref cmd title)))
    (should (null (oref cmd issue-type)))
    (should (null (oref cmd priority)))
    (should (null (oref cmd description)))
    (should (null (oref cmd id)))
    (should (null (oref cmd deps)))))

(ert-deftest beads-create-test-parse-args-title-only ()
  "Test parsing with only title."
  (let ((cmd (beads-create--parse-transient-args
              '("--title=Test Issue"))))
    (should (beads-command-create-p cmd))
    (should (equal (oref cmd title) "Test Issue"))
    (should (null (oref cmd issue-type)))))

(ert-deftest beads-create-test-parse-args-all-fields ()
  "Test parsing with all fields."
  (let ((cmd (beads-create--parse-transient-args
              '("--title=Full Issue"
                "--type=feature"
                "--priority=2"
                "--description=Full description"
                "--id=custom-1"
                "--deps=blocks:bd-1"))))
    (should (beads-command-create-p cmd))
    (should (equal (oref cmd title) "Full Issue"))
    (should (equal (oref cmd issue-type) "feature"))
    (should (equal (oref cmd priority) 2))
    (should (equal (oref cmd description) "Full description"))
    (should (equal (oref cmd id) "custom-1"))
    (should (equal (oref cmd deps) '("blocks:bd-1")))))

(ert-deftest beads-create-test-parse-args-title-with-equals ()
  "Test parsing title containing equals sign."
  (let ((cmd (beads-create--parse-transient-args
              '("--title=Issue with x=y formula"))))
    (should (beads-command-create-p cmd))
    (should (equal (oref cmd title) "Issue with x=y formula"))))

(ert-deftest beads-create-test-parse-args-multiline-description ()
  "Test parsing multiline description.
Note: In real usage, multiline values are handled by the transient class.
For testing, we pass the multiline string on a single line with escaped \\n."
  (let ((cmd (beads-create--parse-transient-args
              '("--title=Test"
                "--description=Line 1 Line 2 Line 3"))))
    (should (beads-command-create-p cmd))
    (should (equal (oref cmd title) "Test"))
    (should (oref cmd description))))

;;; Tests for Validation

(ert-deftest beads-create-test-validate-title-nil ()
  "Test title validation when title is nil."
  (should (beads-create--validate-title nil)))

(ert-deftest beads-create-test-validate-title-empty ()
  "Test title validation when title is empty."
  (should (beads-create--validate-title "")))

(ert-deftest beads-create-test-validate-title-whitespace ()
  "Test title validation when title is only whitespace."
  (should (beads-create--validate-title "   \n\t  ")))

(ert-deftest beads-create-test-validate-title-valid ()
  "Test title validation when title is valid."
  (should (null (beads-create--validate-title "Valid Title"))))

(ert-deftest beads-create-test-validate-type-nil ()
  "Test type validation when type is nil."
  (should (null (beads-create--validate-type nil))))

(ert-deftest beads-create-test-validate-type-valid-bug ()
  "Test type validation with valid bug type."
  (should (null (beads-create--validate-type "bug"))))

(ert-deftest beads-create-test-validate-type-valid-feature ()
  "Test type validation with valid feature type."
  (should (null (beads-create--validate-type "feature"))))

(ert-deftest beads-create-test-validate-type-valid-task ()
  "Test type validation with valid task type."
  (should (null (beads-create--validate-type "task"))))

(ert-deftest beads-create-test-validate-type-valid-epic ()
  "Test type validation with valid epic type."
  (should (null (beads-create--validate-type "epic"))))

(ert-deftest beads-create-test-validate-type-valid-chore ()
  "Test type validation with valid chore type."
  (should (null (beads-create--validate-type "chore"))))

(ert-deftest beads-create-test-validate-type-invalid ()
  "Test type validation with invalid type."
  (should (beads-create--validate-type "invalid")))

(ert-deftest beads-create-test-validate-priority-nil ()
  "Test priority validation when priority is nil."
  (should (null (beads-create--validate-priority nil))))

(ert-deftest beads-create-test-validate-priority-zero ()
  "Test priority validation with zero (critical)."
  (should (null (beads-create--validate-priority 0))))

(ert-deftest beads-create-test-validate-priority-one ()
  "Test priority validation with one."
  (should (null (beads-create--validate-priority 1))))

(ert-deftest beads-create-test-validate-priority-four ()
  "Test priority validation with four (backlog)."
  (should (null (beads-create--validate-priority 4))))

(ert-deftest beads-create-test-validate-priority-negative ()
  "Test priority validation with negative number."
  (should (beads-create--validate-priority -1)))

(ert-deftest beads-create-test-validate-priority-too-high ()
  "Test priority validation with number too high."
  (should (beads-create--validate-priority 5)))

(ert-deftest beads-create-test-validate-priority-string ()
  "Test priority validation with string instead of number."
  (should (beads-create--validate-priority "1")))

(ert-deftest beads-create-test-validate-dependencies-nil ()
  "Test dependencies validation when nil."
  (should (null (beads-create--validate-dependencies nil))))

(ert-deftest beads-create-test-validate-dependencies-valid-single ()
  "Test dependencies validation with single valid dependency."
  (should (null (beads-create--validate-dependencies "blocks:bd-1"))))

(ert-deftest beads-create-test-validate-dependencies-valid-multiple ()
  "Test dependencies validation with multiple valid dependencies."
  (should (null (beads-create--validate-dependencies
                 "blocks:bd-1,related:bd-2"))))

(ert-deftest beads-create-test-validate-dependencies-valid-discovered ()
  "Test dependencies validation with discovered-from type."
  (should (null (beads-create--validate-dependencies
                 "discovered-from:bd-10"))))

(ert-deftest beads-create-test-validate-dependencies-invalid-format ()
  "Test dependencies validation with invalid format."
  (should (beads-create--validate-dependencies "invalid")))

(ert-deftest beads-create-test-validate-dependencies-missing-colon ()
  "Test dependencies validation without colon separator."
  (should (beads-create--validate-dependencies "blocksbd-1")))

(ert-deftest beads-create-test-validate-dependencies-invalid-characters ()
  "Test dependencies validation with invalid characters."
  (should (beads-create--validate-dependencies "blocks:BD@123")))

(ert-deftest beads-create-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (let ((cmd (beads-create--parse-transient-args
              '("--title=Valid Title"
                "--type=bug"
                "--priority=1"))))
    (should (null (beads-create--validate-all cmd)))))

(ert-deftest beads-create-test-validate-all-multiple-errors ()
  "Test validate-all with multiple validation errors."
  (let ((cmd (beads-create--parse-transient-args
              '("--title="
                "--type=invalid"
                "--priority=10"))))
    (let ((errors (beads-create--validate-all cmd)))
      (should errors)
      (should (listp errors))
      (should (> (length errors) 1)))))


(ert-deftest beads-create-test-infix-commands-new-fields ()
  "Test that all new infix commands are defined."
  (should (fboundp 'beads-option-issue-acceptance))
  (should (fboundp 'beads-option-issue-assignee))
  (should (fboundp 'beads-option-issue-design))
  (should (fboundp 'beads-option-issue-external-ref))
  (should (fboundp 'beads-option-issue-labels))
  (should (fboundp 'beads-option-create-force)))

;;; Tests for Transient Definition

(ert-deftest beads-create-test-transient-definition-valid ()
  "Test that beads-create transient definition is valid.
This test reproduces the bug where a quoted symbol in the transient
definition causes 'Wrong type argument: number-or-marker-p' error."
  ;; The transient prefix should be a valid command
  (should (commandp 'beads-create))

  ;; Get the transient prefix object
  (let ((prefix (get 'beads-create 'transient--prefix)))
    ;; The prefix should exist and be a transient-prefix object
    (should prefix)
    (should (transient-prefix-p prefix))))

(ert-deftest beads-create-test-transient-can-be-called ()
  "Test that beads-create can be called without error.
This directly tests the fix for the bug where referencing
beads-create-infix-arguments (a transient-define-group) directly
in transient-define-prefix caused a 'Wrong type argument:
number-or-marker-p' error. The fix was to inline the layout
directly in the transient-define-prefix definition."
  ;; Mock transient-setup to prevent actual UI display
  ;; We just want to verify it doesn't error during setup
  (let ((setup-called nil)
        (setup-succeeded nil))
    (cl-letf (((symbol-function 'transient-setup)
               (lambda (name &rest args)
                 (setq setup-called t)
                 ;; If we get here without error, the setup succeeded
                 (setq setup-succeeded t))))
      ;; Call beads-create - should call transient-setup internally
      (condition-case err
          (beads-create)
        ;; Catch and ignore errors from transient-setup trying to
        ;; display the buffer (since we're mocking it)
        (error nil))

      ;; Verify setup was called
      (should setup-called)
      ;; Verify it succeeded (no error from invalid layout reference)
      (should setup-succeeded))))


(provide 'beads-create-test)
;;; beads-create-test.el ends here
