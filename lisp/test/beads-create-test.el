;;; beads-transient-create-test.el --- Tests for beads-create -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-create.el transient menu.
;; Tests cover transient definition, command construction, validation,
;; execution, and integration with the bd CLI.

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

(defun beads-create-test--set-state (state-alist)
  "Set transient state from STATE-ALIST.
STATE-ALIST is an alist of (variable . value) pairs."
  (setq beads-create--title nil
        beads-create--type nil
        beads-create--priority nil
        beads-create--description nil
        beads-create--custom-id nil
        beads-create--dependencies nil)
  (dolist (binding state-alist)
    (set (car binding) (cdr binding))))

(defmacro beads-create-test-with-state (state &rest body)
  "Execute BODY with beads-create transient state set to STATE.
STATE is an alist expression of (variable . value) pairs."
  (declare (indent 1))
  `(progn
     (beads-create-test--set-state ,state)
     ,@body))

(defun beads-create-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; Tests for State Management

(ert-deftest beads-create-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-create-test-with-state
   '((beads-create--title . "Test")
     (beads-create--type . "bug")
     (beads-create--priority . 1)
     (beads-create--description . "Desc")
     (beads-create--custom-id . "custom-1")
     (beads-create--dependencies . "blocks:bd-1"))
   (beads-create--reset-state)
   (should (null beads-create--title))
   (should (null beads-create--type))
   (should (null beads-create--priority))
   (should (null beads-create--description))
   (should (null beads-create--custom-id))
   (should (null beads-create--dependencies))))

(ert-deftest beads-create-test-reset-state-from-nil ()
  "Test that reset-state works when variables are already nil."
  (beads-create-test-with-state nil
   (beads-create--reset-state)
   (should (null beads-create--title))
   (should (null beads-create--type))
   (should (null beads-create--priority))
   (should (null beads-create--description))
   (should (null beads-create--custom-id))
   (should (null beads-create--dependencies))))

;;; Tests for Value Formatting

(ert-deftest beads-create-test-format-current-value-set ()
  "Test formatting when value is set."
  (let ((result (beads-create--format-current-value "test-value")))
    (should (stringp result))
    (should (string-match-p "test-value" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-create-test-format-current-value-nil ()
  "Test formatting when value is nil."
  (let ((result (beads-create--format-current-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-create-test-format-current-value-empty-string ()
  "Test formatting with empty string."
  (let ((result (beads-create--format-current-value "")))
    (should (stringp result))
    ;; Empty string is truthy, so it should show the value
    (should (string-match-p "\\[\\]" result))))

(ert-deftest beads-create-test-format-current-value-long-string ()
  "Test formatting with long string value."
  (let* ((long-value (make-string 100 ?x))
         (result (beads-create--format-current-value long-value)))
    (should (stringp result))
    (should (string-match-p "xxx" result))))

;;; Tests for Validation

(ert-deftest beads-create-test-validate-title-nil ()
  "Test title validation when title is nil."
  (beads-create-test-with-state nil
   (should (beads-create--validate-title))))

(ert-deftest beads-create-test-validate-title-empty ()
  "Test title validation when title is empty."
  (beads-create-test-with-state '((beads-create--title . ""))
   (should (beads-create--validate-title))))

(ert-deftest beads-create-test-validate-title-whitespace ()
  "Test title validation when title is only whitespace."
  (beads-create-test-with-state '((beads-create--title . "   \n\t  "))
   (should (beads-create--validate-title))))

(ert-deftest beads-create-test-validate-title-valid ()
  "Test title validation when title is valid."
  (beads-create-test-with-state '((beads-create--title . "Valid Title"))
   (should (null (beads-create--validate-title)))))

(ert-deftest beads-create-test-validate-type-nil ()
  "Test type validation when type is nil."
  (beads-create-test-with-state nil
   (should (null (beads-create--validate-type)))))

(ert-deftest beads-create-test-validate-type-valid-bug ()
  "Test type validation with valid bug type."
  (beads-create-test-with-state '((beads-create--type . "bug"))
   (should (null (beads-create--validate-type)))))

(ert-deftest beads-create-test-validate-type-valid-feature ()
  "Test type validation with valid feature type."
  (beads-create-test-with-state '((beads-create--type . "feature"))
   (should (null (beads-create--validate-type)))))

(ert-deftest beads-create-test-validate-type-valid-task ()
  "Test type validation with valid task type."
  (beads-create-test-with-state '((beads-create--type . "task"))
   (should (null (beads-create--validate-type)))))

(ert-deftest beads-create-test-validate-type-valid-epic ()
  "Test type validation with valid epic type."
  (beads-create-test-with-state '((beads-create--type . "epic"))
   (should (null (beads-create--validate-type)))))

(ert-deftest beads-create-test-validate-type-valid-chore ()
  "Test type validation with valid chore type."
  (beads-create-test-with-state '((beads-create--type . "chore"))
   (should (null (beads-create--validate-type)))))

(ert-deftest beads-create-test-validate-type-invalid ()
  "Test type validation with invalid type."
  (beads-create-test-with-state '((beads-create--type . "invalid"))
   (should (beads-create--validate-type))))

(ert-deftest beads-create-test-validate-priority-nil ()
  "Test priority validation when priority is nil."
  (beads-create-test-with-state nil
   (should (null (beads-create--validate-priority)))))

(ert-deftest beads-create-test-validate-priority-zero ()
  "Test priority validation with zero (critical)."
  (beads-create-test-with-state '((beads-create--priority . 0))
   (should (null (beads-create--validate-priority)))))

(ert-deftest beads-create-test-validate-priority-one ()
  "Test priority validation with one."
  (beads-create-test-with-state '((beads-create--priority . 1))
   (should (null (beads-create--validate-priority)))))

(ert-deftest beads-create-test-validate-priority-four ()
  "Test priority validation with four (backlog)."
  (beads-create-test-with-state '((beads-create--priority . 4))
   (should (null (beads-create--validate-priority)))))

(ert-deftest beads-create-test-validate-priority-negative ()
  "Test priority validation with negative number."
  (beads-create-test-with-state '((beads-create--priority . -1))
   (should (beads-create--validate-priority))))

(ert-deftest beads-create-test-validate-priority-too-high ()
  "Test priority validation with number too high."
  (beads-create-test-with-state '((beads-create--priority . 5))
   (should (beads-create--validate-priority))))

(ert-deftest beads-create-test-validate-priority-string ()
  "Test priority validation with string instead of number."
  (beads-create-test-with-state '((beads-create--priority . "1"))
   (should (beads-create--validate-priority))))

(ert-deftest beads-create-test-validate-dependencies-nil ()
  "Test dependencies validation when nil."
  (beads-create-test-with-state nil
   (should (null (beads-create--validate-dependencies)))))

(ert-deftest beads-create-test-validate-dependencies-valid-single ()
  "Test dependencies validation with single valid dependency."
  (beads-create-test-with-state '((beads-create--dependencies . "blocks:bd-1"))
   (should (null (beads-create--validate-dependencies)))))

(ert-deftest beads-create-test-validate-dependencies-valid-multiple ()
  "Test dependencies validation with multiple valid dependencies."
  (beads-create-test-with-state
   '((beads-create--dependencies . "blocks:bd-1,related:bd-2"))
   (should (null (beads-create--validate-dependencies)))))

(ert-deftest beads-create-test-validate-dependencies-valid-discovered ()
  "Test dependencies validation with discovered-from type."
  (beads-create-test-with-state
   '((beads-create--dependencies . "discovered-from:bd-10"))
   (should (null (beads-create--validate-dependencies)))))

(ert-deftest beads-create-test-validate-dependencies-invalid-format ()
  "Test dependencies validation with invalid format."
  (beads-create-test-with-state
   '((beads-create--dependencies . "invalid"))
   (should (beads-create--validate-dependencies))))

(ert-deftest beads-create-test-validate-dependencies-missing-colon ()
  "Test dependencies validation without colon separator."
  (beads-create-test-with-state
   '((beads-create--dependencies . "blocksbd-1"))
   (should (beads-create--validate-dependencies))))

(ert-deftest beads-create-test-validate-dependencies-invalid-characters ()
  "Test dependencies validation with invalid characters."
  (beads-create-test-with-state
   '((beads-create--dependencies . "blocks:BD_1"))
   (should (beads-create--validate-dependencies))))

(ert-deftest beads-create-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (beads-create-test-with-state
   '((beads-create--title . "Valid Title")
     (beads-create--type . "bug")
     (beads-create--priority . 1))
   (should (null (beads-create--validate-all)))))

(ert-deftest beads-create-test-validate-all-multiple-errors ()
  "Test validate-all with multiple validation errors."
  (beads-create-test-with-state
   '((beads-create--title . "")
     (beads-create--type . "invalid")
     (beads-create--priority . 10))
   (let ((errors (beads-create--validate-all)))
     (should errors)
     (should (listp errors))
     (should (> (length errors) 1)))))

;;; Tests for Command Building

(ert-deftest beads-create-test-build-command-args-minimal ()
  "Test building command args with only title."
  (beads-create-test-with-state '((beads-create--title . "Test Issue"))
   (let ((args (beads-create--build-command-args)))
     (should (equal args '("Test Issue"))))))

(ert-deftest beads-create-test-build-command-args-with-type ()
  "Test building command args with title and type."
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue")
     (beads-create--type . "bug"))
   (let ((args (beads-create--build-command-args)))
     (should (equal args '("Test Issue" "-t" "bug"))))))

(ert-deftest beads-create-test-build-command-args-with-priority ()
  "Test building command args with title and priority."
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue")
     (beads-create--priority . 1))
   (let ((args (beads-create--build-command-args)))
     (should (equal args '("Test Issue" "-p" "1"))))))

(ert-deftest beads-create-test-build-command-args-with-description ()
  "Test building command args with description."
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue")
     (beads-create--description . "Test description"))
   (let ((args (beads-create--build-command-args)))
     (should (equal args '("Test Issue" "-d" "Test description"))))))

(ert-deftest beads-create-test-build-command-args-with-custom-id ()
  "Test building command args with custom ID."
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue")
     (beads-create--custom-id . "worker1-100"))
   (let ((args (beads-create--build-command-args)))
     (should (equal args '("Test Issue" "--id" "worker1-100"))))))

(ert-deftest beads-create-test-build-command-args-with-dependencies ()
  "Test building command args with dependencies."
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue")
     (beads-create--dependencies . "blocks:bd-1"))
   (let ((args (beads-create--build-command-args)))
     (should (equal args '("Test Issue" "--deps" "blocks:bd-1"))))))

(ert-deftest beads-create-test-build-command-args-all-options ()
  "Test building command args with all options."
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue")
     (beads-create--type . "feature")
     (beads-create--priority . 2)
     (beads-create--description . "Long description")
     (beads-create--custom-id . "custom-42")
     (beads-create--dependencies . "related:bd-10"))
   (let ((args (beads-create--build-command-args)))
     (should (equal args '("Test Issue"
                          "-t" "feature"
                          "-p" "2"
                          "-d" "Long description"
                          "--id" "custom-42"
                          "--deps" "related:bd-10"))))))

(ert-deftest beads-create-test-build-command-args-title-with-spaces ()
  "Test building command args with title containing spaces."
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue With Spaces"))
   (let ((args (beads-create--build-command-args)))
     (should (equal (car args) "Test Issue With Spaces")))))

(ert-deftest beads-create-test-build-command-args-title-special-chars ()
  "Test building command args with special characters in title."
  (beads-create-test-with-state
   '((beads-create--title . "Test \"quoted\" & special chars"))
   (let ((args (beads-create--build-command-args)))
     (should (equal (car args) "Test \"quoted\" & special chars")))))

(ert-deftest beads-create-test-build-command-args-multiline-description ()
  "Test building command args with multiline description."
  (beads-create-test-with-state
   '((beads-create--title . "Test")
     (beads-create--description . "Line 1\nLine 2\nLine 3"))
   (let ((args (beads-create--build-command-args)))
     (should (member "-d" args))
     (let ((desc (nth (1+ (cl-position "-d" args :test #'equal)) args)))
       (should (string-match-p "\n" desc))))))

(ert-deftest beads-create-test-build-command-args-priority-zero ()
  "Test building command args with priority zero (critical)."
  (beads-create-test-with-state
   '((beads-create--title . "Critical Issue")
     (beads-create--priority . 0))
   (let ((args (beads-create--build-command-args)))
     (should (member "0" args)))))

(ert-deftest beads-create-test-build-command-args-empty-optional-fields ()
  "Test that empty optional fields are not included."
  (beads-create-test-with-state
   '((beads-create--title . "Test")
     (beads-create--description . "")
     (beads-create--custom-id . "  \t\n  ")
     (beads-create--dependencies . ""))
   (let ((args (beads-create--build-command-args)))
     ;; Should only contain title, no flags
     (should (equal args '("Test"))))))

;;; Tests for Execution

(ert-deftest beads-create-test-execute-success ()
  "Test successful issue creation."
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue")
     (beads-create--type . "bug")
     (beads-create--priority . 1))
   (let ((json-output (json-encode
                       beads-create-test--sample-create-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-create-test--mock-call-process 0 json-output))
               ((symbol-function 'y-or-n-p) (lambda (_) nil)))
       (should-not (beads-create--execute))
       ;; State should be reset after successful execution
       (should (null beads-create--title))
       (should (null beads-create--type))
       (should (null beads-create--priority))))))

(ert-deftest beads-create-test-execute-validation-failure ()
  "Test execution fails with validation error."
  (beads-create-test-with-state
   '((beads-create--title . "")
     (beads-create--type . "bug"))
   (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-missing-title ()
  "Test execution fails when title is missing."
  (beads-create-test-with-state
   '((beads-create--type . "bug")
     (beads-create--priority . 1))
   (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-invalid-type ()
  "Test execution fails with invalid type."
  (beads-create-test-with-state
   '((beads-create--title . "Test")
     (beads-create--type . "invalid"))
   (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-invalid-priority ()
  "Test execution fails with invalid priority."
  (beads-create-test-with-state
   '((beads-create--title . "Test")
     (beads-create--priority . 10))
   (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-command-failure ()
  "Test execution handles bd command failure."
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue")
     (beads-create--priority . 1))
   (cl-letf (((symbol-function 'call-process)
              (beads-create-test--mock-call-process 1 "Error: failed")))
     ;; Should not propagate error, just display message
     ;; The function catches errors and returns the message string
     (should (stringp (beads-create--execute))))))

(ert-deftest beads-create-test-execute-with-all-fields ()
  "Test execution with all fields populated."
  (beads-create-test-with-state
   '((beads-create--title . "Full Issue")
     (beads-create--type . "feature")
     (beads-create--priority . 2)
     (beads-create--description . "Full description")
     (beads-create--custom-id . "test-1")
     (beads-create--dependencies . "blocks:bd-1"))
   (let ((json-output (json-encode
                       beads-create-test--sample-create-response))
         (captured-args nil))
     (cl-letf (((symbol-function 'call-process)
                (lambda (program &optional infile destination display
                                &rest args)
                  (setq captured-args args)
                  (when destination
                    (with-current-buffer (current-buffer)
                      (insert json-output)))
                  0))
               ((symbol-function 'y-or-n-p) (lambda (_) nil)))
       (should-not (beads-create--execute))
       ;; Verify all arguments were passed
       (should (member "-t" captured-args))
       (should (member "feature" captured-args))
       (should (member "-p" captured-args))
       (should (member "2" captured-args))
       (should (member "-d" captured-args))
       (should (member "--id" captured-args))
       (should (member "--deps" captured-args))))))

;;; Tests for Preview

(ert-deftest beads-create-test-preview-valid ()
  "Test preview command with valid parameters."
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue")
     (beads-create--type . "bug")
     (beads-create--priority . 1))
   ;; Preview returns a message string
   (should (stringp (beads-create--preview)))))

(ert-deftest beads-create-test-preview-validation-failure ()
  "Test preview shows validation errors."
  (beads-create-test-with-state
   '((beads-create--title . "")
     (beads-create--type . "bug"))
   ;; Preview returns a message string even with validation errors
   (should (stringp (beads-create--preview)))))

;;; Tests for Transient Definition

(ert-deftest beads-create-test-transient-defined ()
  "Test that beads-create transient is defined."
  (should (fboundp 'beads-create)))

(ert-deftest beads-create-test-transient-is-prefix ()
  "Test that beads-create is a transient prefix."
  (should (get 'beads-create 'transient--prefix)))

(ert-deftest beads-create-test-infix-commands-defined ()
  "Test that all infix commands are defined."
  (should (fboundp 'beads-create--infix-title))
  (should (fboundp 'beads-create--infix-type))
  (should (fboundp 'beads-create--infix-priority))
  (should (fboundp 'beads-create--infix-description))
  (should (fboundp 'beads-create--infix-custom-id))
  (should (fboundp 'beads-create--infix-dependencies)))

(ert-deftest beads-create-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-create--execute))
  (should (fboundp 'beads-create--reset))
  (should (fboundp 'beads-create--preview)))

;;; Integration Tests

(ert-deftest beads-create-test-integration-full-workflow ()
  "Test complete workflow from setting params to creation."
  (beads-create-test-with-state nil
   ;; Set parameters
   (setq beads-create--title "Integration Test Issue")
   (setq beads-create--type "task")
   (setq beads-create--priority 3)
   (setq beads-create--description "Integration test description")

   ;; Validate
   (should (null (beads-create--validate-all)))

   ;; Build command
   (let ((args (beads-create--build-command-args)))
     (should (member "Integration Test Issue" args))
     (should (member "-t" args))
     (should (member "task" args))
     (should (member "-p" args))
     (should (member "3" args)))

   ;; Execute (mocked)
   (let ((json-output (json-encode
                       beads-create-test--sample-create-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-create-test--mock-call-process 0 json-output))
               ((symbol-function 'y-or-n-p) (lambda (_) nil)))
       (should-not (beads-create--execute))
       ;; Verify state was reset
       (should (null beads-create--title))))))

(ert-deftest beads-create-test-integration-with-dependencies ()
  "Test workflow with dependencies specified."
  (beads-create-test-with-state
   '((beads-create--title . "Dependent Issue")
     (beads-create--type . "bug")
     (beads-create--priority . 1)
     (beads-create--dependencies . "discovered-from:bd-5"))

   ;; Validate
   (should (null (beads-create--validate-all)))

   ;; Build and verify command includes dependencies
   (let ((args (beads-create--build-command-args)))
     (should (member "--deps" args))
     (should (member "discovered-from:bd-5" args)))

   ;; Execute
   (let ((json-output (json-encode
                       beads-create-test--sample-create-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-create-test--mock-call-process 0 json-output))
               ((symbol-function 'y-or-n-p) (lambda (_) nil)))
       (should-not (beads-create--execute))))))

(ert-deftest beads-create-test-integration-reset-and-recreate ()
  "Test resetting state and creating another issue."
  (beads-create-test-with-state
   '((beads-create--title . "First Issue")
     (beads-create--type . "bug"))

   ;; Reset
   (beads-create--reset-state)
   (should (null beads-create--title))
   (should (null beads-create--type))

   ;; Set new values
   (setq beads-create--title "Second Issue")
   (setq beads-create--type "feature")
   (setq beads-create--priority 2)

   ;; Build command - should only have new values
   (let ((args (beads-create--build-command-args)))
     (should (member "Second Issue" args))
     (should (member "feature" args))
     (should-not (member "bug" args)))))

;;; Edge Cases

(ert-deftest beads-create-test-edge-case-unicode-title ()
  "Test creating issue with Unicode characters in title."
  (beads-create-test-with-state
   '((beads-create--title . "测试 Issue with émojis 😀"))
   (let ((args (beads-create--build-command-args)))
     (should (equal (car args) "测试 Issue with émojis 😀")))))

(ert-deftest beads-create-test-edge-case-very-long-title ()
  "Test creating issue with very long title."
  (let ((long-title (make-string 500 ?x)))
    (beads-create-test-with-state
     `((beads-create--title . ,long-title))
     (let ((args (beads-create--build-command-args)))
       (should (equal (car args) long-title))))))

(ert-deftest beads-create-test-edge-case-title-with-newlines ()
  "Test creating issue with newlines in title."
  (beads-create-test-with-state
   '((beads-create--title . "Title\nWith\nNewlines"))
   ;; This should be valid - bd will handle it
   (should (null (beads-create--validate-title)))
   (let ((args (beads-create--build-command-args)))
     (should (string-match-p "\n" (car args))))))

(ert-deftest beads-create-test-edge-case-description-with-quotes ()
  "Test creating issue with quotes in description."
  (beads-create-test-with-state
   '((beads-create--title . "Test")
     (beads-create--description . "Description with \"quotes\" and 'apostrophes'"))
   (let ((args (beads-create--build-command-args)))
     (should (member "-d" args)))))

(ert-deftest beads-create-test-edge-case-multiple-dependencies ()
  "Test creating issue with multiple dependencies."
  (beads-create-test-with-state
   '((beads-create--title . "Test")
     (beads-create--dependencies . "blocks:bd-1,related:bd-2,parent-child:bd-3"))
   (should (null (beads-create--validate-dependencies)))
   (let ((args (beads-create--build-command-args)))
     (should (member "--deps" args))
     (let ((deps (nth (1+ (cl-position "--deps" args :test #'equal)) args)))
       (should (string-match-p "blocks:bd-1" deps))
       (should (string-match-p "related:bd-2" deps))
       (should (string-match-p "parent-child:bd-3" deps))))))

(ert-deftest beads-create-test-edge-case-custom-id-with-special-chars ()
  "Test custom ID with special characters."
  (beads-create-test-with-state
   '((beads-create--title . "Test")
     (beads-create--custom-id . "worker-1-task-100"))
   (let ((args (beads-create--build-command-args)))
     (should (member "--id" args))
     (should (member "worker-1-task-100" args)))))

;;; Performance Tests

(ert-deftest beads-create-test-performance-command-building ()
  "Test command building performance."
  :tags '(:performance)
  (beads-create-test-with-state
   '((beads-create--title . "Test")
     (beads-create--type . "bug")
     (beads-create--priority . 1)
     (beads-create--description . "Description")
     (beads-create--custom-id . "test-1")
     (beads-create--dependencies . "blocks:bd-1"))
   (let ((start-time (current-time)))
     (dotimes (_ 1000)
       (beads-create--build-command-args))
     (let ((elapsed (float-time (time-subtract (current-time) start-time))))
       ;; Should build 1000 commands in under 0.5 seconds
       (should (< elapsed 0.5))))))

(ert-deftest beads-create-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (beads-create-test-with-state
   '((beads-create--title . "Test Issue")
     (beads-create--type . "bug")
     (beads-create--priority . 1)
     (beads-create--dependencies . "blocks:bd-1"))
   (let ((start-time (current-time)))
     (dotimes (_ 1000)
       (beads-create--validate-all))
     (let ((elapsed (float-time (time-subtract (current-time) start-time))))
       ;; Should validate 1000 times in under 0.5 seconds
       (should (< elapsed 0.5))))))

(provide 'beads-transient-create-test)
;;; beads-transient-create-test.el ends here
