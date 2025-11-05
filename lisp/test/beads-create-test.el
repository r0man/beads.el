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
;; '("--title=Test" "-t=bug" "-p=1").

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
ARGS should be a list of strings like (\"--title=Test\" \"-t=bug\")."
  (lambda (prefix)
    (when (eq prefix 'beads-create)
      args)))

(defun beads-create-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; Tests for Argument Parsing

(ert-deftest beads-create-test-parse-args-empty ()
  "Test parsing empty argument list."
  (let ((parsed (beads-create--parse-transient-args nil)))
    (should (null (plist-get parsed :title)))
    (should (null (plist-get parsed :type)))
    (should (null (plist-get parsed :priority)))
    (should (null (plist-get parsed :description)))
    (should (null (plist-get parsed :custom-id)))
    (should (null (plist-get parsed :dependencies)))))

(ert-deftest beads-create-test-parse-args-title-only ()
  "Test parsing with only title."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Test Issue"))))
    (should (equal (plist-get parsed :title) "Test Issue"))
    (should (null (plist-get parsed :type)))))

(ert-deftest beads-create-test-parse-args-all-fields ()
  "Test parsing with all fields."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Full Issue"
                   "-t=feature"
                   "-p=2"
                   "-d=Full description"
                   "--id=custom-1"
                   "--deps=blocks:bd-1"))))
    (should (equal (plist-get parsed :title) "Full Issue"))
    (should (equal (plist-get parsed :type) "feature"))
    (should (equal (plist-get parsed :priority) 2))
    (should (equal (plist-get parsed :description) "Full description"))
    (should (equal (plist-get parsed :custom-id) "custom-1"))
    (should (equal (plist-get parsed :dependencies) "blocks:bd-1"))))

(ert-deftest beads-create-test-parse-args-title-with-equals ()
  "Test parsing title containing equals sign."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Issue with x=y formula"))))
    (should (equal (plist-get parsed :title) "Issue with x=y formula"))))

(ert-deftest beads-create-test-parse-args-multiline-description ()
  "Test parsing multiline description."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Test"
                   "-d=Line 1\nLine 2\nLine 3"))))
    (should (equal (plist-get parsed :title) "Test"))
    (should (string-match-p "\n" (plist-get parsed :description)))))

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
  (should (beads-create--validate-dependencies "blocks:BD_1")))

(ert-deftest beads-create-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Valid Title"
                   "-t=bug"
                   "-p=1"))))
    (should (null (beads-create--validate-all parsed)))))

(ert-deftest beads-create-test-validate-all-multiple-errors ()
  "Test validate-all with multiple validation errors."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title="
                   "-t=invalid"
                   "-p=10"))))
    (let ((errors (beads-create--validate-all parsed)))
      (should errors)
      (should (listp errors))
      (should (> (length errors) 1)))))

;;; Tests for Command Building

(ert-deftest beads-create-test-build-command-args-minimal ()
  "Test building command args with only title."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test Issue")))
         (args (beads-create--build-command-args parsed)))
    (should (equal args '("Test Issue")))))

(ert-deftest beads-create-test-build-command-args-with-type ()
  "Test building command args with title and type."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test Issue" "-t=bug")))
         (args (beads-create--build-command-args parsed)))
    (should (equal args '("Test Issue" "-t" "bug")))))

(ert-deftest beads-create-test-build-command-args-with-priority ()
  "Test building command args with title and priority."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test Issue" "-p=1")))
         (args (beads-create--build-command-args parsed)))
    (should (equal args '("Test Issue" "-p" "1")))))

(ert-deftest beads-create-test-build-command-args-with-description ()
  "Test building command args with description."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test Issue"
                    "-d=Test description")))
         (args (beads-create--build-command-args parsed)))
    (should (equal args '("Test Issue" "-d" "Test description")))))

(ert-deftest beads-create-test-build-command-args-with-custom-id ()
  "Test building command args with custom ID."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test Issue" "--id=worker1-100")))
         (args (beads-create--build-command-args parsed)))
    (should (equal args '("Test Issue" "--id" "worker1-100")))))

(ert-deftest beads-create-test-build-command-args-with-dependencies ()
  "Test building command args with dependencies."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test Issue" "--deps=blocks:bd-1")))
         (args (beads-create--build-command-args parsed)))
    (should (equal args '("Test Issue" "--deps" "blocks:bd-1")))))

(ert-deftest beads-create-test-build-command-args-all-options ()
  "Test building command args with all options."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test Issue"
                    "-t=feature"
                    "-p=2"
                    "-d=Long description"
                    "--id=custom-42"
                    "--deps=related:bd-10")))
         (args (beads-create--build-command-args parsed)))
    (should (equal args '("Test Issue"
                         "-t" "feature"
                         "-p" "2"
                         "-d" "Long description"
                         "--id" "custom-42"
                         "--deps" "related:bd-10")))))

(ert-deftest beads-create-test-build-command-args-title-with-spaces ()
  "Test building command args with title containing spaces."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test Issue With Spaces")))
         (args (beads-create--build-command-args parsed)))
    (should (equal (car args) "Test Issue With Spaces"))))

(ert-deftest beads-create-test-build-command-args-title-special-chars ()
  "Test building command args with special characters in title."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test \"quoted\" & special chars")))
         (args (beads-create--build-command-args parsed)))
    (should (equal (car args) "Test \"quoted\" & special chars"))))

(ert-deftest beads-create-test-build-command-args-multiline-description ()
  "Test building command args with multiline description."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test"
                    "-d=Line 1\nLine 2\nLine 3")))
         (args (beads-create--build-command-args parsed)))
    (should (member "-d" args))
    (let ((desc (nth (1+ (cl-position "-d" args :test #'equal)) args)))
      (should (string-match-p "\n" desc)))))

(ert-deftest beads-create-test-build-command-args-priority-zero ()
  "Test building command args with priority zero (critical)."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Critical Issue" "-p=0")))
         (args (beads-create--build-command-args parsed)))
    (should (member "0" args))))

(ert-deftest beads-create-test-build-command-args-empty-optional-fields ()
  "Test that empty optional fields are not included."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test"
                    "-d="
                    "--id=  \t\n  "
                    "--deps=")))
         (args (beads-create--build-command-args parsed)))
    ;; Should only contain title, no flags
    (should (equal args '("Test")))))

;;; Tests for Execution

(ert-deftest beads-create-test-execute-success ()
  "Test successful issue creation."
  (let ((json-output (json-encode
                      beads-create-test--sample-create-response)))
    (cl-letf (((symbol-function 'transient-args)
               (beads-create-test--mock-transient-args
                '("--title=Test Issue" "-t=bug" "-p=1")))
              ((symbol-function 'call-process)
               (beads-create-test--mock-call-process 0 json-output))
              ((symbol-function 'y-or-n-p) (lambda (_) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (should-not (beads-create--execute)))))

(ert-deftest beads-create-test-execute-validation-failure ()
  "Test execution fails with validation error."
  (cl-letf (((symbol-function 'transient-args)
             (beads-create-test--mock-transient-args
              '("--title=" "-t=bug"))))
    (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-missing-title ()
  "Test execution fails when title is missing."
  (cl-letf (((symbol-function 'transient-args)
             (beads-create-test--mock-transient-args
              '("-t=bug" "-p=1"))))
    (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-invalid-type ()
  "Test execution fails with invalid type."
  (cl-letf (((symbol-function 'transient-args)
             (beads-create-test--mock-transient-args
              '("--title=Test" "-t=invalid"))))
    (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-invalid-priority ()
  "Test execution fails with invalid priority."
  (cl-letf (((symbol-function 'transient-args)
             (beads-create-test--mock-transient-args
              '("--title=Test" "-p=10"))))
    (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-command-failure ()
  "Test execution handles bd command failure."
  (cl-letf (((symbol-function 'transient-args)
             (beads-create-test--mock-transient-args
              '("--title=Test Issue" "-p=1")))
            ((symbol-function 'call-process)
             (beads-create-test--mock-call-process 1 "Error: failed")))
    ;; Should not propagate error, just display message
    ;; The function catches errors and returns the message string
    (should (stringp (beads-create--execute)))))

(ert-deftest beads-create-test-execute-with-all-fields ()
  "Test execution with all fields populated."
  (let ((json-output (json-encode
                      beads-create-test--sample-create-response))
        (captured-args nil))
    (cl-letf (((symbol-function 'transient-args)
               (beads-create-test--mock-transient-args
                '("--title=Full Issue"
                  "-t=feature"
                  "-p=2"
                  "-d=Full description"
                  "--id=test-1"
                  "--deps=blocks:bd-1")))
              ((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                               &rest args)
                 (setq captured-args args)
                 (when destination
                   (with-current-buffer (current-buffer)
                     (insert json-output)))
                 0))
              ((symbol-function 'y-or-n-p) (lambda (_) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (should-not (beads-create--execute))
      ;; Verify all arguments were passed
      (should (member "-t" captured-args))
      (should (member "feature" captured-args))
      (should (member "-p" captured-args))
      (should (member "2" captured-args))
      (should (member "-d" captured-args))
      (should (member "--id" captured-args))
      (should (member "--deps" captured-args)))))

;;; Tests for Preview

(ert-deftest beads-create-test-preview-valid ()
  "Test preview command with valid parameters."
  (cl-letf (((symbol-function 'transient-args)
             (beads-create-test--mock-transient-args
              '("--title=Test Issue" "-t=bug" "-p=1"))))
    ;; Preview returns a message string
    (should (stringp (beads-create--preview)))))

(ert-deftest beads-create-test-preview-validation-failure ()
  "Test preview shows validation errors."
  (cl-letf (((symbol-function 'transient-args)
             (beads-create-test--mock-transient-args
              '("--title=" "-t=bug"))))
    ;; Preview returns a message string even with validation errors
    (should (stringp (beads-create--preview)))))

;;; Tests for Reset

(ert-deftest beads-create-test-reset ()
  "Test reset command."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
            ((symbol-function 'transient-set) (lambda (&rest _) nil)))
    ;; Reset should call transient-set without errors
    (should-not (beads-create--reset))))

;;; Tests for Transient Definition

(ert-deftest beads-create-test-transient-defined ()
  "Test that beads-create transient is defined."
  (should (fboundp 'beads-create)))

(ert-deftest beads-create-test-transient-is-prefix ()
  "Test that beads-create is a transient prefix."
  (should (get 'beads-create 'transient--prefix)))

(ert-deftest beads-create-test-infix-commands-defined ()
  "Test that all infix commands are defined."
  (should (fboundp 'beads-option-create-title))
  (should (fboundp 'beads-option-create-type))
  (should (fboundp 'beads-option-create-priority))
  (should (fboundp 'beads-option-create-description))
  (should (fboundp 'beads-option-create-custom-id))
  (should (fboundp 'beads-option-create-dependencies)))

(ert-deftest beads-create-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-create--execute))
  (should (fboundp 'beads-create--reset))
  (should (fboundp 'beads-create--preview)))

;;; Integration Tests

(ert-deftest beads-create-test-full-workflow ()
  "Test complete workflow from setting params to creation."
  (let ((json-output (json-encode
                      beads-create-test--sample-create-response)))
    (cl-letf (((symbol-function 'transient-args)
               (beads-create-test--mock-transient-args
                '("--title=Integration Test Issue"
                  "-t=task"
                  "-p=3"
                  "-d=Integration test description")))
              ((symbol-function 'call-process)
               (beads-create-test--mock-call-process 0 json-output))
              ((symbol-function 'y-or-n-p) (lambda (_) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      ;; Parse and validate
      (let* ((args (transient-args 'beads-create))
             (parsed (beads-create--parse-transient-args args)))
        (should (null (beads-create--validate-all parsed)))

        ;; Build command
        (let ((cmd-args (beads-create--build-command-args parsed)))
          (should (member "Integration Test Issue" cmd-args))
          (should (member "-t" cmd-args))
          (should (member "task" cmd-args))
          (should (member "-p" cmd-args))
          (should (member "3" cmd-args))))

      ;; Execute
      (should-not (beads-create--execute)))))

(ert-deftest beads-create-test-with-dependencies ()
  "Test workflow with dependencies specified."
  (let ((json-output (json-encode
                      beads-create-test--sample-create-response)))
    (cl-letf (((symbol-function 'transient-args)
               (beads-create-test--mock-transient-args
                '("--title=Dependent Issue"
                  "-t=bug"
                  "-p=1"
                  "--deps=discovered-from:bd-5")))
              ((symbol-function 'call-process)
               (beads-create-test--mock-call-process 0 json-output))
              ((symbol-function 'y-or-n-p) (lambda (_) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      ;; Validate
      (let* ((args (transient-args 'beads-create))
             (parsed (beads-create--parse-transient-args args)))
        (should (null (beads-create--validate-all parsed)))

        ;; Build and verify command includes dependencies
        (let ((cmd-args (beads-create--build-command-args parsed)))
          (should (member "--deps" cmd-args))
          (should (member "discovered-from:bd-5" cmd-args))))

      ;; Execute
      (should-not (beads-create--execute)))))

;;; Edge Cases

(ert-deftest beads-create-test-edge-case-unicode-title ()
  "Test creating issue with Unicode characters in title."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=测试 Issue with émojis 😀")))
         (args (beads-create--build-command-args parsed)))
    (should (equal (car args) "测试 Issue with émojis 😀"))))

(ert-deftest beads-create-test-edge-case-very-long-title ()
  "Test creating issue with very long title."
  (let* ((long-title (make-string 500 ?x))
         (parsed (beads-create--parse-transient-args
                  (list (concat "--title=" long-title))))
         (args (beads-create--build-command-args parsed)))
    (should (equal (car args) long-title))))

(ert-deftest beads-create-test-edge-case-title-with-newlines ()
  "Test creating issue with newlines in title."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Title\nWith\nNewlines")))
         (title (plist-get parsed :title))
         (args (beads-create--build-command-args parsed)))
    ;; This should be valid - bd will handle it
    (should (null (beads-create--validate-title title)))
    (should (string-match-p "\n" (car args)))))

(ert-deftest beads-create-test-edge-case-description-with-quotes ()
  "Test creating issue with quotes in description."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test"
                    "-d=Description with \"quotes\" and 'apostrophes'")))
         (args (beads-create--build-command-args parsed)))
    (should (member "-d" args))))

(ert-deftest beads-create-test-edge-case-multiple-dependencies ()
  "Test creating issue with multiple dependencies."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test"
                    "--deps=blocks:bd-1,related:bd-2,parent-child:bd-3")))
         (deps (plist-get parsed :dependencies)))
    (should (null (beads-create--validate-dependencies deps)))
    (let ((args (beads-create--build-command-args parsed)))
      (should (member "--deps" args))
      (let ((dep-value (nth (1+ (cl-position "--deps" args :test #'equal))
                            args)))
        (should (string-match-p "blocks:bd-1" dep-value))
        (should (string-match-p "related:bd-2" dep-value))
        (should (string-match-p "parent-child:bd-3" dep-value))))))

(ert-deftest beads-create-test-edge-case-custom-id-with-special-chars ()
  "Test custom ID with special characters."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test" "--id=worker-1-task-100")))
         (args (beads-create--build-command-args parsed)))
    (should (member "--id" args))
    (should (member "worker-1-task-100" args))))

;;; Performance Tests

(ert-deftest beads-create-test-performance-command-building ()
  "Test command building performance."
  :tags '(:performance)
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Test"
                   "-t=bug"
                   "-p=1"
                   "-d=Description"
                   "--id=test-1"
                   "--deps=blocks:bd-1"))))
    (let ((start-time (current-time)))
      (dotimes (_ 1000)
        (beads-create--build-command-args parsed))
      (let ((elapsed (float-time (time-subtract (current-time)
                                                 start-time))))
        ;; Should build 1000 commands in under 0.5 seconds
        (should (< elapsed 0.5))))))

(ert-deftest beads-create-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Test Issue"
                   "-t=bug"
                   "-p=1"
                   "--deps=blocks:bd-1"))))
    (let ((start-time (current-time)))
      (dotimes (_ 1000)
        (beads-create--validate-all parsed))
      (let ((elapsed (float-time (time-subtract (current-time)
                                                 start-time))))
        ;; Should validate 1000 times in under 0.5 seconds
        (should (< elapsed 0.5))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-create-test-transient-menu-defined ()
  "Integration test: Verify beads-create transient menu is defined."
  :tags '(integration)
  (should (fboundp 'beads-create)))

(ert-deftest beads-create-test-execute-function-defined ()
  "Integration test: Verify execute function is defined."
  :tags '(integration)
  (should (fboundp 'beads-create--execute)))

(ert-deftest beads-create-test-validation-workflow ()
  "Integration test: Validate that validation prevents execution."
  :tags '(integration)
  (let* ((parsed (beads-create--parse-transient-args '("--title=")))
         (validation-error (beads-create--validate-all parsed)))
    ;; validate-all returns list of errors, not string
    (should validation-error)
    (should (listp validation-error))))

(ert-deftest beads-create-test-command-building-workflow ()
  "Integration test: Test complete command building workflow."
  :tags '(integration)
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Integration Test Issue"
                    "-t=task"
                    "-p=2"))))
    ;; Should not error during validation
    (should-not (beads-create--validate-all parsed))
    ;; Should build valid command args
    (let ((args (beads-create--build-command-args parsed)))
      (should (member "Integration Test Issue" args))
      (should (member "-t" args))
      (should (member "task" args))
      (should (member "-p" args)))))

(ert-deftest beads-create-test-list-create-command ()
  "Integration test: Verify beads-list-create command exists."
  :tags '(integration)
  (require 'beads-list)
  (should (fboundp 'beads-list-create)))

(ert-deftest beads-create-test-list-keybinding-c ()
  "Integration test: Verify c keybinding in list mode."
  :tags '(integration)
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "c"))))
      (should (eq binding 'beads-list-create)))))

(ert-deftest beads-create-test-list-keybinding-plus ()
  "Integration test: Verify + keybinding in list mode."
  :tags '(integration)
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "+"))))
      (should (eq binding 'beads-list-create)))))

;;; Tests for New Fields (acceptance, assignee, design, external-ref,
;;; labels, force)

(ert-deftest beads-create-test-parse-args-acceptance ()
  "Test parsing acceptance criteria field."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Test" "--acceptance=Criteria text"))))
    (should (equal (plist-get parsed :acceptance) "Criteria text"))))

(ert-deftest beads-create-test-parse-args-assignee ()
  "Test parsing assignee field."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Test" "-a=john"))))
    (should (equal (plist-get parsed :assignee) "john"))))

(ert-deftest beads-create-test-parse-args-design ()
  "Test parsing design notes field."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Test" "--design=Design notes"))))
    (should (equal (plist-get parsed :design) "Design notes"))))

(ert-deftest beads-create-test-parse-args-external-ref ()
  "Test parsing external reference field."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Test" "--external-ref=gh-123"))))
    (should (equal (plist-get parsed :external-ref) "gh-123"))))

(ert-deftest beads-create-test-parse-args-labels ()
  "Test parsing labels field."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Test" "-l=bug,urgent"))))
    (should (equal (plist-get parsed :labels) "bug,urgent"))))

(ert-deftest beads-create-test-parse-args-force ()
  "Test parsing force flag."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Test" "--force"))))
    (should (eq (plist-get parsed :force) t))))

(ert-deftest beads-create-test-parse-args-all-new-fields ()
  "Test parsing with all new fields."
  (let ((parsed (beads-create--parse-transient-args
                 '("--title=Full Test"
                   "--acceptance=Accept criteria"
                   "-a=alice"
                   "--design=Design doc"
                   "--external-ref=jira-ABC"
                   "-l=feature,p1"
                   "--force"))))
    (should (equal (plist-get parsed :acceptance) "Accept criteria"))
    (should (equal (plist-get parsed :assignee) "alice"))
    (should (equal (plist-get parsed :design) "Design doc"))
    (should (equal (plist-get parsed :external-ref) "jira-ABC"))
    (should (equal (plist-get parsed :labels) "feature,p1"))
    (should (eq (plist-get parsed :force) t))))

(ert-deftest beads-create-test-build-command-args-with-acceptance ()
  "Test building command args with acceptance criteria."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test" "--acceptance=Must pass tests")))
         (args (beads-create--build-command-args parsed)))
    (should (member "--acceptance" args))
    (should (member "Must pass tests" args))))

(ert-deftest beads-create-test-build-command-args-with-assignee ()
  "Test building command args with assignee."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test" "-a=bob")))
         (args (beads-create--build-command-args parsed)))
    (should (member "-a" args))
    (should (member "bob" args))))

(ert-deftest beads-create-test-build-command-args-with-design ()
  "Test building command args with design notes."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test" "--design=Architecture notes")))
         (args (beads-create--build-command-args parsed)))
    (should (member "--design" args))
    (should (member "Architecture notes" args))))

(ert-deftest beads-create-test-build-command-args-with-external-ref ()
  "Test building command args with external reference."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test" "--external-ref=gh-42")))
         (args (beads-create--build-command-args parsed)))
    (should (member "--external-ref" args))
    (should (member "gh-42" args))))

(ert-deftest beads-create-test-build-command-args-with-labels ()
  "Test building command args with labels."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test" "-l=bug,critical")))
         (args (beads-create--build-command-args parsed)))
    (should (member "-l" args))
    (should (member "bug,critical" args))))

(ert-deftest beads-create-test-build-command-args-with-force ()
  "Test building command args with force flag."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Test" "--force")))
         (args (beads-create--build-command-args parsed)))
    (should (member "--force" args))))

(ert-deftest beads-create-test-build-command-args-comprehensive ()
  "Test building command args with all fields including new ones."
  (let* ((parsed (beads-create--parse-transient-args
                  '("--title=Comprehensive Test"
                    "-t=feature"
                    "-p=1"
                    "-d=Full description"
                    "--acceptance=Full acceptance"
                    "-a=charlie"
                    "--design=Full design"
                    "--external-ref=jira-XYZ"
                    "-l=feature,p1,urgent"
                    "--id=custom-1"
                    "--deps=blocks:bd-1"
                    "--force")))
         (args (beads-create--build-command-args parsed)))
    (should (equal (car args) "Comprehensive Test"))
    (should (member "-t" args))
    (should (member "feature" args))
    (should (member "-p" args))
    (should (member "1" args))
    (should (member "-d" args))
    (should (member "--acceptance" args))
    (should (member "-a" args))
    (should (member "charlie" args))
    (should (member "--design" args))
    (should (member "--external-ref" args))
    (should (member "jira-XYZ" args))
    (should (member "-l" args))
    (should (member "feature,p1,urgent" args))
    (should (member "--id" args))
    (should (member "--deps" args))
    (should (member "--force" args))))

(ert-deftest beads-create-test-infix-commands-new-fields ()
  "Test that all new infix commands are defined."
  (should (fboundp 'beads-option-create-acceptance))
  (should (fboundp 'beads-option-create-assignee))
  (should (fboundp 'beads-option-create-design))
  (should (fboundp 'beads-option-create-external-ref))
  (should (fboundp 'beads-option-create-labels))
  (should (fboundp 'beads-option-create-force)))


(provide 'beads-create-test)
;;; beads-create-test.el ends here
