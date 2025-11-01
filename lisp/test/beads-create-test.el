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

(defun beads-create-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

(defun beads-create-test--build-transient-args (plist)
  "Build transient arguments list from PLIST.
PLIST should contain keys like :title, :type, :priority, etc."
  (let ((args nil))
    (when-let ((title (plist-get plist :title)))
      (push (concat "title=" title) args))
    (when-let ((type (plist-get plist :type)))
      (push (concat "-t=" type) args))
    (when-let ((priority (plist-get plist :priority)))
      (push (concat "-p=" (number-to-string priority)) args))
    (when-let ((description (plist-get plist :description)))
      (push (concat "-d=" description) args))
    (when-let ((custom-id (plist-get plist :custom-id)))
      (push (concat "--id=" custom-id) args))
    (when-let ((dependencies (plist-get plist :dependencies)))
      (push (concat "--deps=" dependencies) args))
    (nreverse args)))

;;; Tests for Parse Function

(ert-deftest beads-create-test-parse-transient-args-empty ()
  "Test parsing empty transient args."
  (let ((parsed (beads-create--parse-transient-args nil)))
    (should (null (plist-get parsed :title)))
    (should (null (plist-get parsed :type)))
    (should (null (plist-get parsed :priority)))
    (should (null (plist-get parsed :description)))
    (should (null (plist-get parsed :custom-id)))
    (should (null (plist-get parsed :dependencies)))))

(ert-deftest beads-create-test-parse-transient-args-title ()
  "Test parsing title argument."
  (let ((parsed (beads-create--parse-transient-args '("title=Test Issue"))))
    (should (equal (plist-get parsed :title) "Test Issue"))))

(ert-deftest beads-create-test-parse-transient-args-type ()
  "Test parsing type argument."
  (let ((parsed (beads-create--parse-transient-args '("-t=bug"))))
    (should (equal (plist-get parsed :type) "bug"))))

(ert-deftest beads-create-test-parse-transient-args-priority ()
  "Test parsing priority argument."
  (let ((parsed (beads-create--parse-transient-args '("-p=2"))))
    (should (equal (plist-get parsed :priority) 2))))

(ert-deftest beads-create-test-parse-transient-args-description ()
  "Test parsing description argument."
  (let ((parsed (beads-create--parse-transient-args '("-d=Test description"))))
    (should (equal (plist-get parsed :description) "Test description"))))

(ert-deftest beads-create-test-parse-transient-args-custom-id ()
  "Test parsing custom ID argument."
  (let ((parsed (beads-create--parse-transient-args '("--id=worker1-100"))))
    (should (equal (plist-get parsed :custom-id) "worker1-100"))))

(ert-deftest beads-create-test-parse-transient-args-dependencies ()
  "Test parsing dependencies argument."
  (let ((parsed (beads-create--parse-transient-args '("--deps=blocks:bd-1"))))
    (should (equal (plist-get parsed :dependencies) "blocks:bd-1"))))

(ert-deftest beads-create-test-parse-transient-args-all ()
  "Test parsing all arguments together."
  (let ((parsed (beads-create--parse-transient-args
                 '("title=Test Issue"
                   "-t=feature"
                   "-p=1"
                   "-d=Full description"
                   "--id=custom-42"
                   "--deps=blocks:bd-1"))))
    (should (equal (plist-get parsed :title) "Test Issue"))
    (should (equal (plist-get parsed :type) "feature"))
    (should (equal (plist-get parsed :priority) 1))
    (should (equal (plist-get parsed :description) "Full description"))
    (should (equal (plist-get parsed :custom-id) "custom-42"))
    (should (equal (plist-get parsed :dependencies) "blocks:bd-1"))))

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
  (should (null (beads-create--validate-dependencies "blocks:bd-1,related:bd-2"))))

(ert-deftest beads-create-test-validate-dependencies-valid-discovered ()
  "Test dependencies validation with discovered-from type."
  (should (null (beads-create--validate-dependencies "discovered-from:bd-10"))))

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
  (let ((parsed (list :title "Valid Title"
                      :type "bug"
                      :priority 1)))
    (should (null (beads-create--validate-all parsed)))))

(ert-deftest beads-create-test-validate-all-multiple-errors ()
  "Test validate-all with multiple validation errors."
  (let ((parsed (list :title ""
                      :type "invalid"
                      :priority 10)))
    (let ((errors (beads-create--validate-all parsed)))
      (should errors)
      (should (listp errors))
      (should (> (length errors) 1)))))

;;; Tests for Command Building

(ert-deftest beads-create-test-build-command-args-minimal ()
  "Test building command args with only title."
  (let ((parsed (list :title "Test Issue")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (equal args '("Test Issue"))))))

(ert-deftest beads-create-test-build-command-args-with-type ()
  "Test building command args with title and type."
  (let ((parsed (list :title "Test Issue" :type "bug")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (equal args '("Test Issue" "-t" "bug"))))))

(ert-deftest beads-create-test-build-command-args-with-priority ()
  "Test building command args with title and priority."
  (let ((parsed (list :title "Test Issue" :priority 1)))
    (let ((args (beads-create--build-command-args parsed)))
      (should (equal args '("Test Issue" "-p" "1"))))))

(ert-deftest beads-create-test-build-command-args-with-description ()
  "Test building command args with description."
  (let ((parsed (list :title "Test Issue" :description "Test description")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (equal args '("Test Issue" "-d" "Test description"))))))

(ert-deftest beads-create-test-build-command-args-with-custom-id ()
  "Test building command args with custom ID."
  (let ((parsed (list :title "Test Issue" :custom-id "worker1-100")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (equal args '("Test Issue" "--id" "worker1-100"))))))

(ert-deftest beads-create-test-build-command-args-with-dependencies ()
  "Test building command args with dependencies."
  (let ((parsed (list :title "Test Issue" :dependencies "blocks:bd-1")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (equal args '("Test Issue" "--deps" "blocks:bd-1"))))))

(ert-deftest beads-create-test-build-command-args-all-options ()
  "Test building command args with all options."
  (let ((parsed (list :title "Test Issue"
                      :type "feature"
                      :priority 2
                      :description "Long description"
                      :custom-id "custom-42"
                      :dependencies "related:bd-10")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (equal args '("Test Issue"
                           "-t" "feature"
                           "-p" "2"
                           "-d" "Long description"
                           "--id" "custom-42"
                           "--deps" "related:bd-10"))))))

(ert-deftest beads-create-test-build-command-args-title-with-spaces ()
  "Test building command args with title containing spaces."
  (let ((parsed (list :title "Test Issue With Spaces")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (equal (car args) "Test Issue With Spaces")))))

(ert-deftest beads-create-test-build-command-args-title-special-chars ()
  "Test building command args with special characters in title."
  (let ((parsed (list :title "Test \"quoted\" & special chars")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (equal (car args) "Test \"quoted\" & special chars")))))

(ert-deftest beads-create-test-build-command-args-multiline-description ()
  "Test building command args with multiline description."
  (let ((parsed (list :title "Test" :description "Line 1\nLine 2\nLine 3")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (member "-d" args))
      (let ((desc (nth (1+ (cl-position "-d" args :test #'equal)) args)))
        (should (string-match-p "\n" desc))))))

(ert-deftest beads-create-test-build-command-args-priority-zero ()
  "Test building command args with priority zero (critical)."
  (let ((parsed (list :title "Critical Issue" :priority 0)))
    (let ((args (beads-create--build-command-args parsed)))
      (should (member "0" args)))))

(ert-deftest beads-create-test-build-command-args-empty-optional-fields ()
  "Test that empty optional fields are not included."
  (let ((parsed (list :title "Test" :description "" :custom-id "  \t\n  " :dependencies "")))
    (let ((args (beads-create--build-command-args parsed)))
      ;; Should only contain title, no flags
      (should (equal args '("Test"))))))

;;; Tests for Execution

(ert-deftest beads-create-test-execute-success ()
  "Test successful issue creation."
  (let ((json-output (json-encode
                      beads-create-test--sample-create-response)))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) (beads-create-test--build-transient-args
                           (list :title "Test Issue" :type "bug" :priority 1))))
              ((symbol-function 'call-process)
               (beads-create-test--mock-call-process 0 json-output))
              ((symbol-function 'y-or-n-p) (lambda (_) nil))
              ((symbol-function 'transient-reset) (lambda () nil)))
      (should-not (beads-create--execute)))))

(ert-deftest beads-create-test-execute-validation-failure ()
  "Test execution fails with validation error."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) (beads-create-test--build-transient-args
                         (list :title "" :type "bug")))))
    (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-missing-title ()
  "Test execution fails when title is missing."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) (beads-create-test--build-transient-args
                         (list :type "bug" :priority 1)))))
    (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-invalid-type ()
  "Test execution fails with invalid type."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) (beads-create-test--build-transient-args
                         (list :title "Test" :type "invalid")))))
    (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-invalid-priority ()
  "Test execution fails with invalid priority."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) (beads-create-test--build-transient-args
                         (list :title "Test" :priority 10)))))
    (should-error (beads-create--execute) :type 'user-error)))

(ert-deftest beads-create-test-execute-command-failure ()
  "Test execution handles bd command failure."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) (beads-create-test--build-transient-args
                         (list :title "Test Issue" :priority 1))))
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
               (lambda (_) (beads-create-test--build-transient-args
                           (list :title "Full Issue"
                                 :type "feature"
                                 :priority 2
                                 :description "Full description"
                                 :custom-id "test-1"
                                 :dependencies "blocks:bd-1"))))
              ((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                               &rest args)
                 (setq captured-args args)
                 (when destination
                   (with-current-buffer (current-buffer)
                     (insert json-output)))
                 0))
              ((symbol-function 'y-or-n-p) (lambda (_) nil))
              ((symbol-function 'transient-reset) (lambda () nil)))
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
             (lambda (_) (beads-create-test--build-transient-args
                         (list :title "Test Issue" :type "bug" :priority 1)))))
    ;; Preview returns a message string
    (should (stringp (beads-create--preview)))))

(ert-deftest beads-create-test-preview-validation-failure ()
  "Test preview shows validation errors."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (_) (beads-create-test--build-transient-args
                         (list :title "" :type "bug")))))
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

(ert-deftest beads-create-test-full-workflow ()
  "Test complete workflow from setting params to creation."
  ;; Build args
  (let ((args (beads-create-test--build-transient-args
               (list :title "Integration Test Issue"
                     :type "task"
                     :priority 3
                     :description "Integration test description"))))
    ;; Parse them back
    (let ((parsed (beads-create--parse-transient-args args)))
      ;; Validate
      (should (null (beads-create--validate-all parsed)))

      ;; Build command
      (let ((cmd-args (beads-create--build-command-args parsed)))
        (should (member "Integration Test Issue" cmd-args))
        (should (member "-t" cmd-args))
        (should (member "task" cmd-args))
        (should (member "-p" cmd-args))
        (should (member "3" cmd-args)))

      ;; Execute (mocked)
      (let ((json-output (json-encode
                          beads-create-test--sample-create-response)))
        (cl-letf (((symbol-function 'transient-args)
                   (lambda (_) args))
                  ((symbol-function 'call-process)
                   (beads-create-test--mock-call-process 0 json-output))
                  ((symbol-function 'y-or-n-p) (lambda (_) nil))
                  ((symbol-function 'transient-reset) (lambda () nil)))
          (should-not (beads-create--execute)))))))

(ert-deftest beads-create-test-with-dependencies ()
  "Test workflow with dependencies specified."
  (let ((args (beads-create-test--build-transient-args
               (list :title "Dependent Issue"
                     :type "bug"
                     :priority 1
                     :dependencies "discovered-from:bd-5"))))
    (let ((parsed (beads-create--parse-transient-args args)))
      ;; Validate
      (should (null (beads-create--validate-all parsed)))

      ;; Build and verify command includes dependencies
      (let ((cmd-args (beads-create--build-command-args parsed)))
        (should (member "--deps" cmd-args))
        (should (member "discovered-from:bd-5" cmd-args)))

      ;; Execute
      (let ((json-output (json-encode
                          beads-create-test--sample-create-response)))
        (cl-letf (((symbol-function 'transient-args)
                   (lambda (_) args))
                  ((symbol-function 'call-process)
                   (beads-create-test--mock-call-process 0 json-output))
                  ((symbol-function 'y-or-n-p) (lambda (_) nil))
                  ((symbol-function 'transient-reset) (lambda () nil)))
          (should-not (beads-create--execute)))))))

;;; Edge Cases

(ert-deftest beads-create-test-edge-case-unicode-title ()
  "Test creating issue with Unicode characters in title."
  (let ((parsed (list :title "测试 Issue with émojis 😀")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (equal (car args) "测试 Issue with émojis 😀")))))

(ert-deftest beads-create-test-edge-case-very-long-title ()
  "Test creating issue with very long title."
  (let ((long-title (make-string 500 ?x)))
    (let ((parsed (list :title long-title)))
      (let ((args (beads-create--build-command-args parsed)))
        (should (equal (car args) long-title))))))

(ert-deftest beads-create-test-edge-case-title-with-newlines ()
  "Test creating issue with newlines in title."
  (let ((parsed (list :title "Title\nWith\nNewlines")))
    ;; This should be valid - bd will handle it
    (should (null (beads-create--validate-title "Title\nWith\nNewlines")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (string-match-p "\n" (car args))))))

(ert-deftest beads-create-test-edge-case-description-with-quotes ()
  "Test creating issue with quotes in description."
  (let ((parsed (list :title "Test"
                      :description "Description with \"quotes\" and 'apostrophes'")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (member "-d" args)))))

(ert-deftest beads-create-test-edge-case-multiple-dependencies ()
  "Test creating issue with multiple dependencies."
  (let ((parsed (list :title "Test"
                      :dependencies "blocks:bd-1,related:bd-2,parent-child:bd-3")))
    (should (null (beads-create--validate-dependencies "blocks:bd-1,related:bd-2,parent-child:bd-3")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (member "--deps" args))
      (let ((deps (nth (1+ (cl-position "--deps" args :test #'equal)) args)))
        (should (string-match-p "blocks:bd-1" deps))
        (should (string-match-p "related:bd-2" deps))
        (should (string-match-p "parent-child:bd-3" deps))))))

(ert-deftest beads-create-test-edge-case-custom-id-with-special-chars ()
  "Test custom ID with special characters."
  (let ((parsed (list :title "Test" :custom-id "worker-1-task-100")))
    (let ((args (beads-create--build-command-args parsed)))
      (should (member "--id" args))
      (should (member "worker-1-task-100" args)))))

;;; Performance Tests

(ert-deftest beads-create-test-performance-command-building ()
  "Test command building performance."
  :tags '(:performance)
  (let ((parsed (list :title "Test"
                      :type "bug"
                      :priority 1
                      :description "Description"
                      :custom-id "test-1"
                      :dependencies "blocks:bd-1")))
    (let ((start-time (current-time)))
      (dotimes (_ 1000)
        (beads-create--build-command-args parsed))
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        ;; Should build 1000 commands in under 0.5 seconds
        (should (< elapsed 0.5))))))

(ert-deftest beads-create-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (let ((parsed (list :title "Test Issue"
                      :type "bug"
                      :priority 1
                      :dependencies "blocks:bd-1")))
    (let ((start-time (current-time)))
      (dotimes (_ 1000)
        (beads-create--validate-all parsed))
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
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
  (let ((parsed (list :title nil)))  ; Invalid state
    (let ((validation-error (beads-create--validate-all parsed)))
      ;; validate-all returns list of errors, not string
      (should validation-error)
      (should (listp validation-error)))))

(ert-deftest beads-create-test-command-building-workflow ()
  "Integration test: Test complete command building workflow."
  :tags '(integration)
  (let ((parsed (list :title "Integration Test Issue"
                      :type "task"
                      :priority 2)))
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

(provide 'beads-transient-create-test)
;;; beads-transient-create-test.el ends here