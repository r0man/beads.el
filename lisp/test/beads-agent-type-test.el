;;; beads-agent-type-test.el --- Tests for beads-agent-type -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-agent-type.el agent type system.
;; Tests cover the base class, generic functions, type registry,
;; and completion support.
;;
;; This test file uses mock subclasses to test the abstract base class
;; without requiring concrete implementations.

;;; Code:

(require 'ert)
(require 'beads-agent-type)
(require 'beads-agent-backend)

;;; Mock Agent Types for Testing

(defclass beads-agent-type-mock (beads-agent-type)
  ((name :initform "Mock")
   (letter :initform "M")
   (description :initform "Mock agent for testing")
   (prompt-template :initform "Mock prompt template"))
  :documentation "Mock agent type for testing base functionality.")

(defclass beads-agent-type-mock-custom (beads-agent-type)
  ((name :initform "Custom")
   (letter :initform "C")
   (description :initform "Custom prompt agent"))
  :documentation "Mock agent type with custom prompt building.")

(cl-defmethod beads-agent-type-build-prompt
    ((_type beads-agent-type-mock-custom) _issue)
  "Custom prompt building for mock-custom type."
  "Custom built prompt")

(defclass beads-agent-type-mock-plan (beads-agent-type)
  ((name :initform "Plan")
   (letter :initform "P")
   (description :initform "Planning agent"))
  :documentation "Mock plan agent type for testing.")

;;; Mock Backend for Validation Tests

(defclass beads-agent-backend-mock-for-type ()
  ((name :initarg :name :initform "mock-backend" :type string)
   (supports-plan :initarg :supports-plan :initform nil :type boolean))
  :documentation "Mock backend for testing type validation.")

;;; Test Fixtures

(defvar beads-agent-type-test--saved-registry nil
  "Saved registry to restore after tests.")

(defun beads-agent-type-test--setup ()
  "Setup test fixtures."
  ;; Save current registry state
  (setq beads-agent-type-test--saved-registry beads-agent-type--registry)
  ;; Clear registry for clean tests
  (beads-agent-type--clear-registry))

(defun beads-agent-type-test--teardown ()
  "Teardown test fixtures."
  ;; Restore saved registry
  (setq beads-agent-type--registry beads-agent-type-test--saved-registry)
  (setq beads-agent-type-test--saved-registry nil))

;;; Tests for Abstract Base Class

(ert-deftest beads-agent-type-test-abstract-class-error ()
  "Test that instantiating abstract beads-agent-type signals error."
  (beads-agent-type-test--setup)
  (unwind-protect
      (should-error
       (beads-agent-type :name "Test" :letter "T")
       :type 'error)
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-concrete-subclass ()
  "Test that concrete subclass can be instantiated."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock)))
        (should (object-of-class-p type 'beads-agent-type))
        (should (object-of-class-p type 'beads-agent-type-mock)))
    (beads-agent-type-test--teardown)))

;;; Tests for Slots

(ert-deftest beads-agent-type-test-slot-name ()
  "Test name slot accessor."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock)))
        (should (equal (oref type name) "Mock")))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-slot-letter ()
  "Test letter slot accessor."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock)))
        (should (equal (oref type letter) "M")))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-slot-description ()
  "Test description slot accessor."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock)))
        (should (equal (oref type description) "Mock agent for testing")))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-slot-prompt-template ()
  "Test prompt-template slot accessor."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock)))
        (should (equal (oref type prompt-template) "Mock prompt template")))
    (beads-agent-type-test--teardown)))


(ert-deftest beads-agent-type-test-slot-initarg ()
  "Test that slots can be set via initargs."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock
                   :name "Override"
                   :letter "O"
                   :description "Overridden description")))
        (should (equal (oref type name) "Override"))
        (should (equal (oref type letter) "O"))
        (should (equal (oref type description) "Overridden description")))
    (beads-agent-type-test--teardown)))

;;; Tests for Generic Functions

(ert-deftest beads-agent-type-test-build-prompt-with-template ()
  "Test default prompt building with template."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock))
            (issue '(:id "test-123"
                     :title "Test Issue"
                     :description "Test description")))
        (let ((prompt (beads-agent-type-build-prompt type issue)))
          (should (stringp prompt))
          (should (string-match "Mock prompt template" prompt))
          (should (string-match "test-123" prompt))
          (should (string-match "Test Issue" prompt))
          (should (string-match "Test description" prompt))))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-build-prompt-nil-template ()
  "Test prompt building returns nil when no template."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock :prompt-template nil))
            (issue '(:id "test-123" :title "Test Issue" :description "")))
        (should (null (beads-agent-type-build-prompt type issue))))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-build-prompt-custom-method ()
  "Test that subclass can override prompt building."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock-custom))
            (issue '(:id "test-123" :title "Test Issue" :description "")))
        (should (equal (beads-agent-type-build-prompt type issue)
                       "Custom built prompt")))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-build-prompt-empty-description ()
  "Test prompt building handles empty description."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock))
            (issue '(:id "test-123" :title "Test Issue")))
        (let ((prompt (beads-agent-type-build-prompt type issue)))
          (should (stringp prompt))
          (should (string-match "Test Issue" prompt))))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-letter-display ()
  "Test letter display generic function."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock)))
        (should (equal (beads-agent-type-letter-display type) "M")))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-name-display ()
  "Test name display generic function."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock)))
        (should (equal (beads-agent-type-name-display type) "Mock")))
    (beads-agent-type-test--teardown)))

;;; Tests for Backend Validation

(ert-deftest beads-agent-type-test-validate-backend-no-plan-required ()
  "Test validation passes when plan mode not required."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock))
            (backend (beads-agent-backend-mock-for-type)))
        (should (beads-agent-type-validate-backend type backend)))
    (beads-agent-type-test--teardown)))

;;; Tests for Type Registry

(ert-deftest beads-agent-type-test-register-and-get ()
  "Test registering and retrieving a type."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock)))
        (beads-agent-type-register type)
        (should (eq (beads-agent-type-get "Mock") type))
        (should (eq (beads-agent-type-get "mock") type))
        (should (eq (beads-agent-type-get "MOCK") type)))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-register-replaces-existing ()
  "Test that registering same name replaces existing type."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type1 (beads-agent-type-mock :description "First"))
            (type2 (beads-agent-type-mock :description "Second")))
        (beads-agent-type-register type1)
        (beads-agent-type-register type2)
        (should (equal (oref (beads-agent-type-get "mock") description)
                       "Second")))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-get-missing-returns-nil ()
  "Test that getting non-existent type returns nil."
  (beads-agent-type-test--setup)
  (unwind-protect
      (should (null (beads-agent-type-get "nonexistent")))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-list-empty ()
  "Test list returns empty when no types registered."
  (beads-agent-type-test--setup)
  (unwind-protect
      (should (null (beads-agent-type-list)))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-list-returns-all ()
  "Test list returns all registered types."
  (beads-agent-type-test--setup)
  (unwind-protect
      (progn
        (beads-agent-type-register (beads-agent-type-mock))
        (beads-agent-type-register (beads-agent-type-mock-custom))
        (beads-agent-type-register (beads-agent-type-mock-plan))
        (let ((types (beads-agent-type-list)))
          (should (= (length types) 3))
          ;; Check sorted by name
          (should (equal (mapcar (lambda (t) (oref t name)) types)
                         '("Custom" "Mock" "Plan")))))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-names ()
  "Test names returns lowercase type names."
  (beads-agent-type-test--setup)
  (unwind-protect
      (progn
        (beads-agent-type-register (beads-agent-type-mock))
        (beads-agent-type-register (beads-agent-type-mock-custom))
        (should (equal (beads-agent-type-names) '("custom" "mock"))))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-clear-registry ()
  "Test clearing the registry."
  (beads-agent-type-test--setup)
  (unwind-protect
      (progn
        (beads-agent-type-register (beads-agent-type-mock))
        (should (beads-agent-type-get "mock"))
        (beads-agent-type--clear-registry)
        (should (null (beads-agent-type-get "mock")))
        (should (null (beads-agent-type-list))))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-register-validates-type ()
  "Test that register validates the type argument."
  (beads-agent-type-test--setup)
  (unwind-protect
      (progn
        (should-error (beads-agent-type-register "not-a-type") :type 'error)
        (should-error (beads-agent-type-register nil) :type 'error)
        (should-error (beads-agent-type-register 123) :type 'error))
    (beads-agent-type-test--teardown)))

;;; Tests for Multiple Type Registration

(ert-deftest beads-agent-type-test-multiple-types-isolated ()
  "Test that multiple types are isolated from each other."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((mock (beads-agent-type-mock))
            (custom (beads-agent-type-mock-custom))
            (plan (beads-agent-type-mock-plan)))
        (beads-agent-type-register mock)
        (beads-agent-type-register custom)
        (beads-agent-type-register plan)
        ;; Each type should be distinct
        (should (eq (beads-agent-type-get "mock") mock))
        (should (eq (beads-agent-type-get "custom") custom))
        (should (eq (beads-agent-type-get "plan") plan))
        ;; Types should have different properties
        (should (equal (oref (beads-agent-type-get "mock") letter) "M"))
        (should (equal (oref (beads-agent-type-get "custom") letter) "C"))
        (should (equal (oref (beads-agent-type-get "plan") letter) "P")))
    (beads-agent-type-test--teardown)))

;;; Tests for Completion Support

(ert-deftest beads-agent-type-test-completion-table ()
  "Test completion table returns callable."
  (beads-agent-type-test--setup)
  (unwind-protect
      (progn
        (beads-agent-type-register (beads-agent-type-mock))
        (beads-agent-type-register (beads-agent-type-mock-custom))
        (let ((table (beads-agent-type-completion-table)))
          (should (functionp table))
          ;; Test completion
          (let ((completions (all-completions "" table)))
            (should (member "Mock" completions))
            (should (member "Custom" completions)))))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-completion-table-metadata ()
  "Test completion table provides metadata."
  (beads-agent-type-test--setup)
  (unwind-protect
      (progn
        (beads-agent-type-register (beads-agent-type-mock))
        (let ((table (beads-agent-type-completion-table)))
          (let ((metadata (funcall table "" nil 'metadata)))
            (should (eq (car metadata) 'metadata))
            (should (assq 'annotation-function (cdr metadata)))
            (should (eq (cdr (assq 'category (cdr metadata)))
                        'beads-agent-type)))))
    (beads-agent-type-test--teardown)))

(ert-deftest beads-agent-type-test-completion-annotation ()
  "Test completion annotations show descriptions."
  (beads-agent-type-test--setup)
  (unwind-protect
      (progn
        (beads-agent-type-register (beads-agent-type-mock))
        (let* ((table (beads-agent-type-completion-table))
               (metadata (funcall table "" nil 'metadata))
               (ann-fn (cdr (assq 'annotation-function (cdr metadata)))))
          (let ((annotation (funcall ann-fn "Mock")))
            (should (stringp annotation))
            (should (string-match "Mock agent for testing" annotation)))))
    (beads-agent-type-test--teardown)))

;;; Tests for Round-Trip Operations

(ert-deftest beads-agent-type-test-round-trip ()
  "Test complete round-trip: register, lookup, use, verify."
  (beads-agent-type-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-mock :description "Round-trip test")))
        ;; Register
        (beads-agent-type-register type)
        ;; Lookup
        (let ((retrieved (beads-agent-type-get "mock")))
          (should (eq retrieved type))
          ;; Use
          (let ((prompt (beads-agent-type-build-prompt
                         retrieved
                         '(:id "rt-1" :title "Round Trip" :description ""))))
            (should (stringp prompt))
            ;; Verify
            (should (string-match "Round Trip" prompt)))))
    (beads-agent-type-test--teardown)))


(provide 'beads-agent-type-test)

;;; beads-agent-type-test.el ends here
