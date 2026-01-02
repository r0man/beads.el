;;; beads-meta-test.el --- Tests for beads-meta.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-meta.el EIEIO slot property infrastructure.

;;; Code:

(require 'ert)
(require 'beads-meta)

;;; ============================================================
;;; Test Classes
;;; ============================================================

;; Define test classes after loading beads-meta so advice is active

(defclass beads-meta-test-base ()
  ((base-slot
    :initarg :base-slot
    :type (or null string)
    :initform nil
    :documentation "Base option"
    :long-option "--base"
    :option-type :string
    :key "b"
    :level 1
    :group "Base"
    :order 1))
  :abstract t
  :documentation "Test base class with custom slot properties.")

(defclass beads-meta-test-child (beads-meta-test-base)
  ((title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Title (required)"
    :positional 1
    :key "t"
    :class transient-option
    :reader beads-reader-issue-title
    :group "Required"
    :level 1
    :order 1
    :required t)
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Priority"
    :long-option "--priority"
    :short-option "-p"
    :option-type :integer
    :key "p"
    :choices (0 1 2 3 4)
    :prompt "Priority: "
    :group "Options"
    :level 2
    :order 1)
   (labels
    :initarg :labels
    :type (or null list)
    :initform nil
    :documentation "Labels"
    :long-option "--labels"
    :option-type :list
    :option-separator ","
    :key "l"
    :group "Options"
    :level 2
    :order 2)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force"
    :long-option "--force"
    :option-type :boolean
    :key "f"
    :class transient-switch
    :group "Flags"
    :level 3
    :order 1)
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Description"
    :positional 2
    :key "d"
    :group "Content"
    :level 3
    :order 1)
   (no-meta-slot
    :initarg :no-meta-slot
    :type (or null string)
    :initform nil
    :documentation "A slot without custom metadata"))
  :documentation "Test child class with various custom slot properties.")

;;; ============================================================
;;; Tests for beads-meta-slot-property
;;; ============================================================

(ert-deftest beads-meta-slot-property-returns-value ()
  "Test that beads-meta-slot-property returns the correct value."
  (should (equal "t"
                 (beads-meta-slot-property 'beads-meta-test-child 'title
                                           :key)))
  (should (equal 1
                 (beads-meta-slot-property 'beads-meta-test-child 'title
                                           :positional)))
  (should (eq t
              (beads-meta-slot-property 'beads-meta-test-child 'title
                                        :required))))

(ert-deftest beads-meta-slot-property-returns-nil-for-missing ()
  "Test that beads-meta-slot-property returns nil for missing properties."
  (should (null (beads-meta-slot-property 'beads-meta-test-child 'title
                                          :long-option)))
  (should (null (beads-meta-slot-property 'beads-meta-test-child 'no-meta-slot
                                          :key)))
  (should (null (beads-meta-slot-property 'beads-meta-test-child 'nonexistent
                                          :key))))

(ert-deftest beads-meta-slot-property-works-with-various-types ()
  "Test that beads-meta-slot-property works with different value types."
  ;; String
  (should (equal "--priority"
                 (beads-meta-slot-property 'beads-meta-test-child 'priority
                                           :long-option)))
  ;; Symbol
  (should (eq :integer
              (beads-meta-slot-property 'beads-meta-test-child 'priority
                                        :option-type)))
  ;; List
  (should (equal '(0 1 2 3 4)
                 (beads-meta-slot-property 'beads-meta-test-child 'priority
                                           :choices)))
  ;; Boolean
  (should (eq :boolean
              (beads-meta-slot-property 'beads-meta-test-child 'force
                                        :option-type))))

;;; ============================================================
;;; Tests for beads-meta-slot-properties
;;; ============================================================

(ert-deftest beads-meta-slot-properties-returns-all-custom-props ()
  "Test that beads-meta-slot-properties returns all custom properties."
  (let ((props (beads-meta-slot-properties 'beads-meta-test-child 'title)))
    (should (assq :positional props))
    (should (assq :key props))
    (should (assq :class props))
    (should (assq :reader props))
    (should (assq :group props))
    (should (assq :level props))
    (should (assq :order props))
    (should (assq :required props))))

(ert-deftest beads-meta-slot-properties-excludes-standard-props ()
  "Test that beads-meta-slot-properties excludes standard EIEIO props."
  (let ((props (beads-meta-slot-properties 'beads-meta-test-child 'title)))
    ;; Standard EIEIO properties should not be included
    (should-not (assq :initarg props))
    (should-not (assq :type props))
    (should-not (assq :initform props))
    (should-not (assq :documentation props))))

(ert-deftest beads-meta-slot-properties-returns-nil-for-slot-without-meta ()
  "Test that beads-meta-slot-properties returns nil for slots without metadata."
  (should (null (beads-meta-slot-properties 'beads-meta-test-child
                                            'no-meta-slot))))

;;; ============================================================
;;; Tests for beads-meta-slots-with-property
;;; ============================================================

(ert-deftest beads-meta-slots-with-property-finds-all-matching ()
  "Test that beads-meta-slots-with-property finds all slots with property."
  (let ((slots (beads-meta-slots-with-property 'beads-meta-test-child
                                               :key)))
    ;; Should find multiple slots with :key
    (should (> (length slots) 3))
    ;; Each result should be (slot-name . value) pair
    (should (assq 'title slots))
    (should (assq 'priority slots))
    (should (assq 'labels slots))
    (should (assq 'force slots))
    (should (assq 'description slots))
    ;; Values should be correct
    (should (equal "t" (cdr (assq 'title slots))))
    (should (equal "p" (cdr (assq 'priority slots))))))

(ert-deftest beads-meta-slots-with-property-returns-nil-for-no-matches ()
  "Test that beads-meta-slots-with-property returns nil when no matches."
  (should (null (beads-meta-slots-with-property 'beads-meta-test-child
                                                :nonexistent-property))))

(ert-deftest beads-meta-slots-with-property-positional ()
  "Test finding positional slots."
  (let ((slots (beads-meta-slots-with-property 'beads-meta-test-child
                                               :positional)))
    (should (= 2 (length slots)))
    (should (assq 'title slots))
    (should (assq 'description slots))
    (should (= 1 (cdr (assq 'title slots))))
    (should (= 2 (cdr (assq 'description slots))))))

;;; ============================================================
;;; Tests for beads-meta-command-slots
;;; ============================================================

(ert-deftest beads-meta-command-slots-returns-all-slots ()
  "Test that beads-meta-command-slots returns all slot names."
  (let ((slots (beads-meta-command-slots 'beads-meta-test-child)))
    ;; Should include inherited slots
    (should (memq 'base-slot slots))
    ;; Should include own slots
    (should (memq 'title slots))
    (should (memq 'priority slots))
    (should (memq 'labels slots))
    (should (memq 'force slots))
    (should (memq 'description slots))
    (should (memq 'no-meta-slot slots))))

;;; ============================================================
;;; Tests for beads-meta-slot-info
;;; ============================================================

(ert-deftest beads-meta-slot-info-returns-complete-info ()
  "Test that beads-meta-slot-info returns complete slot information."
  (let ((info (beads-meta-slot-info 'beads-meta-test-child 'title)))
    (should info)
    (should (eq 'title (plist-get info :name)))
    (should (eq :title (plist-get info :initarg)))
    (should (plist-get info :custom-props))
    ;; Check custom props are included
    (let ((custom (plist-get info :custom-props)))
      (should (assq :positional custom))
      (should (assq :key custom)))))

(ert-deftest beads-meta-slot-info-returns-nil-for-missing-slot ()
  "Test that beads-meta-slot-info returns nil for nonexistent slot."
  (should (null (beads-meta-slot-info 'beads-meta-test-child 'nonexistent))))

;;; ============================================================
;;; Tests for beads-meta-positional-slots
;;; ============================================================

(ert-deftest beads-meta-positional-slots-returns-sorted ()
  "Test that beads-meta-positional-slots returns slots sorted by position."
  (let ((slots (beads-meta-positional-slots 'beads-meta-test-child)))
    (should (= 2 (length slots)))
    ;; First element should be title (position 1)
    (should (eq 'title (car (nth 0 slots))))
    (should (= 1 (cdr (nth 0 slots))))
    ;; Second element should be description (position 2)
    (should (eq 'description (car (nth 1 slots))))
    (should (= 2 (cdr (nth 1 slots))))))

;;; ============================================================
;;; Tests for beads-meta-option-slots
;;; ============================================================

(ert-deftest beads-meta-option-slots-excludes-positional ()
  "Test that beads-meta-option-slots excludes positional arguments."
  (let ((slots (beads-meta-option-slots 'beads-meta-test-child)))
    ;; Should include named options
    (should (memq 'priority slots))
    (should (memq 'labels slots))
    (should (memq 'force slots))
    ;; Should exclude positional slots (title, description don't have
    ;; :long-option, just :positional)
    (should-not (memq 'title slots))
    (should-not (memq 'description slots))
    ;; Should exclude slots without :long-option or :short-option
    (should-not (memq 'no-meta-slot slots))))

;;; ============================================================
;;; Tests for beads-meta-transient-slots
;;; ============================================================

(ert-deftest beads-meta-transient-slots-returns-sorted-by-group-and-order ()
  "Test that beads-meta-transient-slots returns slots sorted correctly."
  (let ((slots (beads-meta-transient-slots 'beads-meta-test-child)))
    ;; Should have multiple slots
    (should (> (length slots) 3))
    ;; All slots with :key should be included
    (should (memq 'title slots))
    (should (memq 'priority slots))
    (should (memq 'labels slots))
    (should (memq 'force slots))
    (should (memq 'description slots))))

;;; ============================================================
;;; Tests for Inheritance
;;; ============================================================

(ert-deftest beads-meta-inheritance-preserves-parent-properties ()
  "Test that custom properties are inherited from parent classes."
  ;; The child class should have access to base-slot's properties
  (should (equal "--base"
                 (beads-meta-slot-property 'beads-meta-test-child 'base-slot
                                           :long-option)))
  (should (equal "b"
                 (beads-meta-slot-property 'beads-meta-test-child 'base-slot
                                           :key)))
  (should (equal "Base"
                 (beads-meta-slot-property 'beads-meta-test-child 'base-slot
                                           :group))))

;;; ============================================================
;;; Tests for beads-meta--slot-properties constant
;;; ============================================================

(ert-deftest beads-meta--slot-properties-contains-all-expected ()
  "Test that beads-meta--slot-properties contains all expected properties."
  ;; CLI properties
  (should (memq :long-option beads-meta--slot-properties))
  (should (memq :short-option beads-meta--slot-properties))
  (should (memq :option-type beads-meta--slot-properties))
  (should (memq :positional beads-meta--slot-properties))
  (should (memq :option-separator beads-meta--slot-properties))
  ;; Transient properties (note: :documentation is standard EIEIO, not custom)
  (should (memq :key beads-meta--slot-properties))
  (should (memq :class beads-meta--slot-properties))
  (should (memq :reader beads-meta--slot-properties))
  (should (memq :choices beads-meta--slot-properties))
  (should (memq :prompt beads-meta--slot-properties))
  (should (memq :level beads-meta--slot-properties))
  (should (memq :group beads-meta--slot-properties))
  (should (memq :order beads-meta--slot-properties))
  ;; Validation properties
  (should (memq :required beads-meta--slot-properties))
  (should (memq :validator beads-meta--slot-properties)))

;;; ============================================================
;;; Tests for beads-meta--format-slot-value
;;; ============================================================

(ert-deftest beads-meta--format-slot-value-string ()
  "Test formatting string values."
  (should (equal "hello" (beads-meta--format-slot-value "hello" :string nil)))
  (should (null (beads-meta--format-slot-value nil :string nil)))
  (should (null (beads-meta--format-slot-value "" :string nil))))

(ert-deftest beads-meta--format-slot-value-boolean ()
  "Test formatting boolean values."
  (should (eq t (beads-meta--format-slot-value t :boolean nil)))
  (should (null (beads-meta--format-slot-value nil :boolean nil))))

(ert-deftest beads-meta--format-slot-value-integer ()
  "Test formatting integer values."
  (should (equal "42" (beads-meta--format-slot-value 42 :integer nil)))
  (should (equal "0" (beads-meta--format-slot-value 0 :integer nil)))
  (should (null (beads-meta--format-slot-value nil :integer nil))))

(ert-deftest beads-meta--format-slot-value-list ()
  "Test formatting list values."
  (should (equal "a,b,c"
                 (beads-meta--format-slot-value '("a" "b" "c") :list nil)))
  (should (equal "a;b;c"
                 (beads-meta--format-slot-value '("a" "b" "c") :list ";")))
  (should (null (beads-meta--format-slot-value nil :list nil)))
  (should (null (beads-meta--format-slot-value '() :list nil))))

;;; ============================================================
;;; Tests for beads-meta-build-command-line
;;; ============================================================

;; Define a test command class for command-line building tests
(defclass beads-meta-test-command ()
  ((title
    :initarg :title
    :type (or null string)
    :initform nil
    :positional 1)
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :long-option "--priority"
    :option-type :integer)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :long-option "--force"
    :option-type :boolean)
   (labels
    :initarg :labels
    :type (or null list)
    :initform nil
    :long-option "--labels"
    :option-type :list
    :option-separator ","))
  :documentation "Test command class for command-line building tests.")

(ert-deftest beads-meta-build-command-line-positional ()
  "Test that positional arguments are placed first."
  (let ((cmd (beads-meta-test-command :title "My Issue")))
    (should (equal '("My Issue")
                   (beads-meta-build-command-line cmd)))))

(ert-deftest beads-meta-build-command-line-named-option ()
  "Test that named options are formatted correctly."
  (let ((cmd (beads-meta-test-command :priority 1)))
    (should (equal '("--priority" "1")
                   (beads-meta-build-command-line cmd)))))

(ert-deftest beads-meta-build-command-line-boolean-flag ()
  "Test that boolean flags are included when truthy."
  (let ((cmd (beads-meta-test-command :force t)))
    (should (equal '("--force")
                   (beads-meta-build-command-line cmd)))))

(ert-deftest beads-meta-build-command-line-boolean-flag-false ()
  "Test that boolean flags are omitted when falsy."
  (let ((cmd (beads-meta-test-command :force nil)))
    (should (null (beads-meta-build-command-line cmd)))))

(ert-deftest beads-meta-build-command-line-list-option ()
  "Test that list options are joined correctly."
  (let ((cmd (beads-meta-test-command :labels '("bug" "urgent"))))
    (should (equal '("--labels" "bug,urgent")
                   (beads-meta-build-command-line cmd)))))

(ert-deftest beads-meta-build-command-line-multiple-options ()
  "Test command line with multiple options."
  (let ((cmd (beads-meta-test-command :title "My Issue"
                                      :priority 1
                                      :force t
                                      :labels '("bug"))))
    (let ((args (beads-meta-build-command-line cmd)))
      ;; Positional should be first
      (should (equal "My Issue" (car args)))
      ;; Named options should follow
      (should (member "--priority" args))
      (should (member "1" args))
      (should (member "--force" args))
      (should (member "--labels" args))
      (should (member "bug" args)))))

(ert-deftest beads-meta-build-command-line-empty ()
  "Test command line with no values."
  (let ((cmd (beads-meta-test-command)))
    (should (null (beads-meta-build-command-line cmd)))))

;;; ============================================================
;;; Tests for beads-meta-build-full-command-line
;;; ============================================================

(ert-deftest beads-meta-build-full-command-line-with-subcommand ()
  "Test that subcommand is prepended correctly."
  (let ((cmd (beads-meta-test-command :title "My Issue")))
    (should (equal '("create" "My Issue")
                   (beads-meta-build-full-command-line cmd "create")))))

;;; ============================================================
;;; Tests for beads-meta-generate-infix-spec
;;; ============================================================

(ert-deftest beads-meta-generate-infix-spec-basic ()
  "Test generating infix spec from slot with basic properties."
  (let ((spec (beads-meta-generate-infix-spec
               'beads-meta-test-child 'title "beads-create")))
    (should spec)
    (should (eq 'beads-create-infix-title (plist-get spec :name)))
    (should (equal "t" (plist-get spec :key)))
    (should (equal "Title (required)" (plist-get spec :description)))
    (should (eq 'transient-option (plist-get spec :class)))))

(ert-deftest beads-meta-generate-infix-spec-with-long-option ()
  "Test generating infix spec with :long-option."
  (let ((spec (beads-meta-generate-infix-spec
               'beads-meta-test-child 'priority "beads-create")))
    (should spec)
    (should (equal "--priority=" (plist-get spec :argument)))
    (should (equal '(0 1 2 3 4) (plist-get spec :choices)))
    (should (equal "Priority: " (plist-get spec :prompt)))))

(ert-deftest beads-meta-generate-infix-spec-boolean ()
  "Test generating infix spec for boolean (switch) type."
  (let ((spec (beads-meta-generate-infix-spec
               'beads-meta-test-child 'force "beads-create")))
    (should spec)
    (should (eq 'transient-switch (plist-get spec :class)))
    (should (equal "--force" (plist-get spec :argument)))))

(ert-deftest beads-meta-generate-infix-spec-explicit-class ()
  "Test that explicit :class overrides default."
  ;; title has :class transient-option explicitly
  (let ((spec (beads-meta-generate-infix-spec
               'beads-meta-test-child 'title "beads-test")))
    (should (eq 'transient-option (plist-get spec :class)))))

(ert-deftest beads-meta-generate-infix-spec-with-reader ()
  "Test generating infix spec with reader function."
  (let ((spec (beads-meta-generate-infix-spec
               'beads-meta-test-child 'title "beads-test")))
    (should (eq 'beads-reader-issue-title (plist-get spec :reader)))))

(ert-deftest beads-meta-generate-infix-spec-nil-for-no-key ()
  "Test that spec is nil for slots without :key."
  (let ((spec (beads-meta-generate-infix-spec
               'beads-meta-test-child 'no-meta-slot "beads-test")))
    (should (null spec))))

;;; ============================================================
;;; Tests for beads-meta-generate-infix-specs
;;; ============================================================

(ert-deftest beads-meta-generate-infix-specs-generates-all ()
  "Test that specs are generated for all slots with :key."
  (let ((specs (beads-meta-generate-infix-specs
                'beads-meta-test-child "beads-test")))
    ;; Should have multiple specs
    (should (> (length specs) 3))
    ;; Each should have a :name
    (should (cl-every (lambda (s) (plist-get s :name)) specs))))

(ert-deftest beads-meta-generate-infix-specs-includes-group-info ()
  "Test that group/level/order info is included in specs."
  (let ((specs (beads-meta-generate-infix-specs
                'beads-meta-test-child "beads-test")))
    (let ((title-spec (cl-find-if
                       (lambda (s)
                         (eq 'beads-test-infix-title (plist-get s :name)))
                       specs)))
      (should title-spec)
      (should (equal "Required" (plist-get title-spec :group)))
      (should (equal 1 (plist-get title-spec :level)))
      (should (equal 1 (plist-get title-spec :order))))))

;;; ============================================================
;;; Tests for beads-meta-group-infix-specs
;;; ============================================================

(ert-deftest beads-meta-group-infix-specs-groups-correctly ()
  "Test that specs are grouped by :group property."
  (let* ((specs (beads-meta-generate-infix-specs
                 'beads-meta-test-child "beads-test"))
         (grouped (beads-meta-group-infix-specs specs)))
    ;; Should have multiple groups
    (should (> (length grouped) 1))
    ;; Each entry should be (GROUP-NAME . SPECS)
    (should (cl-every (lambda (g)
                        (and (stringp (car g))
                             (listp (cdr g))))
                      grouped))))

(ert-deftest beads-meta-group-infix-specs-sorted-by-level ()
  "Test that groups are sorted by minimum level."
  (let* ((specs (beads-meta-generate-infix-specs
                 'beads-meta-test-child "beads-test"))
         (grouped (beads-meta-group-infix-specs specs))
         (group-names (mapcar #'car grouped)))
    ;; "Required" (level 1) should come before "Options" (level 2)
    ;; and "Flags" (level 3)
    (let ((required-pos (cl-position "Required" group-names :test #'equal))
          (options-pos (cl-position "Options" group-names :test #'equal))
          (flags-pos (cl-position "Flags" group-names :test #'equal)))
      (when (and required-pos options-pos)
        (should (< required-pos options-pos)))
      (when (and options-pos flags-pos)
        (should (< options-pos flags-pos))))))

;;; ============================================================
;;; Tests for beads-meta--normalize-group-name
;;; ============================================================

(ert-deftest beads-meta--normalize-group-name-converts-spaces ()
  "Test that spaces are converted to dashes."
  (should (equal "issue-attributes"
                 (beads-meta--normalize-group-name "Issue attributes")))
  (should (equal "required"
                 (beads-meta--normalize-group-name "Required"))))

;;; ============================================================
;;; Tests for beads-meta-generate-group-spec
;;; ============================================================

(ert-deftest beads-meta-generate-group-spec-basic ()
  "Test generating a group spec."
  (let* ((infix-specs (beads-meta-generate-infix-specs
                       'beads-meta-test-child "beads-test"))
         (grouped (beads-meta-group-infix-specs infix-specs))
         (required-specs (cdr (assoc "Required" grouped)))
         (spec (beads-meta-generate-group-spec
                "Required" required-specs "beads-test")))
    (should spec)
    (should (eq 'beads-test--required-section (plist-get spec :name)))
    (should (equal 1 (plist-get spec :level)))
    (should (equal "Required" (plist-get spec :description)))
    (should (listp (plist-get spec :infixes)))))

;;; ============================================================
;;; Tests for beads-meta-generate-group-specs
;;; ============================================================

(ert-deftest beads-meta-generate-group-specs-generates-all ()
  "Test that group specs are generated for all groups."
  (let ((specs (beads-meta-generate-group-specs
                'beads-meta-test-child "beads-test")))
    ;; Should have multiple groups
    (should (> (length specs) 1))
    ;; Each should have required fields
    (should (cl-every (lambda (s)
                        (and (plist-get s :name)
                             (plist-get s :level)
                             (plist-get s :description)
                             (plist-get s :infixes)))
                      specs))))

;;; ============================================================
;;; Tests for beads-meta-generate-prefix-spec
;;; ============================================================

(ert-deftest beads-meta-generate-prefix-spec-basic ()
  "Test generating a prefix spec."
  (let ((spec (beads-meta-generate-prefix-spec
               'beads-meta-test-child "beads-test" "Test menu")))
    (should spec)
    (should (eq 'beads-test (plist-get spec :name)))
    (should (equal "Test menu" (plist-get spec :docstring)))
    (should (listp (plist-get spec :groups)))
    (should (> (length (plist-get spec :groups)) 0))))

;;; ============================================================
;;; Tests for beads-meta-generate-parse-function-name
;;; ============================================================

(ert-deftest beads-meta-generate-parse-function-name-correct ()
  "Test that parse function name is generated correctly."
  (should (eq 'beads-create--parse-transient-args
              (beads-meta-generate-parse-function-name "beads-create"))))

;;; ============================================================
;;; Tests for beads-command-create Slot Properties
;;; ============================================================

;; These tests verify that beads-command-create has correct slot properties
;; after migration to use beads-meta.

(require 'beads-command)

(ert-deftest beads-meta-create-slot-property-title ()
  "Test that title slot has correct metadata."
  ;; Title is positional
  (should (equal 1 (beads-meta-slot-property
                    'beads-command-create 'title :positional)))
  ;; Transient properties
  (should (equal "t" (beads-meta-slot-property
                      'beads-command-create 'title :key)))
  ;; Note: :description is no longer a custom property - use :documentation
  (should (equal "Required" (beads-meta-slot-property
                             'beads-command-create 'title :group)))
  (should (eq t (beads-meta-slot-property
                 'beads-command-create 'title :required))))

(ert-deftest beads-meta-create-slot-property-priority ()
  "Test that priority slot has correct metadata."
  ;; CLI properties
  (should (equal "--priority" (beads-meta-slot-property
                               'beads-command-create 'priority :long-option)))
  (should (equal "-p" (beads-meta-slot-property
                       'beads-command-create 'priority :short-option)))
  ;; Transient properties
  (should (equal "-p" (beads-meta-slot-property
                       'beads-command-create 'priority :key)))
  (should (equal "Issue attributes" (beads-meta-slot-property
                                     'beads-command-create 'priority
                                     :group))))

(ert-deftest beads-meta-create-slot-property-issue-type ()
  "Test that issue-type slot has correct metadata."
  ;; CLI properties
  (should (equal "--type" (beads-meta-slot-property
                           'beads-command-create 'issue-type :long-option)))
  (should (equal "-t" (beads-meta-slot-property
                       'beads-command-create 'issue-type :short-option)))
  ;; Transient choices
  (should (equal '("bug" "feature" "task" "epic" "chore")
                 (beads-meta-slot-property
                  'beads-command-create 'issue-type :choices))))

(ert-deftest beads-meta-create-slot-property-force ()
  "Test that force slot (boolean) has correct metadata."
  ;; CLI properties
  (should (equal "--force" (beads-meta-slot-property
                            'beads-command-create 'force :long-option)))
  (should (eq :boolean (beads-meta-slot-property
                        'beads-command-create 'force :option-type)))
  ;; Transient properties
  (should (eq 'transient-switch (beads-meta-slot-property
                                 'beads-command-create 'force :class))))

(ert-deftest beads-meta-create-slot-property-labels ()
  "Test that labels slot (list) has correct metadata."
  ;; CLI properties
  (should (equal "--labels" (beads-meta-slot-property
                             'beads-command-create 'labels :long-option)))
  (should (eq :list (beads-meta-slot-property
                     'beads-command-create 'labels :option-type)))
  (should (equal "," (beads-meta-slot-property
                      'beads-command-create 'labels :option-separator))))

(ert-deftest beads-meta-create-slot-property-deps ()
  "Test that deps slot (list) has correct metadata."
  ;; CLI properties
  (should (equal "--deps" (beads-meta-slot-property
                           'beads-command-create 'deps :long-option)))
  (should (eq :list (beads-meta-slot-property
                     'beads-command-create 'deps :option-type)))
  (should (equal "," (beads-meta-slot-property
                      'beads-command-create 'deps :option-separator))))

(ert-deftest beads-meta-create-transient-slots ()
  "Test that all expected slots have transient keys."
  (let ((slots (beads-meta-transient-slots 'beads-command-create)))
    ;; Should have all the transient-enabled slots
    (should (memq 'title slots))
    (should (memq 'priority slots))
    (should (memq 'issue-type slots))
    (should (memq 'description slots))
    (should (memq 'assignee slots))
    (should (memq 'labels slots))
    (should (memq 'deps slots))
    (should (memq 'force slots))))

(ert-deftest beads-meta-create-positional-slots ()
  "Test that title is correctly identified as positional."
  (let ((positionals (beads-meta-positional-slots 'beads-command-create)))
    ;; Should have title as positional 1
    (should (= 1 (length positionals)))
    (should (eq 'title (caar positionals)))
    (should (= 1 (cdar positionals)))))

(ert-deftest beads-meta-create-option-slots ()
  "Test that non-positional CLI options are identified."
  (let ((options (beads-meta-option-slots 'beads-command-create)))
    ;; Should include various options
    (should (memq 'priority options))
    (should (memq 'issue-type options))
    (should (memq 'assignee options))
    (should (memq 'labels options))
    (should (memq 'deps options))
    (should (memq 'force options))
    ;; Should NOT include title (it's positional)
    (should-not (memq 'title options))))

(ert-deftest beads-meta-create-generate-infix-specs ()
  "Test that infix specs can be generated from beads-command-create."
  (let ((specs (beads-meta-generate-infix-specs
                'beads-command-create "beads-create")))
    ;; Should have specs for all transient-enabled slots
    (should (> (length specs) 10))
    ;; Check for specific infixes
    (let ((title-spec (cl-find-if
                       (lambda (s)
                         (eq 'beads-create-infix-title (plist-get s :name)))
                       specs)))
      (should title-spec)
      (should (equal "t" (plist-get title-spec :key))))))

(ert-deftest beads-meta-create-generate-group-specs ()
  "Test that group specs can be generated from beads-command-create."
  (let ((groups (beads-meta-generate-group-specs
                 'beads-command-create "beads-create")))
    ;; Should have multiple groups
    (should (>= (length groups) 4))
    ;; Check for expected groups
    (let ((group-names (mapcar (lambda (g) (plist-get g :description)) groups)))
      (should (member "Required" group-names))
      (should (member "Issue attributes" group-names))
      (should (member "Content" group-names))
      (should (member "Advanced" group-names)))))

;;; ============================================================
;;; Tests for beads-command-update Slot Properties
;;; ============================================================

;; These tests verify that beads-command-update has correct slot properties
;; after migration to use beads-meta.

(ert-deftest beads-meta-update-slot-property-status ()
  "Test that status slot has correct metadata."
  ;; CLI properties
  (should (equal "--status" (beads-meta-slot-property
                             'beads-command-update 'status :long-option)))
  (should (equal "-s" (beads-meta-slot-property
                       'beads-command-update 'status :short-option)))
  (should (eq :string (beads-meta-slot-property
                       'beads-command-update 'status :option-type)))
  ;; Transient properties
  (should (equal "s" (beads-meta-slot-property
                      'beads-command-update 'status :key)))
  (should (eq 'transient-option (beads-meta-slot-property
                                 'beads-command-update 'status :class)))
  (should (equal "--status=" (beads-meta-slot-property
                              'beads-command-update 'status :argument)))
  (should (equal '("open" "in_progress" "blocked" "closed")
                 (beads-meta-slot-property
                  'beads-command-update 'status :choices)))
  (should (equal "Status & Priority" (beads-meta-slot-property
                                      'beads-command-update 'status
                                      :group)))
  (should (equal 1 (beads-meta-slot-property
                    'beads-command-update 'status :level)))
  (should (equal 1 (beads-meta-slot-property
                    'beads-command-update 'status :order))))

(ert-deftest beads-meta-update-slot-property-priority ()
  "Test that priority slot has correct metadata."
  ;; CLI properties
  (should (equal "--priority" (beads-meta-slot-property
                               'beads-command-update 'priority :long-option)))
  (should (equal "-p" (beads-meta-slot-property
                       'beads-command-update 'priority :short-option)))
  ;; Transient properties
  (should (equal "p" (beads-meta-slot-property
                      'beads-command-update 'priority :key)))
  (should (equal "Status & Priority" (beads-meta-slot-property
                                      'beads-command-update 'priority
                                      :group))))

(ert-deftest beads-meta-update-slot-property-title ()
  "Test that title slot has correct metadata."
  ;; CLI properties
  (should (equal "--title" (beads-meta-slot-property
                            'beads-command-update 'title :long-option)))
  ;; Transient properties
  (should (equal "t" (beads-meta-slot-property
                      'beads-command-update 'title :key)))
  (should (equal "Basic Info" (beads-meta-slot-property
                               'beads-command-update 'title :group))))

(ert-deftest beads-meta-update-slot-property-assignee ()
  "Test that assignee slot has correct metadata."
  ;; CLI properties
  (should (equal "--assignee" (beads-meta-slot-property
                               'beads-command-update 'assignee :long-option)))
  (should (equal "-a" (beads-meta-slot-property
                       'beads-command-update 'assignee :short-option)))
  ;; Transient properties
  (should (equal "a" (beads-meta-slot-property
                      'beads-command-update 'assignee :key)))
  (should (equal "Basic Info" (beads-meta-slot-property
                               'beads-command-update 'assignee :group))))

(ert-deftest beads-meta-update-slot-property-external-ref ()
  "Test that external-ref slot has correct metadata."
  ;; CLI properties
  (should (equal "--external-ref" (beads-meta-slot-property
                                   'beads-command-update 'external-ref
                                   :long-option)))
  ;; Transient properties
  (should (equal "x" (beads-meta-slot-property
                      'beads-command-update 'external-ref :key)))
  (should (equal "Basic Info" (beads-meta-slot-property
                               'beads-command-update 'external-ref
                               :group))))

(ert-deftest beads-meta-update-slot-property-description ()
  "Test that description slot has correct metadata."
  ;; CLI properties
  (should (equal "--description" (beads-meta-slot-property
                                  'beads-command-update 'description
                                  :long-option)))
  (should (equal "-d" (beads-meta-slot-property
                       'beads-command-update 'description :short-option)))
  ;; Transient properties
  (should (equal "d" (beads-meta-slot-property
                      'beads-command-update 'description :key)))
  (should (eq 'beads-create-transient-multiline
              (beads-meta-slot-property
               'beads-command-update 'description :class)))
  (should (equal "Description" (beads-meta-slot-property
                                'beads-command-update 'description
                                :field-name)))
  (should (equal "Content" (beads-meta-slot-property
                            'beads-command-update 'description
                            :group))))

(ert-deftest beads-meta-update-slot-property-acceptance ()
  "Test that acceptance slot has correct metadata."
  ;; CLI properties
  (should (equal "--acceptance" (beads-meta-slot-property
                                 'beads-command-update 'acceptance
                                 :long-option)))
  ;; Transient properties
  (should (equal "A" (beads-meta-slot-property
                      'beads-command-update 'acceptance :key)))
  (should (eq 'beads-create-transient-multiline
              (beads-meta-slot-property
               'beads-command-update 'acceptance :class)))
  (should (equal "Acceptance Criteria" (beads-meta-slot-property
                                        'beads-command-update 'acceptance
                                        :field-name))))

(ert-deftest beads-meta-update-slot-property-design ()
  "Test that design slot has correct metadata."
  ;; CLI properties
  (should (equal "--design" (beads-meta-slot-property
                             'beads-command-update 'design :long-option)))
  ;; Transient properties
  (should (equal "G" (beads-meta-slot-property
                      'beads-command-update 'design :key)))
  (should (eq 'beads-create-transient-multiline
              (beads-meta-slot-property
               'beads-command-update 'design :class)))
  (should (equal "Design" (beads-meta-slot-property
                           'beads-command-update 'design :field-name))))

(ert-deftest beads-meta-update-slot-property-notes ()
  "Test that notes slot has correct metadata."
  ;; CLI properties
  (should (equal "--notes" (beads-meta-slot-property
                            'beads-command-update 'notes :long-option)))
  ;; Transient properties
  (should (equal "N" (beads-meta-slot-property
                      'beads-command-update 'notes :key)))
  (should (eq 'beads-create-transient-multiline
              (beads-meta-slot-property
               'beads-command-update 'notes :class)))
  (should (equal "Notes" (beads-meta-slot-property
                          'beads-command-update 'notes :field-name))))

(ert-deftest beads-meta-update-transient-slots ()
  "Test that all expected slots have transient keys."
  (let ((slots (beads-meta-transient-slots 'beads-command-update)))
    ;; Should have all the transient-enabled slots
    (should (memq 'status slots))
    (should (memq 'priority slots))
    (should (memq 'title slots))
    (should (memq 'assignee slots))
    (should (memq 'external-ref slots))
    (should (memq 'description slots))
    (should (memq 'acceptance slots))
    (should (memq 'design slots))
    (should (memq 'notes slots))))

(ert-deftest beads-meta-update-option-slots ()
  "Test that non-positional CLI options are identified."
  (let ((options (beads-meta-option-slots 'beads-command-update)))
    ;; Should include various options
    (should (memq 'status options))
    (should (memq 'priority options))
    (should (memq 'title options))
    (should (memq 'assignee options))
    (should (memq 'external-ref options))
    (should (memq 'description options))
    (should (memq 'acceptance options))
    (should (memq 'design options))
    (should (memq 'notes options))
    ;; Should NOT include issue-ids (no :long-option)
    (should-not (memq 'issue-ids options))))

(ert-deftest beads-meta-update-generate-infix-specs ()
  "Test that infix specs can be generated from beads-command-update."
  (let ((specs (beads-meta-generate-infix-specs
                'beads-command-update "beads-update")))
    ;; Should have specs for all transient-enabled slots
    (should (>= (length specs) 9))
    ;; Check for specific infixes
    (let ((status-spec (cl-find-if
                        (lambda (s)
                          (eq 'beads-update-infix-status (plist-get s :name)))
                        specs)))
      (should status-spec)
      (should (equal "s" (plist-get status-spec :key))))))

(ert-deftest beads-meta-update-generate-group-specs ()
  "Test that group specs can be generated from beads-command-update."
  (let ((groups (beads-meta-generate-group-specs
                 'beads-command-update "beads-update")))
    ;; Should have multiple groups
    (should (>= (length groups) 3))
    ;; Check for expected groups
    (let ((group-names (mapcar (lambda (g) (plist-get g :description)) groups)))
      (should (member "Status & Priority" group-names))
      (should (member "Basic Info" group-names))
      (should (member "Content" group-names)))))

;;; ============================================================
;;; Tests for beads-command-close Slot Properties
;;; ============================================================

;; These tests verify that beads-command-close has correct slot properties
;; after migration to use beads-meta.

(ert-deftest beads-meta-close-slot-property-issue-ids ()
  "Test that issue-ids slot has correct metadata."
  ;; Transient properties
  (should (equal "i" (beads-meta-slot-property
                      'beads-command-close 'issue-ids :key)))
  (should (eq 'transient-option (beads-meta-slot-property
                                 'beads-command-close 'issue-ids
                                 :class)))
  (should (equal "--id=" (beads-meta-slot-property
                          'beads-command-close 'issue-ids :argument)))
  (should (equal "Issue ID: " (beads-meta-slot-property
                               'beads-command-close 'issue-ids
                               :prompt)))
  (should (eq 'beads-reader-close-issue-id (beads-meta-slot-property
                                            'beads-command-close 'issue-ids
                                            :reader)))
  (should (equal "Close Issue" (beads-meta-slot-property
                                'beads-command-close 'issue-ids
                                :group)))
  (should (equal 1 (beads-meta-slot-property
                    'beads-command-close 'issue-ids :level)))
  (should (equal 1 (beads-meta-slot-property
                    'beads-command-close 'issue-ids :order)))
  ;; Validation
  (should (eq t (beads-meta-slot-property
                 'beads-command-close 'issue-ids :required))))

(ert-deftest beads-meta-close-slot-property-reason ()
  "Test that reason slot has correct metadata."
  ;; CLI properties
  (should (equal "--reason" (beads-meta-slot-property
                             'beads-command-close 'reason :long-option)))
  (should (equal "-r" (beads-meta-slot-property
                       'beads-command-close 'reason :short-option)))
  (should (eq :string (beads-meta-slot-property
                       'beads-command-close 'reason :option-type)))
  ;; Transient properties
  (should (equal "r" (beads-meta-slot-property
                      'beads-command-close 'reason :key)))
  (should (eq 'beads-create-transient-multiline (beads-meta-slot-property
                                                 'beads-command-close 'reason
                                                 :class)))
  (should (equal "--reason=" (beads-meta-slot-property
                              'beads-command-close 'reason :argument)))
  (should (equal "Close Reason" (beads-meta-slot-property
                                 'beads-command-close 'reason
                                 :field-name)))
  (should (equal "Close Issue" (beads-meta-slot-property
                                'beads-command-close 'reason :group)))
  (should (equal 1 (beads-meta-slot-property
                    'beads-command-close 'reason :level)))
  (should (equal 2 (beads-meta-slot-property
                    'beads-command-close 'reason :order)))
  ;; Validation
  (should (eq t (beads-meta-slot-property
                 'beads-command-close 'reason :required))))

(ert-deftest beads-meta-close-transient-slots ()
  "Test that all expected slots have transient keys."
  (let ((slots (beads-meta-transient-slots 'beads-command-close)))
    ;; Should have both transient-enabled slots
    (should (memq 'issue-ids slots))
    (should (memq 'reason slots))
    ;; Should have exactly 2 slots with transient keys
    (should (= 2 (length slots)))))

(ert-deftest beads-meta-close-option-slots ()
  "Test that option slots are identified correctly."
  (let ((options (beads-meta-option-slots 'beads-command-close)))
    ;; Should include reason (has :long-option)
    (should (memq 'reason options))
    ;; Should NOT include issue-ids (no :long-option, no :positional)
    (should-not (memq 'issue-ids options))))

(ert-deftest beads-meta-close-generate-infix-specs ()
  "Test that infix specs can be generated from beads-command-close."
  (let ((specs (beads-meta-generate-infix-specs
                'beads-command-close "beads-close")))
    ;; Should have specs for both transient-enabled slots
    (should (= 2 (length specs)))
    ;; Check for specific infixes
    (let ((issue-ids-spec (cl-find-if
                           (lambda (s)
                             (eq 'beads-close-infix-issue-ids (plist-get s :name)))
                           specs)))
      (should issue-ids-spec)
      (should (equal "i" (plist-get issue-ids-spec :key))))
    (let ((reason-spec (cl-find-if
                        (lambda (s)
                          (eq 'beads-close-infix-reason (plist-get s :name)))
                        specs)))
      (should reason-spec)
      (should (equal "r" (plist-get reason-spec :key))))))

(ert-deftest beads-meta-close-generate-group-specs ()
  "Test that group specs can be generated from beads-command-close."
  (let ((groups (beads-meta-generate-group-specs
                 'beads-command-close "beads-close")))
    ;; Should have exactly one group (Close Issue)
    (should (= 1 (length groups)))
    ;; Check for expected group
    (let ((group-names (mapcar (lambda (g) (plist-get g :description)) groups)))
      (should (member "Close Issue" group-names)))))

;;; ============================================================
;;; Tests for beads-command-show Slot Properties
;;; ============================================================

;; These tests verify that beads-command-show has correct slot properties
;; after migration to use beads-meta.

(ert-deftest beads-meta-show-slot-property-issue-ids ()
  "Test that issue-ids slot has correct metadata."
  ;; Transient properties
  (should (equal "i" (beads-meta-slot-property
                      'beads-command-show 'issue-ids :key)))
  (should (eq 'transient-option (beads-meta-slot-property
                                 'beads-command-show 'issue-ids
                                 :class)))
  (should (equal "--id=" (beads-meta-slot-property
                          'beads-command-show 'issue-ids :argument)))
  (should (equal "Issue ID: " (beads-meta-slot-property
                               'beads-command-show 'issue-ids
                               :prompt)))
  (should (eq 'beads-reader-issue-id (beads-meta-slot-property
                                      'beads-command-show 'issue-ids
                                      :reader)))
  (should (equal "Show Issue" (beads-meta-slot-property
                               'beads-command-show 'issue-ids
                               :group)))
  (should (equal 1 (beads-meta-slot-property
                    'beads-command-show 'issue-ids :level)))
  (should (equal 1 (beads-meta-slot-property
                    'beads-command-show 'issue-ids :order)))
  ;; Validation
  (should (eq t (beads-meta-slot-property
                 'beads-command-show 'issue-ids :required))))

(ert-deftest beads-meta-show-transient-slots ()
  "Test that all expected slots have transient keys."
  (let ((slots (beads-meta-transient-slots 'beads-command-show)))
    ;; Should have only issue-ids slot with transient key
    (should (memq 'issue-ids slots))
    ;; Should have exactly 1 slot with transient key
    (should (= 1 (length slots)))))

(ert-deftest beads-meta-show-option-slots ()
  "Test that option slots are identified correctly."
  (let ((options (beads-meta-option-slots 'beads-command-show)))
    ;; Should NOT include issue-ids (no :long-option, no :positional)
    (should-not (memq 'issue-ids options))
    ;; Should be empty since issue-ids is the only slot
    (should (null options))))

(ert-deftest beads-meta-show-generate-infix-specs ()
  "Test that infix specs can be generated from beads-command-show."
  (let ((specs (beads-meta-generate-infix-specs
                'beads-command-show "beads-show")))
    ;; Should have spec for issue-ids slot
    (should (= 1 (length specs)))
    ;; Check for specific infix
    (let ((issue-ids-spec (cl-find-if
                           (lambda (s)
                             (eq 'beads-show-infix-issue-ids (plist-get s :name)))
                           specs)))
      (should issue-ids-spec)
      (should (equal "i" (plist-get issue-ids-spec :key))))))

(ert-deftest beads-meta-show-generate-group-specs ()
  "Test that group specs can be generated from beads-command-show."
  (let ((groups (beads-meta-generate-group-specs
                 'beads-command-show "beads-show")))
    ;; Should have exactly one group (Show Issue)
    (should (= 1 (length groups)))
    ;; Check for expected group
    (let ((group-names (mapcar (lambda (g) (plist-get g :description)) groups)))
      (should (member "Show Issue" group-names)))))

;;; ============================================================
;;; Tests for beads-command-list Slot Properties
;;; ============================================================

;; These tests verify that beads-command-list has correct slot properties
;; after migration to use beads-meta.

(ert-deftest beads-meta-list-slot-property-status ()
  "Test that status slot has correct metadata."
  ;; CLI properties
  (should (equal "--status" (beads-meta-slot-property
                             'beads-command-list 'status :long-option)))
  (should (equal "-s" (beads-meta-slot-property
                       'beads-command-list 'status :short-option)))
  (should (eq :string (beads-meta-slot-property
                       'beads-command-list 'status :option-type)))
  ;; Transient properties
  (should (equal "-s" (beads-meta-slot-property
                       'beads-command-list 'status :key)))
  (should (eq 'transient-option (beads-meta-slot-property
                                 'beads-command-list 'status :class)))
  (should (equal "--status=" (beads-meta-slot-property
                              'beads-command-list 'status :argument)))
  (should (equal "Basic Filters" (beads-meta-slot-property
                                  'beads-command-list 'status :group)))
  (should (equal 1 (beads-meta-slot-property
                    'beads-command-list 'status :level)))
  (should (equal 1 (beads-meta-slot-property
                    'beads-command-list 'status :order))))

(ert-deftest beads-meta-list-slot-property-priority ()
  "Test that priority slot has correct metadata."
  ;; CLI properties
  (should (equal "--priority" (beads-meta-slot-property
                               'beads-command-list 'priority :long-option)))
  (should (equal "-p" (beads-meta-slot-property
                       'beads-command-list 'priority :short-option)))
  (should (eq :integer (beads-meta-slot-property
                        'beads-command-list 'priority :option-type)))
  ;; Transient properties
  (should (equal "-P" (beads-meta-slot-property
                       'beads-command-list 'priority :key)))
  (should (equal "Basic Filters" (beads-meta-slot-property
                                  'beads-command-list 'priority :group))))

(ert-deftest beads-meta-list-slot-property-issue-type ()
  "Test that issue-type slot has correct metadata."
  ;; CLI properties
  (should (equal "--type" (beads-meta-slot-property
                           'beads-command-list 'issue-type :long-option)))
  (should (equal "-t" (beads-meta-slot-property
                       'beads-command-list 'issue-type :short-option)))
  (should (eq :string (beads-meta-slot-property
                       'beads-command-list 'issue-type :option-type)))
  ;; Transient properties
  (should (equal "-T" (beads-meta-slot-property
                       'beads-command-list 'issue-type :key)))
  (should (equal "Basic Filters" (beads-meta-slot-property
                                  'beads-command-list 'issue-type :group))))

(ert-deftest beads-meta-list-slot-property-assignee ()
  "Test that assignee slot has correct metadata."
  ;; CLI properties
  (should (equal "--assignee" (beads-meta-slot-property
                               'beads-command-list 'assignee :long-option)))
  (should (equal "-a" (beads-meta-slot-property
                       'beads-command-list 'assignee :short-option)))
  ;; Transient properties
  (should (equal "-a" (beads-meta-slot-property
                       'beads-command-list 'assignee :key)))
  (should (equal "Basic Filters" (beads-meta-slot-property
                                  'beads-command-list 'assignee :group))))

(ert-deftest beads-meta-list-slot-property-title ()
  "Test that title slot has correct metadata."
  ;; CLI properties
  (should (equal "--title" (beads-meta-slot-property
                            'beads-command-list 'title :long-option)))
  (should (eq :string (beads-meta-slot-property
                       'beads-command-list 'title :option-type)))
  ;; Transient properties
  (should (equal "-ti" (beads-meta-slot-property
                        'beads-command-list 'title :key)))
  (should (equal "Text Search" (beads-meta-slot-property
                                'beads-command-list 'title :group))))

(ert-deftest beads-meta-list-slot-property-desc-contains ()
  "Test that desc-contains slot has correct metadata."
  ;; CLI properties
  (should (equal "--desc-contains" (beads-meta-slot-property
                                    'beads-command-list 'desc-contains
                                    :long-option)))
  ;; Transient properties
  (should (equal "-d" (beads-meta-slot-property
                       'beads-command-list 'desc-contains :key)))
  (should (equal "Text Search" (beads-meta-slot-property
                                'beads-command-list 'desc-contains
                                :group))))

(ert-deftest beads-meta-list-slot-property-created-after ()
  "Test that created-after slot has correct metadata."
  ;; CLI properties
  (should (equal "--created-after" (beads-meta-slot-property
                                    'beads-command-list 'created-after
                                    :long-option)))
  ;; Transient properties
  (should (equal "-Ca" (beads-meta-slot-property
                        'beads-command-list 'created-after :key)))
  (should (equal "Date Filters" (beads-meta-slot-property
                                 'beads-command-list 'created-after
                                 :group)))
  (should (equal 3 (beads-meta-slot-property
                    'beads-command-list 'created-after :level))))

(ert-deftest beads-meta-list-slot-property-priority-min ()
  "Test that priority-min slot has correct metadata."
  ;; CLI properties
  (should (equal "--priority-min" (beads-meta-slot-property
                                   'beads-command-list 'priority-min
                                   :long-option)))
  (should (eq :integer (beads-meta-slot-property
                        'beads-command-list 'priority-min :option-type)))
  ;; Transient properties
  (should (equal "-p<" (beads-meta-slot-property
                        'beads-command-list 'priority-min :key)))
  (should (equal "Advanced Filters" (beads-meta-slot-property
                                     'beads-command-list 'priority-min
                                     :group)))
  (should (equal 4 (beads-meta-slot-property
                    'beads-command-list 'priority-min :level))))

(ert-deftest beads-meta-list-slot-property-label ()
  "Test that label slot (list) has correct metadata."
  ;; CLI properties
  (should (equal "--label" (beads-meta-slot-property
                            'beads-command-list 'label :long-option)))
  (should (equal "-l" (beads-meta-slot-property
                       'beads-command-list 'label :short-option)))
  (should (eq :list (beads-meta-slot-property
                     'beads-command-list 'label :option-type)))
  ;; Transient properties
  (should (equal "-l" (beads-meta-slot-property
                       'beads-command-list 'label :key)))
  (should (equal "Advanced Filters" (beads-meta-slot-property
                                     'beads-command-list 'label :group))))

(ert-deftest beads-meta-list-slot-property-no-assignee ()
  "Test that no-assignee slot (boolean) has correct metadata."
  ;; CLI properties
  (should (equal "--no-assignee" (beads-meta-slot-property
                                  'beads-command-list 'no-assignee :long-option)))
  (should (eq :boolean (beads-meta-slot-property
                        'beads-command-list 'no-assignee :option-type)))
  ;; Transient properties
  (should (equal "-A" (beads-meta-slot-property
                       'beads-command-list 'no-assignee :key)))
  (should (eq 'transient-switch (beads-meta-slot-property
                                 'beads-command-list 'no-assignee
                                 :class))))

(ert-deftest beads-meta-list-slot-property-limit ()
  "Test that limit slot has correct metadata."
  ;; CLI properties
  (should (equal "--limit" (beads-meta-slot-property
                            'beads-command-list 'limit :long-option)))
  (should (equal "-n" (beads-meta-slot-property
                       'beads-command-list 'limit :short-option)))
  (should (eq :integer (beads-meta-slot-property
                        'beads-command-list 'limit :option-type)))
  ;; Transient properties
  (should (equal "-n" (beads-meta-slot-property
                       'beads-command-list 'limit :key)))
  (should (equal "Output Options" (beads-meta-slot-property
                                   'beads-command-list 'limit :group)))
  (should (equal 5 (beads-meta-slot-property
                    'beads-command-list 'limit :level))))

(ert-deftest beads-meta-list-slot-property-long ()
  "Test that long slot (boolean) has correct metadata."
  ;; CLI properties
  (should (equal "--long" (beads-meta-slot-property
                           'beads-command-list 'long :long-option)))
  (should (eq :boolean (beads-meta-slot-property
                        'beads-command-list 'long :option-type)))
  ;; Transient properties
  (should (equal "-Lo" (beads-meta-slot-property
                        'beads-command-list 'long :key)))
  (should (eq 'transient-switch (beads-meta-slot-property
                                 'beads-command-list 'long :class)))
  (should (equal "Output Options" (beads-meta-slot-property
                                   'beads-command-list 'long :group))))

(ert-deftest beads-meta-list-transient-slots ()
  "Test that all expected slots have transient keys."
  (let ((slots (beads-meta-transient-slots 'beads-command-list)))
    ;; Should have all the transient-enabled slots
    (should (memq 'status slots))
    (should (memq 'priority slots))
    (should (memq 'issue-type slots))
    (should (memq 'assignee slots))
    (should (memq 'title slots))
    (should (memq 'title-contains slots))
    (should (memq 'desc-contains slots))
    (should (memq 'notes-contains slots))
    (should (memq 'created-after slots))
    (should (memq 'created-before slots))
    (should (memq 'updated-after slots))
    (should (memq 'updated-before slots))
    (should (memq 'closed-after slots))
    (should (memq 'closed-before slots))
    (should (memq 'priority-min slots))
    (should (memq 'priority-max slots))
    (should (memq 'label slots))
    (should (memq 'label-any slots))
    (should (memq 'id slots))
    (should (memq 'no-assignee slots))
    (should (memq 'empty-description slots))
    (should (memq 'no-labels slots))
    (should (memq 'limit slots))
    (should (memq 'long slots))
    (should (memq 'format slots))
    (should (memq 'all slots))
    ;; Should have 26 slots with transient keys
    (should (= 26 (length slots)))))

(ert-deftest beads-meta-list-option-slots ()
  "Test that non-positional CLI options are identified."
  (let ((options (beads-meta-option-slots 'beads-command-list)))
    ;; Should include all filter options
    (should (memq 'status options))
    (should (memq 'priority options))
    (should (memq 'issue-type options))
    (should (memq 'assignee options))
    (should (memq 'title options))
    (should (memq 'limit options))
    (should (memq 'label options))
    (should (memq 'no-assignee options))
    (should (memq 'all options))
    ;; All slots have :long-option, so all should be option slots
    (should (= 26 (length options)))))

(ert-deftest beads-meta-list-generate-infix-specs ()
  "Test that infix specs can be generated from beads-command-list."
  (let ((specs (beads-meta-generate-infix-specs
                'beads-command-list "beads-list")))
    ;; Should have specs for all 26 transient-enabled slots
    (should (= 26 (length specs)))
    ;; Check for specific infixes
    (let ((status-spec (cl-find-if
                        (lambda (s)
                          (eq 'beads-list-infix-status (plist-get s :name)))
                        specs)))
      (should status-spec)
      (should (equal "-s" (plist-get status-spec :key))))))

(ert-deftest beads-meta-list-generate-group-specs ()
  "Test that group specs can be generated from beads-command-list."
  (let ((groups (beads-meta-generate-group-specs
                 'beads-command-list "beads-list")))
    ;; Should have 5 groups
    (should (= 5 (length groups)))
    ;; Check for expected groups
    (let ((group-names (mapcar (lambda (g) (plist-get g :description)) groups)))
      (should (member "Basic Filters" group-names))
      (should (member "Text Search" group-names))
      (should (member "Date Filters" group-names))
      (should (member "Advanced Filters" group-names))
      (should (member "Output Options" group-names)))))

(provide 'beads-meta-test)
;;; beads-meta-test.el ends here
