;;; beads-meta-demo-test.el --- Demo/proof-of-concept for meta system -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This test file demonstrates the complete beads-meta system as a
;; proof-of-concept.  It shows how a command class with full slot
;; metadata can automatically generate:
;;
;; 1. CLI argument building (beads-meta-build-command-line)
;; 2. Transient infix specifications (beads-meta-generate-infix-specs)
;; 3. Transient group specifications (beads-meta-generate-group-specs)
;; 4. Complete transient prefix specifications
;;
;; This demonstrates the pattern that can be applied to migrate
;; beads-command-create and other command classes to use the meta
;; system.

;;; Code:

(require 'ert)
(require 'beads-meta)
(require 'eieio)
(require 'cl-lib)

;;; ============================================================
;;; Demo Command Class
;;; ============================================================

;; This class demonstrates how beads-command-create would look
;; after migration to use slot metadata.

(defclass beads-meta-demo-command ()
  ((title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Issue title"
    ;; CLI properties
    :positional 1
    ;; Transient properties
    :transient-key "t"
    :transient-description "Title (required)"
    :transient-class transient-option
    :transient-reader beads-reader-issue-title
    :transient-group "Required"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)

   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Issue type"
    :long-option "--type"
    :short-option "-t"
    :option-type :string
    :transient-key "T"
    :transient-description "Type"
    :transient-class transient-option
    :transient-choices ("bug" "feature" "task" "epic" "chore")
    :transient-prompt "Type: "
    :transient-group "Issue attributes"
    :transient-level 2
    :transient-order 1)

   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Issue priority (0-4)"
    :long-option "--priority"
    :short-option "-p"
    :option-type :integer
    :transient-key "p"
    :transient-description "Priority (0-4)"
    :transient-class transient-option
    :transient-choices (0 1 2 3 4)
    :transient-prompt "Priority: "
    :transient-group "Issue attributes"
    :transient-level 2
    :transient-order 2)

   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Issue assignee"
    :long-option "--assignee"
    :short-option "-a"
    :option-type :string
    :transient-key "a"
    :transient-description "Assignee"
    :transient-class transient-option
    :transient-prompt "Assignee: "
    :transient-group "Issue attributes"
    :transient-level 2
    :transient-order 3)

   (labels
    :initarg :labels
    :type (or null list)
    :initform nil
    :documentation "Issue labels"
    :long-option "--labels"
    :short-option "-l"
    :option-type :list
    :option-separator ","
    :transient-key "l"
    :transient-description "Labels"
    :transient-class transient-option
    :transient-prompt "Labels (comma-separated): "
    :transient-group "Issue attributes"
    :transient-level 2
    :transient-order 4)

   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Issue description"
    :long-option "--description"
    :short-option "-d"
    :option-type :string
    :transient-key "d"
    :transient-description "Description"
    :transient-class transient-option
    :transient-group "Content"
    :transient-level 3
    :transient-order 1)

   (deps
    :initarg :deps
    :type (or null list)
    :initform nil
    :documentation "Dependencies"
    :long-option "--deps"
    :option-type :list
    :option-separator ","
    :transient-key "D"
    :transient-description "Dependencies"
    :transient-class transient-option
    :transient-prompt "Dependencies (comma-separated): "
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 1)

   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force creation"
    :long-option "--force"
    :option-type :boolean
    :transient-key "!"
    :transient-description "Force"
    :transient-class transient-switch
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 2))
  :documentation "Demo command class with full slot metadata.
This demonstrates how beads-command-create would look after migration.")

;;; ============================================================
;;; Tests - CLI Building
;;; ============================================================

(ert-deftest beads-meta-demo-cli-title-positional ()
  "Demo: Title as positional argument."
  (let ((cmd (beads-meta-demo-command :title "Fix bug")))
    (should (equal '("Fix bug")
                   (beads-meta-build-command-line cmd)))))

(ert-deftest beads-meta-demo-cli-with-options ()
  "Demo: Command with multiple options."
  (let ((cmd (beads-meta-demo-command
              :title "Fix bug"
              :issue-type "bug"
              :priority 1)))
    (let ((args (beads-meta-build-command-line cmd)))
      ;; Positional first
      (should (equal "Fix bug" (car args)))
      ;; Named options follow
      (should (member "--type" args))
      (should (member "bug" args))
      (should (member "--priority" args))
      (should (member "1" args)))))

(ert-deftest beads-meta-demo-cli-with-list ()
  "Demo: Command with list option."
  (let ((cmd (beads-meta-demo-command
              :title "Fix bug"
              :labels '("bug" "urgent"))))
    (let ((args (beads-meta-build-command-line cmd)))
      (should (member "--labels" args))
      (should (member "bug,urgent" args)))))

(ert-deftest beads-meta-demo-cli-with-boolean ()
  "Demo: Command with boolean flag."
  (let ((cmd (beads-meta-demo-command
              :title "Fix bug"
              :force t)))
    (let ((args (beads-meta-build-command-line cmd)))
      (should (member "--force" args)))))

(ert-deftest beads-meta-demo-cli-full-command ()
  "Demo: Full command with all options."
  (let ((cmd (beads-meta-demo-command
              :title "Implement feature"
              :issue-type "feature"
              :priority 1
              :assignee "alice"
              :labels '("frontend" "important")
              :description "Add new button"
              :deps '("bd-123")
              :force t)))
    (let ((args (beads-meta-build-command-line cmd)))
      ;; Should have all the parts
      (should (equal "Implement feature" (car args)))
      (should (member "--type" args))
      (should (member "--priority" args))
      (should (member "--assignee" args))
      (should (member "--labels" args))
      (should (member "--description" args))
      (should (member "--deps" args))
      (should (member "--force" args)))))

;;; ============================================================
;;; Tests - Infix Generation
;;; ============================================================

(ert-deftest beads-meta-demo-infix-generation ()
  "Demo: Generate infix specs from class metadata."
  (let ((specs (beads-meta-generate-infix-specs
                'beads-meta-demo-command "beads-demo")))
    ;; Should generate specs for all slots with :transient-key
    (should (= 8 (length specs)))  ; 8 slots have :transient-key
    ;; Check title spec
    (let ((title-spec (cl-find-if
                       (lambda (s) (eq 'beads-demo-infix-title
                                       (plist-get s :name)))
                       specs)))
      (should title-spec)
      (should (equal "t" (plist-get title-spec :key)))
      (should (equal "Title (required)" (plist-get title-spec :description)))
      (should (eq 'transient-option (plist-get title-spec :class)))
      (should (eq 'beads-reader-issue-title (plist-get title-spec :reader))))))

(ert-deftest beads-meta-demo-infix-choices ()
  "Demo: Infix with choices."
  (let ((specs (beads-meta-generate-infix-specs
                'beads-meta-demo-command "beads-demo")))
    (let ((type-spec (cl-find-if
                      (lambda (s) (eq 'beads-demo-infix-issue-type
                                      (plist-get s :name)))
                      specs)))
      (should type-spec)
      (should (equal '("bug" "feature" "task" "epic" "chore")
                     (plist-get type-spec :choices))))))

(ert-deftest beads-meta-demo-infix-switch ()
  "Demo: Boolean infix generates switch class."
  (let ((specs (beads-meta-generate-infix-specs
                'beads-meta-demo-command "beads-demo")))
    (let ((force-spec (cl-find-if
                       (lambda (s) (eq 'beads-demo-infix-force
                                       (plist-get s :name)))
                       specs)))
      (should force-spec)
      (should (eq 'transient-switch (plist-get force-spec :class)))
      (should (equal "--force" (plist-get force-spec :argument))))))

;;; ============================================================
;;; Tests - Group Generation
;;; ============================================================

(ert-deftest beads-meta-demo-group-generation ()
  "Demo: Generate group specs from class metadata."
  (let ((groups (beads-meta-generate-group-specs
                 'beads-meta-demo-command "beads-demo")))
    ;; Should have 4 groups (Required, Issue attributes, Content, Advanced)
    (should (= 4 (length groups)))
    ;; Check groups are sorted by level
    (let ((levels (mapcar (lambda (g) (plist-get g :level)) groups)))
      (should (equal levels (sort (copy-sequence levels) #'<))))))

(ert-deftest beads-meta-demo-group-contents ()
  "Demo: Groups contain correct infixes."
  (let ((groups (beads-meta-generate-group-specs
                 'beads-meta-demo-command "beads-demo")))
    ;; Required group should have title
    (let ((required (cl-find-if
                     (lambda (g) (equal "Required" (plist-get g :description)))
                     groups)))
      (should required)
      (should (memq 'beads-demo-infix-title (plist-get required :infixes))))
    ;; Issue attributes should have type, priority, assignee, labels
    (let ((attrs (cl-find-if
                  (lambda (g) (equal "Issue attributes" (plist-get g :description)))
                  groups)))
      (should attrs)
      (let ((infixes (plist-get attrs :infixes)))
        (should (memq 'beads-demo-infix-issue-type infixes))
        (should (memq 'beads-demo-infix-priority infixes))
        (should (memq 'beads-demo-infix-assignee infixes))
        (should (memq 'beads-demo-infix-labels infixes))))))

;;; ============================================================
;;; Tests - Prefix Generation
;;; ============================================================

(ert-deftest beads-meta-demo-prefix-spec ()
  "Demo: Generate prefix spec."
  (let ((spec (beads-meta-generate-prefix-spec
               'beads-meta-demo-command "beads-demo" "Demo menu")))
    (should (eq 'beads-demo (plist-get spec :name)))
    (should (equal "Demo menu" (plist-get spec :docstring)))
    (should (= 4 (length (plist-get spec :groups))))))

;;; ============================================================
;;; Summary Test
;;; ============================================================

(ert-deftest beads-meta-demo-complete-workflow ()
  "Demo: Complete end-to-end workflow demonstration.
This test shows the entire meta-programming system working together."
  ;; 1. Create a command instance
  (let ((cmd (beads-meta-demo-command
              :title "New feature"
              :issue-type "feature"
              :priority 1
              :labels '("frontend"))))

    ;; 2. Generate CLI arguments
    (let ((cli-args (beads-meta-build-command-line cmd)))
      (should (string= "New feature" (car cli-args)))
      (should (member "--type" cli-args))
      (should (member "feature" cli-args)))

    ;; 3. Generate infix specifications
    (let ((infixes (beads-meta-generate-infix-specs
                    'beads-meta-demo-command "beads-demo")))
      (should (= 8 (length infixes))))

    ;; 4. Generate group specifications
    (let ((groups (beads-meta-generate-group-specs
                   'beads-meta-demo-command "beads-demo")))
      (should (= 4 (length groups))))

    ;; 5. Generate prefix specification
    (let ((prefix (beads-meta-generate-prefix-spec
                   'beads-meta-demo-command "beads-demo")))
      (should (eq 'beads-demo (plist-get prefix :name))))))

(provide 'beads-meta-demo-test)
;;; beads-meta-demo-test.el ends here
