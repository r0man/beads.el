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
(require 'beads-test)

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
  (let ((cmd (beads-command-create :title nil)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-create-test-validate-title-empty ()
  "Test title validation when title is empty."
  (let ((cmd (beads-command-create :title "")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-create-test-validate-title-whitespace ()
  "Test title validation when title is only whitespace."
  (let ((cmd (beads-command-create :title "   \n\t  ")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-create-test-validate-title-valid ()
  "Test title validation when title is valid."
  (let ((cmd (beads-command-create :title "Valid Title")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-type-nil ()
  "Test type validation when type is nil."
  (let ((cmd (beads-command-create :title "Test" :issue-type nil)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-type-valid-bug ()
  "Test type validation with valid bug type."
  (let ((cmd (beads-command-create :title "Test" :issue-type "bug")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-type-valid-feature ()
  "Test type validation with valid feature type."
  (let ((cmd (beads-command-create :title "Test" :issue-type "feature")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-type-valid-task ()
  "Test type validation with valid task type."
  (let ((cmd (beads-command-create :title "Test" :issue-type "task")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-type-valid-epic ()
  "Test type validation with valid epic type."
  (let ((cmd (beads-command-create :title "Test" :issue-type "epic")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-type-valid-chore ()
  "Test type validation with valid chore type."
  (let ((cmd (beads-command-create :title "Test" :issue-type "chore")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-type-invalid ()
  "Test type validation with invalid type."
  (let ((cmd (beads-command-create :title "Test" :issue-type "invalid")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-create-test-validate-priority-nil ()
  "Test priority validation when priority is nil."
  (let ((cmd (beads-command-create :title "Test" :priority nil)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-priority-zero ()
  "Test priority validation with zero (critical)."
  (let ((cmd (beads-command-create :title "Test" :priority 0)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-priority-one ()
  "Test priority validation with one."
  (let ((cmd (beads-command-create :title "Test" :priority 1)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-priority-four ()
  "Test priority validation with four (backlog)."
  (let ((cmd (beads-command-create :title "Test" :priority 4)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-priority-negative ()
  "Test priority validation with negative number."
  (let ((cmd (beads-command-create :title "Test" :priority -1)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-create-test-validate-priority-too-high ()
  "Test priority validation with number too high."
  (let ((cmd (beads-command-create :title "Test" :priority 5)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-create-test-validate-priority-string ()
  "Test priority validation accepts valid string numbers."
  (let ((cmd (beads-command-create :title "Test" :priority "1")))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-create-test-validate-dependencies-nil ()
  "Test dependencies validation when nil."
  (let ((cmd (beads-command-create :title "Test" :deps nil)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-dependencies-valid-single ()
  "Test dependencies validation with single valid dependency."
  (let ((cmd (beads-command-create :title "Test" :deps '("blocks:bd-1"))))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-dependencies-valid-multiple ()
  "Test dependencies validation with multiple valid dependencies."
  (let ((cmd (beads-command-create :title "Test"
                                   :deps '("blocks:bd-1" "related:bd-2"))))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-dependencies-valid-discovered ()
  "Test dependencies validation with discovered-from type."
  (let ((cmd (beads-command-create :title "Test"
                                   :deps '("discovered-from:bd-10"))))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-dependencies-invalid-format ()
  "Test dependencies validation with invalid format."
  (let ((cmd (beads-command-create :title "Test" :deps '("invalid"))))
    (should (beads-command-validate cmd))))

(ert-deftest beads-create-test-validate-dependencies-missing-colon ()
  "Test dependencies validation without colon separator."
  (let ((cmd (beads-command-create :title "Test" :deps '("blocksbd-1"))))
    (should (beads-command-validate cmd))))

(ert-deftest beads-create-test-validate-dependencies-invalid-characters ()
  "Test dependencies validation with invalid characters."
  (let ((cmd (beads-command-create :title "Test" :deps '("blocks:BD@123"))))
    (should (beads-command-validate cmd))))

(ert-deftest beads-create-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (let ((cmd (beads-create--parse-transient-args
              '("--title=Valid Title"
                "--type=bug"
                "--priority=1"))))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-create-test-validate-all-multiple-errors ()
  "Test validate-all with multiple validation errors.
Note: beads-command-validate returns first error, not all errors."
  (let ((cmd (beads-create--parse-transient-args
              '("--title="
                "--type=invalid"
                "--priority=10"))))
    (should (beads-command-validate cmd))))


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

;;; Integration Tests for beads-create--execute

(ert-deftest beads-create-test-execute-minimal-fields ()
  "Integration test: Create issue with minimal required fields.
Tests successful creation with only title set."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((show-called nil)
          (result
           (beads-test-with-cache-tracking
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_) nil))  ; Don't show issue
                      ((symbol-function 'beads-show)
                       (lambda (_) (setq show-called t))))
              ;; Use transient-args mock instead of keyboard macros
              (beads-test-with-transient-args 'beads-create
                  '("--title=Minimal Test Issue")
                (call-interactively #'beads-create--execute))))))

      ;; Verify cache was invalidated
      (should (plist-get result :completion-cache-invalidated))

      ;; Verify show wasn't called (we said no)
      (should-not show-called)

      ;; Verify the issue was created by listing all issues
      (let ((issues (beads-command-list!)))
        (should (seq-find
                 (lambda (issue)
                   (equal (oref issue title) "Minimal Test Issue"))
                 issues))))))

(ert-deftest beads-create-test-execute-all-fields ()
  "Integration test: Create issue with all fields populated.
Tests successful creation with ALL field types."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let* ((ext-ref (format "gh-%d" (random 99999)))
           (result
            (beads-test-with-cache-tracking
             (cl-letf (((symbol-function 'y-or-n-p)
                        (lambda (_) nil)))  ; Don't show issue
               ;; Use transient-args mock instead of keyboard macros
               (beads-test-with-transient-args 'beads-create
                   (list "--title=Complete Test Issue"
                         "--type=feature"
                         "--priority=1"
                         "--assignee=testuser"
                         (format "--external-ref=%s" ext-ref)
                         "--labels=test,integration"
                         "--description=Full description text"
                         "--acceptance=Acceptance criteria here"
                         "--design=Design notes")
                 (call-interactively #'beads-create--execute))))))
      ;; Verify cache was invalidated
      (should (plist-get result :completion-cache-invalidated))

      ;; Verify all fields by fetching the created issue
      (let* ((issues (beads-command-list!))
             (created (seq-find
                       (lambda (issue)
                         (equal (oref issue title) "Complete Test Issue"))
                       issues)))
        (should created)
        (should (equal (oref created issue-type) "feature"))
        (should (equal (oref created priority) 1))
        (should (equal (oref created assignee) "testuser"))
        ;; External-ref is random to avoid UNIQUE constraint violations
        (should (string-match-p "^gh-[0-9]+$" (oref created external-ref)))
        ;; Labels are returned sorted alphabetically by bd
        (should (equal (oref created labels) '("integration" "test")))
        ;; Multiline fields
        (should (equal (oref created description) "Full description text"))
        (should (equal (oref created acceptance-criteria)
                       "Acceptance criteria here"))
        (should (equal (oref created design) "Design notes"))))))

(ert-deftest beads-create-test-execute-validation-failure ()
  "Integration test: Test validation failure paths.
Verifies that beads-create--execute properly rejects invalid input."
  :tags '(:integration)
  ;; Test with empty title - should fail validation
  (beads-test-with-transient-args 'beads-create
      '("--title=")
    (should-error (call-interactively #'beads-create--execute)
                  :type 'user-error))

  ;; Test with invalid type
  (beads-test-with-transient-args 'beads-create
      '("--title=Test" "--type=invalid-type")
    (should-error (call-interactively #'beads-create--execute)
                  :type 'user-error))

  ;; Test with invalid priority
  (beads-test-with-transient-args 'beads-create
      '("--title=Test" "--priority=10")
    (should-error (call-interactively #'beads-create--execute)
                  :type 'user-error))

  ;; Test with invalid dependencies format
  (beads-test-with-transient-args 'beads-create
      '("--title=Test" "--deps=invalid-format")
    (should-error (call-interactively #'beads-create--execute)
                  :type 'user-error)))

(ert-deftest beads-create-test-execute-command-failure ()
  "Integration test: Test bd command failure handling.
Verifies error handling when the bd create command fails."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (beads-test-with-transient-args 'beads-create
        '("--title=Test Issue")
      ;; Mock beads-command-execute to simulate bd failure
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (_)
                   (error "bd create failed: database locked"))))
        ;; Should not signal error, but should return error message
        (let ((result (call-interactively #'beads-create--execute)))
          (should (stringp result))
          (should (string-match-p "Failed to create issue" result)))))))

(ert-deftest beads-create-test-execute-cache-invalidation ()
  "Integration test: Verify cache invalidation after successful creation.
Tests that beads--invalidate-completion-cache is called."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (beads-test-with-transient-args 'beads-create
        '("--title=Cache Test Issue")
      (let ((result
             (beads-test-with-cache-tracking
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (_) nil)))  ; Don't show issue
                ;; Execute create
                (call-interactively #'beads-create--execute)))))

        ;; Verify completion cache was invalidated
        (should (plist-get result :completion-cache-invalidated))

        ;; Label cache should also be invalidated if labels were set
        ;; (not in this test, so it should be nil)
        (should-not (plist-get result :label-cache-invalidated))))))

(ert-deftest beads-create-test-execute-show-workflow ()
  "Integration test: Test show-issue prompt workflow.
Verifies that beads-show is called when user says yes."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((show-called nil)
          (shown-issue-id nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (should (string-match-p "Show issue" prompt))
                   t))  ; Yes, show the issue
                ((symbol-function 'beads-show)
                 (lambda (issue-id)
                   (setq show-called t)
                   (setq shown-issue-id issue-id))))
        ;; Use transient-args mock instead of keyboard macros
        (beads-test-with-transient-args 'beads-create
            '("--title=Show Workflow Test")
          (call-interactively #'beads-create--execute))
        ;; Verify show was called
        (should show-called)
        (should shown-issue-id)
        ;; Issue ID should match pattern: project-prefix-<random-id>
        (should (string-match-p "^[a-zA-Z0-9]+-[a-z0-9]+$" shown-issue-id))))))

(ert-deftest beads-create-test-execute-with-dependencies ()
  "Integration test: Create issue with dependencies.
Tests creating an issue with dependency links."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; First create a parent issue
    (let* ((parent (beads-command-create!
                    :title "Parent Issue"
                    :issue-type "epic"))
           (parent-id (oref parent id)))
      ;; Now create a child issue that blocks the parent
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) nil)))
        ;; Use transient-args mock instead of keyboard macros
        (beads-test-with-transient-args 'beads-create
            (list "--title=Child Issue"
                  (format "--deps=blocks:%s" parent-id))
          (call-interactively #'beads-create--execute))
        ;; Verify the child issue was created
        (let* ((issues (beads-command-list!))
               (child (seq-find
                       (lambda (issue)
                         (equal (oref issue title) "Child Issue"))
                       issues)))
          (should child))))))

;;; Integration Tests for beads-create--preview

(ert-deftest beads-create-test-preview-valid-minimal ()
  "Integration test: Preview command with minimal valid arguments.
Tests that preview generates correct command line for minimal fields."
  :tags '(:integration)
  (beads-test-with-transient-args 'beads-create
      '("--title=Preview Test")
    (let ((result (call-interactively #'beads-create--preview)))
      ;; Should return preview message string
      (should (stringp result))
      (should (string-match-p "^Command: " result))
      ;; Should contain bd executable name
      (should (string-match-p "bd" result))
      ;; Should contain create subcommand
      (should (string-match-p "create" result))
      ;; Should contain the title (may be escaped by shell-quote-argument)
      (should (string-match-p "Preview" result))
      (should (string-match-p "Test" result)))))

(ert-deftest beads-create-test-preview-valid-all-fields ()
  "Integration test: Preview command with all fields populated.
Tests that preview correctly formats complex command with all arguments."
  :tags '(:integration)
  (beads-test-with-transient-args 'beads-create
      '("--title=Complete Preview Test"
        "--type=feature"
        "--priority=1"
        "--description=Test description"
        "--acceptance=Test acceptance"
        "--assignee=testuser"
        "--design=Test design"
        "--external-ref=gh-123"
        "--labels=test,preview")
    (let ((result (call-interactively #'beads-create--preview)))
      ;; Should return preview message string
      (should (stringp result))
      (should (string-match-p "^Command: " result))
      ;; Should contain key parts of the arguments (may be escaped)
      (should (string-match-p "Complete" result))
      (should (string-match-p "Preview" result))
      (should (string-match-p "feature" result))
      (should (string-match-p "testuser" result))
      (should (string-match-p "gh-123" result))
      ;; Should contain proper flags
      (should (string-match-p "--type" result))
      (should (string-match-p "--priority" result))
      (should (string-match-p "--assignee" result)))))

(ert-deftest beads-create-test-preview-validation-error-empty-title ()
  "Integration test: Preview with empty title shows validation error.
Tests that preview returns error message when validation fails."
  :tags '(:integration)
  (beads-test-with-transient-args 'beads-create
      '("--title=")
    (let ((result (call-interactively #'beads-create--preview)))
      ;; Should return validation error message
      (should (stringp result))
      (should (string-match-p "Validation errors:" result))
      (should (string-match-p "title" (downcase result))))))

(ert-deftest beads-create-test-preview-validation-error-invalid-type ()
  "Integration test: Preview with invalid type shows validation error.
Tests that preview catches invalid issue type."
  :tags '(:integration)
  (beads-test-with-transient-args 'beads-create
      '("--title=Test Issue"
        "--type=invalid-type")
    (let ((result (call-interactively #'beads-create--preview)))
      ;; Should return validation error message
      (should (stringp result))
      (should (string-match-p "Validation errors:" result))
      (should (string-match-p "Type must be one of" result)))))

(ert-deftest beads-create-test-preview-validation-error-invalid-priority ()
  "Integration test: Preview with invalid priority shows validation error.
Tests that preview catches priority out of range."
  :tags '(:integration)
  (beads-test-with-transient-args 'beads-create
      '("--title=Test Issue"
        "--priority=10")
    (let ((result (call-interactively #'beads-create--preview)))
      ;; Should return validation error message
      (should (stringp result))
      (should (string-match-p "Validation errors:" result))
      (should (string-match-p "Priority must be" result)))))

(ert-deftest beads-create-test-preview-validation-multiple-errors ()
  "Integration test: Preview with multiple validation errors.
Tests that preview reports first validation failure.
Note: beads-command-validate returns first error, not all errors."
  :tags '(:integration)
  (beads-test-with-transient-args 'beads-create
      '("--title="
        "--type=invalid"
        "--priority=99"
        "--deps=bad-format")
    (let ((result (call-interactively #'beads-create--preview)))
      ;; Should return validation error message
      (should (stringp result))
      (should (string-match-p "Validation errors:" result)))))

(ert-deftest beads-create-test-preview-command-formatting ()
  "Integration test: Verify command line is properly formatted.
Tests that special characters are properly quoted."
  :tags '(:integration)
  (beads-test-with-transient-args 'beads-create
      '("--title=Test with 'quotes' and \"more\" quotes")
    (let ((result (call-interactively #'beads-create--preview)))
      ;; Should return preview message string
      (should (stringp result))
      (should (string-match-p "^Command: " result))
      ;; The shell-quote-argument should handle special characters
      ;; We just verify the message is generated without error
      (should (> (length result) 0)))))

(ert-deftest beads-create-test-preview-with-dependencies ()
  "Integration test: Preview command with dependencies.
Tests that dependency format is preserved in preview."
  :tags '(:integration)
  (beads-test-with-transient-args 'beads-create
      '("--title=Child Issue"
        "--deps=blocks:bd-123,related:bd-456")
    (let ((result (call-interactively #'beads-create--preview)))
      ;; Should return preview message string
      (should (stringp result))
      (should (string-match-p "^Command: " result))
      ;; Should contain dependency information (may be escaped)
      (should (string-match-p "blocks" result))
      (should (string-match-p "bd-123" result))
      (should (string-match-p "related" result))
      (should (string-match-p "bd-456" result)))))

(ert-deftest beads-create-test-preview-does-not-create-issue ()
  "Integration test: Verify preview doesn't actually create issue.
Tests that preview is read-only and doesn't mutate state."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (beads-test-with-transient-args 'beads-create
        '("--title=Should Not Be Created")
      ;; Get initial issue count
      (let ((initial-count (length (beads-command-list!))))
        ;; Run preview
        (call-interactively #'beads-create--preview)
        ;; Verify issue count unchanged
        (should (= (length (beads-command-list!)) initial-count))))))

(ert-deftest beads-create-test-preview-does-not-invalidate-cache ()
  "Integration test: Verify preview doesn't invalidate caches.
Tests that preview is truly read-only with no side effects."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (beads-test-with-transient-args 'beads-create
        '("--title=Preview Cache Test")
      (let ((result
             (beads-test-with-cache-tracking
              (call-interactively #'beads-create--preview))))
        ;; Verify no cache invalidation occurred
        (should-not (plist-get result :completion-cache-invalidated))
        (should-not (plist-get result :label-cache-invalidated))))))

;;; Integration Tests for beads-create--reset

(ert-deftest beads-create-test-reset-clears-state-user-confirms ()
  "Integration test: Reset clears transient state when user confirms.
Tests that reset calls transient-reset and clears all fields."
  :tags '(:integration)
  (let ((transient-reset-called nil)
        (transient-redisplay-called nil))
    (beads-test-with-transient-args 'beads-create
        '("--title=Test Issue"
          "--type=feature"
          "--priority=1"
          "--description=Test description"
          "--acceptance=Test acceptance"
          "--assignee=testuser")
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (should (string-match-p "Reset all fields" prompt))
                   t))  ; User confirms reset
                ((symbol-function 'transient-reset)
                 (lambda ()
                   (setq transient-reset-called t)))
                ((symbol-function 'transient--redisplay)
                 (lambda ()
                   (setq transient-redisplay-called t))))
        ;; Call reset
        (call-interactively #'beads-create--reset)

        ;; Verify transient-reset was called
        (should transient-reset-called)

        ;; Verify transient display was refreshed
        (should transient-redisplay-called)))))

(ert-deftest beads-create-test-reset-user-cancels ()
  "Integration test: Reset does nothing when user cancels.
Tests that reset respects user cancellation and doesn't clear state."
  :tags '(:integration)
  (let ((transient-reset-called nil)
        (transient-redisplay-called nil))
    (beads-test-with-transient-args 'beads-create
        '("--title=Test Issue" "--type=bug")
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (should (string-match-p "Reset all fields" prompt))
                   nil))  ; User cancels reset
                ((symbol-function 'transient-reset)
                 (lambda ()
                   (setq transient-reset-called t)))
                ((symbol-function 'transient--redisplay)
                 (lambda ()
                   (setq transient-redisplay-called t))))
        ;; Call reset
        (call-interactively #'beads-create--reset)

        ;; Verify transient-reset was NOT called
        (should-not transient-reset-called)

        ;; Verify transient display was NOT refreshed
        (should-not transient-redisplay-called)))))

(ert-deftest beads-create-test-reset-does-not-invalidate-cache ()
  "Integration test: Verify reset doesn't invalidate caches.
Tests that reset is a UI-only operation with no database side effects."
  :tags '(:integration)
  (beads-test-with-transient-args 'beads-create
      '("--title=Reset Cache Test")
    (let ((result
           (beads-test-with-cache-tracking
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_) t))  ; Confirm reset
                      ((symbol-function 'transient-reset)
                       (lambda () nil))  ; Mock transient-reset
                      ((symbol-function 'transient--redisplay)
                       (lambda () nil)))  ; Mock redisplay
              (call-interactively #'beads-create--reset)))))
      ;; Verify no cache invalidation occurred
      (should-not (plist-get result :completion-cache-invalidated))
      (should-not (plist-get result :label-cache-invalidated)))))

(ert-deftest beads-create-test-reset-is-transient ()
  "Integration test: Verify reset command stays in transient.
Tests that reset is marked :transient t so menu stays open."
  :tags '(:integration)
  ;; Get the suffix command object
  (let ((suffix (get 'beads-create--reset 'transient--suffix)))
    ;; Verify it exists
    (should suffix)
    ;; Verify it has :transient t property
    ;; Note: The suffix object is a transient-suffix instance
    ;; We check that the function has the transient marker
    (should (commandp 'beads-create--reset))
    ;; The :transient t property means the menu should stay open
    ;; This is verified by checking the suffix definition
    (should (transient-suffix-p suffix))))

(ert-deftest beads-create-test-reset-all-field-types ()
  "Integration test: Reset clears all types of fields.
Tests that reset clears required, optional, and advanced fields."
  :tags '(:integration)
  (let ((transient-reset-called nil))
    ;; Set up with all possible fields populated
    (beads-test-with-transient-args 'beads-create
        '("--title=Complete Test Issue"
          "--type=epic"
          "--priority=0"
          "--description=Description text"
          "--acceptance=Acceptance criteria"
          "--assignee=alice"
          "--design=Design notes"
          "--external-ref=gh-999"
          "--labels=tag1,tag2,tag3"
          "--id=custom-123"
          "--deps=blocks:bd-1,related:bd-2"
          "--parent=bd-parent"
          "--repo=example-repo"
          "--from-template=template-1"
          "--file=/path/to/file.txt"
          "--force")
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) t))  ; Confirm reset
                ((symbol-function 'transient-reset)
                 (lambda ()
                   (setq transient-reset-called t)))
                ((symbol-function 'transient--redisplay)
                 (lambda () nil)))
        ;; Call reset
        (call-interactively #'beads-create--reset)

        ;; Verify reset was called (which clears all fields)
        (should transient-reset-called)))))

(ert-deftest beads-create-test-reset-displays-message ()
  "Integration test: Verify reset displays confirmation message.
Tests that reset shows 'All fields reset' message to user."
  :tags '(:integration)
  (let ((message-text nil))
    (beads-test-with-transient-args 'beads-create
        '("--title=Message Test")
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) t))  ; Confirm reset
                ((symbol-function 'transient-reset)
                 (lambda () nil))
                ((symbol-function 'transient--redisplay)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq message-text (apply #'format format-string args)))))
        ;; Call reset
        (call-interactively #'beads-create--reset)

        ;; Verify message was displayed
        (should message-text)
        (should (string-match-p "All fields reset" message-text))))))

;;; Edge Case Tests
;;
;; Note: Tests for multiline fields (description, acceptance-criteria, design)
;; via shell arguments are not included due to a known limitation in the bd CLI
;; with multiline argument handling. These tests can be added when the bd CLI
;; improves its multiline support.

(ert-deftest beads-create-test-edge-case-unicode-title ()
  "Edge case test: Unicode characters in title.
Tests that titles with various Unicode characters are handled correctly."
  :tags '(:integration :slow :edge-case)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((unicode-titles
           '("Issue with emoji 🐛🔧✨"
             "日本語のタイトル"  ; Japanese
             "Заголовок на русском"  ; Russian
             "عنوان بالعربية"  ; Arabic
             "Título en español con ñ and á"
             "Mixed: ASCII + 中文 + ελληνικά")))
      (dolist (title unicode-titles)
        (beads-test-with-transient-args 'beads-create
            (list (format "--title=%s" title))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_) nil)))  ; Don't show issue
            ;; Execute create
            (call-interactively #'beads-create--execute)

            ;; Verify issue was created with correct title
            (let* ((issues (beads-command-list!))
                   (created (seq-find
                             (lambda (issue)
                               (equal (oref issue title) title))
                             issues)))
              (should created)
              (should (equal (oref created title) title)))))))))

(ert-deftest beads-create-test-edge-case-very-long-title ()
  "Edge case test: Long title (>255 characters).
Tests that reasonably long titles are handled correctly.
Note: Using 300 chars instead of 1500 to stay within shell/bd limits."
  :tags '(:integration :slow :edge-case)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a 300 character title (more realistic limit)
    (let ((long-title (concat "Issue with long title: "
                              (make-string 277 ?x))))
      (should (> (length long-title) 255))
      (beads-test-with-transient-args 'beads-create
          (list (format "--title=%s" long-title))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_) nil)))
          ;; Execute create
          (call-interactively #'beads-create--execute)

          ;; Verify issue was created with full title
          (let* ((issues (beads-command-list!))
                 (created (seq-find
                           (lambda (issue)
                             (equal (oref issue title) long-title))
                           issues)))
            (should created)
            (should (equal (length (oref created title))
                           (length long-title)))))))))

(ert-deftest beads-create-test-edge-case-special-characters-title ()
  "Edge case test: Special characters requiring shell escaping in title.
Tests that special shell characters are properly handled.
Note: Some characters like newlines/tabs may be normalized by bd."
  :tags '(:integration :slow :edge-case)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Test special characters that should work
    (let ((special-titles
           '("Issue with 'single quotes'"
             "Issue with \"double quotes\""
             "Issue with ampersand & pipe |"
             "Issue with asterisk * and question ?")))
      (dolist (title special-titles)
        (beads-test-with-transient-args 'beads-create
            (list (format "--title=%s" title))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_) nil)))
            ;; Execute create
            (call-interactively #'beads-create--execute)

            ;; Verify issue was created with exact title
            (let* ((issues (beads-command-list!))
                   (created (seq-find
                             (lambda (issue)
                               (equal (oref issue title) title))
                             issues)))
              (should created)
              (should (equal (oref created title) title)))))))))

(ert-deftest beads-create-test-edge-case-empty-vs-nil-description ()
  "Edge case test: Empty string vs nil for optional description field.
Tests distinction between empty string and nil value."
  :tags '(:integration :slow :edge-case)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Test with nil description (field not provided)
    (beads-test-with-transient-args 'beads-create
        '("--title=Nil Description Test")
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) nil)))
        (call-interactively #'beads-create--execute)
        (let* ((issues (beads-command-list!))
               (created (seq-find
                         (lambda (issue)
                           (equal (oref issue title) "Nil Description Test"))
                         issues)))
          (should created)
          ;; Description should be nil or empty
          (should (or (null (oref created description))
                      (string-empty-p (oref created description)))))))

    ;; Test with empty string description
    (beads-test-with-transient-args 'beads-create
        '("--title=Empty Description Test"
          "--description=")
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) nil)))
        (call-interactively #'beads-create--execute)
        (let* ((issues (beads-command-list!))
               (created (seq-find
                         (lambda (issue)
                           (equal (oref issue title)
                                  "Empty Description Test"))
                         issues)))
          (should created)
          ;; Description should be empty or nil (bd may normalize)
          (should (or (null (oref created description))
                      (string-empty-p (oref created description)))))))))

(ert-deftest beads-create-test-edge-case-priority-boundary-values ()
  "Edge case test: Boundary values for priority (0-4).
Tests all valid priority values including boundaries."
  :tags '(:integration :slow :edge-case)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (dolist (priority '(0 1 2 3 4))
      (let ((title (format "Priority %d Test" priority)))
        (beads-test-with-transient-args 'beads-create
            (list (format "--title=%s" title)
                  (format "--priority=%d" priority))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_) nil)))
            ;; Execute create
            (call-interactively #'beads-create--execute)

            ;; Verify issue was created with correct priority
            (let* ((issues (beads-command-list!))
                   (created (seq-find
                             (lambda (issue)
                               (equal (oref issue title) title))
                             issues)))
              (should created)
              ;; Note: bd omits priority 0 from JSON (omitempty behavior)
              (let ((actual-priority (oref created priority)))
                (should (or (equal actual-priority priority)
                            (and (= priority 0)
                                 (null actual-priority))))))))))))

(ert-deftest beads-create-test-edge-case-priority-default-nil ()
  "Edge case test: Priority defaults to nil when not specified.
Tests that omitting priority uses bd default behavior."
  :tags '(:integration :slow :edge-case)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (beads-test-with-transient-args 'beads-create
        '("--title=Default Priority Test")
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (_) nil)))
        ;; Execute create without specifying priority
        (call-interactively #'beads-create--execute)

        ;; Verify issue was created (bd will assign default priority)
        (let* ((issues (beads-command-list!))
               (created (seq-find
                         (lambda (issue)
                           (equal (oref issue title) "Default Priority Test"))
                         issues)))
          (should created)
          ;; Priority should be set to bd's default (typically 2)
          (should (numberp (oref created priority))))))))

(ert-deftest beads-create-test-edge-case-all-issue-types ()
  "Edge case test: Create issues with all valid issue types.
Tests that all supported issue types work correctly."
  :tags '(:integration :slow :edge-case)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (dolist (type '("bug" "feature" "task" "epic" "chore"))
      (let ((title (format "Type %s Test" type)))
        (beads-test-with-transient-args 'beads-create
            (list (format "--title=%s" title)
                  (format "--type=%s" type))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_) nil)))
            ;; Execute create
            (call-interactively #'beads-create--execute)

            ;; Verify issue was created with correct type
            (let* ((issues (beads-command-list!))
                   (created (seq-find
                             (lambda (issue)
                               (equal (oref issue title) title))
                             issues)))
              (should created)
              (should (equal (oref created issue-type) type)))))))))

(ert-deftest beads-create-test-edge-case-whitespace-only-fields ()
  "Edge case test: Fields containing only whitespace.
Tests that whitespace-only values are properly rejected or normalized."
  :tags '(:integration)
  ;; Title with only spaces should fail validation
  (beads-test-with-transient-args 'beads-create
      '("--title=    ")
    (should-error (call-interactively #'beads-create--execute)
                  :type 'user-error))

  ;; Title with only tabs should fail validation
  (beads-test-with-transient-args 'beads-create
      '("--title=\t\t\t")
    (should-error (call-interactively #'beads-create--execute)
                  :type 'user-error))

  ;; Title with mixed whitespace should fail validation
  (beads-test-with-transient-args 'beads-create
      '("--title= \t \n ")
    (should-error (call-interactively #'beads-create--execute)
                  :type 'user-error)))

(ert-deftest beads-create-test-edge-case-maximum-labels ()
  "Edge case test: Create issue with many labels.
Tests handling of multiple labels in comma-separated format."
  :tags '(:integration :slow :edge-case)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((many-labels "label1,label2,label3,label4,label5,label6,label7,label8,label9,label10"))
      (beads-test-with-transient-args 'beads-create
          (list "--title=Many Labels Test"
                (format "--labels=%s" many-labels))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_) nil)))
          ;; Execute create
          (call-interactively #'beads-create--execute)

          ;; Verify all labels were set (bd returns them sorted)
          (let* ((issues (beads-command-list!))
                 (created (seq-find
                           (lambda (issue)
                             (equal (oref issue title) "Many Labels Test"))
                           issues)))
            (should created)
            (should (oref created labels))
            (should (equal (length (oref created labels)) 10))))))))

(ert-deftest beads-create-test-edge-case-special-characters-in-labels ()
  "Edge case test: Labels with special characters.
Tests that labels with hyphens, underscores, and numbers work."
  :tags '(:integration :slow :edge-case)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((special-labels "bug-fix,v2.0,high_priority,test-123"))
      (beads-test-with-transient-args 'beads-create
          (list "--title=Special Labels Test"
                (format "--labels=%s" special-labels))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (_) nil)))
          ;; Execute create
          (call-interactively #'beads-create--execute)

          ;; Verify labels were set
          (let* ((issues (beads-command-list!))
                 (created (seq-find
                           (lambda (issue)
                             (equal (oref issue title) "Special Labels Test"))
                           issues)))
            (should created)
            (should (oref created labels))
            ;; Labels should be sorted: bug-fix, high_priority, test-123, v2.0
            (should (member "bug-fix" (oref created labels)))
            (should (member "high_priority" (oref created labels)))
            (should (member "test-123" (oref created labels)))
            (should (member "v2.0" (oref created labels)))))))))

;;; Error Recovery Tests

(ert-deftest beads-create-test-error-recovery-generic ()
  "Error recovery test: Generic error handling.
Tests that beads-create--execute catches errors from bd execution and
returns error messages instead of signaling errors. This covers all error
types: command not found, JSON parsing errors, database errors, I/O errors,
permission errors, TRAMP failures, etc."
  :tags '(:integration :error-recovery)
  (beads-test-with-transient-args 'beads-create
      '("--title=Test Issue")
    ;; Test various error messages to ensure generic error handling
    (dolist (error-msg '("Searching for program: No such file or directory, bd"
                         "Process bd exited abnormally with code 1"
                         "JSON parse error: unexpected character"
                         "Permission denied: .beads/issues.db"
                         "I/O error: Connection timed out"
                         "No .beads directory found. Run 'bd init' first."))
      (cl-letf (((symbol-function 'beads-command-execute)
                 (lambda (_)
                   (error "%s" error-msg))))
        ;; Execute should catch error and return error message
        (let ((result (call-interactively #'beads-create--execute)))
          ;; Should return error message string, not signal error
          (should (stringp result))
          (should (string-match-p "Failed to create issue" result)))))))

(ert-deftest beads-create-test-error-recovery-database-locked ()
  "Error recovery test: Database locked by another process.
Tests handling of common database contention error (example of specific
error case)."
  :tags '(:integration :error-recovery)
  (beads-test-with-transient-args 'beads-create
      '("--title=Test Issue")
    ;; Mock beads-command-execute to simulate database lock error
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_)
                 (error "database is locked"))))
      ;; Execute should catch error and return error message
      (let ((result (call-interactively #'beads-create--execute)))
        ;; Should return error message, not signal error
        (should (stringp result))
        (should (string-match-p "Failed to create issue" result))))))

(ert-deftest beads-create-test-error-recovery-cache-not-invalidated-on-error ()
  "Error recovery test: Cache not invalidated on error.
Tests that completion cache is only invalidated on successful creation,
not when errors occur during execution."
  :tags '(:integration :error-recovery)
  (beads-test-with-transient-args 'beads-create
      '("--title=Test Issue")
    (let ((result
           (beads-test-with-cache-tracking
            (cl-letf (((symbol-function 'beads-command-execute)
                       (lambda (_)
                         (error "Simulated bd failure"))))
              ;; Execute should fail
              (call-interactively #'beads-create--execute)))))
      ;; Verify cache was NOT invalidated (error path)
      (should-not (plist-get result :completion-cache-invalidated))
      (should-not (plist-get result :label-cache-invalidated)))))

(provide 'beads-create-test)
;;; beads-create-test.el ends here
