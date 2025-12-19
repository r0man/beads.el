;;; beads-agent-types-test.el --- Tests for built-in agent types -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-agent-types.el built-in agent types.
;; Tests cover all 5 types: Task, Review, Plan, QA, Custom.

;;; Code:

(require 'ert)
(require 'beads-agent-types)

;;; Test Fixtures

(defvar beads-agent-types-test--saved-registry nil
  "Saved registry to restore after tests.")

(defvar beads-agent-types-test--saved-review-prompt nil
  "Saved review prompt for tests.")

(defvar beads-agent-types-test--saved-qa-prompt nil
  "Saved QA prompt for tests.")

(defvar beads-agent-types-test--sample-issue
  '(:id "test-123"
    :title "Test Issue Title"
    :description "Test issue description with details.")
  "Sample issue for testing prompt building.")

(defun beads-agent-types-test--setup ()
  "Setup test fixtures."
  ;; Save current state
  (setq beads-agent-types-test--saved-registry beads-agent-type--registry)
  (setq beads-agent-types-test--saved-review-prompt beads-agent-review-prompt)
  (setq beads-agent-types-test--saved-qa-prompt beads-agent-qa-prompt)
  ;; Clear and re-register to ensure clean state
  (beads-agent-type--clear-registry)
  (setq beads-agent-types--builtin-registered nil)
  (beads-agent-types-register-builtin))

(defun beads-agent-types-test--teardown ()
  "Teardown test fixtures."
  ;; Restore saved state
  (setq beads-agent-type--registry beads-agent-types-test--saved-registry)
  (setq beads-agent-review-prompt beads-agent-types-test--saved-review-prompt)
  (setq beads-agent-qa-prompt beads-agent-types-test--saved-qa-prompt)
  (setq beads-agent-types-test--saved-registry nil))

;;; Tests for Registration

(ert-deftest beads-agent-types-test-all-registered ()
  "Test that all 5 built-in types are registered."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((types (beads-agent-type-list)))
        (should (= (length types) 5))
        (should (beads-agent-type-get "task"))
        (should (beads-agent-type-get "review"))
        (should (beads-agent-type-get "plan"))
        (should (beads-agent-type-get "qa"))
        (should (beads-agent-type-get "custom")))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-registration-idempotent ()
  "Test that registering built-in types multiple times is safe."
  (beads-agent-types-test--setup)
  (unwind-protect
      (progn
        ;; Register again - should not duplicate
        (beads-agent-types-register-builtin)
        (beads-agent-types-register-builtin)
        (should (= (length (beads-agent-type-list)) 5)))
    (beads-agent-types-test--teardown)))

;;; Tests for Task Agent

(ert-deftest beads-agent-types-test-task-letter ()
  "Test Task agent has letter T."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "task")))
        (should (equal (oref type letter) "T")))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-task-name ()
  "Test Task agent has correct name."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "task")))
        (should (equal (oref type name) "Task")))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-task-no-plan-mode ()
  "Test Task agent does not require plan mode."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "task")))
        (should (eq (oref type requires-plan-mode) nil)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-task-prompt-content ()
  "Test Task agent builds correct prompt."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let* ((type (beads-agent-type-get "task"))
             (prompt (beads-agent-type-build-prompt
                      type beads-agent-types-test--sample-issue)))
        (should (stringp prompt))
        ;; Check for embedded prompt content
        (should (string-match "task-completion agent" prompt))
        (should (string-match "Understand the Task" prompt))
        ;; Check for issue context
        (should (string-match "test-123" prompt))
        (should (string-match "Test Issue Title" prompt)))
    (beads-agent-types-test--teardown)))

;;; Tests for Review Agent

(ert-deftest beads-agent-types-test-review-letter ()
  "Test Review agent has letter R."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "review")))
        (should (equal (oref type letter) "R")))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-review-default-prompt ()
  "Test Review agent uses default prompt content."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let* ((type (beads-agent-type-get "review"))
             (prompt (beads-agent-type-build-prompt
                      type beads-agent-types-test--sample-issue)))
        (should (stringp prompt))
        ;; Check for default prompt content
        (should (string-match "code review" prompt))
        (should (string-match "Code quality" prompt))
        (should (string-match "Security" prompt))
        ;; Check for issue context
        (should (string-match "test-123" prompt)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-review-custom-prompt ()
  "Test Review agent uses customized prompt."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((beads-agent-review-prompt "Custom review instructions.")
            (type (beads-agent-type-get "review")))
        (let ((prompt (beads-agent-type-build-prompt
                       type beads-agent-types-test--sample-issue)))
          (should (string-match "Custom review instructions" prompt))
          (should (string-match "test-123" prompt))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-review-no-plan-mode ()
  "Test Review agent does not require plan mode."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "review")))
        (should (eq (oref type requires-plan-mode) nil)))
    (beads-agent-types-test--teardown)))

;;; Tests for Plan Agent

(ert-deftest beads-agent-types-test-plan-letter ()
  "Test Plan agent has letter P."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "plan")))
        (should (equal (oref type letter) "P")))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-plan-requires-plan-mode ()
  "Test Plan agent requires plan mode."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "plan")))
        (should (eq (oref type requires-plan-mode) t)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-plan-returns-nil-prompt ()
  "Test Plan agent returns nil for prompt (uses --plan flag)."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "plan")))
        (should (null (beads-agent-type-build-prompt
                       type beads-agent-types-test--sample-issue))))
    (beads-agent-types-test--teardown)))

;;; Tests for QA Agent

(ert-deftest beads-agent-types-test-qa-letter ()
  "Test QA agent has letter Q."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "qa")))
        (should (equal (oref type letter) "Q")))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-qa-default-prompt ()
  "Test QA agent uses default prompt content."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let* ((type (beads-agent-type-get "qa"))
             (prompt (beads-agent-type-build-prompt
                      type beads-agent-types-test--sample-issue)))
        (should (stringp prompt))
        ;; Check for default prompt content
        (should (string-match "QA agent" prompt))
        (should (string-match "tests" prompt))
        (should (string-match "edge cases" prompt))
        ;; Check for issue context
        (should (string-match "test-123" prompt)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-qa-custom-prompt ()
  "Test QA agent uses customized prompt."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((beads-agent-qa-prompt "Custom QA instructions.")
            (type (beads-agent-type-get "qa")))
        (let ((prompt (beads-agent-type-build-prompt
                       type beads-agent-types-test--sample-issue)))
          (should (string-match "Custom QA instructions" prompt))
          (should (string-match "test-123" prompt))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-qa-no-plan-mode ()
  "Test QA agent does not require plan mode."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "qa")))
        (should (eq (oref type requires-plan-mode) nil)))
    (beads-agent-types-test--teardown)))

;;; Tests for Custom Agent

(ert-deftest beads-agent-types-test-custom-letter ()
  "Test Custom agent has letter C."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "custom")))
        (should (equal (oref type letter) "C")))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-custom-no-plan-mode ()
  "Test Custom agent does not require plan mode."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "custom")))
        (should (eq (oref type requires-plan-mode) nil)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-custom-prompts-user ()
  "Test Custom agent prompts user and builds prompt correctly."
  (beads-agent-types-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional _initial)
                   "User's custom instructions")))
        (let* ((type (beads-agent-type-get "custom"))
               (prompt (beads-agent-type-build-prompt
                        type beads-agent-types-test--sample-issue)))
          (should (stringp prompt))
          (should (string-match "User's custom instructions" prompt))
          (should (string-match "test-123" prompt))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-custom-empty-input-error ()
  "Test Custom agent signals error on empty input."
  (beads-agent-types-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional _initial) "")))
        (let ((type (beads-agent-type-get "custom")))
          (should-error
           (beads-agent-type-build-prompt
            type beads-agent-types-test--sample-issue)
           :type 'user-error)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-custom-stores-last-prompt ()
  "Test Custom agent stores last prompt for history."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((beads-agent-type-custom--last-prompt nil))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_prompt &optional _initial)
                     "Stored prompt")))
          (let ((type (beads-agent-type-get "custom")))
            (beads-agent-type-build-prompt
             type beads-agent-types-test--sample-issue)
            (should (equal beads-agent-type-custom--last-prompt
                           "Stored prompt")))))
    (beads-agent-types-test--teardown)))

;;; Tests for Completion Support

(ert-deftest beads-agent-types-test-completion-all-types ()
  "Test all 5 types appear in completion."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((table (beads-agent-type-completion-table)))
        (let ((completions (all-completions "" table)))
          (should (member "Task" completions))
          (should (member "Review" completions))
          (should (member "Plan" completions))
          (should (member "QA" completions))
          (should (member "Custom" completions))))
    (beads-agent-types-test--teardown)))

;;; Tests for Issue Context Integration

(ert-deftest beads-agent-types-test-prompts-include-issue-title ()
  "Test that prompts include issue title."
  (beads-agent-types-test--setup)
  (unwind-protect
      (dolist (type-name '("task" "review" "qa"))
        (let* ((type (beads-agent-type-get type-name))
               (prompt (beads-agent-type-build-prompt
                        type beads-agent-types-test--sample-issue)))
          (should (string-match "Test Issue Title" prompt))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-prompts-include-issue-description ()
  "Test that prompts include issue description."
  (beads-agent-types-test--setup)
  (unwind-protect
      (dolist (type-name '("task" "review" "qa"))
        (let* ((type (beads-agent-type-get type-name))
               (prompt (beads-agent-type-build-prompt
                        type beads-agent-types-test--sample-issue)))
          (should (string-match "Test issue description" prompt))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-prompts-handle-empty-description ()
  "Test that prompts handle missing description gracefully."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((issue '(:id "test-456" :title "No Description")))
        (dolist (type-name '("task" "review" "qa"))
          (let* ((type (beads-agent-type-get type-name))
                 (prompt (beads-agent-type-build-prompt type issue)))
            (should (stringp prompt))
            (should (string-match "test-456" prompt)))))
    (beads-agent-types-test--teardown)))

;;; Tests for Type Properties

(ert-deftest beads-agent-types-test-each-has-unique-letter ()
  "Test that each type has a unique single letter."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((letters nil))
        (dolist (type (beads-agent-type-list))
          (let ((letter (oref type letter)))
            (should (= (length letter) 1))
            (should-not (member letter letters))
            (push letter letters))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-each-has-description ()
  "Test that each type has a non-empty description."
  (beads-agent-types-test--setup)
  (unwind-protect
      (dolist (type (beads-agent-type-list))
        (should (> (length (oref type description)) 0)))
    (beads-agent-types-test--teardown)))

(provide 'beads-agent-types-test)

;;; beads-agent-types-test.el ends here
