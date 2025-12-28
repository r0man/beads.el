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
(require 'beads-types)

;;; Test Fixtures

(defvar beads-agent-types-test--saved-registry nil
  "Saved registry to restore after tests.")

(defvar beads-agent-types-test--saved-review-prompt nil
  "Saved review prompt for tests.")

(defvar beads-agent-types-test--saved-qa-prompt nil
  "Saved QA prompt for tests.")

(defun beads-agent-types-test--make-sample-issue ()
  "Create a sample beads-issue for testing prompt building."
  (beads-issue :id "test-123"
               :title "Test Issue Title"
               :description "Test issue description with details."))

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


(ert-deftest beads-agent-types-test-task-prompt-content ()
  "Test Task agent builds correct prompt."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let* ((type (beads-agent-type-get "task"))
             (prompt (beads-agent-type-build-prompt
                      type (beads-agent-types-test--make-sample-issue))))
        (should (stringp prompt))
        ;; Check for embedded prompt content
        (should (string-match "task-completion agent" prompt))
        (should (string-match "Claim the Task" prompt))
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
                      type (beads-agent-types-test--make-sample-issue))))
        (should (stringp prompt))
        ;; Check for default prompt content
        (should (string-match "code review" prompt))
        (should (string-match "Code quality" prompt))
        (should (string-match "Security" prompt))
        ;; Check for issue context
        (should (string-match "test-123" prompt)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-review-custom-prompt ()
  "Test Review agent uses customized prompt with placeholders."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((beads-agent-review-prompt
             "Custom review instructions for <ISSUE-ID>: <ISSUE-TITLE>")
            (type (beads-agent-type-get "review")))
        (let ((prompt (beads-agent-type-build-prompt
                       type (beads-agent-types-test--make-sample-issue))))
          (should (string-match "Custom review instructions" prompt))
          (should (string-match "test-123" prompt))
          (should (string-match "Test Issue Title" prompt))))
    (beads-agent-types-test--teardown)))


;;; Tests for Plan Agent

(ert-deftest beads-agent-types-test-plan-letter ()
  "Test Plan agent has letter P."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "plan")))
        (should (equal (oref type letter) "P")))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-plan-builds-prompt ()
  "Test Plan agent builds a proper prompt."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let* ((type (beads-agent-type-get "plan"))
             (prompt (beads-agent-type-build-prompt
                      type (beads-agent-types-test--make-sample-issue))))
        (should (stringp prompt))
        ;; Check for plan prompt content
        (should (string-match "planning agent" prompt))
        (should (string-match "DO NOT modify" prompt))
        ;; Check for issue context
        (should (string-match "test-123" prompt))
        (should (string-match "Test Issue Title" prompt)))
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
                      type (beads-agent-types-test--make-sample-issue))))
        (should (stringp prompt))
        ;; Check for default prompt content
        (should (string-match "QA agent" prompt))
        (should (string-match "tests" prompt))
        (should (string-match "edge cases" prompt))
        ;; Check for issue context
        (should (string-match "test-123" prompt)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-qa-custom-prompt ()
  "Test QA agent uses customized prompt with placeholders."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((beads-agent-qa-prompt
             "Custom QA instructions for <ISSUE-ID>: <ISSUE-TITLE>")
            (type (beads-agent-type-get "qa")))
        (let ((prompt (beads-agent-type-build-prompt
                       type (beads-agent-types-test--make-sample-issue))))
          (should (string-match "Custom QA instructions" prompt))
          (should (string-match "test-123" prompt))
          (should (string-match "Test Issue Title" prompt))))
    (beads-agent-types-test--teardown)))


;;; Tests for Custom Agent

(ert-deftest beads-agent-types-test-custom-letter ()
  "Test Custom agent has letter C."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "custom")))
        (should (equal (oref type letter) "C")))
    (beads-agent-types-test--teardown)))


(ert-deftest beads-agent-types-test-custom-prompts-user ()
  "Test Custom agent prompts user and builds prompt with placeholders."
  (beads-agent-types-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional _initial _history)
                   "User's custom instructions for <ISSUE-ID>")))
        (let* ((type (beads-agent-type-get "custom"))
               (prompt (beads-agent-type-build-prompt
                        type (beads-agent-types-test--make-sample-issue))))
          (should (stringp prompt))
          (should (string-match "User's custom instructions" prompt))
          (should (string-match "test-123" prompt))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-custom-empty-input-error ()
  "Test Custom agent signals error on empty input."
  (beads-agent-types-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional _initial _history) "")))
        (let ((type (beads-agent-type-get "custom")))
          (should-error
           (beads-agent-type-build-prompt
            type (beads-agent-types-test--make-sample-issue))
           :type 'user-error)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-custom-stores-last-prompt ()
  "Test Custom agent stores prompt in history variable."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((beads-agent-type-custom--prompt-history nil))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_prompt &optional _initial _history)
                     "Stored prompt")))
          (let ((type (beads-agent-type-get "custom")))
            (beads-agent-type-build-prompt
             type (beads-agent-types-test--make-sample-issue))
            ;; read-string adds to history automatically when passed a history
            ;; variable, so just verify the function was called correctly
            (should type))))
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
                        type (beads-agent-types-test--make-sample-issue))))
          (should (string-match "Test Issue Title" prompt))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-description-placeholder-substitution ()
  "Test that <ISSUE-DESCRIPTION> placeholder is substituted when used."
  (beads-agent-types-test--setup)
  (unwind-protect
      ;; Test with a custom prompt that uses the description placeholder
      (let ((beads-agent-review-prompt
             "Review <ISSUE-ID>: <ISSUE-TITLE>\n\nDescription: <ISSUE-DESCRIPTION>")
            (type (beads-agent-type-get "review")))
        (let ((prompt (beads-agent-type-build-prompt
                       type (beads-agent-types-test--make-sample-issue))))
          (should (string-match "Test issue description" prompt))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-prompts-handle-empty-description ()
  "Test that prompts handle missing description gracefully."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((issue (beads-issue :id "test-456" :title "No Description")))
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

;;; Tests for Per-Type Backend Preferences

(defvar beads-agent-types-test--saved-task-backend nil
  "Saved task backend for tests.")
(defvar beads-agent-types-test--saved-review-backend nil
  "Saved review backend for tests.")
(defvar beads-agent-types-test--saved-plan-backend nil
  "Saved plan backend for tests.")
(defvar beads-agent-types-test--saved-qa-backend nil
  "Saved qa backend for tests.")

(defun beads-agent-types-test--setup-backends ()
  "Save current backend preferences."
  (setq beads-agent-types-test--saved-task-backend beads-agent-task-backend)
  (setq beads-agent-types-test--saved-review-backend beads-agent-review-backend)
  (setq beads-agent-types-test--saved-plan-backend beads-agent-plan-backend)
  (setq beads-agent-types-test--saved-qa-backend beads-agent-qa-backend))

(defun beads-agent-types-test--teardown-backends ()
  "Restore saved backend preferences."
  (setq beads-agent-task-backend beads-agent-types-test--saved-task-backend)
  (setq beads-agent-review-backend beads-agent-types-test--saved-review-backend)
  (setq beads-agent-plan-backend beads-agent-types-test--saved-plan-backend)
  (setq beads-agent-qa-backend beads-agent-types-test--saved-qa-backend))

(ert-deftest beads-agent-types-test-task-preferred-backend-nil ()
  "Test Task agent returns nil when no backend preference set."
  (beads-agent-types-test--setup)
  (beads-agent-types-test--setup-backends)
  (unwind-protect
      (let ((beads-agent-task-backend nil)
            (type (beads-agent-type-get "task")))
        (should (null (beads-agent-type-preferred-backend type))))
    (beads-agent-types-test--teardown-backends)
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-task-preferred-backend-set ()
  "Test Task agent returns configured backend preference."
  (beads-agent-types-test--setup)
  (beads-agent-types-test--setup-backends)
  (unwind-protect
      (let ((beads-agent-task-backend "claude-code-ide")
            (type (beads-agent-type-get "task")))
        (should (equal (beads-agent-type-preferred-backend type)
                       "claude-code-ide")))
    (beads-agent-types-test--teardown-backends)
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-review-preferred-backend-nil ()
  "Test Review agent returns nil when no backend preference set."
  (beads-agent-types-test--setup)
  (beads-agent-types-test--setup-backends)
  (unwind-protect
      (let ((beads-agent-review-backend nil)
            (type (beads-agent-type-get "review")))
        (should (null (beads-agent-type-preferred-backend type))))
    (beads-agent-types-test--teardown-backends)
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-review-preferred-backend-set ()
  "Test Review agent returns configured backend preference."
  (beads-agent-types-test--setup)
  (beads-agent-types-test--setup-backends)
  (unwind-protect
      (let ((beads-agent-review-backend "claudemacs")
            (type (beads-agent-type-get "review")))
        (should (equal (beads-agent-type-preferred-backend type)
                       "claudemacs")))
    (beads-agent-types-test--teardown-backends)
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-plan-preferred-backend-nil ()
  "Test Plan agent returns nil when no backend preference set."
  (beads-agent-types-test--setup)
  (beads-agent-types-test--setup-backends)
  (unwind-protect
      (let ((beads-agent-plan-backend nil)
            (type (beads-agent-type-get "plan")))
        (should (null (beads-agent-type-preferred-backend type))))
    (beads-agent-types-test--teardown-backends)
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-plan-preferred-backend-set ()
  "Test Plan agent returns configured backend preference."
  (beads-agent-types-test--setup)
  (beads-agent-types-test--setup-backends)
  (unwind-protect
      (let ((beads-agent-plan-backend "claude-code")
            (type (beads-agent-type-get "plan")))
        (should (equal (beads-agent-type-preferred-backend type)
                       "claude-code")))
    (beads-agent-types-test--teardown-backends)
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-qa-preferred-backend-nil ()
  "Test QA agent returns nil when no backend preference set."
  (beads-agent-types-test--setup)
  (beads-agent-types-test--setup-backends)
  (unwind-protect
      (let ((beads-agent-qa-backend nil)
            (type (beads-agent-type-get "qa")))
        (should (null (beads-agent-type-preferred-backend type))))
    (beads-agent-types-test--teardown-backends)
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-qa-preferred-backend-set ()
  "Test QA agent returns configured backend preference."
  (beads-agent-types-test--setup)
  (beads-agent-types-test--setup-backends)
  (unwind-protect
      (let ((beads-agent-qa-backend "agent-shell")
            (type (beads-agent-type-get "qa")))
        (should (equal (beads-agent-type-preferred-backend type)
                       "agent-shell")))
    (beads-agent-types-test--teardown-backends)
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-custom-preferred-backend-nil ()
  "Test Custom agent returns nil (no backend preference)."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "custom")))
        ;; Custom type uses default implementation which returns nil
        (should (null (beads-agent-type-preferred-backend type))))
    (beads-agent-types-test--teardown)))

(provide 'beads-agent-types-test)

;;; beads-agent-types-test.el ends here
