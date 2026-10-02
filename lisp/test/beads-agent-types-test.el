;;; beads-agent-types-test.el --- Tests for built-in agent types -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-agent-types.el built-in agent types.
;; Tests cover the 3 built-in types: Task, Review (incl. QA mode), Plan.
;; QA and Custom are intentionally not classes (F3).

;;; Code:

(require 'ert)
(require 'beads-agent-types)
(require 'beads-types)

;;; Test Fixtures

(defvar beads-agent-types-test--saved-registry nil
  "Saved registry to restore after tests.")

(defvar beads-agent-types-test--saved-review-prompt nil
  "Saved review prompt for tests.")

(defvar beads-agent-types-test--saved-review-qa-prompt nil
  "Saved Review QA prompt for tests.")

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
  (setq beads-agent-types-test--saved-review-qa-prompt beads-agent-review-qa-prompt)
  ;; Clear and re-register to ensure clean state
  (beads-agent-type--clear-registry)
  (setq beads-agent-types--builtin-registered nil)
  (beads-agent-types-register-builtin))

(defun beads-agent-types-test--teardown ()
  "Teardown test fixtures."
  ;; Restore saved state
  (setq beads-agent-type--registry beads-agent-types-test--saved-registry)
  (setq beads-agent-review-prompt beads-agent-types-test--saved-review-prompt)
  (setq beads-agent-review-qa-prompt beads-agent-types-test--saved-review-qa-prompt)
  (setq beads-agent-types-test--saved-registry nil))

;;; Tests for Registration

(ert-deftest beads-agent-types-test-all-registered ()
  "Test that all 3 built-in types are registered."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((types (beads-agent-type-list)))
        (should (= (length types) 3))
        (should (beads-agent-type-get "task"))
        (should (beads-agent-type-get "review"))
        (should (beads-agent-type-get "plan"))
        (should-not (beads-agent-type-get "qa"))
        (should-not (beads-agent-type-get "custom")))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-registration-idempotent ()
  "Test that registering built-in types multiple times is safe."
  (beads-agent-types-test--setup)
  (unwind-protect
      (progn
        ;; Register again - should not duplicate
        (beads-agent-types-register-builtin)
        (beads-agent-types-register-builtin)
        (should (= (length (beads-agent-type-list)) 3)))
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
  "Test Task agent splits role (system) from issue envelope (user)."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let* ((type (beads-agent-type-get "task"))
             (issue (beads-agent-types-test--make-sample-issue))
             (sys (beads-agent-type-system-prompt type issue))
             (user (beads-agent-type-build-user-prompt type issue)))
        (should (stringp sys))
        (should (stringp user))
        ;; Role text lives in the SYSTEM prompt.
        (should (string-match "task-completion agent" sys))
        (should (string-match "Claim the Task" sys))
        ;; Issue context lives in the USER prompt.
        (should (string-match "test-123" user))
        (should (string-match "Test Issue Title" user)))
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
             (issue (beads-agent-types-test--make-sample-issue))
             (sys (beads-agent-type-system-prompt type issue))
             (user (beads-agent-type-build-user-prompt type issue)))
        ;; Role focus in SYSTEM; issue context in USER.
        (should (string-match "code review" sys))
        (should (string-match "Code quality" sys))
        (should (string-match "Security" sys))
        (should (string-match "test-123" user)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-review-custom-prompt ()
  "Test Review agent uses customized prompt with placeholders."
  (beads-agent-types-test--setup)
  (unwind-protect
      ;; Customising the role defcustom now feeds the SYSTEM prompt.
      (let ((beads-agent-review-prompt
             "Custom review instructions for <ISSUE-ID>: <ISSUE-TITLE>")
            (type (beads-agent-type-get "review")))
        (let ((sys (beads-agent-type-system-prompt
                    type (beads-agent-types-test--make-sample-issue))))
          (should (string-match "Custom review instructions" sys))
          (should (string-match "test-123" sys))
          (should (string-match "Test Issue Title" sys))))
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
             (issue (beads-agent-types-test--make-sample-issue))
             (sys (beads-agent-type-system-prompt type issue))
             (user (beads-agent-type-build-user-prompt type issue)))
        ;; Role text in SYSTEM; issue context in USER.
        (should (string-match "planning agent" sys))
        (should (string-match "DO NOT modify" sys))
        (should (string-match "test-123" user))
        (should (string-match "Test Issue Title" user)))
    (beads-agent-types-test--teardown)))

;;; Tests for Review QA mode

(ert-deftest beads-agent-types-test-review-qa-mode-prompt ()
  "Test Review QA mode swaps in the QA role and output envelope."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let* ((type (beads-agent-type-review-qa))
             (issue (beads-agent-types-test--make-sample-issue))
             (sys (beads-agent-type-system-prompt type issue))
             (user (beads-agent-type-build-user-prompt type issue)))
        (should (equal (oref type name) "Review"))
        (should (oref type qa-mode))
        (should (string-match "QA agent" sys))
        (should (string-match "Run Tests" sys))
        (should (string-match "QA Summary" user))
        (should (string-match "test-123" user)))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-review-default-not-qa ()
  "Test a default Review instance is not in QA mode and keeps its text."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let* ((type (beads-agent-type-review))
             (issue (beads-agent-types-test--make-sample-issue)))
        (should-not (oref type qa-mode))
        (should (string-match "code review"
                              (beads-agent-type-system-prompt type issue))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-review-qa-custom-prompt ()
  "Test the relocated QA prompt defcustom feeds the Review QA path."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((beads-agent-review-qa-prompt
             "Custom QA instructions for <ISSUE-ID>: <ISSUE-TITLE>")
            (type (beads-agent-type-review-qa)))
        (let ((sys (beads-agent-type-system-prompt
                    type (beads-agent-types-test--make-sample-issue))))
          (should (string-match "Custom QA instructions" sys))
          (should (string-match "test-123" sys))
          (should (string-match "Test Issue Title" sys))))
    (beads-agent-types-test--teardown)))

;;; Tests for Plan Agent

;;; Tests for Completion Support

(ert-deftest beads-agent-types-test-completion-all-types ()
  "Test all 3 types appear in completion and QA/Custom do not."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((table (beads-agent-type-completion-table)))
        (let ((completions (all-completions "" table)))
          (should (member "Task" completions))
          (should (member "Review" completions))
          (should (member "Plan" completions))
          (should-not (member "QA" completions))
          (should-not (member "Custom" completions))))
    (beads-agent-types-test--teardown)))

;;; Tests for Issue Context Integration

(ert-deftest beads-agent-types-test-prompts-include-issue-title ()
  "Test that prompts include issue title."
  (beads-agent-types-test--setup)
  (unwind-protect
      (dolist (type-name '("task" "review" "plan"))
        (let* ((type (beads-agent-type-get type-name))
               (prompt (beads-agent-type-build-user-prompt
                        type (beads-agent-types-test--make-sample-issue))))
          (should (string-match "Test Issue Title" prompt))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-description-placeholder-substitution ()
  "Test that <ISSUE-DESCRIPTION> placeholder is substituted when used."
  (beads-agent-types-test--setup)
  (unwind-protect
      ;; Test with a custom prompt that uses the description placeholder
      ;; Customised role defcustom feeds SYSTEM; <ISSUE-DESCRIPTION>
      ;; still substitutes there.
      (let ((beads-agent-review-prompt
             "Review <ISSUE-ID>: <ISSUE-TITLE>\n\nDescription: <ISSUE-DESCRIPTION>")
            (type (beads-agent-type-get "review")))
        (let ((sys (beads-agent-type-system-prompt
                    type (beads-agent-types-test--make-sample-issue))))
          (should (string-match "Test issue description" sys))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-prompts-handle-empty-description ()
  "Test that prompts handle missing description gracefully."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((issue (beads-issue :id "test-456" :title "No Description")))
        (dolist (type-name '("task" "review" "plan"))
          (let* ((type (beads-agent-type-get type-name))
                 (prompt (beads-agent-type-build-user-prompt type issue)))
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

;;; Tests for Icon Slot

(ert-deftest beads-agent-types-test-task-icon ()
  "Test Task agent has the eagle icon (U+1F985).
The eagle is the apex-predator role for the Task agent: sharp eye on
the target, decisive strike — autonomous work delivered end to end."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "task")))
        (should (equal (oref type icon) "🦅"))
        (should (equal (oref type icon) (string #x1F985))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-review-icon ()
  "Test Review agent has the deer icon (U+1F98C).
Single-codepoint deer — alert, careful forager — the temperament
of the Review role: pause, look, weigh."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "review")))
        (should (equal (oref type icon) "🦌"))
        (should (equal (oref type icon) (string #x1F98C))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-plan-icon ()
  "Test Plan agent has the raccoon icon (U+1F99D).
Inquisitive, dextrous problem-solver — the planning role pries open
the box, tries pieces, lays out the route."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-type-get "plan")))
        (should (equal (oref type icon) "🦝"))
        (should (equal (oref type icon) (string #x1F99D))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-each-has-icon ()
  "Test that every built-in type has a non-empty icon string."
  (beads-agent-types-test--setup)
  (unwind-protect
      (dolist (type (beads-agent-type-list))
        (let ((icon (oref type icon)))
          (should (stringp icon))
          (should (> (length icon) 0))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-icons-are-unique ()
  "Test that each built-in type uses a distinct icon string."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((icons nil))
        (dolist (type (beads-agent-type-list))
          (let ((icon (oref type icon)))
            (should-not (member icon icons))
            (push icon icons))))
    (beads-agent-types-test--teardown)))

(ert-deftest beads-agent-types-test-icon-slot-default-nil ()
  "Test that the icon slot defaults to nil on a bare subclass.
A new beads-agent-type subclass that does not set :icon should have
icon = nil so the letter slot remains the fallback."
  (beads-agent-types-test--setup)
  (unwind-protect
      (progn
        (defclass beads-agent-types-test--no-icon-type (beads-agent-type)
          ((name :initform "NoIconTest")
           (letter :initform "Z"))
          :documentation "Test-only subclass without an icon.")
        (let ((type (beads-agent-types-test--no-icon-type)))
          (should (null (oref type icon)))))
    (beads-agent-types-test--teardown)))

;;; Tests for Per-Type Backend Preferences

(defvar beads-agent-types-test--saved-task-backend nil
  "Saved task backend for tests.")
(defvar beads-agent-types-test--saved-review-backend nil
  "Saved review backend for tests.")
(defvar beads-agent-types-test--saved-plan-backend nil
  "Saved plan backend for tests.")

(defun beads-agent-types-test--setup-backends ()
  "Save current backend preferences."
  (setq beads-agent-types-test--saved-task-backend beads-agent-task-backend)
  (setq beads-agent-types-test--saved-review-backend beads-agent-review-backend)
  (setq beads-agent-types-test--saved-plan-backend beads-agent-plan-backend))

(defun beads-agent-types-test--teardown-backends ()
  "Restore saved backend preferences."
  (setq beads-agent-task-backend beads-agent-types-test--saved-task-backend)
  (setq beads-agent-review-backend beads-agent-types-test--saved-review-backend)
  (setq beads-agent-plan-backend beads-agent-types-test--saved-plan-backend))

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

(ert-deftest beads-agent-types-test-review-qa-mode-preferred-backend ()
  "Test Review QA mode still uses the Review backend preference."
  (beads-agent-types-test--setup)
  (beads-agent-types-test--setup-backends)
  (unwind-protect
      (let ((beads-agent-review-backend "agent-shell")
            (type (beads-agent-type-review-qa)))
        (should (equal (beads-agent-type-preferred-backend type)
                       "agent-shell")))
    (beads-agent-types-test--teardown-backends)
    (beads-agent-types-test--teardown)))

;;; F3 deletion: classes gone, registry still open

(defclass beads-agent-types-test--out-of-tree (beads-agent-type)
  ((name :initform "OutOfTreeQA")
   (letter :initform "Z")
   (description :initform "Out-of-tree test role"))
  :documentation "Test-only out-of-tree role for registry openness.")

(ert-deftest beads-agent-types-test-qa-custom-classes-gone ()
  "Test the QA and Custom classes are undefined."
  (should-not (find-class 'beads-agent-type-qa nil))
  (should-not (find-class 'beads-agent-type-custom nil))
  (should-not (fboundp 'beads-agent-type-qa))
  (should-not (fboundp 'beads-agent-type-custom)))

(ert-deftest beads-agent-types-test-registry-open-out-of-tree ()
  "Test an out-of-tree role still registers after F3."
  (beads-agent-types-test--setup)
  (unwind-protect
      (let ((type (beads-agent-types-test--out-of-tree)))
        (beads-agent-type-register type)
        (should (eq (beads-agent-type-get "OutOfTreeQA") type))
        (should (eq (beads-agent-type-get-by-letter "Z") type)))
    (beads-agent-type--unregister "OutOfTreeQA")
    (beads-agent-types-test--teardown)))

(provide 'beads-agent-types-test)

;;; beads-agent-types-test.el ends here
