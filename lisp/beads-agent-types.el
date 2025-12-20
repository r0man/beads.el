;;; beads-agent-types.el --- Built-in agent types -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the five built-in agent types for beads.el:
;;
;;   - Task (T): Autonomous task completion agent
;;   - Review (R): Code review agent with customizable prompt
;;   - Plan (P): Planning agent requiring backend plan mode
;;   - QA (Q): Testing/quality assurance agent
;;   - Custom (C): User-provided prompt at runtime
;;
;; All types are registered automatically when this module is loaded.

;;; Code:

(require 'beads-agent-type)

;;; Customization

(defgroup beads-agent-types nil
  "Built-in agent types for beads."
  :group 'beads
  :prefix "beads-agent-")

(defcustom beads-agent-review-prompt
  "You are a code review agent. Please review the code for this issue.

Focus on:
- Code quality and readability
- Potential bugs or edge cases
- Security vulnerabilities
- Style consistency with the project
- Performance considerations

Provide specific, actionable feedback with line references."
  "Prompt template for the Review agent.
This prompt is combined with issue context when starting a Review agent."
  :type 'string
  :group 'beads-agent-types)

(defcustom beads-agent-qa-prompt
  "You are a QA agent. Please verify the implementation for this issue.

Focus on:
- Running relevant tests and checking they pass
- Testing edge cases and error handling
- Verifying the acceptance criteria are met
- Checking for regressions in related functionality

Report your findings with specific test results."
  "Prompt template for the QA agent.
This prompt is combined with issue context when starting a QA agent."
  :type 'string
  :group 'beads-agent-types)

(defcustom beads-agent-plan-prompt
  "You are a planning agent. Your task is to create a detailed implementation \
plan WITHOUT making any changes.

# Critical Constraints

- DO NOT modify any files - only read and analyze
- DO NOT execute commands that change state (no writes, no git commits)
- DO NOT create new files or directories
- ONLY use read operations: read files, search code, explore the codebase

# Your Task

Create a comprehensive implementation plan that includes:

1. **Analysis of Current State**
   - Understand the existing codebase structure
   - Identify relevant files and components
   - Note patterns and conventions used

2. **Implementation Strategy**
   - Break down the work into specific, actionable steps
   - Identify files that need to be modified or created
   - Consider dependencies between changes

3. **Risk Assessment**
   - Potential breaking changes
   - Edge cases to handle
   - Testing requirements

4. **Acceptance Criteria Mapping**
   - How each acceptance criterion will be satisfied
   - What tests should verify each criterion

# Output Format

Provide a structured plan that a human or another AI agent can follow
to implement the changes. Be specific about file paths, function names,
and the nature of changes needed."
  "Prompt template for the Plan agent.
This prompt instructs the agent to plan without making changes."
  :type 'string
  :group 'beads-agent-types)

;;; Task Agent

(defconst beads-agent-type-task--prompt
  "You are a task-completion agent for beads. Your goal is to complete the \
assigned task autonomously.

# Agent Workflow

1. **Understand the Task**
   - Read the task description carefully
   - Check acceptance criteria if available
   - Understand the context and dependencies

2. **Execute the Task**
   - Use available tools to complete the work
   - Follow best practices from project documentation
   - Run tests if applicable
   - Keep changes focused on the task

3. **Track Discoveries**
   - If you find bugs, TODOs, or related work:
     - File new issues using bd create
     - Link them with discovered-from dependencies
   - This maintains context for future work

4. **Verify Completion**
   - Check that all acceptance criteria are met
   - Ensure tests pass
   - Review your changes for quality

# Important Guidelines

- Stay focused on the assigned task
- Don't make unrelated changes
- If blocked, explain clearly what's needed
- Communicate progress and decisions"
  "Embedded prompt template for Task agent type.
Based on beads task-agent.md agent specification.")

(defclass beads-agent-type-task (beads-agent-type)
  ((name :initform "Task")
   (letter :initform "T")
   (description :initform "Autonomous task completion agent"))
  :documentation "Task agent type for autonomous task completion.
Uses a structured prompt that guides the agent through understanding,
executing, and verifying task completion.")

(cl-defmethod beads-agent-type-build-prompt ((type beads-agent-type-task) issue)
  "Build task prompt for TYPE with ISSUE using embedded template.
ISSUE is a beads-issue EIEIO object."
  (ignore type)
  (let ((issue-id (oref issue id))
        (issue-title (oref issue title))
        (issue-desc (or (oref issue description) "")))
    (format "%s\n\n## Issue: %s\n\n**Title:** %s\n\n**Description:**\n%s"
            beads-agent-type-task--prompt issue-id issue-title issue-desc)))

;;; Review Agent

(defclass beads-agent-type-review (beads-agent-type)
  ((name :initform "Review")
   (letter :initform "R")
   (description :initform "Code review agent"))
  :documentation "Review agent type for code review.
Uses the customizable `beads-agent-review-prompt' template.")

(cl-defmethod beads-agent-type-build-prompt ((type beads-agent-type-review)
                                              issue)
  "Build review prompt for TYPE with ISSUE using the defcustom template.
ISSUE is a beads-issue EIEIO object."
  (ignore type)
  (let ((issue-id (oref issue id))
        (issue-title (oref issue title))
        (issue-desc (or (oref issue description) "")))
    (format "%s\n\n## Issue: %s\n\n**Title:** %s\n\n**Description:**\n%s"
            beads-agent-review-prompt issue-id issue-title issue-desc)))

;;; Plan Agent

(defclass beads-agent-type-plan (beads-agent-type)
  ((name :initform "Plan")
   (letter :initform "P")
   (description :initform "Planning agent (read-only analysis)"))
  :documentation "Plan agent type for implementation planning.
Uses a prompt that instructs the agent to analyze and plan without
making changes.  Works with any backend.")

(cl-defmethod beads-agent-type-build-prompt ((_type beads-agent-type-plan)
                                              issue)
  "Build plan prompt for TYPE with ISSUE using the defcustom template.
ISSUE is a beads-issue EIEIO object."
  (let ((issue-id (oref issue id))
        (issue-title (oref issue title))
        (issue-desc (or (oref issue description) "")))
    (format "%s\n\n## Issue: %s\n\n**Title:** %s\n\n**Description:**\n%s"
            beads-agent-plan-prompt issue-id issue-title issue-desc)))

;;; QA Agent

(defclass beads-agent-type-qa (beads-agent-type)
  ((name :initform "QA")
   (letter :initform "Q")
   (description :initform "Testing and quality assurance agent"))
  :documentation "QA agent type for testing and verification.
Uses the customizable `beads-agent-qa-prompt' template.")

(cl-defmethod beads-agent-type-build-prompt ((type beads-agent-type-qa) issue)
  "Build QA prompt for TYPE with ISSUE using the defcustom template.
ISSUE is a beads-issue EIEIO object."
  (ignore type)
  (let ((issue-id (oref issue id))
        (issue-title (oref issue title))
        (issue-desc (or (oref issue description) "")))
    (format "%s\n\n## Issue: %s\n\n**Title:** %s\n\n**Description:**\n%s"
            beads-agent-qa-prompt issue-id issue-title issue-desc)))

;;; Custom Agent

(defclass beads-agent-type-custom (beads-agent-type)
  ((name :initform "Custom")
   (letter :initform "C")
   (description :initform "User-provided prompt at runtime")
   (prompt-template :initform nil))
  :documentation "Custom agent type with user-provided prompt.
Prompts the user for a prompt string via the minibuffer.")

(defvar beads-agent-type-custom--last-prompt nil
  "Last prompt used for Custom agent type.
Stored for history during minibuffer prompt.")

(cl-defmethod beads-agent-type-build-prompt ((_type beads-agent-type-custom)
                                              issue)
  "Prompt user for custom prompt for TYPE and combine with ISSUE.
ISSUE is a beads-issue EIEIO object."
  (let* ((issue-id (oref issue id))
         (issue-title (oref issue title))
         (issue-desc (or (oref issue description) ""))
         (user-prompt (read-string
                       (format "Custom prompt for %s: " issue-id)
                       beads-agent-type-custom--last-prompt)))
    (when (string-empty-p user-prompt)
      (user-error "Custom prompt cannot be empty"))
    (setq beads-agent-type-custom--last-prompt user-prompt)
    (format "%s\n\n## Issue: %s\n\n**Title:** %s\n\n**Description:**\n%s"
            user-prompt issue-id issue-title issue-desc)))

;;; Registration

(defvar beads-agent-types--builtin-registered nil
  "Non-nil if built-in types have been registered.")

(defun beads-agent-types-register-builtin ()
  "Register all built-in agent types.
This function is idempotent and can be called multiple times."
  (unless beads-agent-types--builtin-registered
    (beads-agent-type-register (beads-agent-type-task))
    (beads-agent-type-register (beads-agent-type-review))
    (beads-agent-type-register (beads-agent-type-plan))
    (beads-agent-type-register (beads-agent-type-qa))
    (beads-agent-type-register (beads-agent-type-custom))
    (setq beads-agent-types--builtin-registered t)))

;; Register built-in types at load time
(beads-agent-types-register-builtin)

(provide 'beads-agent-types)

;;; beads-agent-types.el ends here
