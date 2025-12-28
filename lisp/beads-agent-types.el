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

;;; Per-Type Backend Preferences

(defcustom beads-agent-task-backend nil
  "Preferred backend for Task agents.
If nil, uses `beads-agent-default-backend' or first available.
Set to a backend name string to prefer a specific backend for Task agents."
  :type '(choice (const :tag "Use default" nil)
                 (string :tag "Backend name"))
  :group 'beads-agent-types)

(defcustom beads-agent-review-backend nil
  "Preferred backend for Review agents.
If nil, uses `beads-agent-default-backend' or first available.
Set to a backend name string to prefer a specific backend for Review agents."
  :type '(choice (const :tag "Use default" nil)
                 (string :tag "Backend name"))
  :group 'beads-agent-types)

(defcustom beads-agent-plan-backend nil
  "Preferred backend for Plan agents.
If nil, uses `beads-agent-default-backend' or first available.
Set to a backend name string to prefer a specific backend for Plan agents."
  :type '(choice (const :tag "Use default" nil)
                 (string :tag "Backend name"))
  :group 'beads-agent-types)

(defcustom beads-agent-qa-backend nil
  "Preferred backend for QA agents.
If nil, uses `beads-agent-default-backend' or first available.
Set to a backend name string to prefer a specific backend for QA agents."
  :type '(choice (const :tag "Use default" nil)
                 (string :tag "Backend name"))
  :group 'beads-agent-types)

(defcustom beads-agent-review-prompt
  "You are a code review agent. Please work on beads issue <ISSUE-ID>: <ISSUE-TITLE>.

# Constraints

- Read-only: DO NOT modify source files or create new files
- Only use: read files, search code, explore the codebase
- Exception: update the beads issue with your review findings

# Review Focus

- Code quality and readability
- Potential bugs or edge cases
- Security vulnerabilities
- Style consistency with the project
- Performance considerations

Provide specific, actionable feedback with file paths and line references.

# Output

Update the beads issue with your review findings:

```
bd update <ISSUE-ID> --notes \"$(cat <<'EOF'
## Code Review Summary

**Overall Assessment:** <APPROVE / REQUEST_CHANGES / NEEDS_DISCUSSION>

### Findings

<Numbered list of specific issues with file:line references>

### Recommendations

<Prioritized list of suggested improvements>
EOF
)\"
```"
  "Prompt template for the Review agent.
Placeholders <ISSUE-ID>, <ISSUE-TITLE>, and <ISSUE-DESCRIPTION> are replaced
with issue data when the prompt is built."
  :type 'string
  :group 'beads-agent-types)

(defcustom beads-agent-qa-prompt
  "You are a QA agent. Please work on beads issue <ISSUE-ID>: <ISSUE-TITLE>.

# Constraints

- Testing focus: Run tests, verify acceptance criteria, check for regressions
- May run commands: test commands, build commands, verification scripts
- Read-only for source: DO NOT modify source files
- Exception: update the beads issue with your test results

# QA Workflow

1. **Review Acceptance Criteria** - Understand what needs to be verified
2. **Run Tests** - Execute relevant test suite, note any failures
3. **Manual Verification** - Test edge cases and error handling
4. **Check Regressions** - Verify related functionality still works
5. **Document Results** - Update issue with findings

# Output

Update the beads issue with your QA results. Update the --acceptance field \
to mark verified criteria:

```
bd update <ISSUE-ID> \\
  --acceptance \"$(cat <<'EOF'
- [x] First criterion (verified: <how>)
- [x] Second criterion (verified: <how>)
- [ ] Third criterion (FAILED: <reason>)
EOF
)\" \\
  --notes \"$(cat <<'EOF'
## QA Summary

**Test Results:** <PASS / FAIL / PARTIAL>

### Tests Run
- <test-name>: PASS/FAIL

### Manual Verification
<What was tested manually and results>

### Issues Found
<List any bugs or concerns discovered>
EOF
)\"
```"
  "Prompt template for the QA agent.
Placeholders <ISSUE-ID>, <ISSUE-TITLE>, and <ISSUE-DESCRIPTION> are replaced
with issue data when the prompt is built."
  :type 'string
  :group 'beads-agent-types)

(defcustom beads-agent-plan-prompt
  "You are a planning agent. Please work on beads issue <ISSUE-ID>: <ISSUE-TITLE>.

Create a detailed implementation plan WITHOUT making any code changes.

# Constraints

- Read-only for code: DO NOT modify source files or create new files
- Only use: read files, search code, explore the codebase
- Exception: update the beads issue with your plan when done

# Planning Steps

1. **Analyze** - Understand current codebase structure, identify relevant files, \
note patterns and conventions

2. **Design** - Break down work into specific actionable steps, list exact file \
paths that need changes with descriptions, consider dependencies between changes

3. **Define Acceptance Criteria** - Create testable criteria based on requirements, \
or refine existing ones if the issue has them. Show what tests will verify each.

# Plan Review

Before finalizing, verify:
- [ ] All requirements from the issue are addressed
- [ ] Edge cases are considered
- [ ] Scope is appropriate (not over-engineered)
- [ ] Changes are minimal and focused

# Output

Present your complete plan in your response, then update the beads issue. \
Preserve important context from the original description while adding your analysis:

```
bd update <ISSUE-ID> \\
  --description \"$(cat <<'EOF'
<Refined description preserving original context and adding analysis>
EOF
)\" \\
  --design \"$(cat <<'EOF'
<Complete implementation plan with all steps and file changes>
EOF
)\" \\
  --acceptance \"$(cat <<'EOF'
- [ ] First testable criterion
- [ ] Second testable criterion
EOF
)\" \\
  --notes \"$(cat <<'EOF'
Plan created. Key changes: <brief summary of main modifications>
EOF
)\"
```"
  "Prompt template for the Plan agent.
Placeholders <ISSUE-ID>, <ISSUE-TITLE>, and <ISSUE-DESCRIPTION> are replaced
with issue data when the prompt is built."
  :type 'string
  :group 'beads-agent-types)

;;; Task Agent

(defconst beads-agent-type-task--prompt
  "You are a task-completion agent for beads. Please work on beads issue <ISSUE-ID>: <ISSUE-TITLE>.

# Constraints

- Stay focused on the assigned task
- Don't make unrelated changes
- If blocked, explain clearly what's needed
- Communicate progress and decisions

# Agent Workflow

1. **Claim the Task**
   - Update issue status to in_progress: `bd update <ISSUE-ID> --status in_progress`
   - Read the task description carefully
   - Check acceptance criteria if available

2. **Execute the Task**
   - Use available tools to complete the work
   - Follow best practices from project documentation
   - Run tests if applicable
   - Keep changes focused on the task

3. **Track Discoveries**
   - If you find bugs, TODOs, or related work:
     - File new issues using bd create
     - Link them with discovered-from dependencies: `bd dep add <new-id> --type \
discovered-from --target <ISSUE-ID>`
   - This maintains context for future work

4. **Verify Completion**
   - Check that all acceptance criteria are met
   - Ensure tests pass
   - Review your changes for quality

# Output

When work is complete, close the issue with a clear summary:

```
bd close <ISSUE-ID> --reason \"$(cat <<'EOF'
<Summary of what was accomplished, any important decisions made, and verification \
performed>
EOF
)\"
```

If blocked, update the issue status and explain:

```
bd update <ISSUE-ID> --status blocked --notes \"$(cat <<'EOF'
<Clear explanation of what is blocking progress and what is needed to proceed>
EOF
)\"
```"
  "Embedded prompt template for Task agent type.
Placeholders <ISSUE-ID>, <ISSUE-TITLE>, and <ISSUE-DESCRIPTION> are replaced
with issue data when the prompt is built.")

(defclass beads-agent-type-task (beads-agent-type)
  ((name :initform "Task")
   (letter :initform "T")
   (description :initform "Autonomous task completion agent")
   (prompt-template :initform 'beads-agent-type-task--prompt))
  :documentation "Task agent type for autonomous task completion.
Uses a structured prompt that guides the agent through understanding,
executing, and verifying task completion.")

(cl-defmethod beads-agent-type-preferred-backend ((_type beads-agent-type-task))
  "Return preferred backend for Task agents."
  beads-agent-task-backend)

;;; Review Agent

(defclass beads-agent-type-review (beads-agent-type)
  ((name :initform "Review")
   (letter :initform "R")
   (description :initform "Code review agent")
   (prompt-template :initform 'beads-agent-review-prompt))
  :documentation "Review agent type for code review.
Uses the customizable `beads-agent-review-prompt' template.")

(cl-defmethod beads-agent-type-preferred-backend ((_type beads-agent-type-review))
  "Return preferred backend for Review agents."
  beads-agent-review-backend)

;;; Plan Agent

(defclass beads-agent-type-plan (beads-agent-type)
  ((name :initform "Plan")
   (letter :initform "P")
   (description :initform "Planning agent (read-only analysis)")
   (prompt-template :initform 'beads-agent-plan-prompt))
  :documentation "Plan agent type for implementation planning.
Uses a prompt that instructs the agent to analyze and plan without
making changes.  Works with any backend.")

(cl-defmethod beads-agent-type-preferred-backend ((_type beads-agent-type-plan))
  "Return preferred backend for Plan agents."
  beads-agent-plan-backend)

;;; QA Agent

(defclass beads-agent-type-qa (beads-agent-type)
  ((name :initform "QA")
   (letter :initform "Q")
   (description :initform "Testing and quality assurance agent")
   (prompt-template :initform 'beads-agent-qa-prompt))
  :documentation "QA agent type for testing and verification.
Uses the customizable `beads-agent-qa-prompt' template.")

(cl-defmethod beads-agent-type-preferred-backend ((_type beads-agent-type-qa))
  "Return preferred backend for QA agents."
  beads-agent-qa-backend)

;;; Custom Agent

(defclass beads-agent-type-custom (beads-agent-type)
  ((name :initform "Custom")
   (letter :initform "C")
   (description :initform "User-provided prompt at runtime")
   (prompt-template :initform nil))
  :documentation "Custom agent type with user-provided prompt.
Prompts the user for a prompt string via the minibuffer.")

(defvar beads-agent-type-custom--prompt-history nil
  "History list for Custom agent prompts.
Used by `completing-read' for prompt history navigation.")

(cl-defmethod beads-agent-type-build-prompt ((_type beads-agent-type-custom)
                                              issue)
  "Prompt user for custom prompt for TYPE and combine with ISSUE.
ISSUE is a beads-issue EIEIO object.

The user-provided prompt can contain placeholders:
  <ISSUE-ID>          - The issue ID
  <ISSUE-TITLE>       - The issue title
  <ISSUE-DESCRIPTION> - The issue description"
  (let* ((issue-id (oref issue id))
         (issue-title (oref issue title))
         (issue-desc (or (oref issue description) ""))
         (user-prompt (read-string
                       (format "Custom prompt for %s: " issue-id)
                       nil  ; initial-input
                       'beads-agent-type-custom--prompt-history)))
    (when (string-empty-p user-prompt)
      (user-error "Custom prompt cannot be empty"))
    ;; Replace placeholders in user prompt
    (thread-last user-prompt
      (string-replace "<ISSUE-ID>" issue-id)
      (string-replace "<ISSUE-TITLE>" issue-title)
      (string-replace "<ISSUE-DESCRIPTION>" issue-desc))))

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
