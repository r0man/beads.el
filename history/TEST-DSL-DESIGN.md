# EIEIO-Based Test DSL Design for beads.el

## Executive Summary

This document describes a comprehensive EIEIO-based Domain Specific Language
(DSL) for testing beads.el user workflows. The DSL models user actions as
first-class objects, enabling declarative test definitions that read like
user stories while maintaining full programmatic control.

## Design Goals

1. **Declarative Workflows**: Express tests as sequences of user actions
2. **Composability**: Build complex workflows from simple, reusable actions
3. **Rich Assertions**: Comprehensive helpers for verifying outcomes
4. **Stateful Execution**: Track side effects (buffers, caches, messages)
5. **ERT Integration**: Seamless integration with Emacs Lisp Regression Testing
6. **Debugging Support**: Clear error messages and execution traces

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                        Test Definition                          │
│  (beads-test-workflow                                           │
│    :name "create-and-update-issue"                              │
│    :steps (list (action-open-transient ...)                     │
│                 (action-set-infix ...)                          │
│                 (action-submit ...)))                           │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Workflow Runner                            │
│  - Sets up test context (mocks, temp project)                   │
│  - Executes actions sequentially                                │
│  - Collects side effects and state changes                      │
│  - Runs assertions                                              │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Test Context                               │
│  - Tracks created buffers, messages, cache invalidations        │
│  - Provides assertion helpers                                   │
│  - Manages cleanup                                              │
└─────────────────────────────────────────────────────────────────┘
```

## Core Classes

### 1. Base Action Class

```elisp
(defclass beads-test-action ()
  ((description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Human-readable description of this action.")
   (pre-conditions
    :initarg :pre-conditions
    :type list
    :initform nil
    :documentation "List of predicates that must be true before execution.")
   (post-assertions
    :initarg :post-assertions
    :type list
    :initform nil
    :documentation "List of assertions to run after execution."))
  :abstract t
  :documentation "Abstract base class for all test actions.
Subclasses implement `beads-test-action-execute' to perform the action.")
```

### 2. Transient Actions

```elisp
;;; Opening a transient menu
(defclass beads-test-action-open-transient (beads-test-action)
  ((command
    :initarg :command
    :type symbol
    :documentation "The transient command to invoke (e.g., 'beads-create)."))
  :documentation "Action that opens a transient menu.")

;;; Setting an infix value
(defclass beads-test-action-set-infix (beads-test-action)
  ((key
    :initarg :key
    :type string
    :documentation "The key sequence to press (e.g., \"t\" for title).")
   (value
    :initarg :value
    :type (or string number null)
    :documentation "The value to set.")
   (reader-response
    :initarg :reader-response
    :type (or string null)
    :initform nil
    :documentation "Response for interactive readers (completing-read, etc.)."))
  :documentation "Action that sets a transient infix value.")

;;; Submitting/executing a transient suffix
(defclass beads-test-action-execute-suffix (beads-test-action)
  ((key
    :initarg :key
    :type string
    :initform "x"
    :documentation "The key to press to execute (default: \"x\").")
   (expect-error
    :initarg :expect-error
    :type (or null symbol string)
    :initform nil
    :documentation "Expected error type or message pattern."))
  :documentation "Action that executes a transient suffix command.")

;;; Previewing (non-destructive)
(defclass beads-test-action-preview (beads-test-action)
  ((key
    :initarg :key
    :type string
    :initform "P"
    :documentation "The key to press to preview."))
  :documentation "Action that triggers a preview in a transient.")

;;; Resetting transient state
(defclass beads-test-action-reset-transient (beads-test-action)
  ((confirm
    :initarg :confirm
    :type boolean
    :initform t
    :documentation "Whether to confirm the reset prompt."))
  :documentation "Action that resets transient state.")

;;; Quitting a transient
(defclass beads-test-action-quit-transient (beads-test-action)
  ()
  :documentation "Action that quits/cancels the current transient.")
```

### 3. Buffer Actions

```elisp
;;; Navigating in a list buffer
(defclass beads-test-action-list-navigate (beads-test-action)
  ((direction
    :initarg :direction
    :type symbol
    :documentation "Direction: 'next, 'previous, 'first, 'last.")
   (count
    :initarg :count
    :type integer
    :initform 1
    :documentation "Number of items to move."))
  :documentation "Action that navigates in a tabulated-list buffer.")

;;; Selecting/activating an item
(defclass beads-test-action-list-select (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Specific issue ID to select, or nil for current.")
   (method
    :initarg :method
    :type symbol
    :initform 'ret
    :documentation "Selection method: 'ret (RET key), 'mouse, 'goto."))
  :documentation "Action that selects an item in a list buffer.")

;;; Marking issues (for bulk operations)
(defclass beads-test-action-list-mark (beads-test-action)
  ((issue-ids
    :initarg :issue-ids
    :type list
    :initform nil
    :documentation "List of issue IDs to mark, or nil for current.")
   (unmark
    :initarg :unmark
    :type boolean
    :initform nil
    :documentation "If t, unmark instead of mark."))
  :documentation "Action that marks/unmarks issues in a list buffer.")

;;; Refreshing a buffer
(defclass beads-test-action-refresh-buffer (beads-test-action)
  ()
  :documentation "Action that refreshes the current buffer (g key).")

;;; Sorting a list
(defclass beads-test-action-list-sort (beads-test-action)
  ((column
    :initarg :column
    :type string
    :documentation "Column name to sort by.")
   (reverse
    :initarg :reverse
    :type boolean
    :initform nil
    :documentation "If t, reverse the sort order."))
  :documentation "Action that sorts a tabulated-list by column.")
```

### 4. Completion Actions

```elisp
;;; Responding to completing-read
(defclass beads-test-action-complete (beads-test-action)
  ((response
    :initarg :response
    :type (or string list)
    :documentation "The completion response (string or list for multi).")
   (method
    :initarg :method
    :type symbol
    :initform 'exact
    :documentation "Completion method: 'exact, 'first, 'match-prefix."))
  :documentation "Action that responds to a completion prompt.")

;;; Responding to yes-or-no prompts
(defclass beads-test-action-confirm (beads-test-action)
  ((response
    :initarg :response
    :type boolean
    :documentation "Whether to confirm (t) or deny (nil).")
   (prompt-pattern
    :initarg :prompt-pattern
    :type (or null string)
    :initform nil
    :documentation "Optional regex to match against the prompt."))
  :documentation "Action that responds to a yes/no confirmation prompt.")

;;; Responding to read-string prompts
(defclass beads-test-action-input-string (beads-test-action)
  ((response
    :initarg :response
    :type string
    :documentation "The string to input.")
   (prompt-pattern
    :initarg :prompt-pattern
    :type (or null string)
    :initform nil
    :documentation "Optional regex to match against the prompt."))
  :documentation "Action that provides input for read-string prompts.")
```

### 5. High-Level Workflow Actions

```elisp
;;; Composite action for filling a transient and submitting
(defclass beads-test-action-transient-workflow (beads-test-action)
  ((command
    :initarg :command
    :type symbol
    :documentation "The transient command to invoke.")
   (args
    :initarg :args
    :type list
    :documentation "Alist of (key . value) pairs to set.")
   (action
    :initarg :action
    :type symbol
    :initform 'execute
    :documentation "Final action: 'execute, 'preview, 'reset, 'quit.")
   (confirmations
    :initarg :confirmations
    :type list
    :initform '(t)
    :documentation "List of responses for any confirmation prompts."))
  :documentation "High-level action that opens a transient, fills values, and submits.")

;;; Creating an issue (convenience wrapper)
(defclass beads-test-action-create-issue (beads-test-action)
  ((title
    :initarg :title
    :type string
    :documentation "Issue title (required).")
   (type
    :initarg :type
    :type (or null string)
    :initform nil
    :documentation "Issue type (bug, feature, task, epic, chore).")
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Priority (0-4).")
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Issue description.")
   (show-after
    :initarg :show-after
    :type boolean
    :initform nil
    :documentation "Whether to show the issue after creation."))
  :documentation "High-level action that creates an issue via transient.")

;;; Updating an issue (convenience wrapper)
(defclass beads-test-action-update-issue (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type string
    :documentation "ID of issue to update.")
   (changes
    :initarg :changes
    :type list
    :documentation "Alist of (field . value) changes to apply."))
  :documentation "High-level action that updates an issue via transient.")

;;; Closing an issue (convenience wrapper)
(defclass beads-test-action-close-issue (beads-test-action)
  ((issue-id
    :initarg :issue-id
    :type string
    :documentation "ID of issue to close.")
   (reason
    :initarg :reason
    :type string
    :documentation "Reason for closing."))
  :documentation "High-level action that closes an issue via transient.")
```

### 6. Test Context Class

```elisp
(defclass beads-test-context ()
  ((project-dir
    :initarg :project-dir
    :type (or null string)
    :initform nil
    :documentation "Temporary project directory.")
   (created-buffers
    :initarg :created-buffers
    :type list
    :initform nil
    :documentation "List of buffers created during test.")
   (messages
    :initarg :messages
    :type list
    :initform nil
    :documentation "List of messages displayed during test.")
   (errors
    :initarg :errors
    :type list
    :initform nil
    :documentation "List of errors signaled during test.")
   (cache-invalidations
    :initarg :cache-invalidations
    :type list
    :initform nil
    :documentation "Plist of cache invalidation events.")
   (command-executions
    :initarg :command-executions
    :type list
    :initform nil
    :documentation "List of bd commands executed.")
   (created-issues
    :initarg :created-issues
    :type list
    :initform nil
    :documentation "List of issues created during test.")
   (execution-trace
    :initarg :execution-trace
    :type list
    :initform nil
    :documentation "Trace of all actions executed."))
  :documentation "Tracks state and side effects during test execution.")
```

### 7. Workflow Class

```elisp
(defclass beads-test-workflow ()
  ((name
    :initarg :name
    :type string
    :documentation "Human-readable name of the workflow.")
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Longer description of what this workflow tests.")
   (tags
    :initarg :tags
    :type list
    :initform nil
    :documentation "ERT tags for this test (e.g., :integration, :slow).")
   (setup
    :initarg :setup
    :type (or null function)
    :initform nil
    :documentation "Function to run before workflow (receives context).")
   (teardown
    :initarg :teardown
    :type (or null function)
    :initform nil
    :documentation "Function to run after workflow (receives context).")
   (steps
    :initarg :steps
    :type list
    :documentation "List of beads-test-action instances.")
   (assertions
    :initarg :assertions
    :type list
    :initform nil
    :documentation "List of final assertions (functions receiving context)."))
  :documentation "Represents a complete user workflow test.")
```

## Generic Methods

### Action Execution

```elisp
(cl-defgeneric beads-test-action-execute (action context)
  "Execute ACTION within CONTEXT.
Returns the result of the action (action-specific).
Modifies CONTEXT to track side effects.")

(cl-defgeneric beads-test-action-validate (action context)
  "Validate that ACTION can be executed in current CONTEXT.
Returns nil if valid, or an error message string.")

(cl-defgeneric beads-test-action-description (action)
  "Return a human-readable description of ACTION.")
```

### Workflow Execution

```elisp
(cl-defgeneric beads-test-workflow-run (workflow)
  "Execute WORKFLOW and return the final context.
Creates a fresh context, runs setup, executes all steps,
runs assertions, and performs teardown.")

(cl-defgeneric beads-test-workflow-to-ert (workflow)
  "Generate an ERT test definition from WORKFLOW.
Returns a form suitable for `eval' or inclusion in a test file.")
```

## Assertion Helpers

```elisp
;;; Context assertions
(defun beads-test-assert-buffer-exists (context buffer-name-pattern)
  "Assert that a buffer matching BUFFER-NAME-PATTERN was created.")

(defun beads-test-assert-buffer-contains (context buffer-name text)
  "Assert that BUFFER-NAME contains TEXT.")

(defun beads-test-assert-message-displayed (context pattern)
  "Assert that a message matching PATTERN was displayed.")

(defun beads-test-assert-error-signaled (context error-type)
  "Assert that an error of ERROR-TYPE was signaled.")

(defun beads-test-assert-no-errors (context)
  "Assert that no errors were signaled during execution.")

;;; Cache assertions
(defun beads-test-assert-completion-cache-invalidated (context)
  "Assert that the completion cache was invalidated.")

(defun beads-test-assert-label-cache-invalidated (context)
  "Assert that the label cache was invalidated.")

;;; Issue assertions
(defun beads-test-assert-issue-created (context &rest props)
  "Assert that an issue with PROPS was created.
PROPS is a plist of :title, :type, :priority, etc.")

(defun beads-test-assert-issue-exists (context issue-id)
  "Assert that ISSUE-ID exists in the project.")

(defun beads-test-assert-issue-has (context issue-id &rest props)
  "Assert that ISSUE-ID has properties PROPS.")

(defun beads-test-assert-issue-count (context expected)
  "Assert that EXPECTED number of issues exist.")

;;; Buffer state assertions
(defun beads-test-assert-current-buffer (context name-pattern)
  "Assert that current buffer name matches NAME-PATTERN.")

(defun beads-test-assert-point-at-issue (context issue-id)
  "Assert that point is on the line for ISSUE-ID in a list buffer.")

(defun beads-test-assert-marked-issues (context issue-ids)
  "Assert that exactly ISSUE-IDS are marked in the list buffer.")

;;; Command assertions
(defun beads-test-assert-command-executed (context command-pattern)
  "Assert that a bd command matching COMMAND-PATTERN was executed.")

(defun beads-test-assert-command-count (context expected)
  "Assert that EXPECTED number of bd commands were executed.")
```

## Macro for Defining Tests

```elisp
(defmacro beads-define-workflow-test (name &rest body)
  "Define an ERT test from a workflow specification.

NAME is the test name (symbol).
BODY contains :keyword value pairs and workflow steps.

Keywords:
  :description  - Test description string
  :tags         - List of ERT tags
  :setup        - Setup function (lambda (ctx) ...)
  :teardown     - Teardown function (lambda (ctx) ...)
  :steps        - List of action instances
  :assert       - List of assertion functions

Example:
  (beads-define-workflow-test create-issue-workflow
    :description \"Test creating an issue via transient menu\"
    :tags (:integration)
    :steps
    ((beads-test-action-create-issue
      :title \"Test Issue\"
      :type \"bug\"
      :priority 1))
    :assert
    ((beads-test-assert-issue-created ctx :title \"Test Issue\")
     (beads-test-assert-completion-cache-invalidated ctx)))"
  (declare (indent 1))
  ...)
```

## Usage Examples

### Example 1: Simple Issue Creation

```elisp
(beads-define-workflow-test beads-workflow-create-simple-issue
  :description "Create an issue with minimal fields"
  :tags (:integration :workflow)
  :steps
  ((beads-test-action-create-issue
    :title "Fix login bug"
    :type "bug"
    :priority 1
    :show-after nil))
  :assert
  ((beads-test-assert-issue-created ctx :title "Fix login bug" :type "bug")
   (beads-test-assert-completion-cache-invalidated ctx)
   (beads-test-assert-message-displayed ctx "Created issue")))
```

### Example 2: Full Transient Workflow

```elisp
(beads-define-workflow-test beads-workflow-create-with-all-fields
  :description "Create issue using full transient interaction"
  :tags (:integration :workflow :slow)
  :steps
  ((beads-test-action-open-transient :command 'beads-create)
   (beads-test-action-set-infix :key "t" :value "Complete feature")
   (beads-test-action-set-infix :key "T" :value "feature")
   (beads-test-action-set-infix :key "p" :value "1")
   (beads-test-action-set-infix :key "d" :value "Full description here")
   (beads-test-action-preview :key "P")  ; Preview first
   (beads-test-action-execute-suffix :key "x")
   (beads-test-action-confirm :response nil))  ; Don't show after
  :assert
  ((beads-test-assert-no-errors ctx)
   (beads-test-assert-issue-created ctx
     :title "Complete feature"
     :type "feature"
     :priority 1)))
```

### Example 3: List Navigation and Update

```elisp
(beads-define-workflow-test beads-workflow-list-and-update
  :description "Navigate list, select issue, update status"
  :tags (:integration :workflow)
  :setup
  (lambda (ctx)
    ;; Create some issues first
    (beads-command-create! :title "Issue 1" :priority 1)
    (beads-command-create! :title "Issue 2" :priority 2)
    (beads-command-create! :title "Issue 3" :priority 3))
  :steps
  ((beads-test-action-open-transient :command 'beads-list)
   (beads-test-action-execute-suffix :key "x")  ; Execute list
   (beads-test-action-list-navigate :direction 'next :count 1)
   (beads-test-action-list-select :method 'ret)  ; Open update
   (beads-test-action-set-infix :key "s" :value "in_progress")
   (beads-test-action-execute-suffix :key "x"))
  :assert
  ((beads-test-assert-issue-has ctx "Issue 2" :status "in_progress")))
```

### Example 4: Bulk Operations

```elisp
(beads-define-workflow-test beads-workflow-bulk-close
  :description "Mark multiple issues and close them in bulk"
  :tags (:integration :workflow :bulk)
  :setup
  (lambda (ctx)
    (dotimes (i 5)
      (beads-command-create! :title (format "Bulk issue %d" i))))
  :steps
  ((beads-test-action-open-transient :command 'beads-list)
   (beads-test-action-execute-suffix)
   ;; Mark issues 1, 2, 3
   (beads-test-action-list-mark :issue-ids '("bd-1" "bd-2" "bd-3"))
   ;; Bulk close
   (beads-test-action-key-sequence :keys "B c")
   (beads-test-action-input-string :response "Completed in bulk")
   (beads-test-action-confirm :response t))
  :assert
  ((beads-test-assert-issue-has ctx "bd-1" :status "closed")
   (beads-test-assert-issue-has ctx "bd-2" :status "closed")
   (beads-test-assert-issue-has ctx "bd-3" :status "closed")
   (beads-test-assert-issue-has ctx "bd-4" :status "open")))
```

### Example 5: Error Handling

```elisp
(beads-define-workflow-test beads-workflow-validation-error
  :description "Test that validation errors are handled properly"
  :tags (:integration :workflow :error)
  :steps
  ((beads-test-action-open-transient :command 'beads-create)
   ;; Don't set title (required field)
   (beads-test-action-set-infix :key "T" :value "bug")
   (beads-test-action-execute-suffix
    :key "x"
    :expect-error 'user-error))
  :assert
  ((beads-test-assert-error-signaled ctx 'user-error)
   (beads-test-assert-message-displayed ctx "title")))
```

## Implementation Strategy

### Phase 1: Core Infrastructure
1. Define base classes (`beads-test-action`, `beads-test-context`, `beads-test-workflow`)
2. Implement generic methods (`beads-test-action-execute`, etc.)
3. Create context setup/teardown utilities
4. Integrate with existing `beads-test-with-project` macro

### Phase 2: Basic Actions
1. Transient actions (open, set-infix, execute, preview, reset, quit)
2. Confirmation actions (yes/no prompts)
3. Input actions (read-string, completing-read)

### Phase 3: Buffer Actions
1. List navigation and selection
2. Mark/unmark operations
3. Refresh and sort

### Phase 4: High-Level Actions
1. Composite workflows (`beads-test-action-create-issue`, etc.)
2. Chain multiple low-level actions

### Phase 5: Assertions
1. Issue state assertions
2. Buffer state assertions
3. Side effect assertions (caches, messages, commands)

### Phase 6: ERT Integration
1. `beads-define-workflow-test` macro
2. Test discovery and organization
3. Reporting enhancements

## File Organization

```
lisp/test/
├── beads-test-dsl.el           ; Core DSL classes and methods
├── beads-test-actions.el       ; All action class definitions
├── beads-test-assertions.el    ; Assertion helper functions
├── beads-test-context.el       ; Context management
├── beads-test-runner.el        ; Workflow execution engine
├── beads-test-macros.el        ; User-facing macros
└── beads-test-workflows/       ; Example workflow tests
    ├── create-workflows.el
    ├── update-workflows.el
    ├── list-workflows.el
    └── bulk-workflows.el
```

## Benefits

1. **Readability**: Tests read like user stories
2. **Maintainability**: Change action implementation without changing tests
3. **Reusability**: Compose workflows from shared action sequences
4. **Debugging**: Execution trace helps diagnose failures
5. **Coverage**: Systematically test user journeys
6. **Documentation**: Workflows serve as executable documentation

## Comparison with Current Approach

| Aspect | Current | DSL |
|--------|---------|-----|
| Test structure | Procedural with cl-letf | Declarative action sequences |
| Mocking | Manual per-test | Automatic via context |
| Side effects | Ad-hoc tracking | Systematic context tracking |
| Reusability | Copy-paste patterns | Composable action objects |
| Readability | Implementation-focused | User-story focused |
| Debugging | Stack traces | Action-level traces |

## Open Questions

1. **Async Support**: How to handle async operations (background processes)?
2. **Visual Testing**: Should we support screenshot comparisons?
3. **Fuzzing**: Can we generate random action sequences for property testing?
4. **Performance**: Should we track timing for performance regression testing?

## Next Steps

1. Review and approve this design
2. Implement Phase 1 (core infrastructure)
3. Migrate one existing test file as proof of concept
4. Iterate based on feedback
5. Complete remaining phases
