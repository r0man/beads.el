# Generic Method Contracts: EIEIO Command System

**Date**: 2026-01-04
**Feature**: 001-eieio-command-system

## Overview

This document defines the contracts for EIEIO generic methods that command classes must implement or may override. These contracts ensure consistency across all command implementations.

## Required Methods

### beads-command-subcommand

**Purpose**: Return the bd CLI subcommand string for this command.

**Signature**:
```elisp
(cl-defmethod beads-command-subcommand ((command COMMAND-CLASS))
  "Return subcommand string for COMMAND."
  STRING)
```

**Contract**:
- MUST return a non-empty string
- String MUST match bd CLI subcommand exactly
- For multi-level commands, return space-separated: `"daemon list"`
- Called by `beads-command-line` to build CLI invocation

**Examples**:
```elisp
;; Simple command
(cl-defmethod beads-command-subcommand ((_cmd beads-command-ready))
  "ready")

;; Nested command
(cl-defmethod beads-command-subcommand ((_cmd beads-command-daemon-list))
  "daemon list")

;; Command with hyphen
(cl-defmethod beads-command-subcommand ((_cmd beads-command-set-state))
  "set-state")
```

---

### beads-command-validate

**Purpose**: Validate command state before execution.

**Signature**:
```elisp
(cl-defmethod beads-command-validate ((command COMMAND-CLASS))
  "Validate COMMAND, return error string or nil."
  (or STRING nil))
```

**Contract**:
- MUST return `nil` if command is valid
- MUST return descriptive error string if invalid
- Should validate `:required t` slots have non-nil values
- Should validate cross-field dependencies
- Should NOT perform I/O or side effects
- Called before `beads-command-execute`

**Examples**:
```elisp
;; No required fields
(cl-defmethod beads-command-validate ((_cmd beads-command-ready))
  nil)

;; Required title field
(cl-defmethod beads-command-validate ((cmd beads-command-create))
  (unless (oref cmd title)
    "Title is required"))

;; Cross-field validation
(cl-defmethod beads-command-validate ((cmd beads-command-doctor))
  (when (and (oref cmd dry-run) (not (oref cmd fix)))
    "--dry-run requires --fix"))
```

---

## Optional Override Methods

### beads-command-execute

**Purpose**: Execute command synchronously and populate result slots.

**Signature**:
```elisp
(cl-defmethod beads-command-execute ((command COMMAND-CLASS))
  "Execute COMMAND and return it with populated result slots."
  COMMAND)
```

**Contract**:
- MUST return the command object (possibly modified)
- MUST populate `exit-code`, `stdout`, `stderr` slots
- SHOULD call `beads-command-parse` to populate `result` slot
- Default implementation builds CLI args and calls bd executable
- Override to customize execution behavior

**Result Slots**:
| Slot | Type | Description |
|------|------|-------------|
| exit-code | integer | Process exit code (0 = success) |
| stdout | string | Raw standard output |
| stderr | string | Standard error |
| result | any | stdout (raw) for non-JSON, parsed elisp values/objects for JSON |

---

### beads-command-execute-interactive

**Purpose**: Execute command with interactive UI rendering.

**Signature**:
```elisp
(cl-defmethod beads-command-execute-interactive ((command COMMAND-CLASS))
  "Execute COMMAND with interactive rendering."
  nil)
```

**Contract**:
- Called when user triggers "Execute" from transient menu
- Default uses terminal backend (vterm/eat/term/compilation)
- Override to provide custom rendering (tabulated-list, buffer, etc.)
- MAY modify command before execution (e.g., disable JSON for colored output)
- Should display results to user

**Examples**:
```elisp
;; Disable JSON for human-readable output
(cl-defmethod beads-command-execute-interactive ((cmd beads-command-doctor))
  (oset cmd json nil)
  (cl-call-next-method))

;; Custom tabulated-list rendering
(cl-defmethod beads-command-execute-interactive ((cmd beads-command-list))
  (let ((executed (beads-command-execute cmd)))
    (beads-list--display (oref executed result))))
```

---

### beads-command-preview

**Purpose**: Show preview of command that would be executed.

**Signature**:
```elisp
(cl-defmethod beads-command-preview ((command COMMAND-CLASS))
  "Show preview of COMMAND."
  nil)
```

**Contract**:
- MUST NOT execute the command
- Should display full command line in minibuffer or popup
- Called when user triggers "Preview" from transient menu
- Default implementation shows `beads-command-line` output

**Example**:
```elisp
(cl-defmethod beads-command-preview ((cmd beads-command-create))
  (message "Would execute: %s"
           (mapconcat #'shell-quote-argument
                      (beads-command-line cmd) " ")))
```

---

### beads-command-parse

**Purpose**: Parse raw output into domain objects and populate `result` slot.

**Signature**:
```elisp
(cl-defmethod beads-command-parse ((command COMMAND-CLASS))
  "Parse COMMAND output and populate result slot."
  COMMAND)
```

**Contract**:
- Called after execution with `stdout` populated
- MUST return the command object
- Should populate `result` slot with parsed/processed output
- For non-JSON commands: `result` = `stdout` (raw string)
- For JSON commands: `result` = parsed elisp values/objects
- For list commands: may convert to `beads-issue` objects

**Examples**:
```elisp
;; Default for non-JSON commands (in beads-command)
(cl-defmethod beads-command-parse ((cmd beads-command))
  (oset cmd result (oref cmd stdout))
  cmd)

;; JSON parsing (in beads-command-json)
(cl-defmethod beads-command-parse ((cmd beads-command-json))
  (when (oref cmd stdout)
    (oset cmd result (json-parse-string (oref cmd stdout)
                                         :object-type 'plist
                                         :array-type 'list)))
  cmd)

;; Parse list of issues into domain objects
(cl-defmethod beads-command-parse ((cmd beads-command-list))
  (cl-call-next-method)  ; Parse JSON first
  (oset cmd result (mapcar #'beads--parse-issue (oref cmd result)))
  cmd)
```

---

## Transient Integration Methods

### beads-command-from-transient-args

**Purpose**: Create command instance from transient arguments.

**Signature**:
```elisp
(defun COMMAND-from-transient-args (args)
  "Create command from transient ARGS."
  COMMAND-INSTANCE)
```

**Contract**:
- Generated by `beads-meta-define-transient` macro
- Parses transient arg strings into slot values
- Creates and returns new command instance
- Called by execute/preview suffixes

---

## Method Dispatch Order

1. User triggers action from transient
2. `beads-command-from-transient-args` creates instance
3. `beads-command-validate` checks validity
4. If valid:
   - Execute: `beads-command-execute-interactive` or `beads-command-execute`
   - Preview: `beads-command-preview`
5. `beads-command-parse` processes output (if using execute)

```text
┌─────────────────────────────────────────────────────────────────┐
│                    Transient Menu                               │
│  [Execute]  [Preview]  [Reset]                                  │
└──────┬─────────┬─────────────────────────────────────────────────┘
       │         │
       ▼         ▼
┌──────────────────┐
│ from-transient   │
│ -args            │
└────────┬─────────┘
         │
         ▼
┌──────────────────┐
│ validate         │──── error? ──► show error, abort
└────────┬─────────┘
         │ valid
    ┌────┴────┐
    │         │
    ▼         ▼
Execute     Preview
    │         │
    ▼         │
┌────────┐    │
│execute │    │
│-inter- │    │
│active  │    │
└───┬────┘    │
    │         │
    ▼         ▼
┌────────┐  ┌────────┐
│execute │  │preview │
└───┬────┘  └────────┘
    │
    ▼
┌────────┐
│ parse  │
└────────┘
```

## Error Handling Contract

All methods should follow these error handling patterns:

1. **Validation errors**: Return error string from `beads-command-validate`
2. **Execution errors**: Populate `exit-code` with non-zero, `stderr` with message
3. **Parse errors**: Signal `beads-error` with descriptive message
4. **Missing bd**: Signal `beads-error` with installation instructions

```elisp
;; Error signaling pattern
(when (not (executable-find beads-executable))
  (beads-error "bd not found. Install from: https://github.com/steveyegge/beads"))
```
