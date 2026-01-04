# Data Model: EIEIO Command System for Beads.el

**Date**: 2026-01-04
**Feature**: 001-eieio-command-system

## Overview

This document describes the EIEIO class hierarchy for beads.el commands. The model leverages existing infrastructure and extends it for complete bd CLI coverage.

## Class Hierarchy

```text
beads-command (abstract)
├── beads-command-json (abstract, adds --json support)
│   ├── beads-command-create
│   ├── beads-command-update
│   ├── beads-command-list
│   ├── beads-command-show
│   ├── beads-command-close
│   ├── beads-command-delete
│   ├── beads-command-reopen
│   ├── beads-command-doctor
│   ├── beads-command-ready
│   ├── beads-command-blocked
│   ├── beads-command-stale
│   ├── beads-command-count
│   ├── beads-command-status
│   ├── beads-command-info
│   ├── beads-command-search
│   ├── beads-command-duplicate
│   ├── beads-command-duplicates
│   ├── beads-command-orphans
│   ├── beads-command-lint
│   ├── beads-command-activity
│   ├── beads-command-graph
│   ├── beads-command-dep-*
│   ├── beads-command-label-*
│   ├── beads-command-comments-*
│   ├── beads-command-epic-*
│   ├── beads-command-gate-*
│   ├── beads-command-swarm-*
│   ├── beads-command-daemon-list
│   ├── beads-command-config-*
│   ├── beads-command-mol-*
│   ├── beads-command-formula-*
│   ├── beads-command-agent-*
│   ├── beads-command-slot-*
│   ├── beads-command-audit-*
│   ├── beads-command-worktree-*
│   ├── beads-command-jira-*
│   ├── beads-command-linear-*
│   ├── beads-command-repo-*
│   └── beads-command-admin-*
│
├── beads-command-init (no JSON)
├── beads-command-sync (no JSON, interactive)
├── beads-command-export (no JSON, file output)
├── beads-command-import (no JSON, file input)
├── beads-command-edit (no JSON, opens $EDITOR)
├── beads-command-move (no JSON)
├── beads-command-refile (no JSON)
├── beads-command-merge (no JSON, git merge driver)
├── beads-command-restore (no JSON)
├── beads-command-supersede (no JSON)
├── beads-command-cook (no JSON)
├── beads-command-defer (no JSON)
├── beads-command-undefer (no JSON)
├── beads-command-ship (no JSON)
├── beads-command-mail (no JSON)
├── beads-command-daemon-start (no JSON)
├── beads-command-daemon-stop (no JSON)
├── beads-command-hooks-* (no JSON)
├── beads-command-setup (no JSON)
├── beads-command-migrate-* (no JSON)
├── beads-command-repair (no JSON)
├── beads-command-rename-prefix (no JSON)
├── beads-command-preflight (no JSON)
├── beads-command-upgrade (no JSON)
├── beads-command-version (no JSON)
├── beads-command-human (no JSON)
├── beads-command-onboard (no JSON)
├── beads-command-prime (no JSON)
├── beads-command-quickstart (no JSON)
├── beads-command-where (no JSON)
└── beads-command-q (no JSON, minimal output)
```

## Core Entity: beads-command

### Slots (Inherited by All Commands)

| Slot | Type | Default | CLI Option | Description |
|------|------|---------|------------|-------------|
| actor | string \| nil | nil | --actor | Actor name for audit trail |
| db | string \| nil | nil | --db | Database path override |
| no-auto-flush | boolean | nil | --no-auto-flush | Disable JSONL sync |
| no-auto-import | boolean | nil | --no-auto-import | Disable JSONL import |
| no-daemon | boolean | nil | --no-daemon | Force direct mode |
| no-db | boolean | nil | --no-db | Use no-db mode |
| sandbox | boolean | nil | --sandbox | Sandbox mode |
| exit-code | integer \| nil | nil | - | Result: exit code |
| stdout | string \| nil | nil | - | Result: raw stdout |
| stderr | string \| nil | nil | - | Result: stderr |
| result | any | nil | - | Result: stdout (raw) or parsed data (JSON commands) |

### Required Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `beads-command-subcommand` | (command) → string | Return CLI subcommand(s) |
| `beads-command-validate` | (command) → string \| nil | Validate, return error or nil |
| `beads-command-execute` | (command) → command | Execute and populate result slots |
| `beads-command-execute-interactive` | (command) → nil | Execute with UI rendering |
| `beads-command-preview` | (command) → nil | Show command preview |

## Entity: beads-command-json

Extends `beads-command` with JSON output support.

### Additional Slots

| Slot | Type | Default | CLI Option | Description |
|------|------|---------|------------|-------------|
| json | boolean | t | --json | Output in JSON format |

### Method Overrides

- `beads-command-execute`: Parses JSON response into `result` slot (elisp values/objects)
- `beads-command-parse`: Convert JSON to domain objects and populate `result`

## Slot Metadata Properties

Each slot can have the following custom properties for auto-generation:

### CLI Properties

| Property | Type | Description |
|----------|------|-------------|
| :long-option | string | Long CLI option (e.g., "--title") |
| :short-option | string | Short CLI option (e.g., "-t") |
| :option-type | keyword | :string, :boolean, :integer, :list |
| :positional | integer | Position for positional args (1, 2, 3...) |
| :option-separator | string | Separator for :list type (default ",") |

### Transient Properties

| Property | Type | Description |
|----------|------|-------------|
| :transient-key | string | Key binding in menu |
| :transient-description | string | Display description |
| :transient-class | symbol | transient-option, transient-switch, etc. |
| :transient-reader | symbol | Input reader function |
| :transient-choices | list | Valid choices |
| :transient-prompt | string | Input prompt |
| :transient-argument | string | Transient argument format |
| :transient-level | integer | Menu visibility level (1-7) |
| :transient-group | string | Group name |
| :transient-order | integer | Order within group |

### Validation Properties

| Property | Type | Description |
|----------|------|-------------|
| :required | boolean | Is field required? |
| :validator | symbol | Validation function |

## Command File Organization

### File Naming Convention

| File Pattern | Contents |
|--------------|----------|
| `beads-command-<name>.el` | EIEIO class definition(s) for command |
| `beads-<name>.el` | Transient UI using `beads-meta-define-transient` |

### Grouping Rules

1. **Single command** → one file each for class and transient
   - Example: `beads-command-ready.el` + `beads-ready.el`

2. **Command with subcommands** → one class file, may have multiple transients
   - Example: `beads-command-daemon.el` contains:
     - `beads-command-daemon-list`
     - `beads-command-daemon-start`
     - `beads-command-daemon-stop`
     - `beads-command-daemon-status`
   - `beads-daemon.el` contains transients for all

## State Transitions

Commands follow a simple lifecycle:

```text
Created → Validated → Executed → Parsed
   ↓          ↓          ↓         ↓
 slots    validate   exit-code   result
 filled   errors?    stdout      populated
                     stderr
```

**Result slot semantics**:
- For non-JSON commands: `result` = `stdout` (raw string)
- For JSON commands: `result` = parsed elisp values/objects

### Execution Modes

| Mode | Method | Rendering |
|------|--------|-----------|
| Synchronous | `beads-command-execute` | Returns command object |
| Interactive | `beads-command-execute-interactive` | Uses terminal backend |
| Async | `beads-command-execute-async` | Callback on completion |

## Relationships

```text
beads-command 1 ─────────────* beads-command-slot (via class slots)
      │
      │ uses
      ▼
beads-meta (slot property infrastructure)
      │
      │ generates
      ▼
transient-prefix (via beads-meta-define-transient)
```

## Validation Rules

1. **Required slots**: Error if nil when `:required t`
2. **Type constraints**: EIEIO enforces slot types
3. **Custom validators**: Per-slot `:validator` functions
4. **Command-level**: `beads-command-validate` for cross-slot rules

## Example: Complete Command Definition

```elisp
(defclass beads-command-ready (beads-command-json)
  ((limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Maximum issues to show (--limit)."
    :long-option "--limit"
    :short-option "-n"
    :option-type :integer
    :transient-key "n"
    :transient-description "Limit results"
    :transient-class transient-option
    :transient-argument "--limit="
    :transient-prompt "Limit: "
    :transient-reader transient-read-number-N+
    :transient-group "Options"
    :transient-level 2
    :transient-order 1)
   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Output format (--format)."
    :long-option "--format"
    :option-type :string
    :transient-key "f"
    :transient-description "Output format"
    :transient-class transient-option
    :transient-argument "--format="
    :transient-choices ("table" "json" "compact")
    :transient-group "Options"
    :transient-level 2
    :transient-order 2)
   (include-deferred
    :initarg :include-deferred
    :type boolean
    :initform nil
    :documentation "Include deferred issues (--include-deferred)."
    :long-option "--include-deferred"
    :option-type :boolean
    :transient-key "d"
    :transient-description "Include deferred"
    :transient-class transient-switch
    :transient-argument "--include-deferred"
    :transient-group "Filters"
    :transient-level 3
    :transient-order 1))
  :documentation "Represents bd ready command.
Shows ready work (no blockers, open or in_progress).")

(cl-defmethod beads-command-subcommand ((_command beads-command-ready))
  "Return subcommand name for ready command."
  "ready")

(cl-defmethod beads-command-validate ((_command beads-command-ready))
  "Validate ready command. No required fields."
  nil)
```
