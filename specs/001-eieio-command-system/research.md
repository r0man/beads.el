# Research: EIEIO Command System for Beads.el

**Date**: 2026-01-04
**Feature**: 001-eieio-command-system

## Executive Summary

The beads.el codebase already has a well-designed EIEIO command infrastructure. This research documents the existing patterns and identifies what needs to be extended to cover all bd CLI commands.

## Existing Infrastructure Analysis

### Decision: Use Existing beads-meta Slot Property System

**Rationale**: The `beads-meta.el` module provides a complete infrastructure for:
- Custom slot properties (`:long-option`, `:transient-key`, `:positional`, etc.)
- Auto-generation of CLI arguments from slot metadata
- Auto-generation of transient infixes from slot metadata
- The `beads-meta-define-transient` macro for complete menu generation

**Alternatives Considered**:
- Manual transient definitions: Rejected - too verbose (100+ lines vs 1 macro call)
- Code generation scripts: Rejected - less maintainable, existing macro is superior

### Decision: Follow beads-command-doctor Pattern

**Rationale**: `beads-command-doctor.el` + `beads-doctor.el` demonstrate the reference pattern:

1. **Command class file** (`beads-command-<name>.el`):
   - Define EIEIO class extending `beads-command` or `beads-command-json`
   - Include all options as slots with full metadata
   - Implement `beads-command-subcommand` method
   - Implement `beads-command-validate` method
   - Optionally override `beads-command-execute-interactive` for custom rendering

2. **Transient UI file** (`beads-<name>.el`):
   - Require the command class file
   - Call `beads-meta-define-transient` macro
   - Add autoload cookie for the generated transient prefix

**Alternatives Considered**:
- Combined class + transient in one file: Rejected - separates concerns, easier testing
- Manual transient definitions: Rejected - not DRY, error-prone

### Decision: Terminal Backend Fallback Chain

**Rationale**: From clarifications - vterm → eat → term → compilation-mode provides:
- Best available terminal emulation
- Graceful degradation
- User-configurable override via `beads-terminal-backend`

**Implementation**: Already exists in `beads-command.el`:
- `beads-command--detect-best-backend`
- `beads-command--run-in-terminal`
- Individual runners for each backend

### Decision: Use `result` Slot (not `data`)

**Rationale**: From clarifications - the command result slot should be named `result`:
- For non-JSON commands: `result` = `stdout` (raw string)
- For JSON commands: `result` = parsed elisp values/objects
- Subclasses may further transform to domain objects (e.g., `beads-issue`)

**Implementation**: The `beads-command-parse` method populates `result`:
- Base `beads-command`: sets `result` to `stdout`
- `beads-command-json`: parses JSON and sets `result` to elisp values
- Command-specific overrides may convert to domain objects

## BD CLI Command Inventory

From `bd --help` output, commands grouped by category:

### Already Implemented (verify/extend)
- create, update, list, show, close, delete, reopen
- doctor, init, export, import, sync
- graph, dep, label, stats, epic-status

### Needs New Command Classes

**Working With Issues** (high priority):
- `comments` - View/manage comments
- `edit` - Edit issue field in $EDITOR
- `gate` - Manage async coordination gates
- `move` - Move issue to different rig
- `q` - Quick capture
- `refile` - Move issue to different rig
- `search` - Search issues by text
- `set-state` - Set operational state
- `state` - Query current state dimension

**Views & Reports**:
- `activity` - Real-time molecule state feed
- `count` - Count issues matching filters
- `lint` - Check issues for missing sections
- `stale` - Show stale issues
- `status` - Show database overview

**Dependencies & Structure**:
- `duplicate` - Mark as duplicate
- `duplicates` - Find/merge duplicates
- `epic` - Epic management (group)
- `supersede` - Mark as superseded
- `swarm` - Swarm management

**Sync & Data**:
- `daemon` - Daemon management (group: list, start, stop, status)
- `merge` - Git merge driver
- `restore` - Restore from git

**Setup & Configuration**:
- `config` - Manage configuration
- `hooks` - Manage git hooks
- `human` - Show essential commands
- `info` - Show database/daemon info
- `onboard` - Display AGENTS.md snippet
- `prime` - Output AI-optimized context
- `quickstart` - Quick start guide
- `setup` - Setup integration
- `where` - Show active beads location

**Maintenance**:
- `migrate` - Database migration (group)
- `preflight` - PR readiness checklist
- `repair` - Repair corrupted database
- `rename-prefix` - Rename issue prefix
- `upgrade` - Check/manage upgrades
- `worktree` - Manage git worktrees

**Integrations & Advanced**:
- `admin` - Administrative commands (group)
- `jira` - Jira integration (group)
- `linear` - Linear integration (group)
- `repo` - Multiple repository config

**Additional Commands**:
- `agent` - Manage agent bead state
- `audit` - Record agent interactions
- `blocked` - Show blocked issues
- `cook` - Compile formula to proto
- `defer` - Defer issues
- `formula` - Manage workflow formulas
- `mail` - Delegate to mail provider
- `mol` - Molecule commands
- `orphans` - Identify orphaned issues
- `ready` - Show ready work
- `ship` - Publish capability
- `slot` - Manage agent bead slots
- `undefer` - Undefer issues
- `version` - Print version

## Implementation Patterns

### Pattern 1: Simple JSON Command

```elisp
(defclass beads-command-ready (beads-command-json)
  ((format
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
    :transient-order 1))
  :documentation "Represents bd ready command.")

(cl-defmethod beads-command-subcommand ((_command beads-command-ready))
  "Return subcommand name."
  "ready")
```

### Pattern 2: Command with Subcommands (e.g., daemon)

```elisp
;; In beads-command-daemon.el
(defclass beads-command-daemon (beads-command)
  ()
  :abstract t
  :documentation "Abstract base for daemon commands.")

(defclass beads-command-daemon-list (beads-command-daemon beads-command-json)
  ((format ...))
  :documentation "List running daemons.")

(defclass beads-command-daemon-start (beads-command-daemon)
  ((foreground ...))
  :documentation "Start daemon.")

(cl-defmethod beads-command-subcommand ((_command beads-command-daemon-list))
  "daemon list")
```

### Pattern 3: Custom Rendering (e.g., list → tabulated-list)

```elisp
(cl-defmethod beads-command-execute-interactive ((cmd beads-command-list))
  "Execute CMD and display in tabulated-list buffer."
  (let ((result (beads-command-execute cmd)))
    (beads-list--display (oref result data))))
```

## Integration Test Infrastructure

### Decision: Use Temporary Beads Repositories

**Rationale**: Integration tests need real bd execution without polluting working directory.

**Implementation Plan**:
```elisp
(defmacro beads-test-with-temp-repo (&rest body)
  "Execute BODY with a temporary beads repository."
  `(let* ((temp-dir (make-temp-file "beads-test-" t))
          (default-directory temp-dir))
     (unwind-protect
         (progn
           (shell-command "git init && bd init -p test")
           ,@body)
       (delete-directory temp-dir t))))
```

**Alternatives Considered**:
- Mocking only: Rejected - need to verify real CLI behavior
- Using main repo: Rejected - would pollute working directory

## Documentation Approach

### Decision: Org-Based Manual with Transient Previews

**Rationale**: Per FR-015 through FR-017, manual must have synchronized previews.

**Implementation Plan**:
1. Create `doc/beads.org` with standard Info structure
2. Use Org source blocks with elisp to generate transient previews
3. Add CI check to verify previews match actual transients

**Alternatives Considered**:
- Markdown docs: Rejected - can't compile to Info
- Manual screenshots: Rejected - can't auto-verify synchronization

## Priority Order for Implementation

Based on FR and user story priorities:

### Phase 1: Core Infrastructure (P1)
1. Verify/extend existing command classes
2. Update `beads-main.el` to include all commands
3. Ensure all P1 commands work with default behavior

### Phase 2: Command Coverage (P1-P2)
1. Implement missing simple commands (ready, blocked, stale, count)
2. Implement command groups (daemon, config, hooks)
3. Implement complex commands (search, activity)

### Phase 3: Documentation & Testing (P2)
1. Create Org-based manual structure
2. Add transient preview generation
3. Create integration test infrastructure
4. Write integration tests for each module

### Phase 4: Polish & Validation (P3)
1. Terminal mode testing (tmux)
2. Preview synchronization verification
3. Code coverage analysis
