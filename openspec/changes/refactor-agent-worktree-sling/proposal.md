# Change: Refactor Agent System to Use Beads Worktree Commands

## Why

The beads.el agent system currently has its own git worktree implementation
in `beads-git.el` that duplicates functionality now available in the `bd
worktree` CLI commands. The bd worktree commands provide:

1. **Proper beads redirect setup** - Automatic `.beads/redirect` file pointing
   to main repo's database
2. **Safety checks** - Uncommitted changes, unpushed commits, stash detection
3. **Branch management** - Separate branch names from worktree names
4. **Consistent naming** - Managed worktree locations with gitignore entries

Additionally, the agent system needs to support "slinging" work to agents
more flexibly, enabling workflows where agents spawn in worktrees created
by bd commands. This supports both standalone beads.el usage and integration
with Gastown (external AI agent orchestration).

## What Changes

### 1. Replace Custom Worktree Implementation

- **REMOVE** custom `git worktree` calls in `beads-git.el`:
  - `beads-git-create-worktree`
  - `beads-git-create-worktree-async`
  - `beads-git--create-worktree-existing-branch-async`
  - `beads-git-ensure-worktree`
  - `beads-git-ensure-worktree-async`

- **ADD** EIEIO command class `beads-command-worktree` with subclasses:
  - `beads-command-worktree-create` - wraps `bd worktree create`
  - `beads-command-worktree-list` - wraps `bd worktree list --json`
  - `beads-command-worktree-remove` - wraps `bd worktree remove`
  - `beads-command-worktree-info` - wraps `bd worktree info --json`

### 2. Add Transient Menu for Worktree Management

- **ADD** `beads-worktree` transient prefix with:
  - Create worktree (with completing-read for name and branch)
  - List worktrees (display in tabulated-list buffer)
  - Remove worktree (with safety confirmation)
  - Info about current worktree

- **ADD** Reader functions for intelligent completion:
  - `beads-reader-worktree-name` - suggest issue IDs, branch names
  - `beads-reader-worktree-branch` - suggest existing branches
  - `beads-reader-worktree-existing` - complete from `bd worktree list`

### 3. Refactor Agent Spawning for "Sling" Workflow

- **MODIFY** `beads-agent-start` to:
  - Accept optional `worktree-path` parameter (pre-created worktree)
  - Use `bd worktree create` when creating new worktrees
  - Support "sling" pattern: agent starts in existing worktree

- **ADD** `beads-agent-sling` command:
  - Prompts for issue, worktree (create or select existing), and backend
  - Creates worktree via `bd worktree create` if needed
  - Starts agent in the specified worktree
  - Works with or without Gastown

### 4. Gastown Integration Support

- **ADD** `beads-agent-gastown` backend (when gastown.el available):
  - Implements `beads-agent-backend` protocol
  - Delegates to Gastown for actual agent spawning
  - Uses Gastown's hook-based worktree storage

- **MODIFY** agent backend selection to include Gastown when available

### 5. Worktree-Aware Completion

- **ADD** completion table for worktrees: `beads-completion-worktree-table`
- **ADD** annotations showing branch, beads state (redirect/shared/none)
- **ADD** grouping by beads state

## Impact

- **Affected code:**
  - `lisp/beads-git.el` - Remove custom worktree functions
  - `lisp/beads-agent.el` - Modify spawning to use bd worktree
  - `lisp/beads-command.el` - Add worktree command classes
  - `lisp/beads-reader.el` - Add worktree reader functions
  - `lisp/beads-completion.el` - Add worktree completion
  - `lisp/beads-main.el` - Add worktree menu entry
  - NEW: `lisp/beads-worktree.el` - Transient menu
  - NEW: `lisp/beads-agent-gastown.el` - Gastown backend (optional)

- **Breaking changes:** None (existing API remains compatible)

- **Dependencies:** Requires bd CLI with worktree subcommand support
