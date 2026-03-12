---
name: beads
description: Expert knowledge for the bd (beads) CLI issue tracker. Use when working with beads issues, bd commands, .beads/ config files, or dependency management workflows.
---

# Beads (bd) CLI Expert

Beads is a lightweight, dependency-aware issue tracker with first-class
support for agent workflows, Git integration, and Dolt-powered storage.
The CLI is `bd`. This skill provides comprehensive reference for all
commands, flags, and workflows.

## When to Use This Skill

Invoke this skill when:
- Running `bd` commands (create, update, close, show, list, etc.)
- Managing issue dependencies and blockers
- Setting up beads in a new repository
- Debugging `.beads/` configuration or Dolt server issues
- Building automation around the bd CLI
- Understanding the issue lifecycle and status transitions
- Working with molecules (multi-step workflows), epics, or swarms

## Core Concepts

### Issue Lifecycle

Issues flow through these statuses:
- **open** — unstarted work, eligible for `bd ready`
- **in_progress** — claimed and active
- **blocked** — has unresolved blockers (use `bd blocked` to see these)
- **deferred** — hidden until a future date
- **hooked** — assigned to an agent's hook
- **closed** — completed

**No `done` or `complete` status exists** — only `closed`.

### Dependency Semantics (Critical)

Dependencies express "X **needs** Y", not "X comes before Y":

```
bd dep add <dependent> <prerequisite>
```

**WRONG mental model** (temporal): "phase-1 blocks phase-2"
```bash
# WRONG:
bd dep add phase1 phase2   # This means phase1 needs phase2
```

**CORRECT mental model** (requirement): "phase-2 needs phase-1"
```bash
# CORRECT:
bd dep add phase2 phase1   # phase2 depends on phase1
```

Also: `bd dep <blocker> --blocks <blocked>` is a shorthand that reads
naturally: "blocker blocks blocked".

### Issue IDs

IDs are prefixed per-project (e.g., `bd-42`, `be-218`, `gt-7`, `hq-abc`).
The prefix identifies which rig/database owns the issue.
`bd` commands auto-route to the correct database by prefix.

### .beads/ Directory

Created by `bd init`. Contains:
- `*.db` — SQLite or Dolt database redirect file
- `config.yaml` — Project-level configuration
- Gitignored by default in most setups

## Key Commands Reference

### Finding Work

```bash
# Show unblocked, open issues (not in_progress/hooked/deferred)
bd ready

# Find ready work in a specific molecule
bd ready --mol bd-abc

# Show all open issues (flat list)
bd list --status open

# Show blocked issues (dependency-blocked)
bd blocked

# List all issues (including closed)
bd list --all

# Filter by type, assignee, label, priority
bd list -t bug -a alice -l security -p 0
bd list --label-any "P0,P1" --type feature

# Search by text
bd search "authentication"
```

### Viewing Issues

```bash
# Show full details
bd show bd-42

# Short one-line view
bd show bd-42 --short

# Show with all fields (agent, gate, extended metadata)
bd show bd-42 --long

# Show children of an issue
bd show bd-42 --children

# Show reverse references (what references this issue)
bd show bd-42 --refs

# Watch for changes (auto-refresh)
bd show bd-42 --watch

# Show as of a specific Dolt commit
bd show bd-42 --as-of <commit-hash>
```

### Creating Issues

```bash
# Basic creation
bd create "Issue title"

# Full creation with metadata
bd create "Fix auth bug" \
  -t bug \
  -p 1 \
  -d "Description of the problem" \
  -a alice \
  -l security,authentication \
  --due +2d

# Create with dependencies
bd create "Phase 2 work" \
  --deps "discovered-from:bd-10,bd-11"

# Quick capture (outputs ID only, for scripting)
bd q "Quick note"

# Create in a different rig
bd create "Bug in gastown" --rig gastown
bd create "Bug" --prefix gt-

# Dry run (preview without creating)
bd create "Test" --dry-run

# Create multiple from markdown file
bd create -f issues.md

# Create ephemeral (short-lived) issue
bd create "Temporary note" --ephemeral
```

**Issue Types:** `bug`, `feature`, `task`, `epic`, `chore`, `decision`
- Aliases: `enhancement`/`feat` → `feature`, `dec`/`adr` → `decision`

**Priority:** `0` (critical) through `4` (backlog), default `2`

### Updating Issues

```bash
# Update fields
bd update bd-42 --title "New title"
bd update bd-42 --status in_progress
bd update bd-42 -p 1 -a bob
bd update bd-42 --notes "Progress update"
bd update bd-42 --append-notes "Additional note"

# Atomically claim (sets assignee=you, status=in_progress; fails if taken)
bd update bd-42 --claim

# Add/remove labels
bd update bd-42 --add-label security
bd update bd-42 --remove-label wontfix
bd update bd-42 --set-labels "security,high-priority"

# Defer until future date
bd update bd-42 --defer "2025-06-01"
bd update bd-42 --defer "+3d"
bd update bd-42 --defer ""    # Clear defer

# Set due date
bd update bd-42 --due "+1w"
bd update bd-42 --due "next monday"
bd update bd-42 --due ""      # Clear

# Update last-touched issue (no ID needed)
bd update --notes "Progress"
```

**Date formats:** `+6h`, `+1d`, `+2w`, `tomorrow`, `next monday`,
`2025-01-15`, RFC3339

### Closing Issues

```bash
# Close with reason
bd close bd-42 --reason "Completed implementation"

# Close without changes needed
bd close bd-42 --reason "no-changes: already fixed in bd-10"

# Close and suggest next unblocked issues
bd close bd-42 --suggest-next

# Close molecule step and auto-advance
bd close bd-42 --continue

# Force close pinned issues
bd close bd-42 --force

# Alias: bd done bd-42
bd done bd-42 --reason "Done"
```

### Dependencies

```bash
# Add dependency: bd-42 needs bd-10
bd dep add bd-42 bd-10

# Shorthand: bd-10 blocks bd-42
bd dep bd-10 --blocks bd-42

# Remove dependency
bd dep remove bd-42 bd-10

# List dependencies of an issue
bd dep list bd-42

# Detect cycles
bd dep cycles

# Visualize dependency tree
bd dep tree bd-42

# Create bidirectional relates_to link
bd dep relate bd-42 bd-55
```

### Labels

```bash
# Add label to issue
bd label add bd-42 security

# Remove label
bd label remove bd-42 security

# List labels on an issue
bd label list bd-42

# List all labels in database
bd label list-all

# Propagate parent label to children
bd label propagate bd-42 security
```

### Epics and Hierarchy

```bash
# Create epic
bd create "Large feature" -t epic

# Create child issue
bd create "Subtask" --parent bd-epic

# List epic with children
bd epic list
bd show bd-epic --children
```

### Molecules (Multi-Step Workflows)

Molecules are structured sequences of steps (wisps) with a formula
template. Used by Gas Town agents for multi-phase work.

```bash
# Check current molecule step
bd mol current

# List all molecules
bd mol list

# Show molecule status
bd mol status

# Promote a wisp to permanent bead
bd promote bd-wisp-xyz
```

### Swarms

```bash
# Swarm management for structured epics with parallel workers
bd swarm --help
```

## Environment Variables

| Variable | Default | Purpose |
|---|---|---|
| `BEADS_DOLT_PORT` | `3307` | Dolt server port |
| `BEADS_DOLT_PASSWORD` | (empty) | MySQL password for Dolt |
| `BD_ACTOR` | git user.name or $USER | Actor name for audit trail |
| `BD_DEBUG_ROUTING` | (unset) | Set to `1` to debug rig routing |

**Development isolation:** Use port `3308` for dev/test Dolt servers
to avoid touching the production server on `3307`.

## Global Flags

Available on all commands:

| Flag | Description |
|---|---|
| `--json` | Output in JSON format (essential for scripting) |
| `--actor STRING` | Override actor name for audit trail |
| `--db PATH` | Override database path (default: auto-discover `.beads/*.db`) |
| `--dolt-auto-commit on\|off\|batch` | Dolt commit policy |
| `--quiet` / `-q` | Errors only |
| `--verbose` / `-v` | Debug output |
| `--readonly` | Block write operations |
| `--sandbox` | Disable auto-sync |
| `--rig STRING` | Query different rig (e.g., `--rig gastown`, `--rig gt-`) |

## Configuration

```bash
# Show all config
bd config list

# Set a value
bd config set jira.url "https://company.atlassian.net"
bd config set status.custom "awaiting_review,awaiting_testing"
bd config set doctor.suppress.pending-migrations true

# Get a value
bd config get jira.url

# Remove a value
bd config unset jira.url
```

**Common namespaces:** `jira.*`, `linear.*`, `github.*`, `custom.*`,
`status.*`, `doctor.suppress.*`

## Dolt Server Management

Beads uses Dolt (a version-controlled MySQL-compatible database) as its
storage backend.

```bash
# Server lifecycle
bd dolt start
bd dolt stop
bd dolt status

# Version control
bd dolt commit
bd dolt push
bd dolt pull

# Configuration
bd dolt set port 3308
bd dolt set database myproject
bd dolt set host 127.0.0.1
bd dolt set data-dir /path/to/dolt-data

# Diagnostics
bd dolt show
bd dolt test

# Cleanup
bd dolt clean-databases    # Remove stale test databases
bd dolt killall            # Kill orphan Dolt processes
```

**NEVER use `rm -rf` on Dolt data directories.** Use `bd dolt cleanup`
or `bd dolt clean-databases` instead.

## Git Worktrees

```bash
# Create worktree with beads redirect (shares parent .beads/)
bd worktree create feature-auth
bd worktree create bugfix --branch fix-1

# List worktrees
bd worktree list

# Remove worktree (with safety checks)
bd worktree remove feature-auth

# Show current worktree info
bd worktree info
```

Worktrees automatically share the same `.beads/` database via redirect
files, ensuring consistent issue state across all worktrees.

## Initialization and Setup

```bash
# Initialize in current directory
bd init
bd init -p myprefix        # Custom prefix
bd init --stealth          # Personal use, invisible to collaborators

# Set up AI editor integration
bd setup claude            # Claude Code integration
bd setup cursor
bd setup aider

# Health check
bd doctor

# Show AI workflow context
bd prime

# Non-destructive setup (for fresh clones)
bd bootstrap
```

## Sync and Versioning

```bash
# Push/pull Dolt data
bd dolt push
bd dolt pull

# Version history for an issue
bd history bd-42

# Show diff between commits
bd diff bd-42

# Branch management
bd branch list
bd branch create feature-work
```

## Maintenance

```bash
# Database overview and statistics
bd status

# Find stale issues
bd stale

# Count issues matching filters
bd count --type bug --status open

# Compact Dolt history
bd compact

# Garbage collection
bd gc

# Export to JSONL
bd export > issues.jsonl

# Import from JSONL
bd init --from-jsonl

# Check PR readiness
bd preflight
```

## Common Agent Workflows

### Claim and work on an issue

```bash
bd ready                           # Find unblocked work
bd update bd-42 --claim            # Atomically claim
# ... do the work ...
bd close bd-42 --reason "Done"     # Complete
```

### File a discovered issue with context

```bash
bd create "Found bug in auth" \
  -t bug -p 1 \
  -d "Detailed description" \
  --deps "discovered-from:bd-parent"
```

### Multi-rig routing

```bash
# File in a different rig
bd create "Bug in CLI tool" --rig beads
bd create "Gas Town issue" --rig gastown

# Cross-rig show (auto-routes by prefix)
bd show hq-abc       # Routes to HQ rig
bd show gt-7         # Routes to gastown rig
```

### Temporal dependency ordering

**Rule: "X needs Y" not "X comes before Y"**

```bash
# Phase 2 needs Phase 1 to complete first
bd dep add phase2-id phase1-id

# Verify: phase2 should appear in blocked list
bd blocked
```

## JSON Output

Use `--json` for all programmatic access. Key fields in issue JSON:

```json
{
  "id": "bd-42",
  "title": "Fix the bug",
  "status": "open",
  "type": "bug",
  "priority": 1,
  "assignee": "alice",
  "labels": ["security"],
  "description": "...",
  "notes": "...",
  "design": "...",
  "acceptance": "...",
  "created_at": "2025-01-01T00:00:00Z",
  "updated_at": "2025-01-02T00:00:00Z",
  "due_at": null,
  "defer_until": null,
  "parent": null,
  "deps": [{"type": "needs", "id": "bd-10"}]
}
```

## Gotchas

1. **`bd ready` vs `bd list --ready`**: NOT equivalent. `bd ready` uses
   blocker-aware semantics; `bd list --ready` only filters by stored status.

2. **Dependency direction**: `bd dep add A B` means "A needs B" (B must
   complete before A can start). Think requirements, not time order.

3. **`--claim` is atomic**: Fails if already claimed by another agent.
   This is intentional for agent coordination.

4. **No `done`/`complete` status**: Valid statuses are `open`,
   `in_progress`, `blocked`, `deferred`, `hooked`, `closed`.

5. **Dolt port conflicts**: Dev/test MUST use port 3308, never 3307
   (production). Set with `bd dolt set port 3308`.

6. **Auto-routing by prefix**: `bd show be-42` routes to the beads_el
   rig, `bd show gt-7` routes to gastown. Set `BD_DEBUG_ROUTING=1` to
   debug.

7. **`--rig` vs `--prefix`**: `--rig` takes a rig name (e.g., `gastown`);
   `--prefix` takes the issue prefix (e.g., `gt-` or `gt`).
