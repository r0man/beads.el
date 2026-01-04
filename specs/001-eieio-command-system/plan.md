# Implementation Plan: EIEIO Command System for Beads.el

**Branch**: `001-eieio-command-system` | **Date**: 2026-01-04 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-eieio-command-system/spec.md`

## Summary

Extend beads.el's existing EIEIO command infrastructure to cover ALL bd CLI commands with auto-generated transient menus, providing a complete Magit-like interface. The existing `beads-command`, `beads-meta`, and `beads-command-doctor` patterns serve as the foundation.

## Technical Context

**Language/Version**: Emacs Lisp (Emacs 27.1+)
**Primary Dependencies**: transient, EIEIO (built-in), vterm/eat/term (optional)
**Storage**: N/A (uses bd CLI for persistence)
**Testing**: ERT with mocking, integration tests using temporary beads repos
**Target Platform**: GNU Emacs 27.1+, GUI and terminal modes
**Project Type**: Single Emacs package
**Performance Goals**: Transient menus appear instantly (<100ms), command execution latency dominated by bd CLI
**Constraints**: Must work in non-graphical Emacs (terminal/tmux), no external dependencies beyond transient
**Scale/Scope**: ~50 bd commands to implement, organized into ~15 elisp files

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

The constitution template is not populated with project-specific principles. Proceeding with general best practices:

- [x] **Test-First**: Integration tests required for each command module
- [x] **Simplicity**: Build on existing patterns (beads-command, beads-meta)
- [x] **Consistency**: Follow established naming conventions (beads-command-*, beads-*)
- [x] **Documentation**: Org-based manual with synchronized transient previews

## Project Structure

### Documentation (this feature)

```text
specs/001-eieio-command-system/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (API contracts for generic methods)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
lisp/
├── beads.el                    # Core infrastructure (existing)
├── beads-command.el            # Base EIEIO classes (existing, extend)
├── beads-meta.el               # Slot metadata infrastructure (existing)
├── beads-main.el               # Main transient menu (existing, extend)
├── beads-option.el             # Global options (existing)
│
├── # Existing command modules (to extend/verify)
├── beads-create.el             # bd create (existing)
├── beads-update.el             # bd update (existing)
├── beads-list.el               # bd list (existing)
├── beads-show.el               # bd show (existing)
├── beads-close.el              # bd close (existing)
├── beads-delete.el             # bd delete (existing)
├── beads-doctor.el             # bd doctor (existing - reference impl)
├── beads-command-doctor.el     # Doctor class (existing - reference impl)
│
├── # New command modules (one file per top-level command group)
├── beads-command-daemon.el     # daemon, daemon-list, daemon-start, daemon-stop, daemon-status
├── beads-daemon.el             # Transients for daemon commands
├── beads-command-sync.el       # sync command class
├── beads-command-config.el     # config command class
├── beads-command-init.el       # init command class (existing, verify)
├── beads-command-export.el     # export command class
├── beads-command-import.el     # import command class
├── beads-command-graph.el      # graph command class
├── beads-command-dep.el        # dep command class
├── beads-command-label.el      # label command class
├── beads-command-epic.el       # epic, epic-* command classes
├── beads-command-admin.el      # admin command classes
├── beads-command-hooks.el      # hooks command classes
├── beads-command-search.el     # search command class
├── beads-command-gate.el       # gate command class
├── beads-command-ready.el      # ready command class
├── beads-command-blocked.el    # blocked command class
├── beads-command-stale.el      # stale command class
├── beads-command-count.el      # count command class
├── beads-command-lint.el       # lint command class
├── beads-command-status.el     # status command class
├── beads-command-info.el       # info command class
├── beads-command-version.el    # version command class
├── beads-command-activity.el   # activity command class
├── beads-command-comments.el   # comments command class
├── beads-command-edit.el       # edit command class
├── beads-command-move.el       # move, refile command classes
├── beads-command-reopen.el     # reopen command class (existing, verify)
├── beads-command-duplicate.el  # duplicate, duplicates command classes
├── beads-command-supersede.el  # supersede command class
├── beads-command-swarm.el      # swarm command classes
├── beads-command-mol.el        # mol command classes
├── beads-command-formula.el    # formula command classes
├── beads-command-agent.el      # agent command classes
├── beads-command-slot.el       # slot command classes
├── beads-command-audit.el      # audit command class
├── beads-command-ship.el       # ship command class
├── beads-command-worktree.el   # worktree command classes
├── beads-command-jira.el       # jira command classes
├── beads-command-linear.el     # linear command classes
├── beads-command-repo.el       # repo command classes
├── beads-command-migrate.el    # migrate command classes
├── beads-command-repair.el     # repair, rename-prefix command classes
│
├── test/
│   ├── beads-*-test.el         # Existing unit tests
│   └── beads-integration-test.el  # New integration test infrastructure
│
└── doc/
    └── beads.org               # Org-based manual with transient previews
```

**Structure Decision**: Extends existing single-package structure in `lisp/`. One elisp file per top-level command group (e.g., `beads-command-daemon.el` contains classes for `daemon`, `daemon-list`, `daemon-start`, etc.). Transient UI files follow pattern `beads-<command>.el` (e.g., `beads-daemon.el`).

## Complexity Tracking

No constitution violations detected. The design builds on existing proven patterns:
- `beads-command-doctor.el` + `beads-doctor.el` serve as reference implementation
- `beads-meta-define-transient` macro handles transient generation
- Existing terminal backend fallback already implemented
