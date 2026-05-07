# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

beads.el is an Emacs package providing a Magit-like transient-based UI for the [Beads](https://github.com/steveyegge/beads) issue tracker (`bd` CLI). It uses EIEIO classes, transient menus, and tabulated-list-mode to expose all `bd` commands from within Emacs.

## Build & Test Commands

```bash
# Run all tests
eldev test

# Run a single test file
eldev test test/beads-list-test.el

# Run tests matching a pattern
eldev test -p "beads-command-close"

# Verbose test output
eldev -p -dtT test

# Tests with coverage (uses undercover.el + codecov)
eldev -s test --coverage

# Byte-compile
eldev compile

# Lint
eldev lint
```

Tests live in `lisp/test/` and are named `<module>-test.el`. Tests use a suite-level isolated Dolt server on a random port (via `beads-integration-test.el`) so they never hit production. `bd` v0.58.0+ requires Dolt -- there is no JSONL-only fallback.

## Architecture

### EIEIO Command System (core pattern)

The central abstraction is `beads-defcommand` (in `beads-command.el`), a macro that generates from a single class definition:
1. An EIEIO class with typed slots
2. CLI argument serialization (`beads-command-line`)
3. A transient menu with infixes auto-derived from slot metadata
4. Result type declarations for auto-parsing

Class hierarchy: `beads-command` (abstract base) -> `beads-command-global-options` (adds `--actor`, `--db`, `--json` flags) -> concrete commands (`beads-command-list`, `beads-command-create`, `beads-command-close`, etc.).

Each `beads-command-<foo>.el` file in `lisp/` defines one bd subcommand as an EIEIO class.

### Slot Metadata (`beads-meta.el`)

Custom EIEIO slot properties drive code generation. A single slot definition carries CLI properties (`:long-option`, `:short-option`, `:option-type`, `:positional`), transient properties (`:transient`, `:class`, `:reader`, `:choices`, `:group`, `:level`), and validation (`:required`, `:validator`). This avoids duplication between CLI serialization and UI.

### Data Types (`beads-types.el`)

EIEIO classes mirroring the Go structs from `beads/internal/types`: `beads-issue`, `beads-dependency`, `beads-label`, `beads-comment`, `beads-event`, `beads-statistics`, etc. All JSON parsing from `bd --json` output goes through these types.

### UI Layers

- **`beads-command-list.el`** / **`beads-spec.el`**: Tabulated list mode with `beads-issue-spec` filter objects (status/type/priority/sort/limit) that convert to CLI args.
- **`beads-command-show.el`** / **`beads-section.el`**: Issue detail view using magit-section-style rendering.
- **`beads-eldoc.el`**: Hover-to-preview issue references anywhere, with caching.
- **`beads-agent.el`** + backends: AI agent integration with sesman session management and git worktree isolation.

### Key Files

| File | Role |
|------|------|
| `beads.el` | Entry point, main transient menu, core utilities |
| `beads-command.el` | `beads-defcommand` macro, base classes, execution |
| `beads-meta.el` | EIEIO slot property infrastructure (preserves custom props) |
| `beads-types.el` | Data model classes (issue, dependency, etc.) |
| `beads-option.el` | Global option variables and transient groups |
| `beads-spec.el` | Filter spec objects for list views |
| `beads-completion.el` | Completion tables (issue IDs, statuses, etc.) |
| `beads-reader.el` | Reader functions for transient infixes |

## Conventions

- Public API: `beads-` prefix. Internal: `beads--` prefix.
- Each `bd` subcommand gets its own `beads-command-<name>.el` file.
- All bd commands use `--json` for structured output; UI never parses human-readable text.
- Transient menus are auto-generated from slot metadata where possible; use `:transient :manual` only when custom layout is needed.
- Dependencies: Emacs 29.1+, transient 0.10.1+, sesman 0.3.2+, vui 1.0.0+ (from MELPA).

### Top-level group commands (`bd <group> <subcommand>`)

`bd` exposes 24 top-level *group* commands (`admin`, `ado`, `audit`,
`config`, `dep`, `dolt`, `epic`, `federation`, `formula`, `gate`,
`github`, `gitlab`, `hooks`, `jira`, `label`, `linear`, `merge-slot`,
`mol`, `notion`, `repo`, `rules`, `swarm`, `vc`, `worktree`). These
exist purely as routers for subcommands; running `bd <group>` with no
subcommand prints help. They take no positional arguments and no
non-global flags.

**Policy:** every top-level group gets a parent `transient-define-prefix`
menu, but **never** a `beads-defcommand` EIEIO class.

- The transient is named `beads-<group>` (or `beads-<group>-menu` when
  the unsuffixed name is reserved for a leaf — see
  `beads-command-label.el` and `beads-command-worktree.el`).
- Leaf subcommands are EIEIO classes via `beads-defcommand`; the parent
  transient simply binds keys to those leaves' transient suffixes.
- A `beads-defcommand` class for a router would serialize to e.g. `bd
  config` with no args, which prints help and exits — not a useful
  Emacs command. The transient menu *is* the parent UX.

CLI-coverage audits flag these groups under `coverage.md` >
`Missing Classes` > `Top-level group commands (N) — IN POLICY, no
action` (mirrored in `REPORT.md` > `Missing Classes` >
`Top-level group commands without a class`). That bucketing is
expected and in-policy; the audit script filters them out of the
actionable list. Mid-level groups (e.g. `dolt.remote`, `backup`
parents that route to subcommands) follow the same rule: parent
transient yes, EIEIO class no.

## Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd prime` for full workflow context.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work atomically
bd close <id>         # Complete work
bd dolt push          # Push beads data to remote
```

## Non-Interactive Shell Commands

**ALWAYS use non-interactive flags** with file operations to avoid hanging on confirmation prompts.

```bash
cp -f source dest           # NOT: cp source dest
mv -f source dest           # NOT: mv source dest
rm -f file                  # NOT: rm file
rm -rf directory            # NOT: rm -r directory
```

Other commands that may prompt: `scp` (`-o BatchMode=yes`), `ssh` (`-o BatchMode=yes`), `apt-get` (`-y`).

## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

## Session Completion

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd dolt push
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
