# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when
working with code in this repository.

## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Auto-syncs to JSONL for version control
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**
```bash
bd ready --json
```

**Create new issues:**
```bash
bd create "Issue title" -t bug|feature|task -p 0-4 --json
bd create "Issue title" -p 1 --deps discovered-from:bd-123 --json
```

**Claim and update:**
```bash
bd update bd-42 --status in_progress --json
bd update bd-42 --priority 1 --json
```

**Complete work:**
```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`
6. **Commit together**: Always commit the `.beads/issues.jsonl` file together with the code changes so issue state stays in sync with code state

### Auto-Sync

bd automatically syncs with git:
- Exports to `.beads/issues.jsonl` after changes (5s debounce)
- Imports from JSONL when newer (e.g., after `git pull`)
- No manual export/import needed!

### Managing AI-Generated Planning Documents

AI assistants often create planning and design documents during development:
- PLAN.md, IMPLEMENTATION.md, ARCHITECTURE.md
- DESIGN.md, CODEBASE_SUMMARY.md, INTEGRATION_PLAN.md
- TESTING_GUIDE.md, TECHNICAL_DESIGN.md, and similar files

**Best Practice: Use a dedicated directory for these ephemeral files**

**Recommended approach:**
- Create a `history/` directory in the project root
- Store ALL AI-generated planning/design docs in `history/`
- Keep the repository root clean and focused on permanent project files
- Only access `history/` when explicitly asked to review past planning

**Example .gitignore entry (optional):**
```
### Landing the Plane

**When the user says "let's land the plane"**, follow this clean session-ending protocol:

1. **File beads issues for any remaining work** that needs follow-up
2. **Ensure all quality gates pass** (only if code changes were made) - run tests, linters, builds (file P0 issues if broken)
3. **Update beads issues** - close finished work, update status
4. **Sync the issue tracker carefully** - Work methodically to ensure both local and remote issues merge safely. This may require pulling, handling conflicts (sometimes accepting remote changes and re-importing), syncing the database, and verifying consistency. Be creative and patient - the goal is clean reconciliation where no issues are lost.
5. **Clean up git state** - Clear old stashes and prune dead remote branches:
   ```bash
   git stash clear                    # Remove old stashes
   git remote prune origin            # Clean up deleted remote branches
   ```
6. **Verify clean state** - Ensure all changes are committed and pushed, no untracked files remain
7. **Choose a follow-up issue for next session**
   - Provide a prompt for the user to give to you in the next session
   - Format: "Continue work on bd-X: [issue title]. [Brief context about what's been done and what's next]"

**Example "land the plane" session:**

```bash
# 1. File remaining work
bd create "Add integration tests for sync" -t task -p 2 --json

# 2. Run quality gates (only if code changes were made)
guix shell -D -f guix.scm -- eldev -p -dtT compile
guix shell -D -f guix.scm -- eldev -p -dtT lint
guix shell -D -f guix.scm -- BD_NO_DAEMON=1 eldev -p -dtT test

# 3. Close finished issues
bd close bd-42 bd-43 --reason "Completed" --json

# 4. Sync carefully - example workflow (adapt as needed):
git pull --rebase
# If conflicts in .beads/issues.jsonl, resolve thoughtfully:
#   - git checkout --theirs .beads/issues.jsonl (accept remote)
#   - bd import -i .beads/issues.jsonl (re-import)
#   - Or manual merge, then import
bd sync  # Export/import/verify
git push
# Repeat pull/push if needed until clean

# 5. Verify clean state
git status

# 6. Choose next work
bd ready --json
bd show bd-44 --json
```

**Then provide the user with:**

- Summary of what was completed this session
- What issues were filed for follow-up
- Status of quality gates (all passing / issues filed)
- Recommended prompt for next session

# AI planning documents (ephemeral)
history/
```

**Benefits:**
- ✅ Clean repository root
- ✅ Clear separation between ephemeral and permanent documentation
- ✅ Easy to exclude from version control if desired
- ✅ Preserves planning history for archeological research
- ✅ Reduces noise when browsing the project

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ✅ Store AI planning docs in `history/` directory
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems
- ❌ Do NOT clutter repo root with planning documents

For more details, see README.md and QUICKSTART.md.

## Beads Reference Documentation

**CRITICAL - REQUIRED BEFORE IMPLEMENTING ANY BD COMMANDS**

Before implementing or modifying ANY transient command for bd/beads,
you MUST have a fresh, up-to-date checkout of the upstream Beads
repository:

```bash
# Clone beads reference repository (if not already present)
if [ ! -d "beads-reference" ]; then
  git clone https://github.com/steveyegge/beads.git beads-reference
fi

# ALWAYS pull latest changes before implementing commands
cd beads-reference && git pull && cd ..
```

The `beads-reference/` directory is in `.gitignore` and should NOT be
committed.

### Required Reading Before Implementation

**MANDATORY**: Before designing or implementing any bd command, you
MUST read and understand:

1. **README.md** - Understand command options, flags, and usage patterns
   - Read: `beads-reference/README.md`
   - Understand all command options and their behavior
   - Note command-line flag syntax and output formats

2. **Workflow Documentation** - Understand how beads is actually used
   - Read workflow files in `beads-reference/`
   - Understand the user workflows and common patterns
   - Design Emacs commands that fit naturally into these workflows
   - Make commands feel "Emacs-native" while preserving beads semantics

3. **CLI Documentation** - Check command structure and JSON output
   - Review how bd CLI handles similar operations
   - Understand the relationship between bd commands and JSON output
   - Reference official docs for accurate behavior and edge cases

### Implementation Checklist

When implementing new bd/beads commands:
1. **Ensure beads-reference/ is fresh** (clone or pull latest)
2. **Read README.md** to understand the command you're implementing
3. **Review workflow documentation** to understand usage patterns
4. **Design Emacs-ish interface** that fits the beads workflow
5. Implement the transient menu following patterns in beads-create.el
6. Test with actual bd commands (if available)
7. Run all quality checks (test, lint, compile)

**Never implement bd commands without consulting beads-reference first.**

## Common Commands

```bash
# Finding work
bd ready                    # Ready issues
bd list --status open       # All open issues
bd show beads.el-X          # Issue details with dependencies

## Working on issues
bd update beads.el-X --status in_progress
bd update beads.el-X --notes "Progress update"
bd close beads.el-X --reason "Completed"
```

## Project Overview

beads.el is a Magit-like Emacs interface for the Beads issue tracker,
providing keyboard-driven, transient-based UI for managing issues
without leaving Emacs. The codebase integrates with the `bd` CLI tool
and provides comprehensive Emacs Lisp interfaces for all bd commands.

## Code Quality Requirements

**CRITICAL - MANDATORY FOR ALL CODE CHANGES**

After EVERY code change, you MUST run and pass ALL three checks before
considering the change complete:

1. **Tests** - All tests must pass (921/921)
2. **Lint** - All linters must pass with zero warnings
3. **Byte Compilation** - Code must compile without warnings

**A code change is NOT complete until all three checks pass.**

Do NOT commit code that fails any of these checks. Do NOT skip these
checks. Do NOT defer these checks. Run them immediately after making
any code modification.

If any check fails:
- Fix the issue immediately
- Re-run all three checks
- Only proceed when all checks pass

This is a non-negotiable requirement for code quality.

## Build and Test Commands

**MANDATORY**: Run these commands after EVERY code change.

**IMPORTANT**: All build, test, and lint commands MUST be run within
the proper development environment:

- If the `guix` command is available, wrap ALL commands with:
  `guix shell -D -f guix.scm -- <command>`
- Otherwise, assume eldev and dependencies are already installed

The guix.scm file provides emacs-eldev and all required dependencies.

### Build

```bash
# With guix (preferred):
guix shell -D -f guix.scm -- eldev -p -dtT compile

# Without guix (assumes eldev installed):
eldev -p -dtT compile
```

### Test

Run tests with the environment BD_NO_DAEMON=1 because that is faster.

```bash
# With guix (preferred):
guix shell -D -f guix.scm -- BD_NO_DAEMON=1 eldev -p -dtT test

# Without guix (assumes eldev installed):
BD_NO_DAEMON=1 eldev -p -dtT test
```

### Lint

```bash
# With guix (preferred):
guix shell -D -f guix.scm -- eldev -p -dtT lint

# Without guix (assumes eldev installed):
eldev -p -dtT lint
```

### Checking for Guix

To determine which command format to use, check if guix is available:

```bash
if command -v guix >/dev/null 2>&1; then
  # Use: guix shell -D -f guix.scm -- eldev ...
else
  # Use: eldev ...
fi
```

### Development Workflow

When working on an issue:
1. Create a branch for the issue: `git checkout -b beads.el-X-short-description`
2. Update issue status: `bd update beads.el-X --status in_progress`
3. Edit source files in lisp/*.el
4. **MANDATORY: Verify code quality** (run after EVERY code change):
   a. Run tests: `guix shell -D -f guix.scm -- eldev -p -dtT test`
   b. Run linter: `guix shell -D -f guix.scm -- eldev -p -dtT lint`
   c. Run compiler: `guix shell -D -f guix.scm -- eldev -p -dtT compile`
   d. **ALL THREE MUST PASS** - Fix any failures immediately
5. Repeat steps 3-4 until feature is complete and all checks pass
6. Commit changes with descriptive message (only after all checks pass)
7. Push branch to GitHub: `git push -u origin beads.el-X-short-description`
8. Verify tests pass on GitHub Actions CI
9. Create pull request or merge to main after CI passes
10. Close issue: `bd close beads.el-X --reason "Completed"`

**CRITICAL**: Never commit code that fails tests, lint, or compilation.
The workflow at step 4 is mandatory and non-negotiable for every code
modification.

Branch naming convention: `beads.el-X-short-description` where X is the
issue number and short-description briefly describes the work (e.g.,
`beads.el-27-implement-label-commands`).

All tests use mocking and do NOT require a real bd database or .beads
directory.

## Code Architecture

### Module Structure

The codebase is organized into focused modules in lisp/:

**Core Infrastructure (beads.el)**
- Process execution (sync/async) with `beads--run-command`
- JSON parsing with `beads--parse-issue` and `beads--parse-issues`
- Project.el integration with .beads directory discovery
- Completion system with caching for issue IDs
- Global configuration (beads-executable, beads-actor, etc.)
- All bd commands use --json flag for structured output

**UI Modules**
- beads-list.el: Tabulated-list-mode for browsing issues (list,
  ready, blocked)
- beads-show.el: Read-only detail view with markdown-like rendering
  and clickable bd-N references
- beads-main.el: Root transient menu that ties everything together

**Transient Command Modules**
- beads-create.el: Create new issues with validation
- beads-update.el: Update existing issues (context-aware)
- beads-close.el: Close issues with required reason field
- beads-misc.el: Stats, dependencies, import/export commands
- beads-graph.el: Dependency graph visualization

**Test Structure**
- test/beads-*-test.el: One test file per module
- Tests use ERT (Emacs Lisp Regression Testing)
- Extensive mocking to avoid requiring bd CLI

### Key Design Patterns

**Process Execution**
- `beads--run-command`: Synchronous execution, returns parsed JSON
- `beads--run-command-async`: Async with callback, for long operations
- All commands automatically add --json, --db, --actor flags
- Uses process-file for Tramp compatibility (remote bd execution)
- Error handling with user-friendly messages via beads--error

**Caching Strategy**
- Project root cached per-directory in beads--project-cache
- Issue completion cached with 5s TTL in beads--completion-cache
- Database path cached per-project (found via .beads discovery)
- Call beads--invalidate-completion-cache after mutations

**Context Detection**
- Check major-mode to determine context (beads-list-mode,
  beads-show-mode)
- Extract issue ID from buffer-name or current line
- Fall back to completing-read with annotated issue list

**Transient Menus**
- Infixes store arguments in module-specific variables
  (e.g., beads-create--title)
- Suffixes execute commands using stored arguments
- Validation before execution (validate-all pattern)
- Reset commands clear state
- Preview commands show what will be executed

**Buffer Management**
- List buffers preserve project context via default-directory
- Show buffers are read-only with special-mode
- Refresh commands (g key) re-run original bd command
- All buffers follow Emacs naming conventions (*beads-list*, etc.)

## Development Guidelines

### Naming Conventions
- Public functions: `beads-command-name`
- Internal functions: `beads--internal-function` (double dash)
- Module-specific state: `beads-module--state-var` (e.g.,
  beads-create--title)
- Autoload public entry points with ;;;###autoload

### Code Style
- Use lexical-binding: t in all files
- 80-column width (per user's CLAUDE.md preference)
- Comprehensive docstrings for all public functions
- Use pcase for pattern matching (cleaner than cond)
- Prefer seq-* functions over cl-* for sequence operations

### Adding New Commands

When adding a new bd command:
1. Add command execution to appropriate module or create new one
2. Add transient menu if interactive (see beads-create.el as template)
3. Add autoload to beads.el if it's a public entry point
4. Add comprehensive tests (>80% coverage target)
5. **Run all checks (test, lint, compile) - ALL MUST PASS**
6. Update README.md if user-facing
7. Add keybinding to beads-main.el or relevant mode-map

**Remember**: After implementing, you MUST run and pass all three
checks (test, lint, compile) before the work is considered complete.

### Testing Requirements

**MANDATORY**: Run full test suite after EVERY code change.

- All new functions need ERT tests
- Mock bd command output using let-binding over beads--run-command
- Test both success and error paths
- Target >80% code coverage for core modules
- Use descriptive test names: beads-<module>-<function>-<scenario>
- **Tests must pass (921/921) before committing any code**
- Run: `guix shell -D -f guix.scm -- eldev -p -dtT test`

### Transient Menu Patterns

Follow this pattern for new transient menus (based on
beads-create.el):

```elisp
;; State variables (reset after execution)
(defvar beads-cmd--param nil)

;; Reset function
(defun beads-cmd--reset-state () ...)

;; Validation functions (return error string or nil)
(defun beads-cmd--validate-param () ...)
(defun beads-cmd--validate-all () ...)

;; Build command arguments
(defun beads-cmd--build-command-args () ...)

;; Infix commands (set state variables)
(transient-define-infix beads-cmd--infix-param () ...)

;; Suffix commands (execute, preview, reset)
(transient-define-suffix beads-cmd--execute () ...)
(transient-define-suffix beads-cmd--preview () :transient t ...)
(transient-define-suffix beads-cmd--reset () :transient t ...)

;; Main menu
(transient-define-prefix beads-cmd () ...)
```

### Common Gotchas

- **JSON parsing**: bd --json returns array for list commands, object
  for single items. Use beads--parse-issue vs beads--parse-issues.
- **Issue IDs**: Always validate format (e.g., bd-123, worker-1).
  Use beads--issue-completion-table for user input.
- **Project context**: Preserve default-directory when switching
  buffers. List/show buffers should maintain project root for bd
  commands.
- **Tramp compatibility**: Always use process-file and
  start-file-process, never call-process. Strip Tramp prefix with
  file-local-name for --db paths.
- **Completion cache**: Remember to invalidate after create/update/
  close operations.

## External Dependencies

Required at runtime:
- Emacs 27.1+ (for project.el)
- transient library (built-in Emacs 28+, or from MELPA)
- bd executable in PATH

Optional:
- markdown-mode (for better editing experience in description fields)

## Related Documentation

- README.md: User-facing documentation, installation, usage guide
- lisp/beads-pkg.el: Package metadata for MELPA
- Each module has comprehensive Commentary section

## Beads CLI Integration

The bd CLI is the source of truth. All operations go through:
1. Build command with beads--build-command (adds --json, --db,
   --actor)
2. Execute with beads--run-command or beads--run-command-async
3. Parse JSON response with beads--parse-issue(s)
4. Display in appropriate UI (list, show, or message)

The .beads directory is auto-discovered via locate-dominating-file,
starting from project root or current directory. Database path
extracted from .beads/*.db files.
