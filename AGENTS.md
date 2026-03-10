# Agent Instructions

This file provides guidance to Claude Code (claude.ai/code) when
working with code in this repository.

## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Dolt-powered version control with native sync
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**

```bash
bd ready --json
```

**Create new issues:**

```bash
bd create "Issue title" --description="Detailed context" -t bug|feature|task -p 0-4 --json
bd create "Issue title" --description="What this issue is about" -p 1 --deps discovered-from:bd-123 --json
```

**Claim and update:**

```bash
bd update <id> --claim --json
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
2. **Claim your task atomically**: `bd update <id> --claim`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" --description="Details about what was found" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`

### Auto-Sync

bd automatically syncs via Dolt:

- Each write auto-commits to Dolt history
- Use `bd dolt push`/`bd dolt pull` for remote sync
- No manual export/import needed!

### Important Rules

- Use bd for ALL task tracking
- Always use `--json` flag for programmatic use
- Link discovered work with `discovered-from` dependencies
- Check `bd ready` before asking "what should I work on?"
- Do NOT create markdown TODO lists
- Do NOT use external issue trackers
- Do NOT duplicate tracking systems

For more details, see README.md and docs/QUICKSTART.md.

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed):
   - Run byte compilation: `guix shell -D -f guix.scm -- eldev -p -dtT compile`
     - **MUST produce zero compile warnings**
   - Run linter: `guix shell -D -f guix.scm -- eldev -p -dtT lint`
     - **MUST produce zero lint warnings and zero lint errors**
   - Run tests: `guix shell -D -f guix.scm -- eldev -p -dtT test`
     - **MUST show 0 unexpected results** (all tests pass, no failures, no errors)
   - **ALL THREE CHECKS MUST PASS before pushing — no exceptions**
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
- **Any compile warning = STOP, fix it, recompile before proceeding**
- **Any lint warning or error = STOP, fix it, re-lint before proceeding**
- **Any test failure or unexpected result = STOP, fix it, re-test before proceeding**

## Code Quality Requirements

**CRITICAL - MANDATORY FOR ALL CODE CHANGES**

During development, you MUST:

1. **Test in live Emacs first** - Load changes and verify interactively
2. **Run unit tests** - All tests must pass
3. **Do a code review** - Review changes and address critical issues

Before committing (Landing the Plane), also run:
- Byte compilation (must compile without warnings)
- Linter (must pass with zero warnings)

If any check fails:
- Fix the issue immediately
- Re-run the failing check
- Only proceed when all checks pass

This is a non-negotiable requirement for code quality.

## Development Dolt Server

beads.el development and testing uses a **dedicated Dolt server on port 3308**,
completely isolated from the Gas Town production server (port 3307).

### Start the dev Dolt server

Before running integration tests or bd commands in dev, start the dedicated server:

```bash
# Start Dolt on port 3308 (beads.el dev port)
dolt sql-server --port 3308 --data-dir ~/.dolt-beads-dev &
DOLT_PID=$!

# Or use bd dolt start after configuring port in your test repo:
# bd dolt set port 3308
```

Configure your test/dev beads repos to use port 3308:

```bash
cd /path/to/your/test-repo
bd dolt set port 3308
```

### NEVER touch port 3307

Port 3307 is the live Gas Town production Dolt server. Tests and development
work MUST NEVER connect to it. Always verify you are on port 3308 before
running bd commands that touch Dolt.

### Stop the dev server

```bash
kill $DOLT_PID
# Or: bd dolt stop  (in a repo configured for port 3308)
```

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

Run tests (the Eldev file starts an isolated Dolt server automatically).

```bash
# With guix (preferred):
guix shell -D -f guix.scm -- eldev -p -dtT test

# Without guix (assumes eldev installed):
eldev -p -dtT test
```

## Interactive Development Environment

**MANDATORY FOR ALL AGENT WORK** — polecats, crew, any agent
touching the codebase.

All development, feature work, bug fixing, and UX testing on
beads.el MUST happen interactively in a live non-graphical Emacs
server controlled by agents via emacsclient.

### Starting the Emacs Dev Server

Start a dedicated non-graphical Emacs server for development:

```bash
emacs --daemon=beads-dev
```

The server loads beads.el from the working tree. Adapt the load-path
to point at the current worktree's `lisp/` directory (the
`share/emacs.d/init.el` uses `~/workspace/beads.el/lisp` — adjust
for your actual worktree location):

```bash
emacsclient -s beads-dev -e '(add-to-list '\''load-path "'"$(pwd)"'/lisp")'
emacsclient -s beads-dev -e '(require '\''beads)'
```

### Interactive Testing via emacsclient

Use emacsclient against the dev server for all interactive testing:

```bash
# Evaluate elisp expressions
emacsclient -s beads-dev -e '(elisp-expression)'

# Open a terminal UI (in tmux)
emacsclient -s beads-dev -nw
```

### Reloading After Code Changes

After modifying source files, reload them into the running server:

```bash
emacsclient -s beads-dev -e '(load-file "lisp/beads-foo.el")'
```

Agents MUST try the features they develop interactively — invoke
transient menus, trigger commands, verify buffer output, check
error handling.

### Killing the Server

Kill the dev server at session end:

```bash
emacsclient -s beads-dev -e '(kill-emacs)'
```

## Acceptance Testing in tmux

**MANDATORY** — every feature and bug fix MUST have acceptance
testing done in a tmux session. Use the tmux skill to control
Emacs non-graphically.

### Workflow

1. **Create a tmux session:**
   ```bash
   tmux new-session -d -s beads-test
   ```

2. **Start emacsclient in it:**
   Send keys to the pane:
   ```bash
   tmux send-keys -t beads-test 'emacsclient -s beads-dev -nw' Enter
   ```

3. **Drive Emacs via tmux send-keys:**
   Type commands, invoke keybindings:
   ```bash
   tmux send-keys -t beads-test 'M-x beads' Enter
   ```

4. **Capture pane output to verify results:**
   ```bash
   tmux capture-pane -t beads-test -p
   ```

5. **Test actual user-facing behavior:**
   Verify that menus appear, buffers show correct data, errors
   display properly.

This catches UX issues that unit tests cannot: rendering,
keybindings, interactive flows.

**Do NOT skip this step** — a feature is not complete until it has
been acceptance-tested in tmux.

### Using the tmux skill

In Claude Code, use the tmux skill to drive the Emacs session
programmatically:

```
/tmux
```

This allows you to send keystrokes and capture pane output. Drive
the full user workflow: open menus, navigate transients, verify
output — all without a graphical display.

### Full acceptance test example

```bash
# 1. Create tmux session
tmux new-session -d -s beads-accept

# 2. Start non-graphical Emacs with beads loaded
tmux send-keys -t beads-accept \
  "emacs -nw -Q --eval '(progn (add-to-list (quote load-path) \"$(pwd)/lisp\") (require (quote beads)))'" \
  Enter

# 3. Wait for Emacs to start, then invoke the main menu
sleep 2
tmux send-keys -t beads-accept "M-x beads" Enter

# 4. Verify the transient menu appeared
tmux capture-pane -t beads-accept -p | grep -q "Working With Issues"

# 5. Navigate — press l for list
tmux send-keys -t beads-accept "l" ""

# 6. Capture and inspect the result
tmux capture-pane -t beads-accept -p
```

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

### Code Coverage

The project uses undercover.el for code coverage tracking via the Eldev
plugin. The `-U` (or `--undercover-report`) flag enables coverage and
generates a report.

**Running tests with coverage:**

```bash
# With guix (recommended):
CI=true guix shell -D -f guix.scm -- eldev -s -dtT test -U coverage/codecov.json

# Without guix (assumes eldev installed):
CI=true eldev -s -dtT test -U coverage/codecov.json

# The CI=true environment variable is required to enable coverage generation
# The -U flag specifies the report output path
# The -s flag enables source loading mode (required for coverage)
```

**Report formats:**
The report format is configured in the Eldev file via `eldev-undercover-config`:
- `codecov` - Codecov-compatible JSON format (configured for CI)
- `coveralls` - Coveralls format
- `simplecov` - SimpleCov JSON format (not compatible with Codecov's parser)
- `text` - Human-readable text format

**Important notes:**
- `CI=true` environment variable is required (undercover only generates reports in CI mode)
- You MUST use `-s` (source loading mode), NOT `-p` (packaged mode) for coverage
- Undercover cannot instrument byte-compiled files (packaged mode byte-compiles)
- Coverage is configured to track all `lisp/*.el` files except `lisp/test/*`

**Configuration:**
- Coverage fileset: Defined in Eldev via `eldev-undercover-fileset`
- Report format: Defined in Eldev via `eldev-undercover-config` (set to `codecov`)
- Currently tracks: `lisp/*.el` excluding `lisp/test/*.el`
- Plugin enabled via: `(eldev-use-plugin 'undercover)` in Eldev file

### GitHub Actions Workflows

The project uses two GitHub Actions workflows:

**Test Workflow** (`.github/workflows/test.yml`):
- Runs tests across multiple Emacs versions (28.2, 29.4, 30.2, snapshot)
- Triggered on push/PR to main branch
- Uses matrix strategy for parallel testing

**Coverage Workflow** (`.github/workflows/coverage.yml`):
- Runs tests with coverage instrumentation (single Emacs version)
- Uploads coverage reports to Codecov
- Triggered on push/PR to main branch
- Uses `fail_ci_if_error: false` to avoid blocking PRs on Codecov outages

**Codecov Configuration** (`codecov.yml`):
- Target coverage threshold: 80%
- Threshold tolerance: 2%
- Comments on PRs with coverage diff

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

### TDD Loop (MANDATORY)

For every code change, follow this cycle strictly:
1. **Write the test first** — add ERT test to the relevant test file
2. **Run tests — they should FAIL** (red):
   `eldev -p -dtT test`
3. **Implement the minimum code** to make the test pass
4. **Run tests — they should PASS** (green):
   `eldev -p -dtT test`
5. **Refactor** if needed, keeping tests green
6. **Repeat** for the next change

Never write implementation before writing the test. If a test is hard to
write, that is a signal the design needs simplification.

4. **MANDATORY: Interactive testing in live Emacs** (catches issues
   early):
   a. Start the Emacs dev server if not running:
      `emacs --daemon=beads-dev`
   b. Load modified code into server via emacsclient:
      `emacsclient -s beads-dev -e '(load-file "lisp/beads-foo.el")'`
   c. Try the feature interactively — invoke commands, check buffers:
      `emacsclient -s beads-dev -e '(beads)'`
   d. Run acceptance test in tmux — open emacsclient in tmux, drive
      full workflow (see "Acceptance Testing in tmux" section above)
   e. Run unit tests:
      `guix shell -D -f guix.scm -- eldev -p -dtT test`
   f. Run linter and byte compiler:
      `guix shell -D -f guix.scm -- eldev -p -dtT lint`
      `guix shell -D -f guix.scm -- eldev -p -dtT compile`
5. **ALL TESTS MUST PASS** - Fix any failures immediately
6. Repeat steps 3-5 until feature is complete
7. Commit changes with descriptive message (only after tests pass)
8. Push branch to GitHub: `git push -u origin beads.el-X-short-description`
9. Verify tests pass on GitHub Actions CI
10. Create pull request or merge to main after CI passes
11. Close issue: `bd close beads.el-X --reason "Completed"`

**CRITICAL**: Never commit code that fails tests. The live testing
followed by unit/integration tests catches issues early, before they
become harder to debug.

Branch naming convention: `beads.el-X-short-description` where X is the
issue number and short-description briefly describes the work (e.g.,
`beads.el-27-implement-label-commands`).

All tests use mocking and do NOT require a real bd database or .beads
directory.

## Code Architecture

### Module Structure

The codebase is organized into focused modules in lisp/:

**Core Infrastructure (beads.el)**
- Process execution via EIEIO command classes
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
- beads.el: Core module AND root transient menu (main transient merged here)

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
- EIEIO command classes (`beads-command`, `beads-command-list`, etc.)
- All commands use `beads-command-execute` method
- Supports both synchronous and asynchronous execution
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
7. Add keybinding to beads.el (main menu) or relevant mode-map

**Remember**: After implementing, you MUST run and pass all three
checks (test, lint, compile) before the work is considered complete.

### Testing Requirements

**MANDATORY**: Run full test suite after EVERY code change.

- All new functions need ERT tests
- Mock bd command output using let-binding over beads-command-execute
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
2. Execute using EIEIO command classes via beads-command-execute
3. Parse JSON response with beads--parse-issue(s)
4. Display in appropriate UI (list, show, or message)

The .beads directory is auto-discovered via locate-dominating-file,
starting from project root or current directory. Database path
extracted from .beads/*.db files.
