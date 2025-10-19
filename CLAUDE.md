# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when
working with code in this repository.

## Quick Start: "What's Next?"

This project uses **Beads for issue tracking**. When starting a session or asked "what's next":

```bash
bd ready                                   # Show ready work (no blockers)
bd show beads.el-X                         # View issue details
bd update beads.el-X --status in_progress  # Claim the work
```

If you need more information about how beads works, run "bd quickstart".

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

## Project Overview

beads.el is a Magit-like Emacs interface for the Beads issue tracker,
providing keyboard-driven, transient-based UI for managing issues
without leaving Emacs. The codebase integrates with the `bd` CLI tool
and provides comprehensive Emacs Lisp interfaces for all bd commands.

## Build and Test Commands

Always build, test and lint the project after changing code.

### Build

```bash
eldev -p -dtT compile
```

### Test

```bash
eldev -p -dtT test
```

### Lint

```bash
eldev -p -dtT lint
```

### Development environment

If the "guix" command is available you can get a development
environment with "guix shell -D -f guix.scm" in the project root.

### Development Workflow

When working on an issue:
1. Create a branch for the issue: `git checkout -b beads.el-X-short-description`
2. Update issue status: `bd update beads.el-X --status in_progress`
3. Edit source files in lisp/*.el
4. Run relevant test suite: `make test-<module>`
5. Run full test suite: `make test`
6. Check for byte-compile warnings: `make compile`
7. Commit changes with descriptive message
8. Push branch to GitHub: `git push -u origin beads.el-X-short-description`
9. Verify tests pass on GitHub Actions CI
10. Create pull request or merge to main after CI passes
11. Close issue: `bd close beads.el-X --reason "Completed"`

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
5. Update README.md if user-facing
6. Add keybinding to beads-main.el or relevant mode-map

### Testing Requirements
- All new functions need ERT tests
- Mock bd command output using let-binding over beads--run-command
- Test both success and error paths
- Target >80% code coverage for core modules
- Use descriptive test names: beads-<module>-<function>-<scenario>

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
