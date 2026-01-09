# Project Context

## Purpose

beads.el is a Magit-like Emacs porcelain for the [Beads](https://github.com/steveyegge/beads) issue tracker. It provides a keyboard-driven, transient-based UI for managing issues without leaving Emacs.

## Tech Stack

- **Emacs Lisp** (Emacs 27.1+) - Core language
- **EIEIO** - Object-oriented command system (built-in)
- **transient** (0.10.1+) - Magit-style keyboard-driven menus
- **sesman** (0.3.2+) - Session management for AI agents
- **project.el** - Project discovery and integration (built-in)
- **tabulated-list-mode** - Issue list display (built-in)
- **Beads/bd CLI** - External issue tracker backend
- **Gastown** (optional) - AI agent orchestration

### Build/Test Tooling

- **Eldev** - Elisp project build tool
- **ERT** - Emacs Lisp Regression Testing
- **undercover.el** - Code coverage tracking
- **package-lint** - Linting

## Project Conventions

### Code Style

- **lexical-binding: t** in all files
- **80-column width** for code
- **Naming conventions:**
  - Public functions: `beads-command-name`
  - Internal functions: `beads--internal-function` (double dash)
  - Module-specific state: `beads-module--state-var` (e.g., `beads-create--title`)
- **Autoload** public entry points with `;;;###autoload`
- Use `pcase` for pattern matching (cleaner than cond)
- Prefer `seq-*` functions over `cl-*` for sequence operations
- Comprehensive docstrings for all public functions

### Architecture Patterns

A Magit-like interface to all Beads commands built on EIEIO command classes.

**EIEIO Command System:**
- All bd commands are defined as EIEIO classes, subclasses of `beads-command`
- Class hierarchy mirrors bd command structure:
  - `beads-command` - Base class with global flags (--actor, --db)
  - `beads-command-json` - Commands supporting --json flag (abstract)
  - `beads-command-list`, `beads-command-create`, etc. - Concrete commands
- Each class has slots for applicable flags with metadata (short/long option, description)
- Use `beads-command-execute` method for execution
- Use `beads-command-line` for building command strings

**Transient Menu Pattern:**
- Infixes store arguments in module-specific variables
- Suffixes execute commands using stored arguments
- Validation before execution (`validate-all` pattern)
- Reset commands clear state
- Preview commands show what will be executed

**Buffer Management:**
- List buffers use `tabulated-list-mode` with `default-directory` preserved
- Show buffers are read-only with `special-mode`
- Refresh commands (g key) re-run original bd command
- Buffer naming: `*beads-list*`, `*beads-show*`, etc.

**Caching Strategy:**
- Project root cached per-directory in `beads--project-cache`
- Issue completion cached with 5s TTL in `beads--completion-cache`
- Database path cached per-project (found via .beads discovery)
- Call `beads--invalidate-completion-cache` after mutations

**Context Detection:**
- Check `major-mode` to determine context (beads-list-mode, beads-show-mode)
- Extract issue ID from buffer-name or current line
- Fall back to `completing-read` with annotated issue list

**Process Execution:**
- All commands use `--json` flag for structured output
- Uses `process-file` for Tramp compatibility (remote bd execution)
- Error handling with user-friendly messages via `beads--error`

### Module Structure

```
lisp/
├── beads.el                 # Core: process, JSON, project discovery
├── beads-command.el         # EIEIO command classes
├── beads-types.el           # Type definitions and predicates
├── beads-meta.el            # Metadata and reflection
├── beads-custom.el          # Customization variables
├── beads-error.el           # Error handling
├── beads-completion.el      # Issue completion with caching
├── beads-list.el            # Tabulated list mode for browsing
├── beads-show.el            # Read-only detail view
├── beads-main.el            # Root transient menu
├── beads-create.el          # Create issue transient
├── beads-update.el          # Update issue transient
├── beads-close.el           # Close issue transient
├── beads-dep.el             # Dependency management
├── beads-stats.el           # Statistics display
├── beads-eldoc.el           # Eldoc integration for issue refs
├── beads-sesman.el          # Sesman session management
├── beads-agent*.el          # AI agent integration modules
└── test/                    # ERT tests (one per module)
```

### Testing Strategy

- **Fast integration tests** with mocking when necessary
- All tests modifying files or local state run in temporary beads/git directories
- Each feature tested in non-graphical Emacs running inside tmux
- Target **>80% code coverage** for core modules
- Test naming: `beads-<module>-<function>-<scenario>`
- All tests use mocking to avoid requiring a real bd database

### Quality Gates (Mandatory)

After EVERY code change, run and pass ALL checks:
1. **Tests**: `BD_NO_DAEMON=1 eldev -p -dtT test`
2. **Lint**: `eldev -p -dtT lint`
3. **Compile**: `eldev -p -dtT compile`

A code change is NOT complete until all 3 checks pass.

### Git Workflow

- **Branch naming**: `beads.el-X-short-description` (X = issue number)
- **Issue tracking**: Use bd (beads) for all issues
- **Commit style**: Descriptive messages focusing on "why"
- **CI/CD**: GitHub Actions for test + coverage workflows
- **Coverage**: Codecov integration with 80% target

## Domain Context

**Beads** is a git-backed issue tracker designed for AI-assisted development. Key concepts:

- **Issues**: Tracked items with ID, type, status, priority, title, description
- **Issue types**: bug, feature, task, epic, chore
- **Status**: open, in_progress, blocked, closed
- **Dependencies**: blocks, related, parent-child, discovered-from
- **Ready queue**: Issues with no blocking dependencies
- **bd CLI**: Command-line interface to Beads (`bd list`, `bd create`, etc.)
- **.beads directory**: Project root marker containing SQLite database

## Important Constraints

- **No real bd dependency in tests** - All tests use mocking
- **Tramp compatibility** - Use `process-file` and `start-file-process`, never `call-process`
- **JSON output required** - All bd commands must use `--json` flag
- **Emacs 27.1 minimum** - Required for project.el integration
- **80-column width** - Code and docs should respect this limit
- **No emoji** unless explicitly requested

## External Dependencies

### Runtime (Required)

- **Emacs** 27.1+ (for project.el)
- **transient** 0.10.1+ (built-in Emacs 28+, or from MELPA)
- **sesman** 0.3.2+ (for agent session management)
- **bd executable** in PATH

### Optional

- **markdown-mode** - Better editing in description fields
- **vterm/eat** - Enhanced terminal for interactive commands
- **claude-code-ide.el** - AI agent integration
- **gastown.el** - AI agent orchestration

### Development

- **Eldev** - Build tool
- **package-lint** - Linting
- **undercover.el** - Coverage tracking

## Reference Documentation

Before implementing any bd/beads transient commands, clone and read:
```bash
git clone https://github.com/steveyegge/beads.git beads-reference
```
The `beads-reference/` directory is in `.gitignore`.
