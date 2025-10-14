# beads.el - Magit-like Interface for Beads Issue Tracker

A comprehensive Emacs interface for [Beads](https://github.com/your-org/beads),
providing a keyboard-driven, transient-based UI for managing issues without
leaving your editor.

## Features

- 📋 **Tabulated List Mode**: Browse issues with sortable columns (ID, status,
  priority, type, title)
- 🔍 **Issue Detail View**: Rich formatting with markdown-like rendering and
  clickable bd-N references
- ⌨️ **Transient Menus**: Magit-style keyboard-driven interface for all bd
  commands
- 🚀 **Context-Aware**: Automatically detects issue IDs from current buffer
- 🎯 **Complete Coverage**: All bd CLI commands available through transients
- 🔗 **Project Integration**: Seamless integration with Emacs project.el

## Requirements

- **Emacs**: 27.1 or newer
- **Dependencies**:
  - `transient` (included in Emacs 28+, or install from MELPA)
  - `project` (built-in)
- **External**: `bd` executable in PATH

## Installation

### MELPA (Recommended)

```elisp
(use-package beads
  :ensure t
  :bind ("C-c b" . beads-main))
```

### Manual Installation

1. Clone or download this repository
2. Add to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/beads/lisp")
(require 'beads)
```

3. Optionally bind to a key:

```elisp
(global-set-key (kbd "C-c b") 'beads-main)
```

## Quick Start

1. **Open Beads menu**: `M-x beads-main` (or your custom keybinding)
2. **List issues**: Press `l` (list all) or `r` (ready issues)
3. **Navigate**: Use arrow keys or `n`/`p` to move between issues
4. **View issue**: Press `RET` or `s` on an issue
5. **Create issue**: Press `c` from main menu
6. **Update issue**: Press `u` from list or show buffer
7. **Close issue**: Press `x` from list or show buffer

## Usage Guide

### Main Menu (`M-x beads-main`)

The root transient menu provides access to all beads.el commands:

**View Issues:**
- `l` - List all issues (`beads-list`)
- `r` - Show ready issues (`beads-ready`)
- `b` - Show blocked issues (`beads-blocked`)
- `s` - Show specific issue (`beads-show`)

**Create/Edit:**
- `c` - Create new issue (`beads-create`)
- `u` - Update issue (`beads-update`)
- `x` - Close issue (`beads-close`)

**Dependencies:**
- `d` - Manage dependencies (`beads-dep`)
  - `a` - Add dependency
  - `r` - Remove dependency
  - `t` - Show dependency tree
  - `l` - List dependencies

**Admin:**
- `S` - Show statistics (`beads-stats`)
- `e` - Export to JSONL (`beads-export`)
- `i` - Import from JSONL (`beads-import`)
- `I` - Initialize beads project (`beads-init`)

### Issue List Mode

When viewing issue lists (via `beads-list`, `beads-ready`, `beads-blocked`):

**Navigation:**
- `n` / `p` - Next/previous issue
- `RET` / `s` - Show issue details
- `g` - Refresh list
- `q` - Quit buffer

**Sorting:**
- Click column headers or use `S` to sort by different columns

**Marking:**
- `m` - Mark issue
- `u` - Unmark issue
- `M` - Mark all
- `U` - Unmark all

### Issue Show Mode

When viewing issue details (via `beads-show` or pressing `RET` in list):

**Navigation:**
- `n` / `p` - Next/previous section
- `TAB` - Jump to issue reference at point (bd-N links)
- `g` - Refresh issue
- `q` - Quit buffer

**Actions:**
- Issue references (bd-N) are clickable - press `RET` or click to jump

### Creating Issues (`beads-create`)

Transient menu for creating new issues:

**Basic:**
- `-t` - Issue type (bug, feature, task, epic, chore)
- `-p` - Priority (0=critical, 1=high, 2=medium, 3=low, 4=backlog)

**Details:**
- `-d` - Description (supports multiline in minibuffer)
- `-i` - Custom ID (e.g., worker1-100 for parallel agents)
- `-D` - Dependencies (format: type:id,type:id)

**Actions:**
- `RET` - Create issue
- `P` - Preview command
- `r` - Reset all fields

**Example workflow:**
1. `M-x beads-main` then `c`
2. Enter title: "Fix authentication bug"
3. Press `-t` then select `bug`
4. Press `-p` then select `1` (high priority)
5. Press `RET` to create

### Updating Issues (`beads-update`)

Context-aware transient menu for updating existing issues:

**Context Detection:**
- From list mode: Uses issue on current line
- From show mode: Uses displayed issue
- Otherwise: Prompts with completion

**Fields:**
- `-t` - Change title
- `-T` - Change type
- `-s` - Change status (open, in_progress, blocked, closed)
- `-p` - Change priority
- `-d` - Update description
- `-r` - Set reason (for status changes)

**Actions:**
- `RET` - Apply updates
- `P` - Preview command
- `r` - Reset all fields

**Smart Updates:**
Only modified fields are sent to bd CLI (efficient!)

### Closing Issues (`beads-close`)

Quick transient for closing issues:

**Required:**
- `-r` - Reason for closing (required field)

**Actions:**
- `RET` - Close issue (with validation)
- `r` - Reset

### Dependency Management (`beads-dep`)

Submenu for managing issue dependencies:

**Dependency Types:**
- `blocks` - Hard dependency (affects ready queue)
- `related` - Soft relationship
- `parent-child` - Epic/subtask hierarchy
- `discovered-from` - Track discovery lineage

**Actions:**
- `a` - Add dependency (specify from, to, type)
- `r` - Remove dependency
- `t` - Show dependency tree (visual display)
- `l` - List dependencies for an issue

### Statistics (`beads-stats`)

Display project statistics in special buffer:

- Issue counts by status
- Issue counts by type
- Priority breakdown
- Other metrics

Press `g` to refresh, `q` to quit.

### Import/Export

**Export (`beads-export`):**
- `-o` - Output path (default: .beads/issues.jsonl)
- `--no-auto-flush` - Disable automatic flushing

**Import (`beads-import`):**
- `-i` - Input path (required)
- `--dry-run` - Preview changes without applying
- `--resolve-collisions` - Auto-resolve ID conflicts

Useful for syncing issues across branches or sharing with team.

## Configuration

### Customization Variables

```elisp
;; Path to bd executable (default: "bd")
(setq beads-executable "/path/to/bd")

;; Database path (default: auto-discover from .beads directory)
(setq beads-db-path "/custom/path/to/beads.db")

;; Actor name for operations (default: nil, uses bd default)
(setq beads-actor "your-name")

;; Enable debug logging (default: nil)
(setq beads-enable-debug t)
```

### Use-package Configuration

```elisp
(use-package beads
  :ensure t
  :custom
  (beads-executable "bd")
  (beads-db-path nil)  ; auto-discover
  (beads-actor nil)     ; use bd default
  (beads-enable-debug nil)
  :bind
  (("C-c b" . beads-main)
   ("C-c b l" . beads-list)
   ("C-c b r" . beads-ready)
   ("C-c b c" . beads-create)
   ("C-c b s" . beads-show)))
```

### Integration with Projectile

```elisp
;; Open beads for current project
(defun my/beads-for-project ()
  "Open beads main menu for current project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (beads-main)))

(define-key projectile-command-map "b" 'my/beads-for-project)
```

## Keybinding Reference

| Command        | Default Key | Context    | Description           |
|----------------|-------------|------------|-----------------------|
| `beads-main`   | -           | Global     | Open main menu        |
| `beads-list`   | `l`         | Main menu  | List all issues       |
| `beads-ready`  | `r`         | Main menu  | Show ready issues     |
| `beads-blocked`| `b`         | Main menu  | Show blocked issues   |
| `beads-show`   | `s` or `RET`| List mode  | Show issue details    |
| `beads-create` | `c`         | Main menu  | Create new issue      |
| `beads-update` | `u`         | Main menu  | Update issue          |
| `beads-close`  | `x`         | Main menu  | Close issue           |
| `beads-dep`    | `d`         | Main menu  | Dependency submenu    |
| `beads-stats`  | `S`         | Main menu  | Show statistics       |
| `beads-export` | `e`         | Main menu  | Export to JSONL       |
| `beads-import` | `i`         | Main menu  | Import from JSONL     |
| `beads-init`   | `I`         | Main menu  | Initialize project    |

Within list/show buffers:
- `n` / `p` - Navigate
- `g` - Refresh
- `q` - Quit
- `RET` - Show/follow

## Comparison with CLI Workflow

| Task                    | CLI Command                | beads.el Workflow       |
|-------------------------|----------------------------|-------------------------|
| List ready issues       | `bd ready`                 | `M-x beads-ready`       |
| Create issue            | `bd create "Title" -t bug` | `M-x beads-create`      |
| Update status           | `bd update bd-42 -s ...`   | `M-x beads-update`      |
| Show issue              | `bd show bd-42`            | `M-x beads-show`        |
| Add dependency          | `bd dep add ...`           | `M-x beads-dep` then `a`|
| Show dependency tree    | `bd dep tree bd-1`         | `M-x beads-dep` then `t`|
| Close issue             | `bd close bd-42 -r ...`    | `M-x beads-close`       |
| View statistics         | `bd stats`                 | `M-x beads-stats`       |

**Advantages of beads.el:**
- No need to remember CLI syntax
- Context-aware (auto-detects issue IDs)
- Visual feedback with colors and formatting
- Keyboard-driven (no mouse required)
- Transient menus show all available options
- Clickable references between issues
- Integrated with Emacs workflow

## Troubleshooting

### bd executable not found

```
Error: Beads: bd executable not found
```

**Solution:** Ensure `bd` is in your PATH, or set `beads-executable`:

```elisp
(setq beads-executable "/full/path/to/bd")
```

### No .beads directory found

```
Error: Could not find .beads directory
```

**Solution:** Run `M-x beads-init` to initialize beads in your project, or
navigate to a directory containing a `.beads` folder.

### JSON parsing errors

```
Error: Failed to parse JSON output
```

**Solution:** Ensure your bd CLI is up-to-date. All beads.el commands use
`--json` flag which requires bd with JSON support.

**Debug:** Enable debug logging to see raw bd output:

```elisp
(setq beads-enable-debug t)
;; Check *beads-debug* buffer for details
```

### Transient not found

```
Error: Cannot find transient library
```

**Solution:** Install transient from MELPA:

```elisp
(use-package transient :ensure t)
```

Or upgrade to Emacs 28+ where transient is built-in.

### Slow performance

**Symptoms:** Commands take several seconds to complete

**Solutions:**
- Check bd CLI performance directly: `time bd list --json`
- Ensure database is not corrupted: `bd export -o backup.jsonl`
- Reduce issue count if database is very large (>10k issues)
- Check disk I/O if database is on slow filesystem

### Unicode/special characters not displaying

**Solution:** Ensure your Emacs uses UTF-8 encoding:

```elisp
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
```

## Architecture

### File Structure

```
lisp/
├── beads.el                      # Core: process, JSON, project
├── beads-list.el                 # Tabulated list mode
├── beads-show.el                 # Issue detail view
├── beads-create.el               # Create transient
├── beads-update.el               # Update transient
├── beads-misc.el                 # Misc transients (close/dep/stats/etc)
├── beads-main.el                 # Root transient menu
├── Makefile                      # Test runner
└── test/
    ├── beads-process-test.el     # Process execution tests
    ├── beads-project-test.el     # Project discovery tests
    ├── beads-utils-test.el       # Utility function tests
    ├── beads-list-test.el        # List mode tests
    ├── beads-show-test.el        # Show mode tests
    ├── beads-transient-create-test.el
    ├── beads-transient-update-test.el
    ├── beads-main-test.el
    └── beads-misc-test.el
```

### Key Design Patterns

**Process Execution:**
- Synchronous: `beads--run-command` for quick operations
- All commands use `--json` flag for structured output
- Error handling with user-friendly messages

**Caching:**
- Project root cached per directory
- Version info cached globally
- Database path cached per project

**Context Detection:**
- Check `major-mode` to determine context
- Extract issue ID from buffer name or current line
- Fall back to `completing-read` when needed

**Transient Menus:**
- Infixes for arguments (stored in global state)
- Suffixes for actions (execute commands)
- Validation before execution
- Reset commands to clear state

## Testing

### Running Tests

```bash
# Run all tests
make test

# Run specific test suite
make test-process
make test-list
make test-create
# ... etc

# Show test statistics
make test-stats

# Interactive test mode (for debugging)
make test-interactive
```

### Test Coverage

- **Total tests:** 448
- **Overall coverage:** >75%
- **Core modules:** >80%
- **UI modules:** >70%

All tests use mocking to avoid requiring a real bd database.

## Contributing

Contributions are welcome! Please:

1. **Add tests** for new features
2. **Update README** if adding user-facing functionality
3. **Follow conventions**:
   - Use `beads--` prefix for internal functions
   - Use `beads-` prefix for public functions
   - Add docstrings to all public functions
   - Use 80-column width for code
4. **Run tests** before submitting: `make test`

## Known Limitations

- **No real-time updates:** Must manually refresh (`g`) to see changes
- **No multi-selection:** Marking issues is limited to visual feedback
- **No inline editing:** Must use transient menus to modify issues
- **No undo:** Operations execute immediately (use bd CLI to revert if needed)

Future improvements tracked in project issues.

## License

Same license as the Beads project.

## Related Projects

- **Beads** - The CLI issue tracker: https://github.com/your-org/beads
- **Magit** - Git porcelain that inspired this interface
- **Org-mode** - Alternative approach to issue tracking in Emacs

## Credits

Built with ❤️ by the Beads team and AI-assisted coding.

Uses the excellent `transient` library by Jonas Bernoulli.
