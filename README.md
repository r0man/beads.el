# beads.el - Magit-like Interface for Beads Issue Tracker

[![Test](https://github.com/r0man/beads.el/actions/workflows/test.yml/badge.svg)](https://github.com/r0man/beads.el/actions/workflows/test.yml)
[![Coverage](https://codecov.io/gh/r0man/beads.el/branch/main/graph/badge.svg)](https://codecov.io/gh/r0man/beads.el)

⚠️ Status: Experimental

A comprehensive Emacs interface for [Beads](https://github.com/steveyegge/beads),
providing a keyboard-driven, transient-based UI for managing issues without
leaving your editor.

## Features

- 📋 **Tabulated List Mode**: Browse issues with sortable columns (ID, status,
  priority, type, title)
- 🔍 **Issue Detail View**: Rich formatting with markdown-like rendering and
  clickable issue references
- ⌨️ **Transient Menus**: Magit-style keyboard-driven interface for all bd
  commands
- 🚀 **Context-Aware**: Automatically detects issue IDs from current buffer
- 💡 **Eldoc Integration**: Hover over issue references anywhere to see details
- 🎯 **Complete Coverage**: All bd CLI commands available through transients
- 🔗 **Project Integration**: Seamless integration with Emacs project.el

## Requirements

- **Emacs**: 27.1 or newer
- **Dependencies**:
  - `transient` (included in Emacs 28+, or install from MELPA)
  - `project` (built-in)
- **External**: `bd` executable in PATH

## Installation

**Note:** beads.el is not yet available on MELPA. Until then, install from
source using one of the methods below.

### Option 1: use-package with :load-path (Recommended)

1. Clone the repository:

```bash
git clone https://github.com/yourusername/beads.el.git ~/path/to/beads.el
```

2. Configure with use-package:

```elisp
(use-package beads
  :load-path "~/path/to/beads.el/lisp"
  :commands (beads beads-list beads-ready beads-show beads-create)
  :bind ("C-c b" . beads)
  :hook (after-init . beads-eldoc-mode))  ; Enable eldoc support
```

### Option 2: use-package with :vc (Emacs 29+)

For Emacs 29 or newer, you can use the built-in package-vc feature:

```elisp
(use-package beads
  :vc (:fetcher github :repo "yourusername/beads.el")
  :commands (beads beads-list beads-ready beads-show beads-create)
  :bind ("C-c b" . beads)
  :hook (after-init . beads-eldoc-mode))  ; Enable eldoc support
```

This will automatically clone and install the package.

### Option 3: Manual Installation

1. Clone the repository:

```bash
git clone https://github.com/yourusername/beads.el.git ~/path/to/beads.el
```

2. Add to your `load-path` and require:

```elisp
(add-to-list 'load-path "~/path/to/beads.el/lisp")
(require 'beads)
```

3. Optionally bind to a key:

```elisp
(global-set-key (kbd "C-c b") 'beads)
```

### Byte-Compilation (Optional, for better performance)

After cloning, byte-compile for faster loading:

```bash
cd ~/path/to/beads.el
eldev compile
```

Or from within Emacs:

```elisp
(byte-recompile-directory "~/path/to/beads.el/lisp" 0)
```

**Note:** If eldev is not available, the Emacs method will still work for
basic compilation.

## Quick Start

### First-Time Setup

1. **Install bd CLI**: Follow the installation instructions at
   [Beads repository](https://github.com/steveyegge/beads)

2. **Initialize Beads in your project** (if not already done):
   ```bash
   cd /path/to/your/project
   bd init
   ```

   Or from within Emacs:
   ```
   M-x beads-init
   ```

3. **Verify bd is accessible**: Check that `bd` is in your PATH:
   ```bash
   which bd
   bd version
   ```

### Basic Workflow

1. **Open Beads menu**: `M-x beads` (or your custom keybinding)
2. **List issues**: Press `l` (list all) or `r` (ready issues)
3. **Navigate**: Use arrow keys or `n`/`p` to move between issues
4. **View issue**: Press `RET` or `s` on an issue
5. **Create issue**: Press `c` from main menu
6. **Update issue**: Press `u` from list or show buffer
7. **Close issue**: Press `x` from list or show buffer

**Tip:** Run `M-x beads-quickstart` to see an interactive tutorial within
Emacs, or run `bd quickstart` from the command line

## Usage Guide

### Main Menu (`M-x beads`)

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
- `W` - Worktree management (`beads-worktree-menu`)

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

### Eldoc Support

beads.el provides eldoc integration that displays issue information when your
cursor hovers over issue references in any buffer.

**Supported Formats:**
- `beads.el-22` - Project-specific references
- `bd-123` - Standard Beads references
- `worker-1`, `api-42` - Any `project-N` format

**Enable Eldoc Support:**

```elisp
;; Enable globally
(beads-eldoc-mode 1)
```

**What You Get:**

When you position your cursor on an issue reference like `beads.el-22`:
- **Echo area**: Brief info (ID, status, title)
- **Eldoc buffer**: Full issue metadata (description, notes, dates, etc.)

**How It Works:**

1. **In beads buffers** (list/show): Uses text properties for instant lookup
2. **In other buffers** (code, markdown, org): Pattern matching with caching
3. **Performance**: Issues are cached for 5 minutes (configurable)

**Configuration:**

```elisp
;; Customize cache TTL (default: 300 seconds)
(setq beads-eldoc-cache-ttl 600)  ; 10 minutes

;; Customize issue pattern (default supports all project-N formats)
(setq beads-eldoc-issue-pattern "\\b\\([a-zA-Z][a-zA-Z0-9._-]*-[0-9]+\\)\\b")
```

**Example Use Cases:**

- **Code comments**: Hover over `;; Fix beads.el-22` to see issue details
- **Commit messages**: See issue info while writing git commits
- **Org files**: Get context on issues in your planning documents
- **Markdown**: Preview issue details in README or documentation

**Tips:**

- Cache automatically invalidates when you modify issues
- Works with any eldoc-compatible mode
- Combines with eldoc-box for richer display

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
1. `M-x beads` then `c`
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

Display project statistics in an **interactive** buffer:

- Total issues
- Issue counts by status (Open, In Progress, Blocked, Closed)
- Ready to work issues
- Average lead time

**Interactive Features:**

All statistic numbers are **clickable**! Click on any number to instantly view those issues in a filtered list:

- Click **Total Issues** → View all issues
- Click **Open** → View open issues only
- Click **In Progress** → View in-progress issues only
- Click **Blocked** → View blocked issues
- Click **Closed** → View closed issues
- Click **Ready** → View ready-to-work issues

**Keyboard Shortcuts:**
- `g` - Refresh statistics
- `q` - Quit statistics buffer

**Example Workflow:**
1. Run `M-x beads-stats` (or `S` from beads-main menu)
2. See at-a-glance view of project health
3. Click on "Open: 30" to see all open issues
4. Work on issues, then return to stats with `M-x beads-stats`
5. Click on "Ready: 15" to find next issue to work on

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

;; Default limit for list operations (default: 0, meaning no limit)
;; Set to a positive integer to limit the number of issues returned
(setq beads-list-default-limit 50)

;; Enable debug logging (default: nil)
(setq beads-enable-debug t)
```

### Use-package Configuration

Full example with customization and keybindings:

```elisp
(use-package beads
  :load-path "~/path/to/beads.el/lisp"  ; or use :vc for Emacs 29+
  :commands (beads beads-list beads-ready beads-show beads-create)
  :custom
  (beads-executable "bd")               ; path to bd executable
  (beads-db-path nil)                   ; nil = auto-discover
  (beads-actor nil)                     ; nil = use bd default
  (beads-list-default-limit 0)          ; 0 = no limit
  (beads-enable-debug nil)              ; enable for troubleshooting
  :bind
  (("C-c b" . beads)
   ("C-c b l" . beads-list)
   ("C-c b r" . beads-ready)
   ("C-c b c" . beads-create)
   ("C-c b s" . beads-show))
  :hook (after-init . beads-eldoc-mode)) ; Enable eldoc support
```

### Integration with Projectile

```elisp
;; Open beads for current project
(defun my/beads-for-project ()
  "Open beads main menu for current project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (beads)))

(define-key projectile-command-map "b" 'my/beads-for-project)
```

## Keybinding Reference

| Command        | Default Key | Context    | Description           |
|----------------|-------------|------------|-----------------------|
| `beads`        | -           | Global     | Open main menu        |
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
| `beads-worktree-menu` | `W`  | Main menu  | Worktree management   |
| `beads-agent-sling` | `w`    | Agent menu | Sling to worktree     |

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

## AI Agent Integration

beads.el includes integration with AI coding agents, allowing you to start
agents that work on issues directly from Emacs. The agent system uses
[sesman](https://github.com/vspinu/sesman) for session management, providing
context-aware session selection.

### Quick Start

1. **Start an agent** on an issue from `beads-list` or `beads-show`: press `A`
2. **Jump to agent buffer**: `M-x beads-agent-jump`
3. **Stop an agent**: `M-x beads-agent-stop`
4. **Browse sessions**: `M-x beads-sesman-browser`

### Session Concepts

Each agent session is associated with:
- **Issue ID**: The beads issue being worked on
- **Backend**: The AI agent implementation (e.g., `claude-code-ide`)
- **Working Directory**: Either the main project or a git worktree

**Session Naming**: Sessions are named `<issue-id>@<directory>`, e.g.,
`beads.el-42@~/projects/beads.el/`.

### Context-Aware Sessions

Sessions are linked to three context types:
1. **Agent buffer**: The terminal buffer where the AI agent runs
2. **Worktree directory** (primary): When using git worktrees, the session is
   linked to the worktree directory
3. **Main project** (fallback): Also linked to the main repository for context
   from anywhere in the project

This triple linking means sesman automatically selects the right session whether
you're in the agent buffer, editing files in the worktree, or working anywhere
in the project.

### Git Worktree Support

When `beads-agent-use-worktrees` is enabled (default: t), each agent session
gets its own git worktree:

- Worktrees are created as siblings to the main repo
- Named after the issue ID (e.g., `~/projects/beads.el-42/`)
- Branch is created with the same name as the issue ID
- Issues are automatically imported from the main repo's JSONL

This provides isolation between concurrent agent sessions.

### Sesman Keybindings

The `beads-sesman-map` provides these commands (bind to a prefix like `C-c C-s`):

| Key | Command                | Description              |
|-----|------------------------|--------------------------|
| `s` | `beads-sesman-start`   | Start new session        |
| `q` | `beads-sesman-quit`    | Quit current session     |
| `r` | `beads-sesman-restart` | Restart current session  |
| `b` | `beads-sesman-browser` | Open session browser     |
| `l` | `beads-sesman-link`    | Link session to context  |

**Example configuration:**

```elisp
(use-package beads
  :load-path "~/path/to/beads.el/lisp"
  :bind-keymap ("C-c C-s" . beads-sesman-map))
;; Sesman integration is automatically enabled when beads-sesman is loaded
```

### Agent Menu

The agent transient menu (`M-x beads-agent`) provides:

**Agent Actions:**
- `s` - Start agent on issue
- `w` - Sling to worktree (flexible agent assignment)
- `S` - Stop agent session
- `j` - Jump to agent buffer
- `p` - Send prompt to agent

**Session Management:**
- `l` - List active sessions
- `c` - Cleanup stale sessions

### Using the Session Browser

`M-x beads-sesman-browser` opens an interactive buffer showing all sessions:

```
beads.el-42@~/projects/beads.el-42
  Issue: beads.el-42
  Backend: claude-code-ide
  Started: 2025-12-14T10:30:00+0100
  Worktree: ~/projects/beads.el-42

beads.el-43@~/projects/beads.el-43
  Issue: beads.el-43
  Backend: claude-code-ide
  Started: 2025-12-14T11:00:00+0100
  Worktree: ~/projects/beads.el-43
```

From the browser, you can:
- Select a session to make it current
- Quit sessions
- Link sessions to buffers/directories

### Configuration

```elisp
;; Use default backend without prompting
(setq beads-agent-default-backend "claude-code-ide")

;; Disable automatic status update to in_progress
(setq beads-agent-auto-set-in-progress nil)

;; Disable worktree creation (work in main repo)
(setq beads-agent-use-worktrees nil)

;; Custom worktree parent directory
(setq beads-agent-worktree-parent "~/worktrees/")

;; Configure restart delay (seconds)
(setq beads-sesman-restart-delay 0.5)
```

### Supported Backends

Currently implemented:
- **claude-code-ide**: Integration with Claude Code via claude-code-ide.el

Placeholder backends (not yet implemented):
- efrit
- claudemacs
- claude-code.el

To use AI agents, you need:
1. Install claude-code-ide.el package
2. Install Claude Code CLI: `npm install -g @anthropic-ai/claude-code`
3. Ensure `claude` command is in your PATH

### Sling Workflow

The **sling** workflow (`M-x beads-agent-sling` or `w` from agent menu) provides
flexible agent assignment to worktrees. This enables parallel development with
multiple agents working in isolated worktrees.

**Workflow Options:**
- **New Worktree**: Create a new worktree and start an agent there
- **Existing Worktree**: Start an agent in a pre-existing worktree

**Menu Options:**
- `N` - Select "New worktree" mode
- `E` - Select "Existing worktree" mode
- `i` - Set issue (optional - agent can work without specific issue)
- `n` - Set worktree name (defaults to issue ID if set)
- `e` - Select existing worktree from list
- `b` - Select backend
- `x` - Execute sling (start agent)
- `v` - Preview configuration
- `R` - Reset all fields

**Example: Sling issue to new worktree:**
1. `M-x beads-agent-sling` (or `w` from agent menu)
2. Press `i` and select an issue (e.g., `beads.el-42`)
3. Press `N` to select "New worktree" mode (name auto-defaults to issue ID)
4. Press `x` to create worktree and start agent

**Example: Sling to existing worktree:**
1. `M-x beads-agent-sling`
2. Press `E` to select "Existing worktree" mode
3. Press `e` and select a worktree from the completion list
4. Optionally press `i` to associate with an issue
5. Press `x` to start agent in the selected worktree

## Worktree Management

beads.el provides a transient menu for managing git worktrees with beads
integration. Worktrees allow multiple working directories sharing the same git
repository, enabling parallel development.

### Worktree Menu (`M-x beads-worktree-menu`)

Access via `M-x beads-worktree-menu` or `W` from the main beads menu.

**Create Worktree:**
- `-n` - Worktree name (completion suggests issue IDs)
- `-b` - Branch name (optional, defaults to worktree name)

**Remove Worktree:**
- `-t` - Target worktree to remove
- `-f` - Force removal (skip safety checks)

**Actions:**
- `c` - Create worktree
- `l` - List all worktrees
- `r` - Remove worktree
- `i` - Info about current worktree

### Worktree List Mode

The `l` command opens a tabulated list showing all worktrees:

| Column | Description |
|--------|-------------|
| Name | Worktree name |
| Branch | Git branch |
| State | Beads state (shared/redirect/none) |
| Main | Whether this is the main repository |
| Path | Filesystem path |

**Keybindings in list:**
- `RET` - Show worktree info
- `d` - Remove worktree at point
- `g` - Refresh list
- `c` - Create new worktree
- `q` - Quit

### Beads State

When creating worktrees via `bd worktree create`, beads automatically sets up
a redirect file so all worktrees share the same `.beads` database:

- **shared**: Main repository with the actual `.beads` directory
- **redirect**: Worktree with `.beads/redirect` pointing to main repo
- **none**: Directory without beads integration

This ensures consistent issue state across all worktrees.

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
├── beads-eldoc.el                # Eldoc integration for issue refs
├── beads-create.el               # Create transient
├── beads-update.el               # Update transient
├── beads-misc.el                 # Misc transients (close/dep/stats/etc)
├── beads-main.el                 # Root transient menu
├── beads-agent.el                # AI agent integration
├── beads-worktree.el             # Worktree management transient
├── beads-command-worktree.el     # EIEIO classes for bd worktree
├── beads-completion.el           # Completion tables and helpers
├── beads-reader.el               # Reader functions for transient infixes
├── Makefile                      # Test runner
└── test/
    ├── beads-test.el             # Core functionality tests
    ├── beads-list-test.el        # List mode tests
    ├── beads-show-test.el        # Show mode tests
    ├── beads-eldoc-test.el       # Eldoc integration tests
    ├── beads-create-test.el      # Create transient tests
    ├── beads-update-test.el      # Update transient tests
    ├── beads-close-test.el       # Close transient tests
    ├── beads-main-test.el        # Main menu tests
    ├── beads-misc-test.el        # Misc commands tests
    ├── beads-agent-test.el       # Agent integration tests
    └── beads-worktree-test.el    # Worktree menu tests
```

### Key Design Patterns

**Process Execution:**
- EIEIO command classes: `beads-command`, `beads-command-list`, etc.
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
eldev test

# Run tests with verbose output
eldev -p -dtT test

# Run specific test file
eldev test test/beads-list-test.el

# Run tests with coverage
eldev test --coverage
```

### Test Coverage

- **Total tests:** 2889
- **Overall coverage:** >75%
- **Core modules:** >80%
- **UI modules:** >70%
- **Agent modules:** >80%

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
4. **Run tests** before submitting: `eldev test`

## Known Limitations

- **No real-time updates:** Must manually refresh (`g`) to see changes
- **No multi-selection:** Marking issues is limited to visual feedback
- **No inline editing:** Must use transient menus to modify issues
- **No undo:** Operations execute immediately (use bd CLI to revert if needed)

Future improvements tracked in project issues.

## License

Same license as the Beads project.

## Related Projects

- **Beads** - The CLI issue tracker: https://github.com/steveyegge/beads
- **Magit** - Git porcelain that inspired this interface
- **Org-mode** - Alternative approach to issue tracking in Emacs

## Credits

Built with ❤️ by the Beads team and AI-assisted coding.

Uses the excellent `transient` library by Jonas Bernoulli.
