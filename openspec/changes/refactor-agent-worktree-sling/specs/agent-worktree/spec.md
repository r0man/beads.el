# Agent Worktree Capability

## ADDED Requirements

### Requirement: Worktree Command Classes

The system SHALL provide EIEIO command classes for bd worktree operations,
following the established `beads-command` pattern.

#### Scenario: Create worktree via command class

- **WHEN** user creates a `beads-command-worktree-create` instance with name "feature-x"
- **AND** executes the command
- **THEN** the system calls `bd worktree create feature-x --json`
- **AND** parses the JSON response
- **AND** returns worktree info including path and branch

#### Scenario: List worktrees via command class

- **WHEN** user executes `beads-command-worktree-list!`
- **THEN** the system calls `bd worktree list --json`
- **AND** returns a list of worktree objects with name, path, branch, and beads-state

#### Scenario: Create worktree with custom branch

- **WHEN** user creates a `beads-command-worktree-create` with name "wt-1" and branch "feature/auth"
- **AND** executes the command
- **THEN** the system calls `bd worktree create wt-1 --branch feature/auth --json`
- **AND** the worktree is created at ./wt-1 with branch feature/auth

### Requirement: Worktree Transient Menu

The system SHALL provide a transient menu for worktree management accessible
from the main beads menu.

#### Scenario: Access worktree menu

- **WHEN** user invokes `beads-worktree` (or presses `w` from main menu)
- **THEN** a transient menu appears with options:
  - `c` Create worktree
  - `l` List worktrees
  - `r` Remove worktree
  - `i` Current worktree info

#### Scenario: Create worktree from menu

- **WHEN** user presses `c` in worktree menu
- **THEN** system prompts for worktree name with completion
- **AND** offers issue IDs and branch names as suggestions
- **AND** optionally prompts for branch name (default: same as worktree name)
- **AND** creates worktree via `bd worktree create`

#### Scenario: List worktrees from menu

- **WHEN** user presses `l` in worktree menu
- **THEN** a `beads-worktree-list-mode` buffer appears
- **AND** shows tabulated list with columns: Name, Path, Branch, Beads State
- **AND** supports navigation (n/p), removal (d), and refresh (g)

### Requirement: Worktree Completion

The system SHALL provide rich completion for worktree selection with
annotations and grouping.

#### Scenario: Complete worktree name when creating

- **WHEN** user is prompted for worktree name
- **THEN** completion candidates include:
  - Open issue IDs (grouped as "Issues")
  - Existing branch names (grouped as "Branches")
- **AND** free-form input is allowed

#### Scenario: Complete existing worktree

- **WHEN** user is prompted to select existing worktree
- **THEN** completion shows worktrees from `bd worktree list`
- **AND** annotations show branch and beads state
- **AND** candidates are grouped by beads state (redirect/shared/none)

### Requirement: Agent Sling Workflow

The system SHALL provide a "sling" command for assigning work to agents
in worktrees, supporting both new and existing worktrees.

#### Scenario: Sling work to new worktree

- **WHEN** user invokes `beads-agent-sling`
- **AND** selects an issue
- **AND** chooses "Create new worktree"
- **THEN** system creates worktree via `bd worktree create <issue-id>`
- **AND** starts agent in the new worktree
- **AND** sets agent focus to selected issue

#### Scenario: Sling work to existing worktree

- **WHEN** user invokes `beads-agent-sling`
- **AND** selects an issue
- **AND** chooses existing worktree from completion
- **THEN** system starts agent in selected worktree
- **AND** sets agent focus to selected issue

#### Scenario: Sling from issue list

- **WHEN** user is in `beads-list-mode` on an issue
- **AND** invokes `beads-agent-sling`
- **THEN** issue is pre-selected from context
- **AND** user only needs to choose worktree and backend

### Requirement: Gastown Backend Integration

The system SHALL optionally support Gastown as an agent backend when
gastown.el is available.

#### Scenario: Gastown backend availability

- **WHEN** gastown.el is installed and loaded
- **THEN** "gastown" appears in backend completion
- **AND** has appropriate priority (configurable)

#### Scenario: Gastown backend unavailable

- **WHEN** gastown.el is not installed
- **THEN** "gastown" does not appear in available backends
- **AND** other backends work normally

#### Scenario: Start agent via Gastown

- **WHEN** user selects "gastown" backend
- **AND** slings work to a worktree
- **THEN** system delegates to Gastown's agent spawning
- **AND** uses Gastown's hook mechanism for persistence
- **AND** session is tracked by both beads.el and Gastown

## MODIFIED Requirements

### Requirement: Agent Worktree Creation

The agent system SHALL use `bd worktree create` instead of direct git
commands for worktree creation when agents use worktrees.

#### Scenario: Agent auto-creates worktree

- **WHEN** `beads-agent-use-worktrees` is `t`
- **AND** user starts agent on an issue
- **AND** no worktree exists for that issue
- **THEN** system creates worktree via `bd worktree create <issue-id>`
- **AND** worktree has proper `.beads/redirect` to main database
- **AND** worktree path is added to `.gitignore`

#### Scenario: Agent finds existing worktree

- **WHEN** user starts agent on an issue
- **AND** worktree already exists (per `bd worktree list`)
- **THEN** system uses existing worktree
- **AND** does not create duplicate

#### Scenario: Fallback for older bd versions

- **WHEN** bd CLI lacks worktree subcommand (older version)
- **AND** user attempts worktree operation
- **THEN** system logs deprecation warning
- **AND** falls back to direct git worktree commands
- **AND** operation completes successfully

### Requirement: Worktree Detection

The agent system SHALL use `bd worktree list` for worktree detection
instead of parsing git porcelain output.

#### Scenario: Detect existing worktree for issue

- **WHEN** system checks for existing worktree
- **THEN** it calls `bd worktree list --json`
- **AND** matches by worktree name or branch against issue ID
- **AND** returns worktree path if found, nil otherwise
