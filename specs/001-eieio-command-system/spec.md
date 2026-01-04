# Feature Specification: EIEIO Command System for Beads.el

**Feature Branch**: `001-eieio-command-system`
**Created**: 2026-01-04
**Status**: Draft
**Input**: User description: "Design Beads.el with EIEIO-based command system providing transient menus, auto-generated interactive commands, and customizable rendering/execution modes"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Access Main Command Menu (Priority: P1)

A user wants to quickly access all Beads commands from Emacs without remembering individual command names. They invoke `M-x beads` and see a transient menu mirroring `bd --help` structure, allowing navigation to any command or command group.

**Why this priority**: This is the primary entry point for the entire Beads.el experience. Without this, users cannot discover or access any functionality.

**Independent Test**: Can be fully tested by invoking `M-x beads` and verifying the transient menu appears with correct command groups and subcommands listed.

**Acceptance Scenarios**:

1. **Given** Emacs is running with beads.el loaded, **When** user invokes `M-x beads`, **Then** a transient menu appears showing all top-level commands and command groups matching `bd --help` output
2. **Given** the main transient menu is displayed, **When** user selects a command group (e.g., "daemon"), **Then** a sub-menu appears with that group's commands
3. **Given** the main transient menu is displayed, **When** user selects a direct command (e.g., "create"), **Then** that command's transient menu opens

---

### User Story 2 - Execute a Command via Transient (Priority: P1)

A user wants to create a new issue using `bd create`. They invoke the command, fill in positional arguments and options via the transient interface, preview the command, and execute it.

**Why this priority**: Command execution is the core functionality. Users must be able to run commands with full control over arguments and options.

**Independent Test**: Can be fully tested by invoking `M-x beads-create`, filling in required fields, previewing, and executing the command against a test beads repository.

**Acceptance Scenarios**:

1. **Given** user invokes `M-x beads-create`, **When** the transient menu appears, **Then** it shows fields for all positional arguments and command-specific options
2. **Given** the create transient is open with fields filled, **When** user selects "Preview", **Then** the exact `bd create` command that would be executed is displayed
3. **Given** the create transient is open with valid inputs, **When** user selects "Execute", **Then** the command runs and output is displayed appropriately
4. **Given** the create transient is open, **When** user selects "Reset", **Then** all fields return to default/empty state

---

### User Story 3 - Auto-Generated Interactive Commands (Priority: P1)

A developer defining a new EIEIO command class wants an interactive Emacs command auto-generated. They define `beads-command-daemon-list` class with metadata slots, and `M-x beads-daemon-list` becomes available automatically.

**Why this priority**: Auto-generation eliminates boilerplate and ensures consistency. Every command class automatically gets a usable interface.

**Independent Test**: Can be tested by defining a minimal command class and verifying the corresponding `M-x` command exists and opens a transient.

**Acceptance Scenarios**:

1. **Given** a class `beads-command-foo` is defined with required metadata, **When** beads.el loads, **Then** `M-x beads-foo` command is available
2. **Given** `M-x beads-daemon-list` is invoked, **When** the command requires positional arguments, **Then** user is prompted for them before transient opens
3. **Given** the transient opens for a command, **When** positional arguments were provided, **Then** they appear in the transient and can be modified

---

### User Story 4 - Customize Command Rendering (Priority: P2)

A developer wants `beads-command-list` to render output in a tabulated-list buffer instead of terminal output. They override the rendering method for this class, and list commands display results in a structured buffer.

**Why this priority**: Different commands have different optimal output formats. Customization enables rich Emacs-native experiences.

**Independent Test**: Can be tested by running `beads-list` and verifying output appears in a tabulated-list buffer with expected columns and data.

**Acceptance Scenarios**:

1. **Given** `beads-command-list` has custom rendering defined, **When** user executes the command, **Then** results appear in a tabulated-list buffer
2. **Given** default rendering is used, **When** user executes any command, **Then** output appears in the configured terminal mode (vterm/eat/term/compilation)
3. **Given** a command class defines custom execute behavior, **When** the command runs, **Then** the custom behavior is used instead of default

---

### User Story 5 - View Documentation with Live Previews (Priority: P2)

A user wants to learn how to use beads.el commands. They open the Info manual and see exact text-based previews of each transient menu alongside descriptions of functionality.

**Why this priority**: Documentation with live previews ensures users always see accurate information, reducing confusion from stale docs.

**Independent Test**: Can be tested by opening the Info manual and comparing transient previews to actual transient displays.

**Acceptance Scenarios**:

1. **Given** user opens beads.el Info manual, **When** viewing a command section, **Then** an exact text preview of the transient menu is shown
2. **Given** a transient menu changes, **When** the manual is regenerated, **Then** the preview in documentation matches the updated transient
3. **Given** user reads command documentation, **When** they try the actual command, **Then** the transient matches the documented preview exactly

---

### User Story 6 - Run Integration Tests (Priority: P2)

A developer wants to verify command functionality works end-to-end. They run integration tests that execute commands against temporary beads/git repositories without polluting the working directory.

**Why this priority**: Integration tests ensure real functionality works, not just unit-level behavior. Temporary repos prevent test pollution.

**Independent Test**: Can be tested by running the integration test suite and verifying all tests pass with temporary repos created and cleaned up.

**Acceptance Scenarios**:

1. **Given** integration tests are run, **When** a test executes a beads command, **Then** it operates on a temporary beads/git repository
2. **Given** integration tests complete, **When** checking the working directory, **Then** no test artifacts remain
3. **Given** a command's behavior changes, **When** running its integration test, **Then** the test validates the new behavior correctly

---

### User Story 7 - Test Commands in Non-Graphical Emacs (Priority: P3)

A developer needs to verify real user interactions work correctly. They run Emacs inside tmux and manually test command workflows to ensure non-graphical compatibility.

**Why this priority**: Ensures beads.el works in terminal/server environments, not just GUI Emacs.

**Independent Test**: Can be tested by launching Emacs in tmux, invoking commands, and verifying transients and output render correctly.

**Acceptance Scenarios**:

1. **Given** Emacs runs in terminal mode inside tmux, **When** user invokes `M-x beads`, **Then** the transient menu displays correctly
2. **Given** terminal Emacs with a command transient open, **When** user executes a command, **Then** output renders appropriately for terminal display

---

### Edge Cases

- When `bd` executable is not found in PATH: Display error message with installation instructions linking to beads repository/documentation
- How does system handle `bd` command execution failing (network issues, corrupted install, invalid arguments)?
- What happens when user tries to execute with invalid/missing required arguments?
- How does system handle very long command output that exceeds buffer size?
- What happens when user cancels a running command mid-execution?
- How does system behave when no beads repository exists in current directory?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a main transient menu (`M-x beads`) showing all commands matching `bd --help` structure
- **FR-002**: System MUST provide one elisp file per top-level command containing all subcommands (e.g., `beads-command-daemon.el` contains `daemon`, `daemon-list`, `daemon-start`, etc.)
- **FR-003**: System MUST define EIEIO classes for ALL beads commands following naming convention: `bd create` -> `beads-command-create`, `bd daemon list` -> `beads-command-daemon-list`
- **FR-004**: Developers MUST populate EIEIO meta slots by referencing `bd <command> --help` at class design time (manual encoding, not runtime parsing)
- **FR-005**: System MUST auto-generate interactive Emacs commands from class definitions: `beads-command-foo` -> `M-x beads-foo`
- **FR-006**: Auto-generated commands MUST prompt for positional arguments before opening transient menu
- **FR-007**: Transient menus MUST allow modifying positional arguments, command-specific options, and global options
- **FR-008**: Transient menus MUST provide Execute, Preview, and Reset actions
- **FR-009**: Execute/Preview/Reset actions MUST be customizable per class via EIEIO generic methods
- **FR-010**: System MUST provide default execute/render behavior that works without class specializers
- **FR-011**: Default rendering MUST use fallback chain: vterm → eat → term → compilation-mode (first available package wins; user-configurable override supported)
- **FR-012**: Classes MUST be able to override execution mode and rendering via EIEIO generic methods
- **FR-013**: System MUST provide integration tests for each command using temporary beads/git repositories
- **FR-014**: Integration tests MUST NOT pollute the working directory
- **FR-015**: System MUST provide an Org-based manual compilable to Info mode
- **FR-016**: Manual MUST contain exact text-based previews of transient menus
- **FR-017**: Transient menu previews in documentation MUST stay synchronized with actual transient definitions
- **FR-018**: Commands MUST be testable in non-graphical Emacs running inside tmux

### Key Entities

- **beads-command (base class)**: Base EIEIO class for all commands; contains common slots (name, description, positional-args, options, global-options) and default methods
- **Command Metadata**: Information extracted from `bd <command> --help` including description, arguments, options, and their types/defaults
- **Transient Menu**: Dynamically generated interface for each command showing arguments, options, and actions
- **Execution Context**: Environment in which command runs (vterm, eat, term, compilation buffer, custom buffer)
- **Rendering Strategy**: How command output is displayed (terminal, tabulated-list, custom buffer, message)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All commands from `bd --help` have corresponding EIEIO classes and auto-generated interactive commands
- **SC-002**: Users can execute any beads command within 3 interactions from `M-x beads` (menu navigation + execute)
- **SC-003**: 100% of commands work with default behavior without requiring class specializers
- **SC-004**: Integration test suite achieves 80%+ code coverage for command execution paths
- **SC-005**: All integration tests pass with zero working directory pollution (verified by git status)
- **SC-006**: Info manual contains accurate transient previews verified by automated comparison
- **SC-007**: All commands function correctly in both GUI and terminal (tmux) Emacs environments
- **SC-008**: Command preview shows exact `bd` command that will execute, matching actual execution 100%

## Clarifications

### Session 2026-01-04

- Q: What is the default terminal rendering mode? → A: Fallback chain: vterm → eat → term → compilation-mode (use best available)
- Q: Behavior when `bd` executable not found? → A: Show error with installation instructions (link to beads repo/docs)
- Q: When to populate command metadata from `--help`? → A: At class design time - developers manually reference `bd <command> --help` and encode options/descriptions into EIEIO slots

## Assumptions

- The `bd` CLI tool is installed and available in the user's PATH
- Users have Emacs 27.1+ with transient library available
- The beads repository structure follows standard `bd` conventions
- Terminal modes (vterm/eat/term) are available when configured as rendering mode
- Org-mode is available for manual compilation to Info format
- `bd <command> --help` output follows a consistent, parseable format
