# Agent Backends Capability

## ADDED Requirements

### Requirement: ECA Backend

The system SHALL support ECA (Editor Code Assistant) as an agent backend
for AI-assisted issue work when eca-emacs is installed.

#### Scenario: ECA backend availability check

- **WHEN** user queries available backends
- **AND** eca-emacs package is installed and loaded
- **AND** eca executable exists (in PATH or auto-downloaded)
- **THEN** "eca" appears in available backends list
- **AND** backend has priority 20 (after claude-code-ide)
- **AND** backend description is "AI pair-programming via ECA server"

#### Scenario: ECA backend unavailable

- **WHEN** user queries available backends
- **AND** eca-emacs package is NOT installed
- **THEN** "eca" does NOT appear in available backends list
- **AND** other backends work normally

#### Scenario: Start agent via ECA

- **WHEN** user starts agent with backend "eca"
- **AND** provides an initial prompt
- **THEN** system calls `eca` to initialize session
- **AND** sends initial prompt via `eca-chat-send-prompt`
- **AND** returns cons cell (backend-session . buffer)
- **AND** session is registered with sesman

#### Scenario: Stop agent via ECA

- **WHEN** user stops an active ECA agent session
- **THEN** system calls `eca-stop` to terminate session
- **AND** ECA process is terminated
- **AND** session is unregistered from sesman

#### Scenario: Check ECA session status

- **WHEN** system checks if ECA session is active
- **THEN** system calls `eca-process-running-p` with session
- **AND** returns non-nil if ECA server is running
- **AND** returns nil if ECA server has stopped

#### Scenario: Switch to ECA buffer

- **WHEN** user requests to switch to agent buffer
- **AND** session uses ECA backend
- **THEN** system calls `eca-chat-open` with session
- **AND** ECA chat buffer is displayed

#### Scenario: Send prompt to ECA session

- **WHEN** user sends prompt to active ECA session
- **THEN** system calls `eca-chat-send-prompt` with prompt string
- **AND** prompt is delivered to ECA chat
- **AND** agent begins processing

### Requirement: ECA Error Handling

The system SHALL handle ECA-specific errors gracefully and provide
helpful feedback to users.

#### Scenario: ECA startup failure

- **WHEN** user attempts to start ECA agent
- **AND** eca-emacs fails to initialize (missing deps, server error)
- **THEN** system signals error with descriptive message
- **AND** suggests troubleshooting steps
- **AND** session is NOT created

#### Scenario: ECA server crash during session

- **WHEN** ECA server terminates unexpectedly during session
- **AND** user queries session status
- **THEN** `beads-agent-backend-session-active-p` returns nil
- **AND** session can be cleaned up via normal stop flow

### Requirement: ECA Backend Registration

The system SHALL automatically register the ECA backend when the
beads-agent-eca module is loaded.

#### Scenario: Automatic registration on load

- **WHEN** `beads-agent-eca.el` is loaded (require or autoload)
- **THEN** ECA backend is registered via `beads-agent--register-backend`
- **AND** backend is sorted by priority with other backends
- **AND** backend is available for selection immediately

### Requirement: ECA Buffer Renaming

The system SHALL rename ECA chat buffers to the beads naming convention
for consistent buffer management and session tracking.

#### Scenario: Buffer renamed after session start

- **WHEN** ECA backend starts successfully
- **AND** returns chat buffer to beads-agent
- **THEN** beads-agent renames buffer to `*beads-agent[PROJECT]/TYPE#N*`
- **AND** buffer is stored in session's buffer slot
- **AND** buffer's default-directory is set to working directory

#### Scenario: Stop uses renamed buffer

- **WHEN** user stops an ECA session
- **THEN** system uses the renamed beads buffer (not original ECA name)
- **AND** kills the buffer directly (ECA's stop won't find renamed buffer)
- **AND** terminates underlying ECA process

### Requirement: ECA Multi-Session Support

The system SHALL support multiple concurrent ECA sessions, one per
workspace/project directory.

#### Scenario: Multiple ECA sessions in different projects

- **WHEN** user starts ECA agent in project A
- **AND** then starts ECA agent in project B
- **THEN** system creates separate ECA sessions for each project
- **AND** both sessions run concurrently
- **AND** each session has its own chat buffer and process

#### Scenario: Session lookup by workspace

- **WHEN** beads-agent sets `default-directory` to project path
- **AND** calls ECA backend start
- **THEN** ECA finds/creates session for that workspace
- **AND** returns the correct session's chat buffer
