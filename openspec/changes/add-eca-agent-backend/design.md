# Design: ECA Backend Integration

## Architecture Overview

The ECA backend follows the established `beads-agent-backend` protocol,
providing a thin adapter between beads.el's session management and ECA's
JSON-RPC API.

```
┌─────────────────────────────────────────────────────────────┐
│ beads-agent.el (User-facing API)                            │
│  - beads-agent-start                                        │
│  - beads-agent-stop                                         │
│  - beads-agent-switch-to                                    │
└──────────────┬──────────────────────────────────────────────┘
               │ Uses protocol
               ↓
┌──────────────────────────────────────────────────────────────┐
│ beads-agent-backend.el (Abstract Protocol)                   │
│  - beads-agent-backend-available-p                           │
│  - beads-agent-backend-start                                 │
│  - beads-agent-backend-stop                                  │
│  - beads-agent-backend-session-active-p                      │
│  - beads-agent-backend-switch-to-buffer                      │
│  - beads-agent-backend-send-prompt                           │
└──────────────┬───────────────────────────────────────────────┘
               │ Implementation
               ↓
┌──────────────────────────────────────────────────────────────┐
│ beads-agent-eca.el (Concrete Backend)                        │
│  - beads-agent-backend-eca (EIEIO Class)                     │
│  - Implementation of all protocol methods                    │
└──────────────┬───────────────────────────────────────────────┘
               │ Calls
               ↓
┌──────────────────────────────────────────────────────────────┐
│ eca-emacs (External Package)                                 │
│  - eca-start-session                                         │
│  - eca-send-message                                          │
│  - eca-kill-session                                          │
└──────────────────────────────────────────────────────────────┘
```

## Session Model

ECA backend integrates naturally with beads.el's directory-bound model:

- **Session Identity**: `project-dir` (absolute path to project root)
- **Session ID Format**: `proj-name#N` (e.g., `beads.el#1`)
- **Session Storage**: sesman (Session Manager)
- **Multiple Instances**: **YES** - ECA supports multiple sessions per workspace

### ECA's Multi-Session Architecture

ECA stores sessions in an alist keyed by workspace:

```elisp
(defvar eca--sessions '())  ;; alist: ((id1 . session1) (id2 . session2) ...)

(defun eca-session ()
  ;; Returns session for current buffer based on:
  ;; 1. Buffer-local cache (eca--session-id-cache)
  ;; 2. OR workspace folder matching against current buffer path
  ...)
```

This means:
```
┌─────────────────────────────────────────────────────────────┐
│ ECA: Multiple sessions by workspace                         │
│                                                             │
│   Session 1 (project-a)  ←──→  <eca-chat:1:1>              │
│   Session 2 (project-b)  ←──→  <eca-chat:2:1>              │
│   Session 3 (project-c)  ←──→  <eca-chat:3:1>              │
└─────────────────────────────────────────────────────────────┘
```

### Controlling Session Selection

The key is `default-directory`:

```elisp
(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-eca) _issue prompt)
  ;; default-directory is set by beads-agent-start to project/worktree
  ;; ECA's (eca-session) will find/create session for this directory
  (eca)  ;; Creates session for default-directory's workspace
  ...)
```

Since beads-agent already sets `default-directory` before calling backend,
ECA will automatically create/find the correct session for that project.

### Session Lifecycle

1. **Creation** (`beads-agent-backend-start`):
   - Call eca-emacs API to start session
   - Create `beads-agent-session` object
   - Register with sesman via `beads-agent--create-session`
   - Store ECA-specific session object in `:backend-session` slot

2. **Active Check** (`beads-agent-backend-session-active-p`):
   - Check if ECA buffer exists and process is alive
   - Query ECA session status

3. **Cleanup** (`beads-agent-backend-stop`):
   - Send stop command to ECA
   - Kill ECA buffer/process
   - Unregister from sesman via `beads-agent--destroy-session`

## Buffer Renaming

beads.el renames backend buffers to its own naming convention via
`beads-agent--rename-and-store-buffer`. This happens automatically
after `beads-agent-backend-start` returns.

### How It Works

1. Backend returns `(backend-session . buffer)` from start
2. beads-agent calls `beads-agent--rename-and-store-buffer`
3. Buffer is renamed to `*beads-agent[PROJECT]/TYPE#N*`
4. Buffer is stored in session's `buffer` slot
5. Buffer's `default-directory` is set to working directory

### Implications for ECA

- Backend must return the ECA chat buffer from start
- We cannot use `eca-stop` to stop (it won't find renamed buffer)
- Must kill buffer directly and terminate process explicitly
- Must find ECA chat buffer by pattern or via `eca-chat-open`

```elisp
;; In beads-agent-backend-stop:
(cl-defmethod beads-agent-backend-stop
    ((backend beads-agent-backend-eca) session)
  ;; Can't use eca-stop - buffer is renamed
  ;; Must kill buffer directly
  (when-let ((buffer (beads-agent-backend-get-buffer backend session)))
    (when (buffer-live-p buffer)
      (when-let ((proc (get-buffer-process buffer)))
        (delete-process proc))
      (kill-buffer buffer))))
```

## Protocol Implementation Details

### beads-agent-backend-available-p

Check availability requirements:
1. `eca` feature is loaded (or loadable)
2. Required functions exist (`eca`, `eca-stop`, etc.)
3. `eca` executable found in PATH (auto-downloaded by eca-emacs if missing)

```elisp
(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-eca))
  (and (or (featurep 'eca)
           (require 'eca nil t))
       (fboundp 'eca)
       (fboundp 'eca-stop)
       (or (executable-find "eca")
           ;; eca-emacs auto-downloads the server
           (fboundp 'eca-install-server))))
```

### beads-agent-backend-start

Start ECA session with initial prompt:
1. Ensure working directory is set to project root
2. Call `eca` to initialize session (starts server if needed)
3. Get session via `eca-session`
4. Send initial prompt via `eca-chat-send-prompt`
5. Find chat buffer via `eca-chat-open`
6. Return `(cons eca-session buffer)`

```elisp
(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-eca) _issue prompt)
  (require 'eca)
  (require 'eca-chat)
  (let ((default-directory default-directory))
    ;; Start ECA session (interactive, may prompt for workspace)
    (eca)
    ;; Wait for session to be ready
    (let ((session (eca-session)))
      ;; Send initial prompt
      (eca-chat-send-prompt prompt)
      ;; Get the chat buffer
      (let ((buffer (beads-agent-eca--find-chat-buffer session)))
        (cons session buffer)))))
```

### beads-agent-backend-stop

Stop ECA session:
1. Call `eca-stop` to terminate session
2. ECA handles buffer and process cleanup internally

```elisp
(cl-defmethod beads-agent-backend-stop
    ((_backend beads-agent-backend-eca) _session)
  (require 'eca)
  (eca-stop))
```

### beads-agent-backend-session-active-p

Check if session is active:
1. Get ECA session
2. Check process status via `eca-process-running-p`

```elisp
(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-eca) session)
  (require 'eca)
  (require 'eca-process)
  (when-let ((eca-session (oref session backend-session)))
    (eca-process-running-p eca-session)))
```

### beads-agent-backend-switch-to-buffer

Switch to ECA chat buffer:
1. Get session and open chat
2. Use `eca-chat-open` for proper display

```elisp
(cl-defmethod beads-agent-backend-switch-to-buffer
    ((_backend beads-agent-backend-eca) session)
  (require 'eca)
  (require 'eca-chat)
  (when-let ((eca-session (oref session backend-session)))
    (eca-chat-open eca-session)))
```

### beads-agent-backend-send-prompt

Send prompt to existing session:
1. Call `eca-chat-send-prompt` with prompt string

```elisp
(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-eca) _session prompt)
  (require 'eca-chat)
  (eca-chat-send-prompt prompt))
```

## ECA API Reference (Discovered)

Key eca-emacs functions from source analysis:

**Session Management (eca.el):**
- `eca &optional arg` - Start/switch to ECA session (interactive)
- `eca-stop` - Terminate running session
- `eca-restart` - Stop and restart session
- `eca-session` - Get current session object
- `eca-create-session workspaces` - Create session with workspace list

**Process Control (eca-process.el):**
- `eca-process-start` - Start ECA server process
- `eca-process-stop` - Terminate ECA process
- `eca-process-running-p session` - Check if server is active

**Chat Interaction (eca-chat.el):**
- `eca-chat-send-prompt prompt` - Send prompt string to chat
- `eca-chat-open session` - Open/create chat window
- `eca-chat-exit session` - Close chat buffers
- `eca-chat-stop-prompt` - Halt in-progress response

## Error Handling

Follow existing backend error handling patterns:

1. **Availability Checks**: Verify eca feature and functions exist
2. **Session Failures**: Handle `eca` initialization errors gracefully
3. **Process Crashes**: `eca-process-running-p` returns nil when crashed
4. **Buffer Cleanup**: ECA handles cleanup via `eca-stop`

## Testing Strategy

Follow the mock-based testing pattern from other backends:

```elisp
(ert-deftest beads-agent-eca-available-p-when-loaded ()
  "Test availability check when eca-emacs is loaded."
  (with-mocked-eca-environment
    (let ((backend (beads-agent-backend-eca)))
      (should (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-eca-start-creates-session ()
  "Test that start method creates and returns session."
  (with-mocked-eca-environment
    (let* ((backend (beads-agent-backend-eca))
           (result (beads-agent-backend-start backend nil "Test prompt")))
      (should (consp result))
      (should (bufferp (cdr result))))))
```

## Dependencies on eca-emacs API

The implementation requires these eca-emacs components:

1. **Package Feature**: `(require 'eca)` - Main entry point
2. **Chat Module**: `(require 'eca-chat)` - Prompt sending
3. **Process Module**: `(require 'eca-process)` - Status checks

All functions are part of the public eca-emacs API.

## Alternatives Considered

### Alternative 1: Direct JSON-RPC

Bypass eca-emacs and talk directly to ECA server via JSON-RPC. Rejected
because:
- Duplicates eca-emacs functionality
- Harder to maintain
- Breaks when ECA protocol changes

### Alternative 2: Shell Command Integration

Use `eca` CLI instead of programmatic API. Rejected because:
- Poor user experience (no live interaction)
- Hard to track session state
- Doesn't support interactive workflows

## Future Enhancements

1. **Advanced Tool Approval**: Integrate with ECA's MCP tool approval UI
2. **Custom Prompts**: ECA-specific prompt templates
3. **Voice Integration**: Support ECA's whisper.el voice input
4. **Repository Mapping**: Automatically include repo maps in context
5. **Model Selection**: UI for selecting AI models in ECA
