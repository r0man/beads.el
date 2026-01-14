# Change: Add ECA (Editor Code Assistant) Agent Backend

## Why

beads.el currently supports multiple AI agent backends (claude-code,
claude-code-ide, claudemacs, efrit, agent-shell) but does not support ECA
(Editor Code Assistant). ECA is an AI pair-programming client with a
client-server architecture: eca-emacs (Emacs client) communicates with
the eca server via JSON-RPC over stdio.

Adding ECA support provides:

1. **Alternative AI backend** - Users can choose ECA alongside existing agents
2. **MCP tool integration** - ECA has built-in MCP support for tool calling
3. **Model flexibility** - ECA supports multiple AI models via configuration
4. **Mature API** - Well-defined Elisp API (eca.el, eca-chat.el, eca-process.el)
5. **Active development** - Growing ecosystem with editor integrations

## What Changes

### 1. Add ECA Backend Implementation

- **ADD** `lisp/beads-agent-eca.el` module implementing the
  `beads-agent-backend` protocol using eca-emacs public API:
  - `beads-agent-backend-available-p` - Check `eca` feature and executable
  - `beads-agent-backend-start` - Use `eca`, `eca-chat-send-prompt`
  - `beads-agent-backend-stop` - Use `eca-stop`
  - `beads-agent-backend-session-active-p` - Use `eca-process-running-p`
  - `beads-agent-backend-switch-to-buffer` - Use `eca-chat-open`
  - `beads-agent-backend-send-prompt` - Use `eca-chat-send-prompt`

### 2. Backend Configuration

- **ADD** Backend class `beads-agent-backend-eca`:
  - Name: "eca"
  - Priority: 20 (after claude-code-ide=10, before others)
  - Description: "AI pair-programming via ECA server"

### 3. Integration with beads-agent System

- Backend auto-registers on load via `beads-agent--register-backend`
- Works with existing agent-type system (Task, Review, Plan, QA)
- Integrates with sesman session management
- Supports directory-bound sessions (project-dir as identity)

### 4. ECA API Integration

Key eca-emacs functions used:
- `eca` - Start or switch to ECA session (interactive)
- `eca-stop` - Terminate running session
- `eca-session` - Get current session object
- `eca-process-running-p` - Check if server is active
- `eca-chat-send-prompt` - Send prompt string to chat
- `eca-chat-open` - Open chat buffer for session

## Multiple Session Support

**ECA supports multiple concurrent sessions**, one per workspace/directory.
Sessions are stored in `eca--sessions` alist and looked up by workspace path:

- Each project directory can have its own ECA session
- `eca-session` returns session based on buffer's workspace folder
- Buffer renaming preserves session affinity (buffer-local cache)
- Multiple beads ECA agents CAN run in parallel on different projects

This matches beads.el's directory-bound session model well.

## Impact

- **Affected specs:**
  - MODIFIED: `agent-backends` - Add ECA to supported backends list

- **Affected code:**
  - NEW: `lisp/beads-agent-eca.el` - ECA backend implementation (~200 LOC)
  - NEW: `lisp/test/beads-agent-eca-test.el` - ERT tests (~250 LOC)
  - No changes to existing modules (pure addition via registration)

- **Breaking changes:** None (pure addition)

- **Dependencies:**
  - Runtime (optional): `eca-emacs` package (provides eca.el feature)
  - Runtime (optional): `eca` executable in PATH (auto-downloaded by eca-emacs)
  - Backend only available when eca-emacs is installed

- **Key implementation detail:**
  - Must ensure `default-directory` is set correctly before calling `eca`
  - ECA looks up session by workspace path from current buffer

## Implementation Approach

Follow the existing backend pattern from `beads-agent-claude-code-ide.el`:

1. Define EIEIO class inheriting from `beads-agent-backend`
2. Implement required protocol methods using eca-emacs API
3. Handle ECA's workspace-based session model
4. Register backend on module load
5. Add comprehensive tests with mocking

## ECA API Reference

```elisp
;; Session management
(eca &optional arg)              ; Start/switch to session
(eca-stop)                       ; Terminate session
(eca-session)                    ; Get current session
(eca-process-running-p session)  ; Check if process alive

;; Chat interaction
(eca-chat-send-prompt prompt)    ; Send prompt string
(eca-chat-open session)          ; Open chat buffer
```

## Verification

- [ ] ERT tests pass (100% coverage for new code)
- [ ] Backend appears in `beads-agent-start` completion
- [ ] Sessions tracked in sesman
- [ ] Prompts delivered to ECA successfully
- [ ] Byte compilation passes
- [ ] Package-lint passes
