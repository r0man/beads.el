# Tasks: Add ECA Agent Backend

**Epic:** bde-496o

Implementation tasks ordered by dependency. Each task is a discrete unit
of work suitable for conversion to a beads issue.

## 1. Core Implementation

### 1.1 Create beads-agent-eca.el module → bde-jxi5
- [ ] Create `lisp/beads-agent-eca.el` with file header and requires
- [ ] Define `beads-agent-backend-eca` EIEIO class
- [ ] Implement `beads-agent-backend-available-p` method
- [ ] Implement `beads-agent-backend-start` method
- [ ] Implement `beads-agent-backend-stop` method
- [ ] Implement `beads-agent-backend-session-active-p` method
- [ ] Implement `beads-agent-backend-switch-to-buffer` method
- [ ] Implement `beads-agent-backend-send-prompt` method
- [ ] Add backend registration at module load
- [ ] Add `provide` form and file footer

**Verification:**
- Module loads without errors
- Backend appears in `beads-agent--get-all-backends`

### 1.2 Add helper functions → bde-887c
- [ ] `beads-agent-eca--find-chat-buffer` - Find ECA chat buffer for session
- [ ] `beads-agent-eca--get-session-for-dir` - Get ECA session for directory
- [ ] Declare external eca-emacs functions to avoid compiler warnings

**Verification:**
- Byte compilation produces no warnings
- Multiple sessions in different directories work correctly

## 2. Testing

### 2.1 Create test file → bde-ntvn
- [ ] Create `lisp/test/beads-agent-eca-test.el` with file header
- [ ] Add mock helpers for eca-emacs functions
- [ ] Add `with-mocked-eca-environment` macro

### 2.2 Write unit tests → bde-b6g4
- [ ] Test `beads-agent-backend-available-p` when eca loaded
- [ ] Test `beads-agent-backend-available-p` when eca not loaded
- [ ] Test `beads-agent-backend-start` creates session
- [ ] Test `beads-agent-backend-start` sends initial prompt
- [ ] Test `beads-agent-backend-start` with different directories
- [ ] Test `beads-agent-backend-stop` kills buffer and process
- [ ] Test `beads-agent-backend-session-active-p` checks process
- [ ] Test `beads-agent-backend-switch-to-buffer` opens chat
- [ ] Test `beads-agent-backend-send-prompt` sends to chat

**Verification:**
- All tests pass
- Code coverage >80% for new module

## 3. Quality Gates

### 3.1 Compile and lint → bde-u8e0
- [ ] Run byte compilation: `eldev -p -dtT compile`
- [ ] Run linter: `eldev -p -dtT lint`
- [ ] Fix any warnings or errors

### 3.2 Full test suite → bde-7dkk
- [ ] Run all tests: `BD_NO_DAEMON=1 eldev -p -dtT test`
- [ ] Verify no regressions in existing tests

## 4. Integration Testing (Manual)

### 4.1 Test with live eca-emacs in tmux → bde-ouar
- [ ] Create temporary beads project with `bd init`
- [ ] Create a test issue with `bd create "Test issue" --type task`
- [ ] Start non-graphical Emacs in tmux: `emacs -nw`
- [ ] Load beads.el and beads-agent-eca.el
- [ ] Install eca-emacs package if not present
- [ ] Verify backend appears in `beads-agent-start` completion
- [ ] Test starting agent on test issue
- [ ] Verify buffer is renamed to beads format
- [ ] Test sending prompt to running session
- [ ] Test stopping agent via `beads-agent-stop`
- [ ] Test multiple sessions in different directories (worktrees)

**Testing environment:**
```bash
# Create temp project
cd /tmp && mkdir test-eca && cd test-eca
git init && bd init
bd create "Test ECA backend" --type task

# Start non-graphical Emacs in tmux
tmux new-session -s eca-test
emacs -nw
```

**Note:** Manual testing requires eca-emacs and eca server installed.

## Dependencies

```
1.1 → 1.2 → 2.1 → 2.2 → 3.1 → 3.2 → 4.1
```

- 1.2 depends on 1.1 (needs module structure)
- 2.1 depends on 1.2 (needs complete module to test)
- 2.2 depends on 2.1 (needs test framework)
- 3.1 depends on 2.2 (quality gate after tests)
- 3.2 depends on 3.1 (full suite after lint/compile)
- 4.1 depends on 3.2 (manual testing after automated)
