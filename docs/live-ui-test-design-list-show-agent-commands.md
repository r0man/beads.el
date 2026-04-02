# Live UI Test Design: List and Show Buffer Agent Commands

**Bead**: hq-mol-eqmq
**Implements**: `test/beads-live-ui-test.el` (additions to existing file)
**See also**: `docs/live-ui-test-design.md` (parent design)

---

## Scope

This document designs live tests for commands accessible from `beads-list-mode`
and `beads-show-mode` buffers that were deferred to V2 in the parent design:

1. **Agent commands** — the `beads-agent-prefix-map` bound to `a` in both
   list and show modes (agent start, stop, jump)
2. **Remaining list buffer commands** — navigation, mark/unmark, copy-id,
   sort, filter, follow-mode, bulk operations
3. **Remaining show buffer commands** — section navigation, reference
   navigation, field editing, compose commands, refresh

---

## Infrastructure Requirements

### Mock Agent Backend

All agent command tests MUST use `beads-agent-backend-mock` to avoid launching
real AI backends.  The mock provides:

```elisp
(require 'beads-agent-mock)
(beads-agent-mock-register)    ; Setup — call in test fixture
(beads-agent-mock-reset)       ; Clear state between tests
(beads-agent-mock-unregister)  ; Teardown — call in cleanup

;; Assertions
(beads-agent-mock-assert-start-called &optional times)
(beads-agent-mock-assert-session-count expected)
(beads-agent-mock-sessions)   ; All mock sessions
(beads-agent-mock-active-sessions)
```

Tests must override `beads-agent-backend` (or its default selection) to the
mock, and reset all mock state between scenarios.  A helper macro:

```elisp
(defmacro beads-live-test--with-mock-agent (&rest body)
  "Run BODY with mock agent backend registered and reset on exit."
  (declare (indent 0))
  `(progn
     (beads-agent-mock-register)
     (beads-agent-mock-reset)
     (unwind-protect
         (progn ,@body)
       (beads-agent-mock-reset)
       (beads-agent-mock-unregister))))
```

### List Buffer Fixture

Many tests need an open `beads-list-mode` buffer with at least one issue.
Extract a helper:

```elisp
(defmacro beads-live-test--with-list-buffer (issues &rest body)
  "Open a list buffer seeded with ISSUES, run BODY, kill buffer on exit."
  ...)
```

This macro should:
1. Use `beads-test-with-temp-repo-and-issues` for isolation
2. Kill any stale list buffers before opening (as in Scenario 8)
3. Call `beads-list-all` to open the buffer
4. Find the live buffer via `derived-mode-p`
5. Run body with buffer current
6. Kill buffer in unwind-protect

### Show Buffer Fixture

```elisp
(defmacro beads-live-test--with-show-buffer (issue-id &rest body)
  "Open a show buffer for ISSUE-ID, run BODY, kill buffer on exit."
  ...)
```

---

## Scenario 11: Agent Commands — Start at Point (List Buffer)

**Test**: `beads-live-test-agent-start-at-point-from-list`
**Tags**: `:live :integration`
**Prerequisites**: `(executable-find beads-executable)`

```
Setup:
  - Seed temp repo with 1 issue
  - Open list buffer, navigate to issue row
  - Register mock backend

Execute:
  - Call (beads-agent-start-at-point)

Assert:
  - (beads-agent-mock-assert-start-called 1) — one session was started
  - (= 1 (length (beads-agent-mock-active-sessions)))
  - Session issue-id matches the issue at point

Cleanup:
  - Unregister mock, kill list buffer, delete temp repo
```

**Test**: `beads-live-test-agent-start-at-point-from-show`
**Tags**: `:live :integration`

Same as above but open a show buffer for the issue and call from there.

---

## Scenario 12: Agent Commands — Typed Start (List Buffer)

One test per agent type command.  Each follows the same pattern:

**Tests** (with tags `:live :integration`):
- `beads-live-test-agent-start-task-from-list`
- `beads-live-test-agent-start-review-from-list`
- `beads-live-test-agent-start-plan-from-list`
- `beads-live-test-agent-start-qa-from-list`

```
For each type-name in ("Task" "Review" "Plan" "QA"):
  Setup:
    - Seed 1 issue, open list buffer, navigate to row
    - Register mock backend

  Execute:
    - Call (beads-agent-start-task) / etc.

  Assert:
    - (beads-agent-mock-assert-start-called 1)
    - Session type-name matches expected (e.g., "Task")

  Cleanup: same as above
```

**Test**: `beads-live-test-agent-start-task-jumps-to-existing`
**Tags**: `:live :integration`

When a Task session already exists for the issue and the command is called
without prefix arg:

```
Setup:
  - Seed 1 issue, open list buffer
  - Start a mock Task session for the issue
  - Navigate list buffer to issue row

Execute:
  - Call (beads-agent-start-task)  ; no prefix arg

Assert:
  - No NEW session started (beads-agent-mock-assert-start-called 1 still)
  - Current buffer switched to existing agent buffer
```

---

## Scenario 13: Agent Commands — Stop at Point

**Test**: `beads-live-test-agent-stop-at-point-from-list`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue, open list buffer, navigate to row
  - Register mock backend
  - Start a mock session: (beads-agent-start issue-id)

Execute:
  - Call (beads-agent-stop-at-point)

Assert:
  - (= 0 (length (beads-agent-mock-active-sessions)))
  - Message shown (no error signaled)
```

**Test**: `beads-live-test-agent-stop-at-point-no-session`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue, open list buffer, navigate to row
  - No active sessions

Execute:
  - Call (beads-agent-stop-at-point)

Assert:
  - No error signaled
  - Message shown (e.g., "No agents running for ...")
```

---

## Scenario 14: Agent Commands — Jump at Point

**Test**: `beads-live-test-agent-jump-at-point-from-list`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue, open list buffer, navigate to row
  - Register mock backend
  - Start a mock session for the issue

Execute:
  - Call (beads-agent-jump-at-point)

Assert:
  - Current buffer is the agent's buffer (check buffer name or major mode)
  OR
  - No error signaled and buffer switch occurred
```

**Test**: `beads-live-test-agent-jump-at-point-starts-when-no-session`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue, open list buffer, navigate to row
  - No active sessions

Execute:
  - Call (beads-agent-jump-at-point)

Assert:
  - A new session was started (beads-agent-mock-assert-start-called 1)
```

---

## Scenario 15: Agent Commands — Show Buffer Context

Same agent tests as Scenarios 11-14, but exercised from `beads-show-mode`:

- `beads-live-test-agent-start-at-point-from-show`
- `beads-live-test-agent-stop-at-point-from-show`
- `beads-live-test-agent-jump-at-point-from-show`

The key assertion difference: `(beads-agent--detect-issue-id)` must return
the issue ID from the show buffer's `beads-show--issue-id` local variable.

---

## Scenario 16: List Buffer Navigation (n/p)

**Test**: `beads-live-test-list-navigation-next-previous`
**Tags**: `:live :integration`

```
Setup:
  - Seed 2 issues, open list buffer
  - Go to (point-min)

Execute:
  - Record position-before = (point)
  - Call (beads-list-next)
  - Record position-after = (point)

Assert:
  - position-after != position-before  (point moved)
  - (beads-issue-at-point) returns a valid issue ID

Execute:
  - Call (beads-list-previous)

Assert:
  - (point) back near position-before
```

---

## Scenario 17: beads-list-show (RET)

**Test**: `beads-live-test-list-show-opens-show-buffer`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue, open list buffer, navigate to row

Execute:
  - Call (beads-list-show)

Assert:
  - A *beads-show* buffer exists for that issue ID
  - Buffer content includes the issue title
  - (kill show buffer in cleanup)
```

---

## Scenario 18: beads-list-refresh (g)

**Test**: `beads-live-test-list-refresh-shows-new-issue`
**Tags**: `:live :integration`

```
Setup:
  - Open list buffer (initially 1 issue)

Execute:
  - Create a second issue via (beads-command-create! :title "New issue" ...)
  - Call (beads-list-refresh)

Assert:
  - Buffer row count increased
  - "New issue" title appears in buffer text (tabulated-list rows)
```

---

## Scenario 19: Mark / Unmark / Mark-All

**Test**: `beads-live-test-list-mark-unmark`
**Tags**: `:live :integration`

```
Setup:
  - Seed 2 issues, open list buffer, go to point-min

Execute:
  - Call (beads-list-mark) on first row

Assert:
  - beads-list--marked-ids contains the first issue ID
  - Buffer text shows mark indicator (e.g., "*")

Execute:
  - Call (beads-list-unmark)

Assert:
  - beads-list--marked-ids is empty
```

**Test**: `beads-live-test-list-mark-all-unmark-all`
**Tags**: `:live :integration`

```
Setup:
  - Seed 2 issues, open list buffer

Execute:
  - Call (beads-list-mark-all)

Assert:
  - (length beads-list--marked-ids) = 2

Execute:
  - Call (beads-list-unmark-all)

Assert:
  - beads-list--marked-ids is nil/empty
```

---

## Scenario 20: beads-list-copy-id (w)

**Test**: `beads-live-test-list-copy-id`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue, open list buffer, navigate to row

Execute:
  - Call (beads-list-copy-id)

Assert:
  - (car kill-ring) equals the issue ID of the row at point
```

---

## Scenario 21: beads-list-follow-mode (C-c C-f)

**Test**: `beads-live-test-list-follow-mode-toggle`
**Tags**: `:live :integration`

```
Setup:
  - Open list buffer

Execute:
  - Verify beads-list-follow-mode is nil initially
  - Call (beads-list-follow-mode 1)

Assert:
  - beads-list-follow-mode is non-nil

Execute:
  - Call (beads-list-follow-mode 0)

Assert:
  - beads-list-follow-mode is nil
```

**Test**: `beads-live-test-list-follow-mode-shows-issue-on-move`
**Tags**: `:live :integration`

```
Setup:
  - Seed 2 issues, open list buffer, enable follow-mode, go to row 1

Execute:
  - Call (beads-list-next)  ; move to row 2

Assert:
  - A *beads-show* buffer is now open for the issue on row 2
  - (cleanup: kill show buffer, disable follow-mode)
```

---

## Scenario 22: Bulk Operations

**Test**: `beads-live-test-list-bulk-close`
**Tags**: `:live :integration`

```
Setup:
  - Seed 2 issues, open list buffer
  - Mark both issues: (beads-list-mark-all)

Execute:
  - Mock (read-string) to return "Bulk done"
  - Call (beads-list-bulk-close)

Assert:
  - Both issues have status "closed" (verify via beads-command-show!)
  - beads-list--marked-ids cleared
```

**Test**: `beads-live-test-list-bulk-reopen`
**Tags**: `:live :integration`

```
Setup:
  - Seed 2 issues, close both, open list buffer (filter to closed), mark all

Execute:
  - Call (beads-list-bulk-reopen)

Assert:
  - Both issues have status "open"
```

**Test**: `beads-live-test-list-bulk-update-status`
**Tags**: `:live :integration`

```
Setup:
  - Seed 2 issues, open list buffer, mark all

Execute:
  - Mock (completing-read) to return "in_progress"
  - Call (beads-list-bulk-update-status)

Assert:
  - Both issues have status "in_progress"
```

---

## Scenario 23: beads-list-filter (l) and beads-list-filter-menu (/)

**Test**: `beads-live-test-list-filter-transient-renders`
**Tags**: `:live :transient`

```
Setup:
  - Open list buffer with init-beads t

Execute:
  - Call (beads-list-filter) — opens transient with current filter

Assert:
  - Transient is active (beads-live-test--transient-active-p)
  - (transient-quit-all)
```

Note: `beads-list-filter-menu` (/) already has a smoke test in Scenario 10.
This scenario adds functional coverage for `beads-list-filter` (l) specifically.

---

## Scenario 24: Show Buffer Navigation (n/p sections)

**Test**: `beads-live-test-show-section-navigation`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue with description and type set (to generate multiple sections)
  - Open show buffer

Execute:
  - Go to (point-min)
  - Record pos-start = (point)
  - Call (beads-show-next-section)
  - Record pos-next = (point)

Assert:
  - pos-next > pos-start  (moved forward)

Execute:
  - Call (beads-show-previous-section)

Assert:
  - (point) back near pos-start
```

---

## Scenario 25: beads-show-copy-id (w)

**Test**: `beads-live-test-show-copy-id`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue, open show buffer

Execute:
  - Call (beads-show-copy-id)

Assert:
  - (car kill-ring) equals the issue ID of the show buffer
  - (kill buffer in cleanup)
```

---

## Scenario 26: Show Buffer Reference Navigation

**Test**: `beads-live-test-show-reference-navigation`
**Tags**: `:live :integration`

```
Setup:
  - Seed 2 issues
  - Add dependency: issue-2 depends on issue-1
  - Open show buffer for issue-2 (which will display issue-1's ID as a reference)

Execute:
  - Call (beads-show-next-reference)

Assert:
  - Point is on a button/reference overlay
  - No error signaled

Execute:
  - Call (beads-show-previous-reference)

Assert:
  - Point moved back (or stayed if only one reference)
```

---

## Scenario 27: beads-show-follow-reference (RET)

**Test**: `beads-live-test-show-follow-reference`
**Tags**: `:live :integration`

```
Setup:
  - Seed 2 issues with a dependency (issue-2 → issue-1)
  - Open show buffer for issue-2
  - Navigate to the issue-1 reference link: (beads-show-next-reference)

Execute:
  - Call (beads-show-follow-reference)

Assert:
  - A new show buffer opens for issue-1
  - Buffer name contains issue-1's ID
  - (cleanup: kill both show buffers)
```

---

## Scenario 28: beads-show-actions Transient (?)

**Test**: `beads-live-test-show-actions-from-show-buffer`
**Tags**: `:live :transient`

Note: `beads-show-actions` is already smoke-tested in Scenario 4.
Add a **functional** test from inside a real show buffer:

```
Setup:
  - Seed 1 issue, open show buffer for it
  - Navigate into show buffer

Execute:
  - Call (beads-show-actions)

Assert:
  - Transient is active
  - (transient-quit-all)
  - (kill show buffer in cleanup)
```

---

## Scenario 29: beads-show-edit-field (e / C-c C-e)

**Test**: `beads-live-test-show-edit-field-opens`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue, open show buffer

Execute:
  - Mock (completing-read) to select "title" field
  - Mock (read-string) to return "Updated title"
  - Call (beads-show-edit-field)

Assert:
  - beads-command-update! was called (verify via post-command state or mock)
  - OR: Re-fetch issue and verify title changed
  - (kill show buffer in cleanup)
```

---

## Scenario 30: beads-show-compose-edit (E) and beads-show-compose-comment (N)

**Test**: `beads-live-test-show-compose-edit-opens-buffer`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue, open show buffer

Execute:
  - Call (beads-show-compose-edit)

Assert:
  - A *beads-compose* buffer is created
  - Buffer has beads-compose-mode active (or derived mode)
  - (C-c C-k to cancel, kill buffer in cleanup)
```

**Test**: `beads-live-test-show-compose-comment-opens-buffer`
**Tags**: `:live :integration`

Same as above but call `(beads-show-compose-comment)`.

---

## Scenario 31: beads-refresh-show (g from show buffer)

**Test**: `beads-live-test-show-refresh`
**Tags**: `:live :integration`

```
Setup:
  - Seed 1 issue, open show buffer
  - Update the issue externally: (beads-command-update! ... :title "New title")

Execute:
  - Call (beads-refresh-show)

Assert:
  - Show buffer content now includes "New title"
  - (kill show buffer in cleanup)
```

---

## Scenario 32: Actions from Show Buffer (s/d/C/#)

These action commands (`beads-actions-*`) are already exercised from the
command-class level in Scenarios 5, 12, etc.  Add smoke tests specifically
verifying they work when called with point in a show buffer:

**Tests** (tags `:live :integration`):

- `beads-live-test-show-actions-close-from-show` — `d` closes issue
- `beads-live-test-show-actions-claim-from-show` — `C` claims issue
- `beads-live-test-show-actions-set-status-from-show` — `s` sets status
- `beads-live-test-show-actions-set-priority-from-show` — `#` sets priority

Pattern for each:

```
Setup:
  - Seed 1 issue, open show buffer for it

Execute:
  - Mock reading function as needed
  - Call (beads-actions-close) / etc.

Assert:
  - Issue status/assignee/priority updated (verify via beads-command-show!)
  - (kill show buffer in cleanup)
```

---

## Test File Location

All new tests go into the existing `lisp/test/beads-live-ui-test.el`.

Add a new section heading:

```elisp
;;; ============================================================
;;; Scenario 11-32: List and Show Buffer Commands (Agent + Actions)
;;; ============================================================
```

---

## Helper Additions to beads-live-ui-test.el

Add these helpers to the infrastructure section:

```elisp
(defmacro beads-live-test--with-mock-agent (&rest body)
  "Run BODY with mock agent backend active, reset on exit."
  (declare (indent 0))
  `(progn
     (require 'beads-agent-mock)
     (beads-agent-mock-register)
     (beads-agent-mock-reset)
     (unwind-protect
         (progn ,@body)
       (beads-agent-mock-reset)
       (beads-agent-mock-unregister))))

(defun beads-live-test--open-list-buffer ()
  "Kill stale list buffers and open a fresh one.
Returns the live `beads-list-mode' buffer."
  (dolist (b (buffer-list))
    (when (with-current-buffer b (derived-mode-p 'beads-list-mode))
      (kill-buffer b)))
  (beads-list-all)
  (cl-find-if (lambda (b) (with-current-buffer b (derived-mode-p 'beads-list-mode)))
              (buffer-list)))

(defun beads-live-test--open-show-buffer (issue-id)
  "Open a show buffer for ISSUE-ID and return it."
  (beads-show issue-id)
  (cl-find-if (lambda (b) (string-match-p (regexp-quote issue-id) (buffer-name b)))
              (buffer-list)))
```

---

## Skip Guards

All agent tests require:
```elisp
(skip-unless (executable-find beads-executable))
(skip-unless (featurep 'beads-agent-mock))
```

Transient tests additionally require:
```elisp
(skip-unless (beads-live-test--interactive-p))
```

---

## Coverage Summary

| Scenario | Commands Covered | Tags |
|----------|-----------------|------|
| 11 | `a a` (list buffer) | :live :integration |
| 12 | `a t` `a r` `a p` `a q` (list buffer) | :live :integration |
| 13 | `a x` (list buffer) | :live :integration |
| 14 | `a j` (list buffer) | :live :integration |
| 15 | `a a` `a x` `a j` (show buffer) | :live :integration |
| 16 | `n` `p` (list buffer) | :live :integration |
| 17 | `RET` (list buffer) | :live :integration |
| 18 | `g` (list buffer) | :live :integration |
| 19 | `m` `u` `U` `* !` `* *` (list buffer) | :live :integration |
| 20 | `w` `C-w` (list buffer) | :live :integration |
| 21 | `C-c C-f` (list buffer follow-mode) | :live :integration |
| 22 | `B s` `B c` `B o` (list buffer bulk) | :live :integration |
| 23 | `l` (list filter transient) | :live :transient |
| 24 | `n` `p` (show buffer section nav) | :live :integration |
| 25 | `w` `C-w` (show buffer) | :live :integration |
| 26 | `[` `]` `M-n` `M-p` (show buffer references) | :live :integration |
| 27 | `RET` `C-c C-o` (show buffer follow-ref) | :live :integration |
| 28 | `?` (show-actions from show buffer) | :live :transient |
| 29 | `e` `C-c C-e` (show buffer edit-field) | :live :integration |
| 30 | `E` `N` (show buffer compose) | :live :integration |
| 31 | `g` (show buffer refresh) | :live :integration |
| 32 | `d` `C` `s` `#` (show buffer actions) | :live :integration |

**Deferred** (no live test required):
- `S` (beads-list-sort) — opens `beads-list-sort` transient; smoke-test it
  as part of Scenario 10's P3 menu sweep instead
- `]` `[` `G` (pager) — depend on `beads-pager` which is separately tested;
  add pager smoke test to Scenario 10 sweep if needed
- `D` (beads-list-delete) — destructive and irreversible; covered by
  `beads-command-admin-test.el` unit tests
- `a c` (start-custom) — requires a custom prompt string; defer to separate
  custom-agent scenario if needed
- Outline navigation (`C-c C-n/p/f/b/u`) — navigation aids, no data change;
  smoke-test via `beads-show-next-section` in Scenario 24
- `o` (beads-show-follow-reference-other-window) — variant of Scenario 27
  opening in other window; add as a secondary assertion in that test

---

## Implementation Notes for Polecat

1. **Agent tests are the highest priority** (Scenarios 11-15).  They test
   genuinely new behavior not covered by existing tests.

2. **Use `cl-letf` to mock** `beads-agent--should-use-worktree-p` returning
   `nil` in tests to avoid worktree setup prompts during agent start.

3. **List buffer tests** (Scenarios 16-23) should share the `--with-list-buffer`
   helper to reduce boilerplate.

4. **Show buffer tests** (Scenarios 24-32) are transient/interactive context;
   many require `:live :integration` not just `:transient`.

5. **Check `beads-list--marked-ids`** (or the appropriate internal variable)
   for mark assertions; do not rely on buffer text color alone.

6. **Verify beads-agent mock is available** before attempting agent tests;
   `(featurep 'beads-agent-mock)` is the guard.
