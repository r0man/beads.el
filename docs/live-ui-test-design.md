# Live Emacs UI Integration Test Design

**Bead**: be-mol-rdrg
**Status**: Design
**Implements**: `test/beads-live-ui-test.el`

---

## Goals

Verify that every command class, transient menu, and completing-read reader in
beads.el works correctly end-to-end in a live Emacs process against a real beads
database.  This catches bugs that unit tests (which mock `beads-command-execute`)
cannot: rendering, transient menu layout, reader integration, buffer display,
and interactive flows.

---

## Approach

### Driver Strategy: emacsclient over tmux keystrokes

Use `emacsclient -e` expressions to drive a headless `emacs --daemon` process,
**not** raw tmux `send-keys`.  Reasons:

- Reliable: no timing issues, no key encoding ambiguity
- Inspectable: expressions return values we can assert on
- Composable: can call any Emacs Lisp function directly
- Debuggable: errors propagate cleanly

The test file is an ERT test suite tagged `:live`.  Tests run via:

```bash
emacsclient -s beads-live -e '(ert-run-tests-interactively ":live")'
```

Or headlessly:

```bash
emacsclient -s beads-live -e '(ert-run-tests-batch-and-exit ":live")'
```

### Isolation: temp beads/git repo

Every test uses `beads-test-with-temp-repo-and-issues` (from
`beads-integration-test.el`) to get a clean, isolated database.  Tests MUST
NOT share state via global variables or the same database.

### Emacs server lifecycle

The test file provides helpers to start/stop the daemon:

```elisp
(beads-live-test--start-server)  ; emacs --daemon=beads-live
(beads-live-test--stop-server)   ; kill the daemon
```

The server loads beads.el from the worktree at startup.

---

## Command Class Inventory

There are 217 `beads-defcommand` definitions across 50 command modules.
Group them into 6 testing tiers:

### Tier 1 — Core CRUD (must cover)

| Command class | bd sub-command | Transient entry point |
|---------------|----------------|-----------------------|
| beads-command-create | bd create | beads-create |
| beads-command-list | bd list | beads-list |
| beads-command-show | bd show | beads-show-actions |
| beads-command-update | bd update | beads-update--menu |
| beads-command-close | bd close | beads-close |
| beads-command-reopen | bd reopen | beads (o key) |
| beads-command-delete | bd delete | (via beads-advanced-menu) |
| beads-command-search | bd search | beads (/ key) |
| beads-command-count | bd count | beads-more-menu |

### Tier 2 — Workflow (should cover)

| Command class | Purpose |
|---------------|---------|
| beads-command-dep-add | Add dependency |
| beads-command-dep-remove | Remove dependency |
| beads-command-dep-list | List dependencies |
| beads-command-dep-tree | Dependency tree |
| beads-command-blocked | Show blocked issues |
| beads-command-ready | Show ready issues |
| beads-command-defer / undefer | Defer/undefer |
| beads-command-edit | Edit field in-buffer |
| beads-command-state | Set state |

### Tier 3 — Views (should cover)

| Command class | Purpose |
|---------------|---------|
| beads-command-status | Status view |
| beads-command-graph | Dependency graph |
| beads-command-history | Change history |
| beads-command-diff | Diff view |
| beads-command-stats | Statistics |
| beads-command-epic-status | Epic progress |
| beads-command-stale | Stale issues |
| beads-command-branch | Branch listing |

### Tier 4 — Management (should cover)

| Command class | Purpose |
|---------------|---------|
| beads-command-label-* | Label add/remove |
| beads-command-formula-* | Formula list/show |
| beads-command-mol-* | Molecule operations |
| beads-command-config-* | Config get/set |
| beads-command-dolt-* | Dolt operations |
| beads-command-audit-* | Audit records |

### Tier 5 — Admin (nice to have)

beads-command-admin-cleanup, compact, reset, migrate, doctor, compact-analyze

### Tier 6 — Extensions (defer)

beads-agent, beads-swarm, beads-federation, beads-worktree-menu, integrations
(jira, github, linear, ado, gitlab, notion).  These require external services.

---

## Transient Menu Coverage

45+ transient prefixes exist.  Priority order:

### P1 — Test thoroughly

| Transient | Key path |
|-----------|----------|
| `beads` | M-x beads |
| `beads-create` | M-x beads → c |
| `beads-update--menu` | M-x beads → u |
| `beads-list` | M-x beads → l |
| `beads-show-actions` | in *beads-show* buffer → ? |
| `beads-dep` | M-x beads → d |
| `beads-dep-add--menu` | M-x beads-dep → a |
| `beads-dep-remove--menu` | M-x beads-dep → r |

### P2 — Smoke test (open and verify rendered)

`beads-more-menu`, `beads-state-menu`, `beads-label-menu`,
`beads-label-add`, `beads-label-remove`, `beads-formula-menu`,
`beads-mol`, `beads-dolt`, `beads-config`, `beads-edit--menu`,
`beads-compose-metadata`, `beads-list-advanced`, `beads-list-filter-menu`

### P3 — Open and verify no error

`beads-advanced-menu`, `beads-ops-menu`, `beads-admin`,
`beads-audit`, `beads-compact`, `beads-swarm`, `beads-agent`,
`beads-epic-menu`, `beads-worktree-menu`, `beads-sync`, `beads-vc`

### P4 — Defer (require external services)

`beads-jira`, `beads-linear`, `beads-github`, `beads-gitlab`,
`beads-ado`, `beads-notion`, `beads-federation`

---

## Completing-read Reader Coverage

65 reader functions in `beads-reader.el`.  Group by category:

### Category A — Issue selection readers (highest priority)

These gate most operations.  Test that they:
1. Return the at-point ID when point is on an issue
2. Fall back to `completing-read` prompt otherwise
3. Pass the correct value to the backend command

| Reader | Trigger |
|--------|---------|
| `beads--read-issue-at-point-or-prompt` | beads-close, beads-show-actions |
| `beads-reader-issue-id` | beads-dep-add, beads-dep-remove |
| `beads-reader-close-issue-id` | beads-close |
| `beads-reader-reopen-issue-id` | beads-reopen |
| `beads-reader-edit-issue-id` | beads-edit |

### Category B — Creation readers

Test the full create flow through `beads-compose-create`:

| Reader | Purpose |
|--------|---------|
| `beads-reader-issue-title` | Issue title |
| `beads-reader-issue-type` | Type (choices: bug/feature/task/epic/chore) |
| `beads-reader-issue-priority` | Priority (beads--priority-choices) |
| `beads-reader-issue-assignee` | Assignee string |
| `beads-reader-issue-labels` | Labels (completion from label list) |
| `beads-reader-create-custom-id` | Custom ID |
| `beads-reader-create-dependencies` | Dependencies string |
| `beads-reader-create-parent` | Parent issue ID |
| `beads-reader-create-from-template` | Template file |

### Category C — Update readers

Test through `beads-update--menu`:

| Reader | Choices / behavior |
|--------|-------------------|
| `beads-reader-update-status` | "open" "in_progress" "blocked" "closed" |
| `beads-reader-update-priority` | numeric 0-4 via beads--priority-choices |
| `beads-reader-update-type` | "bug" "feature" "task" "epic" "chore" |
| `beads-reader-update-title` | free string |
| `beads-reader-update-assignee` | free string |

### Category D — Dependency readers

Test through `beads-dep-add--menu` and `beads-dep-remove--menu`:

| Reader | Choices / behavior |
|--------|-------------------|
| `beads-reader-dep-add-issue-id` | issue completion |
| `beads-reader-dep-add-depends-on-id` | issue completion |
| `beads-reader-dep-add-type` | "blocks" "related" "parent-child" "discovered-from" |
| `beads-reader-dep-remove-issue-id` | issue completion |
| `beads-reader-dep-remove-depends-on-id` | issue completion |
| `beads-reader-dep-type` | dep type choices |

### Category E — List/filter readers

Test through `beads-list` and `beads-list-advanced`:

`beads-reader-list-status`, `beads-reader-list-type`,
`beads-reader-list-priority`, `beads-reader-list-label`,
`beads-reader-list-id`, `beads-reader-list-format`,
`beads-reader-list-limit`, `beads-reader-list-title-contains`

### Category F — Label readers

`beads-reader-label-issue-ids`, `beads-reader-label-name`

### Category G — Worktree readers

`beads-reader-worktree-name`, `beads-reader-worktree-branch`,
`beads-reader-worktree-existing`

### Category H — Agent readers

`beads-reader-agent-backend` — choices from `beads-agent-backend-types`

---

## Test Scenarios

### Scenario 1: Main menu renders

```elisp
(ert-deftest beads-live-test-main-menu-renders ()
  :tags '(:live :transient)
  (beads-live-test--in-server
    (beads-test-with-temp-repo (:init-beads t)
      (beads)
      (should (beads-live-test--transient-visible-p))
      (transient-quit-all))))
```

### Scenario 2: Create issue end-to-end

Open compose buffer, fill in title/type/priority, submit.  Verify
issue appears in list.

### Scenario 3: List with filter

Open `beads-list`, set `--status=open`, execute.  Verify buffer shows
only open issues.

### Scenario 4: Show issue and action menu

`beads-show` an issue, verify *beads-show* buffer content,
open `beads-show-actions` (?), verify menu renders.

### Scenario 5: Close issue

`beads-close` an open issue, provide reason, verify status
changes to closed.

### Scenario 6: Dependency add/remove

Add a "blocks" dependency, verify with `beads-dep-list`, remove it.

### Scenario 7: Update with each reader

For each update reader (status, priority, type, title, assignee):
open `beads-update--menu`, activate the infix, supply a value via
mocked `completing-read`, execute, verify the field updated.

### Scenario 8: Completing-read at-point detection

Place cursor on an issue line in a `*beads-list*` buffer, call
`beads-show`.  Verify it opens the correct issue without prompting.

### Scenario 9: Priority reader choices

Call `beads-reader-priority-level` with mocked `completing-read` that
selects "1 - High".  Verify return value is "1".

### Scenario 10: Smoke-test all P2/P3 transient menus

For each P2/P3 transient in the list above: call `(transient-setup
'menu-name)`, capture any errors, verify `(beads-live-test--transient-visible-p)`
returns t, then `(transient-quit-all)`.

---

## Test Infrastructure Functions

The test file must provide these helpers:

```elisp
;; Server lifecycle
(beads-live-test--start-server)   ; Start emacs --daemon=beads-live
(beads-live-test--stop-server)    ; Kill daemon

;; Evaluation
(beads-live-test--eval FORM)      ; emacsclient -s beads-live -e FORM

;; Assertions
(beads-live-test--transient-visible-p)  ; t if transient buffer is active
(beads-live-test--buffer-content NAME)  ; String content of buffer NAME
(beads-live-test--messages)             ; Content of *Messages* buffer

;; Data helpers
(beads-live-test--seed-issues REPO SPECS)  ; Create issues in repo
```

---

## Test Data Seed

Each test gets a temp repo seeded with:

```elisp
'(("Epic A"     :type epic    :priority 1 :status open)
  ("Bug B"      :type bug     :priority 0 :status open)
  ("Feature C"  :type feature :priority 2 :status in_progress)
  ("Task D"     :type task    :priority 3 :status blocked)
  ("Chore E"    :type chore   :priority 4 :status closed))
```

This covers all issue types, all priorities, and all common statuses.
Dependency tests add a "Bug B blocks Feature C" relationship.

---

## Scope for V1 Implementation

**Include:**
- All Tier 1 + Tier 2 command classes
- P1 + P2 transient menus
- Categories A, B, C, D readers
- Scenarios 1-10 above
- Bug filing template for failures

**Defer to V2:**
- Tier 5/6 command classes
- P3/P4 transient menus
- Categories E-H readers
- Agent/swarm/federation tests

---

## File Location

`test/beads-live-ui-test.el`

This file sits alongside the existing integration test infrastructure
and requires `beads-integration-test`.  Tests are tagged `:live` to
run separately from the main `eldev test` suite (which runs `:unit` and
`:integration` but not `:live`).

---

## Running the Tests

```bash
# Start dev Emacs server (do this once)
WORKTREE=/home/roman/gt/beads_el/polecats/furiosa/beads_el
emacs --daemon=beads-live -Q --eval \
  "(progn (add-to-list 'load-path \"$WORKTREE/lisp\") (require 'beads))"

# Run live tests
emacsclient -s beads-live -e \
  '(let ((ert-debug-on-error nil))
     (ert-run-tests-batch ":live"))'

# Stop server
emacsclient -s beads-live -e '(kill-emacs)'
```

Or via eldev with a custom target:

```bash
guix shell -D -f guix.scm -- eldev -p -dtT test --test-tags live
```
