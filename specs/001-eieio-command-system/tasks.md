# Tasks: EIEIO Command System for Beads.el

**Input**: Design documents from `/specs/001-eieio-command-system/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/generic-methods.md, quickstart.md

**Tests**: Integration tests are explicitly required per FR-013 and User Story 6.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `lisp/` at repository root
- **Tests**: `lisp/test/` at repository root
- **Documentation**: `doc/` at repository root

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Verify existing infrastructure and prepare for new commands

- [x] T001 Verify existing `beads-command.el` has `data` slot in `lisp/beads-command.el` (DECISION: Keep `data` instead of renaming to `result` - slot is working and tested throughout codebase)
- [x] T002 Verify `beads-command-parse` populates `data` slot correctly in `lisp/beads-command.el`
- [ ] T003 [P] Verify `beads-meta-define-transient` macro generates correct autoload cookies in `lisp/beads-meta.el`
- [ ] T004 [P] Document the quickstart pattern for adding new commands (already in specs, verify accuracy)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T005 Ensure `beads-command-doctor.el` + `beads-doctor.el` serve as reference implementation (verify existing pattern)
- [ ] T006 [P] Verify terminal backend fallback chain in `beads-command.el` (vterm → eat → term → compilation)
- [ ] T007 [P] Verify `beads-option-global-section` exists for shared global options in `lisp/beads-option.el`
- [ ] T008 Ensure `bd` not found error shows installation instructions per clarification in `lisp/beads.el`
- [ ] T009 [P] Create integration test infrastructure macro `beads-test-with-temp-repo` in `lisp/test/beads-integration-test.el`

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Access Main Command Menu (Priority: P1) 🎯 MVP

**Goal**: Users can invoke `M-x beads` and see a transient menu mirroring `bd --help` structure

**Independent Test**: Invoke `M-x beads` and verify transient menu appears with correct command groups and subcommands

### Implementation for User Story 1

- [ ] T010 [US1] Audit `bd --help` output to list all top-level commands and groups
- [ ] T011 [US1] Update `lisp/beads-main.el` to include all command groups (Views, Issues, Sync, Setup, etc.)
- [ ] T012 [P] [US1] Add autoload for `beads-ready` in `lisp/beads-main.el` Views group
- [ ] T013 [P] [US1] Add autoload for `beads-blocked` in `lisp/beads-main.el` Views group
- [ ] T014 [P] [US1] Add autoload for `beads-stale` in `lisp/beads-main.el` Views group
- [ ] T015 [P] [US1] Add autoload for `beads-daemon` group in `lisp/beads-main.el`
- [ ] T016 [P] [US1] Add autoload for `beads-config` in `lisp/beads-main.el`
- [ ] T017 [P] [US1] Add autoload for `beads-hooks` in `lisp/beads-main.el`
- [ ] T018 [US1] Verify navigation from main menu to command groups opens sub-menus
- [ ] T019 [US1] Verify navigation from main menu to direct commands opens command transient

**Checkpoint**: User Story 1 complete - `M-x beads` shows full command menu

---

## Phase 4: User Story 2 - Execute a Command via Transient (Priority: P1)

**Goal**: Users can execute any command with full transient workflow (fill args, preview, execute, reset)

**Independent Test**: Invoke `M-x beads-create`, fill fields, preview, execute against test repo

### Implementation for User Story 2

- [ ] T020 [US2] Verify existing `beads-create.el` implements full workflow (preview, execute, reset)
- [ ] T021 [US2] Verify `beads-command-preview` shows exact command line in `lisp/beads-command.el`
- [ ] T022 [P] [US2] Implement `beads-command-ready` class in `lisp/beads-command-ready.el` (reference: quickstart.md)
- [ ] T023 [P] [US2] Create `beads-ready.el` transient using `beads-meta-define-transient` in `lisp/beads-ready.el`
- [ ] T024 [P] [US2] Implement `beads-command-blocked` class in `lisp/beads-command-blocked.el`
- [ ] T025 [P] [US2] Create `beads-blocked.el` transient in `lisp/beads-blocked.el`
- [ ] T026 [P] [US2] Implement `beads-command-stale` class in `lisp/beads-command-stale.el`
- [ ] T027 [P] [US2] Create `beads-stale.el` transient in `lisp/beads-stale.el`
- [ ] T028 [P] [US2] Implement `beads-command-count` class in `lisp/beads-command-count.el`
- [ ] T029 [P] [US2] Create `beads-count.el` transient in `lisp/beads-count.el`
- [ ] T030 [US2] Verify Reset action clears all fields in transient menus

**Checkpoint**: User Story 2 complete - full transient workflow works for all view commands

---

## Phase 5: User Story 3 - Auto-Generated Interactive Commands (Priority: P1)

**Goal**: Defining an EIEIO command class automatically creates `M-x beads-<name>` command

**Independent Test**: Define minimal command class, verify `M-x beads-<classname>` is available

### Implementation for User Story 3

- [ ] T031 [US3] Verify `beads-meta-define-transient` generates `;;;###autoload` cookie in `lisp/beads-meta.el`
- [ ] T032 [P] [US3] Implement `beads-command-daemon` abstract base class in `lisp/beads-command-daemon.el`
- [ ] T033 [P] [US3] Implement `beads-command-daemon-list` class in `lisp/beads-command-daemon.el`
- [ ] T034 [P] [US3] Implement `beads-command-daemon-start` class in `lisp/beads-command-daemon.el`
- [ ] T035 [P] [US3] Implement `beads-command-daemon-stop` class in `lisp/beads-command-daemon.el`
- [ ] T036 [P] [US3] Implement `beads-command-daemon-status` class in `lisp/beads-command-daemon.el`
- [ ] T037 [US3] Create `beads-daemon.el` with transients for all daemon commands in `lisp/beads-daemon.el`
- [ ] T038 [US3] Verify positional argument prompting works before transient opens
- [ ] T039 [US3] Verify positional arguments appear in transient and can be modified

**Checkpoint**: User Story 3 complete - auto-generated commands work correctly

---

## Phase 6: User Story 4 - Customize Command Rendering (Priority: P2)

**Goal**: Developers can override `beads-command-execute-interactive` for custom rendering

**Independent Test**: Run `beads-list` and verify output appears in tabulated-list buffer

### Implementation for User Story 4

- [ ] T040 [US4] Verify existing `beads-list.el` uses custom tabulated-list rendering
- [ ] T041 [P] [US4] Document custom rendering pattern in `specs/001-eieio-command-system/quickstart.md`
- [ ] T042 [P] [US4] Implement `beads-command-status` with custom buffer rendering in `lisp/beads-command-status.el`
- [ ] T043 [P] [US4] Create `beads-status.el` transient in `lisp/beads-status.el`
- [ ] T044 [P] [US4] Implement `beads-command-info` class in `lisp/beads-command-info.el`
- [ ] T045 [P] [US4] Create `beads-info.el` transient in `lisp/beads-info.el`
- [ ] T046 [US4] Verify default terminal rendering works for commands without custom rendering

**Checkpoint**: User Story 4 complete - custom and default rendering both work

---

## Phase 7: User Story 5 - View Documentation with Live Previews (Priority: P2)

**Goal**: Org-based Info manual contains exact text-based previews of transient menus

**Independent Test**: Open Info manual, compare transient preview to actual display

### Implementation for User Story 5

- [ ] T047 [US5] Create `doc/beads.org` with standard Info manual structure
- [ ] T048 [US5] Add Introduction and Installation sections to `doc/beads.org`
- [ ] T049 [P] [US5] Add Main Menu (`M-x beads`) section with transient preview to `doc/beads.org`
- [ ] T050 [P] [US5] Add Create Command section with transient preview to `doc/beads.org`
- [ ] T051 [P] [US5] Add List Command section with transient preview to `doc/beads.org`
- [ ] T052 [P] [US5] Add Ready/Blocked/Stale commands section to `doc/beads.org`
- [ ] T053 [P] [US5] Add Daemon Commands section to `doc/beads.org`
- [ ] T054 [US5] Create elisp function to generate transient preview text in `lisp/beads-doc.el`
- [ ] T055 [US5] Add Makefile target to compile `doc/beads.org` to Info format
- [ ] T056 [US5] Verify Info manual compiles without errors

**Checkpoint**: User Story 5 complete - Info manual with previews is available

---

## Phase 8: User Story 6 - Run Integration Tests (Priority: P2)

**Goal**: Integration tests execute against temporary beads/git repos without polluting working directory

**Independent Test**: Run integration test suite, verify working directory is clean after

### Tests for User Story 6

- [ ] T057 [P] [US6] Test `beads-test-with-temp-repo` creates and cleans up temp repo in `lisp/test/beads-integration-test.el`
- [ ] T058 [P] [US6] Test `beads-ready` command execution in `lisp/test/beads-ready-test.el`
- [ ] T059 [P] [US6] Test `beads-blocked` command execution in `lisp/test/beads-blocked-test.el`
- [ ] T060 [P] [US6] Test `beads-daemon-list` command execution in `lisp/test/beads-daemon-test.el`

### Implementation for User Story 6

- [ ] T061 [US6] Implement `beads-test-with-temp-repo` macro fully in `lisp/test/beads-integration-test.el`
- [ ] T062 [US6] Add integration test helper functions (create issue, setup test data) in `lisp/test/beads-integration-test.el`
- [ ] T063 [P] [US6] Write integration tests for `beads-create` in `lisp/test/beads-create-integration-test.el`
- [ ] T064 [P] [US6] Write integration tests for `beads-list` in `lisp/test/beads-list-integration-test.el`
- [ ] T065 [P] [US6] Write integration tests for `beads-show` in `lisp/test/beads-show-integration-test.el`
- [ ] T066 [US6] Verify `git status` shows no pollution after running integration tests

**Checkpoint**: User Story 6 complete - integration tests run cleanly

---

## Phase 9: User Story 7 - Test Commands in Non-Graphical Emacs (Priority: P3)

**Goal**: Commands work correctly in terminal Emacs running inside tmux

**Independent Test**: Launch Emacs in tmux, invoke commands, verify transients render

### Implementation for User Story 7

- [ ] T067 [US7] Create test script `scripts/test-in-tmux.sh` for running Emacs in tmux
- [ ] T068 [US7] Document tmux testing procedure in `CONTRIBUTING.md` or `doc/beads.org`
- [ ] T069 [US7] Verify `M-x beads` transient renders correctly in terminal mode
- [ ] T070 [US7] Verify command execution and output display in terminal mode
- [ ] T071 [US7] Test all terminal backends (compilation-mode fallback) in terminal Emacs

**Checkpoint**: User Story 7 complete - terminal/tmux compatibility verified

---

## Phase 10: Additional Command Coverage

**Purpose**: Implement remaining bd commands from inventory (not tied to specific user story)

### Working With Issues Commands

- [ ] T072 [P] Implement `beads-command-search` class in `lisp/beads-command-search.el`
- [ ] T073 [P] Create `beads-search.el` transient in `lisp/beads-search.el`
- [ ] T074 [P] Implement `beads-command-comments` class in `lisp/beads-command-comments.el`
- [ ] T075 [P] Create `beads-comments.el` transient in `lisp/beads-comments.el`
- [ ] T076 [P] Implement `beads-command-edit` class in `lisp/beads-command-edit.el`
- [ ] T077 [P] Create `beads-edit.el` transient in `lisp/beads-edit.el`
- [ ] T078 [P] Implement `beads-command-move` and `beads-command-refile` classes in `lisp/beads-command-move.el`
- [ ] T079 [P] Create `beads-move.el` transient in `lisp/beads-move.el`

### Setup & Configuration Commands

- [ ] T080 [P] Implement `beads-command-config` class in `lisp/beads-command-config.el`
- [ ] T081 [P] Create `beads-config.el` transient in `lisp/beads-config.el`
- [ ] T082 [P] Implement `beads-command-hooks` classes in `lisp/beads-command-hooks.el`
- [ ] T083 [P] Create `beads-hooks.el` transient in `lisp/beads-hooks.el`
- [ ] T084 [P] Implement `beads-command-version` class in `lisp/beads-command-version.el`

### Sync & Data Commands

- [ ] T085 [P] Verify existing `beads-command-sync` class in `lisp/beads-command-sync.el`
- [ ] T086 [P] Verify existing `beads-command-export` class in `lisp/beads-command-export.el`
- [ ] T087 [P] Verify existing `beads-command-import` class in `lisp/beads-command-import.el`

---

## Phase 11: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T088 Run full test suite with coverage: `CI=true BD_NO_DAEMON=1 guix shell -D -f guix.scm -- eldev -s -dtT test -U coverage/codecov.json`
- [ ] T089 Verify 80%+ code coverage for command execution paths (SC-004)
- [ ] T090 Run byte compilation: `guix shell -D -f guix.scm -- eldev -p -dtT compile`
- [ ] T091 Run linter: `guix shell -D -f guix.scm -- eldev -p -dtT lint`
- [ ] T092 [P] Update README.md with new command documentation
- [ ] T093 [P] Create CI workflow to verify transient previews match actual menus
- [ ] T094 Final verification: all commands accessible within 3 interactions from `M-x beads` (SC-002)
- [ ] T095 Final verification: all commands work with default behavior without specializers (SC-003)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-9)**: All depend on Foundational phase completion
  - US1 (P1) → US2 (P1) → US3 (P1) can proceed in parallel after Foundational
  - US4, US5, US6 (P2) can proceed in parallel after US1-3
  - US7 (P3) depends on basic commands working
- **Additional Coverage (Phase 10)**: Can proceed after US1-3 complete
- **Polish (Phase 11)**: Depends on all desired user stories being complete

### User Story Dependencies

| Story | Priority | Can Start After | Dependencies |
|-------|----------|-----------------|--------------|
| US1   | P1       | Foundational    | None         |
| US2   | P1       | Foundational    | None (parallel with US1) |
| US3   | P1       | Foundational    | None (parallel with US1, US2) |
| US4   | P2       | US1-3 complete  | Existing list rendering |
| US5   | P2       | US1-3 complete  | Commands to document |
| US6   | P2       | Foundational    | None (can parallel with US1-3) |
| US7   | P3       | US1-3 complete  | Commands to test |

### Parallel Opportunities

- **Phase 1**: T003, T004 can run in parallel
- **Phase 2**: T006, T007, T009 can run in parallel
- **Phase 3 (US1)**: T012-T017 can all run in parallel (different autoloads)
- **Phase 4 (US2)**: T022-T029 can all run in parallel (different command files)
- **Phase 5 (US3)**: T032-T036 can all run in parallel (same file, different classes)
- **Phase 6 (US4)**: T041-T045 can all run in parallel
- **Phase 7 (US5)**: T049-T053 can all run in parallel (different doc sections)
- **Phase 8 (US6)**: T057-T060 and T063-T065 can all run in parallel
- **Phase 10**: All tasks can run in parallel (different command files)
- **Phase 11**: T092-T093 can run in parallel

---

## Parallel Example: User Story 2

```bash
# Launch all command class implementations together:
Task: "Implement beads-command-ready class in lisp/beads-command-ready.el"
Task: "Implement beads-command-blocked class in lisp/beads-command-blocked.el"
Task: "Implement beads-command-stale class in lisp/beads-command-stale.el"
Task: "Implement beads-command-count class in lisp/beads-command-count.el"

# Then launch all transients together:
Task: "Create beads-ready.el transient in lisp/beads-ready.el"
Task: "Create beads-blocked.el transient in lisp/beads-blocked.el"
Task: "Create beads-stale.el transient in lisp/beads-stale.el"
Task: "Create beads-count.el transient in lisp/beads-count.el"
```

---

## Implementation Strategy

### MVP First (User Stories 1-3 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Main Menu)
4. Complete Phase 4: User Story 2 (Command Execution)
5. Complete Phase 5: User Story 3 (Auto-Generated Commands)
6. **STOP and VALIDATE**: Test all P1 stories independently
7. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add US1 (Main Menu) → Test independently → **MVP Demo!**
3. Add US2 (Execution) → Test independently → Users can run commands
4. Add US3 (Auto-Gen) → Test independently → Developers can add commands easily
5. Add US4 (Custom Rendering) → Enhanced UX for list commands
6. Add US5 (Documentation) → Users have Info manual
7. Add US6 (Integration Tests) → CI/CD confidence
8. Add US7 (Terminal Testing) → Server/headless support

### Suggested MVP Scope

**User Story 1 alone** provides immediate value:
- Users can discover all beads commands via `M-x beads`
- Navigation to existing commands works
- No new command implementations required
- Foundation for all future work

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Tests are included per FR-013 requirement
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Reference `beads-command-doctor.el` + `beads-doctor.el` as pattern for all new commands
- Use `quickstart.md` as step-by-step guide for implementing new commands
