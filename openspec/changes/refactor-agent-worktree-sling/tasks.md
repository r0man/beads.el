# Tasks: Agent Worktree Sling Refactoring

**Epic:** bde-jf4g

## 1. EIEIO Command Classes for bd worktree → bde-td0h ✅

- [x] 1.1 Create `beads-command-worktree.el` module
- [x] 1.2 Implement `beads-command-worktree` base class (domain types only)
- [x] 1.3 Implement `beads-command-worktree-create` with --branch flag support
- [x] 1.4 Implement `beads-command-worktree-list` with JSON parsing
- [x] 1.5 Implement `beads-command-worktree-remove` with --force flag
- [x] 1.6 Implement `beads-command-worktree-info` with JSON parsing
- [x] 1.7 Add convenience functions: `beads-command-worktree-create!`, etc.
- [x] 1.8 Write tests for all worktree command classes

## 2. Worktree Completion Support → bde-h826

- [ ] 2.1 Add `beads-completion-worktree-table` function
- [ ] 2.2 Add annotation function showing branch and beads state
- [ ] 2.3 Add group function (by beads state: redirect/shared/none)
- [ ] 2.4 Add `beads-completion-read-worktree` helper
- [ ] 2.5 Write tests for worktree completion

## 3. Worktree Reader Functions → bde-oz7y

- [ ] 3.1 Add `beads-reader-worktree-name` (suggests issue IDs, branches)
- [ ] 3.2 Add `beads-reader-worktree-branch` (existing branches)
- [ ] 3.3 Add `beads-reader-worktree-existing` (from bd worktree list)
- [ ] 3.4 Write tests for reader functions

## 4. Worktree Transient Menu → bde-360v

- [ ] 4.1 Create `beads-worktree.el` module
- [ ] 4.2 Implement `beads-worktree-create` suffix with name/branch infixes
- [ ] 4.3 Implement `beads-worktree-list` suffix (tabulated-list display)
- [ ] 4.4 Implement `beads-worktree-remove` suffix with confirmation
- [ ] 4.5 Implement `beads-worktree-info` suffix
- [ ] 4.6 Define `beads-worktree` transient prefix
- [ ] 4.7 Add worktree entry to `beads-main` menu
- [ ] 4.8 Write tests for transient menu

## 5. Worktree List Mode → bde-j77g

- [ ] 5.1 Create `beads-worktree-list-mode` (tabulated-list-mode)
- [ ] 5.2 Add columns: Name, Path, Branch, Beads State
- [ ] 5.3 Add keybindings: RET (info), d (remove), g (refresh)
- [ ] 5.4 Write tests for list mode

## 6. Migrate Agent Worktree Creation → bde-o2lv

- [ ] 6.1 Modify `beads-git-create-worktree-async` to use bd command
- [ ] 6.2 Modify `beads-git-ensure-worktree-async` to use bd command
- [ ] 6.3 Update `beads-git-find-worktree-for-issue` to use bd worktree list
- [ ] 6.4 Deprecate direct git worktree functions (add warnings)
- [ ] 6.5 Add bd version check for worktree command availability
- [ ] 6.6 Add fallback to git commands for older bd versions
- [ ] 6.7 Update tests for migrated functions

## 7. Sling Workflow Implementation → bde-83mf

- [ ] 7.1 Add `beads-agent-sling` command
- [ ] 7.2 Implement worktree selection (create new vs existing)
- [ ] 7.3 Implement issue selection for sling
- [ ] 7.4 Integrate with existing agent start flow
- [ ] 7.5 Add `beads-agent-sling` to agent transient menu
- [ ] 7.6 Write tests for sling workflow

## 8. Gastown Backend (Optional) → bde-89aa

- [ ] 8.1 Create `beads-agent-gastown.el` module
- [ ] 8.2 Implement `beads-agent-backend-gastown` class
- [ ] 8.3 Implement `beads-agent-backend-available-p` (check gastown.el)
- [ ] 8.4 Implement `beads-agent-backend-start` (delegate to Gastown)
- [ ] 8.5 Implement `beads-agent-backend-stop` (delegate to Gastown)
- [ ] 8.6 Register backend with appropriate priority
- [ ] 8.7 Write tests with gastown.el mocked

## 9. Documentation → bde-5j33

- [ ] 9.1 Update README.md with worktree management section
- [ ] 9.2 Document sling workflow in README.md
- [ ] 9.3 Document Gastown integration in README.md
- [ ] 9.4 Add docstrings to all new public functions

## 10. Quality Assurance → bde-ks7h

- [ ] 10.1 Run full test suite
- [ ] 10.2 Run linter
- [ ] 10.3 Run byte compiler
- [ ] 10.4 Test in live Emacs session (create worktree, sling work)
- [ ] 10.5 Test with and without Gastown available

## Dependencies

- Task 2 depends on Task 1 (completion needs command output)
- Task 3 depends on Task 2 (readers use completion tables)
- Task 4 depends on Tasks 1, 2, 3 (menu uses commands and readers)
- Task 5 depends on Task 1 (list mode needs list command)
- Task 6 depends on Task 1 (migration uses new commands)
- Task 7 depends on Tasks 4, 6 (sling uses menu and migrated functions)
- Task 8 is independent (optional feature)
- Tasks 9, 10 depend on all other tasks

## Parallelizable Work

- Tasks 1, 8 can run in parallel (independent features)
- Tasks 2, 3 can run in parallel after Task 1
- Tasks 4, 5 can run in parallel after Task 1
