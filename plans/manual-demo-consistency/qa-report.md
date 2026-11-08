# Manual-only black-box QA pass — beads.el manual and screenshots

- **Work item:** W7 (`be-7japy`), root workflow `be-0kjzs`, drain unit convoy
  `be-l3nuz` (parent convoy `be-o4er1`, drain control `be-9sbk2`)
- **Workflow step:** `be-sqrl5` (`do-work`), formula `do-work`
- **Date:** 2026-11-08
- **Method**: run the committed fixture, open a fresh `emacs -nw -Q`, load only
  what the Info/HTML manual says, and drive the documented menus and keys.  The
  implementation sources, tests, and the agent's prior knowledge of the code
  were **not** used to decide how to operate the UI; `M-x`/`describe-key`/
  completion help and the running buffers were the only other sources.

## Environment

| Item | Value |
| --- | --- |
| Emacs | GNU Emacs 31.1 (`emacs --version`) |
| Session | `tmux` session `beads-manual-qa-7japy`, Emacs server `beads-manual-qa-7japy`, `emacs -nw -Q` |
| Setup | documented source install: `add-to-list 'load-path ".../lisp"`, load `lisp/beads-autoloads.el`, `(require 'beads)`; `BEADS_ACTOR=demo` |
| GNU Guix | `guix` channel `f546638fed68114b67e5bf1c681c5c62876a4018`; `asahi` channel `b9132769b361431e999f7d166d715ef4c6e101d2` |
| `bd` | 1.3.2 (dev) |
| Supporting tools | Dolt 2.4.2, Graphviz 7.0.1, sway 1.12, ImageMagick 6.9.13-5, tmux 3.8, Python 3.12.12, git 2.55.0 |
| TMPDIR | `/tmp/beads-manual-qa-7japy` |
| Fixture | `/tmp/beads-manual-qa-7japy/demo-project` (the documented `${TMPDIR:-/tmp}/demo-project` path with a dedicated `TMPDIR`) |
| Dataset | `docs/manual/screenshots/fixtures/beads-fixture.jsonl`, SHA-256 `6504c508bd25f3a2efc8a0e99bc940767b3306ae41352077fa446d2d6c319ca8` |
| Checkout | `/home/roman/workspace/beads.el/worktrees/be-7japy` at `1022ffe5` plus this pass |

The default shared `/tmp/demo-project` was concurrently owned by the sibling
`gascity.el` fixture (its `.gascity-fixture-project` marker), so the pass used
a dedicated `TMPDIR` as the launch instructions allow.  The basename is still
`demo-project`, so every documented buffer name and mode-line string matches.
No live/agent store, `~/emacs-city`, `~/bright-lights`, repository `.beads`
store, or other Emacs session was opened.  The four rerouting variables
(`BEADS_DIR`, `BEADS_DOLT_PORT`, `BEADS_DOLT_SERVER_PORT`, `GC_DOLT_PORT`)
were unset for every `bd` and Emacs invocation.

## Result

After the fixes below, every documented section could be exercised from the
manual alone.  Two passes were made: the first with the manual as committed,
the second after the manual and screenshot fixes.  The second pass re-ran every
flow that had failed or changed and found no remaining manual-sufficiency
failure.

## Section-by-section pass

| Section (manual node) | Tried | Observed | Manual sufficient? |
| --- | --- | --- | --- |
| Install/load (`Installing from Source`) | `emacs -nw -Q`, documented `load-path` + `require`, then `M-x beads` | Board opens as `*beads-dashboard<demo-project>*`; first pass failed later in the epic menu because lazily autoloaded suffixes were absent | After fix: yes. Added the generated-autoloads step to the install section |
| Board (`The Board`, `Board Sections and Folding`) | `M-x beads`; `SPC`; `+`; `-`; `*`; `g`; reopened the board | Stats/Recently Closed/In-progress/Ready/Blocked sections; `SPC` folds a heading (`▾`→`▸`); `+` reveals the hidden Ready rows, `-` hides them again, `*` loads all rows of the section; fold state restored after reopening | After fix: yes. The old text claimed `+`/`-` expand/collapse and `*` toggles everything; corrected to the actual row-count keys |
| List (`Listing Issues`, `List the Issues`) | `? l`; `TAB`/`S-TAB`; `SPC`; `/`; `/ s closed RET`; `/ R`; `S`; `]`/`[`/`G`; `g` | `? l` lists all immediately in `*beads-list[demo-project]*` (ID Type Status Priority Agents Title, grouped by status); `/` opens the spec filter menu (`s` status, `t` type, `p` priority, `a` assignee, `l` label, `r` ready, `A` all, `n` limit, `o` order; `RET` apply, `R` reset); status=closed filtered to demo-6; reset restored all; pager mode line shows page/range | After fix: yes. Corrected the dispatch/`beads-list-issues` description and documented the `/` menu |
| Sorting/pagination/movement | `S`, completion, `TAB`/`S-TAB`, `SPC`, `]`, `[`, `G` | Grouped list prompts `Sort by:` with ID/Type/Status/Priority/Agents/Title/Created/Updated (default = current column, same column toggles direction); pager works | After fix: yes. Old text said "sort by the column under point" only |
| Ready/blocked (`Ready and Blocked Lists`) | `? r`, `? b`; after closing demo-3 and opening demo-4 | `*beads-ready[demo-project]*` and `*beads-blocked[demo-project]*`; demo-4 moved from blocked to ready, demo-5/demo-9 remained in the blocked list | Yes |
| Show (`Show an Issue`, `Viewing Issues`) | `RET` on demo-3; `TAB`/`S-TAB`; `SPC`; `g`; `n`/`p`; `q`; `M-x beads-show` → demo-4 | `*beads-show[demo-project]/demo-3 Fix crash when opening an e...*` in `beads-show-mode`; header `In Progress  P0  Bug  demo`; DESCRIPTION/LABELS/BLOCKS/COMMENTS sections; fold, refresh, quit work; the title is appended to the buffer name | After fix: yes. The old buffer name omitted the title; reference already documented it |
| Create (`Create an Issue`, `Creating Issues`) | `? c`; title; blank line + description; `C-c C-c`; `C-c C-k` | `*beads-create[demo-project]*`; echo `Created issue demo-anv: Walkthrough: document the shared demo`; the new row appeared in the list; cancel also works | Yes (id format clarified) |
| Claim/update (`Triage demo-3`) | `C`; `#` → `1 - High`; `s` → `in_progress`; `g` | Claim echoed `Refreshed demo-3`; priority completion `0 - Critical` … `4 - Backlog`; status completion `blocked`/`in_progress`/`open`; header updated to `In Progress  P1` | Yes |
| Status/priority prompts | TAB completion in each prompt | exact prompts and choices match the manual | Yes |
| Close/unblock (`Unblock demo-4`, `Closing and Reopening`) | `d` → `blocker resolved`; `M-x beads-show` → demo-4; `s` → `open`; `o`/`RET` on demo-6; `d` | `Closed 1 issue(s)`, demo-4 show header `Open`, ready list contains demo-4, blocked list does not; reopen reports `Reopened 1 issue(s)` and `Close demo-6 reason:` reports `Closed 1 issue(s)` | Yes, with the refresh caveat below: the list refresh can regroup rows under point |
| Epic/parent-child (`Inspect the demo-10 Epic`, `Epics`) | `M-x beads-epic-menu`; `s`; `SPC`; `RET` | `*beads-epic-status[demo-project]*` shows `demo-10` and `Progress: 0/2 children closed (0%)`; `SPC` expands demo-12/demo-11; `RET` opens `*beads-show[demo-project]/demo-10 Epic: rewrite the user manual*` with demo-11/demo-12 under BLOCKS | After install fix: yes. Section label clarified |
| Labels (`Labels`) | `? L`; `L` | Menu `a/r/p/l/L` matches; list-all buffer `*beads-labels[demo-project]*` shows Label/Count rows | Yes |
| Comments (`Editing Issues`) | `N` in the show view; typed body; `C-c C-c`; `g` | `*beads-comment[demo-project]/demo-3*`; echo `Added comment to demo-3`; COMMENTS section appears after refresh | Yes |
| Dependencies (`Dependencies`) | `? d`; walkthrough's dependency/unblock step | Dependency menu `a/r/l/t/c/R/U` matches; demo-4's `DEPENDS ON`/blocked state and ready transition verified | Yes |
| Marks/bulk (`Marks and Bulk Operations`, `Bulk Actions`) | `m`; `u`; `*` `*`/`*` `!`; `U`; `B` | `>` mark indicator; unmark; mark-all; unmark-all; `B` shows the bulk prefix (`B s/p/c/o`) | Yes |
| Dispatch menu (`Transients`, `Dispatch and Maintenance Menus`) | `?` from board/list/show; `q` | Issues/Workflow/Views/Manage groups and Maintenance row match the generated table; `? !` opens maintenance | Yes |
| Maintenance menu (`Maintenance Groups`) | `? !`, `C-x l`, `C-x l`, `3`, `RET` | Level-1 groups shown; first `C-x l` shows each suffix's level; the next prompts `Set level for 'beads-maintenance':`; the level variable becomes 3, revealing the deeper groups in a tall frame | After fix: yes. Old text said `C-x l` "cycles" levels |
| Formulas (`Formulas`) | `? F`; `l` | `*beads-formula-list[demo-project]*`; `No formulas found` for the fixture (expected: no project formulas) | Yes |
| Molecules (`Molecules`) | `? m` | Molecule menu with Instantiate/Combine/Lifecycle/Cleanup/Find groups as documented | Yes |
| Agents (`Agent Commands`) | `? a`; `s` (launch transient) | Actual menu: `s` Start, `w` Sling to worktree, `S` Stop, `j` Jump, `p` Send prompt, `l` List sessions, `c` Cleanup, `B` Backend, `g` Refresh, `q` Quit; `s` opens Role (t/r/p), Target (w/T), Backend (b), Prompt (e/P), Actions | After fix: yes. The old `a t/r/p/q/c/x/j/a` table did not match the menu |
| Swarm (`Sling and Swarm`) | `? S` | `*beads-swarm-list[demo-project]*` opens with the Swarm/Epic/Coordinator/Progress/Active/Tasks header (empty for the fixture) | Yes |
| Remote/TRAMP (`remote projects` FAQ, `Orchestration and Terminal`) | `C-x d /ssh:localhost:/tmp/beads-manual-qa-7japy/demo-project/`; `M-x beads`; `? l`; `RET` on demo-3; comment | Remote dashboard renders the same store; remote list/show open in `*beads-*[/ssh:localhost:|demo-project]*`; buttons/async loads work and do not block | Yes for the documented "open from a TRAMP directory" path |
| Key index spot checks | list: `TAB`, `S-TAB`, `SPC`, `RET`, `C`, `d`, `o`, `s`, `#`, `m`, `u`, `U`, `B`, `S`, `]`, `[`, `G`, `g`, `/`, `?`; show: `TAB`, `S-TAB`, `SPC`, `RET`, `n`, `p`, `g`, `q`, `C`, `d`, `s`, `#`, `E`, `N`, `e`, `o`, `D`, `c` | Keys resolve to the documented commands; the list/show split for `E`/`N` and `o` is real (the old context-action table listed list bindings that do not exist) | After fix: yes |
| Screenshot regeneration (`docs/manual/screenshots/README.md`) | `--list-views`; `--scope show`; `--verify show`; `--verify-scope show`; `--scope all`; `--verify all` | 14 manifest views / 18 full images; subset capture, two-run byte-identity, non-selected isolation, full-run capture, and full verify all exit 0; one committed image (`maintenance-dark`/thumb) differed from a fresh all-run and was regenerated; a subset `show` capture legitimately differs by the history-dependent `↩ Back to …` row | Yes after the README note and image refresh |
| Key index/registry | `C-h k`, completion candidates, `describe-key` | Effective bindings and prompts match the generated reference; completion lists match the documented choices | Yes |

## Gaps found and fixes applied

The pass changed documentation and the stale screenshot image only; no
command, option, keymap, or runtime behavior changed.

1. **Source install needs the generated autoloads.** With only
   `add-to-list 'load-path` + `(require 'beads)`, `M-x beads-epic-menu` failed
   with `Suffix command beads-formula-distill is not defined or autoloaded`.
   `doc/installation.texi` now loads `lisp/beads-autoloads.el` after `make`
   (and states that a package install loads it automatically).  Re-verified:
   the epic menu opens and its status view/timeline work.
2. **Listing entry points were misdescribed.** `beads-list-issues` is a
   function, not a command; dispatch `l` runs `beads-list-all` immediately;
   the switch-based filter transient is `beads-list-filters`; `/` in a list
   opens `beads-list-filter-menu` with `s/t/p/a/l`, `r/A/n/o`, `RET`/`R`.
   `doc/issue-workflow.texi` now describes each accurately, including
   `beads-list-advanced`'s `-s`/`-P`/`-T`/`-a` keys.
3. **Sort key.** In a grouped list `S` prompts `Sort by:` for a column and
   toggles direction on the same column; it does not sort directly by the
   column under point (that is the flat-list path).  The manual now says so.
4. **Board row-count keys.** `SPC` folds; `+` loads more rows for the section,
   `-` hides rows, and `*` loads every row of the section.  The manual no
   longer claims `+`/`-` expand/collapse or that `*` toggles everything.
5. **Show buffer names include the title.** The documented example is now
   `*beads-show[demo-project]/demo-3 Fix crash when opening an e...*`; the
   buffer table and the walkthrough state the 30-character title suffix.
6. **List refresh can move the row under point.** After reopening `demo-6`,
   the refresh regroups rows; the follow-up `d` acted on the row that had moved
   under point (`demo-3`, already closed) rather than `demo-6` in the first
   pass.  The manual now tells the reader to re-select the `demo-6` row after
   the refresh.  The underlying "keep point on the same issue across a
   regrouping refresh" behavior is reported as residual work, not changed here.
7. **Maintenance levels are prompted, not cycled.** `C-x l` shows the suffix
   levels and then prompts `Set level for 'beads-maintenance':`; the level is
   entered explicitly.  The manual now describes the prompt and typing `3`.
8. **Agent command table was stale.** The actual menu is
   `s/w/S/j/p/l/c/B/g` with the Role/Target/Backend/Prompt/Actions launch
   transient (`t`/`r`/`p`, `w`/`T`, `b`, `e`/`P`, `s`/`x`/`q`).  The manual's
   old `a t`, `a r`, `a p`, `a q`, `a c`, `a x`, `a j`, `a a` table was
   replaced.
9. **Context-action table mixed list and show keys.** `E`/`N` are show-only
   (in the list, `E` is unbound and `N` moves to the next section); `o`
   reopens only in the list.  The table now states the split.
10. **Label menu command name.** The menu command is `beads-label-menu`;
    `beads-label` is the non-interactive function.  The manual now names the
    command and notes the function.
11. **Epic child section label.** The child list is rendered under `BLOCKS`
    (the parent-of edges), not a section called "parent-child"; the walkthrough
    says `BLOCKS`.
12. **Stale screenshot.** A fresh `--scope all` run reproduced every committed
    image except `maintenance-dark` (and its thumbnail), which differed by a
    small frame-geometry shift; the regenerated image is committed.  The
    README now records that the `↩ Back to …` row makes show/epic subset
    captures differ from the full-run image, so canonical images come from
    `--scope all`.

## Verification

```
make info            # PASS, zero Texinfo warnings
make html            # PASS, self-contained doc/beads.html
make check-manual    # PASS
```

Manual-focused ERT (plain Emacs, the documented test invocation):

```
emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'
# PASS 21/21
emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-reference-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'
# PASS 12/12
emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-walkthrough-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'
# PASS 6/6
```

Screenshot pipeline (dedicated `TMPDIR=/tmp/beads-qa-capture`):

```
capture.sh --list-views          # 14 views
capture.sh --scope show          # PASS, 2 full + 2 thumbnails
capture.sh --verify show         # PASS, byte-identical 2/2
capture.sh --verify-scope show   # PASS, 32 non-selected images unchanged
capture.sh --scope all           # PASS, 18 full + 18 thumbnails
capture.sh --verify all          # PASS, byte-identical 18/18
```

Manual-only acceptance used the fresh `emacs -nw -Q` described above.  The
documented sequences created a bead, claimed/updated demo-3, closed demo-3 and
opened demo-4, reopened/closed demo-6, added a comment, filtered and sorted the
list, marked/bulk-operated rows, walked the epic, opened the formula, molecule,
agent, swarm, label, dependency, dispatch, and maintenance menus, and opened
the TRAMP dashboard/list/show — all from the manual and `C-h` help.

## Residual ambiguity / open questions

- **Point identity across a regrouping refresh.** A list refresh after a
  status change can leave point on the same *line* while the row moves to a
  different section, so a following context action can hit the wrong issue.
  The manual now warns the reader; preserving the issue id across the refresh
  is a UI change outside this documentation pass.
- **`:directory` with a TRAMP name.** The documented
  "open from a TRAMP directory" path works (dashboard, list, show, menus over
  `/ssh:localhost:`), but calling `beads-dashboard :directory
  "/ssh:localhost:/…"` from a non-TRAMP buffer hung the first `emacs -nw -Q`
  session (>60 s, `emacsclient` unresponsive) in this pass.  The reference
  docstring says DIRECTORY may be a TRAMP name, so this is recorded as
  discovered work for the remote path rather than a manual-only instruction
  failure.  The QA pass was repeated after restarting, and the guided TRAMP
  flow passed.
- **Subset screenshot history.** A `--scope show` (or `epic`) run produces a
  different `↩ Back to …` row than a full manifest run because the back link
  records view history.  The README now documents this; `--verify` and
  `--verify-scope` still pass.  Making the shot thunks seed the history would
  remove the difference but would require re-cutting the show/epic images.
- **Agent start was not launched.** The menu, role/target/backend/prompt
  transient, and session list were opened, but no real backend process was
  started; the manual documents the surface, and launching a real agent would
  have touched the host terminal/worktree outside the fixture brief.
- **Formula/molecule data.** The fixture has no project formulas, so
  formula list reports `No formulas found`; the cook/pour flows were not
  exercised against a formula because the manual-only fixture does not provide
  one.  The menus and documented keys were verified.

## Requirements

| ID | Status |
| --- | --- |
| REQ-012 | covered — manual-only black-box pass recorded here |
| REQ-013 | covered — every documented section tried and gaps fixed |
