---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-sqrl5
  formula: do-work
methodology:
  pack: gascity
  name: build-basic
producer:
  formula: do-work
  stage: implement
  attempt: 1
status: approved
trace:
  upstream:
    - path: beads/be-7japy
      hash: bead:be-7japy
      ids: [REQ-012, REQ-013]
    - path: plans/manual-demo-consistency/requirements.md
      hash: sha256:5a665190a49fa226ab49e221ddb10aa108f236b9a0e18e3b8079b8213ff3fd21
    - path: plans/manual-demo-consistency/implementation-plan.md
      hash: sha256:dd851b8ba3f345740b22622682f215a2467c16a38589fdb62698af98a6d10ecd
    - path: docs/manual/screenshots/fixtures/beads-fixture.jsonl
      hash: sha256:6504c508bd25f3a2efc8a0e99bc940767b3306ae41352077fa446d2d6c319ca8
    - path: doc/getting-started.texi
      hash: sha256:worktree-edit
    - path: plans/manual-demo-consistency/qa-report.md
      hash: sha256:worktree-new
  coverage:
    - id: REQ-012
      status: covered
    - id: REQ-013
      status: covered
---

# Implementation Summary: W7 — manual-only black-box QA pass

## Summary

Work item W7 (implementation plan WS-7; requirements REQ-012 and REQ-013) is
the manual-only black-box acceptance pass over the rebuilt manual and the
regenerated screenshot set.  The durable deliverable is
`plans/manual-demo-consistency/qa-report.md`, which records the environment,
the fixture path and dataset hash, the section-by-section
tried/observed/sufficient table, the gaps found and the fixes applied, and the
residual open questions.

The pass followed one fresh `emacs -nw -Q` in a uniquely named tmux session and
Emacs server (`beads-manual-qa-7japy`), loading only the documented source
setup and the fixture at `${TMPDIR}/demo-project`, with
`BEADS_ACTOR=demo`.  It used the built Info/HTML manual, `C-h` help, and the
running UI, never the implementation sources, to decide how to operate the UI.
A second pass re-ran every changed or previously failing flow.

Ten manual/screenshot defects were fixed (all documentation or image changes;
no runtime behavior changed):

- the source-install instructions now load the generated autoloads, which the
  lazily autoloaded epic-menu suffix needs;
- the list entry points are described accurately (`beads-list-all` is the
  dispatch `l`; `beads-list-issues` is a non-interactive function; `/` opens
  the spec filter menu; `beads-list-filters` and `beads-list-advanced` are the
  two M-x transients, with the real `-s`/`-t`/`-p` vs `-s`/`-P`/`-T` keys);
- `S` is documented as prompting for a sort column in grouped lists;
- the board's `SPC` fold versus `+`/`-`/`*` row-count keys are documented as
  implemented;
- show buffer names include the 30-character title suffix;
- the reopen-then-close walkthrough now re-selects the `demo-6` row because a
  regrouping refresh can move the row under point;
- maintenance levels are documented as an explicit `Set level` prompt;
- the agent command and launch tables match the real `s/w/S/j/p/l/c/B/g` menu
  and Role/Target/Backend/Prompt flow;
- the context-action table now separates list-only (`o`) from show-only
  (`E`, `N`) actions;
- the label menu command (`beads-label-menu`) and the epic child section label
  (`BLOCKS`) are corrected.

Two screenshot issues were handled: a fresh `--scope all` run reproduced every
committed image except `maintenance-dark` (and its thumbnail), which was
regenerated and committed, and the screenshot README now explains why the
history-dependent `↩ Back to …` row makes a subset show/epic capture differ
from the full-run image.

## Intended Behavior

- A reader with only the manual can install, load, and exercise every
  documented section in a fresh non-graphical Emacs.
- The shared `/tmp/demo-project` fixture and the recorded dataset hash produce
  the documented ids, states, buffer names, menus, keys, and echo-area
  feedback.
- The committed screenshots are reproducible from the current tree with
  `capture.sh --scope all` and two-run byte-identity (`--verify all`).
- The QA report is a standalone durable artifact, distinct from
  `review-report.md` and `factory-run.md`.

## Changed Files

- `plans/manual-demo-consistency/qa-report.md` — the durable QA record (new).
- `doc/installation.texi` — autoload step for the source install.
- `doc/getting-started.texi` — show buffer name, list columns, context-action
  split, close/reopen refresh note, epic child label, create-id note.
- `doc/issue-workflow.texi` — listing entry points and filter menus, sort
  prompt, label menu command.
- `doc/views.texi` — board fold and row-count keys.
- `doc/orchestration.texi` — maintenance level prompt.
- `doc/agents.texi` — agent command and launch tables.
- `docs/manual/screenshots/README.md` — subset/full-run view-history note.
- `docs/manual/images/maintenance-dark.png`,
  `docs/manual/images/maintenance-dark-thumb.png` — regenerated from the
  current tree.
- `plans/manual-demo-consistency/items/be-7japy-implementation-summary.md` —
  this summary.

Worktree: `/home/roman/workspace/beads.el/worktrees/be-7japy` (detached on the
W1–W6 prerequisite chain at `1022ffe5`).  Implementation commits are made on
this worktree.

## Verification

```
make info                              # PASS, zero warnings
make html                              # PASS, self-contained manual
make check-manual                      # PASS
emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'      # PASS 21/21
emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-reference-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'      # PASS 12/12
emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-walkthrough-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'      # PASS 6/6
capture.sh --list-views                # 14 views
capture.sh --scope show                # PASS
capture.sh --verify show               # PASS, byte-identical 2/2
capture.sh --verify-scope show         # PASS, 32 non-selected unchanged
capture.sh --scope all                 # PASS, 18 full + 18 thumbnails
capture.sh --verify all                # PASS, byte-identical 18/18
```

The manual-only acceptance evidence (buffer names, echo messages, menus, and
the TRAMP path) is in `qa-report.md`; the report records the two passes and
the residual issues.

Coverage table:

| ID | Status |
| --- | --- |
| REQ-012 | covered |
| REQ-013 | covered |

## Remaining Risks

- **Point identity across a regrouping list refresh.** The manual now warns
  the reader; preserving the issue identity across refresh is a UI change
  recorded as residual work, not made here.
- **TRAMP `:directory` argument.** The documented "open from a TRAMP
  directory" path passed (dashboard/list/show/menus over `/ssh:localhost:`),
  but `beads-dashboard :directory "/ssh:localhost:/…"` from a non-TRAMP buffer
  hung the -nw session in the first pass.  Recorded as discovered work.
- **Subset screenshot history.** Show/epic subset captures differ from the
  full-run image in the `↩ Back to …` row; the README documents why and the
  canonical images come from `--scope all`.
- **No real agent process was launched.** The menu/launch transient/session
  list were exercised; starting a real backend was outside the fixture brief.
- **Plan artifacts are untracked in the rig root.** The summary references
  their hashes in `/home/roman/workspace/beads.el/plans/…`, the same pattern
  the earlier W1–W6 summaries use.
