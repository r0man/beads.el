---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-s3joe
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
    - path: beads/be-gn247
      hash: bead:be-gn247
      ids: [REQ-003, REQ-009, REQ-011]
    - path: plans/manual-demo-consistency/requirements.md
      hash: sha256:5a665190a49fa226ab49e221ddb10aa108f236b9a0e18e3b8079b8213ff3fd21
    - path: plans/manual-demo-consistency/implementation-plan.md
      hash: sha256:dd851b8ba3f345740b22622682f215a2467c16a38589fdb62698af98a6d10ecd
    - path: docs/manual/screenshots/fixtures/beads-fixture.jsonl
      hash: sha256:6504c508bd25f3a2efc8a0e99bc940767b3306ae41352077fa446d2d6c319ca8
    - path: doc/getting-started.texi
      hash: sha256:a552caa91e29391531e09f9b004f7fa2561c7f269b9b591af71dbfd67c238d00
    - path: doc/installation.texi
      hash: sha256:cc80016ebda29a11fe0b14554ca3f9a8855e2791629e1759d6f843bd979cf513
    - path: lisp/test/beads-manual-walkthrough-test.el
      hash: sha256:235631546c001d0598b0d4ebe9c4e166ec2b7a1d7225610d95508283df79fca3
  coverage:
    - id: REQ-003
      status: covered
    - id: REQ-009
      status: covered
    - id: REQ-011
      status: covered
---

# Implementation Summary: W6 — one shared `/tmp/demo-project` walkthrough

## Summary

Work item W6 (implementation plan WS-6, requirements REQ-003, REQ-009, and
REQ-011) makes the two manuals describe one reproducible demo scenario against
the shared `/tmp/demo-project` fixture, and gives beads.el the task-oriented
walkthrough the requirements ask for.

`doc/installation.texi` now carries the canonical fixture contract in **The
Shared Demo Project** (W1's section), extended with the twelve literal
`demo-1`…`demo-12` ids, the `demo-rig`/`/tmp/gascity-fixture` cross-repo
sentence, and the `BEADS_ACTOR=demo` ownership note.  W1's closing pointer now
points at the walkthrough instead of the old Issue Workflow prediction.

`doc/getting-started.texi` replaces "Your First Issue" with **The Shared
Walkthrough**: the four shared demo steps in the order REQ-003 fixes (triage
`demo-3`, unblock `demo-4`, close `demo-6`, inspect the `demo-10` epic),
expanded to beads.el's full arc (create, list, show, claim/update, dependency,
close, and maintenance).  Every step states the exact keys, the buffer that
opens, the echo-area/transient feedback and face, and the screenshot anchor
that illustrates it; `-dark` slots are the primary references and the `-light`
slots are secondary.  The fixture/setup path, hash, isolation rule, and
cross-repo contract are cross-referenced from the chapter intro rather than
duplicated.

`doc/installation.texi` gains **Tested with**: Emacs 31.1 (capture and QA),
the GNU Guix `guix` channel commit and `asahi` channel commit, `bd` 1.3.2
(dev), Dolt 2.4.2, Graphviz 7.0.1, sway 1.12, ImageMagick 6.9.13-5, the frozen
dataset and its SHA-256, and the separately cited CI matrix (29.4
`continue-on-error`, 30.2, 31.1, snapshot).

The `fig:` screenshot anchors are renamed to `fig-` (agents, formulas,
getting-started, issue-workflow, orchestration, views) so the new cross
references build with zero Texinfo warnings while keeping every `@shot` name
and image slot unchanged.

`lisp/test/beads-manual-walkthrough-test.el` is replaced.  Its `:unit` half is
a manual-consistency gate: the twelve ids, the path, the actual dataset hash,
the cross-repo contract, the ordered four steps, the required arc headings,
the exact keys, the eight screenshot references, and the "Tested with" values
must all be present.  Its `:integration` half runs the documented entry points
against a fixture created by `fixture.sh create` in a private `TMPDIR`
(basename `demo-project`, so every documented buffer name still matches),
sets `BEADS_ACTOR=demo` as the fixture-setup section documents, and asserts
the same ordered steps and results: board, compose create, list, show,
claim/status/priority, dependency and epic inspection, close/unblock/ready,
reopen/close from the list keys, and the dispatch/maintenance menus.

Worktree: `/home/roman/workspace/beads.el/worktrees/be-gn247` (detached on the
W1–W5 prerequisite chain).  Implementation commit: `b82765d9`.

## Intended Behavior

- **One demo project.** `fixture.sh create` prints the project path
  (`/tmp/demo-project`, or `$TMPDIR/demo-project`), seeds the frozen
  twelve-issue dataset, and the manual names the same path as gascity.el.
- **One scenario, two manuals.** Both manuals describe the same four steps in
  the same order with the same ids and vocabulary (`bead`, `in_progress`,
  `blocked`, `closed`, `epic`); beads.el expands the arc.
- **Reproducible script.** A reader in a fresh non-graphical Emacs can follow
  the keys and reach the documented buffer, echo, face, and result for create,
  list, show, claim/update, dependency, close, and maintenance.
- **Honest ownership.** The fixture's `demo` actor is explained, and the
  `BEADS_ACTOR=demo` launch is the documented way to satisfy bd's claim and
  close rules.
- **Accurate UI text.** Status labels are documented as rendered
  (`In Progress`/`Blocked` in the detail header, raw `in_progress` in list
  cells), and the immediate refresh message (`Refreshed demo-3`) is called out
  next to the action echo.
- **"Tested with" is evidence.** The section names the versions actually used
  for capture and the QA pass and cites the CI matrix separately.
- **No runtime change.** The only `lisp/` edit is the walkthrough test; no
  beads.el command, option, face, or keymap behavior changes.

## Changed Files

- `doc/getting-started.texi` — removes "Your First Issue"; adds The Shared
  Walkthrough with eight ordered subsections, exact keys/buffers/echoes/faces,
  and `-dark`/`-light` screenshot references.
- `doc/installation.texi` — adds the twelve ids and cross-repo/actor contract
  to The Shared Demo Project, fixes its walkthrough pointer, and adds the
  Tested with section and menu entry.
- `doc/beads.texi` — updates the top-level Getting Started menu description.
- `doc/agents.texi`, `doc/formulas.texi`, `doc/issue-workflow.texi`,
  `doc/orchestration.texi`, `doc/views.texi` — rename `fig:` anchors to `fig-`
  (anchor names only; `@shot` slots and images untouched).
- `lisp/test/beads-manual-walkthrough-test.el` — new manual-consistency unit
  gate and rewritten shared-fixture integration walkthrough.
- `plans/manual-demo-consistency/items/be-gn247-implementation-summary.md` —
  this artifact.

## Verification

First verification command, from the worktree root (focused unit + integration
walkthrough):

```
emacs --batch -Q -L lisp -L lisp/test \
  -l lisp/test/beads-manual-walkthrough-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'
```

Observed: PASS — 6/6 unit tests.  The `:integration` walkthrough alone
(`beads-manual-walkthrough-test-shared-fixture-walkthrough`) also PASSED
(11–14 s per run) against a fixture built by `fixture.sh create` with
`BEADS_ACTOR=demo`.

Final proof command, from the worktree root (all quality gates):

```
make check
```

Observed: PASS — `Ran 6706 tests, 6680 results as expected, 0 unexpected,
26 skipped`.  Also PASS: `make lint` (130 files byte-compiled, warnings as
errors), `make info` and `make html` (zero warnings), and
`make check-manual` (forbidden-host gate).

Manual-only acceptance in a fresh `emacs -nw -Q` inside tmux, following only
the documented keys against `/tmp/be-w6-accept/demo-project`:
board (`M-x beads`), dispatch (`?`), compose create (`c`, `C-c C-c`, echo
`Created issue demo-adn: …`), list (`l`), show (`RET` on demo-3), claim
(`C`), priority `1`, status `in_progress` (header `In Progress  P1`), close
demo-3 (`d`) and set demo-4 `open` (ready list starts with demo-4), the
`beads-epic-menu` status buffer (`*beads-epic-status[demo-project]*`,
`Progress: 0/2 children closed (0%)`, SPC shows demo-11/demo-12 in batch), and
`beads-maintenance` (Issue Lifecycle, Views & Reports, Issue Details, Events,
Workflow, Database groups).  All observed text matched the manual.

Coverage table:

| ID | Status |
| --- | --- |
| REQ-003 | covered |
| REQ-009 | covered |
| REQ-011 | covered |

## Remaining Risks

- **Merge-order coupling.** This branch is based on the W1–W5 prerequisite
  chain (shared fixture, default-face captions, regenerated images, PDF note,
  reference/key index).  If those land differently, the installer's one
  duplicate demo-project node and the Tested-with menu entry may need a trivial
  re-anchor; the branch already carries the chain, so the normal MQ order is
  conflict-free.
- **Concurrent sibling fixture.** The shared `/tmp/demo-project` can be owned
  by a concurrent gascity.el fixture (marker `.gascity-fixture-project`).
  beads.el's `fixture.sh` correctly refuses to delete it; the integration test
  therefore uses a private `TMPDIR` and never touches the shared path.
- **Pager depth.** The all-issues list pages by window size; the closed
  section is on a later page.  The manual says to press `]` (or filter) and the
  test pages with `beads-pager-next-page`, but a very short window can still
  hide `demo-6` until the reader pages.
- **`beads-actor` does not affect action commands.** The manual's pre-existing
  `beads-actor` user option is not applied by the sync execution path, so the
  walkthrough documents the working `BEADS_ACTOR=demo` environment instead.
  This is recorded as discovered work, not fixed here (docs-only scope).
- **Version drift.** The "Tested with" versions are the observed local capture/
  QA set; a later toolchain must regenerate them with the screenshots rather
  than leave stale numbers.
