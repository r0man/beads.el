---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-y3apn
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
    - path: beads/be-pdiac
      hash: bead:be-pdiac
      ids: [REQ-011, REQ-012]
    - path: plans/manual-standard/requirements.md
      hash: sha256:0ffcb4eb7f97f2aef5a4416e8810c3b4da0d24f2bcbccf5640fd00a9967202a7
    - path: plans/manual-standard/implementation-plan.md
      hash: sha256:ccfb1a822c2ef5fbe69da8764e4d210ada1271c20520b6817a60bb6f96225498
    - path: plans/manual-standard/decomposition.md
      hash: sha256:015346698dc65d8143b28c77dd4eb8137b445e8896cdbf5163269f443f49f178
    - path: doc/beads.texi
      hash: git:2925336e19f01f8893d7edd7d7fea533111b4345
  coverage:
    - id: REQ-011
      status: covered
    - id: REQ-012
      status: covered
---

# Implementation Summary — be-pdiac (WI-12: Fresh-`emacs -Q` walkthrough and gap fixes)

## Summary

WI-12 adds the documented fresh-`emacs -Q` walkthrough and fixes every gap it
surfaced (REQ-011, REQ-012).

Two artifacts carry the requirement:

* **`lisp/test/beads-manual-walkthrough-test.el`** — a new `:integration`
  ERT test that runs with only the setup the manual documents (the package
  loaded from `lisp/`, `bd` on `PATH`) in a throwaway embedded-Dolt store and
  drives the whole documented path: load → board → create/list/show/close an
  issue → add a dependency → cook and pour a formula → inspect the molecule →
  dispatch (`?`) menu → maintenance menu.  Each documented entry point is
  asserted to exist and the documented result to be reached.
* **`plans/manual-standard/walkthrough.md`** — the recorded interactive
  fresh-`emacs -Q` evidence: method, the observed step table, the gaps found,
  each gap's fix, and the explicit statement that "passes using only the
  manual" is proven by the `:integration` test plus this artifact rather than
  by a literal fresh-daemon manual run inside ERT.

The walkthrough surfaced six prose/key-binding gaps, all fixed in the manual
(REQ-012); no screenshot needed regenerating because every affected screen is
unchanged.

## Intended Behavior

A reader who follows only the manual, in a fresh `emacs -Q` with the
"Installing from Source" setup, can install/load beads.el, open the board,
create/list/show/close an issue, add a dependency, run a formula → molecule
flow, and open the dispatch and maintenance menus, and what the prose tells
them matches what actually happens.

## Changed Files

* `lisp/test/beads-manual-walkthrough-test.el` (new) — the `:integration`
  walkthrough test.
* `plans/manual-standard/walkthrough.md` (new) — interactive evidence and the
  gap/fix ledger.
* `doc/getting-started.texi` — board buffer name uses angle brackets
  (`*beads-dashboard<PROJECT>*`), the phantom dispatch "context group" is
  replaced by the `Maintenance` row and `beads-menu-providers`, and the
  maintenance menu is documented as `? !` / `M-x beads-maintenance`.
* `doc/issue-workflow.texi` — `?` in a show buffer opens the dispatch menu;
  `M-x beads-show-actions` is the stay-open per-issue menu.
* `doc/formulas.texi` — the epic menu is `M-x beads-epic-menu` (no dispatch
  key); the maintenance menu is `? !`; a worked formula → cook → pour →
  inspect example was added.
* `doc/orchestration.texi` — same dispatch/menu corrections as
  `getting-started.texi`.
* `NEWS.md` — Unreleased entry for the walkthrough and corrections.

No `lisp/` runtime file was touched (REQ-017); the only `lisp/` change is the
new test file.

## Verification

First verification command (the automated walkthrough, run before the summary
was written):

```
emacs --batch -Q -L lisp -L lisp/test \
  -l lisp/test/beads-manual-walkthrough-test.el \
  -f ert-run-tests-batch-and-exit
```

Observed: `Ran 1 tests, 1 results as expected, 0 unexpected` — the walkthrough
passes start to finish.

Final proof commands (after the doc and summary edits):

```
makeinfo doc/beads.texi
scripts/dispatch-docs --check
GC_BEAD_ID=be-7760a .gc/scripts/checks/build-artifact-valid.sh
```

Observed: `makeinfo` exits 0 with no warnings; `scripts/dispatch-docs --check`
prints `dispatch docs ok`; the artifact validator prints
`build artifact valid: schema=gc.build.implementation-summary.v1` for this
summary.

### Requirement coverage

| ID | Status |
| --- | --- |
| REQ-011 | covered |
| REQ-012 | covered |

## Remaining Risks

* The manual's show-view key table calls `q` "Quit" while the mode line calls
  it "bury" (`quit-window`); both describe the same window-closing action, so
  it was left unchanged and recorded in `walkthrough.md`.
* The formula and molecule menus carry a few extra verbs (`S` schema index;
  molecule `o`/`a`/`=`) that the short task tables omit.  Omission, not error;
  the full list is in the Command and Variable Reference.
* The epic menu remains unbound in the dispatch menu.  Correcting this in the
  menu (`lisp/`) is forbidden by REQ-017, so the manual now documents the
  truthful `M-x beads-epic-menu` entry point instead.
* The interactive evidence was captured in a local tmux `emacs -Q` against a
  throwaway store, not over TRAMP; the manual's build/test surface is local
  and TRAMP behavior is covered by the existing remote-store tests.
