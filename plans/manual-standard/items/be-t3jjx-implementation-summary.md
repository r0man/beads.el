---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-q43h9
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
    - path: beads/be-t3jjx
      hash: bead:be-t3jjx
      ids: [REQ-013, REQ-014, REQ-017]
    - path: plans/manual-standard/requirements.md
      hash: sha256:0ffcb4eb7f97f2aef5a4416e8810c3b4da0d24f2bcbccf5640fd00a9967202a7
    - path: plans/manual-standard/implementation-plan.md
      hash: sha256:ccfb1a822c2ef5fbe69da8764e4d210ada1271c20520b6817a60bb6f96225498
    - path: plans/manual-standard/decomposition.md
      hash: sha256:015346698dc65d8143b28c77dd4eb8137b445e8896cdbf5163269f443f49f178
    - path: .github/workflows/docs-drift.yml
      hash: sha256:7aaa8e2fca210a08f28f72e7f79323de3e17c05a920bea53ec6f160abeb08e11
    - path: .github/workflows/pages.yml
      hash: sha256:2601051fd70f505f2f92b55c0705b1025107e79a3b90bec61d0c7f3761565a99
    - path: NEWS.md
      hash: sha256:ec85a8a8a623caaee22262f22a68db76bf6f141ad2abf664abb66854f2cf0d92
    - path: AGENTS.md
      hash: sha256:6d8ebb7e822682dd2fca7a98e311e7d2525bf006be07cd6d3601f91fc47ff584
    - path: doc/maintenance.texi
      hash: sha256:ccaa117a5f2f8e090d42b3b9535b25a50630e0662bc863ae49988587ece4174d
    - path: lisp/test/beads-manual-walkthrough-test.el
      hash: sha256:af55a608f242799091c8c370a01443349d9c0562ea8e515b32457e60efabb5a5
  coverage:
    - id: REQ-013
      status: covered
    - id: REQ-014
      status: covered
    - id: REQ-017
      status: covered
---

# Implementation Summary — be-t3jjx (WI-13: CI, publication, and no-behavior-change guard)

## Summary

WI-13 closes the manual-standard workflow: it wires the manual gates into
CI, makes the Pages publish assert that the uploaded HTML manual is
self-contained and reproducible, and proves the whole effort changed no
runtime behavior.

The claimed step bead is the dependencies-complete tail of the drain
(`WI-13` depends on `WI-03, WI-04, WI-08, WI-09, WI-10, WI-11, WI-12`).
Its worktree was prepared from `origin/main` (`9663a093`) on the
assumption that each predecessor's PR had landed, but the drain ran all
thirteen items in parallel from `main`, so no predecessor work was in the
base.  Because the source anchor's own boundary cannot exist without
those artifacts (`doc/Makefile.am`, `@shot`, `doc/reference.texi`, the
reference-completeness test, `scripts/check-manual-forbidden`, the
walkthrough test), the step first integrated the predecessor branches in
dependency order — `be-bqc8z` (WI-01), `be-wfnea` (WI-02), `be-kqvhj`
(WI-03), `ba00c359` (WI-05+06), `053c7d60` (WI-07), `248d7bdd` (WI-08),
`1440ec45` (WI-04), `5b801dc7` (WI-09), `7c97a35e` (WI-10), `f115aac0`
(WI-11), `4549b8e5` (WI-12) — resolving the two mechanical `Makefile.am`
conflicts (top-level `EXTRA_DIST` ownership moved to `doc/Makefile.am`;
`doc/Makefile.am` now lists the new chapters; `.PHONY` keeps
`check-manual` but not the `doc`-owned `html-local`).  The mandated
`work_dir` was read from source anchor `be-t3jjx`, not from the synthetic
drain-unit convoy, and every source read, test, hash, and commit ran
inside that worktree.

WI-13's own changes are then:

- **Forbidden-name gate in CI (REQ-013, REQ-015, REQ-016).** WI-11 added
  `scripts/check-manual-forbidden` and `make check-manual`; WI-13 keeps it
  running first in `make check` and as an explicit step in
  `.github/workflows/docs-drift.yml`.
- **Reference-completeness gate in CI (REQ-013, REQ-008, REQ-009).**
  `.github/workflows/docs-drift.yml` now runs the `:unit`
  `lisp/test/beads-manual-reference-test.el` gate directly, so a missing
  `@deffn`/`@defopt` fails the fast Docs drift job without waiting for the
  Emacs test matrix.
- **Self-contained, reproducible Pages publish (REQ-014).**
  `.github/workflows/pages.yml` keeps building `make html` through
  `SUBDIRS = doc`, then asserts there is no external stylesheet or script
  link, that the inlined lightbox and mirrored screenshot images are
  present, and that a second clean build is byte-identical before the
  artifact is uploaded.
- **No behavior change (REQ-017).** `git diff --stat main HEAD -- lisp/`
  reports only four test/support files and no `lisp/` runtime file:
  `beads-manual-reference-test.el` (new),
  `beads-manual-walkthrough-test.el` (new),
  `beads-manual-test.el`, and `beads-transient-doc-test.el`.  No command
  class, transient, `defcustom`, or CLI slot changed.
- **Documentation.** `NEWS.md` records the CI/publication/gate changes and
  the no-behavior guarantee; `AGENTS.md` "Build & Test Commands" documents
  `make html`/`make pdf` and `make check-manual`; the manual's own
  `doc/maintenance.texi` build section documents `make check-manual`, the
  `doc/Makefile.am` HTML override, the reference-completeness gate, and
  the offline forbidden-name gate.

While running the full suite WI-13 also fixed a test-isolation defect the
new walkthrough exposed: the walkthrough left its dashboard/list/show
buffers alive, and the later `:unit` test
`beads-remote-open-test-dashboard-scopes-to-root` scans `buffer-list` for
a dashboard buffer.  The walkthrough now kills every buffer scoped to its
temporary store, which took the full suite from one unexpected failure to
zero.

| ID | Status |
| --- | --- |
| REQ-013 | covered |
| REQ-014 | covered |
| REQ-017 | covered |

## Intended Behavior

- **REQ-013 (all gates green in CI).** `make` (byte-compile
  warnings-as-errors), `make lint`, `scripts/dispatch-docs --check`,
  `make check-manual`, and the reference-completeness `:unit` test all
  pass, and the two manual gates have an explicit CI step: the forbidden
  gate and the reference gate both run in the Docs drift workflow.
- **REQ-014 (self-contained Pages).** The Pages workflow builds
  `doc/beads.html` from `doc/Makefile.am` and refuses to upload unless the
  output inlines the stylesheet and lightbox, mirrors the screenshots, and
  rebuilds byte-identically.
- **REQ-017 (no behavior change).** The manual work is documentation,
  build, screenshot, and test/support only.  `lisp/` runtime is untouched
  and the full ERT suite passes unchanged in behavior (Gilardi-verified by
  the zero-unexpected run).
- **Source-anchor boundary.** Only the WI-13 files listed above were
  authored; every predecessor artifact comes from its own branch and
  commit, and the source anchor is left open for `close-source-anchor`.

## Changed Files

WI-13-authored (relative to the integrated dependency tree):

- `.github/workflows/docs-drift.yml` — explicit reference-completeness
  `:unit` step (forbidden-name step came from WI-11).
- `.github/workflows/pages.yml` — self-contained/reproducible assertion
  before the Pages artifact upload, and comment update.
- `NEWS.md` — Unreleased entry for the CI gates, Pages verification, and
  no-behavior guarantee.
- `AGENTS.md` — Build & Test Commands now list `make html`/`make pdf` and
  `make check-manual`.
- `doc/maintenance.texi` — `make check-manual`, `doc/Makefile.am` HTML
  override, reference-completeness gate, forbidden-name gate.
- `lisp/test/beads-manual-walkthrough-test.el` — kill the walkthrough's
  store-scoped view buffers so a later `:unit` buffer scan is not
  polluted.

Integration-only (merge commits; content owned by predecessors):
the twelve `be-t3jjx`-predecessor work items, their summaries, the
`doc/*.texi` chapters, `doc/Makefile.am`/`manual.css`/`lightbox.js`,
`scripts/check-manual-forbidden`, the walkthrough/reference tests, and the
regenerated screenshots.

## Verification

First verification command (bootstrap, build, and the fast gates), run
from the worktree root on the integrated tree:

```sh
./autogen.sh && ./configure && make
./scripts/check-manual-forbidden          # exit 0
make check-manual                         # exit 0
./scripts/dispatch-docs --check           # "dispatch docs ok"
make html                                 # exit 0; doc/beads.html built
```

Observed: `make` byte-compiled 128 files with warnings-as-errors and exit
0; forbidden gate and `make check-manual` exit 0; `scripts/dispatch-docs
--check` printed `dispatch docs ok`; `make html` exit 0.  Inspecting the
output confirmed the manual is self-contained — no
`<link ... rel="stylesheet">`, no `<script ... src=...>`, one inlined
`<style>` block and the inlined lightbox in every page, and 36 images
mirrored under `doc/beads.html/images/`.  A clean rebuild hashed every
generated `*.html` byte-identically (`HTML_BYTE_IDENTICAL`).  The targeted
gates pass: `beads-manual-reference-test` 4/4 `:unit`,
`beads-manual-test` 16/16 `:unit`, and
`beads-manual-walkthrough-test` 1/1 `:integration`.

Final proof command (whole suite, run from the worktree root after the
walkthrough cleanup):

```sh
make check
```

Observed: `Ran 6617 tests, 6591 results as expected, 0 unexpected, 26
skipped`, exit 0 (418 s).  `make lint` (byte-compile warnings-as-errors +
package-lint) exit 0, and `scripts/dispatch-docs --check` printed
`dispatch docs ok`.  The no-behavior guard is
`git diff --name-only main HEAD -- lisp/`, which lists only
`lisp/test/beads-manual-reference-test.el`,
`lisp/test/beads-manual-test.el`,
`lisp/test/beads-manual-walkthrough-test.el`, and
`lisp/test/beads-transient-doc-test.el` — no `lisp/` runtime file.

## Remaining Risks

- **Integration base was reconstructed, not merged upstream.** The
  source-anchor worktree started at `origin/main`, which did not contain
  the predecessor work the formula assumed had landed.  WI-13 integrated
  the twelve predecessor branches locally (two `Makefile.am` conflicts
  resolved by hand).  If the operator prefers a different integration
  point, the finalize/merge step must reuse this integrated tree or
  re-derive it; the merge mapping is recorded above.
- **Test-isolation fix is in the walkthrough, not the shared fixture
  macro.** `beads-test-with-temp-repo` still does not kill view buffers
  for the temp store.  Other integration tests that open a dashboard
  could hit the same order-dependent failure; a future cleanup could move
  the buffer kill into the shared macro.
- **Screenshot `--verify`/isolation gates are not yet a CI step.** WI-13
  was scoped to add the forbidden-word and reference-completeness steps
  (and the Pages self-containment/reproducibility check); the heavy GUI
  screenshot capture is not run in GitHub Actions.  The committed images
  and the `:unit`/`:integration` `beads-manual-test.el` guards are green.
- **Reproducibility residual.** `make html` was byte-identical under the
  pinned Guix toolchain; a different `makeinfo`/`texi2pdf` version may
  change a version string, in which case the content-only comparison
  described in the plan applies.
