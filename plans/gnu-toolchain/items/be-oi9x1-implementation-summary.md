---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-fzk4o
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
    - path: beads/be-oi9x1
      hash: bead:be-oi9x1
      ids: [REQ-008, REQ-011]
    - path: plans/gnu-toolchain/requirements.md
      hash: sha256:e10a5d3dbcbb01f0571e768d3ec222bbbf4c3bfa3b98703b8411e1857c4fcaff
    - path: plans/gnu-toolchain/implementation-plan.md
      hash: sha256:6734ce93211d72bae8bb15520a052a84f31d66c97d260426075b7bfd098d0c01
    - path: plans/gnu-toolchain/decomposition.md
      hash: sha256:f27241bd58688fd3345a55de46292040b08c462532c875ed900e95d3c6f8f90f
    - path: lisp/beads-transient-doc.el
      hash: git:d1ff952aa5969dc45ed79d1e226a3e6ba5bacffe
    - path: lisp/beads-dispatch-doc.el
      hash: git:d1ff952aa5969dc45ed79d1e226a3e6ba5bacffe
    - path: lisp/test/beads-transient-doc-test.el
      hash: git:d1ff952aa5969dc45ed79d1e226a3e6ba5bacffe
  coverage:
    - id: REQ-008
      status: covered
    - id: REQ-011
      status: covered
---

# Implementation Summary — be-oi9x1 (W8: generated-doc and key-registry drift gates retargeted to Texinfo)

## Summary

W8 retargets the generated dispatch documentation drift gate from the
now-deleted Org manual to the Texinfo manual. It adds a Texinfo
`@multitable` renderer to the package-agnostic `beads-transient-doc`
engine and points `beads-dispatch-doc` at `doc/orchestration.texi`, so the
dispatch and maintenance tables embedded there are regenerated from the
transient menu code instead of hand-copied. `scripts/dispatch-docs --write`
now rewrites those Texinfo regions, and `scripts/dispatch-docs --check`
(and the `:unit` ERT test) fail when they drift.

The implementation commit is `d1ff952a` in the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-oi9x1`.

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-008 | covered |
| REQ-011 | covered |

## Intended Behavior

- **REQ-008 (quality gates preserved).** The generated `scripts/dispatch-docs`
  drift gate still runs: `beads-dispatch-doc-check` verifies the checked-in
  `docs/dispatch-reference.md` snapshot, the `README.md` generated region,
  and both generated regions in `doc/orchestration.texi`. The
  `lisp/test/beads-transient-doc-test.el` `:unit` tests exercise the diff
  logic without Eldev, and the renderer is byte-compiled warnings-as-errors
  clean.
- **REQ-011 (no stale generated content).** The manual's dispatch and
  maintenance tables are produced by the generator and gated, so they cannot
  drift from the transient definitions. The Org manual target
  (`docs/manual/orchestration.org`), deleted by W7, is no longer referenced;
  the generator now owns `doc/orchestration.texi`. `docs/key-registry.md`
  remains the enforced key contract via its unchanged test
  (`lisp/test/beads-key-registry-test.el`), which still passes.

Design notes:

- `beads-transient-doc-menu-texinfo` renders a suffix list as a Texinfo
  `@multitable` with Group, Key, Command, and Description columns. It is
  package-agnostic like the Markdown and Org renderers, so gascity.el can
  reuse it.
- The existing hand-written Texinfo tables were already byte-identical to
  the new renderer's output, so `scripts/dispatch-docs --write` is
  idempotent and no manual prose changed.
- `scripts/dispatch-docs` itself is untouched: W2 (`be-c8dt2`) already drops
  its Eldev branch and genericizes its comment, so editing it here would
  conflict at merge time for no functional gain.

## Changed Files

- `lisp/beads-transient-doc.el` (modified) — add
  `beads-transient-doc--texinfo-table` and
  `beads-transient-doc-menu-texinfo`; update the public entry-point list.
- `lisp/beads-dispatch-doc.el` (modified) — point
  `beads-dispatch-doc-manual-file` at `doc/orchestration.texi`; change the
  manual markers to Texinfo `@c BEGIN/END GENERATED`; add the dispatch
  region; check and write both the dispatch and maintenance Texinfo regions.
- `lisp/test/beads-transient-doc-test.el` (modified) — add the Texinfo
  renderer test and independent key-coverage tests for the manual dispatch
  and maintenance tables.
- `plans/gnu-toolchain/items/be-oi9x1-implementation-summary.md` — this
  artifact (follow-up commit).

No static manual content changed: `doc/orchestration.texi` is byte-identical
after regeneration.

## Verification

First verification command (scoped `:unit` ERT, plain Emacs, no Eldev):

```sh
emacs -Q --batch -L lisp -L lisp/test -l ert \
  -l lisp/test/beads-transient-doc-test.el \
  --eval '(ert-run-tests-batch-and-exit "beads-transient-doc-test")'
```

Observed: **pass** (exit 0). 11 of 11 tests passed, including
`beads-transient-doc-test-dispatch-check` (the full generated-doc gate),
`beads-transient-doc-test-menu-texinfo`, and the two manual key-coverage
tests.

The enforced key contract test also passes unchanged:

```sh
emacs -Q --batch -L lisp -L lisp/test -l ert \
  -l lisp/test/beads-key-registry-test.el \
  --eval '(ert-run-tests-batch-and-exit "beads-key-registry-test")'
```

Observed: **pass** (exit 0), 3 of 3 tests.

Final proof command (generate-then-check idempotence from the worktree):

```sh
emacs -Q --batch -L lisp --eval \
  '(progn (require (quote beads-dispatch-doc)) (beads-dispatch-doc-write) (beads-dispatch-doc-check) (princ "write+check ok\n"))'
git diff --quiet -- doc/orchestration.texi README.md docs/dispatch-reference.md
```

Observed: **pass** (exit 0). `write+check ok`; the regeneration produced no
working-tree diff in the manual, README, or reference snapshot, and the
check reported no drift.

Byte-compile with warnings-as-errors was also clean for both changed
runtime files:

```sh
emacs -Q --batch -L lisp --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile lisp/beads-transient-doc.el lisp/beads-dispatch-doc.el
```

Observed: **pass** (exit 0), no warnings.

## Remaining Risks

- **Eldev packaging trips on split Texinfo chapters (not W8).** Under this
  worktree's still-present `Eldev`, `eldev package`/`-p test` runs `makeinfo`
  on each `doc/*.texi` fragment and fails on cross-file `@ref`/`@pxref`
  nodes in `doc/views.texi`. That is an Eldev packaging behavior; W2
  (`be-c8dt2`) already replaces the drift script with plain Emacs and W10
  (`be-6s7rx`) removes Eldev. W8's gate is verified with plain Emacs, the
  path the merged build uses.
- **`scripts/dispatch-docs` comment in this isolated worktree** still names
  the deleted `docs/manual/orchestration.org`; W2 rewrites that line, so W8
  leaves it to avoid a merge conflict. The script's behavior is correct
  here because the elisp target paths already point at `doc/orchestration.texi`.
- **README generated region stays for now.** W9 (`be-57u9a`) may move the
  README dispatch table into the manual; if so, it updates the
  `beads-dispatch-doc-*` constants. W8 keeps the README gate as-is.
