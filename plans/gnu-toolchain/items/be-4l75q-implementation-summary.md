---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-emnob
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
    - path: beads/be-4l75q
      hash: bead:be-4l75q
      ids: [REQ-004, REQ-010, REQ-011]
    - path: plans/gnu-toolchain/requirements.md
      hash: sha256:e10a5d3dbcbb01f0571e768d3ec222bbbf4c3bfa3b98703b8411e1857c4fcaff
    - path: plans/gnu-toolchain/implementation-plan.md
      hash: sha256:6734ce93211d72bae8bb15520a052a84f31d66c97d260426075b7bfd098d0c01
    - path: plans/gnu-toolchain/decomposition.md
      hash: sha256:f27241bd58688fd3345a55de46292040b08c462532c875ed900e95d3c6f8f90f
    - path: lisp/test/beads-manual-test.el
      hash: git:9fe10abe
    - path: docs/manual/screenshots/capture.sh
      hash: git:9fe10abe
    - path: docs/manual/screenshots/README.md
      hash: git:9fe10abe
  coverage:
    - id: REQ-004
      status: covered
    - id: REQ-010
      status: covered
    - id: REQ-011
      status: covered
---

# Implementation Summary — be-4l75q (W6: screenshots into Texinfo, manual test rewrite)

## Summary

W6 wires the deterministic screenshot pipeline into the Texinfo manual and
retargets its test from the Org sources to the hand-authored Texinfo
chapters. The `@image{images/<view>-<theme>,,,alt,png}` forms (full plus
thumbnail) already landed with the W3/W4 chapter conversions, so the owned
work here is the test, the pipeline's `--scope changed` source map, and the
screenshot README:

- `lisp/test/beads-manual-test.el` now points
  `beads-manual-test--manual-dir` at `doc/`, enumerates `doc/*.texi`
  instead of `docs/manual/*.org`, reads `@image{...}` forms for the
  image-reference set, and matches `@image{images/<view>-<theme>...}` for
  the manifest assertion. The manifest assertion is the REQ-004
  image-coverage gate: dropping a view's `@image` fails it. A new unit
  test, `beads-manual-texinfo-has-no-org-image-links`, fails if any Org
  `[[file:images/...]]` reference survives in the chapters.
- `docs/manual/screenshots/capture.sh` retargets the per-view
  `VIEW_SOURCES` map used by `--scope changed` from `docs/manual/*.org` to
  the corresponding `doc/*.texi` chapters (dashboard covers
  `getting-started.texi` and `views.texi`; ready/blocked live in
  `views.texi`; dispatch/maintenance in `orchestration.texi`).
- `docs/manual/screenshots/README.md` records `doc/` as the beads.el
  manual directory and notes that the pipeline and committed images stay
  under `docs/manual/` per the plan's WS-2 decision.

The capture contract (manifest of 14 views, `--verify`/`--verify-scope`,
fixture isolation) is unchanged. No `lisp/` runtime file is touched
(REQ-014).

Delivered:

- `lisp/test/beads-manual-test.el` — manual dir moved to `doc/`; Texinfo
  page scanning; `@image` extraction and manifest gate; Org-link guard.
- `docs/manual/screenshots/capture.sh` — `VIEW_SOURCES` retargeted to
  `doc/*.texi`.
- `docs/manual/screenshots/README.md` — path references and cross-repo
  table updated.

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-004 | covered |
| REQ-010 | covered |
| REQ-011 | covered |

## Intended Behavior

- **REQ-004 (screenshot coverage in the manual).** Every one of the 14
  manifest views must appear as an `@image` in `doc/*.texi`. The
  rewritten `beads-manual-manifest-views-have-pages` test asserts this
  from the Texinfo sources, and `beads-manual-image-references-exist`
  asserts every referenced `images/*.png` resolves on disk (through the
  committed `doc/images -> ../docs/manual/images` symlink). The manifest,
  the shot list, and the capture/verify/verify-scope behavior are
  unchanged.
- **REQ-011 (no stale generated/duplicate content).** The manual test no
  longer scans `docs/manual/*.org`; it reads the Texinfo source of truth.
  The new `beads-manual-texinfo-has-no-org-image-links` test fails if an
  Org image link is left in a chapter. `capture.sh --scope changed` now
  keys on the same Texinfo sources, so a chapter edit selects its views.
- **REQ-010 (manual completeness).** The image coverage gate is
  table-driven from the capture manifest, so a chapter that drops a view
  cannot pass `make check`.

## Changed Files

- `lisp/test/beads-manual-test.el` (modified) — Texinfo retarget, `@image`
  extraction, manifest image-coverage gate, Org-link guard.
- `docs/manual/screenshots/capture.sh` (modified) — `VIEW_SOURCES` points
  at `doc/*.texi`.
- `docs/manual/screenshots/README.md` (modified) — manual-directory and
  layout path references.
- `plans/gnu-toolchain/items/be-4l75q-implementation-summary.md` — this
  artifact (follow-up commit).

No `lisp/` runtime file is touched; the change is test and documentation
plumbing only.

## Verification

First verification command (the manual `:unit` suite against the Texinfo
sources, from the source-anchor worktree):

```sh
eldev -dtT test -f beads-manual-test.el '(tag :unit)'
```

Observed: **pass** (exit 0). `Ran 11 tests, 11 results as expected, 0
unexpected`; the 11 include the rewritten
`beads-manual-scope-changed-maps-views` (now editing `doc/views.texi`),
`beads-manual-manifest-views-have-pages`, `beads-manual-image-references-
exist`, and the new `beads-manual-texinfo-has-no-org-image-links`.

Final proof command (the REQ-004 gate is real: removing a view's `@image`
must fail it). Both `@image{images/agents-dark...}` lines were removed
from `doc/agents.texi`, the gate run, and the file restored:

```sh
sed -i '/@image{images\/agents-dark/d' doc/agents.texi
eldev -dtT test -f beads-manual-test.el 'beads-manual-manifest-views-have-pages'
git checkout -- doc/agents.texi
```

Observed: **pass** — the gate reported `FAILED 1/1
beads-manual-manifest-views-have-pages` with the removed view, and it
passes again after restore. The chapters also still build cleanly:

```sh
makeinfo -I doc -o /tmp/beads-w6.info doc/beads.texi
```

Observed: **pass** (exit 0, no missing-node or undefined-reference
warnings). `bash -n docs/manual/screenshots/capture.sh` exits 0.

## Remaining Risks

- **HTML image copying is still W3's target.** `makeinfo` resolves the
  `@image` files through `doc/images`, and the Info/manual build is clean,
  but this environment's `makeinfo --html` does not copy the PNGs into
  `doc/beads.html/` (recorded by W4/W5). That self-contained-HTML
  packaging belongs to the WS-3 build work item, not W6.
- **The Org pages still exist.** `docs/manual/*.org` are deleted by W7
  (`be-knva5`) after the generated-table retargeting lands. The test no
  longer reads them, and `capture.sh` no longer keys on them, so the
  deletion is now unblocked from W6's side.
- **`--scope changed` needs the Texinfo files present.** The map points at
  `doc/*.texi`, which exist in this worktree's stacked build (W3–W5); on a
  checkout without them the changed-scope test would find no changed
  sources. That is the expected order of the implementation convoy.
- **No PDF verification.** This environment has no TeX engine; the proof
  covers the ERT suite and `makeinfo` only, same limitation W4/W5
  recorded.
