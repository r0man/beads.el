---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-9hidc
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
    - path: beads/be-knva5
      hash: bead:be-knva5
      ids: [REQ-003]
    - path: plans/gnu-toolchain/requirements.md
      hash: sha256:e10a5d3dbcbb01f0571e768d3ec222bbbf4c3bfa3b98703b8411e1857c4fcaff
    - path: plans/gnu-toolchain/implementation-plan.md
      hash: sha256:6734ce93211d72bae8bb15520a052a84f31d66c97d260426075b7bfd098d0c01
    - path: plans/gnu-toolchain/decomposition.md
      hash: sha256:f27241bd58688fd3345a55de46292040b08c462532c875ed900e95d3c6f8f90f
    - path: README.md
      hash: git:54b606d3
  coverage:
    - id: REQ-003
      status: covered
---

# Implementation Summary — be-knva5 (W7: remove the Org manual pipeline)

## Summary

W7 removes the bespoke Org manual pipeline now that the user manual source
lives in Texinfo under `doc/` (W3–W6). The change deletes the Org sources,
their build target, and the shared Org-macro setup, and removes the legacy
`docs/beads.org` redirect stub that depended on them:

- Deleted `docs/manual/*.org` (`agents`, `formulas`, `index`,
  `issue-workflow`, `maintenance`, `orchestration`, `views`).
- Deleted `docs/manual/Makefile`, the `org→HTML` target with the
  `#+SETUPFILE`/`org-macro` machinery.
- Deleted `docs/.orgconfig`, the shared `fn`/`code` Org-macro setup.
- Deleted `docs/beads.org`, the redirect stub that `#+SETUPFILE`d the
  removed config and linked every removed manual page. It is replaced by a
  pointer in `README.md` to the Info manual and the Pages URL.
- Kept `docs/*.md` as internal engineering notes, and kept
  `docs/manual/screenshots/` and `docs/manual/images/` (with the committed
  `doc/images -> ../docs/manual/images` symlink) unchanged so the screenshot
  pipeline and its cross-repo contract stay intact.

No `lisp/` runtime file is touched (REQ-014).

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-003 | covered |

## Intended Behavior

- **REQ-003 (one manual source of truth).** After W3–W6 the manual is
  hand-authored Texinfo at `doc/beads.texi` plus `@include`d chapters; there
  must be no second, Org-authored copy and no `org→HTML` target. The deleted
  files are removed from the tree, so the only remaining manual source is
  `doc/*.texi`.
- **No broken redirect.** `docs/beads.org` was a stub whose `#+SETUPFILE`
  and links pointed exclusively at the removed files; deleting it avoids
  shipping an export that cannot resolve. `README.md` now carries a short
  "Documentation" section pointing at the Info manual
  (`C-h i d m beads.el`) and the Pages URL, and `doc/beads.texi` remains the
  source for `make info`/`make html`.
- **Engineering notes stay separate.** `docs/key-registry.md`,
  `docs/dispatch-reference.md`, `docs/defcommand-redesign.md`,
  `docs/cross-repo-ownership.md`, the live-UI design notes, and
  `docs/terminal-scrolling.md` are untouched; `docs/manual/screenshots/`
  and `docs/manual/images/` are untouched.
- **Downstream owners unchanged.** `lisp/beads-dispatch-doc.el` and
  `scripts/dispatch-docs` still name `docs/manual/orchestration.org`; moving
  the generated block to `doc/orchestration.texi` is W8 (`be-oi9x1`). The
  README rewrite, the Pages workflow, and the `AGENTS.md`/`NEWS.md` manual
  descriptions belong to W9/W10. W7 only removes the Org pipeline.

## Changed Files

- `README.md` (modified) — new "Documentation" section pointing at the
  Texinfo/Info manual and the Pages URL, replacing the deleted redirect
  stub's user-facing role.
- `docs/manual/agents.org`, `docs/manual/formulas.org`,
  `docs/manual/index.org`, `docs/manual/issue-workflow.org`,
  `docs/manual/maintenance.org`, `docs/manual/orchestration.org`,
  `docs/manual/views.org` (deleted) — Org manual pages, superseded by
  `doc/*.texi`.
- `docs/manual/Makefile` (deleted) — the `org→HTML` build target and
  `#+SETUPFILE` plumbing.
- `docs/.orgconfig` (deleted) — the shared `fn`/`code` Org-macro setup.
- `docs/beads.org` (deleted) — the legacy redirect stub that depended on
  the removed config and pages.
- `plans/gnu-toolchain/items/be-knva5-implementation-summary.md` — this
  artifact (follow-up commit).

Kept (not changed): `docs/*.md`, `docs/manual/screenshots/`,
`docs/manual/images/`, `doc/*.texi`. No `lisp/` runtime file is touched.

## Verification

First verification command (the manual `:unit` suite must still pass with
the Org sources gone, from the source-anchor worktree):

```sh
eldev -dtT test -f beads-manual-test.el '(tag :unit)'
```

Observed: **pass** (exit 0). `Ran 11 tests, 11 results as expected, 0
unexpected`, including `beads-manual-manifest-views-have-pages`,
`beads-manual-image-references-exist`, and
`beads-manual-texinfo-has-no-org-image-links`.

Final proof command (no Org-pipeline reference survives in the live tree
and the Texinfo manual still builds):

```sh
git grep -n 'orgconfig\|#+SETUPFILE' -- . ':!plans'
# expect no output
for f in docs/manual/agents.org docs/manual/formulas.org docs/manual/index.org \
         docs/manual/issue-workflow.org docs/manual/maintenance.org \
         docs/manual/orchestration.org docs/manual/views.org docs/manual/Makefile \
         docs/.orgconfig docs/beads.org; do test ! -e "$f" || exit 1; done
ls docs/*.md >/dev/null && ls docs/manual/images/*.png >/dev/null
makeinfo -I doc -o /tmp/beads-w7.info doc/beads.texi
```

Observed: **pass** — the scoped `git grep` prints nothing; every listed
path is absent; `docs/*.md` and the 36 committed images/screenshots remain;
`makeinfo` exits 0 with no missing-node/undefined-reference warnings. The
`docs/manual/screenshots/capture.sh` retargeted by W6 still runs against
`doc/*.texi` and is unaffected.

Residual grep note: an unrestricted `git grep` still matches the historical
artifact `plans/user-manual/items/be-0n84-implementation-summary.md` (a
record of the earlier phase that introduced `docs/.orgconfig` and
`docs/manual/*`). It is a frozen workflow record, not a live reference, so
it is left untouched; the live tree (`docs/`, `doc/`, `lisp/`, build files,
`README.md`) is clean. Recorded in Remaining Risks.

## Remaining Risks

- **Downstream references are intentionally still present.** 
  `lisp/beads-dispatch-doc.el` (`beads-dispatch-doc-manual-file`) and
  `scripts/dispatch-docs` still point at `docs/manual/orchestration.org`,
  and `AGENTS.md`/`NEWS.md` still describe the manual as Org pages under
  `docs/manual/`. These are owned by W8 (`be-oi9x1`) and W10 (`be-6s7rx`);
  until they land, `scripts/dispatch-docs --check` and the AGENTS/NEWS
  prose are stale. W7 does not edit them to avoid duplicating those items'
  changes.
- **Historical plan artifact still mentions `.orgconfig`.** The older
  `plans/user-manual/items/be-0n84-implementation-summary.md` records the
  files this item deleted. It is deliberately not rewritten so the record
  stays accurate; an unrestricted `git grep` for `.orgconfig` still hits it.
- **Pages URL not yet published.** The README link to
  <https://r0man.github.io/beads.el/> only resolves once W10 adds the Pages
  workflow (REQ-012 belongs to W10); W7 adds the user-facing pointer that
  replaces the deleted redirect stub.
- **No PDF/HTML end-to-end run.** This environment builds Info with
  `makeinfo` only; `make html`/`make pdf` packaging is W1–W3's build target,
  consistent with W4–W6's recorded limitation.
