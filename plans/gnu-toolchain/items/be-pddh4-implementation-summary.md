---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-bmnjb
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
    - path: beads/be-pddh4
      hash: bead:be-pddh4
      ids: [REQ-001, REQ-002, REQ-010]
    - path: plans/gnu-toolchain/requirements.md
      hash: sha256:e10a5d3dbcbb01f0571e768d3ec222bbbf4c3bfa3b98703b8411e1857c4fcaff
    - path: plans/gnu-toolchain/implementation-plan.md
      hash: sha256:6734ce93211d72bae8bb15520a052a84f31d66c97d260426075b7bfd098d0c01
    - path: plans/gnu-toolchain/decomposition.md
      hash: sha256:f27241bd58688fd3345a55de46292040b08c462532c875ed900e95d3c6f8f90f
    - path: doc/beads.texi
      hash: git:373a5ebf29d1f60d80555a30860d47843b7924c7
    - path: doc/orchestration.texi
      hash: git:373a5ebf29d1f60d80555a30860d47843b7924c7
    - path: doc/maintenance.texi
      hash: git:373a5ebf29d1f60d80555a30860d47843b7924c7
  coverage:
    - id: REQ-001
      status: covered
    - id: REQ-002
      status: covered
    - id: REQ-010
      status: covered
---

# Implementation Summary — be-pddh4 (W5: orchestration, maintenance, completeness and index)

## Summary

W5 completes the hand-written Texinfo manual. It converts the last two
Org pages, `docs/manual/orchestration.org` and
`docs/manual/maintenance.org`, into idiomatic Texinfo chapters
(`doc/orchestration.texi` and `doc/maintenance.texi`), wires them into
`doc/beads.texi`'s master `@menu` and `@include` list, and finishes the
REQ-010 completeness pass: a command and key reference, remote-store
coverage, the Autotools build and lint/documentation-drift nodes, a
contributing section, a troubleshooting section, and the full index (the
`@deffn`/`@defvr`/`@deftypefn` definitions populate the command/variable
index, and `@kindex` entries populate the key index). The implementation
commit is `373a5ebf` in the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-pddh4`.

Delivered:

- `doc/orchestration.texi` — `@chapter Orchestration and Terminal`:
  the dispatch and maintenance menus, the generated command and key
  reference (dispatch and maintenance tables), terminal backends and the
  `beads-terminal-*` registry functions, Dolt and repositories,
  configuration, remote stores, and audit/provenance.
- `doc/maintenance.texi` — `@chapter Maintenance and Extending`: user
  options and faces, the EIEIO command plumbing and execution contract,
  the command bridge protocol, the extension points (macro helpers,
  section modes, thing motion, pagination), the Autotools build
  (prerequisites, bootstrapping, targets, manual, Guix), the
  lint/documentation-drift gates, contributing, troubleshooting, and an
  FAQ.
- `doc/beads.texi` — master `@menu` and `@include`s extended with both
  new chapters; the "remaining task areas" prose now describes them as
  completing the reference and the indexes.

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-001 | covered |
| REQ-002 | covered |
| REQ-010 | covered |

## Intended Behavior

- **REQ-001 (Texinfo source of truth).** The two chapters are
  hand-authored Texinfo, not a mechanical `ox-texinfo` dump. They replace
  the corresponding `docs/manual/*.org` pages and are pulled into the one
  manual by `@include` from `doc/beads.texi`.
- **REQ-002 (idiomatic markup).** The new chapters use
  `@node`/`@menu`/`@section`/`@subsection` structure, `@deffn`/`@defvr`/
  `@deftypefn` definitions with automatic index entries, `@kbd`, `@code`,
  `@command`, `@env`, `@var`, `@file`, `@cindex`, `@kindex`, `@anchor`
  targets for every figure, `@image` forms for the carried screenshots,
  and `@xref`/`@pxref` cross-references that resolve to existing nodes.
  No Org `{{{...}}}`/`[[file:...]]`/`#+` markup remains in `doc/*.texi`.
- **REQ-010 (manual completeness).** The manual now covers getting
  started/installation, concepts, the board/menu and key reference, the
  day-to-day command workflows, agents/sling, remote (TRAMP) usage, the
  maintenance and extension surface, and contributing/troubleshooting.
  The generated dispatch and maintenance reference tables are included
  with the regeneration instruction, and the closing indices collect the
  definitions and key bindings.

## Changed Files

- `doc/orchestration.texi` (new) — Orchestration and Terminal chapter.
- `doc/maintenance.texi` (new) — Maintenance and Extending chapter.
- `doc/beads.texi` (modified) — `@menu` and `@include`s for the two new
  chapters, plus the intro prose.
- `plans/gnu-toolchain/items/be-pddh4-implementation-summary.md` — this
  artifact (follow-up commit).

No `lisp/` runtime file is touched; the change is documentation only
(REQ-014).

## Verification

First verification command (clean Info build from the source-anchor
worktree):

```sh
rm -f doc/beads.info; make info
```

Observed: **pass** (exit 0). `configure` located Emacs 31.1, `makeinfo`,
`texi2pdf`, `bd`, `dolt`, and `graphviz`; `makeinfo -I doc -I ./doc -o
doc/beads.info ./doc/beads.texi` rebuilt the seven-chapter manual with no
missing-node or undefined-reference warnings. A direct `makeinfo -I doc
-o /tmp/beads.info doc/beads.texi` run reports the same clean exit.

Final proof command (clean HTML build):

```sh
rm -rf doc/beads.html; make html
```

Observed: **pass** (exit 0). `makeinfo --html` produced
`doc/beads.html/index.html` and 132 page files, including
`Orchestration-and-Terminal.html` and `Maintenance-and-Extending.html`,
with no warnings. Spot-checks of the generated Info index find
`beads-dispatch`, `beads-maintenance`, `beads-dolt`, `beads-config`,
`beads-audit`, `beads-terminal-register`, `beads-executable`,
`beads-remote-transport`, `beads-thing-forward`, and `beads-pager-slice`,
each under its chapter's node. The Org-leftover check over `doc/*.texi`
found no `{{{...}}}`, `[[file:...]]`, or `#+` constructs.

## Remaining Risks

- **Generated tables are copies for now.** The dispatch and maintenance
  tables are rendered as `@multitable` blocks inside
  `doc/orchestration.texi`. The `scripts/dispatch-docs` generator and the
  `beads-dispatch-doc-*` path constants still target
  `docs/manual/orchestration.org`; retargeting them to the Texinfo source
  is W8 (`be-oi9x1`), which must regenerate these blocks so they cannot
  drift. Until then the `.org` pages remain the generator's target.
- **Image binaries are not copied into the HTML output.** As recorded by
  W4, texinfo 6.8's `makeinfo --html` emits `<img src="images/...">` but
  does not copy the PNGs in this environment. Wiring the screenshot
  pipeline to the Texinfo sources is W6 (`be-4l75q`).
- **No PDF verification.** This environment has no TeX engine, so the
  proof covers `make info` and `make html` only; W4 recorded the same
  limitation.
- **Org pages still present.** `docs/manual/*.org` are deleted by W7
  (`be-knva5`) after the generated-table retargeting lands, so the
  `:unit` manual test that reads them keeps passing in the interim.
