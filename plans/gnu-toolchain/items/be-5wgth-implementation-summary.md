---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-kbh1b
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
    - path: beads/be-5wgth
      hash: bead:be-5wgth
      ids: [REQ-001, REQ-002, REQ-010]
    - path: plans/gnu-toolchain/requirements.md
      hash: sha256:e10a5d3dbcbb01f0571e768d3ec222bbbf4c3bfa3b98703b8411e1857c4fcaff
    - path: plans/gnu-toolchain/implementation-plan.md
      hash: sha256:6734ce93211d72bae8bb15520a052a84f31d66c97d260426075b7bfd098d0c01
    - path: plans/gnu-toolchain/decomposition.md
      hash: sha256:f27241bd58688fd3345a55de46292040b08c462532c875ed900e95d3c6f8f90f
    - path: doc/views.texi
      hash: git:94a8f60faf6b42891f5dcd132f7a76e424daecd1
    - path: doc/formulas.texi
      hash: git:94a8f60faf6b42891f5dcd132f7a76e424daecd1
    - path: doc/agents.texi
      hash: git:94a8f60faf6b42891f5dcd132f7a76e424daecd1
    - path: doc/beads.texi
      hash: git:94a8f60faf6b42891f5dcd132f7a76e424daecd1
  coverage:
    - id: REQ-001
      status: covered
    - id: REQ-002
      status: covered
    - id: REQ-010
      status: covered
---

# Implementation Summary — be-5wgth (W4: Texinfo views, formulas, and agents chapters)

## Summary

W4 continues the hand-converted Texinfo manual from W3 (`be-fvrsi`) with the
three middle chapters named by work item W4 (REQ-001, REQ-002, REQ-010 middle
portion; implementation-plan WS-1). It adds `doc/views.texi`,
`doc/formulas.texi`, and `doc/agents.texi`, and wires them into
`doc/beads.texi`'s master `@menu` and `@include`s, so `make info` and
`make html` now render Getting Started, Issue Workflow, Dashboards and Views,
Formulas/Molecules/Epics, and Agents and Sling in one manual. The
implementation commit is `94a8f60f` in the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-5wgth`.

Delivered:

- `doc/views.texi` — `@chapter Dashboards and Views` from
  `docs/manual/views.org`: the board and dashboard, board sections and
  folding, ready and blocked lists, list/show modes, thing movement,
  pagination, and eldoc previews, with the dashboard, ready, blocked, and
  list/show figures.
- `doc/formulas.texi` — `@chapter Formulas, Molecules, and Epics` from
  `docs/manual/formulas.org`: the formula menu and browser, molecule
  instantiate/combine/lifecycle/cleanup/health verbs, epics, and the
  cook/pour/burn verbs carried on the maintenance menu.
- `doc/agents.texi` — `@chapter Agents and Sling` from
  `docs/manual/agents.org`: the agent command keys, agent types and their two
  prompt functions, the backend registry and `beads-agent-backend-start`,
  sessions and worktree isolation, and sling/swarm.
- `doc/beads.texi` — master `@menu` and `@include`s extended with the three
  new chapters; the "remaining task areas" prose now names only the two
  still-pending areas (orchestration and maintenance).

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-001 | covered |
| REQ-002 | covered |
| REQ-010 | covered |

## Intended Behavior

- **REQ-001 (Texinfo source of truth).** The three chapters are hand-edited
  Texinfo under `doc/`, not a mechanical `ox-texinfo` dump. The chapter files
  replace the corresponding `docs/manual/*.org` pages and are pulled into the
  single manual by `@include` from `doc/beads.texi`.
- **REQ-002 (idiomatic markup).** The new sources use `@node`/`@menu`/
  `@section`/`@subsection` structure (`views` 8 nodes, `formulas` 10,
  `agents` 6); `@deffn`/`@deftypefn` definitions for user-visible commands and
  functions (`views` 8, `formulas` 3, `agents` 3); `@kbd`, `@code`, and
  `@var`; `@cindex` concept entries (`views` 14, `formulas` 19, `agents` 11)
  plus `@kindex`; `@anchor` targets for every figure; `@image` forms for the
  carried-over screenshots (`views` 8, `formulas` 8, `agents` 6); and
  `@pxref`/`@ref` cross-references to the Getting Started and Issue Workflow
  chapters. No Org `{{{...}}}`/`[[file:...]]` markup remains.
- **REQ-010 (manual completeness, middle portion).** Dashboards and Views
  documents the board, lists, show, movement, pagination, and eldoc; Formulas,
  Molecules, and Epics documents the formula/molecule/epic commands; Agents
  and Sling documents the agent types, backends, sessions, sling, and swarm.
  Every user-visible command introduced by a chapter has a `@deffn`/`@deftypefn`
  definition and an index entry, and the concepts carry `@cindex` entries.
  Cross-references point only at anchors that already exist, so the proof
  produces no undefined-reference warnings.

## Changed Files

- `doc/views.texi` (new) — Dashboards and Views chapter.
- `doc/formulas.texi` (new) — Formulas, Molecules, and Epics chapter.
- `doc/agents.texi` (new) — Agents and Sling chapter.
- `doc/beads.texi` (modified) — `@menu` and `@include`s for the three new
  chapters.
- `plans/gnu-toolchain/items/be-5wgth-implementation-summary.md` — this
  artifact.

No `lisp/` runtime file is touched; the change is documentation only
(REQ-014).

## Verification

First verification command (clean Info build from the source-anchor worktree):

```sh
rm -f doc/beads.info; make info
```

Observed: **pass** (exit 0). `makeinfo -I doc -I ./doc -o doc/beads.info
./doc/beads.texi` rebuilt the five-chapter manual with no warnings. A direct
`makeinfo` run over the new files reported no missing-node or
undefined-reference output; the duplicate-anchor collision between
`getting-started.texi` and `views.texi` (both originally defining
`fig:dashboard-dark`) was found and fixed by renaming the views anchors to
`fig:board-dark`/`fig:board-light` before the clean build.

Final proof command (clean HTML build):

```sh
rm -rf doc/beads.html; make html
```

Observed: **pass** (exit 0). `makeinfo --html` produced
`doc/beads.html/index.html` plus one page per node, including
`Dashboards-and-Views.html`, `Formulas-Molecules-and-Epics.html`, and
`Agents-and-Sling.html`. A spot-check of the generated Info index finds
`beads-thing-forward`, `beads-pager-mode`, `beads-eldoc-mode`,
`beads-formula-menu`, `beads-mol`, `beads-epic-menu`,
`beads-agent-type-system-prompt`, and `beads-agent-backend-start`, each under
its chapter's node. The markup and Org-leftover checks over `doc/*.texi`
passed (no `{{{...}}}`, `[[file:...]]`, or `=word=` constructs).

## Remaining Risks

- **Image binaries are not copied into the HTML output.** texinfo 6.8's
  `makeinfo --html` emits `<img src="images/...">` but does not copy the PNGs
  in this environment; the `doc/images` symlink only makes the sources
  resolvable. Wiring the screenshot pipeline and retargeting
  `lisp/test/beads-manual-test.el` to the Texinfo sources is W6 (`be-4l75q`);
  self-contained Pages publishing is W10 (`be-6s7rx`).
- **`doc/beads.texi` still does not include orchestration or maintenance.**
  Those two chapters are W5 (`be-w12s8`'s followers); until they land, the
  master menu intentionally lists only the five chapters plus the indices, and
  the new chapters deliberately avoid cross-references into the missing nodes
  so the build stays warning-free.
- **Duplicate figure usage.** The dashboard figure is rendered in both
  Getting Started and Dashboards and Views with distinct anchors; if the
  screenshot drift test (W6) keys strictly on one image per view it may need a
  single canonical location.
- **No PDF verification.** This environment has no TeX engine (same limitation
  recorded by W1/W3), so the W4 proof covers `make info`/`make html` only.
