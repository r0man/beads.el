---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-lfgkl
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
    - path: beads/be-fvrsi
      hash: bead:be-fvrsi
      ids: [REQ-001, REQ-002, REQ-010]
    - path: plans/gnu-toolchain/requirements.md
      hash: sha256:e10a5d3dbcbb01f0571e768d3ec222bbbf4c3bfa3b98703b8411e1857c4fcaff
    - path: plans/gnu-toolchain/implementation-plan.md
      hash: sha256:6734ce93211d72bae8bb15520a052a84f31d66c97d260426075b7bfd098d0c01
    - path: plans/gnu-toolchain/decomposition.md
      hash: sha256:f27241bd58688fd3345a55de46292040b08c462532c875ed900e95d3c6f8f90f
    - path: doc/beads.texi
      hash: git:ba4891dd77bb284c71effd7def13117268cbe0d2
    - path: doc/getting-started.texi
      hash: git:ba4891dd77bb284c71effd7def13117268cbe0d2
    - path: doc/issue-workflow.texi
      hash: git:ba4891dd77bb284c71effd7def13117268cbe0d2
  coverage:
    - id: REQ-001
      status: covered
    - id: REQ-002
      status: covered
    - id: REQ-010
      status: covered
---

# Implementation Summary — be-fvrsi (W3: Texinfo manual scaffolding and core chapters)

## Summary

W3 replaces the minimal `doc/beads.texi` stub added by W1 (`be-w12s8`) with the
real Texinfo manual front door and the first two hand-converted chapters
(REQ-001, REQ-002, REQ-010 opening portion; decomposition W3; implementation-plan
WS-1). It establishes the markup conventions that the later `doc/` slices (W4,
W5) follow and the screenshot/manual-test slice (W6) extends. The implementation
commit is `ba4891dd` in the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-fvrsi`.

Delivered:

- `doc/beads.texi` — rewritten as a complete manual header: `@settitle`, a
  `@set VERSION`, `@syncodeindex vr fn`, the GFDL 1.3-or-later
  `@copying`/`@insertcopying` block, the migrated `@dircategory Emacs` /
  `@direntry`, a `@titlepage` with `@insertcopying`, `@contents`, `@top`, a
  master `@menu`, `@include`s for the two chapters, and separate Key and
  Command/Variable indices.
- `doc/getting-started.texi` — hand-converted from `docs/manual/index.org`.
  Installation (requirements, MELPA, source, `bd init`), first issue, the three
  interaction patterns (modes/buffers, transients, context actions, compose
  buffers, marks, one movement scheme, store scoping), the concepts section
  (stores, issues, dependency model, molecules and formulas) and related
  resources.
- `doc/issue-workflow.texi` — hand-converted from
  `docs/manual/issue-workflow.org`. Creating, listing, viewing, editing,
  closing/reopening, claiming/status/priority, dependencies, labels and bulk
  actions, with worked examples.
- `doc/images` — a relative symlink to `docs/manual/images` so the `@image`
  forms carried over from the Org sources resolve for `make info`/`make html`.

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-001 | covered |
| REQ-002 | covered |
| REQ-010 | covered |

## Intended Behavior

- **REQ-001 (Texinfo source of truth).** The user manual is authored as Texinfo
  at `doc/beads.texi` plus `@include`d chapter files under `doc/`. The Org
  pages `index.org` and `issue-workflow.org` were converted by hand, not by a
  mechanical `ox-texinfo` dump; the `#+TEXINFO_DIR_CATEGORY`/`#+TEXINFO_DIR_TITLE`
  metadata is migrated to `@dircategory Emacs`/`@direntry` and the GFDL copying
  block is present under the project's license.
- **REQ-002 (idiomatic markup).** The sources use `@node`/`@menu`/`@section`
  structure; `@deffn` (16), `@defvr` (1) and `@deftypefn` (1) for commands,
  variables and functions; `@kbd` (74), `@code` (81), `@var` (1);
  `@cindex` (52), `@fnindex` (3), `@vindex` (1), `@kindex` (5);
  `@pxref` (4)/`@xref` (1); and `@anchor` (8) targets for cross-references.
  `@copying`/`@insertcopying` are present and `makeinfo` validates the manual
  without missing-node or undefined-reference warnings.
- **REQ-010 (manual completeness, opening portion).** Getting Started supplies
  the task-organized concepts coverage (stores, issues, dependencies,
  molecules/formulas) and cross-referenceable structure; Issue Workflow
  documents the create/list/view/edit/close/label commands with worked
  examples. The remaining task areas are added by W4 and W5.

## Changed Files

- `doc/beads.texi` (modified) — real title page, GFDL copying, dircategory,
  master menu, includes and indices; replaces the W1 stub.
- `doc/getting-started.texi` (new, 455 lines) — getting-started chapter.
- `doc/issue-workflow.texi` (new, 321 lines) — issue-workflow chapter.
- `doc/images` (new symlink) — `../docs/manual/images`, so `@image` resolves.
- `plans/gnu-toolchain/items/be-fvrsi-implementation-summary.md` — this
  artifact.

No `lisp/` runtime file is touched; the change is documentation only
(REQ-014).

## Verification

First verification command (clean Info build from the source-anchor worktree):

```sh
rm -f doc/beads.info; make info
```

Observed: **pass** (exit 0). `makeinfo -I doc -I ./doc -o doc/beads.info
./doc/beads.texi` rebuilt the manual with no warnings. A direct
`makeinfo --no-split -o /tmp/beads-v.info doc/beads.texi` also exited 0 with no
missing-node or undefined-reference output.

Final proof command (clean HTML build):

```sh
rm -rf doc/beads.html; make html
```

Observed: **pass** (exit 0). `makeinfo --html` produced
`doc/beads.html/index.html` plus one page per node, including the Getting
Started and Issue Workflow chapters. The markup checklist over `doc/*.texi`
confirmed every required construct (`@node`, `@menu`, `@section`, `@deffn`,
`@defvr`, `@deftypefn`, `@kbd`, `@code`, `@var`, `@cindex`, `@fnindex`,
`@vindex`, `@pxref`, `@xref`, `@anchor`) with the counts quoted under Intended
Behavior.

## Remaining Risks

- **Image binaries are not copied into the HTML output.** texinfo 6.8's
  `makeinfo --html` emits `<img src="images/...">` but did not copy the PNGs in
  this environment; `doc/images` only makes the sources resolvable. Wiring the
  screenshot pipeline into Texinfo and rewriting `lisp/test/beads-manual-test.el`
  to the Texinfo sources is W6 (`be-4l75q`); self-contained Pages publishing is
  W10 (`be-6s7rx`).
- **`doc/images` is a relative symlink.** It works in-tree and in a clone, but
  `make dist` and non-POSIX filesystems should be rechecked by W6/W10 when the
  images are moved or copied.
- **Later chapters are not present yet.** Views, formulas, agents,
  orchestration and maintenance are W4/W5; `doc/beads.texi`'s menu intentionally
  lists only the two chapters plus the indices until then.
- **No PDF verification.** This environment has no TeX engine (same limitation
  recorded by W1), so the W3 proof covers `make info`/`make html` only.
