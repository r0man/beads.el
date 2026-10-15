---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-g6xwg
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
    - path: beads/be-57u9a
      hash: bead:be-57u9a
      ids: [REQ-013]
    - path: plans/gnu-toolchain/requirements.md
      hash: sha256:e10a5d3dbcbb01f0571e768d3ec222bbbf4c3bfa3b98703b8411e1857c4fcaff
    - path: plans/gnu-toolchain/implementation-plan.md
      hash: sha256:6734ce93211d72bae8bb15520a052a84f31d66c97d260426075b7bfd098d0c01
    - path: plans/gnu-toolchain/decomposition.md
      hash: sha256:f27241bd58688fd3345a55de46292040b08c462532c875ed900e95d3c6f8f90f
    - path: README.md
      hash: git:3e81c419
  coverage:
    - id: REQ-013
      status: covered
---

# Implementation Summary — be-57u9a (W9: concise standard project README)

## Summary

W9 rewrites `README.md` as a concise, standard project README (204 lines,
down from 1063). It keeps a one-paragraph description, the Test and
Coverage badges, the experimental status note, Guix and autotools install
instructions, a short quick start, pointers to the user manual (GitHub
Pages URL and `C-h i` Info), contributing guidance, and the
GPLv3-or-later license. The long hand-written usage, keybinding,
CLI-comparison, AI-agent, worktree, troubleshooting, and architecture
sections are removed from the README because the user manual already
documents them; the README now links to the manual, `docs/dispatch-reference.md`,
`docs/key-registry.md`, and `AGENTS.md`.

The `beads-dispatch-doc`-marked generated dispatch region is preserved
verbatim, so `scripts/dispatch-docs --check` still gates it and the
`:unit` ERT contract test still passes. The plan's WS-6 explicitly
sanctions preserving the generated region; moving it would require editing
`lisp/beads-dispatch-doc.el` and `lisp/test/beads-transient-doc-test.el`,
both of which W8 (`be-oi9x1`) owns in this same drain unit, and would
conflict at merge time. All *hand-written* long tables are gone.

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-013 | covered |

The implementation commit is `3e81c419` in the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-57u9a`.

## Intended Behavior

- **REQ-013 (README cleanup).** `README.md` is a concise, standard
  project README. It contains:
  - a one-paragraph description of what beads.el is;
  - the Test and Coverage badges, unchanged and still valid;
  - install via Guix (`guix shell -D -f guix.scm`, plus
    `guix build -f guix.scm` / `guix package -f guix.scm`);
  - install via autotools (`./autogen.sh && ./configure && make && make
    check && make install`), with the `configure` tool-detection and
    `--with-*=no` behavior noted;
  - a short quick start (`bd version`, `bd init`, `M-x beads`, core keys);
  - manual pointers: the GitHub Pages URL
    (<https://r0man.github.io/beads.el/>), the Info node after
    `make install` (`C-h i`, **Beads**), the generated dispatch reference,
    the key contract, and `AGENTS.md`;
  - a contributing section;
  - the GPLv3-or-later license with a `LICENSE` link.
- **Long tables moved out.** No hand-written long table remains. The only
  tables left are the five tables inside the machine-generated
  `<!-- BEGIN GENERATED: dispatch menu -->` region, which is the
  drift-checked dispatch contract (not hand-maintained). The manual
  (`docs/manual/orchestration.org` in this worktree; `doc/orchestration.texi`
  after W8) already carries the dispatch/maintenance reference and links to
  `docs/dispatch-reference.md`, so no content is lost.
- **Drift gate preserved.** `lisp/beads-dispatch-doc.el` is intentionally
  untouched: the README region markers and canonical bytes are unchanged,
  so `scripts/dispatch-docs --check` and
  `beads-transient-doc-test-readme-lists-dispatch-keys` keep passing.
- **No behavior change (REQ-014).** No `lisp/*.el` runtime file changed.

## Changed Files

- `README.md` (rewritten, commit `3e81c419`) — 89 insertions, 948
  deletions; 1063 → 204 lines. Only file changed.
- `plans/gnu-toolchain/items/be-57u9a-implementation-summary.md` — this
  artifact (follow-up commit).

## Verification

First verification command (the drift gate the README region participates
in, run from the worktree):

```sh
scripts/dispatch-docs --check
```

Observed: **pass** (exit 0, prints `dispatch docs ok`).

Final proof commands (generated region is canonical and the ERT contract
still holds):

```sh
# write-then-check idempotence: regeneration must not touch README or the manual
before=$(sha256sum README.md docs/manual/orchestration.org docs/dispatch-reference.md | sha256sum)
scripts/dispatch-docs --write
after=$(sha256sum README.md docs/manual/orchestration.org docs/dispatch-reference.md | sha256sum)
[ "$before" = "$after" ]   # observed: identical (idempotent)

# the README dispatch-region contract test plus the rest of the gate suite
emacs -Q --batch -L lisp -L lisp/test -l ert \
  -l lisp/test/beads-transient-doc-test.el \
  --eval '(ert-run-tests-batch-and-exit "beads-transient-doc-test")'
```

Observed: **pass**. `--write` produced no diff (`git status` shows only the
intended `README.md` modification before commit); the ERT run reported
`Ran 8 tests, 8 results as expected, 0 unexpected`, including
`beads-transient-doc-test-dispatch-check` and
`beads-transient-doc-test-readme-lists-dispatch-keys`.

README review checklist (all pass):

| Requirement | Result |
| --- | --- |
| One-paragraph description present | pass |
| Test badge present | pass |
| Coverage badge present | pass |
| Guix install command present | pass |
| Autotools install sequence present | pass |
| Short quick start present | pass |
| Manual Pages URL present | pass |
| Manual Info pointer (`C-h i`, Beads) present | pass |
| Contributing section present | pass |
| License is GPLv3-or-later with `LICENSE` link | pass |
| No hand-written long table remains | pass |
| Generated dispatch region preserved | pass |
| Repo-relative links resolve (`AGENTS.md`, `docs/dispatch-reference.md`, `docs/key-registry.md`, `LICENSE`) | pass |
| Badge workflows exist (`test.yml`; coverage via Codecov) | pass |

## Remaining Risks

- **Merge coordination with W8.** The plan offers a second, optional path:
  move the generated dispatch region into the manual and drop the README
  gate by editing `beads-dispatch-doc.el` (and its test). That change
  conflicts with W8 (`be-oi9x1`), which already retargeted the same file to
  `doc/orchestration.texi` and added a manual dispatch region. W9 therefore
  takes the plan-sanctioned preserve path; if a later integration step
  prefers the move, it is a small, isolated follow-up on top of W8.
- **Autotools and Pages URLs are forward references in this worktree.** W9's
  worktree is based on `origin/main` (per `prepare-worktree`) and does not
  contain W1's autotools skeleton or W3–W8's `doc/` Texinfo manual, so the
  install commands and the Pages URL describe the merged state. The
  repo-relative links that do exist in this worktree all resolve, and the
  drift gate passes here.
- **`sesman` still listed.** The README's dependency line lists `sesman`
  because the worktree's `lisp/beads.el` `Package-Requires` and `guix.scm`
  still require it on this base. The parallel `be-4fa65-remove-sesman`
  branch removes it (including from the README); the eventual merge must
  reconcile the two README edits. W9 does not add or reintroduce `sesman`.
- **No NEWS entry added.** W10 (`be-6s7rx`) owns the NEWS "Unreleased"
  entry for the autotools/manual/README migration, so W9 leaves `NEWS.md`
  untouched to avoid a duplicate/conflicting entry.
