---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-e0j0
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
    - path: beads/be-yzbs
      hash: bead:be-yzbs
      ids: [REQ-SF-080, REQ-SF-083]
    - path: plans/beads-standalone-formulas/requirements.md
      hash: sha256:eda1b3f937b47f7ac6d4e5acd7590c7c260b4ff6f09fc4c6dc7dd5816e69207b
    - path: plans/beads-standalone-formulas/design.md
      hash: sha256:811b4d4a52bc18755ed29e7513708201087a53e698ec34cc0b0b3d7b851563a9
    - path: plans/beads-standalone-formulas/implementation-plan.md
      hash: sha256:fb81bf91767c9839885f8db204ae2d2704452ca9e224fb075ca1e862f9eeebb3
    - path: plans/beads-standalone-formulas/decomposition.md
      hash: sha256:b709674574b621765367810709baf406d25480f47e84b9efd2a5a7e5ad7a2e88
    - path: lisp/test/beads-standalone-guard-test.el
      hash: git:d4482cc31c19022eb19eafa17bdb29ee37a14dff
    - path: lisp/test/beads-standalone-flow-test.el
      hash: git:8d2f984ab0fc04ec96501fccc8b757a2c2d40481
  coverage:
    - {id: REQ-SF-080, status: covered}
    - {id: REQ-SF-083, status: covered}
---

| ID | Status |
| --- | --- |
| REQ-SF-080 | covered |
| REQ-SF-083 | covered |

## Summary

WI-SF-13 is the standalone acceptance gate: it must prove that every new
formula -> molecule -> gate -> wisp -> swarm flow works with `bd` alone and
that no default path references a gascity symbol.  This change lands the
part of that gate the worktree can truthfully verify and consolidates the
standalone flow suite behind it.

`lisp/test/beads-standalone-guard-test.el` (REQ-SF-080) is a real guard,
not a checklist:

- Its scanner reads actual Lisp forms and walks the form tree for symbols
  in the gascity namespace (`gascity`, `gascity-*`, `gc-*`).  Comments and
  docstrings that mention gascity are not false positives, while a
  `(require 'gascity)`, a `(gascity-run ...)` call and a quoted
  `'gc-run` symbol are all caught.  A self-check plants all four cases and
  asserts the scanner separates them.
- It asserts gascity is genuinely absent (`featurep` and `locate-library`)
  while the guard runs, so the scan cannot pass merely because gascity was
  loaded and its symbols were already interned.
- It scans every module on the standalone surface -- the core command
  layer and formula seams that exist today, plus the modules the plan
  assigns to sibling work items (`beads-molecule`, `beads-gate`,
  `beads-wisp`, `beads-bond`, `beads-formula-edit`, `beads-handoff`,
  `beads-swarm`, `beads-cook`, `beads-command-cook`,
  `beads-command-prime`).  A sibling module that has not landed is reported
  as pending and will be scanned automatically the moment it does; the
  core modules are asserted present so the guard can never pass vacuously.
- It walks each standalone flow through a mocked `beads-command-execute`
  (cook -> pour -> work -> close, cook -> wisp -> squash, gate round-trip,
  bond, distill, setup, swarm validate/create/status) and fails if any
  executed command references gascity by class name or CLI argument.
- It walks the real `beads-formula-launch` entry function with the same
  mock and asserts it irons through `bd mol pour`.

`lisp/test/beads-standalone-flow-test.el` (REQ-SF-083 execution)
consolidates the integration suite the plan calls for.  Each test writes a
self-contained two-step formula into the temp repo's `.beads/formulas/`,
so no standing city, external catalog, or gascity is needed:

- cook -> pour -> work -> close
- cook -> wisp -> squash
- gate create -> check -> resolve -> ready
- distill a poured molecule back into a formula
- setup status

## Intended Behavior

- Any future change that reaches for a gascity symbol on a default path
  fails `beads-standalone-guard-test-no-gascity-symbols` (and, if it is a
  flow command, `beads-standalone-guard-test-flow-commands-are-bd-only`).
- Adding a sibling module extends coverage with no edit to the guard: the
  module is picked up by the manifest scan as soon as it is locatable.
- The standalone command layer is executable end to end against a real
  embedded-Dolt temp store with only `bd` on the path; the flows assert
  domain outcomes (a claimed step closes, a wisp squashes, a resolved gate
  frees the blocked issue) rather than merely that a process exited 0.
- The guard is honest about state: pending sibling modules are visible in
  the failure/pending reporting, never silently counted as covered.

## Changed Files

- `lisp/test/beads-standalone-guard-test.el` (new) -- the no-gascity guard:
  form-tree scanner, module manifest, gascity-absent precondition, mocked
  flow walks, and the `beads-formula-launch` entry walk.
- `lisp/test/beads-standalone-flow-test.el` (new) -- the consolidated
  real-`bd` standalone flow suite.

No production source is changed.  This is a test/acceptance work item; the
sibling WIs own the porcelain modules the guard will cover once they land.

## Verification

First verification command (the bead's stated expectation):

```
eldev test -f beads-standalone-guard-test.el
```

Observed: `Ran 6 tests, 6 results as expected, 0 unexpected` / exit 0.

The consolidated flow suite:

```
eldev test -f beads-standalone-flow-test.el
```

Observed: `Ran 5 tests, 5 results as expected, 0 unexpected` / exit 0.

Byte-compile and lint:

```
eldev -p -dtT compile
eldev -p -dtT lint
```

Observed: `Finished successfully` and `Linters have no complaints` / exit 0.

Final proof command (full non-integration suite):

```
eldev -p -dtT test '(not (tag :integration))'
```

Observed: `Ran 5824 tests, 5800 results as expected, 0 unexpected, 24
skipped` / exit 0.

## Remaining Risks

- The porcelain modules (`beads-molecule`, `beads-gate`, `beads-wisp`,
  `beads-bond`, `beads-formula-edit`, `beads-handoff`, `beads-swarm`) are
  owned by sibling work items and are not present in this worktree's base
  (917280a).  The guard scans them when they land; until then they are
  reported pending, so REQ-SF-080's coverage of those specific modules is
  forward-looking rather than observed here.  This matches WI-SF-12's
  documented dependency gap.
- The aggregate acceptance evidence WI-SF-13 requires -- per-WI live
  `emacs -nw -Q` runs in tmux against `~/bright-lights` (local and
  `/ssh:localhost:~/bright-lights`) and the render guard green over TRAMP
  -- cannot be produced in this headless worktree and is blocked on the
  sibling modules existing.  It must be re-run once the sibling PRs merge;
  this summary records the blockage rather than claiming the live gate.
- The flow suite pins local `.formula.toml` fixtures.  If a future `bd`
  changes the formula file contract, the fixtures move with it; CI pins
  `bd` v1.3.0 and the suite was verified against v1.3.1 locally.
