---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-zyrq
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
    - path: beads/be-85xl
      hash: bead:be-85xl
      ids:
        - REQ-SF-100
  coverage:
    - id: REQ-SF-100
      status: covered
---

## Summary

WI-SF-19 (REQ-SF-100) beads.el side: audit gascity.el for beads-native
logic it reimplements and add the generic seams to beads.el so gascity.el
can be reduced to a thin caller. The named hotspot is the formula-variable
machinery: gascity's `gascity-formula--enum-choices` /
`gascity-formula--methodology` and `gascity-formula--validate-values` /
`gascity-sling--missing-required-vars` duplicate what beads.el should own.
This change adds those seams to `beads-formula.el`, parses the formula
`metadata` payload, and records the before/after module-ownership list in
`docs/cross-repo-ownership.md`, guarded by tests. No gascity.el file is
edited from this worktree; that is tracked in the gascity.el rig.

Coverage:

| ID | Status |
| --- | --- |
| REQ-SF-100 | covered |

## Intended Behavior

- `beads-formula-var-choices` (with `beads-formula-methodology` and the
  new `beads-formula-enum-metadata-keys` custom) resolves a variable's
  allowed values: an explicit `enum` wins, otherwise the variable name ->
  `metadata.gc.methodology` mapping.  `beads-formula-var-reader` now takes
  an optional formula, reports the resolved `:choices`, and classifies a
  methodology-backed variable as `enum`; a variable with neither source
  degrades to plain text.  This is the one place enum resolution lives.
- `beads-formula-validate-vars` checks required variables and `pattern`
  client-side and signals `user-error`; `beads-formula-missing-required-vars`
  is the non-signaling half a preview footer warns with.  Whitespace-only
  values count as missing, matching gc's own pre-launch check.
- `beads-formula` now carries the raw `metadata` alist from
  `bd formula show --json`, which the methodology seam reads.
- Standalone beads.el behavior is unchanged: no new hard dependency, no
  `gascity.el` require, and the default `bd mol pour` launch path is intact.
- `docs/cross-repo-ownership.md` is the hand-off contract for the linked
  gascity.el bead: hotspot inventory with before/after ownership, the
  machine-checked seam table, and the explicit deferral of the sling infix
  machinery (hotspot 7).

## Changed Files

- `lisp/beads-formula.el` - new `beads-formula-var-choices`,
  `beads-formula-methodology`, `beads-formula-validate-vars`,
  `beads-formula-missing-required-vars`; reader takes an optional formula.
- `lisp/beads-custom.el` - new `beads-formula-enum-metadata-keys` custom.
- `lisp/beads-types.el` - `beads-formula` gains the `metadata` slot/parse.
- `lisp/test/beads-formula-test.el` - unit tests for choices, validation,
  and metadata parsing.
- `lisp/test/beads-cross-repo-ownership-test.el` - new guard: seams resolve,
  the document names them, and beads.el never requires gascity.el.
- `docs/cross-repo-ownership.md` - new ownership/hotspot record.
- `docs/ui-redesign.md` - seam table and verification table refreshed.
- `NEWS.md` - Unreleased entry.

## Verification

First verification command (after the source edits):

```
eldev test -f beads-formula-test.el
```

Result: 24/24 formula tests passed (the broader file run also loads
`beads-test.el`; its one order-dependent failure, see Remaining Risks, is
pre-existing on the untouched base commit).

Focused seam tests:

```
eldev test -f beads-cross-repo-ownership-test.el
```

Result: 3/3 passed.

Intermediate gates:

```
eldev compile
eldev -p -dtT lint
eldev test '(not (tag :integration))'
```

Result: compile and lint exit 0 with no warnings (lint initially flagged
one docstring, fixed); the unit suite ran 5830 tests, 0 unexpected, 24
skipped (live).

Final proof command (run from the launcher rig root
`/home/roman/workspace/beads.el`):

```
GC_BEAD_ID=be-1oi4 .gc/scripts/checks/build-artifact-valid.sh
```

Result: `build artifact valid: schema=gc.build.implementation-summary.v1`
(exit 0).

## Remaining Risks

- The gascity.el edits are out of scope for this worktree by REQ-SF-100
  and are tracked in the gascity.el rig (blocked on WI-SF-13).  The
  acceptance "gascity.el byte-compiles and its tests pass against the new
  seams" is therefore verified by that linked bead, not here.
- The WI depends on WI-SF-01..11,15..18; the molecule/gate/wisp/bond/swarm
  porcelain does not exist in this worktree yet, but the formula seams this
  work changes do not depend on it and are additive.
- `beads-test-issue-at-point-from-show-buffer` fails when `beads-test.el`
  runs as a file, independent of this change; it passes in isolation and
  fails identically on the untouched base commit `917280a`.  Not fixed
  here (out of WI scope).
- The sling's generated var infixes and deterministic key assignment
  (hotspot 7) still live in gascity.el, deferred to the WI that lands the
  beads.el sling stage.
