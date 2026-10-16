---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-hbo2d
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
    - path: beads/be-szv6e
      hash: bead:be-szv6e
      ids: [REQ-009, REQ-013]
    - path: plans/manual-standard/requirements.md
      hash: sha256:0ffcb4eb7f97f2aef5a4416e8810c3b4da0d24f2bcbccf5640fd00a9967202a7
    - path: plans/manual-standard/implementation-plan.md
      hash: sha256:ccfb1a822c2ef5fbe69da8764e4d210ada1271c20520b6817a60bb6f96225498
    - path: plans/manual-standard/decomposition.md
      hash: sha256:015346698dc65d8143b28c77dd4eb8137b445e8896cdbf5163269f443f49f178
    - path: lisp/test/beads-transient-doc-test.el
      hash: git:39a5ee0859314e3a4e630bf0440fbc037f3f7b31
  coverage:
    - id: REQ-009
      status: covered
    - id: REQ-013
      status: covered
---

# Implementation Summary — be-szv6e (WI-10: Generated tables and drift gates green)

## Summary

WI-10 keeps the generated dispatch/maintenance tables and the
dispatch/key drift gates green after the WI-05 manual restructure
(REQ-009, REQ-013). Work ran in the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-szv6e`, stacked on the
WI-05 dependency commit `90beed1e`; the launcher checkout was never
edited.

Findings and action:

- The generated dispatch and maintenance tables still live in
  `doc/orchestration.texi` inside the
  `@c BEGIN/END GENERATED: dispatch menu` and
  `@c BEGIN/END GENERATED: maintenance menu` marked regions. The WI-05
  restructure did **not** move them, so no relocation to
  `doc/generated/*.texi` and no `lisp/beads-dispatch-doc.el` path
  constant change was needed.
- `scripts/dispatch-docs --write` is idempotent on the post-WI-05 tree:
  it rewrote `docs/dispatch-reference.md`, the `README.md` block, and
  both manual regions byte-for-byte, leaving `git status` clean. This is
  the expected "already in sync" outcome, not a skipped regeneration.
- The one missing piece was a *negative* regression for the WI-10
  verification bar "fails on a hand-edited menu that was not
  regenerated": the gate was only proven to fail on a stale checked-in
  artifact, not on a changed menu list. `lisp/test/beads-transient-doc-test.el`
  now asserts that `beads-dispatch-doc-check` signals when the menu list
  changes without regeneration, so drift of either direction is covered
  by the `:unit` suite.

No `lisp/` runtime file was touched; the only source change is a test
file (REQ-017 is unaffected).

## Intended Behavior

- `scripts/dispatch-docs --check` stays green on the post-WI-05 manual
  tree and continues to fail when the transient menu code, the
  `docs/dispatch-reference.md` snapshot, the README dispatch block, or
  either `doc/orchestration.texi` marked region disagrees.
- A menu edit that is not regenerated signals `beads-dispatch-doc-check`,
  which `scripts/dispatch-docs --check` surfaces as a non-zero exit.
- `docs/key-registry.md` remains in sync with
  `lisp/test/beads-key-registry-test.el`.

## Changed Files

- `lisp/test/beads-transient-doc-test.el` — added
  `beads-transient-doc-test-check-fails-on-unregenerated-menu`, which
  let-binds a changed `beads-dispatch-doc-menus` list and asserts
  `beads-dispatch-doc-check` errors. This test-only change does not
  alter runtime behavior.

No change was required in `doc/orchestration.texi`,
`lisp/beads-dispatch-doc.el`, `docs/dispatch-reference.md`, or
`README.md`; `--write` reproduced them exactly.

## Verification

First verification command (in the worktree):
`scripts/dispatch-docs --write` then `scripts/dispatch-docs --check` —
observed: `--write` left `git status --short` empty (idempotent), and
`--check` printed `dispatch docs ok` (pass).

Negative gate proof: hand-editing a key in `lisp/beads-menu.el` and a
row in `doc/orchestration.texi` each made `scripts/dispatch-docs --check`
exit 255; reverting restored `dispatch docs ok`. The new unit test
captures the menu-side case.

Final proof commands (in the worktree):

```sh
scripts/dispatch-docs --check
emacs -Q --batch -L lisp -L lisp/test \
  -l lisp/test/beads-transient-doc-test.el \
  -f ert-run-tests-batch-and-exit
emacs -Q --batch -L lisp -L lisp/test \
  -l lisp/test/beads-key-registry-test.el \
  -f ert-run-tests-batch-and-exit
makeinfo -I doc -o /tmp/beads-wi10.info doc/beads.texi
```

Observed: `dispatch docs ok`; `Ran 12 tests, 12 results as expected, 0
unexpected`; `Ran 3 tests, 3 results as expected, 0 unexpected`;
`makeinfo` exit 0 with no warnings. The modified test file also
byte-compiles clean under `byte-compile-error-on-warn`.

| ID | Status |
| --- | --- |
| REQ-009 | covered |
| REQ-013 | covered |

## Remaining Risks

- This item is verification-heavy because the gnu-toolchain W8 work had
  already retargeted the generated dispatch tables and key-registry
  gates to Texinfo before this plan ran; the WI-10 delta is therefore a
  regression test rather than a table rewrite. A reviewer expecting a
  regenerated-artifact diff will see none, by design.
- `scripts/dispatch-docs --check` is exercised here against the local
  tree; full `make check`/`make lint` and the CI docs-drift job remain
  the end-to-end gates and are owned by WI-13.
- `docs/key-registry.md` is asserted by its own `:unit` test, not by
  `scripts/dispatch-docs`; the two gates stay separate.
