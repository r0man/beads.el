---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-rurdm
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
    - path: beads/be-czzui
      hash: bead:be-czzui
      ids:
        - REQ-003
        - REQ-013
    - path: lisp/test/beads-manual-test.el
      hash: sha256:f310ed9e4fa044fc08f6ef6e31f5a63e286e0c87be542b9caf015dc25d099ef7
    - path: plans/manual-standard/requirements.md
      hash: sha256:0ffcb4eb7f97f2aef5a4416e8810c3b4da0d24f2bcbccf5640fd00a9967202a7
    - path: plans/manual-standard/implementation-plan.md
      hash: sha256:ccfb1a822c2ef5fbe69da8764e4d210ada1271c20520b6817a60bb6f96225498
    - path: plans/manual-standard/decomposition.md
      hash: sha256:015346698dc65d8143b28c77dd4eb8137b445e8896cdbf5163269f443f49f178
  coverage:
    - id: REQ-003
      status: covered
    - id: REQ-013
      status: covered
---

# WI-09 — Update manual tests for `@shot` (`be-czzui`)

| ID | Status |
| --- | --- |
| REQ-003 | covered |
| REQ-013 | covered |

## Summary

`lisp/test/beads-manual-test.el` now understands the bounded `@shot` macro
introduced by WI-02 while remaining compatible with any chapter that still
uses a bare `@image`.  The image-reference resolver returns both the
full-size `images/<name>.png` (the HTML lightbox target) and its
`images/<name>-thumb.png` preview for every `@shot{<name>, ...}` call, and
keeps resolving remaining `@image{images/...}` forms.  The 14-view
image-coverage gate accepts either `@shot{<view>-dark|light` or
`@image{images/<view>-dark|light` so it is preserved across the
conversion.  A new gate rejects any unbounded full-size `@image` in an
illustrated chapter (REQ-003), and a new consistency gate pins the capture
manifest's standard dark theme to the shot engine's `BEADS_SHOT_THEME`
default (REQ-004).  All existing fixture, manifest, scope, isolation and
shell-level tests remain green.

## Intended Behavior

- `beads-manual-image-references-exist` proves that every PNG the manual
  references exists, whether the reference is a `@shot{<name>, ...}` macro
  (which emits `images/<name>.png` plus `images/<name>-thumb.png`) or a
  remaining `@image{images/<file>, ...}` form.  The `@shot` matcher is
  anchored to the line start and filters the macro's own
  `@shot{NAME, CAPTION}` documentation comment so a placeholder name never
  becomes a phantom image reference.
- `beads-manual-manifest-views-have-pages` keeps the 14-view image-coverage
  gate by matching `@shot{<view>-dark|light` as well as the legacy
  `@image{images/<view>-dark|light` needle.
- `beads-manual-no-unbounded-full-size-image` fails if any chapter embeds a
  full-size (non-`-thumb`) `@image`, enforcing REQ-003 for illustrated HTML
  chapters.
- `beads-manual-manifest-theme-consistency` fails if `capture.sh`'s dark
  manifest theme and the `beads-shots.el` `BEADS_SHOT_THEME` default
  disagree, so the theme cannot drift between the two capture inputs.  The
  concrete theme value is switched by WI-04; this gate is value-agnostic and
  therefore green on both sides of that switch.
- `beads-manual-texinfo-has-no-org-image-links`, the fixture tests, the
  scope-parser tests, the isolation-list test and the shell-level
  `--verify`/isolation tests are unchanged and stay green.

## Changed Files

- `lisp/test/beads-manual-test.el` — extended
  `beads-manual-test--image-references` to resolve `@shot` references (and
  their thumbnails); added `beads-manual-test--unbounded-image-references`;
  retargeted `beads-manual-manifest-views-have-pages` to `@shot` or
  `@image`; added `beads-manual-no-unbounded-full-size-image` (REQ-003) and
  `beads-manual-manifest-theme-consistency` (REQ-004).
- `plans/manual-standard/items/be-czzui-implementation-summary.md` — this
  `gc.build.implementation-summary.v1` artifact.

No `lisp/` runtime file is touched (REQ-017).

## Verification

First verification command — the `:unit` subset of `beads-manual-test.el`
from the WI-09 worktree:

```
$ emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-test.el \
    --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'
Ran 13 tests, 13 results as expected, 0 unexpected
```

Observed: **pass** (13/13, including
`beads-manual-image-references-exist`,
`beads-manual-manifest-views-have-pages`,
`beads-manual-no-unbounded-full-size-image` and
`beads-manual-manifest-theme-consistency`).

Final proof command — the same `:unit` subset re-run on the committed tree,
then the artifact gate:

```
$ emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-test.el \
    --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'
Ran 13 tests, 13 results as expected, 0 unexpected

$ GC_BEAD_ID=be-ytpuj .gc/scripts/checks/build-artifact-valid.sh
build artifact valid: schema=gc.build.implementation-summary.v1
```

Observed: **pass**.

## Remaining Risks

- The worktree is based on the WI-02 commit (`156022e7`, the `@shot` macro
  and conversion) so the `@shot` paths are exercised, not merely
  tolerated; a later integration that reverts to bare `@image` would still
  pass the compatibility needles but fail
  `beads-manual-no-unbounded-full-size-image`, which is the intended
  REQ-003 guard.
- `beads-manual-manifest-theme-consistency` checks agreement between
  `capture.sh` and `beads-shots.el`, not a hard-coded
  `ef-deuteranopia-dark`; the concrete theme is WI-04's change and this
  test item must not fail while that sibling workstream is still
  unintegrated.  Once WI-04 lands the two values agree on
  `ef-deuteranopia-dark` and the gate stays green.
- The capture manifest in this worktree still lists `ef-elea-dark` for the
  standard dark row because WI-04 is a separate drain item; the theme gate
  is written to be value-agnostic for exactly that reason.
