---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-dhu31
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
    - path: beads/be-s61jf
      hash: bead:be-s61jf
      ids: [REQ-014, REQ-015]
    - path: plans/manual-demo-consistency/requirements.md
      hash: sha256:5a665190a49fa226ab49e221ddb10aa108f236b9a0e18e3b8079b8213ff3fd21
    - path: plans/manual-demo-consistency/implementation-plan.md
      hash: sha256:dd851b8ba3f345740b22622682f215a2467c16a38589fdb62698af98a6d10ecd
    - path: docs/manual/screenshots/fixtures/beads-fixture.jsonl
      hash: sha256:6504c508bd25f3a2efc8a0e99bc940767b3306ae41352077fa446d2d6c319ca8
    - path: NEWS.md
      hash: sha256:cdd57fadad13a29bb41ff920c40142516cbb31712214ff703744f7d02a2d9ce2
    - path: docs/manual/screenshots/README.md
      hash: sha256:0c01066a9390fdb9133c0319ee5776bc2a0f7047f05dfeab7cace230b716695c
    - path: lisp/test/beads-manual-test.el
      hash: sha256:32b02bc307338dd0def42b155377ab5ac2af115584b7b2a6f8caf8c9710cdf2f
  coverage:
    - id: REQ-014
      status: covered
    - id: REQ-015
      status: covered
---

# Implementation Summary: W8 — pin the manual contracts, keep gates green

## Summary

Work item W8 (implementation plan WS-8; requirements REQ-014 and REQ-015) owns
the publishable green state for the shared-demo-project manual change: the new
contracts are pinned in the test suite, the documentation records the
authoritative fixture/theme facts, and every required gate passes in the
worktree.  Publication itself belongs to the workflow's publish stage, so this
item makes no push.

`lisp/test/beads-manual-test.el` now:

- compares every capture-manifest theme slot against the `default` sentinel and
  resolves that sentinel through `beads-shots-theme` to nil (no theme loaded),
  so the manifest cannot claim a theme the capture path does not load;
- gates the capture, test, and build inputs against a third-party theme
  requirement (`use-package ...-themes`, `emacs-...-themes`, or a literal
  `screenshot-theme-library`) through `beads-manual-test--theme-violations`,
  while leaving the verbatim engine docstring's `ef-themes` citation out of the
  load check;
- adds `beads-manual-image-orphans` + `beads-manual-test--orphan-images`, the
  reverse of the existing reference-exists gate: every committed full-size and
  `-thumb` PNG must be embedded by some manual page;
- adds an injected-violation test beside each new gate (`*-detects-injected`)
  so a gate that stops detecting its violation fails even when the tree is
  clean;
- adds `beads-manual-fixture-recreates-stale-marker-project`, proving the
  fixed shared path is torn down and recreated (with one stdout line) when a
  marked project is left behind;
- keeps the fixture path/marker/teardown/lock tests and the
  `FIXTURE_DATA_SHA256`-versus-README/manual assertion that WS-1 added, so a
  data or documentation edit cannot drift silently.

`NEWS.md` gains an `Unreleased` entry describing the documentation change (no
runtime behavior), and `docs/manual/screenshots/README.md` now states the
cross-repo fixture contract explicitly: `${TMPDIR:-/tmp}/demo-project`, the
`demo-1` … `demo-12` ids, and the sibling `gascity.el` `demo-rig` /
`/tmp/gascity-fixture` correspondence.  The WS-5 reference/keybinding gate and
the WS-6 walkthrough test are merged from their source worktrees and stay green.

## Intended Behavior

- A capture with no `BEADS_SHOT_THEME` (or `default`) loads no theme and uses
  Emacs's built-in default faces; the `-dark`/`-light` activations are
  link-stability slots, not theme claims.
- No theme package appears in the capture, test, dev init, or Guix build input,
  and no literal theme library is baked into the pipeline.
- Every committed manual image is still shown by a manual page; adding an
  unreferenced PNG fails `make check`.
- The reference and walkthrough gates continue to prove the documented
  command/keybinding surface and the shared four-step demo flow.

## Changed Files

- `lisp/test/beads-manual-test.el` — manifest-theme sentinel resolution, the
  no-third-party-theme gate, `beads-manual-image-orphans`, the stale-marker
  recreation test, their injected-violation companions, the manifest parsing
  helper, and the shared theme/orphan helpers.
- `NEWS.md` — `Unreleased` entry for the shared fixture, default-face
  screenshots, completed manual, and QA report.
- `docs/manual/screenshots/README.md` — authoritative cross-repo fixture
  contract (path, ids, sibling `demo-rig`/`gascity-fixture` mapping).

No runtime source file, command class, or user-facing behavior changed.

## Verification

The first verification was the new test set in isolation, then the full suite:

```text
emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-test.el \
  --eval '(ert-run-tests-batch-and-exit "beads-manual-\\(manifest-theme-consistency\\|no-third-party-theme\\|image-orphans\\|fixture-dataset-hash-recorded\\)")'
# 6 tests, 6 results as expected, 0 unexpected

make lint
# PASS — byte-compiled 130 file(s)

make check
# PASS — 6711 tests, 6685 results as expected, 0 unexpected, 26 skipped (live UI)

make check-manual
# PASS — forbidden host/city gate clean

scripts/dispatch-docs --check
# PASS — dispatch docs ok

docs/manual/screenshots/fixture.sh verify
# PASS — 12 role-keyed issues OK

docs/manual/screenshots/capture.sh --verify all
# PASS — verify: byte-identical for 18 image(s) (scope all)
```

The injected-violation checks are automated: a temp unreferenced PNG must be
reported by the orphan gate, a temp file with `use-package ef-themes` must be
reported by the theme gate (while the engine docstring citation is ignored),
an `ef-cyprus` manifest slot was exercised manually and failed the sentinel
gate, and a changed README digest failed the dataset-hash gate.

Coverage table:

| ID | Status |
| --- | --- |
| REQ-014 | covered |
| REQ-015 | covered |

## Remaining Risks

- **GUI-only capture gate.** `capture.sh --verify all` needs `sway`, a
  graphical Emacs, and ImageMagick; it passed locally but is not part of the
  CI matrix, so a CI-only regression in the compositor path would not be
  caught there.
- **Link-stability slots are byte-identical by design.** A future re-theming
  must change both the manifest/theming code and this gate deliberately; the
  test intentionally fails if a slot stops resolving to the `default` sentinel.
- **Sibling repository adoption.** The README/manual state the `gascity.el`
  contract, but gascity.el must adopt the same fixture path and vocabulary in
  its own change; beads.el cannot enforce it from this repository.
- **Plan artifacts are untracked in the rig root**, matching the W1–W7
  summaries; the hashes above were recorded from the workspace copies before
  the worktree commit.
