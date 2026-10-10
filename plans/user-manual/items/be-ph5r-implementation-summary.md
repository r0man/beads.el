---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-gzp7
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
    - path: beads/be-ph5r
      hash: bead:be-ph5r
      ids: [REQ-007, REQ-008, REQ-010]
    - path: plans/user-manual/requirements.md
      hash: sha256:937abb5961549361d93d4ff428066c36a12b7a1062bdc407b1d47cf8c03ce1e5
    - path: plans/user-manual/implementation-plan.md
      hash: sha256:c9ab161d8c4f9d6737b885fad4b4f565fdf634ac3c805d248696439c209d920a
    - path: plans/user-manual/plan-review.md
      hash: sha256:a74c8dabbf7ceb73a82ec2c407066c32658a12a5fba8b578bf11089fcfc9585f
    - path: AGENTS.md
      hash: sha256:74158cc19937453a2bb6c29ca9652e5fb4d6f8ca45a494cdc20a2601ed1f0aa4
    - path: NEWS.md
      hash: sha256:ae6593031cf81e237b23c8ed459db6547e7d75faa36dee32782178250adeb6e9
    - path: Eldev
      hash: sha256:5f5a1d002c90f1313c6ae42a9cc62e3ca367ed520985e1804b9e38c7b20742b7
    - path: docs/manual/screenshots/capture.sh
      hash: sha256:c8a1eb969494012bce7223d66f466a6e4c112d24d46f5a1e4ed609529785d930
    - path: lisp/test/beads-manual-test.el
      hash: sha256:b14099b808ea1101c98d7a582ac365e5a7299d8cd063fd58a7fba1c8aba35ec8
  coverage:
    - id: REQ-007
      status: covered
    - id: REQ-008
      status: covered
    - id: REQ-010
      status: covered
---

# Implementation Summary — be-ph5r (W7: agent instructions, NEWS, and manual tests)

## Summary

W7 adds the agent-facing instructions, the `NEWS.md` entry, and the manual
test suite for the user-manual and screenshot pipeline (implementation plan
WI-6; REQ-007, REQ-008, REQ-010; plan-review P2, P3). The changes were made in
the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-ph5r` and committed as
`a06acb4f0b198a33aa012051deeeedc1342b94ee`.

W7 depends on W1–W6. The worktree was created at `origin/main` (`c551853`),
which does not carry the pipeline (manual pages, fixture, engine, formula,
images), so the branch fast-forwards to the completed W6 tip (`743f705`),
whose history already merges W1 (`be-0n84`), W2/W5 (`be-hedp`), and the
user-manual-screenshots formula (W4). The W7 commit is therefore a clean
descendant of all dependencies.

| ID | Status |
| --- | --- |
| REQ-007 | covered |
| REQ-008 | covered |
| REQ-010 | covered |

## Intended Behavior

- **REQ-008 (agent instructions).** `AGENTS.md` gains a "User manual and
  screenshots" section that names the `user-manual-screenshots` formula, the
  throwaway `demo-<role>` fixture (`fixture.sh` +
  `fixtures/beads-fixture.jsonl`), the single `scope` variable
  (`all` | `changed` | comma-separated subset), the committed image
  location/naming (`docs/manual/images/<view>-<theme>.png` plus
  `-thumb.png`), the exact regenerate + verify commands
  (`make -C docs/manual screenshots`,
  `docs/manual/screenshots/capture.sh --scope <scope>` /
  `--verify <scope>` / `--verify-scope <scope>`), the reproducibility check
  (`--verify` fails, naming the drifting image, unless output is
  byte-identical), and a one-line pointer to the cross-repo contract in
  `docs/manual/screenshots/README.md`.
- **REQ-010 (conventions).** `NEWS.md` gains an "Unreleased" entry for the
  manual and pipeline. `lisp/test/beads-manual-test.el` is the only new
  Elisp; no `lisp/` product code changes.
- **REQ-007 (reproducibility guard).** The `:integration` `--verify` test
  drives `capture.sh` with a fake engine that produces a different raw on
  each run and asserts the verifier exits non-zero and reports
  `verify FAILED`, so the check cannot silently pass.
- **P2 (isolation-list drift).** The `:unit` drift test runs
  `fixture.sh print-isolation-vars` and asserts the hardcoded list equals
  `beads-test-isolation-env-vars`, so the two cannot silently diverge.
- **P3 (subset isolation).** The `:integration` `--verify-scope` test
  produces only an out-of-scope raw and asserts the digest/mtime isolation
  check passes; the invalid-scope test asserts `--scope <invalid>` fails
  before capturing.
- **Testability.** `capture.sh` gains a resolution-only `--list-views`
  seam: it resolves `scope` and prints the selected views without a fixture,
  compositor, or capture, so the `:unit` scope-parser tests need no `bd`,
  sway, or graphical Emacs.

## Changed Files

- `AGENTS.md` — new "User manual and screenshots" section (formula, fixture,
  `scope`, image naming, regenerate + verify commands, reproducibility check,
  cross-repo pointer). `sha256:74158cc1…0aa4`.
- `NEWS.md` — "Unreleased" entry describing the manual and pipeline.
  `sha256:ae659303…b6e9`.
- `Eldev` — exclude `docs/manual/screenshots/**` from all eldev build
  targets; those two `.el` files are standalone capture scripts run with
  `emacs -Q` and need their own load path, so `eldev compile` otherwise
  failed on `(require 'screenshot)`. `sha256:5f5a1d00…42b7`.
- `docs/manual/screenshots/capture.sh` — add `--list-views` (resolution-only
  output plus usage text) and fix `changed_views` so a partial `changed`
  selection does not trip `set -e` on the last non-selected view.
  `sha256:c8a1eb96…d930`.
- `lisp/test/beads-manual-test.el` — new test file: fixture JSONL parses,
  role ids unique, dependency targets exist, the scope parser
  (`all`/`changed`/subset), every manifest view has a shot thunk and a manual
  page, every referenced `images/*.png` exists, the P2 isolation-list drift
  assertion, and the three `:integration` shell checks.
  `sha256:b14099b8…5ec8`.
- `plans/user-manual/items/be-ph5r-implementation-summary.md` — this artifact.

## Verification

Run from the worktree `/home/roman/workspace/beads.el/worktrees/be-ph5r`
(worktree HEAD `a06acb4`, `pwd -P` equals the worktree path):

1. **First verification command** — `eldev test -f beads-manual-test.el` —
   result **pass** (`EXIT=0`):
   `Ran 13 tests, 13 results as expected, 0 unexpected`.
2. `eldev test -f beads-manual-test.el '(tag :unit)'` — result **pass**:
   `Ran 10 tests, 10 results as expected, 0 unexpected`.
3. `eldev compile` — result **pass** (`EXIT=0`, all `lisp/*.el` compiled).
4. `eldev -p -dtT lint` — result **pass**:
   `Linters have no complaints`.
5. `eldev test` — result: `Ran 6351 tests, 6324 results as expected, 1
   unexpected, 26 skipped`. The single failure is
   `beads-audit-gate-no-new-slot-drift`, an **`:integration` CLI-parity gate**
   that reads the installed `bd` (1.3.1 dev) and reports
   `list --include-ephemeral`, `purge --limit`, `purge --wisps-plane` as new
   flags. It reproduces at the dependency tip `743f705` with all W7 changes
   stashed, so it is pre-existing environmental drift, not caused by W7; the
   CI-pinned `bd` does not expose those flags. Fixing it belongs to a
   command-parity work item because W7 may not change `lisp/` product code.
6. **Final proof command** — from the launcher rig root
   `/home/roman/workspace/beads.el`:
   `GC_BEAD_ID=be-gzp7 .gc/scripts/checks/build-artifact-valid.sh` — result
   **pass**: `build artifact valid: schema=gc.build.implementation-summary.v1`.

## Remaining Risks

- **`--list-views` is a new capture.sh seam.** It is additive (resolution and
  early exit before any fixture/compositor work) and documented in the script
  help; it does not change the capture, verify, or compress paths.
- **`changed_views` errexit fix.** The previous form ended the function with
  `[ "$hit" = 1 ] && ...`, whose non-zero status aborted the whole script when
  the last manifest view was not selected. The fix uses an `if` so a partial
  `--scope changed` selection works; the `:unit` `changed` test guards it.
- **`eldev test` has one unrelated `:integration` failure** (the bd
  command-parity gate) that also fails on the untouched dependency tip in
  this environment. It is recorded here rather than fixed, per W7's
  no-product-code boundary.
- **`beads-manual-scope-changed-maps-views`** temporarily appends to
  `docs/manual/views.org` and restores it in an `unwind-protect`; the test is
  serial within ERT, so it cannot race another test.
