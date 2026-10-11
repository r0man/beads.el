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
    - path: plans/user-manual/implementation-plan.md
      hash: sha256:c9ab161d8c4f9d6737b885fad4b4f565fdf634ac3c805d248696439c209d920a
    - path: plans/user-manual/requirements.md
      hash: sha256:937abb5961549361d93d4ff428066c36a12b7a1062bdc407b1d47cf8c03ce1e5
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

W7 adds the agent-facing manual/screenshot documentation, the `NEWS.md`
entry, and the manual test suite (implementation plan WI-6; REQ-007, REQ-008,
REQ-010; plan-review P2, P3; AC-7, AC-8, AC-10). The implementation commit is
`a06acb4f0b198a33aa012051deeeedc1342b94ee` in the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-ph5r`:

- `AGENTS.md` — new "User manual and screenshots" section naming the
  `user-manual-screenshots` formula, the throwaway `demo-<role>` fixture, the
  single `scope` variable (`all` / `changed` / comma-separated subset), the
  `docs/manual/images/<view>-<theme>[-thumb].png` naming, the exact
  regenerate + verify commands, the `--verify` reproducibility check, and a
  pointer to the authoritative cross-repo contract in
  `docs/manual/screenshots/README.md`.
- `NEWS.md` — an "Unreleased" entry for the task-organized manual and the
  reproducible screenshot pipeline.
- `lisp/test/beads-manual-test.el` — the manual test suite (13 tests): fixture
  parsing/role-id/dependency checks, the capture scope parser, the
  manifest↔shot↔page↔image contract, the P2 isolation-list drift assertion,
  and the shell-level `--scope`/`--verify`/`--verify-scope` checks.
- `docs/manual/screenshots/capture.sh` — a resolution-only `--list-views`
  seam (print the resolved view names and exit before any fixture/compositor
  work) so the scope parser is unit-testable without `bd`, sway, or Emacs,
  plus a `set -e` fix that lets `--scope changed` complete with an empty
  selection.
- `Eldev` — exclude `docs/manual/screenshots/**` from the package lint/build
  fileset: `screenshot.el`/`beads-shots.el` are standalone capture scripts run
  by `capture.sh` with `emacs -Q`, not package sources.

The W7 worktree was created at `origin/main` (`c551853`), which does not carry
the manual, fixture, formula, or pipeline it must test. To make the tests
self-contained, the branch merges the completed dependency worktrees before
W7: W1 (`be-1sab`, fixture), W2 (`be-ec91`, engine/orchestrator) + W5
(`be-hedp`, contract README), W3 (`be-0n84`, manual pages), W4 (`be-rf87`,
formula), and W6 (`be-z028`, images + `--verify-scope`). Those commits are
ancestors of the W7 commit.

## Intended Behavior

- **REQ-007 (reproducible, verified).** `capture.sh --verify <scope>` runs a
  scope twice and fails, naming each drifting image, unless the raw output is
  byte-identical. `beads-manual-verify-detects-drift` drives that path with a
  stub engine (`BEADS_SHOT_ENGINE`) whose second run writes different bytes
  and asserts the non-zero exit and `verify FAILED` message, so the check can
  fail loudly rather than passing unconditionally. The P3
  `--verify-scope` subset-isolation digest/mtime guarantee is covered by
  `beads-manual-verify-scope-leaves-non-selected`.
- **REQ-008 (agent instructions).** `AGENTS.md` names the formula, fixture,
  `scope` variable, image location/naming, exact regenerate + verify commands,
  and the reproducibility check; it points at
  `docs/manual/screenshots/README.md` as the single cross-repo contract.
- **REQ-010 (repo conventions hold).** `eldev compile` and
  `eldev -p -dtT lint` are clean and `eldev test -f beads-manual-test.el` is
  13/13; `NEWS.md` gains an "Unreleased" entry.
- **P2 (isolation list).** `beads-manual-isolation-list-matches-fixture`
  runs `fixture.sh print-isolation-vars` and asserts the hardcoded four-name
  list equals `beads-test-isolation-env-vars`, so the shell list cannot
  silently drift from the Emacs constant.
- **No product `lisp/` change.** The only Elisp added is the test file; the
  shot list (`beads-shots.el`) is the W2 artifact, copied unchanged.

## Changed Files

- `AGENTS.md` (+33) — "User manual and screenshots" section.
- `NEWS.md` (+13) — "User manual and reproducible screenshot pipeline"
  Unreleased entry.
- `Eldev` (+5/-1) — exclude `docs/manual/screenshots/**` from the standard
  fileset.
- `docs/manual/screenshots/capture.sh` (+14/-1) — `--list-views` argument,
  parse, and resolution-only exit; `set -e`-safe `changed` selection.
- `lisp/test/beads-manual-test.el` (new, 343 lines) — 10 `:unit` and 3
  `:integration` tests.
- `plans/user-manual/items/be-ph5r-implementation-summary.md` — this artifact.

## Verification

Run from the worktree `/home/roman/workspace/beads.el/worktrees/be-ph5r`:

1. **First verification command** — `eldev test -f beads-manual-test.el` —
   result **pass**: `Ran 13 tests, 13 results as expected, 0 unexpected`.
2. `eldev compile` — result **pass** (package sources byte-compiled with no
   errors).
3. `eldev -p -dtT lint` — result **pass**: `Linters have no complaints`.
4. `bash -n docs/manual/screenshots/capture.sh` — result **pass** (no syntax
   errors); `--help` lists `--list-views`.
5. `eldev test` (full suite) — result **one pre-existing environmental
   failure**: `Ran 6351 tests, 6324 results as expected, 1 unexpected, 26
   skipped`; the sole failure is `beads-audit-gate-no-new-slot-drift`, which
   reports CLI/slot drift for the installed `bd`
   (`list --include-ephemeral`, `purge --limit`, `purge --wisps-plane`). No
   product class was touched by W7, so this is the pinned-`bd`-version parity
   gate, not a regression from this commit.
6. **Final proof command**, from the launcher rig root
   `/home/roman/workspace/beads.el` after recording this artifact at
   `gc.implementation.summary_path` on the workflow root:
   `GC_BEAD_ID=be-f809 .gc/scripts/checks/build-artifact-valid.sh` — result
   **pass** (`build artifact valid:
   schema=gc.build.implementation-summary.v1`).

| ID | Status |
| --- | --- |
| REQ-007 | covered |
| REQ-008 | covered |
| REQ-010 | covered |

## Remaining Risks

- **Full-suite parity gate is red in this environment.**
  `beads-audit-gate-no-new-slot-drift` (`:integration`) fails against the
  installed `bd` because the live CLI exposes slots the class inventory does
  not model. W7 changes no command class; the gate is expected to be resolved
  by the separate CLI-parity work or recorded in `beads-meta-parity-*`.
- **The `:integration` shell checks use a stub engine, not a GUI capture.**
  They exercise the orchestrator's selection/verify/subset logic without
  sway/Emacs, so they do not re-prove the pixel reproducibility that W6's
  summary already demonstrated with a real headless capture. A host without
  the GUI stack can still run the full `beads-manual-test.el`.
- **`--scope changed` selection depends on the working tree.** The unit test
  pins `BEADS_SHOT_BASE=HEAD` and mutates `docs/manual/views.org`; a caller
  relying on `origin/HEAD` resolution instead needs the remote ref present.
- **Merged dependency branches.** The W7 commit sits on top of the W1–W6
  merge commits; the publish/integration stage should treat W7 as the
  integrating node rather than merging those worktrees again.
