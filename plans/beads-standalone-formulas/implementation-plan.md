---
schema: beads.standalone-formulas.implementation-plan.v1
artifact: implementation-plan
status: draft-for-review
scope: planning-only
implementation: out-of-scope-until-signoff
requirements: requirements.md
design: design.md
decomposition: decomposition.md
---

# beads.el Standalone-first Formula → Molecule Workflow — Implementation Plan

This plan sequences the `WI-SF-*` work items from `decomposition.md` for
execution on top of PR #67 (`wi18-integration`). It is **pruning-first**: every
wave starts by removing a duplicate or stale path before adding a surface, so
the codebase gets no wider while it gains the workflow porcelain.

## Summary

- **Thesis:** extend PR #67's seams, don't fork them. The command classes,
  `beads-formula-launch`, `beads-formula-var-reader`, `beads-sling-*`,
  `beads-thing` and the async reader are reused; this plan adds the molecule
  work loop and the UI around gates, wisps, bonding, authoring and hand-off.
- **Shape:** six waves, pruning-first, with WI-SF-13 as the acceptance gate
  (no-gascity guard + tests + TRAMP, extended to the swarm modules) before
  WI-SF-14 (docs).
- **No `.el` change in this task.** This document binds a later, separately
  signed-off implementation.

## Requirement traceability

Full matrix in `decomposition.md` §Traceability. Summary: 44 `REQ-SF-*`
requirements across **nine** gaps (the original eight plus swarm, gap #9), each
mapped to at least one of 18 WIs. The critical path is
`WI-SF-05 → WI-SF-01 → WI-SF-02 → WI-SF-13 → WI-SF-14`; the swarm chain
(`WI-SF-15 → WI-SF-16/17 → WI-SF-18`) is parallel and joins at WI-SF-12/13.

## Current System

- **Issue porcelain:** dashboard, list, show, dispatch, sling, agents (PR #67).
- **Execution/parse layer:** `beads-command-{formula,cook,mol,gate,prime}*`
  command classes; `beads-formula.el` browser/detail and the
  `beads-formula-launch` / `beads-formula-var-reader` seams.
- **Missing:** molecule execution view + work loop, gate UI, wisp UI, bonding
  UI, formula authoring, hand-off/context; the instantiate path is a single
  implicit `bd mol pour`.
- **Swarm:** `beads-command-swarm.el` has the four command classes and a
  minimal `beads-swarm` transient (maintenance menu only), but no designed
  list/status/validate surface, no typed result classes, and the classes parse
  raw strings. `bd swarm` is the documented way to work a large epic in
  parallel, so it is treated as gap #9.
- **Known foot-gun:** `beads-command-cook` and `beads-command-prime` live in
  `beads-command-misc.el`, against the one-file-per-subcommand convention.

### Test surface

`lisp/test/beads-*-test.el`; `:unit` mocks `beads-command-execute`,
`:integration` uses `beads-test-with-temp-repo*`. `eldev test` locally,
`eldev -s -dtT test -U coverage/codecov.json '(not (tag :integration))'` for CI
coverage. New tests follow the same `:tags` discipline.

## Proposed Implementation

### Sequencing overview (six waves, all pruning-first)

| Wave | Goal | WIs | Prune first |
|---|---|---|---|
| 0 | Remove dead/duplicated paths | (prep) | move `cook`/`prime` classes out of `beads-command-misc.el`; delete any pour-only shortcut in `beads-formula-launch-standalone`; drop dead dispatch entries |
| 1 | Formula facts | WI-SF-05, WI-SF-04 | collapse duplicate formula-list code paths |
| 2 | Molecule execution | WI-SF-01, WI-SF-02 | remove per-command generated `beads-mol-*`/`beads-gate-*` transients from the primary path |
| 3 | Phase + composition | WI-SF-03, WI-SF-07, WI-SF-08 | remove the implicit pour-only instantiate path |
| 4 | Gates + authoring + hand-off + swarm | WI-SF-06, WI-SF-09, WI-SF-10, WI-SF-11, WI-SF-15, WI-SF-16, WI-SF-17, WI-SF-18 | collapse `bd setup` string parsing into the context view; add typed swarm results before the swarm porcelain |
| 5 | Integration + acceptance | WI-SF-12, WI-SF-13, WI-SF-14 | — |
| 6 | Cross-repo ownership | WI-SF-19 | — |

### Wave 0 — Prune and relocate

1. **Move** `beads-command-cook` → `beads-command-cook.el` and
   `beads-command-prime` → `beads-command-prime.el`; update `require`s and
   autoloads. No slot or behaviour change (drop-in; `beads-command-misc.el`
   keeps the other commands).
2. **Remove** the pour-only shortcut in `beads-formula-launch-standalone`; it
   will be replaced in Wave 3 by the explicit phase flow. Keep
   `beads-formula-launch` (used by sling) delegating to `pour`.
3. **Remove** any `beads-mol-*`/`beads-gate-*` auto-transient from the primary
   key path (they stay reachable only from `beads-mol` / `beads-gate` parent
   menus, per the PR #67 demotion rule).
4. Confirm `beads-command-misc.el` and `Eldev` autoload lists still compile.

Verification: `eldev compile`, `eldev test -f beads-command-misc-test.el`.

### Wave 1 — Formula facts

- **WI-SF-05:** add scope (`project`/`user`/`all`), source path, shadowed
  marker and phase column to the browser; phase/version/`extends`/aspects/bond
  points/gate/`waits_for` to the detail. Reuse `beads-formula-detail-sections`;
  do not re-parse the TOML. Surfacing unknown TOML keys is out of scope
  (deferred to an upstream `bd` change); render only warnings `bd` emits.
- **WI-SF-04:** `beads-cook.el` + the split class; cook transient and a
  `--dry-run` step-tree parse.

Verification: unit tests over fixtures for the summary/detail JSON; a cook
dry-run fixture; `bd formula show --json` parity.

### Wave 2 — Molecule execution (the heart)

- **WI-SF-01:** `beads-molecule.el` view. Aggregate `mol current`/`progress`/
  `show` async with `:cache-key`; render root, progress, steps, gates, output;
  state generic; faces; `beads-thing` movement; range windows.
- **WI-SF-02:** work loop. `n`/`c`/`x`/`r`/`C`/refresh; required close reason;
  close-eligible dry-run then apply; hooks for `a`/`b`/`G`.

Verification: unit tests for the transition table with mocked commands;
integration `bd init` temp repo: create epic + children, open view, claim,
close, close-eligible; ready-only filter.

### Wave 3 — Phase + composition

- **WI-SF-03:** `beads-formula-instantiate`, explicit pour/wisp/root-only,
  typed vars + validation, `--assignee`/`--for`, dry-run preview, follow into
  the molecule view. `beads-formula-launch` delegates with `pour`.
- **WI-SF-07:** `beads-wisp.el` list/lifecycle; phase badges.
- **WI-SF-08:** `beads-bond.el` flow + preview + bond-point attachment.

Verification: unit tests for validation (required/pattern/enum/numeric) and
phase recommendation; integration cook→pour→work and cook→wisp→squash; bond
two formulas dry-run + apply; wisp old-detection fixture.

### Wave 4 — Gates, authoring, hand-off

- **WI-SF-06:** `beads-gate.el` list/detail/actions + molecule integration.
- **WI-SF-09:** `beads-formula-edit.el` scaffold, TOML mode, schema completion
  + validation, diagnostics, convert.
- **WI-SF-10:** distill from the epic view.
- **WI-SF-11:** `beads-handoff.el` hand-off envelope, `M-x beads-context`
  (prime/memories/setup status/policy), split `prime` class.
- **WI-SF-15:** swarm result types in `beads-types.el`, `:result` on the four
  `beads-command-swarm-*` classes, and `beads-swarm-domain-error-p` for the
  exit-0 `{error: …}` payloads. Additive to `beads-command-swarm.el` only.
- **WI-SF-16:** `beads-swarm.el` fleet list + status board + worker lanes.
- **WI-SF-17:** swarm validate / waves / `--verbose` graph.
- **WI-SF-18:** swarm create+coordinator, assign/claim/hand-off, navigation;
  rewire the existing `beads-swarm` transient suffixes.

Verification: unit gate-command assembly; integration gate create→resolve→ready
and gate check `--dry-run`; schema-validator fixtures; distill a temp epic and
re-cook it; prime/setup formatting fixtures; hand-off starts the mock backend;
swarm domain-error fixtures and wave/worker math (unit), plus an integration
epic→validate→create→status→claim→close round-trip.

### Wave 5 — Integration, acceptance, docs

- **WI-SF-12:** dispatch/maintenance entries (swarm is promoted out of
  Maintenance onto the primary dispatch and the epic/molecule views),
  autoloads, `C-c b` extension map, faces, mode-lines; `beads-mol` parent
  entries.
- **WI-SF-13:** `beads-standalone-guard-test.el` (including `beads-swarm.el`
  and `beads-swarm-worker-source`); full unit/integration suite; TRAMP
  verification of every view (swarm included); render guard green;
  byte-compile; lint.
- **WI-SF-14:** `NEWS.md`, PR #67 cross-references, plan status.

Verification: the acceptance command set below.

### Wave 6 — Cross-repo ownership (WI-SF-19)

Audit gascity.el for beads-native logic it reimplements and move it into the
beads.el seams landed in Waves 1–4 (candidate hotspots:
`gascity-formula--validate-values` / `--enum-choices` vs
`beads-formula-var-reader`; formula catalog/recipe reading vs
`beads-command-formula`; terminal/tmux already moved by PR #67). gascity.el is
reduced to thin caller/shim modules. As with all cross-repo work, the
`gascity.el` edits are tracked and executed in the gascity.el rig, blocked on
WI-SF-13; no gascity.el file is written from the beads.el worktree.

Verification: gascity.el byte-compiles and its tests pass against the new
beads.el seams; a documented before/after module-ownership list with no
duplicated helper bodies.

## Execution constraints (operator, 2026-10-07)

These bind the implementation run and take precedence where they conflict
with a non-goal above:

1. **Base branch.** PR #67 (`wi18-integration`) is merged to `main`; the
   implementation branches from `main` (not `wi18-integration`) and publishes
   as its own PR, not by reopening #67. The plan's references to
   `wi18-integration` are historical (its seams are now on `main`).
2. **Live end-to-end acceptance for every WI.** No WI closes on unit tests
   alone. Each user-visible WI is exercised in a **live, non-graphical Emacs**
   (`emacs -nw -Q`) running inside tmux, loaded from the WI's worktree
   `lisp/`, against the `~/bright-lights` city (local and
   `/ssh:localhost:~/bright-lights`), per the WI-20 method in
   `docs/qa/2026-10-04-beads-ui-redesign-wi20-live.md`. WI-SF-13 is the
   aggregate acceptance gate; the per-WI live evidence is a prerequisite for
   it. Load the worktree `lisp/` plus the eldev vui/sesman package dirs, on a
   dedicated tmux session; record the buffer/state observed as bead evidence.
3. **No cross-repo duplication (REQ-SF-100).** beads.el owns the generic
   `bd`-driven machinery; gascity.el owns only `gc`-specific enrichment. Any
   duplicated beads-native logic found in gascity.el is moved to beads.el and
   gascity.el becomes a thin caller (WI-SF-19, in the gascity.el rig). New
   code must not add a second implementation of anything gascity.el already
   has, or vice versa.

## Non-Goals

- Source changes in this planning task.
- New `bd` subcommands/flags; new hard dependencies.
- Gascity code **inside the beads.el worktree**; only the seam list and the
  guard. WI-SF-19 lands its gascity.el edits in the gascity.el rig (REQ-SF-100).
- Redesigning PR #67 surfaces (formula browser/detail, sling, agents,
  terminal).

## Verification

Acceptance gates for the implementation (all must pass):

```
eldev -p -dtT compile
eldev -p -dtT lint
eldev -p -dtT test
eldev -s -dtT test -U coverage/codecov.json '(not (tag :integration))'
eldev test -f beads-standalone-guard-test.el
eldev test -f beads-molecule-test.el
eldev test -f beads-gate-test.el
eldev test -f beads-formula-edit-test.el
eldev test -f beads-swarm-test.el
```

Plus, as a manual acceptance gate (per AGENTS.md): open dashboard, formula
browser/detail, molecule view, gate list, wisp list, bond and context over the
standing local TRAMP store (`/ssh:localhost:~/bright-lights`) in a fresh
tmux Emacs and confirm parity, and confirm every flow with gascity absent.

**No-gascity acceptance (REQ-SF-080, REQ-SF-097):** with only `bd` installed
and gascity absent from the load path, run the full phases: formula browse →
cook preview → instantiate pour → claim → close → close-eligible; instantiate
wisp → squash; gate create → check → resolve → ready; bond; distill; author;
hand-off to the mock backend; **swarm validate → create → status board → claim
→ close → complete**. Any gascity symbol on a default path fails the guard
test.

## Risks (implementation-time)

| Risk | Mitigation | Owner WI |
|---|---|---|
| `bd mol current` output shape drift | defensive parse via the existing result type; fixture test pinned to the CI `bd` version | WI-SF-01 |
| N+1 synchronous commands in the molecule view | async aggregator, per-section boundaries, cache key | WI-SF-01 |
| `bd formula schema` field coverage incomplete | validator degrades to warnings, never blocks save without a diagnostic; schema is the source | WI-SF-09 |
| TOML editing without a hard dep | `toml-mode` when present, `conf-mode` fallback; validation from `bd` | WI-SF-09 |
| Key conflicts with PR #67 | per-surface tables in design.md §9; conflicts resolved in plan-review before implementation | WI-SF-12 |
| Swarm domain errors swallowed by exit 0 | `beads-swarm-domain-error-p` is the single detector; `create`/`validate` fixtures for `already exists` / `not swarmable` / `swarmable=false` | WI-SF-15 |
| Worker lanes issue N `bd list` calls | one call per distinct active assignee, coalesced by `(swarm-workers id)`; lanes render progressively | WI-SF-16 |
| Destructive wisp/burn/purge | dry-run + typed confirm; never bare-key | WI-SF-07 |
| gascity creep | guard test + code review | WI-SF-13 |
| TRAMP wrong-side I/O | render guard + remote acceptance run | WI-SF-13 |

## Rollback

Each WI is an isolated commit on the integration branch (now based on `main`
after PR #67 merged) with its own tests. Rollback
is `git revert <wi-commit>`: the surfaces are additive and nothing in PR #67's
existing behaviour changes except (a) `beads-formula-launch-standalone`'s
pour-only shortcut (restored by revert), (b) the `cook`/`prime` class file move
(pure relocation), and (c) the instantiate key `s` (revert restores the single
`bd mol pour` path). The swarm chain is purely additive: the four command
classes keep their slots, the `beads-swarm` transient keeps its keys (only the
suffixes change target), and WI-SF-15 only adds `:result` + a helper. No data
migration is involved; `bd` owns all state.

## Follow-ups (post-implementation, tracked separately)

- Split the remaining `beads-command-misc.el` commands into per-subcommand
  files if the audit gate calls for it.
- Reconcile any `plan-review.md` deferred findings with the implementation.
