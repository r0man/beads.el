---
schema: gc.build.decomposition.v1
workflow:
  id: be-435k
  formula: build-from-plan
methodology:
  pack: gascity
  name: build-from-plan
producer:
  formula: build-from-plan
  stage: decompose
  attempt: 1
status: approved
trace:
  upstream:
    - path: plans/beads-events-live/requirements.md
      hash: sha256:cbf0f460f960262fcc5f9fd9f3dfc9d43fb663603cf052a080dc0e566db0195f
      ids:
        - US-1
        - US-2
        - US-3
        - US-4
        - US-5
        - AC-1
        - AC-2
        - AC-3
        - AC-4
        - AC-5
        - AC-6
        - AC-7
        - AC-8
        - AC-9
        - HC-1
        - HC-2
        - HC-3
        - HC-4
        - OOS-1
        - OOS-2
        - OOS-3
        - OOS-4
        - OOS-5
    - path: plans/beads-events-live/design.md
      hash: sha256:70fd4f04921caff988a522c8f330b8d49e747bfd9555e9e5541716cb19175db2
    - path: plans/beads-events-live/experiments.md
      hash: sha256:f278c5bf659bee6e3a013f644ca6e45d652a1e5cf17c62f56505b79e5463b222
    - path: plans/beads-events-live/implementation-plan.md
      hash: sha256:073574048dce966bb899906f95e92e01e400b27a653f103ecab454ef5829c377
    - path: plans/beads-events-live/plan-review.md
      hash: sha256:52df6a7538f3ecc95a4563ff280ecee36401d3cb10f2c32e26e8957e6dcf91e0
  coverage:
    - id: US-1
      status: covered
    - id: US-2
      status: covered
    - id: US-3
      status: covered
    - id: US-4
      status: covered
    - id: US-5
      status: covered
    - id: AC-1
      status: covered
    - id: AC-2
      status: covered
    - id: AC-3
      status: covered
    - id: AC-4
      status: covered
    - id: AC-5
      status: covered
    - id: AC-6
      status: covered
    - id: AC-7
      status: covered
    - id: AC-8
      status: covered
    - id: AC-9
      status: covered
    - id: HC-1
      status: covered
    - id: HC-2
      status: covered
    - id: HC-3
      status: covered
    - id: HC-4
      status: covered
    - id: OOS-1
      status: out_of_scope
      rationale: "Requirements Out Of Scope: no new bd subcommands or flags; work targets the documented bd events surface only."
    - id: OOS-2
      status: out_of_scope
      rationale: "Requirements Out Of Scope: no daemon, watcher service, or persistent database; the model is in-memory and rebuilt."
    - id: OOS-3
      status: out_of_scope
      rationale: "Requirements Out Of Scope: no direct Dolt table reads and no second journal implementation."
    - id: OOS-4
      status: out_of_scope
      rationale: "Requirements Out Of Scope: no graphical/canvas renderer; text faces and overlays only."
    - id: OOS-5
      status: out_of_scope
      rationale: "Requirements Out Of Scope: no gascity code changes; only an optional, gascity-free attribution/invalidation seam is shipped."
---

# Decomposition: beads-live — live, rewindable event surface

| ID | Status |
| --- | --- |
| US-1 | covered |
| US-2 | covered |
| US-3 | covered |
| US-4 | covered |
| US-5 | covered |
| AC-1 | covered |
| AC-2 | covered |
| AC-3 | covered |
| AC-4 | covered |
| AC-5 | covered |
| AC-6 | covered |
| AC-7 | covered |
| AC-8 | covered |
| AC-9 | covered |
| HC-1 | covered |
| HC-2 | covered |
| HC-3 | covered |
| HC-4 | covered |
| OOS-1 | out_of_scope |
| OOS-2 | out_of_scope |
| OOS-3 | out_of_scope |
| OOS-4 | out_of_scope |
| OOS-5 | out_of_scope |

## Summary

The approved implementation plan (`implementation-plan.md`, WI-LIVE-01…WI-LIVE-21) decomposes into **21 runnable implementation beads** across the plan's seven ordered waves (0–6), tracked by implementation convoy **`be-r2bm`** (`build-from-plan-be-435k-implementation`). Every requirement id in `requirements.md` (`US-1`…`US-5`, `AC-1`…`AC-9`) maps to at least one work item and is `covered`; the plan-local `HC-1`…`HC-4` are `covered` and `OOS-1`…`OOS-5` are `out_of_scope` with rationale, matching the coverage matrix above and the YAML `trace.coverage`.

The dependency graph is a single acyclic graph (`gc bd dep cycles` reports no cycles) and is fully sequential at the wave boundary, as the plan requires: the pure model (Wave 0) has two parallel roots (`WI-LIVE-01`, `WI-LIVE-02`); each later wave consumes the interface defined by the previous one. Critical path: `WI-LIVE-01 → WI-LIVE-03 → WI-LIVE-04 → WI-LIVE-05 → WI-LIVE-06 → WI-LIVE-08 → WI-LIVE-09 → WI-LIVE-10 → WI-LIVE-11 → WI-LIVE-14 → WI-LIVE-15 → WI-LIVE-16 → WI-LIVE-17 → WI-LIVE-18 → WI-LIVE-19 → WI-LIVE-20 → WI-LIVE-21`.

This decomposition adds no new scope beyond the approved plan. It records the plan-review advisories A1–A4 against the affected beads (A1 → WI-LIVE-09; A2 → WI-LIVE-05; A4 → WI-LIVE-16). Every user-visible work item must record live non-graphical Emacs evidence (`emacs -nw -Q` in tmux against a real store, local and `/ssh:localhost:`) before it closes; WI-LIVE-20 is the aggregate acceptance gate and WI-LIVE-21 is the CI/PR gate.

## Selected Downstream Formulas

- `implement` (the `build-from-plan` implementation step, `drain_policy=separate`): drains implementation convoy `be-r2bm` with the `do-work` formula, one full lifecycle per selected non-convoy runnable member, routed to `beads.el/gc.implementation-worker` with per-member exclusive access.
- `do-work`: per-item lifecycle (`prepare-worktree` → `implement` → `close-source-anchor`) for each convoy member; the `implement` step writes a `gc.build.implementation-summary.v1` artifact per item.
- `decomposition-base`: the methodology used here to translate the plan into this convoy and artifact.
- `review` / `fix-loop-base`: inherited by the build continuation after the drain; not work items in this decomposition.

## Implementation Convoy

- Convoy ID: **`be-r2bm`**
- Name: `build-from-plan-be-435k-implementation`
- Members (21, all `task`, all open/runnable): `be-jx25`, `be-gvlu`, `be-bsqd`, `be-jlp3`, `be-lnf7`, `be-xu5u`, `be-px3r`, `be-op0h`, `be-echs`, `be-outd`, `be-69ey`, `be-81ek`, `be-uzxw`, `be-nsep`, `be-jsrq`, `be-lkdd`, `be-dfrh`, `be-b125`, `be-9ntg`, `be-omxu`, `be-lylj`.
- Created new for this decomposition. It is **not** the original launch/workflow-control convoy (`gc.var.convoy_id` = `be-wb4y`, `beads-live implementation (be-uhlw)`, which tracks the request epic `be-uhlw`), and it is not a planning or request convoy.
- Recorded on workflow root `be-435k` as `gc.input_convoy_id` and `gc.build.implementation_convoy_id`, both `be-r2bm`.

## Work Items

Each work item below is a runnable `task` bead in convoy `be-r2bm`. The `Depends on` edges are real `blocks` dependencies created with the beads; the convoy drains in dependency order. Every user-visible WI requires live Emacs-in-tmux evidence as well as its listed automated verification.

| Work item | Bead | Requirements | Depends on |
| --- | --- | --- | --- |
| WI-LIVE-01 Foundations: pure event model (beads-event.el) | `be-jx25` | US-2, AC-4, AC-5, HC-4 | — |
| WI-LIVE-02 Foundations: beads-live options, faces, autoloads | `be-gvlu` | US-2, HC-4 | — |
| WI-LIVE-03 Stream capability probe + canonical keying | `be-bsqd` | US-1, AC-1, AC-2, HC-2 | WI-LIVE-01, WI-LIVE-02 |
| WI-LIVE-04 Stream spawn + transport | `be-jlp3` | US-1, AC-1, AC-2, HC-2 | WI-LIVE-03 |
| WI-LIVE-05 Parse + deliver with sparse-merge semantics | `be-lnf7` | US-1, AC-4, HC-2 | WI-LIVE-04 |
| WI-LIVE-06 Checkpoint, start/resume, re-baseline | `be-xu5u` | US-1, AC-3, HC-2 | WI-LIVE-05 |
| WI-LIVE-07 Stream classification + backoff | `be-px3r` | US-1, AC-3, HC-2 | WI-LIVE-04 |
| WI-LIVE-08 Poll fallback | `be-op0h` | US-1, AC-2, HC-2 | WI-LIVE-06, WI-LIVE-07 |
| WI-LIVE-09 Attach, subscribe, status, invalidation hook | `be-echs` | US-1, US-4, AC-7, HC-2 | WI-LIVE-08 |
| WI-LIVE-10 Command-layer invalidation seam | `be-outd` | US-1, US-4, AC-7 | WI-LIVE-09 |
| WI-LIVE-11 Dashboard live integration | `be-69ey` | US-1, AC-1, AC-2, HC-1 | WI-LIVE-10 |
| WI-LIVE-12 recent-changes dashboard section | `be-81ek` | US-1, US-2, HC-1 | WI-LIVE-11 |
| WI-LIVE-13 List live refresh | `be-uzxw` | US-1, AC-1, AC-2, HC-1 | WI-LIVE-11 |
| WI-LIVE-14 Show live re-render + ACTIVITY section | `be-nsep` | US-1, US-2, HC-1 | WI-LIVE-11 |
| WI-LIVE-15 Timeline + city timeline views | `be-jsrq` | US-2, US-5, AC-4, AC-5 | WI-LIVE-14 |
| WI-LIVE-16 Per-issue history + rewind | `be-lkdd` | US-2, US-3, AC-5, AC-6 | WI-LIVE-15 |
| WI-LIVE-17 Notifications + mode-line pulse | `be-dfrh` | US-4, AC-7 | WI-LIVE-16 |
| WI-LIVE-18 Menus + autoloads | `be-b125` | US-5, HC-1 | WI-LIVE-17 |
| WI-LIVE-19 Standalone guard, gascity seam, NEWS | `be-9ntg` | US-5, AC-8, HC-3 | WI-LIVE-18 |
| WI-LIVE-20 Integration tests + live acceptance | `be-omxu` | AC-1, AC-2, AC-3, AC-6, AC-9 | WI-LIVE-19 |
| WI-LIVE-21 CI gate + PR | `be-lylj` | AC-9 | WI-LIVE-20 |

### WI-LIVE-01 — Foundations: pure event model (beads-event.el) (`be-jx25`)

Wave 0. Requirements: US-2, AC-4, AC-5, HC-4. No dependencies.

**Files:** `lisp/beads-event.el`, `lisp/test/beads-event-test.el`.

**Verify:** `eldev test -f beads-event-test.el` (`:unit`) for level/glyph/subject/diff/fold including sparse-partial and actor-less rows.

### WI-LIVE-02 — Foundations: beads-live options, faces, autoloads (`be-gvlu`)

Wave 0. Requirements: US-2, HC-4. No dependencies.

**Files:** `lisp/beads-custom.el`, `lisp/beads-faces.el`.

**Verify:** byte-compile + `eldev lint` clean; documented options; `:inherit` faces.

### WI-LIVE-03 — Stream capability probe + canonical keying (`be-bsqd`)

Wave 1. Requirements: US-1, AC-1, AC-2, HC-2. Depends on: WI-LIVE-01, WI-LIVE-02.

**Files:** `lisp/beads-live.el`, `lisp/test/beads-live-test.el`.

**Verify:** `:unit` capability detection and stream-key uniqueness (`/ssh:host:/p` == `/p`, `gc` != `bd`); no subprocess.

### WI-LIVE-04 — Stream spawn + transport (`be-jlp3`)

Wave 1. Requirements: US-1, AC-1, AC-2, HC-2. Depends on: WI-LIVE-03.

**Files:** `lisp/beads-live.el`, `lisp/test/beads-live-test.el`.

**Verify:** `:unit` argv/transport tests through `beads-live--spawn`; remote pipe verified in WI-LIVE-20.

### WI-LIVE-05 — Parse + deliver with sparse-merge semantics (`be-lnf7`)

Wave 1. Requirements: US-1, AC-4, HC-2. Depends on: WI-LIVE-04.

**Files:** `lisp/beads-live.el`, `lisp/test/beads-live-test.el`.

**Verify:** `:unit` chunking (partial/non-JSON), sparse merge (absent `is_blocked` preserved; label add two rows; actor-less cascade), debounce/flush.

### WI-LIVE-06 — Checkpoint, start/resume, re-baseline (`be-xu5u`)

Wave 1. Requirements: US-1, AC-3, HC-2. Depends on: WI-LIVE-05.

**Files:** `lisp/beads-live.el`, `lisp/test/beads-live-test.el`.

**Verify:** `:unit` checkpoint read/write, branch/replica invalidation, pruned-`--since` re-baseline.

### WI-LIVE-07 — Stream classification + backoff (`be-px3r`)

Wave 1. Requirements: US-1, AC-3, HC-2. Depends on: WI-LIVE-04.

**Files:** `lisp/beads-live.el`, `lisp/test/beads-live-test.el`.

**Verify:** `:unit` classification table and `(2 5 15 60)` backoff schedule.

### WI-LIVE-08 — Poll fallback (`be-op0h`)

Wave 1. Requirements: US-1, AC-2, HC-2. Depends on: WI-LIVE-06, WI-LIVE-07.

**Files:** `lisp/beads-live.el`, `lisp/test/beads-live-test.el`.

**Verify:** `:unit` poll-vs-stream classification; journal-off store raises no error and reports `poll` (AC-2).

### WI-LIVE-09 — Attach, subscribe, status, invalidation hook (`be-echs`)

Wave 1. Requirements: US-1, US-4, AC-7, HC-2. Depends on: WI-LIVE-08.

**Files:** `lisp/beads-live.el`, `lisp/test/beads-live-test.el`.

**Verify:** `:unit` hooks fire once per applied record (public `beads-event-hooks`); attach refcount and last-detach stop.

### WI-LIVE-10 — Command-layer invalidation seam (`be-outd`)

Wave 2. Requirements: US-1, US-4, AC-7. Depends on: WI-LIVE-09.

**Files:** `lisp/beads-command.el`, `lisp/test/beads-command-test.el`.

**Verify:** `:unit` invalidation clears the single-flight hash; live-active suppresses the foreground refresh; no circular require.

### WI-LIVE-11 — Dashboard live integration (`be-69ey`)

Wave 3. Requirements: US-1, AC-1, AC-2, HC-1. Depends on: WI-LIVE-10.

**Files:** `lisp/beads-dashboard.el`, `lisp/test/beads-dashboard-test.el`.

**Verify:** extended dashboard tests for the live section and changed-row face; journal-off behaves exactly as today.

### WI-LIVE-12 — recent-changes dashboard section (`be-81ek`)

Wave 3. Requirements: US-1, US-2, HC-1. Depends on: WI-LIVE-11.

**Files:** `lisp/beads-dashboard-sections.el`, `lisp/beads-section.el`, `lisp/test/beads-dashboard-test.el`.

**Verify:** `:unit` render of the registered `recent-changes` section.

### WI-LIVE-13 — List live refresh (`be-uzxw`)

Wave 3. Requirements: US-1, AC-1, AC-2, HC-1. Depends on: WI-LIVE-11.

**Files:** `lisp/beads-command-list.el`, `lisp/test/beads-list-test.el`.

**Verify:** `:unit` list refresh with point/mark preservation; `beads-list-follow-mode` untouched.

### WI-LIVE-14 — Show live re-render + ACTIVITY section (`be-nsep`)

Wave 3. Requirements: US-1, US-2, HC-1. Depends on: WI-LIVE-11.

**Files:** `lisp/beads-command-show.el`, `lisp/test/beads-command-show-test.el`.

**Verify:** `:unit` show re-render and ACTIVITY section via `beads-show--insert-section-with`.

### WI-LIVE-15 — Timeline + city timeline views (`be-jsrq`)

Wave 4. Requirements: US-2, US-5, AC-4, AC-5. Depends on: WI-LIVE-14.

**Files:** `lisp/beads-events.el`, `lisp/test/beads-events-test.el`.

**Verify:** `:unit` rendering, op/actor/issue filters, churn folding, org export, city merge by wall clock.

### WI-LIVE-16 — Per-issue history + rewind (`be-lkdd`)

Wave 4. Requirements: US-2, US-3, AC-5, AC-6. Depends on: WI-LIVE-15.

**Files:** `lisp/beads-events.el` (split if >~700 lines), `lisp/test/beads-events-test.el`.

**Verify:** `:unit` history diffs/tombstones and the AC-6 rewind property (K at/above the ring floor; advisory A4).

### WI-LIVE-17 — Notifications + mode-line pulse (`be-dfrh`)

Wave 4. Requirements: US-4, AC-7. Depends on: WI-LIVE-16.

**Files:** `lisp/beads-events.el`, `lisp/beads-pulse.el`, `lisp/test/beads-events-test.el`.

**Verify:** `:unit` notify inert when disabled; pulse mode-line string is pure/redisplay-safe.

### WI-LIVE-18 — Menus + autoloads (`be-b125`)

Wave 5. Requirements: US-5, HC-1. Depends on: WI-LIVE-17.

**Files:** `lisp/beads-command-events.el`, `lisp/beads-menu.el`, `lisp/beads.el`.

**Verify:** menu/transient smoke tests + byte-compile; no new EIEIO classes.

### WI-LIVE-19 — Standalone guard, gascity seam, NEWS (`be-9ntg`)

Wave 5. Requirements: US-5, AC-8, HC-3. Depends on: WI-LIVE-18.

**Files:** `lisp/test/beads-standalone-guard-test.el`, `NEWS.md`.

**Verify:** AC-8 standalone-first guard green; gascity seam documented, not implemented.

### WI-LIVE-20 — Integration tests + live acceptance (`be-omxu`)

Wave 6. Requirements: AC-1, AC-2, AC-3, AC-6, AC-9. Depends on: WI-LIVE-19.

**Files:** new `lisp/test/beads-live-integration-test.el`, bead evidence.

**Verify:** tagged `:integration` tests plus live `emacs -nw -Q` in tmux (local + `/ssh:localhost:`).

### WI-LIVE-21 — CI gate + PR (`be-lylj`)

Wave 6. Requirements: AC-9. Depends on: WI-LIVE-20.

**Files:** CI config, PR.

**Verify:** byte-compile `--warnings-as-errors`, `eldev lint`, `eldev test`, coverage run, remote render guard.

## Dependency Graph

```
WI-LIVE-01 (root)
WI-LIVE-02 (root)
WI-LIVE-01 + WI-LIVE-02 -> WI-LIVE-03
WI-LIVE-03 -> WI-LIVE-04
WI-LIVE-04 -> WI-LIVE-05
WI-LIVE-05 -> WI-LIVE-06
WI-LIVE-04 -> WI-LIVE-07
WI-LIVE-06 + WI-LIVE-07 -> WI-LIVE-08
WI-LIVE-08 -> WI-LIVE-09
WI-LIVE-09 -> WI-LIVE-10
WI-LIVE-10 -> WI-LIVE-11
WI-LIVE-11 -> WI-LIVE-12
WI-LIVE-11 -> WI-LIVE-13
WI-LIVE-11 -> WI-LIVE-14
WI-LIVE-14 -> WI-LIVE-15
WI-LIVE-15 -> WI-LIVE-16
WI-LIVE-16 -> WI-LIVE-17
WI-LIVE-17 -> WI-LIVE-18
WI-LIVE-18 -> WI-LIVE-19
WI-LIVE-19 -> WI-LIVE-20
WI-LIVE-20 -> WI-LIVE-21
```

`gc bd dep cycles` reports no cycles; the graph is a single acyclic DAG. WI-LIVE-20 (integration + live acceptance) is the aggregate gate; WI-LIVE-21 is the CI/PR gate.

## Verification

Automated acceptance gates for the implementation (all must pass):

```
eldev -p -dtT compile
eldev -p -dtT lint
eldev -p -dtT test
eldev -s -dtT test -U coverage/codecov.json '(not (tag :integration))'
eldev test -f beads-event-test.el
eldev test -f beads-live-test.el
eldev test -f beads-events-test.el
eldev test -f beads-standalone-guard-test.el
```

Live acceptance (per `implementation-plan.md` §Verification): every user-visible WI is exercised in a live non-graphical Emacs (`emacs -nw -Q`) inside tmux, loaded from its worktree `lisp/`, against a real store (local and `/ssh:localhost:`), and the observed buffer/state is recorded as bead evidence. WI-LIVE-20 additionally runs the `:integration` suite and the remote render guard (`lisp/test/beads-render-guard-test.el`).

## Skipped Work

- **Rewind snapshot materialization.** WI-LIVE-16 replays from the bounded ring first; materializing every Nth snapshot is deferred until profiling demands it (design §13a, plan Open Question 1).
- **`bd serve` transport.** `tail --follow` only; `bd serve` refuses embedded Dolt and documents no events route, so there is no HTTP journal transport today (design §13b, plan Open Question 2).
- **`recent-changes` source.** Registered through `beads-section-register` so gascity can reuse it; the dashboard consumes the registry (design §13c, plan Open Question 3).
- **History/rewind file split.** A size decision during Wave 4, not architectural (design §13d, plan Open Question 4).
- **New `bd` CLI flags/subcommands.** The feature targets the documented `bd events` surface only; no `bd` change is requested (OOS-1).
- **Original planning artifacts.** This run does not re-author requirements, design, mockups, plan, or plan-review; it produces the build decomposition and implementation beads only.

## Blocked Work

**No work is blocked at decomposition time.** Every member of convoy `be-r2bm` is open and runnable once its `blocks` prerequisites close; the graph is acyclic and `bd ready` exposes only the frontier, starting with the two Wave 0 roots `WI-LIVE-01` and `WI-LIVE-02`. No cross-repo or external blocker exists: AC-8/`HC-3` require only a documented, not implemented, gascity seam, so gascity.el is not edited (OOS-5).

## Traceability

The requirement→work-item mapping below is the authoritative traceability matrix; it matches the YAML `trace.coverage` and the markdown coverage matrix at the top of this artifact.

| Requirement | Work item(s) |
| --- | --- |
| US-1 | WI-LIVE-03, WI-LIVE-04, WI-LIVE-05, WI-LIVE-06, WI-LIVE-07, WI-LIVE-08, WI-LIVE-09, WI-LIVE-10, WI-LIVE-11, WI-LIVE-12, WI-LIVE-13, WI-LIVE-14 |
| US-2 | WI-LIVE-01, WI-LIVE-02, WI-LIVE-12, WI-LIVE-14, WI-LIVE-15, WI-LIVE-16 |
| US-3 | WI-LIVE-16 |
| US-4 | WI-LIVE-09, WI-LIVE-10, WI-LIVE-17 |
| US-5 | WI-LIVE-15, WI-LIVE-18, WI-LIVE-19 |
| AC-1 | WI-LIVE-03, WI-LIVE-04, WI-LIVE-11, WI-LIVE-13, WI-LIVE-20 |
| AC-2 | WI-LIVE-03, WI-LIVE-04, WI-LIVE-08, WI-LIVE-11, WI-LIVE-13, WI-LIVE-20 |
| AC-3 | WI-LIVE-06, WI-LIVE-07, WI-LIVE-20 |
| AC-4 | WI-LIVE-01, WI-LIVE-05, WI-LIVE-15 |
| AC-5 | WI-LIVE-01, WI-LIVE-15, WI-LIVE-16 |
| AC-6 | WI-LIVE-16, WI-LIVE-20 |
| AC-7 | WI-LIVE-09, WI-LIVE-10, WI-LIVE-17 |
| AC-8 | WI-LIVE-19 |
| AC-9 | WI-LIVE-20, WI-LIVE-21 |
| HC-1 | WI-LIVE-11, WI-LIVE-12, WI-LIVE-13, WI-LIVE-14, WI-LIVE-18 |
| HC-2 | WI-LIVE-03, WI-LIVE-04, WI-LIVE-05, WI-LIVE-06, WI-LIVE-07, WI-LIVE-08, WI-LIVE-09 |
| HC-3 | WI-LIVE-19 |
| HC-4 | WI-LIVE-01, WI-LIVE-02 |
| OOS-1 | out_of_scope — Requirements Out Of Scope: no new bd subcommands or flags; work targets the documented bd events surface only. |
| OOS-2 | out_of_scope — Requirements Out Of Scope: no daemon, watcher service, or persistent database; the model is in-memory and rebuilt. |
| OOS-3 | out_of_scope — Requirements Out Of Scope: no direct Dolt table reads and no second journal implementation. |
| OOS-4 | out_of_scope — Requirements Out Of Scope: no graphical/canvas renderer; text faces and overlays only. |
| OOS-5 | out_of_scope — Requirements Out Of Scope: no gascity code changes; only an optional, gascity-free attribution/invalidation seam is shipped. |

Plan traceability: every work item names its `implementation-plan.md` wave and its expected files or formula assets; WI-LIVE-20 is the integration/acceptance gate and WI-LIVE-21 is the CI/PR gate. All 21 beads are tracked by convoy `be-r2bm` and no bead is outside the approved plan.
