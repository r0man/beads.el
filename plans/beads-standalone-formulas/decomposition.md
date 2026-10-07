---
schema: beads.standalone-formulas.decomposition.v1
artifact: decomposition
status: draft-for-review
scope: planning-only
implementation: out-of-scope-until-signoff
requirements: requirements.md
design: design.md
---

# beads.el Standalone-first Formula → Molecule Workflow — Decomposition

Work items are `WI-SF-NN`, beads-ready for the downstream implementation. Every
`REQ-SF-*` maps to at least one WI (traceability below). All WIs land on top of
PR #67 (`wi18-integration`); none rewrites a PR #67 artifact.

## Summary

| WI | Title | Depends on | Est. | Tags |
|---|---|---|---|---|
| WI-SF-01 | Molecule view foundation (aggregate, sections, states) | WI-SF-05 | M | UI, async |
| WI-SF-02 | Molecule actions + work loop | WI-SF-01 | M | UI, behavior |
| WI-SF-03 | Instantiate flow: pour/wisp + typed vars + validation | WI-SF-01, WI-SF-05 | L | UI, formula |
| WI-SF-04 | Cook preview + split `beads-command-cook.el` | WI-SF-05 | M | UI, formula |
| WI-SF-05 | Formula provenance/detail extension | — | M | formula |
| WI-SF-06 | Gate UI (list/detail/create/check/resolve/waiter/discover) | WI-SF-01 | L | UI, gates |
| WI-SF-07 | Wisp UI (list/squash/burn/purge/phase) | WI-SF-01 | M | UI, wisps |
| WI-SF-08 | Bond flow + preview + bond points | WI-SF-05 | M | UI, mol |
| WI-SF-09 | Formula authoring: TOML, schema validation, convert | WI-SF-05 | L | authoring |
| WI-SF-10 | Distill epic → formula | WI-SF-09 | S | authoring |
| WI-SF-11 | Hand-off + `bd prime` context + `bd setup` status | WI-SF-01 | M | agent, context |
| WI-SF-12 | Menus, autoloads, face/glyph wiring, mode-lines | WI-SF-01..11,16..18 | S | integration |
| WI-SF-13 | Standalone guard + tests + remote verification | WI-SF-02,03,06,07,09,15..18 | M | tests |
| WI-SF-14 | NEWS + docs cross-reference | WI-SF-13 | XS | docs |
| WI-SF-15 | Swarm result types + `:result` + domain-error handling | WI-SF-05 | M | types, swarm |
| WI-SF-16 | Swarm fleet list + status board + worker lanes | WI-SF-15, WI-SF-01 | L | UI, swarm |
| WI-SF-17 | Swarm validate / waves / verbose graph | WI-SF-15 | M | UI, swarm |
| WI-SF-18 | Swarm actions: create+coordinator, assign/claim/hand-off, navigation | WI-SF-16, WI-SF-17, WI-SF-11 | L | UI, swarm, agent |
| WI-SF-19 | Cross-repo ownership: dedup gascity.el → beads.el seams | WI-SF-01..11,15..18 | L | cross-repo, dedup |

Sizes: XS < 0.5d, S < 1d, M 1–3d, L 3–6d.

## Work Items

### WI-SF-01 — Molecule view foundation
A `beads-section-mode` buffer (`beads-molecule.el`) aggregating `bd mol
current`, `bd mol progress`, `bd mol show` (and `bd ready --mol` when
ready-only) through `beads-command-execute-async` with
`:cache-key (molecule root)` and per-section error boundaries. Renders root
header, progress, step tree with `[done]/[current]/[ready]/[blocked]/
[pending]/[blocked-gate]`, gates section, output section. Adds
`beads-molecule-step-state` / `-step-face`, `beads-thing` movement, and the
glyph/face set. Handles large molecules with `--limit`/`--range`.
**Covers:** REQ-SF-020, REQ-SF-021, REQ-SF-024 (render), REQ-SF-033 (render).

### WI-SF-02 — Molecule actions + work loop
`n` next-ready, `c` claim (`beads-command-update --claim`), `x` close
(`beads-command-close`, reason required), `r` ready-only toggle, `C`
close-eligible (`beads-command-epic-close-eligible`, dry-run then apply),
refresh-and-advance after each write, `a` hand-off hook, `b` bond hook, `G`
gate hook. Transition table from `design.md` §8.3.
**Covers:** REQ-SF-022, REQ-SF-023, REQ-SF-024 (actions), REQ-SF-025.

### WI-SF-03 — Instantiate flow: pour/wisp + typed vars + validation
Extend `beads-formula.el`: `beads-formula-instantiate` generic, the
instantiate transient with explicit phase, typed var entry via
`beads-formula-var-reader`, validation (required/pattern/enum/numeric),
`--assignee`/`--for`, dry-run preview, and `beads-formula-follow` opening the
molecule view. `beads-formula-launch` delegates with `pour`.
**Covers:** REQ-SF-013, REQ-SF-014, REQ-SF-015, REQ-SF-016, REQ-SF-042
(phase display), REQ-SF-081.

### WI-SF-04 — Cook preview
Split the `bd cook` class into `beads-command-cook.el`; add `beads-cook.el`
with the cook transient and a parse of the `--dry-run` output into a step tree
(compile/runtime, `--persist`/`--force`/`--prefix`, `--var`).
**Covers:** REQ-SF-012.

### WI-SF-05 — Formula provenance/detail extension
Extend `beads-types.el` (`beads-formula` gains `phase`, `extends`, `aspects`,
`expansions`, `bond-points`; steps gain gate/`waits_for`) and its
`beads-from-json` method. Browser: scope filter (project/user/all), source
path, shadowed marker, phase column. Detail: phase, version, `extends`,
aspects/expansions, bond points, gate/`waits_for` on steps. Exposes shadowing
data for the browser. Surfacing unknown TOML keys is out of scope (deferred to
an upstream `bd` change); only `bd`-emitted warnings render.
**Covers:** REQ-SF-010, REQ-SF-011, REQ-SF-052 (render).

### WI-SF-06 — Gate UI
`beads-gate.el`: tabulated list (open/all/type, waiter count, blocks), sectioned
detail (type/status/await-id/repo/timeout/waiters/blocks/history), create
transient (human/timer/gh:run/gh:pr), check (`--type`, `--dry-run`, apply),
resolve (reason), add-waiter, discover (`--branch`/`--limit`/`--max-age`/
`--dry-run`), and the molecule-view gate interactions.
**Covers:** REQ-SF-030, REQ-SF-031, REQ-SF-032, REQ-SF-033 (interaction).

### WI-SF-07 — Wisp UI
`beads-wisp.el`: tabulated list (`--all`/`--type`, old detection), squash
(`beads-command-mol-squash`, `--summary`/`--keep-children`), burn
(`beads-command-mol-burn`, `--dry-run`/`--force`/batch, typed confirm), purge
(`beads-command-purge` exists, `--dry-run`/`--force`/`--older-than`/
`--pattern`), phase badges; `RET` opens the root molecule view.
**Covers:** REQ-SF-040, REQ-SF-041, REQ-SF-042.

### WI-SF-08 — Bond flow
`beads-bond.el`: two-operand completion (`beads-bond-operand-reader`), bond
type, `--as`, `--pour`/`--ephemeral`, `--ref` + `--var`, dry-run preview;
bond-point attachment from formula detail; molecule-view prefill.
**Covers:** REQ-SF-050, REQ-SF-051, REQ-SF-052 (attachment).

### WI-SF-09 — Formula authoring
`beads-formula-edit.el`: scaffold a new `.formula.toml`, open/edit source in
`toml-mode` (fallback `conf-mode`), schema-driven completion and on-save
validation from `bd formula schema` (`beads-formula-schema-struct`),
diagnostics buffer, convert JSON↔TOML.
**Covers:** REQ-SF-060, REQ-SF-061, REQ-SF-062.

### WI-SF-10 — Distill
Surface `bd mol distill` from the issue/epic detail: formula name, output dir,
`--var value=variable` mapping, dry-run preview, follow to the created source.
**Covers:** REQ-SF-063.

### WI-SF-11 — Hand-off + context
`beads-handoff.el`: hand-off transient (agent type/backend/worktree, molecule/
formula user-prompt envelope, `beads-agent-backend-start` 4-arity),
`M-x beads-context` (`bd prime`, memories, `bd setup` status, policy), split
`beads-command-prime.el`.
**Covers:** REQ-SF-070, REQ-SF-071, REQ-SF-072.

### WI-SF-12 — Menus, autoloads, wiring
Register the new surfaces in `beads-dispatch` / `beads-maintenance`, add
autoloads, install `C-c b` extension maps, add faces, mode-lines, and the
`beads-mol` parent-transient entries.
**Covers:** REQ-SF-082.

### WI-SF-13 — Guard, tests, remote
`beads-standalone-guard-test.el` (no gascity; load each module, drive each
entry with mocked `beads-command-execute`, scan for gascity symbols); unit and
integration tests (cook→pour→work→close, cook→wisp→squash, gate round-trip,
bond, distill, setup check); TRAMP verification of every new view with the
render guard green.
**Covers:** REQ-SF-080, REQ-SF-083 (execution of).

### WI-SF-14 — Docs
`NEWS.md` entries; cross-reference PR #67 docs; update the plan status.
**Covers:** REQ-SF-083.

### WI-SF-15 — Swarm types + `:result` + domain-error handling
Add `beads-swarm-list-item`, `beads-swarm-status`,
`beads-swarm-status-issue`, `beads-swarm-analysis`, `beads-ready-front`,
`beads-swarm-issue-node` to `beads-types.el` with their `beads-from-json`
methods; add `:result` declarations to the four existing
`beads-command-swarm-*` classes (they currently parse raw strings). Add
`beads-swarm-domain-error-p`, the one detector for the `{error: …}` payloads
`bd swarm create` / `bd swarm validate` emit **with exit 0** (`already
exists`, `epic is not swarmable`, `swarmable=false`). No slot changes to the
command classes (they already expose `epic-id`, `coordinator`, `force`,
`swarm-id`).
**Covers:** REQ-SF-099, REQ-SF-098 (domain states).

### WI-SF-16 — Swarm fleet list + status board + worker lanes
`beads-swarm.el`: the `beads-swarm-list-mode` fleet list (`beads-swarm-list-view`)
(`beads-command-swarm-list`, filter/all, `RET`/`v`/`c`/`W`/`x`/`E`) and the
sectioned status board (`beads-command-swarm-status`: header progress and
counts; Completed / Active (assignee) / Ready / Blocked groups; `beads-thing`
rows; large-epic windows). Worker lanes derive from
`beads-swarm-worker-source` (standalone: `bd list --assignee --status
in_progress`) with `active` / `idle-slot` / `over-committed` / headroom
(`max_parallelism − active`). Async per `design.md` §7.1; per-section error
boundaries.
**Covers:** REQ-SF-091, REQ-SF-092, REQ-SF-094, REQ-SF-096 (render), REQ-SF-098
(board states).

### WI-SF-17 — Swarm validate / waves / verbose graph
`beads-swarm.el`: the validate view (`beads-swarm-waves-mode` /
`beads-swarm-waves` over `beads-command-swarm-validate`): the
`swarmable` verdict, `warnings`/`errors` grouped with drill-in, the
**ready fronts (waves)** table, `max_parallelism`, `estimated_sessions`, and
the `--verbose` per-issue graph (`wave`, `depends_on`/`depended_on_by`).
Non-swarmable is a domain state, not an error; `c` is disabled. Reuses
`beads-ready-front` / `beads-swarm-issue-node` from WI-SF-15.
**Covers:** REQ-SF-093, REQ-SF-098 (validated-with-warnings/errors).

### WI-SF-18 — Swarm actions + create/coordinator + navigation
`beads-swarm.el`: create transient `beads-swarm-create-flow` (`--coordinator`,
`--force`, validate preview; auto-wrap reported; already-exists domain panel
offering the existing swarm), coordinator set/replace (`beads-command-update --assignee` on the
swarm molecule, confirmed), step `assign`/`claim`/`hand off`/`close`/`reopen`,
close-eligible root, and the epic ↔ swarm ↔ molecule/step ↔ worker navigation
edges. Hand-off reuses `beads-handoff` (WI-SF-11); merge-slot and gate actions
are wired where applicable. Rewires the existing `beads-swarm` transient's
suffixes to the new views (no key removed).
**Covers:** REQ-SF-090, REQ-SF-095, REQ-SF-096 (actions), REQ-SF-097.

### WI-SF-19 — Cross-repo ownership / gascity.el dedup
Audit gascity.el against the beads.el seams this plan lands and, for every
piece of beads-native logic gascity.el reimplements (candidate hotspots:
`gascity-formula--validate-values` / `--enum-choices` vs
`beads-formula-var-reader`; formula catalog/recipe reading vs
`beads-command-formula`; the terminal/tmux code already moved by PR #67),
**move the logic into beads.el** and reduce gascity.el to a thin caller/shim.
The beads.el side only adds/extends generic seams; the gascity.el edits are
tracked as a bead in the gascity.el rig and blocked on WI-SF-13 so the seams
are stable. Acceptance: gascity.el byte-compiles and its tests pass against
the new beads.el seams with no duplicated helper bodies; a documented
hotspot list with before/after module ownership.
**Covers:** REQ-SF-100.

## Dependency graph (critical path)

```
WI-SF-05 ─┬─► WI-SF-01 ─┬─► WI-SF-02 ─┬─► WI-SF-13 ─► WI-SF-14
          │             ├─► WI-SF-06 ─┤
          │             ├─► WI-SF-07 ─┤
          │             └─► WI-SF-11   │
          ├─► WI-SF-03 ───────────────┤
          ├─► WI-SF-04                 │
          ├─► WI-SF-08                 │
          └─► WI-SF-09 ─► WI-SF-10 ─────┘
WI-SF-05 ─► WI-SF-15 ─┬─► WI-SF-16 ─┬─► WI-SF-18 ─┐
                      └─► WI-SF-17 ─┘            │
WI-SF-01..11,16..18 ─► WI-SF-12 ─► WI-SF-13 ─► WI-SF-19
```

Critical path: `WI-SF-05 → WI-SF-01 → WI-SF-02 → WI-SF-13 → WI-SF-14`.
Parallelisable after WI-SF-01: gates, wisps, hand-off; parallelisable after
WI-SF-15 (swarm types): the swarm list/board, validate and actions. Independent
from the start: WI-SF-04, WI-SF-08, WI-SF-09 (input: WI-SF-05). The swarm chain
joins `WI-SF-12`/`WI-SF-13` with the rest; it never blocks the
formula→molecule critical path.

## Bead metadata template (for the downstream implementation)

```
gc.kind=work
gc.outcome=pass            # set on close
plan=plans/beads-standalone-formulas
wi=WI-SF-NN
requires=REQ-SF-NNN,...    # from the traceability matrix
depends_on=WI-SF-NN,...    # from the dependency graph
tags=unit,integration      # as applicable
verify=<test selector>
live=emacs-tmux-bright-lights  # live evidence for every user-visible WI
```

Each WI becomes one bead; `WI-SF-13`/`WI-SF-14` are gates on the others.

**Live evidence is mandatory for every user-visible WI.** Before a WI closes,
record evidence from a live, non-graphical Emacs (`emacs -nw -Q`) in tmux,
loaded from the WI worktree `lisp/`, against the `~/bright-lights` city (local
and `/ssh:localhost:~/bright-lights`) — see
`implementation-plan.md` §Execution constraints and
`docs/qa/2026-10-04-beads-ui-redesign-wi20-live.md`. Unit tests alone do not
satisfy `verify`.

## Traceability matrix (REQ → WI)

| Requirement | WI |
|---|---|
| REQ-SF-010 | WI-SF-05 |
| REQ-SF-011 | WI-SF-05 |
| REQ-SF-012 | WI-SF-04 |
| REQ-SF-013 | WI-SF-03 |
| REQ-SF-014 | WI-SF-03 |
| REQ-SF-015 | WI-SF-03 |
| REQ-SF-016 | WI-SF-03 |
| REQ-SF-020 | WI-SF-01 |
| REQ-SF-021 | WI-SF-01 |
| REQ-SF-022 | WI-SF-02 |
| REQ-SF-023 | WI-SF-02 |
| REQ-SF-024 | WI-SF-01, WI-SF-02 |
| REQ-SF-025 | WI-SF-02 |
| REQ-SF-030 | WI-SF-06 |
| REQ-SF-031 | WI-SF-06 |
| REQ-SF-032 | WI-SF-06 |
| REQ-SF-033 | WI-SF-01, WI-SF-06 |
| REQ-SF-040 | WI-SF-07 |
| REQ-SF-041 | WI-SF-07 |
| REQ-SF-042 | WI-SF-03, WI-SF-07 |
| REQ-SF-050 | WI-SF-08 |
| REQ-SF-051 | WI-SF-08 |
| REQ-SF-052 | WI-SF-05, WI-SF-08 |
| REQ-SF-060 | WI-SF-09 |
| REQ-SF-061 | WI-SF-09 |
| REQ-SF-062 | WI-SF-09 |
| REQ-SF-063 | WI-SF-10 |
| REQ-SF-070 | WI-SF-11 |
| REQ-SF-071 | WI-SF-11 |
| REQ-SF-072 | WI-SF-11 |
| REQ-SF-080 | WI-SF-13 |
| REQ-SF-081 | WI-SF-03, WI-SF-12 |
| REQ-SF-082 | WI-SF-12 |
| REQ-SF-083 | WI-SF-13, WI-SF-14 |
| REQ-SF-090 | WI-SF-18 |
| REQ-SF-091 | WI-SF-16 |
| REQ-SF-092 | WI-SF-16 |
| REQ-SF-093 | WI-SF-17 |
| REQ-SF-094 | WI-SF-16 |
| REQ-SF-095 | WI-SF-18 |
| REQ-SF-096 | WI-SF-16, WI-SF-18 |
| REQ-SF-097 | WI-SF-18 |
| REQ-SF-098 | WI-SF-15, WI-SF-16, WI-SF-17 |
| REQ-SF-099 | WI-SF-15 |
| REQ-SF-100 | WI-SF-19 |

## Coverage

- Every `REQ-SF-*` is covered.
- Every WI except WI-SF-12/13/14 delivers user-visible behaviour and its own
  tests; WI-SF-13 is the acceptance gate (extended to the swarm modules);
  WI-SF-14 is docs.
- **WI-SF-19 is the only WI that changes gascity.el**; it does so in the
  gascity.el rig (never from the beads.el worktree) and only to delete
  duplicated beads-native logic, reducing gascity.el to thin callers/shim per
  REQ-SF-100. No other WI touches gascity.el; none changes PR #67's decided UI
  (hand-built, dispatch contract, sling, terminal).
- Swarm reuses the existing `beads-command-swarm.el` classes and `beads-swarm`
  transient; WI-SF-15 is the only change to that file and it is additive
  (`:result` + domain-error helper).

## Out of scope

- Implementation of WI-SF-19 in the gascity.el worktree during the beads.el
  build; it is create-a-linked-bead and execute-after-the-seams-land work.
- Implementation. This is a plan; WIs are executed after sign-off.
- New `bd` flags, new dependencies (unless accepted in `plan-review.md`), and
  any re-litigation of PR #67 decisions.
