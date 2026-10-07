# beads.el Standalone-first Formula → Molecule Workflow — Plan (planning only)

**Status: plan for review. Implementation is out of scope until the plan and
its ASCII mockups are signed off.**

This directory holds the complete plan for making the whole `bd` workflow
lifecycle — `formula → proto → molecule → swarm` — first-class in Emacs with
`bd` alone, for a regular agent or a human, with **no gascity**. It extends
`plans/beads-ui-redesign/` (PR #67, `wi18-integration`) rather than replacing
it.

The plan closes **nine gaps**:

1. formula provenance/detail,
2. the molecule execution view + standalone work loop,
3. gates,
4. wisps,
5. bonding/composition,
6. formula authoring,
7. standalone agent hand-off + `bd prime`/`bd setup` context,
8. the no-gascity guarantee, and
9. **swarm / coordination** (`bd swarm create/list/status/validate`) — the
   documented way to work a large epic in parallel, promoted here from raw
   command output to a first-class surface.

## Read in this order

1. [`requirements.md`](requirements.md) — the numbered requirements
   (`REQ-SF-010…083`, plus the swarm block `REQ-SF-090…099`), the hard
   constraints, the nine gaps, and the acceptance criteria for this planning
   task.
2. [`design.md`](design.md) — the thesis, non-goals, surface inventory, module
   map, the magit/forge **extension seams with signatures** (including the
   swarm seams §5.4), the reuse-vs-new matrix, the data/async model, keymaps
   and glyphs/faces, remote/TRAMP, risks, and the design-level verification
   plan.
3. [`menu-mockups.md`](menu-mockups.md) — ASCII renderings of **every** surface
   in every relevant state (loading/empty/populated/folded/filtered/windowed/
   error/destructive-confirm) plus key-flow traces. The swarm surfaces are
   §14: fleet list, status board, validate/waves, worker/parallelism lanes,
   create+coordinator, claim/assign/hand-off, navigation, and empty/loading/
   error/big-epic states.
4. [`decomposition.md`](decomposition.md) — work items `WI-SF-01…18` with
   dependencies and full REQ traceability (44 requirements). Swarm is
   `WI-SF-15…18`.
5. [`implementation-plan.md`](implementation-plan.md) — the ordered,
   **pruning-first** six-wave implementation plan, verification, risks and
   rollback.
6. [`plan-review.md`](plan-review.md) — the recorded critique round: F1–F15
   (formula/molecule/gates/wisps/bonding/authoring/hand-off/guard) and S1–S10
   (the swarm addition), with findings and dispositions.

## The shape of the change (one paragraph)

beads.el already has the *issue* porcelain and the *execution/parse layer* for
formulas, molecules, gates and swarms. This plan adds the missing *workflow
porcelain*: a molecule execution view with a one-key standalone work loop,
first-class gate and wisp views, an explicit pour-vs-wisp instantiate flow, a
bonding flow, a TOML authoring surface, an optional agent hand-off, and a
**swarm surface** — a fleet list, a live status board (Completed / Active with
assignee / Ready / Blocked), a validate view that renders dependency warnings
and the **ready fronts (parallel waves)** with `max_parallelism` and estimated
worker-sessions, worker/parallelism lanes, and one-key create/coordinator,
assign/claim/hand-off. Everything composes the seams PR #67 published
(`beads-formula-launch`, `beads-formula-var-reader`, `beads-sling-*`,
`beads-thing`, the faces palette, `beads-command-execute-async`) and the
existing `beads-command-swarm.el` classes + `beads-swarm` transient — nothing
is forked.

## Non-negotiables

- **Standalone-first.** Every flow works with `bd` alone; gascity is optional
  enrichment through documented seams, never a dependency of a core flow
  (`REQ-SF-080`, `REQ-SF-097`).
- **Reuse PR #67; do not fork it.** Extend `beads-formula-launch`; reuse the
  swarm command classes and transient.
- Hand-built UI only; generated transients are a dispatch backend, never the
  porcelain.
- beads.el must **not** depend on gascity.el; no gascity symbol on a default
  path.
- **Planning only.** No `.el` source file is modified by this package.
- **No TBDs.** Every surface is decided; accepted follow-ups live in
  `plan-review.md`.
