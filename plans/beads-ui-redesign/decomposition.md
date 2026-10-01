---
schema: beads.ui-redesign.decomposition.v1
workflow:
  id: be-59fe
artifact: decomposition
status: draft-for-review
scope: planning-only
note: work items are described here only; no beads are created by this planning task
---

# beads.el UI Redesign — Decomposition

Decomposition of the approved requirements (`requirements.md`,
REQ-001…REQ-033) and the implementation plan (`implementation-plan.md`,
WI-1…WI-20) into beads-sized work items for a **later, separately-approved**
implementation. This artifact describes the work items and their
dependencies; it creates no beads (REQ-030: planning docs only). When the
plan is signed off, each WI becomes one bead with the metadata and edges
recorded below.

## Summary

Twenty work items, six waves, pruning-first. Wave 0 removes; every later
wave builds on a smaller surface. Sequencing is encoded below as both a
dependency table and explicit `depends_on` edges so a downstream
implementation can create real `bd` dependency edges.

Unlike the gascity sling decomposition (independent beads drained by a
convoy), this plan has genuine ordering constraints: the pruning items must
land before the porcelain, and the terminal move's shim must land with the
move. The dependency edges below are therefore part of the contract.

## Work Items

| WI | Title | Wave | Requirements | Depends on |
|---|---|---|---|---|
| WI-1 | Delete the deprecated `beads-more-menu` | 0 | REQ-023, REQ-026 | — |
| WI-2 | Demote auto-generated per-command transients | 0 | REQ-003, REQ-024 | WI-1 |
| WI-3 | Slim the agent role and backend surface | 0 | REQ-012, REQ-025 | WI-1 |
| WI-4 | Extension seams (foundations) | 1 | REQ-020, REQ-021, REQ-022 | WI-2, WI-3 |
| WI-5 | Real status buffer | 1 | REQ-001, REQ-002 | WI-4 |
| WI-6 | Universal navigation contract | 1 | REQ-002, REQ-018 | WI-4 |
| WI-7 | List redesign | 2 | REQ-005, REQ-007 | WI-5, WI-6 |
| WI-8 | Detail redesign | 2 | REQ-006, REQ-007 | WI-5, WI-6 |
| WI-9 | Dispatch and maintenance menus | 2 | REQ-003, REQ-004, REQ-023 | WI-4, WI-5 |
| WI-10 | Sling abstraction (standalone) | 3 | REQ-008, REQ-021 | WI-4 |
| WI-11 | Adaptive sling transient and preview | 3 | REQ-009 | WI-10 |
| WI-12 | Agent launch redesign | 3 | REQ-011, REQ-012 | WI-3, WI-10 |
| WI-13 | Formula browser and launch | 3 | REQ-013, REQ-014 | WI-4, WI-10 |
| WI-14 | Move terminal handling into beads.el | 4 | REQ-015, REQ-016 | WI-4 |
| WI-15 | gascity.el compatibility shim | 4 | REQ-016, REQ-022 | WI-14 |
| WI-16 | Remote helpers for the moved terminal | 4 | REQ-015, REQ-019 | WI-4 |
| WI-17 | Faces and consistency pass | 5 | REQ-017, REQ-018 | WI-5, WI-7, WI-8 |
| WI-18 | Remote/TRAMP parity and test consolidation | 5 | REQ-019, REQ-021, REQ-022 | WI-3, WI-7, WI-8, WI-9, WI-11, WI-12, WI-13, WI-14, WI-15, WI-16 |
| WI-19 | Documentation | 5 | REQ-033, REQ-020 | WI-4, WI-5, WI-9, WI-15, WI-17 |
| WI-20 | Acceptance verification (bright-lights / TRAMP) | 6 | REQ-019, REQ-021, REQ-022 | WI-18 |

### Dependency graph (critical path)

```
WI-1 ─▶ WI-2 ─┐
      └▶ WI-3 ─┴▶ WI-4 ─┬▶ WI-5 ─┬▶ WI-7 ─┐
                          │        ├▶ WI-8 ─┤
                          │        └▶ WI-9 ─┤
                          ├▶ WI-6 ──────────┤
                          ├▶ WI-10 ─┬▶ WI-11┤
                          │         ├▶ WI-12┤
                          │         └▶ WI-13┤
                          └▶ WI-14 ─▶ WI-15┤
                          └▶ WI-16 ─────────┤
                                             ▼
                             WI-17 ─▶ WI-18 ─▶ WI-20
                             WI-19 ─────────▶ (docs gate)
```

Critical path: `WI-1 → WI-2/3 → WI-4 → WI-10 → WI-11 → WI-18 → WI-20`.

## Bead metadata template (for the downstream implementation)

Each work item, when turned into a bead, carries:

- `beads.ui.work_item=WI-<n>`
- `beads.ui.wave=<0..6>`
- `beads.trace.requirements=<comma-separated REQ ids>`
- `beads.ui.redesign_root=be-59fe`
- the plan section (`implementation-plan.md` §3) as the description.

Dependencies use `bd dep add <bead> <blocker>` per the table's "Depends on",
so the implementation convoy drains in wave order.

## Traceability matrix (REQ → WI)

| REQ | Work items |
|---|---|
| REQ-001 | WI-5 |
| REQ-002 | WI-5, WI-6 |
| REQ-003 | WI-2, WI-9 |
| REQ-004 | WI-9 |
| REQ-005 | WI-7 |
| REQ-006 | WI-8 |
| REQ-007 | WI-7, WI-8, WI-12 |
| REQ-008 | WI-10 |
| REQ-009 | WI-11 |
| REQ-010 | WI-11 (via WI-4 seam), WI-15 |
| REQ-011 | WI-12 |
| REQ-012 | WI-3, WI-12 |
| REQ-013 | WI-13 |
| REQ-014 | WI-13 |
| REQ-015 | WI-14, WI-16 |
| REQ-016 | WI-14, WI-15 |
| REQ-017 | WI-17 |
| REQ-018 | WI-6, WI-17 |
| REQ-019 | WI-16, WI-18, WI-20 |
| REQ-020 | WI-4, WI-19 |
| REQ-021 | WI-4, WI-10, WI-18, WI-20 |
| REQ-022 | WI-4, WI-15, WI-18, WI-20 |
| REQ-023 | WI-1, WI-9 |
| REQ-024 | WI-2 |
| REQ-025 | WI-3 |
| REQ-026 | WI-1, WI-3, WI-9 |
| REQ-027…REQ-032 | this planning task (satisfied by these artifacts) |
| REQ-033 | WI-19 |

Every REQ-001…REQ-033 is covered by at least one work item or by this
planning task's artifacts.

## Coverage

| ID | Status |
|---|---|
| REQ-001 | covered |
| REQ-002 | covered |
| REQ-003 | covered |
| REQ-004 | covered |
| REQ-005 | covered |
| REQ-006 | covered |
| REQ-007 | covered |
| REQ-008 | covered |
| REQ-009 | covered |
| REQ-010 | covered |
| REQ-011 | covered |
| REQ-012 | covered |
| REQ-013 | covered |
| REQ-014 | covered |
| REQ-015 | covered |
| REQ-016 | covered |
| REQ-017 | covered |
| REQ-018 | covered |
| REQ-019 | covered |
| REQ-020 | covered |
| REQ-021 | covered |
| REQ-022 | covered |
| REQ-023 | covered |
| REQ-024 | covered |
| REQ-025 | covered |
| REQ-026 | covered |
| REQ-027 | covered |
| REQ-028 | covered |
| REQ-029 | covered |
| REQ-030 | covered |
| REQ-031 | covered |
| REQ-032 | covered |
| REQ-033 | covered |

## Out of scope

Creating the beads, the convoy, and the implementation itself. This artifact
is the specification for that later step.
