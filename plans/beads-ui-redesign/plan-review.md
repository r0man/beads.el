---
schema: beads.ui-redesign.plan-review.v1
workflow:
  id: be-59fe
artifact: plan-review
status: draft-for-review
scope: planning-only
round: 1
---

# beads.el UI Redesign — Plan Review (round 1)

Review of `requirements.md`, `design.md`, `menu-mockups.md`, `slimming.md`,
`implementation-plan.md` and `decomposition.md` for implementation
readiness. Scope: requirements traceability, the extension seams, the
code-movement plan, the slimming audit, task boundaries, test churn, and
risk.

**Verdict: APPROVED for user sign-off, with one decision flagged (F3).**
Two required changes were found and applied to the plan in place (F1, F2);
F4–F6 are non-blocking notes.

## Method

Every load-bearing claim was checked against the repository, not just for
internal consistency.

- **Current-system audit** (`implementation-plan.md` §1): confirmed
  `beads-more-menu` in `beads.el` (marked deprecated), `beads-ops-menu.el`,
  `beads-advanced-menu.el`, `beads-status.el` as `make-obsolete` shim,
  `beads-store-resolve` / `beads-store-directory`, `beads-thing-*`,
  `beads-section-mode` on vui, 59 `beads-command-*.el` files,
  `beads-agent-type.el` + `beads-agent-types.el` (5 roles), 18
  `beads-agent*-test.el` files, `beads-meta-parity-*` constants,
  `beads-render-guard-test.el`.
- **Terminal source**: `gascity.el/lisp/gascity-terminal.el` is 1509 lines
  and owns exactly the tmux/status/mouse/scroll machinery claimed; it
  depends on `gascity-remote-*` helpers, of which `beads-remote.el` already
  has ssh argv, PATH assignment and executable resolution, confirming the
  §6.3 gap list.
- **Slimming targets**: the 5 roles and the 6 user backends + mock were
  confirmed in `beads-agent-types.el` / `beads-agent-backend.el`; the 5
  typed start commands were confirmed in `beads-agent.el`.
- **Test churn**: the suite was grepped for the symbols the plan removes
  (see F1).

## Findings

### F1 (required change — applied): the terminal test port named a file that does not exist

`implementation-plan.md` WI-14 said to "port gascity's
`gascity-terminal-test.el`". Verified: gascity has **no**
`gascity-terminal-test.el`. Terminal coverage lives in
`gascity.el/lisp/test/gascity-test.el` (plus
`gascity-audit-test.el`, `gascity-share-test.el` reference
`gascity-terminal`). Decomposing WI-14 against a nonexistent file would
have silently dropped the terminal port surface — the same class of defect
as the sling review's F1.

**Resolution applied:** WI-14 now names `lisp/test/gascity-test.el` as the
source of the terminal tests to port, with a pointer to this finding.

### F2 (required change — applied): REQ-001 silently repurposed the `beads` symbol

REQ-001 wants `M-x beads` to open the status buffer, but `beads` is today
the **transient prefix** (`(beads-define-prefix beads …)` in `beads.el`,
autoloaded). The plan did not say what happens to that transient. Left
implicit, an implementer could either break the transient (a second
definition clobbers the symbol) or keep `beads` as a transient and fail
REQ-001.

**Resolution applied:** the dispatch transient is renamed
`beads-dispatch` (bound to `?`), and `beads` becomes the status-buffer
entry, documented in REQ-001 and `design.md` §5. This is an intentional,
`NEWS.md`-tracked public change; one-release compatibility is trivial
because the old transient content is preserved verbatim under the new name.

### F3 (flagged for explicit sign-off): the role-roster cut depth

`slimming.md` §3 proposes folding **QA** into Review and removing
**Custom** as a role, leaving Task/Review/Plan. These are the two cuts most
likely to remove a workflow someone relies on (QA as a distinct label in
sessions/history; Custom as a first-class prompt role). The backends cut
(claude-code-ide/claudemacs/eca demoted to an overflow) is uncontroversial
and registry-safe.

The design is registry-preserving (all 5 types and all backends stay
registered and programmatically reachable), so the risk is UX-level, not
capability-level. `slimming.md` §3 states a conservative fallback (keep QA
as a hidden 4th role; keep Custom reachable via the launch prompt) if the
user prefers. **This finding requires a user decision before WI-3; it does
not block the rest of the plan.**

### F4 (note): `C-c b` already exists in gascity's attach map

The design reserves `C-c b` as the universal extension prefix
(`design.md` §4.10), but gascity's attach map already binds `C-c b` to
"bead at point" (`DESIGN-agent-scrolling.md` §4). The plan already resolves
this (canonical `C-c b b`, old binding kept as a one-release alias), and the
moved terminal code arrives in WI-14 while the shim is WI-15, so the alias
lands with the move. No change required; recorded so the implementer does
not "discover" the collision mid-port.

### F5 (note): `beads-dashboard` as superset is consistent with existing scoping

REQ-001 makes `beads-dashboard` the status buffer's superset. Verified this
is compatible with the existing explicit `:directory` scoping
(`beads-dashboard` already takes `:directory` and sets
`beads-store-directory`), and with gascity's delegation (it calls
`beads-dashboard :directory store`). No change required.

### F6 (note): requirement/coverage consistency

The `decomposition.md` coverage table, its traceability matrix, and the
`requirements.md` acceptance criteria agree: REQ-001…REQ-033 all map to at
least one work item or to this planning task. No gaps found.

## Implementation-readiness pass

- **Requirements traceability — PASS.** Every REQ-001…REQ-033 is covered
  (matrix in `decomposition.md`); every WI cites its REQs; the constraints
  from the target bead are restated and not re-opened.
- **Extension seams — PASS.** `design.md` §4 names each seam, its kind, its
  purpose, and the downstream gascity use; the standalone guarantee
  (REQ-021) is testable (WI-4's no-op provider tests).
- **Code-movement plan — PASS (after F1).** Exact source file (1509 lines),
  symbol rename map, target module, the gascity shim, and the remote-helper
  gap list are all named; migration is shim-first so gascity never breaks.
- **Slimming audit — PASS (with F3 decision).** Every removal/collapse has a
  justification and a replacement; the role roster is addressed explicitly;
  the conservative fallback is documented.
- **Mockups — PASS.** Every user-facing surface has an ascii mockup with
  real rendering rules and no "TBD" (status, dispatch, maintenance, list,
  filter, detail, sling ×5, preview, agent launch, sessions, formula
  list/detail, terminal attach/scroll, faces).
- **Task boundaries — PASS.** Twenty WIs, each naming files, functions,
  REQ trace, tests and acceptance; sequencing is explicit and pruning-first;
  the dependency graph has a coherent critical path.
- **Test commands — PASS.** `eldev` invocations match `AGENTS.md`; the
  parity gate and the render guard are named as must-stay-green; the TRAMP
  acceptance procedure is specified.
- **Risk — PASS.** Test churn, generated-transient reach-through, the
  terminal dependency cycle, TRAMP latency, the roster decision, and the
  `C-c b` reservation are explicit with mitigations. Rollback is
  per-work-item; the terminal revert path is safe while the shim coexists.

## Conclusion

The plan is grounded, accurate against the repository, fully traced, and
implementation-ready. F1 and F2 were resolved in place; F3 requires a user
decision on the role-roster depth before WI-3; F4–F6 are non-blocking notes.
**The plan is approved for user sign-off.**

## Sign-off checklist

- [ ] Review `menu-mockups.md` and confirm every surface renders as intended.
- [ ] Decide F3: full roster cut (Task/Review/Plan) or conservative fallback.
- [ ] Confirm the `beads` → status-buffer / `beads-dispatch` rename (F2).
- [ ] Confirm `C-c b` as the reserved extension prefix (F4).
- [ ] Then: implement in wave order, pruning-first, starting at WI-1.
