---
schema: beads.ui-redesign.plan-review.v1
workflow:
  id: be-1iu7
  formula: build-from-plan
  predecessor: be-mv8d
artifact: plan-review
status: approved-for-decomposition
scope: planning-only
round: 3
reviewed_artifact: implementation-plan.md
reviewed_workflow: be-1iu7
---

# beads.el UI Redesign — Plan Review

**Current gate (round 3, workflow `be-1iu7`, bead `be-silg`): review of
`implementation-plan.md` for the `build-from-plan.plan-review` step. Verdict:
APPROVED for decomposition.** Four required corrections (F9–F12) were found
and applied to the plan in place; F13 is a note. Rounds 1–2 (the design-phase
review under `be-mv8d`) are preserved below as history.

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

### F2 (required change — applied; confirmed in round 2): REQ-001 silently repurposed the `beads` symbol

REQ-001 wants `M-x beads` to open the status buffer, but `beads` is today
the **transient prefix** (`(beads-define-prefix beads …)` in `beads.el`,
autoloaded). The plan did not say what happens to that transient. Left
implicit, an implementer could either break the transient (a second
definition clobbers the symbol) or keep `beads` as a transient and fail
REQ-001.

**Resolution applied:** the dispatch transient is renamed
`beads-dispatch` (bound to `?`), and `beads` becomes the status-buffer
entry, documented in REQ-001 and `design.md` §3.5/§5.2. This is an intentional,
`NEWS.md`-tracked public change; one-release compatibility is trivial
because the old transient content is preserved verbatim under the new name.
**Round 2 (user, 2026-10-01) confirmed F2.**

### F3 (resolved in round 2 by user decision — remove-entirely): the role-roster cut depth

Round 1 proposed folding **QA** into Review and removing **Custom** as a
*role*, but left the classes registered (registry-preserving) and flagged the
depth for sign-off, with a conservative fallback (keep QA as a hidden 4th
role; keep Custom reachable from the launch prompt).

**Round 2 (user, 2026-10-01) decided the deeper cut: remove QA and Custom
entirely.** `beads-agent-type-qa`, `beads-agent-type-custom`, their prompts
and `*-backend` defcustoms, `beads-agent-start-qa` /
`beads-agent-start-custom`, the `a q` / `a c` keybindings, the registration
calls, and the affected tests are **deleted**. The QA testing prompt is kept
by moving it onto **Review's QA mode**; Custom's freeform prompt moves onto
**sling's freeform path**. The freed `q` and `c` keys are released (reserved,
not reassigned). The registries stay open for third-party re-registration,
but no built-in QA/Custom class remains. **The conservative fallback is
dropped.** `slimming.md` §3, `requirements.md` REQ-012, `design.md` §9,
`implementation-plan.md` WI-3, and `decomposition.md` are updated to match.

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
- **Slimming audit — PASS (F3 resolved).** Every removal/collapse has a
  justification and a replacement; the role roster is addressed explicitly
  and now removes QA/Custom entirely (prompts relocated, keys freed).
- **Mockups — PASS.** Every user-facing surface has an ascii mockup in every
  relevant state with real rendering rules and no "TBD" (status
  loading/empty/populated/folded/error, dispatch + help, maintenance 2 pages,
  list normal/narrow/long/no-matches/filters/loading-error/filter, detail
  normal/narrow/empty/loading-error/help, sling cold/preseeded/formula/on/
  freeform/validation/picker/gascity, preview local/gc, agent launch
  default/overflow/QA/prompt/lifecycle, sessions populated/empty/error,
  formula list/detail/follow, terminal off/on/wheel/remote, key flows,
  faces).
- **Task boundaries — PASS.** Twenty WIs, each naming files, functions,
  REQ trace, tests and acceptance; sequencing is explicit and pruning-first;
  the dependency graph has a coherent critical path.
- **Test commands — PASS.** `eldev` invocations match `AGENTS.md`; the
  parity gate and the render guard are named as must-stay-green; the TRAMP
  acceptance procedure is specified.
- **Risk — PASS.** Test churn, generated-transient reach-through, the
  terminal dependency cycle, TRAMP latency, the F3 removal, and the
  `C-c b` reservation are explicit with mitigations. Rollback is
  per-work-item; the terminal revert path is safe while the shim coexists.

## Round 2 — refinement pass (be-mv8d, 2026-10-01)

A second, deeper design pass was requested. This round does not re-audit the
repository from scratch; it folds the two user decisions and raises the depth
of `design.md` and `menu-mockups.md` to the gascity sling bar.

### What changed

- **F2 confirmed and folded everywhere** (`requirements.md`, `design.md`
  §3.5/§5.2, `slimming.md` M7, `implementation-plan.md` WI-5).
- **F3 decided (remove-entirely) and folded everywhere** (`requirements.md`
  REQ-012 + constraints, `design.md` §5.2/§9, `slimming.md` §3 rewrite,
  `implementation-plan.md` WI-3/WI-12, `decomposition.md` WI-3). The round-1
  conservative fallback is dropped; the QA/Custom classes are deleted, the
  prompts relocated, and `a q`/`a c` freed.
- **`design.md` deepened** to implementation-ready: a per-view keymap
  contract (§3), a full seam list with signatures and files (§4), a concrete
  module map with ownership and deletion list (§5), a data/async model with
  the four section states and remote rules (§6), faces/design language (§7),
  the standalone sling abstraction with the adaptive-flow lessons (§8),
  agent launch against the post-F3 matrix (§9), formula integration (§10),
  the terminal migration plan (§11), a gascity-usage summary (§12), and
  risks (§13).
- **`menu-mockups.md` deepened to multi-state**: status (loading/empty/
  populated/folded/error), list (normal/narrow/long-title/no-matches/filters/
  loading-error/filter-transient), detail (normal/narrow/empty/loading-error/
  help), sling (cold/preseeded/formula/on/freeform/validation/picker/gascity),
  preview (local/gc dry-run), agent launch (default/overflow/QA-mode/prompt/
  lifecycle), sessions (populated/empty/error), formula
  (list/detail/follow), terminal (off/on/wheel/remote), key-flow traces, and
  the face/glyph reference.

### New findings (round 2, all non-blocking)

### F7 (note): QA prompt relocation touches `beads-agent-types.el` shape
Deleting the QA/Custom classes while keeping the QA prompt text means
`beads-agent-qa-prompt` and `beads-agent-type-qa--user-prompt` must either
move to the Review type or become generic prompt vars consumed by Review's QA
mode. The plan chooses the latter shape (prompt vars kept in
`beads-agent-types.el`, referenced by Review's QA mode); WI-3 names both
options and the implementer picks the one that keeps the existing
customization group intact. No change required now.

### F8 (note): `beads-agent-start-qa` facade vs. hard delete
F3 says delete; the migration/gentleness rule says `beads-agent-start-qa`
keeps a one-release facade to Review+QA. These are compatible (the command
name survives as a facade while the class does not) but the implementer must
not re-register a QA *type* to satisfy the facade. WI-3's acceptance makes
this explicit (the symbol may exist as a facade; `beads-agent-type-qa` must
not).

### Round-2 verdict

The plan is grounded, accurate, fully traced, and now deep enough for
implementation: every surface is mocked in its states, every seam has a
signature, the module map names ownership and deletions, and the terminal
move is phased shim-first. F1/F2 are resolved, F3 is resolved by decision
(remove-entirely), F4–F8 are notes.
**The plan is approved for user sign-off.**

## Round 3 — implementation-plan review (be-1iu7, 2026-10-02)

This gate reviews the **implementation plan** produced by the plan stage
(`implementation-plan.md`, `producer.stage: plan`, attempt 1) against
`requirements.md` (hash `sha256:50fd6788…`), `design.md`, `slimming.md` and
`decomposition.md`, for readiness to decompose. Interaction mode is
`autonomous`; no question round is used.

### Method

- **Upstream integrity.** The requirements hash recorded in the plan front
  matter was recomputed: `sha256:50fd6788be4420ae0fcf2574736bca6059409fa38c24f706c0726af20edcaac9`
  — matches. `trace.coverage` lists REQ-001…REQ-033, all `covered`, matching
  the plan's own traceability table and `decomposition.md`'s matrix.
- **Current-system audit.** Re-checked the load-bearing claims in `lisp/`:
  `beads-more-menu` present and deprecated (`beads.el`), `beads-status` a
  `make-obsolete` shim (`beads-status.el`), `beads-ops-menu.el` /
  `beads-advanced-menu.el` present, `beads-store-resolve` /
  `beads-store-project-root` in `beads-util.el`, five registered roles and
  the `beads-agent-qa-backend` defcustom in `beads-agent-types.el`, and the
  five typed `beads-agent-start-*` commands in `beads-agent.el`.
- **Per-WI file/test claims.** Verified against the tree: 59
  `beads-command-<name>.el` files (256 command classes), 18
  `beads-agent*-test.el` files, existing `beads-status-test.el` (four shim
  tests), `gascity.el/lisp/gascity-terminal.el` at 1509 lines, no
  `gascity-terminal-test.el`, and 281 `gascity-terminal` references in
  gascity's `lisp/test/gascity-test.el` (the WI-14 port source).
- **Seam coverage.** Cross-checked every symbol in `design.md` §4 against
  the WIs that claim to deliver it (see F10/F12).

### Findings (round 3)

#### F9 (required change — applied): stale Current System counts
`implementation-plan.md` §"Current System" claimed "90+
`beads-command-<name>.el` classes" and "~15" agent test files. The tree has
**59** command files (256 classes) and **18** agent test files. Because the
plan advertises the Current System as verified, a wrong count undermines the
audit. **Resolution:** corrected to "59 `beads-command-<name>.el` files
(256 command classes)" and "18 files".

#### F10 (required change — applied): WI-4 omitted/mis-assigned the design.md §4 seams
WI-4 is the only work item that delivers the REQ-020 seam set, and its
acceptance is "every seam in `design.md` §4 exists". As written, its Files
list **mis-assigned** the store seams (`beads-store-resolvers`,
`beads-store-prefix-functions`) to `beads-remote.el` though design §4.1 owns
them in `beads-util.el`; **mis-assigned** `beads-action-providers` to
`beads-command-list.el` / `beads-agent.el` though design §4.4 owns it (with
`beads-actions-context`, `beads-after-action-functions`) in
`beads-actions.el`; and **omitted** `beads-store-descriptor`,
`beads-section-spec` and `beads-dashboard-section-providers`. An
implementer following the plan would have lost REQ-020 seams to the wrong
module. **Resolution applied:** WI-4's Files list now enumerates every
seam it owns with its correct `design.md` file, cites §4 as authoritative,
and names the seams deferred to later WIs (sling backend/validators →
WI-10/11; formula launch/vars → WI-13; terminal attach → WI-14; remote
helpers → WI-16).

#### F11 (required change — applied): `beads-status-test.el` is not "new"
WI-5 declared a **new** `beads-status-test.el`, but the file already exists
with four tests for the compat shim (`beads-status-test-shim-*`).
Decomposing WI-5 against "new" would have risked clobbering or orphaning
those tests — the same class of defect as round 1's F1 (a named file that
does not match reality). **Resolution applied:** WI-5 now says to **rewrite
the existing** `beads-status-test.el` (noting its current four shim tests).

#### F12 (required change — applied): sling and formula WIs named no seams
WI-10 and WI-13 are the delivery vehicles for `design.md` §4.5 and §4.7,
but neither named the generics/registries they must create
(`beads-sling-target-functions`, `beads-sling-targets`,
`beads-sling-backend`, `beads-sling-backend-register`,
`beads-sling-validators`; `beads-formula-launch`,
`beads-formula-launch-context`, `beads-formula-var`,
`beads-formula-var-reader`). **Resolution applied:** both Files lists now
name those symbols and cite design §4.5/§4.7.

#### F13 (note): plan/decomposition consistency is sound
Twenty WIs, six waves, pruning-first; the plan's traceability table, the
`trace.coverage` front matter, and `decomposition.md`'s REQ→WI matrix all
agree, and the dependency edges (`WI-1 → WI-2/3 → WI-4 → …`) are acyclic
with a coherent critical path. The F3 removal (WI-3) is a pure deletion with
an explicit absence-based acceptance. No change required.

### Implementation-readiness pass (round 3)

- **Requirements traceability — PASS.** REQ-001…REQ-033 all map to at least
  one WI or to the planning task; the upstream hash matches.
- **Task boundaries — PASS (after F10–F12).** Each WI names its files,
  functions, REQs, tests and acceptance; every `design.md` §4 seam now has
  exactly one owning WI.
- **Test surface — PASS (after F11).** The port sources are real files
  (gascity `gascity-test.el`; the existing `beads-status-test.el`), not
  phantom paths.
- **Test commands — PASS.** `eldev` invocations match `AGENTS.md`; the
  parity gate (`beads-audit-test.el`) and render guard
  (`beads-render-guard-test.el`) are named as must-stay-green; WI-20 gives
  the bright-lights TRAMP acceptance.
- **Risk / rollback — PASS.** Test churn, generated-transient
  reach-through, the beads↔gascity cycle, TRAMP latency, the F3 removal and
  the `C-c b` reservation all have mitigations; rollback is per-work-item
  and the terminal revert path is safe while the shim coexists.

### Round-3 verdict

**The implementation plan is APPROVED for decomposition.** Four required
corrections (F9–F12) were applied to `implementation-plan.md` in place
during this review; F13 is a note. The plan is grounded, fully traced,
pruning-first, and implementation-ready.

## Conclusion

Rounds 1–2 approved the design/requirements/mockups (F1/F2 resolved in
place; F3 resolved by decision; F4–F8 notes). Round 3 reviewed the
implementation plan for the `build-from-plan.plan-review` gate (bead
`be-silg`): F9–F12 were required corrections, applied to
`implementation-plan.md` in place; F13 is a note. **The implementation plan
is approved for decomposition.**

## Sign-off checklist

- [x] Review `menu-mockups.md` and confirm every surface renders as intended.
  (Refined to multi-state in round 2; all surfaces × states mocked, no TBDs.)
- [x] Decide F3: remove QA and Custom entirely — QA → Review QA mode; Custom
  → sling freeform; `a q`/`a c` freed.
- [x] Confirm the `beads` → status-buffer / `beads-dispatch` rename (F2).
- [x] Confirm `C-c b` as the reserved extension prefix (F4).
- [ ] Then: implement in wave order, pruning-first, starting at WI-1.
  (Pending implementation sign-off; planning-only here.)
