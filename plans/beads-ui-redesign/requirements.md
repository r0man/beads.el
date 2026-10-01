---
schema: beads.ui-redesign.requirements.v1
workflow:
  id: be-mv8d
  predecessor: be-59fe
  kind: planning
  refinement: 2
artifact: requirements
status: draft-for-review
scope: planning-only
implementation: out-of-scope-until-signoff
decisions_folded: [F2, F3]
---

# beads.el UI Redesign — Requirements

Input: bead `be-59fe` ("Redesign beads.el UI (Magit-style, world-class) —
PLAN"), refined by bead `be-mv8d` (deeper design + multi-state mockups;
fold F2 rename and F3 remove QA/Custom). This artifact restates the target
bead and the approved UI direction as verifiable requirements for design,
mockups, decomposition and review.

**Refinement-2 decisions (user, 2026-10-01): F2 and F3 are now decided, not
open.** F2: `M-x beads` opens the status buffer; the old transient is renamed
`beads-dispatch`, bound to `?`. F3: **remove QA and Custom entirely** (delete
the classes, prompts, `*-backend` defcustoms, start commands, keybindings,
registration calls and affected tests) — QA folds into Review as a QA mode
after the whole testing prompt is moved to Review's QA mode; Custom's freeform
prompt moves to sling's freeform path; the freed `q` and `c` keys are
released.
**No implementation is in scope for this planning task**; these
requirements bind the *later, separately-approved* implementation.

Source of truth for process and artifact shape: the gascity.el redesign
artifacts (`gascity.el/plans/sling-command/{requirements,design,menu-mockups,
implementation-plan,decomposition,plan-review}.md`) and the binding "UI
direction" callout in `gascity.el/docs/DESIGN.md` §1 and `docs/DESIGN-agent-scrolling.md`.

## Problem Statement

beads.el today is a capable but *assembled* tool: its porcelain is a large
auto-generated transient tree (`beads` → `beads-ops-menu` /
`beads-advanced-menu` / `beads-more-menu`, plus one generated transient per
`beads-defcommand` class), the list/detail/dashboard were grown
incrementally, the agent subsystem exposes five roles across six backends,
and gascity.el currently owns the terminal (tmux attach/status/mouse/scroll)
that logically belongs in the base package. There is no documented extension
model — gascity.el reaches into beads.el internals. The result is a wide,
inconsistent surface that is hard to learn, hard to extend, and hard to
reason about.

The target bead asks for a *whole-mode* re-think into a world-class,
Magit-like Emacs porcelain for the `bd` bead store: deliberately designed,
keyboard-driven, sectioned buffers, with zero auto-generated transients as
the primary interface, a real magit/forge extension model, a standalone sling
abstraction, first-class formula UX, and an explicit slimming pass that
removes as well as adds. Refinement 2 raises the bar: `design.md` must be
implementation-ready (module map, full seam list with signatures, data/async
model, navigation/keymap contract, faces, sling/agent/formula, terminal
migration) and `menu-mockups.md` must render **every** surface in **multiple
states** (loading/empty/populated/folded/error and the sling/agent/formula
shapes) with no TBDs.

## Hard constraints (already decided; not open)

- **Hand-built UI only.** The primary interface is designed, not generated.
  `beads-meta` command classes remain the execution + parse layer.
  Auto-generated transients may survive **only** as a command-dispatch
  backend (per-list filters, the top menu), never as the porcelain.
- **beads.el must NOT depend on gascity.el.** The extension model is
  magit/forge: a stable, documented set of seams in beads.el that gascity.el
  (and others) build on, with the base package fully usable standalone.
- **Tight gascity.el integration, optional at runtime.** When gascity.el is
  present the beads.el UI integrates cleanly (city/rig context, gc-routed
  stores, agent/session views); when absent every core beads flow works.
- **Planning only.** Docs under `plans/beads-ui-redesign/` only; no source
  file is modified by this task.
- **F2 (decided).** `M-x beads` opens the status buffer; the transient becomes
  `beads-dispatch` on `?`; `beads-dashboard` stays the full board.
- **F3 (decided).** QA and Custom are removed entirely (not hidden); QA is a
  Review mode; Custom is the sling freeform path; `beads-agent-*` registries
  stay open for third-party re-registration.

## W6H

- **Who**: the beads.el user (an Emacs operator driving a `bd` store, often
  also a gascity.el user), and the maintainers of beads.el and gascity.el.
- **What**: a complete plan — requirements, design (with the concrete
  extension seam list), ascii mockups of every redesigned surface, a
  pruning-first implementation plan, a decomposition into work items, a
  review, and a slimming audit including the role roster.
- **When**: the plan is produced now; implementation follows after user
  sign-off, in the pruning-first order.
- **Where**: `plans/beads-ui-redesign/` in the beads.el checkout; later code
  lands in `lisp/` and moves out of `gascity.el/lisp/`.
- **Why**: a world-class, coherent, keyboard-first porcelain with fewer,
  better surfaces; a documented extension model so gascity.el is a thin
  extension rather than a fork; terminal handling in the base package.
- **How**: hand-built sectioned buffers on vui + `tabulated-list-mode`, the
  `beads-meta` execution layer retained, named extension seams, an adaptive
  sling flow, a first-class formula UX, an explicit slimming pass, and a
  code-movement plan for the terminal.

## User Stories

- **US-1 (entry)**: As an operator I type `M-x beads` and land on a Magit-like
  *status buffer* that summarises the store (counts, in-progress, ready,
  blocked, recent activity) and dispatches every command from there, so the
  mode has one obvious front door.
- **US-2 (navigation)**: As an operator, `q` buries, `g` refreshes, `TAB`/`SPC`
  fold sections, and TAB/S-TAB move between things — identically in every
  view — so I never re-learn movement per buffer.
- **US-3 (list + detail)**: As an operator I browse a sectioned bead list and
  drill into a sectioned detail view, acting on the bead at point with
  minimal prompting.
- **US-4 (sling, standalone)**: As an operator I press `S` and dispatch a bead
  to a local agent target with zero prompts when the context already answers
  the questions, and with typed guidance when it does not — no gascity
  required.
- **US-5 (sling, gascity)**: When gascity.el is installed, the same `S` flow
  additionally offers city/rig agents and dispatches through `gc sling`,
  with the same preview and validation affordances.
- **US-6 (agent launch)**: As an operator I start an agent for a bead, choose
  the role, worktree, backend and prompt in one coherent flow, and attach to
  or jump to the running session.
- **US-7 (formulas)**: As an operator I browse formulas, inspect their recipe
  and vars, and launch one against a bead, following the resulting workflow.
- **US-8 (extension author)**: As the author of gascity.el (or another
  package), I extend beads.el through documented seams — hooks, generics,
  registries, menu/section providers, keymap prefixes — without reaching
  into internals.
- **US-9 (slimming)**: As a new user, the surface I am shown is smaller and
  better designed than today's; removed commands point at their replacement.
  QA is reachable as a Review mode, Custom as the sling freeform path, and
  the freed `a q` / `a c` keys are released.
- **US-10 (terminal)**: As an operator attaching to an agent, the tmux
  attach/status/mouse/scroll behaviour ships in beads.el and works whether or
  not gascity.el is installed.

## Technical Stories

- **TS-1**: As the `beads.el` maintainer I retain `beads-defcommand` and the
  `beads-command-*` classes unchanged as the execution + parse layer and
  document them as a public extension surface.
- **TS-2**: As the `beads.el` maintainer I introduce a small number of named
  extension seams (list in `design.md` §4) and document each with the
  downstream use it enables.
- **TS-3**: As a test author I keep every existing `:unit` test meaningful and
  add coverage per work item; `eldev test` stays green (see
  `AGENTS.md` build commands).
- **TS-4**: As the TRAMP gatekeeper I verify every redesigned view over
  `/ssh:localhost:~/bright-lights` (per `AGENTS.md` remote-testing section).
- **TS-5**: As the gascity.el maintainer I keep gascity.el working through
  compatibility shims during the terminal move, then delete the duplicated
  code.

## Behavior Requirements

### Entry and navigation

- **REQ-001 — Status buffer is the entry point.** `M-x beads` (and the
  `beads` prefix command) opens a hand-built, vui-sectioned **status buffer**
  — not a transient — summarising the store and offering dispatch. Because
  `beads` today names the transient prefix, the dispatch transient is
  renamed `beads-dispatch` (bound to `?`; see `plan-review.md` F2) and the
  `beads` symbol becomes the status-buffer entry. `beads-dashboard` remains
  available as the full board and as the status buffer's superset.
- **REQ-002 — Universal navigation contract.** In every redesigned view:
  `q` buries the buffer, `g` refreshes in place, `TAB`/`SPC` toggle the
  section/thing at point, TAB/S-TAB move by `beads-thing` with wrap, `RET`
  drills in (visit/activate), `?` opens the dispatch menu. No view invents
  its own alternative.
- **REQ-003 — Hand-built porcelain.** The primary command surface of every
  redesigned view is hand-built. No auto-generated transient is the primary
  interface. Generated transients are permitted only as (a) the dispatch
  backend behind `?` and (b) per-list `/` filters.
- **REQ-004 — Magit-style dispatch menu.** A hand-designed top-level dispatch
  menu lists the mode's commands by frequency with descriptions; it is the
  only "menu" a new user needs.

### List and detail

- **REQ-005 — List redesign.** The bead list is a sectioned, keyboard-driven
  porcelain: a header (counts, active filter, store), status-grouped (or
  filter-grouped) sections, a sortable column layout, per-list filtering
  through the retained `/` transient, and context actions on point/marked
  rows with minimal prompting.
- **REQ-006 — Detail redesign.** `beads-show` is a sectioned detail buffer
  (header identity block; collapse-able Description / Dependencies /
  Sub-issues / Agent / Comments / History sections) with an action bar and
  inline context actions, and breadcrumbs back to the list/dashboard.
- **REQ-007 — Context actions.** Act on the issue at point or on marked
  issues with minimal prompting (`d` close, `C` claim, `s` status, `#`
  priority, `a` agent, `S` sling, `e` edit), consistently across list,
  detail and dashboard.

### Sling

- **REQ-008 — Standalone sling abstraction.** beads.el defines a target
  abstraction (`beads-sling-target`), a target-discovery seam
  (`beads-sling-target-functions`), and a dispatch generic
  (`beads-sling-dispatch`) that launch a bead to an agent target with **no**
  gascity dependency. Default targets are the local agent backends/roles and
  existing worktrees.
- **REQ-009 — Adaptive sling flow.** One entry point (`S`) covers the plain
  and formula (targeted / untargeted) shapes via one transient that
  re-specialises after each stage (What → Who → How → Preview → Launch →
  Follow). Shape is inferred and rendered as one sentence; a live footer
  shows summary + validation; `P` opens a full preview; preview never gates
  launch.
- **REQ-010 — gascity extension point.** A documented seam lets gascity.el
  contribute city/rig agent targets and supply the `gc sling` backend,
  without beads.el requiring gascity.el.

### Agent launch

- **REQ-011 — Agent-launch redesign.** The agent launch UX is redesigned end
  to end (role, worktree, backend, prompt/context, session lifecycle,
  display/attach). It is distinct from sling (launch is the direct local
  start; sling is dispatch-to-a-target) but shares target discovery, the
  preview footer, and session/attach handling.
- **REQ-012 — Curated roster and backends (F3, remove-entirely).** The
  exposed role roster is Task, Review (with a QA mode), Plan. The QA and
  Custom **classes** and everything that only serves them are **deleted**:
  `beads-agent-type-qa`, `beads-agent-type-custom`, their system/user
  prompts and `beads-agent-qa-backend` (the QA testing prompt is moved onto
  Review's QA mode), `beads-agent-start-qa`, `beads-agent-start-custom`, the
  `a q` / `a c` keybindings, and the registration calls. Custom's freeform
  prompt becomes the sling work-picker freeform escape. The `beads-agent-type`
  and `beads-agent-backend` **registries remain open** as extension seams,
  and the QA/Custom prompts are preserved on a documented path (Review QA
  mode / sling freeform). The affected tests are updated, not merely
  skipped. Backends are curated per `slimming.md` §3 (claude-code,
  agent-shell, terminal exposed; the rest behind `… other`).

### Formulas

- **REQ-013 — Formula browser.** A first-class browser lists formulas
  (grouped, with type/step/var counts), and a detail pane renders recipe
  steps and declared vars with their metadata.
- **REQ-014 — Formula launch.** A formula can be launched against a bead
  (targeted) or standalone; the launched workflow is followable. Standalone
  this means ironing the formula locally/via `bd`; with gascity.el present
  the same flow enriches to `gc` targets and run views.

### Code movement

- **REQ-015 — Terminal handling moves to beads.el.** The tmux
  attach/status/mouse/scroll behaviour currently in
  `gascity.el/lisp/gascity-terminal.el` moves to beads.el as
  `beads-terminal-tmux.el`, on top of the existing `beads-terminal.el`
  backends; generic entry point `beads-terminal-attach`.
- **REQ-016 — Exact migration list + compatibility.** `design.md` names the
  exact files and symbols to move, and gascity.el keeps working through a
  thin compatibility shim during and after the move.

### Consistency

- **REQ-017 — One design language.** One faces palette (with documented face
  names and extension-by-inherit), one layout vocabulary, one
  section/`vui`/`tabulated` usage rule, one set of status/priority/agent
  glyphs across every view.
- **REQ-018 — One interaction language.** Keybindings, mode-line,
  async-reader behaviour and remote/TRAMP handling are uniform. Extension
  keys live under a reserved prefix, not by shadowing core keys.
- **REQ-019 — Remote parity.** Every redesigned view works identically over a
  TRAMP store; buffer identity is host-qualified; a view opened with an
  explicit `:directory` does no wrong-side I/O.

### Extension model

- **REQ-020 — Concrete seam list.** beads.el exposes a named, documented set
  of magit/forge-style seams (hooks, generics, registries, menu/section
  providers, keymap prefixes, faces). `design.md` §4 enumerates each with
  its downstream use.
- **REQ-021 — Standalone.** Every core flow works with gascity.el absent:
  sling to local targets, agent launch, formula browse/launch, list/detail/
  dashboard, terminal attach.
- **REQ-022 — Optional integration.** With gascity.el present, the beads UI
  gains city/rig store scoping, gc sling targets/backend, and agent/session
  views, all through the documented seams and all optional at runtime.

### Slimming

- **REQ-023 — Collapse redundant menus.** `beads-more-menu` is removed;
  `beads-ops-menu` and `beads-advanced-menu` collapse into a single
  maintenance/infrastructure menu reached from the dispatch menu.
- **REQ-024 — Demote generated transients.** Per-command auto-generated
  transients leave the primary key path; only the dispatch backend and the
  per-list filter transient survive.
- **REQ-025 — Slim the role roster.** The exposed agent-role surface is
  reduced with a concrete replacement for each removal (see `slimming.md`).
- **REQ-026 — Every removal earns its replacement.** `slimming.md` lists
  every removal/collapse with a one-line justification and the replacement
  affordance; anything kept must earn its place.

### Plan artifacts

- **REQ-027 — Artifacts exist and are self-consistent.** `requirements.md`,
  `design.md`, `menu-mockups.md`, `decomposition.md`,
  `implementation-plan.md`, `plan-review.md`, `slimming.md` under
  `plans/beads-ui-redesign/`, mutually consistent and traced.
- **REQ-028 — Every surface is mocked up.** Every redesigned user-facing
  element has an ascii mockup with the real rendering rules (groups,
  collapse, keys); no "TBD" placeholders.
- **REQ-029 — Review round recorded.** `plan-review.md` records at least one
  round of critique with findings and resolutions.
- **REQ-030 — No source modified.** This planning task modifies no `.el`
  file.
- **REQ-031 — Pruning-first plan.** `implementation-plan.md` orders
  removals before new surfaces.
- **REQ-032 — Decomposition into work items.** `decomposition.md` breaks the
  plan into beads-sized work items with dependencies and REQ traceability.
- **REQ-033 — Documentation plan.** The plan names the manual/doc deliverable
  (README architecture section + `doc/` chapter as applicable) as part of
  acceptance for the implementation, not an afterthought.

## Acceptance Criteria (for this planning task)

1. All seven artifacts exist under `plans/beads-ui-redesign/` and are
   mutually consistent (REQ-027).
2. A concrete slimming audit exists — an explicit list of removed/collapsed
   surfaces including the role roster, each with a replacement
   (REQ-023…REQ-026, `slimming.md`).
3. Every UI surface has an ascii mockup with real rendering rules and no
   "TBD" (REQ-028, `menu-mockups.md`).
4. The magit/forge extension seams are concrete — named
   functions/generics/hooks/vars with downstream uses (REQ-020,
   `design.md` §4).
5. The code-movement plan lists the exact files/symbols to move and the
   compatibility shim (REQ-015, REQ-016, `design.md` §6).
6. `plan-review.md` records at least one critique round (REQ-029).
7. No `.el` file is modified (REQ-030).
8. The implementation plan is pruning-first and the decomposition is
   beads-sized with dependencies (REQ-031, REQ-032).
9. Implementation is clearly marked out of scope until sign-off
   (front matter + `README.md`).

## Out Of Scope (for this planning task)

- Any `.el` source change; any commit that is not a planning doc.
- The actual implementation, tests, and manual chapter (they are planned
  here, executed later).
- gc-side changes; gascity.el changes beyond the planned compatibility shim
  (gascity.el edits are the downstream bead, not this one).
- Re-deciding the hard constraints above.

## Open Questions

None blocking. Residual detail (exact reserved extension key prefix, exact
face names, exact shim symbol aliases) is resolved in `design.md` and is an
implementation detail, not a requirements question. **The role-roster cut is
no longer open:** F3 is decided (remove QA and Custom entirely; QA → Review
QA mode; Custom → sling freeform). `slimming.md` §3 records the
remove-entirely wording and the freed `q`/`c` keys; `plan-review.md` records
the decision and its one-release aliases.
