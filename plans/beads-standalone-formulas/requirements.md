---
schema: beads.standalone-formulas.requirements.v1
workflow:
  id: be-1qta
  kind: planning
artifact: requirements
status: draft-for-review
scope: planning-only
implementation: out-of-scope-until-signoff
extends:
  plan: plans/beads-ui-redesign
  branch: wi18-integration
  pr: 67
---

# beads.el Standalone-first Formula → Molecule Workflow — Requirements

This artifact restates bead `be-1qta` as verifiable requirements for a
**standalone-first** beads.el: the whole `formula → proto → molecule` lifecycle
driven from Emacs with `bd` alone, for a regular agent or a human, with **no
gascity**. It extends `plans/beads-ui-redesign/` (PR #67, `wi18-integration`)
rather than replacing it. Planning only; no `.el` source changes.

Source of truth for `bd` semantics:
`~/workspace/beads/docs/workflows/{formulas,molecules,wisps,gates}.md`,
`docs/getting-started/ide-setup.md`, and
`docs/cli-reference/{formula,cook,mol,gate,prime,setup,purge}.md`.

## Problem Statement

PR #67 redesigned the issue-side porcelain (dashboard, list, show, dispatch,
sling, agents) and added the **execution + parse layer** for formulas,
molecules and gates: Lisp `bd` command classes exist for every leaf
(`beads-command-formula*`, `beads-command-mol*`, `beads-command-gate*`,
`beads-command-cook`, `beads-command-prime`), plus a formula browser/detail and
a generic `beads-formula-launch` seam. What is missing is the **workflow
porcelain**: there is no molecule execution view, no work loop, no gate UI, no
wisp lifecycle UI, no bonding flow, no formula-authoring surface, no designed
swarm surface, and the instantiate path collapses pour-vs-wisp into a single
implicit `bd mol pour`.

The result: a regular agent can browse a formula but cannot *work* the
molecule it creates from inside Emacs; a human cannot see `[done]/[current]/
[ready]/[blocked]/[pending]`, claim the next ready step, close it, resolve or
create a gate, or clean up a wisp without dropping to a shell. The point of
this plan is to close that gap so the entire standalone loop is drivable from
Emacs, and so the gascity extension stays optional enrichment.

A **ninth gap** sits alongside the eight in the original bead: `bd swarm` —
the documented coordination wrapper around a large epic's DAG (`bd swarm
create/list/status/validate`) — has command classes and a minimal transient in
beads.el, but no designed surface. A swarm is *the* standalone way to work a
large epic in parallel (`bd ready --claim` for self-selection, `bd assign` for
hand-out, gates/`waits-for` for fan-in, merge slots for conflict-prone merges),
so leaving it as raw command output under the Maintenance menu is inconsistent
with the rest of this plan. Section I and `REQ-SF-090..099` add it.

## Hard Constraints (already decided; not open)

- **Standalone-first. Every flow works with `bd` alone.** Gascity is optional
  enrichment surfaced through the documented seams, never a dependency of a
  core flow. This is a hard acceptance (REQ-SF-080).
- **Reuse PR #67; do not fork it.** The command classes, the
  `beads-formula-launch` generic, `beads-formula-var-reader`, the sling
  abstraction, `beads-thing` movement, the faces vocabulary and the async
  reader are the substrate. New code composes them (REQ-SF-081).
- **Hand-built UI only.** New surfaces are designed buffers, not
  auto-generated transients. `beads-meta` command classes stay the execution +
  parse layer; auto-generated transients survive only as the dispatch backend
  and per-list filters (PR #67 hard constraint, unchanged).
- **beads.el must NOT depend on gascity.el.** No `(require 'gascity)` and no
  gascity symbol in a default path (PR #67 hard constraint, unchanged).
- **Planning only.** Docs under `plans/beads-standalone-formulas/` only; no
  source file is modified by this task.
- **No TBDs.** Every surface is decided: requirements, design and ASCII
  mockups are implementation-ready. Open questions, if any, are recorded
  explicitly in `plan-review.md` as accepted follow-ups, not left inline.

## W6H

- **Who**: a regular agent or human (Emacs operator) driving a `bd` store with
  the `bd` CLI installed, and the maintainers of beads.el and gascity.el.
- **What**: a complete standalone-first plan for the formula→molecule→swarm
  lifecycle in Emacs — requirements, design (modules, seams, data/async), ASCII
  mockups of every surface in multiple states, a pruning-first implementation
  plan, a decomposition, and a recorded critique round.
- **When**: the plan is produced now; implementation follows after sign-off,
  on top of PR #67, in the pruning-first order.
- **Where**: `plans/beads-standalone-formulas/`; later code lands under
  `lisp/`.
- **Why**: so the full `bd` workflow template lifecycle is first-class in
  Emacs without gascity, and gascity becomes a thin enrichment rather than the
  only way to run a molecule or swarm an epic.
- **How**: a molecule execution view on the PR #67 section vocabulary, a
  one-key work loop over `bd ready --mol`/`update --claim`/`close`, first-class
  gate and wisp views, a typed instantiate flow that makes pour-vs-wisp an
  explicit choice, a TOML authoring surface with schema validation, an
  optional agent hand-off, and a swarm surface (fleet list, computed status
  board, validate/waves, worker lanes, create+coordinator, claim/assign/
  hand-off).

## Relationship to PR #67 (reused vs new)

**Reused unchanged (do not redesign):** `beads-command-{formula,mol,gate,
cook,prime,swarm}*` classes; the `beads-swarm` transient prefix (its suffixes
are rewired to the new views, no key removed); `beads-formula-launch`,
`beads-formula-var-reader`, `beads-formula-launch-context`,
`beads-formula-launch-standalone`, `beads-formula-seed-sling`;
`beads-formula-list-mode` / `beads-formula-show-mode`; the sling abstraction
(`beads-sling-*`); the agent subsystem and backends; `beads-thing`;
`beads-dispatch`; the faces palette and the async reader.

**Extended (new behaviour on an existing symbol):**
`beads-formula-launch` gains an explicit pour-vs-wisp choice; the formula
detail gains phase/extended/bond-point rendering; `beads-command-mol`'s
parent transient gains the new view entry points.

**New modules:** `beads-molecule.el` (execution view + work loop),
`beads-gate.el` (gate UI), `beads-wisp.el` (wisp lifecycle UI),
`beads-swarm.el` (swarm fleet list, status board, validate/waves, worker
lanes, swarm actions + navigation), `beads-cook.el` (cook preview),
`beads-formula-edit.el` (TOML authoring + schema validation),
`beads-handoff.el` (`bd prime` / `bd setup` / agent hand-off); plus
`beads-command-cook.el` and `beads-command-prime.el` split out of
`beads-command-misc.el` to honour the one-file-per-subcommand convention.
(`beads-command-swarm.el` stays the command+parse layer and keeps the
`beads-swarm` transient; `beads-swarm.el` is the porcelain, exactly as
`beads-command-gate.el` / `beads-gate.el` are split.)

## User Stories

- **US-1 (browse)**: As an operator I open the formula browser, see type-grouped
  formulas with their provenance (project/user/all and shadowing), and filter
  by type, so I know exactly which template a name resolves to.
- **US-2 (inspect)**: As an operator I open a formula and see its full recipe:
  vars with type/enum/pattern/default/required, steps with type/needs/gate,
  phase, `extends`, and bond points, so I can judge it before running it.
- **US-3 (cook)**: As an operator I preview a cook in compile or runtime mode,
  with a `--dry-run` step tree and optional `--persist`, so I can inspect a
  proto without touching the database.
- **US-4 (instantiate)**: As an operator I choose pour (persistent) or wisp
  (ephemeral) explicitly, get the formula's phase recommendation, enter typed
  variable values with validation, set an assignee, and see the resulting
  molecule root.
- **US-5 (work a molecule)**: As an operator I open a molecule view showing
  `[done]/[current]/[ready]/[blocked]/[pending]`, progress and ETA, toggle a
  ready-only frontier, and drive the loop — claim the next ready step, do the
  work, close it — one key at a time, entirely inside Emacs.
- **US-6 (gates)**: As an operator I list gates, inspect a gate and its
  waiters, create human/timer/gh:run/gh:pr gates, run `bd gate check`, resolve
  a gate, add a waiter, and discover a run id — and I see gated steps inline in
  the molecule view.
- **US-7 (wisps)**: As an operator I list wisps, squash them into a digest
  (promoting to persistent), burn or purge them, and see the phase of a
  molecule root.
- **US-8 (bond)**: As an operator I bond protos/molecules/formulas with a bond
  type, phase override, dynamic `--ref`, and a dry-run preview.
- **US-9 (author)**: As an operator I create/edit `.formula.toml` in Emacs with
  schema-driven completion and validation, convert JSON↔TOML, and distill a
  reusable formula from an existing epic.
- **US-10 (hand off)**: As an operator I launch an agent against a molecule or
  formula, surface `bd prime` context and `bd setup` policy status, all without
  gascity.
- **US-11 (standalone)**: As an operator with only `bd` installed, every flow
  above works; installing gascity adds city/rig targets and run views without
  changing any of them.
- **US-12 (swarm)**: As a coordinator I create a swarm for an epic (or
  auto-wrap a lone issue), name a coordinator, list all swarms with progress
  and active workers, and open a first-class status board showing Completed /
  Active (with assignee) / Ready / Blocked groups, so I can run a large epic in
  parallel without leaving Emacs.
- **US-13 (validate + waves)**: As a coordinator I run `bd swarm validate` and
  see dependency-direction warnings, orphans/missing-deps/cycles/disconnected
  subgraphs, the ready fronts (parallel waves), estimated worker-sessions and
  maximum parallelism, and I can drill into the offending bead; I can also see
  which workers are idle versus saturated against the max-parallelism
  headroom.

## Behaviour Requirements

Requirement IDs are `REQ-SF-NNN`, namespaced to avoid collision with the PR #67
`REQ-NNN` set. Each requirement names the gap it closes and the surface in
`menu-mockups.md` that renders it.

### A. Formula lifecycle

- **REQ-SF-010 — Provenance and search-path shadowing.** The browser shows each
  formula's source path and a `shadowed` marker when a same-name formula exists
  lower on the search path; a scope filter switches between project / user /
  all. Scope order is the `bd` order: resolved-beads-dir, checkout-root,
  `~/.beads`, `$GT_ROOT`. *(mockup §1)*
- **REQ-SF-011 — Complete detail.** The detail renders `type`, `description`,
  `version`, `phase`, `extends`, every var (`type`, `enum`, `pattern`,
  `default`, `required`), every step (`type`, `needs`/`depends_on`, `gate`,
  `waits_for`), bond points, and the source path. Only warnings `bd` itself
  emits (real validation errors) are shown. *(mockup §2)*
- **REQ-SF-012 — Cook preview.** `c` cooks the formula at point with an
  explicit mode: compile (placeholders kept) or runtime (vars substituted),
  `--dry-run` step-tree preview, `--persist` (optional `--force`, `--prefix`),
  and `--var`. The preview shows the exact step/dependency tree that would be
  created; nothing is written unless `--persist`. *(mockup §3)*
- **REQ-SF-013 — Explicit pour vs wisp.** The instantiate flow makes the phase
  an explicit choice — pour (persistent) and wisp (ephemeral), plus wisp
  `--root-only`; when the formula declares `phase = "vapor"`, wisp is the
  recommended default and choosing pour shows the `bd` warning; when it does
  not, pour is the default. The choice is never implicit. *(mockup §4)*
- **REQ-SF-014 — Typed variable entry and validation.** Variable entry reuses
  `beads-formula-var-reader`; `enum` → completing-read, `bool` → y-or-n,
  `numeric` → read-number, `file`/`directory` → path readers, `agent` → target
  completion, else string. Required-missing, pattern mismatch, enum-outside and
  non-numeric values block launch with the var named. *(mockup §4)*
- **REQ-SF-015 — Assignee / `--for`.** The instantiate flow can assign the root
  to an agent (`--assignee`) or freeform user, and the molecule view can scope a
  query with `--for <agent>`. Default is unassigned. *(mockup §4, §5)*
- **REQ-SF-016 — Follow the result.** After a successful instantiate the
  molecule view opens at the created root (id from the `bd --json` result);
  after a cook `--persist` it opens the proto. Failure leaves the preview open
  with the error. *(mockup §5)*

### B. Molecule execution (the standalone loop)

- **REQ-SF-020 — Molecule view.** A first-class view renders the root plus its
  step tree with per-step state `[done]/[current]/[ready]/[blocked]/[pending]`
  derived from `bd mol current`, retaining dependency indentation. The root
  header shows id, title, phase, assignee, and `bd mol progress`. *(mockup §5)*
- **REQ-SF-021 — Progress and ETA.** The header (and a `p` refresh/goto)
  shows completed/total, percentage, rate (steps/hour) and ETA from
  `bd mol progress`; a large molecule (`>100` steps) renders a summary row and
  uses `--limit`/`--range` windows. *(mockup §5)*
- **REQ-SF-022 — Ready frontier.** `r` toggles a ready-only filter that runs
  `bd ready --mol <root>` and dims/folds non-ready steps, so the operator sees
  exactly what can start now. *(mockup §5)*
- **REQ-SF-023 — Work loop.** The view offers the whole loop: `n` selects the
  next ready step, `c` claims it atomically (`bd update --claim`), `x` closes it
  with a required reason, and the view refreshes and advances. A single
  `C-c C-c`-style "advance" is not used; each step is explicit. The loop needs
  only `bd`. *(mockup §6)*
- **REQ-SF-024 — Step actions.** `RET` inspects a step in the issue detail
  view; `c`/`x` claim/close; `TAB`/`S-TAB`/`SPC` follow the `beads-thing`
  movement contract; `o` opens the step in the list view. *(mockup §5)*
- **REQ-SF-025 — Root close-eligible.** When every child is closed the view
  offers `C` to sweep the root with `bd epic close-eligible` (preview then
  apply), so a finished molecule does not linger open. *(mockup §5)*

### C. Gates

- **REQ-SF-030 — Gate list.** A `tabulated-list-mode` view lists open gates by
  default (`--all` includes closed) with type, await-id, timeout, waiter count
  and target; `t` filters by type. *(mockup §7)*
- **REQ-SF-031 — Gate detail.** A gate detail view shows type, status,
  timeout/expiry, await-id, repo, reason, blocked issue(s) and waiters, using
  the same section vocabulary as the issue detail. *(mockup §7)*
- **REQ-SF-032 — Gate actions.** Create (`human`/`timer`/`gh:run`/`gh:pr` with
  `--blocks`, `--await-id`, `--timeout`, `--reason`), check (with `--type` and
  `--dry-run`), resolve (required reason), add-waiter (gate + waiter), and
  discover (`--branch`, `--limit`, `--max-age`, `--dry-run`) are all reachable
  from the gate list/detail. *(mockup §7)*
- **REQ-SF-033 — Molecule integration.** A gated step renders with a gate glyph
  and the gate id; `RET` on it opens the gate detail; `bd ready --gated`
  results are surfaced as a "gate just closed" refresh on the molecule view.
  *(mockup §5, §6)*

### D. Wisps

- **REQ-SF-040 — Wisp list.** A view lists wisps (`--all`, `--type`), marks
  those not updated in 24h+ as old, and shows status/start/updated. *(mockup
  §8)*
- **REQ-SF-041 — Wisp lifecycle actions.** Squash (with an optional
  agent-provided `--summary` and `--keep-children`), burn (`--dry-run`,
  `--force`, batch), and purge (`--dry-run`, `--force`, `--older-than`,
  `--pattern`) are reachable and require confirmation for destructive paths.
  *(mockup §8)*
- **REQ-SF-042 — Phase visibility and promotion.** The molecule root shows
  `phase: persistent|vapor`; promoting a wisp (squash, or bond `--pour`)
  updates the display. `bd mol bond --pour/--ephemeral` phase overrides are
  honoured. *(mockup §8, §9)*

### E. Bonding and composition

- **REQ-SF-050 — Bond flow.** A two-operand completion (formula/proto/molecule
  for A and B), a bond type (sequential/parallel/conditional, default
  sequential), and optional `--as` for proto+proto, driven from the molecule or
  formula view. *(mockup §9)*
- **REQ-SF-051 — Phase override, dynamic ref, preview.** The bond flow exposes
  `--pour`/`--ephemeral`, `--ref` with `{{var}}` substitution and `--var`, and
  a `--dry-run` preview of the created/attached issues before committing.
  *(mockup §9)*
- **REQ-SF-052 — Bond points in detail.** Formula detail renders declared
  `compose.bond_points` (`id`, `description`, `before_step`/`after_step`,
  `parallel`) as attachment sites, and the bond flow can target a named bond
  point. *(mockup §2, §9)*

### F. Formula authoring

- **REQ-SF-060 — Create/edit TOML.** Create a new `.formula.toml` from a
  minimal scaffold, and open an existing formula's source in Emacs TOML mode
  (or `toml-mode` if available) in the correct search-path directory, with the
  buffer associated to the formula. *(mockup §10)*
- **REQ-SF-061 — Schema completion and validation.** Completion and on-save
  validation are driven by `bd formula schema` (already a command class), using
  `beads-formula-schema-struct` field names/types/tags; wrong types and
  missing required fields are reported inline and in a compile-style results
  buffer. (Surfacing unknown keys is out of scope; see Out Of Scope.)
  *(mockup §10)*
- **REQ-SF-062 — JSON↔TOML convert.** `bd formula convert` is surfaced as a
  one-key action on a JSON formula with `--stdout` and `--delete` variants,
  writing `.formula.toml`. *(mockup §10)*
- **REQ-SF-063 — Distill.** `bd mol distill <epic>` is surfaced from the issue
  detail/epic view with `--var value=variable` mapping, `--output`, `--dry-run`
  preview, and follow to the created `.formula.toml`. *(mockup §10)*

### G. Standalone agent hand-off and context

- **REQ-SF-070 — Hand off a molecule/formula to an agent.** Launch an agent
  (reusing `beads-agent` + backends and the 4-arity
  `beads-agent-backend-start`) with the molecule root or formula in the user
  prompt envelope; the molecule view exposes `a` to hand off at the root or at
  a step. *(mockup §11)*
- **REQ-SF-071 — `bd prime` context surface.** The current operational context
  (`bd prime`, optionally `--full`/`--memories-only`/`--stealth`) is viewable
  and copyable/insertable into an agent prompt without leaving Emacs. *(mockup
  §11)*
- **REQ-SF-072 — `bd setup` recipe/policy visibility.** A view lists installed
  recipe status (via `bd setup <tool> --check`/`--list`) and the active policy
  (`agent.profile` / `BD_AGENT_PROFILE`), and can run install/remove for a
  chosen recipe. No policy is written silently. *(mockup §11)*

### H. Standalone guarantee and consistency

- **REQ-SF-080 — No-gascity hard acceptance.** Every flow in A–I works with
  `bd` alone. A `beads-standalone-guard-test.el` test loads each new module
  with gascity absent, walks each flow's entry function through a mocked
  `beads-command-execute`, and fails if a default path references a gascity
  symbol. *(test surface, §4 of design)*
- **REQ-SF-081 — Reuse, not fork.** New surfaces compose PR #67 seams:
  `beads-formula-launch` (extended, not replaced),
  `beads-formula-var-reader`, `beads-sling-*`, `beads-thing`, the faces
  palette and `beads-command-execute-async`. The reuse-vs-new matrix in this
  document and in `design.md` §6 is authoritative.
- **REQ-SF-082 — Consistency.** One glyph set, one faces set (derive with
  `:inherit`), one keymap vocabulary, uniform async/loading/empty/error states,
  and remote/TRAMP parity for every new view. New top-level entries live in the
  dispatch and maintenance menus; extension keys use `C-c b`, not core keys.
- **REQ-SF-083 — Plan artifact set.** This plan ships `requirements.md`,
  `design.md`, `menu-mockups.md`, `decomposition.md`,
  `implementation-plan.md`, and `plan-review.md` in
  `plans/beads-standalone-formulas/`, referencing (not duplicating) PR #67.
- **REQ-SF-100 — Cross-repo code ownership (no duplicate logic with
  gascity.el).** beads.el owns the generic, `bd`-driven machinery
  (formula var reading/validation, launch/instantiate, molecule/gate/wisp/
  bond/swarm porcelain, terminal/tmux, faces, tabulated/section rendering).
  gascity.el must consume those seams and host only `gc`-specific enrichment
  (city/rig scoping, live sessions/pools, the run view). Where gascity.el
  currently contains a second implementation of beads-native logic, that code
  is **moved to beads.el** and gascity.el is reduced to a thin caller/shim.
  New code added by this plan must not duplicate a gascity.el implementation.
  The audit and any gascity.el edits land in the gascity.el rig under a bead
  linked to this epic (explainable in `decomposition.md` WI-SF-19); no
  gascity.el edit is made from the beads.el worktree. *(design §6;
  acceptance §"Cross-repo parity")*

### I. Swarm / coordination (gap #9)

A *swarm* is the `bd` coordination wrapper for a large epic: a molecule marked
`mol_type=swarm` linked to the epic, optionally naming a coordinator, that
can be picked up by any coordinator agent. `bd swarm status` is computed
from the beads themselves, so the UI is a live projection of the DAG, not a
stored view. All swarm flows are standalone (`bd` alone); gascity only
enriches worker lanes when present.

- **REQ-SF-090 — Swarm lifecycle from epic/molecule/list.** Create a swarm
  from an epic (auto-wrapping a single non-epic issue, reported to the
  operator), with an optional `--coordinator` and an explicit `--force` for
  the duplicate case; list, status and validate are first-class actions.
  Reachable from the primary dispatch, the issue/epic view, the molecule view
  and the swarm list — never buried only in Maintenance. Reuses the existing
  `beads-command-swarm-{create,list,status,validate}` classes and extends the
  existing `beads-swarm` transient; the new surfaces live in `beads-swarm.el`.
  *(mockup §14a, §14i)*
- **REQ-SF-091 — Swarm fleet list.** A first-class list of
  `bd swarm list --json` items: id, title, epic id/title, status, coordinator,
  progress (completed/total, %), active count. It is the entry hub: `RET`
  opens the status board, `v` validates, `W` shows worker lanes, `E` jumps to
  the epic, `x` jumps to the swarm molecule. Empty and filterable (`t`
  type/status, `a` all vs active). *(mockup §14a, §14b)*
- **REQ-SF-092 — Status board.** A first-class view of
  `bd swarm status --json`: header progress (completed/total, %) plus
  `Completed` / `Active (assignee)` / `Ready` / `Blocked` groups with counts
  and dependency annotations (`blocked_by`), each row a `beads-thing`; `RET`
  jumps to the issue detail; async refresh coalesces per `(swarm-status id)`;
  large epics window/paginate. *(mockup §14c, §14d)*
- **REQ-SF-093 — Validate / waves view.** Render `bd swarm validate --json`:
  the `swarmable` verdict, `warnings` and `errors`, the **ready fronts
  (parallel waves)** as a wave table, `max_parallelism`,
  `estimated_sessions`, and `--verbose` per-issue nodes with their `wave`,
  `depends_on`/`depended_on_by`; orphan / missing-dependency / cycle /
  disconnected-subgraph warnings are grouped and each drill-ins to its bead.
  A non-swarmable result (`swarmable=false` plus `errors`) is a **domain
  state**, not a command error. *(mockup §14f, §14g)*
- **REQ-SF-094 — Worker / parallelism visualization.** Show assignee
  **lanes** (who holds what `[current]`, and their in-progress beads via
  `bd list --assignee <a> --status in_progress`), the ready fronts, and the
  headroom against `max_parallelism` (idle slots = `max_parallelism -
  active_count`), so a coordinator can see idle versus saturated workers. The
  lane data comes from `beads-swarm-worker-source`; the standalone default is
  `bd list`, gascity may enrich with live sessions/pools. *(mockup §14h)*
- **REQ-SF-095 — Swarm actions.** One-key **assign**
  (`bd assign <step> <agent>`), **claim** (`bd update <step> --claim`),
  **hand off** (assign + comment; reuses the `beads-handoff` envelope),
  close/reopen a step, create/bind the swarm, and set/replace the coordinator
  (the swarm molecule's assignee). Merge slots (`bd merge-slot
  check/acquire/release`) and gates are integrated where they apply to
  coordination. Destructive/overriding actions (`--force`, coordinator
  replace) confirm first. *(mockup §14i, §14j, §14k)*
- **REQ-SF-096 — Swarm navigation.** epic ↔ swarm ↔ molecule/step, and swarm
  ↔ worker (assignee) ↔ that worker's in-progress beads; from the validate
  wave table into the offending bead; from a status-board row into the issue
  detail and back. One `beads-thing` movement scheme; `q`/`g`/
  `RET`/`TAB`/`SPC` consistent with every other view. *(mockup §14l)*
- **REQ-SF-097 — Standalone-first swarm; optional gascity enrichment.**
  Every swarm flow works with `bd` alone. The existing `beads-swarm`
  transient stays usable as the dispatch backend; gascity, when present,
  overrides `beads-swarm-worker-source` / `beads-swarm-display` to add live
  sessions and city/rig scoping, but no default path references a gascity
  symbol (extends REQ-SF-080). *(test surface, design §13)*
- **REQ-SF-098 — Swarm states.** Empty (no swarms; or the epic is not
  swarmable), loading, command error, JSON **domain error** (`{error: "swarm
  already exists", ...}` / `{error: "epic is not swarmable", ...}` — `bd`
  exits 0 in these cases, so the UI must inspect the payload, not the exit
  code), validated-with-warnings, and all-complete (100%, every child closed,
  root still open). Big-epic state: many steps, accurate progress, windowed
  rendering. *(mockup §14b, §14d, §14e, §14g, §14n)*
- **REQ-SF-099 — Reuse, not fork; no re-parse.** The swarm UI composes the
  existing command classes and the existing transient; it adds typed result
  classes (`beads-swarm-list-item`, `beads-swarm-status`,
  `beads-swarm-status-issue`, `beads-swarm-analysis`, `beads-ready-front`,
  `beads-swarm-issue-node`) to
  `beads-types.el` and `:result` declarations to `beads-command-swarm.el`. It
  does not invent a second JSON parser or a parallel command layer, and it
  keeps the `coordinator` semantics (`swarm.assignee`). *(design §4, §5.4, §8.4)*

## Acceptance Criteria (for this planning task)

- Every gap listed in the bead (formula lifecycle, molecule execution, gates,
  wisps, bonding, authoring, hand-off, no-gascity guarantee) **plus the swarm
  gap (#9)** has one or more concrete `REQ-SF-*`, a module in `design.md`, and
  a stateful mockup in `menu-mockups.md` with no TBDs. The swarm surface is
  ascii-mocked for list, status board, validate/waves, worker/parallelism,
  create+coordinator, claim/assign/hand-off, navigation, and empty/loading/
  error/big-epic states.
- `design.md` names the exact new/changed modules, the seam list with
  signatures, the data/async model, and the keymap/glyph/faces additions.
- The reuse-vs-new mapping from PR #67 is explicit and complete, and the
  swarm section **explicitly reuses** the existing
  `beads-command-swarm-{create,list,status,validate}` classes and the
  `beads-swarm` transient while naming what is new (typed result classes, the
  porcelain views, and the domain-error detector).
- Standalone (no gascity) is a hard requirement for every swarm flow;
  gascity enrichment of worker lanes is optional and seam-bound.
- `decomposition.md` maps every `REQ-SF-*` to a work item (`WI-SF-*`) and the
  dependency graph has a single critical path.
- `implementation-plan.md` sequences the work pruning-first and states
  verification, risks and rollback.
- `plan-review.md` records a critique round with findings, dispositions, and
  any accepted follow-ups.
- No `.el` file is modified.

## Out Of Scope (for this planning task)

- Any source change. Implementation begins only after sign-off.
- gascity-owned surfaces (city/rig scoping, `gc` run view, tmux terminal);
  only the seams they build on are in scope.
- Re-litigating PR #67 decisions (hand-built UI, dispatch contract, sling
  model, terminal move).
- Any gascity-owned swarm surface (live city/rig worker pools); only the
  standalone worker-lane default and the gascity enrichment seams are in
  scope.
- The `bd` side: no new `bd` CLI flags are requested; the UI targets the
  surface documented in `docs/cli-reference/`.
- **Unknown-key fidelity warnings.** `bd` silently drops unknown formula TOML
  keys; a client-side raw-TOML-vs-decoded-model diff would duplicate `bd`'s
  formula schema and drift, so surfacing unknown keys is out of scope and
  deferred to an upstream `bd` change. (The known `Step.OnComplete` field is
  parsed but its runtime execution is not yet wired; it is not an example of a
  dropped key.)
