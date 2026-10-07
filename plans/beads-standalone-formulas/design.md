---
schema: beads.standalone-formulas.design.v1
artifact: design
status: draft-for-review
scope: planning-only
implementation: out-of-scope-until-signoff
requirements: requirements.md
extends:
  plan: plans/beads-ui-redesign
  branch: wi18-integration
  pr: 67
---

# beads.el Standalone-first Formula → Molecule Workflow — Design

## 1. Thesis

PR #67 gave beads.el the *issue* porcelain and the *execution layer* for
formulas, molecules and gates. This plan adds the missing *workflow porcelain*:
a molecule execution view with a one-key standalone work loop, first-class gate
and wisp views, an explicit pour-vs-wisp instantiate flow, a bonding flow, a
TOML authoring surface, and an optional agent hand-off.

The design rule is **compose at the seams PR #67 already published**:
`beads-formula-launch` is extended (not replaced), variable reading stays
`beads-formula-var-reader`, movement stays `beads-thing`, async stays
`beads-command-execute-async`, faces derive with `:inherit`, and the new
top-level entries hang off `beads-dispatch` / `beads-maintenance`. Every new
surface must render with gascity absent; gascity overrides the launch/follow
generics to add its run view, exactly as it does for sling.

## 2. Non-goals (explicitly decided)

- No new `bd` subcommands or flags. The UI targets the documented CLI surface.
- No re-implementation of the formula browser/detail (`beads-formula.el`) or
  the formula/sling launch seam; this plan extends them.
- No terminal/tmux work (PR #67's migration owns that); the agent hand-off
  reuses `beads-agent`'s terminal spawn.
- No gascity code changes; only the seam list and the no-gascity guard are in
  scope.
- No client-side warning for unknown formula TOML keys. `bd` silently drops
  them; a raw-TOML-vs-decoded-model diff would duplicate `bd`'s schema and
  drift. Surfacing unknown keys is out of scope and deferred to an upstream
  `bd` change. Only warnings `bd` itself emits are rendered.

## 3. Surface inventory

| Surface | Mode | Entry | New? |
|---|---|---|---|
| Formula browser (provenance, scope) | tabulated | `M-x beads-formula-browse` (extend) | extended |
| Formula detail (phase/extends/bond points) | sectioned | `RET` in browser | extended |
| Cook preview | sectioned/compilation | `c` from browser/detail | new (`beads-cook.el`) |
| Instantiate flow (pour/wisp + typed vars) | transient | `s` from browser/detail | extended |
| Molecule execution view + work loop | sectioned | `M-x beads-molecule`, follow | new (`beads-molecule.el`) |
| Gate list | tabulated | `M-x beads-gate-list` | new (`beads-gate.el`) |
| Gate detail | sectioned | `RET` in gate list / gated step | new |
| Wisp list / lifecycle | tabulated | `M-x beads-wisp-list` | new (`beads-wisp.el`) |
| Bond flow | transient + preview | `M-x beads-bond` / `b` in molecule | new (`beads-bond.el`) |
| Formula authoring / validation | fundamental-ish | `M-x beads-formula-new` / source | new (`beads-formula-edit.el`) |
| Distill | transient | `D` in epic/show | new in `beads-formula-edit.el` |
| Agent hand-off | transient | `a` in molecule / formula | new (`beads-handoff.el`) |
| Prime / setup status | sectioned | `M-x beads-context` | new (`beads-handoff.el`) |
| Swarm fleet list | tabulated | `M-x beads-swarm-list-view` / `beads-swarm l` | new (`beads-swarm.el`) |
| Swarm status board (+ worker lanes) | sectioned | `RET` in swarm list / `M-x beads-swarm-board` | new (`beads-swarm.el`) |
| Swarm validate / waves | sectioned | `v` in swarm list / `M-x beads-swarm-waves` | new (`beads-swarm.el`) |
| Swarm create + coordinator | transient | `beads-swarm c` / `M-x beads-swarm-create-flow` | new (`beads-swarm.el`), reusing `beads-command-swarm-create` |

## 4. Module map

### 4.1 New modules

| Module | Owns | Key symbols |
|---|---|---|
| `beads-molecule.el` | molecule execution view, work loop, step actions, close-eligible, ready frontier | `beads-molecule-mode`, `beads-molecule-open`, `beads-molecule-refresh`, `beads-molecule-claim`, `beads-molecule-close`, `beads-molecule-next`, `beads-molecule-toggle-ready`, `beads-molecule-step-state`, `beads-molecule-close-eligible` |
| `beads-gate.el` | gate list/detail, create/check/resolve/add-waiter/discover, molecule integration query | `beads-gate-list-mode`, `beads-gate-open`, `beads-gate-create`, `beads-gate-check`, `beads-gate-resolve`, `beads-gate-add-waiter`, `beads-gate-discover` |
| `beads-wisp.el` | wisp list, squash/burn/purge lifecycle, phase badges | `beads-wisp-list-mode`, `beads-wisp-squash`, `beads-wisp-burn`, `beads-wisp-purge`, `beads-wisp-kind` |
| `beads-cook.el` | cook preview flow (compile/runtime, dry-run, persist) | `beads-cook`, `beads-cook-preview`, `beads-cook-parse-tree` |
| `beads-bond.el` | bond flow and preview | `beads-bond`, `beads-bond-read-operands`, `beads-bond-preview` |
| `beads-formula-edit.el` | TOML create/edit, schema completion/validation, convert, distill | `beads-formula-new`, `beads-formula-edit-source`, `beads-formula-edit-validate`, `beads-formula-convert-at-point`, `beads-formula-distill`, `beads-formula--schema-completions` |
| `beads-handoff.el` | `bd prime` surface, `bd setup` status/policy, agent hand-off envelope | `beads-context`, `beads-context-prime`, `beads-setup-status`, `beads-handoff-agent` |
| `beads-swarm.el` | swarm fleet list, status board, validate/waves, worker lanes, swarm actions and navigation | `beads-swarm-list-mode`, `beads-swarm-list-view`, `beads-swarm-board-mode`, `beads-swarm-board`, `beads-swarm-waves-mode`, `beads-swarm-waves`, `beads-swarm-create-flow`, `beads-swarm-open`, `beads-swarm-refresh`, `beads-swarm-step`, `beads-swarm-assign`, `beads-swarm-claim`, `beads-swarm-handoff`, `beads-swarm-coordinator`, `beads-swarm-worker-lanes`, `beads-swarm-wave-table`, `beads-swarm-step-action` |
| `beads-command-cook.el` | `bd cook` command class (split from `beads-command-misc.el`) | `beads-command-cook`, `beads-cook` |
| `beads-command-prime.el` | `bd prime` command class (split from `beads-command-misc.el`) | `beads-command-prime`, `beads-prime` |
| `lisp/test/beads-standalone-guard-test.el` | no-gascity guard (REQ-SF-080) | `beads-standalone-guard-test` |

`beads-command-setup` and `beads-command-purge` already exist in
`beads-command-misc.el` and are **reused as-is** (they may be split out in
the same wave for convention, but that is cosmetic).

**Naming rule (no shadowing).** `beads-defcommand` already generates the
interactive commands `beads-swarm-list`, `beads-swarm-status`,
`beads-swarm-validate` and `beads-swarm-create` in
`beads-command-swarm.el` (the transient's raw suffixes). The porcelain
entry points therefore carry a distinct suffix (`-view` / `-board` /
`-waves` / `-flow`) and live in `beads-swarm.el`; the raw generated commands remain the
M-x fallback, and the `beads-swarm` transient keys are rebound to the porcelain
entry points. This mirrors how `beads-molecule-open` sits beside the generated
`beads-mol-*` suffixes.

### 4.2 Changed modules

| Module | Change |
|---|---|
| `beads-formula.el` | add phase/extends/bond-point rendering to `beads-formula-detail-sections`; make `beads-formula-launch-standalone` route through the pour/wisp choice; `beads-formula-follow` opens the molecule view; add `beads-formula-scope` shadowing data |
| `beads-command-swarm.el` | keep the `beads-swarm` transient (rewire `l`/`s`/`v` to the new view entry points, add `w` worker lanes and `C` coordinator); add `:result` declarations to the four classes and domain-error handling for `create`/`validate`. No slot change (the classes already have `epic-id`, `coordinator`, `force`, `swarm-id`). |
| `beads-command-mol.el` | parent `beads-mol` transient gains "Open view" / "Bond" / "Hand off"; no slot changes |
| `beads-command-gate.el` | unchanged classes; wire keys to new UI via autoloaded entry points in `beads-gate.el` |
| `beads-command-ready.el` | work-loop uses existing `:mol` and `:claim` slots; no change |
| `beads-types.el` | add slots to `beads-formula` (`phase`, `extends`, `aspects`, `expansions`, `bond-points`) and the per-step gate/`waits_for` fields, with the `beads-from-json` method reading them; **add swarm result types** `beads-swarm-list-item`, `beads-swarm-status`, `beads-swarm-status-issue`, `beads-swarm-analysis`, `beads-ready-front`, `beads-swarm-issue-node` (REQ-SF-011/052/099) |
| `beads-menu.el` | `beads-dispatch` / `beads-maintenance` gain Molecules / Gates / Wisps / Context entries |
| `beads.el` | autoloads for the new entry points |
| `beads.el` `Package-Requires`/`Eldev`/`guix.scm` | only if a new dependency (`toml`) is accepted; default plan reuses `json`/`conf`-mode and `toml-mode` if present (no new hard dep) |

### 4.3 Unchanged (do not touch)

`beads-command.el`, `beads-meta.el`, `beads-types.el`, `beads-error.el`,
`beads-buffer.el`, `beads-sling.el` (consumed), `beads-agent*.el`,
`beads-thing.el`, `beads-section.el`, `beads-remote.el`, `beads-command-*.el`
other than above.

## 5. Seams (magit/forge extension model)

Every seam is `beads-` public, documented, and optional to override. New seams
are listed with the PR #67 seam they extend.

### 5.1 Launch / instantiate (extends PR #67 §4.7)

| Symbol | Kind | Signature | File | Default | Downstream |
|---|---|---|---|---|---|
| `beads-formula-launch` | cl-defgeneric (exist) | `(formula bead &optional vars) → context` | `beads-formula.el` | `bd mol pour` via `beads-command-mol-pour` | gascity overrides for `gc sling --formula/--on` |
| `beads-formula-instantiate` | cl-defgeneric **new** | `(formula phase bead &optional vars) → result` | `beads-formula.el` | `phase` ∈ `pour`/`wisp`/`wisp-root-only`; dispatches to `bd mol pour` / `bd mol wisp` | gascity may override to a city-scoped instantiate |
| `beads-formula-phase` | defun **new** | `(formula) → "persistent"|"vapor"|nil` | `beads-formula.el` | reads formula `phase` if parsed | gascity uses for city policy hints |
| `beads-formula-follow` | cl-defgeneric (exist, extended) | `(result context) → context` | `beads-formula.el` | opens `beads-molecule-open` on the root id | gascity opens the `gc` run view |

The default `beads-formula-launch` is kept for shape compatibility with sling
and PR #67; the interactive instantiate flow calls `beads-formula-instantiate`
with an explicit phase. `beads-formula-launch` delegates to it with `pour` when
no phase is supplied.

### 5.2 Molecule rendering

| Symbol | Kind | Signature | File | Default | Downstream |
|---|---|---|---|---|---|
| `beads-molecule-step-state` | cl-defgeneric **new** | `(step &optional mol) → state` | `beads-molecule.el` | state from `bd mol current` letter plus `ready`/`gated` refinement | gascity may add `[agent]/[rig]` states |
| `beads-molecule-step-face` | defun **new** | `(state) → face` | `beads-molecule.el` | derives from `beads-face-status-*` (`:inherit`) | extensions derive |
| `beads-molecule-sections` | generic/hook **new** | `(mol) → section list` | `beads-molecule.el` | Header, Progress, Steps, Gates, Output | gascity adds a "City/rig" section |
| `beads-molecule-action` | cl-defgeneric **new** | `(action step) → result` | `beads-molecule.el` | `claim`/`close`/`inspect` | gascity may add `dispatch` |

### 5.3 Gates, wisps, bonding, authoring, hand-off

| Symbol | Kind | Signature | File | Notes |
|---|---|---|---|---|
| `beads-gate-display` | cl-defgeneric **new** | `(gate) → plist` | `beads-gate.el` | row/detail fields; gascity may add `rig` |
| `beads-gate-create-command` | defun **new** | `(type blocks &rest kwargs) → command` | `beads-gate.el` | builds the existing `beads-command-gate-create` |
| `beads-wisp-kind` | defun **new** | `(wisp) → `new`|`old`|`closed`` | `beads-wisp.el` | old = not updated in 24h |
| `beads-bond-operand-reader` | hook **new** | `(prompt) → id` | `beads-bond.el` | completion over formula/proto/molecule; gascity adds `gc` targets |
| `beads-formula-schema-validator` | defun **new** | `(toml-text) → diagnostics` | `beads-formula-edit.el` | uses `beads-command-formula-schema`; no new parser |
| `beads-handoff-envelope` | cl-defgeneric **new** | `(work system-prompt user-prompt) → start-args` | `beads-handoff.el` | bridges to `beads-agent-backend-start` (4-arity) |

### 5.4 Swarm seams

| Symbol | Kind | Signature | File | Default | Downstream |
|---|---|---|---|---|---|
| `beads-swarm-display` | cl-defgeneric **new** | `(swarm-item) → plist` | `beads-swarm.el` | row/detail fields from `bd swarm list/status`; no new parser | gascity may add `city`/`rig` and live `sessions` |
| `beads-swarm-worker-source` | cl-defgeneric **new** | `(assignee) → worker-plist` | `beads-swarm.el` | standalone `bd list --assignee <a> --status in_progress --json` | gascity overrides with live session/pool state; the default never requires gascity |
| `beads-swarm-step-action` | cl-defgeneric **new** | `(action step) → result` | `beads-swarm.el` | `claim` / `assign` / `handoff` / `close` / `reopen` | gascity may add `dispatch` |
| `beads-swarm-create-flow` | defun **new** | `(epic-id &optional coordinator force) → swarm-id` | `beads-swarm.el` | runs validate first, then builds the existing `beads-command-swarm-create`; surfaces the auto-wrap and already-exists domain results | gascity may override with a city-scoped create |
| `beads-swarm-coordinator` | defun **new** | `(swarm-id new-coordinator) → result` | `beads-swarm.el` | `beads-command-update <swarm-id> --assignee` (the coordinator *is* the swarm molecule's assignee) | gascity may suggest an address |
| `beads-swarm-domain-error-p` | defun **new** | `(parsed) → error-string\|nil` | `beads-swarm.el` | detects `{error: …}` payloads that `bd swarm create/validate` emit **with exit 0**; the single place that turns a domain payload into a user-facing error | reused by tests |

`bd swarm status` accepts an **epic or a swarm molecule**; the status board
resolves the id once through `beads-command-swarm-status` and never reimplements
the `relates-to` lookup. `bd swarm validate` requires an **epic** id; the list
and molecule views pass the epic id, not the swarm id.

### 5.5 Consumed PR #67 seams (unchanged)

`beads-formula-var-reader` (§4.7), `beads-formula-launch-context` (§4.7),
`beads-sling-target-functions` / `beads-sling-backend-register` /
`beads-sling-dispatch` (§4.5), `beads-command-execute-async` (§4.11 with
`:queue`/`:cache-key`), `beads-mode-extension-map` (`C-c b`, §4.10),
`beads-face-*` (§4.9), `beads-terminal-attach` (§4.8, for hand-off),
`beads-buffer-*` (§4.12, remote naming).

## 6. Reuse-vs-new matrix (authoritative)

| Capability | PR #67 artifact | This plan |
|---|---|---|
| Formula list/detail | `beads-command-formula.el` + `beads-formula.el` browser/detail | extend columns/rendering only |
| Typed vars | `beads-formula-var-reader` | reuse verbatim |
| Launch | `beads-formula-launch`, `beads-formula-launch-context` | add `beads-formula-instantiate` phase arg; keep generic |
| Sling | `beads-sling.el` | reuse for hand-off targeting |
| Movement | `beads-thing.el` | reuse (`beads-thing-define-keys`) |
| Faces | `beads-face-*` (`beads-faces.el`) | derive new state faces; no new palette |
| Async | `beads-command-execute-async` | reuse with per-molecule `:cache-key` |
| Issue detail | `beads-command-show.el` / `beads-section.el` | step `RET` opens it |
| Agent launch | `beads-agent.el`, `beads-agent-backend-start` 4-arity | add molecule/formula envelope in `beads-handoff.el` |
| Command classes | `mol`/`gate`/`cook`/`prime`/`formula` | reuse; add UI only |
| Molecule execution | — | **new** (`beads-molecule.el`) |
| Work loop | `beads-command-ready` (`:mol`,`:claim`), `beads-command-update`, `beads-command-close` | **new orchestration** |
| Gates UI | command classes only | **new** (`beads-gate.el`) |
| Wisps UI | `mol wisp` classes only | **new** (`beads-wisp.el`) |
| Bonding UX | `beads-command-mol-bond` only | **new** (`beads-bond.el`) |
| Authoring | `formula convert`, `formula schema`, `mol distill` classes | **new** editor/validator (`beads-formula-edit.el`) |
| Prime/setup | `beads-command-prime`, `beads-command-setup` (exist) | **new** context view |
| Swarm commands | `beads-command-swarm-{create,list,status,validate}` (exist), `beads-swarm` transient (exist) | extend transient suffixes; add `:result` types + `beads-swarm.el` porcelain |
| Swarm status board | `bd swarm status --json` (computed, no stored state) | **new** (`beads-swarm.el`) |
| Swarm validate/waves | `bd swarm validate --json` `ready_fronts`/`max_parallelism`/`estimated_sessions` | **new** renderer (`beads-swarm.el`) |
| Worker matching | `bd list --assignee --status in_progress` (issue porcelain) | **new** lane view + `beads-swarm-worker-source` seam |
| No-gascity guard | — | **new** test (extends to `beads-swarm.el`) |

## 7. Data and async model

### 7.1 Read model

All reads go through `beads-command-execute-async` with a per-view cache key
and the global concurrency cap.

| View | Commands (async) | Cache key | Refresh trigger |
|---|---|---|---|
| Formula browser | `beads-command-formula-list` | `(formula-list scope type)` | `g`; scope/type change |
| Formula detail | `beads-command-formula-show` | `(formula-show name)` | on open; `g` |
| Molecule view | `beads-command-mol-current`, `-progress`, `-show`, and `beads-command-ready :mol` when ready-only | `(molecule root)` | on open; after claim/close; `g`; `bd ready --gated` tick |
| Gate list | `beads-command-gate-list` | `(gate-list all type)` | `g`; after create/check/resolve |
| Gate detail | `beads-command-gate-show` | `(gate-show id)` | on open; `g` |
| Wisp list | `beads-command-mol-wisp-list` | `(wisp-list all type)` | `g` |
| Context | `beads-command-prime` | `(prime flags)` | on open; `g` |
| Swarm fleet list | `beads-command-swarm-list` | `(swarm-list all type)` | on open; `g`; after create/coordinator change |
| Swarm status board | `beads-command-swarm-status` | `(swarm-status id)` | on open; `g`; after claim/assign/close |
| Swarm worker lanes | `beads-command-list :assignee :status in_progress` per active assignee (or one union call when the backend supports it) | `(swarm-workers id)` | with the status board; `g` |
| Swarm validate | `beads-command-swarm-validate` (`:verbose` toggle) | `(swarm-validate epic verbose)` | on open; `g`; `V` toggle verbose |

The molecule view is the one multi-command aggregator: `current` drives the
step tree, `progress` the header, `show` the phase/vars, and ready-only adds
`ready --mol`. Each lands in its own `vui-error-boundary`-equivalent section
(a section failure renders an error line, the rest render) — the same contract
as the dashboard.

### 7.2 Write model (synchronous, previewed)

| Action | Command class | Preview rule |
|---|---|---|
| Cook | `beads-command-cook` | `--dry-run` first unless `--persist` was explicitly confirmed; preview buffer |
| Instantiate | `beads-command-mol-pour` / `-mol-wisp` | `--dry-run` preview, then apply; result opens molecule view |
| Claim | `beads-command-update` `--claim` (or `ready --claim`) | no preview (atomic, idempotent-ish) |
| Close | `beads-command-close` (`--reason` required) | no preview; refresh + advance |
| Close-eligible | `beads-command-epic-close-eligible` | `--dry-run` preview, then apply |
| Gate create/check/resolve | `beads-command-gate-*` | check/create support `--dry-run`; resolve requires reason |
| Wisp squash/burn/purge | `beads-command-mol-squash` / `-burn` / `beads-command-purge` (exist) | dry-run/confirm for burn/purge |
| Bond | `beads-command-mol-bond` | `--dry-run` preview |
| Distill | `beads-command-mol-distill` | `--dry-run` preview |
| Convert | `beads-command-formula-convert` | preview file path; `--stdout` variant |
| Swarm create (`--force`) | `beads-command-swarm-create` | validate first (`swarm validate`); on `swarmable=false` show errors, do not create; on `already exists` show the existing swarm and require explicit `--force` confirmation; auto-wrap of a non-epic is reported |
| Swarm assign / claim | `beads-command-assign`, `beads-command-update --claim` | claim is atomic (no preview); assign shows the target |
| Swarm coordinator | `beads-command-update --assignee` on the swarm molecule | preview old→new; confirm |
| Swarm step close/reopen | `beads-command-close`, `beads-command-update --status open` | close requires reason; reopen is explicit |
| Merge slot | `beads-command-merge-slot-{check,acquire,release}` (exist) | check first; acquire confirmed |

Destructive actions (burn, purge, `--delete`, `--force`) always require an
explicit confirmation buffer/`y-or-n-p`, never a bare key.

### 7.3 Concurrency, remote and errors

- Async fan-out respects `beads-command-async-max-concurrent`; molecule view
  uses `:cache-key (molecule root)` so repeated opens coalesce.
- Remote/TRAMP: every command gets `--directory` from `beads-store-directory`
  via `beads-meta-build-global-options`; buffer names use `beads-buffer-*`;
  no new file I/O on a remote store (render-guard test remains green).
- Errors: reuse `beads-validation-error`, `beads-command-error`,
  `beads-json-parse-error`; each section/action reports concisely and keeps
  the buffer usable.

## 8. State and command assembly

### 8.1 Instantiate flow state

```
beads-formula-instantiate-state (buffer-local plist)
  :formula   name (string)
  :recipe    beads-formula object
  :phase     pour | wisp | wisp-root-only
  :vars      (NAME . VALUE) alist        ; normalized by beads-formula--normalize-vars
  :assignee  string|nil                  ; bd mol pour/wisp --assignee
  :warnings  list
```

The flow is a transient (`beads-formula-instantiate--transient`) seeded with
`:formula`/`:recipe` (same scope-plist pattern as
`beads-formula-sling-scope`). Validation on every infix change recomputes
warnings; `s` blocks when warnings contain missing-required/pattern/enum/numeric
errors.

### 8.2 Molecule view state

```
beads-molecule--root      string
beads-molecule--data      hash: current/progress/show/ready results
beads-molecule--ready-only boolean
beads-molecule--expanded  hash: section -> bool
beads-molecule--steps     list of step plists (id,title,state,needs,gate,assignee)
beads-molecule--range     nil | (start . end)   ; large molecules
```

Step plists come from `bd mol current`; `ready` refines state to `[ready]`;
a step with a gate id is `[blocked-gate]`. The step row carries a
`beads-thing` property so movement is identical to other views.

### 8.3 Work-loop transition table

| State | Key | Action | Next |
|---|---|---|---|
| any | `n` | select next `[ready]` step | mark current, scroll to it |
| selected ready | `c` | `update --claim` | refresh → `[current]` |
| selected current | `x` | prompt reason, `close` | refresh → `[done]`; select next ready |
| all done | `C` | `epic close-eligible --dry-run`, confirm | close root, refresh |
| gated | `RET` on gate glyph | open gate detail | — |
| large mol | `]`/`[` | `current --range` window | render window |

### 8.4 Swarm view state

```
beads-swarm--id          string            ; epic or swarm molecule id
beads-swarm--epic-id     string            ; resolved epic (validate needs it)
beads-swarm--swarm-id    string|nil        ; linked swarm molecule, if any
beads-swarm--data        hash: list/status/validate/workers results
beads-swarm--all         boolean           ; list: all vs active
beads-swarm--type-filter string|nil
beads-swarm--verbose     boolean           ; validate --verbose (issue graph)
beads-swarm--collapsed   hash: section -> bool
beads-swarm--waves       list of beads-ready-front
beads-swarm--lanes       alist: assignee -> (active . beads)
```

The status board reads the `beads-command-swarm-status` result once and derives the four groups
plus `progress_percent`, `active_count`, `ready_count`, `blocked_count`
directly from the payload — no client-side recomputation. The worker lanes are
the only derived layer: for each distinct `active` assignee it asks
`beads-swarm-worker-source` for that worker's in-progress beads; a lane with no
`[current]` step but in-progress beads is flagged `idle-slot` (assigned but not
on a swarm step), a lane whose worker holds more than one `[current]` step is
`over-committed`. Headroom is `max_parallelism - active_count` from the
*validate* payload (the status payload has no max-parallelism field, so the
board requests validate in parallel when the headroom row is shown), clipped at
zero with a `saturated` note.

### 8.5 Swarm domain-error table (`bd` exits 0, payload says otherwise)

| Command | Payload | UI state |
|---|---|---|
| `swarm create` | `{error: "swarm already exists", existing_id, existing_title}` | warning panel; offer `RET` to open the existing swarm; only `--force` creates another |
| `swarm create` | `{error: "epic is not swarmable", analysis}` | render the embedded analysis as the validate board; no swarm created |
| `swarm validate` | `swarmable: false` + `errors` | validated-with-errors state (domain), never a command error |
| `swarm validate` | `swarmable: true` + `warnings` | validated-with-warnings state |
| `swarm status` | `error` string from a real command failure (non-zero) | normal command-error state (`g` retry) |

## 9. Keymap and navigation contract

New modes install `beads-thing-define-keys` for `TAB`/`S-TAB`/`SPC` and
`beads-mode--install-extension-map` for `C-c b`. Core keys per surface:

- **Formula browser/detail (extended):** `RET` inspect, `l` seed sling,
  `s` instantiate (was launch), `c` cook, `C` convert, `e` edit source,
  `S` schema, `g` refresh, `q` bury. (`s` keeps the PR #67 meaning "launch
  standalone" and now opens the pour/wisp choice; `l` still seeds sling.)
- **Molecule view:** `RET` inspect step, `n` next ready, `c` claim, `x` close,
  `C` close-eligible root, `r` toggle ready frontier, `p` refresh progress,
  `b` bond, `a` hand off, `G` gates, `]`/`[` range, `g` refresh, `q` bury.
- **Gate list/detail:** `RET` detail, `c` create, `C` check, `R` resolve,
  `w` add-waiter, `d` discover, `a` toggle all, `t` type, `g`, `q`.
- **Wisp list:** `RET` open root molecule, `s` squash, `b` burn, `P` purge,
  `a` toggle all, `t` type, `g`, `q`.
- **Bond transient:** `A`/`B` operands, `t` type, `p` phase, `r` ref, `v` var,
  `a` as, `P` preview, `s` bond.
- **Authoring:** `M-x beads-formula-new`, `C-c C-c` validate+save,
  `C-c C-v` validate, `C-c C-x` convert.
- **Hand-off:** `a` in molecule/formula opens the agent prompt with the
  molecule/formula envelope; `M-x beads-context` shows prime/setup.
- **Swarm fleet list:** `RET` status board, `v` validate, `c` create, `w`
  worker lanes, `x` jump to swarm molecule, `E` jump to epic, `f` filter,
  `a` all/active, `g`, `q`.
- **Swarm status board:** `RET` issue detail, `A` assign, `c` claim, `h` hand
  off, `x` close step, `o` reopen step, `C` close-eligible root, `W` worker
  lanes, `V` validate, `w` jump to the worker's in-progress beads, `g`, `q`.
- **Swarm validate/waves:** `RET` on a wave row lists the wave's beads, `RET`
  on a warning jumps to the offending bead, `V` toggle `--verbose` (issue
  graph), `c` create swarm (when `swarmable`), `g`, `q`.
- **Swarm create transient:** `c` coordinator, `f` force, `P` preview
  (`swarm validate` first), `s` create.
- **Swarm coordinator:** `M-x beads-swarm-set-coordinator` (or `C` from the
  status board) prompts old→new assignee and confirms.

Reserved `C-c b`: beads owns it; extensions register subkeys. No new core key
shadows a PR #67 binding; any conflict is called out in `plan-review.md`.

## 10. Glyphs, faces and rendering rules

- **Glyphs:** `✓ [done]`, `◐ [current]`, `○ [ready]`, `⊘ [blocked]`,
  `· [pending]`, `⊘g [blocked-gate]`, `▲ [failed]`. Persistent root `◆`,
  vapor root `◇`. Gate types: `⚑ human`, `⏱ timer`, `⚙ gh:run`, `⇄ gh:pr`,
  `◈ bead`.
- **Faces:** reuse the PR #67 palette (`beads-face-status-*`,
  `beads-face-success/warning/error`, `beads-face-id/key/section/header`);
  add `beads-face-molecule-root`, `beads-face-molecule-step`,
  `beads-face-swarm-coordinator`, `beads-face-swarm-lane`, derive with
  `:inherit`. No new palette, no face hook.
- **Swarm glyphs:** swarm molecule `🐝`/`S`, coordinator `★`, wave `W<n>`,
  saturated lane `▰`, idle slot `▱`, over-committed `▲`, orphan/cycle warning
  `⚠`. The four status groups reuse the molecule state glyphs
  (`✓/◐/○/⊘`).
- **Rendering rules:** sections are foldable `beads-section` sections; rows are
  `beads-thing`s; four-state (loading/empty/populated/error) is mandatory per
  section; large lists paginate with `beads-pager` (molecule uses
  `--limit`/`--range` instead).

## 11. Remote / TRAMP

Every new view is store-scoped: it reads `beads-store-directory` and passes
`:directory` so `beads-meta-build-global-options` adds `--directory` to every
command. Buffers are named with `beads-buffer-*` (host-qualified on remote).
The render guard (`lisp/test/beads-render-guard-test.el`) must stay green:
opening/rendering/folding does no remote file I/O. Formula authoring is the
one file-touching surface; it localizes paths through `beads-remote` helpers
and is the only place that may open a remote file.

## 12. Risks and mitigations

| Risk | Mitigation |
|---|---|
| Duplicating the PR #67 formula launch path | `beads-formula-launch` extended, `beads-formula-instantiate` is the new phase-aware entry and the generic delegates; reuse matrix is authoritative |
| `bd mol current` output shape varies by version | parse defensively via the existing `beads-command-mol-current` result type; if absent, fall back to `dep tree` + status; covered by a fixture test |
| Molecule view doing N synchronous calls | async aggregator with cache key and per-section boundaries |
| Destructive wisp/burn actions | dry-run + explicit confirm; never bare-key destructive |
| New hard dependency (TOML) | default uses `toml-mode` when present and `conf-mode` fallback; validation uses `bd formula schema`, not a bundled parser; no `Package-Requires` change unless accepted |
| Key conflicts with PR #67 | per-surface tables above; conflicts flagged and resolved in `plan-review.md` before implementation |
| gascity creeping into a default path | REQ-SF-080 guard test loads modules without gascity and scans for gascity symbols; extended to `beads-swarm.el` and `beads-swarm-worker-source` (REQ-SF-097) |
| Swarm domain errors hidden by exit 0 | `beads-swarm-domain-error-p` is the single detector; `create`/`validate` are tested against the `already exists` / `not swarmable` / `swarmable=false` fixtures |
| Status board asks N per-assignee `bd list` calls | one request per distinct active assignee, coalesced by `(swarm-workers id)`; lanes render progressively and the headroom row alone pulls validate |
| `bd swarm status` is computed, so a stale board lies | async refresh after every swarm write; board is recomputed from beads, never cached across writes |

## 13. Verification plan (design-level)

- Unit (`:unit`, mocked `beads-command-execute`): instantiate state machine and
  validation; work-loop transition table; gate create/check/resolve command
  assembly; wisp kind classification; bond command assembly; distill var
  mapping; prime/setup formatting.
- Integration (`:integration`, real `bd` temp repo): cook→pour→work→close;
  cook→wisp→squash; gate create→resolve→ready; bond two formulas; distill an
  epic; setup `--check` status.
- Swarm (`:unit`, mocked): fleet-list row mapping; status-board group
  partitioning and progress; worker-lane idle/over-committed/headroom math;
  wave-table rendering; domain-error detection for all `create`/`validate`
  payloads; create `--force` preview; coordinator update command assembly;
  navigation targets (epic ↔ swarm ↔ molecule ↔ worker).
- Swarm (`:integration`, real `bd` temp repo): epic + DAG → `swarm validate`
  (waves, max parallelism) → `swarm create` → `swarm status` groups →
  claim/assign/close a step → `swarm status` recomputes; `swarm create` twice
  (already-exists) → `--force`; non-swarmable epic → domain state.
- Guard: `beads-standalone-guard-test.el` (no gascity), including the swarm
  module and the `beads-swarm-worker-source` default.
- Remote: open each new view over the standing TRAMP test store; render guard.

## 14. References

- `plans/beads-ui-redesign/{requirements,design,menu-mockups,implementation-plan}.md`
  (PR #67) — the extended plan.
- `~/workspace/beads/docs/workflows/{formulas,molecules,wisps,gates}.md`,
  `docs/getting-started/ide-setup.md`,
  `docs/cli-reference/{formula,cook,mol,gate,prime,setup,purge}.md`.
- `docs/defcommand-redesign.md`, `MAGIT_PATTERNS.md`, `AGENTS.md`.
