---
schema: beads.standalone-formulas.plan-review.v1
artifact: plan-review
status: draft-for-review
scope: planning-only
implementation: out-of-scope-until-signoff
reviews: requirements.md design.md menu-mockups.md decomposition.md implementation-plan.md
---

# beads.el Standalone-first Formula → Molecule Workflow — Plan Review

A recorded critique round on the planning package. The review is adversarial:
it looks for duplication of PR #67, unstated `bd` semantics, gaps in the eight
required areas, impossible or under-specified UI states, and places where the
plan asserts a field or command the code does not have. Findings that changed
the artifacts are marked **fixed**; anything left open is an explicit accepted
follow-up, not a hidden TBD.

## Method

1. Re-read the bead `be-1qta`, the `bd` docs it cites, and the PR #67 plan.
2. Cross-check every planned symbol against the live code on
   `wi18-integration` (command classes, formula types, transients, menus).
3. Walk each of the nine gaps (the original eight plus swarm) and ask: does a
   `REQ-SF-*` cover it, does a
   module own it, does a mockup render it, and does a WI build it?
4. Probe edge cases per surface (empty, error, large, remote, destructive,
   gascity absent).

## Findings

### F1 — Duplicate DAG rendering between the molecule view and `bd mol show` — medium — **fixed**

`beads-command-mol-show` already returns structure/variables, and the plan's
molecule view also builds a step tree from `bd mol current`. Left ambiguous,
this invites two renderers.

**Disposition.** `design.md` §7.1 fixes ownership: `bd mol current` is the
*single* step-tree source (state lives there); `bd mol show` contributes only
the phase/vars summary to the molecule header; `beads-formula-launch`'s detail
path is untouched. `beads-molecule-sections` is the one renderer.

### F2 — Two launch generics (`beads-formula-launch` and `beads-formula-instantiate`) — medium — **fixed by clarification**

Having both risks an ABI fork from PR #67 (sling and gascity override
`beads-formula-launch`).

**Disposition.** `design.md` §5.1 states the contract: `beads-formula-launch`
remains the public shape-compatible generic and delegates to
`beads-formula-instantiate` with `pour` when no phase is given. The instantiate
transient is the only caller that passes an explicit phase. The reuse matrix
makes this authoritative.

### F3 — `beads-formula` type lacks `phase`, `extends`, bond points — high — **fixed**

REQ-SF-011/REQ-SF-052 ask the detail to render fields that the live
`beads-formula` EIEIO class does not carry (it has only name/description/
version/type/vars/steps/source).

**Disposition.** WI-SF-05 now explicitly extends `beads-types.el` with `phase`,
`extends`, `aspects`, `expansions`, `bond-points` and per-step gate/
`waits_for`, plus the `beads-from-json` method. `design.md` §4.2 lists
`beads-types.el` as changed. This was the largest concrete correction.

### F4 — `--for` does not exist on `bd mol pour`/`wisp` — high — **fixed**

The instantiate flow's buffer-local state carried `:for`, but `--for` is a
`bd mol current` query flag, not an instantiate flag. (Instantiate assigns with
`--assignee`.)

**Disposition.** `design.md` §8.1 removes `:for` from the instantiate state;
`--for` is now only a molecule-view query scope (REQ-SF-015, mockup §5g).
Instantiate keeps `--assignee`.

### F5 — `bd purge`/`bd setup` dependency assumed missing; they exist — medium — **fixed**

The first draft treated `purge` and `setup` as new command classes. Both already
exist in `beads-command-misc.el` (`beads-command-purge`,
`beads-command-setup`).

**Disposition.** `design.md` §4.1 and the reuse matrix now mark them reused
as-is; splitting them into their own files is cosmetic and part of the same
pruning wave as `cook`/`prime`. `decomposition.md` WI-SF-07 references the
existing class.

### F6 — Under-specified destructive confirmation — medium — **fixed**

`burn`, `purge`, `--delete` and `--force` could be reached from a single key.

**Disposition.** `design.md` §7.2 mandates dry-run + explicit confirmation
(typed id for batch burn, `--force` gate for purge); mockups §8c/§8d render
the confirm state. No bare-key destructive path exists anywhere in the
mockups.

### F7 — Large-molecule rendering unspecified — low — **fixed**

`bd mol current` has `--limit`/`--range` and a summary threshold, which the
first draft ignored.

**Disposition.** REQ-SF-021, `design.md` §8.2, and mockup §5f define the
windowed state (`]`/`[`, `--range`), with the view reporting the visible range.

### F8 — Formula shadowing detection assumes data `bd` may not expose — medium — **accepted follow-up**

`bd formula list --json` exposes `source` but not an explicit shadow flag.
Shadowing must be inferred by grouping names and comparing search-path rank.

**Disposition.** WI-SF-05 owns the inference and a fixture test; if a future
`bd` exposes first-class provenance, the inference is replaced. Recorded here,
not treated as a TBD. If sources are ever absent, the browser degrades to "no
provenance" rather than failing (mockup §1d covers the empty case).

### F9 — `bd setup --check` output is human text, not JSON — medium — **accepted follow-up**

`setup` recipe status cannot be parsed structurally today.

**Disposition.** WI-SF-11 v1 renders `bd setup --list` and per-recipe
`--check` in a compilation-style results buffer and surfaces the pass/fail
line; it does not attempt rich parsing. If `bd setup --json` later exists, the
view upgrades. The planned `beads-command-setup` reuse is text-mode only.

### F10 — No-gascity guard could false-pass on string scanning alone — medium — **fixed by design**

A symbol-name scan is a heuristic; a default path could still call gascity via
`funcall` on a variable.

**Disposition.** `design.md` §12 and WI-SF-13 specify a two-part guard:
(a) load the module with gascity removed from the load path and drive each
entry with mocked `beads-command-execute`, and (b) scan default-path forms for
gascity symbols/packages. Both must pass. The guard is complement to, not a
replacement for, the TRAMP/manual acceptance run.

### F11 — Key `s` meaning overlap (launch standalone vs seed sling) — low — **verified consistent**

In formula modes `s` = instantiate (formerly "launch standalone") and `l` =
seed sling. The semantic is preserved; only pour-vs-wisp is added. No PR #67
binding is shadowed. `design.md` §9 records this explicitly.

### F12 — Molecule view's "work loop" could be read as auto-execution — low — **fixed by clarification**

The bead asks for an affordance a regular agent/human can run; an auto-runner
would be out of scope and unsafe.

**Disposition.** REQ-SF-023 and mockup §6 state each step is explicit
(`n`→`c`→work→`x`); the loop never auto-closes and never claims more than one
step. Hand-off (`a`) is the path for delegating the work.

### F13 — Artifact-set completeness against the bead — low — **fixed**

The bead names `requirements.md`, `design.md`, `menu-mockups.md`,
`decomposition.md`, `implementation-plan.md`, `plan-review.md`.

**Disposition.** All six are present under `plans/beads-standalone-formulas/`;
`requirements.md` §Out Of Scope confirms no `.el` change. Verified below.

### F14 — TRAMP parity for file-touching authoring — medium — **fixed by scoping**

Authoring is the only surface that opens a file, which risks wrong-side I/O on
a remote store.

**Disposition.** `design.md` §11 scopes authoring as the single file-touching
surface, localizes via `beads-remote` helpers, and keeps the render guard
green for all other views. WI-SF-13 verifies over the standing TRAMP store.

### F15 — Dropped-keys fidelity warning was out of scope and mischaracterized `on_complete` — medium — **fixed**

The first draft added a §2c "Dropped-keys warning (TOML fidelity)" panel that
diffed the raw TOML against the decoded model and advised
`bd formula show --json` to surface keys `bd` silently drops, and REQ-SF-011
required the same. It also used `on_complete` as the canonical dropped key.
Both were wrong: `on_complete` is a **known, parsed field** (`Step.OnComplete`)
whose runtime execution is not yet wired, not an unknown key; and a
raw-TOML-vs-decoded-model diff in Elisp would duplicate `bd`'s formula schema
and drift from it.

**Disposition (user decision, 2026-10-07).** Do not build a client-side
workaround. `bd` is silent about unknown keys, it is a minor authoring-fidelity
gap, and no upstream issue is filed. `menu-mockups.md` §2c is removed (the
remaining subsections renumber; only warnings `bd` itself emits render);
REQ-SF-011 drops the fidelity clause; REQ-SF-061 and mockup §10c no longer
claim unknown-key detection and the `on_complete` diagnostic is dropped;
`design.md` §2, `implementation-plan.md` WI-SF-05 and `decomposition.md`
WI-SF-05 state the omission as out of scope / deferred to an upstream `bd`
change. No `.el` source is touched.

## Round: swarm addition (gap #9), 2026-10-07

Follow-up review (`be-v31j`) extends the package with full standalone swarm
support. The same adversarial method was applied against
`~/workspace/beads/docs/cli-reference/swarm.md`,
`docs/multi-agent/coordination.md`, `docs/workflows/{molecules,gates}.md` and
the live `cmd/bd/swarm.go` JSON shapes.

### S1 — `bd swarm create`/`validate` emit `{error: …}` with exit 0 — high — **fixed**

`create` returns `{"error":"swarm already exists", "existing_id",
"existing_title"}` and `{"error":"epic is not swarmable", "analysis"}` through
`outputJSON` + `SilentExit` (exit 0); `validate` returns `swarmable=false` +
`errors` (exit 0). A UI keyed on process exit would treat these as success.

**Disposition.** `beads-swarm-domain-error-p` (`design.md` §5.4) is the single
detector; `design.md` §8.5 is the domain-error table; mockup §14i/§14g render
the states; WI-SF-15 tests the fixtures. `already exists` offers the existing
swarm and gates `--force`; `not swarmable` renders the embedded analysis and
disables create.

### S2 — Swarm command classes parse raw strings — high — **fixed**

The four `beads-command-swarm-*` classes in `lisp/beads-command-swarm.el` have
no `:result`, so `beads-command-execute` returns raw JSON text. The status
board and wave view would need a second parser.

**Disposition.** WI-SF-15 adds the six result classes to `beads-types.el` and
`:result` declarations to the classes; no UI code parses JSON. `beads-swarm.el`
consumes typed objects. This is the only change to `beads-command-swarm.el`.

### S3 — Status payload has no max-parallelism — medium — **fixed by design**

`bd swarm status` reports counts and `progress_percent` but not
`max_parallelism`; only `bd swarm validate` does. The headroom row therefore
cannot be derived from status alone.

**Disposition.** `design.md` §8.4: the status board derives its groups only
from the status payload; the headroom row requests validate in parallel and is
labelled `saturated` at zero. No client-side DAG recomputation.

### S4 — “Coordinator” is the swarm molecule's assignee — medium — **fixed**

`--coordinator` is stored as `swarm.Assignee` and shown as `coordinator` in
`swarm list`; there is no separate field. “Set coordinator” is therefore an
assignee update, not a swarm-specific verb.

**Disposition.** REQ-SF-095 and `design.md` §5.4/§9 define
`beads-swarm-coordinator` as `beads-command-update --assignee` on the swarm
molecule, confirmed old→new; mockup §14k renders it.

### S5 — `validate` takes an epic; `status` takes epic *or* swarm — medium — **fixed**

`bd swarm validate` rejects a swarm molecule (it requires an epic/molecule),
while `bd swarm status` follows a swarm's `relates-to` link. Passing the wrong
id would fail.

**Disposition.** `design.md` §5.4 states it explicitly: the list/molecule/board
pass the resolved **epic** id to validate; status resolves once through
`beads-command-swarm-status`. The navigation graph §14l makes the distinction
visible (`E` = epic, `x` = swarm).

### S6 — Worker lanes need an assignee→in-progress mapping the swarm payload lacks — medium — **fixed**

`bd swarm status` carries only the `active` rows' assignee. Idle/saturated
lanes need each worker's other in-progress beads.

**Disposition.** `beads-swarm-worker-source` (`design.md` §5.4) with the
standalone default `bd list --assignee <a> --status in_progress --json`;
gascity overrides with live sessions. This keeps standalone a hard requirement
(REQ-SF-097) while leaving the enrichment seam open.

### S7 — Swarm is buried in Maintenance — medium — **fixed**

Today `beads-swarm` is reachable only from `!` → `w Swarm`, contradicting the
plan's “primary dispatch” rule for first-class work.

**Disposition.** REQ-SF-090/REQ-SF-091 promote the swarm list to the primary
dispatch and add entries from the epic/issue and molecule views; the existing
`beads-swarm` transient stays as the dispatch backend and gains `W` worker
lanes. WI-SF-12 wires it; mockup §14l shows every entry edge.

### S8 — Avoid a parallel “swarm detail” view — medium — **fixed**

A swarm status board could tempt a bespoke detail buffer, duplicating issue and
molecule rendering.

**Disposition.** `menu-mockups.md` §14l: every navigation edge lands on an
existing view — a step is an issue (`beads-command-show`), the swarm molecule is
the molecule view (§5), the epic is the issue view, the worker is the list view
scoped `--assignee --status in_progress`. The only new buffers are the fleet
list, status board and validate board.

### S9 — Non-epic auto-wrap must be reported — low — **fixed**

`bd swarm create <single-issue>` silently creates a wrapper epic. Hiding that
would surprise the operator.

**Disposition.** Mockup §14i renders the note (`created epic be-wrap-1 …`);
REQ-SF-090 and WI-SF-18 require the report before the follow-to-swarm step.

### S10 — Big-epic accuracy and mapping keys — low — **fixed**

A 400-step epic must not report window-local progress, and the swarm list must
show overall `progress_percent`.

**Disposition.** Mockup §14d keeps the header on the full
`progress_percent`/counts while groups page with `]`/`[`; mockup §14a maps
`total_issues`/`completed_issues`/`active_issues` explicitly.

## Gaps-walk verdict (bead's eight areas, plus swarm)

| Bead gap | REQ | Module | Mockup | WI |
|---|---|---|---|---|
| Complete formula lifecycle | REQ-SF-010..016 | `beads-cook.el`, `beads-formula.el` | §1–§4 | WI-SF-03,04,05 |
| Molecule execution UX | REQ-SF-020..025 | `beads-molecule.el` | §5–§6 | WI-SF-01,02 |
| Gates UI | REQ-SF-030..033 | `beads-gate.el` | §7 | WI-SF-06 |
| Wisp lifecycle UI | REQ-SF-040..042 | `beads-wisp.el` | §8 | WI-SF-07 |
| Bonding/composition | REQ-SF-050..052 | `beads-bond.el` | §9 | WI-SF-08 |
| Formula authoring | REQ-SF-060..063 | `beads-formula-edit.el` | §10 | WI-SF-09,10 |
| Standalone hand-off | REQ-SF-070..072 | `beads-handoff.el` | §11 | WI-SF-11 |
| No-gascity guarantee | REQ-SF-080..083 | guard test + reuse matrix | — | WI-SF-13 |
| **Swarm / coordination (gap #9)** | REQ-SF-090..099 | `beads-swarm.el` + `beads-command-swarm.el` `:result` | §14 | WI-SF-15,16,17,18 |

All nine are addressed; no gap is left to a TBD.

## Residual risks / accepted follow-ups

1. **Formula provenance inference** (F8) depends on `source` being present; a
   fixture test pins it and the view degrades gracefully.
2. **Setup text-mode parsing** (F9) is intentionally shallow; upgrade when
   `bd` exposes structured output.
3. **`bd mol current` shape drift** across `bd` versions: fixture pinned to the
   CI `bd` (v1.0.5 per AGENTS.md); defensive parse in WI-SF-01.
4. **`toml-mode` may be absent**: fallback `conf-mode`; no new hard dependency
   is introduced by the plan.
5. **Unknown-key fidelity is not surfaced** (F15): accepted. `bd` silently
   drops unknown formula TOML keys and `on_complete` is parsed but its runtime
   execution is unwired. Revisit only if `bd` emits its own warning; no
   upstream issue is filed by this plan.
6. **Swarm domain-error payloads** (S1) are stable in `bd` v1.3.x; a fixture
   test pins them and `beads-swarm-domain-error-p` degrades to “treat as
   success” only if the payload shape disappears, which would surface as a
   visible empty result, not a silent write.
7. **Worker-lane enrichment** (S6) is gascity-only; the standalone default is
   `bd list`, verified by the no-gascity guard.

None blocks implementation. None is a TBD in the artifacts.

## Verdict

The plan is **implementation-ready for sign-off**:

- Every bead gap has concrete requirements, a module, a stateful mockup and a
  work item.
- The reuse-vs-new boundary with PR #67 is explicit and complete; the plan
  extends `beads-formula-launch` and `beads-formula-var-reader` rather than
  forking them.
- The four high/medium corrections (F3 type slots, F4 `--for`, F5 existing
  classes, F6 destructive confirm) are folded into the artifacts.
- The standalone guarantee has both a guard test and a manual acceptance run.
- No `.el` source is modified by this planning task.

Recommended disposition from the critique: **approve to implement**, in the
six-wave, pruning-first order of `implementation-plan.md`, with WI-SF-13 as the
acceptance gate.

## Sign-off checklist

- [x] Six required planning artifacts present, plus the plan `README.md`
      overview.
- [x] All `REQ-SF-*` (44, across nine gaps) trace to WIs; all WIs trace to
      requirements.
- [x] Every surface has loading/empty/populated/error (and folded/windowed/
      confirm where relevant) — the swarm surfaces included (§14n).
- [x] Reuse-vs-new matrix explicit; swarm reuses the existing
      `beads-command-swarm.el` classes and `beads-swarm` transient.
- [x] No `.el` change in this task.
- [x] Critique round recorded with dispositions (F1–F15 and S1–S10).
