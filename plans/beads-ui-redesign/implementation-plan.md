---
schema: gc.build.plan.v1
workflow:
  id: be-1iu7
  formula: build-from-plan
methodology:
  pack: gascity
  name: build-from-plan
producer:
  formula: build-from-plan
  stage: plan
  attempt: 1
status: approved
trace:
  upstream:
    - path: plans/beads-ui-redesign/requirements.md
      hash: sha256:50fd6788be4420ae0fcf2574736bca6059409fa38c24f706c0726af20edcaac9
      ids:
        - REQ-001
        - REQ-002
        - REQ-003
        - REQ-004
        - REQ-005
        - REQ-006
        - REQ-007
        - REQ-008
        - REQ-009
        - REQ-010
        - REQ-011
        - REQ-012
        - REQ-013
        - REQ-014
        - REQ-015
        - REQ-016
        - REQ-017
        - REQ-018
        - REQ-019
        - REQ-020
        - REQ-021
        - REQ-022
        - REQ-023
        - REQ-024
        - REQ-025
        - REQ-026
        - REQ-027
        - REQ-028
        - REQ-029
        - REQ-030
        - REQ-031
        - REQ-032
        - REQ-033
  coverage:
    - id: REQ-001
      status: covered
    - id: REQ-002
      status: covered
    - id: REQ-003
      status: covered
    - id: REQ-004
      status: covered
    - id: REQ-005
      status: covered
    - id: REQ-006
      status: covered
    - id: REQ-007
      status: covered
    - id: REQ-008
      status: covered
    - id: REQ-009
      status: covered
    - id: REQ-010
      status: covered
    - id: REQ-011
      status: covered
    - id: REQ-012
      status: covered
    - id: REQ-013
      status: covered
    - id: REQ-014
      status: covered
    - id: REQ-015
      status: covered
    - id: REQ-016
      status: covered
    - id: REQ-017
      status: covered
    - id: REQ-018
      status: covered
    - id: REQ-019
      status: covered
    - id: REQ-020
      status: covered
    - id: REQ-021
      status: covered
    - id: REQ-022
      status: covered
    - id: REQ-023
      status: covered
    - id: REQ-024
      status: covered
    - id: REQ-025
      status: covered
    - id: REQ-026
      status: covered
    - id: REQ-027
      status: covered
    - id: REQ-028
      status: covered
    - id: REQ-029
      status: covered
    - id: REQ-030
      status: covered
    - id: REQ-031
      status: covered
    - id: REQ-032
      status: covered
    - id: REQ-033
      status: covered
---

# beads.el UI Redesign — Implementation Plan

Ordered plan for the later, separately-approved implementation. The plan is
**pruning-first** (REQ-031): removals land before new surfaces, so the new
porcelain is built on a smaller surface, never in front of the debris.

This document is grounded in the current repository state (audited in
"Current System"). Every work item names its files, functions, REQ trace,
tests, and acceptance proof. Implementation is out of scope until the plan,
mockups and slimming audit are signed off.

## Summary

Deliver one coherent, keyboard-first, hand-built porcelain for the `bd` bead
store without changing the `beads-defcommand` execution/parse layer, in a
pruning-first order. The approved requirement set (33 requirements,
`plans/beads-ui-redesign/requirements.md`) is realised across six waves and
20 work items: Wave 0 prunes deprecated/duplicate surfaces (WI-1..3); Wave 1
lays the extension seams and the real status buffer plus the universal
navigation contract (WI-4..6); Wave 2 redesigns list, detail and the dispatch
menus (WI-7..9); Wave 3 adds standalone sling, adaptive sling, agent launch
and formula UX (WI-10..13); Wave 4 moves terminal handling into beads.el with
a gascity.el shim and remote helpers (WI-14..16); Wave 5 does the faces,
remote-parity and documentation pass (WI-17..19); Wave 6 is the bright-lights
TRAMP acceptance (WI-20). beads.el never depends on gascity.el; every core
flow works standalone (REQ-021), and gascity.el integrates only through the
documented seams (REQ-022). This producer stage owns only the plan artifact;
no `.el` source is modified.

### Requirement traceability

| ID | Status |
| --- | --- |
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

## Current System

Verified in `lisp/` (2026-10-01):

- **Entry/menus.** `beads` transient (`beads.el` §"Main Transient Menu"),
  `beads-more-menu` (in `beads.el`, marked deprecated), `beads-ops-menu`
  (`beads-ops-menu.el`), `beads-advanced-menu` (`beads-advanced-menu.el`),
  `beads-dispatch`-less. `beads-status` (`beads-status.el`) is a
  `make-obsolete` shim forwarding to `beads-dashboard`.
- **Execution layer.** `beads-defcommand` (`beads-command.el`), slot metadata
  and `beads-meta-define-transient` (`beads-meta.el`), `beads-meta-parity-*`
  policy constants, 59 `beads-command-<name>.el` files (256 command classes).
- **Store scoping.** `beads-store-resolve` / `beads-store-directory`
  (used by `beads-dashboard`), `beads--project-root`, `beads-remote-*`
  (`beads-remote.el`), `beads-issue-id-prefixes`, `beads-buffer.el` names.
- **List.** `beads-command-list.el`: `beads-list`/`beads-list-advanced`
  generated transients, `beads-list--populate-buffer`,
  `beads-list--issue-to-entry`, `beads-list--marked-issues`,
  `beads-actions-*` (`beads-actions.el`), `beads-pager.el`.
- **Detail.** `beads-command-show.el`: `beads-show-mode-map`,
  `beads-show--render`/`-update-buffer`, sections via `beads-section.el`.
- **Dashboard/status.** `beads-dashboard.el` + `beads-dashboard-sections.el`,
  vui, sections hook `beads-status-sections-hook`, async loaders.
- **Movement.** `beads-thing.el` (`beads-thing-forward/-backward/-toggle`,
  `beads-thing-define-keys`).
- **Agents.** `beads-agent.el` (prefixes `beads-agent`, `beads-agent-issue`,
  `beads-agent-start-menu`; `beads-agent-start-*` task/review/plan/qa/custom;
  `beads-agent-sling`), `beads-agent-type.el` + `beads-agent-types.el`
  (5 roles), `beads-agent-backend.el` (registry), 6 backends,
  `beads-agent-list.el` (tabulated), `beads-agent-display.el`,
  `beads-agent-keys.el`. F3 deletes `beads-agent-type-qa`,
  `beads-agent-type-custom`, `beads-agent-qa-backend`,
  `beads-agent-start-qa`, `beads-agent-start-custom`, and the `a q`/`a c`
  bindings.
- **Formulas.** `beads-command-formula.el` (list/show/convert/schema classes,
  `beads-formula-list`, `beads-formula-show`, `beads-formula-menu`).
- **Terminal.** `beads-terminal.el` (backends + registry); tmux/status/mouse/
  scroll live in `gascity.el/lisp/gascity-terminal.el`.

### Test surface

- `lisp/test/` has per-module tests; agent subsystem has 18 files
  (`beads-agent*-test.el`); menus have `beads-ops-menu-test.el`,
  `beads-advanced-menu-test.el`, `beads-actions-test.el`; core has
  `beads-core-test.el`; remote/render guards
  (`beads-render-guard-test.el`, `beads-command-remote-test.el`).
- `beads-audit.el`/`beads-audit-test.el` gate CLI parity via
  `beads-meta-parity-*`; the redesign must keep that gate green (no class
  inventory change required).

### Commands

- Focused test: `eldev test -f <file>.el`.
- Full loop: `eldev -p -dtT test`; coverage `eldev -s -dtT test -U
  coverage/codecov.json '(not (tag :integration))'`.
- Lint/compile: `eldev compile`, `eldev -p -dtT lint`.
- TRAMP acceptance: open `/ssh:localhost:~/bright-lights` from a local Emacs
  (or the tmux-Emacs e2e per `AGENTS.md`).

## Proposed Implementation

### Sequencing overview (six waves, all pruning-first)

Each wave ends with `eldev -p -dtT test` and `eldev compile` green, on `main`
(one commit per work item). Wave 0 ships nothing new; it only removes.

| Wave | Work items | Outcome |
|---|---|---|
| 0 — Prune | WI-1, WI-2, WI-3 | deprecated/duplicate menus deleted, generated transients demoted, roster slimmed |
| 1 — Foundation | WI-4, WI-5, WI-6 | extension seams, real status buffer, universal navigation |
| 2 — Porcelain | WI-7, WI-8, WI-9 | list redesign, detail redesign, dispatch menu |
| 3 — Sling/agent/formula | WI-10, WI-11, WI-12, WI-13 | sling abstraction + transient, agent launch, formula browser/launch |
| 4 — Movement | WI-14, WI-15, WI-16 | terminal moved to beads, gascity shim, remote helpers |
| 5 — Polish | WI-17, WI-18, WI-19 | faces/consistency, remote parity + tests, docs/manual |
| 6 — Verify | WI-20 | TRAMP/bright-lights acceptance |

### Work items

#### WI-1 — Delete the deprecated `beads-more-menu`
- **Files:** `beads.el` (remove the prefix + autoload), `beads-ops-menu.el`
  / `beads-advanced-menu.el` (absorb the genuine entries into a temporary
  home until WI-9). `NEWS.md`.
- **REQ:** REQ-023, REQ-026. **Mockup:** §3.
- **Tests:** delete/prune the menu tests that assert `beads-more-menu`;
  add a reachability test that every command it held is reachable via the
  dispatch or a context key.
- **Accept:** `M-x beads-more-menu` no longer defined; no dangling autoload;
  suite green.

#### WI-2 — Demote auto-generated per-command transients
- **Files:** `beads-meta.el` (keep generation but stop registering the
  generated prefixes in the primary dispatch), `beads.el`/future
  `beads-menu.el` (sub-dispatch reach-through), menu tests.
- **REQ:** REQ-003, REQ-024.
- **Tests:** a test asserting the primary `?` no longer references generated
  `beads-<cmd>` prefixes but the commands remain callable; the parity gate
  (`beads-audit-test.el`) stays green.
- **Accept:** no generated transient is on the default key path; per-list `/`
  filter survives; commands callable.
- **Note:** do **not** delete the generated suffixes — they are the
  reach-through and are covered by existing tests. This is a demotion.

#### WI-3 — Remove QA and Custom entirely; slim the backend surface (F3)
- **Files:** `beads-agent.el` (delete `beads-agent-start-qa` /
  `beads-agent-start-custom`; add the Review QA-mode entry; launch menu),
  `beads-agent-types.el` (delete the `beads-agent-type-qa` /
  `beads-agent-type-custom` classes, `beads-agent-qa-backend`, and the
  QA/Custom registration calls; move `beads-agent-qa-prompt` +
  `beads-agent-type-qa--user-prompt` onto the Review QA mode),
  `beads-agent-keys.el` (drop the `a q` / `a c` bindings),
  `beads-agent-backend.el`/`beads-custom.el` (curated backend list +
  overflow), `beads-sling.el` (freeform work picking, from Custom),
  `NEWS.md`.
- **REQ:** REQ-012, REQ-025. **Mockup:** §8, §6e. **Audit:** `slimming.md` §3.
- **Tests:** update the 5 affected `beads-agent-*-test.el` files for the new
  entry points; add a registry test proving the **registry API** still
  registers an out-of-tree type; add a test that the deleted class/command
  symbols are gone (and that the one-release `beads-agent-start-qa` facade, if
  shipped, maps to Review+QA).
- **Accept:** launch UI offers Task/Review(+QA mode)/Plan and
  claude-code/agent-shell/terminal (+ other); `beads-agent-type-qa`,
  `beads-agent-type-custom`, `beads-agent-start-qa`,
  `beads-agent-start-custom` are undefined; QA testing prompt text is present
  on the Review QA path; freeform work is the sling path; `a q`/`a c` freed.

#### WI-4 — Extension seams (foundations)
- **Files:** `beads-util.el` (`beads-store-resolvers`,
  `beads-store-prefix-functions`, `beads-store-descriptor`),
  `beads-section.el` (`beads-section-register`, `beads-section-spec`,
  `beads-status-sections-hook`), `beads-dashboard-sections.el`
  (`beads-dashboard-section-providers`), `beads-actions.el`
  (`beads-action-providers`, `beads-actions-context`,
  `beads-after-action-functions`), **new** `beads-menu.el`
  (`beads-menu-providers`), **new** `beads-sling.el` (`beads-sling-target`,
  `beads-sling-target-functions`, `beads-sling-dispatch`), `beads-buffer.el`
  (`beads-mode-extension-map`, `C-c b`), faces naming.
- **REQ:** REQ-020, REQ-021, REQ-022. **Design:** §4 (authoritative seam
  list). Every seam in `design.md` §4 must exist after this item except the
  ones owned by a later WI: the sling backend registry/validators
  (WI-10/WI-11), the formula launch/vars generics (WI-13),
  `beads-terminal-attach` (WI-14) and the `beads-remote-*` helpers (WI-16).
- **Tests:** unit tests that each hook/provider is consulted and that an
  empty provider list is a no-op (standalone guarantee); a test that
  `beads-sling-dispatch` has a default local method.
- **Accept:** every seam in `design.md` §4 exists with a docstring; no
  gascity reference in beads.

#### WI-5 — Real status buffer (F2)
- **Files:** `beads-status.el` (replace the `make-obsolete` shim with the
  status buffer), `beads.el` (`M-x beads` → `beads-status`; the transient
  prefix moves to `beads-menu.el` as `beads-dispatch`), `beads-dashboard.el`
  (reuse board loaders; `beads-dashboard` stays the full alias),
  `beads-section.el`. `NEWS.md`.
- **REQ:** REQ-001, REQ-002. **Mockup:** §1a–§1e.
- **Tests:** rewrite the existing `beads-status-test.el` (currently four
  tests for the compat shim) for section registration, async loaders,
  `RET`/`TAB`/`q`/`g` behaviour, and the four section states; update
  `beads-dashboard-test.el` for the shared loader; render-guard test.
- **Accept:** `M-x beads` opens a sectioned board; `?` opens
  `beads-dispatch`; `beads-dashboard` unchanged; no sync bd at render; works
  with gascity absent.

#### WI-6 — Universal navigation contract
- **Files:** `beads-section.el`, `beads-thing.el`, `beads-command-show.el`,
  `beads-command-list.el`, `beads-dashboard.el`, `beads-status.el`.
- **REQ:** REQ-002, REQ-018. **Mockups:** all.
- **Tests:** a cross-view test that `q`/`g`/`TAB`/`S-TAB`/`SPC`/`RET`/`?`
  resolve to the contract commands in every porcelain mode; an
  extension-key test (`C-c b` reserved, no core shadowing).
- **Accept:** all views obey the table; `?` opens the dispatch menu.

#### WI-7 — List redesign
- **Files:** `beads-command-list.el`, `beads-pager.el`, `beads-actions.el`,
  `beads-spec.el`.
- **REQ:** REQ-005, REQ-007. **Mockup:** §4.
- **Tests:** extend `beads-list-test.el`; sections/header/filter/marked
  actions; `/` filter transient unchanged; paging.
- **Accept:** mockup §4 rendered state-for-state.

#### WI-8 — Detail redesign
- **Files:** `beads-command-show.el`, `beads-section.el`.
- **REQ:** REQ-006, REQ-007. **Mockup:** §5.
- **Tests:** extend show tests; sections, `RET` on refs, action bar,
  breadcrumbs, agent section attach.
- **Accept:** mockup §5 rendered state-for-state.

#### WI-9 — Dispatch and maintenance menus
- **Files:** **new** `beads-menu.el` (`beads-dispatch`,
  `beads-maintenance`, `beads-menu-providers`), delete the absorbed parts of
  `beads-ops-menu.el`/`beads-advanced-menu.el`, `beads.el`.
- **REQ:** REQ-003, REQ-004, REQ-023. **Mockups:** §2, §3.
- **Tests:** new `beads-menu-test.el`; reachability of every absorbed
  command; provider composition (gascity group absent standalone).
- **Accept:** `?` renders §2; `!` renders §3; every old top-level command
  reachable.

#### WI-10 — Sling abstraction (standalone)
- **Files:** **new** `beads-sling.el` (`beads-sling-target`,
  `beads-sling-target-functions`, `beads-sling-targets`,
  `beads-sling-shape`, `beads-sling-dispatch`, `beads-sling-backend`,
  `beads-sling-backend-register`, `beads-sling-validators`);
  `beads-agent.el`; `beads-completion.el`.
- **REQ:** REQ-008, REQ-021. **Design:** §7, §4.5. **Mockups:** §6f.
- **Tests:** `beads-sling-test.el` (new): default target collection,
  `beads-sling-shape` inference (pure, table-driven), default
  `beads-sling-dispatch` method launches a local agent (stubbed boundary).
- **Accept:** sling works to local targets with gascity absent.

#### WI-11 — Adaptive sling transient and preview
- **Files:** `beads-sling.el`.
- **REQ:** REQ-009. **Mockups:** §6a–§6e, §7.
- **Tests:** shape inference wired to the header; stage collapse; typed How
  readers; live footer; `P` preview launches. Port the gascity sling lessons
  as client-side pure functions.
- **Accept:** mockups §6/§7 rendered state-for-state; `s` never gated by
  preview.

#### WI-12 — Agent launch redesign
- **Files:** `beads-agent.el`, `beads-agent-display.el`,
  `beads-agent-list.el`, `beads-agent-prompt-edit.el`.
- **REQ:** REQ-011, REQ-012. **Mockup:** §8a–§8e. **Audit:** `slimming.md`
  §3.
- **Tests:** launch flow role/target/backend/prompt; Review QA mode; session
  list; attach/jump/stop; prompt preview; lifecycle hook.
- **Accept:** mockup §8; roster is Task/Review(+QA)/Plan; QA/Custom classes
  gone (WI-3); registry API intact.

#### WI-13 — Formula browser and launch
- **Files:** **new** `beads-formula.el` (UI) with
  `beads-formula-launch`, `beads-formula-launch-context`,
  `beads-formula-var`, `beads-formula-var-reader`;
  `beads-command-formula.el` (classes unchanged).
- **REQ:** REQ-013, REQ-014. **Design:** §4.7. **Mockups:** §10a, §10b.
- **Tests:** `beads-formula-test.el` extended: browser grouping, detail
  sections, `l` seeds sling, `s` standalone launch, `beads-formula-launch`
  generic default.
- **Accept:** mockups §10; standalone launch works; gascity can override.

#### WI-14 — Move terminal handling into beads.el
- **Files:** **new** `beads-terminal-tmux.el`; `beads-terminal.el`
  (`beads-terminal-attach`); port from
  `gascity.el/lisp/gascity-terminal.el`. `NEWS.md`.
- **REQ:** REQ-015, REQ-016. **Design:** §6. **Mockup:** §11.
- **Tests:** port the terminal tests that live in gascity's
  `lisp/test/gascity-test.el` (there is no dedicated
  `gascity-terminal-test.el`; see `plan-review.md` F1) to
  `beads-terminal-tmux-test.el`; attach argv/script, status mirror, mouse
  ensure/teardown, scroll sequence + wheel, preload.
- **Accept:** attach works standalone; no `gascity-` symbol referenced;
  suite green.

#### WI-15 — gascity.el compatibility shim
- **Files (gascity checkout):** `gascity.el/lisp/gascity-terminal.el` becomes
  a shim (`defalias`/wrappers) delegating to `beads-terminal-tmux-*`,
  keeping `gascity-tmux-socket` resolution and `gascity-terminal-attach-tmux`.
- **REQ:** REQ-016, REQ-022.
- **Tests:** gascity's terminal tests still pass against the shim; a test
  that the shim and the beads implementation agree.
- **Accept:** gascity.el unchanged in behaviour; duplicated code deleted.

#### WI-16 — Remote helpers for the moved terminal
- **Files:** `beads-remote.el` (`beads-remote-localize-path`,
  `beads-remote-terminfo-p`, optional `beads-remote-prewarm`),
  `beads-buffer.el` (host-qualified names if needed).
- **REQ:** REQ-015, REQ-019. **Design:** §6.3.
- **Tests:** `beads-remote` unit tests for localization/terminfo; TRAMP mock
  method (the tramp-tests pattern already in gascity) ported.
- **Accept:** terminal attach works over the mock/TRAMP path.

#### WI-17 — Faces and consistency pass
- **Files:** all porcelain modules; **new** faces where missing.
- **REQ:** REQ-017, REQ-018. **Mockup:** §12.
- **Tests:** a face-coverage test that every documented `beads-face-*`
  exists; a glyph test that agent/status faces are used consistently.
- **Accept:** one palette across status/list/detail/formula/agent; `NEWS.md`
  notes any face rename.

#### WI-18 — Remote/TRAMP parity and test consolidation
- **Files:** tests across the suite; `beads-render-guard-test.el`.
- **REQ:** REQ-019, REQ-021, REQ-022.
- **Tests:** enumerate the full layout-coupled test surface (the sling
  review's lesson): search all tests referencing the removed menus,
  generated prefixes, `beads-more-menu`, the old role commands; port them.
- **Accept:** full suite green; render guard green; no test references a
  removed symbol.

#### WI-19 — Documentation
- **Files:** `README.md` (architecture section refresh), `docs/`
  (e.g. `docs/ui-redesign.md`, `docs/terminal-scrolling.md` moved from
  gascity), `NEWS.md`, `MAGIT_PATTERNS.md` if conventions change.
- **REQ:** REQ-033, REQ-020.
- **Accept:** the extension seams and the new entry point are documented;
  removed surfaces listed.

#### WI-20 — Acceptance verification (bright-lights / TRAMP)
- **Files:** none (verification).
- **REQ:** REQ-019, REQ-021, REQ-022.
- **Procedure:** from a local Emacs, open
  `/ssh:localhost:~/bright-lights`, then: open the status buffer; browse
  list + detail; sling to a local-target (and, with gascity loaded, a
  gc target); start and attach an agent; browse/launch a formula; drive
  terminal scroll. Confirm identical behaviour to a local store.
- **Accept:** all flows pass over TRAMP; screenshots/notes recorded.

## Non-Goals

Everything not listed above is out of scope for this implementation plan; in
particular:

- No `.el` source change and no test change in this planning task
  (REQ-030); this artifact plans only.
- No gascity.el change beyond the WI-15 compatibility shim; gascity.el edits
  are a downstream bead.
- No gc-side changes.
- Implementation does not begin until the plan, mockups, decomposition and
  slimming audit are signed off (F2/F3 are already decided and are not
  re-opened here).
- Re-deciding the hard constraints in `requirements.md` (hand-built UI,
  beads.el must not depend on gascity.el, planning-only scope) is a non-goal.

## Verification

| Level | Command / procedure |
|---|---|
| Unit | `eldev test -f <module>-test.el` |
| Full | `eldev -p -dtT test` |
| Coverage | `eldev -s -dtT test -U coverage/codecov.json '(not (tag :integration))'` |
| Compile/lint | `eldev compile`; `eldev -p -dtT lint` |
| CLI parity | `beads-audit-test.el` gate (must stay green) |
| Remote render | `beads-render-guard-test.el` |
| E2E | WI-20 bright-lights TRAMP / tmux-Emacs pass |

## Risks (implementation-time)

- **Test churn.** Removing menus and the roster touches many tests; WI-18
  enumerates them exhaustively before the churn (the sling review's F1
  lesson). Mitigation: grep the suite for every removed symbol in WI-1/2/3
  and track the list.
- **Generated-transient reach-through.** Demoting without deleting must keep
  `M-x beads-<cmd>` working; tests prove it.
- **Terminal move cycle.** beads must never require gascity; the shim lives
  in gascity (WI-15).
- **TRAMP latency.** Sling/How typed readers must fail soft; no sync remote
  I/O at render (WI-16/WI-18).
- **Role-roster removal (F3).** WI-3 deletes the QA/Custom classes
  entirely; the QA testing prompt is kept on Review's QA mode and the
  Custom freeform prompt on the sling path. Mitigation: the one-release
  `beads-agent-start-qa` facade, `NEWS.md`, and the registry API staying
  open for user subclasses.
- **`C-c b` reservation.** Keep the old binding as an alias for one release;
  `NEWS.md`.

## Rollback

Per-work-item commits on `main`. Wave 0 is pure deletion and is trivially
reverted (it only removes deprecated/duplicate surfaces). The terminal move
(WI-14/15) keeps the gascity implementation until WI-15 is green, so a
revert of WI-14 alone restores gascity's code.
