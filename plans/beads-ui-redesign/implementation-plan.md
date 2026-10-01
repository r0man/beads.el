---
schema: beads.ui-redesign.implementation-plan.v1
workflow:
  id: be-59fe
artifact: implementation-plan
status: draft-for-review
scope: planning-only
order: pruning-first
---

# beads.el UI Redesign — Implementation Plan

Ordered plan for the later, separately-approved implementation. The plan is
**pruning-first** (REQ-031): removals land before new surfaces, so the new
porcelain is built on a smaller surface, never in front of the debris.

This document is grounded in the current repository state (audited in §1).
Every work item names its files, functions, REQ trace, tests, and acceptance
proof. Implementation is out of scope until the plan, mockups and slimming
audit are signed off.

---

## 1. Current-system audit (what exists today)

Verified in `lisp/` (2026-10-01):

- **Entry/menus.** `beads` transient (`beads.el` §"Main Transient Menu"),
  `beads-more-menu` (in `beads.el`, marked deprecated), `beads-ops-menu`
  (`beads-ops-menu.el`), `beads-advanced-menu` (`beads-advanced-menu.el`),
  `beads-dispatch`-less. `beads-status` (`beads-status.el`) is a
  `make-obsolete` shim forwarding to `beads-dashboard`.
- **Execution layer.** `beads-defcommand` (`beads-command.el`), slot metadata
  and `beads-meta-define-transient` (`beads-meta.el`), `beads-meta-parity-*`
  policy constants, 90+ `beads-command-<name>.el` classes.
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
  (5 roles), `beads-agent-backend.el` (registry), 6 backends, `beads-agent-list.el`
  (tabulated), `beads-agent-display.el`, `beads-agent-keys.el`.
- **Formulas.** `beads-command-formula.el` (list/show/convert/schema classes,
  `beads-formula-list`, `beads-formula-show`, `beads-formula-menu`).
- **Terminal.** `beads-terminal.el` (backends + registry); tmux/status/mouse/
  scroll live in `gascity.el/lisp/gascity-terminal.el`.

### Test surface

- `lisp/test/` has per-module tests; agent subsystem has ~15 files
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

---

## 2. Sequencing overview (six waves, all pruning-first)

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

---

## 3. Work items

### WI-1 — Delete the deprecated `beads-more-menu`
- **Files:** `beads.el` (remove the prefix + autoload), `beads-ops-menu.el`
  / `beads-advanced-menu.el` (absorb the genuine entries into a temporary
  home until WI-9). `NEWS.md`.
- **REQ:** REQ-023, REQ-026. **Mockup:** §3.
- **Tests:** delete/prune the menu tests that assert `beads-more-menu`;
  add a reachability test that every command it held is reachable via the
  dispatch or a context key.
- **Accept:** `M-x beads-more-menu` no longer defined; no dangling autoload;
  suite green.

### WI-2 — Demote auto-generated per-command transients
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

### WI-3 — Slim the agent role and backend surface
- **Files:** `beads-agent.el` (`beads-agent-start-qa` → Review+QA alias;
  `beads-agent-start-custom` → sling freeform alias; launch menu),
  `beads-agent-types.el` (QA folds into Review mode; Custom stays
  registered), `beads-agent-backend.el`/`beads-custom.el` (curated backend
  list + overflow), `NEWS.md`.
- **REQ:** REQ-012, REQ-025. **Mockup:** §8. **Audit:** `slimming.md` §3.
- **Tests:** update `beads-agent-*-test.el` for the new entry points; add a
  registry test proving all 5 roles / 7 backends remain registered and
  reachable programmatically.
- **Accept:** launch UI offers Task/Review/Plan and claude-code/agent-shell/
  terminal (+ other); QA and Custom remain reachable; registries intact.

### WI-4 — Extension seams (foundations)
- **Files:** `beads-section.el` (`beads-section-register`,
  `beads-status-sections-hook`), `beads-command-list.el` / `beads-agent.el`
  (`beads-action-providers`), `beads.el`/`beads-menu.el`
  (`beads-menu-providers`), **new** `beads-sling.el` (target class/hook,
  `beads-sling-dispatch`), `beads-buffer.el` (`beads-mode-extension-map`,
  `C-c b`), `beads-remote.el` (resolver/prefix hooks), faces naming.
- **REQ:** REQ-020, REQ-021, REQ-022. **Design:** §4.
- **Tests:** unit tests that each hook/provider is consulted and that an
  empty provider list is a no-op (standalone guarantee); a test that
  `beads-sling-dispatch` has a default local method.
- **Accept:** every seam in `design.md` §4 exists with a docstring; no
  gascity reference in beads.

### WI-5 — Real status buffer
- **Files:** `beads-status.el` (replace the shim with the status buffer),
  `beads.el` (`M-x beads` → status), `beads-dashboard.el` (reuse board
  loaders; `beads-dashboard` stays the full alias), `beads-section.el`.
- **REQ:** REQ-001, REQ-002. **Mockup:** §1.
- **Tests:** `beads-status-test.el` (new) for section registration, async
  loaders, `RET`/`TAB`/`q`/`g` behaviour; update
  `beads-dashboard-test.el` for the shared loader; render-guard test.
- **Accept:** `M-x beads` opens a sectioned board; no sync bd at render;
  works with gascity absent.

### WI-6 — Universal navigation contract
- **Files:** `beads-section.el`, `beads-thing.el`, `beads-command-show.el`,
  `beads-command-list.el`, `beads-dashboard.el`, `beads-status.el`.
- **REQ:** REQ-002, REQ-018. **Mockups:** all.
- **Tests:** a cross-view test that `q`/`g`/`TAB`/`S-TAB`/`SPC`/`RET`/`?`
  resolve to the contract commands in every porcelain mode; an
  extension-key test (`C-c b` reserved, no core shadowing).
- **Accept:** all views obey the table; `?` opens the dispatch menu.

### WI-7 — List redesign
- **Files:** `beads-command-list.el`, `beads-pager.el`, `beads-actions.el`,
  `beads-spec.el`.
- **REQ:** REQ-005, REQ-007. **Mockup:** §4.
- **Tests:** extend `beads-list-test.el`; sections/header/filter/marked
  actions; `/` filter transient unchanged; paging.
- **Accept:** mockup §4 rendered state-for-state.

### WI-8 — Detail redesign
- **Files:** `beads-command-show.el`, `beads-section.el`.
- **REQ:** REQ-006, REQ-007. **Mockup:** §5.
- **Tests:** extend show tests; sections, `RET` on refs, action bar,
  breadcrumbs, agent section attach.
- **Accept:** mockup §5 rendered state-for-state.

### WI-9 — Dispatch and maintenance menus
- **Files:** **new** `beads-menu.el` (`beads-dispatch`,
  `beads-maintenance`, `beads-menu-providers`), delete the absorbed parts of
  `beads-ops-menu.el`/`beads-advanced-menu.el`, `beads.el`.
- **REQ:** REQ-003, REQ-004, REQ-023. **Mockups:** §2, §3.
- **Tests:** new `beads-menu-test.el`; reachability of every absorbed
  command; provider composition (gascity group absent standalone).
- **Accept:** `?` renders §2; `!` renders §3; every old top-level command
  reachable.

### WI-10 — Sling abstraction (standalone)
- **Files:** **new** `beads-sling.el`; `beads-agent.el`; `beads-completion.el`.
- **REQ:** REQ-008, REQ-021. **Design:** §7. **Mockups:** §6f.
- **Tests:** `beads-sling-test.el` (new): default target collection,
  `beads-sling-shape` inference (pure, table-driven), default
  `beads-sling-dispatch` method launches a local agent (stubbed boundary).
- **Accept:** sling works to local targets with gascity absent.

### WI-11 — Adaptive sling transient and preview
- **Files:** `beads-sling.el`.
- **REQ:** REQ-009. **Mockups:** §6a–§6e, §7.
- **Tests:** shape inference wired to the header; stage collapse; typed How
  readers; live footer; `P` preview launches. Port the gascity sling lessons
  as client-side pure functions.
- **Accept:** mockups §6/§7 rendered state-for-state; `s` never gated by
  preview.

### WI-12 — Agent launch redesign
- **Files:** `beads-agent.el`, `beads-agent-display.el`,
  `beads-agent-list.el`, `beads-agent-prompt-edit.el`.
- **REQ:** REQ-011, REQ-012. **Mockup:** §8. **Audit:** `slimming.md` §3.
- **Tests:** launch flow role/target/backend/prompt; session list;
  attach/jump/stop; prompt preview.
- **Accept:** mockup §8; roster slimmed; registries intact (WI-3).

### WI-13 — Formula browser and launch
- **Files:** **new** `beads-formula.el` (UI), `beads-command-formula.el`
  (classes unchanged).
- **REQ:** REQ-013, REQ-014. **Mockups:** §10a, §10b.
- **Tests:** `beads-formula-test.el` extended: browser grouping, detail
  sections, `l` seeds sling, `s` standalone launch, `beads-formula-launch`
  generic default.
- **Accept:** mockups §10; standalone launch works; gascity can override.

### WI-14 — Move terminal handling into beads.el
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

### WI-15 — gascity.el compatibility shim
- **Files (gascity checkout):** `gascity.el/lisp/gascity-terminal.el` becomes
  a shim (`defalias`/wrappers) delegating to `beads-terminal-tmux-*`,
  keeping `gascity-tmux-socket` resolution and `gascity-terminal-attach-tmux`.
- **REQ:** REQ-016, REQ-022.
- **Tests:** gascity's terminal tests still pass against the shim; a test
  that the shim and the beads implementation agree.
- **Accept:** gascity.el unchanged in behaviour; duplicated code deleted.

### WI-16 — Remote helpers for the moved terminal
- **Files:** `beads-remote.el` (`beads-remote-localize-path`,
  `beads-remote-terminfo-p`, optional `beads-remote-prewarm`),
  `beads-buffer.el` (host-qualified names if needed).
- **REQ:** REQ-015, REQ-019. **Design:** §6.3.
- **Tests:** `beads-remote` unit tests for localization/terminfo; TRAMP mock
  method (the tramp-tests pattern already in gascity) ported.
- **Accept:** terminal attach works over the mock/TRAMP path.

### WI-17 — Faces and consistency pass
- **Files:** all porcelain modules; **new** faces where missing.
- **REQ:** REQ-017, REQ-018. **Mockup:** §12.
- **Tests:** a face-coverage test that every documented `beads-face-*`
  exists; a glyph test that agent/status faces are used consistently.
- **Accept:** one palette across status/list/detail/formula/agent; `NEWS.md`
  notes any face rename.

### WI-18 — Remote/TRAMP parity and test consolidation
- **Files:** tests across the suite; `beads-render-guard-test.el`.
- **REQ:** REQ-019, REQ-021, REQ-022.
- **Tests:** enumerate the full layout-coupled test surface (the sling
  review's lesson): search all tests referencing the removed menus,
  generated prefixes, `beads-more-menu`, the old role commands; port them.
- **Accept:** full suite green; render guard green; no test references a
  removed symbol.

### WI-19 — Documentation
- **Files:** `README.md` (architecture section refresh), `docs/`
  (e.g. `docs/ui-redesign.md`, `docs/terminal-scrolling.md` moved from
  gascity), `NEWS.md`, `MAGIT_PATTERNS.md` if conventions change.
- **REQ:** REQ-033, REQ-020.
- **Accept:** the extension seams and the new entry point are documented;
  removed surfaces listed.

### WI-20 — Acceptance verification (bright-lights / TRAMP)
- **Files:** none (verification).
- **REQ:** REQ-019, REQ-021, REQ-022.
- **Procedure:** from a local Emacs, open
  `/ssh:localhost:~/bright-lights`, then: open the status buffer; browse
  list + detail; sling to a local-target (and, with gascity loaded, a
  gc target); start and attach an agent; browse/launch a formula; drive
  terminal scroll. Confirm identical behaviour to a local store.
- **Accept:** all flows pass over TRAMP; screenshots/notes recorded.

---

## 4. Verification strategy

| Level | Command / procedure |
|---|---|
| Unit | `eldev test -f <module>-test.el` |
| Full | `eldev -p -dtT test` |
| Coverage | `eldev -s -dtT test -U coverage/codecov.json '(not (tag :integration))'` |
| Compile/lint | `eldev compile`; `eldev -p -dtT lint` |
| CLI parity | `beads-audit-test.el` gate (must stay green) |
| Remote render | `beads-render-guard-test.el` |
| E2E | WI-20 bright-lights TRAMP / tmux-Emacs pass |

---

## 5. Risks (implementation-time)

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
- **Role-roster decision.** WI-3 waits on the `slimming.md` §3 sign-off;
  conservative fallback documented.
- **`C-c b` reservation.** Keep the old binding as an alias for one release;
  `NEWS.md`.

## 6. Rollback

Per-work-item commits on `main`. Wave 0 is pure deletion and is trivially
reverted (it only removes deprecated/duplicate surfaces). The terminal move
(WI-14/15) keeps the gascity implementation until WI-15 is green, so a
revert of WI-14 alone restores gascity's code.

## 7. Out of scope

Everything not listed above; in particular the actual code changes and tests
until sign-off, and any gascity.el change beyond the WI-15 shim.
