---
schema: beads.ui-redesign.slimming.v1
workflow:
  id: be-mv8d
  predecessor: be-59fe
artifact: slimming
status: draft-for-review
scope: planning-only
refinement: 2
decisions_folded: [F3]
---

# beads.el UI Redesign — Slimming / Surface-Area Reduction Audit

The redesign must **remove** as well as add (REQ-023…REQ-026). This is the
explicit removal/collapse audit: every surface that goes away, why, and what
replaces it. Anything kept must earn its place. Nothing here is
"deprecated but kept forever": each removal ships a one-release alias where
that is cheap, plus a `NEWS.md` entry.

Legend: **Delete** (gone), **Collapse** (merged into one), **Demote** (kept
as a backend/overflow, off the primary path), **Slim** (fewer items, same
kind).

---

## 1. Menus

| # | Surface | Action | Justification | Replacement |
|---|---|---|---|---|
| M1 | `beads-more-menu` | **Delete** | already marked deprecated in-source; ~80 entries duplicating other menus and per-command transients; a dumping ground. | Its genuine entries move into `beads-menu.el` maintenance/dispatch groups; the rest are reachable as generated sub-dispatch (`i Show…`, `L Labels…`). |
| M2 | `beads-ops-menu` | **Collapse** into `beads-menu.el` (`!` Maintenance) | mid-frequency menu; groups overlap `beads-advanced-menu` (dolt/admin/backup). | one `beads-maintenance` menu, paginated (mockup §3). |
| M3 | `beads-advanced-menu` | **Collapse** into `beads-menu.el` (`!` Maintenance / `>` Advanced page 2) | maintenance/admin surface; an extra hop with no distinct purpose. | same menu, second page. |
| M4 | Auto-generated per-command transients (`beads-<cmd>` from `beads-defcommand`) | **Demote** | they are the primary key path today but are generated layouts, not designed porcelain; REQ-003 forbids them as the primary interface. | reachable as sub-dispatch from `?` (`i Show…`, `L Labels…`, `F Formulas…`, `k Dolt…`, `. Config`); the command classes themselves are unchanged and remain the execution layer. |
| M5 | `beads-list` generated filter transient | **Keep** (allowed) | per-list filters are the one permitted generated surface (constraint). | unchanged `/` filter (mockup §4). |
| M6 | Duplicate start menus (`beads-agent-start-menu`, `beads-agent`, `beads-agent-issue`, generated per-role suffixes) | **Collapse** into the redesigned agent-launch flow + `beads-agent` status entries | four overlapping ways to start an agent. | one launch transient (mockup §8), one `beads-agent` dispatch; sessions list stays. |
| M7 | Both `beads-status` (shim) and `beads-dashboard` as separate "entries" | **Collapse** | `beads-status` forwards to `beads-dashboard` and is already obsolete; two names for one board is confusion. | one real status buffer (`beads-status`) plus `beads-dashboard` as its full-board alias (REQ-001). |

## 2. Commands and keybindings

| # | Surface | Action | Justification | Replacement |
|---|---|---|---|---|
| C1 | 5 typed start commands (`-task`/`-review`/`-plan`/`-qa`/`-custom`) | **Delete `-qa` and `-custom`; keep 3** (`-task`/`-review`/`-plan`) | QA is Review with a QA prompt mode; Custom is not a role but the freeform escape. F3 removes them entirely, not by hiding. | `-qa` deleted (its testing prompt becomes Review's QA mode; a one-release facade accepts the old command name); `-custom` deleted (its freeform prompt becomes the sling work-picker escape). The freed `a q` / `a c` keys are **released** (reserved, not reused by this plan). |
| C2 | `beads-q` (quick capture) | **Demote** | a thin alias for create; the redesigned create compose buffer is the primary. | `beads-compose-create`; `beads-q` kept as an alias. |
| C3 | `beads-promote` (promote wisp) | **Keep** | distinct mol workflow action. | unchanged, reached via molecules. |
| C4 | `beads-agent-keys` redundant keymap entries | **Collapse** | duplicate bindings of the agent-prefix-map. | one `beads-agent-prefix-map`. |
| C5 | Ways to reach the same action (`s` status/`e` edit in show; `?` actions menu) | **Consolidate** | overlap between context keys, action bar and `?`. | context keys are canonical; `?` dispatches; the show action bar advertises the same keys. |

## 3. Agent role roster (F3 — remove entire classes)

> **Status (WI-3, implemented):** the deletions in §3.1 are landed.
> `beads-agent-type-qa` / `beads-agent-type-custom`,
> `beads-agent-start-qa` / `beads-agent-start-custom`, and
> `beads-agent-qa-backend` are gone; the QA prompt text lives on as
> `beads-agent-review-qa-prompt` used by Review's `qa-mode` slot, and
> the `a q` / `a c` keys are freed.  `beads-agent-start-custom` has no
> facade (the sling freeform escape replaces it); `beads-agent-start-qa`
> was removed rather than aliased.  See `NEWS.md`.

Today: **5 roles** (Task, Review, Plan, QA, Custom) × **6 backends**
(claude-code, claude-code-ide, claudemacs, eca, agent-shell, terminal; plus
mock for tests) = 30 exposed combinations.

F3 is deeper than round 1: QA and Custom are **removed entirely**, not hidden.
The target roster is Task/Review/Plan, with QA surviving only as a **mode** of
Review and the QA testing prompt kept on that path, and Custom surviving only
as the **sling freeform path**.

### 3.1 What is deleted (not hidden)

| Deleted surface | File | Disposition |
|---|---|---|
| `beads-agent-type-qa` class | `beads-agent-types.el` | deleted; QA is Review + QA mode |
| `beads-agent-type-custom` class | `beads-agent-types.el` | deleted; freeform is sling |
| `beads-agent-qa-prompt`, `beads-agent-type-qa--user-prompt` | `beads-agent-types.el` | **kept but relocated** as the QA mode of Review (the class is deleted; the prompt text stays) |
| `beads-agent-qa-backend` defcustom | `beads-agent-types.el` | deleted; QA uses Review's backend |
| `beads-agent-start-qa` | `beads-agent.el` | deleted; one-release facade maps the name to Review+QA |
| `beads-agent-start-custom` | `beads-agent.el` | deleted; freeform is the sling work escape |
| `a q`, `a c` bindings | `beads-agent-keys.el` | deleted; the keys are freed |
| QA/Custom registration calls | `beads-agent-types.el` | deleted |
| references in 5 test files | `lisp/test/beads-agent*-test.el` | updated/pruned |

### 3.2 Resulting exposed roster

| Role | Action | Justification | Replacement |
|---|---|---|---|
| **Task** | **Keep** | the workhorse; autonomous completion. | unchanged; default. |
| **Review** | **Keep, absorbs QA** | review and QA are the same "check the work" role with different prompts; one role with a QA mode is one mental model. | `Review` gains a QA mode (`r` then the `q` toggle); the QA testing prompt is moved into it. |
| **Plan** | **Keep** | read-only planning is a genuinely distinct mode (backend plan mode). | unchanged. |
| **QA** | **Delete class** | not a distinct lifecycle; a prompt variant. | Review in QA mode (prompt kept). |
| **Custom** | **Delete class** | a freeform prompt is not a role; it belongs in sling's freeform path. | sling work-picker freeform text (`A` with a prefix arg); the freeform prompt text is kept there. |

### 3.3 Backends

| Backend | Action | Justification | Replacement |
|---|---|---|---|
| claude-code | **Keep (preferred)** | primary backend. | default in `b`. |
| agent-shell | **Keep** | generic shell-backed agent; widely usable. | `b` list. |
| terminal | **Keep** | raw terminal launch; distinct capability. | `b` list. |
| claude-code-ide | **Demote** | niche IDE integration. | `… other` overflow; registry unchanged. |
| claudemacs | **Demote** | niche. | `… other` overflow. |
| eca | **Demote** | niche. | `… other` overflow. |
| mock | **Keep (test-only)** | test infrastructure. | never in the user menu. |

### 3.4 Freed keys

`a q` and `a c` are released by the F3 deletion. This plan does **not**
reassign them; they are reserved so a downstream package or a future role can
claim them without a second collision. (`c` elsewhere in beads is
create/comment per the context table; the agent prefix is separate.)

### 3.5 Registry stance

The `beads-agent-type` and `beads-agent-backend` **registries** remain open
and documented (REQ-020). There is **no** built-in QA/Custom class left to
re-register; a user who wants either writes a subclass and calls
`beads-agent-type-register`, exactly as the manual demonstrates. This is a
conscious change from round 1, which would have kept the classes registered
but hidden.

### 3.6 Migration/gentleness

- `beads-agent-start-qa` keeps a one-release `defalias` to a Review+QA
  start; `beads-agent-start-custom` has no alias (the freeform path is the
  replacement) and is documented as removed in `NEWS.md`.
- No hidden class is retained, so there is no "hidden registry" to audit.
- The 5 affected test files are updated in the same work item (WI-3); no
test may reference a deleted class.

## 4. Gascity.el surface (de-duplication, not user-facing removal)

| Surface | Action | Justification | Replacement |
|---|---|---|---|
| `gascity-terminal.el` implementation (tmux/status/mouse/scroll) | **Move** to `beads-terminal-tmux.el` | logically base-package capability; gascity already depends on beads. | thin `gascity-terminal.el` shim (REQ-015/016). |
| `gascity-terminal--beads-integrate` etc. | **Move** | these are beads operations. | beads `beads-terminal.el`. |
| gascity tmux socket resolution | **Keep in gascity** | city-specific. | passed as `:socket` to `beads-terminal-attach`. |
| `gascity-remote-localize-path`, `-terminfo-p`, `-prewarm` | **Move** to `beads-remote.el` | generic remote helpers. | `beads-remote-*`. |

This *adds* code to beads.el but *removes* a whole module's worth of
duplication from the ecosystem; gascity.el ends thinner.

## 5. What is kept and why it earns its place

- **`beads-meta` command classes** — the execution + parse layer; the whole
  design rests on them.
- **`beads-section` / `vui`** — the sectioned porcelain substrate.
- **`beads-thing`** — one movement scheme; removing it would reintroduce
  per-view movement.
- **`beads-dashboard`** — the full board; the status buffer is a focused
  front door, not a replacement.
- **`beads-pager`** — window-sized pagination; used by list and formula list.
- **Per-list `/` filter transient** — explicitly allowed, and genuinely
  useful.
- **`beads-agent` registries** — extension seams; F3 deletes the built-in
  QA/Custom **classes** but leaves the registries open and documented.
- **`beads-eldoc`** — cross-cutting preview; costs nothing at the menu level.
- **`beads-compose`** — buffer-based create/edit; the primary mutate flow.

## 6. Migration / gentleness rules

1. Every deleted **menu** leaves its commands reachable via `?` sub-dispatch
   for one release. Deleted **agent commands**: `beads-agent-start-qa`
   keeps a one-release facade to Review+QA; `beads-agent-start-custom` is
   removed with the freeform sling path as its replacement. All removals get
   a `NEWS.md` "Deprecated"/"Removed" entry.
2. Built-in QA/Custom **classes are deleted** (F3); only the *registry API*
   stays open for user-defined subclasses. The QA testing prompt text is
   preserved on Review's QA mode; the Custom freeform prompt text is
   preserved on the sling freeform path.
3. Removed menus leave their *commands* reachable via `?` sub-dispatch for
   one release before the generated reach-through is trimmed.
4. The terminal move keeps the gascity shim until a deprecation window
   passes.

## 7. Coverage of the slimming requirement

- REQ-023 (collapse redundant menus): M1–M3, M6, M7.
- REQ-024 (demote generated transients): M4, M5 (allowed keep).
- REQ-025 (slim role roster): §3.
- REQ-026 (replacement for every removal): every table row has a
  "Replacement" cell; §5 justifies what is kept.
