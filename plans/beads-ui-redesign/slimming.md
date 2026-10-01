---
schema: beads.ui-redesign.slimming.v1
workflow:
  id: be-59fe
artifact: slimming
status: draft-for-review
scope: planning-only
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
| C1 | 5 typed start commands (`-task`/`-review`/`-plan`/`-qa`/`-custom`) | **Slim to 3** (`-task`/`-review`/`-plan`) | QA is Review with a QA prompt mode; Custom is not a role but the freeform escape. Five near-identical commands add surface. | `-qa` becomes `-review` with a prefix; `-custom` becomes sling freeform / `C-u` on the launch flow. Aliases kept for one release. |
| C2 | `beads-q` (quick capture) | **Demote** | a thin alias for create; the redesigned create compose buffer is the primary. | `beads-compose-create`; `beads-q` kept as an alias. |
| C3 | `beads-promote` (promote wisp) | **Keep** | distinct mol workflow action. | unchanged, reached via molecules. |
| C4 | `beads-agent-keys` redundant keymap entries | **Collapse** | duplicate bindings of the agent-prefix-map. | one `beads-agent-prefix-map`. |
| C5 | Ways to reach the same action (`s` status/`e` edit in show; `?` actions menu) | **Consolidate** | overlap between context keys, action bar and `?`. | context keys are canonical; `?` dispatches; the show action bar advertises the same keys. |

## 3. Agent role roster (explicitly called out)

Today: **5 roles** (Task, Review, Plan, QA, Custom) × **6 backends**
(claude-code, claude-code-ide, claudemacs, eca, agent-shell, terminal; plus
mock for tests) = 30 exposed combinations.

Proposed **exposed** roster after slimming:

| Role | Action | Justification | Replacement |
|---|---|---|---|
| **Task** | **Keep** | the workhorse; autonomous completion. | unchanged; default. |
| **Review** | **Keep, absorb QA** | review and QA are the same "check the work" role with different prompts; one role with a QA mode is one mental model. | `Review` gains a QA mode (`r` then a QA toggle, or `beads-agent-start-qa` maps to Review+QA). |
| **Plan** | **Keep** | read-only planning is a genuinely distinct mode (backend plan mode). | unchanged. |
| **QA** | **Collapse into Review** | not a distinct lifecycle; a prompt variant. | Review in QA mode. |
| **Custom** | **Delete as a role** | a freeform prompt is not a role; it belongs in sling's freeform path. | sling work-picker freeform text / `C-u` on Task; the `beads-agent-type-custom` class stays registered for those who want it. |

Backend surface:

| Backend | Action | Justification | Replacement |
|---|---|---|---|
| claude-code | **Keep (preferred)** | primary backend. | default in `b`. |
| agent-shell | **Keep** | generic shell-backed agent; widely usable. | `b` list. |
| terminal | **Keep** | raw terminal launch; distinct capability. | `b` list. |
| claude-code-ide | **Demote** | niche IDE integration. | `… other` overflow / `beads-agent-switch-backend`; registry unchanged. |
| claudemacs | **Demote** | niche. | `… other` overflow. |
| eca | **Demote** | niche. | `… other` overflow. |
| mock | **Keep (test-only)** | test infrastructure. | never in the user menu. |

Net: exposed roster **5 → 3 roles**, exposed backends **6 → 3 + an overflow**
(registry unchanged at 7). Every removal keeps the registry entry, so power
users and gascity.el are unaffected; only the *surfaced* default shrinks.

**Genuine decision flagged for sign-off** (`plan-review.md` F3): the
QA→Review fold and the Custom removal are the two cuts most likely to
affect an existing workflow. Conservative fallback if the user prefers:
keep QA as a 4th role (it is already registered) but hide it from the launch
menu behind `… more roles`; keep Custom reachable from the launch flow's
`e` prompt as today. The backend demotion is uncontroversial and lands
either way.

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
- **`beads-agent` registries** — extension seams; slimming is about the
  *exposed* roster, never the registries.
- **`beads-eldoc`** — cross-cutting preview; costs nothing at the menu level.
- **`beads-compose`** — buffer-based create/edit; the primary mutate flow.

## 6. Migration / gentleness rules

1. Every deleted command keeps a one-release `defalias` to its replacement
   (C1, C2) and a `NEWS.md` "Deprecated"/"Removed" entry.
2. Registry entries are never removed (roles, backends); only surfaced
   defaults change.
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
