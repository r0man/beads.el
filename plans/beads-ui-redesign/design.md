---
schema: beads.ui-redesign.design.v1
workflow:
  id: be-59fe
artifact: design
status: draft-for-review
scope: planning-only
---

# beads.el UI Redesign — Design

*A world-class, Magit-style Emacs porcelain for the `bd` bead store.*

Status: **plan / design for review** (2026-10-01). Supersedes the
auto-generated-transient framing of the current `beads` menu. Implementation
is out of scope until this plan and `menu-mockups.md` are reviewed and signed
off. Mirrors the process and artifact shape of
`gascity.el/plans/sling-command/` and the binding "UI direction" in
`gascity.el/docs/DESIGN.md` §1.

---

## 1. Thesis

`bd` is the *plumbing*; beads.el is the *porcelain*. Every view is a function
of `bd … --json` output. We do not reimplement bd logic — we render its state
and dispatch its commands, keyboard-first, the way Magit fronts git and Forge
fronts GitHub.

Three things are deliberately **reused rather than rebuilt**:

1. **`beads-meta` command classes** — the EIEIO slot-metadata engine stays
   exactly as it is: the **execution + parse layer**. A class declares slots;
   `beads-meta` infers CLI options, builds the command line, and
   `beads-command-execute` / `-async` run `bd` and decode `--json` into
   `beads-types` objects. Auto-generated transients are still *produced* by
   the macro, but per REQ-003 they are demoted to a dispatch backend, not the
   porcelain.
2. **vui + `tabulated-list-mode`** — the rendering layer. `beads-section-mode`
   derives from `vui-mode`; the dashboard derives from that. Rich,
   heterogeneous, collapsible views are vui; homogeneous sortable/filterable
   tables are `tabulated-list-mode` (REQ-005).
3. **`beads-thing`** — the one movement scheme. A text property marks
   "things"; TAB/S-TAB move, SPC toggles. Every view inherits it, so movement
   never varies.

What we build is the **porcelain layer**: the status buffer, the redesigned
list and detail, the sling and agent-launch flows, the formula UX, a
documented extension model, and the terminal handling moved in from
gascity.el.

### View-technology decision matrix

| Surface | Mechanism | Rationale |
|---|---|---|
| Status board / store overview | **vui** (sections, async per-section) | heterogeneous, collapsible, async |
| Bead list | **tabulated-list-mode** | sorting, point identity, paging, filters |
| Bead detail | **vui / `beads-section-mode`** | heterogeneous sections + inline actions |
| Formula list | **tabulated-list-mode** | homogeneous; type-grouped |
| Formula detail | **vui / `beads-section-mode`** | steps + vars, collapsible |
| Sling / agent-launch | **transient** (hand-built) | argument collection + live footer; never renders content |
| Sling full preview | **special-mode buffer** | DAG + routing + warnings, launchable |
| Sessions/agents list | **tabulated-list-mode** | homogeneous |
| Terminal attach | **`beads-terminal` backends** | pty is the content |
| Dispatch menu | **transient** (hand-built) | command dispatch only |

Anti-patterns (binding, inherited from the gascity UI direction): never
render content in a transient; never use vui for homogeneous lists; never
`insert` inside a vui buffer; batch multi-field `vui-set-state`.

---

## 2. Non-goals (explicitly decided)

- **No implementation in this planning task.** Docs only.
- **No gascity.el dependency in beads.el.** gascity depends on beads; the
  reverse never happens.
- **No reimplementation of bd.** We render and dispatch.
- **No preservation of a generated transient as porcelain.** The generated
  transients may back `?`/dispatch and list filters, nothing more.
- **No new UI features outside the eight redesign areas.** The plan removes
  as much as it adds (REQ-023…REQ-026).
- **No change to the `beads-command-*` class inventory** beyond what the
  redesign needs; classes are the stable layer.

---

## 3. The one command surface

Every view obeys REQ-002. The reserved per-view keys:

```
q     bury buffer            g     refresh in place
TAB   toggle thing/section   S-TAB move backward
SPC   toggle thing           RET   visit / activate
?     dispatch menu          C-u g hard refresh (clear caches)
n p   next/previous item     N P   next/previous section
```

Extension keys are *not* placed on these; they live under a reserved prefix
`C-c b` (see §4.12). A downstream package that needs a first-class key
proposes it to beads.el; it does not shadow a core key.

---

## 4. The magit/forge extension model (concrete seam list)

Magit/Forge work because downstream code attaches at *named* points. beads.el
currently has almost none. This section is the concrete list: each seam names
the symbol, its kind, its purpose, and the gascity.el (or other) downstream
use it enables. All names are `beads-` / `beads--`; all internal-only symbols
are marked `--`.

### 4.1 Store scoping and resolution

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-store-resolve` | function | directory (or explicit store) → store descriptor; already exists (dashboard) | gascity binds/forwards a rig directory |
| `beads-store-resolvers` | defvar hook | list of functions consulted when `beads-store-resolve` cannot resolve from a directory | gascity maps a bead id prefix → rig store; remote cities |
| `beads-store-descriptor` | EIEIO class | `{root, database, remote, label, prefixes}` | uniform scoping everywhere |
| `beads-store-directory` | buffer-local var | already exists; the explicit-store scoping var | retained as the scoping ABI |
| `beads-issue-id-prefixes` | defvar | known id prefixes (exists) | gascity adds gc prefixes |
| `beads-store-prefix-functions` | defvar hook | prefix → owning store | gascity routes `gc bd` stores by prefix |

The rule: a view resolves its store from `:directory` (buffer-local
`beads-store-directory`) or `default-directory`, and never guesses. This is
the interface gascity already uses (binding `default-directory` or passing
`:directory`); the seam makes it explicit and lets gascity contribute
resolvers.

### 4.2 Menu and command dispatch

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-menu-providers` | defvar hook | functions returning transient groups appended to the hand-built dispatch menu | gascity adds a "City" / "Rig" group |
| `beads-dispatch-menu` | transient prefix (hand-built) | the `?` dispatch backend | — |
| `beads-define-prefix` / `beads-define-group` | macros (exist) | directory-scoped prefix definitions | gascity builds on them (already does) |
| `beads--extract-option`, `beads--derive-transient-name` | functions (exist) | metadata helpers | gascity reuses (already does) |

### 4.3 Sections and dashboard composition

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-section-register` | function | register a named section (key, title, loader, renderer, keys) | gascity registers rig/agent sections |
| `beads-status-sections-hook` | defvar hook (exist) | sections of the status buffer | preserved for downstream |
| `beads-dashboard-section-providers` | defvar hook | extra vui sections on the board | gascity adds city pulse |
| `beads-section-mode` | major mode (exist) | vui section base | gascity derives from it (already does) |
| `beads-section` text property | contract (exist) | identity for RET/eldoc/actions | documented ABI |

### 4.4 At-point actions

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-action-providers` | defvar hook | `(context . actions)` contributed to the action bar + `?` | gascity adds sling/agent/drain actions |
| `beads-actions-*` | functions (exist) | close/claim/status/priority | retained; documented |
| `beads-after-action-functions` | defvar hook | run after a mutation (refresh fan-out) | gascity refreshes rig views |

### 4.5 Sling

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-sling-target` | EIEIO class | `{name, kind, scope, backend, description, metadata}` | gascity's gc targets are instances |
| `beads-sling-target-functions` | defvar hook | functions returning target lists | gascity contributes city/rig agents |
| `beads-sling-targets` | function | collect + dedupe/annotate targets | shared by sling and agent launch |
| `beads-sling-dispatch` | cl-defgeneric | `(target bead prompt)` → launch | gascity overrides for gc targets (`gc sling`) |
| `beads-sling-shape` | function | infer plain/`--on`/`--formula` from work+formula | pure, testable |
| `beads-sling-validators` | defvar hook | pre-launch validation functions | gascity adds the bl-bdj/cross-store warnings |
| `beads-sling-backend` | EIEIO class | a named dispatch backend (default local; `gc` from gascity) | gascity registers `gc` |

Default backend (`beads-sling-dispatch` for local targets) starts a local
agent on the bead using the existing agent subsystem. With gascity.el
present, `beads-sling-target-functions` supplies gc agents whose
`beads-sling-target-backend` is `gc`, and the `gc` backend method calls
`gc sling`.

### 4.6 Agent subsystem (existing registries, now documented as ABI)

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-agent-type` / `beads-agent-type-register` | class + registry (exist) | role roster | gascity roles are types; roster slimming keeps this open |
| `beads-agent-backend` / `beads-agent-backend-register` | class + registry (exist) | backend roster | gascity registers gc-backed backends |
| `beads-agent-backend-start` | cl-defgeneric, 4-arity (exist) | `(backend issue system-prompt user-prompt)` | documented ABI; gascity must not break it |
| `beads-agent-state-change-hook` | hook (exist) | session lifecycle fan-out | gascity observes sessions |

### 4.7 Formulas

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-formula-launch` | cl-defgeneric | `(formula bead &optional vars)` → launch, follow | gascity overrides for `gc sling --formula/--on` |
| `beads-formula-launch-context` | EIEIO class | resolved launch (shape, vars, target, warnings) | shared with sling |
| `beads-formula-var` | EIEIO class | declared var + reader metadata | typed How-generated readers |

### 4.8 Terminal

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-terminal` classes + registry (exist) | EIEIO + registry | render backends (vterm/eat/ghostel/term/auto) | retained |
| `beads-terminal-spawn` (exist) | cl-defgeneric | spawn argv into a buffer | retained ABI |
| `beads-terminal-attach` | cl-defgeneric | attach to a named agent/session in its backend | gascity supplies session + socket |
| `beads-terminal-tmux-*` | functions (new, moved) | tmux probes, attach argv/script, status mirror, mouse, scroll | gascity becomes a thin caller |

### 4.9 Faces

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-face-*` | defface set | the palette (status/priority/agent/section/header) | extensions derive with `:inherit` |
| `beads-faces-hook` | — (not used) | faces are extended by name, not by hook | document the naming contract instead |

Decision: no face hook. Extensions define derived faces
(`defface my-rig-face ((t (:inherit beads-face-header)))`) and reference the
stable `beads-face-*` names. The design names the exact face symbols in
`menu-mockups.md` §Rendering rules so gascity can match.

### 4.10 Keymaps

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-mode-extension-map` | keymap (new) | reserved `C-c b` prefix merged into all beads modes | gascity adds `C-c b c` (city), etc. |
| `beads-list-mode-map`, `beads-show-mode-map`, `beads-dashboard-mode-map`, `beads-section-mode-map` | keymaps (exist) | per-view bindings | documented; extensions read, not redefine |

### 4.11 Async reader

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-command-execute-async` (exist) | cl-defgeneric | non-blocking with callbacks, queue/cache-key | retained ABI |
| `beads-command-async-max-concurrent` (exist) | defcustom | concurrency cap | shared |

### 4.12 Remote/TRAMP

| Symbol | Kind | Purpose | Downstream use |
|---|---|---|---|
| `beads-remote-*` (exist) | functions | executable resolution, PATH fragment, ssh argv | retained; extended per §6.3 |
| `beads-buffer-name` helpers (exist) | functions | host-qualified buffer names | retained |
| `beads-remote-localize-path` | function (new, moved) | host path → view's TRAMP path | terminal/agent targets |
| `beads-remote-terminfo-p` | function (new, moved) | terminfo presence on host | terminal probing |

**Reserved extension key:** `C-c b`. beads.el owns `C-c b`, `C-c b ?`;
downstream packages use `C-c b <letter>` via `beads-mode-extension-map`.
(Existing `C-c b` usage in gascity's attach map — "bead at point" — becomes
the canonical `C-c b b` and the old binding is kept as an alias for one
release.)

---

## 5. Module / architecture target layout

All symbols prefixed `beads-`; internal `beads--`. One `beads-command-<name>.el`
per bd subcommand is unchanged. New/changed modules:

**Entry & porcelain**
- `beads.el` — entry: `M-x beads` opens the status buffer (the transient
  prefix is renamed `beads-dispatch`, bound to `?`; see `plan-review.md`
  F2); dispatch menu defs; core utilities.
- `beads-status.el` — **now the real status buffer** (today a deprecation
  shim). `beads-status` = the Magit-like front door; `beads-dashboard`
  remains the full board.
- `beads-menu.el` (new) — hand-built `beads-dispatch` and
  `beads-maintenance` menus, `beads-menu-providers` composition.
- `beads-ops-menu.el`, `beads-advanced-menu.el`, `beads-more-menu` —
  **removed**; their genuine entries fold into `beads-menu.el` (see
  `slimming.md`).

**List & detail**
- `beads-command-list.el` — list mode redesigned (sections, header, filters,
  actions); `/` filter transient retained.
- `beads-command-show.el` — sectioned detail redesign.
- `beads-section.el` — `beads-section-register`, section primitives, the
  `beads-section` text-property contract.
- `beads-thing.el` — unchanged movement ABI.
- `beads-actions.el` — context actions; `beads-action-providers`.

**Sling, agents, formulas**
- `beads-sling.el` (new) — target abstraction, shape inference, adaptive
  transient, preview buffer, validators.
- `beads-formula.el` (new, split from `beads-command-formula.el`) — formula
  browser/detail/launch UI; the command classes stay in
  `beads-command-formula.el`.
- `beads-agent*.el` — launch flow redesign; roster slimmed; registries
  retained.

**Terminal & remote**
- `beads-terminal.el` — backends (unchanged), `beads-terminal-attach` added.
- `beads-terminal-tmux.el` (new) — moved from `gascity-terminal.el`.
- `beads-remote.el` — extended per §6.3.
- `beads-buffer.el` — host-qualified names (extended).

**Foundation (unchanged ABI)**
- `beads-command.el`, `beads-meta.el`, `beads-types.el`, `beads-prefix.el`,
  `beads-custom.el`, `beads-error.el`, `beads-git.el`, `beads-completion.el`,
  `beads-reader.el`, `beads-spec.el`, `beads-state.el`, `beads-eldoc.el`,
  `beads-agent-type.el`, `beads-agent-backend.el`, `beads-terminal.el`
  (backends), `beads-audit.el`.

Layout rule: **browse/act/menu are native modes; view/edit/interact are
vui**. Lists are tabulated; boards/details are vui.

---

## 6. Code-movement plan (gascity.el → beads.el)

### 6.1 What moves: the terminal

`gascity.el/lisp/gascity-terminal.el` (≈1500 lines) owns, on top of
`beads-terminal.el`:

- tmux probes: `gascity-terminal-tmux-session-exists-p`,
  `gascity-terminal-pane-cwd`, `gascity-terminal--tmux`,
  `gascity-terminal--run-async`, `gascity-terminal--sh`,
  `gascity-terminal--tmux-sh`, `gascity-terminal--host-argv`.
- attach construction: `gascity-terminal--attach-argv`,
  `gascity-terminal--attach-script`, `gascity-terminal-attach-tmux`,
  `gascity-terminal--attach-finish`, `gascity-terminal--term-to-probe`.
- status mirror: `gascity-terminal--status-*` (install/refresh/string/
  segment/script/teardown/tick).
- mouse: `gascity-terminal--mouse-*`, `gascity-terminal--arm-wheel`, the
  `WheelDownPane` binding fragment.
- scroll mode: `gascity-terminal-scroll-mode`, `-scroll-key`, `-toggle`,
  `-wheel`, `-scroll-sequence`, `-scroll-backend`, `-send-raw`, the
  per-backend raw-key adapter (`ghostel`/`vterm`/`term`/`eat`).
- backend selection/preload: `gascity-terminal--backend-class`,
  `-client-term`, `-remote-term`, `-working-dir`, `-live-buffer`,
  `-backend-loaded-p`, `-preload-backend`, `-preload-when-idle`,
  `-arm-preload`, `-schedule-preload`, `gascity-terminal-run`.
- beads integration: `gascity-terminal--beads-integrate`,
  `gascity-terminal--project-root`.

**Target:** `beads.el/lisp/beads-terminal-tmux.el`, symbols renamed
`gascity-terminal-*` → `beads-terminal-tmux-*` (or `beads-terminal-*` where
the name is generic: `beads-terminal-attach`, `beads-terminal-run`). The
beads-integration helpers move to `beads-terminal.el`.

**beads.el stays free of gascity:** the moved code must not reference
`gascity-*`; the only gascity-specific parameter is the tmux **socket**,
passed in via `beads-terminal-attach`'s `:socket` argument.

### 6.2 What stays in gascity.el

- `gascity-tmux-socket` resolution (city-name inference + override).
- `gascity-context-*` (city/rig resolution) and everything gascity-domain.
- The `gascity-*` command classes and views.
- A **thin compatibility shim** `gascity-terminal.el` that keeps the old
  entry points working: `gascity-terminal-attach-tmux`,
  `gascity-terminal-run`, and the scroll-mode symbol are `defalias`ed or
  defined as wrappers onto the `beads-terminal-tmux-*` functions, resolving
  the socket and passing it. This keeps existing gascity.el callers and
  user configs green during and after the move.

### 6.3 Remote helpers that must move or be added to beads

The terminal code uses remote helpers currently in `gascity-remote.el`:

| Needed | beads.el status | Action |
|---|---|---|
| ssh argv | `beads-remote-ssh-argv` exists | reuse |
| PATH assignment | `beads-remote-path-assignment` exists | reuse |
| executable resolution | `beads-remote-find-executable` exists | reuse |
| `gascity-remote-localize-path` | missing | add `beads-remote-localize-path` |
| `gascity-remote-terminfo-p` | missing | add `beads-remote-terminfo-p` |
| `gascity-remote-buffer-name` | equivalent in `beads-buffer.el` | reuse/extend |
| `gascity-remote-prewarm` | missing | add `beads-remote-prewarm` (optional; terminal preload) |

No gascity-specific logic (city context, result reconciliation) moves.

### 6.4 Migration order (safe, shim-first)

1. Add `beads-terminal-attach` + `beads-terminal-tmux.el` in beads.el,
   porting the tmux/status/mouse/scroll code against beads-only dependencies;
   port the gascity terminal tests to `beads-terminal-tmux-test.el`.
2. Add the missing `beads-remote-localize-path` / `-terminfo-p` helpers.
3. Make gascity.el's `gascity-terminal.el` a shim delegating to the beads
   implementation; keep `gascity-tmux-socket` resolution in gascity.
4. Verify all three terminal surfaces (attach, status mirror, scroll) over
   TRAMP against bright-lights, from both packages.
5. Delete the duplicated implementation from gascity.el once green; keep the
   shim until a deprecation window passes.

### 6.5 Other de-duplication

- `beads-terminal.el` already owns backend selection; the moved code must use
  it (it mostly does).
- The tmux scroll/mouse work is documented in
  `gascity.el/docs/DESIGN-agent-scrolling.md`; that document becomes a
  beads.el doc (`docs/terminal-scrolling.md`) with the moved code, and the
  gascity doc links to it.

---

## 7. Standalone sling design (no gascity)

The sling abstraction is deliberately tiny so it works with only `bd` + local
agents.

```
beads-sling-target           ; EIEIO: name kind scope backend description metadata
beads-sling-targets()        ; collect from beads-sling-target-functions
  default provider:
    - one target per available agent backend   (kind 'agent)
    - one target per existing worktree         (kind 'worktree)
    - one target per role for the project       (kind 'role)
beads-sling-shape(work formula) ; plain | on | formula  (pure)
beads-sling-dispatch(target bead prompt)        ; cl-defgeneric
  default method (local): beats-agent start on BEAD in TARGET's worktree
beads-sling-validators        ; hook: functions (ctx) -> warnings
beads-sling--transient        ; adaptive staged UI (mockup §4)
beads-sling--preview          ; special-mode preview buffer (mockup §4b)
```

Work-item shape (the `bd` work) is the same in both packages: a bead id or
freeform text. Formula shape (`--on` / `--formula`) is inferred, not flagged
(REQ-009, mirroring the approved gascity sling design). The formula **launch**
backend is a generic: local ironing; gascity overrides with `gc sling`.

---

## 8. Consistency rules (REQ-017…REQ-019)

- **Faces.** One palette, stable names: `beads-face-header`,
  `beads-face-section`, `beads-face-issue-line`, `beads-face-status-*`,
  `beads-face-priority-*`, `beads-face-agent-*`, `beads-face-id`,
  `beads-face-key`. Extensions derive with `:inherit`.
- **Glyphs.** One set: status markers, priority bars, agent state glyphs
  (`beads-agent-display-*` today) reused everywhere.
- **Mode line.** Each porcelain buffer shows `[store] · counts · filter ·
  agent state`; the format is a single function per view, not ad hoc.
- **Async.** Every loader goes through `beads-command-execute-async`; no sync
  `bd` call at render time (the remote render guard test stays green).
- **Remote.** Buffer identity host-qualified via `beads-buffer.el`; explicit
  `:directory` scoping does no wrong-side I/O; `default-directory` is pinned
  at open.

---

## 9. Decomposition and implementation

See `decomposition.md` for the work items with dependencies and REQ trace,
and `implementation-plan.md` for the ordered, **pruning-first** plan. See
`slimming.md` for the removal audit and `menu-mockups.md` for every rendered
surface. `plan-review.md` records the critique round.

---

## 10. Risks and mitigations

| Risk | Mitigation |
|---|---|
| Auto-generated transients are load-bearing in tests | Keep them as dispatch backends; tests port to the hand-built surface; a compatibility alias keeps `M-x beads-<cmd>` working |
| Role-roster cut removes a real workflow (QA/Custom) | Fold QA into Review (mode); keep Custom as the freeform sling escape; registries unchanged so users can re-register |
| Terminal move creates a beads↔gascity cycle | beads owns the implementation; gascity keeps only the socket + shim; no `gascity-` reference in beads |
| TRAMP regressions in the moved terminal | Move only after a shim; verify attach/status/scroll over `/ssh:localhost:~/bright-lights` |
| Section/`vui` churn breaks the render guard | All loaders async; `lisp/test/beads-render-guard-test.el` remains the gate |
| Reserving `C-c b` collides with existing configs | Keep old binding as an alias for one release; document the change in `NEWS.md` |
| Slimming too aggressive for existing users | Each removal ships a one-release alias + a `NEWS.md` entry; `slimming.md` marks the conservative fallback |

## 11. References

- `gascity.el/docs/DESIGN.md` — UI direction (binding).
- `gascity.el/plans/sling-command/*` — process/artifact shape to mirror.
- `gascity.el/docs/DESIGN-agent-scrolling.md` — the terminal scrolling design
  that moves into beads.el.
- `gascity.el/lisp/gascity-terminal.el` — the code to move (§6).
- `lisp/beads-prefix.el`, `beads-meta.el` (`beads-meta-parity-*`),
  `beads-section.el`, `beads-thing.el`, `beads-command-list.el`,
  `beads-command-show.el`, `beads-dashboard.el`, `beads-agent*.el`,
  `beads-command-formula.el`, `beads-terminal.el`, `beads-remote.el`,
  `beads-buffer.el`.
- `AGENTS.md` — build/test/remote-testing conventions; `MAGIT_PATTERNS.md`.
