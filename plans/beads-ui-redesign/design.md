---
schema: beads.ui-redesign.design.v1
workflow:
  id: be-mv8d
  predecessor: be-59fe
artifact: design
status: draft-for-review
scope: planning-only
refinement: 2
decisions_folded: [F2, F3]
---

# beads.el UI Redesign — Design

*A world-class, Magit-style Emacs porcelain for the `bd` bead store.*

Status: **plan / design for review** (2026-10-01, refinement 2). Supersedes
the auto-generated-transient framing of the current `beads` menu and the
shallower round-1 `design.md` (be-59fe). Implementation is out of scope until
this plan and `menu-mockups.md` are reviewed and signed off. Mirrors the
process and artifact shape of `gascity.el/plans/sling-command/` and the
binding "UI direction" in `gascity.el/docs/DESIGN.md` §1.

**Refinement 2 folds two user decisions** (documented in
`plan-review.md`):

- **F2** — `M-x beads` opens the **status buffer**; the old transient prefix is
  renamed **`beads-dispatch`** and bound to **`?`** (magit status/dispatch
  split). `beads-dashboard` remains the full board. One-release compatibility
  keeps the old transient content available under the new name.
- **F3** — **remove QA and Custom entirely** (deeper than round 1, which only
  proposed hiding them). `beads-agent-type-qa` and `beads-agent-type-custom`
  and everything that only exists to serve them are **deleted**; QA folds into
  **Review as a QA mode** (its testing prompt is kept); Custom's freeform
  prompt moves to **sling's freeform path**. The freed `q` and `c` keys under
  the agent prefix are released. The registries themselves stay open, but no
  hidden/deferred QA or Custom class is retained.

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

### 1.1 View-technology decision matrix

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

## 3. The one command surface (navigation + keymap contract)

This is the full keyboard contract. It is deliberately one table, not a
per-view list, because REQ-002/REQ-018 demand that movement never varies.

### 3.1 Reserved keys

```
q     bury buffer            g     refresh in place
C-u g hard refresh (drop caches)
TAB   move to next thing     S-TAB move to previous thing
SPC   toggle thing/section   RET   visit / activate at point
n p   next/previous item     N P   next/previous section
?     dispatch menu          /     filter (list surfaces only)
```

Context action keys (`d C s # a S e m c` and friends) are documented per
view in `menu-mockups.md`; they are stable across views where the action
makes sense (REQ-007). Extension keys live under the reserved prefix
`C-c b` (§4.12) and may not shadow a reserved key.

### 3.2 Keymap inheritance chain

The porcelain uses **map inheritance**, not key duplication:

```
special-mode-map
  ├─ beads-list-mode-map            (tabulated-list-mode-map parent)
  ├─ beads-formula-list-mode-map    (tabulated-list-mode-map parent)
  ├─ beads-agent-list-mode-map      (tabulated-list-mode-map parent)
  ├─ beads-show-mode-map            (special-mode parent; vui sections)
  └─ beads-sling-preview-mode-map   (special-mode parent)

vui-mode-map
  └─ beads-section-mode-map
        ├─ beads-status-mode-map     (NEW; the status buffer)
        └─ beads-dashboard-mode-map  (existing)
```

Every one of these maps is merged with `beads-mode-extension-map` (the
`C-c b` prefix) at mode definition time via a helper
`beads-mode--install-extension-map`. `beads-thing-define-keys` is called once
per interactive map; there is no view-local movement.

### 3.3 Per-view binding table (canonical keys)

| Key | list | show | status | dashboard | formula-list | formula-detail | agent-list |
|---|---|---|---|---|---|---|---|
| `q` | bury | bury | bury | bury | bury | bury | bury |
| `g` | refresh | refresh | refresh | refresh | refresh | refresh | refresh |
| `C-u g` | hard | hard | hard | hard | hard | hard | hard |
| `TAB`/`S-TAB` | thing | thing | thing | thing | item | thing | item |
| `SPC` | fold | fold | fold | fold | — | fold | — |
| `RET` | visit | visit | visit | visit | inspect | visit ref | attach |
| `n`/`p` | item | section | item | item | item | section | item |
| `N`/`P` | section | section | section | section | — | section | — |
| `?` | dispatch | dispatch | dispatch | dispatch | dispatch | dispatch | dispatch |
| `/` | filter | — | — | — | filter | — | — |
| `m` | mark | — | — | mark | — | — | — |
| `d` | close | close | close | close | — | — | — |
| `C` | claim | claim | claim | claim | — | — | — |
| `s` | status | status | status | status | launch | launch | — |
| `#` | priority | priority | priority | priority | — | — | — |
| `a` | agent-prefix | agent-prefix | agent-prefix | agent-prefix | — | — | — |
| `S` | sling | sling | sling | sling | — | — | — |
| `e` | edit | edit-field | — | — | — | — | — |
| `c` | create | comment | create | create | convert | — | — |
| `x` | — | — | — | — | — | — | stop |
| `j` | — | jump | — | jump | — | — | jump |

`beads-mode-extension-map` provides `C-c b` for all rows.

### 3.4 Key-flow contract (what `?` does)

`?` opens the **same** `beads-dispatch` transient everywhere; the transient
builds its "context actions" group from `beads-action-providers` keyed on the
current `beads-section` / buffer mode. So a user learns one menu, and each
view only changes the small context group. `beads-dispatch` never renders
content (anti-pattern).

### 3.5 The F2 entry split

- `M-x beads` → `beads-status` (new): the Magit-like status buffer.
- `M-x beads-dispatch` → the hand-built transient (was `beads`).
- `?` in every porcelain buffer → `beads-dispatch`.
- `beads-dashboard` → the full board (superset), unchanged name and
  `:directory` scoping.
- `M-x beads-status` continues to open the status buffer (it is now the real
  implementation, no longer a `make-obsolete` shim). The old shim behaviour
  (forward to `beads-dashboard`) is dropped at the same time as the F2 rename
  lands, because `beads-status` is now the front door.

A one-release alias `beads` → the old transient content is intentionally
**not** kept: `beads` must be the status entry. The transient content is
preserved verbatim as `beads-dispatch`, so nothing is lost; `NEWS.md` records
the rename.

---

## 4. The magit/forge extension model (concrete seam list, with signatures)

Magit/Forge work because downstream code attaches at *named* points. beads.el
currently has almost none. This section is the concrete list: each seam names
the symbol, its kind, its **signature**, its file, its purpose, and the
gascity.el (or other) downstream use it enables. All names are `beads-` /
`beads--`; internal-only symbols are marked `--`. A signature of `HOOK` means
the variable is a hook whose functions are called as documented in the
purpose cell.

### 4.1 Store scoping and resolution

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-store-resolve` | defun | `(directory) → dir-or-nil` | `beads-util.el` | normalize an explicit store dir (exists) | gascity forwards a rig dir |
| `beads-store-project-root` | defun | `(store) → root` | `beads-util.el` | root of an explicit store (exists) | gascity scoping |
| `beads-store-resolvers` | defvar hook | `HOOK`: each `(fn dir) → dir-or-nil` | `beads-util.el` (new) | consulted, in order, when a directory cannot resolve to a store | gascity maps bead-prefix → rig store; remote cities |
| `beads-store-prefix-functions` | defvar hook | `HOOK`: each `(fn prefix) → store-or-nil` | `beads-util.el` (new) | map a bead id prefix to its owning store | gascity routes `gc bd` stores by prefix |
| `beads-store-descriptor` | EIEIO class | slots `{root database remote label prefixes}` | `beads-util.el` (new) | uniform scoping value object | gascity describes a rig store |
| `beads-store-directory` | buffer-local var | `dir-or-nil` | `beads-util.el` | the scoping ABI (exists) | retained |
| `beads-issue-id-prefixes` | defcustom | `list-of-string-or-nil` | `beads-util.el` | recognized prefixes (exists) | gascity adds `gc` prefixes |

Rule: a view resolves its store from `:directory` (buffer-local
`beads-store-directory`) or `default-directory`, and never guesses. A resolver
may only translate a directory or prefix to a store; it does no I/O beyond
what `beads-store-resolve` already does.

### 4.2 Menu and command dispatch

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-menu-providers` | defvar hook | `HOOK`: each `(fn) → list-of-transient-group` | `beads-menu.el` (new) | groups appended to the hand-built dispatch menu | gascity appends a `[City]` group |
| `beads-dispatch` | transient prefix | hand-built | `beads-menu.el` (new) | the `?` dispatch backend | — |
| `beads-maintenance` | transient prefix | hand-built | `beads-menu.el` (new) | the `!` maintenance menu | — |
| `beads-define-prefix` / `beads-define-group` | defmacro | `(name arglist &rest groups)` | `beads-prefix.el` | directory-scoped prefixes (exist) | gascity builds on them (already does) |
| `beads--extract-option`, `beads--derive-transient-name` | defun | metadata helpers (exist) | `beads-meta.el` | reuse (existing) | gascity reuses (already does) |

Provider contract: `beads-menu-providers` functions take no arguments and
return a list of transient group vectors acceptable to
`transient-define-prefix`; providers must be side-effect-free (they run on
every `?`). An empty list is the standalone no-op (REQ-021).

### 4.3 Sections and dashboard composition

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-section-register` | defun | `(key title loader renderer &optional keys) → symbol` | `beads-section.el` (new) | register a named async/renderable section | gascity registers rig/agent sections |
| `beads-section-spec` | EIEIO class | slots `{key title loader renderer keys order}` | `beads-section.el` (new) | the section descriptor | shared by status/dashboard/formula |
| `beads-status-sections-hook` | defvar hook | `HOOK`: each `(fn) → vnode-or-nil` | `beads-section.el` | status-buffer sections (exists) | preserved |
| `beads-dashboard-section-providers` | defvar hook | `HOOK`: each `(fn) → list-of-section-spec` | `beads-dashboard-sections.el` (new) | extra board sections | gascity adds a city pulse |
| `beads-section-mode` | major mode | derived from `vui-mode` (exists) | `beads-section.el` | vui section base | gascity derives (already does) |
| `beads-section` text property | contract | stamped by `beads-section--propertize` | `beads-section.el` | identity for RET/eldoc/actions (exists) | documented ABI |

### 4.4 At-point actions

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-action-providers` | defvar hook | `HOOK`: each `(fn context) → (KEY . ACTION) list` | `beads-actions.el` (new) | actions contributed to the action bar and `?` context group | gascity adds drain/nudge/sling actions |
| `beads-actions-close` / `-claim` / `-set-status` / `-set-priority` | commands | `(&optional issue)` (exist) | `beads-actions.el` | canonical mutations | retained; documented |
| `beads-after-action-functions` | defvar hook | `HOOK`: each `(fn action issues)` | `beads-actions.el` (new) | refresh fan-out after a mutation | gascity refreshes rig views |
| `beads-actions-context` | defun | `() → (CONTEXT . ISSUES)` | `beads-actions.el` (new) | resolve context at point/marks | shared by views and providers |

Context is a keyword: `:list`, `:show`, `:status`, `:dashboard`,
`:formula-list`, `:agent-list`; actions receive the resolved issue list so a
provider need not know the buffer.

### 4.5 Sling

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-sling-target` | EIEIO class | slots `{name kind scope backend description metadata}` | `beads-sling.el` (new) | a dispatchable target | gascity's gc targets are instances |
| `beads-sling-target-functions` | defvar hook | `HOOK`: each `(fn) → list-of-target` | `beads-sling.el` (new) | target discovery | gascity contributes city/rig agents |
| `beads-sling-targets` | defun | `(&optional bead) → list-of-target` | `beads-sling.el` (new) | collect, dedupe, annotate | shared by sling and agent launch |
| `beads-sling-shape` | defun | `(work formula) → (plain \| on \| formula)` | `beads-sling.el` (new) | pure inference | tested table-driven |
| `beads-sling-dispatch` | cl-defgeneric | `(target bead prompt) → session` | `beads-sling.el` (new) | launch | gascity overrides for gc targets |
| `beads-sling-validators` | defvar hook | `HOOK`: each `(fn context) → warning-string-or-nil` | `beads-sling.el` (new) | pre-launch validation | gascity adds cross-store/`bl-bdj` warnings |
| `beads-sling-backend` | EIEIO class | slots `{name dispatch description}` | `beads-sling.el` (new) | named dispatch backend | gascity registers `gc` |
| `beads-sling-backend-register` | defun | `(backend) → backend` | `beads-sling.el` (new) | backend registry | gascity registers `gc` |

Default backend (`beads-sling-dispatch` for local targets) starts a local
agent on the bead using the existing agent subsystem. With gascity.el
present, `beads-sling-target-functions` supplies gc agents whose
`beads-sling-target-backend` is `gc`, and the `gc` backend method calls
`gc sling`.

### 4.6 Agent subsystem (existing registries, now documented as ABI)

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-agent-type` / `beads-agent-type-register` | class + defun | `(type) → type` (exist) | `beads-agent-type.el` | role roster | gascity roles are types; F3 deletions do not close this |
| `beads-agent-type-get` | defun | `(name) → type-or-nil` | `beads-agent-type.el` | lookup by name | gascity resolves roles |
| `beads-agent-type-build-user-prompt` | cl-defgeneric | `(type issue) → string` (exist) | `beads-agent-type.el` | user envelope | documented ABI |
| `beads-agent-type-system-prompt` | cl-defgeneric | `(type issue) → string` (exist) | `beads-agent-type.el` | role prompt | documented ABI |
| `beads-agent-backend` / `beads-agent-backend-register` | class + defun | `(backend) → backend` (exist) | `beads-agent-backend.el` | backend roster | gascity registers gc-backed backends |
| `beads-agent-backend-start` | cl-defgeneric | `(backend issue system-prompt user-prompt) → (session . buffer)` (exist) | `beads-agent-backend.el` | launch | documented ABI |
| `beads-agent-state-change-hook` | defvar hook | `HOOK`: each `(fn action session)` (exist) | `beads-agent-backend.el` | lifecycle fan-out | gascity observes sessions |

### 4.7 Formulas

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-formula-launch` | cl-defgeneric | `(formula bead &optional vars) → session` | `beads-formula.el` (new) | launch + follow | gascity overrides for `gc sling --formula/--on` |
| `beads-formula-launch-context` | EIEIO class | slots `{shape vars target warnings}` | `beads-formula.el` (new) | resolved launch | shared with sling |
| `beads-formula-var` | EIEIO class | slots `{name required type description default}` | `beads-formula.el` (new) | typed How readers | shared with sling How stage |
| `beads-formula-var-reader` | cl-defgeneric | `(var) → reader-spec` | `beads-formula.el` (new) | type → transient infix kind | gascity may add readers |

### 4.8 Terminal

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-terminal` classes + registry | EIEIO + defun | `(backend) → backend` (exist) | `beads-terminal.el` | render backends | retained |
| `beads-terminal-spawn` | cl-defgeneric | `(terminal argv &optional name) → buffer` (exist) | `beads-terminal.el` | spawn argv | retained ABI |
| `beads-terminal-attach` | cl-defgeneric | `(terminal target &optional socket) → buffer` | `beads-terminal.el` (new) | attach to a named agent/session | gascity supplies session + socket |
| `beads-terminal-tmux-*` | functions | (moved) | `beads-terminal-tmux.el` (new) | tmux probes, attach argv/script, status mirror, mouse, scroll | gascity becomes a thin caller |

### 4.9 Faces

| Symbol | Kind | File | Purpose | Downstream use |
|---|---|---|---|---|
| `beads-face-*` | defface set | `beads-section.el` / new `beads-faces.el` | the palette | extensions derive with `:inherit` |

Decision: **no face hook.** Extensions define derived faces
(`(defface my-rig-face ((t (:inherit beads-face-header))))`). The design names
the exact face symbols in `menu-mockups.md` §12 so gascity can match them.
Full list: `beads-face-header`, `beads-face-section`, `beads-face-issue-line`,
`beads-face-id`, `beads-face-key`, `beads-face-status-{open,in-progress,blocked,closed}`,
`beads-face-priority-{critical,high,medium,low}`, `beads-face-agent-{running,idle,failed}`,
`beads-face-success`, `beads-face-warning`, `beads-face-error`.

### 4.10 Keymaps

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-mode-extension-map` | keymap | `C-c b` prefix | `beads-buffer.el` (new) | reserved extension prefix merged into every beads map | gascity adds `C-c b c` (city), etc. |
| `beads-mode--install-extension-map` | defun | `(map) → map` | `beads-buffer.el` (new) | install the prefix via `:parent`/`define-key` | called from every major mode |
| `beads-list-mode-map`, `beads-show-mode-map`, `beads-dashboard-mode-map`, `beads-section-mode-map` | keymaps | (exist) | respective files | per-view bindings | documented; extensions read, not redefine |

Reserved extension key: `C-c b`. beads.el owns `C-c b` and `C-c b ?`;
downstream packages use `C-c b <letter>` via `beads-mode-extension-map`.
(Existing `C-c b` usage in gascity's attach map — "bead at point" — becomes
the canonical `C-c b b`, and the old binding is kept as an alias for one
release.)

### 4.11 Async reader

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-command-execute-async` | cl-defgeneric | `(command on-success &optional on-error &rest kwargs)` (exist); kwargs `:queue`, `:cache-key`, `:timeout` | `beads-command.el` | non-blocking with callbacks | retained ABI |
| `beads-command-async-max-concurrent` | defcustom | integer, `auto`, or `unlimited` (exist) | `beads-command.el` | concurrency cap | shared |

### 4.12 Remote/TRAMP

| Symbol | Kind | Signature | File | Purpose | Downstream use |
|---|---|---|---|---|---|
| `beads-remote-ssh-argv`, `beads-remote-path-assignment`, `beads-remote-find-executable` | defuns | (exist) | `beads-remote.el` | executable resolution, PATH fragment, ssh argv | retained |
| `beads-remote-ssh-command`, `beads-remote-ssh-call`, `beads-remote-ssh-find-up` | defuns | (exist) | `beads-remote.el` | local ssh pipe transport | retained |
| `beads-remote-localize-path` | defun | `(path) → tramp-path` (new, moved) | `beads-remote.el` | host path → view's TRAMP path | terminal/agent targets |
| `beads-remote-terminfo-p` | defun | `(dir &optional term) → bool` (new, moved) | `beads-remote.el` | terminfo presence on host | terminal probing |
| `beads-remote-prewarm` | defun | `(dir) → nil` (new, moved, optional) | `beads-remote.el` | pre-warm the remote ssh pipe | terminal preload |
| `beads-buffer-*` helpers | defuns | (exist) | `beads-buffer.el` | host-qualified buffer names | retained |

---

## 5. Module map (new / changed / deleted / unchanged, and ownership)

One `beads-command-<name>.el` per bd subcommand is unchanged. The map below is
the target layout after implementation; "owns" states the single
responsibility.

### 5.1 New modules

| Module | Owns |
|---|---|
| `beads-menu.el` | The hand-built `beads-dispatch` (`?`) and `beads-maintenance` (`!`) transients; `beads-menu-providers` composition. No command classes. |
| `beads-sling.el` | The `beads-sling-target` / `beads-sling-backend` classes, discovery, `beads-sling-shape`, the adaptive transient, the preview buffer, validators, the default local `beads-sling-dispatch` method. |
| `beads-formula.el` | The formula browser/detail/launch/follow **UI**; `beads-formula-launch`, `beads-formula-var`, typed readers. The command classes stay in `beads-command-formula.el`. |
| `beads-terminal-tmux.el` | The moved tmux probes, attach construction, status mirror, mouse, scroll mode + raw-key adapters, backend selection helpers, `beads-terminal-run`. |
| `beads-faces.el` (optional split) | The `beads-face-*` palette; may stay in `beads-section.el`, but the names are the contract either way. |

### 5.2 Changed modules

| Module | Change |
|---|---|
| `beads.el` | `M-x beads` → `beads-status`. The transient prefix moves out to `beads-menu.el` as `beads-dispatch`. Core utilities stay. |
| `beads-status.el` | **Now the real status buffer** (was a `make-obsolete` shim). `beads-status` opens the vui board; `beads-dashboard` is the full-board alias. |
| `beads-command-list.el` | Sectioned list redesign; header/filter/actions; `/` filter transient retained; `beads-list-mode-map` gains the universal contract; `beads-action-providers`. |
| `beads-command-show.el` | Sectioned detail redesign; action bar; breadcrumbs; `RET` on refs; agent section attach. |
| `beads-section.el` | Adds `beads-section-register`, `beads-section-spec`, generic section rendering; keeps `beads-section-mode` and the `beads-section` property contract. |
| `beads-actions.el` | Adds `beads-action-providers`, `beads-actions-context`, `beads-after-action-functions`; existing action commands retained. |
| `beads-agent.el` | Launch-flow redesign; `beads-agent-start-qa`/`-custom` **deleted**; Review gains a QA mode; Custom freeform moves to sling; start-menu collapse (M6). |
| `beads-agent-types.el` | `beads-agent-type-qa` and `beads-agent-type-custom` classes, their prompts, and `beads-agent-qa-backend` **deleted**; QA prompt/user-prompt retained as the Review QA mode's prompt; registration drops to 3 types. |
| `beads-agent-keys.el` | `a q` and `a c` bindings removed; freed keys reserved (not reused in this plan); single `beads-agent-prefix-map`. |
| `beads-terminal.el` | Adds `beads-terminal-attach`; backends unchanged; gamified `beads-terminal-run` (now from the tmux module) kept as a thin dispatcher. |
| `beads-remote.el` | Adds `beads-remote-localize-path`, `-terminfo-p`, `-prewarm` (moved from gascity). |
| `beads-buffer.el` | Adds `beads-mode-extension-map` + installer; host-qualified names extended if needed. |
| `beads-dashboard-sections.el` | Adds `beads-dashboard-section-providers`; shares `beads-section-spec` loaders with `beads-status.el`. |

### 5.3 Deleted modules / surfaces

| Module / surface | Disposition |
|---|---|
| `beads-ops-menu.el` | **Deleted**; genuine entries fold into `beads-menu.el` (M2). |
| `beads-advanced-menu.el` | **Deleted**; genuine entries fold into `beads-menu.el` (M3). |
| `beads-more-menu` (in `beads.el`) | **Deleted** (M1). |
| `beads-agent-type-qa`, `beads-agent-type-custom` | **Deleted** classes + prompts + `beads-agent-qa-backend` (F3). |
| `beads-agent-start-qa`, `beads-agent-start-custom` | **Deleted** commands (F3). |
| `a q`, `a c` bindings | **Deleted**; keys freed (F3). |
| `make-obsolete 'beads-status` shim | **Deleted**; `beads-status` is real (F2). |

### 5.4 Unchanged ABI (foundation)

`beads-command.el`, `beads-meta.el`, `beads-types.el`, `beads-prefix.el`,
`beads-custom.el`, `beads-error.el`, `beads-git.el`, `beads-completion.el`,
`beads-reader.el`, `beads-spec.el`, `beads-state.el`, `beads-eldoc.el`,
`beads-pager.el`, `beads-agent-type.el` (registry), `beads-agent-backend.el`
(registry), `beads-terminal.el` (backends), `beads-audit.el`.

Layout rule: **browse/act/menu are native modes; view/edit/interact are
vui**. Lists are tabulated; boards/details are vui.

---

## 6. Data / async model

Every read is a function of `bd … --json`; the model is uniform across views
(REQ-018, REQ-019).

### 6.1 Reader

- **One read path.** Views construct a `beads-command-*` object and call
  `beads-command-execute-async` (`command on-success &optional on-error &rest
  kwargs`). Sync execution exists only for first-contact root discovery
  (`beads--project-root` over the ssh pipe) and tests.
- **kwargs.** `:queue t` applies the `beads-command-async-max-concurrent` cap
  (FIFO); `:cache-key` coalesces identical in-flight requests (single-flight);
  `:timeout` bounds a request. The dashboard's per-section async key is the
  model for cache keys: `(list 'beads-dashboard-section async-key)`.
- **No sync I/O at render.** The render guard test
  (`lisp/test/beads-render-guard-test.el`) enforces this; it stays green.

### 6.2 Store and scoping

- A view resolves its store once at open: `beads-store-resolve` (explicit
  `:directory`) or `beads-store-directory`, else `default-directory`; the
  resolved directory is pinned buffer-local so later refreshes never
  re-resolve against a stray `default-directory`.
- The store descriptor (`{root database remote label prefixes}`) is the value
  the buffer carries; the mode line reads `[store]` from it.
- Resolvers/hooks (§4.1) contribute on top; standalone they are empty, so
  scoping degrades to the explicit dir / `bd` default.

### 6.3 Caching and refresh

- `g` refreshes in place without clearing process coalescing caches;
  `C-u g` (`beads-*-hard-refresh`) clears the store's caches first.
- Per-section state is preserved across refresh: fold state (persistent per
  store, via `beads-dashboard--save-visibility`), point/restore
  (`beads-dashboard--with-point-restore`), and loaded depth
  (`beads-dashboard-depth-*` / `load-more`).
- Negative caching is explicit where it pays (eldoc), never implicit in the
  reader.

### 6.4 Loading / empty / error states

Each async section has exactly three non-data states, rendered by the shared
helpers (`beads-dashboard--loading-line`, `-empty-line`, `-error-line`):

| State | Rendering | Recovery |
|---|---|---|
| loading | spinner line + section title, key still foldable | async result swaps in |
| empty | "No <X>" line, `beads-dashboard--data-empty-p` | refresh |
| error | `beads-dashboard--error-line` with the condition's message | `g` retries; error is section-local, other sections still render |

The whole board is **not** wrapped in a single error boundary; each section is
(`vui-error-boundary`), so one failing `bd` call never blanks the view.

### 6.5 Remote / TRAMP

- Buffer identity is host-qualified (`beads-buffer.el`); the same buffer name
  on two hosts is two buffers.
- A view opened with an explicit `:directory` does no wrong-side I/O; path
  localization for spawned processes uses `beads-remote-localize-path`.
- Async spawns on a single-hop ssh-family store run as a **local `ssh -T`
  pipe** (`beads-remote-ssh-command`), never a `tramp-sh` `make-process`.
- First contact with a remote host is the one synchronous exception
  (`beads-remote-ssh-find-up`, bounded by `beads-remote-sync-timeout`,
  remembered once per directory, negatives included).

---

## 7. Faces and design language (REQ-017)

- **One palette** with stable names (§4.9). Extensions derive via
  `:inherit`; no face hook.
- **One glyph set:** status `○ ◐ ⛔ ✓`; priority `P0…P4` (P0 red);
  agent state `🦅 … ✗` with letter fallbacks `T/R/P`.
- **One section header rule:** `▾ Title (n)` expanded / `▸ Title (n)`
  collapsed, a `beads-section` thing, with the count always present.
- **One mode-line rule:** each porcelain buffer renders
  `[store] · counts · filter · agent-state` from a single per-view format
  function (no ad-hoc `mode-line-format` edits).
- **One layout vocabulary:** header block, sections, action bar; nothing
  invents a second. `beads-section-spec` is the only section shape.

---

## 8. Standalone sling abstraction (REQ-008, REQ-009)

The sling abstraction is deliberately tiny so it works with only `bd` + local
agents.

```
;; Target/value objects
beads-sling-target       ; EIEIO: name kind scope backend description metadata
beads-sling-backend      ; EIEIO: name dispatch description

;; Discovery
beads-sling-targets(bead)            ; collect from beads-sling-target-functions
  default provider:
    - one target per available agent backend   (kind 'agent)
    - one target per existing worktree         (kind 'worktree)
    - one target per role for the project       (kind 'role)

;; Pure inference
beads-sling-shape(work formula) → plain | on | formula

;; Dispatch
beads-sling-dispatch(target bead prompt)   ; cl-defgeneric
  default method (local): beads-agent start on BEAD in TARGET's worktree
beads-sling-validators                     ; hook: (ctx) → warning|nil
beads-sling--transient                     ; adaptive staged UI (mockup §6)
beads-sling--preview                       ; special-mode preview (mockup §7)
```

Work-item shape (the `bd` work) is the same in both packages: a bead id or
freeform text. Formula shape (`--on` / `--formula`) is inferred, not flagged
(REQ-009, mirroring the approved gascity sling design). The formula **launch**
backend is a generic: local ironing; gascity overrides with `gc sling`.

### 8.1 Adaptive flow lessons (from `gascity.el/plans/sling-command/`)

Carried over verbatim as design constraints:

- **Stages collapse when pre-seeded**: What → Who → How → Preview → Launch →
  Follow. A fully pre-seeded dispatch is "press `S`, press `s`".
- **Shape is inferred and shown as one sentence**, never chosen by a flag and
  never toggled.
- **One smart picker per stage**, with a prefix-arg freeform escape where the
  stage admits text (Custom's freeform prompt lands here, F3).
- **The live footer is always on** (a mini-preview); `P` is a fuller preview
  and never gates launch.
- **Typed How readers** are generated from `beads-formula-var`; unknown types
  fail soft to string entry.
- **Client-side validation** warns before gc would refuse (cross-store routes;
  the `bl-bdj` v2 `run_targets` city-scope trap).

---

## 9. Agent-launch redesign (REQ-011, REQ-012)

Launch is the **direct local start**, distinct from sling (dispatch to a
target). It shares target discovery, the preview footer, and session/attach
handling.

### 9.1 Post-slimming matrix (F3 — remove-entirely)

| Role | Status | How reached | Prompt |
|---|---|---|---|
| Task | kept | `t` | `beads-agent-type-task--system-prompt` + user envelope |
| Review | kept | `r` | `beads-agent-review-prompt` + user envelope |
| Review (QA mode) | kept as a **mode**, not a role | `r` then `q` toggle (or `beads-agent-start-qa` → Review+QA alias for one release) | `beads-agent-qa-prompt` + `beads-agent-type-qa--user-prompt` (prompts kept; class deleted) |
| Plan | kept | `p` | `beads-agent-plan-prompt` + user envelope |
| QA | **deleted class** | — | prompt folded into Review's QA mode |
| Custom | **deleted class** | sling freeform path | freeform prompt moves to sling |

| Backend | Status | How reached |
|---|---|---|
| claude-code | preferred | `b` |
| agent-shell | kept | `b` |
| terminal | kept | `b` |
| claude-code-ide / claudemacs / eca | demoted | `… other` overflow |
| mock | test-only | never in the menu |

The **registries** (`beads-agent-type-register`,
`beads-agent-backend-register`) are unchanged and open. But unlike round 1,
there is **no QA/Custom class left registered by default**; a user who wants
them re-registers their own subclass through the documented ABI.

### 9.2 Flow

```
Role → Target(worktree/branch) → Backend → Prompt(edit/preview) → Start → Attach/Follow
```

- The live footer mirrors sling: `✓ Ready — Task · worktree be-abcd · backend
  claude-code`.
- Prompt editing uses `beads-agent-prompt-edit.el`; the system + user prompts
  are previewable before launch.
- Session lifecycle: the launch writes through `beads-agent-state-change-hook`
  so the sessions list (`beads-agent-list.el`) updates; `RET` attaches,
  `j` jumps, `x` stops (mockup §9). Attach goes through
  `beads-terminal-attach`.

### 9.3 Relation to sling

Agent launch is the "who = me/here" case of sling with a richer local start
surface. Shared code: `beads-sling-targets`, `beads-sling-validators`,
footer, session/attach. Sling adds the remote/city target set and the
`gc` backend; launch adds backend/prompt/session detail for the local case.

---

## 10. Formula integration (REQ-013, REQ-014)

- **Browse** (`beads-formula-list-mode`): `tabulated-list-mode`, type-grouped
  (`workflow` / `expansion` / `aspect`), columns name/type/steps/vars/
  description (mockup §10a). The existing `beads-formula-list` is the base.
- **Detail** (`beads-formula-show-mode` → `beads-section-mode`): Vars (typed,
  required flags), Steps (recipe with `needs` edges), Source (open the
  `.toml`). `RET` opens a var/step context, `l` seeds sling, `s` standalone
  launch (mockup §10b).
- **Vars** are first-class `beads-formula-var` objects with the same typed
  metadata the sling How stage uses, so a var is read the same way in both
  places.
- **Launch** uses `beads-formula-launch` (`formula bead &optional vars`). The
  default method irons locally/via `bd`; gascity overrides it to run
  `gc sling --formula/--on` and expose the run view. **Follow** opens the
  resulting session/workflow view through the same session/attach path.
- Standalone, a formula launch is a sling `formula`/`on` shape with no target
  beyond local; with gascity, the Who stage gains city/rig targets.

---

## 11. Terminal migration plan (REQ-015, REQ-016)

### 11.1 What moves: the terminal

`gascity.el/lisp/gascity-terminal.el` (≈1509 lines) owns, on top of
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

### 11.2 What stays in gascity.el

- `gascity-tmux-socket` resolution (city-name inference + override).
- `gascity-context-*` (city/rig resolution) and everything gascity-domain.
- The `gascity-*` command classes and views.
- A **thin compatibility shim** `gascity-terminal.el` that keeps the old
  entry points working: `gascity-terminal-attach-tmux`,
  `gascity-terminal-run`, and the scroll-mode symbol are `defalias`ed or
  defined as wrappers onto the `beads-terminal-tmux-*` functions, resolving
  the socket and passing it.

### 11.3 Remote helpers (moved/added to beads)

| Needed | beads.el status | Action |
|---|---|---|
| ssh argv | `beads-remote-ssh-argv` exists | reuse |
| PATH assignment | `beads-remote-path-assignment` exists | reuse |
| executable resolution | `beads-remote-find-executable` exists | reuse |
| `gascity-remote-localize-path` | missing | add `beads-remote-localize-path` |
| `gascity-remote-terminfo-p` | missing | add `beads-remote-terminfo-p` |
| `gascity-remote-buffer-name` | equivalent in `beads-buffer.el` | reuse/extend |
| `gascity-remote-prewarm` | missing | add `beads-remote-prewarm` (optional) |

### 11.4 Migration order (safe, shim-first)

1. Add `beads-terminal-attach` + `beads-terminal-tmux.el` in beads.el,
   porting the tmux/status/mouse/scroll code against beads-only dependencies.
2. Add the missing `beads-remote-localize-path` / `-terminfo-p` helpers.
3. Make gascity.el's `gascity-terminal.el` a shim delegating to the beads
   implementation; keep `gascity-tmux-socket` resolution in gascity.
4. Verify all three terminal surfaces (attach, status mirror, scroll) over
   TRAMP against bright-lights, from both packages.
5. Delete the duplicated implementation from gascity.el once green; keep the
   shim until a deprecation window passes.

### 11.5 Other de-duplication

- `beads-terminal.el` already owns backend selection; the moved code must use
  it (it mostly does).
- The tmux scroll/mouse work is documented in
  `gascity.el/docs/DESIGN-agent-scrolling.md`; that document becomes a
  beads.el doc (`docs/terminal-scrolling.md`) with the moved code, and the
  gascity doc links to it.
- Terminal tests currently live in gascity's `lisp/test/gascity-test.el`
  (there is no dedicated `gascity-terminal-test.el`; see `plan-review.md`
  F1); they port to `beads-terminal-tmux-test.el`.

---

## 12. How gascity.el uses each seam (extension summary)

| Seam | gascity.el use |
|---|---|
| `beads-store-resolvers` / `beads-store-prefix-functions` | map a `gc bd` prefix / rig dir to the owning store |
| `beads-menu-providers` | append a `[City]` group to `beads-dispatch` |
| `beads-action-providers` | add drain/nudge/sling actions to bead views |
| `beads-after-action-functions` | refresh rig views after a mutation |
| `beads-section-register` / `beads-dashboard-section-providers` | register city/rig/agent sections on the board |
| `beads-sling-target-functions` | contribute city/rig agents as sling targets |
| `beads-sling-backend-register` + a `gc` `beads-sling-dispatch` method | dispatch through `gc sling` |
| `beads-formula-launch` override | run `gc sling --formula/--on`, expose the run view |
| `beads-agent-*-register` | register gc-backed agent backends |
| `beads-terminal-attach` (`:socket`) + `beads-terminal-tmux-*` | attach to a city tmux socket |
| `beads-mode-extension-map` (`C-c b c`, …) | city/rig commands without shadowing core keys |
| `beads-face-*` names | derive city/rig faces with `:inherit` |

beads.el remains fully usable with **none** of these present (REQ-021).

---

## 13. Risks and mitigations

| Risk | Mitigation |
|---|---|
| Auto-generated transients are load-bearing in tests | Keep them as dispatch backends; tests port to the hand-built surface; a compatibility alias keeps `M-x beads-<cmd>` working |
| F3 removes a real workflow (QA/Custom) | QA is a Review mode with the prompt kept; Custom is the sling freeform path; registries stay open for re-registration; `NEWS.md` records the removal |
| Terminal move creates a beads↔gascity cycle | beads owns the implementation; gascity keeps only the socket + shim; no `gascity-` reference in beads |
| TRAMP regressions in the moved terminal | Move only after a shim; verify attach/status/scroll over `/ssh:localhost:~/bright-lights` |
| Section/`vui` churn breaks the render guard | All loaders async; `lisp/test/beads-render-guard-test.el` remains the gate |
| Reserving `C-c b` collides with existing configs | Keep old binding as an alias for one release; document in `NEWS.md` |
| `beads` → status / `beads-dispatch` rename breaks configs | `?` and `beads-dispatch` carry the old content verbatim; `NEWS.md`; `beads-dashboard` unchanged |
| Slimming too aggressive for existing users | Each removal ships a one-release alias + a `NEWS.md` entry; freed `a q`/`a c` are reserved |

## 14. References

- `gascity.el/docs/DESIGN.md` — UI direction (binding).
- `gascity.el/plans/sling-command/*` — process/artifact shape to mirror and
  the adaptive-flow lessons (§8.1).
- `gascity.el/docs/DESIGN-agent-scrolling.md` — the terminal scrolling design
  that moves into beads.el.
- `gascity.el/lisp/gascity-terminal.el` — the code to move (§11).
- `lisp/beads-prefix.el`, `beads-meta.el` (`beads-meta-parity-*`),
  `beads-section.el`, `beads-thing.el`, `beads-command-list.el`,
  `beads-command-show.el`, `beads-dashboard.el`, `beads-dashboard-sections.el`,
  `beads-agent*.el`, `beads-command-formula.el`, `beads-terminal.el`,
  `beads-remote.el`, `beads-buffer.el`, `beads-util.el`.
- `AGENTS.md` — build/test/remote-testing conventions; `MAGIT_PATTERNS.md`.
