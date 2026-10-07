# The beads.el UI redesign — architecture and extension model

This document describes the keyboard-first porcelain that replaced the
generated-transient front end, the entry points it exposes, and the
magit/forge-style seams downstream packages (notably `gascity.el`) attach
to.  It is the long-form companion to the design record in
`plans/beads-ui-redesign/` — `design.md` is the authoritative seam list,
`menu-mockups.md` the screen-by-screen rendering rules, and
`implementation-plan.md` / `decomposition.md` the work breakdown.

- **REQ-020** — a named, documented seam list.
- **REQ-033** — this manual plus the README architecture refresh.
- **REQ-002 / REQ-018** — one navigation and interaction language.

`bd` is the plumbing; beads.el is the porcelain.  Every view is a function
of `bd … --json` output: we render its state and dispatch its commands, and
we never reimplement bd logic.

## 1. Entry points

The redesign splits the old single `M-x beads` transient into the Magit
status/dispatch pair.

| Entry | Opens | Notes |
|---|---|---|
| `M-x beads` | `beads-status` | the Magit-like status board — the front door |
| `M-x beads-status` | `beads-status` | same buffer; no longer an obsolete shim |
| `M-x beads-dispatch` | `beads-dispatch` transient | the command menu; `?` opens it from any beads buffer |
| `M-x beads-dashboard` | `beads-dashboard` | the full board (a superset); `:directory` scoping unchanged |
| `!` (from a bead view) | `beads-maintenance` | admin/infrastructure commands |
| `M-x beads-list` / `beads-ready` / `beads-blocked` | list views | tabulated, filterable |
| `M-x beads-show` | detail view | vui sections + action bar |
| `M-x beads-sling` | sling transient | dispatch work to a target |
| `M-x beads-agent-*` | agent launch | local agent start |
| `M-x beads-formula-browse` | formula browser | tabulated, type-grouped |

`beads-dispatch` carries the *content* of the old `beads` transient
verbatim, so nothing is lost; only the name and the default entry changed.
`NEWS.md` records the rename.

Context actions in every view are collected by `beads-actions-context` and
contributed by `beads-action-providers`, so `?` opens the **same** menu
everywhere and each view only changes its small context group.  A transient
never renders content (anti-pattern), and content is never put in a
transient.

## 2. One navigation contract

Movement never varies by view.  `beads-thing.el` stamps the `beads-thing`
text property on whatever the user can move to; `TAB` / `S-TAB` move and
wrap, `SPC` toggles (folds a section, expands an epic, opens the issue at
point), and `beads-thing-define-keys` installs the keys in every mode.

Reserved keys (design.md §3.1):

| Key | Meaning |
|---|---|
| `q` | bury buffer |
| `g` / `C-u g` | refresh in place / hard refresh (drop caches) |
| `TAB` / `S-TAB` | next / previous thing (wraps) |
| `SPC` | toggle the thing at point |
| `RET` | visit / activate at point |
| `n` / `p` | next / previous item |
| `N` / `P` | next / previous section |
| `?` | `beads-dispatch` |
| `/` | filter (list surfaces only) |

Context action keys (`d` close, `C` claim, `s` status, `#` priority, `a`
agent prefix, `S` sling, `e` edit, `c` create/comment, `x` stop, `j` jump)
are stable across views where the action makes sense.

### The reserved extension prefix

beads.el owns `C-c b` and `C-c b ?`.  Every beads major mode installs the
prefix through `beads-mode--install-extension-map`, so downstream packages
add commands under `C-c b <letter>` via `beads-mode-extension-map` without
shadowing a core key.  `C-c b b` is "bead at point".  See §4.10 below.

## 3. Layering and module map

Three layers, unchanged from the start of the redesign, plus the new
porcelain:

1. **Command classes** — `beads-defcommand` in `beads-command.el`.  A class
   declares typed slots; `beads-meta.el` infers CLI options, builds the
   command line, and the execution generics run `bd` and decode `--json`
   into `beads-types.el` objects.  This is the stable layer: one
   `beads-command-<name>.el` per `bd` subcommand.
2. **Execution** — `beads-command-execute` (sync, tests and first-contact
   root discovery only), `beads-command-execute-interactive` (transient
   suffixes), and `beads-command-execute-async` (non-blocking, with
   `:queue` concurrency cap and `:cache-key` single-flight).  Every view
   read is async; no synchronous I/O at render is enforced by
   `beads-render-guard-test.el`.
3. **Renderers** — `vui` for heterogeneous, collapsible, async boards and
   detail views; `tabulated-list-mode` for homogeneous sortable/filterable
   tables; `transient` for argument collection and dispatch only.

### View-technology rule

| Surface | Mechanism |
|---|---|
| Status board | `vui` (`beads-status-mode`) |
| Dashboard / store overview | `vui` (`beads-dashboard-mode`) |
| Bead list | `tabulated-list-mode` (`beads-list-mode`) |
| Bead detail | `vui` (`beads-show-mode`) |
| Formula list | `tabulated-list-mode` |
| Formula detail | `vui` |
| Agent/session list | `tabulated-list-mode` |
| Sling / agent launch | `transient` + special-mode preview |
| Terminal attach | `beads-terminal` backends |

### Modules

**New in the redesign**

- `beads-menu.el` — the hand-built `beads-dispatch` (`?`) and
  `beads-maintenance` (`!`) transients and `beads-menu-providers`.  No
  command classes.
- `beads-sling.el` — the sling target/backend classes, target discovery,
  shape inference, the adaptive transient, the preview buffer, validators,
  and the default local dispatch method.
- `beads-formula.el` — the formula browser/detail/launch/follow UI; the
  command classes stay in `beads-command-formula.el`.
- `beads-terminal-tmux.el` — the tmux attach, status mirror, mouse and
  scroll support moved out of `gascity.el` (see
  `docs/terminal-scrolling.md`).
- `beads-faces.el` — the `beads-face-*` palette.

**Changed**

- `beads.el` — core utilities; `M-x beads` now opens `beads-status`, and
  the transient prefix lives in `beads-menu.el` as `beads-dispatch`.
- `beads-status.el` — the real status board (was an obsolete shim).
- `beads-command-list.el`, `beads-command-show.el` — universal keymap
  contract, action providers, sectioned detail.
- `beads-section.el`, `beads-dashboard-sections.el` — section registry and
  provider hooks.
- `beads-actions.el` — action providers and after-action fan-out.
- `beads-agent*.el` — launch-flow redesign; QA and Custom roles removed
  (see §5).
- `beads-remote.el`, `beads-buffer.el` — remote helpers and the reserved
  `C-c b` prefix.

**Deleted**

- `beads-ops-menu.el`, `beads-advanced-menu.el` — folded into
  `beads-menu.el`.
- `beads-more-menu` — removed.

## 4. Extension model (the concrete seam list)

Magit/Forge work because downstream code attaches at *named* points.  All
symbols are `beads-` / `beads--`; internal ones are marked `--`.  This
section is the ABI contract; `design.md` §4 holds the full table with
signatures.  gascity.el's use of each seam is summarised in §6.

### 4.1 Store scoping and resolution — `beads-util.el`

| Symbol | Kind | Purpose |
|---|---|---|
| `beads-store-resolve` | defun `(directory) → dir-or-nil` | normalize an explicit store dir |
| `beads-store-project-root` | defun `(store) → root` | root of an explicit store |
| `beads-store-resolvers` | hook `(fn dir) → dir-or-nil` | consulted when a directory does not resolve to a store |
| `beads-store-prefix-functions` | hook `(fn prefix) → store-or-nil` | map a bead-id prefix to its owning store |
| `beads-store-descriptor` | EIEIO class `{root database remote label prefixes}` | uniform scoping value object |
| `beads-store-directory` | buffer-local var | the scoping ABI |
| `beads-issue-id-prefixes` | defcustom | recognized id prefixes |

Rule: a view resolves its store from `:directory` (buffer-local
`beads-store-directory`) or `default-directory`, and never guesses.  A
resolver only translates a directory or prefix to a store.

### 4.2 Menu and command dispatch — `beads-menu.el`

| Symbol | Kind | Purpose |
|---|---|---|
| `beads-menu-providers` | hook `(fn) → list-of-transient-group` | groups appended to the dispatch menu |
| `beads-dispatch` | transient prefix | the `?` dispatch backend |
| `beads-maintenance` | transient prefix | the `!` maintenance menu |
| `beads-define-prefix` / `beads-define-group` | defmacro | directory-scoped prefixes (`beads-prefix.el`) |

Provider contract: providers take no arguments, return transient group
vectors, and are side-effect-free — they run on every `?`.  An empty list
is the standalone no-op.

### 4.3 Sections and dashboard composition — `beads-section.el`

| Symbol | Kind | Purpose |
|---|---|---|
| `beads-section-register` | defun `(key title loader renderer &optional keys) → symbol` | register a named async/renderable section |
| `beads-section-spec` | EIEIO class `{key title loader renderer keys order}` | the section descriptor |
| `beads-status-sections-hook` | hook `(fn) → vnode-or-nil` | status-buffer sections |
| `beads-dashboard-section-providers` | hook `(fn) → list-of-section-spec` | extra board sections (`beads-dashboard-sections.el`) |
| `beads-section-mode` | major mode | vui-derived base |
| `beads-section` text property | contract | identity for `RET`/eldoc/actions |

### 4.4 At-point actions — `beads-actions.el`

| Symbol | Kind | Purpose |
|---|---|---|
| `beads-action-providers` | hook `(fn context) → (KEY . ACTION) list` | actions in the bar and the `?` context group |
| `beads-actions-context` | defun `() → (CONTEXT . ISSUES)` | resolve context at point/marks |
| `beads-after-action-functions` | hook `(fn action issues)` | refresh fan-out after a mutation |

Context is a keyword: `:list`, `:show`, `:status`, `:dashboard`,
`:formula-list`, `:agent-list`.

### 4.5 Sling — `beads-sling.el`

| Symbol | Kind | Purpose |
|---|---|---|
| `beads-sling-target` | EIEIO class `{name kind scope backend description metadata}` | a dispatchable target |
| `beads-sling-target-functions` | hook `(fn) → list-of-target` | target discovery |
| `beads-sling-targets` | defun `(&optional bead) → list-of-target` | collect, dedupe, annotate |
| `beads-sling-shape` | defun `(work formula) → (plain \| on \| formula)` | pure inference |
| `beads-sling-dispatch` | cl-defgeneric `(target bead prompt) → session` | launch |
| `beads-sling-validators` | hook `(fn context) → warning-or-nil` | pre-launch validation |
| `beads-sling-backend` | EIEIO class `{name dispatch description}` | named dispatch backend |
| `beads-sling-backend-register` | defun `(backend) → backend` | backend registry |

### 4.6 Agent subsystem — documented ABI

| Symbol | Kind | Purpose |
|---|---|---|
| `beads-agent-type` / `beads-agent-type-register` | class + defun | role roster |
| `beads-agent-type-get` | defun | lookup by name |
| `beads-agent-type-system-prompt` / `-build-user-prompt` | cl-defgeneric | role and issue-envelope prompts |
| `beads-agent-backend` / `beads-agent-backend-register` | class + defun | backend roster |
| `beads-agent-backend-start` | cl-defgeneric `(backend issue system-prompt user-prompt)` | launch |
| `beads-agent-state-change-hook` | hook `(fn action session)` | lifecycle fan-out |

The registries stay open even though QA and Custom are no longer
registered by default (§5); a user may register their own subclass.

### 4.7 Formulas — `beads-formula.el`

| Symbol | Kind | Purpose |
|---|---|---|
| `beads-formula-launch` | cl-defgeneric `(formula bead &optional vars)` | launch + follow |
| `beads-formula-launch-context` | EIEIO class `{shape vars target warnings}` | resolved launch |
| `beads-formula-var` | EIEIO class `{name required type description default}` (`beads-types.el`) | typed var |
| `beads-formula-var-reader` | cl-defgeneric `(var &optional formula) → reader-spec` | type → transient infix kind |
| `beads-formula-var-kind` | defun `(var &optional formula) → symbol` | reader-kind shortcut |
| `beads-formula-var-choices` | cl-defgeneric `(var &optional formula) → list-or-nil` | enum + `metadata.gc.methodology` choices |
| `beads-formula-methodology` | cl-defgeneric `(formula) → alist-or-nil` | methodology metadata access |
| `beads-formula-validate-vars` | defun `(formula vars) → formula` | required/pattern check (signals `user-error`) |
| `beads-formula-missing-required-vars` | defun `(formula vars) → list` | non-signaling missing-required list |

Vars are read the same way in formula launch and the sling How stage.

### 4.8 Terminal — `beads-terminal.el`, `beads-terminal-tmux.el`

| Symbol | Kind | Purpose |
|---|---|---|
| `beads-terminal` classes + registry | EIEIO + defun | render backends |
| `beads-terminal-spawn` | cl-defgeneric `(terminal argv &optional name) → buffer` | spawn argv |
| `beads-terminal-attach` | defun `(session &optional socket dir store) → buffer` | attach to a named agent/session |
| `beads-terminal-tmux-*` | functions | tmux probes, attach argv/script, status mirror, mouse, scroll |

`beads-terminal-tmux-attach` opens the tmux client; the only
gascity-specific input is the tmux **socket**, passed as `:socket`.

### 4.9 Faces — `beads-faces.el`

No face hook.  Extensions derive with `:inherit` from the documented
`beads-face-*` names: `beads-face-header`, `-section`, `-issue-line`, `-id`,
`-key`; `beads-face-status-{open,in-progress,blocked,closed}`;
`beads-face-priority-{critical,high,medium,low}`;
`beads-face-agent-{running,idle,failed}`; `beads-face-success`, `-warning`,
`-error`.

### 4.10 Keymaps — `beads-buffer.el`

| Symbol | Kind | Purpose |
|---|---|---|
| `beads-mode-extension-map` | keymap | reserved `C-c b` prefix merged into every beads map |
| `beads-mode--install-extension-map` | defun `(map) → map` | install the prefix in a major-mode map |
| per-view maps | keymaps | `beads-list-mode-map`, `beads-show-mode-map`, `beads-status-mode-map`, `beads-dashboard-mode-map`, `beads-section-mode-map` |

### 4.11 Async reader — `beads-command.el`

`beads-command-execute-async` (`(command on-success &optional on-error
&rest kwargs)`, kwargs `:queue`, `:cache-key`, `:timeout`) and the custom
`beads-command-async-max-concurrent` are retained ABI.

### 4.12 Remote/TRAMP — `beads-remote.el`

`beads-remote-ssh-argv`, `beads-remote-path-assignment`,
`beads-remote-find-executable`, `beads-remote-ssh-command`,
`beads-remote-ssh-call`, `beads-remote-ssh-find-up`,
`beads-remote-localize-path`, `beads-remote-terminfo-p`,
`beads-remote-prewarm`, and the `beads-buffer-*` name helpers.

## 5. Removed surfaces

The redesign prunes as much as it adds.  Every removal has a replacement.

| Removed | Replacement |
|---|---|
| `M-x beads` (transient) | `M-x beads` → status board; the transient is `beads-dispatch` (`?`) |
| `beads-ops-menu.el` | folded into `beads-menu.el` (`!` maintenance; dispatch groups) |
| `beads-advanced-menu.el` | folded into `beads-menu.el` |
| `beads-more-menu` | dispatch/maintenance menus |
| Generated per-command transients | demoted to the dispatch backend; `M-x beads-<cmd>` still works |
| `make-obsolete` `beads-status` shim | `beads-status` is the real implementation |
| `beads-agent-type-qa` | Review's QA mode (prompt kept) |
| `beads-agent-type-custom` | sling freeform path |
| `beads-agent-start-qa` / `-custom`, `a q` / `a c` | Review QA mode / sling; keys freed and reserved |

## 6. Standalone and optional integration

beads.el is fully usable with only `bd` + local agents: every core flow
(status board, list, detail, sling to local targets, agent launch, formula
browse/launch, terminal attach) works with **no** provider registered.
`beads-menu-providers`, the section/action/sling hooks and the store
resolvers all default to empty, which is the standalone no-op.

With `gascity.el` present it attaches only through the seams above:

| Seam | gascity.el use |
|---|---|
| `beads-store-resolvers` / `beads-store-prefix-functions` | map a `gc bd` prefix / rig dir to the owning store |
| `beads-menu-providers` | append a `[City]` dispatch group |
| `beads-action-providers` / `beads-after-action-functions` | drain/nudge/sling actions; refresh rig views |
| `beads-section-register` / `beads-dashboard-section-providers` | city/rig/agent board sections |
| `beads-sling-target-functions` / `beads-sling-backend-register` + a `gc` `beads-sling-dispatch` method | contribute and dispatch to gc targets |
| `beads-formula-launch` override | run `gc sling --formula/--on` and expose the run view |
| `beads-agent-*-register` | gc-backed agent backends |
| `beads-terminal-attach` `:socket` + `beads-terminal-tmux-*` | attach to a city tmux socket |
| `beads-mode-extension-map` | `C-c b c` and friends without shadowing core keys |
| `beads-face-*` names | derive city/rig faces with `:inherit` |

beads.el must never require gascity.el; the dependency runs one way.

## 7. Verification

| Level | Command / gate |
|---|---|
| Unit | `eldev test -f <module>-test.el` |
| Full | `eldev -p -dtT test` |
| Compile / lint | `eldev compile`; `eldev -p -dtT lint` |
| CLI parity | `beads-audit-test.el` |
| Remote render | `beads-render-guard-test.el` |
| Extension seams | `beads-extension-seams-test.el` |
| Cross-repo ownership | `beads-cross-repo-ownership-test.el` (`docs/cross-repo-ownership.md`) |
| Navigation | `beads-navigation-test.el` |
| E2E | bright-lights TRAMP / tmux-Emacs pass |

## Further reading

- `plans/beads-ui-redesign/design.md` — the design and the authoritative
  seam tables.
- `plans/beads-ui-redesign/menu-mockups.md` — every screen's rendering.
- `plans/beads-ui-redesign/implementation-plan.md` — work items and gates.
- `docs/terminal-scrolling.md` — the moved terminal scrolling design.
- `MAGIT_PATTERNS.md` — the magit/forge conventions beads.el follows.
