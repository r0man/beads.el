# beads.el NEWS

User-visible and API-breaking changes, newest first.

## Unreleased

### Formula provenance and richer detail (WI-SF-05, REQ-SF-010/011/052)

The formula browser now shows a **Phase** column and the **Source** path, and
marks a formula whose name shadows a same-name formula lower on the `bd`
search path with a `⧉shad` badge (help-echo names the shadowed path).  The
scope filter (`/` in the browser, or a prefix arg to
`beads-formula-browse`) switches between `project`, `user` and `all`.

The formula detail now renders the declared `phase`, `version`, `extends`,
`aspects`, expansion formulas, `compose.bond_points` (id, before/after,
parallel), and each step's `type`, `depends_on`, `gate` and `waits_for`, plus
every variable's declared type, enum, pattern, default and required flag.

`beads-types.el` gains the matching parsed model: `beads-formula` slots for
`phase`, `extends`, `compose`, `aspects`, `expansions` and `bond-points`,
`beads-formula-step` slots for `step-type`, `gate` and `waits-for`, and the
new `beads-formula-gate` and `beads-formula-bond-point` classes.

### Agent launch works in non-git beads projects / Gas City workspaces

`beads-agent-start` (and the sling, typed, and text-menu start paths)
resolved the project directory with `beads-git-find-project-root',
which is nil outside a git repo.  The nil then reached
`default-directory' and surfaced as a misleading `Failed to parse
issue: Wrong type argument: stringp, nil' in the fetch-issue callback
instead of a diagnosable error, so no agent could start in a non-git
beads project.  Agent launch now resolves via a new
`beads-agent--project-root' (git first, then the non-git `.beads'/Gas
City marker walk, `default-directory' as a last resort), the start
path guards a nil project directory, and the fetch wrapper's
`condition-case' guards only JSON extraction so callback errors
propagate unmislabelled (be-kw9o).

Because git worktrees cannot exist outside a repo,
`beads-git-should-use-worktree-p' now returns nil when
`beads-git-find-project-root' is nil, so a non-git project starts the
agent in place rather than failing on a worktree it cannot create.

### Grouped list `mark-all` no longer loops on section headers

`beads-list-mark-all` walked the buffer assuming every row was an
issue.  With the redesigned status-grouped list (on by default), the
section headers carry non-string tabulated ids, so neither
`beads-list-mark` nor the trailing `(forward-line 0)` advanced point
and the command looped forever -- which hung the full `eldev test`
run at `beads-live-test-list-bulk-close`.  Point now advances exactly
one entry per iteration and skips headers (be-ka4s).  A grouped-list
ERT pins the behaviour.

### `beads-sling` menu renders on cold entry

Fixed a crash in `M-x beads-sling` that fired before any What/Who
stage was answered (be-d9ht): the generated transient put function
objects in suffix-description slots, which transient 0.13.8 treated as
the suffix command and refused to parse.  The work/formula/target lines
now take their dynamic descriptions from the pick commands themselves,
and empty optional actions are filtered out, so the cold menu parses
and renders.  An ERT now parses the menu both cold and seeded.

### `beads-sling` header/footer render on Emacs 29.4

The adaptive sling menu's header and live footer were built with
`(:info (lambda () ...))`.  On Emacs 29.4 a lexical closure is a list
`(closure ...)`, and transient 0.13.8 `eval`s an `:info` value unquoted,
so the closure was called as a function and `transient-setup` died with
`void-function closure'; Emacs 31 hid this because its closures are
self-evaluating `interpreted-function' objects.  The two `:info`
descriptions are now quoted lambda forms that read the live scope, so
they parse on both 29.4 and 31.1 and still recompute on every redraw
(be-ylw4).

### Remote/TRAMP parity and test consolidation (WI-18)

Test-only consolidation across the redesign: the retired
`beads-menu-reachability-test.el` (which required the deleted
`beads-ops-menu` / `beads-advanced-menu` files) is superseded by
`beads-menu-test.el`, whose reachability inventory walks the hand-built
`beads-dispatch` / `beads-maintenance` layouts.  Layout-coupled tests
were ported to the redesigned views (navigation dispatch, agent-list
`RET`, the detail action bar, the live list-row helpers), and a
duplicate `beads-remote-localize-path` test was renamed.  The detail
action bar now also advertises `beads-sling-dispatch` once the sling
work item is present (REQ-019, REQ-021, REQ-022).

### `beads-more-menu` removed

The deprecated `beads-more-menu` transient is gone: `M-x
beads-more-menu` is no longer defined and its dangling autoload is
removed (REQ-023, REQ-026).  Its genuine entries now live temporarily
in the `!` `beads-ops-menu` and `>` `beads-advanced-menu` (further
`beads-create`, `beads-q`, `beads-list-advanced`, `beads-prune`,
`beads-batch`, `beads-ping`, `beads-init-safety`, `beads-bootstrap`,
`beads-human`, `beads-onboard`); the dispatch-menu collapse in WI-9
will move them to their final home.  Every command the old menu held
remains reachable from the dispatch or a context key, enforced by
`beads-menu-reachability-test.el`.

### Generated per-command transients demoted off the primary dispatch

REQ-003/REQ-024: the primary `beads` dispatch no longer binds the
per-command transients generated by `beads-defcommand` (`beads-close`,
`beads-reopen`, `beads-search`, `beads-history`, `beads-diff`).
Close and reopen now use the hand-built context actions
`beads-actions-close` / `beads-actions-reopen` on `x` / `o`; the
generated transients, plus search, history and diff, are reachable
through the new reach-through menu `beads-commands-menu` (`a` in the
main dispatch) and via `M-x beads-<command>`.  The generated suffixes
are *not* deleted: `beads-meta-generated-transient-p` marks them so
the policy is testable, and `beads-meta` keeps generating them.

### Removed: QA and Custom agent roles (F3)

### Agent-launch redesign (WI-12)

The agent start menu is rebuilt as a single role → target → backend →
prompt flow with a live readiness footer (mockup §8):

- `beads-agent-start-menu` (aliased `beads-agent-launch`) now offers
  the slimmed roster Task/Review/Plan, with Review's QA mode as a `Q`
  toggle, a derived or chosen worktree target, a curated backend picker
  (the demoted backends stay reachable under `… other`), and prompt
  editing/preview before launch.
- New `beads-agent-prompt-preview` renders the system role prompt and
  the user issue envelope in a read-only buffer (mockup §8d).
- New `beads-agent-attach` is the session-attach seam; until the
  terminal migration (WI-14) lands it falls back to `beads-agent-jump`.
- The sessions list (`beads-agent-list`) follows mockup §9:
  Issue/Role/Backend/Status/Duration/Worktree columns, `RET` attach,
  `j` jump, `x` stop, `X` stop all, `d` Dired on the worktree.
- The lifecycle hook (`beads-agent-state-change-hook`) drives live
  refresh of the sessions list (mockup §8e).

- QA is now the **Review** role with a QA mode.  `Review` gains a
  `qa-mode` slot and a `beads-agent-type-review-qa` constructor; in QA
  mode it uses `beads-agent-review-qa-prompt` (the QA prompt text,
  relocated) and the former QA output envelope.  Sessions remain
  Review sessions.
- Custom's freeform prompt is reached through the sling work-picker
  escape rather than a role.
- The `a q` / `a c` keys in `beads-agent-prefix-map` are freed and
  reserved.
- The `beads-agent-type` / `beads-agent-backend` registry APIs are
  unchanged: users can still register an out-of-tree QA or Custom
  role, and the launch UI's curated backend list
  (`beads-agent-curated-backends`: claude-code, agent-shell, terminal)
  demotes the rest behind an `... other` overflow without
  unregistering them.

### Sectioned, keyboard-driven bead list (WI-7)

The bead list (`beads-list-mode`, used by `beads-list-issues`,
`beads-ready` and `beads-blocked`) is now a sectioned porcelain.  Issues
are grouped into collapsible status sections (`▾ In progress (2)`),
with a header line showing the issue count, per-status counts and the
active filter, a full mode-line action bar, and `N`/`P` section motion.
`SPC` on a section header folds it; `x` clears the active filter.
`beads-list-group-by-status` (default t) controls the grouped layout;
nil restores the flat table with `tabulated-list-mode` column sorting.
The retained `/` filter transient is unchanged.

### Detail redesign: identity block, breadcrumb and action bar

The `beads-show` buffer gains the detail-redesign chrome from the UI
mockups.  Under the title rule it renders a navigation hint line, a
local `Store` identity line (skipped for a remote store, so opening a
remote detail does no host I/O) and a breadcrumb back to the view the
detail was opened from (`^` returns to it, `beads-show-goto-origin`).
The footer is now an action bar built from the live commands plus the
`beads-actions-provider-actions` `:show` context, advertising
`d close · C claim · s status · # priority · e edit · c comment ·
w copy id · ? dispatch` (and `S sling` once the sling abstraction is
loaded).  `RET` on an agent session row now attaches to or jumps to
that session (`beads-show-attach-session-at-point`, also on `j`) via
`beads-terminal-attach` when it is available.  `c` is a comment alias.

### One dispatch menu and one maintenance menu (breaking)

`?` opens the hand-built `beads-dispatch` menu (the former `beads`
prefix, regrouped by frequency with descriptions).  `!` opens the new
`beads-maintenance` menu, which collapses the former `beads-ops-menu`
and `beads-advanced-menu` into a single maintenance/infrastructure
surface (REQ-023); those two menus, their files and their tests are
gone, and every command they held is reachable from
`beads-maintenance`.  Downstream packages append their own dispatch
groups through `beads-menu-providers`, spliced into `beads-dispatch`
at render time.

### The full board is now the entry point (breaking key: `M-x beads`)

`M-x beads` (and `beads`) opens the hand-built **full board**
(`beads-dashboard`, `beads-dashboard-mode`) instead of the transient
menu — the superset board with provider sections, section toggles,
next/prev navigation, auto/idle refresh and per-project visibility
persistence.  It obeys the universal navigation contract: `q` buries,
`g` refreshes in place, `TAB`/`S-TAB` move by thing, `SPC` folds, `RET`
visits, `?` opens the menu.

- The former `beads` transient prefix moved to `beads-menu.el` as
  `beads-dispatch`; bind it with `?` (REQ-002, REQ-004).
- Downstream packages extend the board with
  `beads-section-register`/`beads-section-spec` or
  `beads-dashboard-section-providers`.

### Universal navigation contract: `?` dispatch and reserved `C-c b`

Every porcelain view now installs the same navigation map through
`beads-mode--install-navigation-keys`, so the contract cannot drift
per view:

- `?` opens the shared dispatch menu everywhere.  Show buffers keep
their quick-actions transient: `beads-dispatch` delegates to
`beads-show-actions` there and to the main `beads` transient
elsewhere.
- `C-c b` is reserved for downstream extensions in every map (via
`beads-mode--install-extension-map`); `C-c b b` visits the issue at
point and `C-c b ?` opens the dispatch.  Extensions add bindings under
`beads-mode-extension-map` instead of shadowing a core key.
- TAB/S-TAB/SPC stay the one movement scheme (`beads-thing`), and
`q`/`g`/`RET` keep their mode-specific targets.

### Adaptive sling transient and preview

`M-x beads-sling` (REQ-009) opens the adaptive sling transient.  One
entry point covers the plain, formula and targeted `--on` shapes: the
shape is inferred and rendered as a single sentence, each stage
collapses to its answered line, and a live footer shows the pending
dispatch and any client-side warning (`beads-sling--header-sentence`,
`beads-sling--footer`).

- The What stage picks work (`A`, `C-u A` for freeform text) and a
  formula (`f`); the picked formula's vars render as a `How` group of
typed infixes (`beads-sling--var-children`): enum, boolean, file,
directory, agent and numeric readers chosen from the var's declared
shape, failing soft to string entry.
- `T` picks the Who target; the plain shape also shows the routing
  flags.
- `s` launches through the existing `beads-sling-dispatch` seam, `P`
  opens a full special-mode preview (`beads-sling-preview-mode`) whose
  own `s` launches exactly what was previewed.  The preview never
  gates launch.
- `beads-sling-validators` warnings feed the footer and the preview.

### Standalone sling abstraction

`beads-sling.el` now carries the full standalone sling surface, not
just the WI-4 seam.  New public API:

- `beads-sling-shape` — pure inference of the plain / `on` / formula
  dispatch shapes.
- `beads-sling-backend` + `beads-sling-backend-register` — named
  dispatch backends an extension registers (for example Gas City's
  `gc` backend); a target's `backend` slot selects one.
- `beads-sling-validators` — pre-launch validation hook.
- `beads-sling-targets` now has a default provider
  (`beads-sling--default-targets`) that collects one target per local
  agent role, one per available agent backend, and one per existing
  git worktree, so sling works with `bd` and the local agent stack
  alone (REQ-008, REQ-021).  Bind the hook to nil to get the empty
  standalone no-op.

The interactive agent start (`beads-agent-start-interactive`) now
routes through `beads-sling-dispatch`, so the existing command and the
future sling transient share one launch path.  New completion support
(`beads-completion-read-sling-target`, `beads-completion-sling-target-table`)
renders targets with their kind and description.

### Extension seams for downstream packages

beads.el exposes a documented magit/forge-style extension ABI
(design.md §4).  Downstream packages attach at named points instead of
shadowing core internals:

- Store scoping: `beads-store-resolvers`, `beads-store-prefix-functions`
  and the `beads-store-descriptor` value object
  (`beads-store-descriptor-for`, `beads-store-for-prefix`).
- Sections: `beads-section-register`, the `beads-section-spec` class and
  `beads-section-spec-for`; the existing `beads-status-sections-hook`
  and the new `beads-dashboard-section-providers` hook.
- Actions: `beads-action-providers`, `beads-after-action-functions`,
  `beads-actions-context` and `beads-actions-provider-actions`.
- Menus: `beads-menu-providers` and `beads-menu-provider-groups` in the
  new `beads-menu.el`.
- Sling: the `beads-sling-target` class, `beads-sling-target-functions`,
  `beads-sling-targets` and the `beads-sling-dispatch` generic in the
  new `beads-sling.el`, whose default method starts a local agent.
- Keymaps/faces: `beads-mode-extension-map` (reserved `C-c b` prefix),
  `beads-mode--install-extension-map`, and the canonical
  `beads-face-*` set in the new `beads-faces.el`.

Every hook/provider is a no-op when empty, so beads.el remains fully
standalone with no downstream package present.

### Removed: QA and Custom agent roles (F3)

The built-in QA and Custom agent *classes* are removed:
`beads-agent-type-qa` and `beads-agent-type-custom` are undefined, as
are the `beads-agent-start-qa` and `beads-agent-start-custom`
commands and the `beads-agent-qa-backend` / `beads-agent-qa-prompt`
defcustoms.

- QA is now the **Review** role with a QA mode.  `Review` gains a
  `qa-mode` slot and a `beads-agent-type-review-qa` constructor; in QA
  mode it uses `beads-agent-review-qa-prompt` (the QA prompt text,
  relocated) and the former QA output envelope.  Sessions remain
  Review sessions.
- Custom's freeform prompt is reached through the sling work-picker
  escape rather than a role.
- The `a q` / `a c` keys in `beads-agent-prefix-map` are freed and
  reserved.
- The `beads-agent-type` / `beads-agent-backend` registry APIs are
  unchanged: users can still register an out-of-tree QA or Custom
  role, and the launch UI's curated backend list
  (`beads-agent-curated-backends`: claude-code, agent-shell, terminal)
  demotes the rest behind an `... other` overflow without
  unregistering them.

### Formula browser, detail and launch (`beads-formula.el`)

Formulas now have a first-class browser and launch flow.  The new
`beads-formula.el` owns the UI and the extension seams; the `bd
formula` command classes stay in `beads-command-formula.el`.

- `beads-formula-browse` groups formulas by type (workflow, expansion,
  aspect) with header rows, and `beads-formula-detail-sections` exposes
  the Vars/Steps/Source structure.
- In a formula list or detail buffer, `l` seeds the sling flow with the
  formula and prompts only for the work bead, while `s` launches it
  standalone (prompting for required vars through typed readers).
- `beads-formula-var-reader` maps a variable's declared metadata to a
  reader kind (enum, bool, numeric, file, directory, agent, string);
  `beads-formula-launch-context` is the shared resolved launch.
- `beads-formula-launch` (`formula bead &optional vars`) is the launch
  generic.  The default method irons the formula locally with `bd mol
  pour`; downstream packages (for example Gas City) override it to run
  their own backend and return their run session.
- Formula-type headers are non-selectable rows built by
  `beads-formula-grouped-entries`; the flat list remains the default for
  `beads-formula-list` without the grouped entry builder.

### Terminal handling now lives in beads.el

The tmux attach/status/mouse/scroll subsystem moved out of
`gascity.el/lisp/gascity-terminal.el` into the new
`beads-terminal-tmux.el`, on top of the existing `beads-terminal.el`
backends.  A new generic entry point, `beads-terminal-attach`, attaches
to a tmux session in a terminal buffer without blocking Emacs; it is
usable standalone, with no gascity.el dependency.  The tmux server
socket is a caller argument (`:socket`), so multiple cities and stores
coexist.

- Attach is asynchronous: one host pre-step checks the session, tmux,
  terminfo for the backend's `TERM`, and the working directory, then
  opens a local terminal (a local `ssh -t` for a remote store).
- The session's tmux status bar is mirrored in the mode line and the
  tmux `mouse` option (plus a copy-mode wheel-to-bottom binding) is
  ensured, both restored when the terminal buffer is killed.
- `C-c s` toggles the Emacs-keys scroll sub-mode; on terminals that do
  not report the mouse the wheel is translated to tmux's own SGR mouse
  events.  New options: `beads-terminal-tmux-backend`,
  `-remote-term`, `-mode-line-status`, `-ensure-mouse`,
  `-unshadow-minor-modes`, `-status-interval` and `-preload-idle`.
- New remote helpers in `beads-remote.el`: `beads-remote-prefix`,
  `beads-remote-localize-path`, `beads-remote-buffer-name`,
  `beads-remote-terminfo-p`, `beads-remote-with-timeout` (signals
  `beads-remote-timeout`), `beads-remote-call-unshared` and the
  `beads-remote-async-timeout` option.

For package authors: `beads-terminal-running` is unchanged; the moved
code is loaded lazily, so requiring `beads-terminal` stays cheap.

### Remote project root resolves `~`-relative TRAMP stores

A store addressed as `/ssh:host:~/store` now resolves its project
root: the remote find-up walk expands a leading `~` against the
host's `$HOME` before testing marker directories, so `beads--project-root`
returns the real store root and buffers are host-qualified instead of
falling back to the unqualified default (REQ-019).

### One faces palette and one glyph set (breaking status glyphs)

A single `beads-face-*` palette in the new `beads-faces.el` now backs
every porcelain surface -- status, list, detail, formula, epic and
agent views.  Extensions derive faces with `:inherit`; there is no
face hook.  The module faces (`beads-list-*`, `beads-show-*`,
`beads-epic-*`, `beads-formula-*`, `beads-agent-list-*`,
`beads-issue-line`) are kept as derived aliases so existing themes
keep working, but new code should use the canonical `beads-face-*`
names.

Standard status glyphs are now `○` open, `◐` in-progress, `⛔`
blocked and `✓` closed in every view.  The show buffer previously
rendered `●` for closed and `✗` for blocked; those changed.  Agent
outcome marks stay `✓` finished / `✗` failed.

### Documentation: the redesigned architecture

The UI redesign is documented for users and package authors:

- New `docs/ui-redesign.md` covers the entry points, the universal
  navigation contract, the view-technology rule, the full extension seam
  list (REQ-020), the standalone/optional-integration split, and the
  removed-surface inventory.
- `docs/terminal-scrolling.md` now lives in beads.el (moved from
  `gascity.el/docs/DESIGN-agent-scrolling.md`), renamed to the
  `beads-terminal-tmux-*` symbols with the tmux attach, status mirror,
  transparent mouse and Emacs scroll sub-mode.
- The README architecture section is refreshed to the three-layer module
  map, the entry points, and the extension model, and `MAGIT_PATTERNS.md`
  records the dispatch/status split and menu providers.

The documented surface changes: `M-x beads` opens the status board and
`beads-dispatch` is the `?` menu; `beads-ops-menu.el`,
`beads-advanced-menu.el` and `beads-more-menu` are gone; the QA and Custom
agent roles are removed (Review QA mode and the sling freeform path
replace them); `C-c b` is the reserved extension prefix.

### One movement scheme: TAB/S-TAB next thing, SPC toggles (breaking keys)

Every beads.el view now moves the same way.  `TAB` (and `<tab>`) goes
to the next *thing*, `S-TAB` (`<backtab>`, `S-<tab>`) to the previous
one; both wrap and echo `Wrapped`.  `SPC` toggles the thing at point,
and `DEL` / `S-SPC` are unbound.  Things are section headers, issue
rows, the `… and N more (+)` line, show-buffer headings and references,
epics, and every row of a list buffer.

- Dashboard: `TAB` no longer folds the section at point: `SPC` on a
  header folds it, `N`/`P` jump between sections (`M-n`/`M-p` still
  work).  Folding never re-reads: a folded section keeps its data and
  its header count.  Fold glyphs are now `▾`/`▸`.
- Show: `TAB` moves between headings and references; `SPC` on a
  heading folds its section.
- Lists: `TAB` moves by row; `SPC` shows the issue at point in another
  window, or closes that window again (it no longer means next-line).
- Epic status: `SPC` expands an epic as before; `N`/`P` jump between
  epics.

For package authors: `beads-thing.el` is the primitive behind it.
Stamp the `beads-thing` text property on what should be a thing (a
toggle function, or a plist `(:kind KIND :toggle FN)`), install the
keys with `beads-thing-define-keys`, and plug extra toggles into the
buffer-local hook `beads-thing-toggle-functions`.

### Explicit store scoping with `:directory`

`beads-show`, `beads-ready`, `beads-blocked`, the new programmatic
`beads-list-issues` (`&key directory spec`) and `beads-dashboard`
take `:directory`.  It becomes the buffer's store
(`beads-store-directory`): every bd command run from that buffer --
refreshes and actions included -- gets `--directory`, so a shared Dolt
server cannot route it to another project's database.  Buffers opened
from a scoped buffer inherit its store.  A TRAMP name is fine; a
host-local path is taken on the caller's host.

### Remote stores: no blocking, no TRAMP pty for bd

- Asynchronous bd on a single-hop ssh-family store (`/ssh:`, `/scp:`
  ...) now runs as a local `ssh -T` pipe process that `cd`s to the
  store on the host, instead of a TRAMP `make-process`.  Starting it
  never blocks Emacs, and it no longer shares TRAMP's pty-backed ssh
  master, which could deadlock.  New options: `beads-remote-transport`
  (`ssh`, or `tramp` for the old behaviour; other methods always use
  TRAMP), `beads-remote-ssh-options` (ControlMaster, ControlPersist)
  and `beads-remote-ssh-control-path`.  The pipes run with `-n` and
  `-o ForwardX11=no`.  ssh runs in BatchMode: key or agent
  authentication (or an open master) is required.
- Opening a remote store does no TRAMP I/O on an ssh-transport host.
  The project root is found by one shell command over the ssh pipe
  (markers `beads-project-root-markers` and `.git`, no VC walk) and
  remembered per directory, negative answers included
  (`beads-forget-project-roots` clears them).  A remote dashboard
  without `:directory` is scoped to that root with `--directory`
  instead of scanning for the database path.  Synchronous bd commands
  run over the pipe too (`beads-remote-sync-timeout`), so no executable
  probe runs.  The one remaining wait is the first ssh handshake to a
  host (~0.7 s on localhost, ~10 ms once its master is up).
- `beads-show` fetches asynchronously for a remote store
  (`beads-show-async`, default `remote`; `beads-show-async-timeout`),
  and neither it nor `beads-dashboard` runs git or walks the directory
  tree over TRAMP when opening, rendering or folding.
- New `beads-remote.el`: one remote executable resolver
  (`beads-remote-find-executable`, per-connection cache, a failed
  lookup remembered for `beads-remote-miss-ttl` seconds), the PATH
  fragment for remote commands, and the ssh argv builders
  (`beads-remote-ssh-argv`, `beads-remote-ssh-pipe-argv`,
  `beads-remote-ssh-command`).  `beads-remote-search-path` now also
  covers `/run/current-system/profile/bin`.
- Remote helpers for terminal attach: `beads-remote-localize-path`
  re-prefixes a host-local path for a remote view (pure, no I/O),
  `beads-remote-terminfo-p` probes the host's terminfo with `infocmp`
  and a compiled-entry sweep (positive results cached per
  connection x TERM), and `beads-remote-prewarm` resolves
  `beads-remote-prewarm-programs` on an ssh-transport host in the
  background so the first attach need not run a synchronous
  executable probe.

### `beads-show` links only real bead ids

With `beads-issue-id-prefixes` unset, a show buffer recognises the
prefixes of the shown bead and its dependencies, so hyphenated words
such as `build-basic` are no longer links.

### Menus remember the project they were opened for

Transient menus now run their commands in the directory they were
opened for.  Before, opening `beads` for another project from
`project-switch-project` (`C-x p p`) or `project-any-command` ran the
chosen command against the buffer you started from, because the menu
returns before you pick a command.  `beads-dashboard` called from the
switch menu likewise opens the chosen project's board, with root and
database from the same project.

For package authors: define beads menus with `beads-define-prefix` and
`beads-define-group` (`beads-prefix.el`) instead of
`transient-define-prefix` and `transient-define-group`.  The menu
records the directory under `:directory` in its scope plist
(`beads-prefix-directory`), and every group gets `:advice*
beads-prefix-call-in-directory`.  A custom prefix `:class` must derive
from `beads-prefix`.

### `beads show` hides empty sections

Show-buffer sections with no data (empty description/design/notes,
no dependencies, no labels, no metadata, no lease, no comments,
…) are now skipped entirely instead of rendering a dim "(none)"
placeholder under the header: no header, no placeholder, no noise.
A closed bead without a close reason no longer gets a CLOSE REASON
section either.  The "(N comments omitted)" note when comment
bodies were not fetched is unchanged.

### Menu registration for the bd 1.3.x command additions

The command transients added in the bd 1.3.x sync are now reachable
from the menus instead of only `M-x`:

- Ops menu (`!`): `u' Unclaim, `e' Events, `C' Conflicts,
  `r' Reclaim, `B' Heartbeat.
- Advanced menu (`>`): `Y' Dolt sync, `s' Schema,
  `5' Migrate personal, `H' Provenance.

### CHANGELOG behavior follow-through (bd 1.3.x)

Behavior changes required by the bd 1.3.x CHANGELOG now have UI
follow-through:

- The global `CPU profile' option passes `--cpu-profile`.  bd 1.3.0
  renamed the persistent flag from `--profile' to `--cpu-profile' with
  no alias (#5126); the old spelling now fails as an unknown flag.
  The command-class global-options slot serializes the new spelling
  too, and the new `--mem-profile' heap-profile global flag has an
  infix (key `=M') and a typed slot.
- `bd dep cycles' rendering accepts the bd 1.3.x JSON shape: each cycle
  is now an object `{"members": [{"id"}, …], "partial": bool}' with
  canonical (lowest-id-first) member order, and a cycle with a missing
  member row renders every member and is marked "(partial — a member
  row is missing)" instead of silently shrinking.  The pre-1.3.0
  array-of-ids shape still renders unchanged.
- The event-type vocabulary knows the `claimed' and `lease_reclaimed'
  event types a bd 1.2.x+/1.3.x store emits (claims and replica-aware
  lease reclaims), so history/event rendering and validation no longer
  treat them as unknown.
- `beads-search` documents that bd 1.3.x search INCLUDES CLOSED issues
  by default (bd-t5yex), and its status filter documents that passing
  `open' restores the old open-only behavior.
- The show buffer header docstring names the second-line creator value
  `Created by' (bd 1.3.x labels the `created_by' field `Created by:';
  `owner' is a separate CV-attribution field).  The buffer itself
  already rendered the bare creator name without an `Owner:' label, so
  no visible change was needed there.
- The internal `alist' Elisp type was renamed `beads-alist'
  (package-prefix policy); the coercion method and every typed slot
  that accepted a JSON object moved with it.  Out-of-tree slot
  definitions that said `:type alist' need the new name.

### Command/flag gap closure: bd 1.3.0 command surface

Every bd 1.3.0 command now has an Emacs surface: new `beads-defcommand`
classes for the category-1 leaves (`conflicts`, `events`, `heartbeat`,
`migrate-personal`, `provenance`, `reclaim`, `schema`, `serve`, `sync`,
`unclaim`, and friends — one `beads-command-<name>.el` each), parent
transient-only menus for the new top-level groups per AGENTS.md policy,
and the category-2 flag gaps filled as class slots (`update --force`,
`ready/list --brief/--max-rows/--offset`, `init` proxied-server knobs,
`create --status/--allow-empty-description/--storage-class`, and the
rest of the audit's category-2 list).  New commands are registered on
the main/ops/advanced menus.  The command-parity drift gate now passes
against live `bd 1.3.0`.

### Show buffer: full terminal `bd show` section parity

The show buffer now renders every section terminal `bd show` displays
for any bead: a METADATA map (sorted keys, clickable issue-id values),
LABELS badges, a LEASE section (expiry with relative time, heartbeat,
granting node), TRACKS / TRACKED BY sections for `tracks`-type edges
in both directions, a COMMENTS section rendering full comment threads
(the interactive show path now requests `--include-comments`), and —
on closed beads — a CLOSE REASON section plus an `Outcome:` header
line sourced from the `gc.outcome` metadata key.  DESCRIPTION, DESIGN,
ACCEPTANCE CRITERIA and NOTES body sections are always rendered: an
empty section shows a dim "(none)" placeholder under its header
instead of being silently skipped, so the section inventory is
identical for every bead.  An absent `labels` key and an explicitly
empty label list render identically.  `beads-show-next-section` and
`beads-show-previous-section` return nil when no section is found.

### Show: bd 1.3.0 flags (--brief-deps, --include-comments) and richer data source

`beads-command-show` gains the two missing bd 1.3.0 show flags as class
slots: `brief-deps` (`--brief-deps`, JSON-only dependency compaction)
and `include-comments` (`--include-comments`, stream full comment
bodies; transient key `C`).  The show transient now exposes and serializes
the full 1.3.0 flag surface (`--long`, `--as-of`, `--refs`, `--children`,
`--local-time`, `--include-dependents`, `--brief-deps`,
`--include-comments`).  The interactive show path (`beads-show`,
`beads-show-update-buffer`, `beads-refresh-show`) now requests `--long
--include-comments --include-dependents` so the show buffer renders
from complete data; note this trades latency for completeness on
comment-heavy beads.  Programmatic `beads-execute` calls are unchanged.
The parity drift gate's accepted-drift entry for `show
--include-comments` is gone — the slot is real now.

### Eldoc: asynchronous, base-36 ids, terminal buffers

`beads-eldoc-mode` no longer runs `bd show` synchronously: it used to
freeze Emacs for every eldoc tick on a remote (TRAMP) store — one to
four seconds per lookup, plus a remote-to-local copy of a stderr temp
file.  Lookups now go through `beads-command-execute-async`, results
are cached per store (a remote host's `bd-1` and a local `bd-1` are
distinct entries), unknown ids are cached as misses for
`beads-eldoc-negative-cache-ttl` seconds, concurrent requests for one
id share a single spawn, and nothing is spawned for a remote store
whose connection is not already open.  A result that lands after
point has left the id is dropped.  `beads-show--eldoc-function` uses
the same cache.

Issue ids are recognised by one shared regexp, `beads-issue-id-regexp`,
whose hash part is base-36 — real ids look like `bs-lc1lb`, `gce-hck`
or `bde-dww`, which the previous hex-only patterns in eldoc, `beads-show`
and `beads-issue-at-point` never matched outside a button.  Point-based
eldoc therefore now works in any text buffer, including shell/comint
buffers and `vterm-copy-mode`.  Because the wider syntax also matches
hyphenated words, `beads-issue-id-prefixes` (alias
`beads-eldoc-issue-prefixes`) restricts detection to known store
prefixes; it and the new `beads-eldoc-directory` (the store, or a
function from id to store, that lookups resolve in) are meant to be set
buffer-locally by front ends such as gascity.el.  The default of
`beads-eldoc-issue-pattern` is now nil (use the shared regexp); a
customised value is still honoured.  New helpers:
`beads-issue-id-search-forward`, `beads-issue-id-at-point`.

### Remote (TRAMP) stores: async bd runs on the remote host

`beads-command-execute-async` (and the dashboard's concurrency
probe) now spawn bd through the TRAMP file handler when
`default-directory` is remote, so async views — dashboard sections,
list refreshes, and (see below) eldoc — read the remote store instead
of failing or silently querying a local one.  Sync execution already worked.  On
remote spawns bd's stderr is discarded on the remote side (TRAMP
cannot safely separate it; error reports carry an empty `:stderr`),
and a missing remote directory is reported as an error instead of
hanging the request forever.  Works under both tramp-sh and the
Emacs 30 direct-async handler.

Buffer names are qualified with the remote prefix — e.g.
`*beads-show[/ssh:user@example.com:|rig]/bd-1*`,
`*beads-dashboard</ssh:user@example.com:|rig>*` — so a local and a
remote project with the same name no longer collide in one buffer.
The `beads-buffer-parse-*` functions gained a `:remote` key (nil for
local buffers); all other keys are unchanged.

### Breaking: `beads-agent-backend-start` is now 4-arity

The backend protocol generic changed signature:

```elisp
;; before
(beads-agent-backend-start backend issue prompt)
;; after
(beads-agent-backend-start backend issue system-prompt user-prompt)
```

There is **no backwards-compatibility shim and no deprecated alias**.
Every in-tree backend migrated atomically. An out-of-tree backend that
still defines a 3-arity `cl-defmethod` will, on the first start
attempt, hit a default method on the abstract base
`beads-agent-backend` that signals a plain `error`:

> Backend `<class>` must implement 4-arity beads-agent-backend-start
> (backend issue system-prompt user-prompt).  See NEWS

(This replaces what would otherwise be a cryptic
`cl-no-applicable-method`.)

**Migration recipe** for an out-of-tree backend:

```
;; arglist
s/(backend issue prompt)/(backend issue system-prompt user-prompt)/

;; body, if your backend has no dedicated system-prompt channel:
(let ((prompt (beads-agent-backend--combine-prompt
               system-prompt user-prompt)))
  ...existing body unchanged...)
```

`beads-agent-backend--combine-prompt` prepends a non-empty
SYSTEM-PROMPT to USER-PROMPT separated by a blank line, and returns
USER-PROMPT unchanged when SYSTEM-PROMPT is nil or empty. An empty
system prompt is treated as absent (no stray blank line).

`issue` may be `nil` (project-level agents call with no issue); methods
must tolerate that in slot 2 (this was already true pre-change).

In this phase (1a-i) the rendered prompt is **byte-identical** to the
prior release: the new system-prompt channel exists but every built-in
agent type still yields `nil` for it. The default-value rewrites that
make the split observable land in a separate, separately-revertable
change.

### Breaking: `beads-agent-type-build-prompt` renamed

`beads-agent-type-build-prompt` is now
`beads-agent-type-build-user-prompt` (same arity, same behaviour, same
return value). No alias.

**Migration recipe:**

```
s/beads-agent-type-build-prompt/beads-agent-type-build-user-prompt/
```

### New: `beads-agent-type-system-prompt` generic

`(beads-agent-type-system-prompt type issue)` returns the agent
role/identity string (with `<ISSUE-...>` placeholders substituted), or
`nil` when the type has no distinct system prompt. Builder types (such
as Custom) and — in this phase — every built-in type return `nil`. A
new `system-prompt` slot was added to `beads-agent-type`
(string/symbol, default `nil`), mirroring `prompt-template`.

### Breaking: prompt-edit callback signature + cancel sentinel

`beads-agent-prompt-edit-show`'s CALLBACK is now invoked with **two**
arguments, `(SYSTEM USER)`, instead of one:

- Confirm: `(funcall callback SYSTEM USER)`. In this phase the buffer
  is still single-region, so `SYSTEM` is always `nil` and `USER`
  carries the edited text. (The two-region editing UI lands in a
  later phase.)
- Cancel: `(funcall callback nil nil)`.

The **cancel sentinel is `(nil nil)`**. "No system override, real user
prompt" is `(nil "the user text")` and proceeds to launch. The
orchestrator distinguishes cancel — `(and (null sys) (null user))` —
from "use the default system prompt" — `(null sys)` with non-nil
`user`. Any out-of-tree code installing a prompt-edit callback must
accept two arguments and treat `(nil nil)` as cancel.

### Behavioural break: default prompts split into system + user

The built-in agent-type default prompts were **rewritten** (not
relabelled). The role/identity preamble is now a **role-only** system
prompt; the issue envelope (`<ISSUE-ID>: <ISSUE-TITLE>` +
`<ISSUE-DESCRIPTION>`) and the type-specific `bd close`/`bd update`
shell blocks moved to a new **user-prompt** defconst per type:

- `beads-agent-type-task--prompt` → split into
  `beads-agent-type-task--system-prompt` (role) +
  `beads-agent-type-task--user-prompt` (envelope/output).
- `beads-agent-{review,plan,qa}-prompt` defcustom **default values**
  rewritten to role-only; new
  `beads-agent-type-{review,plan,qa}--user-prompt` defconsts carry the
  envelope/output. Defcustom *names* are unchanged.
- Custom and the orchestration fallback are builders (no
  `<ISSUE-...>`); `system-prompt` stays nil; their builder is
  unchanged (renamed only, in 1a-i).

**Consequence for users who `setq`/`customize`d the role defcustoms:**
your value is now delivered as the **system** prompt. Embedded
`<ISSUE-...>` placeholders still substitute. Embedded `bd close`/`bd
update` instructions now arrive in the *role* channel — move them into
the matching `beads-agent-type-*--user-prompt` if you relied on them.

The prompt editor now shows **two editable regions** (`## System
prompt` / `## User prompt`) with read-only marked headings; a blank
system region means "use the backend's built-in identity".

#### Old default values (verbatim, for diffing)

`beads-agent-type-task--prompt` (removed):

```
You are a task-completion agent for beads. Please work on beads issue <ISSUE-ID>: <ISSUE-TITLE>.

# Constraints

- Stay focused on the assigned task
- Don't make unrelated changes
- If blocked, explain clearly what's needed
- Communicate progress and decisions

# Agent Workflow

1. **Claim the Task**
   - Update issue status to in_progress: `bd update <ISSUE-ID> --status in_progress`
   - Read the task description carefully
   - Check acceptance criteria if available

2. **Execute the Task**
   - Use available tools to complete the work
   - Follow best practices from project documentation
   - Run tests if applicable
   - Keep changes focused on the task

3. **Track Discoveries**
   - If you find bugs, TODOs, or related work:
     - File new issues using bd create
     - Link them with discovered-from dependencies: `bd dep add <new-id> --type discovered-from --target <ISSUE-ID>`
   - This maintains context for future work

4. **Verify Completion**
   - Check that all acceptance criteria are met
   - Ensure tests pass
   - Review your changes for quality

# Output

When work is complete, close the issue with a clear summary:

    bd close <ISSUE-ID> --reason "$(cat <<'EOF'
    <Summary of what was accomplished, any important decisions made, and verification performed>
    EOF
    )"

If blocked, update the issue status and explain:

    bd update <ISSUE-ID> --status blocked --notes "$(cat <<'EOF'
    <Clear explanation of what is blocking progress and what is needed to proceed>
    EOF
    )"
```

`beads-agent-review-prompt` old default began:
`"You are a code review agent. Please work on beads issue <ISSUE-ID>:
<ISSUE-TITLE>."` followed by the Constraints/Review Focus sections and
an `# Output` block with `bd update <ISSUE-ID> --notes …`.

`beads-agent-qa-prompt` old default began: `"You are a QA agent.
Please work on beads issue <ISSUE-ID>: <ISSUE-TITLE>."` followed by
the Constraints/QA Workflow sections and an `# Output` block with `bd
update <ISSUE-ID> --acceptance … --notes …`.

`beads-agent-plan-prompt` old default began: `"You are a planning
agent. Please work on beads issue <ISSUE-ID>: <ISSUE-TITLE>."` then
`"Create a detailed implementation plan WITHOUT making any code
changes."`, the Constraints/Planning Steps/Plan Review sections, and
an `# Output` block with `bd update <ISSUE-ID> --description … --design
… --acceptance … --notes …`.

The combined rendered prompt (system + blank line + user) for the
built-in template types still contains the same substituted issue id
and the same instruction content as before — only the delivery
channel split.

### Terminal backend registered (opt-in); efrit removed

`beads-agent-backend-claude` (the `claude` CLI spawned directly into a
terminal — collision-free by construction) is now **registered and
selectable**. It is **opt-in**: the per-type backend defcustoms
(`beads-agent-{task,review,plan,qa}-backend`) are **not** flipped.

> Net effect for default users in this release: `beads-agent.el`'s
> orchestrator `rename-buffer` is **not** patched and the per-type
> backend defcustom defaults are **not** flipped. A user who never
> customised their backend gets *exactly the bde-h93r behaviour after
> this PR as before it*. Only users who explicitly opt in via
> `(setq beads-agent-task-backend "claude")` are protected. The
> originating bug is *displaced*, not fixed, this release.

The `efrit` backend was **removed** (`beads-agent-efrit.el` and its
test deleted; the `require`, header comment, and `"efrit"` test
fixtures dropped). No deprecation alias.

`beads-reader-terminal` was added (completes over registered
terminals, returns the class symbol for `beads-agent-default-terminal`).
It resolves the class from the registered terminal *instance* rather
than reconstructing `beads-terminal-<name>`, so a third-party terminal
whose registered name differs from its class symbol resolves
correctly.

#### Two terminal knobs coexist (time-boxed)

For one release the `beads-terminal` group holds **two** knobs:

- `beads-terminal-backend` — *symbol* (`nil`/`vterm`/`eat`/`term`),
  governs one-shot `bd` command execution
  (`beads-command--run-in-terminal`).
- `beads-agent-default-terminal` — *class symbol* (default
  `beads-terminal-auto`), governs agent terminal spawning.

`beads-terminal--symbol->class` bridges the old vocabulary so the
Phase 3 unification (collapse onto one knob) is mechanical.

#### Per-backend system-prompt seam status

The Phase 2 spike requires reading the upstream source of each wrapper
package to confirm its system-prompt seam. In this build environment
**none of `claude-code-ide`, `claude-code`, `claudemacs`, `eca`, or
`agent-shell` is installed**, so no seam could be verified. Per the
plan's spike-gating rule, every wrapper backend therefore **holds the
Phase 1a-i concat shim** (system + blank line + user combined via
`beads-agent-backend--combine-prompt`) and ships unchanged. Wiring a
dedicated seam for any wrapper is deferred until its upstream source
can be verified. Only the terminal `beads-agent-backend-claude`
delivers the system prompt through a dedicated channel
(`--append-system-prompt`).

### New: `pi` terminal backend; ghostel `ghostel-exec` fix

`beads-agent-backend-pi` (the `pi` CLI spawned directly into a
terminal) is now **registered and selectable**, configuration-identical
to `beads-agent-backend-claude` (`--append-system-prompt` + positional
message). Like `claude` it is **opt-in** — the per-type backend
defcustoms are **not** flipped.

`beads-terminal-ghostel` now spawns through ghostel's public
`ghostel-exec` (PROGRAM + ARGS) instead of the single-shell
`ghostel-shell` defcustom, fixing a bug where ghostel tried to exec a
program literally named `"claude --append-system-prompt …"`. Its
priority dropped 15 → 5 so `auto` prefers ghostel → vterm → eat →
ansi-term → term.

> vterm trade-off: vterm has no argv-direct entry point, so
> `beads-terminal-vterm` joins the argv through
> `shell-quote-argument` and feeds it to `/bin/sh -c`. This is safe
> for shell metacharacters (including single quotes in a system
> prompt, which become correct POSIX `'…'"'"'…'` quoting), but the
> quoted form may display surprisingly inside the vterm buffer. Use
> ghostel/eat/term for argv-direct spawning if that matters.
