# Cross-repo ownership — beads.el ↔ gascity.el seams

This is the WI-SF-19 ownership record for **REQ-SF-100**: the generic,
`bd`-driven machinery lives in beads.el, gascity.el consumes it, and
gascity.el keeps only `gc`-specific enrichment (city/rig scoping, live
sessions and pools, the run view).  It is the before/after hotspot list
the plan asks for, and the source of the machine-checked seam list in
`lisp/test/beads-cross-repo-ownership-test.el`.

The dependency is one-way and enforced: **beads.el never requires
gascity.el** (`beads.el` is usable with only `bd`).  The cross-repo edits
themselves (the gascity.el side) are tracked and executed in the
gascity.el rig, blocked on WI-SF-13, and are never written from the
beads.el worktree.  This document is the hand-off contract for that bead.

## The rule

| Layer | Owner | Examples |
|---|---|---|
| Generic `bd` machinery, read/decode, typed domain objects, typed var readers, validation, laundry/launch porcelain, terminal/tmux, faces, tabulated/section rendering | **beads.el** | `beads-formula-*`, `beads-command-formula-*`, `beads-terminal-*`, `beads-face-*` |
| `gc`-specific enrichment | **gascity.el** | city/rig scoping, rosters, live sessions/pools, the run view, `gc formula catalog` |
| The one agreed seam between them | beads.el | the symbols in the seam table below |

When a piece of beads-native logic is found inside gascity.el it is
moved here and gascity.el is reduced to a caller/shim.  New code must
not add a second implementation of anything the other side already has.

## Hotspot inventory (before → after)

| # | Hotspot in gascity.el | Duplicated beads-native logic | beads.el seam | After |
|---|---|---|---|---|
| 1 | `gascity-formula--enum-choices`, `gascity-formula--methodology`, `gascity-formula--enum-metadata-keys` (`gascity-formula.el` L328–360) | Resolve a var's allowed values: explicit `enum`, else the name → `metadata.gc.methodology` key mapping | `beads-formula-var-choices`, `beads-formula-methodology`, `beads-formula-enum-metadata-keys` | gascity calls the beads generic; the mapping table and fallback live in beads.el |
| 2 | `gascity-formula--validate-values`, `gascity-sling--missing-required-vars`, `gascity-formula--blank` / `--nonblank` (L604–670) | Client-side required/pattern validation of formula var values before a launch | `beads-formula-validate-vars`, `beads-formula-missing-required-vars` | gascity's footer warning and launch refusal both call the beads functions |
| 3 | `gascity-sling-formula--var-class`, `gascity-sling-formula--reader-classes`, `--read-file`/`--read-directory`/`--read-agent`/`--read-numeric`/`--read-string` (L921–1130) | Map a var's declared metadata (type, enum, name convention) to a reader kind | `beads-formula-var-reader`, `beads-formula-var-kind` | beads owns type → reader-kind inference; gascity maps the returned `:kind` to its transient infix class |
| 4 | `gascity-formula` class + `gascity-domain` decode (`gascity-domain.el`) | Decode `* formula show --json' into typed objects (name/vars/steps, metadata) | `beads-formula` + `beads-formula-var` + `beads-formula-from-json` | beads owns the generic JSON shape; gascity subclasses/specializes only its extra gc fields |
| 5 | `gascity-formula-catalog`, `gascity-formula-list`, `gascity-formula-recipe` + caches (L106–260) | Read a formula catalog/recipe and memoize it | `beads-command-formula-show`, `beads-command-formula-list`; **no cache seam** | the command classes are beads.el's; the city-scoped cache and `gc formula catalog` stay in gascity (gc-specific) |
| 6 | Terminal/tmux attach and scrolling | tmux probes, attach argv, status mirror, mouse, scroll | `beads-terminal-*` (`beads-terminal.el`, `beads-terminal-tmux.el`) | already moved by PR #67; gascity passes only the tmux `:socket` — **no action** |
| 7 | `gascity-sling-formula--var-infixes`, deterministic var-key assignment, recipe preview (L1132–1336) | Generate per-var transient infixes and a deterministic key layout | none yet (candidate for `beads-sling.el` under WI-SF-03/WI-SF-08) | **deferred**: the sling transient still lives in gascity.el until the beads sling stage lands |

Hotspots 1–4 are the dedup this WI activates; 5 is a partial (command
classes only); 6 is done; 7 is explicitly deferred to the WI that lands
the beads.el sling stage.

## The beads.el seam list

Every symbol below is owned by beads.el and resolves in the beads.el
worktree.  `lisp/test/beads-cross-repo-ownership-test.el` asserts both
that each resolves and that this table names it, so the contract cannot
drift silently.

| Seam | Kind | Purpose |
|---|---|---|
| `beads-formula-var-reader` | cl-defgeneric `(var &optional formula) → reader-spec` | declared metadata → reader kind |
| `beads-formula-var-kind` | defun `(var &optional formula) → symbol` | reader-kind shortcut |
| `beads-formula-var-choices` | cl-defgeneric `(var &optional formula) → list-or-nil` | enum + methodology choice resolution |
| `beads-formula-methodology` | cl-defgeneric `(formula) → alist-or-nil` | `metadata.gc.methodology` access |
| `beads-formula-validate-vars` | defun `(formula vars) → formula` | required/pattern validation (signals `user-error`) |
| `beads-formula-missing-required-vars` | defun `(formula vars) → list` | non-signaling missing-required list |
| `beads-formula-launch` | cl-defgeneric `(formula bead &optional vars)` | launch + follow |
| `beads-formula-launch-context` | EIEIO class | resolved launch value object |
| `beads-formula` | EIEIO class | typed formula (bd formula show) |
| `beads-formula-var` | EIEIO class | typed formula variable |
| `beads-command-formula-show` | EIEIO command | `bd formula show` |
| `beads-command-formula-list` | EIEIO command | `bd formula list` |
| `beads-sling-targets` | defun `(&optional bead) → list-of-target` | target discovery |
| `beads-sling-dispatch` | cl-defgeneric `(target bead prompt)` | launch dispatch |
| `beads-terminal-spawn` | cl-defgeneric `(terminal argv &optional name)` | spawn a terminal |
| `beads-terminal-attach` | defun `(session &optional socket dir store)` | attach to a session |

The generic seams 1–4 are the ones the gascity.el dedup bead consumes;
the rest are the stable porcelain and terminal seams the package already
exposes (`docs/ui-redesign.md` §4).

## gascity.el-rig hand-off

The linked gascity.el bead (blocks on WI-SF-13) should:

1. Delete the hotspot 1–2 bodies and call `beads-formula-var-choices`,
   `beads-formula-methodology`, `beads-formula-validate-vars`, and
   `beads-formula-missing-required-vars` instead.
2. Rewrite `gascity-sling-formula--var-class` to switch on
   `beads-formula-var-kind` and keep only its transient-infix mapping.
3. Keep `gascity-formula-catalog` / `list` caching and the run view —
   those are `gc`-specific enrichment.
4. Byte-compile gascity.el and run its tests against the published
   beads.el seam version, then record the before/after module-ownership
   list (this document, superseded by the gascity.el copy).

Acceptance (from the work item): gascity.el byte-compiles, its tests
pass against the new beads.el seams, and no duplicated helper body
remains.

## Verification

| Gate | Command |
|---|---|
| Seam behavior (unit) | `eldev test -f beads-formula-test.el` |
| Ownership guard (unit) | `eldev test -f beads-cross-repo-ownership-test.el` |
| Byte-compile / lint | `eldev compile`; `eldev -p -dtT lint` |

## Further reading

- `plans/beads-standalone-formulas/requirements.md` — REQ-SF-100.
- `plans/beads-standalone-formulas/decomposition.md` — WI-SF-19.
- `docs/ui-redesign.md` §4/§6 — the foundation seam list.
