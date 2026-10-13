# beads.el — Magit-like interface for the Beads issue tracker

[![Test](https://github.com/r0man/beads.el/actions/workflows/test.yml/badge.svg)](https://github.com/r0man/beads.el/actions/workflows/test.yml)
[![Coverage](https://codecov.io/gh/r0man/beads.el/branch/main/graph/badge.svg)](https://codecov.io/gh/r0man/beads.el)

⚠️ Status: Experimental

beads.el is an Emacs package that brings the
[Beads](https://github.com/steveyegge/beads) issue tracker into Emacs. It is a
keyboard-driven, Magit-like porcelain over the `bd` command-line tool: every
view is rendered from `bd … --json`, and the whole `bd` surface is reachable
from transient menus without leaving the editor.

## Highlights

- 🪟 **Full board**: `M-x beads` opens a sectioned, Magit-like board rendered
  from `bd --json`.
- 📋 **List and detail views**: sortable tabulated lists and collapsible issue
  pages with clickable cross-references.
- ⌨️ **Transient menus**: one `beads-dispatch` menu on `?` plus a
  `beads-maintenance` menu on `!` cover the whole CLI.
- 💡 **Eldoc previews**: hover an issue reference anywhere in Emacs to see its
  details, without blocking.
- 🧩 **Extension seams**: documented magit/forge-style providers, hooks, and
  registries for downstream packages such as `gascity.el`.

## Requirements

- **Emacs** 29.1 or newer.
- **Dependencies** (from MELPA): `transient` 0.10.1+, `vui` 1.0.0+.
- **External**: the `bd` executable on `PATH`. `dolt` and `graphviz` are
  additionally required to run the full test suite.

## Installation

### Guix

The repository ships a `guix.scm` that provides a development environment with
every dependency:

```sh
guix shell -D -f guix.scm
```

To build and install the package itself:

```sh
guix build -f guix.scm
guix package -f guix.scm
```

### Autotools

beads.el uses the standard GNU build system:

```sh
./autogen.sh
./configure
make
make check
make install
```

`make check` runs the ERT suite. `configure` detects `bd`, `dolt`, and
`graphviz`; when they are absent it runs the `:unit` tests only. Pass
`--with-bd=no` (and `--with-dolt=no`, `--with-graphviz=no`) to force the
reduced run explicitly.

### Emacs

After installing, load the package and bind the entry point:

```elisp
(use-package beads
  :commands (beads beads-dispatch beads-list-all beads-ready
             beads-show beads-create)
  :bind ("C-c b" . beads)                 ; M-x beads opens the full board
  :hook (after-init . beads-eldoc-mode))  ; Enable eldoc support
```

## Quick start

1. Install the `bd` CLI ([Beads repository](https://github.com/steveyegge/beads))
   and make sure it is on `PATH`:

   ```sh
   bd version
   ```

2. Initialise Beads in your project (once):

   ```sh
   cd /path/to/project
   bd init
   ```

3. `M-x beads` opens the **board** (`beads-dashboard`), the single front door.
   Press `?` for the dispatch menu and `!` for the maintenance menu.

4. From the board: `l` lists all issues, `r` shows ready work, `c` creates an
   issue, `RET` shows the issue at point, and `g` refreshes.

## Documentation

The full user manual is built from the Texinfo sources under `doc/`:

- **HTML**: <https://r0man.github.io/beads.el/>
- **Info**: after `make install`, press `C-h i` and choose the **Beads** node
  (or `M-x info-display-manual RET beads`).
- **Generated command reference**:
  [`docs/dispatch-reference.md`](docs/dispatch-reference.md).
- **Key contract**: [`docs/key-registry.md`](docs/key-registry.md).
- **Internals and extension seams**: [`AGENTS.md`](AGENTS.md).

### Dispatch menu (`?`)

The dispatch menu is the single command menu in every beads buffer; `!` opens
the maintenance menu. The table below is generated from the menu definition by
`scripts/dispatch-docs`; do not edit it by hand. Run
`scripts/dispatch-docs --write` after changing a menu — `scripts/dispatch-docs
--check` (what CI runs) fails when the code and this table disagree.

<!-- BEGIN GENERATED: dispatch menu -->
#### Dispatch menu (`?`)

##### Issues

| Key | Command | Description |
| --- | --- | --- |
| `l` | `beads-list-all` | List beads |
| `c` | `beads-compose-create` | Create bead |
| `/` | `beads-search` | Search |
| `i` | `beads-show` | Show... |
| `u` | `beads-update` | Update bead |
| `x` | `beads-close` | Close bead |
| `o` | `beads-reopen` | Reopen bead |
| `e` | `beads-edit` | Edit field |

##### Workflow

| Key | Command | Description |
| --- | --- | --- |
| `r` | `beads-ready` | Ready work |
| `b` | `beads-blocked` | Blocked work |
| `d` | `beads-dep` | Dependencies |
| `#` | `beads-actions-set-priority` | Priority |
| `C` | `beads-actions-claim` | Claim |
| `s` | `beads-actions-set-status` | Set status |
| `m` | `beads-mol` | Molecules |
| `G` | `beads-gate` | Gates |
| `w` | `beads-wisp-list` | Wisps |
| `S` | `beads-swarm-list-view` | Swarms |
| `T` | `beads-types` | Types |

##### Views

| Key | Command | Description |
| --- | --- | --- |
| `a` | `beads-agent` | Agents... |
| `F` | `beads-formula-menu` | Formulas... |
| `v` | `beads-graph-all` | Graph |
| `D` | `beads-dashboard` | Dashboard |
| `H` | `beads-history` | History |
| `R` | `beads-events-timeline` | Recent changes |
| `Y` | `beads-orphans` | Orphans |
| `t` | `beads-stats` | Stats |
| `X` | `beads-context` | Context |

##### Manage

| Key | Command | Description |
| --- | --- | --- |
| `L` | `beads-label-menu` | Labels... |
| `k` | `beads-dolt` | Dolt... |
| `.` | `beads-config` | Config |
| `W` | `beads-worktree-menu` | Worktrees... |

##### Maintenance

| Key | Command | Description |
| --- | --- | --- |
| `!` | `beads-maintenance` | Maintenance... |
| `g` | `beads-refresh-menu` | Refresh cache |
| `q` | `transient-quit-one` | Quit |
<!-- END GENERATED: dispatch menu -->

The full, test-enforced key contract lives in `docs/key-registry.md`.

## Contributing

Contributions are welcome.

1. Add tests for new features and run `make check` before submitting.
2. Follow the conventions in [`AGENTS.md`](AGENTS.md): `beads-` for public and
   `beads--` for internal names, docstrings on public functions, 80-column
   code.
3. Update the manual under `doc/`. When you change a transient menu, run
   `scripts/dispatch-docs --write` so the generated tables stay in sync.

## License

beads.el is free software, distributed under the terms of the **GNU General
Public License, version 3 or later** (GPLv3-or-later). See [`LICENSE`](LICENSE).
