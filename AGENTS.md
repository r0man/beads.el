# AGENTS.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

> `CLAUDE.md` is a gitignored symlink to `AGENTS.md`. Edit `AGENTS.md`; that is the tracked file.

## Project Overview

beads.el is an Emacs package providing a Magit-like transient-based UI for the [Beads](https://github.com/steveyegge/beads) issue tracker (`bd` CLI). It uses EIEIO classes, transient menus, tabulated-list-mode, and vui (for the dashboard) to expose all `bd` commands from within Emacs.

## Build & Test Commands

```bash
# Run all tests (CI form: packaged mode, verbose, trace)
eldev test
eldev -p -dtT test

# Run a single test file (-f matches against lisp/test/*.el)
eldev test -f beads-list-test.el
eldev test test/beads-list-test.el      # dwim: path is treated as a file pattern

# Run tests matching a name pattern (positional ERT selector; NOT `-p`)
eldev test beads-command-close

# Select by tag (ERT selector sexp)
eldev test '(tag :unit)'
eldev test '(not (tag :integration))'
eldev test -f beads-list-test.el '(tag :unit)'

# Coverage: source mode (-s) is required, undercover cannot instrument .elc.
# CI skips :integration tests here because they add no Elisp coverage.
eldev -s -dtT test -U coverage/codecov.json '(not (tag :integration))'

# Byte-compile / lint
eldev compile
eldev -p -dtT lint
```

`eldev` global flags (`-p`, `-s`, `-dtT`) go *before* the `test` subcommand; `-p` after `test` fails with "Unknown option". There is no `--coverage` option; use `-s` plus `-U FILE`.

External requirements for the full suite: `bd` (CI pins v1.0.5), `dolt`, `graphviz`, plus the Emacs packages `transient`, `sesman`, `vui`. Under Guix use `guix shell -D -f guix.scm`. `scripts/show-menu` launches `emacs -Q` with `lisp/` on the load-path and opens the main menu; `scripts/emacs-minimal` does the same inside a pure `guix shell` with the config in `share/emacs.d`.

CI runs tests on Emacs 29.4, 30.2, and snapshot; 29.4 is `continue-on-error` because of a known EIEIO byte-compilation bug with cross-file `cl-defmethod` dispatch, so a 29.4-only failure is not necessarily yours.

### Tests

Tests live in `lisp/test/` and are named `<module>-test.el`. Every `ert-deftest` carries a `:tags` list:

- `:unit` — mocks `beads-command-execute`; no `bd` needed. Most tests.
- `:integration` — runs the real `bd` against a temp repo. Often combined with `:slow`, `:transient`, `:error-recovery`, `:edge-case`.
- `:live` — drives a running `emacs --daemon` via `emacsclient` (see the header of `lisp/test/beads-live-ui-test.el` and `docs/live-ui-test-design.md`). Not run by `eldev test` in CI.

Infrastructure lives in `beads-test.el` (`beads-test-with-project`, `beads-test-with-shared-project`, `beads-test-with-transient-args`, `beads-test-with-mocked-interaction`, `beads-test-with-temp-config`, buffer/cache tracking) and `beads-integration-test.el` (`beads-test-with-temp-repo`, `beads-test-with-temp-repo-and-issues`). Integration tests should use these macros rather than calling `bd init` themselves.

Isolation: tests use bd's embedded Dolt engine, so each temp repo gets its own `.beads/embeddeddolt/` directory with true filesystem-level isolation and no shared sql-server or port plumbing. The macros unset every variable in `beads-test-isolation-env-vars` (`BEADS_DIR`, `BEADS_DOLT_PORT`, ...) so a value inherited from the outer shell (e.g. a Gas Town agent shell exporting `BEADS_DIR`) cannot reroute `bd` to a production store.

## Architecture

### EIEIO Command System (core pattern)

The central abstraction is `beads-defcommand` (in `beads-command.el`), a macro that generates from a single class definition:
1. An EIEIO class with typed slots
2. CLI argument serialization (`beads-command-line`)
3. A transient menu with infixes auto-derived from slot metadata (via `beads-meta-define-transient` in `beads-meta.el`)
4. Result type declarations (`:result`) for auto-parsing into `beads-types.el` objects

Class hierarchy: `beads-command` (abstract base) -> `beads-command-global-options` (adds `--actor`, `--db`, `--json` flags) -> concrete commands (`beads-command-list`, `beads-command-create`, `beads-command-close`, etc.).

Each `beads-command-<foo>.el` file in `lisp/` defines one bd subcommand as an EIEIO class. Macro keywords: `:cli-command` (override the subcommand string derived from the class name), `:result` (e.g. `beads-issue` or `(list-of beads-issue)`), `:json nil` for commands without `--json` (doctor, init), and `:transient` (`t` auto-generate, `nil` none, `:manual` a hand-written `transient-define-prefix` follows in the same file). The transient name is the class name with `-command-` stripped (`beads-command-close` -> `beads-close`).

Design rationale and decisions: `docs/defcommand-redesign.md`.

### Execution contract (`beads-command.el`)

Three generics, all dispatching on the command object; command objects are never mutated (execution clones them):

- `beads-command-execute` — synchronous. Forces `--json`, runs bd, parses through `beads-from-json` per `:result`, returns typed domain objects (or a raw string when `:json nil`). Signals `beads-validation-error`, `beads-command-error`, `beads-json-parse-error` (hierarchy in `beads-error.el`). Unit tests mock this.
- `beads-command-execute-interactive` — entry point for transient suffixes. Default runs the command in a `compilation-mode` buffer with human-readable output; commands override it to show/refresh buffers.
- `beads-command-execute-async` — non-blocking with `on-success`/`on-error` callbacks. Optional `:queue` enables the global concurrency cap (`beads-command-async-max-concurrent`), `:cache-key` coalesces concurrent identical requests. All dashboard loaders and eldoc go through this.

Remote (TRAMP) stores are supported. On a single-hop ssh-family store, async spawns run as a LOCAL `ssh -T` pipe process built by `beads-remote-ssh-command` (cd to the store, pure PATH fragment, no TRAMP round trip; `beads-remote-transport`), never a tramp-sh `make-process`, whose pty mux clients can deadlock the shared ssh master against TRAMP's waits. Other methods go through the TRAMP file handler (remote stderr discarded). `--db`/`--directory` are localized, and buffer names are qualified with the remote prefix (`beads-buffer.el`). `beads-remote.el` owns executable resolution, the PATH fragment and the ssh argv builders; gascity.el uses them too. Opening, rendering and folding a view must do no file I/O on a remote store: `lisp/test/beads-render-guard-test.el` enforces that with a file-name handler that signals on any non-name operation.

Store scoping: `beads-show`, `beads-ready`, `beads-blocked`, `beads-list-issues` and `beads-dashboard` take `:directory`, which becomes the buffer-local `beads-store-directory`; `beads-meta-build-global-options` adds `--directory` from it to every command whose slot is unset.

### Slot Metadata (`beads-meta.el`)

Custom EIEIO slot properties drive code generation. A single slot definition carries CLI properties (`:long-option`, `:short-option`, `:option-type`, `:positional`), transient properties (`:transient`, `:class`, `:reader`, `:choices`, `:group`, `:level`), and validation (`:required`, `:validator`). This avoids duplication between CLI serialization and UI.

### Data Types (`beads-types.el`)

EIEIO classes mirroring the Go structs from `beads/internal/types`: `beads-issue`, `beads-dependency`, `beads-label`, `beads-comment`, `beads-event`, `beads-statistics`, etc. All JSON parsing from `bd --json` output goes through these types. `beads-worktree-types.el` is a compatibility shim that re-exports `beads-types`.

### UI Layers

The UX is layered by frequency: the main dispatch (`beads.el`) -> `beads-ops-menu.el` (`!`, mid-frequency) -> `beads-advanced-menu.el` (`>`, maintenance/admin). Three interaction patterns coexist:

- **Pattern 1, context actions** (`beads-actions.el`): act on the issue at point or marked issues in list/show buffers with minimal prompting (`d` close, `C` claim, `s` status, `#` priority).
- **Pattern 2, transient menus**: switch-based prefixes with curated infixes (e.g. the magit-log-style filter menu in `beads-command-list.el`); most are auto-generated from command classes.
- **Pattern 3, compose buffers** (`beads-compose.el`): buffer-based create/edit/comment with `C-c C-c` to submit.

Modules:

- **`beads-command-list.el`** / **`beads-spec.el`**: Tabulated list mode with `beads-issue-spec` filter objects (status/type/priority/sort/limit) that convert to CLI args. `beads-pager.el` adds window-sized pagination to tabulated-list buffers.
- **`beads-command-show.el`** / **`beads-section.el`**: Issue detail view using magit-section-style rendering.
- **`beads-thing.el`**: the one movement scheme of every view: the `beads-thing` text property marks things; `beads-thing-forward`/`-backward` (TAB/S-TAB, wrap) and `beads-thing-toggle` (SPC); `beads-thing-define-keys` installs the keys. Tabulated rows are things without stamping.
- **`beads-dashboard.el`** / **`beads-dashboard-sections.el`**: vui-based project pulse buffer; each section loads asynchronously inside a `vui-error-boundary`. `beads-status` forwards here.
- **`beads-eldoc.el`**: Hover-to-preview issue references anywhere, async with per-store caching and negative caching. Issue ids are matched by the shared `beads-issue-id-regexp` (base-36 hash part) restricted by `beads-issue-id-prefixes`.
- **`beads-agent.el`** + backends: AI agent integration with sesman session management and git worktree isolation. A session = agent type (`beads-agent-type.el`: Task/Review/Plan/QA/Custom) + backend (`beads-agent-backend.el` registry: claude-code, claude-code-ide, claudemacs, eca, agent-shell, mock, terminal). The prompt protocol is split into a role-only **system** prompt (`beads-agent-type-system-prompt`) and an issue-envelope **user** prompt (`beads-agent-type-build-user-prompt`); `beads-agent-backend-start` is 4-arity `(backend issue system-prompt user-prompt)`.
- **`beads-terminal.el`** / **`beads-agent-backend-terminal.el`**: EIEIO terminal subsystem (vterm/ghostel/eat/ansi-term/term + auto + registry) and the opt-in collision-free `beads-agent-backend-claude` (spawns the CLI directly into a terminal). The `efrit` backend was removed.

### Key Files

| File | Role |
|------|------|
| `beads.el` | Entry point, main transient menu, core utilities |
| `beads-command.el` | `beads-defcommand` macro, base classes, sync/interactive/async execution |
| `beads-meta.el` | EIEIO slot property infrastructure, `beads-meta-define-transient`, parity policy constants |
| `beads-types.el` | Data model classes (issue, dependency, etc.) |
| `beads-option.el` | Global option variables and transient groups |
| `beads-state.el` | Ephemeral runtime state shared by command modules (breaks option/reader cycles) |
| `beads-custom.el` | All `defcustom` user configuration |
| `beads-util.el` | Low-level helpers; require this instead of `beads` to avoid circular deps |
| `beads-buffer.el` | Centralized buffer naming (`[PROJECT]`, `[PROJECT@BRANCH]`, `[REMOTE|PROJECT]`) |
| `beads-error.el` | Error condition hierarchy |
| `beads-spec.el` | Filter spec objects for list views |
| `beads-completion.el` | Completion tables (issue IDs, statuses, etc.) |
| `beads-reader.el` | Reader functions for transient infixes |
| `beads-git.el` | project.el root discovery, branch/worktree helpers, async git |
| `beads-sesman.el` | sesman integration for agent sessions |
| `beads-audit.el` | CLI-parity audit (see below) |

`MAGIT_PATTERNS.md` documents the Magit/Forge conventions the transients and sections follow. `NEWS.md` lists user-visible and API-breaking changes newest first under "Unreleased"; add an entry when you make one. The README's Architecture section is older than the code; prefer this file and the module commentaries.

## Conventions

- Public API: `beads-` prefix. Internal: `beads--` prefix.
- Each `bd` subcommand gets its own `beads-command-<name>.el` file.
- All bd commands use `--json` for structured output; UI never parses human-readable text.
- Transient menus are auto-generated from slot metadata where possible; use `:transient :manual` only when custom layout is needed.
- Define menus with `beads-define-prefix` / `beads-define-group` (`beads-prefix.el`), never bare `transient-define-prefix` / `transient-define-group`: the wrappers make every suffix run in the directory the menu was opened for, which is what keeps menus opened via `project-switch-project` working.  A custom prefix `:class` must derive from `beads-prefix`.
- Every EIEIO class and every slot carries a `:documentation` string.
- Dependencies: Emacs 29.1+, transient 0.10.1+, sesman 0.3.2+, vui 1.0.0+ (from MELPA). Declared in `lisp/beads.el` `Package-Requires` and mirrored in `Eldev` and `guix.scm`; keep all three in sync.
- Autoloads: `;;;###autoload` works directly on `beads-defcommand` and `beads-meta-define-transient` forms (the `Eldev` file preloads `beads-meta.el` for the autoload generator).

### Top-level group commands (`bd <group> <subcommand>`)

`bd` exposes 24 top-level *group* commands (`admin`, `ado`, `audit`,
`config`, `dep`, `dolt`, `epic`, `federation`, `formula`, `gate`,
`github`, `gitlab`, `hooks`, `jira`, `label`, `linear`, `merge-slot`,
`mol`, `notion`, `repo`, `rules`, `swarm`, `vc`, `worktree`). These
exist purely as routers for subcommands; running `bd <group>` with no
subcommand prints help. They take no positional arguments and no
non-global flags.

**Policy:** every top-level group gets a parent `transient-define-prefix`
menu, but **never** a `beads-defcommand` EIEIO class.

- The transient is named `beads-<group>` (or `beads-<group>-menu` when
  the unsuffixed name is reserved for a leaf — see
  `beads-command-label.el` and `beads-command-worktree.el`).
- Leaf subcommands are EIEIO classes via `beads-defcommand`; the parent
  transient simply binds keys to those leaves' transient suffixes.
- A `beads-defcommand` class for a router would serialize to e.g. `bd
  config` with no args, which prints help and exits — not a useful
  Emacs command. The transient menu *is* the parent UX.

CLI-coverage audits flag these groups under `coverage.md` >
`Missing Classes` > `Top-level group commands (N) — IN POLICY, no
action` (mirrored in `REPORT.md` > `Missing Classes` >
`Top-level group commands without a class`). That bucketing is
expected and in-policy; the audit script filters them out of the
actionable list. Mid-level groups (e.g. `dolt.remote`, `backup`
parents that route to subcommands) follow the same rule: parent
transient yes, EIEIO class no.

### Command-parity drift gate

`lisp/beads-audit.el` + `lisp/test/beads-audit-test.el` keep the command
classes in lockstep with the live `bd` CLI surface so drift cannot be
merged silently. The gate walks `bd <cmd> --help` (help only — never
touches the database) and introspects the class inventory live, then
fails CI on a **new unclassed command** or a **new slot gap**.
`M-x beads-audit-report` renders the same audit interactively.

The policy that decides what is *not* a gap lives as data in the
`beads-meta-parity-*` constants (`beads-meta.el`), not in prose: router
groups (`beads-meta-parity-router-groups`), non-goal commands/flags
(`-non-goal-commands`, `-non-goal-flags`), the `admin compact`
intentional multi-class cluster (`-intentional-collisions`, audited as
a union), and a baseline of accepted deferred drift (`-accepted-drift`).
When the gate fails, either add the missing class/slot or record the
intentional omission in the matching constant — the test failure names
the exact constant to edit. The `:integration` gate tests skip when
`bd` is absent; the `:unit` tests cover the diff logic without it.
Historical audit snapshots live in `.cli-audit/<timestamp>/` and the
design notes in `.designs/command-parity/`.

## Remote store testing (TRAMP)

beads.el already supports remote (TRAMP) stores; keep it that way. The
`bright-lights` city at `/home/roman/bright-lights` is the standing test
target: open it from a local Emacs as
`/ssh:localhost:/home/roman/bright-lights` (default user) and confirm bead views
(dashboard, list, show, slings, transient menus) work identically there.
Any change touching `default-directory` handling, process spawning, path
localization, or buffer-name keying must be verified over that TRAMP path,
not only locally.

End-to-end tests of user-facing flows run in a **fresh Emacs inside tmux**
(`tmux new-session -d -s e2e 'emacs'`, optionally `-Q` with `lisp/` on the
`load-path`) connected to the remote path above. ERT covers mocked units;
the tmux-Emacs TRAMP session is the acceptance gate for interactive
features.
