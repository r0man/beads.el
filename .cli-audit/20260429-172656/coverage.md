# Coverage Diff — bd CLI ↔ beads.el classes

Source: `commands.json` (259 paths) ↔ `classes.json` (216 classes, 212 unique cli_paths).

## Missing Classes

CLI commands with no `beads-defcommand` class. Bucketed for triage:

### Top-level group commands (24) — IN POLICY, no action

**Policy decision (bde-s84l.4, 2026-04-29):** top-level groups are
exposed via `transient-define-prefix` parent menus only — they get
**no** `beads-defcommand` class. See CLAUDE.md > Conventions >
"Top-level group commands". All 24 groups already have a parent
transient (`beads-<group>` or `beads-<group>-menu`); their absence
from `classes.json` is expected and should not be re-flagged.

- `admin` — Administrative commands for database maintenance
- `ado` — Azure DevOps integration commands
- `audit` — Record and label agent interactions (append-only JSONL)
- `config` — Manage configuration settings
- `dep` — Manage dependencies
- `dolt` — Configure Dolt database settings
- `epic` — Epic management commands
- `federation` — Manage peer-to-peer federation with other workspaces
- `formula` — Manage workflow formulas
- `gate` — Manage async coordination gates
- `github` — GitHub integration commands
- `gitlab` — GitLab integration commands
- `hooks` — Manage git hooks for beads integration
- `jira` — Jira integration commands
- `label` — Manage issue labels
- `linear` — Linear integration commands
- `merge-slot` — Manage merge-slot gates for serialized conflict resolution
- `mol` — Molecule commands (work templates)
- `notion` — Notion integration commands
- `repo` — Manage multiple repository configuration
- `rules` — Audit and compact Claude rules
- `swarm` — Swarm management for structured epics
- `vc` — Version control operations
- `worktree` — Manage git worktrees for parallel development

### Mid-level group subcommand (1) — IN POLICY, no action

- `dolt.remote` — Manage Dolt remotes (parent of `dolt.remote.add/list/remove`, all of which DO have classes). Same policy as top-level groups: parent transient `beads-dolt-remote` exists; no class needed.

### Top-level commands with no class (6)

These are real commands the user can run directly and have no Emacs
counterpart. Most likely warrant a new `beads-defcommand`.

- `batch` — Run multiple write operations in a single database transaction
- `compact` — Squash old Dolt commits to reduce history size (note: the existing `beads-command-compact-*` classes target `admin compact`, not this top-level command)
- `init-safety` — Explain bd init flag semantics and the destroy-token format
- `ping` — Check database connectivity
- `prune` — Delete old closed beads to reclaim space and shrink exports
- `audit` — listed above; ungrouped per CLI taxonomy

### Subcommands with no class (16)

Likely the highest-value gaps to fill.

- `ado.pull` — Pull specific items from Azure DevOps
- `ado.push` — Push specific beads to Azure DevOps
- `config.apply` — Reconcile system state to match configuration
- `config.drift` — Detect config-vs-reality inconsistencies
- `config.show` — Show all effective configuration with provenance
- `gate.create` — Create a gate that blocks an issue
- `github.pull` — Pull specific items from GitHub
- `github.push` — Push specific beads to GitHub
- `gitlab.pull` — Pull specific items from GitLab
- `gitlab.push` — Push specific beads to GitLab
- `graph.check` — Check dependency graph integrity
- `jira.pull` — Pull specific items from Jira
- `jira.push` — Push specific beads to Jira
- `linear.pull` — Pull specific items from Linear
- `linear.push` — Push specific beads to Linear
- `notion.pull` — Pull specific items from Notion
- `notion.push` — Push specific beads to Notion

## Orphan Classes

Classes whose `cli_path` is not present in `commands.json`.

_None._ Every `beads-defcommand` class resolves to a real `bd` subcommand.

## Path Mismatches / Collisions

### Multiple classes target the same CLI command

`admin.compact` (= `bd admin compact`) is the target of **5 classes**.
This appears intentional (the project splits the command's flag-modes
into separate UX classes), but means slot drift cannot be checked
1-to-1 against the CLI in the next step — each class only carries the
slots relevant to its mode.

| Class | File:line | `:cli-command` |
|-------|-----------|----------------|
| `beads-command-admin-compact` | lisp/beads-command-admin.el:76 | _(none — derived)_ |
| `beads-command-compact-stats` | lisp/beads-command-compact.el:46 | `"admin compact"` |
| `beads-command-compact-analyze` | lisp/beads-command-compact.el:63 | `"admin compact"` |
| `beads-command-compact-apply` | lisp/beads-command-compact.el:96 | `"admin compact"` |
| `beads-command-compact-auto` | lisp/beads-command-compact.el:141 | `"admin compact"` |

### Stale `:cli-command` overrides

_None detected._ Every class with an explicit `:cli-command` resolves
to a CLI path that exists in `commands.json`.

## Counts

- CLI commands enumerated: **259**
- Classes inventoried: **216**
- Unique class cli_paths: **212**
- Missing classes: **47** (24 top-level groups + 1 mid-level group + 6 top-level commands + 16 subcommands)
- Orphan classes: **0**
- Duplicate cli_path mappings: **1** (5 classes -> `admin.compact`)
