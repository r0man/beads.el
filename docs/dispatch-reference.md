# Dispatch reference

This file is generated from the transient prefix definitions by
`scripts/dispatch-docs`.  Do not edit it by hand: run
`scripts/dispatch-docs --write` to regenerate it, and the test
suite fails when the checked-in copy and the code disagree.

Groups contributed at runtime through `beads-menu-providers' (for
example gascity.el's city group) are not listed here; only the
prefixes owned by beads.el are part of this contract.

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
| `*` | `beads-log` | Command log |
| `g` | `beads-refresh-menu` | Refresh cache |
| `q` | `transient-quit-one` | Quit |

#### Maintenance menu (`!`)

##### Issue Lifecycle

| Key | Command | Description |
| --- | --- | --- |
| `f` | `beads-defer` | Defer |
| `F` | `beads-undefer` | Undefer |
| `D` | `beads-delete` | Delete |
| `p` | `beads-promote` | Promote wisp |
| `R` | `beads-rename` | Rename |
| `u` | `beads-unclaim` | Unclaim |

##### Views & Reports

| Key | Command | Description |
| --- | --- | --- |
| `c` | `beads-count` | Count |
| `t` | `beads-stats` | Stats |
| `s` | `beads-stale` | Stale |
| `T` | `beads-types` | Types |
| `l` | `beads-lint` | Lint |
| `O` | `beads-find-duplicates` | Find duplicates |
| `o` | `beads-orphans` | Orphans |
| `$` | `beads-wisp-list` | Wisps |
| `*` | `beads-log` | Command log |

##### Issue Details

| Key | Command | Description |
| --- | --- | --- |
| `a` | `beads-children` | Children |
| `=` | `beads-comments-menu` | Comments |
| `[` | `beads-todo` | Todo |
| `e` | `beads-events` | Events |
| `/` | `beads-query` | Query |

##### Events (live)

| Key | Command | Description |
| --- | --- | --- |
| `,` | `beads-events-timeline` | Recent changes |
| `.` | `beads-events-timeline-city` | City timeline |
| `#` | `beads-live-toggle` | Toggle live |

##### Events (time travel)

| Key | Command | Description |
| --- | --- | --- |
| `-` | `beads-events-history` | Issue history |
| `^` | `beads-events-rewind` | Rewind... |

##### Workflow

| Key | Command | Description |
| --- | --- | --- |
| `g` | `beads-gate` | Gate |
| `w` | `beads-swarm` | Swarm |
| `K` | `beads-cook` | Cook |
| `H` | `beads-ship` | Ship |
| `z` | `beads-set-state` | Set state |
| `Z` | `beads-state-menu` | State menu |
| `C` | `beads-conflicts` | Conflicts |
| `r` | `beads-reclaim` | Reclaim |
| `B` | `beads-heartbeat` | Heartbeat |

##### Database

| Key | Command | Description |
| --- | --- | --- |
| `b` | `beads-backup` | Backup |
| `E` | `beads-export` | Export JSONL |
| `P` | `beads-prune` | Prune closed |
| `G` | `beads-gc` | GC |
| `d` | `beads-batch` | Batch ops |
| `h` | `beads-compact` | Compact |
| `i` | `beads-flatten` | Flatten |
| `j` | `beads-ping` | Ping |
| `+` | `beads-doctor` | Doctor |
| `M` | `beads-migrate-menu` | Migrate... |
| `U` | `beads-upgrade` | Upgrade |
| `!` | `beads-preflight` | Preflight |
| `8` | `beads-purge` | Purge |

##### Structure

| Key | Command | Description |
| --- | --- | --- |
| `1` | `beads-duplicate` | Mark duplicate |
| `2` | `beads-duplicates` | Find duplicates |
| `3` | `beads-supersede` | Supersede |
| `4` | `beads-restore` | Restore |
| `J` | `beads-sql` | SQL query |
| `k` | `beads-rename-prefix` | Rename prefix |
| `_` | `beads-forget` | Forget memory |

##### Data & Sync

| Key | Command | Description |
| --- | --- | --- |
| `V` | `beads-vc` | VC |
| `Y` | `beads-sync` | Dolt sync |
| `S` | `beads-schema` | Schema |
| `m` | `beads-migrate-personal` | Migrate personal |
| `<` | `beads-branch` | Branch |
| ``` | `beads-diff` | Diff |
| `%` | `beads-history` | History |
| `&` | `beads-provenance` | Provenance |

##### Integrations

| Key | Command | Description |
| --- | --- | --- |
| `n` | `beads-integrations-menu` | Integrations... |

##### Setup

| Key | Command | Description |
| --- | --- | --- |
| `I` | `beads-setup-menu` | Setup & migration... |

##### Actions

| Key | Command | Description |
| --- | --- | --- |
| `q` | `transient-quit-one` | Quit |

#### Integrations menu

##### Integrations

| Key | Command | Description |
| --- | --- | --- |
| `n` | `beads-jira` | Jira |
| `N` | `beads-linear` | Linear |
| `y` | `beads-gitlab` | GitLab |
| `9` | `beads-github` | GitHub |
| `v` | `beads-repo` | Repo |
| `A` | `beads-ado` | ADO |
| `>` | `beads-federation` | Federation |
| `*` | `beads-mail` | Mail delegate |

##### Actions

| Key | Command | Description |
| --- | --- | --- |
| `q` | `transient-quit-one` | Quit |

#### Setup menu

##### Setup

| Key | Command | Description |
| --- | --- | --- |
| `I` | `beads-init` | Init project |
| `x` | `beads-init-safety` | Init safety |
| `;` | `beads-setup` | Setup integrations |
| `:` | `beads-info` | Info |
| `'` | `beads-version` | Version |
| `L` | `beads-bootstrap` | Bootstrap |
| `Q` | `beads-hooks` | Hooks... |
| `W` | `beads-quickstart` | Quickstart |
| `X` | `beads-where` | Where |
| `0` | `beads-context` | Context |

##### Memory

| Key | Command | Description |
| --- | --- | --- |
| `|` | `beads-memories` | Memories |
| `5` | `beads-kv` | KV store |
| `~` | `beads-prime` | Prime |
| `{` | `beads-recall` | Recall memory |
| `}` | `beads-remember` | Remember |

##### Admin

| Key | Command | Description |
| --- | --- | --- |
| `@` | `beads-audit` | Audit |
| `6` | `beads-admin` | Admin |
| `7` | `beads-worktree-menu` | Worktrees... |

##### Actions

| Key | Command | Description |
| --- | --- | --- |
| `q` | `transient-quit-one` | Quit |

Generated by `beads-transient-doc-markdown'.
