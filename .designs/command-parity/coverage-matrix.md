# Full Coverage Matrix — bd CLI ↔ beads.el

> Generated 20260604-184038 against `bd 1.0.5 (dev)`. Source data:
> `.cli-audit/20260604-184038/{commands,classes}.json` + `flags.md`.
> Status: **SUPPORTED** = class exists, slots clean · **PARTIAL** = class
> exists, ≥1 slot-drift finding · **MISSING-GROUP** = router group, no class
> by policy · **MISSING-LEAF** = leaf command with no class.

| bd command | Status | beads.el symbol | File | Summary |
|------------|--------|-----------------|------|---------|
| `admin.cleanup` | SUPPORTED | `beads-command-admin-cleanup` | lisp/beads-command-admin.el | Delete closed issues to reduce database size |
| `admin.compact` | PARTIAL | `beads-command-compact-stats` | lisp/beads-command-compact.el | Compact old closed issues to save space |
| `admin` | MISSING-GROUP | `—` | — | Administrative commands for database maintenance |
| `admin.reset` | SUPPORTED | `beads-command-admin-reset` | lisp/beads-command-admin.el | Remove all beads data and configuration |
| `ado` | MISSING-GROUP | `—` | — | Azure DevOps integration commands |
| `ado.projects` | SUPPORTED | `beads-command-ado-projects` | lisp/beads-command-integrations.el | List accessible Azure DevOps projects |
| `ado.pull` | SUPPORTED | `beads-command-ado-pull` | lisp/beads-command-integrations.el | Pull specific items from Azure DevOps |
| `ado.push` | SUPPORTED | `beads-command-ado-push` | lisp/beads-command-integrations.el | Push specific beads to Azure DevOps |
| `ado.status` | SUPPORTED | `beads-command-ado-status` | lisp/beads-command-integrations.el | Show Azure DevOps sync status |
| `ado.sync` | SUPPORTED | `beads-command-ado-sync` | lisp/beads-command-integrations.el | Sync issues with Azure DevOps |
| `assign` | SUPPORTED | `beads-command-assign` | lisp/beads-command-misc.el | Assign an issue to someone |
| `audit.label` | SUPPORTED | `beads-command-audit-label` | lisp/beads-command-audit.el | Append a label entry referencing an existing interaction |
| `audit` | MISSING-GROUP | `—` | — | Record and label agent interactions (append-only JSONL) |
| `audit.record` | SUPPORTED | `beads-command-audit-record` | lisp/beads-command-audit.el | Append an audit interaction entry |
| `backup.init` | SUPPORTED | `beads-command-backup-init` | lisp/beads-command-misc.el | Set up a Dolt backup destination |
| `backup.remove` | SUPPORTED | `beads-command-backup-remove` | lisp/beads-command-misc.el | Remove the configured backup destination |
| `backup.restore` | SUPPORTED | `beads-command-backup-restore` | lisp/beads-command-misc.el | Restore database from a Dolt backup |
| `backup.status` | SUPPORTED | `beads-command-backup-status` | lisp/beads-command-misc.el | Show last backup status |
| `backup` | SUPPORTED | `beads-command-backup` | lisp/beads-command-misc.el | Back up your beads database |
| `backup.sync` | SUPPORTED | `beads-command-backup-sync` | lisp/beads-command-misc.el | Push database to configured Dolt backup |
| `batch` | SUPPORTED | `beads-command-batch` | lisp/beads-command-batch.el | Run multiple write operations in a single database transaction |
| `blocked` | SUPPORTED | `beads-command-blocked` | lisp/beads-command-blocked.el | Show blocked issues |
| `bootstrap` | SUPPORTED | `beads-command-bootstrap` | lisp/beads-command-init.el | Non-destructive database setup for fresh clones and recovery |
| `branch` | SUPPORTED | `beads-command-branch` | lisp/beads-command-branch.el | List or create branches |
| `children` | SUPPORTED | `beads-command-children` | lisp/beads-command-misc.el | List child beads of a parent |
| `close` | SUPPORTED | `beads-command-close` | lisp/beads-command-close.el | Close one or more issues |
| `comments.add` | SUPPORTED | `beads-command-comments-add` | lisp/beads-command-comments.el | Add a comment to an issue |
| `comments.list` | MISSING-LEAF | `—` | — | Invalid — use bd comments <issue-id> to list comments |
| `comments` | SUPPORTED | `beads-command-comments` | lisp/beads-command-comments.el | View or manage comments on an issue |
| `comment` | SUPPORTED | `beads-command-comment` | lisp/beads-command-misc.el | Add a comment to an issue |
| `compact` | SUPPORTED | `beads-command-compact` | lisp/beads-command-compact.el | Squash old Dolt commits to reduce history size |
| `config.apply` | SUPPORTED | `beads-command-config-apply` | lisp/beads-command-config.el | Reconcile system state to match configuration |
| `config.drift` | SUPPORTED | `beads-command-config-drift` | lisp/beads-command-config.el | Detect config-vs-reality inconsistencies |
| `config.get` | SUPPORTED | `beads-command-config-get` | lisp/beads-command-config.el | Get a configuration value |
| `config.list` | SUPPORTED | `beads-command-config-list` | lisp/beads-command-config.el | List all configuration |
| `config` | MISSING-GROUP | `—` | — | Manage configuration settings |
| `config.set-many` | SUPPORTED | `beads-command-config-set-many` | lisp/beads-command-config.el | Set multiple configuration values in one operation |
| `config.set` | SUPPORTED | `beads-command-config-set` | lisp/beads-command-config.el | Set a configuration value |
| `config.show` | SUPPORTED | `beads-command-config-show` | lisp/beads-command-config.el | Show all effective configuration with provenance |
| `config.unset` | SUPPORTED | `beads-command-config-unset` | lisp/beads-command-config.el | Delete a configuration value |
| `config.validate` | SUPPORTED | `beads-command-config-validate` | lisp/beads-command-config.el | Validate sync-related configuration |
| `context` | SUPPORTED | `beads-command-context` | lisp/beads-command-misc.el | Show effective backend identity and repository context |
| `cook` | SUPPORTED | `beads-command-cook` | lisp/beads-command-misc.el | Compile a formula into a proto (ephemeral by default) |
| `count` | SUPPORTED | `beads-command-count` | lisp/beads-command-count.el | Count issues matching filters |
| `create-form` | SUPPORTED | `beads-command-create-form` | lisp/beads-command-misc.el | Create a new issue using an interactive form |
| `create` | SUPPORTED | `beads-command-create` | lisp/beads-command-create.el | Create a new issue (or batch from markdown/graph JSON) |
| `defer` | SUPPORTED | `beads-command-defer` | lisp/beads-command-defer.el | Defer one or more issues for later |
| `delete` | SUPPORTED | `beads-command-delete` | lisp/beads-command-delete.el | Delete one or more issues and clean up references |
| `dep.add` | PARTIAL | `beads-command-dep-add` | lisp/beads-command-dep.el | Add a dependency |
| `dep.cycles` | SUPPORTED | `beads-command-dep-cycles` | lisp/beads-command-dep.el | Detect dependency cycles |
| `dep.list` | SUPPORTED | `beads-command-dep-list` | lisp/beads-command-dep.el | List dependencies or dependents of one or more issues |
| `dep` | MISSING-GROUP | `—` | — | Manage dependencies |
| `dep.relate` | SUPPORTED | `beads-command-dep-relate` | lisp/beads-command-dep.el | Create a bidirectional relates_to link between issues |
| `dep.remove` | SUPPORTED | `beads-command-dep-remove` | lisp/beads-command-dep.el | Remove a dependency |
| `dep.tree` | SUPPORTED | `beads-command-dep-tree` | lisp/beads-command-dep.el | Show dependency tree |
| `dep.unrelate` | SUPPORTED | `beads-command-dep-unrelate` | lisp/beads-command-dep.el | Remove a relates_to link between issues |
| `diff` | SUPPORTED | `beads-command-diff` | lisp/beads-command-diff.el | Show changes between two commits or branches |
| `doctor` | PARTIAL | `beads-command-doctor` | lisp/beads-command-doctor.el | Check and fix beads installation health (start here) |
| `dolt.clean-databases` | SUPPORTED | `beads-command-dolt-clean-databases` | lisp/beads-command-dolt.el | Drop stale test databases from the Dolt server |
| `dolt.commit` | SUPPORTED | `beads-command-dolt-commit` | lisp/beads-command-dolt.el | Create a Dolt commit from pending changes |
| `dolt.killall` | SUPPORTED | `beads-command-dolt-killall` | lisp/beads-command-dolt.el | Kill all orphan Dolt server processes |
| `dolt` | MISSING-GROUP | `—` | — | Configure Dolt database settings |
| `dolt.pull` | SUPPORTED | `beads-command-dolt-pull` | lisp/beads-command-dolt.el | Pull commits from Dolt remote |
| `dolt.push` | SUPPORTED | `beads-command-dolt-push` | lisp/beads-command-dolt.el | Push commits to Dolt remote |
| `dolt.remote.add` | SUPPORTED | `beads-command-dolt-remote-add` | lisp/beads-command-dolt.el | Add a Dolt remote (both SQL server and CLI) |
| `dolt.remote.list` | SUPPORTED | `beads-command-dolt-remote-list` | lisp/beads-command-dolt.el | List configured Dolt remotes (SQL server + CLI) |
| `dolt.remote` | MISSING-GROUP | `—` | — | Manage Dolt remotes |
| `dolt.remote.remove` | SUPPORTED | `beads-command-dolt-remote-remove` | lisp/beads-command-dolt.el | Remove a Dolt remote (both SQL server and CLI) |
| `dolt.set` | SUPPORTED | `beads-command-dolt-set` | lisp/beads-command-dolt.el | Set a Dolt configuration value |
| `dolt.show` | SUPPORTED | `beads-command-dolt-show` | lisp/beads-command-dolt.el | Show current Dolt configuration with connection status |
| `dolt.start` | SUPPORTED | `beads-command-dolt-start` | lisp/beads-command-dolt.el | Start the Dolt SQL server for this project |
| `dolt.status` | SUPPORTED | `beads-command-dolt-status` | lisp/beads-command-dolt.el | Show Dolt engine status |
| `dolt.stop` | SUPPORTED | `beads-command-dolt-stop` | lisp/beads-command-dolt.el | Stop the Dolt SQL server for this project |
| `dolt.test` | SUPPORTED | `beads-command-dolt-test` | lisp/beads-command-dolt.el | Test connection to Dolt server |
| `duplicates` | SUPPORTED | `beads-command-duplicates` | lisp/beads-command-misc.el | Find and optionally merge duplicate issues |
| `duplicate` | SUPPORTED | `beads-command-duplicate` | lisp/beads-command-misc.el | Mark an issue as a duplicate of another |
| `edit` | SUPPORTED | `beads-command-edit` | lisp/beads-command-edit.el | Edit an issue field in $EDITOR |
| `epic.close-eligible` | SUPPORTED | `beads-command-epic-close-eligible` | lisp/beads-command-epic.el | Close epics where all children are complete |
| `epic` | MISSING-GROUP | `—` | — | Epic management commands |
| `epic.status` | SUPPORTED | `beads-command-epic-status` | lisp/beads-command-epic.el | Show epic completion status |
| `export` | SUPPORTED | `beads-command-export` | lisp/beads-command-misc.el | Export issues to JSONL format |
| `federation.add-peer` | SUPPORTED | `beads-command-federation-add-peer` | lisp/beads-command-federation.el | Add a federation peer with optional SQL credentials |
| `federation.list-peers` | SUPPORTED | `beads-command-federation-list-peers` | lisp/beads-command-federation.el | List configured federation peers |
| `federation` | MISSING-GROUP | `—` | — | Manage peer-to-peer federation with other workspaces |
| `federation.remove-peer` | SUPPORTED | `beads-command-federation-remove-peer` | lisp/beads-command-federation.el | Remove a federation peer |
| `federation.status` | SUPPORTED | `beads-command-federation-status` | lisp/beads-command-federation.el | Show federation sync status |
| `federation.sync` | SUPPORTED | `beads-command-federation-sync` | lisp/beads-command-federation.el | Synchronize with a peer town |
| `find-duplicates` | SUPPORTED | `beads-command-find-duplicates` | lisp/beads-command-misc.el | Find semantically similar issues using text analysis or AI |
| `flatten` | SUPPORTED | `beads-command-flatten` | lisp/beads-command-misc.el | Squash all Dolt history into a single commit |
| `forget` | SUPPORTED | `beads-command-forget` | lisp/beads-command-misc.el | Remove a persistent memory |
| `formula.convert` | SUPPORTED | `beads-command-formula-convert` | lisp/beads-command-formula.el | Convert formula from JSON to TOML |
| `formula.list` | SUPPORTED | `beads-command-formula-list` | lisp/beads-command-formula.el | List available formulas |
| `formula` | MISSING-GROUP | `—` | — | Manage workflow formulas |
| `formula.show` | SUPPORTED | `beads-command-formula-show` | lisp/beads-command-formula.el | Show formula details |
| `gate.add-waiter` | SUPPORTED | `beads-command-gate-add-waiter` | lisp/beads-command-gate.el | Add a waiter to a gate |
| `gate.check` | SUPPORTED | `beads-command-gate-check` | lisp/beads-command-gate.el | Evaluate gates and close resolved ones |
| `gate.create` | SUPPORTED | `beads-command-gate-create` | lisp/beads-command-gate.el | Create a gate that blocks an issue |
| `gate.discover` | SUPPORTED | `beads-command-gate-discover` | lisp/beads-command-gate.el | Discover await_id for gh:run gates |
| `gate.list` | SUPPORTED | `beads-command-gate-list` | lisp/beads-command-gate.el | List gate issues |
| `gate` | MISSING-GROUP | `—` | — | Manage async coordination gates |
| `gate.resolve` | SUPPORTED | `beads-command-gate-resolve` | lisp/beads-command-gate.el | Manually resolve (close) a gate |
| `gate.show` | SUPPORTED | `beads-command-gate-show` | lisp/beads-command-gate.el | Show a gate issue |
| `gc` | SUPPORTED | `beads-command-gc` | lisp/beads-command-misc.el | Garbage collect: decay old issues, compact Dolt commits, run Dolt GC |
| `github` | MISSING-GROUP | `—` | — | GitHub integration commands |
| `github.pull` | SUPPORTED | `beads-command-github-pull` | lisp/beads-command-integrations.el | Pull specific items from GitHub |
| `github.push` | SUPPORTED | `beads-command-github-push` | lisp/beads-command-integrations.el | Push specific beads to GitHub |
| `github.repos` | SUPPORTED | `beads-command-github-repos` | lisp/beads-command-integrations.el | List accessible GitHub repositories |
| `github.status` | SUPPORTED | `beads-command-github-status` | lisp/beads-command-integrations.el | Show GitHub sync status |
| `github.sync` | SUPPORTED | `beads-command-github-sync` | lisp/beads-command-integrations.el | Sync issues with GitHub |
| `gitlab` | MISSING-GROUP | `—` | — | GitLab integration commands |
| `gitlab.projects` | SUPPORTED | `beads-command-gitlab-projects` | lisp/beads-command-integrations.el | List accessible GitLab projects |
| `gitlab.pull` | SUPPORTED | `beads-command-gitlab-pull` | lisp/beads-command-integrations.el | Pull specific items from GitLab |
| `gitlab.push` | SUPPORTED | `beads-command-gitlab-push` | lisp/beads-command-integrations.el | Push specific beads to GitLab |
| `gitlab.status` | SUPPORTED | `beads-command-gitlab-status` | lisp/beads-command-integrations.el | Show GitLab sync status |
| `gitlab.sync` | SUPPORTED | `beads-command-gitlab-sync` | lisp/beads-command-integrations.el | Sync issues with GitLab |
| `graph.check` | SUPPORTED | `beads-command-graph-check` | lisp/beads-command-graph.el | Check dependency graph integrity |
| `graph` | SUPPORTED | `beads-command-graph` | lisp/beads-command-graph.el | Display issue dependency graph |
| `history` | SUPPORTED | `beads-command-history` | lisp/beads-command-history.el | Show version history for an issue |
| `hooks.install` | SUPPORTED | `beads-command-hooks-install` | lisp/beads-command-hooks.el | Install bd git hooks |
| `hooks.list` | SUPPORTED | `beads-command-hooks-list` | lisp/beads-command-hooks.el | List installed git hooks status |
| `hooks` | MISSING-GROUP | `—` | — | Manage git hooks for beads integration |
| `hooks.run` | SUPPORTED | `beads-command-hooks-run` | lisp/beads-command-hooks.el | Execute a git hook (called by thin shims) |
| `hooks.uninstall` | SUPPORTED | `beads-command-hooks-uninstall` | lisp/beads-command-hooks.el | Uninstall bd git hooks |
| `human.dismiss` | SUPPORTED | `beads-command-human-dismiss` | lisp/beads-command-misc.el | Dismiss a human-needed bead |
| `human.list` | SUPPORTED | `beads-command-human-list` | lisp/beads-command-misc.el | List all human-needed beads |
| `human.respond` | SUPPORTED | `beads-command-human-respond` | lisp/beads-command-misc.el | Respond to a human-needed bead |
| `human.stats` | SUPPORTED | `beads-command-human-stats` | lisp/beads-command-misc.el | Show summary statistics for human-needed beads |
| `human` | SUPPORTED | `beads-command-human` | lisp/beads-command-misc.el | Show essential commands for human users |
| `import` | SUPPORTED | `beads-command-import` | lisp/beads-command-misc.el | Import issues from a JSONL file or stdin into the database |
| `info` | PARTIAL | `beads-command-info` | lisp/beads-command-info.el | Show database information |
| `init` | PARTIAL | `beads-command-init` | lisp/beads-command-init.el | Initialize bd in the current directory |
| `init-safety` | SUPPORTED | `beads-command-init-safety` | lisp/beads-command-init.el | Explain bd init flag semantics and the destroy-token format |
| `jira` | MISSING-GROUP | `—` | — | Jira integration commands |
| `jira.pull` | SUPPORTED | `beads-command-jira-pull` | lisp/beads-command-integrations.el | Pull specific items from Jira |
| `jira.push` | SUPPORTED | `beads-command-jira-push` | lisp/beads-command-integrations.el | Push specific beads to Jira |
| `jira.status` | SUPPORTED | `beads-command-jira-status` | lisp/beads-command-integrations.el | Show Jira sync status |
| `jira.sync` | SUPPORTED | `beads-command-jira-sync` | lisp/beads-command-integrations.el | Synchronize issues with Jira |
| `kv.clear` | SUPPORTED | `beads-command-kv-clear` | lisp/beads-command-misc.el | Delete a key-value pair |
| `kv.get` | SUPPORTED | `beads-command-kv-get` | lisp/beads-command-misc.el | Get a value by key |
| `kv.list` | SUPPORTED | `beads-command-kv-list` | lisp/beads-command-misc.el | List all key-value pairs |
| `kv.set` | SUPPORTED | `beads-command-kv-set` | lisp/beads-command-misc.el | Set a key-value pair |
| `kv` | SUPPORTED | `beads-command-kv` | lisp/beads-command-misc.el | Key-value store commands |
| `label.add` | SUPPORTED | `beads-command-label-add` | lisp/beads-command-label.el | Add a label to one or more issues |
| `label.list-all` | SUPPORTED | `beads-command-label-list-all` | lisp/beads-command-label.el | List all unique labels in the database |
| `label.list` | SUPPORTED | `beads-command-label-list` | lisp/beads-command-label.el | List labels for an issue |
| `label` | MISSING-GROUP | `—` | — | Manage issue labels |
| `label.propagate` | SUPPORTED | `beads-command-label-propagate` | lisp/beads-command-label.el | Propagate a label from a parent issue to all its children |
| `label.remove` | SUPPORTED | `beads-command-label-remove` | lisp/beads-command-label.el | Remove a label from one or more issues |
| `linear` | MISSING-GROUP | `—` | — | Linear integration commands |
| `linear.pull` | SUPPORTED | `beads-command-linear-pull` | lisp/beads-command-integrations.el | Pull specific items from Linear |
| `linear.push` | SUPPORTED | `beads-command-linear-push` | lisp/beads-command-integrations.el | Push specific beads to Linear |
| `linear.status` | SUPPORTED | `beads-command-linear-status` | lisp/beads-command-integrations.el | Show Linear sync status |
| `linear.sync` | PARTIAL | `beads-command-linear-sync` | lisp/beads-command-integrations.el | Synchronize issues with Linear |
| `linear.teams` | SUPPORTED | `beads-command-linear-teams` | lisp/beads-command-integrations.el | List available Linear teams |
| `link` | SUPPORTED | `beads-command-link` | lisp/beads-command-misc.el | Link two issues with a dependency |
| `lint` | SUPPORTED | `beads-command-lint` | lisp/beads-command-misc.el | Check issues for missing template sections |
| `list` | PARTIAL | `beads-command-list` | lisp/beads-command-list.el | List issues |
| `mail` | SUPPORTED | `beads-command-mail` | lisp/beads-command-misc.el | Delegate to mail provider (e.g., gt mail) |
| `memories` | SUPPORTED | `beads-command-memories` | lisp/beads-command-misc.el | List or search persistent memories |
| `merge-slot.acquire` | SUPPORTED | `beads-command-merge-slot-acquire` | lisp/beads-command-merge-slot.el | Acquire the merge slot |
| `merge-slot.check` | SUPPORTED | `beads-command-merge-slot-check` | lisp/beads-command-merge-slot.el | Check merge slot availability |
| `merge-slot.create` | SUPPORTED | `beads-command-merge-slot-create` | lisp/beads-command-merge-slot.el | Create a merge slot bead for the current rig |
| `merge-slot` | MISSING-GROUP | `—` | — | Manage merge-slot gates for serialized conflict resolution |
| `merge-slot.release` | SUPPORTED | `beads-command-merge-slot-release` | lisp/beads-command-merge-slot.el | Release the merge slot |
| `migrate.hooks` | PARTIAL | `beads-command-migrate-hooks` | lisp/beads-command-migrate.el | Plan or apply git hook migration to marker-managed format |
| `migrate.issues` | SUPPORTED | `beads-command-migrate-issues` | lisp/beads-command-migrate.el | Move issues between repositories |
| `migrate` | PARTIAL | `beads-command-migrate` | lisp/beads-command-migrate.el | Database migration commands |
| `migrate.schema` | MISSING-LEAF | `—` | — | Apply pending schema migrations (idempotent) |
| `migrate.sync` | PARTIAL | `beads-command-migrate-sync` | lisp/beads-command-migrate.el | Set up sync.branch workflow for multi-clone setups |
| `mol.bond` | SUPPORTED | `beads-command-mol-bond` | lisp/beads-command-mol.el | Bond two protos or molecules together |
| `mol.burn` | SUPPORTED | `beads-command-mol-burn` | lisp/beads-command-mol.el | Delete a molecule without creating a digest |
| `mol.current` | SUPPORTED | `beads-command-mol-current` | lisp/beads-command-mol.el | Show current position in molecule workflow |
| `mol.distill` | SUPPORTED | `beads-command-mol-distill` | lisp/beads-command-mol.el | Extract a formula from an existing epic |
| `mol.last-activity` | SUPPORTED | `beads-command-mol-last-activity` | lisp/beads-command-mol.el | Show last activity timestamp for a molecule |
| `mol` | MISSING-GROUP | `—` | — | Molecule commands (work templates) |
| `mol.pour` | SUPPORTED | `beads-command-mol-pour` | lisp/beads-command-mol.el | Instantiate a proto as a persistent mol (solid -> liquid) |
| `mol.progress` | SUPPORTED | `beads-command-mol-progress` | lisp/beads-command-mol.el | Show molecule progress summary |
| `mol.ready` | SUPPORTED | `beads-command-mol-ready` | lisp/beads-command-mol.el | Find molecules ready for gate-resume dispatch |
| `mol.seed` | SUPPORTED | `beads-command-mol-seed` | lisp/beads-command-mol.el | Verify formula accessibility |
| `mol.show` | SUPPORTED | `beads-command-mol-show` | lisp/beads-command-mol.el | Show molecule details |
| `mol.squash` | SUPPORTED | `beads-command-mol-squash` | lisp/beads-command-mol.el | Compress molecule execution into a digest |
| `mol.stale` | SUPPORTED | `beads-command-mol-stale` | lisp/beads-command-mol.el | Detect complete-but-unclosed molecules |
| `mol.wisp.create` | SUPPORTED | `beads-command-mol-wisp-create` | lisp/beads-command-mol.el | Instantiate a proto as a wisp (solid -> vapor) |
| `mol.wisp.gc` | SUPPORTED | `beads-command-mol-wisp-gc` | lisp/beads-command-mol.el | Garbage collect old/abandoned wisps |
| `mol.wisp.list` | SUPPORTED | `beads-command-mol-wisp-list` | lisp/beads-command-mol.el | List all wisps in current context |
| `mol.wisp` | SUPPORTED | `beads-command-mol-wisp` | lisp/beads-command-mol.el | Create or manage wisps (ephemeral molecules) |
| `note` | SUPPORTED | `beads-command-note` | lisp/beads-command-misc.el | Append a note to an issue |
| `notion.connect` | SUPPORTED | `beads-command-notion-connect` | lisp/beads-command-integrations.el | Connect bd to an existing Notion database or data source |
| `notion.init` | SUPPORTED | `beads-command-notion-init` | lisp/beads-command-integrations.el | Create a dedicated Beads database in Notion |
| `notion` | MISSING-GROUP | `—` | — | Notion integration commands |
| `notion.pull` | SUPPORTED | `beads-command-notion-pull` | lisp/beads-command-integrations.el | Pull specific items from Notion |
| `notion.push` | SUPPORTED | `beads-command-notion-push` | lisp/beads-command-integrations.el | Push specific beads to Notion |
| `notion.status` | SUPPORTED | `beads-command-notion-status` | lisp/beads-command-integrations.el | Show Notion sync status |
| `notion.sync` | SUPPORTED | `beads-command-notion-sync` | lisp/beads-command-integrations.el | Sync issues with Notion |
| `onboard` | SUPPORTED | `beads-command-onboard` | lisp/beads-command-misc.el | Display minimal snippet for agent instructions file |
| `orphans` | SUPPORTED | `beads-command-orphans` | lisp/beads-command-misc.el | Identify orphaned issues (referenced in commits but still open) |
| `ping` | SUPPORTED | `beads-command-ping` | lisp/beads-command-ping.el | Check database connectivity |
| `preflight` | PARTIAL | `beads-command-preflight` | lisp/beads-command-misc.el | Show PR readiness checklist |
| `prime` | PARTIAL | `beads-command-prime` | lisp/beads-command-misc.el | Output AI-optimized workflow context |
| `priority` | SUPPORTED | `beads-command-priority` | lisp/beads-command-misc.el | Set the priority of an issue |
| `promote` | SUPPORTED | `beads-command-promote` | lisp/beads-command-misc.el | Promote a wisp to a permanent bead |
| `prune` | SUPPORTED | `beads-command-prune` | lisp/beads-command-prune.el | Delete old closed beads to reclaim space and shrink exports |
| `purge` | SUPPORTED | `beads-command-purge` | lisp/beads-command-misc.el | Delete closed ephemeral beads to reclaim space |
| `q` | SUPPORTED | `beads-command-q` | lisp/beads-command-misc.el | Quick capture: create issue and output only ID |
| `query` | SUPPORTED | `beads-command-query` | lisp/beads-command-misc.el | Query issues using a simple query language |
| `quickstart` | SUPPORTED | `beads-command-quickstart` | lisp/beads-command-quickstart.el | Quick start guide for bd |
| `ready` | SUPPORTED | `beads-command-ready` | lisp/beads-command-ready.el | Show ready work (open, no active blockers) |
| `recall` | SUPPORTED | `beads-command-recall` | lisp/beads-command-misc.el | Retrieve a specific memory |
| `remember` | SUPPORTED | `beads-command-remember` | lisp/beads-command-misc.el | Store a persistent memory |
| `rename-prefix` | SUPPORTED | `beads-command-rename-prefix` | lisp/beads-command-misc.el | Rename the issue prefix for all issues in the database |
| `rename` | SUPPORTED | `beads-command-rename` | lisp/beads-command-misc.el | Rename an issue ID |
| `reopen` | SUPPORTED | `beads-command-reopen` | lisp/beads-command-reopen.el | Reopen one or more closed issues |
| `repo.add` | PARTIAL | `beads-command-repo-add` | lisp/beads-command-integrations.el | Add an additional repository to sync |
| `repo.list` | PARTIAL | `beads-command-repo-list` | lisp/beads-command-integrations.el | List all configured repositories |
| `repo` | MISSING-GROUP | `—` | — | Manage multiple repository configuration |
| `repo.remove` | PARTIAL | `beads-command-repo-remove` | lisp/beads-command-integrations.el | Remove a repository from sync configuration |
| `repo.sync` | PARTIAL | `beads-command-repo-sync` | lisp/beads-command-integrations.el | Manually trigger multi-repo sync |
| `restore` | PARTIAL | `beads-command-restore` | lisp/beads-command-restore.el | Restore full history of a compacted issue from Dolt history |
| `rules.audit` | SUPPORTED | `beads-command-rules-audit` | lisp/beads-command-misc.el | Scan rules for contradictions and merge opportunities |
| `rules.compact` | SUPPORTED | `beads-command-rules-compact` | lisp/beads-command-misc.el | Merge related rules into composites |
| `rules` | MISSING-GROUP | `—` | — | Audit and compact Claude rules |
| `search` | SUPPORTED | `beads-command-search` | lisp/beads-command-search.el | Search issues by text query |
| `set-state` | SUPPORTED | `beads-command-set-state` | lisp/beads-command-state.el | Set operational state (creates event + updates label) |
| `setup` | PARTIAL | `beads-command-setup` | lisp/beads-command-misc.el | Setup integration with AI editors |
| `ship` | SUPPORTED | `beads-command-ship` | lisp/beads-command-misc.el | Publish a capability for cross-project dependencies |
| `show` | PARTIAL | `beads-command-show` | lisp/beads-command-show.el | Show issue details |
| `sql` | SUPPORTED | `beads-command-sql` | lisp/beads-command-sql.el | Execute raw SQL against the beads database |
| `stale` | SUPPORTED | `beads-command-stale` | lisp/beads-command-stale.el | Show stale issues (not updated recently) |
| `state.list` | SUPPORTED | `beads-command-state-list` | lisp/beads-command-state.el | List all state dimensions on an issue |
| `state` | SUPPORTED | `beads-command-state` | lisp/beads-command-state.el | Query the current value of a state dimension |
| `statuses` | SUPPORTED | `beads-command-statuses` | lisp/beads-command-misc.el | List valid issue statuses |
| `status` | SUPPORTED | `beads-command-status` | lisp/beads-command-status.el | Show issue database overview and statistics |
| `supersede` | SUPPORTED | `beads-command-supersede` | lisp/beads-command-misc.el | Mark an issue as superseded by a newer one |
| `swarm.create` | SUPPORTED | `beads-command-swarm-create` | lisp/beads-command-swarm.el | Create a swarm molecule from an epic |
| `swarm.list` | SUPPORTED | `beads-command-swarm-list` | lisp/beads-command-swarm.el | List all swarm molecules |
| `swarm` | MISSING-GROUP | `—` | — | Swarm management for structured epics |
| `swarm.status` | SUPPORTED | `beads-command-swarm-status` | lisp/beads-command-swarm.el | Show current swarm status |
| `swarm.validate` | PARTIAL | `beads-command-swarm-validate` | lisp/beads-command-swarm.el | Validate epic structure for swarming |
| `tag` | SUPPORTED | `beads-command-tag` | lisp/beads-command-misc.el | Add a label to an issue |
| `todo.add` | SUPPORTED | `beads-command-todo-add` | lisp/beads-command-misc.el | Add a new TODO item |
| `todo.done` | SUPPORTED | `beads-command-todo-done` | lisp/beads-command-misc.el | Mark TODO(s) as done |
| `todo.list` | SUPPORTED | `beads-command-todo-list` | lisp/beads-command-misc.el | List TODO items |
| `todo` | SUPPORTED | `beads-command-todo` | lisp/beads-command-misc.el | Manage TODO items (convenience wrapper for task issues) |
| `types` | SUPPORTED | `beads-command-types` | lisp/beads-command-misc.el | List valid issue types |
| `undefer` | SUPPORTED | `beads-command-undefer` | lisp/beads-command-defer.el | Undefer one or more issues (restore to open) |
| `update` | SUPPORTED | `beads-command-update` | lisp/beads-command-update.el | Update one or more issues |
| `upgrade.ack` | SUPPORTED | `beads-command-upgrade-ack` | lisp/beads-command-misc.el | Acknowledge the current bd version |
| `upgrade.review` | SUPPORTED | `beads-command-upgrade-review` | lisp/beads-command-misc.el | Review changes since last bd version |
| `upgrade.status` | SUPPORTED | `beads-command-upgrade-status` | lisp/beads-command-misc.el | Check if bd version has changed |
| `upgrade` | SUPPORTED | `beads-command-upgrade` | lisp/beads-command-misc.el | Check and manage bd version upgrades |
| `vc.commit` | SUPPORTED | `beads-command-vc-commit` | lisp/beads-command-vc.el | Create a commit with all staged changes |
| `vc.merge` | SUPPORTED | `beads-command-vc-merge` | lisp/beads-command-vc.el | Merge a branch into the current branch |
| `vc` | MISSING-GROUP | `—` | — | Version control operations |
| `vc.status` | SUPPORTED | `beads-command-vc-status` | lisp/beads-command-vc.el | Show current branch and uncommitted changes |
| `version` | SUPPORTED | `beads-command-version` | lisp/beads-command-misc.el | Print version information |
| `where` | SUPPORTED | `beads-command-where` | lisp/beads-command-misc.el | Show active beads location |
| `worktree.create` | SUPPORTED | `beads-command-worktree-create` | lisp/beads-command-worktree.el | Create a worktree |
| `worktree.info` | SUPPORTED | `beads-command-worktree-info` | lisp/beads-command-worktree.el | Show worktree info for current directory |
| `worktree.list` | SUPPORTED | `beads-command-worktree-list` | lisp/beads-command-worktree.el | List all git worktrees |
| `worktree` | MISSING-GROUP | `—` | — | Manage git worktrees for parallel development |
| `worktree.remove` | SUPPORTED | `beads-command-worktree-remove` | lisp/beads-command-worktree.el | Remove a worktree with safety checks |
