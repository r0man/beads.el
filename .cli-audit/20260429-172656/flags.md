# Slot Drift — Per-Class Findings

> **GENERATED SNAPSHOT — DO NOT EDIT BY HAND.** Captured 2026-04-29
> 17:26:56. Re-run the audit (`audit.el`) to refresh.

Source: 216 classes audited against `bd <cmd> --help`.
Global flags handled separately in `inheritance.md`.

### beads-command-admin-cleanup (`admin.cleanup`)
**File:** lisp/beads-command-admin.el:28

- [extra slot] `hard` (slot `hard`) — 

### beads-command-admin-compact (`admin.compact`)
**File:** lisp/beads-command-admin.el:76

- [missing slot] `--actor` (string) — Actor name for audit trail (default "agent")
- [missing slot] `--dolt` (boolean) — Dolt mode: run Dolt garbage collection on .beads/dolt
- [missing slot] `--json` (boolean) — Output JSON format
- [extra slot] `older-than` (slot `older-than`) — 

### beads-command-ado-sync (`ado.sync`)
**File:** lisp/beads-command-integrations.el:264

- [missing slot] `--area-path` (string) — Filter to ADO area path (e.g., "Project\Team")
- [missing slot] `--bootstrap-match` (boolean) — Enable heuristic matching for first sync
- [missing slot] `--issues` (string) — Comma-separated bead IDs to sync selectively (e.g., bd-abc,bd-def). Mutually exclusive with --parent.
- [missing slot] `--iteration-path` (string) — Filter to ADO iteration path (e.g., "Project\Sprint 1")
- [missing slot] `--no-create` (boolean) — Never create new items in either direction (pull or push)
- [missing slot] `--parent` (string) — Limit push to this bead and its descendants (push only). Mutually exclusive with --issues.
- [missing slot] `--prefer-ado` (boolean) — On conflict, use Azure DevOps version
- [missing slot] `--prefer-local` (boolean) — On conflict, keep local beads version
- [missing slot] `--prefer-newer` (boolean) — On conflict, use most recent version (default)
- [missing slot] `--project` (strings) — Project name(s) to sync (overrides configured project/projects)
- [missing slot] `--reconcile` (boolean) — Force reconciliation scan for deleted items
- [missing slot] `--states` (string) — Filter to ADO states, comma-separated (e.g., "New,Active,Resolved")
- [missing slot] `--types` (string) — Filter to work item types, comma-separated (e.g., "Bug,Task,User Story")

### beads-command-audit-label (`audit.label`)
**File:** lisp/beads-command-audit.el:99

- [drift desc] `--reason`
      CLI:   Reason for label
      slot:  Label Reason

### beads-command-backup (`backup`)
**File:** lisp/beads-command-misc.el:637

- [extra slot] `force` (slot `force`) — 

### beads-command-backup-restore (`backup.restore`)
**File:** lisp/beads-command-misc.el:698

- [missing slot] `--force` (boolean) — Overwrite existing database with backup contents
- [extra slot] `dry-run` (slot `dry-run`) — 

### beads-command-bootstrap (`bootstrap`)
**File:** lisp/beads-command-init.el:289

- [missing slot] `--non-interactive` (boolean) — Alias for --yes
- [missing slot] `--yes` (boolean) — Skip confirmation prompts (for CI/automation)

### beads-command-close (`close`)
**File:** lisp/beads-command-close.el:42

- [missing slot] `--no-auto` (boolean) — With --continue, show next step but don't claim it
- [missing slot] `--reason-file` (string) — Read close reason from file (use - for stdin)
- [drift desc] `--reason`
      CLI:   Reason for closing
      slot:  Close Reason

### beads-command-comments (`comments`)
**File:** lisp/beads-command-comments.el:42

- [missing slot] `--local-time` (boolean) — Show timestamps in local time instead of UTC

### beads-command-compact-analyze (`admin.compact`)
**File:** lisp/beads-command-compact.el:63

- [missing slot] `--actor` (string) — Actor name for audit trail (default "agent")
- [missing slot] `--all` (boolean) — Process all candidates
- [missing slot] `--analyze` (boolean) — Analyze mode: export candidates for agent review
- [missing slot] `--apply` (boolean) — Apply mode: accept agent-provided summary
- [missing slot] `--auto` (boolean) — Auto mode: AI-powered compaction (legacy)
- [missing slot] `--batch-size` (int) — Issues per batch (default 10)
- [missing slot] `--dolt` (boolean) — Dolt mode: run Dolt garbage collection on .beads/dolt
- [missing slot] `--dry-run` (boolean) — Preview without compacting
- [missing slot] `--force` (boolean) — Force compact (bypass checks, requires --id)
- [missing slot] `--id` (string) — Compact specific issue
- [missing slot] `--json` (boolean) — Output JSON format
- [missing slot] `--stats` (boolean) — Show compaction statistics
- [missing slot] `--summary` (string) — Path to summary file (use '-' for stdin)
- [missing slot] `--workers` (int) — Parallel workers (default 5)

### beads-command-compact-apply (`admin.compact`)
**File:** lisp/beads-command-compact.el:96

- [missing slot] `--actor` (string) — Actor name for audit trail (default "agent")
- [missing slot] `--all` (boolean) — Process all candidates
- [missing slot] `--analyze` (boolean) — Analyze mode: export candidates for agent review
- [missing slot] `--apply` (boolean) — Apply mode: accept agent-provided summary
- [missing slot] `--auto` (boolean) — Auto mode: AI-powered compaction (legacy)
- [missing slot] `--batch-size` (int) — Issues per batch (default 10)
- [missing slot] `--dolt` (boolean) — Dolt mode: run Dolt garbage collection on .beads/dolt
- [missing slot] `--dry-run` (boolean) — Preview without compacting
- [missing slot] `--force` (boolean) — Force compact (bypass checks, requires --id)
- [missing slot] `--json` (boolean) — Output JSON format
- [missing slot] `--limit` (int) — Limit number of candidates (0 = no limit)
- [missing slot] `--stats` (boolean) — Show compaction statistics
- [missing slot] `--tier` (int) — Compaction tier (1 or 2) (default 1)
- [missing slot] `--workers` (int) — Parallel workers (default 5)

### beads-command-compact-auto (`admin.compact`)
**File:** lisp/beads-command-compact.el:141

- [missing slot] `--actor` (string) — Actor name for audit trail (default "agent")
- [missing slot] `--analyze` (boolean) — Analyze mode: export candidates for agent review
- [missing slot] `--apply` (boolean) — Apply mode: accept agent-provided summary
- [missing slot] `--auto` (boolean) — Auto mode: AI-powered compaction (legacy)
- [missing slot] `--dolt` (boolean) — Dolt mode: run Dolt garbage collection on .beads/dolt
- [missing slot] `--json` (boolean) — Output JSON format
- [missing slot] `--limit` (int) — Limit number of candidates (0 = no limit)
- [missing slot] `--stats` (boolean) — Show compaction statistics
- [missing slot] `--summary` (string) — Path to summary file (use '-' for stdin)

### beads-command-compact-stats (`admin.compact`)
**File:** lisp/beads-command-compact.el:46

- [missing slot] `--actor` (string) — Actor name for audit trail (default "agent")
- [missing slot] `--all` (boolean) — Process all candidates
- [missing slot] `--analyze` (boolean) — Analyze mode: export candidates for agent review
- [missing slot] `--apply` (boolean) — Apply mode: accept agent-provided summary
- [missing slot] `--auto` (boolean) — Auto mode: AI-powered compaction (legacy)
- [missing slot] `--batch-size` (int) — Issues per batch (default 10)
- [missing slot] `--dolt` (boolean) — Dolt mode: run Dolt garbage collection on .beads/dolt
- [missing slot] `--dry-run` (boolean) — Preview without compacting
- [missing slot] `--force` (boolean) — Force compact (bypass checks, requires --id)
- [missing slot] `--id` (string) — Compact specific issue
- [missing slot] `--json` (boolean) — Output JSON format
- [missing slot] `--limit` (int) — Limit number of candidates (0 = no limit)
- [missing slot] `--stats` (boolean) — Show compaction statistics
- [missing slot] `--summary` (string) — Path to summary file (use '-' for stdin)
- [missing slot] `--tier` (int) — Compaction tier (1 or 2) (default 1)
- [missing slot] `--workers` (int) — Parallel workers (default 5)

### beads-command-cook (`cook`)
**File:** lisp/beads-command-misc.el:368

- [missing slot] `--dry-run` (boolean) — Preview what would be created
- [missing slot] `--force` (boolean) — Replace existing proto if it exists (requires --persist)
- [missing slot] `--mode` (string) — Cooking mode: compile (keep placeholders) or runtime (substitute vars)
- [missing slot] `--persist` (boolean) — Persist proto to database (legacy behavior)
- [missing slot] `--prefix` (string) — Prefix to prepend to proto ID (e.g., 'gt-' creates 'gt-mol-feature')
- [missing slot] `--search-path` (strings) — Additional paths to search for formula inheritance
- [missing slot] `--var` (stringArray) — Variable substitution (key=value), enables runtime mode

### beads-command-count (`count`)
**File:** lisp/beads-command-count.el:30

- [missing slot] `--priority` (int) — Filter by priority (0-4: 0=critical, 1=high, 2=medium, 3=low, 4=backlog)
- [missing slot] `--type` (string) — Filter by type (bug, feature, task, epic, chore, decision, merge-request, molecule, gate)
- [missing slot] `--updated-after` (string) — Filter issues updated after date (YYYY-MM-DD or RFC3339)
- [missing slot] `--updated-before` (string) — Filter issues updated before date (YYYY-MM-DD or RFC3339)
- [wrong short] `--label` — CLI advertises -l, slot has (none)

### beads-command-create (`create`)
**File:** lisp/beads-command-create.el:46

- [missing slot] `--title` (string) — Issue title (alternative to positional argument)
- [extra slot] `prefix` (slot `prefix-arg`) — 
- [extra slot] `rig` (slot `rig`) — 
- [extra slot] `from-template` (slot `from-template`) — 
- [extra slot] `agent-rig` (slot `agent-rig`) — 
- [extra slot] `role-type` (slot `role-type`) — 
- [drift desc] `--description`
      CLI:   Issue description
      slot:  Description
- [drift desc] `--design`
      CLI:   Design notes
      slot:  Design
- [drift desc] `--notes`
      CLI:   Additional notes
      slot:  Notes
- [drift desc] `--append-notes`
      CLI:   Append to existing notes (with newline separator)
      slot:  Append Notes
- [drift desc] `--context`
      CLI:   Additional context for the issue
      slot:  Context

### beads-command-create-form (`create-form`)
**File:** lisp/beads-command-misc.el:419

- [missing slot] `--parent` (string) — Parent issue ID for creating a hierarchical child (e.g., 'bd-a3f8e9')

### beads-command-dep-add (`dep.add`)
**File:** lisp/beads-command-dep.el:50

- [missing slot] `--depends-on` (string) — Issue ID that the first issue depends on (alias for --blocked-by)
- [missing slot] `--file` (string) — Read dependency edges from JSONL file, or '-' for stdin
- [missing slot] `--no-cycle-check` (boolean) — Skip cycle detection after adding (use for bulk wiring — run 'bd dep cycles' to verify afterwards)

### beads-command-dep-tree (`dep.tree`)
**File:** lisp/beads-command-dep.el:198

- [missing slot] `--reverse` (boolean) — Show dependent tree (deprecated: use --direction=up)
- [extra slot] `type` (slot `dep-type`) — 

### beads-command-doctor (`doctor`)
**File:** lisp/beads-command-doctor.el:34

- [missing slot] `--agent` (boolean) — Agent-facing diagnostic mode: rich context for AI agents (ZFC-compliant)
- [missing slot] `--migration` (string) — Run Dolt migration validation: 'pre' (before migration) or 'post' (after migration)
- [missing slot] `--orchestrator` (boolean) — Running in orchestrator multi-workspace mode (routes.jsonl is expected, higher duplicate tolerance)
- [missing slot] `--orchestrator-duplicates-threshold` (int) — Duplicate tolerance threshold for orchestrator mode (wisps are ephemeral) (default 1000)
- [missing slot] `--server` (boolean) — Run Dolt server mode health checks (connectivity, version, schema)
- [missing slot] `--verbose` (boolean) — Show all checks (default shows only warnings/errors)
- [extra slot] `force` (slot `force`) — 
- [extra slot] `source` (slot `source`) — 

### beads-command-dolt-pull (`dolt.pull`)
**File:** lisp/beads-command-dolt.el:135

- [missing slot] `--remote` (string) — Pull from a specific named remote instead of the default

### beads-command-dolt-push (`dolt.push`)
**File:** lisp/beads-command-dolt.el:118

- [missing slot] `--remote` (string) — Push to a specific named remote instead of the default

### beads-command-dolt-remote-remove (`dolt.remote.remove`)
**File:** lisp/beads-command-dolt.el:224

- [missing slot] `--force` (boolean) — Force remove even when SQL and CLI URLs conflict

### beads-command-duplicates (`duplicates`)
**File:** lisp/beads-command-misc.el:50

- [missing slot] `--auto-merge` (boolean) — Automatically merge all duplicates
- [missing slot] `--dry-run` (boolean) — Show what would be merged without making changes
- [extra slot] `merge` (slot `merge`) — 

### beads-command-export (`export`)
**File:** lisp/beads-command-misc.el:747

- [missing slot] `--all` (boolean) — Include all records (infra, templates, gates)
- [missing slot] `--include-infra` (boolean) — Include infrastructure beads (agents, rigs, roles, messages)
- [missing slot] `--no-memories` (boolean) — Exclude persistent memories from the export
- [missing slot] `--output` (string) — Output file path (default: stdout)
- [missing slot] `--scrub` (boolean) — Exclude test/pollution records

### beads-command-find-duplicates (`find-duplicates`)
**File:** lisp/beads-command-misc.el:625

- [missing slot] `--limit` (int) — Maximum number of pairs to show (default 50)
- [missing slot] `--method` (string) — Detection method: mechanical, ai (default "mechanical")
- [missing slot] `--model` (string) — AI model to use (only with --method ai; default from config ai.model)
- [missing slot] `--status` (string) — Filter by status (default: non-closed)
- [missing slot] `--threshold` (float) — Similarity threshold (0.0-1.0, lower = more results) (default 0.5)

### beads-command-flatten (`flatten`)
**File:** lisp/beads-command-misc.el:769

- [missing slot] `--dry-run` (boolean) — Preview without making changes
- [missing slot] `--force` (boolean) — Confirm irreversible history squash

### beads-command-gate-discover (`gate.discover`)
**File:** lisp/beads-command-gate.el:129

- [missing slot] `--branch` (string) — Filter runs by branch (default: current branch)
- [missing slot] `--dry-run` (boolean) — Preview mode: show matches without updating
- [missing slot] `--limit` (int) — Max runs to query from GitHub (default 10)
- [missing slot] `--max-age` (duration) — Max age for gate/run matching (default 30m0s)

### beads-command-gate-resolve (`gate.resolve`)
**File:** lisp/beads-command-gate.el:84

- [drift desc] `--reason`
      CLI:   Reason for resolving the gate
      slot:  Resolve Reason

### beads-command-gc (`gc`)
**File:** lisp/beads-command-misc.el:775

- [missing slot] `--dry-run` (boolean) — Preview without making changes
- [missing slot] `--force` (boolean) — Skip confirmation prompts
- [missing slot] `--older-than` (int) — Delete closed issues older than N days (default 90)
- [missing slot] `--skip-decay` (boolean) — Skip issue deletion phase
- [missing slot] `--skip-dolt` (boolean) — Skip Dolt garbage collection phase

### beads-command-github-sync (`github.sync`)
**File:** lisp/beads-command-integrations.el:219

- [missing slot] `--issues` (string) — Comma-separated bead IDs to sync selectively (e.g., bd-abc,bd-def). Mutually exclusive with --parent.
- [missing slot] `--parent` (string) — Limit push to this bead and its descendants (push only). Mutually exclusive with --issues.
- [missing slot] `--prefer-github` (boolean) — On conflict, use GitHub version
- [missing slot] `--prefer-local` (boolean) — On conflict, keep local beads version
- [missing slot] `--prefer-newer` (boolean) — On conflict, use most recent version (default)
- [missing slot] `--pull-only` (boolean) — Only pull issues from GitHub
- [missing slot] `--push-only` (boolean) — Only push issues to GitHub
- [extra slot] `pull` (slot `pull`) — 
- [extra slot] `push` (slot `push`) — 

### beads-command-gitlab-sync (`gitlab.sync`)
**File:** lisp/beads-command-integrations.el:145

- [missing slot] `--assignee` (string) — Filter by assignee username
- [missing slot] `--exclude-type` (string) — Exclude these issue types from sync (comma-separated)
- [missing slot] `--issues` (string) — Comma-separated bead IDs to sync selectively (e.g., bd-abc,bd-def). Mutually exclusive with --parent.
- [missing slot] `--label` (string) — Filter by labels (comma-separated, AND logic)
- [missing slot] `--milestone` (string) — Filter by milestone title
- [missing slot] `--no-ephemeral` (boolean) — Exclude ephemeral/wisp issues from push (default: true) (default true)
- [missing slot] `--parent` (string) — Limit push to this bead and its descendants (push only). Mutually exclusive with --issues.
- [missing slot] `--prefer-gitlab` (boolean) — On conflict, use GitLab version
- [missing slot] `--prefer-local` (boolean) — On conflict, keep local beads version
- [missing slot] `--prefer-newer` (boolean) — On conflict, use most recent version (default)
- [missing slot] `--project` (string) — Filter to issues from this project ID (group mode)
- [missing slot] `--pull-only` (boolean) — Only pull issues from GitLab
- [missing slot] `--push-only` (boolean) — Only push issues to GitLab
- [missing slot] `--type` (string) — Only sync these issue types (comma-separated, e.g. 'epic,feature,task')
- [extra slot] `pull` (slot `pull`) — 
- [extra slot] `push` (slot `push`) — 

### beads-command-hooks-install (`hooks.install`)
**File:** lisp/beads-command-hooks.el:27

- [missing slot] `--beads` (boolean) — Install hooks to .beads/hooks/ (recommended for Dolt backend)

### beads-command-import (`import`)
**File:** lisp/beads-command-misc.el:753

- [missing slot] `--dedup` (boolean) — Skip lines whose title matches an existing open issue
- [missing slot] `--input` (string) — Read JSONL from a specific file

### beads-command-info (`info`)
**File:** lisp/beads-command-info.el:36

- [missing slot] `--json` (boolean) — Output in JSON format

### beads-command-init (`init`)
**File:** lisp/beads-command-init.el:48

- [missing slot] `--agents-file` (string) — Custom filename for agent instructions (default: AGENTS.md)
- [missing slot] `--agents-profile` (string) — AGENTS.md profile: 'minimal' (default, pointer to bd prime) or 'full' (complete command reference)
- [missing slot] `--agents-template` (string) — Path to custom AGENTS.md template (overrides embedded default)
- [missing slot] `--backend` (string) — Storage backend (default: dolt). --backend=sqlite prints deprecation notice.
- [missing slot] `--database` (string) — Use existing server database name (overrides prefix-based naming)
- [missing slot] `--destroy-token` (string) — Explicit confirmation token for destructive re-init in non-interactive mode (format: 'DESTROY-<prefix>')
- [missing slot] `--discard-remote` (boolean) — Authorize discarding the configured remote's Dolt history when re-initializing. Requires --destroy-token in non-interactive mode; see 'bd help init-safety'.
- [missing slot] `--external` (boolean) — Server is externally managed (skip server startup); use with --shared-server or --server
- [missing slot] `--non-interactive` (boolean) — Skip all interactive prompts (auto-detected in CI or non-TTY environments)
- [missing slot] `--quiet` (boolean) — Suppress output (quiet mode)
- [missing slot] `--reinit-local` (boolean) — Re-initialize local .beads/ over existing local data. Does NOT authorize remote divergence; see --discard-remote.
- [missing slot] `--remote` (string) — Dolt remote URL to clone from and persist as sync.remote
- [missing slot] `--role` (string) — Set beads role without prompting: "maintainer" or "contributor"
- [missing slot] `--server` (boolean) — Use external dolt sql-server instead of embedded engine
- [missing slot] `--server-socket` (string) — Unix domain socket path (overrides host/port)
- [missing slot] `--shared-server` (boolean) — Enable shared Dolt server mode (all projects share one server at ~/.beads/shared-server/)
- [missing slot] `--skip-agents` (boolean) — Skip AGENTS.md and Claude settings generation
- [extra slot] `branch` (slot `branch`) — 

### beads-command-jira-sync (`jira.sync`)
**File:** lisp/beads-command-integrations.el:29

- [missing slot] `--create-only` (boolean) — Only create new issues, don't update existing
- [missing slot] `--issues` (string) — Comma-separated bead IDs to sync selectively (e.g., bd-abc,bd-def). Mutually exclusive with --parent.
- [missing slot] `--parent` (string) — Limit push to this bead and its descendants (push only). Mutually exclusive with --issues.
- [missing slot] `--prefer-jira` (boolean) — Prefer Jira version on conflicts
- [missing slot] `--prefer-local` (boolean) — Prefer local version on conflicts
- [missing slot] `--project` (strings) — Project key(s) to sync (overrides configured project/projects)
- [missing slot] `--state` (string) — Issue state to sync: open, closed, all (default "all")

### beads-command-linear-sync (`linear.sync`)
**File:** lisp/beads-command-integrations.el:64

- [missing slot] `--create-only` (boolean) — Only create new issues, don't update existing
- [missing slot] `--exclude-type` (strings) — Exclude issues of these types (can be repeated)
- [missing slot] `--include-ephemeral` (boolean) — Include ephemeral issues (wisps, etc.) when pushing to Linear
- [missing slot] `--issues` (string) — Comma-separated bead IDs to sync selectively (e.g., bd-abc,bd-def). Mutually exclusive with --parent.
- [missing slot] `--parent` (string) — Limit push to this beads ticket and its descendants
- [missing slot] `--prefer-linear` (boolean) — Prefer Linear version on conflicts
- [missing slot] `--prefer-local` (boolean) — Prefer local version on conflicts
- [missing slot] `--relations` (boolean) — Import Linear relations as bd dependencies when pulling
- [missing slot] `--state` (string) — Issue state to sync: open, closed, all (default "all")
- [missing slot] `--team` (strings) — Team ID(s) to sync (overrides configured team_id/team_ids)
- [missing slot] `--type` (strings) — Only sync issues of these types (can be repeated)
- [missing slot] `--update-refs` (boolean) — Update external_ref after creating Linear issues (default true)

### beads-command-list (`list`)
**File:** lisp/beads-command-list.el:73

- [missing slot] `--exclude-label` (strings) — Exclude issues that have ANY of these labels
- [missing slot] `--no-pager` (boolean) — Disable pager output

### beads-command-migrate (`migrate`)
**File:** lisp/beads-command-migrate.el:25

- [missing slot] `--json` (boolean) — Output migration statistics in JSON format
- [extra slot] `cleanup` (slot `cleanup`) — 

### beads-command-migrate-hooks (`migrate.hooks`)
**File:** lisp/beads-command-migrate.el:78

- [missing slot] `--json` (boolean) — Output in JSON format

### beads-command-migrate-issues (`migrate.issues`)
**File:** lisp/beads-command-migrate.el:61

- [missing slot] `--dry-run` (boolean) — Show plan without making changes
- [missing slot] `--from` (string) — Source repository (required)
- [missing slot] `--id` (strings) — Specific issue IDs to migrate (can specify multiple)
- [missing slot] `--ids-file` (string) — File containing issue IDs (one per line)
- [missing slot] `--include` (string) — Include dependencies: none/upstream/downstream/closure (default "none")
- [missing slot] `--label` (strings) — Filter by labels (can specify multiple)
- [missing slot] `--priority` (int) — Filter by priority (0-4) (default -1)
- [missing slot] `--status` (string) — Filter by status (open/closed/all)
- [missing slot] `--strict` (boolean) — Fail on orphaned dependencies or missing repos
- [missing slot] `--to` (string) — Destination repository (required)
- [missing slot] `--type` (string) — Filter by issue type (bug/feature/task/epic/chore/decision)
- [missing slot] `--within-from-only` (boolean) — Only include dependencies from source repo (default true)
- [missing slot] `--yes` (boolean) — Skip confirmation prompt

### beads-command-migrate-sync (`migrate.sync`)
**File:** lisp/beads-command-migrate.el:71

- [missing slot] `--dry-run` (boolean) — Show what would be done without making changes
- [missing slot] `--json` (boolean) — Output in JSON format

### beads-command-mol-bond (`mol.bond`)
**File:** lisp/beads-command-mol.el:224

- [wrong type] `--ref` — CLI string, slot boolean

### beads-command-mol-seed (`mol.seed`)
**File:** lisp/beads-command-mol.el:438

- [missing slot] `--var` (stringArray) — Variable substitution for condition filtering (key=value)
- [extra slot] `patrol` (slot `patrol`) — 

### beads-command-mol-stale (`mol.stale`)
**File:** lisp/beads-command-mol.el:429

- [missing slot] `--all` (boolean) — Include molecules with 0 children
- [missing slot] `--blocking` (boolean) — Only show molecules blocking other work
- [missing slot] `--unassigned` (boolean) — Only show unassigned molecules

### beads-command-mol-wisp (`mol.wisp`)
**File:** lisp/beads-command-mol.el:94

- [missing slot] `--root-only` (boolean) — Create only the root issue (no child step issues)

### beads-command-notion-sync (`notion.sync`)
**File:** lisp/beads-command-integrations.el:354

- [missing slot] `--issues` (string) — Comma-separated bead IDs to sync selectively (e.g., bd-abc,bd-def). Mutually exclusive with --parent.
- [missing slot] `--parent` (string) — Limit push to this bead and its descendants (push only). Mutually exclusive with --issues.

### beads-command-orphans (`orphans`)
**File:** lisp/beads-command-misc.el:89

- [missing slot] `--label` (strings) — Filter by labels (AND: must have ALL). Can combine with --label-any
- [missing slot] `--label-any` (strings) — Filter by labels (OR: must have AT LEAST ONE). Can combine with --label

### beads-command-preflight (`preflight`)
**File:** lisp/beads-command-misc.el:277

- [missing slot] `--check` (boolean) — Run checks automatically
- [missing slot] `--fix` (boolean) — Auto-fix issues where possible (not yet implemented)
- [missing slot] `--json` (boolean) — Output results as JSON
- [missing slot] `--skip-lint` (boolean) — Skip lint check explicitly

### beads-command-prime (`prime`)
**File:** lisp/beads-command-misc.el:266

- [missing slot] `--export` (boolean) — Output default content (ignores PRIME.md override)
- [missing slot] `--full` (boolean) — Force full CLI output (ignore MCP detection)
- [missing slot] `--mcp` (boolean) — Force MCP mode (minimal output)
- [missing slot] `--stealth` (boolean) — Stealth mode (no git operations, flush only)

### beads-command-promote (`promote`)
**File:** lisp/beads-command-misc.el:426

- [missing slot] `--reason` (string) — Reason for promotion

### beads-command-purge (`purge`)
**File:** lisp/beads-command-misc.el:781

- [missing slot] `--dry-run` (boolean) — Preview what would be purged with stats
- [missing slot] `--force` (boolean) — Actually purge (without this, shows preview)
- [missing slot] `--older-than` (string) — Only purge beads closed more than N ago (e.g., 7d, 2w, 30)
- [missing slot] `--pattern` (string) — Only purge beads matching ID glob pattern (e.g., *-wisp-*)

### beads-command-query (`query`)
**File:** lisp/beads-command-misc.el:440

- [missing slot] `--all` (boolean) — Include closed issues (default: exclude closed)
- [missing slot] `--limit` (int) — Limit results (default: 50, 0 = unlimited) (default 50)
- [missing slot] `--long` (boolean) — Show detailed multi-line output for each issue
- [missing slot] `--parse-only` (boolean) — Only parse the query and show the AST (for debugging)
- [missing slot] `--reverse` (boolean) — Reverse sort order
- [missing slot] `--sort` (string) — Sort by field: priority, created, updated, closed, status, id, title, type, assignee

### beads-command-ready (`ready`)
**File:** lisp/beads-command-ready.el:42

- [missing slot] `--claim` (boolean) — Atomically claim the first ready issue matching the filters
- [missing slot] `--exclude-label` (strings) — Exclude issues that have ANY of these labels

### beads-command-remember (`remember`)
**File:** lisp/beads-command-misc.el:908

- [missing slot] `--key` (string) — Explicit key for the memory (auto-generated from content if not set). If a memory with this key already exists, it will be updated in place

### beads-command-rename-prefix (`rename-prefix`)
**File:** lisp/beads-command-misc.el:328

- [missing slot] `--dry-run` (boolean) — Preview changes without applying them
- [missing slot] `--repair` (boolean) — Repair database with multiple prefixes by consolidating them

### beads-command-reopen (`reopen`)
**File:** lisp/beads-command-reopen.el:45

- [drift desc] `--reason`
      CLI:   Reason for reopening
      slot:  Reopen Reason

### beads-command-repo-add (`repo.add`)
**File:** lisp/beads-command-integrations.el:106

- [missing slot] `--json` (boolean) — Output JSON

### beads-command-repo-list (`repo.list`)
**File:** lisp/beads-command-integrations.el:115

- [missing slot] `--json` (boolean) — Output JSON

### beads-command-repo-remove (`repo.remove`)
**File:** lisp/beads-command-integrations.el:122

- [missing slot] `--json` (boolean) — Output JSON

### beads-command-repo-sync (`repo.sync`)
**File:** lisp/beads-command-integrations.el:131

- [missing slot] `--json` (boolean) — Output JSON
- [missing slot] `--verbose` (boolean) — Show detailed sync progress

### beads-command-restore (`restore`)
**File:** lisp/beads-command-restore.el:30

- [missing slot] `--json` (boolean) — Output restore results in JSON format

### beads-command-search (`search`)
**File:** lisp/beads-command-search.el:37

- [missing slot] `--query` (string) — Search query (alternative to positional argument)

### beads-command-set-state (`set-state`)
**File:** lisp/beads-command-state.el:29

- [drift desc] `--reason`
      CLI:   Reason for the state change (recorded in event)
      slot:  State Change Reason

### beads-command-setup (`setup`)
**File:** lisp/beads-command-misc.el:344

- [missing slot] `--add` (string) — Add a custom recipe with given name
- [missing slot] `--check` (boolean) — Check if integration is installed
- [missing slot] `--global` (boolean) — Install globally (claude/mux; writes to ~/.claude/settings.json or ~/.mux/AGENTS.md)
- [missing slot] `--list` (boolean) — List all available recipes
- [missing slot] `--output` (string) — Write template to custom path
- [missing slot] `--print` (boolean) — Print the template to stdout
- [missing slot] `--project` (boolean) — Install for this project only (gemini/mux)
- [missing slot] `--remove` (boolean) — Remove the integration
- [missing slot] `--stealth` (boolean) — Use stealth mode (claude/gemini)

### beads-command-ship (`ship`)
**File:** lisp/beads-command-misc.el:356

- [missing slot] `--dry-run` (boolean) — Preview without making changes
- [missing slot] `--force` (boolean) — Ship even if issue is not closed

### beads-command-show (`show`)
**File:** lisp/beads-command-show.el:59

- [missing slot] `--id` (stringArray) — Issue ID (use for IDs that look like flags, e.g., --id=gt--xyz)

### beads-command-swarm-validate (`swarm.validate`)
**File:** lisp/beads-command-swarm.el:70

- [missing slot] `--verbose` (boolean) — Include detailed issue graph in output

### beads-command-update (`update`)
**File:** lisp/beads-command-update.el:46

- [drift desc] `--description`
      CLI:   Issue description
      slot:  Description
- [drift desc] `--design`
      CLI:   Design notes
      slot:  Design
- [drift desc] `--notes`
      CLI:   Additional notes
      slot:  Notes
- [drift desc] `--append-notes`
      CLI:   Append to existing notes (with newline separator)
      slot:  Append Notes

### beads-command-vc-commit (`vc.commit`)
**File:** lisp/beads-command-vc.el:30

- [missing slot] `--stdin` (boolean) — Read commit message from stdin

### beads-command-version (`version`)
**File:** lisp/beads-command-misc.el:217

- [extra slot] `daemon` (slot `daemon`) — 


---

## Summary
- Clean classes: 144
- Classes with findings: 72
- Total findings: 295
