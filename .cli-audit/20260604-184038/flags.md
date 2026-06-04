# Slot Drift — Per-Class Findings

Source: 216 classes audited against `bd <cmd> --help`.
Global flags handled separately in `inheritance.md`.

### beads-command-admin-compact (`admin.compact`)
**File:** lisp/beads-command-admin.el:82

- [missing slot] `--actor` (string) — Actor name for audit trail (default "agent")
- [missing slot] `--json` (boolean) — Output JSON format

### beads-command-compact-analyze (`admin.compact`)
**File:** lisp/beads-command-compact.el:97

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
**File:** lisp/beads-command-compact.el:130

- [missing slot] `--actor` (string) — Actor name for audit trail (default "agent")
- [missing slot] `--all` (boolean) — Process all candidates
- [missing slot] `--analyze` (boolean) — Analyze mode: export candidates for agent review
- [missing slot] `--apply` (boolean) — Apply mode: accept agent-provided summary
- [missing slot] `--auto` (boolean) — Auto mode: AI-powered compaction (legacy)
- [missing slot] `--batch-size` (int) — Issues per batch (default 10)
- [missing slot] `--dolt` (boolean) — Dolt mode: run Dolt garbage collection on .beads/dolt
- [missing slot] `--dry-run` (boolean) — Preview without compacting
- [missing slot] `--json` (boolean) — Output JSON format
- [missing slot] `--limit` (int) — Limit number of candidates (0 = no limit)
- [missing slot] `--stats` (boolean) — Show compaction statistics
- [missing slot] `--tier` (int) — Compaction tier (1 or 2) (default 1)
- [missing slot] `--workers` (int) — Parallel workers (default 5)

### beads-command-compact-auto (`admin.compact`)
**File:** lisp/beads-command-compact.el:181

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
**File:** lisp/beads-command-compact.el:80

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

### beads-command-dep-add (`dep.add`)
**File:** lisp/beads-command-dep.el:50

- [missing slot] `--depends-on` (string) — Issue ID that the first issue depends on (alias for --blocked-by)

### beads-command-doctor (`doctor`)
**File:** lisp/beads-command-doctor.el:34

- [missing slot] `--verbose` (boolean) — Show all checks (default shows only warnings/errors)

### beads-command-info (`info`)
**File:** lisp/beads-command-info.el:36

- [missing slot] `--json` (boolean) — Output in JSON format

### beads-command-init (`init`)
**File:** lisp/beads-command-init.el:48

- [missing slot] `--debug` (boolean) — Run the managed Dolt sql-server with --loglevel=debug and CPU profiling (--prof cpu). Persisted to config.yaml as dolt.debug. No effect on externally-managed servers.
- [missing slot] `--proxied-server` (boolean) — [EXPERIMENTAL] Use a per-workspace proxied dolt sql-server (proxy + child dolt) rooted at .beads/proxieddb
- [missing slot] `--proxied-server-config-path` (string) — [EXPERIMENTAL] Absolute path to an existing dolt sql-server YAML config (proxied-server mode only). When set, bd uses this file instead of auto-generating one. Relative paths are rejected.
- [missing slot] `--proxied-server-external-host` (string) — [EXPERIMENTAL] Hostname or IP of an externally-managed dolt sql-server the proxy should front (proxied-server mode only). Mutually exclusive with --proxied-server-external-socket-path.
- [missing slot] `--proxied-server-external-keep-alive` (duration) — [EXPERIMENTAL] TCP keepalive period for the proxy→external connection. Zero uses the package default (30s).
- [missing slot] `--proxied-server-external-port` (int) — [EXPERIMENTAL] TCP port of the externally-managed dolt sql-server (proxied-server mode only). Required when --proxied-server-external-host is set.
- [missing slot] `--proxied-server-external-socket-path` (string) — [EXPERIMENTAL] Absolute unix socket path of the externally-managed dolt sql-server (proxied-server mode only). Mutually exclusive with --proxied-server-external-host. Relative paths are rejected.
- [missing slot] `--proxied-server-external-tls` (boolean) — [EXPERIMENTAL] Require TLS when connecting to the externally-managed dolt sql-server (proxied-server mode only).
- [missing slot] `--proxied-server-external-tls-cert-path` (string) — [EXPERIMENTAL] Absolute path to a client TLS certificate (for mTLS to the externally-managed dolt sql-server). Must be paired with --proxied-server-external-tls-key-path. Relative paths are rejected.
- [missing slot] `--proxied-server-external-tls-key-path` (string) — [EXPERIMENTAL] Absolute path to the client TLS private key (for mTLS to the externally-managed dolt sql-server). Must be paired with --proxied-server-external-tls-cert-path. Relative paths are rejected.
- [missing slot] `--proxied-server-external-user` (string) — [EXPERIMENTAL] MySQL user for the externally-managed dolt sql-server (proxied-server mode only). Defaults to "root" when empty. Password is read at runtime from $BEADS_PROXIED_SERVER_EXTERNAL_PASSWORD and is never persisted to disk.
- [missing slot] `--proxied-server-log-path` (string) — [EXPERIMENTAL] Absolute path to the proxied dolt sql-server log file (proxied-server mode only). Default: <beadsDir>/proxieddb/server.log. Relative paths are rejected.
- [missing slot] `--proxied-server-root-path` (string) — [EXPERIMENTAL] Absolute directory holding the proxied dolt sql-server's lockfiles, pidfiles, and child .dolt repository (proxied-server mode only). Default: <beadsDir>/proxieddb. May not exist yet — bd will create it. Relative paths are rejected.
- [missing slot] `--quiet` (boolean) — Suppress output (quiet mode)

### beads-command-linear-sync (`linear.sync`)
**File:** lisp/beads-command-integrations.el:141

- [missing slot] `--milestones` (boolean) — Reconstruct Linear project milestones as local epic parents when pulling
- [missing slot] `--no-wait` (boolean) — Fail immediately if another sync is running instead of waiting
- [missing slot] `--pull-if-stale` (boolean) — Pull only if Linear data is stale (skip if fresh)
- [missing slot] `--threshold` (duration) — Staleness threshold for --pull-if-stale (default 20m) (default 20m0s)

### beads-command-list (`list`)
**File:** lisp/beads-command-list.el:74

- [missing slot] `--no-pager` (boolean) — Disable pager output
- [missing slot] `--skip-labels` (boolean) — Skip label hydration. The labels field in output will be empty regardless of actual labels. Use only when the caller does not depend on label data. Cannot combine with --label, --label-any, --label-pattern, --label-regex, --exclude-label, or --no-labels.

### beads-command-migrate (`migrate`)
**File:** lisp/beads-command-migrate.el:25

- [missing slot] `--json` (boolean) — Output migration statistics in JSON format

### beads-command-migrate-hooks (`migrate.hooks`)
**File:** lisp/beads-command-migrate.el:171

- [missing slot] `--json` (boolean) — Output in JSON format

### beads-command-migrate-sync (`migrate.sync`)
**File:** lisp/beads-command-migrate.el:151

- [missing slot] `--json` (boolean) — Output in JSON format

### beads-command-preflight (`preflight`)
**File:** lisp/beads-command-misc.el:328

- [missing slot] `--json` (boolean) — Output results as JSON

### beads-command-prime (`prime`)
**File:** lisp/beads-command-misc.el:294

- [missing slot] `--hook-json` (boolean) — Wrap output in the SessionStart hook JSON envelope (Claude Code, Gemini CLI, Codex)
- [missing slot] `--memories-only` (boolean) — Output only persistent memories for compact hook contexts

### beads-command-repo-add (`repo.add`)
**File:** lisp/beads-command-integrations.el:292

- [missing slot] `--json` (boolean) — Output JSON

### beads-command-repo-list (`repo.list`)
**File:** lisp/beads-command-integrations.el:307

- [missing slot] `--json` (boolean) — Output JSON

### beads-command-repo-remove (`repo.remove`)
**File:** lisp/beads-command-integrations.el:319

- [missing slot] `--json` (boolean) — Output JSON

### beads-command-repo-sync (`repo.sync`)
**File:** lisp/beads-command-integrations.el:334

- [missing slot] `--json` (boolean) — Output JSON
- [missing slot] `--verbose` (boolean) — Show detailed sync progress

### beads-command-restore (`restore`)
**File:** lisp/beads-command-restore.el:30

- [missing slot] `--json` (boolean) — Output restore results in JSON format

### beads-command-setup (`setup`)
**File:** lisp/beads-command-misc.el:427

- [missing slot] `--global` (boolean) — Install globally (claude/codex/mux; writes to ~/.claude/settings.json, $CODEX_HOME/AGENTS.md or ~/.codex/AGENTS.md, or ~/.mux/AGENTS.md)

### beads-command-show (`show`)
**File:** lisp/beads-command-show.el:60

- [missing slot] `--include-comments` (boolean) — Stream full comment bodies in JSON output (--json only; may be slow on issues with many comments)
- [drift desc] `--include-dependents`
      CLI:   Stream full dependent issues in JSON output (--json only; may be slow on hub beads)
      slot:  Include downstream relationship data in the result.
Passes --include-dependents so the JSON adds dependents[] (CHILDREN
via parent-child, BLOCKS via blocks) plus epic_total_children,
epic_closed_children, and epic_closeable.  Plain `bd show --json'
omits dependents[] (only dependent_count), so this flag is required
to render the CHILDREN and BLOCKS sections in the show buffer.

### beads-command-swarm-validate (`swarm.validate`)
**File:** lisp/beads-command-swarm.el:70

- [missing slot] `--verbose` (boolean) — Include detailed issue graph in output


---

## Summary
- Clean classes: 214
- Classes with findings: 24
- Total findings: 93
