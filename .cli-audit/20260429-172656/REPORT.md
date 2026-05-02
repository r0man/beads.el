# bd CLI ↔ beads.el Audit — 20260429-172656

Source of truth: `bd --version` = `1.0.3 (dev)`, parsed at audit time.
Artifact under test: `lisp/beads-command-*.el`.

## Summary

| Metric | Count |
|--------|------:|
| CLI commands enumerated | 259 |
| EIEIO classes inventoried | 216 |
| Unique class cli_paths | 212 |
| Missing classes (CLI without class) | 47 |
| Orphan classes (class without CLI) | 0 |
| cli_path collisions (>1 class same target) | 1 (5 classes → `admin.compact`) |
| Classes with slot drift | 72 |
| Total slot-drift findings | 294 |
| ↳ Missing slots (CLI flag, no slot) | 258 |
| ↳ Extra slots (slot, no CLI flag) | 21 |
| ↳ Description drift | 14 |
| ↳ CLI advertises a short option that the slot doesn't match | 1 |
| Inheritance gaps (`Global Flags:` shown, class doesn't inherit) | 0 |
| `beads-command-global-options` missing slots | 2 (`--directory`, `--global`) |
| `beads-command-global-options` stale slots | 6 |

> **Note (post-audit):** This snapshot was the *input* to the
> reconciliation work. The global-options changes listed above
> (missing slots added, stale slots removed) were resolved in
> commit 170abe0 (`feat(global-options): sync with bd 1.0.3 Global
> Flags (bde-s84l.1)`). All 13 sub-epics under bde-s84l have since
> closed.
>
> The actionable findings in this snapshot have been addressed in
> the PR. The summary counts above (47 missing classes, 72 drifted
> classes, 294 slot findings) reflect the *pre-fix* state and are
> intentionally preserved for reproducibility. Remaining
> classifications that are **not** actionable bugs and stay out of
> scope going forward:
>
> - 24 of the 47 "missing classes" are top-level group commands
>   (`admin`, `dolt`, `mol`, …) that, by project policy, get a
>   parent transient menu but never a `beads-defcommand` class.
>   See `CLAUDE.md` § "Top-level group commands".
> - The 5-class collision on `admin.compact` is intentional — each
>   class exposes a different `--mode` through its own transient.
>   The 4 `compact-*` classes inflate the missing-slot count
>   because they each model only one mode; treat the cluster as
>   one unit.
>
> Re-running the audit after the fix commits will produce a new
> snapshot under `.cli-audit/<timestamp>/` with the residual
> baseline.

## Artifacts

- [`commands.json`](commands.json) — all 259 dotted CLI paths + summaries
- [`classes.json`](classes.json) — all 216 `beads-defcommand` records
- [`coverage.md`](coverage.md) — missing/orphan/collision breakdown
- [`flags.md`](flags.md) — per-class slot drift findings
- [`inheritance.md`](inheritance.md) — `beads-command-global-options` audit
- [`help/`](help/) — cached `bd <cmd> --help` output (259 files)
- [`audit.el`](audit.el) — Emacs Lisp script that ran the comparison

## Methodology notes

- Slot-vs-flag matching joins on `--long-option`. `:short-option` in
  beads.el doubles as a transient menu key (see
  `lisp/beads-meta.el:32`); CLI invocation only uses the long option
  (see `beads-meta-build-command-line` in `lisp/beads-meta.el:1011`).
  We therefore flag a "wrong short" only when bd itself advertises a
  short letter that the slot disagrees with; transient-only
  `:short-option` choices are not counted as drift.
- Description drift uses first-sentence equality after lowercase + trim
  + trailing-period strip + 30-char prefix tolerance. Fourteen
  findings remain — these are real divergences worth reviewing.
- Positional argument checks were not performed. The `Usage:` line
  syntax (`<id>...`, `<key=value>`, `--flag <value>`) is too ambiguous
  to parse reliably; beads.el often collapses a CLI's required +
  variadic positional pair into a single `list-of-string` slot.
- Global flags (the `Global Flags:` section) are excluded from
  per-class slot diffs. They're audited once against the
  `beads-command-global-options` superclass in `inheritance.md`.

## Missing Classes (47)

47 CLI commands have no `beads-defcommand` class. Three buckets:

### Top-level group commands without a class (24)

These commands serve primarily as roots for subcommands. The project
has classes for the children but not for the parent. Whether each one
warrants its own class depends on UX: some (e.g. `dolt`, `mol`,
`epic`, `label`) host transient menus that route to children; others
might genuinely have no Emacs counterpart. Triage before filing.

`admin`, `ado`, `audit`, `config`, `dep`, `dolt`, `epic`,
`federation`, `formula`, `gate`, `github`, `gitlab`, `hooks`, `jira`,
`label`, `linear`, `merge-slot`, `mol`, `notion`, `repo`, `rules`,
`swarm`, `vc`, `worktree`.

### Mid-level group subcommand without a class (1)

- `dolt.remote` — children (`dolt.remote.add/list/remove`) all have
  classes.

### Top-level commands with no class (6)

These are real, directly-callable commands. Most warrant a class.

| CLI command | Summary |
|-------------|---------|
| `batch` | Run multiple write operations in a single database transaction |
| `compact` | Squash old Dolt commits to reduce history size (NB: existing `beads-command-compact-*` classes target `admin compact`, not this) |
| `init-safety` | Explain bd init flag semantics and the destroy-token format |
| `ping` | Check database connectivity |
| `prune` | Delete old closed beads to reclaim space and shrink exports |

(`audit` is listed under "top-level groups" above.)

### Subcommands without a class (16)

These are likely the highest-value gaps to fill.

`ado.pull`, `ado.push`, `config.apply`, `config.drift`, `config.show`,
`gate.create`, `github.pull`, `github.push`, `gitlab.pull`,
`gitlab.push`, `graph.check`, `jira.pull`, `jira.push`,
`linear.pull`, `linear.push`, `notion.pull`, `notion.push`.

## Orphan Classes

_None._ Every `beads-defcommand` class resolves to an existing `bd`
subcommand path.

## cli_path Collisions

`admin.compact` is targeted by 5 classes:

| Class | File:line |
|-------|-----------|
| `beads-command-admin-compact` | `lisp/beads-command-admin.el:76` |
| `beads-command-compact-stats` | `lisp/beads-command-compact.el:46` |
| `beads-command-compact-analyze` | `lisp/beads-command-compact.el:63` |
| `beads-command-compact-apply` | `lisp/beads-command-compact.el:96` |
| `beads-command-compact-auto` | `lisp/beads-command-compact.el:141` |

This appears intentional — each class exposes a different `--mode`
through the transient UI — but every one of them is independently
audited against `bd admin compact --help` in `flags.md`, so each
flags some "missing" entries that are actually handled by sibling
classes. Reviewers should treat the `admin.compact` group as one
unit.

## Slot Drift

72 classes have ≥ 1 finding (294 findings total). The full per-class
breakdown is in [`flags.md`](flags.md). Highlights:

- **`bd close`** lacks `--no-auto` and `--reason-file` slots; `--reason`
  description drifted from "Reason for closing" to "Close Reason".
- **`bd ado sync`** is missing 13 slots covering filters, conflict
  resolution preferences, and reconciliation behavior.
- **`bd update`**, **`bd create`**, **`bd dep add`**, **`bd diff`**,
  and the GitHub/GitLab/Linear integration commands all have several
  missing slots — these are the densest clusters of drift.
- The four `compact-*` classes inflate the missing-slot count
  because they each only model one mode of `bd admin compact`.

## Inheritance

- 0 classes need a different superclass (every command with `Global
  Flags:` already inherits `beads-command-global-options`).
- `beads-command-global-options` itself has drift:
  - Missing slots: `--directory` (-C), `--global`
  - Stale slots (no longer in `bd` Global Flags): `--allow-stale`,
    `--lock-timeout`, `--no-auto-flush`, `--no-auto-import`,
    `--no-daemon`, `--no-db`. Six slots that every command now
    inherits but cannot use — worth removing or reverifying with
    `bd --help`.

## Suggested Beads Issues

A starter set the user can file (one per cluster, not one per
finding). Filing per-finding would generate ~120 issues; filing
per-cluster gives a tractable backlog of ~15.

```bash
# Foundational: fix the global options class first.
bd create --type=task --priority=1 \
  --title="Update beads-command-global-options to match bd 1.0.3 Global Flags" \
  --description="Add --directory and --global slots; remove or re-justify --allow-stale, --lock-timeout, --no-auto-flush, --no-auto-import, --no-daemon, --no-db. See .cli-audit/20260429-172656/inheritance.md."

# Per-command slot drift (file one per command with substantial drift).
bd create --type=task --priority=2 \
  --title="beads-command-close: add --no-auto and --reason-file slots" \
  --description="bd close --help advertises both flags; class lacks them. See .cli-audit/20260429-172656/flags.md."

bd create --type=task --priority=2 \
  --title="beads-command-ado-sync: add 13 missing slots (filters, conflict resolution, reconciliation)"

bd create --type=task --priority=2 \
  --title="beads-command-update: reconcile slots with bd update --help"

bd create --type=task --priority=2 \
  --title="beads-command-create: reconcile slots with bd create --help"

bd create --type=task --priority=2 \
  --title="GitHub/GitLab/Linear/Jira/Notion/Azure DevOps sync classes: add per-provider sync slots"

# Missing-class clusters.
bd create --type=feature --priority=2 \
  --title="Add classes for top-level commands missing from beads.el (batch, compact, ping, prune, init-safety)" \
  --description="See .cli-audit/20260429-172656/coverage.md → 'Top-level commands with no class'."

bd create --type=feature --priority=2 \
  --title="Add classes for missing pull/push subcommands across integrations" \
  --description="ado.pull/push, github.pull/push, gitlab.pull/push, jira.pull/push, linear.pull/push, notion.pull/push. See .cli-audit/20260429-172656/coverage.md."

bd create --type=feature --priority=2 \
  --title="Add classes for missing config subcommands (config.apply, config.drift, config.show)"
```

Repo-relative path to this report:

    .cli-audit/20260429-172656/REPORT.md
