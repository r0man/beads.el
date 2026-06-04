# beads.el Command Parity — State, Path to 100%, DRY Exploration, gascity.el Benefit

> **Analysis / plan only — no implementation.** Deliverable for `bde-qj4i`.
> Captured against **`bd 1.0.5 (dev)`** on **2026-06-04**. All counts are
> reproducible from a fresh audit snapshot under
> `.cli-audit/20260604-184038/` (see [Reproducibility](#reproducibility)).

---

## TL;DR

| Question | Answer |
|----------|--------|
| **Command coverage** | **234 / 235 addressable bd commands = 99.6%.** One real gap: `bd migrate schema` (new in 1.0.5). |
| **Raw path coverage** | 234 / 261 = 89.7% (the 27-path shortfall is 25 router groups that are classless *by policy* + `comments.list`, which bd itself marks "Invalid"). |
| **Slot fidelity** | 214 / 234 implemented commands have **zero** slot drift = 91.5% clean. 20 carry minor drift; >½ of all drift is one measurement artifact. |
| **Orphans / inheritance** | 0 orphan classes, 0 inheritance gaps. Every class maps to a live `bd` command; every command with `Global Flags:` inherits `beads-command-global-options`. |
| **Should we generate classes from the CLI?** | **No (net-negative).** bd has no machine schema; the boilerplate that actually costs is the Emacs transient-UX layer, which is *not* CLI-derivable. Keep explicit definitions; promote the existing **audit harness** to a drift gate instead. |
| **Lightest DRY win that pays** | A shared *sync-provider* option mixin for the 24 integration classes (6 providers × 4 ops) in `beads-command-integrations.el`. |
| **gascity.el** | Inherits a hardened shared `beads-meta` engine, can reuse the audit harness verbatim, and — because `gc` *does* expose `--json-schema` — can pursue the schema-driven generation that bd cannot. |

Bottom line: parity is **effectively reached**. This is a *maintenance* problem
(keeping ~234 classes in lockstep with an evolving CLI), not a *coverage*
problem. Spend effort on the drift gate, not on a generator.

---

## Reproducibility

This analysis did not trust the prior snapshot (`.cli-audit/20260429-172656/`,
`bd 1.0.3`, 2026-04-29) — that snapshot predates the `bde-s84l` reconciliation
epic that *fixed* most of its findings, so its headline numbers (47 missing, 72
drifted) are stale. A fresh snapshot was generated:

1. **Command surface** — `bd <cmd> --help` walked recursively via cobra's
   universal `Available Commands:` section (depth-capped, cycle-guarded).
   → `commands.json`: **261 paths** (104 top-level + 157 subcommands).
2. **Class inventory** — every class deriving from `beads-command` was
   instantiated and its CLI path resolved by the *real*
   `beads-command-subcommand` method (ground truth, not a re-implementation).
   → `classes.json`: **238 classes**, **234 unique cli_paths**.
3. **Coverage diff** — set difference of the two path sets.
4. **Slot drift + inheritance** — the existing `audit.el` (re-pointed at the
   fresh run dir) parsed `Flags:` / `Global Flags:` per command and compared
   against each class's slot metadata.

Artifacts: `.cli-audit/20260604-184038/{commands.json, classes.json, flags.md,
inheritance.md, help/*.txt, audit.el}`. Full per-command table:
[`coverage-matrix.md`](./coverage-matrix.md).

---

## Part 1 — Command Parity

### 1.1 The coverage matrix (summary)

| Status | Count | Meaning |
|--------|------:|---------|
| **SUPPORTED** (clean) | 214 | Class exists; every CLI flag has a matching slot. |
| **PARTIAL** | 20 | Class exists; ≥1 slot-drift finding (missing flag). |
| **MISSING-GROUP** | 25 | Router group (`bd dolt`, `bd dep`, …) — classless **by policy**. |
| **MISSING-LEAF** | 2 | `migrate.schema` (real gap) + `comments.list` (bd-invalid). |
| **Total CLI paths** | **261** | |

(238 classes collapse to 234 cli_paths; the 5 `admin compact` mode-classes
collapse to one path, so 24 drifted classes appear as 20 PARTIAL rows.)

Full row-by-row table — all 261 commands with implementing symbol + file — is in
[`coverage-matrix.md`](./coverage-matrix.md).

### 1.2 Parity %, three honest framings

- **Raw path coverage:** `234 / 261 = 89.7%`. Pessimistic — it counts the 25
  router groups as "missing" even though the project deliberately gives them
  parent transients, not classes (`CLAUDE.md` → "Top-level group commands").
- **Policy-adjusted addressable coverage:** exclude the 25 by-policy groups and
  the bd-invalid `comments.list` → `234 / 235 = 99.6%`. **This is the honest
  number.** The lone shortfall is `migrate.schema`.
- **Clean-fidelity rate:** of 234 implemented commands, 214 have zero slot
  drift → `91.5%` are flag-perfect.

### 1.3 The gap (what's missing / partial), grouped

**A. MISSING-LEAF — actionable (1 command):**

| Command | Summary | Effort |
|---------|---------|-------:|
| `bd migrate schema` | Apply pending schema migrations (idempotent) | **~20 min** |

`migrate.schema` is **new in 1.0.5** (absent from the April surface). The
`migrate` group and its siblings `migrate.issues` / `migrate.sync` /
`migrate.hooks` already have classes in `lisp/beads-command-migrate.el`, so this
is a copy-of-sibling addition: one `beads-defcommand` with no command-specific
flags (it inherits global options only). This is the *entire* path to 100%
command presence.

**B. MISSING-LEAF — not actionable (1 command):**

`comments.list` — `bd comments list` prints *"Invalid — use `bd comments
<issue-id>` to list comments"* and exits. bd registers it only to emit that
redirect; it is not a real command. beads.el correctly omits it. **No action.**

**C. MISSING-GROUP — in policy (25 commands):** the 24 top-level router groups
(`admin`, `ado`, `audit`, `config`, `dep`, `dolt`, `epic`, `federation`,
`formula`, `gate`, `github`, `gitlab`, `hooks`, `jira`, `label`, `linear`,
`merge-slot`, `mol`, `notion`, `repo`, `rules`, `swarm`, `vc`, `worktree`) plus
the mid-level `dolt.remote`. Each already has a `transient-define-prefix` parent
menu and, per project policy, gets **no** `beads-defcommand` class (a class for
a router serializes to `bd <group>` with no args, which just prints help). **No
action — in policy.**
*(Note: `human`, `backup`, and `migrate` groups* do *carry classes — over-
coverage, harmless.)*

**D. PARTIAL — 20 commands with slot drift (93 findings).** All drift is
"missing slot" (92) + one description divergence (1); there are **zero** wrong
types, wrong short options, extra slots, or inheritance gaps. The 93 findings
decompose as:

| Cluster | Findings | Verdict |
|---------|---------:|---------|
| `admin compact` 5-class mode split | 54 | **Measurement artifact.** Each mode-class (`compact-stats/analyze/apply/auto` + `admin-compact`) is audited against the *combined* `bd admin compact --help`, so each flags the *other* modes' flags as "missing". The cluster as a whole is covered. See `lisp/beads-command-compact.el` header + `audit--intentional-collisions`. |
| `init` `--proxied-server*` / `--debug` | 14 | **Mostly intentional non-goals.** 12 of 14 are `[EXPERIMENTAL]` per-workspace proxied-dolt server tuning flags (TLS paths, sockets, keepalive); plus `--debug`, `--quiet`. Niche server-ops knobs that an Emacs UI legitimately need not surface. |
| Integration sync tuning (`linear.sync`, …) | ~6 | **Real but minor.** e.g. `--pull-if-stale`, `--threshold`, `--milestones`, `--no-wait`. |
| Aliases / globally-handled | ~3 | **Not real gaps.** e.g. `dep.add --depends-on` is an explicit *alias* for `--blocked-by` (already a slot); `list --no-pager` is handled globally. |
| Genuine 1-flag misses (`show --include-comments`, `list --skip-labels`, `info`, `doctor`, `setup`, `preflight`, `restore`, `prime`, `repo.*`, `swarm.validate`) | ~16 | **Real, low-priority.** One niche flag each. |

The single `[drift desc]` (`show --include-dependents`) is a **false positive**:
the slot's `:documentation` is *richer* than the CLI's one-liner (it explains the
JSON shape), which the first-sentence comparison flags as divergent. Leave it.

### 1.4 Path to 100% — effort per group

| Target | Work | Effort |
|--------|------|-------:|
| **100% command presence** | Add `beads-command-migrate-schema`. | **~20 min** |
| **100% "real" slot fidelity** | Add the ~16 genuine single-flag misses + ~6 sync-tuning flags across ~20 classes. Each is a 1–4 line slot addition. | **~3–4 h** |
| **Silence the `admin compact` artifact** | Teach `audit.el` to audit the 5-class cluster as a unit (union their slots before diffing against the combined help). Pure tooling change; no product code. | **~1–2 h** |
| **Resolve `init` experimental flags** | *Decision, not code:* declare `--proxied-server*` an explicit non-goal and annotate the class so the audit suppresses them. | **~30 min** |
| **Commands that are awkward / non-goals in Emacs** | `sql` (raw SQL — exists but a REPL fits better than a transient), `completion` (shell-only, correctly skipped), `__complete` (cobra-internal), the `--profile`/`--proxied-server*` server-ops flags. Flag as non-goals; don't chase. | — |

**Net: one ~20-minute class reaches 100% command presence.** Everything else is
fidelity polish where the residual is dominated by an artifact and by flags that
are reasonable to omit. Parity is, for practical purposes, done.

---

## Part 2 — DRY-ing Command Definitions (light)

Maintainer steer: *"what we have is already good — don't go too crazy, but
explore some grounds."* Honest cost/benefit follows.

### 2.1 What the machinery already collapses

`beads-defcommand` is already a thin wrapper over the shared engine
`beads-meta--expand-defcommand` (`lisp/beads-command.el:149`), extracted in
commit `9725a1b`. The engine + inference (`beads-meta.el`) already removes most
mechanical boilerplate:

- **CLI long-option inferred** from the slot name (`beads-meta--resolve-long-option`, `beads-meta.el:559`).
- **option-type / transient argument / description / reader-class / prompt
  inferred** from minimal slot specs (`beads-meta--run-inference`, `:674`).
- **Concise property names** (`:key`, `:transient`) expand bidirectionally to
  legacy names (`beads-meta--expand-concise-properties`, `:620`).
- **`:short-option` doubles** as the transient menu key.
- Custom slot props survive EIEIO subclassing via advice (`:707`, `:752`).

So a slot is often just `(reason :short-option "r" :transient beads-transient-
multiline :documentation "…" :group "…" :level 1)` — the long-option, type, and
argument are all derived. **The easy, CLI-shaped part is already DRY.**

### 2.2 Could we *generate* classes from the bd CLI? — No (net-negative)

Two questions: is there a schema source, and would generation actually save
work?

**(a) Is there an introspection source?**

| Source | Available on `bd`? | Yields |
|--------|--------------------|--------|
| `--json-schema` (like `gc` has) | **No** — `bd list --json-schema` → *"unknown flag"*. | — |
| `bd <cmd> --help` text | Yes | long-option, short-option, type (via metavar), description, sections. **The richest source.** |
| `bd __complete <cmd> "--"` (cobra) | Yes | flag long-name + description, machine-readable (TSV). **But no types, no short options, no positional/section info.** |

Verified: `gc` exposes `--json-schema` (`gc … --json-schema` →
`emit JSON Schema for this command`), but `gc bd <cmd> --json-schema` describes
only gc's *passthrough envelope* — *"The payload is owned by the bd CLI, not
gc"*, `additionalProperties: true`. **There is no machine schema for bd's own
flags.** Generation would rest on `--help` text parsing (the audit already notes
positional `Usage:` syntax is "too ambiguous to parse reliably").

**(b) Would generation save work? No — it targets the wrong half.**

Look at a real definition (`beads-command-close`, `beads-command-close.el:42`):
the lines that cost are `:group "Close Issue"`, `:level 1`, `:order 2`,
`:transient beads-transient-multiline`, `:reader beads--read-issue-at-point-or-
prompt`, `:prompt "Issue ID: "`. **None of that is derivable from `bd close
--help`.** It is the Emacs transient-UX layer — grouping, ordering, level
gating, custom readers, completion. A `--help`-driven generator produces exactly
the part inference *already* fills in (long-option, type, description) and is
silent on the part that actually takes judgment.

The failure mode of generation here is the classic one: you generate a class,
then hand-edit it for UX, then the next `bd` release regenerates and clobbers
your edits — so you build a merge/override layer, and now you maintain the
generator **and** the overrides **and** the CLI. That is *more* surface than 234
explicit, readable `defcommand` forms.

**Verdict:** generation **does not pay** for beads.el. The clarity the
maintainer values is worth more than the boilerplate it would (partially)
remove.

### 2.3 The right tool already exists: promote the audit to a drift gate

The real risk isn't *writing* 234 classes — it's *keeping* them in lockstep as
`bd` evolves (this analysis exists because the April snapshot went stale). The
`cli-audit` formula + `audit.el` already solve the hard parsing problem in the
*verification* direction. **Recommendation (highest ROI):**

- Promote `audit.el` from a one-shot formula artifact to a repeatable check —
  an `eldev` target / CI job that regenerates the snapshot and **fails on new
  MISSING-LEAF or new slot drift** (excluding the policy buckets + the
  `admin compact` cluster + declared non-goals).
- This keeps explicit definitions *and* makes drift impossible to merge
  silently — 90% of the "stay at 100%" benefit at ~10% of a generator's cost
  and zero loss of clarity.

### 2.4 Lighter factoring that *does* pay

1. **Sync-provider option mixin (the one real duplication).**
   `lisp/beads-command-integrations.el` defines 34 classes; 24 are a 6-provider
   (`ado/github/gitlab/jira/linear/notion`) × 4-op (`pull/push/sync/status`)
   matrix. The per-op flag sets are near-identical across providers. A shared
   option-group (a slot mixin or `beads-defcommand` superclass carrying the
   common sync flags) would cut the densest repetition in the tree **and** is
   where slot drift concentrates (the `*.sync` PARTIALs). *Pays.*

2. **Codify the policy buckets in data, not prose.** The 25 router groups + the
   `admin compact` cluster live partly in `CLAUDE.md` prose and partly in
   `audit--intentional-collisions`. Lift the router-group list into a single
   `beads-meta` constant the audit consumes directly. *Small, pays — removes a
   prose/code drift risk.*

### 2.5 What to leave alone (the maintainer is right)

- **Per-command explicit slot definitions for leaf commands.** The transient UX
  is the value and it can't be generated. Don't touch.
- **The inference engine's current depth.** It's at a good level; more "magic"
  (e.g. guessing groups/levels) would hurt readability for marginal savings.
- **The `admin compact` 5-class split.** It's an intentional UX decision (one
  transient per mode); fix the *audit's* view of it, not the product.

---

## Part 3 — How gascity.el Benefits

gascity.el is the Emacs UI for the `gc` CLI, built *on top of* beads.el's command
infra. `gascity-command.el` `(require 'beads-meta)` + `(require 'beads-command)`,
and `gascity-defcommand` is a thin wrapper over `beads-meta-defcommand` that
forwards `:global-section` and adds bridge methods
(`gascity-command.el:50–90`). It reuses `beads-meta-build-command-line`,
`beads-meta-slot-property`, and `beads-meta-define-transient`. So:

1. **A hardened shared engine, for free.** Every parity/fidelity fix flows
   through `beads-meta`, the exact code gascity.el's command layer compiles
   against. The 0-orphan / 0-inheritance-gap / 91.5%-clean state of beads.el is a
   direct proxy for the robustness of gascity.el's foundation. No gascity.el
   code changes required to benefit.

2. **The audit harness transfers verbatim.** `audit.el` is engine-agnostic
   (`beads-meta-command-slots`, `beads-meta-slot-info`). Point it at
   `gascity-command-*` classes and `gc <cmd> --help` and gascity.el gets the same
   coverage matrix + drift report. The drift-gate recommendation (§2.3) is a
   shared investment.

3. **gascity.el can do the generation beads.el can't.** `gc` *does* expose
   `--json-schema` per command (verified). The generation path that is
   net-negative for bd (no schema, text-only) is *viable* for gc (real machine
   schema). If schema-driven generation is ever worth building, it belongs on
   the gascity.el side — and it would emit classes through the *same*
   `beads-meta-defcommand` engine, so the investment is shared, not forked.

4. **The sync-mixin (§2.4.1) is reusable.** If common option-groups land in
   `beads-meta`, `gascity-defcommand` can pull the same mixins, cutting
   duplication in gascity.el's own command layer (which has analogous repeated
   shapes).

5. **More bd commands surfaced = more building blocks.** gascity.el reuses
   `beads-section`, `beads-dashboard`, `beads-pager`, `beads-tabulated`. Every
   command beads.el covers cleanly is a component gascity.el's dashboards can
   embed without re-deriving the command line.

**Net:** the parity work is *infrastructure* gascity.el sits on. Hardening
`beads-meta` and shipping the audit gate benefits both UIs from one effort, and
the one place generation makes sense (schema-driven, on `gc`) naturally lives in
gascity.el over the shared engine.

---

## Appendix — Key Numbers at a Glance

| Metric | Apr 29 (`1.0.3`) | Jun 4 (`1.0.5`) | Δ |
|--------|------:|------:|---|
| CLI command paths | 259 | 261 | +2 (`comments.list`, `migrate.schema`) |
| Classes | 216 | 238 | +22 |
| Unique cli_paths | 212 | 234 | +22 |
| Missing (raw) | 47 | 27 | −20 |
| Missing (actionable leaf) | ~6 | **1** | −5 |
| Orphans | 0 | 0 | — |
| Drifted classes | 72 | 24 | −48 |
| Slot-drift findings | 294 | 93 | −201 |
| Inheritance gaps | 0 (post-fix) | 0 | — |

The `bde-s84l` reconciliation epic (closed since April) cut actionable missing
classes by ~85% and slot drift by ~68%. The remaining `bd 1.0.5` surface added
exactly one addressable command (`migrate.schema`). **beads.el is at parity; the
task now is keeping it there — which is a gate, not a generator.**
