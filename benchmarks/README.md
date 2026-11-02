# Large-store benchmark

`large-store.sh` builds a throwaway git + beads project seeded with N
issues (default 1200; the dogfood runs use 2500) and runs
`large-store.el` under a batch Emacs to time the list/dashboard hot paths
and count `bd` reads per operation.  The seed has explicit ids, mixed
statuses/priorities/types/labels, deterministic timestamps with ties, and
blocking/related dependencies, so `bd ready` and the dashboard sections
see realistic data.

It is the measurement tool behind the "Measure and improve behavior on
large stores" work; the numbers below are a point-in-time record.

## Usage

```sh
# From the repository root:
benchmarks/large-store.sh                    # 1200 issues, best of 3
benchmarks/large-store.sh 3000               # custom issue count
benchmarks/large-store.sh 1200 --repeat 5    # more repetitions
benchmarks/large-store.sh 1200 --label pr-78 --keep
benchmarks/large-store.sh 2500 --repeat 3 --keep --profile
benchmarks/large-store.sh 500 --proxied --seed-only
```

Options:

| Flag | Meaning |
|------|---------|
| `N` | Issue count to seed (default 1200). |
| `--repeat K` | Repetitions per measurement, best-of (default 3). |
| `--label TEXT` | Free-form label shown in the report header. |
| `--keep` | Keep the seeded store and print its path instead of deleting it. |
| `--proxied` | Init with `bd init --proxied-server` instead of the embedded engine.  Required for `bd --offset` experiments. |
| `--deps N` | Make every Nth issue depend on the issue N slots earlier (default 4; 0 disables).  Also adds a `related` edge every 10th issue. |
| `--seed-only` | Build the fixture, print its path, and exit without running Emacs.  Implies `--keep`. |
| `--profile` | Run the CPU profiler over list-all + dashboard open/refresh and write `large-store-profile.txt` into the fixture. |

The store lives under `mktemp -d` and is removed on exit unless
`--keep`.  Every isolation variable that could reroute `bd` to a shared
production store (`BEADS_DIR`, `BEADS_DOLT_PORT`,
`BEADS_DOLT_SERVER_PORT`, `GC_DOLT_PORT`) is unset before any `bd`
call — the same list the test suite uses.

## Requirements

`bd`, `dolt`, `git`, a POSIX shell with arithmetic, and an Emacs with
beads.el's dependencies (`transient`, `vui`) on the load path.  Point
`EMACS` at a specific binary to override the default.  Under Guix:
`guix shell -D -f guix.scm -- benchmarks/large-store.sh`.

The seeded JSONL intentionally uses varied statuses, types, priorities,
assignees, labels and dependencies so every dashboard section and list
grouping has data.  Three consecutive issues share a `created_at`
timestamp so equal-key sort ties are exercised.  The generator unsets
`BEADS_DIR` and the Dolt port variables before every `bd` call, so it
cannot write to a real store.

Proxied fixtures are deliberately slow to seed (500 issues took about two
minutes here; 2500 did not finish in 15 minutes), because the proxied
route commits differently.  Use embedded for the 2500 timing run and a
small proxied fixture (`--seed-only`) only for `--offset` probes.

## What is measured

* `bd list --json` end to end — subprocess + JSON parse + object coercion.
* `json-read-from-string` — the raw parse floor, no coercion.
* `beads-command-parse` — string to `beads-issue` objects (the hot path).
* `beads-list--populate` flat and status-grouped.
* Dashboard `render-ready` / `render-blocked`.
* Dashboard open fan-out — all 8 built-in section loaders driven
  concurrently through `beads-command-execute-async`.
* Per-section loader latency (one async read each).
* List open/refresh through `beads-list--refresh` (default spec and
  limit 0), filters, and mark/unmark-all.
* Dashboard open/refresh, M-0..M-4 depth re-renders, plus RSS after the
  list and dashboard paths.

The count column is the number of `bd` reads issued during the
operation, counted with advice on `beads-command-execute` and
`beads-command-execute-async`.  Most pure-Emacs rows are 0; the first
row is 1.

## Recorded baseline (2026-10, Emacs 31.1, bd 1.3.1, 1200 seeded)

`beads-from-json` used to re-derive each slot's JSON key, initarg and
type for every parsed object.  `beads-issue` has 63 slots, so the
coercion was O(slots²) per object.  Memoizing the per-class slot table
(`beads-meta--class-slot-meta`) changed the picture:

| Measurement | Before | After | Speedup |
|-------------|-------:|------:|--------:|
| `bd list --json` (process + parse + coerce) | 1.64 s | 0.56 s | 2.9x |
| `beads-command-parse` (coercion) | 1.26 s | 0.16 s | 8.1x |
| `json-read-from-string` (baseline) | 0.036 s | 0.036 s | — |
| `beads-list--populate` flat | 0.034 s | 0.034 s | — |
| `beads-list--populate` grouped | 0.044 s | 0.044 s | — |
| dashboard `render-ready` | 0.001 s | 0.001 s | — |
| dashboard open fan-out (8 sections, async) | 2.84 s | 2.13 s | 1.3x |

The coercion win is the point: it is pure Emacs CPU on the critical
path.  The full `bd list` round trip only moves 2.9x because it still
pays the `bd`/Dolt subprocess and the raw `json-read-from-string`
parse, both outside beads.el.  The dashboard open fan-out is
subprocess-latency bound (8 `bd` reads under the concurrency cap), so
it benefits least (1.3x); its remaining cost is not object coercion.

## Recorded baseline (2026-11, Emacs 31.1, bd 1.3.2, 2500 seeded; best of 3)

The dogfood report with the full table is
`docs/qa/2026-11-02-large-store-dogfood.md`.  Headline numbers:

| Measurement | Best | bd reads |
|---|---:|---:|
| `bd list --json` (process + parse + coerce) | 0.927 s | 1 |
| `beads-command-parse` (coercion) | 0.355 s | 0 |
| list open all (limit 0, 2250 rows) | 1.191 s | 1 |
| list default spec (50 rows) | 0.488 s | 1 |
| dashboard open all sections | 5.129 s | 9 |
| dashboard refresh all sections | 5.461 s | 9 |
| dashboard open fan-out (8 loaders) | 3.039 s | 8 |
| RSS after list-all open | 200.0 MB | — |

The profile over list-all + dashboard open/refresh is dominated by
`vui--rerender-instance` (~55% cumulative) and
`beads-coerce-json-value`/`beads-from-json` (~39%).  Section fetch
latency is mostly `bd` subprocess/server time; list parsing is still
the main pure-Emacs cost.

## Adding a measurement

`large-store.el` is a plain batch script.  Add a `beads-bench--measure`
call with a label and thunk; `beads-bench--main` prints the table.
Measurements are best-of-`--repeat` so a cold first run does not
distort the result.
