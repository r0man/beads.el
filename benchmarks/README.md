# Large-store benchmark

`large-store.sh` builds a throwaway git + beads project seeded with N
issues (default 1200, the size of the real stores this exists to
protect) and runs `large-store.el` under a batch Emacs to time the
pure-Emacs hot paths and count `bd` reads per operation.

It is the measurement tool behind the "Measure and improve behavior on
large stores" work; the numbers below are a point-in-time record.

## Usage

```sh
# From the repository root:
benchmarks/large-store.sh                    # 1200 issues, best of 3
benchmarks/large-store.sh 3000               # custom issue count
benchmarks/large-store.sh 1200 --repeat 5    # more repetitions
benchmarks/large-store.sh 1200 --label pr-78 --keep
```

Options:

| Flag | Meaning |
|------|---------|
| `N` | Issue count to seed (default 1200). |
| `--repeat K` | Repetitions per measurement, best-of (default 3). |
| `--label TEXT` | Free-form label shown in the report header. |
| `--keep` | Keep the seeded store and print its path instead of deleting it. |

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
assignees and labels so every dashboard section and list grouping has
data.

## What is measured

* `bd list --json` end to end — subprocess + JSON parse + object coercion.
* `json-read-from-string` — the raw parse floor, no coercion.
* `beads-command-parse` — string to `beads-issue` objects (the hot path).
* `beads-list--populate` flat and status-grouped.
* Dashboard `render-ready` / `render-blocked`.
* Dashboard open fan-out — all 8 built-in section loaders driven
  concurrently through `beads-command-execute-async`.

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

## Adding a measurement

`large-store.el` is a plain batch script.  Add a `beads-bench--measure`
call with a label and thunk; `beads-bench--main` prints the table.
Measurements are best-of-`--repeat` so a cold first run does not
distort the result.
