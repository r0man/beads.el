# beads.el dashboard open/refresh cost — 2.5k-store investigation

- Date: 2026-11-02
- Bead: `be-07g7g` (Dashboard open/refresh costs ~5 s and 9 `bd`
  reads on a 2.5k store), follow-up to `be-wje4a`
- Build: `main` at `224f330c`, Emacs 31.1, `bd` 1.3.2 (dev)
- Fixture: throwaway 2500-issue embedded-Dolt store built by
  `benchmarks/large-store.sh 2500 --seed-only`
- Measurement: `benchmarks/large-store.el` (best of 3); every
  `BEADS_*`/`GC_DOLT_*` isolation variable unset before each run, as
  the shell driver does.

## Summary

The embedded-store dashboard open/refresh is **wall-clock bound by
nine serial `bd` subprocesses**, not by vui rendering or JSON
coercion.  On this fixture nothing safe cuts the default open by 30%
or removes three `bd` reads: every cheap read is already bounded,
every fold is already fetched by design, and the only way to remove
reads is to re-derive `bd`'s blocker/stale/orphan semantics in Emacs,
which would change visible rows on stores that use gates, deferred
work or non-default `bd` config.

One real, safe win was found and shipped for the **proxied-server**
route (Gas Town's production route): the concurrency probe treated
only `mode=server` as server-backed, so a `proxied-server` store was
capped at one reader and paid the full serialized cost even though its
dolt sql-server handles concurrent readers.  Mapping
`mode=proxied-server` to the server cap cuts dashboard open from
**1.12 s to 0.31 s** and refresh from **0.96 s to 0.24 s** on a
1.8k-issue proxied store, with byte-identical dashboard contents.
Embedded stores keep the cap of one (parallel readers are slower
there — measured below).

## Baseline (embedded 2.5k fixture, best of 3)

| Measurement | Best | bd reads |
|---|---:|---:|
| dashboard open, all sections | 3.873 s | 9 |
| dashboard refresh, all sections | 4.377 s | 10 |
| dashboard open fan-out (8 loaders, async) | 4.157 s | 9 |

Run-to-run spread on this host is ±25% (the same benchmark has since
reported 3.9–5.0 s open and 3.7–4.4 s refresh), so only changes larger
than that are meaningful here.

Per-section single-loader fetch:

| Section | Best |
|---|---:|
| stats (`bd status`) | 0.203 s |
| closed (`bd list --status closed --limit 5 --sort closed`) | 0.453 s |
| in-flight (`bd list --status in_progress`) | 0.625 s |
| ready (`bd ready`) | 0.452 s |
| blocked (`bd blocked`) | 0.805 s |
| epics (`bd epic status`) | 0.201 s |
| stale (`bd stale`) | 0.252 s |
| orphans (`bd orphans`) | 0.403 s |

The nine reads are distinct command shapes:

```text
bd status
bd list --status closed --limit 5 --sort closed
bd list --status in_progress
bd ready
bd blocked
bd epic status
bd stale
bd orphans
bd list --all --limit 5 --sort updated      # registered recent-changes
```

There is no duplicate shape to coalesce, and `beads-command--single-flight`
already coalesces identical `:cache-key`s.

## Directions evaluated

### 1. Fetch-on-expand for folded sections — rejected

Folded sections deliberately *do* load: the count in a folded header
comes from the payload, and unfolding is instant (§5.4; enforced by
`beads-dashboard-test-section-fold-keeps-loaded-data`).  The default
fold state expands every section except Federation, which only mounts
on a server backend, so the default 2.5k open has no folded section to
skip and the measured read count would not move.  Making folds lazy
would either drop the header count while folded or make unfolding show
a loading skeleton — both visible changes.

### 2. Coalesce identical reads — no-op

See the command list above: nine distinct shapes, no shared request.
The dashboard's `:cache-key`s and `beads-command--single-flight` already
guarantee one process per shape per generation.

### 3. Derive sections from fewer fetches — rejected as unsafe

`bd list --all --limit 0 --json --brief` is 0.53–0.92 s on the fixture
and is a superset of the four list-shaped sections (closed, in-flight,
recent-changes, stale).  It is still the wrong trade:

- **Scalability regression.**  The current loaders are bounded
  (5/50/100 rows, plus `bd blocked`/`bd orphans`, which are unbounded
  in the CLI but still one dataset each).  Replacing them with one
  `--limit 0` fetch of the whole store makes a 100k-issue store load
  every issue on every refresh.
- **Order is not reproducible.**  Local sorting does not match `bd`'s
  tie handling.  On this fixture, `bd list --sort updated` orders the
  three issues sharing `2026-01-01T00:13:52Z` as
  `perf-02497, perf-02498, perf-02499` (id ascending), while a local
  `(updated_at desc, id desc)` sort gives the reverse.  Closed-at ties
  have the same issue.
- **Semantics are missing from the list payload.**  `bd ready` applies
  blocker, gate, deferred-until and ephemeral/wisp rules, and
  `bd blocked`/`bd stale`/`bd orphans` have their own definitions and
  configurable thresholds.  A client-side reimplementation would drift
  from `bd` on exactly the stores that need those rules, and would
  need a `bd`-version compatibility matrix to stay honest.  The stats
  summary cannot substitute either: on this fixture `bd status`
  reports `ready_issues=626` while `bd ready` reports 1001.

### 4. Rendering / coercion — not the bottleneck

Instrumenting `vui--rerender-instance` across a dashboard open plus
two soft refreshes (31 rerenders) measured **0.12–0.15 s total** of
render time, ~4% of open.  `beads-command-parse` (all 2250 non-closed
rows) is 0.319 s; the per-section payloads are far smaller.  The vui
and `beads-from-json` hotspots in the original profile predate the
vui 1.4/current render path and are not wall-clock-relevant.

### 5. More concurrency on embedded Dolt — measured slower

Fan-out of the eight built-in loaders against the embedded fixture,
same fixture and transport, cap set explicitly:

| Cap | Wall time (3 runs) |
|---:|---|
| 1 | 3.44–4.13 s |
| 2 | 3.96–5.22 s |
| 4 | 4.09–6.34 s |
| 8 | 5.80–7.94 s |

The embedded store's exclusive lock makes parallel `bd` readers queue
and spin; the probe's cap of one is correct there.

## Shipped change: proxied-server concurrency

`bd dolt status --json` reports `mode: proxied-server` for a store
whose CLI talks to a dolt sql-server through the local proxy.  The
policy probe mapped only the literal `server` to 8 and everything
else — including `proxied-server` — to 1.  Because `proxied-server` is
server-backed, concurrent readers are safe; the new
`beads-command--policy-server-modes` constant lists both server modes.

Measured on the real beads.el store (1766 rows, proxied, read-only)
with the dashboard's nine loaders, then with the full board:

| Scenario | Before (cap 1) | After (cap 8) |
|---|---:|---:|
| 9-loader fan-out (best of 3) | 0.995 s | 0.301 s |
| dashboard open (3 runs) | 1.117–1.200 s | 0.307–0.322 s |
| dashboard refresh (6 runs) | 0.962–1.464 s | 0.242–0.358 s |

Rendered-board dumps at cap 1 and cap 8 are byte-identical (`diff`
empty); no section errors at either cap.
Embedded behavior is unchanged (cap 1): the post-change benchmark run
reported 4.771 s open / 3.728 s refresh / 9 and 9 reads, inside the
run-to-run spread of the baseline.

Tests: `beads-command-test-async-policy-probe-proxied-server-mode` and
`beads-command-test-async-policy-probe-stub-embedded-mode` (stub `bd`
reporting each mode); the existing remote probe test still covers
`server`.

## What would actually move the embedded number

The embedded open is ~0.2–0.8 s per `bd` process × nine.  Safe
client-side options are exhausted; the levers are:

1. An upstream `bd` read-batch/multi-query command (one process, one
   store open, several datasets), or `--limit`/`--brief` for
   `bd blocked` (currently unbounded and returns 624 rows on this
   fixture for five visible rows).
2. A product decision to make the hygiene sections (Blocked, Stale,
   Orphans, Epics) fold-and-fetch-on-expand by default, accepting that
   their counts appear only after unfolding.  That is a visible change
   and belongs in its own bead.
3. A running dolt server for large embedded stores (the proxied route
   is already 3–4× cheaper per read and now parallel).

Recommendation: keep the default open as-is.  The proxied concurrency
fix removes the serialization penalty on the production route; the
embedded 9-read floor needs an upstream CLI improvement.

## Reproduce

```sh
# Embedded 2.5k fixture (keep the store for reruns).
benchmarks/large-store.sh 2500 --seed-only
BEADS_BENCH_DIR=<printed path> BEADS_BENCH_N=2500 \
  BEADS_BENCH_REPEAT=3 emacs --batch -Q -L lisp -l benchmarks/large-store.el

# Proxied route: open the board against a real store, before/after the
# policy cap.  Bind `beads-command-async-max-concurrent' to 1 or 8 to
# model the old and new probe.
```
