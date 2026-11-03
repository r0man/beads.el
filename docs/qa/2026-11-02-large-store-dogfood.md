# beads.el large-store dogfood — performance and pagination

- Date: 2026-11-02
- Bead: `be-wje4a` (Dogfood: large-store performance and pagination via
  `bd --offset/--limit`)
- Build: `main` at `4cdcab34`, Emacs 31.1, `bd` 1.3.2 (dev), Dolt
  embedded for the timing fixture and proxied-server for the real-store
  read-only probe.
- Fixture: throwaway `mktemp -d` store, 2500 issues, built by
  `benchmarks/large-store.sh`. No real `.beads` store was written to.

## Fixture

The existing large-store generator was extended rather than replaced:

```sh
# 2500 issues, embedded Dolt, dependencies, deterministic timestamps,
# keep the store for the pagination probes, and write a CPU profile.
benchmarks/large-store.sh 2500 --repeat 3 --keep --profile

# Proxied-server route, required for bd --offset experiments.
benchmarks/large-store.sh 500 --proxied --seed-only

# Build only and print the path.
benchmarks/large-store.sh 2500 --seed-only
```

What the generator now emits:

- Explicit ids `perf-00001`…`perf-N`, so dependency edges can be imported
  in one pass.
- Mixed statuses (open/in_progress/blocked/closed/deferred), all five
  priorities, task/bug/feature/chore types, three labels per issue, and
  assignees.
- Dependencies: every 4th issue `blocks` the issue 4 slots earlier; every
  10th also has a `related` edge. This makes `bd ready`/blocked filtering
  realistic.
- Deterministic `created_at`/`updated_at` from a fixed epoch, with three
  consecutive issues sharing a timestamp so equal-key tie behavior is
  exercised.
- Isolation variables (`BEADS_DIR`, `BEADS_DOLT_PORT`, …) are unset
  before any `bd` call, exactly as the test suite and screenshot fixture do.

The 2500-issue fixture contains 250 closed issues, so `bd list` without
`--all` returns 2250 rows. That is why the "parsed 2250 issues" row below
is smaller than 2500.

## Method

`benchmarks/large-store.el` times each operation best-of-3 in one batch
Emacs. `bd` reads are counted with advice on `beads-command-execute` and
`beads-command-execute-async`. Dashboard measurements pump the event loop
until no section is `Loading…` **and** the global async queue is empty;
the earlier "refresh" number was wrong until this was fixed because
stale-while-revalidate keeps old rows on screen during a soft refresh.

Resident set size is read from `/proc/self/status` (`VmRSS`).
The CPU profile uses Emacs' sampling profiler around list-all plus
dashboard open/refresh; the full tree is expanded before writing.

## Numbers (2500 issues, embedded Dolt, best of 3)

### Core fetches and pure-Emacs work

| Measurement | Best | bd reads |
|---|---:|---:|
| `bd list --json` (process + parse + coerce) | 0.927 s | 1 |
| `json-read-from-string` (parse floor) | 0.087 s | 0 |
| `beads-command-parse` (coercion) | 0.355 s | 0 |
| `beads-list--populate` flat (2250 rows) | 0.072 s | 0 |
| `beads-list--populate` grouped | 0.079 s | 0 |
| dashboard `render-ready` | 0.002 s | 0 |
| dashboard `render-blocked` | 0.001 s | 0 |

### List view (`l` / filter path)

| Measurement | Best | Rows fetched |
|---|---:|---:|
| open default spec (status=open, limit 50) | 0.488 s | 50 |
| refresh default spec | 0.501 s | 50 |
| open all (limit 0, no `--all`) | 1.191 s | 2250 |
| refresh all (limit 0) | 1.219 s | 2250 |
| filter status=open, limit 0 | 1.017 s | 1000 |
| filter type=bug, limit 0 | 0.799 s | 425 |
| filter priority=2, limit 0 | 0.903 s | 500 |
| filter label=performance, limit 0 | 1.134 s | 2250 |
| filter ready-only, limit 0 | 1.091 s | — |
| mark all visible rows | 0.027 s | 0 |
| unmark all rows | 0.007 s | 0 |

RSS after list-all open: **200.0 MB** (the process had already parsed and
rendered the same data in earlier measurements, so the delta read 0; the
absolute figure is the useful one).

### Dashboard (vui board)

| Measurement | Best | bd reads |
|---|---:|---:|
| open all sections | 5.129 s | 9 |
| refresh all sections | 5.461 s | 9 |
| depth M-1 re-render | <0.001 s | 0 |
| depth M-2 re-render | <0.001 s | 0 |
| depth M-3 re-render | <0.001 s | 0 |
| depth M-4 re-render | <0.001 s | 0 |
| depth M-0 re-render | <0.001 s | 0 |
| open fan-out (8 async loaders) | 3.039 s | 8 |

Per-section single-loader fetch (one async read each):

| Section | Best |
|---|---:|
| stats | 0.202 s |
| closed | 1.011 s |
| in-flight | 0.770 s |
| ready | 0.629 s |
| blocked | 1.006 s |
| epics | 0.309 s |
| stale | 0.256 s |
| orphans | 0.410 s |

RSS after dashboard open: **200.2 MB**.

### Profile hotspots

Sampling profile over list-all + dashboard open/refresh (895 samples):

- `vui--rerender-instance` / `vui--render-vnode` — **~55% cumulative**.
  Most of the board's CPU is vui reconciling/rendering the section tree.
- `beads-coerce-json-value` → `beads-from-json` — **~39% cumulative**.
  Object coercion still dominates the pure-Emacs side; `json-read` itself
  is ~6%.
- `Automatic GC` — **13%**.
- `process-file`/async subprocess waiting is small in CPU terms but is the
  wall-clock cost of the per-section fetch timings above.

These are tracked as `be-07g7g` (dashboard open/refresh cost).

## Pagination investigation

### What works

`bd list --offset` works **only on the proxied-server route**. On the
embedded/direct route used by tests and throwaway fixtures:

```text
$ bd list --limit 2 --offset 2 --json --flat
Error: --offset is only supported under --proxied-server
(exit 1)
```

On a 200-issue proxied fixture, paging with `--limit 50` covered every
row exactly once for default sort and for `created`, `priority`, `status`,
`updated`, and `title`:

```text
sort=None     pages=4 rows=200 unique=200 dups=0
sort=created  pages=4 rows=200 unique=200 dups=0
sort=priority pages=4 rows=200 unique=200 dups=0
sort=status   pages=4 rows=200 unique=200 dups=0
sort=updated  pages=4 rows=200 unique=200 dups=0
sort=title    pages=4 rows=200 unique=200 dups=0
```

The same result held three times in a row for each sort on a static
fixture, including created/priority ties (the fixture deliberately has
three issues per created second). `--limit 0 --offset 10` returned the
remaining 190 rows, so offset is applied before the limit. Filters and
offset compose correctly (status/type/priority/label paged sets matched
the full set).

`bd ready --offset` exists. `bd blocked` has **no** `--offset` flag.

`--sort id` is rejected outright:

```text
$ bd list --all --limit 50 --offset 0 --sort id --json --flat
Error: --offset is not supported with --sort id (sort requires fetching
the full result set)
```

### Why it is not enough

1. **Route split.** beads.el supports both embedded and proxied stores.
   Wiring offset in would need a capability probe and a fallback, or list
   paging would break on every standalone/local fixture.
2. **Not every command pages.** The dashboard's Blocked section uses
   `bd blocked`, which has no offset; search has its own limit semantics.
   A uniform list/dashboard pager cannot be built on this flag alone.
3. **No snapshot or deterministic tiebreaker.** Fetching page 1,
   inserting one new issue, then fetching page 2 duplicated a row:

   ```text
   before insert p1: perf-00199 perf-00200 perf-00196 ... perf-00191
   before insert p2: perf-00192 perf-00187 ... perf-00183
   insert: perf-snto
   after insert p2:  perf-00191 perf-00192 ... perf-00182
   overlap after insert: {perf-00191}
   ```

   Equal-key ties were stable in repeated static-fixture runs, but the CLI
   documents no unique secondary sort key. A cursor/keyset API would be
   needed for duplicate-free scrolling under writes.
4. **The latency win is modest.** On the real rig store (1759 rows,
   proxied, read-only) the full fetch is not much slower than one page:

   | Query | Best of 3 |
   |---|---:|
   | page 50, offset 0 | 0.159 s |
   | page 50, offset 800 | 0.222 s |
   | full 1759 rows (`--limit 0`) | 0.238 s |
   | default 50 (no offset) | 0.122 s |

   On the 500-issue proxied fixture, a full fetch (0.106 s) was as fast as
   one 50-row page (0.107 s); ten sequential pages took 0.924 s. Process
   and server startup dominate, and the client still has to parse and
   render whatever it receives. The current client-side `beads-pager` over
   a bounded fetch is the better trade-off.

### Decision

Do not ship partial server-side pagination in beads.el now. Keep the
existing client-side pager and the default CLI limits. Revisit if `bd`
gains keyset/cursor pagination, offset parity on the direct route, offset
on blocked/search, and a documented unique tiebreaker. Tracked as
`be-y0v5c`.

### Addendum 2026-11-02: optional server-side paging (be-ps0ki)

The blanket "do not ship" decision above was superseded the same day.
The list view now pages on the server **only** when capability detection
confirms the store runs on the proxied-server route:

- `bd context --json` is the primary probe (`dolt_mode` is
  `proxied-server`); `.beads/metadata.json` is the fallback, and a
  cached one-row `bd list --limit 1 --offset 0` probe is the last
  resort.  The answer is cached per store.
- A capable list buffer fetches `--limit PAGE --offset K` per page,
  preserving filters and the user's overall limit.  Page size is the
  pager's window-derived size.  The mode line shows `Page N, items A-B`
  and `(more)` while a page is full; no total is computed.
- Embedded/direct stores keep the existing client-side pager over a
  bounded fetch.  `bd blocked`, search and dashboard sections are
  untouched.
- `--sort id` is never combined with `--offset`.  Duplicate ids caused
  by an insert between fetches are dropped, and `g` reloads page 1.  An
  insert can still shift rows, so the snapshot caveat remains.

## Findings filed

- `be-4tjw2` — List UI ignores `beads-list-default-limit`; "All issues"
  fetches 50 and skips closed.
- `be-07g7g` — Dashboard open/refresh costs ~5 s and 9 `bd` reads on a
  2.5k store.  Follow-up measurement and the proxied-server concurrency
  fix are in `docs/qa/2026-11-02-dashboard-open-refresh.md`; the
  embedded 9-read floor is unchanged (it needs an upstream `bd` read
  batch or a product decision to lazy-load folded sections).
- `be-y0v5c` — Pagination decision: keep client-side pager; `bd --offset`
  not viable yet.

## Reproduce

```sh
# Timing run (roughly 4.5 minutes on this machine)
benchmarks/large-store.sh 2500 --repeat 3 --keep --profile

# List the generated files; the report is written next to the store.
ls /tmp/beads-large-store.*/large-store-profile.txt

# Pagination route check: direct must fail, proxied must page.
benchmarks/large-store.sh 500 --proxied --seed-only
# cd to the path printed by the command above.
cd /tmp/beads-large-store.XXXXXXXX
for off in 0 50 100; do
  bd list --all --limit 50 --offset "$off" --json --flat \
    | python3 -c 'import json,sys; print(len(json.load(sys.stdin)))'
done
bd list --all --limit 0 --json --flat \
  | python3 -c 'import json,sys; print(len(json.load(sys.stdin)))'
```

The profile file is a plain Emacs `profiler-report` call tree. Numbers in
this report were taken on one machine and are best-of-3; treat them as
relative guidance, not an SLA.
