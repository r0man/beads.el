# Server-side pagination live QA — 1768-row proxied store

- Date: 2026-11-03
- Bead: `be-a4p0r` (QA: live-verify server-side pagination at ~1.7k rows +
  manual update)
- Feature bead: `be-ps0ki` (commit `224f330c`)
- Build: `main` at `c3683323` (no local code changes for this pass)
- Emacs: GNU Emacs 31.1 (`emacs -nw -Q`, `lisp/` from this checkout,
  `transient` 0.13.8 and `vui` 1.4.0 from the ambient site-lisp)
- `bd`: 1.3.2 (dev)
- Real store: `/home/roman/workspace/beads.el/.beads`, `dolt_mode` =
  `proxied-server`, 1768 issues (`bd list --all --limit 0`).
- Terminal: tmux `160x50`. Derived page size **43** entries
  (`window-body-height` 46 minus the pager's 3-row reserve; floor 5).
  Confirmed by the observed `items 1-43` range.

The real store was only read during the QA pass: no close/update/claim or
other issue-content write was issued, and marks stayed buffer-local. (The
worker claimed `be-a4p0r` before starting, as the workflow requires.)
Mutation and insert-shift tests ran on throwaway `mktemp -d` fixtures.

## Method

A fresh non-graphical Emacs per store, started in tmux:

```sh
tmux new-session -d -s qa-pager -x 160 -y 50
tmux send-keys -t qa-pager \
  "emacs -nw -Q -l /tmp/beads-qa-init.el" Enter
# /tmp/beads-qa-init.el:
#   (add-to-list 'load-path "/home/roman/workspace/beads.el/lisp")
#   (require 'beads) (require 'beads-command-list) (require 'beads-store)
```

The list was opened with `M-x beads-list-all` (all issues, limit 0,
`--order newest`). Menus and prompts were driven with keys
(`]`, `[`, `G`, `g`, `/`, `l`, `-s`); output below is `tmux
capture-pane` text, verbatim apart from elided buffer names.

Fixture stores:

```sh
# Proxied (server paging) — 60 issues.
benchmarks/large-store.sh 60 --proxied --seed-only

# Embedded (client pager) — 60 issues.
benchmarks/large-store.sh 60 --seed-only
```

Every fixture invocation unsets `BEADS_DIR` and the other isolation
variables, because the agent shell exports `BEADS_DIR` for the rig store.
The Emacs sessions for the fixtures were started with those variables
removed (`env -u BEADS_DIR ...`) so `bd` resolved the fixture, not the
rig.

## Real store, default spec (all issues, limit 0)

| Action | Observed mode line / message |
|---|---|
| open | `[Page 1, items 1-43 (more)]` |
| `]` | `[Page 2, items 44-86 (more)]` |
| `]` | `[Page 3, items 87-129 (more)]` |
| `[` | `[Page 2, items 44-86 (more)]` |
| `[` `[` | `[Page 1, items 1-43 (more)]` |
| `[` on page 1 | `Already on first page` |
| `G` 42 RET | `[Page 42, items 1764-1768]` (short last page, no `(more)`) |
| `]` on page 42 | `Already on last page` |
| `G` 43 RET | `[Page 43, no items]` + `No issues on page 43` |
| `[` from page 43 | `[Page 42, items 1764-1768]` |

1768 issues = 41 full pages of 43 plus 5 on page 42, matching the observed
boundaries. The last page has no `(more)`; `]` stops there.

## Real store, overall limit preserved

`beads-list-default-limit` was set to 100 for a separate session
(`/tmp/beads-qa-limit-init.el`):

| Action | Observed |
|---|---|
| open | `[Page 1, items 1-43 (more)]` |
| `]` | `[Page 2, items 44-86 (more)]` |
| `]` | `[Page 3, items 87-100]` (14 rows, no `(more)`) |
| `]` | `Already on last page` |

The per-page fetch is `min(page-size, remaining overall limit)`, so the
cap is not exceeded and the final page is short without asking for a
total.

## Filters (`/` and `l`)

- `/` → `s` → `closed` → RET (apply): page 1 was
  `[Page 1, items 1-43 (more)]` with only the `Closed` section, `]` →
  `[Page 2, items 44-86 (more)]`.
- From page 2, `/` → `s` → `open` → RET: **reset to page 1**,
  `[Page 1, items 1-1]` (the rig has one open issue).
- `l` (full `beads-list-filters` menu) → `-s` → `closed` → `l`:
  `[Page 1, items 1-43 (more)]`, then `]` →
  `[Page 2, items 44-86 (more)]`.

So both filter entry points compose with server paging and a new filter
resets to page 1. Filter/page indicators stayed on the filtered result
set throughout.

## Refresh behavior

- From page 3, `g` → `[Page 1, items 1-43 (more)]` and
  `Refreshed 43 issues on this page`: an explicit refresh resets to page 1.
- From page 2, `(beads-list-refresh t)` (the silent path the live journal
  uses) → page stayed at `[Page 2, items 44-86 (more)]`.

## Insert shift and duplicate-id drop (throwaway proxied fixture)

Fixture: 60 issues, proxied-server, page size 43.

1. Open `beads-list-all`: page 1 fetched `perf-00001`…`perf-00043`
   (`[Page 1, items 1-43 (more)]`).
2. Between page fetches, import an issue with an older `created_at` so
   the `--sort created --reverse` order shifts by one position:

   ```sh
   bd import /tmp/perf-old.jsonl    # id perf-old, created_at 2025-01-01
   ```

   (A first `bd create perf-8cu` had a current timestamp and sorted last,
   so it did not shift the window. `perf-old` is the row that shifts.)

3. `]` fetched page 2. `bd list --all --limit 43 --offset 43 --sort
   created --reverse` starts with `perf-00043` — the last row of the
   already-displayed page 1.
4. The UI page 2 did **not** contain `perf-00043`. Visible ids:
   `perf-00044`…`perf-00060`, `perf-8cu`; 18 rows,
   `[Page 2, items 44-61]` (no `(more)`).
5. `g` reset to page 1 and refetched; `perf-old` was present and
   `perf-00043` had moved to page 2.

The duplicate was dropped as designed. The mode-line range is offset-based
and counts kept rows, so after a drop it can understate the raw offset span
(see Finding 2).

`--sort id` is excluded from server paging live as well:

```elisp
(list (beads-list--server-paging-supported-p
       (beads-command-list :sort "id"))
      (beads-list--server-paging-supported-p
       (beads-command-list :sort "created")))
;; => (nil t)
```

## Embedded store keeps the client-side pager

Fixture: 60 issues, embedded Dolt, same 160x50 terminal.

| Action | Observed |
|---|---|
| open | `[Page 1/2, items 1-43 of 65]` |
| `]` | `[Page 2/2, items 44-65 of 65]` |
| `G` 1 RET | `[Page 1/2, items 1-43 of 65]` |

State in the buffer:

```elisp
(list beads-pager--server-paging
      beads-list--server-command
      (beads-store-offset-capable-p default-directory))
;; => (nil nil nil)
```

`bd` on the embedded route accepts `--offset 0` but rejects a meaningful
offset:

```text
$ bd list --limit 2 --offset 0 --json --flat   # exit 0
$ bd list --limit 2 --offset 2 --json --flat
Error: --offset is only supported under --proxied-server
# exit 1
```

The client pager mode line counts section headers as entries (`65` for
60 issues plus 5 status headers), so it differs from the server mode
line's issue count. That is pre-existing grouped-list behavior, not a
server-paging regression.

## Manual

`doc/views.texi` was checked against the observations above. Two
clarifications were added: the page size is window-derived and a resize
refetches the current page; `G` past the last page shows an empty page and
`[` returns. The existing statements about `(more)`, the short final page,
filters/overall limit, `--sort id`, refresh, duplicate dropping and the
embedded fallback all match the live behavior.

## Screenshot

No pagination image was added. The committed screenshot pipeline
(`docs/manual/screenshots/fixture.sh create`) builds a deterministic
**embedded** store with **12 issues** and a 220x88-class graphical frame,
so a page holds the whole list and server paging is never exercised. The
pipeline has no proxied-server/1.7k-row fixture, and adding one would
change its reproducibility contract (stable ids, embedded store, no
server process). Reproducing this view would therefore need a separate
fixture design, not a simple scope run.

```text
$ docs/manual/screenshots/fixture.sh create
...
Imported 12 issues from .../fixtures/beads-fixture.jsonl
$ bd context --json | jq -r .dolt_mode
embedded
```

## Findings (not regressions of the paging path)

1. **The last-resort capability probe is too weak.**
   `beads-store--probe-mode` tests `bd list --limit 1 --offset 0`, but the
   embedded route accepts offset 0. On an embedded store whose `bd` has no
   `bd context` and no `metadata.json`, the probe reports
   `"proxied-server"`. The primary path (`bd context`) returned
   `"embedded"` on the live fixture, so the observed UI was correct:

   ```elisp
   ;; batch Emacs, embedded fixture
   (list (beads-store--detect-backend store)
         (beads-store--probe-mode store)
         (beads-store-offset-capable-p store))
   ;; => ("embedded" "proxied-server" nil)
   ```

   Suggested fix: probe with `--offset 1` (or verify that offset 0 vs 1
   returns different rows). Low risk because `bd context`/metadata cover
   current stores, but the fallback is wrong as written.

2. **Transient/minibuffer resize + dedupe can show an empty page.**
   Opening a transient or `M-:` shrinks the list window, so the resize
   hook refetches the current page at the minimum page size (5). If that
   smaller page overlaps rows already shown on the previous page, the
   duplicate filter drops all of them and the menu is drawn over
   `Page N, no items`. Closing the menu refetches at 43 and the page is
   correct again. After such a refetch with drops, the mode-line range is
   computed from `(page-1)*page-size` and the kept count, so it can
   understate the real span (observed `Page 2, items 38-68` for kept rows
   that were really 44-74). Cosmetic and transient, but a follow-up could
   clear `beads-list--server-id-pages` when the effective page size
   changes, and/or compute the range from the kept ids.

3. **Out of scope: `newest` ordering looks reversed against bd 1.3.2.**
   `beads-issue-spec` maps `newest` to `--sort created --reverse`, but on
   1.3.2 `--sort created` returns newest first and `--reverse` returns the
   oldest first (`perf-00001` first, the newly inserted issue last). This
   changes which rows page 1 contains, but paging itself is consistent.
   Not touched here; worth a separate look.

   **Resolved (`be-uvqor`).**  `newest` now sends `--sort created` and
   `oldest` sends `--sort created --reverse`; `priority` (ascending) and
   `updated` (descending) were already correct.  Version matrix,
   checked directly with `bd list --flat --json` on throwaway embedded
   fixtures (one issue per second, then `--sort` / `--sort --reverse`):

   | bd | `--sort created` | `--sort created --reverse` | `--sort priority` | `--sort updated` |
   |---|---|---|---|---|
   | 1.3.1 | newest first | oldest first | priority ascending | newest first |
   | 1.3.2 | newest first | oldest first | priority ascending | newest first |

   So the mapping is correct for both the CI-pinned 1.3.1 and the city's
   1.3.2; there is no version split to carry.  ERT pins the argv in
   `lisp/test/beads-spec-test.el` (`beads-spec-test-to-args-order-*`,
   `beads-spec-test-refresh-order-*-command-line`).

## Quality gates

Run from the repository root before committing:

```sh
make check
make lint
make check-manual
```

Result on this machine: `make lint` byte-compiled 130 package files (the
only messages were unrelated `lexical-binding` notices from site-lisp
`sly` autoload files, not beads.el); `make check` ran 6670 tests with 6644
expected results, 0 unexpected and 26 skipped (`:live` UI tests);
`make check-manual` passed. `make` rebuilt the Info manual from the edited
`doc/views.texi`.

## Cleanup

The tmux sessions (`qa-pager`, `qa-limit`, `qa-fixture`, `qa-embedded`)
were killed and the throwaway fixtures were removed:

```sh
rm -rf /tmp/beads-large-store.Y0eiVQ /tmp/beads-large-store.iXspZj
docs/manual/screenshots/fixture.sh teardown \
  /tmp/beads-fixture.run.znETRpZ3/beads-fixture.project
```
