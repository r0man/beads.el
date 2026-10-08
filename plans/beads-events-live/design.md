---
plan_slug: beads-events-live
phase: design
rig: beads.el
rig_root: /home/roman/workspace/beads.el
artifact_root: /home/roman/workspace/beads.el/plans
schema: beads.events-live.design.v1
artifact: design
status: draft
requirements: requirements.md
supersedes: design-seed.md
scope: planning-only
bd_version: 1.3.1
created_at: 2026-10-08T20:15:00Z
updated_at: 2026-10-08T20:15:00Z
---

# beads-live — Design: extend the existing beads.el / gascity.el UI with the bd events journal

> Planning only. This document changes no `.el` source. It is the design of
> record for `plans/beads-events-live/`; it supersedes `design-seed.md`.

## 0. Thesis in one paragraph

`bd` 1.3 already journals every mutation as an ordered, replayable row
(`bd events tail --since N --follow`). This design adds **one** supervised
`bd events tail --follow` stream per store, mirrors the proven `gascity-live` /
`gascity-events` / `gascity-event` / `gascity-pulse` pattern, and feeds the
**existing** beads surfaces — the vui dashboard, the tabulated list, the
sectioned show buffer, the `beads-section-register` registry, and the
`beads-menu` transients — from that stream instead of a 30s full `bd` poll.
Rewind, per-issue diffs, hooks, notifications, and a merged city timeline are
beads-specific layers on the same model. No parallel dashboard, view, event
model, or stream is created.

## 1. Critique of the seed (what this design supersedes)

`design-seed.md` is a good sketch but it is **not** the design of record. Three
substantive problems, and the corrections adopted here:

1. **It reinvents the gascity file topology instead of mirroring it.** The seed
   proposed seven `beads-events-*` modules plus a new `beads-live-mode`, while
   the constraint is to mirror `gascity-live.el` / `gascity-events.el` /
   `gascity-event.el` / `gascity-pulse.el` — four files, one concern each. The
   seed's `beads-events-model.el` reimplements the role `gascity-event.el`
   already plays, and its `beads-events.el` mixes stream, model, views, and
   customization. **Correction:** four net-new modules with a 1:1 role map
   (§3), and no model class at all — `beads-event-record` already exists in
   `beads-types.el` and matches the `bd events` contract exactly (§1.3).

2. **It runs the stream through `beads-command-execute-async`, which cannot
   work.** That generic is request/response: it forces `--json`, waits for the
   process to exit, and calls a completion callback. `tail --follow` never
   exits. The seed even forbids `shell-command`, but the actual gascity pattern
   spawns a process directly (`gascity-live--spawn`), remote included, via a
   local `ssh` pipe. **Correction:** `beads-live.el` spawns the stream with
   `make-process` (§3.1); `beads-command-execute-async` is used only for the
   finite snapshot/poll reads, where its single-flight and concurrency cap are
   appropriate (§4.4).

3. **It leaves "no duplicate streams" and the gascity relationship vague.** The
   seed says "one stream per city" but never defines the key, never separates
   the `gc events` journal from the `bd events` journal, and proposes a global
   `beads-live-mode` minor mode instead of per-view attach/detach. **Correction:**
   streams are keyed by canonical store root (one `bd events` stream per store,
   shared by every beads view); `gc events` and `bd events` are different
   journals and never conflated; a store with the journal off starts no stream
   at all (§4.1, §8).

Everything else in the seed — reuse of `beads-event-record`, debounced
invalidation, `◈` changed-row marks, rewind, city timeline, opt-in
notifications — is kept and sharpened below.

### 1.3 Reuse decision: the wire model already exists

`beads-types.el` already defines `beads-event-record` with exactly the slots
`bd events tail` documents (`seq ts op issue-id actor issue dep comment`), plus
`beads-events-records-from-json-lines` (the JSONL parser) and
`beads-event-record-from-json`. The `op` constants
(`beads-event-created`, `-updated`, `-closed`, `-dependency-added`, …) already
exist. **None of this is reimplemented.** `beads-event.el` (new) adds only
*presentation over* that record: signal level, glyph, subject, field diff, and
churn folding.

## 2. Audit of the current surfaces (the "before" state)

Every surface below was read in the worktree at `origin/main` (`41bb542`,
`bd 1.3.1`). The design extends each; none is duplicated.

### 2.1 Dashboard (`beads-dashboard.el`, `beads-dashboard-sections.el`)

- `beads-dashboard` mounts a `vui` root (`beads-dashboard--root`) with a
  **generation counter** (`:generation`, bumped by `beads-dashboard-refresh`).
  Each section is a `beads-dashboard--section` vnode whose `:async-key` is
  `(key generation db-path extra-rows)`; changing the key re-runs the loader and
  re-renders. This is the natural live target: a live batch bumps nothing, it
  `vui-set-state`s the model-backed row data and re-renders in place.
- Loaders run `bd` through `beads-command-execute-async` and are cached
  per `db-path`. `beads-dashboard--idle-refresh` fires on a 30s idle timer
  (`beads-dashboard-auto-refresh-interval`) and simply bumps generation.
- A provider registry already exists: `beads-dashboard-section-providers`
  (populated from `beads-section-register` specs via
  `beads-dashboard--provider-specs`), rendered by
  `beads-dashboard--provider-vnodes`. The empty registry is the standalone
  no-op. **This is where a `recent-changes` section plugs in with no dashboard
  edit.**
- The dashboard header is a vnode (`beads-dashboard--header-vnode`), not a
  `header-line`; freshness today is `mode-line-misc-info`
  (`beads-dashboard--update-mode-line`: `refreshed 28s ago · dolt:… · cap:…`).

### 2.2 List (`beads-command-list.el`)

- `tabulated-list-mode`; `beads-list-refresh` → `beads-list--fetch` →
  `beads-list--populate-buffer`; `beads-list-refresh-all` refreshes every list
  buffer (used by `beads-list--on-agent-state-change`).
- `beads-list--header-line` shows `Beads list — NAME [N issues · counts ·
  filter] (g refresh)`; `beads-list--mode-line` is separate.
- **Naming hazard:** `beads-list-follow-mode` is a *list → show* follow (updates
  an adjacent show buffer on navigation via `beads-show-update-buffer` and a
  coalescing timer). It is unrelated to journal `--follow`. The design keeps it
  untouched and calls the new behaviour "live refresh" to avoid collision.

### 2.3 Show (`beads-command-show.el`)

- Sectioned `special-mode` buffer. `beads-show--load` fetches via
  `beads-command-execute-async` (or sync) and calls `beads-show--render-issue`,
  which inserts sections with `beads-show--insert-section-with`; empty sections
  are skipped. `beads-refresh-show` re-fetches. Buffers register with a worktree
  session (`beads-show--register-with-session`).
- There is no per-issue activity/history section today. `bd history` exists as a
  separate command (`beads-command-history.el`) but is not embedded in show.

### 2.4 Command layer (`beads-command.el`)

- `beads-command-execute` (sync), `beads-command-execute-interactive`
  (transient suffix), `beads-command-execute-async` (`:queue`, `:cache-key`
  single-flight, `:timeout`).
- `beads-command--single-flight` is a hash of in-flight `cache-key`s;
  `beads-command--policy-probe` fills `beads-command--policy`. Completed
  foreground writes do **not** invalidate any cache; the dashboard relies on its
  timer. **This is the seam a live invalidation belongs at** (AC §2.4): after a
  live batch, clear the single-flight cache and the dashboard's
  `last-good-data` so the next read is fresh.

### 2.5 Section registry (`beads-section.el`)

- `beads-section-register KEY TITLE LOADER RENDERER &optional KEYS` stores a
  `beads-section-spec`; `beads-section-registered-vnodes` runs loaders and
  renderers. `beads-status-sections-hook` lists the status-buffer sections.
  Reused as-is: the activity/history section registers here; no edit to this
  file.

### 2.6 Menus (`beads-menu.el`, `beads-command-events.el`)

- `beads-dispatch` (Views group has Dashboard/History/…) and `beads-maintenance`
  (Issue Details group already has `e` → `beads-events`). A provider seam
  (`beads-menu-provider-groups`) lets downstream packages add groups.
- `beads-command-events.el` already defines `beads-events-tail`,
  `beads-events-export`, `beads-events-prune`, and the `beads-events` parent
  transient (Journal group). **The parent transient is extended in place** with
  Live and Time-travel groups; no new command class.

### 2.7 Events / record types (`beads-command-events.el`, `beads-types.el`)

Already audited in §1.3: command classes, JSONL parser, and `beads-event-record`
exist and are reused. `bd events --help` confirms the record contract and the
lifecycle caveats that drive §4: per-branch, per-replica, retention floors,
`--since` below the oldest retained record **fails** (it does not silently skip),
and `bd dolt pull` / `bd sql` writes are **not** journaled.

### 2.8 What does *not* exist in beads.el (do not pretend it does)

- No `beads-store.el` (the gascity data/cache layer) and no `beads-timer.el`.
  `beads-live.el` keeps its own in-memory model and uses ordinary timers plus
  `beads-command--single-flight`. A remote stream is a **local** `ssh` pipe
  process, so no TRAMP I/O ever happens in a timer — the gascity timer-suspension
  guard is unnecessary here, and this is called out as a deliberate difference.

## 3. Architecture: module plan, seams, and the gascity mirror

Four net-new modules, one concern each, plus small integration edits.

```
                         ┌──────────────────────────────────────────────┐
  bd events tail --follow│ beads-live.el   (stream supervisor, 1/store) │
        ─────────────────┤  spawn · parse · debounce · route           │
                         │  checkpoint · backoff · poll · re-baseline   │
                         └───────┬───────────────────────────┬──────────┘
                                 │ beads-event-record         │ invalidate hook
                                 ▼                            ▼
             ┌────────────────────────────┐   ┌───────────────────────────────┐
             │ beads-event.el (pure model)│   │ beads-command.el (seam)        │
             │  level · glyph · subject   │   │  clear single-flight +         │
             │  field diff · churn fold   │   │  dashboard last-good on batch  │
             └───────┬────────────────────┘   └───────────────────────────────┘
                     │ rendered by
                     ▼
   ┌───────────────────────────┬───────────────────────┬──────────────────────┐
   │ beads-dashboard(.el/sect) │ beads-command-list.el │ beads-command-show.el│
   │  live sections, ◈ rows,   │  live rows, ◈, chip   │  live re-render +    │
   │  status chip              │                       │  ACTIVITY section    │
   └───────────────────────────┴───────────────────────┴──────────────────────┘
                     │                                       ▲
                     ▼                                       │ beads-section-register
             ┌────────────────────┐                  ┌───────┴────────┐
             │ beads-events.el    │── timeline, history, rewind ───────┘
             │  (view module)     │
             └─────────┬──────────┘
                       ▼
               ┌───────────────┐
               │ beads-pulse.el│  mode-line lighter
               └───────────────┘
```

### 3.1 `lisp/beads-live.el` — the stream supervisor ← `gascity-live.el`

One `beads-live--stream` struct per canonical store root:

```elisp
(cl-defstruct (beads-live--stream (:constructor beads-live--stream-create)
                                  (:copier nil))
  root host name
  process (partial "") seq
  (state 'off) reason
  (attempt 0) retry-timer retry-at
  (enabled t) views subscribers
  debounce-timer pending
  stderr-buffer
  (mode 'stream) poll-timer poll-busy
  started stopping resume confirm-timer
  ;; beads-specific:
  branch replica checkpoint baseline)
```

Public surface (mirrors gascity names so the pattern is legible):

| Function | Role |
|---|---|
| `beads-live-attach` / `beads-live-detach` | a view joins/leaves its store's stream; last out stops it; `kill-buffer` detaches |
| `beads-live-subscribe` / `beads-live-unsubscribe` | raw undebounced records to the Events view |
| `beads-live-status` / `beads-live-header-string` | pure plist / header fragment from buffer state (safe at redisplay) |
| `beads-live-invalidate-functions` | abnormal hook `(ROOT KINDS OPS)` run per debounced batch |
| `beads-live-active-p` | is a `live`/`poll` stream covering DIR? (the command-layer seam) |
| `beads-live-toggle` (`W`), `beads-live-reconnect` (`g`), `beads-live-stop-all` | controls |

Internals mirroring gascity: `beads-live-parse-chunk` (JSONL split; lines that
are not JSON objects are skipped), `beads-live-route` (op → invalidation kinds),
`beads-live--queue`/`--flush` (debounce via `beads-live-debounce`),
`beads-live--spawn`, `beads-live--exited`/`--classify`, `beads-live--schedule-retry`,
`beads-live--start-poll`/`--poll`, `beads-live--start`, `--stop`.

**argv.** `beads-live--args` builds
`("events" "tail" "--follow" "--json" [ "--since" SEQ ] [global options])`.
Global options come from the store: `--directory` (the resolved beads dir) —
reuse the same scoping the views already use via `beads-store-directory`. The
program is resolved by `beads-remote-find-executable` for the store's host.

**Transport.** Local: `make-process :connection-type 'pipe`.
Remote single-hop ssh: a **local** `ssh -T` pipe built by
`beads-remote-ssh-pipe-argv` (already in `beads-remote.el`), `cd` to the store,
pure PATH fragment, no TRAMP round trip — never `tramp-sh` `make-process`, for
the same deadlock reason documented for `beads-remote` and `gascity-live`.
Other remote methods: poll only (§4.4). stderr goes to a dedicated
`*beads-live: STORE*` buffer whose last line is the header `reason`
(`beads-live--classify`).

**Test seam.** All process creation goes through `beads-live--spawn`; unit tests
substitute a scripted emitter (no subprocess), exactly as the seed proposed and
`gascity-live` enables.

### 3.2 `lisp/beads-event.el` — the pure model ← `gascity-event.el`

Pure functions over `beads-event-record` and `beads-issue`; no buffers, no
processes, safe to require from the views and the pulse without the stream.

- `beads-event-level` / `beads-event-glyph`: map `op` to a signal level. Default:
  `close`/`delete`/`dep_add`(unblocking) → attention, `update`/`status_changed`
  → watch, `comment`/`create` → none. Memoized like `gascity-event-level` and
  user-extensible via `beads-event-levels`.
- `beads-event-subject`: `"issue-id  title — op"` text for a row.
- `beads-event-diff`: successive `beads-issue` snapshots → field-level diff
  (`status open → in_progress`, `assignee — → alice`,
  `dependency_added issue-2`). Powers the history and the show section.
- `beads-event-fold`: coalesce same-op bursts per issue per time bucket into a
  `×N` row (the gascity churn convention), with signal ops never folded.
- `beads-event-time` / `beads-event-since-arg`.

### 3.3 `lisp/beads-events.el` — the views ← `gascity-events.el`

One view module, `tabulated-list-mode` over `beads-pager` like
`gascity-events.el`, newest-first, live append, churn folding, op/actor/issue
filters, org export. Header line mirrors `gascity-events--header-line`:

```
 <project> events  last <window> · <shown> · signal ≥ <level>  <filters>   <live chip>
```

It defines the timeline, the **merged city timeline**, per-issue **history**, and
**rewind** (the latter two are beads-specific extensions gascity lacks; if the
file grows past ~700 lines, history/rewind split into
`beads-events-history.el` / `beads-events-rewind.el` — a size decision, not an
architectural one). It also registers the dashboard's `recent-changes` section
through `beads-section-register` and defines `beads-events-notify-mode`.

### 3.4 `lisp/beads-pulse.el` — the mode-line lighter ← `gascity-pulse.el`

`beads-pulse-publish` / `beads-pulse-cities` / `beads-pulse-record` +
`beads-pulse-sparkline` + `beads-pulse-mode-line-string` +
`beads-pulse-mode` (global minor mode). Fed from in-memory model state only
(`beads-live-cities`), never runs `bd` at redisplay. Coexists with
`beads-dashboard--update-mode-line` (which owns `mode-line-misc-info` for a
dashboard buffer); the pulse is the global cross-store lighter.

### 3.5 gascity mirror map (decision table)

| gascity.el | beads.el | Decision |
|---|---|---|
| `gascity-live.el` | `beads-live.el` | **NEW.** Same supervisor design; beads-specific argv (`bd events tail --follow`), checkpoint identity, re-baseline. Shares `beads-remote` ssh-pipe builders (already in beads). |
| `gascity-events.el` | `beads-events.el` | **NEW.** Same tabulated/pager view conventions; rows over `beads-event-record`. |
| `gascity-event.el` | `beads-event.el` | **NEW, pure.** No code shared: gc events are alists, bd events are `beads-event-record` EIEIO; only the *role* is shared. |
| `gascity-pulse.el` | `beads-pulse.el` | **NEW.** Same lighter pattern, fed by the model. |
| `gascity-store.el` | — | **None.** beads has no store layer; the model is a per-store hash in `beads-live`. |
| `gascity-timer.el` | — | **None.** beads uses ordinary timers; remote streams never do TRAMP I/O in a timer. |
| `beads-section-register` | (reused) | The activity/history section registers through beads' own registry; gascity may consume it. |

**Shared vs beads-specific, explicitly.** *Shared concept (re-implemented, not
imported, because gascity is not a dependency):* one stream per store, debounce,
invalidation hook, attach/detach, status/header, backoff, poll fallback,
subscribe, lighter. *Beads-specific:* `bd events` argv; per-branch/per-replica
checkpoint keys; prune/sync re-baseline; rewind; field diffs; `beads-issue`
snapshots in `record.issue`; the `beads-command` cache-invalidation seam; the
`beads-section-register` activity section. beads.el **must not** require
gascity (standalone-first, AC-8); gascity already depends on beads, so it could
later adopt `beads-live` — a follow-up, not this plan.

## 4. Event lifecycle

### 4.1 Capability detection and stream key

At first attach for a store, read the journal capability once
(`bd config get events-journal`, cached per store/model). The stream key is the
**canonical store root**, plus the journal kind `bd-events` (so a gascity
`gc events` stream for the same directory is a different key and neither
duplicates the other). Checksum/remote canonicalization follows
`beads-remote-prefix` / `beads-buffer` keying so `/ssh:host:/p` and `/p` cannot
produce two streams.

- journal on → `mode 'stream`, state `connecting` → `live`.
- journal off, or `bd` too old to know `events` → `mode 'poll`, state `poll`;
  **no stream process is started**, and the existing dashboard timer / list
  refresh behave exactly as today (AC-2).

### 4.2 Start / resume

1. Load the per-store checkpoint: `(root, branch, replica)`. Checkpoints live
   under a new `beads-live-state-directory` (default
   `(locate-user-emacs-file "beads-live/")`), file
   `beads-live-<hash(root,branch,replica)>.eld`. The branch and replica are read
   from the bd store (branch from `beads-git-get-branch`/`bd config`); a
   checkpoint whose branch or replica does not match the store is **discarded**,
   never carried across (the help's hard rule).
2. If a valid checkpoint exists, spawn `tail --follow --since <seq>`
   (gap-free; `--since` is "greater than", so the last applied seq is the
   argument).
3. Otherwise bootstrap: run one full baseline read with the **existing**
   loaders (`beads-command-list`/`-ready`/`-blocked`/`-stats`, or `bd list`
   through `beads-list-refresh`) to seed the model's `issues` hash, then start
   at the current head seq (`tail --since <head>`), or from 0 for a small
   journal.

### 4.3 Tail, parse, apply, checkpoint

`make-process` → process filter `beads-live--filter` → `beads-live-parse-chunk`
(JSONL) → fold each line into `beads-event-record` via
`beads-event-record-from-json` → `beads-live--deliver`:

1. advance `seq` to the max record seq;
2. apply `record.issue` (the full post-mutation snapshot) to the model's
   per-issue `issues` hash (nil snapshot = delete ⇒ tombstone stub);
3. append to the bounded ring buffer and the per-issue log;
4. run subscribers with the raw record;
5. queue the `op` for debounced invalidation.

After each debounced **batch** (not each line) the checkpoint seq is written
(coalesced, e.g. 1s), so a crash replays at most one batch. A burst of factory
events coalesces into one redisplay (the seed's 150ms idea generalizes to the
batch window).

### 4.4 Poll fallback

When `mode` is `poll` (journal off, non-ssh remote method, or `bd` too old), run
`beads-command-events-tail` **without** `--follow` (`:since <seq>`, small
`:limit`) through `beads-command-execute-async` on a
`beads-live-poll-interval` (30s) timer, with a `:cache-key` so polls never
overlap. Deliver only records with `seq > last`. This is the one place the
command class is reused for the live feature, and it keeps the single-flight and
concurrency caps.

### 4.5 Reconnect / backoff

On process exit, `beads-live--classify` from the stderr tail:

- journal-disabled/unknown-subcommand → `poll` (stop retrying the stream);
- connection/host failure → `offline`;
- anything else → `reconnecting`.

Backoff follows `beads-live-backoff` `(2 5 15 60)` repeating, reset after
`beads-live-stable-after` (15s) up. `beads-live-reconnect` (`g`) retries at
once; a resume queues one full refresh (`:all`) so anything that went stale
during the gap is re-read.

### 4.6 Re-baseline (prune, sync, replica change)

The journal is per-branch, per-replica, and bounded. Re-baseline (full re-read +
reset checkpoint to current head + brief `partial` state, with the reason in the
header tooltip) happens when:

- `--since <seq>` **fails** because the prefix was pruned (the documented
  failure, AC-3);
- the branch changed (checkout) or the replica identity changed;
- a periodic reconcile elapses (`beads-live-reconcile`, 600s, relaxed from the
  30s timer) — the safety net for `bd dolt pull` / `bd sql` writes the journal
  never saw (AC non-coverage).

### 4.7 Own-write interaction and the command-layer seam

When the user acts in Emacs, the write goes through the existing
`beads-command-*` classes. On success:

- clear `beads-command--single-flight` (and the dashboard's `last-good-data`) so
  the next read is fresh, via a new public `beads-command-invalidate-cache`;
- optionally apply the returned issue snapshot to the model optimistically;
- the stream's echo of the same mutation is deduped by seq (records at/below the
  last applied seq are ignored).

When a live stream is active, `beads-live-active-p` tells the command layer that
invalidation is the stream's job, so a completed foreground action does not
double-refresh.

## 5. Data flow

```
 bd events tail --follow --json  ──► parse (beads-live-parse-chunk)
        ▲                                    │  beads-event-record
        │                                    ▼
        │                         beads-live--deliver ──► model (issues/log/ring)
        │                                    │                    │
        └── checkpoint seq ◄── debounced ◄───┤                    ├─► dashboard sections (in place, ◈)
                                             │                    ├─► list rows (in place, ◈)
                                             ├──► subscribers ────►├─► show re-render + ACTIVITY
                                             │   (Events view)     └─► city timeline / pulse
                                             └──► invalidate hook ──► beads-command cache clear
 baseline: existing loaders (beads-command-list/-ready/-blocked/-stats)
           ─────────────────────────────────► model (seed + reconcile + rewind base)
 rewind:   baseline@K + replay ring K..N ────► rendered read-only
```

## 6. Reuse-vs-new matrix

| File | Status | What changes |
|---|---|---|
| `lisp/beads-live.el` | **NEW** | stream supervisor (spawn/parse/debounce/route/checkpoint/backoff/poll/re-baseline) |
| `lisp/beads-event.el` | **NEW** | pure model: level/glyph/subject/diff/fold over `beads-event-record` |
| `lisp/beads-events.el` | **NEW** | timeline + city timeline + history + rewind views; `beads-section-register` of `recent-changes`; notify mode |
| `lisp/beads-pulse.el` | **NEW** | mode-line lighter fed from the model |
| `lisp/beads-dashboard.el` | EDIT | attach live; model-backed section data + in-place re-render; ◈ changed rows; `live/poll/partial` chip in header vnode and mode-line; keep the 30s timer only as the poll/reconcile fallback |
| `lisp/beads-dashboard-sections.el` | EDIT | a `recent-changes` renderer + reuse of existing loaders as the baseline; no new dashboard |
| `lisp/beads-command-list.el` | EDIT | subscribe live; debounced `beads-list-refresh` on invalidation; ◈ changed rows; chip in `beads-list--header-line`; `beads-list-follow-mode` untouched/disambiguated |
| `lisp/beads-command-show.el` | EDIT | subscribe live; re-render when the shown issue changed; new `ACTIVITY` section via `beads-show--insert-section-with` |
| `lisp/beads-command.el` | EDIT | `beads-command-invalidate-cache`; `beads-command-live-p-function` seam; clear single-flight + dashboard cache on a live batch |
| `lisp/beads-command-events.el` | EDIT | extend the `beads-events` transient with Live + Time-travel groups; no new classes |
| `lisp/beads-menu.el` | EDIT | add "Recent changes" to `beads-dispatch` Views; add Live/Time-travel to `beads-maintenance` |
| `lisp/beads-types.el` | **REUSE** | `beads-event-record`, `beads-events-records-from-json-lines`, op constants — unchanged |
| `lisp/beads-section.el` | **REUSE** | `beads-section-register` — unchanged |
| `lisp/beads-remote.el` | **REUSE** | `beads-remote-ssh-pipe-argv`, `-find-executable`, `-prefix`, `-pure-path-assignment` — unchanged |
| `lisp/beads-pager.el` | **REUSE** | window page size for the Events view — unchanged |
| `lisp/beads-buffer.el` | **REUSE** | `beads-mode--install-navigation-keys` — unchanged |
| `lisp/beads-custom.el` | EDIT | `defcustom` group `beads-live` for the new options/faces |
| `lisp/beads-faces.el` | EDIT | `beads-event-changed`, `beads-events-status`, `beads-events-rewind` (all `:inherit`) |
| `lisp/beads.el` | EDIT | `;;;###autoload` on the new commands only; no dependency change |
| `lisp/test/beads-live-test.el` | **NEW** | stream/scripted-emitter, checkpoint, backoff, re-baseline, poll |
| `lisp/test/beads-event-test.el` | **NEW** | pure diff/fold/level |
| `lisp/test/beads-events-test.el` | **NEW** | view rendering, filters, rewind, org export |
| `lisp/test/beads-dashboard-test.el` | EDIT | live section + changed-row assertions |
| `NEWS.md` | EDIT | Unreleased entry |

## 7. UI mockups (current surfaces; before → after)

`◈` = `beads-event-changed` face (changed within `beads-live-change-window`,
default 30s). Faces always `:inherit`.

### 7.1 Dashboard — before (today)

```
Beads-Dashboard — beads.el (/home/roman/workspace/beads.el/.beads/beads.db)
▾ 📊 Stats                open 24 · in flight 3 · blocked 5 · ready 12
▾ 📦 Recently Closed (5)
  be-ndi6  ○ github-pr-review: PR #68 review            closed 12m ago
  be-ijfx  ○ WI-SF-02 molecule actions + work loop      closed 18m ago
▾ 🚧 In progress (3)
  be-oyfn  ◐ standalone-formulas build        @beads/worker      22m
  be-w5b4  ◐ WI-SF-17 swarm validate / waves  @beads/worker       8m
▾ ✅ Ready (12)
  be-m3vd  ○ WI-SF-18 swarm actions + nav     P2 · L
  be-ijfx  ○ WI-SF-02 molecule actions        P2 · M
▾ 🔒 Blocked (5)
  be-ozmm  ⊘ waiting on be-25en               P2 · 2 blockers
▾ 🎯 Epic Progress (1)
  bde      ██████░░░░ 3/5
KEYS: TAB/S-TAB next/prev · SPC fold · N/P section · M-0..M-4 depth · +/-/* rows · g refresh · c claim · b blocker · a agent · RET visit · q quit
mode-line:  · refreshed 28s ago · dolt:embedded · cap:4 · auto:on
```

### 7.2 Dashboard — after (live)

```
Beads-Dashboard — beads.el (...)                                   ● live ∿3/s · seq 1047
▾ 📊 Stats                open 24 · in flight 3 · blocked 5 · ready 12 · ◈ 4
▾ 📦 Recently Closed (5)
  ◈ be-ndi6  ○ github-pr-review: PR #68 review          closed  2s ago
    be-ijfx  ○ WI-SF-02 molecule actions + work loop    closed 18m ago
▾ 🚧 In progress (3)
  ◈ be-ijfx  ◐ WI-SF-02 molecule actions + work loop   @mayor       0s
    be-oyfn  ◐ standalone-formulas build               @beads/worker 22m
    be-w5b4  ◐ WI-SF-17 swarm validate / waves         @beads/worker  8m
▾ ✅ Ready (12)
    be-m3vd  ○ WI-SF-18 swarm actions + nav            P2 · L
  ◈ be-yzbs  ○ WI-SF-20 dedup dependency graph         P2 · M   ← ready now
▾ 🔒 Blocked (5)
    be-ozmm  ⊘ waiting on be-25en                      P2 · 2 blockers
▾ 🕑 Recent changes (live)                              [open timeline… (t)]
    #1047 12:31:07  close   be-ndi6  github-pr-review      @beads/reviewer
    #1046 12:30:02  close   be-ijfx  WI-SF-02 (superseded) @mayor
    #1045 12:29:48  update  ga-gi00b implement → shipped   @gascity/worker
KEYS: TAB/S-TAB next/prev · SPC fold · N/P section · [+/-/*] rows · g refresh · t timeline · R rewind · W live · q quit
mode-line:  · live ∿3/s · seq 1047 · refreshed 1s ago · dolt:embedded · cap:4
```

Notes: sections update **in place** (vui `vui-set-state`, not a generation bump);
a burst coalesces into one redisplay; `◈` fades after the window; the
`live ∿3/s · seq N` chip is `beads-event-status` and becomes `poll`/`partial` as
in §7.6.

### 7.3 List — before (today)

```
Beads list — beads  [7 issues · Open 3 · In progress 2 · Blocked 2 · filter: none]  (g refresh)
  ID       P  STATUS       TITLE                                   ASSIGNEE
  ──────────────────────────────────────────────────────────────────────────────
▾ Open
  be-m3vd  2  ○ Open       WI-SF-18 swarm actions + navigation     -
  be-yzbs  2  ○ Open       WI-SF-20 dedup dependency graph         -
▾ In progress
  be-oyfn  2  ◐ In progress standalone-formulas build             beads/worker
```

### 7.4 List — after (live)

```
Beads list — beads  [7 issues · Open 3 · In progress 2 · Blocked 2 · filter: none · ● live ∿3/s]  (g refresh · W live · t timeline)
  ID       P  STATUS       TITLE                                   ASSIGNEE
  ──────────────────────────────────────────────────────────────────────────────
▾ Open
  be-m3vd  2  ○ Open       WI-SF-18 swarm actions + navigation     -
◈ be-yzbs  2  ○ Open       WI-SF-20 dedup dependency graph         -            (ready now)
▾ In progress
◈ be-ijfx  2  ◐ In progress WI-SF-02 molecule actions + work loop mayor         (just claimed)
  be-oyfn  2  ◐ In progress standalone-formulas build             beads/worker
```

The changed rows are reverted in `beads-list--populate-buffer` from the model;
point and marks are preserved (reuse the existing point-restore path in
`beads-list-refresh`). `beads-list-follow-mode` is unchanged.

### 7.5 Show — before (today, abridged)

```
bde-go3g: beads.el: Magit-like Emacs interface for Beads
○ Open  P1  Epic  Roman Scherer
──────────────────────────────────────────────────────────────────────────────
  q bury · g refresh · ? dispatch
  store: /home/roman/workspace/beads.el/.beads
Created: 2026-08-01   Updated: 12m ago   Notes: 4

DESCRIPTION
  A Magit-like transient UI for beads …

LABELS
  beads  ui  dashboard
...
```

### 7.6 Show — after (live + ACTIVITY section)

```
bde-go3g: beads.el: Magit-like Emacs interface for Beads            ● live · seq 1047
○ Open  P1  Epic  Roman Scherer
──────────────────────────────────────────────────────────────────────────────
  q bury · g refresh · ? dispatch
  store: /home/roman/workspace/beads.el/.beads
Created: 2026-08-01   Updated: 2s ago   Notes: 4

DESCRIPTION
  A Magit-like transient UI for beads …

ACTIVITY (live, newest first)                                   [open history… (H)]
  ◈ #1046  12:30:02  @mayor
      status        open        → closed   (reopened)
      assignee      —           → alice
    #1041  12:20:11  @beads/worker
      status        open        → in_progress
      priority      P2          → P1
    #1030  12:18:09  @mayor
      create        "bde-go3g — beads.el: Magit-like Emacs interface"

LABELS
  beads  ui  dashboard
```

Sections are inserted with the existing `beads-show--insert-section-with`; empty
sections are still skipped. Only the ACTIVITY section (and the `Updated:` /
live marker) changes on a live batch; point is preserved.

### 7.7 Recent changes / Events view — new (gascity-events conventions)

```
beads events  last 1h · 47 · signal ≥ watch   op=close actor=@beads/*      ● live ∿3/s
  seq     time      lvl op                issue                    actor
  ─────────────────────────────────────────────────────────────────────────────────
  1047    12:31:07  ■  close             be-ndi6 github-pr-review @beads/reviewer
  1046    12:30:02  ■  close             be-ijfx WI-SF-02         @mayor
  1045    12:29:48  ▲  update            ga-gi00b implement→shipped @gascity/worker
  1044    12:29:31     comment           be-ndi6 "review passed"  @beads/reviewer
  1043    12:28:02  ■  dep_add           be-ijfx ← be-yzbs        @mayor        (unblocked)
  1042    12:27:44     create            be-ijfx WI-SF-02 …       @mayor
  ─── rewind cursor ▲ (r to rewind here) ────────────────────────────────────────────
  1041    12:20:11  ▲  status_changed    be-w5b4 open→in_progress @beads/worker
  j/k move · g follow · f filter(op/actor/issue/since) · C-c C-g group · RET visit · r rewind · e org · H issue history
```

Grouped by issue (`f group:issue`), same data, churn folded:

```
beads events  grouped by issue · 6 issues · 12 churn folded                       ● live
▾ be-ndi6  github-pr-review: PR #68 review        closed · @beads/reviewer
    #1047  12:31:07  close        → closed
    #1044  12:29:31  comment      "review passed"
    #1031  12:18:40  status_changed open → in_progress
    #1030  12:18:09  create       github-pr-review
▾ be-ijfx  WI-SF-02 molecule actions + work loop  closed · @mayor
    #1046  12:30:02  close        → closed
    #1043  12:28:02  dep_add      ← be-yzbs  (was blocked)
    #1042  12:27:44  create       WI-SF-02 molecule actions
```

### 7.8 Rewind — new (read-only time travel)

```
Rewind to seq (or -N / +N, blank = live): 1040 RET
```

```
Beads-Dashboard — beads.el  ⏪ REWIND @1040 · +7 events · read-only
▾ 📊 Stats                open 22 · in flight 2 · blocked 4 · ready 11
▾ 🚧 In progress (2)
    be-oyfn  ◐ standalone-formulas build        @beads/worker  22m
    be-w5b4  ◐ WI-SF-17 swarm validate / waves  @beads/worker   8m
▾ 🕑 Recent changes (up to #1040)
    #1040 12:26:58  create  be-m3vd  WI-SF-18 swarm actions  @mayor
g forward event · G back event · l/R resume live · R jump · q close
```

Rewind never writes; a distinct face/modeline marks the non-live state; `g`/`G`
step event-by-event so the user can watch the replay.

### 7.9 Journal off / onboarding — the honest fallback (AC-2)

```
Beads-Dashboard — beads.el (.../beads.el/.beads)                                 poll 30s
  ⓘ events journal is not enabled in this store — refreshing every 30s.
    Enable live updates:  bd config set events-journal true   [copy (w)]
    (this edits .beads/config.yaml; every bd write then records an event)
▾ 📊 Stats                open 24 · in flight 3 · blocked 5 · ready 12
▾ 📦 Recently Closed (5)
  be-ndi6  ○ github-pr-review: PR #68 review            closed 12m ago
...
KEYS: … · g refresh · t timeline (disabled) · W live · q quit
mode-line:  · poll 30s · refreshed 28s ago · dolt:embedded · cap:4 · auto:on
```

No error is raised; the timeline/rewind entries are disabled with a
`user-error` pointing at `bd config`. If `bd events` is unknown (older `bd`),
the chip reads `poll` with an incompatible-version tooltip.

### 7.10 Modeline / status strip (any frame)

```
 *beads-dashboard*  Beads-dash  ◐ 3·5·12   ∿3/s  seq 1047   [live]
 *beads-events*     Beads-EV    47 rows    ⏪ @1040 +7        [rewind]
 *beads-dashboard*  Beads-dash  ◐ 3·5·12   ↻ 30s             [poll]
```

`beads-pulse-mode-line-string` is public so users can compose it;
`beads-pulse-mode` is opt-in.

### 7.11 Notification (opt-in, `beads-events-notify-mode`)

```
   ┌─────────────────────────────────────────────────────────────┐
   │ beads-live · beads.el                                        │
   │ be-yzbs ready — unblocked by be-ijfx     [open] [sling] [mute]│
   └─────────────────────────────────────────────────────────────┘
```

Uses `notifications-notify` when available, falls back to `message`; `[sling]`
reuses the existing beads sling seam; rate-limited to one per issue per minute
by default.

### 7.12 Gas City city cockpit showing beads-store activity (target)

The cockpit header already renders `gascity-live-header-string`; a beads-backed
rig adds a `Beads` section fed by `beads-live`'s model. This mockup is the
**integration target**; the wiring is a gascity-side follow-up (§8), not part of
this delivery.

```
gc city  bright-lights                                              ● live ∿5/s
  AGENTS  12 running · 2 idle · 1 failed
  WORK    orders 4 firing · convoys 2
▾ BEADS (rig: beads.el)                                                          ● live
    ◈ be-ijfx  close   WI-SF-02 molecule actions         @mayor          2s
      be-ndi6  close   github-pr-review: PR #68 review  @beads/reviewer 40s
    #1047 12:31:07  close  be-ndi6   @beads/reviewer
    #1046 12:30:02  close  be-ijfx   @mayor
? help  j jump  g refresh
```

## 8. gascity.el integration (no duplicate streams)

- **Two journals, never conflated.** `gc events` (gascity-live) and `bd events`
  (beads-live) describe different stores' mutations. Stream keys include the
  journal kind, so the same directory never spawns two `bd events` streams and
  never confuses a `gc` stream with a `bd` one.
- **Exactly one `bd events` stream per beads store**, shared by dashboard,
  list, show, timeline, history, and rewind (attach/detach refcounts views; the
  last one out stops it). A store with the journal off starts none.
- **Optional cross-invalidation, no gascity change required.** `beads-live`
  exposes the public `beads-live-invalidate` and the
  `beads-live-invalidate-functions` hook. gascity's `bead.*` routing
  (`gascity-live--bead-routes`) can, in a follow-up, call
  `(when (fboundp 'beads-live-invalidate) …)` so a beads store backing a rig
  invalidates beads views too. This plan ships the seam; it does not edit
  gascity (requirements Out Of Scope: gascity code changes only as optional
  attribution at an existing seam).
- **Optional attribution generic.** `beads-event-actor-description` maps
  `record.actor` (or a session id) to a friendly name; gascity (or a user) can
  override it. The default path references **no gascity symbol** (AC-8). The
  merged city timeline (`beads-events-timeline-city`) is beads-only: it merges
  beads store models, never mixes `gc` and `bd` seq spaces.

## 9. Verification strategy

- **Unit (ERT, `:unit`, no `bd`):** `beads-event` level/glyph/subject/diff/fold;
  `beads-live` JSONL chunking (partial lines, non-JSON lines), debounce/flush,
  routing table, checkpoint read/write and branch/replica invalidation,
  backoff schedule, re-baseline trigger on a pruned-`--since` error,
  poll-vs-stream classification, capability detection.
- **Injection:** a scripted emitter substituted for `beads-live--spawn` drives
  deterministic sequences (create → dep_add → status_changed → comment → close →
  delete) with no subprocess; assert dashboard/list/show/events buffer contents
  through `vui` render helpers, in the style of `beads-dashboard-test.el`.
- **Integration (`:integration`, tagged, skips without `bd`):** enable the
  journal in a temp repo (`beads-test-with-temp-repo-and-issues`), run real
  `bd create`/`dep add`/`close`, tail, and assert the model and buffers.
- **Live acceptance (operator constraint):** `emacs -nw -Q` in tmux against a
  real store — verify live update, rewind, and the journal-off fallback; record
  buffer state as bead evidence.
- **Guards:** standalone-first (no gascity symbol on any default path);
  byte-compile `--warnings-as-errors`; `eldev lint`; CI test/lint/coverage;
  remote-store render guard (`lisp/test/beads-render-guard-test.el`) still green
  — opening/rendering/folding a view must do no file I/O on a remote store.

## 10. Risks and mitigations

| Risk | Mitigation |
|---|---|
| Journal absent / `bd < 1.3` | Capability probe once; `poll` mode; no stream spawned; no error (AC-2) |
| Journal is per-branch / per-replica | Checkpoint keyed by (root, branch, replica); discard on mismatch; never carry across (AC-3, help contract) |
| Checkpoint seq pruned | `--since` failure detected → documented re-baseline (full read + reset + reason), not a silent stall (AC-3) |
| `bd dolt pull` / `bd sql` writes unjournaled | Periodic relaxed reconcile baseline (600s) + stream resume full refresh; UI never claims `live` while reconciling |
| Event burst | Debounced batch (one redisplay per burst) + bounded ring/log |
| Duplicate streams | One stream per (canonical root, journal kind); attach/detach refcount; journal-off starts none; never mix `gc`/`bd` |
| Remote deadlock | Local `ssh -T` pipe via `beads-remote-ssh-pipe-argv`; never `tramp-sh` `make-process`; no TRAMP I/O in timers/redisplay |
| Naming collision `beads-list-follow-mode` | Live feature called "live refresh"; journal follow stays internal to `beads-live` |
| Emacs 29.4 EIEIO cross-file dispatch | New code is plain functions/cl-defstruct; avoid new cross-file `cl-defmethod` dispatch to stay CI-green on 29.4 |
| Model/baseline divergence | Stream deltas are authoritative only between reconciles; baseline loaders remain the source of the seed and the rewind base |

## 11. Non-goals

- No new `bd` subcommands or flags; the documented `bd events` surface only.
- No daemon, watcher service, or persistent database; the model is in-memory,
  rebuilt from a baseline read + journal replay.
- No direct Dolt table access and no second journal implementation.
- No graphical/canvas renderer; text faces/overlays only.
- **No gascity code changes** (attribution generic + documented seam only).
- No parallel dashboard or view: every surface is an extension of the existing
  one.
- Not a replacement for `bd history`; the ACTIVITY section and history view read
  the journal, not the Dolt history table.
- No `.el` change in this planning deliverable.

## 12. Requirements coverage

| Req | Covered by |
|---|---|
| US-1 watch without polling | §3.1, §4.1–4.3, §7.2, §7.4; AC-1, AC-2 |
| US-2 recent + per-issue history | §3.3, §3.2 (`beads-event-diff`), §7.5–7.7 |
| US-3 rewind | §3.3, §4.6, §7.8; AC-6 |
| US-4 hooks/notifications/subscribe | §3.1 (`beads-live-subscribe`, invalidate hook), §3.3 (`beads-events-notify-mode`), §7.11; AC-7 |
| US-5 city-wide feed | §3.3 (`beads-events-timeline-city`), §8, §7.12 |
| AC-1 live reflect without a tick | §4.3, §7.2, §9 live acceptance |
| AC-2 degrade to poll, no error | §4.1, §4.4, §7.9 |
| AC-3 pruned checkpoint re-baseline | §4.6, §10 |
| AC-4 timeline correctness | §3.3, §9 injection/integration |
| AC-5 diffs + tombstone | §3.2, §7.6, §7.7 |
| AC-6 rewind equivalence | §4.6, §7.8, §9 property assertion |
| AC-7 hooks fire once; notify inert when off | §3.1, §3.3, §7.11 |
| AC-8 standalone-first | §3.5, §8, §11, §9 standalone-first guard |
| AC-9 compile/lint/CI green | §9 |
| Hard constraint: extend not reinvent | §1, §2, §3.5, §6, §7 |
| Hard constraint: mirror gascity pattern | §3 (mirror map), §3.5 |
| Hard constraint: gascity coexist, no dup streams | §4.1, §8 |
| Mockups 1–6 + before/after | §7.1–7.6, §7.9, §7.12 (plus rewind/history/pulse/notify) |
| Reuse `beads-event-record`, command classes | §1.3, §2.7, §6 |

## 13. Decisions log / open questions

**Decided.** Four net-new modules (mirror map §3.5), not the seed's seven; no new
event class; stream spawned via `make-process`, not `execute-async`; one stream
per (root, journal kind); checkpoint keyed by branch+replica; re-baseline on
prune/sync/replica-change; gascity integration is a documented seam, not a code
change.

**Open.** (a) Materialize rewind snapshots every Nth record, or replay from the
bounded ring at the 100k retention floor? Proposed: replay from ring first,
materialize only if profiling demands it. (b) Add a `bd serve` HTTP transport
alongside `tail --follow`? Proposed: a transport generic in `beads-live`, `tail`
first. (c) Does the `recent-changes` dashboard section render from the model or
from `beads-section-register` only? Proposed: register it so gascity can reuse
it; the dashboard consumes the registry. (d) Split history/rewind into their own
files once `beads-events.el` exceeds ~700 lines.

---

*Bead: `be-ca9j` (plan: `beads-events-live`). Planning only; no `.el` source was
changed. The seed `design-seed.md` is retained as an input but is superseded by
this document.*
