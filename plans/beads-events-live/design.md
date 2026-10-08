---
plan_slug: beads-events-live
phase: design
rig: beads.el
rig_root: /home/roman/workspace/beads.el
artifact_root: /home/roman/workspace/beads.el/plans
schema: beads.events-live.design.v1
artifact: design
status: approved
scope: planning-and-build
requirements: requirements.md
created_at: 2026-10-08T19:30:00Z
updated_at: 2026-10-08T19:30:00Z
---

# beads-live — Design

## 1. Thesis

Mardi Gras is a terminal parade that follows the beads journal and shows the
board move. It is a good lens, but it is a separate process, a separate binary,
and it can only show *now* and a short *recent changes* list. Emacs can do
strictly more, because the tracker, the code, the agents, and the user's editing
surfaces all live in one programmable process:

1. **Live, in place.** The existing magit-style dashboard (`beads-dashboard`)
   updates from an event stream instead of a 30s timer, marking changed rows.
2. **A real timeline.** The journal is a first-class buffer: filter, group,
   search, tag-jump, and export to org.
3. **Per-bead history.** Every issue has an ordered log with field diffs.
4. **Rewind.** Time-travel the whole surface to any sequence number and resume.
   No terminal visualizer offers this.
5. **Programmable.** A hook/subscription seam lets magit, org, agents, and the
   user react to change; desktop notifications are opt-in.
6. **City-wide.** One merged feed across every beads store, with rig + actor /
   session attribution.
7. **A control surface.** Writes keep going through the existing
   `beads-command-*` classes, so the live board is also where you triage.

The design rule is **reuse the seams beads.el already has**: `bd events` command
classes, `beads-event-record`, `beads-command-execute-async`,
`beads-dashboard--section`, the `beads-section` registry,
`beads-mode--install-navigation-keys`, and faces that `:inherit`. Nothing here
requires gascity; gascity may enrich attribution at an existing generic.

## 2. Non-goals

- No new `bd` flags; target the documented `bd events` surface.
- No daemon/watcher/database. The model is in-memory, rebuilt from a baseline
  read plus journal replay.
- No direct Dolt access and no second journal implementation.
- No graphical renderer; everything is text, faces, and overlays.
- No gascity code changes; at most an optional attribution generic.

## 3. Current system (what already exists)

- `lisp/beads-command-events.el` — `beads-command-events-tail` (`--since`,
  `--limit`, `--follow`), `-export`, `-prune`, the `beads-events` transient,
  and a JSON-lines parser producing `beads-event-record`.
- `lisp/beads-types.el` — `beads-event-record` (`seq ts op issue-id actor issue
  dep comment`) plus op constants (`created`, `updated`, `status_changed`,
  `dependency_added`, …).
- `lisp/beads-dashboard.el` — a 30s auto-refreshing `vui` dashboard with
  sections, magit-idiomatic navigation, per-section load-more, and a
  visibility/extra cache.
- `lisp/beads-dashboard-sections.el` — section loaders/renderers
  (stats, stale, orphans, in-flight, ready, blocked, closed, epic, federation).
- `lisp/beads-section.el` — the `beads-section-spec` registry and issue-line
  vnodes.
- `lisp/beads-command-execute-async` — the async command runner used elsewhere.

## 4. Architecture

New modules (one concern each), plus small integration points:

```
lisp/beads-events-stream.el   stream supervisor: process, parse, checkpoint,
                              reconnect/backoff, fallback detection, replay-from
lisp/beads-events-model.el    per-store model: apply record, issue index, ring
                              buffer, per-issue log, queries, rewind snapshots
lisp/beads-events-timeline.el timeline + city timeline views
lisp/beads-events-history.el  per-issue history/diff view
lisp/beads-events-rewind.el   time-travel commands + read-only render
lisp/beads-events-notify.el   hooks/subscription + desktop notifications
lisp/beads-events.el          parent transient, dispatch/maintenance entries,
                              defcustoms, faces, whole-feature minor mode
```

Integration edits (small):

- `beads-dashboard.el`: when `beads-live-mode` is on, subscribe the dashboard to
  the model; section updates re-render in place; footer gains the status chip.
- `beads-dispatch` / `beads-maintenance` prefixes: add the `beads-live` entry.
- `beads.el`: autoloads.

### 4.1 Stream supervisor (`beads-events-stream.el`)

One supervisor per store (keyed by absolute repo root + branch + replica id).

- Start: `bd events tail --since <checkpoint> --follow --json`, run through
  `beads-command-execute-async` (never `shell-command`); lines accumulate in a
  process filter and are parsed with the existing
  `beads-events-records-from-json-lines`.
- Checkpoint: `seq` persisted per store under a beads state dir
  (`beads-events-checkpoint-<hash>.eld`), written after each applied batch
  (debounced). Advertise the store/branch/replica so a checkpoint is never
  carried across replicas.
- Fallback: if the journal is off (`bd config get events-journal` = false) or the
  tail errors "journal disabled", mark the store `poll` and let the existing
  dashboard timer drive; if tail exits, reconnect with capped backoff; if
  `seq` is below the store's minimum (pruned), re-baseline (full read, reset
  checkpoint) and log why.
- Reconciliation: because `bd dolt pull` and `bd sql` are not journaled, run an
  occasional full baseline (the existing 30s timer, relaxed) as a safety net;
  label the stream `live` only while journal deltas are arriving.
- Injection seam: all process creation goes through a `beads-events-stream--spawn`
  generic so tests substitute a scripted emitter (no subprocess).

### 4.2 Model (`beads-events-model.el`)

Pure, testable, buffer-independent.

- `beads-events-model` class: store identity, `last-seq`, `issues` hash
  (id → latest `beads-issue` snapshot), `records` ring buffer (bounded), and
  `logs` hash (id → ordered records).
- `beads-events-model-apply` folds a record: update/insert the issue snapshot
  from `record.issue` (nil for delete → keep a tombstone stub), append to the
  per-issue log and the ring buffer, stamp a `changed-at` for the ◈ marker.
- `beads-events-model--snapshot-at seq`: reconstruct state at a seq by replaying
  the ring from the nearest retained baseline; used by rewind.
- Queries power every view: `-in-flight`, `-ready`, `-blocked`, `-recent`,
  `-history`, `-stats`, `-changes-since`.

### 4.3 Dashboard live integration

`beads-dashboard` already builds sections from loaders. Add a live provider that
returns vnodes from the model when the stream is `live`, falling back to the
existing `bd` loaders otherwise. Section re-render is debounced (e.g. 150 ms) so
a burst of events (the factory drains fast) coalesces into one redisplay. Rows
that changed within `beads-events-change-mark-duration` (default 30s) carry a `◈`
prefix using an `:inherit` face; the footer chip reads `live ∿ <rate>` or
`poll`/`partial`.

### 4.4 Views

- **Timeline** (`beads-events-timeline`): a derived `vui-mode` (or
  `tabulated-list` + `vui` section header) over `-recent`. Filters by op, actor,
  issue, and time; `g` toggles follow; `RET` visits the issue; `e` exports to an
  org buffer; `r` rewinds to the selected seq.
- **History** (`beads-events-history`): one issue's log, newest last, each entry
  with a compact field diff computed from successive snapshots.
- **Rewind** (`beads-events-rewind`): prompt `seq` or `N` events ago; render the
  timeline/dashboard from `-snapshot-at`; a `REWIND @seq` header and a distinct
  face/modeline; `G`/`g` step, `l`/`r` resume live.
- **City timeline** (`beads-events-timeline-city`): merge all registered store
  models, sorted by wall-clock (records are not seq-comparable across stores),
  each row tagged with rig and actor/session.

### 4.5 Hooks, notifications, subscription

- `beads-event-hooks` (abnormal hook) runs once per applied record with
  `(record store)`.
- `beads-events-subscribe` registers a callback; returns an unsubscribe
  function. Internal views use it; external packages use it too.
- `beads-events-notify-mode`: opt-in; notifies for a configurable op set
  (default close + `dependency_added` that makes a bead ready). Never notifies
  for the user's own foreground command unless configured.

### 4.6 Actor attribution

`record.actor` is the acting identity. Render it directly. Optionally enrich via
a generic `beads-events-actor-description` that gascity (or any package) can
override to map an actor/session to a friendly agent name. No gascity symbol on
the default path.

## 5. Data flow

```
 bd events tail --since S --follow  ──► parse ──► model-apply ──► subscribers
        ▲                                             │
        └── checkpoint S' ◄── debounced save          ├─► dashboard sections (debounced redisplay)
                                                      ├─► timeline / history buffers
                                                      ├─► hooks / notifications
                                                      └─► city aggregate
 baseline: bd list/show (existing loaders) ────────────┘ (reconcile + rewind base)
```

## 6. UI mockups

Mockups are ASCII, matching the plan conventions in this repo. Faces named in
comments are Emacs faces; all derive with `:inherit`.

### 6.1 Live dashboard (`beads-dashboard`, `beads-live-mode` on)

```
*b e a d s - d a s h b o a r d*                              live ∿ 3/s   seq 1042
────────────────────────────────────────────────────────────────────────────────
  beads · /home/roman/workspace/beads.el · ● main · journal ON
────────────────────────────────────────────────────────────────────────────────
▾ STATS                open 24 · in flight 3 · blocked 5 · ready 12 · ◈ 4
────────────────────────────────────────────────────────────────────────────────
▾ IN FLIGHT (3)                                                    [all 3 ▾]
  ◈ be-ndi6   ◐ github-pr-review: PR #68 review      @beads/reviewer   1m
    be-oyfn   ◐ standalone-formulas build             @beads/worker    22m
    ga-gi00b  ◐ WI-SF-19 dedup                         @gascity/worker 3m
▾ READY (12)                                                       [4 more ▾]
    be-m3vd   ○ WI-SF-18 swarm actions + navigation    P2 · L
    be-w5b4   ○ WI-SF-17 swarm validate / waves        P2 · M
    be-ijfx   ○ WI-SF-02 molecule actions + work loop  P2 · M     ← ready now
▾ BLOCKED (5)
    be-ozmm   ⊘ waiting on be-25en                     P2 · 2 blockers
    ...
▾ RECENT CHANGES (live)                              [open timeline… (t)]
    #1047 12:31:07  close   be-ndi6  github-pr-review      @beads/reviewer
    #1046 12:30:02  close   be-ijfx  WI-SF-02 (superseded) @mayor
    #1045 12:29:48  update  ga-gi00b  implement → shipped  @gascity/worker
────────────────────────────────────────────────────────────────────────────────
  n/p move · SPC fold · RET visit · TAB section · t timeline · R rewind · s sling
```

Notes:
- `◈` (face `beads-events-changed`) marks rows changed within 30s; it fades on
  the next redisplay after the window.
- The header chip `live ∿ 3/s seq 1042` is the `beads-events-status` face; it
  becomes `poll` (see 6.7) with the journal off, and `partial` while
  reconciling after a non-journaled write.

### 6.2 Timeline — recent changes (`beads-events-timeline`)

```
*b e a d s - t i m e l i n e*                       live ∿ 3/s   filter: op=any
────────────────────────────────────────────────────────────────────────────────
  seq     time      op              issue                       actor
────────────────────────────────────────────────────────────────────────────────
  1047    12:31:07  close           be-ndi6  github-pr-review   @beads/reviewer
  1046    12:30:02  close           be-ijfx  WI-SF-02            @mayor
  1045    12:29:48  update          ga-gi00b implement→shipped   @gascity/worker
  1044    12:29:31  comment         be-ndi6  "review passed"     @beads/reviewer
  1043    12:28:02  dependency_added be-ijfx ← be-yzbs           @mayor
  1042    12:27:44  create          be-ijfx  WI-SF-02 molecule   @mayor
  ─── rewind cursor  ▲                                                        ───
  1041    12:20:11  status_changed  be-w5b4  open → in_progress @beads/worker
────────────────────────────────────────────────────────────────────────────────
  j/k move · g follow · f filter(op/actor/issue) · RET visit · r rewind · e org
```

Filtering (inline minibuffer prompt, `f`):

```
  f op:close                   f actor:@gascity/*        f issue:ga-gi00b
  f since:12:20                f type:bug,label:backend  f "review passed"
```

### 6.3 Timeline — grouped by issue (`C-c C-g` or `f group:issue`)

```
*b e a d s - t i m e l i n e*                       grouped by issue · 6 issues
────────────────────────────────────────────────────────────────────────────────
▾ be-ndi6  github-pr-review: PR #68 review        closed · @beads/reviewer
    #1047  12:31:07  close        → closed
    #1044  12:29:31  comment      "review passed"
    #1031  12:18:40  status_changed open → in_progress
    #1030  12:18:09  create       github-pr-review
▾ ga-gi00b  WI-SF-19 dedup                        in_progress · @gascity/worker
    #1045  12:29:48  update       implement → shipped
    #1039  12:20:03  status_changed open → in_progress
▾ be-ijfx  WI-SF-02 molecule actions + work loop  closed · @mayor
    #1046  12:30:02  close        → closed
    #1043  12:28:02  dependency_added ← be-yzbs
    #1042  12:27:44  create       WI-SF-02 molecule actions
```

### 6.4 Per-bead history (`beads-events-history`)

```
*b e a d s - h i s t o r y :  be-ijfx*            WI-SF-02 · 3 records · closed
────────────────────────────────────────────────────────────────────────────────
  #1046  12:30:02  @mayor
  close
    status            open        → closed
    close_reason      —           → "superseded by PR #68 (4904d2a)"
────────────────────────────────────────────────────────────────────────────────
  #1043  12:28:02  @mayor
  dependency_added
    be-ijfx           blocks      ← be-yzbs
    (be-ijfx was blocked: true)
────────────────────────────────────────────────────────────────────────────────
  #1042  12:27:44  @mayor
  create
    title             —           → "WI-SF-02 — Molecule actions + work loop"
    type               —          → task
    priority           —          → P2
    status             —          → open
────────────────────────────────────────────────────────────────────────────────
  n/p record · TAB fold · r rewind to this seq · RET visit · q quit
```

A deleted issue renders its tombstone:

```
  #1051  12:44:10  @cleanup-bot
  delete
    be-xyz9           (tombstone — snapshot null)
```

### 6.5 Rewind / time travel (`beads-events-rewind`)

Prompt then render:

```
  Rewind to seq (or -N / +N, blank = live): 1040 RET
```

```
*b e a d s - d a s h b o a r d*            ⏪ REWIND @1040  ·  +7 events  ·  read-only
────────────────────────────────────────────────────────────────────────────────
  beads · /home/roman/workspace/beads.el · ● main · as of 12:27:44
────────────────────────────────────────────────────────────────────────────────
▾ STATS                open 22 · in flight 2 · blocked 4 · ready 11
────────────────────────────────────────────────────────────────────────────────
▾ IN FLIGHT (2)
    be-oyfn   ◐ standalone-formulas build             @beads/worker    22m
    be-w5b4   ◐ WI-SF-17 swarm validate / waves       @beads/worker    8m
▾ RECENT CHANGES (up to #1040)
    #1040 12:26:58  create  be-m3vd  WI-SF-18 swarm actions   @mayor
    ...
────────────────────────────────────────────────────────────────────────────────
  g forward event · G back event · l/R resume live · R jump · q close
```

Stepping forward crosses into the future relative to the cursor and re-renders
at each seq, so the user can watch the replay.

### 6.6 Parent transient (`beads-events`, extended) and dispatch entry

```
press e (from dispatch) or M-x beads-events
╭──────────────────────────────────────────────────────────────────────────────╮
│ Events                                                                        │
╭──────────────────────────────┬───────────────────────────────────────────────┤
│ Live                                                                          │
│ l  Live dashboard         (beads-dashboard, live mode)                        │
│ t  Timeline               (beads-events-timeline)                             │
│ h  History for issue      (beads-events-history)                              │
│ c  City timeline          (beads-events-timeline-city)                        │
├──────────────────────────────┼───────────────────────────────────────────────┤
│ Journal                                                                       │
│ t  Tail records           (beads-events-tail)                                 │
│ e  Export journal         (beads-events-export)                               │
│ p  Prune records          (beads-events-prune)                                │
├──────────────────────────────┼───────────────────────────────────────────────┤
│ Time travel                                                                   │
│ R  Rewind to seq          (beads-events-rewind)                               │
│ s  Snapshot at seq…       (beads-events-snapshot)                             │
├──────────────────────────────┼───────────────────────────────────────────────┤
│ Settings                                                                      │
│ n  Toggle notifications   (beads-events-notify-mode)                          │
│ C  Copy checkpoint        (beads-events-copy-checkpoint)                      │
│ q  Quit                                                                       │
╰──────────────────────────────┴───────────────────────────────────────────────╯
```

### 6.7 Journal off / onboarding (no error, honest fallback)

```
*b e a d s - d a s h b o a r d*                                              poll 30s
────────────────────────────────────────────────────────────────────────────────
  beads · /home/roman/workspace/beads.el · ● main · journal OFF
  ⓘ events journal is not enabled here — refreshing every 30s.
    Enable live updates:  bd config set events-journal true   [copy (w)]
    (this edits .beads/config.yaml; every bd write then records an event)
────────────────────────────────────────────────────────────────────────────────
▾ STATS                open 24 · in flight 3 · blocked 5 · ready 12
...
```

### 6.8 Modeline / status strip (any frame)

```
  *beads-dashboard*   Beads-dash  ◐ 3·5·12   ∿3/s  seq 1047   [live]
  *beads-timeline*    Beads-TL    ◐ 3·5·12   ⏪ @1040 +7      [rewind]
  *beads-dashboard*   Beads-dash  ◐ 3·5·12   ↻ 30s            [poll]
```

The strip is optional (`beads-events-modeline-mode`); the segment function
`beads-events-mode-line-string` is public so users can compose it.

### 6.9 City timeline (`beads-events-timeline-city`)

```
*b e a d s - c i t y - t i m e l i n e*                 live · 2 stores · merged
────────────────────────────────────────────────────────────────────────────────
  time      rig        op              issue       actor / session
────────────────────────────────────────────────────────────────────────────────
  12:31:07  beads.el   close           be-ndi6     @beads/reviewer
  12:31:01  gascity.el update          ga-gi00b    @gascity/worker (ec-wisp-j3ne2b)
  12:30:02  beads.el   close           be-ijfx     @mayor
  12:29:48  gascity.el status_changed  ga-gi00b    @gascity/worker
  ...
────────────────────────────────────────────────────────────────────────────────
  j/k move · f rig:gascity.el · RET visit (cross-rig) · e org · q quit
```

### 6.10 Notification (opt-in, `beads-events-notify-mode`)

```
   ┌─────────────────────────────────────────────────────────────┐
   │ beads-live · beads.el                                        │
   │ be-ijfx unblocked — ready to work        [open] [sling] [mute]│
   └─────────────────────────────────────────────────────────────┘
```

Desktop notifications use `notifications-notify` when available and fall back
to `message`; `[sling]` invokes the existing `beads-sling` seam.

## 7. Emacs-native advantages over Mardi Gras

| Dimension | Mardi Gras (Go TUI) | beads-live (Emacs) |
|---|---|---|
| Live source | `bd events` / `bd serve` / poll | `bd events tail --follow` (or poll), same journal |
| Time travel | recent-changes list only | full rewind/replay to any seq |
| Per-issue history | activity list | ordered log with field-level diffs |
| Extensibility | fixed feature set | hooks + `beads-events-subscribe`; any package reacts |
| Integration | separate binary, tmux | same process as magit/org/agents; org export; notifications |
| Writes | its own keymap | existing `beads-command-*` transients/porcelain |
| Multi-store | single project | merged city timeline with rig/actor attribution |
| Rendering | bespoke TUI | faces/overlays, ada/Emacs accessibility, isearch/occur/imenu |

## 8. Testing strategy

- **Unit (ERT):** model apply/fold (create/update/close/reopen/dep_add/
  dep_remove/comment/delete), `-snapshot-at` rewind equivalence,
  checkpoint read/write/`prune` re-baseline, filter grammar, diff computation.
- **Injection:** a scripted stream emitter (no subprocess) drives deterministic
  event sequences; assert buffer contents via `vui` render helpers, the same
  style as `beads-dashboard-test.el`.
- **Integration (tagged `:integration`):** enable the journal in a temp store,
  run real `bd create/dep add/close`, tail, and assert the model; skip cleanly
  when `bd` or journaling is unavailable.
- **Live acceptance (operator constraint):** `emacs -nw -Q` in tmux against a
  real store; record buffer state as bead evidence; verify live update, rewind,
  and the journal-off fallback.
- **Guards:** standalone-first guard test (no gascity symbol on default paths);
  byte-compile `--warnings-as-errors`; `eldev` lint; CI test/lint/coverage.

## 9. Rollout / risk

- Feature is additive and opt-in (`beads-live-mode`); with it off, behaviour is
  exactly today's polling dashboard.
- Risk: journal is per-branch/per-replica and can be pruned; mitigated by
  per-replica checkpoints + re-baseline + a reconciling baseline read.
- Risk: event bursts; mitigated by debounced redisplay and bounded ring/log.
- Risk: `bd` version drift (journal present only in 1.3+); detect capability
  once and fall back to poll silently.

## 10. Open questions

- Should rewind snapshots be materialized (persist every Nth snapshot) for deep
  histories, or is replay-from-ring sufficient at the 100k retention floor?
- Do we want a `bd serve` HTTP-stream transport in addition to `tail --follow`
  (mardi-gras supports both)? Proposed: a pluggable transport generic in the
  stream supervisor, `tail` first.
- Should `beads-events-notify-mode` default to a rate limit per issue to avoid
  notification storms during a factory drain? Proposed: yes, one per issue per
  minute.
