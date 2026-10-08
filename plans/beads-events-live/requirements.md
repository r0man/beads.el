---
plan_slug: beads-events-live
phase: requirements
rig: beads.el
rig_root: /home/roman/workspace/beads.el
artifact_root: /home/roman/workspace/beads.el/plans
schema: beads.events-live.requirements.v1
artifact: requirements
status: approved
scope: planning-and-build
created_at: 2026-10-08T19:30:00Z
updated_at: 2026-10-08T19:30:00Z
---

# Requirements: beads-live — a live, rewindable event surface for beads.el

Problem source: `bd` 1.3 added a durable, ordered events journal
(`bd events tail --since <seq> [--follow]`, `bd events export`,
`bd events prune`). Community tools now build live visualizers on it — e.g.
Mardi Gras (`quietpublish/mardi-gras`), a Go/Bubble Tea terminal parade, and
`bddb`, a live kanban board. beads.el already has the `bd events` command
classes (`beads-command-events.el`) and a polling magit-style dashboard
(`beads-dashboard.el`), but it does not consume the journal, so the dashboard
refreshes on a 30s timer and no view answers "what changed, and when?".

This plan gives beads.el a first-class, Emacs-native event surface that is
*live* (follows the journal), *rewindable* (reconstruct any past state), and
*programmable* (hooks/notifications/org export), reusing the existing
dashboard, command classes, `vui` section registry, and transient system.

## Problem Statement

A human doing beads triage in Emacs cannot see change as motion:

1. The dashboard polls `bd` on a timer; changes made by agents, scripts, or
   other clones appear late, and every poll is a full `bd` process.
2. There is no view of the journal itself: no "recent changes" feed, no
   per-issue activity log, no actor attribution.
3. There is no way to answer "what did the board look like when I left?" or
   "what changed since seq N?" without re-reading the store.
4. Other Emacs surfaces (magit, org agenda, notifications, agent sessions)
   cannot react to bead changes because there is no event seam.

## Solution

A new `beads-live` feature set, built on `bd events`:

- **Stream engine** — a supervised async `bd events tail --since <seq>
  --follow` process that emits parsed `beads-event-record` values to a
  per-store model, with checkpoint persistence, reconnect/backoff, and
  graceful fallback to polling when the journal is off or a checkpoint is
  stale.
- **Live dashboard** — the existing `beads-dashboard` sections update in place
  from the event model, mark changed rows, and show a live/poll status chip.
- **Timeline** — a dedicated "Recent changes" view with op/actor/issue
  filters, grouping, and org export.
- **Per-bead history** — an issue's lifecycle as an ordered log with
  field-level diffs and `git log`-style navigation.
- **Rewind** — time-travel the whole surface to any sequence number, then
  resume live. This is the feature no terminal visualizer offers.
- **Hooks + notifications** — a `beads-event-hooks` seam and opt-in desktop
  notifications so other Emacs packages and the user can react.
- **City-wide aggregate** — one timeline across all beads stores in a Gas City
  workspace, with actor/session attribution.

## User Stories

### US-1: Watch work move without polling

As an Emacs user, I open the beads dashboard and see agent work land within a
second, without a 30s poll.

- `bd config set events-journal true` in the store; dashboard footer shows
  `live` and updates arrive via the journal.
- With the journal off, the footer shows `poll` and behaviour is exactly the
  current timer refresh (no regression).
- Idle cost is one long-lived `bd events tail --follow` process and no
  per-tick `bd list`.

### US-2: See what changed, recently and per issue

As a triager, I want a chronological feed and an issue-local history.

- `beads-events-timeline` lists records newest-first: seq, time, op, issue
  title, actor.
- `beads-events-history` on an issue shows each mutation with a field diff
  (`status open → in_progress`, `assignee — → alice`, `dependency_added
  issue-2`).
- Filters by op, actor, and issue; results export to an org buffer.

### US-3: Rewind the board

As a reviewer, I want to see the workspace as of an earlier sequence number.

- `beads-events-rewind` prompts for a seq (or a relative "N events ago") and
  renders the model as of that seq in a read-only "REWIND" header.
- `g`/`G` step event-by-event forward/back; `l`/`r` resume live.
- Rewind never writes; a distinct face/modeline makes the non-live state
  obvious.

### US-4: React to changes in Emacs

As a user, I want Emacs to tell me when something I care about changes.

- `beads-event-hooks` runs for each applied record with the record and store.
- Opt-in `beads-events-notify-mode` raises a desktop notification for
  configured ops (default: close, dependency_added that unblocks me).
- A public `beads-events-subscribe` lets magit/org/agents refresh on change.

### US-5: One city, one feed

As a Gas City operator, I want one timeline across all rigs.

- `beads-events-timeline-city` merges records from every registered bead store,
  tagging each row with its rig and, when known, the session/agent.
- Per-store seq spaces stay separate; the merged view sorts by wall-clock and
  never mixes checkpoints.

## Acceptance Criteria

- AC-1: With the journal enabled, a `bd create`/`bd close` in a shell is
  reflected on an open dashboard without a timer tick (verified live in
  `emacs -nw -Q`, recorded as bead evidence).
- AC-2: With the journal disabled, the feature degrades to the existing
  polling dashboard and reports `poll`; no error is raised.
- AC-3: A checkpoint whose seq has been pruned (below the retention floor)
  triggers a documented re-baseline (full reload + reset checkpoint), not a
  silent stall.
- AC-4: `beads-events-timeline` renders the record set for a scripted sequence
  of create/dep_add/close/comment; op, actor, issue id, and status are correct.
- AC-5: `beads-events-history` shows field-level diffs for update/close/
  reopen; a deleted issue renders its tombstone record.
- AC-6: Rewind to seq K renders exactly the state that live rendering showed
  when its checkpoint was K (property-style assertion over a scripted run).
- AC-7: `beads-event-hooks` fires once per applied record; notification mode is
  inert when disabled.
- AC-8: Everything renders with gascity absent (standalone-first); no gascity
  symbol on any default path.
- AC-9: Byte-compile clean, `eldev` lint clean, unit + integration ERT green;
  CI (test/lint/coverage) green on the PR.

## Out Of Scope

- New `bd` subcommands or flags; the feature targets the documented
  `bd events` surface only.
- A separate daemon, watcher service, or persistent database; the model is
  in-memory and rebuilt from `bd events` + a baseline `bd` read.
- Reading Dolt tables directly or reimplementing the journal.
- A graphical/canvas renderer; mockups are text faces/overlays.
- gascity code changes; only optional attribution enrichment at an existing
  seam.

## Other Notes

- Source of truth for `bd` semantics: `bd events --help`, the 1.3 release
  notes, and the journal caveats (per-branch, per-replica, `bd dolt pull` and
  `bd sql` are not journaled, retention floors).
- Prior art to exceed: `quietpublish/mardi-gras` (parade, recent-changes,
  polling+journal+`bd serve`), `bddb` (kanban, live via journal+`bd serve`).
- Reuse, do not duplicate: `beads-command-events-*`, `beads-event-record`
  (`beads-types.el`), `beads-command-execute-async`, `beads-dashboard--section`,
  the `beads-section` registry, `beads-mode--install-navigation-keys`, faces
  derive with `:inherit`.
- The design (with the required UI mockups) is `design.md` in this directory.
