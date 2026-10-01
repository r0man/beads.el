---
schema: beads.ui-redesign.mockups.v1
workflow:
  id: be-mv8d
  predecessor: be-59fe
artifact: menu-mockups
status: draft-for-review
scope: planning-only
refinement: 2
---

# beads.el UI Redesign — ASCII Menu Mockups (multi-state)

ASCII renderings of every redesigned user-facing element in **every relevant
state**, for review before implementation. Real rendering rules (groups,
collapse, keys) follow each figure. Data is representative of a real
beads.el store (the `beads.el` project; bead `be-mv8d` is this refinement).
No "TBD" placeholders.

**Global rendering rules** (apply to every figure unless overridden):

- Buffers are **hand-built**, not transient-rendered, except the dispatch and
  sling transients, which render *only* arguments/actions and never content.
- `q` buries, `g` refreshes in place, `C-u g` hard-refreshes, `TAB`/`S-TAB`
  move by `beads-thing`, `SPC` toggles the thing/section at point, `RET`
  visits/activates, `?` opens the dispatch menu.
- Sections render as `▾ Title (n)` expanded / `▸ Title (n)` collapsed;
  collapse is persistent per store.
- Status glyphs: `○` open, `◐` in-progress, `⛔` blocked, `✓` closed.
  Priority: `P0…P4` (P0 red). Agent state: `🦅` running, `…` idle, `✗` failed
  (letters `T/R/P` fall back when icons are off).
- The mode line carries `[store] · counts · filter · agent-state` in every
  porcelain view.
- Extension keys live under the `C-c b` prefix, shown in `?` only.
- Every async section has exactly four states: **loading / empty / populated /
  error**. All four are mocked below where the surface can show them.

Reserved single-letter keys in beads buffers:
`q g TAB S-TAB SPC RET ? n p N P` and the action keys
`d C s # a S e m / c` (context-dependent; documented per view).

---

## 1. Entry — the status buffer (`M-x beads`)

The Magit-like front door (F2). vui; each section loads asynchronously.
`RET` visits, `TAB`/`SPC` folds a section, `g` refreshes.

### 1a. Loading (open, first paint)

```
Beads — beads.el                                          (g refresh · ? help · q bury)
──────────────────────────────────────────────────────────────────────────────────────
  Store    .beads/embeddeddolt · prefix be- · 42 issues · bd 1.0.5
  Repo     /home/roman/workspace/beads.el · branch redesign/beads-ui-plan · clean
  Agents   loading…                                        (a agents · A attach)
  Remote   local

▾ In progress (…)                                            ⠋ loading — bd list --status in_progress
▾ Ready (…)                                                  ⠋ loading — bd ready
▾ Blocked (…)                                                ⠋ loading — bd blocked
▾ Recent activity (…)                                        ⠋ loading — bd history
──────────────────────────────────────────────────────────────────────────────────────
 s sling  a agent  c create  l list  / search  ! maintenance  ? dispatch  q bury
```

Rendering rules: each section's loader goes through
`beads-command-execute-async` with `:queue t`; the mode line is updated once
per section completion, not per render. Section keys are still foldable while
loading.

### 1b. Empty (a store with no issues)

```
Beads — beads.el                                          (g refresh · ? help · q bury)
──────────────────────────────────────────────────────────────────────────────────────
  Store    .beads/embeddeddolt · prefix be- · 0 issues · bd 1.0.5
  Repo     /home/roman/workspace/beads.el · branch main · clean
  Agents   none
  Remote   local

▾ In progress (0)                                                  (SPC fold)
    No in-progress issues.
▾ Ready (0)
    Nothing ready — `c` to create a bead.
▾ Blocked (0)
    No blocked issues.
▾ Recent activity (0)
    No recent activity.
──────────────────────────────────────────────────────────────────────────────────────
 c create  l list  ? dispatch  q bury
```

Rendering rules: empty is a first-class state (`beads-dashboard--data-empty-p`
→ `beads-dashboard--empty-line`), never a blank buffer. The footer drops
`S`/`a` when no bead exists to act on.

### 1c. Populated (the canonical state)

```
Beads — beads.el                                          (g refresh · ? help · q bury)
──────────────────────────────────────────────────────────────────────────────────────
  Store    .beads/embeddeddolt · prefix be- · 42 issues · bd 1.0.5
  Repo     /home/roman/workspace/beads.el · branch redesign/beads-ui-plan · clean
  Agents   2 running · 1 failed                            (a agents · A attach)
  Remote   local

▾ In progress (2)                                                   (SPC fold)
    ◐ be-mv8d  P2  Task    Refine beads.el UI redesign plan                 @gc.design-author  2m
    ◐ be-1234  P1  Bug     Fix list refresh race                            @you                1h
▾ Ready (3)
    ○ be-abcd  P2  Task    Add formula browser                              —
    ○ be-ef01  P3  Bug     Graph layout for epics                           —
    ○ be-2345  P2  Task    Cache eldoc negatives                            —
▾ Blocked (1)
    ⛔ be-9999  P2  Task    Waiting on be-59fe                              ← be-59fe
▾ Recent activity (3)
    2m   be-1234  status open → in_progress
    1h   be-59fe  created by mayor
    3h   be-abcd  priority P3 → P2

──────────────────────────────────────────────────────────────────────────────────────
 s sling  a agent  c create  l list  / search  ! maintenance  ? dispatch  q bury
```

Rendering rules: sections are `beads-section-spec` entries registered via
`beads-section-register`; each loader is async; the footer is the mode line's
`help-echo` summary and mirrors `?`.

### 1d. One section folded (fold state persistent per store)

```
Beads — beads.el                                          (g refresh · ? help · q bury)
──────────────────────────────────────────────────────────────────────────────────────
  Store    .beads/embeddeddolt · prefix be- · 42 issues · bd 1.0.5
  Repo     /home/roman/workspace/beads.el · branch redesign/beads-ui-plan · clean
  Agents   2 running · 1 failed
  Remote   local

▸ In progress (2)                                                   (SPC expand)
▾ Ready (3)
    ○ be-abcd  P2  Task    Add formula browser                              —
    ...
```

Rendering rules: folding is `SPC` on the section header (a `beads-section`
thing); the count stays visible; the collapsed state is saved per store
(`beads-dashboard--save-visibility`), so reopening the same store restores it.

### 1e. Error (one section fails, the rest render)

```
Beads — beads.el                                          (g refresh · ? help · q bury)
──────────────────────────────────────────────────────────────────────────────────────
  Store    .beads/embeddeddolt · prefix be- · 42 issues · bd 1.0.5
  Repo     /home/roman/workspace/beads.el · branch redesign/beads-ui-plan · clean
  Agents   2 running · 1 failed
  Remote   local

▾ In progress (2)
    ◐ be-mv8d  P2  Task    Refine beads.el UI redesign plan                 @gc.design-author  2m
    ◐ be-1234  P1  Bug     Fix list refresh race                            @you                1h
▾ Ready (0)
    ⚠ failed: beads-command-error "bd ready exited 1: database is locked"
      (g to retry this section)
▾ Blocked (1)
    ⛔ be-9999  P2  Task    Waiting on be-59fe                              ← be-59fe

──────────────────────────────────────────────────────────────────────────────────────
 g retry failed section  ? dispatch  q bury
```

Rendering rules: `vui-error-boundary` wraps each section, so one failure never
blanks the board; the error line carries the condition message and a
section-local retry (`g`). `C-u g` clears caches and retries everything.

---

## 2. The dispatch menu (`?`)

Hand-built transient; argument/dispatch only, never content (REQ-004).
Grouped by frequency; every command has a description. Downstream groups are
appended via `beads-menu-providers` (the `[City]` group appears only when
gascity.el is present).

```
Beads — beads.el                                                          (q quit)
──────────────────────────────────────────────────────────────────────────────────────
 Issues
  l  List beads            c  Create bead           /  Search            i  Show…
  u  Update bead           x  Close bead            o  Reopen            e  Edit field
 Workflow
  r  Ready work            b  Blocked work          d  Dependencies      #  Priority
  C  Claim                 s  Set status            m  Molecules         T  Types
 Views
  S  Sling…                a  Agents…               F  Formulas…         v  Graph
  D  Dashboard             H  History               Y  Orphans           t  Stats
 Manage
  L  Labels…               k  Dolt…                 .  Config            W  Worktrees…
 Maintenance
  !  Maintenance…          >  Advanced…             g  Refresh cache     q  Quit
 [City]                                                    (only with gascity.el)
  C-c b c  City status     C-c b r  Rig list        C-c b s  gc sling
──────────────────────────────────────────────────────────────────────────────────────
```

### 2a. Context group (opened from a list/detail row)

When `?` is opened with a bead at point, a `Context` group is prepended from
`beads-action-providers`; standalone it is empty and the group is absent:

```
Beads — list · be-abcd                                              (q quit)
──────────────────────────────────────────────────────────────────────────────────────
 Context — be-abcd (open)
  RET  Visit               d  Close                C  Claim              s  Status
  #    Priority            a  Agents…              S  Sling              m  Mark
 Issues
  l  List beads            c  Create bead           /  Search            i  Show…
 ...
──────────────────────────────────────────────────────────────────────────────────────
```

### 2b. Help / `?` inside the dispatch menu

`C-h` (or the transient's own help) lists every binding with its full
description; the close-on-`q` rule is unchanged. The menu never renders
bead content, only labels.

---

## 3. Maintenance menu (`!`)

The single collapsed successor of `beads-ops-menu` + `beads-advanced-menu`
(REQ-023). Paginated groups (`C-n`/`C-p`) rather than ten stacked groups.

### 3a. Page 1 — Database / Structure

```
Beads maintenance — beads.el                                               (q quit)
──────────────────────────────────────────────────────────────────────────────────────
 Database
  B  Backup                E  Export JSONL           P  Prune closed      G  GC
  b  Batch ops             c  Compact                F  Flatten           i  Ping
  +  Doctor                M  Migrate…               y  Snapshot          x  Cleanup
 Structure
  1  Duplicate kind        2  Find duplicates        3  Supersede         4  Restore
  q  Query (SQL)           R  Rename prefix          _  Forget memory
 ────────────────────────────────────────────────────────────────────────────────────
 Page 1/2   (C-n next page · C-p previous · q quit)
```

### 3b. Page 2 — Integrations / Setup

```
Beads maintenance — beads.el                                               (q quit)
──────────────────────────────────────────────────────────────────────────────────────
 Integrations
  j  Jira                  N  Linear                G  GitHub            y  GitLab
  A  ADO                   *  Mail delegate         f  Federation
 Setup
  I  Init project          S  Init safety           O  Bootstrap         h  Hooks…
  K  KV store              |  Memories              W  Worktrees…        r  Rules…
 ────────────────────────────────────────────────────────────────────────────────────
 Page 2/2   (C-n next page · C-p previous · q quit)
```

Rendering rules: a command that is a core action (close, claim, status,
priority) lives in the status/list/detail context keys, not here. Every
retained entry has a one-line description.

---

## 4. List redesign

`tabulated-list-mode` with `beads-thing` marking and vui-free sections.
Sections group by status (or by the active filter). Paging via `beads-pager`.
A header line summarises counts and filter. `/` opens the retained filter
transient (the one allowed generated surface).

### 4a. Normal

```
Beads list — beads.el          [42 issues · ready 3 · in-progress 2 · filter: none]   (g refresh)
────────────────────────────────────────────────────────────────────────────────────────────
   ID       P  Status       Type     Title                                          Agent
▾ Ready (3)
   be-abcd  P2 ○ open       task     Add formula browser                            —
   be-ef01  P3 ○ open       bug      Graph layout for epics                         —
   be-2345  P2 ○ open       task     Cache eldoc negatives                          —
▾ In progress (2)
   be-mv8d  P2 ◐ in_progress task    Refine beads.el UI redesign plan               🦅 design
   be-1234  P1 ◐ in_progress bug     Fix list refresh race                          🦅 you
▾ Blocked (1)
   be-9999  P2 ⛔ blocked     task     Waiting on be-59fe                             —
────────────────────────────────────────────────────────────────────────────────────────────
  RET visit · SPC fold · m mark · d close · C claim · s status · # priority · a agent · S sling · / filter
```

### 4b. Narrow window (40 cols: title truncates, agent column drops)

```
Beads list — beads.el  [42 · ready 3]        (g refresh)
────────────────────────────────────────────────────────────
   ID       P  St      Title
▾ Ready (3)
   be-abcd  P2 ○ open  Add formula browser
   be-ef01  P3 ○ open  Graph layout for epi…
   be-2345  P2 ○ open  Cache eldoc negatives
▾ In progress (2)
   be-mv8d  P2 ◐ in_pro… Refine beads.el UI …
   be-1234  P1 ◐ in_pro… Fix list refresh r…
────────────────────────────────────────────────────────────
  RET visit · / filter · ? dispatch
```

Rendering rules: columns are `beads-spec`-driven and responsive — the Agent
and Type columns drop before the Title truncates; IDs never truncate.

### 4c. Long titles (title wraps to a continuation row, indented)

```
Beads list — beads.el          [42 issues · filter: none]                            (g refresh)
────────────────────────────────────────────────────────────────────────────────────────────
   ID       P  Status       Type     Title
▾ In progress (1)
   be-mv8d  P2 ◐ in_progress task    Refine beads.el UI redesign plan: deeper design +
                                     thorough multi-state mockups (fold F2 rename +
                                     F3 remove QA/Custom entirely)
────────────────────────────────────────────────────────────────────────────────────────────
```

Rendering rules: continuation rows carry no ID and are not things; TAB/S-TAB
skip them; `RET` on any continuation row visits the owning bead.

### 4d. No matches

```
Beads list — beads.el          [0 issues · filter: status=closed type=bug]           (g refresh)
────────────────────────────────────────────────────────────────────────────────────────────
   ID       P  Status       Type     Title
    No beads match this filter.
       /  change filter      x  clear filter      c  create a bead
────────────────────────────────────────────────────────────────────────────────────────────
```

Rendering rules: an empty result is a first-class state; the footer offers
filter-edit/clear/create.

### 4e. Filters active (header shows the spec)

```
Beads list — beads.el          [7 issues · ready 3 · filter: type=task status≠closed]  (g refresh)
────────────────────────────────────────────────────────────────────────────────────────────
   ID       P  Status       Type     Title                                          Agent
▾ Ready (3)
   be-abcd  P2 ○ open       task     Add formula browser                            —
   be-2345  P2 ○ open       task     Cache eldoc negatives                          —
   ...
────────────────────────────────────────────────────────────────────────────────────────────
  / filter (active) · x clear · g refresh · ? dispatch
```

### 4f. Loading / error

```
Beads list — beads.el          [42 issues · filter: none]                            (g refresh)
────────────────────────────────────────────────────────────────────────────────────────────
   ID       P  Status       Type     Title
   ⠋ loading…  bd list --status open --json
────────────────────────────────────────────────────────────────────────────────────────────
```

```
Beads list — beads.el          [42 issues · filter: none]                            (g refresh)
────────────────────────────────────────────────────────────────────────────────────────────
   ID       P  Status       Type     Title
   ⚠ beads-command-error: bd list exited 1: database is locked     (g retry)
────────────────────────────────────────────────────────────────────────────────────────────
```

Rendering rules: the previous good data is retained behind the loading/error
overlay (`beads-dashboard--last-good-data` pattern) so `g` never flashes an
empty buffer.

### 4g. Filter transient (`/`, the retained per-list generated surface)

```
Beads list filter — beads.el                                              (q back)
──────────────────────────────────────────────────────────────────────────────────────
  -s  Status        [open, in_progress, blocked, closed]
  -t  Type          [task, bug, feature, epic, …]
  -p  Priority      [0..4]
  -l  Limit         integer
  -o  Sort          [created, updated, priority]
  -a  Assignee
  L   Labels
──────────────────────────────────────────────────────────────────────────────────────
  a Apply filters   x Clear   q Back
```

Rendering rules: rows are things *without* stamping; `m` marks for multi-issue
actions (`beads-list--marked-issues`); marked actions apply to the marked set
or to the row at point (REQ-007). Section headers are `beads-section` things,
so TAB/S-TAB stop on them.

---

## 5. Detail redesign (`beads-show`)

vui/`beads-section-mode`, sectioned, with an action bar. Collapse per section,
persistent per store. Breadcrumb back to the originating list.

### 5a. Normal

```
be-mv8d — Refine beads.el UI redesign plan                        (q bury · g refresh · ? dispatch)
────────────────────────────────────────────────────────────────────────────────────────────
  Status ◐ in_progress    Priority P2    Type task    Labels design, plan
  Owner  gc.design-author  Created 2h ago  Updated 1m ago  ID be-mv8d (w copy)
  Store  beads.el · .beads/embeddeddolt

▾ Description                                                              (SPC fold)
    Refine the beads.el UI redesign plan — deeper design + thorough multi-state
    mockups. Fold F2 (M-x beads = status buffer; beads-dispatch on ?) and F3
    (remove QA/Custom entirely). PLANNING ONLY. …

▾ Dependencies (1)
    ⛔ be-9999  P2  Waiting on be-mv8d                (blocks)   ← blocker of 1
▾ Sub-issues (0)
▾ Agent (1)
    🦅 design-author · claude-code · running · session ec-rq8an    (RET attach · j jump)
▾ Comments (0)                                        (c comment)
▾ History (3)
    2h   created by mayor
    2h   assigned gc.design-author
    1m   status open → in_progress

────────────────────────────────────────────────────────────────────────────────────────────
 d close · C claim · s status · # priority · e edit · a agent · S sling · c comment · w copy id
```

### 5b. Narrow window (agent line wraps, action bar shrinks)

```
be-mv8d — Refine beads.el UI redesign plan      (q bury · g refresh)
──────────────────────────────────────────────────────
  ◐ in_progress  P2  task  design, plan
  gc.design-author · be-mv8d (w copy)
  Store beads.el

▾ Description                              (SPC fold)
    Refine the beads.el UI redesign plan …

▾ Dependencies (1)
    ⛔ be-9999  Waiting on be-mv8d  ← blocker
▾ Agent (1)
    🦅 design-author · running
       (RET attach)
▾ History (3)
    2h  created by mayor
──────────────────────────────────────────────────────
 d close · C claim · S sling · ? dispatch
```

### 5c. No comments / no dependencies (empty sections render a line, or fold away)

When a section is empty it may render its empty line once (so the user knows
it was checked) or be omitted per `beads-dashboard-default-collapsed`; the
default is to render the count and collapse:

```
▸ Sub-issues (0)
▸ Comments (0)
```

### 5d. Loading / error

```
be-mv8d — Refine beads.el UI redesign plan      (q bury · g refresh)
────────────────────────────────────────────────────────────────────────────
  Status ◐ in_progress    Priority P2    Type task    Labels design, plan
  Owner  gc.design-author  ID be-mv8d

▾ Description                                                              (SPC fold)
    ⠋ loading — bd show be-mv8d --json
▾ Dependencies (…)
    ⠋ loading
▾ History (…)
    ⚠ failed: bd history exited 1      (g retry)
```

### 5e. Action bar / help

`?` opens the dispatch menu with a `Context` group identical to the list's
(§2a). The visible action bar advertises the same keys; there is no separate
actions menu (C5 in `slimming.md`).

Rendering rules: every section is a `beads-section-spec`; `RET` on a
dependency/sub-issue opens that bead; `RET` on the agent row attaches
(`beads-terminal-attach`). The action bar is populated from
`beads-action-providers`.

---

## 6. Sling flow (standalone)

Adaptive transient (REQ-009). Stages are stacked groups; an answered stage
collapses to one line while its key still changes it. Shape is inferred and
shown as one sentence; only the plain shape shows routing flags. The Who
picker is agent-centric; standalone it lists local roles/backends/worktrees.

Reserved keys: `A f T c a n m t s P r g x q`.

### 6a. Fully pre-seeded plain dispatch (point on a bead)

```
Sling — beads.el
  Sling bead be-abcd to agent beads.el/task
  ✓ Ready — local route · target beads.el/task · worktree be-abcd

What
  A    Work: be-abcd — Add formula browser
  f    Formula: (none — f to pick)

Who
  T    Target: beads.el/task · local · available          (derived for this rig)

Routing flags
  -w   Use worktree: be-abcd          -b   Branch: be-abcd
  -n   Nudge target after launch

Actions
  s    Launch            P    Full preview
  x    Reset             q    Quit
```

### 6b. Cold entry

```
Sling — beads.el
  Sling (no work — A or point at a bead) to (no target — T or default)
  ⚠ No work chosen — pick work before launching

What
  A    Work: (none — A to pick an open bead)
  f    Formula: (none — f to pick)

Who
  T    Target: (no default derivable — T to choose)

Actions
  s    Launch            P    Full preview
  x    Reset             q    Quit
```

### 6c. Formula shape (`pancakes`, no work)

```
Sling — beads.el
  Run pancakes (formula) locally
  ✓ Ready — formula run · target local · 0 vars

What
  A    Work: (none — a formula run needs no work)
  f    Formula: pancakes — Make pancakes from scratch

Who
  T    Target: (local — formula runs in this repo)

Actions
  s    Launch            P    Full preview
  r    Recipe preview    q    Quit
```

### 6d. Targeted formula shape with typed How (build-basic `--on be-abcd`)

```
Sling — beads.el
  Run build-basic against bead be-abcd, drained by beads.el/task
  ✓ Ready — on run · target local · 4 of 18 vars set

What
  A    Work: be-abcd — Add formula browser
  f    Formula: build-basic — Run the default full-lifecycle build

How — build-basic vars
  ar   artifact_root (required) — Build artifact root.
         = plans/add-formula-browser/            [dir]
  cp   context_path — Optional source context bundle path.
         = (unset)                               [file]
  im   implementation_target — Role target for implementation work.
         = beads.el/task                         [agent]
  mi   max_iterations — Maximum fix attempts.
         = 10                                    [numeric]
  …    (remaining vars, same shape)

Who
  T    Target: beads.el/task · local · derived

Actions
  s    Launch            P    Full preview
  r    Recipe preview    q    Quit
```

### 6e. Freeform work (folded Custom, F3)

Custom's freeform prompt is now the **work-picker freeform escape**: a prefix
arg on `A` (or RET on empty input) enters free text, which becomes the
dispatch payload with no bead.

```
Sling — beads.el
  Sling freeform text to agent beads.el/task
  ✓ Ready — local route · target beads.el/task · freeform work

What
  A    Work: "summarise the open blockers in the plan"
  f    Formula: (none — f to pick)

Who
  T    Target: beads.el/task · local · available

Actions
  s    Launch            P    Full preview
```

### 6f. Live footer validation states

```
  ✓ Ready — on run · target local · 4 of 18 vars set
  ⚠ Missing required vars: artifact_root
  ⚠ build-basic drains a bead — pick work with A (or point at one)
  ⚠ cross-store route: bead hw-ab12 lives in hello-world but the target reads
    gascity.el — gc will refuse (pick a city agent)
  ⚠ v2 formula binding-qualified `run_targets` fails on a city-scoped target
    (bl-bdj) — pick a rig-scoped agent
```

### 6g. Who picker (minibuffer) — standalone vs. with gascity

```
Target: beads.el/task
  beads.el/task            local · role · available
  beads.el/review          local · role · available
  beads.el/plan            local · role · available
  worktree: be-abcd        local · worktree (branch be-abcd)
  worktree: be-2345        local · worktree

# with gascity.el present, the same picker adds:
  mayor                                    city · active
  hello-world/gc.implementation-worker     hello-world · derived default
  hello-world/gc.run-operator              hello-world · stopped
```

Rendering rules: the transient never renders content beyond one-line stage
answers. `P` opens the preview (§7), which *does* render content. Type
metadata (`[dir] [file] [agent] [numeric] [choice] [bool]`) comes from
`beads-formula-var`; an unrecognised var fails soft to string entry.

---

## 7. Sling full preview (`P`)

`special-mode` buffer; launchable in place (`s`), so preview never gates
launch.

### 7a. Standalone (local `bd` ironing)

```
Sling preview — beads.el                                        (s launch · q quit)
──────────────────────────────────────────────────────────────────────────────────────
  Run build-basic against bead be-mv8d, drained by beads.el/task

Validation
  ✓ target beads.el/task is available
  ✓ no cross-store route
  ✓ required vars set: artifact_root

Recipe — build-basic (steps → needs)
  prepare               needs artifact_root
  requirements          needs prepare
  plan                  needs requirements
  plan-review           needs plan
  decompose             needs plan-review
  implement             needs decompose
  review                needs implement
  finalize              needs review

Routing plan (dry run)
  Work:   be-mv8d — "Refine beads.el UI redesign plan" · open
  Target: beads.el/task
  Vars:   artifact_root=plans/beads-ui-redesign/ max_iterations=10
──────────────────────────────────────────────────────────────────────────────────────
```

### 7b. With gascity.el (the same section becomes `gc sling … --dry-run`)

```
Sling preview — beads.el                                        (s launch · q quit)
──────────────────────────────────────────────────────────────────────────────────────
  Run build-basic against bead hw-ab12, drained by hello-world/gc.run-operator

Validation
  ✓ target hello-world/gc.run-operator is available
  ✓ no cross-store route
  ✓ required vars set: artifact_root

Recipe — build-basic (steps → needs)
  prepare → requirements → plan → plan-review → decompose → implement → review → finalize

Routing plan (gc sling --dry-run)
  work:   hw-ab12
  target: hello-world/gc.run-operator
  formula: build-basic --on hw-ab12
  vars:   artifact_root=plans/hw/ max_iterations=10
──────────────────────────────────────────────────────────────────────────────────────
```

Rendering rules: the preview opens in a reused buffer, never mutates the
transient; `s` launches from either place.

---

## 8. Agent-launch flow

Distinct from sling: this is the direct local start (REQ-011). A hand-built
transient with a role/target/backend/prompt model, a live footer, and a
prompt preview. Sessions report into the agent list (§9).

### 8a. Default (Task, claude-code, worktree derived)

```
Start agent — be-abcd (Add formula browser)                                 (q quit)
  ✓ Ready — Task · worktree be-abcd · backend claude-code

Role
  t    Task (default)     r    Review (incl. QA mode)     p    Plan

Target
  w    Worktree: be-abcd · branch be-abcd · new          (T choose existing)

Backend
  b    Backend: claude-code (preferred)                  (… other: agent-shell,
                                                            eca, claudemacs,
                                                            claude-code-ide)

Prompt
  e    Edit user prompt                                  (v preview system+user)

Actions
  s    Start              P    Preview prompt            x    Reset      q Quit
```

### 8b. Backend overflow (the demoted backends)

```
Backend — select
  claude-code        preferred · available
  agent-shell        available
  terminal           available
  ── other ──
  claude-code-ide    registered · niche
  claudemacs         registered · niche
  eca                registered · niche
  mock               test-only (hidden)
```

### 8c. QA mode (Review + QA prompt; no QA role)

```
Start agent — be-abcd (Add formula browser)                                 (q quit)
  ✓ Ready — Review (QA mode) · worktree be-abcd · backend claude-code

Role
  t    Task                r    Review (QA mode ON)      p    Plan
  q    toggle QA mode      (uses the QA testing prompt under Review)

Target
  w    Worktree: be-abcd · branch be-abcd · new

Backend
  b    Backend: claude-code (preferred)

Prompt
  e    Edit user prompt                                  (v preview system+user)

Actions
  s    Start              P    Preview prompt            x    Reset      q Quit
```

Registration note: `beads-agent-start-qa` is deleted; for one release it is a
`defalias` to a Review start with QA mode on (F3 migration).

### 8d. Prompt preview (system + user)

```
Agent prompt preview — Task · be-abcd                            (s start · q back)
──────────────────────────────────────────────────────────────────────────────────────
 System (role)
   You are a task-completion agent for beads. … (Task system prompt)

 User (issue envelope)
   be-abcd: Add formula browser

   <ISSUE-DESCRIPTION>

   # Output
   When work is complete, close the issue with a clear summary: …
──────────────────────────────────────────────────────────────────────────────────────
```

### 8e. Session lifecycle

```
start ─▶ running ─┬─▶ idle ──▶ (RET attach) ──▶ running
                  ├─▶ stopped        (x stop)
                  └─▶ failed         (g retry start)
```

Each transition runs `beads-agent-state-change-hook`; the sessions list and
the status buffer's `Agents` line update without a manual refresh.

Rendering rules: role keys are `t`/`r`/`p` only — the roster is slimmed (F3);
QA is the Review role with a QA mode; Custom is reached through the sling
freeform path (§6e). Backends beyond the curated set are behind `… other` but
remain registered. The live footer mirrors the sling footer.

---

## 9. Agent/session list (`beads-agent-list`)

`tabulated-list-mode`; homogeneous. `RET` attaches, `j` jumps to buffer,
`x` stops.

### 9a. Populated

```
Sessions — beads.el                                       (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────────────
  Issue     Role    Backend       Status    Duration   Worktree
  be-mv8d   Task    claude-code   running   12m        be-mv8d
  be-1234   Review  agent-shell   idle      2m         be-1234
  be-2345   Plan    claude-code   failed    1m         be-2345
──────────────────────────────────────────────────────────────────────────────────────
  RET attach · j jump · x stop · X stop all · r restart · d dired · q bury
```

### 9b. Empty

```
Sessions — beads.el                                       (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────────────
  Issue     Role    Backend       Status    Duration   Worktree
    No agent sessions.
       a  start an agent at point   RET  start from a bead
──────────────────────────────────────────────────────────────────────────────────────
```

### 9c. Error

```
Sessions — beads.el                                       (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────────────
  Issue     Role    Backend       Status    Duration   Worktree
    ⚠ failed to read sessions: sesman error     (g retry)
──────────────────────────────────────────────────────────────────────────────────────
```

---

## 10. Formula browser and detail

### 10a. Formula list (tabulated, type-grouped)

```
Formulas — beads.el                                                  (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────────────────
  Name           Type        Steps  Vars  Description
▾ workflow
  build-basic    workflow    13     18    Run the default full-lifecycle build
  pancakes       workflow     3      0    Make pancakes from scratch
▾ expansion
  e2e-demo       expansion    5      2    End-to-end demo workflow
▾ aspect
  lint           aspect       2      1    Lint aspect
──────────────────────────────────────────────────────────────────────────────────────────
  RET inspect · l launch against bead · s launch standalone · c convert · S schema
```

Empty / error states are the shared four-state pattern (loading line, "No
formulas found", error line with `g` retry).

### 10b. Formula detail (vui)

```
build-basic — Run the default full-lifecycle build                    (q bury · g refresh)
──────────────────────────────────────────────────────────────────────────────────────────
  Type workflow · 13 steps · 18 vars · source .beads/formulas/build-basic.toml

▾ Vars (18)                                                          (SPC fold)
    artifact_root        required  dir     Build artifact root.
    context_path                   file    Optional source context bundle path.
    implementation_target          agent   Role target for implementation work.
    max_iterations                 numeric Maximum implementation/review fix attempts.
    interaction_mode               choice  interactive | autonomous | headless
    open_pr                        bool    Allow final PR creation.
    …  (12 more)

▾ Steps (13)
    prepare            needs artifact_root
    requirements       needs prepare
    plan               needs requirements
    …  (10 more)

▾ Source
    .beads/formulas/build-basic.toml                          (RET open · o other window)
──────────────────────────────────────────────────────────────────────────────────────────
  l launch against bead · s launch standalone · w copy name · q bury
```

### 10c. Formula follow (after launch)

```
Run pancakes — beads.el                                       (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────────────────
  Run id  run-7c2a · formula pancakes · status running · 0 vars
▾ Preflight (1)
    ✓ local prerequisites satisfied
▾ Steps (3)
  ✓ gather      done
  ◐ cook        running (1m)
  ○ serve       pending
▾ Output
    …
──────────────────────────────────────────────────────────────────────────────────────────
  RET attach · x stop · q bury
```

(With gascity.el the follow view is the `gc` run view contributed through the
seam; standalone it renders the local `bd` ironing progress.)

Rendering rules: `l` pre-seeds the sling formula stage with this formula and
prompts only for the work bead; `s` runs the untargeted formula shape. Var
rows carry the same typed metadata the How stage uses.

---

## 11. Terminal attach and scroll

Terminal attach moves into beads.el (REQ-015). Buffer name is host-qualified
per `beads-buffer.el`. The scroll sub-mode is the `DESIGN-agent-scrolling.md`
design, now shipped in beads.

### 11a. Attached, scroll off

```
beads-agent[be-mv8d]/beads.el:design-author — claude-code (ghostel)   [scroll: off]
──────────────────────────────────────────────────────────────────────────────────────
  ┌─ agent TUI ──────────────────────────────────────────────────────────────────────┐
  │ > working on be-mv8d …                                                            │
  └──────────────────────────────────────────────────────────────────────────────────┘
──────────────────────────────────────────────────────────────────────────────────────
  C-c s toggle scroll · C-c b b bead at point · RET visit bead · q bury
```

### 11b. Scroll mode active (mode line gains `[scroll]`)

```
beads-agent[be-mv8d]/beads.el:design-author — claude-code (ghostel)   [scroll: on]
  C-p/C-n line · C-v/M-v page · M-</M-> top/bottom · q leave scroll mode
```

### 11c. Mouse wheel behavior per backend

| Backend | Wheel handling |
|---|---|
| ghostel / vterm | raw key sequence translated to the backend's scroll adapter (`beads-terminal-tmux--send-raw`); pane stays in copy-mode buffer |
| term / ansi-term | tmux copy-mode entered on wheel-up, left on wheel-down; `WheelDownPane` fragment armed on attach |
| reporting backends | native Emacs scroll of the transcript; no tmux round trip |

Rendering rules: `C-c b` is the reserved extension prefix; `C-c b b` is the
canonical "bead at point" (the old `C-c b` is kept as an alias for one
release). All tmux side effects ride the attach pre-step's single host round
trip; no sync call at render time.

### 11d. Remote (TRAMP) attach

```
beads-agent[be-mv8d]/ssh:localhost:~/bright-lights:design-author — claude-code (ghostel)
──────────────────────────────────────────────────────────────────────────────────────
  (terminal spawns a LOCAL `ssh -T` pipe; pty content streams back)
──────────────────────────────────────────────────────────────────────────────────────
  C-c s scroll · q bury
```

---

## 12. Key-flow traces

Concrete traces showing where each key goes, for the acceptance pass.

```
T1  Entry:        M-x beads                      → beads-status (board)
                  ?                              → beads-dispatch
                  l                              → beads-list
                  / -s open RET a                → list filtered to open
                  RET (on be-abcd)               → beads-show be-abcd
                  a t                            → agent launch, Task
                  s                              → start; sessions list shows be-abcd
                  RET (sessions)                 → attach (beads-terminal-attach)

T2  Sling local:  M-x beads → RET be-abcd → S     → sling (plain preseeded)
                  s                              → launch via default method
                  ; or A (prefix) "free text" s  → sling freeform (folded Custom)

T3  Sling city:   (gascity loaded) be-abcd → S → T → hello-world/gc.run-operator
                  s                              → gc sling backend
                  P → s                          → preview then launch

T4  Formula:      M-x beads → F                   → formula-list
                  RET build-basic                → formula-detail
                  l → A be-abcd → s              → on-run launch → follow view

T5  Terminal:     sessions → RET be-mv8d          → attach
                  C-c s                          → scroll mode on
                  (wheel)                        → transcript/copy-mode scroll
                  q                              → leave scroll mode
                  q                              → bury

T6  Extension:    C-c b c                         → gascity city status
                  C-c b b                         → bead at point (canonical)
```

---

## 13. Faces and glyph reference (REQ-017)

| Face | Used for | Default |
|---|---|---|
| `beads-face-header` | buffer/section titles | `bold`, foreground |
| `beads-face-section` | section header line | `bold` + accent |
| `beads-face-issue-line` | issue rows | default |
| `beads-face-id` | bead ids | `fixed-pitch`, accent |
| `beads-face-key` | key hints | `shadow` |
| `beads-face-status-open` | `○` | green-ish |
| `beads-face-status-in-progress` | `◐` | yellow |
| `beads-face-status-blocked` | `⛔` | red |
| `beads-face-status-closed` | `✓` | `shadow` |
| `beads-face-priority-critical…low` | `P0…P4` | red → dim |
| `beads-face-agent-running/idle/failed` | agent state glyphs | green/grey/red |
| `beads-face-success` / `beads-face-warning` / `beads-face-error` | footers, validation | — |

Extensions derive, e.g.
`(defface my-city-face ((t (:inherit beads-face-header))))`.
No face hook: names are the contract.

---

## Key summary (all redesigned surfaces)

```
Global      q bury · g refresh · C-u g hard refresh · TAB/S-TAB move ·
            SPC toggle · RET visit · ? dispatch · C-c b extension prefix
Status/list S sling · a agent · c create · / filter · m mark
Detail      d close · C claim · s status · # priority · e edit · c comment
Sling       A work · f formula · T target · s launch · P preview · r recipe · x reset
Agent       t/r/p role · q QA-mode toggle (Review) · w worktree · b backend ·
            e prompt · s start · P preview
Formula     RET inspect · l launch-on-bead · s standalone · c convert · S schema
Terminal    C-c s scroll · C-c b b bead-at-point · q bury
Freed keys  a q, a c (F3; reserved, not reused by this plan)
```
