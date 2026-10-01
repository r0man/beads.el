---
schema: beads.ui-redesign.mockups.v1
workflow:
  id: be-59fe
artifact: menu-mockups
status: draft-for-review
scope: planning-only
---

# beads.el UI Redesign — ASCII Menu Mockups

ASCII renderings of every redesigned user-facing element, for review before
implementation. Real rendering rules (groups, collapse, keys) follow each
figure. Data is representative of a real beads.el store (the `beads.el`
project; bead `be-59fe` is this task). No "TBD" placeholders.

**Global rendering rules** (apply to every figure unless overridden):

- Buffers are **hand-built**, not transient-rendered, except the two
  dispatch/sling transients, which render *only* arguments/actions and never
  content.
- `q` buries, `g` refreshes in place, `C-u g` hard-refreshes, `TAB`/`S-TAB`
  move by `beads-thing`, `SPC` toggles the thing/section at point, `RET`
  visits/activates, `?` opens the dispatch menu.
- Sections render as `▾ Title (n)` expanded / `▸ Title (n)` collapsed;
  collapse is persistent per store.
- Status glyphs: `○` open, `◐` in-progress, `⛔` blocked, `✓` closed.
  Priority: `P0…P4` (P0 red). Agent state: `🦅` running, `…` idle, `✗` failed
  (icons optional; letters `T/R/P` fall back).
- The mode line carries `[store] · counts · filter · agent-state` in every
  porcelain view.
- Extension keys live under the `C-c b` prefix, shown in `?` only.

Reserved single-letter keys in beads buffers:
`q g TAB S-TAB SPC RET ? n p N P` and the action keys
`d C s # a S e m /` (context-dependent; documented per view).

---

## 1. Entry — the status buffer (`M-x beads`)

The Magit-like front door. vui; each section loads asynchronously. `RET`
visits, `TAB`/`SPC` folds a section, `g` refreshes.

```
Beads — beads.el                                          (g refresh · ? help · q bury)
──────────────────────────────────────────────────────────────────────────────────────
  Store    .beads/embeddeddolt · prefix be- · 42 issues · bd 1.0.5
  Repo     /home/roman/workspace/beads.el · branch main · clean
  Agents   2 running · 1 failed                            (a agents · A attach)
  Remote   local

▾ In progress (2)                                                   (SPC fold)
    ◐ be-59fe  P2  Redesign beads.el UI (Magit-style, world-class) — PLAN   @gc.design-author  2m
    ◐ be-1234  P1  Fix list refresh race                                   @you                1h
▾ Ready (5)
    ○ be-abcd  P2  Add formula browser                                     —
    ○ be-ef01  P3  Graph layout for epics                                  —
    ○ be-2345  P2  Cache eldoc negatives                                   —
▾ Blocked (1)
    ⛔ be-9999  P2  Waiting on be-59fe                                      ← be-59fe
▾ Recent activity
    2m   be-1234  status open → in_progress
    1h   be-59fe  created by mayor
    3h   be-abcd  priority P3 → P2

──────────────────────────────────────────────────────────────────────────────────────
 s sling  a agent  c create  l list  / search  ! maintenance  ? dispatch  q bury
```

Rendering rules: sections are `beads-section-register` entries; each loader
runs through `beads-command-execute-async`; the footer line is the mode
line's `help-echo` summary and mirrors the `?` dispatch. `M-x beads` opens
this buffer (REQ-001); the transient is demoted to `?` (REQ-003).

---

## 2. The dispatch menu (`?`)

Hand-built transient; argument/dispatch only, never content (REQ-004).
Grouped by frequency; every command has a description. Downstream groups are
appended via `beads-menu-providers` (the `City` group appears only when
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
  S  Slip… (sling)         a  Agents…               F  Formulas…         v  Graph
  D  Dashboard             H  History               Y  Orphans           t  Stats
 Manage
  L  Labels…               k  Dolt…                 .  Config            W  Worktrees…
 Maintenance
  !  Maintenance…          >  Advanced…             g  Refresh cache     q  Quit
 [City]                                                    (only with gascity.el)
  C-c b c  City status     C-c b r  Rig list        C-c b s  gc sling
──────────────────────────────────────────────────────────────────────────────────────
```

Rendering rules: `Maintenance…` opens figure §3; `Advanced…` opens the merged
maintenance menu's second page; the `[City]` group is contributed by gascity
via `beads-menu-providers` and is absent when gascity.el is not loaded. This
menu is the only place the demoted generated transients are reachable (as
`i Show…`, `L Labels…`, etc. sub-dispatch).

---

## 3. Maintenance menu (`!`)

The single collapsed successor of `beads-ops-menu` + `beads-advanced-menu`
(REQ-023). Everything kept earns its place (see `slimming.md`); everything
else is deleted.

```
Beads maintenance — beads.el                                               (q quit)
──────────────────────────────────────────────────────────────────────────────────────
 Database
  B  Backup                E  Export JSONL           P  Prune closed      G  GC
  b  Batch ops             c  Compact                C  Compact commits   F  Flatten
  i  Ping                  +  Doctor                 M  Migrate…
 Structure
  1  Duplicate kind        2  Find duplicates        3  Supersede         4  Restore
  q  Query (SQL)           R  Rename prefix          _  Forget memory
 Integrations
  j  Jira                  N  Linear                G  GitHub            y  GitLab
  A  ADO                   *  Mail delegate
 Setup
  I  Init project          S  Init safety            O  Bootstrap         h  Hooks…
  K  KV store              |  Memories
 ────────────────────────────────────────────────────────────────────────────────────
 Page 1/2   (C-n next page · C-p previous · q quit)
```

Rendering rules: paginated groups (`C-n`/`C-p`) rather than ten stacked
groups; this replaces the ~80-entry `beads-more-menu`. A command that is a
core action (close, claim, status, priority) lives in the status/list/detail
context keys, not here.

---

## 4. List redesign

`tabulated-list-mode` with `beads-thing` marking and vui-free sections.
Sections group by status (or by the active filter). Paging via `beads-pager`.
A header line summarises counts and filter. `/` opens the retained filter
transient (per-list filters: the one allowed generated surface).

```
Beads list — beads.el          [42 issues · ready 5 · in-progress 2 · filter: none]   (g refresh)
────────────────────────────────────────────────────────────────────────────────────────────
   ID       P  Status       Type     Title                                          Agent
▾ Ready (5)
   be-abcd  P2 ○ open       task     Add formula browser                            —
   be-ef01  P3 ○ open       bug      Graph layout for epics                         —
   be-2345  P2 ○ open       task     Cache eldoc negatives                          —
▾ In progress (2)
   be-59fe  P2 ◐ in_progress task    Redesign beads.el UI (Magit-style, world-…     🦅 design
   be-1234  P1 ◐ in_progress bug     Fix list refresh race                          🦅 you
▾ Blocked (1)
   be-9999  P2 ⛔ blocked     task     Waiting on be-59fe                             —
────────────────────────────────────────────────────────────────────────────────────────────
  RET visit · SPC fold · m mark · d close · C claim · s status · # priority · a agent · S sling · / filter
```

Filter transient (`/`, the retained per-list generated surface):

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

Rendering rules: rows are things *without* stamping (per `beads-thing`;
tabulated rows are things by position). `m` sets the mark for multi-issue
actions (`beads-list--marked-issues`). Marked actions apply to the marked set
or to the row at point (REQ-007). The section headers are `beads-section`
things, so TAB/S-TAB stop on them.

---

## 5. Detail redesign (`beads-show`)

vui/`beads-section-mode`, sectioned, with an action bar. Collapse per
section, persistent per store. Breadcrumb back to the originating list.

```
be-59fe — Redesign beads.el UI (Magit-style, world-class) — PLAN            (q bury · g refresh)
────────────────────────────────────────────────────────────────────────────────────────────
  Status ◐ in_progress    Priority P2    Type task    Labels design, plan
  Owner  gc.design-author  Created 2h ago  Updated 1m ago  ID be-59fe (w copy)
  Store  beads.el · .beads/embeddeddolt

▾ Description                                                              (SPC fold)
    Plan the complete redesign of beads.el's Emacs user interface. Do not
    implement anything in this task — produce a proper plan with hand-designed,
    ascii/text UI mockups. …

▾ Dependencies (1)
    ⛔ be-9999  P2  Waiting on be-59fe                (blocks)   ← blocker of 1
▾ Sub-issues (0)
▾ Agent (1)
    🦅 design-author · claude-code · running · session ec-erfys    (RET attach · j jump)
▾ Comments (0)                                        (c comment)
▾ History (3)
    2h   created by mayor
    2h   assigned gc.design-author
    1m   status open → in_progress

────────────────────────────────────────────────────────────────────────────────────────────
 d close · C claim · s status · # priority · e edit · a agent · S sling · c comment · w copy id
```

Rendering rules: every section is a `beads-section-register` entry; `RET` on
a dependency/sub-issue opens that bead; `RET` on the agent row attaches
(§8). The action bar is populated from `beads-action-providers` so gascity
can add drain/nudge without editing the view.

---

## 6. Sling flow (standalone)

Adaptive transient (REQ-009). Stages are stacked groups; an answered stage
collapses to one line while its key still changes it. Shape is inferred and
shown as one sentence; only the plain shape shows routing flags. The Who
picker is agent-centric; standalone it lists local roles/backends/worktrees.

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

### 6e. Live footer validation states

```
  ✓ Ready — on run · target local · 4 of 18 vars set
  ⚠ Missing required vars: artifact_root
  ⚠ build-basic drains a bead — pick work with A (or point at one)
```

With gascity.el present, the Who picker (6f) additionally lists city/rig
agents and the footer names the `gc` backend:

```
  ✓ Ready — gc on run · target hello-world/gc.implementation-worker · 6 vars
  ⚠ cross-store route: bead hw-ab12 lives in the hello-world store but the
    target reads the gascity.el store — gc will refuse (pick a city agent)
```

### 6f. Who picker (minibuffer) — standalone vs. with gascity

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
answers (anti-pattern rule). `P` opens the preview (§7), which *does* render
content. Type metadata (`[dir] [file] [agent] [numeric] [choice] [bool]`)
comes from `beads-formula-var`; an unrecognised var fails soft to string
entry.

---

## 7. Sling full preview (`P`)

`special-mode` buffer; launchable in place (`s`), so preview never gates
launch.

```
Sling preview — beads.el                                        (s launch · q quit)
──────────────────────────────────────────────────────────────────────────────────────
  Run build-basic against bead be-abcd, drained by beads.el/task

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
  Work:   be-abcd — "Add formula browser" · open
  Target: beads.el/task
  Vars:   artifact_root=plans/add-formula-browser/ max_iterations=10
  (with gascity.el present this section is the `gc sling … --dry-run` output)
──────────────────────────────────────────────────────────────────────────────────────
```

---

## 8. Agent-launch flow

Distinct from sling: this is the direct local start (REQ-011). A hand-built
transient with a role/target/backend/prompt model, a live footer, and a
prompt preview. Sessions report into the agent list (§9).

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

Rendering rules: role keys are `t`/`r`/`p` only — the roster is slimmed
(REQ-012, `slimming.md`); QA is the Review role with a QA mode; Custom is
reached through the sling freeform path, not as a role. Backends beyond the
curated set are behind the `… other` overflow but remain registered. The live
footer mirrors the sling footer.

---

## 9. Agent/session list (`beads-agent-list`)

`tabulated-list-mode`; homogeneous. `RET` attaches, `j` jumps to buffer,
`x` stops.

```
Sessions — beads.el                                       (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────────────
  Issue     Role    Backend       Status    Duration   Worktree
  be-59fe   Task    claude-code   running   12m        be-59fe
  be-1234   Review  agent-shell   idle      2m         be-1234
  be-2345   Plan    claude-code   failed    1m         be-2345
──────────────────────────────────────────────────────────────────────────────────────
  RET attach · j jump · x stop · X stop all · r restart · d dired · q bury
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

Rendering rules: `l` pre-seeds the sling formula stage with this formula and
prompts only for the work bead; `s` runs the untargeted formula shape. Var
rows carry the same typed metadata the How stage uses.

---

## 11. Terminal attach and scroll

Terminal attach moves into beads.el (REQ-015). Buffer name is
host-qualified per `beads-buffer.el`. The scroll sub-mode is the
`DESIGN-agent-scrolling.md` design, now shipped in beads.

```
beads-agent[be-59fe]/beads.el:design-author — claude-code (ghostel)   [scroll: off]
──────────────────────────────────────────────────────────────────────────────────────
  ┌─ agent TUI ──────────────────────────────────────────────────────────────────────┐
  │ > working on be-59fe …                                                            │
  └──────────────────────────────────────────────────────────────────────────────────┘
──────────────────────────────────────────────────────────────────────────────────────
  C-c s toggle scroll · C-c b b bead at point · RET visit bead · q bury
```

Scroll mode active (mode line gains `[scroll]`):

```
beads-agent[be-59fe]/beads.el:design-author — claude-code (ghostel)   [scroll: on]
  C-p/C-n line · C-v/M-v page · M-</M-> top/bottom · q leave scroll mode
```

Rendering rules: `C-c b` is the reserved extension prefix; `C-c b b` is the
canonical "bead at point" (the old `C-c b` is kept as an alias for one
release). Mouse wheel scrolls the transcript on reporting backends and is
translated to tmux copy-mode on vterm/term. All tmux side effects ride the
attach pre-step's single host round trip; no sync call at render time.

---

## 12. Faces and glyph reference (REQ-017)

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

Extensions derive, e.g. `(defface my-city-face ((t (:inherit beads-face-header))))`.
No face hook: names are the contract.

---

## Key summary (all redesigned surfaces)

```
Global      q bury · g refresh · C-u g hard refresh · TAB/S-TAB move ·
            SPC toggle · RET visit · ? dispatch · C-c b extension prefix
Status/list S sling · a agent · c create · / filter · m mark
Detail      d close · C claim · s status · # priority · e edit · c comment
Sling       A work · f formula · T target · s launch · P preview · r recipe · x reset
Agent       t/r/p role · w worktree · b backend · e prompt · s start · P preview
Formula     RET inspect · l launch-on-bead · s standalone · c convert · S schema
Terminal    C-c s scroll · C-c b b bead-at-point · q bury
```
