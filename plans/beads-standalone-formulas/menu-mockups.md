---
schema: beads.standalone-formulas.menu-mockups.v1
artifact: menu-mockups
status: draft-for-review
scope: planning-only
implementation: out-of-scope-until-signoff
requirements: requirements.md
design: design.md
---

# beads.el Standalone-first Formula → Molecule Workflow — Menu Mockups

Every surface is rendered in the states that matter (loading, empty,
populated, folded, filtered, error, destructive-confirm). Glyphs, faces and
key text follow `design.md` §9–§10. No TBDs.

---

## 1. Formula browser (`beads-formula-browse`)

Columns: Name · Type · Steps · Vars · Phase · Source. Search-path shadowing
marks a shadowed same-name formula. Scope filter is `project` / `user` / `all`.

### 1a. Populated (scope: all, no filter)

```
Formulas — beads.el                                         (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────
  Name              Type       Steps Vars Phase  Source
▾ workflow
  build-basic       workflow   13    18   —      .beads/formulas/build-basic.toml
  pancakes          workflow    3     0   —      .beads/formulas/pancakes.toml
  release           workflow    6     1   vapor  ~/.beads/formulas/release.toml
▾ expansion
  e2e-demo          expansion   5     2   —      ~/.beads/formulas/e2e-demo.toml
▾ aspect
  lint              aspect      2     1   —      ~/.beads/formulas/lint.toml
──────────────────────────────────────────────────────────────────────────────
  RET inspect · s instantiate · c cook · e edit source · l sling · S schema
  / scope project|user|all (now: all) · t type filter
```

### 1b. Shadowed name (same formula in two scopes)

```
  Name              Type       Steps Vars Phase  Source
▾ workflow
  build-basic       workflow   13    18   —      .beads/formulas/build-basic.toml
  build-basic ⧉shad workflow   12    18   —      ~/.beads/formulas/build-basic.toml
```

`⧉shad` face = `beads-face-warning`. `RET` on the shadowed row says
"shadowed by <path>"; `d` opens the diff of the two sources.

### 1c. Type filter (t workflow)

```
  Type filter: workflow                                 (t clear · g refresh)
  Name              Type       Steps Vars Phase  Source
  build-basic       workflow   13    18   —      .beads/formulas/build-basic.toml
  pancakes          workflow    3     0   —      .beads/formulas/pancakes.toml
  release           workflow    6     1   vapor  ~/.beads/formulas/release.toml
```

### 1d. Loading / empty / error

```
Loading formulas…

No formulas found on the search path.
  Paths checked: .beads/formulas · <checkout>/.beads/formulas · ~/.beads/formulas
  Create one with M-x beads-formula-new.

Error: bd formula list failed (exit 1)
  <stderr tail>
  g retry
```

---

## 2. Formula detail (sectioned)

### 2a. Normal

```
build-basic — Run the default full-lifecycle build           (q bury · g refresh)
──────────────────────────────────────────────────────────────────────────────
  Type workflow · version 1 · 13 steps · 18 vars · phase persistent
  Source .beads/formulas/build-basic.toml            (RET open · o other window)

▾ Vars (18)                                                       (SPC fold)
    artifact_root        required  dir     Build artifact root.
    context_path                   file    Optional source context bundle path.
    implementation_target          agent   Role target for implementation work.
    max_iterations                 numeric Maximum implementation/review attempts.
    interaction_mode               enum    interactive | autonomous | headless
    open_pr                        bool    Allow final PR creation.
    …  (12 more)

▾ Steps (13)
    prepare            task      needs —
    requirements       task      needs prepare
    plan               task      needs requirements      gate —
    wait-for-ci        task      needs plan              gate ⚙ gh:run
    …  (9 more)

▾ Bond points (2)
    entry              before design    Attach setup work here
    release-after      after  tag       parallel

▾ Composition
    extends — · aspects lint, security-scan · expansions —

▾ Source
    .beads/formulas/build-basic.toml
──────────────────────────────────────────────────────────────────────────────
  s instantiate · c cook · e edit · C convert · l sling · S schema
```

### 2b. Vapor-phase formula (recommendation)

```
release — Standard release workflow                         (q bury · g refresh)
  Type workflow · 6 steps · 1 var · phase vapor ⚠ recommends wisp
  Source ~/.beads/formulas/release.toml
  …
  s instantiate (will default to wisp — pour warns)
```

### 2c. Empty steps / error

```
▾ Steps (0)
    No steps declared. This formula is a header only; cooking creates the
    root proto with no children.

Error: bd formula show failed (exit 1)
  formula not found: buidl-basic
  g retry · q bury
```

---

## 3. Cook preview (`beads-cook`)

`c` opens the cook transient; `P` previews. Compile keeps `{{vars}}`, runtime
substitutes. `--persist` writes; `--dry-run` never does.

### 3a. Transient

```
Cook — beads.el
  Formula: build-basic
  Mode: compile (keep {{vars}})        (m toggle compile|runtime)
  Persist: no                          (p toggle)
  Force: no (requires persist)         (f toggle)
  Prefix: (none)                       (x set)
  Vars: 0 set                          (v add key=value)

  P  Full preview (dry-run)     s  Cook
  q  Quit
```

### 3b. Compile preview (`P`, dry-run)

```
Cook preview — build-basic (compile, dry-run)                (q bury)
──────────────────────────────────────────────────────────────────────────────
  Would create proto mol-build-basic (+ 13 child steps)
  Root:  mol-build-basic [template]
  Steps:
    prepare         task     Implement {{context_path}}   needs —
    requirements    task     Requirements for {{...}}     needs prepare
    plan            task     Plan {{...}}                 needs requirements
    wait-for-ci     task     Wait for CI                  needs plan  [gate ⚙gh:run]
    … (9 more)
  Dependencies: 12 edges (11 blocks, 1 waits-for)
  Persist: no (preview only)
──────────────────────────────────────────────────────────────────────────────
  s Cook (write proto) · q bury
```

### 3c. Runtime preview (`m` runtime, `--var` values set)

```
Cook — build-basic (runtime)                                 (q bury)
  Vars: artifact_root=plans/x, implementation_target=beads.el/task, +2
  Mode: runtime (substitute vars)
  P  Full preview     s Cook
```

```
Cook preview — build-basic (runtime, dry-run)                (q bury)
  Would create proto mol-build-basic (+ 13 children)
    prepare         Implement plans/x
    requirements    Requirements for plans/x
    plan            Plan plans/x
    …
  Validation: all 18 vars resolved (required + defaults)
  s Persist proto · q bury
```

### 3d. Error (missing vars / cook failure)

```
⚠ Missing required vars: artifact_root, implementation_target
   Fill them with v, or switch to compile mode (m) to keep placeholders.
```

```
Error: bd cook failed (exit 1)
  formula build-basic: cycle in needs: plan -> prepare -> plan
  q bury
```

---

## 4. Instantiate flow — explicit pour vs wisp + typed vars

`s` opens the instantiate transient seeded with the recipe. Phase is an
explicit choice; the formula's `phase` supplies the recommended default.
Warnings recompute on every change; `s` blocks on hard errors.

### 4a. Pour chosen, persistent formula (all vars valid)

```
Instantiate — build-basic                                    (q quit · P preview)
──────────────────────────────────────────────────────────────────────────────
  Formula build-basic — Run the default full-lifecycle build
  ✓ Ready — pour (persistent) · 18/18 vars · unassigned

Phase
  p    Pour (persistent, liquid)     ◉ selected
  w    Wisp (ephemeral, vapor)       ○
  W    Wisp root only (no steps)     ○   (wisp only)

Vars — build-basic (18)
  ar   artifact_root (required) [dir]      = plans/x
  cp   context_path [file]                  = (unset)
  im   implementation_target (required) [agent] = beads.el/task
  mi   max_iterations [numeric]             = 10
  im   interaction_mode [enum: interactive|autonomous|headless] = autonomous
  op   open_pr [bool]                       = false
  …    (12 more; RET to edit, TAB between)

Assign
  a    Assignee: (none)              (agent/user; sets --assignee)
  f    --for: (none)                 (scope queries to an agent)

  s  Instantiate        P  Full preview        q  Quit
```

### 4b. Vapor formula, pour chosen (warns, pour allowed)

```
Instantiate — release                                        (q quit · P preview)
  Formula release — Standard release workflow (phase vapor ⚠)
  ⚠ Vapor-phase formula: wisp is recommended. Pour will warn:
    "formula release declares phase=vapor; pour creates persistent work."
Phase
  p    Pour (persistent, liquid)     ◉ selected  ⚠
  w    Wisp (ephemeral, vapor)       ○ recommended
  W    Wisp root only                ○
```

### 4c. Missing required var (blocks)

```
  ⚠ Missing required vars: artifact_root, implementation_target
  s  Instantiate (blocked — fill required vars)     P  Full preview
```

### 4d. Pattern / enum / numeric validation

```
  ar   artifact_root (required) [dir]      = plans/x
  ve   version (required) [pattern ^\d+\.\d+\.\d+$] = 1.0
       ⚠ must match ^\d+\.\d+\.\d+$ (e.g. 1.0.0)
  en   environment [enum: staging|production] = dev
       ⚠ not in enum: staging, production
  mi   max_iterations [numeric] = many
       ⚠ not a number
  s  Instantiate (blocked)
```

### 4e. Full preview (`P`)

```
Instantiate preview — build-basic (pour, dry-run)            (q bury)
──────────────────────────────────────────────────────────────────────────────
  Would create:
    mol-build-basic  (root, persistent)  assignee: (none)
    ├─ prepare          needs —
    ├─ requirements     needs prepare
    ├─ plan             needs requirements
    ├─ wait-for-ci      needs plan   [gate ⚙ gh:run release.yml, 30m]
    └─ … (9 more)
  Vars substituted: 18/18
  Phase: persistent (liquid)
  Command: bd mol pour build-basic \
    --var artifact_root=plans/x --var implementation_target=beads.el/task …
──────────────────────────────────────────────────────────────────────────────
  s Instantiate · q bury
```

### 4f. Wisp preview (root only)

```
Instantiate preview — patrol (wisp --root-only, dry-run)
  Would create: mol-patrol (root, Ephemeral=true; no child steps)
  Phase: vapor
  s Instantiate · q bury
```

### 4g. Success (follow into molecule view) / error

```
Instantiated build-basic — molecule mol-build-basic…          (q bury)
  Opening molecule view…
```

```
Error: bd mol pour failed (exit 1)
  no proto or formula named build-basi
  q bury · e edit formula in this flow
```

---

## 5. Molecule execution view (`beads-molecule-open`)

The heart of the standalone loop. Header = root + phase + assignee + progress;
Steps = the DAG with state; Gates = gated steps; Ready = only when toggled.

### 5a. Populated (fresh molecule)

```
mol-build-basic — Run the default full-lifecycle build        (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────
  ◆ persistent · assignee (none) · 0/13 done (0%) · rate — · ETA —
  Ready frontier: 0  ·  Blocked: 12  ·  Gated: 1

▾ Steps (13)                                                  (SPC fold · r ready)
  · prepare              task                          [ready]
  · requirements         task    needs prepare         [blocked]
  · plan                 task    needs requirements    [blocked]
  ⊘ wait-for-ci          task    needs plan            [blocked-gate ⚙ release.yml]
  · review               task    needs plan            [blocked]
  … (8 more)

▾ Gates (1)
  ⚙ gh:run release.yml  gate-be-9f21  open  timeout 30m  waiters 0

▾ Output
  (no closed steps yet)
──────────────────────────────────────────────────────────────────────────────
  c claim · x close · n next ready · C close-eligible root · r ready-only
  RET inspect · b bond · a hand off · G gates · g refresh · q bury
```

### 5b. Mid-flight (current step, some done)

```
mol-build-basic — Run the default full-lifecycle build        (g refresh · q bury)
  ◆ persistent · assignee beads.el/task · 4/13 done (31%) · rate 2.4/h · ETA 3.8h
  Ready frontier: 1  ·  Blocked: 7  ·  Gated: 1

▾ Steps (13)
  ✓ prepare              task    done        (closed 11:20, reason "Prepared")
  ✓ requirements         task    done        (closed 12:04)
  ✓ plan                 task    done        (closed 12:51)
  ◐ wait-for-ci          task    current     claimed by beads.el/task
  ○ review               task    ready       needs plan
  ⊘ deploy               task    blocked-gate ⚙ human gate-be-1130 (approved)
  … (7 more)
```

### 5c. Ready-only (`r`)

```
mol-build-basic — ready only                                  (g refresh · q bury)
  Ready frontier: 1  ·  non-ready steps folded

▾ Ready (1)
  ○ review               task    needs plan → plan done
  c claim · n next · r show all
```

### 5d. Gated emphasis (`bd ready --gated` tick found a closed gate)

```
  ⚑ 1 gate closed since last refresh — resume candidates:
    wait-for-ci   gate ⚙ release.yml closed ✓
    R  refresh now · G  open gate list
```

### 5e. All steps done → close-eligible root

```
mol-build-basic — Run the default full-lifecycle build        (g refresh · q bury)
  ◆ persistent · 13/13 done (100%) · complete — root still open
  C  Close eligible root (bd epic close-eligible)

▾ Steps (13)
  ✓ prepare … ✓ merge (all done)
```

`C` → preview:

```
Close-eligible preview                                        (C confirm · q bury)
  Would close: mol-build-basic (epic, all children closed)
  Command: bd epic close-eligible --dry-run
  C Confirm · q bury
```

### 5f. Large molecule (>100 steps) windowed

```
mol-hanoi — Tower of Hanoi                                 (g refresh · q bury)
  ◆ persistent · 342/1000 done (34%) · showing steps 342–391
  ] next range · [ prev range · a all (slow)
```

### 5g. Loading / empty / error

```
Loading molecule mol-build-basic…

No molecule found: mol-nope. Open with M-x beads-molecule and an explicit id.

Error: bd mol current failed (exit 1)
  unknown molecule mol-nope
  g retry · q bury
```

---

## 6. Work loop trace (keys only)

```
Key  Screen                                  Result
n    Steps, cursor lands on review [ready]   review highlighted
c    "Claim review? y-or-n"                  bd update review --claim
g    auto-refresh (after claim)              review → [current], 5/13
     (operator does the work in the terminal/editor)
x    "Close reason: Done in <commit>"        bd close review --reason …
g    auto-refresh (after close)              review → [done], 6/13
n    selects wait-for-ci [ready]             advance
...
C    all 13 done                             bd epic close-eligible
q    bury                                    root closed, buffer buried
```

No step is skipped: claim and close are separate explicit actions; the loop
never auto-closes.

---

## 7. Gates

### 7a. Gate list (open only)

```
Gates — beads.el                                             (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────
  Gate        Type     Await              Timeout  Waiters  Blocks
  gate-be-9f2 ⚙ gh:run release.yml        30m      1        wait-for-ci
  gate-be-113 ⚑ human  design sign-off    —        2        deploy
  gate-be-220 ⏱ timer  —                  24h      1        bake
  gate-be-331 ⇄ gh:pr  42                 —        1        land
──────────────────────────────────────────────────────────────────────────────
  RET detail · c create · C check · R resolve · w add-waiter · d discover
  a all · t type · g refresh · q bury
```

### 7b. Gate list (all, with closed)

```
  gate-be-9f2 ⚙ gh:run release.yml   30m  1  wait-for-ci   closed ✓
  gate-be-331 ⇄ gh:pr 42             —    1  land          escalated ▲
```

### 7c. Gate detail

```
gate-be-9f2 — gate                                    (q bury · g refresh)
──────────────────────────────────────────────────────────────────────────────
  Type gh:run · status open · await-id release.yml · repo (current)
  Created 10:12 · timeout 30m · expires 10:42
  Reason "Wait for release workflow"

▾ Blocks (1)
    wait-for-ci   mol-build-basic   [blocked-gate]
▾ Waiters (1)
    beads.el/task
▾ History (2)
    created · checked (no match yet)
──────────────────────────────────────────────────────────────────────────────
  C check (evaluate) · D discover run · R resolve · w add-waiter · q bury
```

### 7d. Create gate transient

```
Create gate                                                  (s create · q quit)
  Type: ⚑ human   (t cycle human|timer|gh:run|gh:pr)
  Blocks: (required)          (b pick issue)
  Await id: —                 (a set; gh:run/gh:pr)
  Timeout: —                  (T set; timer/gh:run)
  Repo: —                     (o set; OWNER/REPO)
  Reason: —                   (r set)
```

### 7e. Check result (dry-run and apply)

```
Gate check (dry-run)                                         (q bury)
  2 evaluated
    ✓ gate-be-220 timer: elapsed 24h > timeout → would close
    ✓ gate-be-9f2 gh:run: completed/success → would close
    · gate-be-113 human: manual only
  C apply · q bury
```

```
Gate check applied
  closed: gate-be-220, gate-be-9f2
  next: `bd ready --gated` will resume wait-for-ci and bake
```

### 7f. Resolve / add-waiter / discover

```
Resolve gate-be-113
  Reason: (required) Design approved 2026-10-07
  R confirm · q quit
```

```
Add waiter — gate-be-113
  Waiter: beads.el/task
  w add · q quit
```

```
Discover run ids (dry-run)                                   (q bury)
  gate-be-9f2 matched run 1234567 (branch main, +1m12s)
  D apply · b branch … · q bury
```

### 7g. Empty / error

```
No gates. Create one with c, or wait for a formula step with [steps.gate].

Error: bd gate list failed (exit 1)  <stderr>
  g retry · q bury
```

---

## 8. Wisps

### 8a. Wisp list

```
Wisps — beads.el                                             (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────
  Id              Type    Status       Started      Updated      Age
  mol-patrol-a1   patrol  in_progress  10:02        10:31        29m
  mol-health-b2   task    closed       09:00        09:12        1h22m ⚠old
  mol-diag-c3     task    open         08:40        08:40        1h52m ⚠old
──────────────────────────────────────────────────────────────────────────────
  RET open root · s squash · b burn · P purge · a all · t type · g · q
```

### 8b. Squash (with summary)

```
Squash mol-patrol-a1                                         (s squash · q quit)
  Children: 4 ephemeral steps → digest
  Keep children: no            (k toggle)
  Summary: (optional)          (m edit; else bd concatenates)
    "Patrol found 1 stale task and no regressions."
  s Squash · q quit
```

```
Squashed — digest be-d991 [done], 4 wisps promoted to persistent
  ⚠ mol-patrol-a1 is now ◆ persistent
```

### 8c. Burn (destructive confirm)

```
Burn mol-health-b2 ⚠                                          (B burn · q quit)
  This permanently deletes the wisp and all children. No digest.
  Dry-run preview:
    Would delete: mol-health-b2 + 3 children (ephemeral)
  Type the id to confirm: mol-health-b2
  B Burn · q quit
```

### 8d. Purge

```
Purge closed ephemeral beads                                  (P purge · q quit)
  Older than: 7d               (o set)
  Pattern:    *-wisp-*         (p set)
  Dry-run: 12 beads, 4 deps, 9 events would be deleted
  P Purge (--force) · q quit
```

### 8e. Empty / error

```
No wisps. Create one with M-x beads-mol -> w, or formula instantiate -> wisp.

Error: bd mol wisp list failed (exit 1)  <stderr>
  g retry · q bury
```

---

## 9. Bonding

### 9a. Bond transient

```
Bond (mol bond)                                              (P preview · s bond)
  A (source): build-basic            (A pick formula/proto/molecule)
  B (target): lint                   (B pick formula/proto/molecule)
  Type: sequential (default)         (t cycle sequential|parallel|conditional)
  Result name: compound-basic-lint    (a set; proto+proto only)
  Phase: follow target (default)     (p cycle follow|pour|ephemeral)
  Ref: arm-{{name}}                  (r set; dynamic child id)
  Vars: name=ace                      (v add key=value)

  P  Dry-run preview     s  Bond     q  Quit
```

### 9b. Dry-run preview

```
Bond preview — build-basic + lint (sequential, dry-run)
  proto + proto → compound proto mol-compound-basic-lint
    lint.before at bond point "entry" (before design), parallel=false
  B depends on A; A closes → B unblocks.
  s Bond · q bury
```

### 9c. Dynamic ref / phase override

```
Bond preview — arm + patrol (sequential, --ref arm-{{name}}, --var name=ace)
  Would create: mol-patrol.arm-ace (+ children .capture)
  Phase: ephemeral (--ephemeral): excluded from Dolt sync
  s Bond · q bury
```

### 9d. Bond entry from molecule view

```
mol-build-basic — …                                          (b bond · q bury)
  b opens Bond with A = mol-build-basic prefilled.
```

### 9e. Error

```
Error: bd mol bond failed (exit 1)
  cannot bond: both operands are persistent molecules with no --as
  q bury
```

---

## 10. Formula authoring

### 10a. New formula scaffold (`M-x beads-formula-new`)

```
New formula
  Name: my-workflow
  Type: workflow      (t cycle workflow|expansion|aspect)
  Scope: project      (s project|user)
  Path: .beads/formulas/my-workflow.formula.toml
  n create
```

```
.beads/formulas/my-workflow.formula.toml       (TOML · C-c C-c validate+save)
formula = "my-workflow"
description = ""
version = 1
type = "workflow"

[vars.feature_name]
description = "Name of the feature"
required = true

[[steps]]
id = "design"
title = "Design {{feature_name}}"
```

### 10b. Editing with schema completion

```
.formula/build-basic.formula.toml              (TOML · C-c C-c validate+save)
[[ste|]]
        ↑ company/completion from `bd formula schema`
          steps            struct StepSpec
          steps.gate       struct GateSpec {type,id,await_id,timeout,repo}
          steps.needs      []string
          phase            "persistent"|"vapor"
```

### 10c. Validation results (`C-c C-v`)

```
*beads-formula-validate*                                     (q bury)
──────────────────────────────────────────────────────────────────────────────
  build-basic.formula.toml — 1 diagnostic
  line 27: steps[3].needs references unknown step `desgin`
  ✓ vars: 18 · steps: 13 · dependencies: 12 (no cycles)
──────────────────────────────────────────────────────────────────────────────
  RET jump to diagnostic · C-c C-c save anyway (warns) · q bury
```

### 10d. Convert JSON→TOML

```
Convert formula                                               (C convert · q quit)
  Source: shiny.formula.json
  Output: shiny.formula.toml          (--stdout: no)
  Delete JSON after: no               (D toggle)
  C Convert · q quit
```

```
Converted shiny.formula.json → shiny.formula.toml (source preserved)
```

### 10e. Distill (from an epic)

```
Distill epic                                                  (s distill · q quit)
  Epic: be-abc123 — Dark-mode feature
  Formula name: dark-mode-workflow
  Output dir: .beads/formulas/         (o set)
  Var mappings:                        (v add value=variable)
    dark-mode-auth=feature_name
    design-doc=design_ref
  Dry-run preview:
    Would write dark-mode-workflow.formula.toml with 5 steps, 2 vars.
  s Distill · q quit
```

### 10f. Error

```
Error: validation failed — missing required field `formula`
  C-c C-c blocked · fix line 1 · q bury
```

---

## 11. Agent hand-off, prime and setup

### 11a. Hand-off transient (`a` in molecule/formula)

```
Hand off — mol-build-basic                                   (s start · q quit)
  Agent type: task            (t cycle task|review|plan|qa|custom)
  Backend: claude-code        (b pick registered backend)
  Worktree: derived be-xxxx   (w toggle)
  System prompt: task role    (S preview)
  User prompt:                (U preview)
    Molecule mol-build-basic (persistent, 13 steps, 4/13 done).
    Next ready: review. bd ready --mol mol-build-basic.
    Context: <bd prime excerpt — see 11b>
  Context: prime              (c toggle none|prime|memories)
  s Start agent · q quit
```

### 11b. `bd prime` context (`M-x beads-context`)

```
Context — beads.el                                          (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────
  Store: .beads  · branch main  · policy conservative
▾ Prime (bd prime)
    ## Beads workflow
    …  (1–2k tokens; SPC fold)
    [c copy · i insert into hand-off prompt]
▾ Memories (bd prime --memories-only)
    (none)
▾ Setup status
    claude  current   .claude/settings.json + CLAUDE.md
    codex   stale     .agents/skills/beads + AGENTS.md
    cursor  missing
    hooks   installed
──────────────────────────────────────────────────────────────────────────────
  c copy prime · i insert · r install/remove recipe · g refresh · q bury
```

### 11c. Setup install/remove

```
Setup recipe — codex                                         (r run · q quit)
  Status: stale (hash mismatch)
  Action: ⧉ update    (a cycle install|update|check|remove)
  Target: project     (t project|global)
  r Run · q quit

Setup recipe — cursor
  Status: missing
  Command: bd setup cursor
  r Run · q quit
```

Policy is displayed, never written silently; changing `agent.profile` is an
explicit `r` action with a confirmation and the exact `bd config set` shown.

### 11d. Error

```
Error: bd prime failed (exit 1)  <stderr>
  g retry · q bury
```

---

## 12. Key-flow traces

### 12a. Formula → pour → work → close (standalone)

```
M-x beads-formula-browse   → browser
RET build-basic            → detail (phase persistent)
s                          → instantiate (pour default)
RET ar artifact_root…      → edit dir var; im target
s                          → preview
s                          → instantiate → molecule view opens
n                          → next ready (prepare)
c                          → claim
x → "Prepared"             → close; refresh; 1/13
… repeat …
C                          → close-eligible preview → confirm → root closed
q                          → bury
```

### 12b. Formula vapor → wisp → squash

```
browser → release (phase vapor ⚠)
s → instantiate; wisp recommended (w) → preview → s
molecule view (◇ vapor) → work steps → all done
M-x beads-wisp-list → mol-release-a1
s → squash; m summary … → s → digest created, phase ◆ persistent
```

### 12c. Gate round-trip

```
molecule view → wait-for-ci [blocked-gate ⚙]
RET        → gate detail
C check    → dry-run (no match) → apply
… CI finishes …
C check    → would close → apply
g on molecule → "1 gate closed" banner → R refresh → wait-for-ci [ready]
```

### 12d. Distill → edit → instantiate

```
show epic be-abc → D distill
  formula dark-mode-workflow, vars, preview → s
formula browser → RET dark-mode-workflow → e edit source
C-c C-c validate+save
s → instantiate (pour) → molecule view
```

### 12e. Hand off

```
molecule view → a
  hand off transient; c context=prime; U preview user prompt
  s start → agent backend starts in a terminal, session tracked in beads-agent-list
```

### 12f. Swarm create → status → claim → close → complete

```
show epic be-abc → S create swarm
  swarm create transient: coordinator=beads.el/witness · P preview (validate)
  s create → "Created swarm mol-swarm-1 (+ linked to be-abc)" → swarm list
swarm list → RET mol-swarm-1
  status board: Ready 3 · Active 0 · Blocked 5 · 0/8 (0%) · headroom 3
A on step "api" → assign beads.el/task
n → next ready "ui" → c claim
… worker does the work …
x → "Done: API" → refresh → Completed 1/8 (12%)
W → worker lanes: beads.el/task ▰ saturated, beads.el/review ▱ idle
V → validate waves: W1 {api,ui}, W2 {merge} · max_parallelism 3
C → close-eligible root → confirm → swarm 8/8 (100%), root closed
```

---

## 13. Faces, glyphs and mode-line reference

### 13a. Glyphs

| Glyph | Meaning | Face |
|---|---|---|
| `✓` | `[done]` | `beads-face-status-closed` |
| `◐` | `[current]` / in_progress | `beads-face-status-in-progress` |
| `○` | `[ready]` | `beads-face-success` |
| `⊘` | `[blocked]` (dependency) | `beads-face-status-blocked` |
| `⊘g` | `[blocked-gate]` | `beads-face-status-blocked` + gate face |
| `·` | `[pending]` | `beads-face-issue-line` |
| `▲` | failed/escalated | `beads-face-error` |
| `◆` | persistent root | `beads-face-molecule-root` |
| `◇` | vapor root | `beads-face-molecule-root` (dimmed) |
| `⚑` `⏱` `⚙` `⇄` `◈` | human/timer/gh:run/gh:pr/bead gate | `beads-face-key` |
| `⧉shad` | shadowed formula | `beads-face-warning` |
| `🐝`/`S` | swarm molecule | `beads-face-swarm-coordinator` |
| `★` | coordinator set | `beads-face-swarm-coordinator` |
| `W<n>` | ready front / parallel wave | `beads-face-key` |
| `▰` / `▱` | saturated / idle worker slot | `beads-face-swarm-lane` |
| `⚠` | validate warning / orphan / cycle | `beads-face-warning` |

### 13b. New faces (derive with `:inherit`)

```
beads-face-molecule-root   :inherit beads-face-header :weight bold
beads-face-molecule-step   :inherit beads-face-issue-line
beads-face-gate            :inherit beads-face-key
beads-face-swarm-coordinator :inherit beads-face-header :weight bold
beads-face-swarm-lane      :inherit beads-face-issue-line
```

### 13c. Mode line

```
  beads-molecule  mol-build-basic  4/13 ●  beads-formula  release  vapor
  beads-swarm  mol-swarm-1  3 ready · 2 active · max 5 · headroom 3
```

### 13d. Mandatory per-surface states

Every surface above renders: **loading** (a `Loading …` line), **empty**
(an actionable empty line), **populated**, and **error** (message + `g`
retry). Folding adds a **folded** state and long lists a **windowed** state.
Destructive actions add a **confirm** state. This is the PR #67 four-state
contract, applied uniformly.

---

## 14. Swarm / coordination

A swarm is the coordination wrapper around an epic's DAG. `bd swarm status`
is *computed from beads*, so every swarm surface is a live projection: any
claim/assign/close changes it. All flows below work with `bd` alone; gascity
only enriches the worker lanes when present. Glyphs/faces follow §13 and
`design.md` §10.

### 14a. Swarm fleet list (populated)

```
Swarms — beads.el                                            (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────
  Swarm              Status     Progress      Active  Coordinator        Epic
▾ active
  🐝 mol-swarm-1     open       3/8 (38%)     2       beads.el/witness   be-abc123  Dark-mode feature
  🐝 mol-swarm-7     open       0/5 (0%)      0       —                  be-def456  Parser rewrite
▾ closed
  🐝 mol-swarm-2     closed     5/5 (100%)    0       beads.el/witness   be-999     DB cleanup
──────────────────────────────────────────────────────────────────────────────
  RET status · v validate · c create · W worker lanes · x swarm mol · E epic
  a all/active (now: active) · f filter · g refresh · q bury
```

`Active` is the `active_issues` count; `Progress` is
`completed_issues/total_issues` and `progress_percent`. `Coordinator` is the
swarm molecule's assignee (`—` when unset).

### 14b. Swarm fleet list — loading / empty / error / domain error

```
Loading swarms…
```

```
No swarms.
  Create one from an epic with c (or S on an issue/epic).
  Candidate epics: be-abc123 Dark-mode feature · be-def456 Parser rewrite
```

```
Error: bd swarm list failed (exit 1)
  database is locked
  g retry · q bury
```

Domain error is not possible on `list` (it always returns `{"swarms": []}`);
the domain cases below belong to `create`/`validate`.

### 14c. Swarm status board (populated)

```
mol-swarm-1 — Swarm: Dark-mode feature                      (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────
  🐝 swarm · epic be-abc123 · coordinator ★ beads.el/witness
  Progress: 3/8 (38%)  ·  Active 2  ·  Ready 1  ·  Blocked 2
  Headroom: max_parallelism 5 − active 2 = 3 idle slots            (V validate)

▾ Active (2)                                                    (SPC fold)
  ◐ api          task    beads.el/task    in_progress
  ◐ parser       task    beads.el/review  in_progress

▾ Ready (1)
  ○ ui           task    needs api, parser

▾ Blocked (2)
  ⊘ merge        task    blocked by ui, docs
  ⊘ docs         task    blocked by api

▾ Completed (3)
  ✓ spec         task    closed 09:12
  ✓ schema       task    closed 10:40
  ✓ fixtures     task    closed 11:05

▾ Worker lanes (2)                                              (W open lanes)
  ▰ beads.el/task   1 current (api)     · 0 other in-progress   active
  ▰ beads.el/review 1 current (parser)  · 0 other in-progress   active
──────────────────────────────────────────────────────────────────────────────
  RET detail · A assign · c claim · h hand off · x close · o reopen
  W worker lanes · V validate · C close-eligible · g refresh · q bury
```

The four groups are read straight from `completed`/`active`/`ready`/`blocked`
in the `bd swarm status --json` payload (`active` rows show their `assignee`,
`blocked` rows their `blocked_by`); nothing is recomputed client-side. A
`[blocked-gate]` step is annotated with its gate glyph and `RET` opens the gate
detail.

### 14d. Swarm status board — big epic / windowed

```
mol-swarm-42 — Swarm: Migrate 400 services                     (g refresh · q bury)
  🐝 swarm · epic be-big · coordinator ★ beads.el/witness
  Progress: 173/402 (43%)  ·  Active 12  ·  Ready 5  ·  Blocked 212
  Headroom: max_parallelism 20 − active 12 = 8 idle slots
▾ Active (12)  showing 12/12              ] more ready · [ prev · a all (slow)
  ◐ svc-173 … ◐ svc-184
▾ Ready (5)  showing 5/5
  ○ svc-201 … ○ svc-205
▾ Blocked (212)  showing 1–40 of 212     (] next 40 · [ prev 40)
▾ Completed (173)  showing 1–40 of 173    (] next 40 · [ prev 40)
```

Windowed groups page with `]`/`[` over the full payload; the header progress
is always the accurate `progress_percent`, never the visible window.

### 14e. Swarm status board — all complete

```
mol-swarm-2 — Swarm: DB cleanup                                (g refresh · q bury)
  🐝 swarm · epic be-999 · coordinator ★ beads.el/witness
  Progress: 5/5 (100%)  ·  Active 0  ·  Ready 0  ·  Blocked 0
  ✓ every child closed — root still open
  C  Close eligible root (bd epic close-eligible)
▾ Completed (5)
  ✓ drop-old-tables … ✓ vacuum
```

`C` opens the same close-eligible preview as the molecule view (§5e) and, on
confirm, closes the epic; the status board then reports `100%` and the swarm
molecule is offered for closure.

### 14f. Swarm validate — swarmable with warnings and waves

```
Validate — be-abc123 Dark-mode feature                        (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────
  ✓ Swarmable: YES  ·  issues 8 (3 closed)  ·  max parallelism 5
  Estimated worker-sessions: 6

▾ Ready fronts (waves of parallel work)
  W1  api, ui              (width 2)
  W2  merge, docs          (width 2)
  W3  release              (width 1)

▾ Warnings (2)                                        (RET jump to bead)
  ⚠ orphan: docs has no dependents
  ⚠ missing dependency: release should depend on merge

▾ Graph (--verbose)                                   (V show/hide)
  api      wave 1  depends_on —            depended_on_by ui, docs, merge
  ui       wave 1  depends_on api          depended_on_by merge
  merge    wave 2  depends_on api, ui      depended_on_by release
  release  wave 3  depends_on merge        depended_on_by —
──────────────────────────────────────────────────────────────────────────────
  RET wave/bead · c create swarm · V verbose · g refresh · q bury
```

Waves come from `ready_fronts[{wave, issues, titles}]`; the width and
`max_parallelism` are shown so the coordinator can size the worker pool.
`Estimated worker-sessions` is `estimated_sessions`. When `--verbose` is off,
the `Graph` section is hidden (this is the only `--verbose` toggle).

### 14g. Swarm validate — not swarmable (domain state)

```
Validate — be-bad000 Broken epic                              (g refresh · q bury)
  ✗ Swarmable: NO  ·  fix errors first

▾ Errors (2)
  • cycle: a -> b -> c -> a
  • disconnected subgraph: docs, extras

▾ Warnings (1)
  ⚠ orphan: extras has no dependents

▾ Ready fronts
  (none — the graph has a cycle)
  RET on an error jumps to the bead · E open the epic · g refresh · q bury
```

`bd swarm validate` exits **0** here and prints the analysis; the UI shows the
error group, does not create a swarm, and disables `c`. This is a domain
state, not a command error.

### 14h. Worker / parallelism lanes

```
Worker lanes — mol-swarm-1                                  (g refresh · q bury)
──────────────────────────────────────────────────────────────────────────────
  max_parallelism 5 · active 2 · ready 1 · idle slots 3

  ▰ beads.el/task     1 swarm step: api              · 0 other in-progress
  ▰ beads.el/review   1 swarm step: parser           · 0 other in-progress
  ▱ beads.el/qa       0 swarm steps                  · 2 other in-progress  idle-slot ⚠
  ▱ (unassigned)      —                              · ready: ui

▾ beads.el/qa — other in-progress
  ◐ be-77a  Polish error messages      (not a swarm step)
  ◐ be-78c  Flaky test triage          (not a swarm step)
──────────────────────────────────────────────────────────────────────────────
  RET open worker's in-progress bead · A assign a ready step · g refresh · q bury
```

A lane holding a `[current]` step is `active` (`▰`); a lane with in-progress
beads but no `[current]` swarm step is `idle-slot` (`▱` + ⚠); a worker holding
more than one `[current]` step is `over-committed` (`▲`). Headroom is
`max_parallelism − active` and is labelled `saturated` at zero. Standalone lane
data is `bd list --assignee <a> --status in_progress --json`; gascity overrides
`beads-swarm-worker-source` to add live session/pool state without changing the
layout.

### 14i. Swarm create + coordinator

Transient (`beads-swarm c` or `S` on an epic/issue):

```
Create swarm — be-abc123 Dark-mode feature                   (P preview · s create)
  Epic: be-abc123 (epic, 8 children)
  Coordinator: (none)              (c set; e.g. beads.el/witness)
  Force: no                        (f toggle; only if a swarm exists)
  P  Validate + preview (bd swarm validate)
  s  Create swarm
  q  Quit
```

Preview (`P`):

```
Create swarm preview — be-abc123                             (q bury)
  bd swarm create be-abc123 --coordinator beads.el/witness
  Validate: swarmable YES · 8 issues · max parallelism 5 · waves 3
  Would create: mol-swarm-… (mol_type=swarm) ↔ relates-to be-abc123
  s Create · q bury
```

Success and auto-wrap of a lone issue:

```
Created swarm mol-swarm-1 ↔ be-abc123 (coordinator ★ beads.el/witness)
```

```
Note: be-task-456 is not an epic — bd auto-wrapped it:
  created epic be-wrap-1 with be-task-456 as its only child
Created swarm mol-swarm-2 ↔ be-wrap-1
```

Already exists (domain error, `bd` exits 0):

```
⚠ Swarm already exists: mol-swarm-1 — Swarm: Dark-mode feature
  f  --force create another    RET  Open existing swarm    q  Quit
```

Not swarmable (domain error, delegates to validate §14g):

```
✗ Epic is not swarmable — fix errors first (see validate board)
  V  Open validate board    q  Quit
```

### 14j. Swarm step actions — assign / claim / hand off

```
Assign step — ui                                             (s assign · q quit)
  Step: ui   task   ready   needs api, parser
  Agent: beads.el/review      (pick from targets, or type a freeform user)
  Comment: (optional)         (m edit; added after assign)
  s Assign · q quit
```

```
Claim step — api                                            (c confirm · q quit)
  bd update api --claim
  Assignee will be you (beads.el/gc.design-author-1); status → in_progress.
  c Claim · q quit
```

```
Hand off — ui                                              (s start · q quit)
  Assign ui → beads.el/review with a comment, then launch the agent:
  Agent type: task           (t cycle task|review|plan|qa|custom)
  Backend: claude-code       (b pick registered backend)
  User prompt:               (U preview)
    Swarm step ui (task, ready) in mol-swarm-1.
    Needs api, parser (done). bd swarm status mol-swarm-1 for the board.
    Context: <bd prime excerpt>
  s Assign + start · A assign only · q quit
```

Close / reopen a step:

```
Close step — api                                            (x confirm · q quit)
  Reason: (required) API endpoints landed in 1f2a9c
  x Close · q quit
```

```
Reopen step — spec                                          (o confirm · q quit)
  bd update spec --status open   (assignee kept: beads.el/task)
  o Reopen · q quit
```

### 14k. Coordinator set / replace

```
Set coordinator — mol-swarm-1                               (C confirm · q quit)
  Current: ★ beads.el/witness
  New:     beads.el/observer/                (pick or type an address)
  bd update mol-swarm-1 --assignee beads.el/observer/
  C Confirm · q quit
```

### 14l. Navigation contract (epic ↔ swarm ↔ molecule ↔ worker)

```
swarm list ──RET──▶ status board ──RET──▶ issue detail (step)
    │                   │  A/c/h     └──▶ bd list (worker in-progress)
    ├─v─▶ validate/waves ──RET──▶ offending bead / wave members
    ├─x─▶ swarm molecule view (§5) ──RET──▶ step issue detail
    └─E─▶ epic show ──S──▶ create swarm ──▶ swarm list
```

Every edge uses the existing views — no swarm-private detail buffer: a step is
an issue (`beads-command-show`), the swarm molecule is a molecule (molecule
view §5), the epic is an epic (issue show/list), and the worker's beads are the
list view scoped by `--assignee --status in_progress`. `q` returns one level
up; `g` refreshes the current swarm surface.

### 14m. Key-flow trace (create → status → claim → complete)

```
S on epic be-abc123        → create transient; c coordinator; P preview; s create
swarm list                 → 🐝 mol-swarm-1 0/8 · active 0
RET                        → status board; V validate: waves W1{api,ui}, max 5
A on ui                    → assign beads.el/review
n on api → c claim         → api [current], active 1
W                          → lanes: beads.el/review ▰; unassigned ▱
… work … x "API done"      → api [done], 1/8 (12%); status recomputed
a on parser → hand off     → assign + agent start
… repeat until all done …
C                          → close-eligible root → confirm → 8/8 (100%), epic closed
```

### 14n. Swarm surface states

| Surface | Loading | Empty | Error | Domain error | Big | Complete |
|---|---|---|---|---|---|---|
| Fleet list (§14a) | `Loading swarms…` | no swarms + candidate epics | command error + `g` | n/a | windowed rows | closed group |
| Status board (§14c) | per-section loading | not swarmable / no children | command error + `g` | covered by validate | §14d | §14e |
| Validate (§14f) | `Validating…` | no issues | command error + `g` | §14g | verbose graph windowed | all-closed note |
| Worker lanes (§14h) | lane placeholders | no active workers | per-lane error, rest render | n/a | paged lanes | idle, all `▱` |
| Create (§14i) | — | — | command error + `g` | already exists / not swarmable | — | — |

