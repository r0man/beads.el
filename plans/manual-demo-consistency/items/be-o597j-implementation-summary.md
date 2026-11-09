---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-w7pjp
  formula: do-work
methodology:
  pack: gascity
  name: build-basic
producer:
  formula: do-work
  stage: implement
  attempt: 1
status: approved
trace:
  upstream:
    - path: beads/be-o597j
      hash: bead:be-o597j
      ids: [REQ-008, REQ-010]
    - path: plans/manual-demo-consistency/requirements.md
      hash: sha256:5a665190a49fa226ab49e221ddb10aa108f236b9a0e18e3b8079b8213ff3fd21
    - path: plans/manual-demo-consistency/implementation-plan.md
      hash: sha256:dd851b8ba3f345740b22622682f215a2467c16a38589fdb62698af98a6d10ecd
    - path: docs/key-registry.md
      hash: sha256:b7bc58a58c36dc2fa16ab8f8f584604ffbc0f72cdccaa9af9ba5ca3df4d780dd
    - path: doc/reference.texi
      hash: sha256:12fdfbc1c157e02661064f278035ee3c1c245ad0a8efcffef7224573bbd2c4fa
    - path: lisp/test/beads-manual-reference-test.el
      hash: sha256:a41c0b9a39883b500c3cae1cadf4db3839b02fcd41fefead834c3cce29cde85d
  coverage:
    - id: REQ-008
      status: covered
    - id: REQ-010
      status: covered
---

# Implementation Summary: W5 reference, keybinding, and index completeness

## Summary

Work item W5 (implementation plan WS-5, requirements REQ-008 and REQ-010)
closes the reference chapter's keybinding and index gaps and records a
manual-vs-observed-UI spot check.

`doc/reference.texi` now documents **the exact keybinding surface of the
documented dispatch menus**.  The old Keybinding Reference mixed two
different surfaces: full key paths from the `?` menu (`! f`, `d a`, …) and a
second, bare-key listing of the maintenance and integration menus (`f`,
`n`, `I`, …).  The bare rows were ambiguous — `l` appeared both as "List
beads" and "Lint" — and 179 reachable menu keys were missing entirely.  The
chapter is now derived from the live transient layouts: 304 rows in 15
sections, one row per reachable suffix, with generated infix pseudo-commands
named by the option they set.  Every distinct key a row presents carries a
`@kindex` entry.

`@defvr {Keymap}` entries document the five keymaps the task chapters rely
on (`beads-list-mode-map`, `beads-show-mode-map`,
`beads-dashboard-mode-map`, `beads-section-mode-map`,
`beads-compose-mode-map`); their `@table @kbd` rows are the registry's
declared keys and the test compares each row with the live `lookup-key`.

The reference-completeness `:unit` gate
(`lisp/test/beads-manual-reference-test.el`) grew from three checks to
twelve.  It now also re-derives the keybinding surface from the menus, the
keymap surface from `docs/key-registry.md`, checks that every presented key
is indexed and every indexed key is bound, rejects duplicate key-index
entries per node, rejects duplicate definitions, and rejects explicit
`@findex`/`@vindex` orphans.  The tests also had three latent bugs fixed:
a naive `@kbd{...}` regexp that dropped `@}`-escaped keys, a lost match-data
bug in the key-index reader, and a symbol/string mismatch when comparing
registry keymap names.

Smaller accuracy fixes found by the gate and by the UI pass: the
maintenance `User Options` list and orchestration's `beads-terminal-backend`
no longer repeat `@defopt` entries from the reference chapter, the stale
`@vindex beads-formula-scope` (a function, not a variable) is gone,
`@kindex keys` is gone, and the agent-menu key paths in `agents.texi` and
`mark all` in `getting-started.texi` use `@kbd{...}` markup so each
keystroke is indexed individually.

## Intended Behavior

- **Keybinding Reference is exact (REQ-008).** Walking `beads-dispatch`,
  recursing into every reachable transient prefix, and skipping
  `transient-quit-one` yields 304 `(path . name)` pairs.  The chapter has
  exactly those rows.  A renamed menu key, a moved command, or a dropped
  row fails `beads-manual-reference-test-keybinding-reference-matches-menus`
  naming the key and command.
- **Indexes are complete and honest (REQ-008).** Every key in a
  `@table @kbd` anywhere in the manual has a `@kindex`; every `@kindex`
  names a key that some transient or mode map binds; a node indexes a key
  at most once; no name is defined twice; and an explicit `@findex`/
  `@vindex` for a `beads-…` identifier must name something the manual
  defines.
- **Keymaps match the registry and the code (REQ-008, REQ-010).** Each
  documented keymap is a real keymap variable, every documented row is the
  map's effective binding, and every `(map, key . command)` declaration in
  `docs/key-registry.md` appears in the matching `@defvr` table.  The
  registry's own declarations are re-checked against `lookup-key`, so the
  manual and the registry test now enforce the same contract.
- **The manual matches the running UI (REQ-010).** The observed/expected
  table under Verification records the live check of the dispatch menu,
  the list and show views, the at-point actions, the actions transient, the
  compose/comment buffers and metadata sidebar, the dashboard empty states,
  the remote (TRAMP) store, and the standalone (no gascity) path.
- **No generator is added.**  The chapter stays hand-maintained; the pinned
  derivation rule in its header comment was updated to describe rules 2–5
  and is enforced by the gate.  `scripts/dispatch-docs --check` remains
  green (no documented menu changed).

| ID | Status |
| --- | --- |
| REQ-008 | covered |
| REQ-010 | covered |

## Changed Files

- `doc/reference.texi` — regenerated Keybinding Reference (304 rows,
  15 sections, per-key `@kindex`); new `Keymaps` subsection with five
  `@defvr {Keymap}` entries and their registry tables; pinned contract
  comment updated; duplicate key-index entries removed.
- `lisp/test/beads-manual-reference-test.el` — WS-5 gate: nine new checks
  (keybinding reference, key index coverage, orphan/duplicate index
  entries, registry declarations, keymap definitions, single definition,
  explicit index orphans, documented hooks/keymaps) plus the three parser
  bug fixes; commentary updated.
- `doc/agents.texi` — agent-menu key paths are `@kbd{...}` markup and the
  individual keystrokes are indexed.
- `doc/getting-started.texi` — `mark all` is `@kbd{*} @kbd{!}` and both
  keys are indexed.
- `doc/formulas.texi` — removed the stale `@vindex beads-formula-scope`.
- `doc/issue-workflow.texi`, `doc/views.texi`, `doc/orchestration.texi` —
  `@kindex` entries for the keys their `@table @kbd` blocks present.
- `doc/maintenance.texi` — removed the `@defopt` duplicates (they are
  documented once in User Option Reference, now `@xref`d); indexed the
  local key tables.
- `doc/orchestration.texi` — removed the duplicate
  `@defopt beads-terminal-backend` and pointed at User Option Reference.
- `plans/manual-demo-consistency/items/be-o597j-implementation-summary.md`
  — this summary.

## Verification

First verification command (targeted gate):

```
$ emacs --batch -Q -L lisp -L lisp/test \
    -l lisp/test/beads-manual-reference-test.el \
    -f ert-run-tests-batch-and-exit
Ran 12 tests, 12 results as expected, 0 unexpected
```

Final proof commands (from the item worktree):

```
$ make check
Ran 6693 tests, 6667 results as expected, 0 unexpected, 26 skipped
(exit 0; the 26 skipped are the :live emacsclient tests)

$ make lint
Byte-compiled 130 file(s)          (exit 0, no warnings)

$ make check-manual                (exit 0)
$ scripts/dispatch-docs --check    → dispatch docs ok
$ (cd doc && makeinfo --no-split -o /tmp/beads.info beads.texi)   (exit 0, no warnings)
$ (cd doc && makeinfo --html --no-split -o /tmp/beads-html beads.texi)  (exit 0, no warnings)
```

### Observed vs. documented UI (REQ-010)

Fresh `emacs -nw -Q` in tmux, `lisp/` on `load-path`, no gascity loaded,
fixture created by `docs/manual/screenshots/fixture.sh create` (the base
branch fixture, not the shared `/tmp/demo-project`, which WS-1 has not
landed yet).

| Area | Documented | Observed |
| --- | --- | --- |
| Dispatch (`?`) | Issues / Workflow / Views / Manage / Maintenance groups | Same five groups; keys `l c / i u x o e`, `r b d m G w S T`, `a F v D H R Y t X`, `L k .`, `! * g q` all present |
| List (`? l`) | `*beads-list[PROJECT]*`, footer `RET visit · SPC fold · m mark · d close · C claim · / filter · ? dispatch` | Buffer named exactly that; footer text matched |
| At-point actions | `d` closes after a reason prompt, `C` claims, `s` status, `#` priority, `w` copies the id | "Close demo-3 reason:", "Claimed 1 issue(s)", "Status for demo-3:", "Priority for demo-3:", "Copied issue ID: demo-3" |
| Show (`M-x beads-show`) | `*beads-show[PROJECT]/ID*`; sections Store, Created/Started/Updated, Assignee, DESCRIPTION, LABELS, dependencies; footer `RET follow · c comment · d close · C claim · g refresh · q bury · ? dispatch` | Buffer and sections matched; labels `[bug] [dashboard]`, `← ⛔ demo-4 …` block row present |
| Actions transient (`M-x beads-show-actions`) | Status/Navigate/Edit/Other; `s # c C o`, `d B P D`, `e n L`, `w g q` | Menu and keys matched exactly |
| Compose/comment (`c`) | `*beads-comment[PROJECT]/ID*`, `C-c C-c` submit, `C-c C-k` cancel | Header "Comment on demo-3: C-c C-c submit, C-c C-k cancel"; `C-c C-k` returned to the list |
| Metadata sidebar (`C-c C-a`) | `t` Type, `p` Priority, `a` Assignee, `l` Labels, `P` Parent | Sidebar showed exactly those five rows |
| Dashboard empty states | Empty sections say `Nothing to show.` / no claimed work | "Nothing to show." (Orphaned dependencies 0) and "No work claimed." (In progress 0) |
| Remote (TRAMP) | Same views work on `/ssh:localhost:~/bright-lights`; buffer names are qualified | `*beads-dashboard</ssh:localhost:\|bright-lights>*` loaded Open 45 / Ready 31 / Blocked 14 / Closed 594 and "No work claimed." |
| Standalone (no gascity) | Dispatch works with beads only | Menu contained only beads groups; no gascity group or error |

One tmux-specific artifact, not a code defect: `tmux send-keys RET` in
this terminal produced a stray `T` key event, so `RET` from the list was
verified with `C-h k`/`lookup-key` (`RET → beads-list-show`) and by opening
the show buffer explicitly; the live-UI suite already covers
`beads-live-test-show-renders-buffer`.

## Remaining Risks

- The dispatch-key surface gate is hermetic (`:unit`): it reads parsed
  transient layouts, not rendered menus.  Rendering differences remain the
  live-UI tests' job.
- `beads-compose-mode-map` is documented (and its three rows checked)
  although `docs/key-registry.md` only covers the four mode views; the
  compose keys are checked against the live map but not against the
  registry tables.
- The base branch fixture still writes a `mktemp` run directory.  WS-1
  retargets it to `/tmp/demo-project`; this item's UI pass deliberately
  used `fixture.sh create`'s printed path so the spot check does not depend
  on WS-1 landing first.
- The Keybinding Reference is now 304 derived rows; any future menu change
  must be mirrored by hand (the pinned contract says so) and the gate will
  name the missing row.
