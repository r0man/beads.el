---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-6fwx
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
    - path: beads/be-ux4q
      hash: bead:be-ux4q
      ids:
        - REQ-SF-020
        - REQ-SF-021
        - REQ-SF-024
        - REQ-SF-033
  coverage:
    - id: REQ-SF-020
      status: covered
    - id: REQ-SF-021
      status: covered
    - id: REQ-SF-024
      status: covered
    - id: REQ-SF-033
      status: covered
---

# WI-SF-01 — Molecule view foundation

## Summary

Added `lisp/beads-molecule.el`, the first-class molecule execution view,
and `lisp/test/beads-molecule-test.el` with 25 unit tests plus a
self-contained `bd init` integration test.  The view is a
`beads-section-mode`/`vui` buffer that aggregates `bd mol current`,
`bd mol progress`, `bd mol show` and the molecule's gates through
`beads-command-execute-async` with per-molecule `:cache-key` and a
generation-keyed reload, renders the root header, progress/frontier
counts, the step DAG with per-step state and glyphs, a gates section and
an output section, and installs the shared `beads-thing` movement
contract.  No existing tracked file was modified: the work is additive.

## Intended Behavior

- **REQ-SF-020 (molecule view).** `beads-molecule` / `beads-molecule-open`
  opens `beads-molecule-mode` for a root id.  The header shows the root
  id and title, phase (`persistent`/`vapor`, derived from the step
  issues' `ephemeral` flag), assignee, `bd mol progress` completion and
  percentage.  The step tree renders one row per `bd mol current` step
  with `[ready]`/`[current]`/`[done]`/`[blocked]`/`[pending]`/
  `[blocked-gate]` derived by the extensible `beads-molecule-step-state`
  generic plus `beads-molecule-step-face`/`-glyph`; `blocked` is computed
  from `bd mol show` `blocks` edges that are not yet done.
- **REQ-SF-021 (progress and ETA, render).** The header shows
  completed/total, percentage, rate and ETA.  Rate and ETA render as `—`
  when the running bd does not emit them (bd 1.3.1 emits neither), and
  are read when present.  `]`/`[` shift a `--range` window (default 50
  steps) so large molecules are windowed by `bd mol current --range`.
- **REQ-SF-024 (step actions, render).** `RET` on a step opens the issue
  detail view via the `beads-molecule-action` `inspect` method; TAB/
  S-TAB/SPC follow the shared `beads-thing` contract.  The write actions
  (`c`/`x`/`C`) are intentionally out of scope for this foundation and
  land in WI-SF-02.
- **REQ-SF-033 (molecule integration, render).** An open gate that blocks
  a step annotates it `[blocked-gate ...]` with the gate id, and the
  gates section lists type, id, status and blocked steps.  Gate
  correlation is structured: open gates are fetched with
  `bd list --type gate`, then resolved in one batched
  `bd show --include-dependents` call.  Gate detail interaction is
  WI-SF-06.
- Folding state is buffer-local and persists across soft refreshes;
  stale-while-revalidate keeps the previous payload on screen during a
  refresh; each rendered section is wrapped in `vui-error-boundary` so
  one failure cannot blank the view.

### Coverage

| ID | Status |
| --- | --- |
| REQ-SF-020 | covered |
| REQ-SF-021 | covered |
| REQ-SF-024 | covered |
| REQ-SF-033 | covered |

## Changed Files

- `lisp/beads-molecule.el` (new) — molecule view: faces/glyphs, JSON
  normalisation, step-state generic, model builder, async aggregate
  loader, `vui` root component, `beads-molecule-mode` keymap and the
  `beads-molecule`/`beads-molecule-open` entry points.
- `lisp/test/beads-molecule-test.el` (new) — 25 `:unit` tests (state
  transition table, faces/glyphs, normalisation, model construction,
  rendering, range shifting, aggregate loader fan-out with a stubbed
  executor and error/soft-failure paths) plus one `:integration` test
  that writes a self-contained formula, pours it, derives the model,
  attaches a real gate and asserts `blocked-gate`.

No other tracked file was modified; `lisp/beads-section.el` was listed
as an asset but needed no change because the view reuses
`beads-section-mode`, `beads-section-glyph-*` and `beads-thing` as-is.

## Verification

First verification command:

```
eldev -p -dtT test -f beads-molecule-test.el '(tag :unit)'
```

Observed: `Ran 25 tests, 25 results as expected, 0 unexpected` — pass.

Final proof command:

```
eldev -p -dtT test -f beads-molecule-test.el
```

Observed: `Ran 168 tests, 168 results as expected, 0 unexpected` — pass
(the file filter also loads and runs the `beads-test`/
`beads-integration-test` infrastructure tests).

Additional evidence:

- `eldev -p -dtT compile` — clean (no warnings from `beads-molecule.el`).
- `eldev -p -dtT lint` — `Linters have no complaints`.
- `eldev -p -dtT test '(not (tag :integration))'` — whole project unit
  suite passed (live tests skipped).
- Live render, local: a fresh `bd init` temp repo with a poured
  `miniflow` molecule rendered the header, `ready`/`blocked` steps,
  the gates section and the footer; after creating a `human` gate the
  step flipped to `blocked-gate`, `r` flipped the section to `Ready (1)`
  and SPC folded the gates section.
- Live render, remote (`/ssh:localhost:/tmp/...`): the identical buffer
  rendered over the TRAMP ssh-pipe transport, confirming the async spawn
  path does no local-store shortcut.

## Remaining Risks

- `bd ready --mol <root>` was not used: with bd 1.3.1 it emits a
  molecule object, but `beads-command-ready` declares `(list-of
  beads-issue)`, so that coercion would fabricate empty issues.  The
  ready-only filter therefore derives the frontier from `bd mol current`
  statuses, which is equivalent and one subprocess cheaper; a future WI
  can add a result-less variant if the parallel-group data is needed.
- Rate/ETA are not emitted by bd 1.3.1, so the header shows `—`; the
  renderer already reads `rate`/`rate_per_hour`/`eta`/`eta_seconds` when
  a newer bd provides them.
- Phase is derived from the step `ephemeral` flag because bd does not
  expose the formula phase on `bd mol show`; WI-SF-05 surfaces the
  authoritative phase for the formula detail and can be reused later.
- Gate correlation fetches all open gates then one batched
  `bd show --include-dependents`; on a store with very many gates this is
  two extra subprocesses regardless of molecule size.
