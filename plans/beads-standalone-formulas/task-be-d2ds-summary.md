---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-fc1i
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
    - path: beads/be-d2ds
      hash: bead:be-d2ds
      ids:
        - REQ-SF-063
  coverage:
    - id: REQ-SF-063
      status: covered
---

## Summary

WI-SF-10 (REQ-SF-063) surfaces `bd mol distill <epic>` from the
issue/epic detail.  The new `beads-formula-edit.el` module owns the
distill flow: an autoloaded `beads-formula-distill` entry point opens a
transient seeded with the epic, collects an optional formula name, output
directory and `variable=value` mappings, shows a `--dry-run` preview, and
follows to the created `.formula.*` source file.  The flow drives the
existing `beads-command-mol-distill` class; that class gained the missing
`formula-name` positional and the correct long flags
(`--dry-run`/`--output`/`--var`), because the previous short flags
(`-n`/`-o`/`-v`) are rejected by `bd` (`-v` even collides with the global
`--verbose`).

Coverage:

| ID | Status |
| --- | --- |
| REQ-SF-063 | covered |

## Intended Behavior

- `D` in `beads-show-mode` (issue/epic detail) and in
  `beads-epic-status-mode` (epic at point) opens the distill transient;
  `beads-epic-menu` gains a matching entry.  The entry falls back to
  issue completion when point is not on an epic id.
- The transient shows live state in each suffix label: epic, formula name
  (blank means bd derives it), output directory, and the number of var
  mappings.  `P` runs `bd mol distill --dry-run` into
  `*beads-distill-preview*`; `s` runs the real distill and opens the
  reported path with `find-file`.
- `beads-formula-distill-var-args` normalizes mappings (drops blanks,
  requires two non-empty sides) and signals `user-error` on malformed
  input.
- `beads-formula-distill--output-path` recognizes both the dry-run
  `Output:` line and the applied `Path:` line, so the follow works
  whichever command produced the text.
- `beads-command-mol-distill` now emits positionals
  `EPIC [FORMULA-NAME]` and the long options `--dry-run`, `--output`,
  `--var`, and runs with `:json nil` (bd prints human-readable text for
  this subcommand).
- Standalone: no gascity dependency; remote (TRAMP) stores work through
  the existing command plumbing (`beads-command-execute` selects the ssh
  pipe, and the reported path is opened with `find-file`).

## Changed Files

- `lisp/beads-formula-edit.el` (new) — distill transient, entry point,
  command assembly, preview buffer and follow.
- `lisp/test/beads-formula-edit-test.el` (new) — unit tests plus a live
  integration test that distills a temp epic and re-cooks the resulting
  formula.
- `lisp/beads-command-mol.el` — `beads-command-mol-distill` gains
  `formula-name` (positional 2), switches to the
  `--dry-run`/`--output`/`--var` long options, and declares `:json nil`.
- `lisp/beads-command-show.el` — `D` binding in `beads-show-mode-map`
  plus a forward declaration.
- `lisp/beads-command-epic.el` — `D` in `beads-epic-status-mode-map`, the
  `beads-epic-menu` entry, and a forward declaration.

## Verification

First verification command (from the item worktree):

```
eldev test -f beads-formula-edit-test.el
```

Result: Ran 151 tests, 151 results as expected, 0 unexpected (the 9 new
tests all pass).  `beads-formula-edit-test-distill-integration` distills
an epic in a temp repo, asserts the `.formula.*` file exists, and
re-cooks it with `bd cook --mode compile` — live `bd` evidence.

Supporting gates:

```
eldev -p -dtT compile
eldev -p -dtT lint
eldev test '(not (tag :integration))'
```

Result: compile and lint exit 0 with no warnings; the unit suite ran 5826
tests, 5802 expected, 0 unexpected, 24 skipped (live).  Note: running
`eldev test -f beads-test.el` in isolation still trips the pre-existing,
order-dependent `beads-test-issue-at-point-from-show-buffer` failure on
the untouched base commit 917280a; it does not occur in the canonical
full-unit run.

Final proof command (run from the launcher rig root
`/home/roman/workspace/beads.el`):

```
GC_BEAD_ID=be-2mgq .gc/scripts/checks/build-artifact-valid.sh
```

Result: `build artifact valid: schema=gc.build.implementation-summary.v1`.

## Remaining Risks

- `bd` 1.3.1 writes `.formula.json`, not `.formula.toml`; the flow
  follows whatever path bd reports (`Output:`/`Path:` line) and does not
  assume the extension.  A bd build with a different distill output shape
  may need the path regex extended.
- `beads-formula-edit.el` is shared with WI-SF-09 (formula
  create/edit/convert).  This change implements only the distill section;
  the merge must keep a single `provide` and require set.
- `beads-show` binds `D` at the mode level for distill while the
  `beads-show-actions` quick-actions transient keeps `D` for "Children".
  The two surfaces differ intentionally (mode key vs transient key).
- `NEWS.md` and menu documentation updates are deferred to the WI-SF-14
  docs item to respect that work item's file boundary.
