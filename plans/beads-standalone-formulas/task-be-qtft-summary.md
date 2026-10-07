---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-6b49
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
    - path: beads/be-qtft
      hash: bead:be-qtft
      ids:
        - REQ-SF-060
        - REQ-SF-061
        - REQ-SF-062
    - path: lisp/beads-formula-edit.el
      hash: sha256:835800fca925f20c454da66bfbacc8b829cd6e2742dcd8ae1745f8e2b6b04f3c
    - path: lisp/test/beads-formula-edit-test.el
      hash: sha256:01214379d68a1fa8c116eb2e94153c96e69f787aa320e49a34ebf46e4038a40c
  coverage:
    - id: REQ-SF-060
      status: covered
    - id: REQ-SF-061
      status: covered
    - id: REQ-SF-062
      status: covered
---

## Summary

WI-SF-09 (REQ-SF-060, REQ-SF-061, REQ-SF-062) adds the formula
authoring surface in a new `lisp/beads-formula-edit.el`: create a
minimal `.formula.toml` scaffold in the project or user search-path
directory and open it in TOML mode; open an existing formula's source
the same way; complete and validate the buffer against
`bd formula schema` (`beads-formula-schema-struct` field names, types
and required flags) plus the `bd` TOML parser, rendering inline
diagnostics and a compile-style results buffer; and convert a JSON
formula to TOML with `--stdout`/`--delete` variants.

Coverage:

| ID | Status |
| --- | --- |
| REQ-SF-060 | covered |
| REQ-SF-061 | covered |
| REQ-SF-062 | covered |

## Intended Behavior

- `beads-formula-new` (autoloaded) reads a name, type
  (`workflow`/`expansion`/`aspect`) and scope (`project` under
  `.beads/formulas`, `user` under `~/.beads/formulas`), writes
  `beads-formula-edit-scaffold` text, and opens the file associated
  with the formula name.  A name with a path separator or an existing
  file is rejected with `user-error`.
- `beads-formula-edit-source` resolves a formula name/summary/object to
  its `source` through `bd formula show`, localizes the path for remote
  (TRAMP) stores via `beads-remote-localize-path`, and opens it in the
  authoring mode.  `beads-formula-edit-source-at-point` wires it to the
  formula list/detail views under `e`.
- Authoring buffers use `toml-mode` when installed, else `toml-ts-mode`
  when the grammar is present, else `conf-mode`, plus
  `beads-formula-edit-minor-mode` (`C-c C-c` validate+save, `C-c C-v`
  validate, `C-c C-x` convert) and schema completion through
  `completion-at-point`.
- `beads-formula--schema-completions` maps the enclosing TOML section
  (`vars` -> `VarDef`, `steps`/`template` -> `Step`, `compose` ->
  `ComposeRules`, ... , top level -> `Formula`) to the struct returned by
  `bd formula schema` and offers that struct's field names.
- `beads-formula-edit-validate` merges a client-side check (required
  top-level `Formula` fields and TOML value-kind mismatches, from the
  live schema) with the `bd` TOML parser diagnostics (captured from
  `bd formula show` stderr, `line N (last key ...)` shape), paints an
  inline overlay per diagnostic, and renders
  `*beads-formula-validate*` in `beads-formula-edit-validate-mode`
  (`RET` jumps to the source line).  Unknown keys are out of scope, per
  REQ-SF-061.
- `beads-formula-convert-at-point` (autoloaded) opens a transient seeded
  from a `*.formula.json` buffer with `--stdout` and `--delete`
  toggles; the command runs with `:json nil` because `--stdout` emits
  TOML.  `--stdout` output lands in `*beads-formula-convert*`; the file
  variant reports the conversion.  Wired to `C` in the formula views.
- No new hard dependency: completion and validation are driven by
  `bd formula schema`, never a bundled TOML parser, so `Package-Requires`,
  `Eldev` and `guix.scm` are unchanged.

## Changed Files

- `lisp/beads-formula-edit.el` (new) — scaffold/search-path resolution,
  open-source, schema completion, validation (client + `bd`) with inline
  overlays and the results buffer, JSON->TOML convert transient, the
  authoring minor mode, and the `e`/`C` bindings for the formula views.
- `lisp/test/beads-formula-edit-test.el` (new) — 23 unit tests plus one
  live integration test; the unit tests mock `beads-command-execute`, so
  no `bd` is required.

## Verification

First verification command (from the item worktree):

```
eldev -s test beads-formula-edit-test
```

Result: Ran 24 tests, 24 results as expected, 0 unexpected.  The 23
unit tests cover scaffold/path resolution, open-source resolution,
schema completion (top level, section, prefix, field annotation),
required/type-mismatch validation, `bd` parse-error parsing, inline +
results rendering, the results jump, convert command assembly and the
stdout buffer, and the minor-mode keys.  The `:integration` test
`beads-formula-edit-test-validate-integration` scaffolds a formula in a
temp repo, validates it clean, then writes a wrong `version` type to
disk and asserts the `bd` parser reports `line 2 ... incompatible
types` — live `bd` evidence.

Supporting gates (from the item worktree):

```
eldev -p -dtT compile
eldev -p lint
eldev test -f beads-formula-edit-test.el
```

Result: compile finished successfully; lint reports "Linters have no
complaints".  The `-f beads-formula-edit-test.el` run loads the shared
suite as well (166 tests, 165 expected) and still reports the
pre-existing, order-dependent `beads-test-issue-at-point-from-show-buffer`
failure on the untouched base commit 917280a (it passes in isolation);
all 24 `beads-formula-edit-test` tests pass.

Final proof command (run from the launcher rig root
`/home/roman/workspace/beads.el`):

```
GC_BEAD_ID=be-8drt .gc/scripts/checks/build-artifact-valid.sh
```

Result: `build artifact valid: schema=gc.build.implementation-summary.v1`.

## Remaining Risks

- `beads-formula-edit.el` is the shared home of WI-SF-09 (this change)
  and WI-SF-10 (distill).  The concurrent WI-SF-10 worktree adds the
  distill section to the same file; the merge must keep a single
  `provide`, a single require set, and both sections.  This change
  deliberately implements only the create/edit/schema/convert section.
- Client-side type checking is intentionally shallow (top-level
  `Formula` scalars/containers) and the `bd` TOML parser is the authority
  for deep/nested errors.  A `bd` build whose parse message drops the
  `line N` prefix would yield no inline line for that diagnostic; the
  client check still reports the top-level mismatch.
- `bd` 1.3.1 accepts either `variable=value` or `value=variable` for
  convert/distill vars and silently drops unknown keys; REQ-SF-061
  scopes unknown-key detection out, so the validator does not attempt it.
- Remote authoring writes through TRAMP (`make-directory`/`with-temp-file`
  on the localized path).  This was not exercised in this session's tests;
  the standing TRAMP acceptance run for WI-SF-13 remains the gate.
- `NEWS.md` and menu-documentation updates are deferred to the WI-SF-14
  docs item to respect that work item's file boundary.
