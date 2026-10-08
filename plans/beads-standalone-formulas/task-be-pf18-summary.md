---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-zj91
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
    - path: beads/be-pf18
      hash: bead:be-pf18
      ids: [REQ-SF-012]
    - path: lisp/beads-command-cook.el
      hash: sha256:e8db3d1955652ce718a89eacc4fddf577ef26b776320f410575bbd1ee512f976
    - path: lisp/beads-command-misc.el
      hash: sha256:e242a499134a3f9ebd78fd6bdd7933d7e1fac601e544b514b594e4548067080c
    - path: lisp/beads-cook.el
      hash: sha256:e8e74ac4edc55b01bc250fbf3aa39a7ec6823a29944c8e9e64e32254aa8d653c
    - path: lisp/test/beads-cook-test.el
      hash: sha256:62987d69186be7592205b588795ef4fac02f59ad855c4f555e18662f5db3f1d9
    - path: lisp/test/beads-command-misc-test.el
      hash: sha256:d9deb10280b8df16c2ece382377ad3c9c8ac78d20cdf29bfd56e359774581462
    - path: lisp/beads.el
      hash: sha256:33c2156715bbc67ca25a25cd4dd2d6e6a08956e6babfd878f5ee2c3bd59ed842
    - path: NEWS.md
      hash: sha256:ab0dbddf9067de47f7af6b1e755e7c63ece924934d053f091c596abf85306be1
  coverage:
    - id: REQ-SF-012
      status: covered
---

## Summary

WI-SF-04 implements the `bd cook` porcelain and splits its command class out
of `beads-command-misc.el` (REQ-SF-012). `lisp/beads-command-cook.el` now owns
the `beads-command-cook` EIEIO class; the auto-generated transient is
suppressed (`:transient nil`) because the porcelain prefix lives in the new
`lisp/beads-cook.el`. That module provides the `beads-cook` transient
(compile/runtime mode, `--persist` with `--force`/`--prefix`, `--var`), the
`beads-cook-preview` command and `beads-cook-parse-tree`, which turns the
human-readable `bd cook --dry-run` output into a step/dependency tree rendered
in a `beads-cook-preview-mode` buffer. `P` always previews with `--dry-run`
and never writes; `s` cooks, writing a proto only when Persist is on, and
Force without Persist is rejected before `bd` runs. The `beads-cook` autoload
now resolves to `beads-cook.el`, and the cook test moved from
`beads-command-misc-test.el` to `lisp/test/beads-cook-test.el`.

## Intended Behavior

- `bd cook` remains reachable as the `beads-command-cook` class; its slot
  metadata and CLI serialization are unchanged except for the module move and
  the suppression of the auto-transient.
- `M-x beads-cook` (and `K` in the maintenance menu) opens a transient with:
  `m` compile|runtime mode, `p` persist, `f` force (requires persist),
  `x` proto prefix, `v` key=value variables.
- `P` runs `bd cook --dry-run` (non-JSON, so `bd` prints the step tree),
  parses it into `:formula`, `:proto`, `:mode`, `:step-count`,
  `:variables-used` and a `:steps` list, and renders it. Nothing is written.
- `s` runs the cook without `--dry-run`; because `--persist` is the only
  writing path, a proto is created only when Persist is on. Force without
  Persist signals a `user-error` before any process runs.
- The parser tolerates unknown lines and values containing brackets
  (e.g. `[from: build-basic@steps[0]]`) and multiple keys in one bracket
  (`[depends: x, needs: y]`).

## Changed Files

- `lisp/beads-command-cook.el` (new): the relocated `beads-command-cook`
  class, `:transient nil`.
- `lisp/beads-command-misc.el`: removed the cook class block.
- `lisp/beads-cook.el` (new): `beads-cook` transient, `beads-cook-preview`,
  `beads-cook-preview-mode`, `beads-cook-parse-tree` and the arg helpers.
- `lisp/test/beads-cook-test.el` (new): unit tests for the class, parser,
  preview and suffix guard, plus an `:integration` dry-run/parity test.
- `lisp/test/beads-command-misc-test.el`: removed the cook command-line test
  (moved to the new file).
- `lisp/beads.el`: `beads-cook` autoload now points at `beads-cook`.
- `NEWS.md`: Unreleased entry for the new porcelain.

## Verification

First verification command:

```
eldev test -f beads-cook-test.el '(tag :unit)'
```

Observed: `Ran 10 tests, 10 results as expected, 0 unexpected`.

Integration / live-evidence command (real `bd`, temp repo, cook dry-run parse
plus `bd cook --json` vs `bd formula show --json` parity):

```
eldev test -f beads-cook-test.el beads-cook-test-integration-dry-run-and-parity
```

Observed: `Ran 1 tests, 1 results as expected, 0 unexpected`.

Final proof command (byte-compile plus the two cook selectors):

```
eldev compile && eldev test -f beads-cook-test.el '(tag :unit)' \
  && eldev test -f beads-cook-test.el beads-cook-test-integration-dry-run-and-parity
```

Observed: `eldev compile` produced no warnings or errors for the new/changed
files; both test selectors passed as above. `eldev test -f beads-menu-test.el`
also passes (8/8), confirming `beads-cook` remains reachable from
`beads-maintenance`. Two pre-existing, unrelated failures show when
`beads-command-misc-test.el` is run in isolation
(`beads-command-restore-test-*`, because `beads-command-restore` is not
required by that file); they are unaffected by this change and pass in the
full suite where the module is loaded.

Coverage:

| ID | Status |
| --- | --- |
| REQ-SF-012 | covered |

## Remaining Risks

- The `bd cook --dry-run` format is human-readable text, not JSON, so
  `beads-cook-parse-tree` depends on `bd`'s current wording and branch
  glyphs. The parser is defensive (unknown lines are ignored) and pinned by
  unit fixtures plus the live integration test, but a future `bd` formatting
  change could reduce parsed detail without signalling.
- The class now suppresses its auto-generated transient, so the raw
  `beads-cook` prefix is the porcelain in `beads-cook.el` rather than the
  generated command menu. This is deliberate (the design/mockup place the cook
  transient in the UI module), and the class remains the execution bridge.
- Other drain members touch `NEWS.md` and `lisp/beads.el`; the merge of this
  branch may need conflict resolution in those two files.
