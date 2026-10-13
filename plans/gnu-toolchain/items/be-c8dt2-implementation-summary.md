---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-nxipz
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
    - path: beads/be-c8dt2
      hash: bead:be-c8dt2
      ids: [REQ-006, REQ-008, REQ-009, REQ-014]
    - path: plans/gnu-toolchain/requirements.md
      hash: sha256:e10a5d3dbcbb01f0571e768d3ec222bbbf4c3bfa3b98703b8411e1857c4fcaff
    - path: plans/gnu-toolchain/implementation-plan.md
      hash: sha256:6734ce93211d72bae8bb15520a052a84f31d66c97d260426075b7bfd098d0c01
    - path: plans/gnu-toolchain/decomposition.md
      hash: sha256:f27241bd58688fd3345a55de46292040b08c462532c875ed900e95d3c6f8f90f
    - path: build-aux/beads-compile.el
      hash: sha256:7da06f0231d6776a2fbabd4e4874fe2e98f9b2aa5399f85be5b199fe3d687ac6
    - path: Eldev
      hash: sha256:5f5a1d002c90f1313c6ae42a9cc62e3ca367ed520985e1804b9e38c7b20742b7
  coverage:
    - id: REQ-006
      status: covered
    - id: REQ-008
      status: covered
    - id: REQ-009
      status: covered
    - id: REQ-014
      status: covered
---

# Implementation Summary — be-c8dt2 (W2: autotools check and lint targets)

## Summary

W2 adds the no-Eldev quality gates on top of the W1 autotools skeleton
(`d7708f99`, worktree `/home/roman/workspace/beads.el/worktrees/be-c8dt2`,
branch `be-c8dt2`). It implements REQ-006, REQ-008, REQ-009 (test-support
portion) and REQ-014 from the decomposition's W2 (`be-c8dt2`).

Delivered:

- `make check` (`check-local` in `Makefile.am`) runs the whole ERT suite in
  one `$(EMACS) --batch -Q -L lisp -L lisp/test` invocation. The
  `bd`/`dolt`/`graphviz` `HAVE_*` conditionals become `check_have_*` make
  variables, forwarded to `build-aux/beads-check.el` as
  `BEADS_CHECK_TOOLS`; the runner runs the full suite (including
  `:integration`) only when all three are present, otherwise it runs the
  `:unit` selector and prints the missing tools. `--with-bd=no` (and the
  `dolt`/`graphviz` siblings) forces the reduced run.
- `build-aux/beads-check.el` reproduces the three Eldev test-harness
  behaviours: it deletes stale `lisp/*.elc`/`lisp/test/*.elc` immediately
  before ERT (F2, be-47jo); it redirects the JUnit report to
  `EMACS_TEST_JUNIT_REPORT` (creating its directory) because ERT otherwise
  names the file from the loaded test (F3); and it routes `message' through
  a nil-returning wrapper, which the close/reopen `--execute` assertions
  depend on.
- `make lint` invokes `scripts/lint`, which byte-compiles every `lisp/*.el`
  with warnings-as-errors (`build-aux/beads-compile.el`, the historical
  gate) and then runs `package-lint` on `lisp/beads.el`
  (`build-aux/beads-lint.el`). `package-lint` is resolved from
  `--with-package-lint=DIR`/`PACKAGE_LINT_DIR`, the ambient
  `EMACSLOADPATH` (the Guix `emacs-package-lint` input) or MELPA, and
  `configure.ac` gains `--with-package-lint`. A cold package archive cache
  is refreshed once so the `transient`/`vui` `Package-Requires` resolve.
- `scripts/dispatch-docs` drops its Eldev branch and drives plain Emacs
  (`$EMACS`, `-Q -L lisp`, plus `TRANSIENT_DIR`/`VUI_DIR`), keeping the
  `--check`/`--write` contract.

No `lisp/` runtime or `bd` CLI behaviour changes (REQ-014).

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-006 | covered |
| REQ-008 | covered |
| REQ-009 | covered |
| REQ-014 | covered |

## Intended Behavior

- **REQ-006 (`make check`).** `make check` runs the entire `lisp/test/*.el`
  ERT suite, and any failing test fails the target. The full/reduced
  `:integration` decision comes from configure's tool detection: all of
  `bd`, `dolt`, `graphviz` present runs the full suite; any missing runs
  `:unit` and names the missing tools; `--with-bd=no` forces reduced.
- **REQ-008 (quality gates preserved).** Byte-compile warnings remain fatal
  through `make lint` (an injected unused variable fails it), `package-lint`
  findings fail it, and `scripts/dispatch-docs --check` still runs and fails
  on drift without Eldev. Stale project byte-code is dropped before ERT and
  `coverage/junit.xml` is written from `EMACS_TEST_JUNIT_REPORT`.
- **REQ-009 (dependency resolution).** `make check`/`make lint` resolve
  `transient`/`vui` through `--with-*` or `EMACSLOADPATH`; `package-lint`
  through `--with-package-lint`/`EMACSLOADPATH`/MELPA. No new dependency and
  no `sesman` requirement is added (REQ-016).
- **REQ-014 (no behavior change).** Only build files, build helpers and the
  dispatch-docs script change; `git diff` against W1 shows no `lisp/`
  runtime change.

## Changed Files

- `Makefile.am` (+54) — `EMACS_BATCH_Q`, the `check_have_*` tool variables,
  `check-local`, the `lint` target, `EXTRA_DIST` additions.
- `configure.ac` (+10) — `--with-package-lint=DIR` / `with_package_lint`.
- `build-aux/beads-check.el` (new, 124 lines) — plain-Emacs ERT runner.
- `build-aux/beads-lint.el` (new, 54 lines) — package-lint driver.
- `scripts/lint` (new, executable) — byte-compile + package-lint wrapper.
- `scripts/dispatch-docs` (modified) — Eldev branch removed, plain-Emacs
  `$EMACS`/`TRANSIENT_DIR`/`VUI_DIR` handling added.
- `plans/gnu-toolchain/items/be-c8dt2-implementation-summary.md` — this
  artifact.

Implementation commit: `d7708f99` (build files; the summary is a follow-up
commit).

## Verification

First verification command (full suite with `bd`/`dolt`/`graphviz`
present, from the worktree):

```sh
./autogen.sh && ./configure && make && make check
```

Observed: **pass** (exit 0). `make check` reported `bd/dolt/graphviz
present; running the full suite` and `Ran 6630 tests, 6604 results as
expected, 0 unexpected, 26 skipped`.

Additional checks:

- `./configure --with-bd=no && make check` → exit 0;
  `beads-check: missing bd; running only :unit tests`;
  `Ran 6358 tests, 6334 results as expected, 0 unexpected` (proves the
  reduced decision and that the missing tool is named).
- Injected failing test (`lisp/test/beads-zzz-check-probe-test.el` with
  `(should (= 1 2))`) → `make check` exit **2**, `1 unexpected results`;
  probe removed afterwards.
- Stale byte-code guard: bogus `lisp/beads.elc` and
  `lisp/test/beads-test.elc` were deleted by the runner before ERT and the
  suite still passed.
- `EMACS_TEST_JUNIT_REPORT=coverage/junit.xml make check` → exit 0;
  `coverage/junit.xml` written (`<testsuites ... failures="0">`), so the
  path the coverage job uploads is honoured.
- `make lint` (with `--with-package-lint` pointing at the Guix
  `emacs-package-lint`) → exit 0; `Byte-compiled 127 file(s)` and
  package-lint reported no findings.
- Injected lint finding (temporary probe file with an unused lexical
  variable) → `make lint` exit **255**,
  `Error: Unused lexical variable 'unused-variable'`; probe removed.
- `./scripts/dispatch-docs --check` → exit 0 (`dispatch docs ok`) without
  Eldev; `--write` was idempotent and a following `--check` passed.

Final proof command (reproducibility of the gates on a fresh configure):

```sh
./autogen.sh && ./configure --with-bd=no && make check && make lint \
  && ./scripts/dispatch-docs --check
```

Observed: **pass** (each step exit 0; see the additional checks above for
the exact ERT/lint output).

## Remaining Risks

- **Duplicate sibling session.** A second live session
  (`ec-wisp-g7gdiw`, trigger bead `be-6kh3h`, the `implement` step for the
  same root) edited this worktree concurrently and started from a competing
  design (`build-aux/beads-test.el`, an unused
  `INTEGRATION_TESTS`/`TEST_SELECTOR` configure block). This change
  supersedes that design and incorporates its key insight (the
  nil-returning `message` harness behaviour). The two sessions can still
  race on commits until the dispatcher settles the duplicate claim.
- **`message` reroute.** The nil-returning `message` advice is a
  deliberate reproduction of Eldev's test harness; it changes test-visible
  behaviour only. If Eldev did not in fact do this in every environment,
  the close/reopen assertions would need fixing instead.
- **package-lint cold cache.** On a machine with no cached MELPA archive
  contents and no network, `make lint` fails on the archive refresh even
  though the byte-compile half passes. CI/Guix provides
  `emacs-package-lint` and network, so the intended path is covered.
- **Byte-compile is not `-Q`.** `scripts/lint`'s byte-compile step uses
  `--batch` without `-Q` because `beads-sesman.el` references
  `sesman-browser`, which is only autoloaded via site-start until the
  parallel `be-4fa65` removes `sesman`. `make check` is `-Q`-clean.
- **Full `make check` runtime.** The full suite (with `:integration`) takes
  roughly 7–8 minutes locally; CI's test matrix is unchanged by W2 (W10
  rewires it).
- **Do-work root vs implement step.** `gc gc claim` returned the workflow
  latch `be-nxipz` (`gc.kind=workflow`) rather than the `implement` step
  `be-6kh3h` owned by the sibling session; the artifact was therefore
  validated with `GC_BEAD_ID=be-6kh3h` (the step that carries the
  `gc.build.implementation-summary.v1` contract) while the root records
  `gc.implementation.summary_path`.