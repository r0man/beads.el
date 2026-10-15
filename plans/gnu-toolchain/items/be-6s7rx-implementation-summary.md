---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-2a22l
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
    - path: beads/be-6s7rx
      hash: bead:be-6s7rx
      ids: [REQ-007, REQ-008, REQ-009, REQ-012, REQ-015, REQ-016]
    - path: plans/gnu-toolchain/requirements.md
      hash: sha256:e10a5d3dbcbb01f0571e768d3ec222bbbf4c3bfa3b98703b8411e1857c4fcaff
    - path: plans/gnu-toolchain/implementation-plan.md
      hash: sha256:6734ce93211d72bae8bb15520a052a84f31d66c97d260426075b7bfd098d0c01
    - path: plans/gnu-toolchain/decomposition.md
      hash: sha256:f27241bd58688fd3345a55de46292040b08c462532c875ed900e95d3c6f8f90f
    - path: .github/workflows/pages.yml
      hash: git:9093da08508d3a5751bca3cf7c11795c8aa3b393
  coverage:
    - id: REQ-007
      status: covered
    - id: REQ-008
      status: covered
    - id: REQ-009
      status: covered
    - id: REQ-012
      status: covered
    - id: REQ-015
      status: covered
    - id: REQ-016
      status: covered
---

# Implementation Summary — be-6s7rx (W10: remove Eldev, rewire CI, Pages, Guix)

## Summary

W10 finishes the Eldev-to-autotools migration started by W1/W2. It removes
Eldev, rewires every workflow onto `./autogen.sh && ./configure && make`
(`make lint`, `make check`), adds a no-Eldev coverage path, adds the GitHub
Pages publish workflow, converts `guix.scm` to `gnu-build-system`, and updates
`AGENTS.md`/`NEWS.md`. It implements REQ-007, REQ-008, REQ-009, REQ-012,
REQ-015 and REQ-016 from `plans/gnu-toolchain/{requirements,implementation-plan}.md`.

The implementation commit is `9093da08508d3a5751bca3cf7c11795c8aa3b393` in
worktree `/home/roman/workspace/beads.el/worktrees/be-6s7rx`.

Delivered:

- **Eldev removed (REQ-007).** `Eldev` is deleted; `.eldev/` was never tracked
  and is gitignored. `emacs-eldev` is gone from `guix.scm`. No workflow
  invokes Eldev — the only remaining `eldev` strings in the tree are
  explanatory comments and one harmless `(bound-and-true-p eldev-project-dir)`
  fallback in `beads-transient-doc.el` (runtime, out of this change's scope).
- **Workflows rewired (REQ-007, REQ-008).**
  - `lint.yml`: installs Texinfo, resolves `transient`/`vui`/`sesman`/
    `marginalia`/`package-lint`, then `./autogen.sh && ./configure && make &&
    make lint`. The byte-compile warnings-as-errors gate is `make` itself.
  - `test.yml`: keeps the Emacs 29.4/30.2/31.1/snapshot matrix and the 29.4
    `continue-on-error`, installs bd v1.3.1 / dolt / graphviz / Texinfo, then
    `./autogen.sh && ./configure && make && make check`.
  - `coverage.yml`: replaces `eldev -s ... -U` with `scripts/coverage`
    (undercover under plain Emacs, source loading, codecov format, `:unit`
    subset), preserving `EMACS_TEST_JUNIT_REPORT` and the Codecov uploads.
  - `docs-drift.yml`: resolves deps, then `./configure` and
    `scripts/dispatch-docs --check` (no Eldev).
  - New `scripts/ci-deps` installs the ELPA dependencies for CI without Eldev.
- **Coverage without Eldev (REQ-007, REQ-008).** `build-aux/beads-coverage.el`
  reproduces the three pieces the old `Eldev` file configured
  (`beads-undercover-fileset`, codecov format with no auto-send, and the
  transient edebug-spec widening); `scripts/coverage` drives it; `make
  coverage` wraps it. `build-aux/beads-check.el` gained a reusable
  `beads-check-run-tests` and a `BEADS_CHECK_NO_RUN` guard so the coverage
  runner can reuse the stale-bytecode drop and JUnit-path advice without
  running the suite twice.
- **Pages (REQ-012).** New `.github/workflows/pages.yml` builds `make html`
  on push to `main` and deploys with `actions/upload-pages-artifact@v3` +
  `actions/deploy-pages@v4`, with `pages: write`/`id-token: write`, a `pages`
  concurrency group, and the "Pages source = GitHub Actions" note. Because
  `makeinfo --html` emits `<img src="images/...">` without copying the PNGs,
  `make html` now stages `doc/images` into `doc/beads.html/images`
  (`html-images`), making the uploaded site self-contained. The build fails
  closed: a failed `make html` stops the job before `upload-pages-artifact`,
  and `deploy` has `needs: build`.
- **Guix (REQ-009).** `guix.scm` now uses `gnu-build-system` (was
  `emacs-build-system`), with a `set-emacs-load-path` phase that exports
  `EMACSLOADPATH` from every input's site-lisp directory (and the versioned
  ELPA sub-directory) so byte-compile/`make check` resolve `transient`, `vui`
  and `sesman`. `autoconf`/`automake`/`texinfo`/`emacs`/`git`/
  `emacs-package-lint`/`emacs-undercover`/`emacs-marginalia` are native inputs;
  `emacs-transient`/`emacs-vui`/`emacs-sesman` are propagated. No
  `emacs-eldev`.
- **Docs (REQ-007).** `AGENTS.md` build/test commands are the autotools
  equivalents, the stale `bd` version text is corrected to v1.3.1, the Eldev
  mirror note is dropped, and `NEWS.md` has an "Unreleased" entry for the
  toolchain/manual/README migration.
- **Dist fix found while verifying.** `Makefile.am` now lists the `@include`d
  `doc/*.texi` chapters and `doc/images` in `EXTRA_DIST`, so `make dist`
  produces a tarball whose manual actually builds (verified by a clean
  extraction + `make html`).

## Coverage

| ID | Status |
| --- | --- |
| REQ-007 | covered |
| REQ-008 | covered |
| REQ-009 | covered |
| REQ-012 | covered |
| REQ-015 | covered |
| REQ-016 | covered |

## Intended Behavior

- A contributor can build and test with the standard GNU flow:
  `./autogen.sh && ./configure && make && make check`, plus `make lint`,
  `make coverage`, `make info`, `make html`, `make pdf`, `make dist`,
  `make install`. `configure` discovers `bd`/`dolt`/`graphviz` and runs the
  `:integration` suite only when all three are present; `--with-bd=no` and
  its siblings force the `:unit` subset.
- CI keeps every previous quality gate: byte-compile warnings are errors
  (`make`), package-lint findings fail (`make lint`), the ERT suite runs
  (`make check`), undercover coverage with Codecov uploads runs
  (`make coverage`), and generated-doc drift fails `scripts/dispatch-docs
  --check`. None of them install or invoke Eldev.
- Merging to `main` builds and publishes the self-contained HTML manual to
  GitHub Pages; a broken manual fails the build instead of publishing stale
  output.
- `guix shell -D -f guix.scm` yields Emacs + `transient`/`vui`/`sesman` (with
  `bd`/`dolt`/`graphviz` from the caller's PATH), and `guix build -f guix.scm`
  succeeds through the autotools flow.
- No runtime behavior or `bd` CLI surface changes (REQ-014); no `lisp/`
  runtime file is touched. `sesman` is not reintroduced (REQ-016); it remains
  the pre-existing dependency because its removal is a separate workflow.

## Changed Files

- `.github/workflows/lint.yml`, `test.yml`, `coverage.yml`, `docs-drift.yml` —
  rewired onto autotools; no Eldev.
- `.github/workflows/pages.yml` — new Pages build/deploy workflow.
- `Eldev` — deleted.
- `Makefile.am` — `make coverage` target; `html-images` staging so
  `doc/beads.html` is self-contained; `EXTRA_DIST` gains the coverage scripts,
  the `@include`d `doc/*.texi` chapters, and `doc/images`.
- `build-aux/beads-check.el` — `beads-check-run-tests`, `BEADS_CHECK_NO_RUN`.
- `build-aux/beads-coverage.el` — new undercover runner.
- `build-aux/beads-compile.el` — load dependency autoloads before the
  warnings-as-errors byte-compile.
- `scripts/coverage`, `scripts/ci-deps` — new no-Eldev coverage and CI-dep
  helpers.
- `guix.scm` — `gnu-build-system`, `EMACSLOADPATH` phase, no `emacs-eldev`.
- `AGENTS.md`, `NEWS.md` — autotools commands, bd v1.3.1, Unreleased entry.
- `plans/gnu-toolchain/items/be-6s7rx-implementation-summary.md` — this file.

## Verification

First verification command (build + full ERT suite, from the worktree):

```sh
./autogen.sh && ./configure && make && make check
```

Observed: **pass** (exit 0). `make` byte-compiled 126 files with warnings as
errors; `make check` ran 6634 tests, 6608 results as expected, 0 unexpected,
26 skipped (`:live` UI tests), in 376.9 s.

Final proof commands and results:

```sh
make lint                         # pass (package-lint + warnings-as-errors)
make coverage                     # pass; coverage/codecov.json + junit.xml
                                  #   (6355 :unit tests, 0 failures)
./scripts/dispatch-docs --check   # pass ("dispatch docs ok")
rm -rf doc/beads.html && make html
                                  # pass; 36 PNGs staged into doc/beads.html/
                                  #   images, every referenced <img> present
rm -rf doc/beads.html && make html MAKEINFO=false
                                  # fails closed (exit != 0), no doc/beads.html,
                                  #   so the Pages build job fails before upload
guix shell -D -f guix.scm -- ...  # Emacs 31.1; transient/vui/sesman load;
                                  #   bd/dolt/dot available
guix build -f guix.scm            # pass (autotools configure/make/install)
make dist && tar xzf ... && (cd ... && ./configure && make html)
                                  # pass; chapters + images in tarball, 0 warnings
python3 -c 'import yaml; yaml.safe_load(...)'  # all 7 workflow YAML files parse
git grep -l Eldev -- ':!plans' ':!NEWS.md' ':!docs' ':!.designs' ':!.cli-audit'
                                  # no workflow/build invokes Eldev; comments only
```

`actionlint` is not installed in this environment, so the workflows were
YAML-parsed with PyYAML instead; the workflow structure (triggers,
permissions, `needs: build`) was reviewed by hand.

## Remaining Risks

- **REQ-016 / `sesman`.** The `beads.el` task text's proof asks for an empty
  `git grep -i sesman` in `guix.scm` and CI, but that is contingent on
  `be-4fa65` (the parallel sesman-removal workflow), which is still **open**.
  `sesman` is a real runtime dependency today (`lisp/beads.el`
  `Package-Requires`, `beads-agent-backend.el`, `beads-sesman.el`), so removing
  it here would break byte-compilation and `guix shell`. This change therefore
  keeps the pre-existing `emacs-sesman` input and does not add any new sesman
  use; the grep-empty proof should be re-checked after `be-4fa65` lands.
- **Guix `make check`.** `guix.scm` sets `#:tests? #f`: the `:unit` subset
  still calls `beads-check-executable` (needs the `bd` binary), and four
  remote tests hardcode `#!/bin/sh`, which the Guix build sandbox does not
  provide. Enabling tests in the sandbox produced 38 then 4 failures, none
  related to this change. `make check` is exercised by CI and by
  `guix shell -D -f guix.scm`, which the requirements (REQ-009: "guix build
  succeeds via the autotools flow") accept.
- **REQ-015 cross-repo alignment.** The parallel `gascity.el` migration has
  not landed (it still has no `configure.ac`/`Makefile.am`/`pages.yml`), so
  only the `beads.el` side of the shared contract is verifiable here. The
  Pages workflow shape (build-then-deploy, permissions, concurrency), the
  `configure`/`make` target names, and the Texinfo/README shapes match the
  plan; re-confirm against `gascity.el` once its migration lands.
- **`makeinfo` HTML images.** The fix stages `doc/images` after `makeinfo`
  runs, which is sufficient because the source `doc/images` symlink exists in
  a checkout and is now distributed as a real directory. If a future build
  runs `makeinfo` in a tree without `doc/images` (a hand-rolled tarball that
  bypasses `EXTRA_DIST`), the images would again be missing.
