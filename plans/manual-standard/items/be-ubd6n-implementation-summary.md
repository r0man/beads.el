---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-2sveh
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
    - path: beads/be-ubd6n
      hash: bead:be-ubd6n
      ids: [REQ-015, REQ-016]
    - path: plans/manual-standard/requirements.md
      hash: sha256:0ffcb4eb7f97f2aef5a4416e8810c3b4da0d24f2bcbccf5640fd00a9967202a7
    - path: plans/manual-standard/implementation-plan.md
      hash: sha256:ccfb1a822c2ef5fbe69da8764e4d210ada1271c20520b6817a60bb6f96225498
    - path: plans/manual-standard/decomposition.md
      hash: sha256:015346698dc65d8143b28c77dd4eb8137b445e8896cdbf5163269f443f49f178
  coverage:
    - id: REQ-015
      status: covered
    - id: REQ-016
      status: covered
---

# Implementation Summary — be-ubd6n (WI-11: Offline `burningswell` gate and sanitization)

## Summary

WI-11 is the decomposition's offline gate for the forbidden host/city name
(`be-ubd6n`, WI-11, REQ-015/REQ-016). It adds `scripts/check-manual-forbidden`,
a dependency-free POSIX `sh` script that scans the manual sources
(`doc/*.texi`), the manual build inputs (`docs/manual/`, excluding the committed
PNG binaries under `images/`), and the generated `doc/beads.html` when present,
case-insensitively for `burningswell`. It prints each offending `path:line` and
exits non-zero; a clean tree exits 0 and prints nothing. The gate is wired into
`make check` as a `check-manual` prerequisite of `check-local` and runs as its
own step in the Docs drift workflow, and `lisp/test/beads-manual-test.el` gains
three `:unit` tests that prove the script passes on the tree and fails on an
injected token in both a chapter source and a build input (while ignoring
binary `images/`).

Sanitization was already complete on the origin/main base this worktree was cut
from: `grep -ri burningswell doc/ docs/` is empty, so no `doc/*.texi`,
`docs/manual/` input, or `docs/qa/` note needed an edit. The only remaining
tree-wide occurrences are non-manual and left untouched by design: the
maintainer address in `configure.ac`'s `AC_INIT` and the owner string in a
`lisp/test/beads-types-test.el` fixture; neither is a manual source or build
input and REQ-017 forbids unrelated `lisp/` runtime churn. All work was done in
the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-ubd6n`; the launcher checkout was
never edited.

Delivered:

- **`scripts/check-manual-forbidden`** — builds a file list with `find` (top-level
  `doc/*.texi`, all of `docs/manual/` except `*/images/*`, plus `doc/beads.html`
  when it exists), then greps each file with `grep -IinH`. `-I` skips binaries,
  `-i` is case-insensitive, `-n`/`-H` produce the required `path:line`, and a
  `mktemp` list plus an `EXIT`/signal trap keeps it clean.
- **`Makefile.am`** — adds the script to `EXTRA_DIST`, a dependency-free
  `check-manual` target, `check-local: check-manual` so `make check` runs it
  first, and `check-manual` in `.PHONY`.
- **`.github/workflows/docs-drift.yml`** — a new "Check manual for forbidden host
  name" step that runs the script after checkout, before the Emacs/deps setup, so
  the gate is visible and fast in CI even though `make check` also runs it.
- **`lisp/test/beads-manual-test.el`** — a `beads-manual-test--forbidden` defconst
  and three `:unit` tests: `beads-manual-forbidden-gate-passes-tree`,
  `beads-manual-forbidden-gate-detects-injected`, and
  `beads-manual-forbidden-gate-detects-build-input`. The injected-tree tests copy
  the real script into a temp root (so its script-relative root resolution is
  exercised) and assert non-zero exit plus `path:line` output.

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-015 | covered |
| REQ-016 | covered |

## Intended Behavior

- **REQ-015 (no burningswell in docs).** The checked-in `doc/*.texi` manual
  sources and the `docs/` tree contain no `burningswell`/`burningswell.com`
  token (verified by the gate itself and by `grep -ri burningswell doc/ docs/`,
  both empty). Manual examples stay neutral (`bright-lights`, `hello-world`,
  generic bead ids); this item neither introduces nor reintroduces any host
  name, it only adds the enforcement so a future edit cannot.
- **REQ-016 (offline gate).** `scripts/check-manual-forbidden` fails,
  printing `path:line`, whenever `burningswell` reappears case-insensitively in
  a chapter source, a manual build input under `docs/manual/`, or the generated
  HTML. It is offline (no network, no database, no Emacs), so `make check` and
  the CI step can run it unconditionally. Binary screenshots under
  `docs/manual/images/` are excluded: they are regenerated artifacts, not
  greppable manual text.
- **REQ-017 (no product behavior change).** Only the test file under `lisp/` is
  touched; no `lisp/*.el` runtime file and no `bd` CLI surface changes. The gate
  runs as a build/CI check, not as product behavior.
- **Gate failure semantics.** A clean tree exits `0` with empty output; any hit
  exits `1` after printing every offending line and a one-line diagnostic on
  stderr. The test harness and the Makefile/CI steps rely on exactly that
  contract.

## Changed Files

- `scripts/check-manual-forbidden` — new offline grep gate (executable).
- `Makefile.am` — `EXTRA_DIST` entry, `check-manual` target,
  `check-local: check-manual`, and `.PHONY` update.
- `.github/workflows/docs-drift.yml` — new CI step running the gate.
- `lisp/test/beads-manual-test.el` — defconst + three `:unit` tests.
- `plans/manual-standard/items/be-ubd6n-implementation-summary.md` — this artifact.

`git diff --stat` for the item: 4 tracked files changed plus 1 new script. The
`doc/`, `docs/manual/`, and `docs/qa/` trees are unchanged because they were
already clean.

## Verification

First verification command (worktree, before commit), the new gate and its
injected-token behavior:

```sh
./scripts/check-manual-forbidden; echo "exit=$?"
```

Observed: **exit=0** (clean tree, no output).

Final proof command, the ERT `:unit` subset plus the tree grep:

```sh
emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-test.el \
  --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'
grep -ri burningswell doc/ docs/; echo "grep-exit=$?"
```

Observed: **pass** — 14/14 `:unit` tests in `beads-manual-test.el` pass,
including the three new `beads-manual-forbidden-gate-*` tests; `grep-exit=1`
(no matches). `make check-manual` (after `./autogen.sh && ./configure`) prints
`scripts/check-manual-forbidden` and exits 0, proving the Automake wiring.

Additional checks, all **pass**:

- **Injected token fails.** A temp tree built by the tests with a case-variant
  `Burningswell.example` in `doc/beads.html` and `burningswell` in
  `docs/manual/screenshots/fixtures/x.jsonl` makes the gate print
  `doc/beads.html:1:visit Burningswell.example` and exit non-zero; a
  `burningswell` byte sequence planted in `docs/manual/images/x.png` is
  ignored.
- **Build-input failure names the line.**
  `docs/manual/screenshots/fixture.sh:1:` is reported for an injected comment.
- **Whole manual test file.** All 14 `:unit` tests in
  `lisp/test/beads-manual-test.el` pass; the file byte-compiles cleanly with
  `byte-compile-error-on-warn`.
- **POSIX shell.** The script uses only `find`, `grep`, `mktemp`, `printf`,
  and shell builtins; no bashisms.

## Remaining Risks

- **Binary false negatives/positives.** The gate deliberately skips
  `docs/manual/images/` (PNG binaries). If the forbidden token were embedded in
  a screenshot image it would not be caught; REQ-015/REQ-016 scope the gate to
  manual text and generated HTML, and screenshots are regenerated from the
  sanitized fixture, so this is accepted.
- **`doc/beads.html` is optional.** The generated HTML is scanned only when it
  exists. CI's `make check` path builds the Info manual but not HTML, so the CI
  coverage of generated HTML comes from whatever tree state is present. The
  Texinfo sources are always scanned, and any token in them would also land in
  the HTML; the Pages/docs workflows own the full HTML build.
- **Non-manual occurrences remain.** `configure.ac` and a `lisp/test` fixture
  still contain the maintainer address/owner string. They are out of the
  manual gate's scope and left for REQ-017 reasons; broadening the gate to the
  whole repo would fail on them and is intentionally not done.
- **`make check` was not run end-to-end here.** The manual gate and its
  Automake target were verified directly; the full integration ERT suite is
  CI's job, and this item changes no runtime code that the suite exercises.
