---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-8q3m5
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
    - path: beads/be-w12s8
      hash: bead:be-w12s8
      ids: [REQ-005, REQ-006, REQ-009, REQ-014]
    - path: plans/gnu-toolchain/requirements.md
      hash: sha256:e10a5d3dbcbb01f0571e768d3ec222bbbf4c3bfa3b98703b8411e1857c4fcaff
    - path: plans/gnu-toolchain/implementation-plan.md
      hash: sha256:6734ce93211d72bae8bb15520a052a84f31d66c97d260426075b7bfd098d0c01
    - path: plans/gnu-toolchain/decomposition.md
      hash: sha256:f27241bd58688fd3345a55de46292040b08c462532c875ed900e95d3c6f8f90f
    - path: Eldev
      hash: sha256:5f5a1d002c90f1313c6ae42a9cc62e3ca367ed520985e1804b9e38c7b20742b7
  coverage:
    - id: REQ-005
      status: covered
    - id: REQ-006
      status: covered
    - id: REQ-009
      status: covered
    - id: REQ-014
      status: covered
---

# Implementation Summary — be-w12s8 (W1: autotools build skeleton)

## Summary

W1 adds a real GNU autotools front door for beads.el (REQ-005, REQ-006,
REQ-009, REQ-014; decomposition W1; implementation-plan WS-3, split by the
plan reviewer). It is the foundation for W2 (`make check`/`make lint`) and the
CI/Guix conversion in W10. The implementation commit is `f7fb76ba` in the
source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-w12s8`.

Delivered:

- `configure.ac` — `AC_INIT([beads.el],[0.1.0],…)`,
  `AM_INIT_AUTOMAKE([-Wall foreign])`, `AC_CONFIG_AUX_DIR([build-aux])`,
  `AC_CONFIG_MACRO_DIRS([m4])`, `AC_CONFIG_FILES([Makefile])`, `AC_OUTPUT`.
  Locates Emacs and fails with a clear message when it is missing; resolves
  `transient`/`vui` through `--with-transient=`/`--with-vui=` and/or the
  ambient `EMACSLOADPATH` (the Guix path); locates `makeinfo`/`texi2pdf`;
  detects `bd`/`dolt`/`graphviz` with `AC_PATH_PROG` plus
  `--with-bd`/`--with-dolt`/`--with-graphviz` overrides and `AM_CONDITIONAL`s
  that W2 consumes; exposes `lispdir` for the installed package. `sesman` is
  deliberately not resolved or required (REQ-016).
- `Makefile.am` — `all` byte-compiles `lisp/*.el` in one Emacs invocation with
  warnings as errors and generates `beads-autoloads.el`; `make install`
  installs `*.el`/`*.elc`, `beads-autoloads.el` and the Info manual;
  `info_TEXINFOS` gives `make info`/`html`/`pdf`; `make dist` builds a tarball
  that includes the `doc/` sources and images, `lisp/`, `scripts/`,
  `share/`, `docs/*.md`, `guix.scm` and the autotools helpers.
- `build-aux/beads-compile.el` — reproduces the byte-compile
  `--warnings-as-errors` gate. Every warning category is fatal except
  transient's `interactive-only`, which fires on the deliberately
  Lisp-invoked wrapper prefixes and depends on unrelated load order.
- `build-aux/beads-autoloads.el` — preloads `beads-meta.el` (which pulls in
  `beads-prefix.el`) before `loaddefs-generate`, reproducing the Eldev advice
  so `;;;###autoload` on `beads-defcommand`/`beads-meta-define-transient`
  forms macroexpand and the installed package keeps its `M-x beads` entry
  points.
- `autogen.sh` — `autoreconf -fi`, executable.
- `doc/beads.texi` — a minimal, valid Texinfo manual stub (GFDL copying,
  `@dircategory`/`@direntry`, `@node`/`@menu`) so `make info`, `make install`
  and `make dist` work before W3 authors the real manual.
- `.gitignore` — generated autotools/build artifacts, including
  `lisp/beads-autoloads.el` and the dist tarball.

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-005 | covered |
| REQ-006 | covered |
| REQ-009 | covered |
| REQ-014 | covered |

## Intended Behavior

- **REQ-005 (autotools front door).** `./autogen.sh` produces `configure` and
  `Makefile.in`; `configure.ac`/`Makefile.am`/`build-aux/` exist and the
  standard `./configure && make` flow works without Eldev.
- **REQ-006 (make targets).** `make` byte-compiles the package; `make info`,
  `make html` and `make pdf` build the manual; `make install` installs the
  Elisp and Info files; `make dist` produces a tarball.
- **REQ-009 (dependency resolution and Guix).** `transient`/`vui` resolve via
  `--with-*` or `EMACSLOADPATH`; `bd`/`dolt`/`graphviz` are discovered and
  exposed as `HAVE_BD`/`HAVE_DOLT`/`HAVE_GRAPHVIZ` conditionals for W2;
  `sesman` is not reintroduced (REQ-016). The Guix path is documented in
  `configure.ac`.
- **REQ-014 (no behavior change).** No `lisp/` file is modified; only build
  files, a manual stub and `.gitignore` are added. `git diff --stat` against
  the source anchor shows no runtime or CLI change.

## Changed Files

- `configure.ac` (new, 110 lines) — the autotools configuration.
- `Makefile.am` (new, 87 lines) — byte-compile, autoloads, install, manual
  and dist rules.
- `autogen.sh` (new, executable) — `autoreconf -fi`.
- `build-aux/beads-compile.el` (new) — warnings-as-errors byte-compile helper.
- `build-aux/beads-autoloads.el` (new) — autoload generation with the
  `beads-meta` preload.
- `m4/.gitkeep` (new) — keeps the registered macro directory in git.
- `doc/beads.texi` (new) — minimal manual stub replaced by the manual
  workstream.
- `.gitignore` (+42) — generated autotools/build artifacts.
- `plans/gnu-toolchain/items/be-w12s8-implementation-summary.md` — this
  artifact.

## Verification

First verification command (clean build from the worktree):

```sh
./autogen.sh && ./configure && make
```

Observed: **pass** (exit 0). `autoreconf -fi` generated `configure` and
`Makefile.in`; `configure` found Emacs 31.1, `transient`/`vui` via
`EMACSLOADPATH`, and `bd`/`dolt`/`dot`; `make` reported
`Byte-compiled 126 file(s)`, generated `lisp/beads-autoloads.el` (contains the
`M-x beads` entry point, `(autoload 'beads "beads-dashboard" …)`), and built
`doc/beads.info`.

Additional checks, all **pass**:

- `make install DESTDIR=/tmp/stage` → exit 0; `/tmp/stage` contains 126
  `*.elc`, `lisp/beads-autoloads.el` and `usr/local/share/info/beads.info`
  (with an `install-info` dir entry).
- `make html` → exit 0; `doc/beads.html/index.html` exists.
- Injected warning: adding a temporary `lisp/beads-zzz-warning-probe.el` with
  an unused lexical variable made `make` fail (exit 2) with
  `Error: Unused lexical variable 'unused-variable'`; the probe file was
  removed afterwards.
- `./configure --with-emacs=/nonexistent/emacs` → exit 1 with
  `configure: error: Emacs is required but was not found.`
- `git diff --stat` shows no `lisp/` source change (REQ-014).

Final proof command (distribution round-trip):

```sh
make dist && tar xzf beads.el-0.1.0.tar.gz -C /tmp/beads-dist \
  && cd /tmp/beads-dist/beads.el-0.1.0 && ./configure && make
```

Observed: **pass** (exit 0). `make dist` produced
`beads.el-0.1.0.tar.gz` (2.2 MB); the tarball contains `configure`,
`Makefile.in`, `aclocal.m4`, `build-aux/beads-compile.el`,
`build-aux/beads-autoloads.el`, `m4/`, `doc/beads.texi`, `docs/` with images,
`lisp/`, `scripts/dispatch-docs`, `share/`, and `guix.scm`; the clean
extraction configured and built (126 files, autoloads and `doc/beads.info`
regenerated).

## Remaining Risks

- **`make pdf` needs TeX.** The `pdf` target exists and uses `TEXI2PDF`
  (`texi2pdf`), but this environment has no TeX engine, so `make pdf` fails
  with texi2pdf's "you'll need to install TeX" message. W10's
  `guix.scm`/CI conversion must provide TeX or keep pdf out of the default
  gate; `make info`/`html` are unaffected.
- **`interactive-only` excluded from the gate.** `build-aux/beads-compile.el`
  makes all byte-compile warnings fatal except `interactive-only`, because
  transient marks every generated prefix interactive-only while wrapper
  commands such as `beads-dep-add`/`beads-update` intentionally call theirs
  from Lisp, and whether the warning fires depends on unrelated load order.
  This is a deliberate, documented narrowing of the historical
  `--warnings-as-errors` gate; unused variables and undefined functions are
  still fatal (verified with an injected unused variable).
- **`doc/beads.texi` is a stub.** It exists only so `info`/`install`/`dist`
  have a source; W3–W5 replace its contents, and W8 retargets the generated
  documentation blocks. The build must not assume the manual is final.
- **In-tree build assumed.** The byte-compile and manual rules use
  `$(abs_top_srcdir)` and write `.elc` next to the sources; VPATH/out-of-tree
  builds are not yet exercised. The proof and CI use in-tree builds.
- **Concurrent build workers.** The shared worktree briefly carried partial
  autotools artifacts from a concurrent builder (a `doc/Makefile.am`,
  generated files, and lisp warning annotations) that were not part of this
  change. They were removed; the committed tree contains only the files
  listed above. (The annotations were reverted, not adopted.)
