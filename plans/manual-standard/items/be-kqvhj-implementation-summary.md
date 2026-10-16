---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-rg2m2
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
    - path: beads/be-kqvhj
      hash: bead:be-kqvhj
      ids: [REQ-002, REQ-014]
    - path: plans/manual-standard/requirements.md
      hash: sha256:0ffcb4eb7f97f2aef5a4416e8810c3b4da0d24f2bcbccf5640fd00a9967202a7
    - path: plans/manual-standard/implementation-plan.md
      hash: sha256:ccfb1a822c2ef5fbe69da8764e4d210ada1271c20520b6817a60bb6f96225498
    - path: plans/manual-standard/decomposition.md
      hash: sha256:015346698dc65d8143b28c77dd4eb8137b445e8896cdbf5163269f443f49f178
    - path: gascity.el/doc/Makefile.am
      hash: git:6a2953527dd6e2f53e1d4ec7c75a634724cedc09
    - path: doc/manual.css
      hash: sha256:999ef2757ea6a0d6cad72b84df3f5482f86026f00dda6f81f79e73970f5470b7
    - path: doc/lightbox.js
      hash: sha256:5ddbfb3f98e1e95b6e323175acd3695a6514997f6911eee2714e638c8b6c6180
    - path: doc/Makefile.am
      hash: sha256:53f6d5f1a118c4ce85f6eca533c0383fde1c0f326e62d8df6451951a35755405
    - path: Makefile.am
      hash: sha256:f139e7bbd17c93d1497a3d74f457939a64bb30c33296ae89f9be71388e6c2188
    - path: configure.ac
      hash: sha256:60e5a17ae9ad4dc09ad9d20faacfb906b9e369fffa571dad9b061dfc638c0b19
  coverage:
    - id: REQ-002
      status: covered
    - id: REQ-014
      status: covered
---

# Implementation Summary — be-kqvhj (WI-03: Add `doc/Makefile.am` and rewire the top-level build)

## Summary

WI-03 moves the Texinfo manual build out of the top-level `Makefile.am` into a
dedicated `doc/Makefile.am`, exactly as the decomposition's WI-03
(`be-kqvhj`) specifies (REQ-002) and so the manual is a self-contained,
styled artifact of the distribution and the published Pages site (REQ-014).
The change is build-system only: no `lisp/` runtime file and no `bd` CLI
surface is touched (REQ-017).

Delivered:

- **`doc/Makefile.am` (new)** — modelled on `gascity.el/doc/Makefile.am`:
  - `info_TEXINFOS = beads.texi`.
  - `beads_TEXINFOS` lists `manual.css`, `lightbox.js`, the seven
    `@include`d chapter files, the generated includes
    (`$(sort $(wildcard $(srcdir)/generated/*.texi))`) and the screenshot
    PNGs (`$(sort $(wildcard $(srcdir)/images/*.png))`); `EXTRA_DIST`
    distributes them.
  - `AM_MAKEINFOHTMLFLAGS` inlines the stylesheet with `--css-include` and
    the lightbox through `PRE_BODY_CLOSE`, producing a self-contained,
    split multi-page HTML tree.
  - `html-local` mirrors the **symlinked** `doc/images` into
    `beads.html/images` with `cp -RL` (a bare `cp -R` would copy the
    symlink itself and yield broken images — the plan-review F1 correction).
  - `clean-local` removes the built HTML tree and PDF.
- **`Makefile.am`** — adds `SUBDIRS = doc`; removes `info_TEXINFOS`, the
  image-staging `html-local`/`HTML_IMAGES` block, the `doc/*.texi` and
  `doc/images` entries from `EXTRA_DIST`, and the two `doc/beads.*` lines
  from `clean-local`. The lisp byte-compile/autoload/check/lint/coverage
  rules are untouched.
- **`configure.ac`** — emits `doc/Makefile` (`AC_CONFIG_FILES([Makefile
  doc/Makefile])`) and adds `-Wno-portability`, matching
  `gascity.el`'s `AM_INIT_AUTOMAKE` because `doc/Makefile.am` expands its
  image and generated-include lists with GNU make `sort`/`wildcard`.

Two deliberate, documented deviations from the decomposition's literal
snippet, both required to keep the build correct:

1. The seven `@include`d chapter files are listed in `beads_TEXINFOS`. The
   snippet omitted them; Automake does not parse `@include`, so without
   them `make dist` would ship a `beads.texi` whose chapters are missing
   and the extracted-tarball build would fail.
2. `-Wno-portability` is added to `AM_INIT_AUTOMAKE` (mirroring the
   `gascity.el` model) so `./autogen.sh` stays warning-free while the
   snippet's `$(sort $(wildcard ...))` form is kept.

The source-anchor worktree `/home/roman/workspace/beads.el/worktrees/be-kqvhj`
was based on `origin/main`; because WI-03 depends on WI-01 for
`doc/manual.css` and `doc/lightbox.js`, the branch fast-forwards to WI-01's
`163e7df1` before the WI-03 commit, so the worktree is self-contained and
buildable. The implementation commit is `d7ddf60a`; the launcher checkout was
never edited.

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-002 | covered |
| REQ-014 | covered |

## Intended Behavior

- **REQ-002 (`doc/Makefile.am` exists and the manual still builds).**
  `doc/Makefile.am` owns the manual through `info_TEXINFOS = beads.texi`.
  `./autogen.sh && ./configure && make info && make html && make pdf`,
  `make -C doc html`, `make dist`, an extracted-tarball build and
  `make install` all succeed, and the built `doc/beads.html` path is
  unchanged so `.github/workflows/pages.yml` still uploads it. The
  top-level `Makefile.am` no longer owns `info_TEXINFOS` or `html-local`.
- **REQ-014 (self-contained, reproducible HTML).** `AM_MAKEINFOHTMLFLAGS`
  inlines `manual.css` and the lightbox `<script>` into every content page
  and `html-local` copies the PNGs into `beads.html/images`, so the Pages
  output needs no external stylesheet, script, or image fetch. `make dist`
  ships `doc/Makefile.am`, the stylesheet, the lightbox, the chapters and
  the 36 PNGs, and a tarball build reproduces the same output.
- **No behavior change (REQ-017).** No `lisp/*.el` file and no command
  class, transient, `defcustom`, or CLI slot is modified; the change is
  confined to `Makefile.am`, `configure.ac` and the new `doc/Makefile.am`.

## Changed Files

- `doc/Makefile.am` (new, 66 lines) — the manual's Autotools rules:
  `info_TEXINFOS`, `beads_TEXINFOS`/`EXTRA_DIST`, the CSS/lightbox
  inlining, the `cp -RL` image mirror, and the HTML/PDF cleanup.
- `Makefile.am` (modified) — `SUBDIRS = doc`; drops `info_TEXINFOS`, the
  doc `EXTRA_DIST` entries, the image-staging `html-local` and the doc
  `clean-local` lines; `.PHONY` drops `html-local`.
- `configure.ac` (modified) — `AC_CONFIG_FILES([Makefile doc/Makefile])`
  and `-Wno-portability` in `AM_INIT_AUTOMAKE`.
- `plans/manual-standard/items/be-kqvhj-implementation-summary.md` — this
  artifact.

Dependency content carried in the branch (WI-01, commit `163e7df1`), not
authored by this item: `doc/manual.css`, `doc/lightbox.js`,
`doc/README.md` and WI-01's summary.

## Verification

First verification command (worktree, after the build-system edit):

```sh
./autogen.sh && ./configure
```

Observed: **pass** (exit 0). `autoreconf` emitted no warnings (the
`-Wno-portability` change removes the `sort`/`wildcard` portability
warning), `configure` created both `Makefile` and `doc/Makefile`, and it
found `bd`, `dolt` and `graphviz`.

Build targets, all **pass**:

- `make info` — builds `doc/beads.info` through `doc/Makefile.am`.
- `make html` — `makeinfo --html --css-include=./manual.css
  --set-customization-variable "PRE_BODY_CLOSE=<script>$(cat
  ./lightbox.js)</script>"`, then `cp -RL ./images beads.html/images`.
- `make pdf` — succeeds with `texlive-scheme-basic texlive-epsf`
  (`make pdf` alone needs TeX, which is not on the default PATH; the
  `.texi.pdf` rule is unchanged from the old top-level build).
  Output: `doc/beads.pdf`, 70 pages.
- `make -C doc html` — the documented subdirectory entry point works.
- `make dist` — produces `beads.el-0.1.0.tar.gz`; the tarball contains
  `doc/Makefile.am`, `doc/Makefile.in`, `manual.css`, `lightbox.js`,
  all seven chapter files, `beads.texi`, and 36 `doc/images/*.png`.
- Extracted-tarball build (`tar xzf … && ./configure && make info &&
  make html && make pdf`) — **pass**; a forced `rm doc/beads.info && make
  info` rebuilds it from source.
- `make install DESTDIR=/tmp/beads-install` — **pass**; installs
  `share/info/beads.info` and runs `install-info`, alongside the lisp
  files.
- `make clean` — **pass**; removes `doc/beads.html` and `doc/beads.pdf`
  (automake keeps the distributed `beads.info`, as before).

Final proof command (worktree and extracted tarball):

```sh
make html && grep -c 'max-width: 100%' doc/beads.html/index.html \
  && grep -c '__beadsLightbox' doc/beads.html/index.html \
  && ls doc/beads.html/images/*.png | wc -l
```

Observed: **pass** — the built page contains the inlined stylesheet
(`max-width: 100%`), the inlined lightbox script (`__beadsLightbox`), and
`images/` is a real directory holding all 36 PNGs; `grep` for
`link rel="stylesheet"` and `<script src` returns 0, so nothing is fetched
externally. The same checks pass on the extracted-tarball build.

## Remaining Risks

- **The `-Wno-portability` change is project-wide.** It was added because
  the instructed `$(sort $(wildcard ...))` form is a GNU make extension and
  the `gascity.el` model disables the same warning; the alternative
  (expanding the lists in `configure.ac` like the lisp lists) was rejected
  to keep the decomposition's `beads_TEXINFOS` shape. A future GNU make
  function in the top-level `Makefile.am` would no longer be reported.
- **`make pdf` needs a TeX installation.** The rule is the same automake
  `.texi.pdf` rule the top-level used, so this is not a regression; the
  local proof used `guix shell texlive-scheme-basic texlive-epsf`. CI that
  builds the PDF must provide the equivalent TeX packages (and
  `texlive-epsf` for the PNG `@image` support).
- **WI-02/WI-05/WI-07 are not yet merged.** The current manual still uses
  bare `@image` pairs and has no `doc/generated/` includes; the
  `$(wildcard $(srcdir)/generated/*.texi)` term is empty today and picks up
  generated tables when WI-10 adds them. The build rewire does not depend
  on those items landing.
- **`doc/README.md` and the WI-01 assets are carried by the WI-01 commits**
  in this branch. If the finalize merges WI-01 and WI-03 separately, the
  identical blobs resolve cleanly; if it only takes WI-03's own commit, the
  assets still arrive with WI-01.
