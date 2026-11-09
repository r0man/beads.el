---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-rtqoj
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
    - path: beads/be-k8nn0
      hash: bead:be-k8nn0
      ids: [REQ-007]
    - path: plans/manual-demo-consistency/requirements.md
      hash: sha256:5a665190a49fa226ab49e221ddb10aa108f236b9a0e18e3b8079b8213ff3fd21
    - path: plans/manual-demo-consistency/implementation-plan.md
      hash: sha256:dd851b8ba3f345740b22622682f215a2467c16a38589fdb62698af98a6d10ecd
    - path: doc/beads.texi
      hash: sha256:a94cf627469a267c33bd75946ef4a596c57241c95888a727fb287f05c5c1b8f3
    - path: doc/Makefile.am
      hash: sha256:1c97d5ae80d213f04ceb4f378e9548fb3db16ac5744d5046595382542b1c84fd
    - path: doc/maintenance.texi
      hash: sha256:2d8453038a5dce456d6eb3d605242187e3c356c63966360f1f4ce4b757a87715
  coverage:
    - id: REQ-007
      status: covered
---

# Implementation Summary: W4 complete, warning-free Texinfo structure

## Summary

Work item W4 (implementation plan WS-4) delivers REQ-007: the Texinfo manual
must have a complete node/menu tree, every `@xref`/`@ref`/`@pxref`/`@anchor`
target must resolve, and a clean checkout must build `make info`,
`make html`, and (when the TeX toolchain exists) `make pdf` with zero
warnings.

The tree already satisfied that structure at the branch base
(`origin/main` d38a8726): the earlier restructure (`be-4juqo`) and markup
pass (`be-nlim4`) had left one `@node` per `@include`d chapter, a Top menu
in include order, section-accurate chapter menus, unique anchors, and
resolvable cross-references. This item therefore did not rewrite chapters.
It ran the full audit anyway, found exactly one real gap in the
*PDF* half of the requirement, and fixed it: `build-aux/texinfo.tex`
aborts any `@image` unless `epsf.tex` is on `TEXINPUTS`, so `make pdf`
fails on a TeX installation that omits that file (for example a minimal
`texlive-scheme-basic`), and neither the manual nor `doc/Makefile.am`
recorded that dependency. The `make pdf` table entry in
`doc/maintenance.texi` now states the requirement, and `doc/Makefile.am`
documents it next to the PDF rule, so a reader can build the optional PDF
instead of guessing at a TeX error.

No `.texi` chapter content, `@node`, `@menu`, `@anchor`, `@xref`, `@shot`,
`@syncodeindex`, GFDL block, or direntry changed. No beads.el runtime code
changed.

## Intended Behavior

- **Node/menu audit (unchanged, verified).** Every chapter `@include`d by
  `doc/beads.texi` (`introduction`, `installation`, `getting-started`,
  `issue-workflow`, `views`, `formulas`, `agents`, `orchestration`,
  `maintenance`, `reference`, `concepts`) opens with a `@node` followed by
  its `@chapter`/`@top`, the Top menu names the chapters in include order,
  the Index nodes close the menu, and every chapter/section menu lists
  exactly that node's child nodes. The audit over all 12 `.texi` files
  reports 102 nodes, 0 unlisted nodes, 0 multiply-listed nodes, 0 duplicate
  node names, 0 nodes without a sectioning command, and 0 menu entries that
  do not name a node.
- **Cross-references (unchanged, verified).** 65 `@xref`/`@ref`/`@pxref`
  forms resolve to a node or to one of the 36 unique `@anchor`s; no anchor
  is defined twice, and `makeinfo` reports no "unresolved cross-reference"
  or "node not found" warning in Info or HTML.
- **Clean-checkout build (verified).** `make clean && ./autogen.sh &&
  ./configure && make info && make html` exits 0 with zero warnings; the
  split HTML tree and `doc/beads.info` are rebuilt. The `make dist`
  tarball ships `build-aux/texinfo.tex` through automake's automatic
  `TEXINFO_TEX` distribution and builds the identical manual.
- **PDF dependency documented (new).** The `make pdf` row in
  `doc/maintenance.texi` now says the target is optional, needs a working
  `@TeX{}` installation that provides `epsf.tex`, names the
  `texlive-epsf` package as the minimal fix, and notes that the repeated
  cross-reference pass proves every `@xref`-family target resolves in
  print. `doc/Makefile.am` carries the same note in the PDF comment so the
  maintainer sees it where the build rule lives. `make pdf` then succeeds
  with `texlive-scheme-basic` + `texlive-epsf`: 123 pages, 0 undefined
  cross-references, 0 warnings.
- **No structural churn.** `@shot` (20 uses), `@syncodeindex vr fn`, the
  GFDL `@copying` block, the `@dircategory`/`@direntry` pair, and the
  merged Command and Variable Index are untouched.

### Coverage

| ID | Status |
| --- | --- |
| REQ-007 | covered |

## Changed Files

| File | sha256 | Change |
| --- | --- | --- |
| `doc/maintenance.texi` | `2d8453038a5dce456d6eb3d605242187e3c356c63966360f1f4ce4b757a87715` | `make pdf` table item documents the optional TeX + `epsf.tex` requirement, the `texlive-epsf` package, and the repeated cross-reference pass; no node, menu, anchor, or other content changed. |
| `doc/Makefile.am` | `1c97d5ae80d213f04ceb4f378e9548fb3db16ac5744d5046595382542b1c84fd` | PDF comment records that `build-aux/texinfo.tex` aborts without `epsf.tex` when `@image` is used, and that `make info`/`make html` do not need it. Distribution inputs unchanged. |

Every other `doc/*.texi` file and `doc/beads.texi` is unchanged from the
branch base; `doc/beads.texi` still hashes
`sha256:a94cf627469a267c33bd75946ef4a596c57241c95888a727fb287f05c5c1b8f3`.

## Verification

First verification command (the plan's manual-structure gate, run before any
edit, from the worktree `/home/roman/workspace/beads.el/worktrees/be-k8nn0`
after removing the untracked `doc/beads.info` and `doc/beads.html`):

```
make clean && ./autogen.sh && ./configure && make info && make html
```

Observed: **PASS** — every step exit 0 and the build logs contain **zero**
warning/error lines. This confirmed the node/menu tree was already complete
at the branch base, so the item's source change is limited to the PDF
dependency documentation.

Supporting audit (run from `doc/`, over `beads.texi` plus every
`@include`d chapter): a script walk of `@node`, `@menu`/`@end menu`,
`@xref`/`@ref`/`@pxref`, and `@anchor` reported 102 nodes, 102 listed in a
menu, 36 unique anchors, 65 refs, and **no** unlisted node, duplicate node,
multi-listed node, node without a sectioning command, dangling menu entry,
duplicate anchor, or unresolved reference. `makeinfo -c
CHECK_NORMAL_MENU_STRUCTURE=1` adds no warning either, under the profile's
Texinfo 6.8 and the Guix Texinfo 7.3.

Bead proof commands (all from the worktree, after the final edit):

| Proof | Command | Observed |
| --- | --- | --- |
| clean Info + HTML, zero warnings | `make clean && ./autogen.sh && ./configure && make info && make html` | exit 0; 0 warning/error lines; `doc/beads.info` and split `doc/beads.html/` rebuilt |
| PDF with TeX + epsf | `guix shell make texinfo texlive-scheme-basic texlive-epsf -- make pdf` | exit 0; **123 pages**, `doc/beads.pdf` 674938 bytes; 0 warning/error lines; 0 "Undefined cross reference" in `beads.log` |
| PDF without epsf fails as documented | `guix shell make texinfo texlive-scheme-basic -- make pdf` | exit 2 with `epsf.tex not found, images will be ignored` / `@epsf.tex must be installed` — the documented gap |
| distributed sources build the same manual | `make dist`, extract to a temp dir, `./configure && make info && make html`, then `make pdf` under the same shell | exit 0 for all; 0 warning/error lines; PDF **123 pages**, 674938 bytes (same as the worktree) |
| forbidden-name gate | `make check-manual` | exit 0 |
| dispatch tables still in sync | `scripts/dispatch-docs --check` | exit 0, `dispatch docs ok` |
| manual `:unit` suite | `emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-test.el --eval '(ert-run-tests-batch-and-exit (quote (tag :unit)))'` | exit 0, `Ran 16 tests, 16 results as expected, 0 unexpected` |
| lint | `make lint` | exit 0, `Byte-compiled 129 file(s)` (only the pre-existing third-party `sly` autoload warnings) |

Final proof command (the project gate, run after the last source edit):

```
make check
```

Observed: **PASS** — exit 0,
`Ran 6685 tests, 6659 results as expected, 0 unexpected, 26 skipped`
(the 26 skips are the pre-existing `:live` suite). `make check` runs
`check-manual` first, so the forbidden-name gate is green inside it too.

## Remaining Risks

- **Structure was already complete.** REQ-007's node/menu/xref half was
  satisfied by the earlier manual restructure, not by this item. A reviewer
  expecting chapter edits should read the audit evidence above; the only
  defect this item found and fixed is the undocumented PDF `epsf.tex`
  dependency. If that is judged too small, the work is still a correct
  "verified, no gap" outcome rather than a skipped audit.
- **PDF is not a CI gate.** `.github/workflows/{lint,test,pages}.yml` build
  Info and HTML only; the TeX toolchain is optional by plan design
  (`Open Question 4`). A future `build-aux/texinfo.tex` refresh, or a TeX
  package split that moves `epsf.tex`, can change the PDF requirement again;
  the note in `doc/Makefile.am` and the manual is where to re-check it.
- **PDF is not byte-reproducible.** Successive runs embed different
  creation metadata, so `cmp` on two PDFs differs even though page count
  (123) and byte length (674938) match. Only Info and HTML are claimed
  byte-reproducible.
- **Local TeX profile caveat.** TeX binaries that exist in a bare Guix
  profile without formats fail with `I can't find the format file
  'tex.fmt'`; the measured PDF proof uses `guix shell texlive-scheme-basic
  texlive-epsf`, which supplies formats. This is an environment property,
  not a manual defect.
- **Branch base vs sibling drain items.** This worktree is `origin/main`
  d38a8726 and does not contain the WS-1/WS-2 siblings (`be-rtqte`,
  `be-95bl3`), which edit `capture.sh`, the screenshot README, and the four
  `-light` captions in `doc/views.texi`, `doc/issue-workflow.texi`, and
  `doc/formulas.texi`. This item touches `doc/maintenance.texi` and
  `doc/Makefile.am` only, so the refinery merge should not conflict textually.
- **Attempt 1.** No repair loop ran: `gc.attempt` is 1 and the first
  recorded clean gate passed. The 0-warning property is asserted by
  observation (build logs), not by an automated warnings-as-errors target;
  wiring the manual build into `make check` is WS-8's test/gate sweep, not
  this item's file boundary.
- **Transient external rewrite observed during the full suite.** One
  `make check` run saw four `doc/*.texi` files rewritten with
  menu-entry descriptions (`* Title: Node.  Description.`) by a process
  outside this item and outside the repository's own code (no file in
  `lisp/`, `scripts/`, or `build-aux/` reads `@node` or writes a menu
  entry). The rewrite is content-only and was reverted; the committed
  tree was re-verified afterwards (`git diff` shows only these two files,
  plus this summary), and the final clean Info/HTML/PDF build and audit
  above ran on exactly that tree. If a sibling stage or harness tool
  intentionally normalizes menus, it should do so on its own branch and
  commit the result, not mutate a running worktree.
