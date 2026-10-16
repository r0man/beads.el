---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-fqogu
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
    - path: beads/be-4juqo
      hash: bead:be-4juqo
      ids: [REQ-006, REQ-010]
    - path: plans/manual-standard/requirements.md
      hash: sha256:0ffcb4eb7f97f2aef5a4416e8810c3b4da0d24f2bcbccf5640fd00a9967202a7
    - path: plans/manual-standard/implementation-plan.md
      hash: sha256:ccfb1a822c2ef5fbe69da8764e4d210ada1271c20520b6817a60bb6f96225498
    - path: plans/manual-standard/decomposition.md
      hash: sha256:015346698dc65d8143b28c77dd4eb8137b445e8896cdbf5163269f443f49f178
    - path: doc/beads.texi
      hash: git:9663a0939744a73997e2294d81bc436dbd58783b
  coverage:
    - id: REQ-006
      status: covered
    - id: REQ-010
      status: covered
---

# Implementation Summary — be-4juqo (WI-05: Restructure manual menu and chapters)

## Summary

WI-05 restructures the beads.el Texinfo manual to the Guix/PostgreSQL
standard shape required by REQ-006. The top-level `@menu` and `@node` tree
now opens with `Introduction` and `Installation and Requirements`, keeps
`Getting Started` as the tutorial, lists the six task chapters, adds a
`Command and Variable Reference` chapter, and closes with a `Concepts`
terminology chapter before the two indexes. The manual stays a single
Texinfo source: `doc/*.texi` are the only manual files and the Org
pipeline is not reintroduced (REQ-010).

All work was done in the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-4juqo`; the launcher checkout
was never edited.

Delivered:

- **`doc/beads.texi`** — the Top menu and `@include` list now name the
  introduction, installation, and concepts chapters plus the reference
  chapter, in the same order as the includes. The GFDL
  `@copying`/`@insertcopying` block and `VERSION` are unchanged.
- **`doc/introduction.texi`** (new) — `Introduction` chapter with the
  product overview, a `Reading This Manual` section that maps the manual,
  and the `Related Resources` list moved out of `getting-started.texi`.
- **`doc/installation.texi`** (new) — `Installation and Requirements`
  chapter with `Requirements`, `Installing from MELPA`, `Installing from
  Source`, and `Initial Setup`, moved verbatim from `getting-started.texi`.
- **`doc/concepts.texi`** (new) — `Concepts` chapter with the `Stores`,
  `Issues`, `Dependency Model`, and `Molecules and Formulas` terminology,
  moved from `getting-started.texi`.
- **`doc/reference.texi`** (new) — `Command and Variable Reference`
  chapter shell with `Command Reference` and `User Option Reference`
  sections; it carries the chapter's purpose and cross-references so the
  menu/node tree is consistent (the WI-07 inventory fills it).
- **`doc/getting-started.texi`** — trimmed to the tutorial
  (`Your First Issue`) plus the shared interaction patterns
  (`Interface Concepts` and its subsections); its former intro,
  installation, concepts, and related-resources material now lives in the
  new chapters.
- **`Makefile.am`** — added the four new `doc/*.texi` files to
  `EXTRA_DIST` so `make dist` carries them.
- **`NEWS.md`** — an `Unreleased` entry describing the new chapter
  structure.

No runtime or `bd` CLI change (REQ-017 is WI-13's guard); only `doc/`
sources, their distribution list, and NEWS are touched.

| ID | Status |
| --- | --- |
| REQ-006 | covered |
| REQ-010 | covered |

## Intended Behavior

- **REQ-006 (manual structure).** The manual presents the
  Guix/PostgreSQL-standard sequence: an Overview/Introduction; Installation
  and requirements; the Getting Started tutorial; the task chapters
  (Issue Workflow; Dashboards and Views; Formulas, Molecules and Epics;
  Agents and Sling; Orchestration and Terminal; Maintenance and
  Extending); a Command and Variable Reference; and a
  Concepts/terminology chapter. Each is a distinct top-level `@node` and
  `@chapter`, reachable from the Top menu in reading order.
- **REQ-010 (single Texinfo source of truth).** `doc/beads.texi` remains
  the master file and every chapter is an `@include`d `.texi`. No Org
  manual file is added and no Org-to-Texinfo pipeline is reintroduced. The
  chapter-splitting is purely a reorganisation of the existing Texinfo
  prose; no content is dropped and the GFDL `@copying` block is byte for
  byte unchanged.
- **Consistent `@menu`/`@node` tree.** Every entry in the Top menu
  resolves to exactly one `@node`, the menu order matches the `@include`
  order and the generated Info `Next`/`Prev` chain, and `makeinfo` emits no
  errors or warnings.
- **Cross-references stay valid.** Nodes that moved
  (`Molecules and Formulas`, `Stores`, `Issues`, `Dependency Model`,
  `Requirements`, `Related Resources`) keep their names, so existing
  `@ref`/`@pxref` targets such as `@pxref{Interface Concepts}`,
  `@pxref{Store Scoping}`, and `@ref{Molecules and Formulas}` still
  resolve.

## Changed Files

- `doc/beads.texi` — rewritten Top menu and `@include` list.
- `doc/introduction.texi` — new (overview, reading guide, related resources).
- `doc/installation.texi` — new (requirements and installation).
- `doc/concepts.texi` — new (terminology).
- `doc/reference.texi` — new (Command and Variable Reference chapter shell).
- `doc/getting-started.texi` — reduced to the tutorial and interface concepts.
- `Makefile.am` — new chapters added to `EXTRA_DIST`.
- `NEWS.md` — Unreleased entry.
- `plans/manual-standard/items/be-4juqo-implementation-summary.md` — this artifact.

`git diff --numstat` for the edits to pre-existing tracked files
(`Makefile.am`, `NEWS.md`, `doc/beads.texi`, `doc/getting-started.texi`):
40 insertions(+), 189 deletions(-). The four new chapter files and this
summary are added by the item commit.

## Verification

First verification command (worktree, Info build):

```sh
makeinfo -o /tmp/be-4juqo-build/beads.info doc/beads.texi
```

Observed: **pass** (exit 0, no errors, no warnings).

Final proof command (worktree, HTML build plus the structure and
menu/node audit):

```sh
makeinfo --html --no-split -o /tmp/be-4juqo-build/beads.html doc/beads.texi \
  && makeinfo --no-split -o /tmp/be-4juqo-build/beads.info doc/beads.texi \
  && python3 - <<'PY'
import re, glob
nodes = set()
for f in glob.glob('doc/*.texi'):
    for line in open(f):
        m = re.match(r'@node\s+(.*)', line)
        if m:
            nodes.add(m.group(1).strip())
menu = re.search(r'@menu(.*?)@end menu', open('doc/beads.texi').read(), re.S).group(1)
missing = [m.group(1).strip() for m in
           (re.match(r'\*\s+([^:]+)::', line) for line in menu.strip().splitlines())
           if m and m.group(1).strip() not in nodes]
assert not missing, missing
required = ['Introduction', 'Installation and Requirements', 'Getting Started',
            'Issue Workflow', 'Dashboards and Views', 'Formulas Molecules and Epics',
            'Agents and Sling', 'Orchestration and Terminal',
            'Maintenance and Extending', 'Command and Variable Reference', 'Concepts']
assert all(n in nodes for n in required), [n for n in required if n not in nodes]
assert not glob.glob('doc/*.org'), 'Org manual file reintroduced'
print('structure ok:', len(nodes), 'nodes')
PY
```

Observed: **pass** (exit 0). The HTML build succeeds; every Top-menu entry
resolves to a node; all 11 required top-level chapters are present; no
`doc/*.org` file exists; the audit prints `structure ok: 101 nodes`.

Additional checks, all **pass**:

- `emacs --batch -Q -L lisp -L lisp/test -l lisp/test/beads-manual-test.el -f
  ert-run-tests-batch-and-exit` — 15/15 tests pass, including
  `beads-manual-manifest-views-have-pages` and
  `beads-manual-manifest-views-have-shots`, which proves the illustrated
  chapters still resolve after the moves.
- `scripts/dispatch-docs --check` — prints `dispatch docs ok` (the
  generated dispatch/maintenance marked regions in `doc/orchestration.texi`
  are untouched).
- `git diff doc/beads.texi` shows the `@copying`/`@insertcopying` block
  unchanged.

## Remaining Risks

- **`doc/reference.texi` is a chapter shell, not the finished reference.**
  WI-05 owns the *structure* (REQ-006); the command/option inventory is
  WI-07's (`be-50rac`) deliverable, which fills `Command Reference` and
  `User Option Reference`. The shell keeps the menu/node tree valid so the
  manual builds in the meantime. If the reviewer prefers WI-05 to leave the
  file out entirely and let WI-07 add both the file and the menu include,
  this is a one-line revert plus deleting the file.
- **WI-02's `@shot` conversion is not in this worktree's base.**
  `prepare-worktree` cut the worktree from `origin/main` (9663a093), which
  predates `be-wfnea`. The illustrations here are therefore still the
  original `@image` pairs; this item deliberately does not touch them and
  moves the surrounding prose only. Merging after WI-02 is a mechanical
  rebase over the moved chapter files.
- **`Makefile.am` overlaps WI-03.** WI-03 (`be-kqvhj`) rewires the manual
  build into `doc/Makefile.am`; both items edit `EXTRA_DIST`. After WI-03
  lands, the four new chapter files must also be listed in
  `doc/Makefile.am`'s `EXTRA_DIST` (`$(beads_TEXINFOS)`-style). The new
  files are recorded here so the merge/rebase cannot silently drop them
  from the distributed tarball.
