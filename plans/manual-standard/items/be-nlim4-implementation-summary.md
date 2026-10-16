---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-rcwly
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
    - path: beads/be-nlim4
      hash: bead:be-nlim4
      ids: [REQ-007]
    - path: plans/manual-standard/requirements.md
      hash: sha256:0ffcb4eb7f97f2aef5a4416e8810c3b4da0d24f2bcbccf5640fd00a9967202a7
    - path: plans/manual-standard/implementation-plan.md
      hash: sha256:ccfb1a822c2ef5fbe69da8764e4d210ada1271c20520b6817a60bb6f96225498
    - path: plans/manual-standard/decomposition.md
      hash: sha256:015346698dc65d8143b28c77dd4eb8137b445e8896cdbf5163269f443f49f178
    - path: doc/beads.texi
      hash: git:90beed1e329ddc7c0e4a4f26aa925e78abc34bee
    - path: doc/maintenance.texi
      hash: sha256:78a748dbfc2c45889d1f2aa9f1092c4d2bdd534db8444faad24282592900d23d
    - path: doc/orchestration.texi
      hash: sha256:513c39d7b4e252abbee2d2015f2a750c9f0e3ee76e2ea17a9e6d64894661adca
    - path: doc/concepts.texi
      hash: sha256:262fbc63df8f93ed77bdba65cbafad917d79ee927ad4d85dbc263579c840c2e0
    - path: doc/getting-started.texi
      hash: sha256:c4bbed496e305646d4dc6d6bd279b318b251ecf2d7ad1efb45737662f87bc237
  coverage:
    - id: REQ-007
      status: covered
---

# Implementation Summary — be-nlim4 (WI-06: Idiomatic Texinfo markup pass)

## Summary

WI-06 is the idiomatic Texinfo markup pass over the `beads.el` user
manual requested by REQ-007. The manual already used `@deffn`,
`@deftypefn`, `@kbd`, `@code`, `@var`, the index commands, `@menu` and
`@node`; the remaining gap was the definition form for user options,
which were written with the generic `@defvr {User Option}` form and the
only definition command REQ-007 names that the manual did not use.

The pass makes two changes:

- **User options now use `@defopt`.** The twelve real user options
  (`defcustom`s) in `doc/maintenance.texi` and `doc/orchestration.texi`
  are defined with `@defopt`, the standard Texinfo command for
  `defcustom`. `@defopt` still emits the variable-index entry, so the
  Command and Variable Index keeps a point for every option.
- **Definition forms now match the object kind.** Two names that were
  mislabelled `@defvr {User Option}` are not user options at all:
  `beads-issue-id-regexp` is a `defconst` and `beads-menu-providers` is a
  `defvar`. Both are now `@defvr {Variable}`. `beads-store-directory`
  stays `@defvr {Buffer-Local Variable}` and
  `beads-thing-toggle-functions` stays `@defvr {Variable}`.
- **Cross-references use the sentence-appropriate command.** Three
  in-sentence `see @ref{...}` pairs became `@pxref{...}` (parenthetical or
  after a semicolon) and a leading "See @ref{...}" became `@xref{...}`.
  The manual now uses `@ref`, `@pxref` and `@xref` in their idiomatic
  positions.

No prose meaning, chapter structure, menu, or `@node` changed. The GFDL
`@copying`/`@insertcopying` block is untouched (`doc/beads.texi` is not
modified at all by this item). No `lisp/` runtime file and no `bd` CLI
surface changed (REQ-017).

| ID | Status |
| --- | --- |
| REQ-007 | covered |

## Intended Behavior

- **REQ-007 (idiomatic Texinfo).** Every user option in the manual is
  introduced with `@defopt`; every variable and buffer-local variable
  with `@defvr`; commands and functions with `@deffn`/`@deftypefn`;
  keystrokes with `@kbd`; code and metasyntactic names with `@code` and
  `@var`; index points with `@cindex`, `@fnindex` and `@vindex`; and
  cross-references with `@ref`, `@pxref` and `@xref`. The chapter tree is
  carried by `@menu`/`@node` and the license block is the verbatim GFDL
  `@copying`/`@insertcopying`.
- **Definition kind matches the source object.** `@defopt` for the
  `defcustom` names, `@defvr {Variable}` for `defvar`/`defconst`
  names. `beads-issue-id-regexp` and `beads-menu-providers` are no longer
  presented as customizable options.
- **Index completeness is preserved.** Because `@defopt` and `@defvr`
  both emit variable-index entries (merged into the function index by
  `@syncodeindex vr fn`), all twelve options keep an entry in the
  "Command and Variable Index".
- **Build stays clean and C-h i still works.** `makeinfo` emits the same
  `beads.info` with no missing-node or undefined-reference warnings, and
  the `@dircategory`/`@direntry` block that lets `C-h i` find the manual
  is unchanged.

## Changed Files

- `doc/maintenance.texi` — 11 `@defvr {User Option}` definitions became
  `@defopt`; `beads-issue-id-regexp` became `@defvr {Variable}`.
- `doc/orchestration.texi` — `beads-terminal-backend` became `@defopt`;
  `beads-menu-providers` became `@defvr {Variable}`.
- `doc/concepts.texi` — `(see @ref{...})` became `(@pxref{...})`.
- `doc/getting-started.texi` — two `see @ref{...}` became `@pxref{...}`
  and one leading "See @ref{...}" became `@xref{...}`.
- `plans/manual-standard/items/be-nlim4-implementation-summary.md` —
  this artifact.

`git diff --stat` for the four manual files: 4 files changed, 32
insertions(+), 32 deletions(-). `doc/beads.texi` is byte-for-byte
unchanged.

## Verification

First verification command (worktree, Info build):

```sh
makeinfo -o /tmp/be-nlim4-build/beads.info doc/beads.texi
```

Observed: **pass** (exit 0, no errors, no warnings).

Final proof command (worktree, HTML + Info build plus the markup/index
audit):

```sh
rm -rf /tmp/be-nlim4-build && mkdir -p /tmp/be-nlim4-build \
  && makeinfo -o /tmp/be-nlim4-build/beads.info doc/beads.texi \
  && makeinfo --html --no-split -o /tmp/be-nlim4-build/beads.html doc/beads.texi \
  && python3 - <<'PY'
import glob, re
src = {f: open(f, encoding='utf-8').read() for f in glob.glob('doc/*.texi')}
alltext = '\n'.join(src.values())
assert '@defvr {User Option}' not in alltext
defined = set(re.findall(r'^@defopt\s+([a-z0-9-]+)', alltext, re.M))
required = {'beads-executable','beads-actor','beads-remote-search-path',
 'beads-remote-transport','beads-remote-ssh-options',
 'beads-remote-ssh-control-path','beads-show-async',
 'beads-show-async-timeout','beads-dashboard-section-limit',
 'beads-command-async-max-concurrent','beads-issue-id-prefixes',
 'beads-terminal-backend'}
assert not (required - defined), sorted(required - defined)
for name in ('beads-issue-id-regexp','beads-menu-providers'):
    assert re.search(r'^@defvr \{Variable\} '+re.escape(name)+r'\b', alltext, re.M)
for cmd in ('@deffn','@deftypefn','@defvr','@defopt','@kbd','@code','@var',
            '@cindex','@fnindex','@vindex','@pxref','@xref','@menu','@node'):
    assert cmd in alltext, cmd
assert not re.search(r'(?i)\bsee\s+@ref\{', alltext)
assert not re.search(r'(?i)\(\s*see\s+@ref\{', alltext)
info = open('/tmp/be-nlim4-build/beads.info', encoding='utf-8', errors='replace').read()
for name in sorted(required):
    assert re.search(r'^\*\s+'+re.escape(name)+r':', info, re.M), name
texi = src['doc/beads.texi']
assert 'GNU Free Documentation License, Version 1.3' in texi
assert '@copying' in texi and '@insertcopying' in texi
nodes = set(re.findall(r'^@node\s+(.+?)\s*$', alltext, re.M))
menu = re.search(r'@menu(.*?)@end menu', texi, re.S).group(1)
missing = [m.group(1) for m in
           (re.match(r'\*\s+([^:]+)::', line) for line in menu.strip().splitlines())
           if m and m.group(1) not in nodes]
assert not missing, missing
print('markup ok:', len(defined), '@defopt definitions;', len(required), 'indexed user options')
print('structure ok:', len(nodes), 'nodes')
PY
```

Observed: **pass** (exit 0). The HTML and Info builds succeed with no
warnings; no `@defvr {User Option}` remains; all twelve `@defopt` names
have Command and Variable Index entries; `@deffn`, `@deftypefn`, `@defvr`,
`@defopt`, `@kbd`, `@code`, `@var`, `@cindex`, `@fnindex`, `@vindex`,
`@pxref`, `@xref`, `@menu` and `@node` are all used; no non-idiomatic
`see @ref{...}` remains; the GFDL block is intact; and every Top-menu
entry resolves to a node. The audit prints
`markup ok: 12 @defopt definitions; 12 indexed user options` and
`structure ok: 101 nodes`.

Additional checks, all **pass**:

- `scripts/dispatch-docs --check` — prints `dispatch docs ok` (the
  generated regions in `doc/orchestration.texi` are untouched).
- `emacs --batch -Q -L lisp -L lisp/test -l
  lisp/test/beads-manual-test.el -f ert-run-tests-batch-and-exit` —
  15/15 tests pass, including `beads-manual-texinfo-has-no-org-image-links`
  and the manifest/image/page guards.
- `git diff --quiet HEAD -- doc/beads.texi` — `doc/beads.texi` is
  unchanged, so the GFDL `@copying`/`@insertcopying` block is byte for
  byte identical to the base.

## Remaining Risks

- **WI-07 owns the full reference inventory.** Several `defcustom`s
  (for example `beads-list-default-limit`, `beads-formula-scope`,
  `beads-agent-curated-backends`) are only mentioned in prose with a
  `@vindex`; this item did not add new `@defopt` definitions beyond the
  options already defined in the Customizing and Terminal chapters. The
  reference-completeness pass (WI-07) and its gate (WI-08) remain the
  owners of "every `defcustom` has a `@defopt`".
- **Duplicate index points are unchanged.** Commands such as
  `beads-dispatch`, `beads-maintenance`, `beads-list-mode` and
  `beads-thing-forward` are defined in more than one task chapter, so the
  index lists each with multiple points. That duplication predates this
  item and is left for the reference chapter to consolidate.
- **Illustrations are still the WI-02 `@image` pairs.** This worktree is
  cut from `origin/main` (9663a093) plus WI-05 (90beed1e), so the
  `@shot`/thumbnail conversion (REQ-003, WI-02/WI-09) is not in this
  base and was deliberately not touched.
