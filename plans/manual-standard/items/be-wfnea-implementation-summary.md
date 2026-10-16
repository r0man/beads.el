---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-rfwar
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
    - path: beads/be-wfnea
      hash: bead:be-wfnea
      ids: [REQ-003]
    - path: plans/manual-standard/requirements.md
      hash: sha256:0ffcb4eb7f97f2aef5a4416e8810c3b4da0d24f2bcbccf5640fd00a9967202a7
    - path: plans/manual-standard/implementation-plan.md
      hash: sha256:ccfb1a822c2ef5fbe69da8764e4d210ada1271c20520b6817a60bb6f96225498
    - path: plans/manual-standard/decomposition.md
      hash: sha256:015346698dc65d8143b28c77dd4eb8137b445e8896cdbf5163269f443f49f178
  coverage:
    - id: REQ-003
      status: covered
---

# Implementation Summary — be-wfnea (WI-02: Define `@shot` macro and convert illustrations)

## Summary

WI-02 gives the beads.el manual a single `@shot{NAME, CAPTION}` Texinfo macro
(mirroring `gascity.texi`) and converts every illustration to it, so HTML shows
a bounded thumbnail that opens the full-resolution image in the `doc/lightbox.js`
overlay and no HTML chapter emits an unbounded full-size `@image` (REQ-003). It
is exactly the decomposition's WI-02 (`be-wfnea`): the macro in `doc/beads.texi`
plus the 38-call conversion across the six illustrated chapter files. All work
was done in the source-anchor worktree
`/home/roman/workspace/beads.el/worktrees/be-wfnea`; the launcher checkout was
never edited.

Delivered:

- **`doc/beads.texi`** — defines `@macro shot{name, caption}` before the
  `@include` lines. `@ifhtml` emits
  `<div class="screenshot"><a class="screenshot-link" href="images/\name\.png"
  title="…"><img src="images/\name\-thumb.png" alt="…" loading="lazy"></a>
  <div class="screenshot-caption">…</div></div>`; `@ifnothtml` emits the
  thumbnail `@image` plus a centered `@emph` caption. The comment documents the
  thumbnail/lightbox contract and warns that captions must stay free of commas
  (the Texinfo macro argument separator).
- **The 38 `@image` calls collapsed to 19 `@shot` calls** — each full-size plus
  `…-thumb` pair became one `@shot{<view>-<theme>, <caption>}`, and the redundant
  full-size `@image` was dropped. The 19 calls cover 18 theme-qualified names
  (`dashboard-dark` appears in both `getting-started.texi` and `views.texi`) and
  all 14 manifest views (agents, agent-menu, swarm, formula, molecule, epic,
  dashboard, compose, list, show, dispatch, maintenance, ready, blocked).
- **No new assets** — every reference uses the existing
  `images/<view>-<theme>-thumb.png` convention and the matching CSS classes
  (`.screenshot`, `.screenshot-link`, `.screenshot-caption`) from WI-01.

No `lisp/` runtime file is touched and no `bd` CLI surface changes (the
companion REQ-017 guard is WI-13's; this item only edits `doc/*.texi`).

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-003 | covered |

## Intended Behavior

- **REQ-003 (shot macro and thumbnails).** The manual uses a `@shot`-equivalent
  macro backed by the `…-thumb` image convention on every illustrated chapter.
  In HTML the macro renders a bounded thumbnail inside a `screenshot-link`
  anchor whose `href` is the full-size image; `doc/lightbox.js` opens that
  `href` in the in-page overlay, and with JavaScript disabled the anchor still
  navigates to the full image. In Info and print the `@ifnothtml` branch renders
  a plain `@image` of the thumbnail with a centered caption, so the Info manual
  keeps its illustrations. Because the pair is collapsed, no HTML chapter emits
  an unbounded full-size `@image`; the only full-size references are anchor
  `href`s to the lightbox target.
- **Caption hygiene.** Every caption is comma-free so the two-argument Texinfo
  macro parses. Three captions from the old full-size images that contained
  commas were reworded (the swarm list, the molecule view, and the board's
  section list) with no change in meaning; the mechanical "… thumbnail."
  captions of the deleted duplicate images are gone.
- **Selector parity with WI-01.** The emitted classes and the
  `images/<view>-<theme>-thumb.png` source path are exactly the contract WI-01's
  `doc/manual.css` and `doc/lightbox.js` style and match, so
  `makeinfo --css-include`/`PRE_BODY_CLOSE` inlining (WI-03) drops in without
  further asset edits.

## Changed Files

- `doc/beads.texi` — added the `@shot` macro definition (22 added lines).
- `doc/agents.texi` — 3 `@shot` calls (agents-dark, agent-menu-dark, swarm-dark).
- `doc/formulas.texi` — 4 `@shot` calls (formula-dark, molecule-dark, epic-dark, epic-light).
- `doc/getting-started.texi` — 1 `@shot` call (dashboard-dark).
- `doc/issue-workflow.texi` — 5 `@shot` calls (compose-dark, list-dark, list-light, show-dark, show-light).
- `doc/orchestration.texi` — 2 `@shot` calls (dispatch-dark, maintenance-dark).
- `doc/views.texi` — 4 `@shot` calls (dashboard-dark, dashboard-light, ready-dark, blocked-dark).
- `plans/manual-standard/items/be-wfnea-implementation-summary.md` — this artifact.

`git diff --stat` for the item: 7 files changed, 41 insertions(+), 38 deletions(-)
— the 38 deleted lines are the removed `@image` calls.

## Verification

First verification command (worktree, before commit), Info build:

```sh
makeinfo -o /tmp/be-wfnea-build/beads.info doc/beads.texi
```

Observed: **pass** (exit 0, no macro or node errors).

Final proof command, HTML build plus the bounded-image audit:

```sh
makeinfo --html --no-split -o /tmp/be-wfnea-build/beads.html doc/beads.texi
grep -c 'class="screenshot"' /tmp/be-wfnea-build/beads.html
grep -o '<img src="images/[^"]*"' /tmp/be-wfnea-build/beads.html | grep -v -- '-thumb.png'
```

Observed: **pass** (exit 0). The HTML contains 19 `class="screenshot"` divs,
19 `class="screenshot-link"` anchors, and 19 `class="screenshot-caption"`
divs; every `<img src>` is a `-thumb.png` (the negative grep finds nothing), so
there is no unbounded full-size `@image` in the HTML. The 19 anchors point at 18
unique full-size images (dashboard-dark shared).

Additional checks, all **pass**:

- **All pairs converted.** `grep -rn '@image' doc/*.texi` finds only the two
  occurrences inside the `@shot` macro definition in `doc/beads.texi` (the
  comment and the `@ifnothtml` branch); no chapter file has a bare `@image`.
- **Manifest coverage.** For each of the 14 view names, at least one
  `@shot{<view>-dark|light` occurs:
  `agents 1, agent-menu 1, swarm 1, formula 1, molecule 1, epic 2, dashboard 3,
  compose 1, list 2, show 2, dispatch 1, maintenance 1, ready 1, blocked 1`.
- **Comma-free captions.** A search for a second top-level comma inside any
  `@shot{…}` argument finds none.
- **No new assets.** The conversion adds no file under `doc/images/`; it only
  references the committed `…-thumb.png` files.

## Remaining Risks

- **The CSS/lightbox assets are not in this worktree.** WI-01's
  `doc/manual.css` and `doc/lightbox.js` are committed on the sibling
  `be-bqc8z` worktree, not yet on the `origin/main` base this worktree was cut
  from, so a full `makeinfo --css-include` / `PRE_BODY_CLOSE` render cannot be
  exercised here. WI-03's build wiring and WI-13's CI guard own that end-to-end
  proof; this item proves the macro emits exactly the documented classes and the
  thumbnail path.
- **Info output is verified by build success, not visual diff.** `makeinfo`
  accepts the `@ifnothtml` branch and emits the thumbnail `@image`; the visual
  layout of the centered caption is WI-01's CSS and WI-06's markup pass scope.
- **Caption rewording is semantic-preserving but editorial.** The three
  comma-bearing captions were reworded to satisfy the macro parser; if a later
  review prefers different wording, it is a one-line change per call.
