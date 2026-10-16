---
schema: gc.build.implementation-summary.v1
workflow:
  id: be-mprvk
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
    - path: beads/be-bqc8z
      hash: bead:be-bqc8z
      ids: [REQ-001, REQ-014]
    - path: plans/manual-standard/requirements.md
      hash: sha256:0ffcb4eb7f97f2aef5a4416e8810c3b4da0d24f2bcbccf5640fd00a9967202a7
    - path: plans/manual-standard/implementation-plan.md
      hash: sha256:ccfb1a822c2ef5fbe69da8764e4d210ada1271c20520b6817a60bb6f96225498
    - path: plans/manual-standard/decomposition.md
      hash: sha256:015346698dc65d8143b28c77dd4eb8137b445e8896cdbf5163269f443f49f178
    - path: gascity.el/doc/manual.css
      hash: git:6a2953527dd6e2f53e1d4ec7c75a634724cedc09
    - path: gascity.el/doc/lightbox.js
      hash: git:6a2953527dd6e2f53e1d4ec7c75a634724cedc09
    - path: doc/manual.css
      hash: sha256:999ef2757ea6a0d6cad72b84df3f5482f86026f00dda6f81f79e73970f5470b7
    - path: doc/lightbox.js
      hash: sha256:5ddbfb3f98e1e95b6e323175acd3695a6514997f6911eee2714e638c8b6c6180
    - path: doc/README.md
      hash: sha256:fe27911888d808967fdea91324f80e066cae8f9f682e4459ac0a9a46218024e4
  coverage:
    - id: REQ-001
      status: covered
    - id: REQ-014
      status: covered
---

# Implementation Summary — be-bqc8z (WI-01: Port gascity presentation assets)

## Summary

WI-01 ports the `gascity.el` manual presentation assets into `beads.el` so the
two manuals present as one documentation family (REQ-001) and so the later
`@shot`/HTML work (WI-02, WI-03) has a styled, self-contained home in the
published Pages output (REQ-014). It is exactly the decomposition's WI-01
(`be-bqc8z`): `doc/manual.css`, `doc/lightbox.js`, and a deliberate-differences
record in `doc/README.md`. The assets were committed in the source-anchor
worktree `/home/roman/workspace/beads.el/worktrees/be-bqc8z` (asset commit
`e3a8ccf2`; the README refinement lands in the same item's follow-up commit),
and the launcher checkout was never edited.

Delivered:

- **`doc/manual.css`** — the `gascity.el` stylesheet, ported and retargeted to
  beads.el (product name, copyright, provenance header). It keeps the Guix
  layout, typography, `@code`/`@verbatim` blocks, admonitions, table of
  contents, and screenshot/lightbox rules, and it adds an explicit
  **bounded-image rule** (`.screenshot img, img { max-width: 100%; height:
  auto; }`, REQ-003) plus a `max-width: 520px` cap on the `.screenshot`
  wrapper — the capture pipeline's `THUMB_W`. A preview therefore never
  exceeds the content column, renders at its natural size on a wide screen and
  shrinks on a narrow one, and an illustration that still uses a bare `@image`
  is bounded by the same rule.
- **`doc/lightbox.js`** — the `gascity.el` lightbox, ported behaviour-for-
  behaviour except for the product/provenance header, the
  `window.__beadsLightbox` guard rename, and the `beads.texi` cross-reference.
  Clicking a `.screenshot-link` opens the full-resolution `href` in an in-page
  overlay; the close button, `Esc`, and a backdrop click dismiss it; without
  JavaScript the anchor navigates to the image. No external dependencies and
  no build step.
- **`doc/README.md`** — records the deliberate differences from `gascity.el`
  (retargeted product name/comments, the explicit bounded-image rule, the
  explicit `.screenshot-link` selector, the intentionally unchanged
  palette/typography for visual parity, and the repo's no-SPDX header style)
  plus the `.screenshot` / `.screenshot-link` / `.screenshot-caption` selector
  contract, so a reviewer can confirm the `diff` contains nothing else.

No `lisp/` runtime file is touched and no `bd` CLI surface changes (REQ-017);
the palette is deliberately the shared neutral Guix chrome rather than a
beads-specific recolour.

Requirement coverage:

| ID | Status |
| --- | --- |
| REQ-001 | covered |
| REQ-014 | covered |

## Intended Behavior

- **REQ-001 (styling parity assets).** `doc/manual.css` and `doc/lightbox.js`
  exist, are ported/adapted from `gascity.el`, and render the published HTML
  visually equivalent to `gascity.el` in layout, typography, code/verbatim
  styling, admonitions, table of contents, screenshot presentation, and
  lightbox behaviour. The CSS selectors the `@shot` macro will emit
  (`.screenshot`, `.screenshot-link`, `.screenshot-caption`) and the lightbox
  classes are unchanged, so WI-02's macro and WI-03's `makeinfo` inlining drop
  in without further asset edits. Every deliberate difference is recorded in
  `doc/README.md`.
- **REQ-014 (published Pages output).** The assets are self-contained: the CSS
  has no web fonts, no `@import`, and no network `url()` (the only `url()` is
  an inline SVG `data:` URI), and the JS references no external script.
  `makeinfo --css-include` and `PRE_BODY_CLOSE` (wired in WI-03) can therefore
  inline them into every page with no asset-copy step, so the Pages site picks
  up the styled, functional presentation on merge.
- **No behavior change (REQ-017).** No `lisp/*.el` file is modified; the change
  is documentation assets plus their provenance record.

## Changed Files

- `doc/manual.css` (new, 308 lines / 9761 bytes) — the beads.el stylesheet,
  including the explicit bounded-image rule and the 520px preview cap.
- `doc/lightbox.js` (new, 145 lines / 5583 bytes) — the dependency-free
  screenshot lightbox.
- `doc/README.md` (new, 49 lines / 2675 bytes) — provenance and the
  deliberate-differences record.
- `plans/manual-standard/items/be-bqc8z-implementation-summary.md` — this
  artifact.

## Verification

First verification command (worktree, after the asset commit):

```sh
node --check doc/lightbox.js
```

Observed: **pass** (exit 0, no syntax error).

Additional checks, all **pass**:

- **Only documented differences.** `diff -u
  /home/roman/workspace/gascity.el/doc/manual.css doc/manual.css` and the same
  for `lightbox.js` show only the changes `doc/README.md` records: the
  product/copyright/provenance header, the explicit bounded-image rule and
  520px wrapper cap (`box-sizing: border-box` added so the cap includes the
  preview's padding/border), the explicit `.screenshot-link` selector, and the
  `__beadsLightbox` rename. No other rule, selector, or behaviour differs.
- **`@shot` selector contract.** `grep -nE
  '\.screenshot(-link|-caption)?\b' doc/manual.css` finds `.screenshot`,
  `.screenshot-link` (anchor, hover/focus and badge rules), and
  `.screenshot-caption`; the JS matches `a.screenshot-link` and
  `.screenshot-caption`.
- **No external network/asset dependency.** `grep -nE 'https?://|@import'`
  finds only the W3C XML namespace string inside the inline SVG `data:` URI of
  the magnifier badge — not a fetch; there is no `@import`, web font, remote
  URL, or external script.
- **Well-formed sources.** The braces/quotes balance in both files, and
  `node --check doc/lightbox.js` accepts the script.
- **No runtime change.** `git diff --stat -- lisp/` is empty across the item's
  commits; only `doc/` files were added.

Final proof command (HTML inlining, run inside the worktree's `doc/`):

```sh
makeinfo --html --no-split \
  --css-include=manual.css \
  --set-customization-variable "PRE_BODY_CLOSE=<script>$(cat lightbox.js)</script>" \
  -o /tmp/beads-html-test/beads.html beads.texi
```

Observed: **pass** (exit 0). The generated page contains the inlined
stylesheet (the bounded-image rule, `.screenshot-link` and `.lightbox-overlay`
rules are present) and the inlined script inlined once (`__beadsLightbox`), with
no external stylesheet or script reference. This proves the two assets are
consumable by the `makeinfo` inlining WI-03 will wire up.

## Remaining Risks

- **The assets are not yet exercised end-to-end.** WI-02 defines the `@shot`
  macro and WI-03 adds `doc/Makefile.am` with the `--css-include` /
  `PRE_BODY_CLOSE` inlining, so the full `make html` content pipeline cannot be
  run in this wave (the current `beads.texi` still uses bare `@image`). The
  `makeinfo` inlining proof above and the no-dependency audit are the strongest
  checks available at this point; WI-03 owns the full build proof.
- **Palette intentionally unchanged.** REQ-001 asks for visual parity with
  `gascity.el`, so no beads-specific recolour was introduced. If a beads.el web
  palette is later defined, it must be added here and the divergence recorded
  in `doc/README.md`; for now the product colours live in the Emacs
  screenshots, not the stylesheet.
- **Lightbox global rename is defensive.** Renaming `window.__gascityLightbox`
  to `window.__beadsLightbox` only matters if both manuals' scripts ever share
  a page; it changes nothing about single-page behaviour.
