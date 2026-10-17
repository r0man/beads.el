# beads.el manual build assets

Small, self-contained presentation assets for the Texinfo user manual in this
directory.  Both are ported from the sibling `gascity.el` manual so the two
manuals render equivalently (REQ-001); this file records the deliberate
differences required by that requirement.

## Files

- **`manual.css`** — the manual's stylesheet.  Inlined into every HTML page by
  `makeinfo --css-include` from `doc/Makefile.am`, so the published HTML needs
  no external stylesheet.  No web fonts and no external assets.
- **`lightbox.js`** — progressive enhancement for the manual's screenshots:
  clicking a `.screenshot-link` opens the full-resolution image in an in-page
  overlay (Esc, the backdrop, or the close button dismiss it).  Inlined into
  every page through the `PRE_BODY_CLOSE` customization variable.  Without
  JavaScript the anchor navigates to the image, the original behaviour.

Both files are referenced by the `@shot` macro defined in `beads.texi`, which
renders a bounded `…-thumb.png` preview wrapped in a link to the full PNG.  The
class contract is:

| Class | Element | Role |
|-------|---------|------|
| `.screenshot` | `div` | the preview wrapper |
| `.screenshot-link` | `a` | the anchor to the full-resolution image |
| `.screenshot-caption` | `div` | the caption under the preview |

## Deliberate differences from `gascity.el`

1. **Product name and comments** retargeted to beads.el.  The lightbox guard
   global is `window.__beadsLightbox` (gascity's was `window.__gascityLightbox`)
   and the `@shot` cross-reference points at `beads.texi`.
2. **Bounded illustrations (REQ-003).**  `manual.css` adds an explicit
   `.screenshot img, img { max-width: 100%; height: auto; }` rule and caps the
   `.screenshot` wrapper at the capture pipeline's thumbnail width
   (`THUMB_W = 520px` in `docs/manual/screenshots/capture.sh`).  gascity relies
   on `div.screenshot img` alone; the bare `img` clause is the safety net for an
   illustration rendered outside the macro.
3. **Explicit `.screenshot-link` selector.**  The anchor rules use
   `div.screenshot a.screenshot-link` rather than gascity's generic
   `div.screenshot a`, so the stylesheet names every class of the `@shot`
   contract.
4. **Layout, typography, colours, code/verbatim blocks, admonitions, table of
   contents and lightbox styling are intentionally unchanged** for visual
   parity with `gascity.el` (REQ-001).  beads.el has no separate HTML accent
   palette; the shared Guix-inspired light palette is the standard.
5. **No SPDX line** in the file headers, matching the rest of the beads.el
   repository; the copyright holder is `Beads Contributors`.
