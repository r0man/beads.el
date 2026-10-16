# beads.el manual assets

This directory holds the Texinfo sources of the beads.el user manual plus the
two web-presentation assets that the HTML build inlines:

- `manual.css` — the page styling, injected into every page's `<head>` with
  `makeinfo --css-include`.
- `lightbox.js` — the in-page screenshot lightbox, injected at the bottom of
  every page through the `PRE_BODY_CLOSE` customization variable.

Both are progressive enhancements: without JavaScript the screenshot anchor
still navigates to the full-resolution image. Neither file references any
external network resource, web font, or third-party script; the CSS data-URI
magnifier badge and the script are the whole dependency set, so the built
site is self-contained.

## Provenance

`manual.css` and `lightbox.js` are ported and adapted from the sibling
`gascity.el` manual so the two projects present as one documentation family
(REQ-001). They were taken from `gascity.el` at revision
`6a2953527dd6e2f53e1d4ec7c75a634724cedc09` (`gascity.el/doc/manual.css`,
`gascity.el/doc/lightbox.js`).

## Deliberate differences from gascity.el

Keeping the two stylesheets close is the point; the following differences are
intentional and the only ones. A `diff -u` of each file against the
`gascity.el` original shows exactly these lines.

1. **Product name, copyright, and provenance header.** `gascity.el` / "Gas
   City Contributors" become `beads.el` / "Beads Contributors", and each file
   records that it was ported from `gascity.el` and points here.
2. **Product-colour note in `manual.css`.** The palette itself is unchanged:
   the Guix/PostgreSQL-style neutral chrome is shared on purpose (REQ-001).
   beads.el has no separate web palette, and its faces, colours, and buffer
   names live in the Emacs screenshots embedded by the `@shot` macro, not in
   this stylesheet. The header comment states this so a reader does not
   expect a beads-specific recolour.
3. **Explicit bounded-image rule (REQ-003).** `manual.css` adds a combined
   `.screenshot img, img { max-width: 100%; height: auto; }` rule. gascity.el
   bounds only `div.screenshot img`; beads.el additionally bounds every
   image, so an illustration in a chapter that still uses a bare `@image`
   cannot exceed the content column. The committed thumbnails are 520px wide
   (narrower than the 54rem content column), so `max-width: 100%` leaves them
   at their natural size on a wide screen and shrinks them on a narrow one.
   The former `max-width`/`height` declarations were removed from the more
   specific `div.screenshot img` block; its decoration is otherwise unchanged.
4. **Lightbox globals renamed.** `window.__gascityLightbox` becomes
   `window.__beadsLightbox` so the two manuals cannot collide if a page ever
   includes both scripts, and the comment points at the `@shot` macro in
   `beads.texi`.

No other rule, selector, or behaviour differs. In particular the CSS
selectors used by the `@shot` macro — `.screenshot`, `.screenshot-link`,
`.screenshot-caption` (plus `.lightbox-*`) — are unchanged, so the same
macro markup works in both manuals.