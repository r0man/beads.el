/* doc/lightbox.js --- In-page lightbox for the beads.el HTML manual.
 *
 * Copyright © 2026 Beads Contributors
 *
 * Progressive enhancement for the manual's screenshots.  Each shot renders
 * (see the @shot macro in beads.texi) as
 *
 *   <div class="screenshot">
 *     <a class="screenshot-link" href="images/NAME.png" ...>
 *       <img src="images/NAME-thumb.png" ...></a>
 *     <div class="screenshot-caption">CAPTION</div>
 *   </div>
 *
 * Without JavaScript a click follows the anchor to the full-resolution
 * PNG -- the manual's original behaviour, and the reason the markup stays
 * a real <a href>.  With JavaScript we intercept the click and show that
 * full image in an overlay above the current page instead, so the reader
 * never navigates away.  The overlay closes on the close button, the Esc
 * key, or a click on the backdrop.
 *
 * No dependencies and no build step: makeinfo inlines this file verbatim
 * into the bottom of every page through the PRE_BODY_CLOSE customization
 * variable (see doc/Makefile.am), the script counterpart to the CSS that
 * `--css-include' inlines into every <head>.  The result is self-contained
 * HTML with no external script asset to copy or to break.
 *
 * Ported from the sibling `gascity.el/doc/lightbox.js' (REQ-001).  The
 * behaviour is identical; only the product name, the guard global and the
 * cross-reference to the manual source were retargeted to beads.el.
 * Deliberate differences are recorded in doc/README.md.
 */
(function () {
  "use strict";

  /* Defensive no-op on double inclusion.  The script is inlined once per
   * page, but re-evaluating it must never build two overlays. */
  if (window.__beadsLightbox) { return; }
  window.__beadsLightbox = true;

  var overlay, image, caption, closeButton, lastFocused;

  /* Build the overlay lazily on first use and reuse it thereafter, so a
   * page with no screenshot clicks adds nothing to the DOM. */
  function buildOverlay() {
    overlay = document.createElement("div");
    overlay.className = "lightbox-overlay";
    overlay.hidden = true;
    overlay.setAttribute("role", "dialog");
    overlay.setAttribute("aria-modal", "true");
    overlay.setAttribute("aria-label", "Screenshot viewer");

    var figure = document.createElement("figure");
    figure.className = "lightbox-figure";

    image = document.createElement("img");
    image.className = "lightbox-image";
    image.alt = "";

    caption = document.createElement("figcaption");
    caption.className = "lightbox-caption";

    figure.appendChild(image);
    figure.appendChild(caption);

    closeButton = document.createElement("button");
    closeButton.type = "button";
    closeButton.className = "lightbox-close";
    closeButton.setAttribute("aria-label", "Close");
    closeButton.textContent = "\u00D7";    /* a multiplication sign, the close "x" */

    overlay.appendChild(figure);
    overlay.appendChild(closeButton);
    document.body.appendChild(overlay);

    /* A click on the backdrop -- but not on the figure or the image --
     * closes.  The close button has its own handler below. */
    overlay.addEventListener("click", function (event) {
      if (event.target === overlay) { close(); }
    });
    closeButton.addEventListener("click", close);
  }

  function open(href, alt, captionText) {
    if (!overlay) { buildOverlay(); }
    lastFocused = document.activeElement;
    image.setAttribute("src", href);
    image.setAttribute("alt", alt || "");
    if (captionText) {
      caption.textContent = captionText;
      caption.hidden = false;
    } else {
      caption.textContent = "";
      caption.hidden = true;
    }
    overlay.hidden = false;
    document.body.classList.add("lightbox-open");
    closeButton.focus();
    document.addEventListener("keydown", onKeydown);
  }

  function close() {
    if (!overlay || overlay.hidden) { return; }
    overlay.hidden = true;
    document.body.classList.remove("lightbox-open");
    image.removeAttribute("src");                /* release the decoded image */
    document.removeEventListener("keydown", onKeydown);
    if (lastFocused && typeof lastFocused.focus === "function") {
      lastFocused.focus();                       /* restore the reader's place */
    }
  }

  function onKeydown(event) {
    if (event.key === "Escape" || event.key === "Esc") {
      event.preventDefault();
      close();
    } else if (event.key === "Tab") {
      /* Minimal focus trap: the close button is the only focusable thing
       * in the modal, so keep focus pinned to it. */
      event.preventDefault();
      closeButton.focus();
    }
  }

  /* One delegated listener covers every screenshot on the page and keeps
   * working after lazily-loaded thumbnails appear. */
  document.addEventListener("click", function (event) {
    var target = event.target;
    var link = target && target.closest ?
        target.closest("a.screenshot-link") : null;
    if (!link) { return; }
    /* Respect the browser's own gestures: modifier- and middle-clicks
     * (open in a new tab) fall through to the real anchor navigation. */
    if (event.defaultPrevented || event.button !== 0 ||
        event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) {
      return;
    }
    event.preventDefault();
    var thumb = link.querySelector("img");
    var shot = link.closest(".screenshot");
    var cap = shot ? shot.querySelector(".screenshot-caption") : null;
    open(link.getAttribute("href"),
         thumb ? thumb.getAttribute("alt") : "",
         cap ? cap.textContent : "");
  });
})();
