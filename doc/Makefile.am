## doc/Makefile.am --- build and install the beads.el manual
##
## The manual is built by automake's `info_TEXINFOS' machinery, so
## `make', `make info', `make install' and `make dist' all handle it
## without a standalone Makefile.  The stylesheet, lightbox, the
## `@include'd chapters and the screenshot PNGs are part of the
## distribution so a tarball builds the manual as shipped.
##
## The HTML recipe stays automake's built-in `.texi.html' rule, extended
## through AM_MAKEINFOHTMLFLAGS so the shipped manual is a self-contained,
## split multi-page tree: the stylesheet is inlined by `--css-include' and
## the lightbox script through the PRE_BODY_CLOSE customization variable.
## The `html-local' hook then mirrors the raw screenshot images into the
## output directory, because makeinfo emits them as <img> without copying
## them.  Unlike gascity.el/doc/images, beads.el's doc/images is a
## symlink to docs/manual/images, so the mirror must use `cp -RL': a bare
## `cp -R' copies the symlink itself and yields broken images.  The PDF
## keeps automake's built-in `.texi.pdf' rule, the same `texi2pdf'
## invocation the standalone Makefile used.
##
## `make html' and `make pdf' at the top level recurse here through
## `SUBDIRS = doc' in the top-level Makefile.am.

info_TEXINFOS = beads.texi

## Every Texinfo input the manual @includes, plus the presentation assets
## and the screenshot PNGs, is part of the distribution so `make dist'
## yields a tarball whose manual builds as shipped.  The generated tables
## (REQ-009, REQ-013) live inline in `orchestration.texi' inside their
## BEGIN/END GENERATED markers, so there is no separate generated source
## directory to distribute.
beads_TEXINFOS = \
  manual.css \
  lightbox.js \
  introduction.texi \
  installation.texi \
  getting-started.texi \
  issue-workflow.texi \
  views.texi \
  formulas.texi \
  agents.texi \
  orchestration.texi \
  maintenance.texi \
  reference.texi \
  concepts.texi \
  $(sort $(wildcard $(srcdir)/images/*.png))

EXTRA_DIST = $(beads_TEXINFOS)

## Inline the stylesheet and the lightbox into every HTML page.  Automake
## passes these flags to `$(MAKEINFO) --html' for its built-in
## `.texi.html' rule; the shell command substitution lands the lightbox
## as a single `--set-customization-variable' argument, so its quotes and
## `$' characters need no escaping.
AM_MAKEINFOHTMLFLAGS = \
  --css-include=$(srcdir)/manual.css \
  --set-customization-variable "PRE_BODY_CLOSE=<script>$$(cat $(srcdir)/lightbox.js)</script>"

## `makeinfo' does not copy the screenshot images the manual references as
## raw <img>, so mirror doc/images/ into the built HTML tree once it
## exists.  Automake wires this hook into the `html' target through
## `html-am'.  doc/images is a symlink to docs/manual/images, hence `-L'.
html-local: beads.html
	rm -rf beads.html/images
	cp -RL $(srcdir)/images beads.html/images

## Automake's texinfo clean leaves the built HTML tree and the PDF behind;
## remove them so `make clean' restores a pristine checkout (the same
## cleanup the top-level Makefile.am used to own).
clean-local:
	rm -rf beads.html
	rm -f beads.pdf
