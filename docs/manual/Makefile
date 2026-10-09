# Makefile for the beads.el user manual and its screenshot pipeline.
#
# The manual is written as plain Org under this directory; the screenshots are
# produced by the pipeline in screenshots/ and committed under images/.
#
#   make manual                 export the Org pages to HTML under html/
#   make screenshots            regenerate every screenshot (scope all)
#   make screenshots SCOPE=views
#                               regenerate one view subset
#   make verify                 capture twice and assert byte-identical output
#   make verify-scope SCOPE=show
#                               assert a subset leaves other images untouched
#   make clean                  remove generated HTML

MANUAL_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
IMAGE_DIR  := $(MANUAL_DIR)/images
HTML_DIR   := $(MANUAL_DIR)/html
PAGES      := $(wildcard $(MANUAL_DIR)/*.org)
SCOPE      ?= all
EMACS      ?= emacs

.PHONY: all manual screenshots verify verify-scope clean

all: manual

# Export every Org page to HTML.  The pages share docs/.orgconfig through their
# #+SETUPFILE, so the kbd/var/cmd macros render consistently.
manual:
	@mkdir -p "$(HTML_DIR)"
	@for page in $(PAGES); do \
	  echo "exporting $$page"; \
	  $(EMACS) -Q --batch --eval \
	    "(progn (require 'org) (require 'ox-html) \
	       (find-file \"$$page\") \
	       (org-html-export-to-html))" || exit 1; \
	  mv -f "$${page%.org}.html" "$(HTML_DIR)/" || exit 1; \
	done
	@echo "manual written to $(HTML_DIR)"

# Regenerate the screenshots selected by SCOPE (all, changed, or a comma
# separated view list).  The pipeline creates its own temporary fixture store
# and never reads the repository's .beads store.
screenshots:
	"$(MANUAL_DIR)/screenshots/capture.sh" --scope "$(SCOPE)"

# Capture the same scope twice and fail, naming the drifting image, unless the
# output is byte-identical.  Safe: it uses throwaway staging and leaves the
# committed images alone.
verify:
	"$(MANUAL_DIR)/screenshots/capture.sh" --verify "$(SCOPE)"

# Snapshot the committed images, run SCOPE, and fail if any non-selected image
# changed (digest or mtime).  Requires a prior `make screenshots`.
verify-scope:
	"$(MANUAL_DIR)/screenshots/capture.sh" --verify-scope "$(SCOPE)"

clean:
	rm -rf "$(HTML_DIR)"
	@echo "removed $(HTML_DIR)"
