## Makefile.am --- Autotools build rules for beads.el
##
## Targets:
##   make            byte-compile lisp/*.el (warnings are errors) and
##                   generate beads-autoloads.el
##   make info       build the Texinfo manual (doc/beads.info)
##   make html       build the self-contained HTML manual
##   make pdf        build the PDF manual
##   make install    install *.el, *.elc, beads-autoloads.el and the Info manual
##   make dist       build the distribution tarball
##
## `make check' and `make lint' are added by the W2 work item; this file
## provides the skeleton they extend.

ACLOCAL_AMFLAGS = -I m4

EMACS = @EMACS@

## Dependency load-path flags resolved by configure.  Ambient EMACSLOADPATH
## (the Guix path) is passed through untouched.
TRANSIENT_DIR = @with_transient@
VUI_DIR = @with_vui@
DEP_LOAD_PATH = $(if $(TRANSIENT_DIR),-L "$(TRANSIENT_DIR)") \
                $(if $(VUI_DIR),-L "$(VUI_DIR)")

EMACS_BATCH = $(EMACS) --batch -L "$(abs_top_srcdir)/lisp" $(DEP_LOAD_PATH)
COMPILE_HELPER = "$(abs_top_srcdir)/build-aux/beads-compile.el"
AUTOLOADS_HELPER = "$(abs_top_srcdir)/build-aux/beads-autoloads.el"

BEADS_LISP_SOURCES = $(filter-out $(srcdir)/lisp/beads-autoloads.el, \
                              $(wildcard $(srcdir)/lisp/*.el))
BEADS_LISP_TEST_SOURCES = $(wildcard $(srcdir)/lisp/test/*.el)
BEADS_LISP_COMPILED = $(BEADS_LISP_SOURCES:.el=.elc)

AUTOLOADS_FILE = lisp/beads-autoloads.el

info_TEXINFOS = doc/beads.texi

EXTRA_DIST = \
  autogen.sh \
  build-aux/beads-compile.el \
  build-aux/beads-autoloads.el \
  m4/.gitkeep \
  guix.scm \
  scripts/dispatch-docs \
  share \
  docs \
  $(BEADS_LISP_SOURCES) \
  $(BEADS_LISP_TEST_SOURCES)

all-local: byte-compile autoloads

## Byte-compile every lisp/*.el in one Emacs invocation.  The helper sets
## `byte-compile-error-on-warn' and compiles in dependency order so that a
## file is compiled before any other file loads it (loading a file sets the
## `interactive-only' property of its transient prefixes, which would turn
## a legitimate call from a wrapper command into a byte-compile warning).
byte-compile: $(BEADS_LISP_SOURCES)
	$(EMACS_BATCH) -l $(COMPILE_HELPER) $(BEADS_LISP_SOURCES)

## Generate beads-autoloads.el.  The helper preloads beads-meta.el (which
## pulls in beads-prefix.el) so `;;;###autoload' on beads-defcommand and
## beads-meta-define-transient forms macroexpand during generation.
autoloads: $(AUTOLOADS_FILE)

$(AUTOLOADS_FILE): $(BEADS_LISP_SOURCES)
	$(EMACS_BATCH) -l $(AUTOLOADS_HELPER) \
	  "$(abs_top_srcdir)/lisp" "$(abs_builddir)/$(AUTOLOADS_FILE)"

.PHONY: all-local byte-compile autoloads

install-data-local:
	$(MKDIR_P) "$(DESTDIR)$(lispdir)"
	$(INSTALL_DATA) $(BEADS_LISP_SOURCES) "$(DESTDIR)$(lispdir)/"
	$(INSTALL_DATA) $(BEADS_LISP_COMPILED) "$(DESTDIR)$(lispdir)/"
	$(INSTALL_DATA) "$(abs_builddir)/$(AUTOLOADS_FILE)" \
	  "$(DESTDIR)$(lispdir)/beads-autoloads.el"

uninstall-local:
	rm -f "$(DESTDIR)$(lispdir)"/$(notdir $(BEADS_LISP_SOURCES))
	rm -f "$(DESTDIR)$(lispdir)"/$(notdir $(BEADS_LISP_COMPILED))
	rm -f "$(DESTDIR)$(lispdir)/beads-autoloads.el"

clean-local:
	rm -f $(BEADS_LISP_COMPILED) $(AUTOLOADS_FILE)
	rm -rf "$(builddir)/doc/beads.html"
	rm -f "$(builddir)/doc/beads.pdf"
