dnl configure.ac --- Autotools front door for beads.el -*- Autoconf -*-
dnl
dnl This is the GNU-standard build entry point for beads.el.  It locates
dnl Emacs and the Elisp runtime dependencies, discovers the optional
dnl bd/dolt/graphviz CLI toolchain used by the integration tests, and
dnl generates the Makefile that byte-compiles, generates autoloads,
dnl installs, and distributes the package.
dnl
dnl The build has no C components, so it uses the "foreign" Automake
dnl strictness and does not call AC_PROG_CC.

AC_PREREQ([2.69])
AC_INIT([beads.el], [0.1.0], [roman@burningswell.com], [beads.el])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([-Wall foreign])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_SRCDIR([lisp/beads.el])

dnl ---------------------------------------------------------------------
dnl Emacs
dnl ---------------------------------------------------------------------
AC_ARG_WITH([emacs],
  [AS_HELP_STRING([--with-emacs=PATH],
                  [path to the Emacs executable (default: search PATH)])],
  [AC_PATH_PROG([EMACS], [$withval])],
  [AC_PATH_PROG([EMACS], [emacs])])
AS_IF([test -z "$EMACS" || test ! -x "$EMACS"],
      [AC_MSG_ERROR([Emacs is required but was not found.

Install Emacs, put it on PATH, or pass --with-emacs=/path/to/emacs.])])
AC_SUBST([EMACS])

AC_MSG_CHECKING([for Emacs version])
EMACS_VERSION=`"$EMACS" --batch --eval '(princ emacs-version)' 2>/dev/null`
AS_IF([test -z "$EMACS_VERSION"], [EMACS_VERSION=unknown])
AC_MSG_RESULT([$EMACS_VERSION])

dnl ---------------------------------------------------------------------
dnl Elisp runtime dependencies: transient and vui.
dnl
dnl Resolution order at build time is: --with-<pkg>=DIR load-path
dnl arguments, then whatever the ambient EMACSLOADPATH already provides
dnl (this is the Guix path: `guix shell -D -f guix.scm' exports
dnl EMACSLOADPATH).
dnl ---------------------------------------------------------------------
AC_ARG_WITH([transient],
  [AS_HELP_STRING([--with-transient=DIR],
                  [directory holding transient.el (default: EMACSLOADPATH)])],
  [], [with_transient=])
AC_ARG_WITH([vui],
  [AS_HELP_STRING([--with-vui=DIR],
                  [directory holding vui.el (default: EMACSLOADPATH)])],
  [], [with_vui=])
AC_SUBST([with_transient])
AC_SUBST([with_vui])

## Build the make-level load-path flags from --with-transient/--with-vui
## here rather than in Makefile.am, so Makefile.am stays portable
## Automake (no GNU make if function).  A user who relies only on
## the ambient EMACSLOADPATH simply passes neither option.
DEP_LOAD_PATH=
AS_IF([test -n "$with_transient"],
      [DEP_LOAD_PATH="$DEP_LOAD_PATH -L \"$with_transient\""])
AS_IF([test -n "$with_vui"],
      [DEP_LOAD_PATH="$DEP_LOAD_PATH -L \"$with_vui\""])
AC_SUBST([DEP_LOAD_PATH])

dnl Development lint dependency.  `make lint' runs package-lint; resolve
dnl it from --with-package-lint=DIR or the ambient EMACSLOADPATH (the Guix
dnl `emacs-package-lint' input).  scripts/lint fails clearly when it is
dnl missing.
AC_ARG_WITH([package-lint],
  [AS_HELP_STRING([--with-package-lint=DIR],
                  [directory holding package-lint.el (default: EMACSLOADPATH)])],
  [], [with_package_lint=])
AC_SUBST([with_package_lint])

dnl Optional manual tooling.
AC_PATH_PROG([MAKEINFO], [makeinfo], [makeinfo])
AC_PATH_PROG([TEXI2PDF_BIN], [texi2pdf], [texi2pdf])
AC_SUBST([MAKEINFO])
AC_SUBST([TEXI2PDF_BIN])

dnl ---------------------------------------------------------------------
dnl Optional CLI toolchain for the :integration test suite.
dnl --with-<tool>=no forces it off; --with-<tool>=PATH names it;
dnl otherwise AC_PATH_PROG searches PATH.
dnl ---------------------------------------------------------------------
AC_ARG_WITH([bd],
  [AS_HELP_STRING([--with-bd=PATH],
                  [path to the bd CLI (default: search PATH; no to disable)])],
  [AS_IF([test "x$withval" = "xno"], [BD=""], [BD="$withval"])],
  [AC_PATH_PROG([BD], [bd])])
AC_SUBST([BD])
AM_CONDITIONAL([HAVE_BD], [test -n "$BD"])

AC_ARG_WITH([dolt],
  [AS_HELP_STRING([--with-dolt=PATH],
                  [path to the dolt CLI (default: search PATH; no to disable)])],
  [AS_IF([test "x$withval" = "xno"], [DOLT=""], [DOLT="$withval"])],
  [AC_PATH_PROG([DOLT], [dolt])])
AC_SUBST([DOLT])
AM_CONDITIONAL([HAVE_DOLT], [test -n "$DOLT"])

AC_ARG_WITH([graphviz],
  [AS_HELP_STRING([--with-graphviz=PATH],
                  [path to the graphviz dot CLI (default: search PATH; no to disable)])],
  [AS_IF([test "x$withval" = "xno"], [GRAPHVIZ=""], [GRAPHVIZ="$withval"])],
  [AC_PATH_PROG([GRAPHVIZ], [dot])])
AC_SUBST([GRAPHVIZ])
AM_CONDITIONAL([HAVE_GRAPHVIZ], [test -n "$GRAPHVIZ"])

AC_MSG_CHECKING([which optional CLI tools are available])
AC_MSG_RESULT([bd=${BD:-no} dolt=${DOLT:-no} graphviz=${GRAPHVIZ:-no}])

dnl ---------------------------------------------------------------------
dnl Installation directories.
dnl ---------------------------------------------------------------------
AC_ARG_WITH([lispdir],
  [AS_HELP_STRING([--with-lispdir=DIR],
                  [install Elisp files into DIR (default: DATADIR/emacs/site-lisp/beads)])],
  [lispdir="$withval"],
  [lispdir='${datadir}/emacs/site-lisp/beads'])
AC_SUBST([lispdir])

AC_PROG_INSTALL
AC_PROG_MKDIR_P

dnl ---------------------------------------------------------------------
dnl Elisp file lists.
dnl
dnl Expand the lists here so Makefile.am needs no GNU make functions
dnl (wildcard, filter-out, addprefix, notdir), which portable Automake
dnl rejects.  The values carry literal srcdir/builddir references so make
dnl resolves them per build tree.
dnl ---------------------------------------------------------------------
beads_lisp_files=
beads_lisp_basenames=
beads_lisp_compiled=
beads_lisp_elc_basenames=
for f in "$srcdir"/lisp/*.el; do
  base=`basename "$f"`
  case "$base" in
    beads-autoloads.el) continue ;;
  esac
  beads_lisp_files="$beads_lisp_files \$(srcdir)/lisp/$base"
  beads_lisp_basenames="$beads_lisp_basenames $base"
  beads_lisp_compiled="$beads_lisp_compiled \$(builddir)/lisp/${base%.el}.elc"
  beads_lisp_elc_basenames="$beads_lisp_elc_basenames ${base%.el}.elc"
done
BEADS_LISP_FILES=$beads_lisp_files
BEADS_LISP_BASENAMES=$beads_lisp_basenames
BEADS_LISP_COMPILED=$beads_lisp_compiled
BEADS_LISP_ELC_BASENAMES=$beads_lisp_elc_basenames
AC_SUBST([BEADS_LISP_FILES])
AC_SUBST([BEADS_LISP_BASENAMES])
AC_SUBST([BEADS_LISP_COMPILED])
AC_SUBST([BEADS_LISP_ELC_BASENAMES])

beads_lisp_test_files=
for f in "$srcdir"/lisp/test/*.el; do
  base=`basename "$f"`
  beads_lisp_test_files="$beads_lisp_test_files \$(srcdir)/lisp/test/$base"
done
BEADS_LISP_TEST_FILES=$beads_lisp_test_files
AC_SUBST([BEADS_LISP_TEST_FILES])

AC_OUTPUT
