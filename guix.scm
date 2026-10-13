;;; guix.scm --- Guix package definition for emacs-beads

;; Build and development-environment definition for beads.el.
;;
;; Use with:   guix shell -D -f guix.scm
;; Build with: guix build -f guix.scm
;;
;; beads.el is a GNU-style autotools project (see configure.ac and
;; Makefile.am); this definition packages it through `gnu-build-system',
;; i.e. the same ./configure && make && make install flow used from a
;; checkout.  Eldev is not part of the build or the development inputs.
;;
;; `transient' and `vui' are Guix packages and are propagated
;; as runtime inputs.  A bare `gnu-build-system' does not put them on
;; `EMACSLOADPATH', so the `set-emacs-load-path' phase below builds the
;; variable from every input's site-lisp directory (and the versioned
;; sub-directory Guix installs each ELPA package into); byte-compilation
;; and `make check' then resolve them exactly as they do in a checkout.
;;
;; `bd', `dolt' and `graphviz' are external CLI tools, not Guix inputs.
;; `configure' discovers them with `AC_PATH_PROG' (and `--with-*=no'
;; forces the reduced run); `guix shell -D' preserves the caller's PATH,
;; so a development shell still sees the installed toolchain.

;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules
 (guix packages)
 (guix gexp)
 (guix git-download)
 (guix build-system gnu)
 ((guix licenses) #:prefix license:)
 (gnu packages autotools)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (gnu packages emacs-build)
 (gnu packages texinfo)
 (gnu packages version-control))

(define %source-dir (dirname (current-filename)))

(define emacs-beads
  (package
    (name "emacs-beads")
    (version "0.1.0")
    (source (local-file %source-dir
                        "emacs-beads-checkout"
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; The ERT suite is not run in the Guix build sandbox.  The
      ;; `:unit' subset still probes for the `bd' executable, and a few
      ;; remote tests spawn `/bin/sh' scripts, neither of which the
      ;; sandbox provides; the full suite runs in CI and from a
      ;; `guix shell -D -f guix.scm' checkout instead.  The autotools
      ;; flow (`./configure && make && make install') is exercised here.
      #:tests? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1))
      #:phases
      #~(modify-phases %standard-phases
          ;; gnu-build-system does not use emacs-build-system's
          ;; expand-load-path, so build EMACSLOADPATH explicitly from
          ;; every input's share/emacs/site-lisp directory and the
          ;; versioned sub-directory Guix installs each ELPA package
          ;; into (transient.el, vui.el, ...).  Byte-compile
          ;; and `make check' then find the runtime dependencies.
          (add-after 'set-paths 'set-emacs-load-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((dirs
                      (delete-duplicates
                       (append-map
                        (lambda (entry)
                          (let* ((dir (cdr entry))
                                 (site (string-append dir
                                                      "/share/emacs/site-lisp")))
                            (if (file-exists? site)
                                (cons site
                                      (map (lambda (sub)
                                             (string-append site "/" sub))
                                           (filter
                                            (lambda (sub)
                                              (file-is-directory?
                                               (string-append site "/" sub)))
                                            (scandir site
                                                     (lambda (name)
                                                       (not (member name
                                                                    '("." ".."))))))))
                                '())))
                        inputs)))
                     (inherited (getenv "EMACSLOADPATH"))
                     (value (string-join
                             (if inherited (append dirs (list inherited)) dirs)
                             ":")))
                (setenv "EMACSLOADPATH" value)
                #t))))))
    (native-inputs
     ;; Build tooling, the lint/coverage helpers and `git' for the test
     ;; fixtures.  `emacs-marginalia' is a compile-time dependency:
     ;; beads-completion.el uses the `marginalia--fields' macro, and the
     ;; warnings-as-errors byte-compile gate needs it present.  There is
     ;; deliberately no emacs-eldev.
     (list autoconf automake texinfo emacs git emacs-package-lint
           emacs-undercover emacs-marginalia))
    (propagated-inputs
     (list emacs-transient emacs-vui))
    (home-page "https://github.com/r0man/beads.el")
    (synopsis "Magit-like Emacs interface for the Beads issue tracker")
    (description
     "This package provides a comprehensive Emacs interface for the Beads
issue tracker, inspired by Magit.  It offers keyboard-driven,
transient-based UI for managing issues without leaving Emacs.

Features:
@itemize
@item Tabulated list view for browsing issues
@item Detailed issue view with markdown-like rendering
@item Transient menus for creating, updating, and closing issues
@item Project.el integration with automatic .beads directory discovery
@item Support for dependencies, labels, and issue graphs
@item Async process execution for responsive UI
@end itemize

Requires the bd CLI tool to be installed and available in PATH.")
    (license license:gpl3+)))

emacs-beads
