;;; guix.scm --- Guix package definition for emacs-beads

;; This file defines the emacs-beads package for GNU Guix.
;; Use with: guix shell -D -f guix.scm
;; This provides a development environment with emacs-eldev and
;; other dependencies.

(use-modules
 (guix packages)
 (guix gexp)
 (guix git-download)
 (guix build-system emacs)
 ((guix licenses) #:prefix license:)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (gnu packages emacs-build)
 (guix git))

(define %source-dir (dirname (current-filename)))

(define emacs-beads
  (package
    (name "emacs-beads")
    (version "0.1.0")
    (source (local-file %source-dir
                        "emacs-beads-checkout"
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f  ; Tests require bd CLI and mock setup
      #:lisp-directory "lisp"
      #:exclude #~(cons ".*-test\\.el$" %default-exclude)))
    (propagated-inputs
     (list emacs-transient))
    (native-inputs
     (list emacs-eldev))
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
