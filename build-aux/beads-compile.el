;;; beads-compile.el --- Byte-compile beads.el with warnings as errors  -*- lexical-binding: t; -*-
;;
;; Usage:
;;   emacs --batch -L lisp -l build-aux/beads-compile.el lisp/*.el
;;
;; Compiles every file named in command-line-args-left in one Emacs
;; invocation with `byte-compile-error-on-warn' set, so a real warning
;; (an unused variable, a call to an undefined function, an obsolete
;; call, ...) fails the build.  This is the project's `--warnings-as-errors'
;; gate (REQ-008).
;;
;; The `interactive-only' category is excluded.  beads.el defines
;; transient prefixes with `beads-define-prefix' and deliberately calls
;; some of them from the Lisp wrapper commands (`beads-dep-add',
;; `beads-update', ...); transient marks every prefix `interactive-only',
;; so the compiler would flag those calls.  Whether the flag fires also
;; depends on whether an unrelated file happened to load the prefix
;; before this one was compiled, which would make the gate
;; order-dependent.  Every other warning category stays fatal.

;;; Code:

(require 'bytecomp)

(setq byte-compile-error-on-warn t)
(setq byte-compile-warnings '(not interactive-only))

(let ((files (copy-sequence command-line-args-left))
      (failed nil))
  (when (null files)
    (error "beads-compile: no source files given"))
  (dolist (file files)
    (unless (byte-compile-file file)
      (setq failed t)))
  (when failed
    (error "beads-compile: one or more files failed to byte-compile"))
  (message "Byte-compiled %d file(s)" (length files)))

;;; beads-compile.el ends here
