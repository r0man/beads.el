;;; beads-lint.el --- Run package-lint for beads.el under plain Emacs  -*- lexical-binding: t; -*-
;;
;; Usage (from scripts/lint):
;;
;;   BEADS_LINT_FILE=/abs/path/lisp/beads.el \
;;     emacs --batch -Q -L lisp [-L PKG_LINT_DIR] -l build-aux/beads-lint.el
;;
;; `make lint' runs this after the byte-compile warnings-as-errors gate.
;; It is the no-Eldev equivalent of `eldev lint' for the package-lint half.
;;
;; package-lint resolution: it must be on the Emacs `load-path'.  Put
;; emacs-package-lint on it with the Guix input (`guix shell emacs-package-lint'
;; or the package's own `guix.scm') or with MELPA
;; (`M-x package-install RET package-lint RET'); configure's
;; `--with-package-lint=DIR' (or `PACKAGE_LINT_DIR') adds that directory
;; to the load path.  When package-lint is not already available it is
;; installed from MELPA into the user's `package-user-dir'.
;;
;; package-lint also checks that every `Package-Requires' dependency is
;; installable, so the standard GNU/NonGNU/MELPA archives are registered
;; (this is what Eldev configured) and refreshed once when its archive
;; cache is empty.  A missing package-lint is a hard error: a lint gate
;; that silently disappears is worse than none (REQ-008).

;;; Code:

(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)

;; Populate the archive cache so package-lint can resolve `transient' and
;; `vui'.  A warm cache (the CI cache or a local package.el) is reused.
(unless (assq 'vui package-archive-contents)
  (package-refresh-contents))

(unless (require 'package-lint nil t)
  (package-install 'package-lint)
  (unless (require 'package-lint nil t)
    (error "package-lint is not on the Emacs load path and could not be installed.
Install it with Guix (the `emacs-package-lint' input) or from MELPA
(`M-x package-install RET package-lint RET'), or pass
--with-package-lint=DIR / set PACKAGE_LINT_DIR.")))

(let ((file (or (getenv "BEADS_LINT_FILE") "lisp/beads.el")))
  (unless (file-readable-p file)
    (error "beads-lint: cannot read %s" file))
  (setq command-line-args-left (list file))
  (package-lint-batch-and-exit))

;;; beads-lint.el ends here