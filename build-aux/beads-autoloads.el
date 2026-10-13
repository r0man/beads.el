;;; beads-autoloads.el --- Generate beads.el autoloads  -*- lexical-binding: t; -*-
;;
;; Usage:
;;   emacs --batch -L lisp -l build-aux/beads-autoloads.el LISPDIR OUTPUT
;;
;; `;;;###autoload' cookies in beads.el sit directly on
;; `beads-defcommand' and `beads-meta-define-transient' forms.  The
;; autoload generator has to macroexpand those forms to find the cookies,
;; so beads-meta.el (which pulls in beads-prefix.el) must be loaded first.
;; This reproduces the Eldev `loaddefs-generate' advice; without it the
;; generated file silently loses the `M-x beads' entry points.

;;; Code:

(require 'loaddefs-gen)

(let* ((lisp-dir (pop command-line-args-left))
       (output (pop command-line-args-left)))
  (unless (and lisp-dir output)
    (error "beads-autoloads: usage: LISPDIR OUTPUT"))
  (let ((lisp-dir (file-name-as-directory (expand-file-name lisp-dir)))
        (output (expand-file-name output)))
    (add-to-list 'load-path lisp-dir)
    (dolist (lib '("beads-prefix" "beads-meta"))
      (let ((file (expand-file-name lib lisp-dir)))
        (when (file-exists-p (concat file ".el"))
          (load file nil t))))
    (loaddefs-generate lisp-dir output)))

;;; beads-autoloads.el ends here
