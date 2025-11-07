;;; init.el --- Minimal init.el for testing  -*- lexical-binding: t; -*-

(use-package ef-themes
  :hook (after-init . (lambda () (ef-themes-rotate ef-themes-collection))))

(use-package magit
  :bind (("C-x C-g s" . magit-status)))

(use-package beads
  :commands (beads)
  :load-path ("~/workspace/beads.el/lisp"
              "~/workspace/beads.el/lisp/test")
  :bind ("C-c b" . beads)
  :hook (after-init . beads)
  :config
  (require 'beads-eldoc)
  (beads-eldoc-mode))

(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t))
