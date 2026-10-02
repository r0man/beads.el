;;; beads-menu.el --- Hand-built dispatch menu and provider seam -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, menus

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Home of the hand-built dispatch/maintenance menus (WI-9) and the
;; `beads-menu-providers' extension seam (design.md §4.2).  Downstream
;; packages append their own transient groups to the dispatch menu
;; without redefining it, in the same spirit as Magit's
;; `magit-define-popup' extensions.
;;
;; The provider contract is deliberately narrow: a provider is a
;; function of no arguments that returns a list of transient group
;; vectors acceptable to `transient-define-prefix'.  Providers run on
;; every menu render, so they must be side-effect-free.  With no
;; providers installed the dispatch menu is exactly beads.el's own.

;;; Code:

(require 'cl-lib)

;;; Menu Providers

(defvar beads-menu-providers nil
  "Hook of functions contributing groups to the dispatch menu.
Each function is called with no arguments and returns a list of
transient group vectors acceptable to `transient-define-prefix'.
Providers run on every menu render, so they must be side-effect-free;
they must not depend on the current buffer.  An empty hook (the
standalone default) contributes nothing.

Use `beads-menu-provider-groups' to collect the contributions.

Downstream use: Gas City appends a `[City]' group.")

(defun beads-menu-provider-groups ()
  "Return the combined transient groups from `beads-menu-providers'.
Calls each provider in order and appends its list.  An empty hook
returns nil, leaving the dispatch menu unchanged (standalone no-op)."
  (apply #'append
         (delq nil (mapcar #'funcall beads-menu-providers))))

(provide 'beads-menu)
;;; beads-menu.el ends here