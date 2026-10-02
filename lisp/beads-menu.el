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
;;
;; `beads-dispatch' is the former top-level `beads' prefix, renamed in
;; the UI redesign: `M-x beads' now opens the status buffer and the
;; menu is reached with `?' from every porcelain view (REQ-002,
;; REQ-004).

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'beads)
(require 'beads-prefix)

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

;;; Dispatch Menu

;;;###autoload (autoload 'beads-dispatch "beads-menu" nil t)
(beads-define-prefix beads-dispatch ()
  "Hand-built dispatch menu for beads.el.

Opened with `?' from any porcelain view.  Groups are ordered by
frequency; `beads-menu-providers' appends downstream groups such as
gascity's `[City]' group when that package is present."
  [:description
   (lambda () (beads-main--format-project-header))
   :class transient-row
   ("" "" ignore :if (lambda () nil))]
  [["Issues"
    ("l" "List" beads-list)
    ("c" "Create" beads-compose-create)
    ("/" "Search" beads-search)
    ("i" "Show" beads-show)
    ("u" "Update" beads-update)
    ("x" "Close" beads-close)]
   ["Workflow"
    ("r" "Ready" beads-ready)
    ("b" "Blocked" beads-blocked)
    ("d" "Dependencies" beads-dep)
    ("e" "Edit" beads-edit)
    ("o" "Reopen" beads-reopen)]
   ["Views"
    ("s" "Dashboard" beads-dashboard)
    ("S" "Stats" beads-stats)
    ("v" "Graph" beads-graph-all)
    ("E" "Epic" beads-epic-menu)
    ("H" "History" beads-history)
    ("D" "Diff" beads-diff)]
   ["Manage"
    ("L" "Labels" beads-label-menu)
    ("F" "Formula" beads-formula-menu)
    ("m" "Molecule" beads-mol)
    ("k" "Dolt" beads-dolt)
    ("." "Config" beads-config)]]
  [["Context"
    :if beads--in-beads-buffer-p
    ("#" "Set priority" beads-actions-set-priority)
    ("C" "Claim" beads-actions-claim)
    ("?" "Actions..." beads-show-actions)]
   ["Actions"
    ("!" "Ops..." beads-ops-menu)
    (">" "Advanced..." beads-advanced-menu)
    ("g" "Refresh" beads-refresh-menu)
    ("q" "Quit" transient-quit-one)]])

(provide 'beads-menu)
;;; beads-menu.el ends here