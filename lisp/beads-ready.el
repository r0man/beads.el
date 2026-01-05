;;; beads-ready.el --- Ready command transient for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides a transient menu for the `bd ready' command.
;;
;; The bd ready command shows issues that are ready to work on (no blockers).
;; This transient provides a UI for setting filters before executing:
;; - Assignee filter
;; - Priority filter
;; - Type filter
;; - Label filters (AND/OR)
;; - Limit and sort options
;; - Molecule and parent scope filters
;;
;; Usage:
;;   M-x beads-ready-transient RET
;;
;; The transient menu is auto-generated from slot metadata in
;; `beads-command-ready' using `beads-meta-define-transient'.
;;
;; For a quick view without filters, use `beads-ready' from beads-list.el.

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-ready-transient "beads-ready" nil t)
(beads-meta-define-transient beads-command-ready "beads-ready-transient"
  "Show issues ready to work on with optional filters.

Ready issues have no unresolved blockers and are not deferred.

Filter options:
  -a: Filter by assignee
  -p: Filter by priority (0-4)
  -T: Filter by type (task, bug, feature, epic)
  -l: Filter by labels (AND logic - must have all)
  -L: Filter by labels (OR logic - must have any)

Display options:
  -n: Limit number of results
  -s: Sort policy (hybrid, priority, oldest)
  -y: Pretty tree format

Scope options:
  -m: Filter to steps within a molecule
  -M: Filter by molecule type
  -P: Filter to descendants of a parent

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Basic filters (assignee, priority, type, limit, sort)
  Level 2: Advanced (labels, deferred, molecule, parent, pretty)"
  beads-option-global-section)

(provide 'beads-ready)
;;; beads-ready.el ends here
