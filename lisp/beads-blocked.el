;;; beads-blocked.el --- Blocked command for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides the `bd blocked' command interface for beads.el.
;;
;; The bd blocked command shows issues that are blocked by dependencies.
;; Use this to identify bottlenecks and see what work is waiting.
;;
;; Usage:
;;   M-x beads-blocked RET
;;
;; The transient menu is auto-generated from slot metadata in
;; `beads-command-blocked' using `beads-meta-define-transient'.

;;; Code:

(require 'beads)
(require 'beads-command-blocked)
(require 'beads-meta)
(require 'beads-option)

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-blocked "beads-blocked" nil t)
(beads-meta-define-transient beads-command-blocked "beads-blocked"
  "Show blocked issues.

Lists issues that are waiting on dependencies to complete.
Use --parent to filter to a specific epic's descendants.

Transient levels control which options are visible (cycle with C-x l):
  Level 2: Filters (parent)"
  beads-option-global-section)

(provide 'beads-blocked)
;;; beads-blocked.el ends here
