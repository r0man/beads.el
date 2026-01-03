;;; beads-doctor.el --- Doctor command for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides the `bd doctor' command interface for beads.el.
;;
;; The bd doctor command performs sanity checks on the beads installation:
;; - Database version and migration status
;; - Schema compatibility
;; - Daemon health
;; - Circular dependencies
;; - Git hooks status
;; - And more...
;;
;; Usage:
;;   M-x beads-doctor RET
;;
;; The transient menu is auto-generated from slot metadata in
;; `beads-command-doctor' using `beads-meta-define-transient'.

;;; Code:

(require 'beads)
(require 'beads-command-doctor)
(require 'beads-meta)
(require 'beads-option)

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-doctor "beads-doctor" nil t)
(beads-meta-define-transient beads-command-doctor "beads-doctor"
  "Run diagnostics on beads installation.

Checks database health, schema, daemon status, dependencies, and more.

Transient levels control which options are visible (cycle with C-x l):
  Level 2: Basic checks (specific check, health, deep, perf)
  Level 3: Fix options and output settings
  Level 4: Advanced fix options"
  beads-option-global-section)

(provide 'beads-doctor)
;;; beads-doctor.el ends here
