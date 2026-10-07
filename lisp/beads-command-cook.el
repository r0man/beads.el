;;; beads-command-cook.el --- Cook command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The `beads-command-cook' EIEIO class for `bd cook', split out of
;; `beads-command-misc.el' so every `bd' subcommand lives in its own
;; module (REQ-SF-012).  `bd cook' compiles a formula into a proto.
;;
;; The class keeps its slot metadata but suppresses the auto-generated
;; transient (`:transient nil'): the cook porcelain — the mode/persist
;; transient and the `--dry-run' step-tree preview — lives in
;; `beads-cook.el', which owns the `beads-cook' prefix.  The raw class
;; remains the CLI/execution bridge used by that porcelain.

;;; Code:

(require 'beads-command)

;;; ============================================================
;;; Command Class: beads-command-cook
;;; ============================================================

(beads-defcommand beads-command-cook (beads-command-global-options)
  ((formula-id
    :positional 1)
   (dry-run
    :type boolean
    :group "Options"
    :level 1
    :order 1
    :documentation "Preview what would be created")
   (force
    :type boolean
    :group "Options"
    :level 1
    :order 2
    :documentation "Replace existing proto if it exists (requires --persist)")
   (mode
    :type (or null string)
    :prompt "Mode (compile|runtime): "
    :choices ("compile" "runtime")
    :group "Options"
    :level 1
    :order 3
    :documentation "Cooking mode: compile (keep placeholders) or runtime (substitute vars)")
   (persist
    :type boolean
    :group "Options"
    :level 1
    :order 4
    :documentation "Persist proto to database (legacy behavior)")
   (prefix
    :type (or null string)
    :prompt "Proto ID prefix: "
    :group "Options"
    :level 2
    :order 5
    :documentation "Prefix to prepend to proto ID (e.g., 'gt-' creates 'gt-mol-feature')")
   (search-path
    :type (list-of string)
    :separator ","
    :transient transient-option
    :argument "--search-path="
    :prompt "Additional formula search path: "
    :group "Options"
    :level 2
    :order 6
    :documentation "Additional paths to search for formula inheritance")
   (var
    :type (list-of string)
    :separator ","
    :transient transient-option
    :argument "--var="
    :prompt "Variable (key=value): "
    :group "Options"
    :level 2
    :order 7
    :documentation "Variable substitution (key=value), enables runtime mode"))
  :documentation "Represents bd cook command.
Compiles a formula into a proto (ephemeral by default)."
  :transient nil)

(provide 'beads-command-cook)
;;; beads-command-cook.el ends here
