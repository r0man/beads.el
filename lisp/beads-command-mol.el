;;; beads-command-mol.el --- Mol command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd mol' operations.
;; Mol manages molecules (work templates) for agent workflows.
;;
;; Each command class includes full slot metadata for automatic
;; transient menu generation via `beads-defcommand'.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-mol-show
;;; ============================================================

;;;###autoload (autoload 'beads-mol-show "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-show (beads-command-global-options)
  ((mol-id
    :positional 1)
   (parallel
    :short-option "p"
    :type boolean
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd mol show command.
Shows molecule details.")


;;; ============================================================
;;; Command Class: beads-command-mol-pour
;;; ============================================================

;;;###autoload (autoload 'beads-mol-pour "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-pour (beads-command-global-options)
  ((proto-id
    :positional 1)
   (var
    :type (list-of string)
    :short-option "v"
    :prompt "Variable (key=value): "
    :group "Options"
    :level 1
    :order 1)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 2)
   (assignee
    :type (or null string)
    :short-option "a"
    :group "Options"
    :level 1
    :order 3)
   (attach
    :type (or null string)
    :short-option "A"
    :prompt "Attach to: "
    :group "Options"
    :level 2
    :order 4)
   (attach-type
    :type (or null string)
    :short-option "T"
    :prompt "Attach type (after/child/gate): "
    :choices ("after" "child" "gate")
    :group "Options"
    :level 2
    :order 5))
  :documentation "Represents bd mol pour command.
Instantiates proto as persistent mol (liquid phase).")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp
;;; ============================================================

;;;###autoload (autoload 'beads-mol-wisp "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-wisp (beads-command-global-options)
  ((proto-id
    :positional 1)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1)
   (var
    :type (list-of string)
    :short-option "v"
    :prompt "Variable (key=value): "
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd mol wisp command.
Creates or manages wisps (ephemeral molecules).")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp-create
;;; ============================================================

;;;###autoload (autoload 'beads-mol-wisp-create "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-wisp-create (beads-command-global-options)
  ((proto-id
    :positional 1)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1)
   (root-only
    :type boolean
    :short-option "r"
    :group "Options"
    :level 1
    :order 2)
   (var
    :type (list-of string)
    :short-option "v"
    :prompt "Variable (key=value): "
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol wisp create command.
Instantiates a proto as an ephemeral wisp (solid -> vapor).")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp-list
;;; ============================================================

;;;###autoload (autoload 'beads-mol-wisp-list "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-wisp-list (beads-command-global-options)
  ((show-all
    :long-option "all"
    :type boolean
    :short-option "a"
    :group "Options"
    :level 1
    :order 1)
   (type-filter
    :long-option "type"
    :type (or null string)
    :short-option "t"
    :prompt "Issue type: "
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd mol wisp list command.
Lists all wisps (ephemeral molecules) in the current context.")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp-gc
;;; ============================================================

;;;###autoload (autoload 'beads-mol-wisp-gc "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-wisp-gc (beads-command-global-options)
  ((dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1)
   (age
    :type (or null string)
    :short-option "A"
    :prompt "Age threshold (e.g., 1h, 30m): "
    :group "Options"
    :level 1
    :order 2)
   (show-all
    :long-option "all"
    :type boolean
    :short-option "a"
    :group "Options"
    :level 1
    :order 3)
   (closed
    :type boolean
    :short-option "c"
    :group "Options"
    :level 1
    :order 4)
   (force
    :type boolean
    :short-option "f"
    :group "Options"
    :level 1
    :order 5)
   (exclude-type
    :type (list-of string)
    :short-option "e"
    :prompt "Exclude type: "
    :group "Options"
    :level 1
    :order 6))
  :documentation "Represents bd mol wisp gc command.
Garbage-collects abandoned or closed wisps.")


;;; ============================================================
;;; Command Class: beads-command-mol-bond
;;; ============================================================

;;;###autoload (autoload 'beads-mol-bond "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-bond (beads-command-global-options)
  ((first-id
    :positional 1)
   (second-id
    :positional 2)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1)
   (bond-type
    :long-option "type"
    :type (or null string)
    :short-option "t"
    :prompt "Bond type (seq/par/gate): "
    :choices ("seq" "par" "gate")
    :group "Options"
    :level 1
    :order 2)
   (as
    :type (or null string)
    :short-option "a"
    :prompt "Result name: "
    :group "Options"
    :level 1
    :order 3)
   (pour
    :type boolean
    :short-option "p"
    :group "Options"
    :level 1
    :order 4)
   (ephemeral
    :type boolean
    :short-option "e"
    :group "Options"
    :level 2
    :order 5)
   (ref
    :type boolean
    :short-option "r"
    :group "Options"
    :level 2
    :order 6)
   (var
    :type (list-of string)
    :short-option "v"
    :prompt "Variable (key=value): "
    :group "Options"
    :level 2
    :order 7))
  :documentation "Represents bd mol bond command.
Bonds two protos or molecules together.")


;;; ============================================================
;;; Command Class: beads-command-mol-squash
;;; ============================================================

;;;###autoload (autoload 'beads-mol-squash "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-squash (beads-command-global-options)
  ((mol-id
    :positional 1)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1)
   (keep-children
    :type boolean
    :short-option "k"
    :group "Options"
    :level 1
    :order 2)
   (summary
    :type (or null string)
    :short-option "s"
    :prompt "Summary file: "
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol squash command.
Compresses molecule execution into a digest.")


;;; ============================================================
;;; Command Class: beads-command-mol-burn
;;; ============================================================

;;;###autoload (autoload 'beads-mol-burn "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-burn (beads-command-global-options)
  ((mol-id
    :positional 1)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1)
   (force
    :type boolean
    :short-option "f"
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd mol burn command.
Deletes a molecule without creating a digest.")


;;; ============================================================
;;; Command Class: beads-command-mol-distill
;;; ============================================================

;;;###autoload (autoload 'beads-mol-distill "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-distill (beads-command-global-options)
  ((epic-id
    :positional 1)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1)
   (output
    :short-option "o"
    :type (or null string)
    :prompt "Output file: "
    :group "Options"
    :level 1
    :order 2)
   (var
    :type (list-of string)
    :short-option "v"
    :prompt "Variable (key=value): "
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol distill command.
Extracts a formula from an existing epic.")


;;; ============================================================
;;; Command Class: beads-command-mol-current
;;; ============================================================

;;;###autoload (autoload 'beads-mol-current "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-current (beads-command-global-options)
  ((mol-id
    :positional 1)
   (for-agent
    :long-option "for"
    :type (or null string)
    :short-option "f"
    :prompt "Agent ID: "
    :group "Options"
    :level 1
    :order 1)
   (limit
    :type (or null string integer)
    :short-option "l"
    :prompt "Limit: "
    :group "Options"
    :level 1
    :order 2)
   (range
    :type (or null string)
    :short-option "r"
    :prompt "Range (e.g., 1-10): "
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol current command.
Shows current position in molecule workflow.")


;;; ============================================================
;;; Command Class: beads-command-mol-progress
;;; ============================================================

;;;###autoload (autoload 'beads-mol-progress "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-progress (beads-command-global-options)
  ((mol-id
    :positional 1))
  :documentation "Represents bd mol progress command.
Shows molecule progress summary.")


;;; ============================================================
;;; Command Class: beads-command-mol-ready
;;; ============================================================

;;;###autoload (autoload 'beads-mol-ready "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-ready (beads-command-global-options)
  ()
  :documentation "Represents bd mol ready command.
Finds molecules ready for gate-resume dispatch.")


;;; ============================================================
;;; Command Class: beads-command-mol-stale
;;; ============================================================

;;;###autoload (autoload 'beads-mol-stale "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-stale (beads-command-global-options)
  ()
  :documentation "Represents bd mol stale command.
Detects complete-but-unclosed molecules.")


;;; Mol Seed Command

;;;###autoload (autoload 'beads-mol-seed "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-seed (beads-command-global-options)
  ((formula-name
    :positional t
    :positional-order 1)
   (patrol
    :type boolean
    :short-option "p"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd mol seed command.
Verify that formulas are accessible and can be cooked."
  :cli-command "mol seed")

;;; Mol Last-Activity Command

;;;###autoload (autoload 'beads-mol-last-activity "beads-command-mol" nil t)
(beads-defcommand beads-command-mol-last-activity (beads-command-global-options)
  ((molecule-id
    :positional t
    :positional-order 1))
  :documentation "Represents bd mol last-activity command.
Show the most recent activity timestamp for a molecule."
  :cli-command "mol last-activity")

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-mol "beads-command-mol" nil t)
(transient-define-prefix beads-mol ()
  "Manage molecules (work templates).

Proto: template epic, Molecule: instantiated work."
  ["Instantiate"
   ("p" "Pour (persistent)" beads-mol-pour)
   ("w" "Wisp (ephemeral)" beads-mol-wisp)]
  ["Combine"
   ("b" "Bond" beads-mol-bond)
   ("d" "Distill" beads-mol-distill)]
  ["Lifecycle"
   ("s" "Show" beads-mol-show)
   ("c" "Current" beads-mol-current)
   ("P" "Progress" beads-mol-progress)]
  ["Cleanup"
   ("S" "Squash" beads-mol-squash)
   ("B" "Burn" beads-mol-burn)]
  ["Find"
   ("r" "Ready" beads-mol-ready)
   ("t" "Stale" beads-mol-stale)]
  ["Health"
   ("e" "Seed (verify)" beads-mol-seed)
   ("l" "Last activity" beads-mol-last-activity)])

(provide 'beads-command-mol)
;;; beads-command-mol.el ends here
