;;; beads-command-mol.el --- Mol command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd mol' operations.
;; Mol manages molecules (work templates) for agent workflows.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-mol-show
;;; ============================================================

(beads-defcommand beads-command-mol-show (beads-command-global-options)
  ((mol-id
    :positional 1)
   (parallel
    :short-option "p"
    :option-type :boolean
    :key "p"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd mol show command.
Shows molecule details.")


;;; ============================================================
;;; Command Class: beads-command-mol-pour
;;; ============================================================

(beads-defcommand beads-command-mol-pour (beads-command-global-options)
  ((proto-id
    :positional 1)
   (var
    :option-type :list
    :key "v"
    :prompt "Variable (key=value): "
    :group "Options"
    :level 1
    :order 1)
   (dry-run
    :option-type :boolean
    :key "n"
    :group "Options"
    :level 1
    :order 2)
   (assignee
    :option-type :string
    :key "a"
    :group "Options"
    :level 1
    :order 3)
   (attach
    :option-type :string
    :key "A"
    :prompt "Attach to: "
    :group "Options"
    :level 2
    :order 4)
   (attach-type
    :option-type :string
    :key "T"
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

(beads-defcommand beads-command-mol-wisp (beads-command-global-options)
  ((proto-id
    :positional 1)
   (dry-run
    :option-type :boolean
    :key "n"
    :group "Options"
    :level 1
    :order 1)
   (var
    :option-type :list
    :key "v"
    :prompt "Variable (key=value): "
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd mol wisp command.
Creates or manages wisps (ephemeral molecules).")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp-create
;;; ============================================================

(beads-defcommand beads-command-mol-wisp-create (beads-command-global-options)
  ((proto-id
    :positional 1)
   (dry-run
    :option-type :boolean
    :key "n"
    :group "Options"
    :level 1
    :order 1)
   (root-only
    :option-type :boolean
    :key "r"
    :group "Options"
    :level 1
    :order 2)
   (var
    :option-type :list
    :key "v"
    :prompt "Variable (key=value): "
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol wisp create command.
Instantiates a proto as an ephemeral wisp (solid -> vapor).")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp-list
;;; ============================================================

(beads-defcommand beads-command-mol-wisp-list (beads-command-global-options)
  ((show-all
    :long-option "all"
    :option-type :boolean
    :key "a"
    :group "Options"
    :level 1
    :order 1)
   (type-filter
    :long-option "type"
    :option-type :string
    :key "t"
    :prompt "Issue type: "
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd mol wisp list command.
Lists all wisps (ephemeral molecules) in the current context.")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp-gc
;;; ============================================================

(beads-defcommand beads-command-mol-wisp-gc (beads-command-global-options)
  ((dry-run
    :option-type :boolean
    :key "n"
    :group "Options"
    :level 1
    :order 1)
   (age
    :option-type :string
    :key "A"
    :prompt "Age threshold (e.g., 1h, 30m): "
    :group "Options"
    :level 1
    :order 2)
   (show-all
    :long-option "all"
    :option-type :boolean
    :key "a"
    :group "Options"
    :level 1
    :order 3)
   (closed
    :option-type :boolean
    :key "c"
    :group "Options"
    :level 1
    :order 4)
   (force
    :option-type :boolean
    :key "f"
    :group "Options"
    :level 1
    :order 5)
   (exclude-type
    :option-type :list
    :key "e"
    :prompt "Exclude type: "
    :group "Options"
    :level 1
    :order 6))
  :documentation "Represents bd mol wisp gc command.
Garbage-collects abandoned or closed wisps.")


;;; ============================================================
;;; Command Class: beads-command-mol-bond
;;; ============================================================

(beads-defcommand beads-command-mol-bond (beads-command-global-options)
  ((first-id
    :positional 1)
   (second-id
    :positional 2)
   (dry-run
    :option-type :boolean
    :key "n"
    :group "Options"
    :level 1
    :order 1)
   (bond-type
    :long-option "type"
    :option-type :string
    :key "t"
    :prompt "Bond type (seq/par/gate): "
    :choices ("seq" "par" "gate")
    :group "Options"
    :level 1
    :order 2)
   (as
    :option-type :string
    :key "a"
    :prompt "Result name: "
    :group "Options"
    :level 1
    :order 3)
   (pour
    :option-type :boolean
    :key "p"
    :group "Options"
    :level 1
    :order 4)
   (ephemeral
    :option-type :boolean
    :key "e"
    :group "Options"
    :level 2
    :order 5)
   (ref
    :option-type :boolean
    :key "r"
    :group "Options"
    :level 2
    :order 6)
   (var
    :option-type :list
    :key "v"
    :prompt "Variable (key=value): "
    :group "Options"
    :level 2
    :order 7))
  :documentation "Represents bd mol bond command.
Bonds two protos or molecules together.")


;;; ============================================================
;;; Command Class: beads-command-mol-squash
;;; ============================================================

(beads-defcommand beads-command-mol-squash (beads-command-global-options)
  ((mol-id
    :positional 1)
   (dry-run
    :option-type :boolean
    :key "n"
    :group "Options"
    :level 1
    :order 1)
   (keep-children
    :option-type :boolean
    :key "k"
    :group "Options"
    :level 1
    :order 2)
   (summary
    :option-type :string
    :key "s"
    :prompt "Summary file: "
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol squash command.
Compresses molecule execution into a digest.")


;;; ============================================================
;;; Command Class: beads-command-mol-burn
;;; ============================================================

(beads-defcommand beads-command-mol-burn (beads-command-global-options)
  ((mol-id
    :positional 1)
   (dry-run
    :option-type :boolean
    :key "n"
    :group "Options"
    :level 1
    :order 1)
   (force
    :option-type :boolean
    :key "f"
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd mol burn command.
Deletes a molecule without creating a digest.")


;;; ============================================================
;;; Command Class: beads-command-mol-distill
;;; ============================================================

(beads-defcommand beads-command-mol-distill (beads-command-global-options)
  ((epic-id
    :positional 1)
   (dry-run
    :option-type :boolean
    :key "n"
    :group "Options"
    :level 1
    :order 1)
   (output
    :short-option "o"
    :option-type :string
    :key "o"
    :prompt "Output file: "
    :group "Options"
    :level 1
    :order 2)
   (var
    :option-type :list
    :key "v"
    :prompt "Variable (key=value): "
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol distill command.
Extracts a formula from an existing epic.")


;;; ============================================================
;;; Command Class: beads-command-mol-current
;;; ============================================================

(beads-defcommand beads-command-mol-current (beads-command-global-options)
  ((mol-id
    :positional 1)
   (for-agent
    :long-option "for"
    :option-type :string
    :key "f"
    :prompt "Agent ID: "
    :group "Options"
    :level 1
    :order 1)
   (limit
    :option-type :integer
    :key "l"
    :prompt "Limit: "
    :group "Options"
    :level 1
    :order 2)
   (range
    :option-type :string
    :key "r"
    :prompt "Range (e.g., 1-10): "
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol current command.
Shows current position in molecule workflow.")


;;; ============================================================
;;; Command Class: beads-command-mol-progress
;;; ============================================================

(beads-defcommand beads-command-mol-progress (beads-command-global-options)
  ((mol-id
    :positional 1))
  :documentation "Represents bd mol progress command.
Shows molecule progress summary.")


;;; ============================================================
;;; Command Class: beads-command-mol-ready
;;; ============================================================

(beads-defcommand beads-command-mol-ready (beads-command-global-options)
  ()
  :documentation "Represents bd mol ready command.
Finds molecules ready for gate-resume dispatch.")


;;; ============================================================
;;; Command Class: beads-command-mol-stale
;;; ============================================================

(beads-defcommand beads-command-mol-stale (beads-command-global-options)
  ()
  :documentation "Represents bd mol stale command.
Detects complete-but-unclosed molecules.")


;;; Execute Interactive Methods












;;; Transient Menus

;;;###autoload (autoload 'beads-mol-show "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-show "beads-mol-show"
  "Show molecule/proto details."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-pour "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-pour "beads-mol-pour"
  "Instantiate proto as persistent mol."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-wisp "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-wisp "beads-mol-wisp"
  "Create ephemeral molecule (wisp)."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-wisp-create "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-wisp-create "beads-mol-wisp-create"
  "Instantiate a proto as a wisp (solid -> vapor)."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-wisp-list "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-wisp-list "beads-mol-wisp-list"
  "List all wisps in current context."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-wisp-gc "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-wisp-gc "beads-mol-wisp-gc"
  "Garbage-collect abandoned or closed wisps."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-bond "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-bond "beads-mol-bond"
  "Bond two protos or molecules."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-squash "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-squash "beads-mol-squash"
  "Compress molecule into digest."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-burn "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-burn "beads-mol-burn"
  "Delete molecule without digest."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-distill "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-distill "beads-mol-distill"
  "Extract formula from epic."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-current "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-current "beads-mol-current"
  "Show current workflow position."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-progress "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-progress "beads-mol-progress"
  "Show molecule progress."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-ready "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-ready "beads-mol-ready"
  "Find ready molecules."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mol-stale "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-stale "beads-mol-stale"
  "Find complete-but-unclosed molecules."
  beads-option-global-section)

;;; Mol Seed Command

(beads-defcommand beads-command-mol-seed (beads-command-global-options)
  ((formula-name
    :positional t
    :positional-order 1)
   (patrol
    :option-type :boolean
    :key "p"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd mol seed command.
Verify that formulas are accessible and can be cooked."
  :cli-command "mol seed")

;;;###autoload (autoload 'beads-mol-seed "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-seed "beads-mol-seed"
  "Verify formula accessibility."
  beads-option-global-section)

;;; Mol Last-Activity Command

(beads-defcommand beads-command-mol-last-activity (beads-command-global-options)
  ((molecule-id
    :positional t
    :positional-order 1))
  :documentation "Represents bd mol last-activity command.
Show the most recent activity timestamp for a molecule."
  :cli-command "mol last-activity")

;;;###autoload (autoload 'beads-mol-last-activity "beads-command-mol" nil t)
(beads-meta-define-transient beads-command-mol-last-activity
  "beads-mol-last-activity"
  "Show most recent molecule activity."
  beads-option-global-section)

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
