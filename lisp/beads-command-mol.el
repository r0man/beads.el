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
    :initarg :mol-id
    :type (or null string)
    :initform nil
    :documentation "Molecule/proto ID."
    :positional 1)
   (parallel
    :initarg :parallel
    :type boolean
    :initform nil
    :documentation "Show parallelizable steps."
    :long-option "parallel"
    :short-option "p"
    :option-type :boolean
    :key "p"
    :transient "--parallel"
    :class transient-switch
    :argument "--parallel"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd mol show command.
Shows molecule details.")


;;; ============================================================
;;; Command Class: beads-command-mol-pour
;;; ============================================================

(beads-defcommand beads-command-mol-pour (beads-command-global-options)
  ((proto-id
    :initarg :proto-id
    :type (or null string)
    :initform nil
    :documentation "Proto ID to instantiate."
    :positional 1)
   (var
    :initarg :var
    :type list
    :initform nil
    :documentation "Variable substitutions (key=value)."
    :long-option "var"
    :option-type :list
    :key "v"
    :transient "--var"
    :class transient-option
    :argument "--var="
    :prompt "Variable (key=value): "
    :transient-group "Options"
    :level 1
    :order 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without creating."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 2)
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Override assignee for all steps."
    :long-option "assignee"
    :option-type :string
    :key "a"
    :transient "--assignee"
    :class transient-option
    :argument "--assignee="
    :prompt "Assignee: "
    :transient-group "Options"
    :level 1
    :order 3)
   (attach
    :initarg :attach
    :type (or null string)
    :initform nil
    :documentation "ID to attach to (parent molecule)."
    :long-option "attach"
    :option-type :string
    :key "A"
    :transient "--attach"
    :class transient-option
    :argument "--attach="
    :prompt "Attach to: "
    :transient-group "Options"
    :level 2
    :order 4)
   (attach-type
    :initarg :attach-type
    :type (or null string)
    :initform nil
    :documentation "Attach as dep type: after, child, gate."
    :long-option "attach-type"
    :option-type :string
    :key "T"
    :transient "--attach-type"
    :class transient-option
    :argument "--attach-type="
    :prompt "Attach type (after/child/gate): "
    :choices ("after" "child" "gate")
    :transient-group "Options"
    :level 2
    :order 5))
  :documentation "Represents bd mol pour command.
Instantiates proto as persistent mol (liquid phase).")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp
;;; ============================================================

(beads-defcommand beads-command-mol-wisp (beads-command-global-options)
  ((proto-id
    :initarg :proto-id
    :type (or null string)
    :initform nil
    :documentation "Proto ID to instantiate as wisp."
    :positional 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without creating."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1)
   (var
    :initarg :var
    :type list
    :initform nil
    :documentation "Variable substitutions (key=value)."
    :long-option "var"
    :option-type :list
    :key "v"
    :transient "--var"
    :class transient-option
    :argument "--var="
    :prompt "Variable (key=value): "
    :transient-group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd mol wisp command.
Creates or manages wisps (ephemeral molecules).")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp-create
;;; ============================================================

(beads-defcommand beads-command-mol-wisp-create (beads-command-global-options)
  ((proto-id
    :initarg :proto-id
    :type (or null string)
    :initform nil
    :documentation "Proto ID to instantiate as wisp."
    :positional 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without creating."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1)
   (root-only
    :initarg :root-only
    :type boolean
    :initform nil
    :documentation "Create only root issue, no child step issues."
    :long-option "root-only"
    :option-type :boolean
    :key "r"
    :transient "--root-only"
    :class transient-switch
    :argument "--root-only"
    :transient-group "Options"
    :level 1
    :order 2)
   (var
    :initarg :var
    :type list
    :initform nil
    :documentation "Variable substitutions (key=value)."
    :long-option "var"
    :option-type :list
    :key "v"
    :transient "--var"
    :class transient-option
    :argument "--var="
    :prompt "Variable (key=value): "
    :transient-group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol wisp create command.
Instantiates a proto as an ephemeral wisp (solid -> vapor).")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp-list
;;; ============================================================

(beads-defcommand beads-command-mol-wisp-list (beads-command-global-options)
  ((show-all
    :initarg :show-all
    :type boolean
    :initform nil
    :documentation "Include closed wisps."
    :long-option "all"
    :option-type :boolean
    :key "a"
    :transient "--all"
    :class transient-switch
    :argument "--all"
    :transient-group "Options"
    :level 1
    :order 1)
   (type-filter
    :initarg :type-filter
    :type (or null string)
    :initform nil
    :documentation "Filter by issue type (e.g., agent, task, patrol)."
    :long-option "type"
    :option-type :string
    :key "t"
    :transient "--type"
    :class transient-option
    :argument "--type="
    :prompt "Issue type: "
    :transient-group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd mol wisp list command.
Lists all wisps (ephemeral molecules) in the current context.")


;;; ============================================================
;;; Command Class: beads-command-mol-wisp-gc
;;; ============================================================

(beads-defcommand beads-command-mol-wisp-gc (beads-command-global-options)
  ((dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview what would be cleaned."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1)
   (age
    :initarg :age
    :type (or null string)
    :initform nil
    :documentation "Age threshold for abandoned wisp detection (default: 1h)."
    :long-option "age"
    :option-type :string
    :key "A"
    :transient "--age"
    :class transient-option
    :argument "--age="
    :prompt "Age threshold (e.g., 1h, 30m): "
    :transient-group "Options"
    :level 1
    :order 2)
   (show-all
    :initarg :show-all
    :type boolean
    :initform nil
    :documentation "Also clean closed wisps older than threshold."
    :long-option "all"
    :option-type :boolean
    :key "a"
    :transient "--all"
    :class transient-switch
    :argument "--all"
    :transient-group "Options"
    :level 1
    :order 3)
   (closed
    :initarg :closed
    :type boolean
    :initform nil
    :documentation "Delete all closed wisps (ignores --age threshold)."
    :long-option "closed"
    :option-type :boolean
    :key "c"
    :transient "--closed"
    :class transient-switch
    :argument "--closed"
    :transient-group "Options"
    :level 1
    :order 4)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Actually delete (default: preview only)."
    :long-option "force"
    :option-type :boolean
    :key "f"
    :transient "--force"
    :class transient-switch
    :argument "--force"
    :transient-group "Options"
    :level 1
    :order 5)
   (exclude-type
    :initarg :exclude-type
    :type list
    :initform nil
    :documentation "Exclude wisps of these types from GC (e.g., agent,rig)."
    :long-option "exclude-type"
    :option-type :list
    :key "e"
    :transient "--exclude-type"
    :class transient-option
    :argument "--exclude-type="
    :prompt "Exclude type: "
    :transient-group "Options"
    :level 1
    :order 6))
  :documentation "Represents bd mol wisp gc command.
Garbage-collects abandoned or closed wisps.")


;;; ============================================================
;;; Command Class: beads-command-mol-bond
;;; ============================================================

(beads-defcommand beads-command-mol-bond (beads-command-global-options)
  ((first-id
    :initarg :first-id
    :type (or null string)
    :initform nil
    :documentation "First proto/mol ID."
    :positional 1)
   (second-id
    :initarg :second-id
    :type (or null string)
    :initform nil
    :documentation "Second proto/mol ID."
    :positional 2)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without bonding."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1)
   (bond-type
    :initarg :bond-type
    :type (or null string)
    :initform nil
    :documentation "Bond type: seq (default), par, gate."
    :long-option "type"
    :option-type :string
    :key "t"
    :transient "--type"
    :class transient-option
    :argument "--type="
    :prompt "Bond type (seq/par/gate): "
    :choices ("seq" "par" "gate")
    :transient-group "Options"
    :level 1
    :order 2)
   (as
    :initarg :as
    :type (or null string)
    :initform nil
    :documentation "Name for bonded result."
    :long-option "as"
    :option-type :string
    :key "a"
    :transient "--as"
    :class transient-option
    :argument "--as="
    :prompt "Result name: "
    :transient-group "Options"
    :level 1
    :order 3)
   (pour
    :initarg :pour
    :type boolean
    :initform nil
    :documentation "Pour result immediately."
    :long-option "pour"
    :option-type :boolean
    :key "p"
    :transient "--pour"
    :class transient-switch
    :argument "--pour"
    :transient-group "Options"
    :level 1
    :order 4)
   (ephemeral
    :initarg :ephemeral
    :type boolean
    :initform nil
    :documentation "Create as wisp (ephemeral)."
    :long-option "ephemeral"
    :option-type :boolean
    :key "e"
    :transient "--ephemeral"
    :class transient-switch
    :argument "--ephemeral"
    :transient-group "Options"
    :level 2
    :order 5)
   (ref
    :initarg :ref
    :type boolean
    :initform nil
    :documentation "Reference second as dependency only."
    :long-option "ref"
    :option-type :boolean
    :key "r"
    :transient "--ref"
    :class transient-switch
    :argument "--ref"
    :transient-group "Options"
    :level 2
    :order 6)
   (var
    :initarg :var
    :type list
    :initform nil
    :documentation "Variable substitutions (key=value)."
    :long-option "var"
    :option-type :list
    :key "v"
    :transient "--var"
    :class transient-option
    :argument "--var="
    :prompt "Variable (key=value): "
    :transient-group "Options"
    :level 2
    :order 7))
  :documentation "Represents bd mol bond command.
Bonds two protos or molecules together.")


;;; ============================================================
;;; Command Class: beads-command-mol-squash
;;; ============================================================

(beads-defcommand beads-command-mol-squash (beads-command-global-options)
  ((mol-id
    :initarg :mol-id
    :type (or null string)
    :initform nil
    :documentation "Molecule ID to squash."
    :positional 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without squashing."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1)
   (keep-children
    :initarg :keep-children
    :type boolean
    :initform nil
    :documentation "Keep child issues instead of deleting."
    :long-option "keep-children"
    :option-type :boolean
    :key "k"
    :transient "--keep-children"
    :class transient-switch
    :argument "--keep-children"
    :transient-group "Options"
    :level 1
    :order 2)
   (summary
    :initarg :summary
    :type (or null string)
    :initform nil
    :documentation "Path to summary file."
    :long-option "summary"
    :option-type :string
    :key "s"
    :transient "--summary"
    :class transient-option
    :argument "--summary="
    :prompt "Summary file: "
    :transient-group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol squash command.
Compresses molecule execution into a digest.")


;;; ============================================================
;;; Command Class: beads-command-mol-burn
;;; ============================================================

(beads-defcommand beads-command-mol-burn (beads-command-global-options)
  ((mol-id
    :initarg :mol-id
    :type (or null string)
    :initform nil
    :documentation "Molecule ID to burn."
    :positional 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without burning."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force burn without confirmation."
    :long-option "force"
    :option-type :boolean
    :key "f"
    :transient "--force"
    :class transient-switch
    :argument "--force"
    :transient-group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd mol burn command.
Deletes a molecule without creating a digest.")


;;; ============================================================
;;; Command Class: beads-command-mol-distill
;;; ============================================================

(beads-defcommand beads-command-mol-distill (beads-command-global-options)
  ((epic-id
    :initarg :epic-id
    :type (or null string)
    :initform nil
    :documentation "Epic ID to distill into formula."
    :positional 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without distilling."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1)
   (output
    :initarg :output
    :type (or null string)
    :initform nil
    :documentation "Output file path."
    :long-option "output"
    :short-option "o"
    :option-type :string
    :key "o"
    :transient "--output"
    :class transient-option
    :argument "--output="
    :prompt "Output file: "
    :transient-group "Options"
    :level 1
    :order 2)
   (var
    :initarg :var
    :type list
    :initform nil
    :documentation "Variable definitions for parameterization."
    :long-option "var"
    :option-type :list
    :key "v"
    :transient "--var"
    :class transient-option
    :argument "--var="
    :prompt "Variable (key=value): "
    :transient-group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol distill command.
Extracts a formula from an existing epic.")


;;; ============================================================
;;; Command Class: beads-command-mol-current
;;; ============================================================

(beads-defcommand beads-command-mol-current (beads-command-global-options)
  ((mol-id
    :initarg :mol-id
    :type (or null string)
    :initform nil
    :documentation "Molecule ID."
    :positional 1)
   (for-agent
    :initarg :for-agent
    :type (or null string)
    :initform nil
    :documentation "Filter for specific agent."
    :long-option "for"
    :option-type :string
    :key "f"
    :transient "--for"
    :class transient-option
    :argument "--for="
    :prompt "Agent ID: "
    :transient-group "Options"
    :level 1
    :order 1)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit number of steps shown."
    :long-option "limit"
    :option-type :integer
    :key "l"
    :transient "--limit"
    :class transient-option
    :argument "--limit="
    :prompt "Limit: "
    :transient-group "Options"
    :level 1
    :order 2)
   (range
    :initarg :range
    :type (or null string)
    :initform nil
    :documentation "Show steps in range (e.g., 1-10)."
    :long-option "range"
    :option-type :string
    :key "r"
    :transient "--range"
    :class transient-option
    :argument "--range="
    :prompt "Range (e.g., 1-10): "
    :transient-group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd mol current command.
Shows current position in molecule workflow.")


;;; ============================================================
;;; Command Class: beads-command-mol-progress
;;; ============================================================

(beads-defcommand beads-command-mol-progress (beads-command-global-options)
  ((mol-id
    :initarg :mol-id
    :type (or null string)
    :initform nil
    :documentation "Molecule ID."
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
    :initarg :formula-name
    :type (or null string)
    :initform nil
    :documentation "Formula name to verify (positional argument)."
    :positional t
    :positional-order 1)
   (patrol
    :initarg :patrol
    :type boolean
    :initform nil
    :documentation "Verify all patrol formulas (--patrol)."
    :long-option "patrol"
    :option-type :boolean
    :key "p"
    :transient "--patrol"
    :class transient-switch
    :argument "--patrol"
    :transient-group "Options"
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
    :initarg :molecule-id
    :type (or null string)
    :initform nil
    :documentation "Molecule ID (positional argument)."
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
