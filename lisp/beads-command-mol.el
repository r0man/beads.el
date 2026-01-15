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

(eval-and-compile
(beads-defcommand beads-command-mol-show (beads-command-json)
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
    :long-option "--parallel"
    :short-option "-p"
    :option-type :boolean
    :transient-key "-p"
    :transient-description "--parallel"
    :transient-class transient-switch
    :transient-argument "--parallel"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd mol show command.
Shows molecule details."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-show))
  "Return \"mol show\" as the CLI subcommand."
  "mol show")

;;; ============================================================
;;; Command Class: beads-command-mol-pour
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mol-pour (beads-command-json)
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
    :long-option "--var"
    :option-type :list
    :transient-key "-v"
    :transient-description "--var"
    :transient-class transient-option
    :transient-argument "--var="
    :transient-prompt "Variable (key=value): "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without creating."
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Override assignee for all steps."
    :long-option "--assignee"
    :option-type :string
    :transient-key "-a"
    :transient-description "--assignee"
    :transient-class transient-option
    :transient-argument "--assignee="
    :transient-prompt "Assignee: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 3)
   (attach
    :initarg :attach
    :type (or null string)
    :initform nil
    :documentation "ID to attach to (parent molecule)."
    :long-option "--attach"
    :option-type :string
    :transient-key "-A"
    :transient-description "--attach"
    :transient-class transient-option
    :transient-argument "--attach="
    :transient-prompt "Attach to: "
    :transient-group "Options"
    :transient-level 2
    :transient-order 4)
   (attach-type
    :initarg :attach-type
    :type (or null string)
    :initform nil
    :documentation "Attach as dep type: after, child, gate."
    :long-option "--attach-type"
    :option-type :string
    :transient-key "-T"
    :transient-description "--attach-type"
    :transient-class transient-option
    :transient-argument "--attach-type="
    :transient-prompt "Attach type (after/child/gate): "
    :transient-choices ("after" "child" "gate")
    :transient-group "Options"
    :transient-level 2
    :transient-order 5))
  :documentation "Represents bd mol pour command.
Instantiates proto as persistent mol (liquid phase)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-pour))
  "Return \"mol pour\" as the CLI subcommand."
  "mol pour")

;;; ============================================================
;;; Command Class: beads-command-mol-wisp
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mol-wisp (beads-command-json)
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
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (var
    :initarg :var
    :type list
    :initform nil
    :documentation "Variable substitutions (key=value)."
    :long-option "--var"
    :option-type :list
    :transient-key "-v"
    :transient-description "--var"
    :transient-class transient-option
    :transient-argument "--var="
    :transient-prompt "Variable (key=value): "
    :transient-group "Options"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd mol wisp command.
Creates or manages wisps (ephemeral molecules)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-wisp))
  "Return \"mol wisp\" as the CLI subcommand."
  "mol wisp")

;;; ============================================================
;;; Command Class: beads-command-mol-bond
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mol-bond (beads-command-json)
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
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (bond-type
    :initarg :bond-type
    :type (or null string)
    :initform nil
    :documentation "Bond type: seq (default), par, gate."
    :long-option "--type"
    :option-type :string
    :transient-key "-t"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Bond type (seq/par/gate): "
    :transient-choices ("seq" "par" "gate")
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (as
    :initarg :as
    :type (or null string)
    :initform nil
    :documentation "Name for bonded result."
    :long-option "--as"
    :option-type :string
    :transient-key "-a"
    :transient-description "--as"
    :transient-class transient-option
    :transient-argument "--as="
    :transient-prompt "Result name: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 3)
   (pour
    :initarg :pour
    :type boolean
    :initform nil
    :documentation "Pour result immediately."
    :long-option "--pour"
    :option-type :boolean
    :transient-key "-p"
    :transient-description "--pour"
    :transient-class transient-switch
    :transient-argument "--pour"
    :transient-group "Options"
    :transient-level 1
    :transient-order 4)
   (ephemeral
    :initarg :ephemeral
    :type boolean
    :initform nil
    :documentation "Create as wisp (ephemeral)."
    :long-option "--ephemeral"
    :option-type :boolean
    :transient-key "-e"
    :transient-description "--ephemeral"
    :transient-class transient-switch
    :transient-argument "--ephemeral"
    :transient-group "Options"
    :transient-level 2
    :transient-order 5)
   (ref
    :initarg :ref
    :type boolean
    :initform nil
    :documentation "Reference second as dependency only."
    :long-option "--ref"
    :option-type :boolean
    :transient-key "-r"
    :transient-description "--ref"
    :transient-class transient-switch
    :transient-argument "--ref"
    :transient-group "Options"
    :transient-level 2
    :transient-order 6)
   (var
    :initarg :var
    :type list
    :initform nil
    :documentation "Variable substitutions (key=value)."
    :long-option "--var"
    :option-type :list
    :transient-key "-v"
    :transient-description "--var"
    :transient-class transient-option
    :transient-argument "--var="
    :transient-prompt "Variable (key=value): "
    :transient-group "Options"
    :transient-level 2
    :transient-order 7))
  :documentation "Represents bd mol bond command.
Bonds two protos or molecules together."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-bond))
  "Return \"mol bond\" as the CLI subcommand."
  "mol bond")

;;; ============================================================
;;; Command Class: beads-command-mol-squash
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mol-squash (beads-command-json)
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
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (keep-children
    :initarg :keep-children
    :type boolean
    :initform nil
    :documentation "Keep child issues instead of deleting."
    :long-option "--keep-children"
    :option-type :boolean
    :transient-key "-k"
    :transient-description "--keep-children"
    :transient-class transient-switch
    :transient-argument "--keep-children"
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (summary
    :initarg :summary
    :type (or null string)
    :initform nil
    :documentation "Path to summary file."
    :long-option "--summary"
    :option-type :string
    :transient-key "-s"
    :transient-description "--summary"
    :transient-class transient-option
    :transient-argument "--summary="
    :transient-prompt "Summary file: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 3))
  :documentation "Represents bd mol squash command.
Compresses molecule execution into a digest."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-squash))
  "Return \"mol squash\" as the CLI subcommand."
  "mol squash")

;;; ============================================================
;;; Command Class: beads-command-mol-burn
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mol-burn (beads-command-json)
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
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force burn without confirmation."
    :long-option "--force"
    :option-type :boolean
    :transient-key "-f"
    :transient-description "--force"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Options"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd mol burn command.
Deletes a molecule without creating a digest."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-burn))
  "Return \"mol burn\" as the CLI subcommand."
  "mol burn")

;;; ============================================================
;;; Command Class: beads-command-mol-distill
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mol-distill (beads-command-json)
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
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (output
    :initarg :output
    :type (or null string)
    :initform nil
    :documentation "Output file path."
    :long-option "--output"
    :short-option "-o"
    :option-type :string
    :transient-key "-o"
    :transient-description "--output"
    :transient-class transient-option
    :transient-argument "--output="
    :transient-prompt "Output file: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (var
    :initarg :var
    :type list
    :initform nil
    :documentation "Variable definitions for parameterization."
    :long-option "--var"
    :option-type :list
    :transient-key "-v"
    :transient-description "--var"
    :transient-class transient-option
    :transient-argument "--var="
    :transient-prompt "Variable (key=value): "
    :transient-group "Options"
    :transient-level 1
    :transient-order 3))
  :documentation "Represents bd mol distill command.
Extracts a formula from an existing epic."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-distill))
  "Return \"mol distill\" as the CLI subcommand."
  "mol distill")

;;; ============================================================
;;; Command Class: beads-command-mol-current
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mol-current (beads-command-json)
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
    :long-option "--for"
    :option-type :string
    :transient-key "-f"
    :transient-description "--for"
    :transient-class transient-option
    :transient-argument "--for="
    :transient-prompt "Agent ID: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit number of steps shown."
    :long-option "--limit"
    :option-type :integer
    :transient-key "-l"
    :transient-description "--limit"
    :transient-class transient-option
    :transient-argument "--limit="
    :transient-prompt "Limit: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (range
    :initarg :range
    :type (or null string)
    :initform nil
    :documentation "Show steps in range (e.g., 1-10)."
    :long-option "--range"
    :option-type :string
    :transient-key "-r"
    :transient-description "--range"
    :transient-class transient-option
    :transient-argument "--range="
    :transient-prompt "Range (e.g., 1-10): "
    :transient-group "Options"
    :transient-level 1
    :transient-order 3))
  :documentation "Represents bd mol current command.
Shows current position in molecule workflow."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-current))
  "Return \"mol current\" as the CLI subcommand."
  "mol current")

;;; ============================================================
;;; Command Class: beads-command-mol-progress
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mol-progress (beads-command-json)
  ((mol-id
    :initarg :mol-id
    :type (or null string)
    :initform nil
    :documentation "Molecule ID."
    :positional 1))
  :documentation "Represents bd mol progress command.
Shows molecule progress summary."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-progress))
  "Return \"mol progress\" as the CLI subcommand."
  "mol progress")

;;; ============================================================
;;; Command Class: beads-command-mol-ready
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mol-ready (beads-command-json)
  ()
  :documentation "Represents bd mol ready command.
Finds molecules ready for gate-resume dispatch."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-ready))
  "Return \"mol ready\" as the CLI subcommand."
  "mol ready")

;;; ============================================================
;;; Command Class: beads-command-mol-stale
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mol-stale (beads-command-json)
  ()
  :documentation "Represents bd mol stale command.
Detects complete-but-unclosed molecules."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mol-stale))
  "Return \"mol stale\" as the CLI subcommand."
  "mol stale")

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-show))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-pour))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-wisp))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-bond))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-squash))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-burn))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-distill))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-current))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-progress))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-ready))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mol-stale))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

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
   ("t" "Stale" beads-mol-stale)])

(provide 'beads-command-mol)
;;; beads-command-mol.el ends here
