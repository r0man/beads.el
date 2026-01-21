;;; beads-command-slot.el --- Slot command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd slot' operations.
;; Slots manage named references on agent beads.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-slot-show
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-slot-show (beads-command-json)
    ((agent-id
      :initarg :agent-id
      :type (or null string)
      :initform nil
      :documentation "Agent bead ID."
      :positional 1))
    :documentation "Represents bd slot show command.
  Shows all slots on an agent bead."))

(cl-defmethod beads-command-subcommand ((_command beads-command-slot-show))
  "Return \"slot show\" as the CLI subcommand."
  "slot show")

(cl-defmethod beads-command-validate ((command beads-command-slot-show))
  "Validate slot show COMMAND."
  (with-slots (agent-id) command
    (if (not agent-id) "Agent ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-slot-set
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-slot-set (beads-command-json)
    ((agent-id
      :initarg :agent-id
      :type (or null string)
      :initform nil
      :documentation "Agent bead ID."
      :positional 1)
     (slot-name
      :initarg :slot-name
      :type (or null string)
      :initform nil
      :documentation "Slot name (hook, role)."
      :positional 2)
     (bead-id
      :initarg :bead-id
      :type (or null string)
      :initform nil
      :documentation "Bead ID to set in slot."
      :positional 3))
    :documentation "Represents bd slot set command.
  Sets a slot on an agent bead."))

(cl-defmethod beads-command-subcommand ((_command beads-command-slot-set))
  "Return \"slot set\" as the CLI subcommand."
  "slot set")

(cl-defmethod beads-command-validate ((command beads-command-slot-set))
  "Validate slot set COMMAND."
  (with-slots (agent-id slot-name bead-id) command
    (cond
     ((not agent-id) "Agent ID is required")
     ((not slot-name) "Slot name is required")
     ((not bead-id) "Bead ID is required")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-slot-clear
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-slot-clear (beads-command-json)
    ((agent-id
      :initarg :agent-id
      :type (or null string)
      :initform nil
      :documentation "Agent bead ID."
      :positional 1)
     (slot-name
      :initarg :slot-name
      :type (or null string)
      :initform nil
      :documentation "Slot name to clear."
      :positional 2))
    :documentation "Represents bd slot clear command.
  Clears a slot on an agent bead."))

(cl-defmethod beads-command-subcommand ((_command beads-command-slot-clear))
  "Return \"slot clear\" as the CLI subcommand."
  "slot clear")

(cl-defmethod beads-command-validate ((command beads-command-slot-clear))
  "Validate slot clear COMMAND."
  (with-slots (agent-id slot-name) command
    (cond
     ((not agent-id) "Agent ID is required")
     ((not slot-name) "Slot name is required")
     (t nil))))

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-slot-show))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-slot-set))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-slot-clear))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-slot-show "beads-command-slot" nil t)
(beads-meta-define-transient beads-command-slot-show "beads-slot-show"
  "Show all slots on an agent bead."
  beads-option-global-section)

;;;###autoload (autoload 'beads-slot-set "beads-command-slot" nil t)
(beads-meta-define-transient beads-command-slot-set "beads-slot-set"
  "Set a slot on an agent bead.

Slot names: hook (current work), role (role definition)."
  beads-option-global-section)

;;;###autoload (autoload 'beads-slot-clear "beads-command-slot" nil t)
(beads-meta-define-transient beads-command-slot-clear "beads-slot-clear"
  "Clear a slot on an agent bead."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-slot "beads-command-slot" nil t)
(transient-define-prefix beads-slot ()
  "Manage slots on agent beads.

Slots are named references: hook (0..1), role (required)."
  ["Slot Commands"
   ("s" "Show slots" beads-slot-show)
   ("S" "Set slot" beads-slot-set)
   ("c" "Clear slot" beads-slot-clear)])

(provide 'beads-command-slot)
;;; beads-command-slot.el ends here
