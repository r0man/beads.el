;;; beads-command-swarm.el --- Swarm command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd swarm' operations.
;; Swarm manages parallel work coordination on epics.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-swarm-create
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-swarm-create (beads-command-json)
    ((epic-id
      :initarg :epic-id
      :type (or null string)
      :initform nil
      :documentation "Epic ID to create swarm from."
      :positional 1)
     (coordinator
      :initarg :coordinator
      :type (or null string)
      :initform nil
      :documentation "Agent to act as swarm coordinator."
      :long-option "coordinator"
      :option-type :string
      :key "c"
      :transient "--coordinator"
      :class transient-option
      :argument "--coordinator="
      :prompt "Coordinator agent: "
      :transient-group "Options"
      :level 1
      :order 1)
     (force
      :initarg :force
      :type boolean
      :initform nil
      :documentation "Force creation even with validation warnings."
      :long-option "force"
      :option-type :boolean
      :key "f"
      :transient "--force"
      :class transient-switch
      :argument "--force"
      :transient-group "Options"
      :level 1
      :order 2))
    :documentation "Represents bd swarm create command.
  Creates a swarm molecule from an epic."))

(cl-defmethod beads-command-subcommand ((_command beads-command-swarm-create))
  "Return \"swarm create\" as the CLI subcommand."
  "swarm create")

(cl-defmethod beads-command-validate ((command beads-command-swarm-create))
  "Validate swarm create COMMAND."
  (with-slots (epic-id) command
    (if (not epic-id) "Epic ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-swarm-list
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-swarm-list (beads-command-json)
    ()
    :documentation "Represents bd swarm list command.
  Lists all swarm molecules."))

(cl-defmethod beads-command-subcommand ((_command beads-command-swarm-list))
  "Return \"swarm list\" as the CLI subcommand."
  "swarm list")

;;; ============================================================
;;; Command Class: beads-command-swarm-status
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-swarm-status (beads-command-json)
    ((swarm-id
      :initarg :swarm-id
      :type (or null string)
      :initform nil
      :documentation "Swarm ID to show status for."
      :positional 1))
    :documentation "Represents bd swarm status command.
  Shows current swarm status."))

(cl-defmethod beads-command-subcommand ((_command beads-command-swarm-status))
  "Return \"swarm status\" as the CLI subcommand."
  "swarm status")

;;; ============================================================
;;; Command Class: beads-command-swarm-validate
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-swarm-validate (beads-command-json)
    ((epic-id
      :initarg :epic-id
      :type (or null string)
      :initform nil
      :documentation "Epic ID to validate."
      :positional 1))
    :documentation "Represents bd swarm validate command.
  Validates epic structure for swarming."))

(cl-defmethod beads-command-subcommand ((_command beads-command-swarm-validate))
  "Return \"swarm validate\" as the CLI subcommand."
  "swarm validate")

(cl-defmethod beads-command-validate ((command beads-command-swarm-validate))
  "Validate swarm validate COMMAND."
  (with-slots (epic-id) command
    (if (not epic-id) "Epic ID is required" nil)))

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-swarm-create))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-swarm-list))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-swarm-status))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-swarm-validate))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-swarm-create "beads-command-swarm" nil t)
(beads-meta-define-transient beads-command-swarm-create "beads-swarm-create"
  "Create a swarm molecule from an epic."
  beads-option-global-section)

;;;###autoload (autoload 'beads-swarm-list "beads-command-swarm" nil t)
(beads-meta-define-transient beads-command-swarm-list "beads-swarm-list"
  "List all swarm molecules."
  beads-option-global-section)

;;;###autoload (autoload 'beads-swarm-status "beads-command-swarm" nil t)
(beads-meta-define-transient beads-command-swarm-status "beads-swarm-status"
  "Show current swarm status."
  beads-option-global-section)

;;;###autoload (autoload 'beads-swarm-validate "beads-command-swarm" nil t)
(beads-meta-define-transient beads-command-swarm-validate "beads-swarm-validate"
  "Validate epic structure for swarming."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-swarm "beads-command-swarm" nil t)
(transient-define-prefix beads-swarm ()
  "Swarm management for structured epics.

A swarm is parallel work coordination on an epic's DAG."
  ["Swarm Commands"
   ("c" "Create swarm" beads-swarm-create)
   ("l" "List swarms" beads-swarm-list)
   ("s" "Show status" beads-swarm-status)
   ("v" "Validate epic" beads-swarm-validate)])

(provide 'beads-command-swarm)
;;; beads-command-swarm.el ends here
