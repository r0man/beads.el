;;; beads-command-agent.el --- Agent command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd agent' operations.
;; Agent manages state on agent beads for ZFC-compliant state reporting.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-agent-state
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-agent-state (beads-command-json)
  ((agent-id
    :initarg :agent-id
    :type (or null string)
    :initform nil
    :documentation "Agent bead ID."
    :positional 1)
   (state
    :initarg :state
    :type (or null string)
    :initform nil
    :documentation "State to set (idle, spawning, running, working, stuck, done, stopped, dead)."
    :positional 2))
  :documentation "Represents bd agent state command.
Sets agent state for monitoring."))

(cl-defmethod beads-command-subcommand ((_command beads-command-agent-state))
  "Return \"agent state\" as the CLI subcommand."
  "agent state")

(cl-defmethod beads-command-validate ((command beads-command-agent-state))
  "Validate agent state COMMAND."
  (with-slots (agent-id state) command
    (cond
     ((not agent-id) "Agent ID is required")
     ((not state) "State is required")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-agent-heartbeat
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-agent-heartbeat (beads-command-json)
  ((agent-id
    :initarg :agent-id
    :type (or null string)
    :initform nil
    :documentation "Agent bead ID."
    :positional 1))
  :documentation "Represents bd agent heartbeat command.
Updates agent last_activity timestamp."))

(cl-defmethod beads-command-subcommand ((_command beads-command-agent-heartbeat))
  "Return \"agent heartbeat\" as the CLI subcommand."
  "agent heartbeat")

(cl-defmethod beads-command-validate ((command beads-command-agent-heartbeat))
  "Validate agent heartbeat COMMAND."
  (with-slots (agent-id) command
    (if (not agent-id) "Agent ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-agent-show
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-agent-show (beads-command-json)
  ((agent-id
    :initarg :agent-id
    :type (or null string)
    :initform nil
    :documentation "Agent bead ID."
    :positional 1))
  :documentation "Represents bd agent show command.
Shows agent bead details."))

(cl-defmethod beads-command-subcommand ((_command beads-command-agent-show))
  "Return \"agent show\" as the CLI subcommand."
  "agent show")

(cl-defmethod beads-command-validate ((command beads-command-agent-show))
  "Validate agent show COMMAND."
  (with-slots (agent-id) command
    (if (not agent-id) "Agent ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-agent-backfill-labels
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-agent-backfill-labels (beads-command-json)
  ((dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Show what would be done."
    :long-option "dry-run"
    :option-type :boolean
    :transient-key "n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd agent backfill-labels command.
Backfills role_type/rig labels on existing agent beads."))

(cl-defmethod beads-command-subcommand ((_command beads-command-agent-backfill-labels))
  "Return \"agent backfill-labels\" as the CLI subcommand."
  "agent backfill-labels")

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-agent-state))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-agent-heartbeat))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-agent-show))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-agent-backfill-labels))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-agent-state "beads-command-agent" nil t)
(beads-meta-define-transient beads-command-agent-state "beads-agent-state"
  "Set agent state.

States: idle, spawning, running, working, stuck, done, stopped, dead"
  beads-option-global-section)

;;;###autoload (autoload 'beads-agent-heartbeat "beads-command-agent" nil t)
(beads-meta-define-transient beads-command-agent-heartbeat "beads-agent-heartbeat"
  "Update agent last_activity timestamp."
  beads-option-global-section)

;;;###autoload (autoload 'beads-agent-show "beads-command-agent" nil t)
(beads-meta-define-transient beads-command-agent-show "beads-agent-show"
  "Show agent bead details."
  beads-option-global-section)

;;;###autoload (autoload 'beads-agent-backfill-labels "beads-command-agent" nil t)
(beads-meta-define-transient beads-command-agent-backfill-labels
    "beads-agent-backfill-labels"
  "Backfill role_type/rig labels on existing agent beads."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-agent-menu "beads-command-agent" nil t)
(transient-define-prefix beads-agent-menu ()
  "Manage agent bead state.

Agent beads (labeled gt:agent) can self-report their state."
  ["Agent Commands"
   ("s" "Set state" beads-agent-state)
   ("h" "Heartbeat" beads-agent-heartbeat)
   ("S" "Show details" beads-agent-show)
   ("b" "Backfill labels" beads-agent-backfill-labels)])

(provide 'beads-command-agent)
;;; beads-command-agent.el ends here
