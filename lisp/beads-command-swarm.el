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

(beads-defcommand beads-command-swarm-create (beads-command-global-options)
  ((epic-id
    :positional 1)
   (coordinator
    :option-type :string
    :key "c"
    :prompt "Coordinator agent: "
    :group "Options"
    :level 1
    :order 1)
   (force
    :option-type :boolean
    :key "f"
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd swarm create command.
Creates a swarm molecule from an epic.")


(cl-defmethod beads-command-validate ((command beads-command-swarm-create))
  "Validate swarm create COMMAND."
  (with-slots (epic-id) command
    (if (not epic-id) "Epic ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-swarm-list
;;; ============================================================

(beads-defcommand beads-command-swarm-list (beads-command-global-options)
  ()
  :documentation "Represents bd swarm list command.
Lists all swarm molecules.")


;;; ============================================================
;;; Command Class: beads-command-swarm-status
;;; ============================================================

(beads-defcommand beads-command-swarm-status (beads-command-global-options)
  ((swarm-id
    :positional 1))
  :documentation "Represents bd swarm status command.
Shows current swarm status.")


;;; ============================================================
;;; Command Class: beads-command-swarm-validate
;;; ============================================================

(beads-defcommand beads-command-swarm-validate (beads-command-global-options)
  ((epic-id
    :positional 1))
  :documentation "Represents bd swarm validate command.
Validates epic structure for swarming.")


(cl-defmethod beads-command-validate ((command beads-command-swarm-validate))
  "Validate swarm validate COMMAND."
  (with-slots (epic-id) command
    (if (not epic-id) "Epic ID is required" nil)))

;;; Execute Interactive Methods





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
