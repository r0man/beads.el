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

;;;###autoload (autoload 'beads-swarm-create "beads-command-swarm" nil t)
(beads-defcommand beads-command-swarm-create (beads-command-global-options)
  ((epic-id
    :positional 1
    :required t)
   (coordinator
    :type (or null string)
    :short-option "c"
    :prompt "Coordinator agent: "
    :group "Options"
    :level 1
    :order 1)
   (force
    :type boolean
    :short-option "f"
    :group "Options"
    :level 1
    :order 2))
  :documentation "Creates a swarm molecule from an epic.")

;;; ============================================================
;;; Command Class: beads-command-swarm-list
;;; ============================================================

;;;###autoload (autoload 'beads-swarm-list "beads-command-swarm" nil t)
(beads-defcommand beads-command-swarm-list (beads-command-global-options)
  ()
  :documentation "Lists all swarm molecules.")

;;; ============================================================
;;; Command Class: beads-command-swarm-status
;;; ============================================================

;;;###autoload (autoload 'beads-swarm-status "beads-command-swarm" nil t)
(beads-defcommand beads-command-swarm-status (beads-command-global-options)
  ((swarm-id
    :positional 1))
  :documentation "Shows current swarm status.")

;;; ============================================================
;;; Command Class: beads-command-swarm-validate
;;; ============================================================

;;;###autoload (autoload 'beads-swarm-validate "beads-command-swarm" nil t)
(beads-defcommand beads-command-swarm-validate (beads-command-global-options)
  ((epic-id
    :positional 1
    :required t))
  :documentation "Validates epic structure for swarming.")

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
