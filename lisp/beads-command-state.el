;;; beads-command-state.el --- State command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd state' and `bd set-state'
;; operations.  State manages operational state dimensions on issues.
;; The class includes full slot metadata for automatic transient menu
;; generation via `beads-defcommand'.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-set-state
;;; ============================================================

;;;###autoload (autoload 'beads-set-state "beads-command-state" nil t)
(beads-defcommand beads-command-set-state (beads-command-global-options)
  ((issue-id
    :positional 1
    :required t)
   (dimension-value
    :positional 2
    :required t)
   (reason
    :type (or null string)
    :short-option "r"
    :transient beads-transient-multiline
    :documentation "Reason for the state change (recorded in event)"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd set-state command.
Atomically sets operational state on an issue."
  :cli-command "set-state")

(cl-defmethod beads-command-validate ((command beads-command-set-state))
  "Validate set-state COMMAND.
Checks :required slots via base method, plus format validation."
  (let ((errors (cl-call-next-method)))
    (with-slots (dimension-value) command
      (when (and dimension-value (not (string-match-p "=" dimension-value)))
        (push "State must be in dimension=value format" errors)))
    errors))

;;; ============================================================
;;; Command Class: beads-command-state
;;; ============================================================

(beads-defcommand beads-command-state (beads-command-global-options)
  ((issue-id
    :positional 1
    :required t)
   (dimension
    :positional 2
    :required t))
  :documentation "Represents bd state command.
Queries the current value of a state dimension."
  :transient :manual)

;;; ============================================================
;;; Command Class: beads-command-state-list
;;; ============================================================

;;;###autoload (autoload 'beads-state-list "beads-command-state" nil t)
(beads-defcommand beads-command-state-list (beads-command-global-options)
  ((issue-id
    :positional 1
    :required t))
  :documentation "Represents bd state list command.
Lists all state dimensions on an issue.")


;;; Transient Menus

;;;###autoload (autoload 'beads-state-query "beads-command-state" nil t)
(beads-meta-define-transient beads-command-state "beads-state-query"
  "Query state dimension value."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-state-menu "beads-command-state" nil t)
(transient-define-prefix beads-state-menu ()
  "State management for issues.

State labels: dimension:value (e.g., patrol:active, mode:degraded)."
  ["State Commands"
   ("s" "Set state" beads-set-state)
   ("q" "Query state" beads-state-query)
   ("l" "List states" beads-state-list)])

(provide 'beads-command-state)
;;; beads-command-state.el ends here
