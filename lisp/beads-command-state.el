;;; beads-command-state.el --- State command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd state' and `bd set-state'
;; operations.  State manages operational state dimensions on issues.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-set-state
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-set-state (beads-command-json)
    ((issue-id
      :initarg :issue-id
      :type (or null string)
      :initform nil
      :documentation "Issue ID."
      :positional 1)
     (dimension-value
      :initarg :dimension-value
      :type (or null string)
      :initform nil
      :documentation "State change as dimension=value."
      :positional 2)
     (reason
      :initarg :reason
      :type (or null string)
      :initform nil
      :documentation "Reason for the state change."
      :long-option "reason"
      :option-type :string
      :key "r"
      :transient "--reason"
      :class beads-transient-multiline
      :argument "--reason="
      :field-name "State Change Reason"
      :transient-group "Options"
      :level 1
      :order 1))
    :documentation "Represents bd set-state command.
  Atomically sets operational state on an issue."))

(cl-defmethod beads-command-subcommand ((_command beads-command-set-state))
  "Return \"set-state\" as the CLI subcommand."
  "set-state")

(cl-defmethod beads-command-validate ((command beads-command-set-state))
  "Validate set-state COMMAND."
  (with-slots (issue-id dimension-value) command
    (cond
     ((not issue-id) "Issue ID is required")
     ((not dimension-value) "State (dimension=value) is required")
     ((not (string-match-p "=" dimension-value))
      "State must be in dimension=value format")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-state
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-state (beads-command-json)
    ((issue-id
      :initarg :issue-id
      :type (or null string)
      :initform nil
      :documentation "Issue ID."
      :positional 1)
     (dimension
      :initarg :dimension
      :type (or null string)
      :initform nil
      :documentation "State dimension to query."
      :positional 2))
    :documentation "Represents bd state command.
  Queries the current value of a state dimension."))

(cl-defmethod beads-command-subcommand ((_command beads-command-state))
  "Return \"state\" as the CLI subcommand."
  "state")

(cl-defmethod beads-command-validate ((command beads-command-state))
  "Validate state COMMAND."
  (with-slots (issue-id dimension) command
    (cond
     ((not issue-id) "Issue ID is required")
     ((not dimension) "Dimension is required")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-state-list
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-state-list (beads-command-json)
    ((issue-id
      :initarg :issue-id
      :type (or null string)
      :initform nil
      :documentation "Issue ID."
      :positional 1))
    :documentation "Represents bd state list command.
  Lists all state dimensions on an issue."))

(cl-defmethod beads-command-subcommand ((_command beads-command-state-list))
  "Return \"state list\" as the CLI subcommand."
  "state list")

(cl-defmethod beads-command-validate ((command beads-command-state-list))
  "Validate state list COMMAND."
  (with-slots (issue-id) command
    (if (not issue-id) "Issue ID is required" nil)))

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-set-state))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-state))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-state-list))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-set-state "beads-command-state" nil t)
(beads-meta-define-transient beads-command-set-state "beads-set-state"
  "Set operational state on an issue."
  beads-option-global-section)

;;;###autoload (autoload 'beads-state-query "beads-command-state" nil t)
(beads-meta-define-transient beads-command-state "beads-state-query"
  "Query state dimension value."
  beads-option-global-section)

;;;###autoload (autoload 'beads-state-list "beads-command-state" nil t)
(beads-meta-define-transient beads-command-state-list "beads-state-list"
  "List all state dimensions on issue."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-state "beads-command-state" nil t)
(transient-define-prefix beads-state-menu ()
  "State management for issues.

State labels: dimension:value (e.g., patrol:active, mode:degraded)."
  ["State Commands"
   ("s" "Set state" beads-set-state)
   ("q" "Query state" beads-state-query)
   ("l" "List states" beads-state-list)])

(provide 'beads-command-state)
;;; beads-command-state.el ends here
