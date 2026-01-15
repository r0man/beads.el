;;; beads-command-gate.el --- Gate command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd gate' operations.
;; Gates are async wait conditions that block workflow steps.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-gate-list
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-gate-list (beads-command-json)
  ((all
    :initarg :all
    :type boolean
    :initform nil
    :documentation "Show all gates including closed."
    :long-option "--all"
    :short-option "-a"
    :option-type :boolean
    :transient-key "-a"
    :transient-description "--all"
    :transient-class transient-switch
    :transient-argument "--all"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit number of gates (default 50)."
    :long-option "--limit"
    :short-option "-n"
    :option-type :integer
    :transient-key "-n"
    :transient-description "--limit"
    :transient-class transient-option
    :transient-argument "--limit="
    :transient-prompt "Limit: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd gate list command.
Lists gate issues."))

(cl-defmethod beads-command-subcommand ((_command beads-command-gate-list))
  "Return \"gate list\" as the CLI subcommand."
  "gate list")

;;; ============================================================
;;; Command Class: beads-command-gate-check
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-gate-check (beads-command-json)
  ((type
    :initarg :type
    :type (or null string)
    :initform nil
    :documentation "Evaluate only gates of this type."
    :long-option "--type"
    :short-option "-t"
    :option-type :string
    :transient-key "-t"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Gate type: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without closing gates."
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (escalate
    :initarg :escalate
    :type boolean
    :initform nil
    :documentation "Escalate all open gates, skipping evaluation."
    :long-option "--escalate"
    :short-option "-e"
    :option-type :boolean
    :transient-key "-e"
    :transient-description "--escalate"
    :transient-class transient-switch
    :transient-argument "--escalate"
    :transient-group "Options"
    :transient-level 1
    :transient-order 3)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit number of gates to check."
    :long-option "--limit"
    :short-option "-l"
    :option-type :integer
    :transient-key "-l"
    :transient-description "--limit"
    :transient-class transient-option
    :transient-argument "--limit="
    :transient-prompt "Limit: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 4))
  :documentation "Represents bd gate check command.
Evaluates gates and closes resolved ones."))

(cl-defmethod beads-command-subcommand ((_command beads-command-gate-check))
  "Return \"gate check\" as the CLI subcommand."
  "gate check")

;;; ============================================================
;;; Command Class: beads-command-gate-resolve
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-gate-resolve (beads-command-json)
  ((gate-id
    :initarg :gate-id
    :type (or null string)
    :initform nil
    :documentation "Gate ID to resolve."
    :positional 1)
   (reason
    :initarg :reason
    :type (or null string)
    :initform nil
    :documentation "Reason for manual resolution."
    :long-option "--reason"
    :short-option "-r"
    :option-type :string
    :transient-key "-r"
    :transient-description "--reason"
    :transient-class transient-option
    :transient-argument "--reason="
    :transient-prompt "Reason: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd gate resolve command.
Manually resolves (closes) a gate."))

(cl-defmethod beads-command-subcommand ((_command beads-command-gate-resolve))
  "Return \"gate resolve\" as the CLI subcommand."
  "gate resolve")

(cl-defmethod beads-command-validate ((command beads-command-gate-resolve))
  "Validate gate resolve COMMAND."
  (with-slots (gate-id) command
    (if (not gate-id) "Gate ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-gate-show
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-gate-show (beads-command-json)
  ((gate-id
    :initarg :gate-id
    :type (or null string)
    :initform nil
    :documentation "Gate ID to show."
    :positional 1))
  :documentation "Represents bd gate show command.
Shows a gate issue."))

(cl-defmethod beads-command-subcommand ((_command beads-command-gate-show))
  "Return \"gate show\" as the CLI subcommand."
  "gate show")

(cl-defmethod beads-command-validate ((command beads-command-gate-show))
  "Validate gate show COMMAND."
  (with-slots (gate-id) command
    (if (not gate-id) "Gate ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-gate-add-waiter
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-gate-add-waiter (beads-command-json)
  ((gate-id
    :initarg :gate-id
    :type (or null string)
    :initform nil
    :documentation "Gate ID."
    :positional 1)
   (waiter-id
    :initarg :waiter-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to add as waiter."
    :positional 2))
  :documentation "Represents bd gate add-waiter command.
Adds a waiter to a gate."))

(cl-defmethod beads-command-subcommand ((_command beads-command-gate-add-waiter))
  "Return \"gate add-waiter\" as the CLI subcommand."
  "gate add-waiter")

;;; ============================================================
;;; Command Class: beads-command-gate-discover
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-gate-discover (beads-command-json)
  ((gate-id
    :initarg :gate-id
    :type (or null string)
    :initform nil
    :documentation "Gate ID."
    :positional 1))
  :documentation "Represents bd gate discover command.
Discovers await_id for gh:run gates."))

(cl-defmethod beads-command-subcommand ((_command beads-command-gate-discover))
  "Return \"gate discover\" as the CLI subcommand."
  "gate discover")

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-gate-list))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-gate-check))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-gate-resolve))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-gate-show))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-gate-add-waiter))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-gate-discover))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-gate-list "beads-command-gate" nil t)
(beads-meta-define-transient beads-command-gate-list "beads-gate-list"
  "List gate issues."
  beads-option-global-section)

;;;###autoload (autoload 'beads-gate-check "beads-command-gate" nil t)
(beads-meta-define-transient beads-command-gate-check "beads-gate-check"
  "Evaluate gates and close resolved ones."
  beads-option-global-section)

;;;###autoload (autoload 'beads-gate-resolve "beads-command-gate" nil t)
(beads-meta-define-transient beads-command-gate-resolve "beads-gate-resolve"
  "Manually resolve (close) a gate."
  beads-option-global-section)

;;;###autoload (autoload 'beads-gate-show "beads-command-gate" nil t)
(beads-meta-define-transient beads-command-gate-show "beads-gate-show"
  "Show a gate issue."
  beads-option-global-section)

;;;###autoload (autoload 'beads-gate-add-waiter "beads-command-gate" nil t)
(beads-meta-define-transient beads-command-gate-add-waiter "beads-gate-add-waiter"
  "Add a waiter to a gate."
  beads-option-global-section)

;;;###autoload (autoload 'beads-gate-discover "beads-command-gate" nil t)
(beads-meta-define-transient beads-command-gate-discover "beads-gate-discover"
  "Discover await_id for gh:run gates."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-gate "beads-command-gate" nil t)
(transient-define-prefix beads-gate ()
  "Manage async coordination gates.

Gate types: human, timer, gh:run, gh:pr, bead"
  ["Gate Commands"
   ("l" "List gates" beads-gate-list)
   ("c" "Check/evaluate" beads-gate-check)
   ("r" "Resolve gate" beads-gate-resolve)
   ("s" "Show gate" beads-gate-show)
   ("a" "Add waiter" beads-gate-add-waiter)
   ("d" "Discover await_id" beads-gate-discover)])

(provide 'beads-command-gate)
;;; beads-command-gate.el ends here
