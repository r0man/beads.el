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

(beads-defcommand beads-command-gate-list (beads-command-global-options)
  ((all
    :initarg :all
    :type boolean
    :initform nil
    :documentation "Show all gates including closed."
    :long-option "all"
    :short-option "a"
    :option-type :boolean
    :key "a"
    :transient "--all"
    :class transient-switch
    :argument "--all"
    :transient-group "Options"
    :level 1
    :order 1)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit number of gates (default 50)."
    :long-option "limit"
    :short-option "n"
    :option-type :integer
    :key "n"
    :transient "--limit"
    :class transient-option
    :argument "--limit="
    :prompt "Limit: "
    :transient-group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd gate list command.
Lists gate issues.")


;;; ============================================================
;;; Command Class: beads-command-gate-check
;;; ============================================================

(beads-defcommand beads-command-gate-check (beads-command-global-options)
  ((type
    :initarg :type
    :type (or null string)
    :initform nil
    :documentation "Evaluate only gates of this type."
    :long-option "type"
    :short-option "t"
    :option-type :string
    :key "t"
    :transient "--type"
    :class transient-option
    :argument "--type="
    :prompt "Gate type: "
    :transient-group "Options"
    :level 1
    :order 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without closing gates."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 2)
   (escalate
    :initarg :escalate
    :type boolean
    :initform nil
    :documentation "Escalate all open gates, skipping evaluation."
    :long-option "escalate"
    :short-option "e"
    :option-type :boolean
    :key "e"
    :transient "--escalate"
    :class transient-switch
    :argument "--escalate"
    :transient-group "Options"
    :level 1
    :order 3)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit number of gates to check."
    :long-option "limit"
    :short-option "l"
    :option-type :integer
    :key "l"
    :transient "--limit"
    :class transient-option
    :argument "--limit="
    :prompt "Limit: "
    :transient-group "Options"
    :level 1
    :order 4))
  :documentation "Represents bd gate check command.
Evaluates gates and closes resolved ones.")


;;; ============================================================
;;; Command Class: beads-command-gate-resolve
;;; ============================================================

(beads-defcommand beads-command-gate-resolve (beads-command-global-options)
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
    :long-option "reason"
    :short-option "r"
    :option-type :string
    :key "r"
    :transient "--reason"
    :class beads-transient-multiline
    :argument "--reason="
    :field-name "Resolve Reason"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd gate resolve command.
Manually resolves (closes) a gate.")


(cl-defmethod beads-command-validate ((command beads-command-gate-resolve))
  "Validate gate resolve COMMAND."
  (with-slots (gate-id) command
    (if (not gate-id) "Gate ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-gate-show
;;; ============================================================

(beads-defcommand beads-command-gate-show (beads-command-global-options)
  ((gate-id
    :initarg :gate-id
    :type (or null string)
    :initform nil
    :documentation "Gate ID to show."
    :positional 1))
  :documentation "Represents bd gate show command.
Shows a gate issue.")


(cl-defmethod beads-command-validate ((command beads-command-gate-show))
  "Validate gate show COMMAND."
  (with-slots (gate-id) command
    (if (not gate-id) "Gate ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-gate-add-waiter
;;; ============================================================

(beads-defcommand beads-command-gate-add-waiter (beads-command-global-options)
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
Adds a waiter to a gate."
  :cli-command "gate add-waiter")

;;; ============================================================
;;; Command Class: beads-command-gate-discover
;;; ============================================================

(beads-defcommand beads-command-gate-discover (beads-command-global-options)
  ((gate-id
    :initarg :gate-id
    :type (or null string)
    :initform nil
    :documentation "Gate ID."
    :positional 1))
  :documentation "Represents bd gate discover command.
Discovers await_id for gh:run gates.")


;;; Execute Interactive Methods







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
