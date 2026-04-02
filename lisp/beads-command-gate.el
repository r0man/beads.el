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
    :short-option "a"
    :option-type :boolean
    :key "a"
    :transient-group "Options"
    :level 1
    :order 1)
   (limit
    :short-option "n"
    :option-type :integer
    :key "n"
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
    :short-option "t"
    :option-type :string
    :key "t"
    :prompt "Gate type: "
    :transient-group "Options"
    :level 1
    :order 1)
   (dry-run
    :option-type :boolean
    :key "n"
    :transient-group "Options"
    :level 1
    :order 2)
   (escalate
    :short-option "e"
    :option-type :boolean
    :key "e"
    :transient-group "Options"
    :level 1
    :order 3)
   (limit
    :short-option "l"
    :option-type :integer
    :key "l"
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
    :positional 1)
   (reason
    :short-option "r"
    :option-type :string
    :key "r"
    :transient beads-transient-multiline
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
    :positional 1)
   (waiter-id
    :positional 2))
  :documentation "Represents bd gate add-waiter command.
Adds a waiter to a gate."
  :cli-command "gate add-waiter")

;;; ============================================================
;;; Command Class: beads-command-gate-discover
;;; ============================================================

(beads-defcommand beads-command-gate-discover (beads-command-global-options)
  ((gate-id
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
