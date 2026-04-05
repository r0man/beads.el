;;; beads-command-audit.el --- Audit command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd audit' operations.
;; Audit records and labels agent interactions for auditing and datasets.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-reader)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-audit-record
;;; ============================================================

(beads-defcommand beads-command-audit-record (beads-command-global-options)
  ((kind
    :type (or null string)
    :short-option "k"
    :choices ("llm_call" "tool_call" "label")
    :group "Entry"
    :level 1
    :order 1)
   (issue-id
    :type (or null string)
    :short-option "i"
    :prompt "Issue ID: "
    :reader beads-reader-issue-id
    :group "Entry"
    :level 1
    :order 2)
   (model
    :type (or null string)
    :short-option "m"
    :group "LLM Call"
    :level 2
    :order 1)
   (prompt
    :type (or null string)
    :short-option "p"
    :group "LLM Call"
    :level 2
    :order 2)
   (response
    :type (or null string)
    :short-option "r"
    :group "LLM Call"
    :level 2
    :order 3)
   (tool-name
    :type (or null string)
    :short-option "t"
    :group "Tool Call"
    :level 2
    :order 1)
   (exit-code
    :type (or null string integer)
    :short-option "e"
    :group "Tool Call"
    :level 2
    :order 2)
   (error-msg
    :long-option "error"
    :type (or null string)
    :short-option "E"
    :group "Tool Call"
    :level 2
    :order 3)
   (stdin
    :type boolean
    :short-option "s"
    :group "Entry"
    :level 2
    :order 3))
  :documentation "Represents bd audit record command.
Appends an audit interaction entry.")


;;; ============================================================
;;; Command Class: beads-command-audit-label
;;; ============================================================

(beads-defcommand beads-command-audit-label (beads-command-global-options)
  ((entry-id
    :positional 1)
   (label
    :type (or null string)
    :short-option "l"
    :prompt "Label (good/bad): "
    :choices ("good" "bad")
    :group "Options"
    :level 1
    :order 1)
   (reason
    :type (or null string)
    :short-option "r"
    :transient beads-transient-multiline
    :documentation "Label Reason"
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd audit label command.
Appends a label entry referencing an existing interaction.")


(cl-defmethod beads-command-validate ((command beads-command-audit-label))
  "Validate audit label COMMAND."
  (with-slots (entry-id label) command
    (cond
     ((not entry-id) "Entry ID is required")
     ((not label) "Label is required")
     (t nil))))

;;; Execute Interactive Methods



;;; Transient Menus

;;;###autoload (autoload 'beads-audit-record "beads-command-audit" nil t)
(beads-meta-define-transient beads-command-audit-record "beads-audit-record"
  "Record an audit interaction entry."
  beads-option-global-section)

;;;###autoload (autoload 'beads-audit-label "beads-command-audit" nil t)
(beads-meta-define-transient beads-command-audit-label "beads-audit-label"
  "Label an existing audit entry."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-audit "beads-command-audit" nil t)
(transient-define-prefix beads-audit ()
  "Audit log management.

Audit entries are appended to .beads/interactions.jsonl."
  ["Audit Commands"
   ("r" "Record entry" beads-audit-record)
   ("l" "Label entry" beads-audit-label)])

(provide 'beads-command-audit)
;;; beads-command-audit.el ends here
