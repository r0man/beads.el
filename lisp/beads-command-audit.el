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
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-audit-record
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-audit-record (beads-command-json)
  ((kind
    :initarg :kind
    :type (or null string)
    :initform nil
    :documentation "Entry kind (e.g. llm_call, tool_call, label)."
    :long-option "kind"
    :option-type :string
    :transient-key "k"
    :transient-description "--kind"
    :transient-class transient-option
    :transient-argument "--kind="
    :transient-prompt "Kind: "
    :transient-choices ("llm_call" "tool_call" "label")
    :transient-group "Entry"
    :transient-level 1
    :transient-order 1)
   (issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Related issue ID."
    :long-option "issue-id"
    :option-type :string
    :transient-key "i"
    :transient-description "--issue-id"
    :transient-class transient-option
    :transient-argument "--issue-id="
    :transient-prompt "Issue ID: "
    :transient-group "Entry"
    :transient-level 1
    :transient-order 2)
   (model
    :initarg :model
    :type (or null string)
    :initform nil
    :documentation "Model name (llm_call)."
    :long-option "model"
    :option-type :string
    :transient-key "m"
    :transient-description "--model"
    :transient-class transient-option
    :transient-argument "--model="
    :transient-prompt "Model: "
    :transient-group "LLM Call"
    :transient-level 2
    :transient-order 1)
   (prompt
    :initarg :prompt
    :type (or null string)
    :initform nil
    :documentation "Prompt text (llm_call)."
    :long-option "prompt"
    :option-type :string
    :transient-key "p"
    :transient-description "--prompt"
    :transient-class transient-option
    :transient-argument "--prompt="
    :transient-prompt "Prompt: "
    :transient-group "LLM Call"
    :transient-level 2
    :transient-order 2)
   (response
    :initarg :response
    :type (or null string)
    :initform nil
    :documentation "Response text (llm_call)."
    :long-option "response"
    :option-type :string
    :transient-key "r"
    :transient-description "--response"
    :transient-class transient-option
    :transient-argument "--response="
    :transient-prompt "Response: "
    :transient-group "LLM Call"
    :transient-level 2
    :transient-order 3)
   (tool-name
    :initarg :tool-name
    :type (or null string)
    :initform nil
    :documentation "Tool name (tool_call)."
    :long-option "tool-name"
    :option-type :string
    :transient-key "t"
    :transient-description "--tool-name"
    :transient-class transient-option
    :transient-argument "--tool-name="
    :transient-prompt "Tool name: "
    :transient-group "Tool Call"
    :transient-level 2
    :transient-order 1)
   (exit-code
    :initarg :exit-code
    :type (or null integer)
    :initform nil
    :documentation "Exit code (tool_call)."
    :long-option "exit-code"
    :option-type :integer
    :transient-key "e"
    :transient-description "--exit-code"
    :transient-class transient-option
    :transient-argument "--exit-code="
    :transient-prompt "Exit code: "
    :transient-group "Tool Call"
    :transient-level 2
    :transient-order 2)
   (error-msg
    :initarg :error-msg
    :type (or null string)
    :initform nil
    :documentation "Error string."
    :long-option "error"
    :option-type :string
    :transient-key "E"
    :transient-description "--error"
    :transient-class transient-option
    :transient-argument "--error="
    :transient-prompt "Error: "
    :transient-group "Tool Call"
    :transient-level 2
    :transient-order 3)
   (stdin
    :initarg :stdin
    :type boolean
    :initform nil
    :documentation "Read JSON object from stdin."
    :long-option "stdin"
    :option-type :boolean
    :transient-key "s"
    :transient-description "--stdin"
    :transient-class transient-switch
    :transient-argument "--stdin"
    :transient-group "Entry"
    :transient-level 2
    :transient-order 3))
  :documentation "Represents bd audit record command.
Appends an audit interaction entry."))

(cl-defmethod beads-command-subcommand ((_command beads-command-audit-record))
  "Return \"audit record\" as the CLI subcommand."
  "audit record")

;;; ============================================================
;;; Command Class: beads-command-audit-label
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-audit-label (beads-command-json)
  ((entry-id
    :initarg :entry-id
    :type (or null string)
    :initform nil
    :documentation "Entry ID to label."
    :positional 1)
   (label
    :initarg :label
    :type (or null string)
    :initform nil
    :documentation "Label value (e.g., good or bad)."
    :long-option "label"
    :option-type :string
    :transient-key "l"
    :transient-description "--label"
    :transient-class transient-option
    :transient-argument "--label="
    :transient-prompt "Label (good/bad): "
    :transient-choices ("good" "bad")
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (reason
    :initarg :reason
    :type (or null string)
    :initform nil
    :documentation "Reason for label."
    :long-option "reason"
    :option-type :string
    :transient-key "r"
    :transient-description "--reason"
    :transient-class transient-option
    :transient-argument "--reason="
    :transient-prompt "Reason: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd audit label command.
Appends a label entry referencing an existing interaction."))

(cl-defmethod beads-command-subcommand ((_command beads-command-audit-label))
  "Return \"audit label\" as the CLI subcommand."
  "audit label")

(cl-defmethod beads-command-validate ((command beads-command-audit-label))
  "Validate audit label COMMAND."
  (with-slots (entry-id label) command
    (cond
     ((not entry-id) "Entry ID is required")
     ((not label) "Label is required")
     (t nil))))

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-audit-record))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-audit-label))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

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
