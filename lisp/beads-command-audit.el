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
    :key "k"
    :transient "--kind"
    :class transient-option
    :argument "--kind="
    :prompt "Kind: "
    :choices ("llm_call" "tool_call" "label")
    :transient-group "Entry"
    :level 1
    :order 1)
   (issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Related issue ID."
    :long-option "issue-id"
    :option-type :string
    :key "i"
    :transient "--issue-id"
    :class transient-option
    :argument "--issue-id="
    :prompt "Issue ID: "
    :transient-group "Entry"
    :level 1
    :order 2)
   (model
    :initarg :model
    :type (or null string)
    :initform nil
    :documentation "Model name (llm_call)."
    :long-option "model"
    :option-type :string
    :key "m"
    :transient "--model"
    :class transient-option
    :argument "--model="
    :prompt "Model: "
    :transient-group "LLM Call"
    :level 2
    :order 1)
   (prompt
    :initarg :prompt
    :type (or null string)
    :initform nil
    :documentation "Prompt text (llm_call)."
    :long-option "prompt"
    :option-type :string
    :key "p"
    :transient "--prompt"
    :class transient-option
    :argument "--prompt="
    :prompt "Prompt: "
    :transient-group "LLM Call"
    :level 2
    :order 2)
   (response
    :initarg :response
    :type (or null string)
    :initform nil
    :documentation "Response text (llm_call)."
    :long-option "response"
    :option-type :string
    :key "r"
    :transient "--response"
    :class transient-option
    :argument "--response="
    :prompt "Response: "
    :transient-group "LLM Call"
    :level 2
    :order 3)
   (tool-name
    :initarg :tool-name
    :type (or null string)
    :initform nil
    :documentation "Tool name (tool_call)."
    :long-option "tool-name"
    :option-type :string
    :key "t"
    :transient "--tool-name"
    :class transient-option
    :argument "--tool-name="
    :prompt "Tool name: "
    :transient-group "Tool Call"
    :level 2
    :order 1)
   (exit-code
    :initarg :exit-code
    :type (or null integer)
    :initform nil
    :documentation "Exit code (tool_call)."
    :long-option "exit-code"
    :option-type :integer
    :key "e"
    :transient "--exit-code"
    :class transient-option
    :argument "--exit-code="
    :prompt "Exit code: "
    :transient-group "Tool Call"
    :level 2
    :order 2)
   (error-msg
    :initarg :error-msg
    :type (or null string)
    :initform nil
    :documentation "Error string."
    :long-option "error"
    :option-type :string
    :key "E"
    :transient "--error"
    :class transient-option
    :argument "--error="
    :prompt "Error: "
    :transient-group "Tool Call"
    :level 2
    :order 3)
   (stdin
    :initarg :stdin
    :type boolean
    :initform nil
    :documentation "Read JSON object from stdin."
    :long-option "stdin"
    :option-type :boolean
    :key "s"
    :transient "--stdin"
    :class transient-switch
    :argument "--stdin"
    :transient-group "Entry"
    :level 2
    :order 3))
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
    :key "l"
    :transient "--label"
    :class transient-option
    :argument "--label="
    :prompt "Label (good/bad): "
    :choices ("good" "bad")
    :transient-group "Options"
    :level 1
    :order 1)
   (reason
    :initarg :reason
    :type (or null string)
    :initform nil
    :documentation "Reason for label."
    :long-option "reason"
    :option-type :string
    :key "r"
    :transient "--reason"
    :class transient-option
    :argument "--reason="
    :prompt "Reason: "
    :transient-group "Options"
    :level 1
    :order 2))
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
