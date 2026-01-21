;;; beads-command-edit.el --- Edit command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the EIEIO command class for `bd edit' operation.
;; Edit opens an issue field in $EDITOR for modification.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;; Forward declarations
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-buffer-parse-show "beads-buffer")
(declare-function beads-completion-read-issue "beads-completion")
(declare-function beads-check-executable "beads")
(declare-function beads--sanitize-string "beads")
(declare-function beads--string-blank-p "beads")
(defvar beads-show--issue-id)
(defvar beads-edit--issue-id)

;;; ============================================================
;;; Command Class: beads-command-edit
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-edit (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to edit."
    :positional 1
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID (required)"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue ID: "
    :transient-reader beads-reader-edit-issue-id
    :transient-group "Edit Issue"
    :transient-level 1
    :transient-order 1)
   (title
    :initarg :title
    :type boolean
    :initform nil
    :documentation "Edit the title."
    :long-option "title"
    :option-type :boolean
    :transient-key "t"
    :transient-description "--title"
    :transient-class transient-switch
    :transient-argument "--title"
    :transient-group "Field"
    :transient-level 1
    :transient-order 1)
   (description
    :initarg :description
    :type boolean
    :initform nil
    :documentation "Edit the description (default)."
    :long-option "description"
    :option-type :boolean
    :transient-key "d"
    :transient-description "--description"
    :transient-class transient-switch
    :transient-argument "--description"
    :transient-group "Field"
    :transient-level 1
    :transient-order 2)
   (design
    :initarg :design
    :type boolean
    :initform nil
    :documentation "Edit the design notes."
    :long-option "design"
    :option-type :boolean
    :transient-key "D"
    :transient-description "--design"
    :transient-class transient-switch
    :transient-argument "--design"
    :transient-group "Field"
    :transient-level 1
    :transient-order 3)
   (notes
    :initarg :notes
    :type boolean
    :initform nil
    :documentation "Edit the notes."
    :long-option "notes"
    :option-type :boolean
    :transient-key "n"
    :transient-description "--notes"
    :transient-class transient-switch
    :transient-argument "--notes"
    :transient-group "Field"
    :transient-level 1
    :transient-order 4)
   (acceptance
    :initarg :acceptance
    :type boolean
    :initform nil
    :documentation "Edit the acceptance criteria."
    :long-option "acceptance"
    :option-type :boolean
    :transient-key "a"
    :transient-description "--acceptance"
    :transient-class transient-switch
    :transient-argument "--acceptance"
    :transient-group "Field"
    :transient-level 1
    :transient-order 5))
  :documentation "Represents bd edit command.
Opens an issue field in $EDITOR for modification."))

(cl-defmethod beads-command-subcommand ((_command beads-command-edit))
  "Return \"edit\" as the CLI subcommand."
  "edit")

(cl-defmethod beads-command-validate ((command beads-command-edit))
  "Validate edit COMMAND.  Requires issue-id."
  (with-slots (issue-id) command
    (cond
     ((not issue-id) "Issue ID is required")
     ((string-empty-p issue-id) "Issue ID cannot be empty")
     (t nil))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-edit))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Interactive Edit Workflow

(defun beads-edit--detect-issue-id ()
  "Detect issue ID from current context.
Returns issue ID string or nil if not found."
  (or
   ;; From beads-list buffer
   (when (derived-mode-p 'beads-list-mode)
     (beads-list--current-issue-id))
   ;; From beads-show buffer
   (when (derived-mode-p 'beads-show-mode)
     beads-show--issue-id)
   ;; From buffer name (*beads-show[PROJECT]/ISSUE-ID*)
   (when-let ((parsed (beads-buffer-parse-show (buffer-name))))
     (plist-get parsed :issue-id))))

(defun beads-edit--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-edit instance.
Returns a beads-command-edit object populated with values from ARGS."
  (let* ((issue-id (beads--sanitize-string
                    (transient-arg-value "--id=" args)))
         (title (transient-arg-value "--title" args))
         (description (transient-arg-value "--description" args))
         (design (transient-arg-value "--design" args))
         (notes (transient-arg-value "--notes" args))
         (acceptance (transient-arg-value "--acceptance" args)))
    (beads-command-edit
     :issue-id issue-id
     :title (when title t)
     :description (when description t)
     :design (when design t)
     :notes (when notes t)
     :acceptance (when acceptance t))))

(defun beads-edit--validate-issue-id (issue-id)
  "Validate that ISSUE-ID is set.
Returns error message string if invalid, nil if valid."
  (when (beads--string-blank-p issue-id)
    "Issue ID is required"))

(defun beads-edit--validate-all (cmd)
  "Validate all parameters from CMD beads-command-edit instance.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-edit--validate-issue-id (oref cmd issue-id)))))

;;; Suffix Commands

(transient-define-suffix beads-edit--execute ()
  "Execute the bd edit command with current parameters."
  :key "x"
  :description "Edit field"
  (interactive)
  (let* ((args (transient-args 'beads-edit--menu))
         (cmd (beads-edit--parse-transient-args args))
         (errors (beads-edit--validate-all cmd)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (beads-command-execute-interactive cmd))))

(transient-define-suffix beads-edit--reset ()
  "Reset all parameters to their default values."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "All fields reset")))

(transient-define-suffix beads-edit--preview ()
  "Preview the bd edit command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-edit--menu))
         (cmd (beads-edit--parse-transient-args args))
         (errors (beads-edit--validate-all cmd)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                               (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (let* ((cmd-list (beads-command-line cmd))
             (cmd-string (mapconcat #'shell-quote-argument cmd-list " "))
             (preview-msg (format "Command: %s" cmd-string)))
        (message "%s" preview-msg)
        preview-msg))))

;;; Transient Infixes

(transient-define-infix beads-edit--infix-issue-id ()
  :class 'transient-option
  :key "i"
  :description "Issue ID (required)"
  :argument "--id="
  :prompt "Issue ID: "
  :reader #'beads-reader-edit-issue-id)

;;; Main Transient Menu

(transient-define-prefix beads-edit--menu ()
  "Transient menu for editing an issue field in Beads."
  ["Edit Issue"
   (beads-edit--infix-issue-id)]
  ["Field to Edit"
   ("t" "--title" "--title")
   ("d" "--description" "--description")
   ("D" "--design" "--design")
   ("n" "--notes" "--notes")
   ("a" "--acceptance" "--acceptance")]
  beads-option-global-section
  ["Actions"
   (beads-edit--execute)
   (beads-edit--preview)
   (beads-edit--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-edit (&optional issue-id)
  "Edit an issue field in $EDITOR.

This function provides an interactive interface for editing issue
fields via a transient menu.  The function is context-aware and
automatically detects the issue ID from beads-list or beads-show
buffers.

If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user."
  (interactive
   (list (or (beads-edit--detect-issue-id)
             (beads-completion-read-issue
              "Edit issue: " nil t nil 'beads--issue-id-history))))
  ;; Check executable
  (beads-check-executable)
  ;; Show the transient menu with initial issue ID if provided
  (if issue-id
      ;; Set initial value using transient-args
      (transient-setup 'beads-edit--menu nil nil
                       :value (list (concat "--id=" issue-id)))
    (transient-setup 'beads-edit--menu)))

;; Auto-generated transient for testing/internal use
;;;###autoload (autoload 'beads-edit-transient "beads-command-edit" nil t)
(beads-meta-define-transient beads-command-edit "beads-edit-transient"
  "Edit an issue field in $EDITOR (auto-generated menu).

Opens the specified field (or description by default) in your
configured editor for modification.

See `beads-edit' for the full user-facing transient menu."
  beads-option-global-section)

(provide 'beads-command-edit)
;;; beads-command-edit.el ends here
