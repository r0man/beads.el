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
    :positional 1)
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

;;; Transient Menu

;;;###autoload (autoload 'beads-edit "beads-command-edit" nil t)
(beads-meta-define-transient beads-command-edit "beads-edit"
  "Edit an issue field in $EDITOR.

Opens the specified field (or description by default) in your
configured editor for modification."
  beads-option-global-section)

(provide 'beads-command-edit)
;;; beads-command-edit.el ends here
