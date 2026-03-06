;;; beads-command-branch.el --- Branch command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-branch' EIEIO class for the
;; `bd branch' command.  Lists or creates Dolt branches in the beads
;; database.
;;
;; Without arguments: lists all branches.
;; With a name argument: creates a new branch.
;;
;; Usage:
;;   (beads-command-branch!)           ; list branches
;;   (beads-command-branch! :name "feature")  ; create branch
;;   (beads-branch)  ; invoke transient menu

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; Branch Command

(beads-defcommand beads-command-branch (beads-command-global-options)
  ((name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Branch name to create (positional, optional).
When nil, lists all branches."
    :positional 1
    :option-type :string
    :key "n"
    :transient "Branch name"
    :class transient-option
    :argument "--name="
    :prompt "Branch name (empty=list): "
    :transient-group "Branch"
    :level 1
    :order 1))
  :documentation "List or create Dolt branches.
Without arguments, lists all branches.
With a name, creates a new branch.
Requires Dolt backend.")


(cl-defmethod beads-command-validate ((_command beads-command-branch))
  "Validate branch COMMAND.
No required fields, returns nil (valid)."
  nil)


;;; Transient Menu

;;;###autoload (autoload 'beads-branch "beads-command-branch" nil t)
(beads-meta-define-transient beads-command-branch "beads-branch"
  "List or create Dolt branches.

Without a branch name, lists all branches.
With a branch name, creates a new branch.
Requires Dolt backend."
  beads-option-global-section)

(provide 'beads-command-branch)
;;; beads-command-branch.el ends here
