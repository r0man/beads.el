;;; beads-command-vc.el --- Version control command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd vc' operations.
;; Provides git-like version control for the beads Dolt database:
;; - vc commit: Create a commit
;; - vc merge: Merge a branch
;; - vc status: Show branch and uncommitted changes

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-vc-commit
;;; ============================================================

(beads-defcommand beads-command-vc-commit (beads-command)
  ((message
    :initarg :message
    :type (or null string)
    :initform nil
    :documentation "Commit message (-m, --message)."
    :long-option "message"
    :short-option "m"
    :option-type :string
    :key "m"
    :transient "--message"
    :class transient-option
    :argument "--message="
    :prompt "Commit message: "
    :transient-group "Commit"
    :level 1
    :order 1))
  :documentation "Create a Dolt commit from pending changes.
Requires Dolt backend.")



;;; ============================================================
;;; Command Class: beads-command-vc-merge
;;; ============================================================

(beads-defcommand beads-command-vc-merge (beads-command)
  ((branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Branch name to merge (positional argument)."
    :positional 1
    :option-type :string
    :key "b"
    :transient "Branch (required)"
    :class transient-option
    :argument "--branch="
    :prompt "Branch to merge: "
    :transient-group "Merge"
    :level 1
    :order 1
    :required t)
   (strategy
    :initarg :strategy
    :type (or null string)
    :initform nil
    :documentation "Conflict resolution strategy (--strategy).
Values: ours, theirs."
    :long-option "strategy"
    :option-type :string
    :key "s"
    :transient "--strategy"
    :class transient-option
    :argument "--strategy="
    :prompt "Strategy (ours|theirs): "
    :transient-group "Options"
    :level 2
    :order 2))
  :documentation "Merge a Dolt branch into the current branch.
Requires Dolt backend.")


(cl-defmethod beads-command-validate ((command beads-command-vc-merge))
  "Validate vc merge COMMAND.
Branch is required."
  (with-slots (branch) command
    (when (or (null branch) (string-empty-p branch))
      "Must provide a branch name")))


;;; ============================================================
;;; Command Class: beads-command-vc-status
;;; ============================================================

(beads-defcommand beads-command-vc-status (beads-command)
  ()
  :documentation "Show current branch and uncommitted changes.
Requires Dolt backend.")



;;; Transient Menus

;;;###autoload (autoload 'beads-vc-commit "beads-command-vc" nil t)
(beads-meta-define-transient beads-command-vc-commit "beads-vc-commit"
  "Create a Dolt commit from pending changes."
  beads-option-global-section)

;;;###autoload (autoload 'beads-vc-merge "beads-command-vc" nil t)
(beads-meta-define-transient beads-command-vc-merge "beads-vc-merge"
  "Merge a Dolt branch into the current branch."
  beads-option-global-section)

;;;###autoload (autoload 'beads-vc-status "beads-command-vc" nil t)
(beads-meta-define-transient beads-command-vc-status "beads-vc-status"
  "Show current branch and uncommitted changes."
  beads-option-global-section)

;;; Parent Menu

;;;###autoload (autoload 'beads-vc "beads-command-vc" nil t)
(transient-define-prefix beads-vc ()
  "Version control operations for the beads Dolt database.

Provides git-like version control for issue data."
  ["Version Control"
   ("c" "Commit" beads-vc-commit)
   ("m" "Merge branch" beads-vc-merge)
   ("s" "Status" beads-vc-status)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-command-vc)
;;; beads-command-vc.el ends here
