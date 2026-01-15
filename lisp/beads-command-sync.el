;;; beads-command-sync.el --- Sync command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the EIEIO command class for `bd sync' operation.
;; Sync synchronizes issues with git remote through pull, merge, export,
;; commit, and push operations.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-sync
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-sync (beads-command-json)
  ((dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview sync without making changes."
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (no-pull
    :initarg :no-pull
    :type boolean
    :initform nil
    :documentation "Skip pulling from remote."
    :long-option "--no-pull"
    :option-type :boolean
    :transient-key "-P"
    :transient-description "--no-pull"
    :transient-class transient-switch
    :transient-argument "--no-pull"
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (no-push
    :initarg :no-push
    :type boolean
    :initform nil
    :documentation "Skip pushing to remote."
    :long-option "--no-push"
    :option-type :boolean
    :transient-key "-p"
    :transient-description "--no-push"
    :transient-class transient-switch
    :transient-argument "--no-push"
    :transient-group "Options"
    :transient-level 1
    :transient-order 3)
   (flush-only
    :initarg :flush-only
    :type boolean
    :initform nil
    :documentation "Only export pending changes to JSONL."
    :long-option "--flush-only"
    :option-type :boolean
    :transient-key "-f"
    :transient-description "--flush-only"
    :transient-class transient-switch
    :transient-argument "--flush-only"
    :transient-group "Options"
    :transient-level 2
    :transient-order 4)
   (import-only
    :initarg :import-only
    :type boolean
    :initform nil
    :documentation "Only import from JSONL."
    :long-option "--import-only"
    :option-type :boolean
    :transient-key "-i"
    :transient-description "--import-only"
    :transient-class transient-switch
    :transient-argument "--import-only"
    :transient-group "Options"
    :transient-level 2
    :transient-order 5)
   (squash
    :initarg :squash
    :type boolean
    :initform nil
    :documentation "Accumulate changes without committing."
    :long-option "--squash"
    :option-type :boolean
    :transient-key "-s"
    :transient-description "--squash"
    :transient-class transient-switch
    :transient-argument "--squash"
    :transient-group "Options"
    :transient-level 2
    :transient-order 6)
   (message
    :initarg :message
    :type (or null string)
    :initform nil
    :documentation "Commit message."
    :long-option "--message"
    :short-option "-m"
    :option-type :string
    :transient-key "-m"
    :transient-description "--message"
    :transient-class transient-option
    :transient-argument "--message="
    :transient-prompt "Commit message: "
    :transient-group "Options"
    :transient-level 2
    :transient-order 7)
   (status-flag
    :initarg :status-flag
    :type boolean
    :initform nil
    :documentation "Show diff between sync branch and main."
    :long-option "--status"
    :option-type :boolean
    :transient-key "-S"
    :transient-description "--status"
    :transient-class transient-switch
    :transient-argument "--status"
    :transient-group "Options"
    :transient-level 2
    :transient-order 8)
   (merge-flag
    :initarg :merge-flag
    :type boolean
    :initform nil
    :documentation "Merge sync branch back to main."
    :long-option "--merge"
    :option-type :boolean
    :transient-key "-M"
    :transient-description "--merge"
    :transient-class transient-switch
    :transient-argument "--merge"
    :transient-group "Options"
    :transient-level 2
    :transient-order 9)
   (check
    :initarg :check
    :type boolean
    :initform nil
    :documentation "Pre-sync integrity check."
    :long-option "--check"
    :option-type :boolean
    :transient-key "-c"
    :transient-description "--check"
    :transient-class transient-switch
    :transient-argument "--check"
    :transient-group "Options"
    :transient-level 2
    :transient-order 10)
   (accept-rebase
    :initarg :accept-rebase
    :type boolean
    :initform nil
    :documentation "Accept rebased history from remote."
    :long-option "--accept-rebase"
    :option-type :boolean
    :transient-key "-r"
    :transient-description "--accept-rebase"
    :transient-class transient-switch
    :transient-argument "--accept-rebase"
    :transient-group "Options"
    :transient-level 3
    :transient-order 11)
   (from-main
    :initarg :from-main
    :type boolean
    :initform nil
    :documentation "Merge from main branch instead of sync branch."
    :long-option "--from-main"
    :option-type :boolean
    :transient-key "-F"
    :transient-description "--from-main"
    :transient-class transient-switch
    :transient-argument "--from-main"
    :transient-group "Options"
    :transient-level 3
    :transient-order 12)
   (no-git-history
    :initarg :no-git-history
    :type boolean
    :initform nil
    :documentation "Disable git history tracking in sync."
    :long-option "--no-git-history"
    :option-type :boolean
    :transient-key "-H"
    :transient-description "--no-git-history"
    :transient-class transient-switch
    :transient-argument "--no-git-history"
    :transient-group "Options"
    :transient-level 3
    :transient-order 13)
   (rename-on-import
    :initarg :rename-on-import
    :type boolean
    :initform nil
    :documentation "Rename issues to match repo on import."
    :long-option "--rename-on-import"
    :option-type :boolean
    :transient-key "-R"
    :transient-description "--rename-on-import"
    :transient-class transient-switch
    :transient-argument "--rename-on-import"
    :transient-group "Options"
    :transient-level 3
    :transient-order 14))
  :documentation "Represents bd sync command.
Synchronizes issues with git remote."))

(cl-defmethod beads-command-subcommand ((_command beads-command-sync))
  "Return \"sync\" as the CLI subcommand."
  "sync")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-sync))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menu

;;;###autoload (autoload 'beads-sync "beads-command-sync" nil t)
(beads-meta-define-transient beads-command-sync "beads-sync"
  "Synchronize issues with git remote.

Operations performed:
1. Pull from remote (fetch + merge)
2. Merge local and remote issues (3-way merge)
3. Export merged state to JSONL
4. Commit changes to git
5. Push to remote

Transient levels control which options are visible (cycle with C-x l):
  Level 1: dry-run, no-pull, no-push
  Level 2: flush-only, import-only, squash, message, status, merge, check"
  beads-option-global-section)

(provide 'beads-command-sync)
;;; beads-command-sync.el ends here
