;;; beads-command-migrate.el --- Migrate command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd migrate' operations.
;; Migrate provides database migration and data transformation commands.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-migrate (main)
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-migrate (beads-command-json)
    ((dry-run
      :initarg :dry-run
      :type boolean
      :initform nil
      :documentation "Show what would be done."
      :long-option "dry-run"
      :option-type :boolean
      :key "n"
      :transient "--dry-run"
      :class transient-switch
      :argument "--dry-run"
      :transient-group "Options"
      :level 1
      :order 1)
     (cleanup
      :initarg :cleanup
      :type boolean
      :initform nil
      :documentation "Remove old database files after migration."
      :long-option "cleanup"
      :option-type :boolean
      :key "c"
      :transient "--cleanup"
      :class transient-switch
      :argument "--cleanup"
      :transient-group "Options"
      :level 1
      :order 2)
     (yes
      :initarg :yes
      :type boolean
      :initform nil
      :documentation "Auto-confirm prompts."
      :long-option "yes"
      :short-option "y"
      :option-type :boolean
      :key "y"
      :transient "--yes"
      :class transient-switch
      :argument "--yes"
      :transient-group "Options"
      :level 1
      :order 3)
     (inspect
      :initarg :inspect
      :type boolean
      :initform nil
      :documentation "Show migration plan for AI analysis."
      :long-option "inspect"
      :option-type :boolean
      :key "i"
      :transient "--inspect"
      :class transient-switch
      :argument "--inspect"
      :transient-group "Options"
      :level 2
      :order 4)
     (update-repo-id
      :initarg :update-repo-id
      :type boolean
      :initform nil
      :documentation "Update repo_id on issues to match repo name."
      :long-option "update-repo-id"
      :option-type :boolean
      :key "u"
      :transient "--update-repo-id"
      :class transient-switch
      :argument "--update-repo-id"
      :transient-group "Options"
      :level 2
      :order 5))
    :documentation "Represents bd migrate command.
  Detects and migrates database schema to current version."))

(cl-defmethod beads-command-subcommand ((_command beads-command-migrate))
  "Return \"migrate\" as the CLI subcommand."
  "migrate")

;;; ============================================================
;;; Command Class: beads-command-migrate-hash-ids
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-migrate-hash-ids (beads-command-json)
    ((dry-run
      :initarg :dry-run
      :type boolean
      :initform nil
      :documentation "Show what would be done."
      :long-option "dry-run"
      :option-type :boolean
      :key "n"
      :transient "--dry-run"
      :class transient-switch
      :argument "--dry-run"
      :transient-group "Options"
      :level 1
      :order 1))
    :documentation "Represents bd migrate hash-ids command.
  Migrates sequential IDs to hash-based IDs (legacy)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-migrate-hash-ids))
  "Return \"migrate hash-ids\" as the CLI subcommand."
  "migrate hash-ids")

;;; ============================================================
;;; Command Class: beads-command-migrate-issues
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-migrate-issues (beads-command-json)
    ((source
      :initarg :source
      :type (or null string)
      :initform nil
      :documentation "Source repository path."
      :positional 1)
     (target
      :initarg :target
      :type (or null string)
      :initform nil
      :documentation "Target repository path."
      :positional 2))
    :documentation "Represents bd migrate issues command.
  Moves issues between repositories."))

(cl-defmethod beads-command-subcommand ((_command beads-command-migrate-issues))
  "Return \"migrate issues\" as the CLI subcommand."
  "migrate issues")

;;; ============================================================
;;; Command Class: beads-command-migrate-sync
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-migrate-sync (beads-command-json)
    ()
    :documentation "Represents bd migrate sync command.
  Migrates to sync.branch workflow for multi-clone setups."))

(cl-defmethod beads-command-subcommand ((_command beads-command-migrate-sync))
  "Return \"migrate sync\" as the CLI subcommand."
  "migrate sync")

;;; ============================================================
;;; Command Class: beads-command-migrate-tombstones
;;; ============================================================

(eval-and-compile
  (beads-defcommand beads-command-migrate-tombstones (beads-command-json)
    ((dry-run
      :initarg :dry-run
      :type boolean
      :initform nil
      :documentation "Show what would be done."
      :long-option "dry-run"
      :option-type :boolean
      :key "n"
      :transient "--dry-run"
      :class transient-switch
      :argument "--dry-run"
      :transient-group "Options"
      :level 1
      :order 1))
    :documentation "Represents bd migrate tombstones command.
  Converts deletions.jsonl to inline tombstones."))

(cl-defmethod beads-command-subcommand ((_command beads-command-migrate-tombstones))
  "Return \"migrate tombstones\" as the CLI subcommand."
  "migrate tombstones")

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-migrate))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-migrate-hash-ids))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-migrate-issues))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-migrate-sync))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-migrate-tombstones))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-migrate "beads-command-migrate" nil t)
(beads-meta-define-transient beads-command-migrate "beads-migrate"
  "Detect and migrate database schema."
  beads-option-global-section)

;;;###autoload (autoload 'beads-migrate-hash-ids "beads-command-migrate" nil t)
(beads-meta-define-transient beads-command-migrate-hash-ids "beads-migrate-hash-ids"
  "Migrate sequential IDs to hash-based IDs."
  beads-option-global-section)

;;;###autoload (autoload 'beads-migrate-issues "beads-command-migrate" nil t)
(beads-meta-define-transient beads-command-migrate-issues "beads-migrate-issues"
  "Move issues between repositories."
  beads-option-global-section)

;;;###autoload (autoload 'beads-migrate-sync "beads-command-migrate" nil t)
(beads-meta-define-transient beads-command-migrate-sync "beads-migrate-sync"
  "Migrate to sync.branch workflow."
  beads-option-global-section)

;;;###autoload (autoload 'beads-migrate-tombstones "beads-command-migrate" nil t)
(beads-meta-define-transient beads-command-migrate-tombstones "beads-migrate-tombstones"
  "Convert deletions.jsonl to inline tombstones."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-migrate-menu "beads-command-migrate" nil t)
(transient-define-prefix beads-migrate-menu ()
  "Database migration commands."
  ["Migrate Commands"
   ("m" "Auto-migrate" beads-migrate)
   ("h" "Hash IDs (legacy)" beads-migrate-hash-ids)
   ("i" "Move issues" beads-migrate-issues)
   ("s" "Sync workflow" beads-migrate-sync)
   ("t" "Tombstones" beads-migrate-tombstones)])

(provide 'beads-command-migrate)
;;; beads-command-migrate.el ends here
