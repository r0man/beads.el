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

(beads-defcommand beads-command-migrate (beads-command-global-options)
  ((dry-run
    :option-type :boolean
    :key "n"
    :group "Options"
    :level 1
    :order 1)
   (cleanup
    :option-type :boolean
    :key "c"
    :group "Options"
    :level 1
    :order 2)
   (yes
    :short-option "y"
    :option-type :boolean
    :key "y"
    :group "Options"
    :level 1
    :order 3)
   (inspect
    :option-type :boolean
    :key "i"
    :group "Options"
    :level 2
    :order 4)
   (update-repo-id
    :option-type :boolean
    :key "u"
    :group "Options"
    :level 2
    :order 5))
  :documentation "Represents bd migrate command.
Detects and migrates database schema to current version.")


;;; ============================================================
;;; Command Class: beads-command-migrate-issues
;;; ============================================================

(beads-defcommand beads-command-migrate-issues (beads-command-global-options)
  ((source
    :positional 1)
   (target
    :positional 2))
  :documentation "Represents bd migrate issues command.
Moves issues between repositories.")


;;; ============================================================
;;; Command Class: beads-command-migrate-sync
;;; ============================================================

(beads-defcommand beads-command-migrate-sync (beads-command-global-options)
  ()
  :documentation "Represents bd migrate sync command.
Migrates to sync.branch workflow for multi-clone setups.")


;;; Execute Interactive Methods






;;; Transient Menus

;;;###autoload (autoload 'beads-migrate "beads-command-migrate" nil t)
(beads-meta-define-transient beads-command-migrate "beads-migrate"
  "Detect and migrate database schema."
  beads-option-global-section)

;;;###autoload (autoload 'beads-migrate-issues "beads-command-migrate" nil t)
(beads-meta-define-transient beads-command-migrate-issues "beads-migrate-issues"
  "Move issues between repositories."
  beads-option-global-section)

;;;###autoload (autoload 'beads-migrate-sync "beads-command-migrate" nil t)
(beads-meta-define-transient beads-command-migrate-sync "beads-migrate-sync"
  "Migrate to sync.branch workflow."
  beads-option-global-section)

;;; Migrate Hooks Command

(beads-defcommand beads-command-migrate-hooks (beads-command-global-options)
  ((dry-run
    :option-type :boolean
    :key "n"
    :group "Options"
    :level 1
    :order 1)
   (apply
    :option-type :boolean
    :key "a"
    :group "Options"
    :level 1
    :order 2)
   (yes
    :option-type :boolean
    :key "y"
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd migrate hooks command.
Analyze git hook files and migrate to marker-managed format."
  :cli-command "migrate hooks")

;;;###autoload (autoload 'beads-migrate-hooks "beads-command-migrate" nil t)
(beads-meta-define-transient beads-command-migrate-hooks "beads-migrate-hooks"
  "Migrate git hooks to marker-managed format."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-migrate-menu "beads-command-migrate" nil t)
(transient-define-prefix beads-migrate-menu ()
  "Database migration commands."
  ["Migrate Commands"
   ("m" "Auto-migrate" beads-migrate)
   ("H" "Hooks" beads-migrate-hooks)
   ("i" "Move issues" beads-migrate-issues)
   ("s" "Sync workflow" beads-migrate-sync)])

(provide 'beads-command-migrate)
;;; beads-command-migrate.el ends here
