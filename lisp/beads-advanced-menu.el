;;; beads-advanced-menu.el --- Low-frequency maintenance/setup menu -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Low-frequency operations transient menu for beads.el, accessible
;; via > from the main dispatch.  Contains maintenance, setup,
;; integrations, and administration commands.
;;
;; This replaces the low-frequency portion of the old beads-more-menu
;; (Phase 6 of the beads.el UX redesign).

;;; Code:

(require 'transient)

;; Forward declarations
(declare-function beads-doctor "beads-command-misc" nil)
(declare-function beads-preflight "beads-command-misc" nil)
(declare-function beads-upgrade "beads-command-misc" nil)
(declare-function beads-compact "beads-command-compact" nil)
(declare-function beads-flatten "beads-command-misc" nil)
(declare-function beads-gc "beads-command-misc" nil)
(declare-function beads-purge "beads-command-misc" nil)
(declare-function beads-rename-prefix "beads-command-misc" nil)
(declare-function beads-backup "beads-command-misc" nil)
(declare-function beads-export "beads-command-misc" nil)
(declare-function beads-restore "beads-command-restore" nil)
(declare-function beads-branch "beads-command-branch" nil)
(declare-function beads-vc "beads-command-vc" nil)
(declare-function beads-federation "beads-command-federation" nil)
(declare-function beads-sql "beads-command-sql" nil)
(declare-function beads-duplicate "beads-command-misc" nil)
(declare-function beads-duplicates "beads-command-misc" nil)
(declare-function beads-supersede "beads-command-misc" nil)
(declare-function beads-migrate-menu "beads-command-migrate" nil)
(declare-function beads-jira "beads-command-integrations" nil)
(declare-function beads-linear "beads-command-integrations" nil)
(declare-function beads-gitlab "beads-command-integrations" nil)
(declare-function beads-github "beads-command-integrations" nil)
(declare-function beads-repo "beads-command-integrations" nil)
(declare-function beads-mail "beads-command-integrations" nil)
(declare-function beads-init "beads-command-init" nil)
(declare-function beads-bootstrap "beads-command-misc" nil)
(declare-function beads-context "beads-command-misc" nil)
(declare-function beads-quickstart "beads-command-quickstart" nil)
(declare-function beads-hooks "beads-command-hooks" nil)
(declare-function beads-info "beads-command-info" nil)
(declare-function beads-where "beads-command-misc" nil)
(declare-function beads-human "beads-command-misc" nil)
(declare-function beads-onboard "beads-command-misc" nil)
(declare-function beads-prime "beads-command-misc" nil)
(declare-function beads-setup "beads-command-misc" nil)
(declare-function beads-version "beads-command-misc" nil)
(declare-function beads-kv "beads-command-misc" nil)
(declare-function beads-memories "beads-command-misc" nil)
(declare-function beads-recall "beads-command-misc" nil)
(declare-function beads-remember "beads-command-misc" nil)
(declare-function beads-forget "beads-command-misc" nil)
(declare-function beads-audit "beads-command-audit" nil)
(declare-function beads-slot "beads-command-slot" nil)
(declare-function beads-admin "beads-command-admin" nil)
(declare-function beads-worktree-menu "beads-command-worktree" nil)
(declare-function beads-diff "beads-command-diff" nil)
(declare-function beads-history "beads-command-history" nil)

;;;###autoload (autoload 'beads-advanced-menu "beads-advanced-menu" nil t)
(transient-define-prefix beads-advanced-menu ()
  "Low-frequency maintenance, setup, and integration commands.

Accessible via > from the main beads dispatch.  Contains
maintenance tools, data management, integrations, setup,
memory, and administration."
  ["Maintenance"
   ("d" "Doctor" beads-doctor)
   ("P" "Preflight" beads-preflight)
   ("U" "Upgrade" beads-upgrade)
   ("c" "Compact" beads-compact)
   ("f" "Flatten" beads-flatten)
   ("g" "GC" beads-gc)
   ("p" "Purge" beads-purge)
   ("R" "Rename prefix" beads-rename-prefix)]
  ["Data & Sync"
   ("b" "Backup" beads-backup)
   ("e" "Export" beads-export)
   ("4" "Restore" beads-restore)
   ("B" "Branch" beads-branch)
   ("V" "VC" beads-vc)
   ("F" "Federation" beads-federation)
   ("J" "SQL" beads-sql)]
  ["Duplicates & Migration"
   ("1" "Mark duplicate" beads-duplicate)
   ("2" "Find duplicates" beads-duplicates)
   ("3" "Supersede" beads-supersede)
   ("m" "Migrate" beads-migrate-menu)]
  ["Integrations"
   ("j" "Jira" beads-jira)
   ("n" "Linear" beads-linear)
   ("y" "GitLab" beads-gitlab)
   ("G" "GitHub" beads-github)
   ("o" "Repo" beads-repo)
   ("M" "Mail" beads-mail)]
  ["Setup"
   ("i" "Init" beads-init)
   ("I" "Info" beads-info)
   ("h" "Hooks" beads-hooks)
   ("Q" "Quickstart" beads-quickstart)
   ("w" "Where" beads-where)
   ("v" "Version" beads-version)
   ("x" "Context" beads-context)
   ("S" "Setup integrations" beads-setup)]
  ["Memory"
   ("k" "KV store" beads-kv)
   ("!" "Prime" beads-prime)
   ("~" "Memories" beads-memories)
   ("{" "Recall" beads-recall)
   ("}" "Remember" beads-remember)
   ("_" "Forget" beads-forget)]
  ["Audit & Admin"
   ("@" "Audit" beads-audit)
   ("$" "Slot" beads-slot)
   ("a" "Admin" beads-admin)
   ("W" "Worktree" beads-worktree-menu)
   ("`" "Diff" beads-diff)
   ("%" "History" beads-history)]
  ["Actions"
   ("q" "Quit" transient-quit-one)])

(provide 'beads-advanced-menu)
;;; beads-advanced-menu.el ends here
