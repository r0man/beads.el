;;; beads-command-misc.el --- Misc command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for miscellaneous `bd' commands
;; that don't warrant their own module file.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-reader)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-duplicate
;;; ============================================================

;;;###autoload (autoload 'beads-duplicate "beads-command-misc" nil t)
(beads-defcommand beads-command-duplicate (beads-command-global-options)
  ((issue-id
    :positional 1
    :required t)
   (of
    :type (or null string)
    :short-option "o"
    :prompt "Canonical issue: "
    :reader beads-reader-issue-id
    :group "Options"
    :level 1
    :order 1
    :required t))
  :documentation "Represents bd duplicate command.
Marks an issue as a duplicate of another.")


;;; ============================================================
;;; Command Class: beads-command-duplicates
;;; ============================================================

;;;###autoload (autoload 'beads-duplicates "beads-command-misc" nil t)
(beads-defcommand beads-command-duplicates (beads-command-global-options)
  ((auto-merge
    :type boolean
    :long-option "auto-merge"
    :short-option "m"
    :group "Options"
    :level 1
    :order 1
    :documentation "Automatically merge all duplicates")
   (dry-run
    :type boolean
    :long-option "dry-run"
    :short-option "n"
    :group "Options"
    :level 1
    :order 2
    :documentation "Show what would be merged without making changes"))
  :documentation "Represents bd duplicates command.
Finds and optionally merges duplicate issues.")


;;; ============================================================
;;; Command Class: beads-command-supersede
;;; ============================================================

;;;###autoload (autoload 'beads-supersede "beads-command-misc" nil t)
(beads-defcommand beads-command-supersede (beads-command-global-options)
  ((issue-id
    :positional 1
    :required t)
   (with-id
    :long-option "with"
    :type (or null string)
    :short-option "w"
    :prompt "Replacement issue: "
    :reader beads-reader-issue-id
    :group "Options"
    :level 1
    :order 1
    :required t))
  :documentation "Represents bd supersede command.
Marks an issue as superseded by a newer one.")


;;; ============================================================
;;; Command Class: beads-command-orphans
;;; ============================================================

;;;###autoload (autoload 'beads-orphans "beads-command-misc" nil t)
(beads-defcommand beads-command-orphans (beads-command-global-options)
  ((details
    :type boolean
    :short-option "d"
    :group "Options"
    :level 1
    :order 1)
   (fix
    :short-option "f"
    :type boolean
    :group "Options"
    :level 1
    :order 2)
   (label
    :type (list-of string)
    :long-option "label"
    :short-option "l"
    :prompt "Labels (AND): "
    :reader beads-reader-issue-labels
    :group "Filters"
    :level 2
    :order 1
    :documentation "Filter by labels (AND: must have ALL). Can combine with --label-any")
   (label-any
    :type (list-of string)
    :long-option "label-any"
    :prompt "Labels (OR): "
    :reader beads-reader-issue-labels
    :group "Filters"
    :level 2
    :order 2
    :documentation "Filter by labels (OR: must have AT LEAST ONE). Can combine with --label"))
  :documentation "Represents bd orphans command.
Identifies orphaned issues referenced in commits but still open.")


;;; ============================================================
;;; Command Class: beads-command-lint
;;; ============================================================

;;;###autoload (autoload 'beads-lint "beads-command-misc" nil t)
(beads-defcommand beads-command-lint (beads-command-global-options)
  ((issue-ids
    :positional 1
    :type (list-of string)
    :separator nil)
   (status
    :short-option "s"
    :type (or null string)
    :choices ("open" "in_progress" "blocked" "closed" "all")
    :group "Options"
    :level 1
    :order 1)
   (issue-type
    :long-option "type"
    :short-option "t"
    :type (or null string)
    :choices ("bug" "task" "feature" "epic" "chore")
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd lint command.
Checks issues for missing template sections.")


;;; ============================================================
;;; Command Class: beads-command-q (quick capture)
;;; ============================================================

;;;###autoload (autoload 'beads-q "beads-command-misc" nil t)
(beads-defcommand beads-command-q (beads-command-global-options)
  ((title
    :positional 1
    :short-option "T"
    :argument "--title="
    :group "Quick Capture"
    :level 1
    :order 0
    :required t)
   (issue-type
    :long-option "type"
    :short-option "t"
    :type (or null string)
    :choices ("task" "bug" "feature" "epic" "chore")
    :group "Options"
    :level 1
    :order 1)
   (priority
    :short-option "p"
    :type (or null string)
    :group "Options"
    :level 1
    :order 2)
   (labels
    :short-option "l"
    :type (list-of string)
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd q command.
Quick capture: creates issue and outputs only ID.")


;;; ============================================================
;;; Command Class: beads-command-note
;;; ============================================================

;;;###autoload (autoload 'beads-note "beads-command-misc" nil t)
(beads-defcommand beads-command-note (beads-command-global-options)
  ((issue-id
    :positional 1
    :short-option "i"
    :argument "--id="
    :reader beads--read-issue-at-point-or-prompt
    :group "Append Note"
    :level 1
    :order 0
    :required t)
   (text
    :positional 2
    :type (list-of string)
    :separator " "
    :prompt "Note text: "
    :group "Append Note"
    :level 1
    :order 1)
   (stdin
    :type boolean
    :short-option "s"
    :group "Input"
    :level 2
    :order 1)
   (file
    :type (or null string)
    :short-option "f"
    :group "Input"
    :level 2
    :order 2))
  :documentation "Represents bd note command.
Appends a note to an issue's notes field.")


;;; ============================================================
;;; Command Class: beads-command-version
;;; ============================================================

;;;###autoload (autoload 'beads-version "beads-command-misc" nil t)
(beads-defcommand beads-command-version (beads-command-global-options)
  ((daemon
    :type boolean
    :short-option "d"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd version command.
Prints version information.")


;;; ============================================================
;;; Command Class: beads-command-where
;;; ============================================================

;;;###autoload (autoload 'beads-where "beads-command-misc" nil t)
(beads-defcommand beads-command-where (beads-command-global-options)
  ()
  :documentation "Represents bd where command.
Shows active beads location.")


;;; ============================================================
;;; Command Class: beads-command-human
;;; ============================================================

;;;###autoload (autoload 'beads-human "beads-command-misc" nil t)
(beads-defcommand beads-command-human (beads-command-global-options)
  ()
  :documentation "Represents bd human command.
Shows essential commands for human users.")


;;; ============================================================
;;; Command Class: beads-command-onboard
;;; ============================================================

;;;###autoload (autoload 'beads-onboard "beads-command-misc" nil t)
(beads-defcommand beads-command-onboard (beads-command-global-options)
  ()
  :documentation "Represents bd onboard command.
Displays minimal snippet for AGENTS.md.")


;;; ============================================================
;;; Command Class: beads-command-prime
;;; ============================================================

;;;###autoload (autoload 'beads-prime "beads-command-misc" nil t)
(beads-defcommand beads-command-prime (beads-command-global-options)
  ()
  :documentation "Represents bd prime command.
Outputs AI-optimized workflow context.")


;;; ============================================================
;;; Command Class: beads-command-preflight
;;; ============================================================

;;;###autoload (autoload 'beads-preflight "beads-command-misc" nil t)
(beads-defcommand beads-command-preflight (beads-command-global-options)
  ()
  :documentation "Represents bd preflight command.
Shows PR readiness checklist.")


;;; ============================================================
;;; Command Class: beads-command-upgrade
;;; ============================================================

(beads-defcommand beads-command-upgrade (beads-command-global-options)
  ()
  :documentation "Represents bd upgrade command.
Check and manage bd version upgrades."
  :transient :manual)

;;; ============================================================
;;; Command Class: beads-command-upgrade-status
;;; ============================================================

;;;###autoload (autoload 'beads-upgrade-status "beads-command-misc" nil t)
(beads-defcommand beads-command-upgrade-status (beads-command-global-options)
  ()
  :documentation "Represents bd upgrade status command.
Check if bd has been upgraded since last use.")

;;; ============================================================
;;; Command Class: beads-command-upgrade-review
;;; ============================================================

;;;###autoload (autoload 'beads-upgrade-review "beads-command-misc" nil t)
(beads-defcommand beads-command-upgrade-review (beads-command-global-options)
  ()
  :documentation "Represents bd upgrade review command.
Show what's new in bd since the last version you used.")

;;; ============================================================
;;; Command Class: beads-command-upgrade-ack
;;; ============================================================

;;;###autoload (autoload 'beads-upgrade-ack "beads-command-misc" nil t)
(beads-defcommand beads-command-upgrade-ack (beads-command-global-options)
  ()
  :documentation "Represents bd upgrade ack command.
Acknowledge the current bd version to suppress upgrade notifications.")

;;; ============================================================
;;; Command Class: beads-command-rename-prefix
;;; ============================================================

;;;###autoload (autoload 'beads-rename-prefix "beads-command-misc" nil t)
(beads-defcommand beads-command-rename-prefix (beads-command-global-options)
  ((old-prefix
    :positional 1
    :required t)
   (new-prefix
    :positional 2
    :required t))
  :documentation "Represents bd rename-prefix command.
Renames the issue prefix for all issues in the database."
  :cli-command "rename-prefix")

;;; ============================================================
;;; Command Class: beads-command-setup
;;; ============================================================

;;;###autoload (autoload 'beads-setup "beads-command-misc" nil t)
(beads-defcommand beads-command-setup (beads-command-global-options)
  ((editor
    :positional 1))
  :documentation "Represents bd setup command.
Setup integration with AI editors.")


;;; ============================================================
;;; Command Class: beads-command-ship
;;; ============================================================

;;;###autoload (autoload 'beads-ship "beads-command-misc" nil t)
(beads-defcommand beads-command-ship (beads-command-global-options)
  ((capability
    :positional 1))
  :documentation "Represents bd ship command.
Publishes a capability for cross-project dependencies.")


;;; ============================================================
;;; Command Class: beads-command-cook
;;; ============================================================

;;;###autoload (autoload 'beads-cook "beads-command-misc" nil t)
(beads-defcommand beads-command-cook (beads-command-global-options)
  ((formula-id
    :positional 1))
  :documentation "Represents bd cook command.
Compiles a formula into a proto (ephemeral by default).")


;;; ============================================================
;;; Command Class: beads-command-mail
;;; ============================================================

;;;###autoload (autoload 'beads-mail "beads-command-misc" nil t)
(beads-defcommand beads-command-mail (beads-command-global-options)
  ()
  :documentation "Represents bd mail command.
Delegates to mail provider.")

;;; Transient Menus

;;;###autoload (autoload 'beads-upgrade "beads-command-misc" nil t)
(transient-define-prefix beads-upgrade ()
  "Check and manage bd version upgrades.

Version tracking is automatic - bd updates metadata.json on every run.
  Status: Check if bd version has changed since last use
  Review: Show all changes since your last version
  Ack: Acknowledge the current version"
  ["Upgrade"
   ("s" "Status (check if upgraded)" beads-upgrade-status)
   ("r" "Review (show changes since last version)" beads-upgrade-review)
   ("a" "Ack (acknowledge current version)" beads-upgrade-ack)]
  ["Quick Actions"
   ("q" "Quit" transient-quit-one)])

;;; Working With Issues -- stubs

;;;###autoload (autoload 'beads-children "beads-command-misc" nil t)
(beads-defcommand beads-command-children (beads-command-global-options)
  ((issue-id
    :positional 1
    :short-option "i"
    :argument "--id="
    :prompt "Parent issue ID: "
    :reader beads--read-issue-at-point-or-prompt
    :group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd children command.
Lists child beads of a parent issue.")

;;;###autoload (autoload 'beads-create-form "beads-command-misc" nil t)
(beads-defcommand beads-command-create-form (beads-command-global-options)
  ()
  :documentation "Represents bd create-form command.
Creates a new issue using an interactive form."
  :cli-command "create-form")

;;;###autoload (autoload 'beads-promote "beads-command-misc" nil t)
(beads-defcommand beads-command-promote (beads-command-global-options)
  ((issue-id
    :positional 1
    :short-option "i"
    :argument "--id="
    :prompt "Issue ID to promote: "
    :reader beads-reader-issue-id
    :group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd promote command.
Promotes a wisp to a permanent bead.")

;;;###autoload (autoload 'beads-query "beads-command-misc" nil t)
(beads-defcommand beads-command-query (beads-command-global-options)
  ((query-string
    :positional 1
    :short-option "q"
    :argument "--query="
    :group "Options"
    :level 1
    :order 0)
   (all
    :type boolean
    :short-option "a"
    :group "Options"
    :level 1
    :order 1
    :documentation "Include closed issues (default: exclude closed)")
   (limit
    :type (or null string integer)
    :short-option "n"
    :prompt "Limit: "
    :group "Options"
    :level 1
    :order 2
    :documentation "Limit results (default: 50, 0 = unlimited)")
   (sort
    :type (or null string)
    :short-option "s"
    :prompt "Sort by: "
    :group "Options"
    :level 1
    :order 3
    :documentation "Sort by field: priority, created, updated, closed, status, id, title, type, assignee")
   (reverse
    :type boolean
    :short-option "r"
    :group "Options"
    :level 1
    :order 4
    :documentation "Reverse sort order")
   (long
    :type boolean
    :short-option "l"
    :group "Options"
    :level 2
    :order 1
    :documentation "Show detailed multi-line output for each issue")
   (parse-only
    :type boolean
    :short-option "P"
    :group "Options"
    :level 3
    :order 1
    :documentation "Only parse the query and show the AST (for debugging)"))
  :documentation "Represents bd query command.
Queries issues using a simple query language.")

;;;###autoload (autoload 'beads-todo "beads-command-misc" nil t)
(beads-defcommand beads-command-todo (beads-command-global-options)
  ()
  :documentation "Represents bd todo command.
Manages TODO items (convenience wrapper for task issues).")

;;; ============================================================
;;; Command Class: beads-command-todo-add
;;; ============================================================

;;;###autoload (autoload 'beads-todo-add "beads-command-misc" nil t)
(beads-defcommand beads-command-todo-add (beads-command-global-options)
  ((title
    :positional 1
    :type (or null string)
    :short-option "T"
    :argument "--title="
    :prompt "TODO title: "
    :group "Add TODO"
    :level 1
    :order 1
    :required t)
   (priority
    :short-option "p"
    :type (or null string)
    :prompt "Priority (0-4): "
    :reader beads-reader-issue-priority
    :group "Add TODO"
    :level 2
    :order 2)
   (description
    :short-option "d"
    :type (or null string)
    :transient-key "D"
    :group "Add TODO"
    :level 2
    :order 3))
  :documentation "Represents bd todo add command.
Adds a new TODO item (task issue with priority 2).")


;;; ============================================================
;;; Command Class: beads-command-todo-list
;;; ============================================================

;;;###autoload (autoload 'beads-todo-list "beads-command-misc" nil t)
(beads-defcommand beads-command-todo-list (beads-command-global-options)
  ((all
    :type boolean
    :short-option "a"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd todo list command.
Lists TODO items (open task issues).")

;;; ============================================================
;;; Command Class: beads-command-todo-done
;;; ============================================================

;;;###autoload (autoload 'beads-todo-done "beads-command-misc" nil t)
(beads-defcommand beads-command-todo-done (beads-command-global-options)
  ((issue-ids
    :positional 1
    :type (list-of string)
    :separator " "
    :short-option "i"
    :transient transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads-reader-issue-id
    :group "Mark Done"
    :level 1
    :order 1
    :required t)
   (reason
    :type (or null string)
    :short-option "r"
    :prompt "Reason: "
    :group "Mark Done"
    :level 2
    :order 2))
  :documentation "Represents bd todo done command.
Marks one or more TODO items as done.")


;;; ============================================================
;;; Command Class: beads-command-human-list
;;; ============================================================

;;;###autoload (autoload 'beads-human-list "beads-command-misc" nil t)
(beads-defcommand beads-command-human-list (beads-command-global-options)
  ((status
    :type (or null string)
    :short-option "s"
    :choices ("open" "in_progress" "blocked" "closed" "all")
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd human list command.
Lists all issues labeled with the human tag.")

;;; ============================================================
;;; Command Class: beads-command-human-respond
;;; ============================================================

;;;###autoload (autoload 'beads-human-respond "beads-command-misc" nil t)
(beads-defcommand beads-command-human-respond (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :short-option "i"
    :argument "--issue-id="
    :reader beads-reader-issue-id
    :group "Options"
    :level 1
    :order 1
    :required t)
   (response
    :type (or null string)
    :short-option "r"
    :argument "--response="
    :group "Options"
    :level 1
    :order 2
    :required t))
  :documentation "Represents bd human respond command.
Responds to a human-needed bead by adding a comment and closing it.")

;;; ============================================================
;;; Command Class: beads-command-human-dismiss
;;; ============================================================

;;;###autoload (autoload 'beads-human-dismiss "beads-command-misc" nil t)
(beads-defcommand beads-command-human-dismiss (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :short-option "i"
    :argument "--issue-id="
    :reader beads-reader-issue-id
    :group "Options"
    :level 1
    :order 1
    :required t)
   (reason
    :type (or null string)
    :short-option "r"
    :argument "--reason="
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd human dismiss command.
Dismisses a human-needed bead permanently without responding.")

;;; ============================================================
;;; Command Class: beads-command-human-stats
;;; ============================================================

;;;###autoload (autoload 'beads-human-stats "beads-command-misc" nil t)
(beads-defcommand beads-command-human-stats (beads-command-global-options)
  ()
  :documentation "Represents bd human stats command.
Shows summary statistics for human-needed beads.")

;;; Views & Reports -- stubs

;;;###autoload (autoload 'beads-types "beads-command-misc" nil t)
(beads-defcommand beads-command-types (beads-command-global-options)
  ()
  :documentation "Represents bd types command.
Lists valid issue types.")

;;;###autoload (autoload 'beads-find-duplicates "beads-command-misc" nil t)
(beads-defcommand beads-command-find-duplicates (beads-command-global-options)
  ((limit
    :type (or null string integer)
    :short-option "n"
    :prompt "Limit: "
    :group "Options"
    :level 1
    :order 1
    :documentation "Maximum number of pairs to show (default 50)")
   (method
    :type (or null string)
    :long-option "method"
    :short-option "m"
    :choices ("mechanical" "ai")
    :group "Options"
    :level 1
    :order 2
    :documentation "Detection method: mechanical, ai (default \"mechanical\")")
   (threshold
    :type (or null string)
    :long-option "threshold"
    :short-option "T"
    :prompt "Threshold (0.0-1.0): "
    :group "Options"
    :level 1
    :order 3
    :documentation "Similarity threshold (0.0-1.0, lower = more results) (default 0.5)")
   (status
    :type (or null string)
    :short-option "s"
    :prompt "Status: "
    :group "Filters"
    :level 2
    :order 1
    :documentation "Filter by status (default: non-closed)")
   (model
    :type (or null string)
    :long-option "model"
    :prompt "AI model: "
    :group "Options"
    :level 2
    :order 2
    :documentation "AI model to use (only with --method ai; default from config ai.model)"))
  :documentation "Represents bd find-duplicates command.
Finds semantically similar issues using text analysis or AI."
  :cli-command "find-duplicates")

;;; Sync & Data -- stubs

;;; ============================================================
;;; Command Class: beads-command-backup (root)
;;; ============================================================

(beads-defcommand beads-command-backup (beads-command-global-options)
  ((force
    :type boolean
    :short-option "f"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd backup command.
Backs up your beads database by exporting all tables to JSONL."
  :transient :manual)

;;;###autoload (autoload 'beads-backup-root "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-backup "beads-backup-root"
  "Back up your beads database (export JSONL snapshot)."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-backup-init
;;; ============================================================

;;;###autoload (autoload 'beads-backup-init "beads-command-misc" nil t)
(beads-defcommand beads-command-backup-init (beads-command-global-options)
  ((path
    :positional 1
    :type (or null string)
    :short-option "p"
    :transient transient-option
    :argument "--path="
    :prompt "Backup destination (path or DoltHub URL): "
    :group "Options"
    :level 1
    :order 1
    :required t))
  :documentation "Represents bd backup init command.
Configures a Dolt backup destination (filesystem path or DoltHub URL).")

;;; ============================================================
;;; Command Class: beads-command-backup-status
;;; ============================================================

;;;###autoload (autoload 'beads-backup-status "beads-command-misc" nil t)
(beads-defcommand beads-command-backup-status (beads-command-global-options)
  ()
  :documentation "Represents bd backup status command.
Shows last JSONL and Dolt backup status.")

;;; ============================================================
;;; Command Class: beads-command-backup-sync
;;; ============================================================

;;;###autoload (autoload 'beads-backup-sync "beads-command-misc" nil t)
(beads-defcommand beads-command-backup-sync (beads-command-global-options)
  ()
  :documentation "Represents bd backup sync command.
Pushes the database to the configured Dolt backup destination.")

;;; ============================================================
;;; Command Class: beads-command-backup-restore
;;; ============================================================

;;;###autoload (autoload 'beads-backup-restore "beads-command-misc" nil t)
(beads-defcommand beads-command-backup-restore (beads-command-global-options)
  ((path
    :positional 1
    :type (or null string)
    :short-option "p"
    :transient transient-option
    :argument "--path="
    :prompt "Backup directory (leave empty for .beads/backup/): "
    :group "Options"
    :level 1
    :order 1)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd backup restore command.
Restores the database from JSONL backup files.")


;;; ============================================================
;;; Parent Transient Menu: beads-backup
;;; ============================================================

;;;###autoload (autoload 'beads-backup "beads-command-misc" nil t)
(transient-define-prefix beads-backup ()
  "Back up and restore your beads database.

JSONL backup commands (portable snapshot):
  Backup: Export all tables to .beads/backup/*.jsonl
  Status: Show last backup status
  Restore: Import from JSONL files

Dolt-native backup commands (preserve full commit history):
  Init: Configure a backup destination (filesystem or DoltHub)
  Sync: Push to configured destination"
  ["JSONL Backup"
   ("B" "Backup now (export JSONL)" beads-backup-root)
   ("s" "Status" beads-backup-status)
   ("r" "Restore from JSONL" beads-backup-restore)]
  ["Dolt-native Backup"
   ("i" "Init destination" beads-backup-init)
   ("S" "Sync to backup" beads-backup-sync)
   ("x" "Remove destination" beads-backup-remove)]
  ["Quick Actions"
   ("q" "Quit" transient-quit-one)])

;;;###autoload (autoload 'beads-export "beads-command-misc" nil t)
(beads-defcommand beads-command-export (beads-command-global-options)
  ()
  :documentation "Represents bd export command.
Exports issues to JSONL format.")

;;;###autoload (autoload 'beads-import "beads-command-misc" nil t)
(beads-defcommand beads-command-import (beads-command-global-options)
  ((file
    :positional 1
    :type (or null string))
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd import command.
Imports issues from a JSONL file into the database.")

;;; Maintenance -- stubs

;;;###autoload (autoload 'beads-flatten "beads-command-misc" nil t)
(beads-defcommand beads-command-flatten (beads-command-global-options)
  ()
  :documentation "Represents bd flatten command.
Squashes all Dolt history into a single commit.")

;;;###autoload (autoload 'beads-gc "beads-command-misc" nil t)
(beads-defcommand beads-command-gc (beads-command-global-options)
  ()
  :documentation "Represents bd gc command.
Garbage collects: decays old issues, compacts Dolt, runs GC.")

;;;###autoload (autoload 'beads-purge "beads-command-misc" nil t)
(beads-defcommand beads-command-purge (beads-command-global-options)
  ()
  :documentation "Represents bd purge command.
Deletes closed ephemeral beads to reclaim space.")

;;; Setup & Configuration -- memory stubs

;;;###autoload (autoload 'beads-forget "beads-command-misc" nil t)
(beads-defcommand beads-command-forget (beads-command-global-options)
  ((memory-id
    :positional 1
    :type (or null string)
    :short-option "m"
    :transient transient-option
    :argument "--id="
    :prompt "Memory ID: "
    :group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd forget command.
Removes a persistent memory.")

;;;###autoload (autoload 'beads-kv "beads-command-misc" nil t)
(beads-defcommand beads-command-kv (beads-command-global-options)
  ()
  :documentation "Represents bd kv command.
Key-value store commands.")

;;; Command Class: beads-command-kv-get
;;; ============================================================

;;;###autoload (autoload 'beads-kv-get "beads-command-misc" nil t)
(beads-defcommand beads-command-kv-get (beads-command-global-options)
  ((kv-key
    :positional 1
    :type (or null string)
    :short-option "k"
    :transient transient-option
    :argument "--key="
    :prompt "Key: "
    :group "Options"
    :level 1
    :order 1
    :required t))
  :documentation "Represents bd kv get command.
Retrieves a value by key from the key-value store.")

;;; Command Class: beads-command-kv-set
;;; ============================================================

;;;###autoload (autoload 'beads-kv-set "beads-command-misc" nil t)
(beads-defcommand beads-command-kv-set (beads-command-global-options)
  ((kv-key
    :positional 1
    :type (or null string)
    :short-option "k"
    :transient transient-option
    :argument "--key="
    :prompt "Key: "
    :group "Options"
    :level 1
    :order 1
    :required t)
   (kv-value
    :positional 2
    :type (or null string)
    :short-option "v"
    :transient transient-option
    :argument "--value="
    :prompt "Value: "
    :group "Options"
    :level 1
    :order 2
    :required t))
  :documentation "Represents bd kv set command.
Sets a key-value pair in the key-value store.")

;;; Command Class: beads-command-kv-clear
;;; ============================================================

;;;###autoload (autoload 'beads-kv-clear "beads-command-misc" nil t)
(beads-defcommand beads-command-kv-clear (beads-command-global-options)
  ((kv-key
    :positional 1
    :type (or null string)
    :short-option "k"
    :transient transient-option
    :argument "--key="
    :prompt "Key: "
    :group "Options"
    :level 1
    :order 1
    :required t))
  :documentation "Represents bd kv clear command.
Deletes a key-value pair from the key-value store.")

;;; Command Class: beads-command-kv-list
;;; ============================================================

;;;###autoload (autoload 'beads-kv-list "beads-command-misc" nil t)
(beads-defcommand beads-command-kv-list (beads-command-global-options)
  ()
  :documentation "Represents bd kv list command.
Lists all key-value pairs in the key-value store.")

;;;###autoload (autoload 'beads-memories "beads-command-misc" nil t)
(beads-defcommand beads-command-memories (beads-command-global-options)
  ()
  :documentation "Represents bd memories command.
Lists or searches persistent memories.")

;;;###autoload (autoload 'beads-recall "beads-command-misc" nil t)
(beads-defcommand beads-command-recall (beads-command-global-options)
  ((memory-id
    :positional 1
    :type (or null string)
    :short-option "m"
    :transient transient-option
    :argument "--id="
    :prompt "Memory ID: "
    :group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd recall command.
Retrieves a specific memory.")

;;;###autoload (autoload 'beads-remember "beads-command-misc" nil t)
(beads-defcommand beads-command-remember (beads-command-global-options)
  ((content
    :positional 1
    :type (or null string)
    :short-option "c"
    :transient transient-option
    :argument "--content="
    :prompt "Memory content: "
    :group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd remember command.
Stores a persistent memory.")

;;; Additional Commands -- rename stub

;;;###autoload (autoload 'beads-rename "beads-command-misc" nil t)
(beads-defcommand beads-command-rename (beads-command-global-options)
  ((old-id
    :positional 1
    :type (or null string)
    :short-option "o"
    :transient transient-option
    :argument "--old-id="
    :prompt "Old issue ID: "
    :reader beads--read-issue-at-point-or-prompt
    :group "Rename Issue"
    :level 1
    :order 0
    :required t)
   (new-id
    :positional 2
    :type (or null string)
    :short-option "n"
    :transient transient-option
    :argument "--new-id="
    :prompt "New issue ID: "
    :group "Rename Issue"
    :level 1
    :order 1
    :required t))
  :documentation "Represents bd rename command.
Renames an issue from one ID to another, updating all references.")


(cl-defmethod beads-command-validate ((command beads-command-rename))
  "Validate rename COMMAND."
  (or (cl-call-next-method)
      (with-slots (old-id new-id) command
        (when (equal old-id new-id)
          "Old and new issue IDs must be different"))))

;;; Context Command

;;;###autoload (autoload 'beads-context "beads-command-misc" nil t)
(beads-defcommand beads-command-context (beads-command-global-options)
  ()
  :documentation "Represents bd context command.
Show effective backend identity and repository context.")

;;; ============================================================
;;; Command Class: beads-command-assign
;;; ============================================================

;;;###autoload (autoload 'beads-assign "beads-command-misc" nil t)
(beads-defcommand beads-command-assign (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :short-option "i"
    :transient transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads--read-issue-at-point-or-prompt
    :group "Assign Issue"
    :level 1
    :order 0
    :required t)
   (assignee
    :positional 2
    :type (or null string)
    :short-option "a"
    :transient transient-option
    :argument "--assignee="
    :prompt "Assignee: "
    :group "Assign Issue"
    :level 1
    :order 1))
  :documentation "Represents bd assign command.
Shorthand for bd update <id> --assignee <name>.
Pass an empty string as assignee to unassign.")


;;; ============================================================
;;; Command Class: beads-command-comment
;;; ============================================================

(beads-defcommand beads-command-comment (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :short-option "i"
    :transient transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads--read-issue-at-point-or-prompt
    :group "Add Comment"
    :level 1
    :order 0
    :required t)
   (text
    :positional 2
    :type (or null string)
    :short-option "t"
    :transient transient-option
    :argument "--text="
    :prompt "Comment: "
    :group "Add Comment"
    :level 1
    :order 1)
   (stdin
    :type boolean
    :short-option "s"
    :group "Options"
    :level 2
    :order 1)
   (file
    :type (or null string)
    :short-option "f"
    :prompt "File path: "
    :group "Options"
    :level 2
    :order 2))
  :documentation "Represents bd comment command.
Shorthand for bd comments add <id> \"text\".
Add a comment to an issue."
  :transient :manual)


;;;###autoload (autoload 'beads-add-comment "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-comment "beads-add-comment"
  "Add a comment to an issue.

Shorthand for: bd comments add <id> \"text\""
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-link
;;; ============================================================

;;;###autoload (autoload 'beads-link "beads-command-misc" nil t)
(beads-defcommand beads-command-link (beads-command-global-options)
  ((id1
    :positional 1
    :type (or null string)
    :short-option "1"
    :transient transient-option
    :argument "--id1="
    :prompt "Dependent issue ID: "
    :reader beads--read-issue-at-point-or-prompt
    :group "Link Issues"
    :level 1
    :order 0
    :required t)
   (id2
    :positional 2
    :type (or null string)
    :short-option "2"
    :transient transient-option
    :argument "--id2="
    :prompt "Blocker issue ID: "
    :reader beads-reader-issue-id
    :group "Link Issues"
    :level 1
    :order 1
    :required t)
   (link-type
    :long-option "type"
    :type (or null string)
    :short-option "t"
    :prompt "Type (blocks/tracks/related/parent-child/discovered-from): "
    :choices ("blocks" "tracks" "related" "parent-child" "discovered-from")
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd link command.
Shorthand for bd dep add <id1> <id2>.
By default creates a 'blocks' dependency (id2 blocks id1).")


(cl-defmethod beads-command-validate ((command beads-command-link))
  "Validate link COMMAND."
  (or (cl-call-next-method)
      (with-slots (id1 id2) command
        (when (equal id1 id2)
          "Cannot link an issue to itself"))))

;;; ============================================================
;;; Command Class: beads-command-priority
;;; ============================================================

;;;###autoload (autoload 'beads-priority "beads-command-misc" nil t)
(beads-defcommand beads-command-priority (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :short-option "i"
    :transient transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads--read-issue-at-point-or-prompt
    :group "Set Priority"
    :level 1
    :order 0
    :required t)
   (level
    :positional 2
    :type (or null string integer)
    :short-option "p"
    :transient transient-option
    :argument "--level="
    :prompt "Priority (0-4): "
    :reader beads-reader-priority-level
    :group "Set Priority"
    :level 1
    :order 1
    :required t))
  :documentation "Represents bd priority command.
Shorthand for bd update <id> --priority <n>.
Priority levels: 0=critical, 1=high, 2=medium, 3=low, 4=backlog.")


(cl-defmethod beads-command-validate ((command beads-command-priority))
  "Validate priority COMMAND."
  (or (cl-call-next-method)
      (with-slots (level) command
        (let ((level-int (cond ((integerp level) level)
                               ((and (stringp level)
                                     (string-match-p "\\`-?[0-9]+\\'" level))
                                (string-to-number level))
                               (t nil))))
          (when (not (and (numberp level-int) (<= 0 level-int 4)))
            "Priority level must be 0 (critical), 1 (high), 2 (medium), 3 (low), or 4 (backlog)")))))

;;; ============================================================
;;; Command Class: beads-command-tag
;;; ============================================================

;;;###autoload (autoload 'beads-tag "beads-command-misc" nil t)
(beads-defcommand beads-command-tag (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :short-option "i"
    :transient transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads--read-issue-at-point-or-prompt
    :group "Add Label"
    :level 1
    :order 0
    :required t)
   (label
    :positional 2
    :type (or null string)
    :short-option "l"
    :transient transient-option
    :argument "--label="
    :prompt "Label: "
    :reader beads-reader-label-name
    :group "Add Label"
    :level 1
    :order 1
    :required t))
  :documentation "Represents bd tag command.
Shorthand for bd update <id> --add-label <label>.
Add a label to an issue.")


;;; ============================================================
;;; Command Class: beads-command-statuses
;;; ============================================================

;;;###autoload (autoload 'beads-statuses "beads-command-misc" nil t)
(beads-defcommand beads-command-statuses (beads-command-global-options)
  ()
  :documentation "Represents bd statuses command.
List all valid issue statuses and their categories.
Built-in and custom statuses from config are shown.")


;;; ============================================================
;;; Command Class: beads-command-rules-audit
;;; ============================================================

;;;###autoload (autoload 'beads-rules-audit "beads-command-misc" nil t)
(beads-defcommand beads-command-rules-audit (beads-command-global-options)
  ((path
    :type (or null string)
    :prompt "Rules directory: "
    :group "Options"
    :level 2
    :order 1)
   (threshold
    :type (or null number)
    :prompt "Similarity threshold: "
    :group "Options"
    :level 2
    :order 2))
  :documentation "Represents bd rules audit command.
Scans rules for contradictions and merge opportunities."
  :cli-command "rules audit")

;;; ============================================================
;;; Command Class: beads-command-rules-compact
;;; ============================================================

;;;###autoload (autoload 'beads-rules-compact "beads-command-misc" nil t)
(beads-defcommand beads-command-rules-compact (beads-command-global-options)
  ((path
    :type (or null string)
    :prompt "Rules directory: "
    :group "Options"
    :level 2
    :order 1)
   (auto
    :type boolean
    :group "Options"
    :level 1
    :order 2)
   (dry-run
    :type boolean
    :group "Options"
    :level 1
    :order 3)
   (group
    :type (or null string)
    :prompt "Rule names to merge: "
    :group "Options"
    :level 2
    :order 4))
  :documentation "Represents bd rules compact command.
Merges related rules into composites."
  :cli-command "rules compact")

;;; Parent Transient Menu: beads-rules

;;;###autoload (autoload 'beads-rules "beads-command-misc" nil t)
(transient-define-prefix beads-rules ()
  "Audit and compact Claude rules."
  ["Rules Commands"
   ("a" "Audit rules" beads-rules-audit)
   ("c" "Compact rules" beads-rules-compact)])

;;; ============================================================
;;; Command Class: beads-command-backup-remove
;;; ============================================================

;;;###autoload (autoload 'beads-backup-remove "beads-command-misc" nil t)
(beads-defcommand beads-command-backup-remove (beads-command-global-options)
  ()
  :documentation "Represents bd backup remove command.
Removes the configured backup destination."
  :cli-command "backup remove")

(provide 'beads-command-misc)
;;; beads-command-misc.el ends here
