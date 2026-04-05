;;; beads-command-integrations.el --- Integration command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for external integrations:
;; - Jira synchronization
;; - Linear synchronization
;; - Multi-repo management

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Jira Commands
;;; ============================================================

;;;###autoload (autoload 'beads-jira-sync "beads-command-integrations" nil t)
(beads-defcommand beads-command-jira-sync (beads-command-global-options)
  ((pull
    :type boolean
    :short-option "p"
    :group "Direction"
    :level 1
    :order 1)
   (push
    :type boolean
    :short-option "P"
    :group "Direction"
    :level 1
    :order 2)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd jira sync command.
Synchronizes issues with Jira.")


;;;###autoload (autoload 'beads-jira-status "beads-command-integrations" nil t)
(beads-defcommand beads-command-jira-status (beads-command-global-options)
  ()
  :documentation "Represents bd jira status command.
Shows Jira sync status.")


;;; ============================================================
;;; Linear Commands
;;; ============================================================

;;;###autoload (autoload 'beads-linear-sync "beads-command-integrations" nil t)
(beads-defcommand beads-command-linear-sync (beads-command-global-options)
  ((pull
    :type boolean
    :short-option "p"
    :group "Direction"
    :level 1
    :order 1)
   (push
    :type boolean
    :short-option "P"
    :group "Direction"
    :level 1
    :order 2)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd linear sync command.
Synchronizes issues with Linear.")


;;;###autoload (autoload 'beads-linear-status "beads-command-integrations" nil t)
(beads-defcommand beads-command-linear-status (beads-command-global-options)
  ()
  :documentation "Represents bd linear status command.
Shows Linear sync status.")


;;;###autoload (autoload 'beads-linear-teams "beads-command-integrations" nil t)
(beads-defcommand beads-command-linear-teams (beads-command-global-options)
  ()
  :documentation "Represents bd linear teams command.
Lists available Linear teams.")


;;; ============================================================
;;; Repo Commands
;;; ============================================================

;;;###autoload (autoload 'beads-repo-add "beads-command-integrations" nil t)
(beads-defcommand beads-command-repo-add (beads-command-global-options)
  ((repo-path
    :positional 1
    :required t))
  :documentation "Represents bd repo add command.
Adds an additional repository to sync.")


;;;###autoload (autoload 'beads-repo-list "beads-command-integrations" nil t)
(beads-defcommand beads-command-repo-list (beads-command-global-options)
  ()
  :documentation "Represents bd repo list command.
Lists all configured repositories.")


;;;###autoload (autoload 'beads-repo-remove "beads-command-integrations" nil t)
(beads-defcommand beads-command-repo-remove (beads-command-global-options)
  ((repo-path
    :positional 1
    :required t))
  :documentation "Represents bd repo remove command.
Removes a repository from sync configuration.")


;;;###autoload (autoload 'beads-repo-sync "beads-command-integrations" nil t)
(beads-defcommand beads-command-repo-sync (beads-command-global-options)
  ()
  :documentation "Represents bd repo sync command.
Manually triggers multi-repo sync.")


;;; Execute Interactive Methods


;;; ============================================================
;;; GitLab Commands
;;; ============================================================

;;;###autoload (autoload 'beads-gitlab-sync "beads-command-integrations" nil t)
(beads-defcommand beads-command-gitlab-sync (beads-command-global-options)
  ((pull
    :type boolean
    :short-option "p"
    :group "Direction"
    :level 1
    :order 1)
   (push
    :type boolean
    :short-option "P"
    :group "Direction"
    :level 1
    :order 2)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd gitlab sync command.
Synchronizes issues with GitLab.")


;;;###autoload (autoload 'beads-gitlab-status "beads-command-integrations" nil t)
(beads-defcommand beads-command-gitlab-status (beads-command-global-options)
  ()
  :documentation "Represents bd gitlab status command.
Shows GitLab sync status.")


;;;###autoload (autoload 'beads-gitlab-projects "beads-command-integrations" nil t)
(beads-defcommand beads-command-gitlab-projects (beads-command-global-options)
  ()
  :documentation "Represents bd gitlab projects command.
Lists accessible GitLab projects.")


;;; Parent Transient Menus

;;;###autoload (autoload 'beads-jira "beads-command-integrations" nil t)
(transient-define-prefix beads-jira ()
  "Jira integration commands."
  ["Jira Commands"
   ("s" "Sync" beads-jira-sync)
   ("S" "Status" beads-jira-status)])

;;;###autoload (autoload 'beads-linear "beads-command-integrations" nil t)
(transient-define-prefix beads-linear ()
  "Linear integration commands."
  ["Linear Commands"
   ("s" "Sync" beads-linear-sync)
   ("S" "Status" beads-linear-status)
   ("t" "Teams" beads-linear-teams)])

;;;###autoload (autoload 'beads-repo "beads-command-integrations" nil t)
(transient-define-prefix beads-repo ()
  "Multi-repo management commands."
  ["Repo Commands"
   ("a" "Add repo" beads-repo-add)
   ("l" "List repos" beads-repo-list)
   ("r" "Remove repo" beads-repo-remove)
   ("s" "Sync all" beads-repo-sync)])

;;;###autoload (autoload 'beads-gitlab "beads-command-integrations" nil t)
(transient-define-prefix beads-gitlab ()
  "GitLab integration commands."
  ["GitLab Commands"
   ("s" "Sync" beads-gitlab-sync)
   ("S" "Status" beads-gitlab-status)
   ("p" "Projects" beads-gitlab-projects)])

;;; GitHub Integration Commands

;;;###autoload (autoload 'beads-github-sync "beads-command-integrations" nil t)
(beads-defcommand beads-command-github-sync (beads-command-global-options)
  ((pull
    :type boolean
    :short-option "p"
    :group "Direction"
    :level 1
    :order 1)
   (push
    :type boolean
    :short-option "P"
    :group "Direction"
    :level 1
    :order 2)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd github sync command.
Sync issues with GitHub.")

;;;###autoload (autoload 'beads-github-status "beads-command-integrations" nil t)
(beads-defcommand beads-command-github-status (beads-command-global-options)
  ()
  :documentation "Represents bd github status command.
Shows GitHub sync status.")

;;;###autoload (autoload 'beads-github-repos "beads-command-integrations" nil t)
(beads-defcommand beads-command-github-repos (beads-command-global-options)
  ()
  :documentation "Represents bd github repos command.
Lists accessible GitHub repositories.")

;;;###autoload (autoload 'beads-github "beads-command-integrations" nil t)
(transient-define-prefix beads-github ()
  "GitHub integration commands."
  ["GitHub Commands"
   ("s" "Sync" beads-github-sync)
   ("S" "Status" beads-github-status)
   ("r" "Repos" beads-github-repos)])

;;; Azure DevOps Integration Commands

;;;###autoload (autoload 'beads-ado-sync "beads-command-integrations" nil t)
(beads-defcommand beads-command-ado-sync (beads-command-global-options)
  ((dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1)
   (pull-only
    :type boolean
    :short-option "p"
    :group "Direction"
    :level 1
    :order 2)
   (push-only
    :type boolean
    :short-option "P"
    :group "Direction"
    :level 1
    :order 3))
  :documentation "Represents bd ado sync command.
Synchronize issues between beads and Azure DevOps."
  :cli-command "ado sync")

;;;###autoload (autoload 'beads-ado-status "beads-command-integrations" nil t)
(beads-defcommand beads-command-ado-status (beads-command-global-options)
  ()
  :documentation "Represents bd ado status command.
Show Azure DevOps sync status."
  :cli-command "ado status")

;;;###autoload (autoload 'beads-ado-projects "beads-command-integrations" nil t)
(beads-defcommand beads-command-ado-projects (beads-command-global-options)
  ()
  :documentation "Represents bd ado projects command.
List accessible Azure DevOps projects."
  :cli-command "ado projects")

;;;###autoload (autoload 'beads-ado "beads-command-integrations" nil t)
(transient-define-prefix beads-ado ()
  "Azure DevOps integration commands."
  ["Azure DevOps Commands"
   ("s" "Sync" beads-ado-sync)
   ("S" "Status" beads-ado-status)
   ("p" "Projects" beads-ado-projects)])

;;; ============================================================
;;; Notion Integration Commands
;;; ============================================================

;;;###autoload (autoload 'beads-notion-connect "beads-command-integrations" nil t)
(beads-defcommand beads-command-notion-connect (beads-command-global-options)
  ((url
    :type (or null string)
    :short-option "u"
    :prompt "Notion URL: "
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd notion connect command.
Connect bd to an existing Notion database or data source.")


;;;###autoload (autoload 'beads-notion-init "beads-command-integrations" nil t)
(beads-defcommand beads-command-notion-init (beads-command-global-options)
  ((parent
    :type (or null string)
    :short-option "p"
    :prompt "Parent page ID: "
    :group "Options"
    :level 1
    :order 1)
   (title
    :type (or null string)
    :short-option "t"
    :prompt "Title (default: Beads Issues): "
    :group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd notion init command.
Create a dedicated Beads database in Notion.")


;;;###autoload (autoload 'beads-notion-status "beads-command-integrations" nil t)
(beads-defcommand beads-command-notion-status (beads-command-global-options)
  ()
  :documentation "Represents bd notion status command.
Show Notion sync status.")


;;;###autoload (autoload 'beads-notion-sync "beads-command-integrations" nil t)
(beads-defcommand beads-command-notion-sync (beads-command-global-options)
  ((pull
    :type boolean
    :short-option "P"
    :group "Direction"
    :level 1
    :order 1)
   (push
    :type boolean
    :short-option "p"
    :group "Direction"
    :level 1
    :order 2)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 3)
   (state
    :type (or null string)
    :short-option "s"
    :prompt "State (open/closed/all): "
    :choices ("open" "closed" "all")
    :group "Options"
    :level 2
    :order 4)
   (create-only
    :type boolean
    :short-option "c"
    :group "Options"
    :level 2
    :order 5)
   (prefer-local
    :type boolean
    :short-option "l"
    :group "Conflict"
    :level 2
    :order 6)
   (prefer-notion
    :type boolean
    :short-option "N"
    :group "Conflict"
    :level 2
    :order 7))
  :documentation "Represents bd notion sync command.
Synchronize issues between beads and Notion.
By default performs bidirectional sync.")


;;;###autoload (autoload 'beads-notion "beads-command-integrations" nil t)
(transient-define-prefix beads-notion ()
  "Notion integration commands."
  ["Notion Commands"
   ("c" "Connect" beads-notion-connect)
   ("i" "Init database" beads-notion-init)
   ("s" "Status" beads-notion-status)
   ("S" "Sync" beads-notion-sync)])

(provide 'beads-command-integrations)
;;; beads-command-integrations.el ends here
