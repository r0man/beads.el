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

(beads-defcommand beads-command-jira-sync (beads-command-global-options)
  ((pull
    :initarg :pull
    :type boolean
    :initform nil
    :documentation "Import issues from Jira."
    :long-option "pull"
    :option-type :boolean
    :key "p"
    :transient "--pull"
    :class transient-switch
    :argument "--pull"
    :transient-group "Direction"
    :level 1
    :order 1)
   (push
    :initarg :push
    :type boolean
    :initform nil
    :documentation "Export issues to Jira."
    :long-option "push"
    :option-type :boolean
    :key "P"
    :transient "--push"
    :class transient-switch
    :argument "--push"
    :transient-group "Direction"
    :level 1
    :order 2)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview sync without changes."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd jira sync command.
Synchronizes issues with Jira.")


(beads-defcommand beads-command-jira-status (beads-command-global-options)
  ()
  :documentation "Represents bd jira status command.
Shows Jira sync status.")


;;; ============================================================
;;; Linear Commands
;;; ============================================================

(beads-defcommand beads-command-linear-sync (beads-command-global-options)
  ((pull
    :initarg :pull
    :type boolean
    :initform nil
    :documentation "Import issues from Linear."
    :long-option "pull"
    :option-type :boolean
    :key "p"
    :transient "--pull"
    :class transient-switch
    :argument "--pull"
    :transient-group "Direction"
    :level 1
    :order 1)
   (push
    :initarg :push
    :type boolean
    :initform nil
    :documentation "Export issues to Linear."
    :long-option "push"
    :option-type :boolean
    :key "P"
    :transient "--push"
    :class transient-switch
    :argument "--push"
    :transient-group "Direction"
    :level 1
    :order 2)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview sync without changes."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd linear sync command.
Synchronizes issues with Linear.")


(beads-defcommand beads-command-linear-status (beads-command-global-options)
  ()
  :documentation "Represents bd linear status command.
Shows Linear sync status.")


(beads-defcommand beads-command-linear-teams (beads-command-global-options)
  ()
  :documentation "Represents bd linear teams command.
Lists available Linear teams.")


;;; ============================================================
;;; Repo Commands
;;; ============================================================

(beads-defcommand beads-command-repo-add (beads-command-global-options)
  ((repo-path
    :initarg :repo-path
    :type (or null string)
    :initform nil
    :documentation "Repository path to add."
    :positional 1))
  :documentation "Represents bd repo add command.
Adds an additional repository to sync.")


(cl-defmethod beads-command-validate ((command beads-command-repo-add))
  "Validate repo add COMMAND."
  (with-slots (repo-path) command
    (if (not repo-path) "Repository path is required" nil)))

(beads-defcommand beads-command-repo-list (beads-command-global-options)
  ()
  :documentation "Represents bd repo list command.
Lists all configured repositories.")


(beads-defcommand beads-command-repo-remove (beads-command-global-options)
  ((repo-path
    :initarg :repo-path
    :type (or null string)
    :initform nil
    :documentation "Repository path to remove."
    :positional 1))
  :documentation "Represents bd repo remove command.
Removes a repository from sync configuration.")


(cl-defmethod beads-command-validate ((command beads-command-repo-remove))
  "Validate repo remove COMMAND."
  (with-slots (repo-path) command
    (if (not repo-path) "Repository path is required" nil)))

(beads-defcommand beads-command-repo-sync (beads-command-global-options)
  ()
  :documentation "Represents bd repo sync command.
Manually triggers multi-repo sync.")


;;; Execute Interactive Methods










;;; Transient Menus

;;;###autoload (autoload 'beads-jira-sync "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-jira-sync "beads-jira-sync"
  "Synchronize issues with Jira."
  beads-option-global-section)

;;;###autoload (autoload 'beads-jira-status "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-jira-status "beads-jira-status"
  "Show Jira sync status."
  beads-option-global-section)

;;;###autoload (autoload 'beads-linear-sync "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-linear-sync "beads-linear-sync"
  "Synchronize issues with Linear."
  beads-option-global-section)

;;;###autoload (autoload 'beads-linear-status "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-linear-status "beads-linear-status"
  "Show Linear sync status."
  beads-option-global-section)

;;;###autoload (autoload 'beads-linear-teams "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-linear-teams "beads-linear-teams"
  "List available Linear teams."
  beads-option-global-section)

;;;###autoload (autoload 'beads-repo-add "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-repo-add "beads-repo-add"
  "Add repository to sync configuration."
  beads-option-global-section)

;;;###autoload (autoload 'beads-repo-list "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-repo-list "beads-repo-list"
  "List configured repositories."
  beads-option-global-section)

;;;###autoload (autoload 'beads-repo-remove "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-repo-remove "beads-repo-remove"
  "Remove repository from sync configuration."
  beads-option-global-section)

;;;###autoload (autoload 'beads-repo-sync "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-repo-sync "beads-repo-sync"
  "Trigger multi-repo sync."
  beads-option-global-section)

;;; ============================================================
;;; GitLab Commands
;;; ============================================================

(beads-defcommand beads-command-gitlab-sync (beads-command-global-options)
  ((pull
    :initarg :pull
    :type boolean
    :initform nil
    :documentation "Import issues from GitLab."
    :long-option "pull"
    :option-type :boolean
    :key "p"
    :transient "--pull"
    :class transient-switch
    :argument "--pull"
    :transient-group "Direction"
    :level 1
    :order 1)
   (push
    :initarg :push
    :type boolean
    :initform nil
    :documentation "Export issues to GitLab."
    :long-option "push"
    :option-type :boolean
    :key "P"
    :transient "--push"
    :class transient-switch
    :argument "--push"
    :transient-group "Direction"
    :level 1
    :order 2)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview sync without changes."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd gitlab sync command.
Synchronizes issues with GitLab.")


(beads-defcommand beads-command-gitlab-status (beads-command-global-options)
  ()
  :documentation "Represents bd gitlab status command.
Shows GitLab sync status.")


(beads-defcommand beads-command-gitlab-projects (beads-command-global-options)
  ()
  :documentation "Represents bd gitlab projects command.
Lists accessible GitLab projects.")


;;; Autoloads for GitLab sub-commands

;;;###autoload (autoload 'beads-gitlab-sync "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-gitlab-sync "beads-gitlab-sync"
  "Sync issues with GitLab."
  beads-option-global-section)

;;;###autoload (autoload 'beads-gitlab-status "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-gitlab-status "beads-gitlab-status"
  "Show GitLab sync status."
  beads-option-global-section)

;;;###autoload (autoload 'beads-gitlab-projects "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-gitlab-projects "beads-gitlab-projects"
  "List accessible GitLab projects."
  beads-option-global-section)

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

(beads-defcommand beads-command-github-sync (beads-command-global-options)
  ((pull
    :initarg :pull
    :type boolean
    :initform nil
    :documentation "Import issues from GitHub."
    :long-option "pull"
    :option-type :boolean
    :key "p"
    :transient "--pull"
    :class transient-switch
    :argument "--pull"
    :transient-group "Direction"
    :level 1
    :order 1)
   (push
    :initarg :push
    :type boolean
    :initform nil
    :documentation "Export issues to GitHub."
    :long-option "push"
    :option-type :boolean
    :key "P"
    :transient "--push"
    :class transient-switch
    :argument "--push"
    :transient-group "Direction"
    :level 1
    :order 2)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Show what would be synced without syncing."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd github sync command.
Sync issues with GitHub.")

(beads-defcommand beads-command-github-status (beads-command-global-options)
  ()
  :documentation "Represents bd github status command.
Shows GitHub sync status.")

(beads-defcommand beads-command-github-repos (beads-command-global-options)
  ()
  :documentation "Represents bd github repos command.
Lists accessible GitHub repositories.")

;;; Autoloads for GitHub sub-commands

;;;###autoload (autoload 'beads-github-sync "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-github-sync "beads-github-sync"
  "Sync issues with GitHub."
  beads-option-global-section)

;;;###autoload (autoload 'beads-github-status "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-github-status "beads-github-status"
  "Show GitHub sync status."
  beads-option-global-section)

;;;###autoload (autoload 'beads-github-repos "beads-command-integrations" nil t)
(beads-meta-define-transient beads-command-github-repos "beads-github-repos"
  "List accessible GitHub repositories."
  beads-option-global-section)

;;;###autoload (autoload 'beads-github "beads-command-integrations" nil t)
(transient-define-prefix beads-github ()
  "GitHub integration commands."
  ["GitHub Commands"
   ("s" "Sync" beads-github-sync)
   ("S" "Status" beads-github-status)
   ("r" "Repos" beads-github-repos)])

(provide 'beads-command-integrations)
;;; beads-command-integrations.el ends here
