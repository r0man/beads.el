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

(eval-and-compile
(beads-defcommand beads-command-jira-sync (beads-command-json)
  ((pull
    :initarg :pull
    :type boolean
    :initform nil
    :documentation "Import issues from Jira."
    :long-option "--pull"
    :option-type :boolean
    :transient-key "-p"
    :transient-description "--pull"
    :transient-class transient-switch
    :transient-argument "--pull"
    :transient-group "Direction"
    :transient-level 1
    :transient-order 1)
   (push
    :initarg :push
    :type boolean
    :initform nil
    :documentation "Export issues to Jira."
    :long-option "--push"
    :option-type :boolean
    :transient-key "-P"
    :transient-description "--push"
    :transient-class transient-switch
    :transient-argument "--push"
    :transient-group "Direction"
    :transient-level 1
    :transient-order 2)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview sync without changes."
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd jira sync command.
Synchronizes issues with Jira."))

(cl-defmethod beads-command-subcommand ((_command beads-command-jira-sync))
  "Return \"jira sync\" as the CLI subcommand."
  "jira sync")

(eval-and-compile
(beads-defcommand beads-command-jira-status (beads-command-json)
  ()
  :documentation "Represents bd jira status command.
Shows Jira sync status."))

(cl-defmethod beads-command-subcommand ((_command beads-command-jira-status))
  "Return \"jira status\" as the CLI subcommand."
  "jira status")

;;; ============================================================
;;; Linear Commands
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-linear-sync (beads-command-json)
  ((pull
    :initarg :pull
    :type boolean
    :initform nil
    :documentation "Import issues from Linear."
    :long-option "--pull"
    :option-type :boolean
    :transient-key "-p"
    :transient-description "--pull"
    :transient-class transient-switch
    :transient-argument "--pull"
    :transient-group "Direction"
    :transient-level 1
    :transient-order 1)
   (push
    :initarg :push
    :type boolean
    :initform nil
    :documentation "Export issues to Linear."
    :long-option "--push"
    :option-type :boolean
    :transient-key "-P"
    :transient-description "--push"
    :transient-class transient-switch
    :transient-argument "--push"
    :transient-group "Direction"
    :transient-level 1
    :transient-order 2)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview sync without changes."
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd linear sync command.
Synchronizes issues with Linear."))

(cl-defmethod beads-command-subcommand ((_command beads-command-linear-sync))
  "Return \"linear sync\" as the CLI subcommand."
  "linear sync")

(eval-and-compile
(beads-defcommand beads-command-linear-status (beads-command-json)
  ()
  :documentation "Represents bd linear status command.
Shows Linear sync status."))

(cl-defmethod beads-command-subcommand ((_command beads-command-linear-status))
  "Return \"linear status\" as the CLI subcommand."
  "linear status")

(eval-and-compile
(beads-defcommand beads-command-linear-teams (beads-command-json)
  ()
  :documentation "Represents bd linear teams command.
Lists available Linear teams."))

(cl-defmethod beads-command-subcommand ((_command beads-command-linear-teams))
  "Return \"linear teams\" as the CLI subcommand."
  "linear teams")

;;; ============================================================
;;; Repo Commands
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-repo-add (beads-command-json)
  ((repo-path
    :initarg :repo-path
    :type (or null string)
    :initform nil
    :documentation "Repository path to add."
    :positional 1))
  :documentation "Represents bd repo add command.
Adds an additional repository to sync."))

(cl-defmethod beads-command-subcommand ((_command beads-command-repo-add))
  "Return \"repo add\" as the CLI subcommand."
  "repo add")

(cl-defmethod beads-command-validate ((command beads-command-repo-add))
  "Validate repo add COMMAND."
  (with-slots (repo-path) command
    (if (not repo-path) "Repository path is required" nil)))

(eval-and-compile
(beads-defcommand beads-command-repo-list (beads-command-json)
  ()
  :documentation "Represents bd repo list command.
Lists all configured repositories."))

(cl-defmethod beads-command-subcommand ((_command beads-command-repo-list))
  "Return \"repo list\" as the CLI subcommand."
  "repo list")

(eval-and-compile
(beads-defcommand beads-command-repo-remove (beads-command-json)
  ((repo-path
    :initarg :repo-path
    :type (or null string)
    :initform nil
    :documentation "Repository path to remove."
    :positional 1))
  :documentation "Represents bd repo remove command.
Removes a repository from sync configuration."))

(cl-defmethod beads-command-subcommand ((_command beads-command-repo-remove))
  "Return \"repo remove\" as the CLI subcommand."
  "repo remove")

(cl-defmethod beads-command-validate ((command beads-command-repo-remove))
  "Validate repo remove COMMAND."
  (with-slots (repo-path) command
    (if (not repo-path) "Repository path is required" nil)))

(eval-and-compile
(beads-defcommand beads-command-repo-sync (beads-command-json)
  ()
  :documentation "Represents bd repo sync command.
Manually triggers multi-repo sync."))

(cl-defmethod beads-command-subcommand ((_command beads-command-repo-sync))
  "Return \"repo sync\" as the CLI subcommand."
  "repo sync")

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-jira-sync))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-jira-status))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-linear-sync))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-linear-status))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-linear-teams))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-repo-add))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-repo-list))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-repo-remove))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-repo-sync))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

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

(provide 'beads-command-integrations)
;;; beads-command-integrations.el ends here
