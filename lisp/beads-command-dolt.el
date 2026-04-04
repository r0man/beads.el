;;; beads-command-dolt.el --- Dolt command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd dolt' operations.
;; Provides Dolt database configuration and server management:
;; - dolt show: Show config with connection test
;; - dolt set: Set config value
;; - dolt test: Test server connection
;; - dolt commit: Commit pending changes
;; - dolt push: Push to Dolt remote
;; - dolt pull: Pull from Dolt remote
;; - dolt start: Start the Dolt SQL server
;; - dolt stop: Stop the Dolt SQL server
;; - dolt status: Show Dolt server status
;; - dolt remote: Manage Dolt remotes

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-dolt-show
;;; ============================================================

(beads-defcommand beads-command-dolt-show (beads-command-global-options)
  ()
  :documentation "Show current Dolt configuration.
Displays backend, database, host, port, and connection status.")



;;; ============================================================
;;; Command Class: beads-command-dolt-set
;;; ============================================================

(beads-defcommand beads-command-dolt-set (beads-command-global-options)
  ((config-key
    :positional 1
    :type (or null string)
    :short-option "k"
    :argument "--key="
    :prompt "Config key (database|host|port|user|data-dir): "
    :group "Set Config"
    :level 1
    :order 1
    :required t)
   (config-value
    :positional 2
    :type (or null string)
    :short-option "v"
    :argument "--value="
    :group "Set Config"
    :level 1
    :order 2
    :required t)
   (update-config
    :type boolean
    :short-option "u"
    :group "Options"
    :level 2
    :order 3))
  :documentation "Set a Dolt configuration value.
Valid keys: database, host, port, user, data-dir.")


(cl-defmethod beads-command-validate ((command beads-command-dolt-set))
  "Validate dolt set COMMAND.
Both key and value are required."
  (with-slots (config-key config-value) command
    (cond
     ((or (null config-key) (string-empty-p config-key))
      "Must provide a config key")
     ((or (null config-value) (string-empty-p config-value))
      "Must provide a value"))))


;;; ============================================================
;;; Command Class: beads-command-dolt-test
;;; ============================================================

(beads-defcommand beads-command-dolt-test (beads-command-global-options)
  ()
  :documentation "Test connection to Dolt server.
Reports host, port, and connection status.")



;;; ============================================================
;;; Command Class: beads-command-dolt-commit
;;; ============================================================

(beads-defcommand beads-command-dolt-commit (beads-command-global-options)
  ((message
    :short-option "m"
    :type (or null string)
    :prompt "Commit message (empty=auto): "
    :group "Commit"
    :level 1
    :order 1))
  :documentation "Create a Dolt commit from pending changes.
Auto-generates commit message if not provided.")



;;; ============================================================
;;; Command Class: beads-command-dolt-push
;;; ============================================================

(beads-defcommand beads-command-dolt-push (beads-command-global-options)
  ((force
    :type boolean
    :short-option "f"
    :group "Options"
    :level 2
    :order 1))
  :documentation "Push commits to Dolt remote.
Use --force to overwrite remote changes.")



;;; ============================================================
;;; Command Class: beads-command-dolt-pull
;;; ============================================================

(beads-defcommand beads-command-dolt-pull (beads-command-global-options)
  ()
  :documentation "Pull commits from Dolt remote.")



;;; ============================================================
;;; Command Class: beads-command-dolt-start
;;; ============================================================

(beads-defcommand beads-command-dolt-start (beads-command-global-options)
  ()
  :documentation "Start the Dolt SQL server.
Starts server with per-project derived port.")


;;; ============================================================
;;; Command Class: beads-command-dolt-stop
;;; ============================================================

(beads-defcommand beads-command-dolt-stop (beads-command-global-options)
  ((force
    :type boolean
    :short-option "f"
    :group "Options"
    :level 2
    :order 1))
  :documentation "Stop the Dolt SQL server.")


;;; ============================================================
;;; Command Class: beads-command-dolt-status
;;; ============================================================

(beads-defcommand beads-command-dolt-status (beads-command-global-options)
  ()
  :documentation "Show Dolt server status.
Reports PID, port, and running status.")



;;; ============================================================
;;; Command Class: beads-command-dolt-remote-add
;;; ============================================================

(beads-defcommand beads-command-dolt-remote-add (beads-command-global-options)
  ((remote-name
    :positional 1
    :type (or null string)
    :short-option "n"
    :argument "--name="
    :prompt "Remote name: "
    :group "Add Remote"
    :level 1
    :order 1
    :required t)
   (url
    :positional 2
    :type (or null string)
    :short-option "u"
    :argument "--url="
    :prompt "Remote URL: "
    :group "Add Remote"
    :level 1
    :order 2
    :required t))
  :documentation "Add a Dolt remote.")


(cl-defmethod beads-command-validate ((command beads-command-dolt-remote-add))
  "Validate dolt remote add COMMAND."
  (with-slots (remote-name url) command
    (cond
     ((or (null remote-name) (string-empty-p remote-name))
      "Must provide a remote name")
     ((or (null url) (string-empty-p url))
      "Must provide a remote URL"))))

;;; ============================================================
;;; Command Class: beads-command-dolt-remote-list
;;; ============================================================

(beads-defcommand beads-command-dolt-remote-list (beads-command-global-options)
  ()
  :documentation "List configured Dolt remotes.")



;;; ============================================================
;;; Command Class: beads-command-dolt-remote-remove
;;; ============================================================

(beads-defcommand beads-command-dolt-remote-remove (beads-command-global-options)
  ((remote-name
    :positional 1
    :type (or null string)
    :short-option "n"
    :argument "--name="
    :prompt "Remote name to remove: "
    :group "Remove Remote"
    :level 1
    :order 1
    :required t))
  :documentation "Remove a Dolt remote.")


(cl-defmethod beads-command-validate ((command beads-command-dolt-remote-remove))
  "Validate dolt remote remove COMMAND."
  (with-slots (remote-name) command
    (when (or (null remote-name) (string-empty-p remote-name))
      "Must provide a remote name")))

;;; Transient Menus

;;;###autoload (autoload 'beads-dolt-show "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-show "beads-dolt-show"
  "Show current Dolt configuration."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dolt-set "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-set "beads-dolt-set"
  "Set a Dolt configuration value."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dolt-test "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-test "beads-dolt-test"
  "Test connection to Dolt server."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dolt-commit "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-commit "beads-dolt-commit"
  "Create a Dolt commit from pending changes."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dolt-push "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-push "beads-dolt-push"
  "Push commits to Dolt remote."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dolt-pull "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-pull "beads-dolt-pull"
  "Pull commits from Dolt remote."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dolt-start "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-start "beads-dolt-start"
  "Start the Dolt SQL server."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dolt-stop "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-stop "beads-dolt-stop"
  "Stop the Dolt SQL server."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dolt-status "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-status "beads-dolt-status"
  "Show Dolt server status."
  beads-option-global-section)

;;; Dolt Remote Submenu

;;;###autoload (autoload 'beads-dolt-remote "beads-command-dolt" nil t)
(transient-define-prefix beads-dolt-remote ()
  "Manage Dolt remotes."
  ["Dolt Remotes"
   ("a" "Add remote" beads-dolt-remote-add)
   ("l" "List remotes" beads-dolt-remote-list)
   ("r" "Remove remote" beads-dolt-remote-remove)
   ("q" "Quit" transient-quit-one)])

;;;###autoload (autoload 'beads-dolt-remote-add "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-remote-add "beads-dolt-remote-add"
  "Add a Dolt remote."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dolt-remote-list "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-remote-list "beads-dolt-remote-list"
  "List configured Dolt remotes."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dolt-remote-remove "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-remote-remove
  "beads-dolt-remote-remove"
  "Remove a Dolt remote."
  beads-option-global-section)

;;; Clean-Databases Command

(beads-defcommand beads-command-dolt-clean-databases
    (beads-command-global-options)
  ((dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd dolt clean-databases command.
Drop leftover test and polecat databases from the shared Dolt server."
  :cli-command "dolt clean-databases")

;;;###autoload (autoload 'beads-dolt-clean-databases "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-clean-databases
  "beads-dolt-clean-databases"
  "Drop stale test databases from Dolt server."
  beads-option-global-section)

;;; Killall Command

(beads-defcommand beads-command-dolt-killall
    (beads-command-global-options)
  ()
  :documentation "Represents bd dolt killall command.
Find and kill orphan dolt sql-server processes not tracked by the
canonical PID file for the current repo's Dolt data directory."
  :cli-command "dolt killall")

;;;###autoload (autoload 'beads-dolt-killall "beads-command-dolt" nil t)
(beads-meta-define-transient beads-command-dolt-killall
  "beads-dolt-killall"
  "Kill orphan Dolt server processes."
  beads-option-global-section)

;;; Parent Menu

;;;###autoload (autoload 'beads-dolt "beads-command-dolt" nil t)
(transient-define-prefix beads-dolt ()
  "Dolt database configuration and server management.

Provides commands for configuring the Dolt backend, managing
the SQL server, and syncing with Dolt remotes."
  [["Configuration"
    ("s" "Show config" beads-dolt-show)
    ("S" "Set config" beads-dolt-set)
    ("t" "Test connection" beads-dolt-test)]
   ["Data"
    ("c" "Commit" beads-dolt-commit)
    ("p" "Push" beads-dolt-push)
    ("P" "Pull" beads-dolt-pull)]
   ["Server"
    ("1" "Start server" beads-dolt-start)
    ("0" "Stop server" beads-dolt-stop)
    ("i" "Server status" beads-dolt-status)]
   ["Remotes"
    ("r" "Remote menu" beads-dolt-remote)]
   ["Maintenance"
    ("C" "Clean databases" beads-dolt-clean-databases)
    ("K" "Kill orphan servers" beads-dolt-killall)
    ("q" "Quit" transient-quit-one)]])

(provide 'beads-command-dolt)
;;; beads-command-dolt.el ends here
