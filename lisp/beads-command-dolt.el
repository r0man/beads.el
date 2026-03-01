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

(beads-defcommand beads-command-dolt-show (beads-command-json)
  ()
  :documentation "Show current Dolt configuration.
Displays backend, database, host, port, and connection status.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-show))
  "Return \"dolt show\" as the CLI subcommand."
  "dolt show")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-dolt-show))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-dolt-set
;;; ============================================================

(beads-defcommand beads-command-dolt-set (beads-command-json)
  ((config-key
    :initarg :config-key
    :type (or null string)
    :initform nil
    :documentation "Configuration key to set (positional).
Valid keys: database, host, port, user, data-dir."
    :positional 1
    :option-type :string
    :key "k"
    :transient "Config key (required)"
    :class transient-option
    :argument "--key="
    :prompt "Config key (database|host|port|user|data-dir): "
    :transient-group "Set Config"
    :level 1
    :order 1
    :required t)
   (config-value
    :initarg :config-value
    :type (or null string)
    :initform nil
    :documentation "Value to set (positional)."
    :positional 2
    :option-type :string
    :key "v"
    :transient "Value (required)"
    :class transient-option
    :argument "--value="
    :prompt "Value: "
    :transient-group "Set Config"
    :level 1
    :order 2
    :required t)
   (update-config
    :initarg :update-config
    :type boolean
    :initform nil
    :documentation "Also write to config.yaml for team-wide defaults (--update-config)."
    :long-option "update-config"
    :option-type :boolean
    :key "u"
    :transient "--update-config"
    :class transient-switch
    :argument "--update-config"
    :transient-group "Options"
    :level 2
    :order 3))
  :documentation "Set a Dolt configuration value.
Valid keys: database, host, port, user, data-dir.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-set))
  "Return \"dolt set\" as the CLI subcommand."
  "dolt set")

(cl-defmethod beads-command-validate ((command beads-command-dolt-set))
  "Validate dolt set COMMAND.
Both key and value are required."
  (with-slots (config-key config-value) command
    (cond
     ((or (null config-key) (string-empty-p config-key))
      "Must provide a config key")
     ((or (null config-value) (string-empty-p config-value))
      "Must provide a value"))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-dolt-set))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-dolt-test
;;; ============================================================

(beads-defcommand beads-command-dolt-test (beads-command-json)
  ()
  :documentation "Test connection to Dolt server.
Reports host, port, and connection status.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-test))
  "Return \"dolt test\" as the CLI subcommand."
  "dolt test")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-dolt-test))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-dolt-commit
;;; ============================================================

(beads-defcommand beads-command-dolt-commit (beads-command-json)
  ((message
    :initarg :message
    :type (or null string)
    :initform nil
    :documentation "Commit message (-m, --message).
Auto-generates if not provided."
    :long-option "message"
    :short-option "m"
    :option-type :string
    :key "m"
    :transient "--message"
    :class transient-option
    :argument "--message="
    :prompt "Commit message (empty=auto): "
    :transient-group "Commit"
    :level 1
    :order 1))
  :documentation "Create a Dolt commit from pending changes.
Auto-generates commit message if not provided.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-commit))
  "Return \"dolt commit\" as the CLI subcommand."
  "dolt commit")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-dolt-commit))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-dolt-push
;;; ============================================================

(beads-defcommand beads-command-dolt-push (beads-command-json)
  ((force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force push, overwrite remote changes (--force)."
    :long-option "force"
    :option-type :boolean
    :key "f"
    :transient "--force"
    :class transient-switch
    :argument "--force"
    :transient-group "Options"
    :level 2
    :order 1))
  :documentation "Push commits to Dolt remote.
Use --force to overwrite remote changes.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-push))
  "Return \"dolt push\" as the CLI subcommand."
  "dolt push")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-dolt-push))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-dolt-pull
;;; ============================================================

(beads-defcommand beads-command-dolt-pull (beads-command-json)
  ()
  :documentation "Pull commits from Dolt remote.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-pull))
  "Return \"dolt pull\" as the CLI subcommand."
  "dolt pull")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-dolt-pull))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-dolt-start
;;; ============================================================

(beads-defcommand beads-command-dolt-start (beads-command)
  ()
  :documentation "Start the Dolt SQL server.
Starts server with per-project derived port.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-start))
  "Return \"dolt start\" as the CLI subcommand."
  "dolt start")

;;; ============================================================
;;; Command Class: beads-command-dolt-stop
;;; ============================================================

(beads-defcommand beads-command-dolt-stop (beads-command)
  ((force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force stop even when managed by daemon (--force)."
    :long-option "force"
    :option-type :boolean
    :key "f"
    :transient "--force"
    :class transient-switch
    :argument "--force"
    :transient-group "Options"
    :level 2
    :order 1))
  :documentation "Stop the Dolt SQL server.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-stop))
  "Return \"dolt stop\" as the CLI subcommand."
  "dolt stop")

;;; ============================================================
;;; Command Class: beads-command-dolt-status
;;; ============================================================

(beads-defcommand beads-command-dolt-status (beads-command-json)
  ()
  :documentation "Show Dolt server status.
Reports PID, port, and running status.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-status))
  "Return \"dolt status\" as the CLI subcommand."
  "dolt status")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-dolt-status))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-dolt-remote-add
;;; ============================================================

(beads-defcommand beads-command-dolt-remote-add (beads-command-json)
  ((remote-name
    :initarg :remote-name
    :type (or null string)
    :initform nil
    :documentation "Remote name (positional argument)."
    :positional 1
    :option-type :string
    :key "n"
    :transient "Name (required)"
    :class transient-option
    :argument "--name="
    :prompt "Remote name: "
    :transient-group "Add Remote"
    :level 1
    :order 1
    :required t)
   (url
    :initarg :url
    :type (or null string)
    :initform nil
    :documentation "Remote URL (positional argument)."
    :positional 2
    :option-type :string
    :key "u"
    :transient "URL (required)"
    :class transient-option
    :argument "--url="
    :prompt "Remote URL: "
    :transient-group "Add Remote"
    :level 1
    :order 2
    :required t))
  :documentation "Add a Dolt remote.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-remote-add))
  "Return \"dolt remote add\" as the CLI subcommand."
  "dolt remote add")

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

(beads-defcommand beads-command-dolt-remote-list (beads-command-json)
  ()
  :documentation "List configured Dolt remotes.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-remote-list))
  "Return \"dolt remote list\" as the CLI subcommand."
  "dolt remote list")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-dolt-remote-list))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-dolt-remote-remove
;;; ============================================================

(beads-defcommand beads-command-dolt-remote-remove (beads-command-json)
  ((remote-name
    :initarg :remote-name
    :type (or null string)
    :initform nil
    :documentation "Remote name to remove (positional argument)."
    :positional 1
    :option-type :string
    :key "n"
    :transient "Name (required)"
    :class transient-option
    :argument "--name="
    :prompt "Remote name to remove: "
    :transient-group "Remove Remote"
    :level 1
    :order 1
    :required t))
  :documentation "Remove a Dolt remote.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dolt-remote-remove))
  "Return \"dolt remote remove\" as the CLI subcommand."
  "dolt remote remove")

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
    ("r" "Remote menu" beads-dolt-remote)
    ("q" "Quit" transient-quit-one)]])

(provide 'beads-command-dolt)
;;; beads-command-dolt.el ends here
