;;; beads-command-daemon.el --- Daemon command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO classes for the `bd daemon' subcommands.
;; Each class includes full slot metadata for automatic transient menu
;; generation via `beads-meta-define-transient'.
;;
;; Available daemon subcommands:
;; - list: List all running bd daemons
;; - start: Start the background daemon
;; - stop: Stop a specific bd daemon
;; - status: Show daemon status
;; - health: Check health of all daemons
;; - killall: Stop all running daemons
;; - logs: View logs for a daemon
;; - restart: Restart a daemon

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; ============================================================
;;; Daemon List Command
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-daemon-list (beads-command-json)
  ((no-cleanup
    :initarg :no-cleanup
    :type boolean
    :initform nil
    :documentation "Skip auto-cleanup of stale sockets (--no-cleanup)."
    :long-option "no-cleanup"
    :option-type :boolean
    :key "c"
    :transient "Skip cleanup"
    :class transient-switch
    :argument "--no-cleanup"
    :transient-group "Options"
    :level 2
    :order 1)
   (search
    :initarg :search
    :type (or null list)
    :initform nil
    :documentation "Directories to search for daemons (--search).
Default: home, /tmp, cwd."
    :long-option "search"
    :option-type :string
    :key "s"
    :transient "Search directories"
    :class transient-option
    :argument "--search="
    :prompt "Search dirs (comma-sep): "
    :transient-group "Options"
    :level 2
    :order 2))
  :documentation "Represents bd daemon list command.
Lists all running bd daemons with metadata."))

(cl-defmethod beads-command-subcommand ((_command beads-command-daemon-list))
  "Return subcommand name for daemon list command."
  "daemon list")

(cl-defmethod beads-command-validate ((_command beads-command-daemon-list))
  "Validate daemon list COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-daemon-list))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Daemon Start Command
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-daemon-start (beads-command-json)
  ((auto-commit
    :initarg :auto-commit
    :type boolean
    :initform nil
    :documentation "Automatically commit changes (--auto-commit)."
    :long-option "auto-commit"
    :option-type :boolean
    :key "c"
    :transient "Auto-commit"
    :class transient-switch
    :argument "--auto-commit"
    :transient-group "Sync Options"
    :level 1
    :order 1)
   (auto-push
    :initarg :auto-push
    :type boolean
    :initform nil
    :documentation "Automatically push commits (--auto-push).
Implies --auto-commit."
    :long-option "auto-push"
    :option-type :boolean
    :key "p"
    :transient "Auto-push"
    :class transient-switch
    :argument "--auto-push"
    :transient-group "Sync Options"
    :level 1
    :order 2)
   (auto-pull
    :initarg :auto-pull
    :type boolean
    :initform nil
    :documentation "Automatically pull from remote (--auto-pull).
Default: true when sync.branch configured."
    :long-option "auto-pull"
    :option-type :boolean
    :key "P"
    :transient "Auto-pull"
    :class transient-switch
    :argument "--auto-pull"
    :transient-group "Sync Options"
    :level 2
    :order 3)
   (foreground
    :initarg :foreground
    :type boolean
    :initform nil
    :documentation "Run in foreground, don't daemonize (--foreground).
Useful for systemd/supervisord."
    :long-option "foreground"
    :option-type :boolean
    :key "f"
    :transient "Foreground mode"
    :class transient-switch
    :argument "--foreground"
    :transient-group "Mode"
    :level 2
    :order 1)
   (local
    :initarg :local
    :type boolean
    :initform nil
    :documentation "Run in local-only mode (--local).
No git required, no sync."
    :long-option "local"
    :option-type :boolean
    :key "l"
    :transient "Local-only mode"
    :class transient-switch
    :argument "--local"
    :transient-group "Mode"
    :level 2
    :order 2)
   (interval
    :initarg :interval
    :type (or null string)
    :initform nil
    :documentation "Sync check interval (--interval).
Default: 5s."
    :long-option "interval"
    :option-type :string
    :key "i"
    :transient "Sync interval"
    :class transient-option
    :argument "--interval="
    :prompt "Interval (e.g., 5s, 1m): "
    :transient-group "Logging"
    :level 3
    :order 1)
   (log-file
    :initarg :log-file
    :type (or null string)
    :initform nil
    :documentation "Log file path (--log).
Default: .beads/daemon.log."
    :long-option "log"
    :option-type :string
    :key "L"
    :transient "Log file"
    :class transient-option
    :argument "--log="
    :prompt "Log file: "
    :transient-group "Logging"
    :level 3
    :order 2)
   (log-level
    :initarg :log-level
    :type (or null string)
    :initform nil
    :documentation "Log level (--log-level).
Values: debug, info, warn, error. Default: info."
    :long-option "log-level"
    :option-type :string
    :key "v"
    :transient "Log level"
    :class transient-option
    :argument "--log-level="
    :choices ("debug" "info" "warn" "error")
    :transient-group "Logging"
    :level 3
    :order 3)
   (log-json
    :initarg :log-json
    :type boolean
    :initform nil
    :documentation "Output logs in JSON format (--log-json)."
    :long-option "log-json"
    :option-type :boolean
    :key "j"
    :transient "JSON logs"
    :class transient-switch
    :argument "--log-json"
    :transient-group "Logging"
    :level 3
    :order 4))
  :documentation "Represents bd daemon start command.
Starts the background sync daemon."))

(cl-defmethod beads-command-subcommand ((_command beads-command-daemon-start))
  "Return subcommand name for daemon start command."
  "daemon start")

(cl-defmethod beads-command-validate ((_command beads-command-daemon-start))
  "Validate daemon start COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-daemon-start))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Daemon Stop Command
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-daemon-stop (beads-command-json)
  ((target
    :initarg :target
    :type (or null string)
    :initform nil
    :documentation "Workspace path or PID to stop (positional argument)."
    :positional 1))
  :documentation "Represents bd daemon stop command.
Stops a specific bd daemon gracefully."))

(cl-defmethod beads-command-subcommand ((_command beads-command-daemon-stop))
  "Return subcommand name for daemon stop command."
  "daemon stop")

(cl-defmethod beads-command-validate ((command beads-command-daemon-stop))
  "Validate daemon stop COMMAND.
Target is optional (defaults to current workspace)."
  (ignore command)
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-daemon-stop))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Daemon Status Command
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-daemon-status (beads-command-json)
  ((all
    :initarg :all
    :type boolean
    :initform nil
    :documentation "Show status of all daemons (--all)."
    :long-option "all"
    :option-type :boolean
    :key "a"
    :transient "All daemons"
    :class transient-switch
    :argument "--all"
    :transient-group "Options"
    :level 1
    :order 1)
   (search
    :initarg :search
    :type (or null list)
    :initform nil
    :documentation "Directories to search for daemons (--search).
Use with --all."
    :long-option "search"
    :option-type :string
    :key "s"
    :transient "Search directories"
    :class transient-option
    :argument "--search="
    :prompt "Search dirs (comma-sep): "
    :transient-group "Options"
    :level 2
    :order 2))
  :documentation "Represents bd daemon status command.
Shows daemon status for current workspace or all daemons."))

(cl-defmethod beads-command-subcommand ((_command beads-command-daemon-status))
  "Return subcommand name for daemon status command."
  "daemon status")

(cl-defmethod beads-command-validate ((_command beads-command-daemon-status))
  "Validate daemon status COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-daemon-status))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Daemon Health Command
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-daemon-health (beads-command-json)
  ((search
    :initarg :search
    :type (or null list)
    :initform nil
    :documentation "Directories to search for daemons (--search).
Default: home, /tmp, cwd."
    :long-option "search"
    :option-type :string
    :key "s"
    :transient "Search directories"
    :class transient-option
    :argument "--search="
    :prompt "Search dirs (comma-sep): "
    :transient-group "Options"
    :level 2
    :order 1))
  :documentation "Represents bd daemon health command.
Checks health of all running daemons."))

(cl-defmethod beads-command-subcommand ((_command beads-command-daemon-health))
  "Return subcommand name for daemon health command."
  "daemon health")

(cl-defmethod beads-command-validate ((_command beads-command-daemon-health))
  "Validate daemon health COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-daemon-health))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Daemon Killall Command
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-daemon-killall (beads-command-json)
  ((force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Use SIGKILL immediately if graceful fails (--force)."
    :long-option "force"
    :option-type :boolean
    :key "f"
    :transient "Force kill"
    :class transient-switch
    :argument "--force"
    :transient-group "Options"
    :level 2
    :order 1)
   (search
    :initarg :search
    :type (or null list)
    :initform nil
    :documentation "Directories to search for daemons (--search).
Default: home, /tmp, cwd."
    :long-option "search"
    :option-type :string
    :key "s"
    :transient "Search directories"
    :class transient-option
    :argument "--search="
    :prompt "Search dirs (comma-sep): "
    :transient-group "Options"
    :level 2
    :order 2))
  :documentation "Represents bd daemon killall command.
Stops all running bd daemons."))

(cl-defmethod beads-command-subcommand ((_command beads-command-daemon-killall))
  "Return subcommand name for daemon killall command."
  "daemon killall")

(cl-defmethod beads-command-validate ((_command beads-command-daemon-killall))
  "Validate daemon killall COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-daemon-killall))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Daemon Logs Command
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-daemon-logs (beads-command-json)
  ((target
    :initarg :target
    :type (or null string)
    :initform nil
    :documentation "Workspace path or PID (positional argument)."
    :positional 1)
   (follow
    :initarg :follow
    :type boolean
    :initform nil
    :documentation "Follow log output like tail -f (--follow)."
    :long-option "follow"
    :short-option "f"
    :option-type :boolean
    :key "f"
    :transient "Follow output"
    :class transient-switch
    :argument "--follow"
    :transient-group "Options"
    :level 1
    :order 1)
   (lines
    :initarg :lines
    :type (or null integer)
    :initform nil
    :documentation "Number of lines to show from end (--lines).
Default: 50."
    :long-option "lines"
    :short-option "n"
    :option-type :integer
    :key "n"
    :transient "Number of lines"
    :class transient-option
    :argument "--lines="
    :prompt "Lines (default 50): "
    :transient-group "Options"
    :level 2
    :order 2))
  :documentation "Represents bd daemon logs command.
Views logs for a specific daemon."))

(cl-defmethod beads-command-subcommand ((_command beads-command-daemon-logs))
  "Return subcommand name for daemon logs command."
  "daemon logs")

(cl-defmethod beads-command-validate ((command beads-command-daemon-logs))
  "Validate daemon logs COMMAND.
Target is optional (defaults to current workspace)."
  (ignore command)
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-daemon-logs))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Daemon Restart Command
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-daemon-restart (beads-command-json)
  ((target
    :initarg :target
    :type (or null string)
    :initform nil
    :documentation "Workspace path or PID to restart (positional argument)."
    :positional 1)
   (search
    :initarg :search
    :type (or null list)
    :initform nil
    :documentation "Directories to search for daemons (--search).
Default: home, /tmp, cwd."
    :long-option "search"
    :option-type :string
    :key "s"
    :transient "Search directories"
    :class transient-option
    :argument "--search="
    :prompt "Search dirs (comma-sep): "
    :transient-group "Options"
    :level 2
    :order 1))
  :documentation "Represents bd daemon restart command.
Restarts a specific daemon."))

(cl-defmethod beads-command-subcommand ((_command beads-command-daemon-restart))
  "Return subcommand name for daemon restart command."
  "daemon restart")

(cl-defmethod beads-command-validate ((command beads-command-daemon-restart))
  "Validate daemon restart COMMAND.
Target is optional (defaults to current workspace)."
  (ignore command)
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-daemon-restart))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Transient Menus
;;; ============================================================

;; Individual subcommand transients
;;;###autoload (autoload 'beads-daemon-list "beads-command-daemon" nil t)
(beads-meta-define-transient beads-command-daemon-list "beads-daemon-list"
  "List all running bd daemons.

Shows metadata including workspace path, PID, version, uptime,
last activity, and exclusive lock status.

Transient levels control which options are visible (cycle with C-x l):
  Level 2: All options (no-cleanup, search)"
  beads-option-global-section)

;;;###autoload (autoload 'beads-daemon-start "beads-command-daemon" nil t)
(beads-meta-define-transient beads-command-daemon-start "beads-daemon-start"
  "Start the background sync daemon.

The daemon will:
- Poll for changes at configurable intervals
- Export pending database changes to JSONL
- Auto-commit/push changes if enabled
- Pull remote changes periodically
- Auto-import when remote changes detected

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Sync options (auto-commit, auto-push)
  Level 2: Mode options (foreground, local, auto-pull)
  Level 3: Logging options"
  beads-option-global-section)

;;;###autoload (autoload 'beads-daemon-stop "beads-command-daemon" nil t)
(beads-meta-define-transient beads-command-daemon-stop "beads-daemon-stop"
  "Stop a specific bd daemon.

Sends shutdown command via RPC, with SIGTERM fallback.
Without arguments, stops the current workspace's daemon."
  beads-option-global-section)

;;;###autoload (autoload 'beads-daemon-status "beads-command-daemon" nil t)
(beads-meta-define-transient beads-command-daemon-status "beads-daemon-status"
  "Show daemon status.

Shows status of current workspace's daemon, or all daemons with --all.

Transient levels control which options are visible (cycle with C-x l):
  Level 1: All daemons switch
  Level 2: Search directories"
  beads-option-global-section)

;;;###autoload (autoload 'beads-daemon-health "beads-command-daemon" nil t)
(beads-meta-define-transient beads-command-daemon-health "beads-daemon-health"
  "Check health of all running daemons.

Reports issues including stale sockets, version mismatches,
and unresponsive daemons.

Transient levels control which options are visible (cycle with C-x l):
  Level 2: Search directories"
  beads-option-global-section)

;;;###autoload (autoload 'beads-daemon-killall "beads-command-daemon" nil t)
(beads-meta-define-transient beads-command-daemon-killall "beads-daemon-killall"
  "Stop all running bd daemons.

Uses escalating shutdown: RPC (2s) -> SIGTERM (3s) -> SIGKILL (1s).

Transient levels control which options are visible (cycle with C-x l):
  Level 2: Force and search options"
  beads-option-global-section)

;;;###autoload (autoload 'beads-daemon-logs "beads-command-daemon" nil t)
(beads-meta-define-transient beads-command-daemon-logs "beads-daemon-logs"
  "View logs for a specific daemon.

Supports tail mode (last N lines) and follow mode (like tail -f).

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Follow output
  Level 2: Number of lines"
  beads-option-global-section)

;;;###autoload (autoload 'beads-daemon-restart "beads-command-daemon" nil t)
(beads-meta-define-transient beads-command-daemon-restart "beads-daemon-restart"
  "Restart a specific daemon.

Stops the daemon gracefully, then starts a new one.
Without arguments, restarts the current workspace's daemon.

Transient levels control which options are visible (cycle with C-x l):
  Level 2: Search directories"
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-daemon "beads-command-daemon" nil t)
(transient-define-prefix beads-daemon ()
  "Manage bd background daemons.

The daemon automatically syncs issues with git remote:
- Polls for changes at configurable intervals
- Exports pending changes to JSONL
- Auto-commits and pushes (if enabled)
- Pulls and imports remote changes"
  ["Daemon Commands"
   ("s" "Start daemon" beads-daemon-start)
   ("S" "Status" beads-daemon-status)
   ("o" "Stop daemon" beads-daemon-stop)
   ("r" "Restart daemon" beads-daemon-restart)]
  ["View"
   ("l" "List all daemons" beads-daemon-list)
   ("L" "View logs" beads-daemon-logs)
   ("h" "Health check" beads-daemon-health)]
  ["Danger"
   ("K" "Kill all daemons" beads-daemon-killall)])

(provide 'beads-command-daemon)
;;; beads-command-daemon.el ends here
