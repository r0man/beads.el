;;; beads-daemon.el --- Daemon management for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides the transient menu interface for managing the
;; bd daemon.  The daemon handles automatic syncing of issues with git
;; remote.
;;
;; The core daemon command classes, data structures, and parsing functions
;; are defined in beads-command.el.  This module provides:
;; - Transient menus for daemon management
;; - High-level API functions
;; - Interactive commands
;;
;; Usage:
;;   M-x beads-daemon RET    ; Open daemon transient menu
;;   M-x beads-daemon-show-status RET  ; Show daemon status

;;; Code:

(require 'beads-command)
(require 'cl-lib)
(require 'eieio)
(require 'transient)

;; Forward declarations
(defvar beads-executable)
(declare-function beads--log "beads")

;;; ============================================================
;;; High-Level API Functions
;;; ============================================================

(defun beads-daemon--running-p ()
  "Check if the daemon is currently running.
Returns t if daemon is running, nil otherwise."
  (condition-case nil
      (let ((status (beads-command-execute (beads-daemon-command-status))))
        (oref status running))
    (error nil)))

(defun beads-daemon--get-status ()
  "Get current daemon status.
Returns a `beads-daemon-status' struct or nil on error."
  (condition-case nil
      (beads-command-execute (beads-daemon-command-status))
    (error nil)))

(defun beads-daemon--get-health ()
  "Get current daemon health.
Returns a `beads-daemon-health' struct or nil on error."
  (condition-case nil
      (beads-command-execute (beads-daemon-command-health))
    (error nil)))

(defun beads-daemon--get-metrics ()
  "Get current daemon metrics.
Returns a `beads-daemon-metrics' struct or nil on error."
  (condition-case nil
      (beads-command-execute (beads-daemon-command-metrics))
    (error nil)))

(defun beads-daemon--get-log-path ()
  "Get the daemon log file path.
Returns the path string or nil if daemon is not running."
  (when-let ((status (beads-daemon--get-status)))
    (oref status log-path)))

;;; ============================================================
;;; Transient State Variables
;;; ============================================================

(defvar beads-daemon-start--auto-commit nil
  "Auto-commit flag for daemon start.")

(defvar beads-daemon-start--auto-push nil
  "Auto-push flag for daemon start.")

(defvar beads-daemon-start--foreground nil
  "Foreground flag for daemon start.")

(defvar beads-daemon-start--local nil
  "Local-only flag for daemon start.")

(defvar beads-daemon-start--interval nil
  "Sync interval for daemon start.")

(defvar beads-daemon-start--log nil
  "Log file path for daemon start.")

;;; ============================================================
;;; Transient Infix Commands
;;; ============================================================

(transient-define-infix beads-daemon-start--infix-auto-commit ()
  "Toggle auto-commit flag."
  :class 'transient-lisp-variable
  :variable 'beads-daemon-start--auto-commit
  :key "-a"
  :description "Auto-commit changes"
  :argument "--auto-commit")

(transient-define-infix beads-daemon-start--infix-auto-push ()
  "Toggle auto-push flag."
  :class 'transient-lisp-variable
  :variable 'beads-daemon-start--auto-push
  :key "-p"
  :description "Auto-push commits"
  :argument "--auto-push")

(transient-define-infix beads-daemon-start--infix-foreground ()
  "Toggle foreground flag."
  :class 'transient-lisp-variable
  :variable 'beads-daemon-start--foreground
  :key "-f"
  :description "Run in foreground"
  :argument "--foreground")

(transient-define-infix beads-daemon-start--infix-local ()
  "Toggle local-only flag."
  :class 'transient-lisp-variable
  :variable 'beads-daemon-start--local
  :key "-l"
  :description "Local-only mode"
  :argument "--local")

(transient-define-infix beads-daemon-start--infix-interval ()
  "Set sync interval."
  :class 'transient-option
  :key "-i"
  :description "Sync interval"
  :argument "--interval="
  :reader (lambda (prompt _initial _history)
            (read-string prompt "5s")))

(transient-define-infix beads-daemon-start--infix-log ()
  "Set log file path."
  :class 'transient-option
  :key "-L"
  :description "Log file path"
  :argument "--log="
  :reader (lambda (prompt _initial _history)
            (read-file-name prompt)))

;;; ============================================================
;;; Transient Suffix Commands
;;; ============================================================

(defun beads-daemon-start--reset-state ()
  "Reset all daemon start state variables."
  (setq beads-daemon-start--auto-commit nil
        beads-daemon-start--auto-push nil
        beads-daemon-start--foreground nil
        beads-daemon-start--local nil
        beads-daemon-start--interval nil
        beads-daemon-start--log nil))

(defun beads-daemon-start--parse-transient-args (args)
  "Parse transient ARGS into a beads-daemon-command-start instance."
  (beads-daemon-command-start
   :auto-commit (transient-arg-value "--auto-commit" args)
   :auto-push (transient-arg-value "--auto-push" args)
   :foreground (transient-arg-value "--foreground" args)
   :local (transient-arg-value "--local" args)
   :interval (transient-arg-value "--interval=" args)
   :log (transient-arg-value "--log=" args)))

(transient-define-suffix beads-daemon-start--execute ()
  "Execute daemon start with current options."
  :key "s"
  :description "Start daemon"
  (interactive)
  (let* ((args (transient-args 'beads-daemon-start))
         (cmd (beads-daemon-start--parse-transient-args args))
         (error-msg (beads-command-validate cmd)))
    (if error-msg
        (user-error "Validation failed: %s" error-msg)
      (condition-case err
          (progn
            (beads-command-execute cmd)
            (beads-daemon-start--reset-state)
            (message "Daemon started successfully"))
        (error
         (message "Failed to start daemon: %s" (error-message-string err)))))))

(transient-define-suffix beads-daemon-start--preview ()
  "Preview the daemon start command."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-daemon-start))
         (cmd (beads-daemon-start--parse-transient-args args))
         (error-msg (beads-command-validate cmd)))
    (if error-msg
        (message "Validation errors: %s" error-msg)
      (let* ((cmd-list (beads-command-line cmd))
             (cmd-string (mapconcat #'shell-quote-argument cmd-list " ")))
        (message "Command: %s" cmd-string)))))

(transient-define-suffix beads-daemon-start--reset ()
  "Reset all daemon start options."
  :key "R"
  :description "Reset options"
  :transient t
  (interactive)
  (beads-daemon-start--reset-state)
  (transient-reset)
  (message "Options reset"))

(transient-define-suffix beads-daemon--stop ()
  "Stop the running daemon."
  :key "S"
  :description "Stop daemon"
  (interactive)
  (if (not (beads-daemon--running-p))
      (message "Daemon is not running")
    (when (y-or-n-p "Stop the daemon? ")
      (condition-case err
          (progn
            (beads-command-execute (beads-daemon-command-stop))
            (message "Daemon stopped"))
        (error
         (message "Failed to stop daemon: %s" (error-message-string err)))))))

(transient-define-suffix beads-daemon--restart ()
  "Restart the daemon."
  :key "r"
  :description "Restart daemon"
  (interactive)
  (condition-case err
      (progn
        (when (beads-daemon--running-p)
          (beads-command-execute (beads-daemon-command-stop))
          (sit-for 0.5))
        (beads-command-execute (beads-daemon-command-start))
        (message "Daemon restarted"))
    (error
     (message "Failed to restart daemon: %s" (error-message-string err)))))

(transient-define-suffix beads-daemon--show-status ()
  "Show daemon status in minibuffer."
  :key "t"
  :description "Status"
  :transient t
  (interactive)
  (let ((status (beads-daemon--get-status)))
    (if (null status)
        (message "Daemon is not running or not accessible")
      (message "Daemon: %s, PID: %s, Started: %s"
               (if (oref status running) "Running" "Stopped")
               (or (oref status pid) "N/A")
               (or (oref status started) "N/A")))))

(transient-define-suffix beads-daemon--show-health ()
  "Show daemon health in minibuffer."
  :key "h"
  :description "Health"
  :transient t
  (interactive)
  (let ((health (beads-daemon--get-health)))
    (if (null health)
        (message "Daemon is not running or not accessible")
      (message "Health: %s, Version: %s, Uptime: %s, DB: %.2fms, Mem: %s"
               (or (oref health status) "unknown")
               (or (oref health version) "unknown")
               (beads-daemon--format-uptime
                (oref health uptime-seconds))
               (or (oref health db-response-ms) 0)
               (beads-daemon--format-bytes
                (oref health memory-alloc-mb))))))

(transient-define-suffix beads-daemon--show-metrics ()
  "Show daemon metrics in minibuffer."
  :key "m"
  :description "Metrics"
  :transient t
  (interactive)
  (let ((metrics (beads-daemon--get-metrics)))
    (if (null metrics)
        (message "Daemon is not running or not accessible")
      (message "Uptime: %s, Ops: %d, Conns: %d/%d, Mem: %s/%s, Goroutines: %d"
               (beads-daemon--format-uptime
                (oref metrics uptime-seconds))
               (length (oref metrics operations))
               (or (oref metrics active-connections) 0)
               (or (oref metrics total-connections) 0)
               (beads-daemon--format-bytes
                (oref metrics memory-alloc-mb))
               (beads-daemon--format-bytes
                (oref metrics memory-sys-mb))
               (or (oref metrics goroutine-count) 0)))))

(transient-define-suffix beads-daemon--view-log ()
  "View daemon log file."
  :key "l"
  :description "View log"
  (interactive)
  (let ((log-path (beads-daemon--get-log-path)))
    (if (null log-path)
        (message "Daemon is not running or log path not available")
      (if (file-exists-p log-path)
          (view-file log-path)
        (message "Log file does not exist: %s" log-path)))))

(transient-define-suffix beads-daemon--tail-log ()
  "Tail daemon log file with auto-refresh."
  :key "L"
  :description "Tail log"
  (interactive)
  (let ((log-path (beads-daemon--get-log-path)))
    (if (null log-path)
        (message "Daemon is not running or log path not available")
      (if (file-exists-p log-path)
          (progn
            (find-file log-path)
            (goto-char (point-max))
            (auto-revert-tail-mode 1)
            (message "Following log file (auto-revert-tail-mode enabled)"))
        (message "Log file does not exist: %s" log-path)))))

;;; ============================================================
;;; Transient Menus
;;; ============================================================

;;;###autoload (autoload 'beads-daemon-start "beads-daemon" nil t)
(transient-define-prefix beads-daemon-start ()
  "Start the bd daemon with options.

This transient menu provides an interface for starting the daemon
with various configuration options."
  ["Options"
   (beads-daemon-start--infix-auto-commit)
   (beads-daemon-start--infix-auto-push)
   (beads-daemon-start--infix-foreground)
   (beads-daemon-start--infix-local)
   (beads-daemon-start--infix-interval)
   (beads-daemon-start--infix-log)]
  ["Actions"
   (beads-daemon-start--execute)
   (beads-daemon-start--preview)
   (beads-daemon-start--reset)])

(defun beads-daemon--format-header ()
  "Format header showing daemon status for transient menu."
  (let ((status (beads-daemon--get-status)))
    (if (and status (oref status running))
        (concat
         (propertize "Daemon: " 'face 'bold)
         (propertize "Running" 'face 'success)
         (propertize " (PID: " 'face 'shadow)
         (propertize (number-to-string
                      (or (oref status pid) 0))
                     'face 'font-lock-constant-face)
         (propertize ")" 'face 'shadow))
      (concat
       (propertize "Daemon: " 'face 'bold)
       (propertize "Stopped" 'face 'warning)))))

;;;###autoload (autoload 'beads-daemon "beads-daemon" nil t)
(transient-define-prefix beads-daemon ()
  "Manage the bd daemon.

The daemon handles automatic syncing of issues with git remote.
This menu provides access to all daemon management operations."
  [:description
   (lambda () (beads-daemon--format-header))]
  ["Actions"
   ("s" "Start" beads-daemon-start)
   (beads-daemon--stop)
   (beads-daemon--restart)]
  ["Status"
   (beads-daemon--show-status)
   (beads-daemon--show-health)
   (beads-daemon--show-metrics)]
  ["Logs"
   (beads-daemon--view-log)
   (beads-daemon--tail-log)]
  ["Other"
   ("q" "Quit" transient-quit-one)])

;;; ============================================================
;;; Interactive Commands
;;; ============================================================

;;;###autoload
(defun beads-daemon-show-status ()
  "Display daemon status information interactively."
  (interactive)
  (let ((status (beads-daemon--get-status)))
    (if (null status)
        (message "Daemon is not running or not accessible")
      (message "Daemon: %s, PID: %s, Started: %s, Log: %s"
               (if (oref status running) "Running" "Stopped")
               (or (oref status pid) "N/A")
               (or (oref status started) "N/A")
               (or (oref status log-path) "N/A")))))

(provide 'beads-daemon)
;;; beads-daemon.el ends here
