;;; beads-daemons.el --- Multi-daemon management for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides the interface for managing multiple bd daemons
;; across all repositories and worktrees.  Unlike beads-daemon.el which
;; manages a single project's daemon, this module provides system-wide
;; daemon management.
;;
;; Features:
;; - List all running daemons across repositories
;; - Health check all daemons
;; - Stop/restart specific daemons by workspace or PID
;; - View logs for any daemon
;; - Kill all daemons at once
;;
;; The command classes and data structures are defined in beads-command.el.
;;
;; Usage:
;;   M-x beads-daemons RET          ; Open daemons transient menu
;;   M-x beads-daemons-list RET     ; Open daemons list buffer

;;; Code:

(require 'beads-command)
(require 'cl-lib)
(require 'eieio)
(require 'iso8601)
(require 'tabulated-list)
(require 'transient)

;; Forward declarations
(defvar beads-executable)
(declare-function beads--log "beads")
(declare-function beads-daemon--format-uptime "beads-command")

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup beads-daemons nil
  "Multi-daemon management settings."
  :group 'beads
  :prefix "beads-daemons-")

(defcustom beads-daemons-list-auto-refresh nil
  "If non-nil, automatically refresh daemons list buffer.
When set to a number, refresh every N seconds."
  :type '(choice (const :tag "No auto-refresh" nil)
                 (integer :tag "Refresh interval (seconds)"))
  :group 'beads-daemons)

;;; ============================================================
;;; Faces
;;; ============================================================

(defface beads-daemons-healthy-face
  '((t :inherit success))
  "Face for healthy daemon status."
  :group 'beads-daemons)

(defface beads-daemons-stale-face
  '((t :inherit error))
  "Face for stale daemon status."
  :group 'beads-daemons)

(defface beads-daemons-mismatch-face
  '((t :inherit warning))
  "Face for version-mismatched daemon status."
  :group 'beads-daemons)

(defface beads-daemons-workspace-face
  '((t :inherit font-lock-string-face))
  "Face for workspace paths."
  :group 'beads-daemons)

(defface beads-daemons-pid-face
  '((t :inherit font-lock-constant-face))
  "Face for PIDs."
  :group 'beads-daemons)

(defface beads-daemons-version-face
  '((t :inherit font-lock-type-face))
  "Face for version strings."
  :group 'beads-daemons)

(defface beads-daemons-lock-face
  '((t :inherit font-lock-warning-face))
  "Face for exclusive lock indicator."
  :group 'beads-daemons)

;;; ============================================================
;;; High-Level API Functions
;;; ============================================================

(defun beads-daemons--list (&optional search)
  "Get list of all running daemons.
Optional SEARCH is a list of directories to search.
Returns a list of `beads-daemon-info' objects."
  (condition-case nil
      (beads-command-execute
       (beads-daemons-command-list :search (or search nil)))
    (error nil)))

(defun beads-daemons--health (&optional search)
  "Get health report for all daemons.
Optional SEARCH is a list of directories to search.
Returns a `beads-daemons-health-report' object."
  (condition-case nil
      (beads-command-execute
       (beads-daemons-command-health :search (or search nil)))
    (error nil)))

(defun beads-daemons--stop (target)
  "Stop a daemon by TARGET (workspace path or PID).
Returns a `beads-daemons-stop-result' object."
  (beads-command-execute
   (beads-daemons-command-stop :target target)))

(defun beads-daemons--restart (target &optional search)
  "Restart a daemon by TARGET (workspace path or PID).
Optional SEARCH is a list of directories to search.
Returns a `beads-daemons-restart-result' object."
  (beads-command-execute
   (beads-daemons-command-restart :target target :search (or search nil))))

(defun beads-daemons--logs (target &optional lines)
  "Get logs for a daemon by TARGET (workspace path or PID).
Optional LINES specifies number of lines to return (default 50).
Returns a `beads-daemons-logs-result' object."
  (beads-command-execute
   (beads-daemons-command-logs :target target :lines lines)))

(defun beads-daemons--killall (&optional force search)
  "Kill all running daemons.
If FORCE is non-nil, use SIGKILL immediately.
Optional SEARCH is a list of directories to search.
Returns a `beads-daemons-killall-result' object."
  (beads-command-execute
   (beads-daemons-command-killall :force force :search (or search nil))))

;;; ============================================================
;;; List Buffer Mode
;;; ============================================================

(defvar-local beads-daemons-list--data nil
  "Cached daemon data for current list buffer.
A list of `beads-daemon-info' objects.")

(defvar-local beads-daemons-list--refresh-timer nil
  "Timer for auto-refresh in daemons list buffer.")

(defvar beads-daemons-list-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Refresh/quit
    (define-key map (kbd "g") #'beads-daemons-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    ;; Actions on daemon at point
    (define-key map (kbd "s") #'beads-daemons-list-stop)
    (define-key map (kbd "r") #'beads-daemons-list-restart)
    (define-key map (kbd "l") #'beads-daemons-list-view-log)
    (define-key map (kbd "RET") #'beads-daemons-list-view-log)
    ;; Bulk actions
    (define-key map (kbd "K") #'beads-daemons-list-killall)
    (define-key map (kbd "H") #'beads-daemons-list-health)
    ;; Help
    (define-key map (kbd "?") #'beads-daemons-list-help)
    map)
  "Keymap for `beads-daemons-list-mode'.")

(defun beads-daemons-list--revert-buffer (_ignore-auto _noconfirm)
  "Revert function for daemons list buffer.
Arguments IGNORE-AUTO and NOCONFIRM are ignored."
  (beads-daemons-list-refresh))

(define-derived-mode beads-daemons-list-mode tabulated-list-mode "Beads-Daemons"
  "Major mode for displaying list of bd daemons.

\\{beads-daemons-list-mode-map}"
  (setq tabulated-list-format
        [("Workspace" 40 t)
         ("PID" 8 t)
         ("Version" 10 t)
         ("Uptime" 12 t)
         ("Activity" 12 t)
         ("Lock" 15 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Workspace" . nil))
  (tabulated-list-init-header)
  (setq-local revert-buffer-function #'beads-daemons-list--revert-buffer)
  ;; Set up auto-refresh timer if configured
  (when (and beads-daemons-list-auto-refresh
             (numberp beads-daemons-list-auto-refresh))
    (setq beads-daemons-list--refresh-timer
          (run-with-timer beads-daemons-list-auto-refresh
                          beads-daemons-list-auto-refresh
                          #'beads-daemons-list--auto-refresh
                          (current-buffer))))
  ;; Clean up timer on buffer kill
  (add-hook 'kill-buffer-hook #'beads-daemons-list--cleanup nil t))

(defun beads-daemons-list--cleanup ()
  "Clean up resources when daemons list buffer is killed."
  (when beads-daemons-list--refresh-timer
    (cancel-timer beads-daemons-list--refresh-timer)
    (setq beads-daemons-list--refresh-timer nil)))

(defun beads-daemons-list--auto-refresh (buffer)
  "Auto-refresh daemons list in BUFFER if still visible."
  (when (and (buffer-live-p buffer)
             (get-buffer-window buffer))
    (with-current-buffer buffer
      (beads-daemons-list-refresh))))

;;; ============================================================
;;; List Buffer Entry Formatting
;;; ============================================================

(defun beads-daemons-list--format-workspace (workspace)
  "Format WORKSPACE path for display."
  (if (null workspace)
      (propertize "(unknown)" 'face 'shadow)
    (propertize (abbreviate-file-name workspace)
                'face 'beads-daemons-workspace-face)))

(defun beads-daemons-list--format-pid (pid)
  "Format PID for display."
  (if (null pid)
      "-"
    (propertize (number-to-string pid) 'face 'beads-daemons-pid-face)))

(defun beads-daemons-list--format-version (version)
  "Format VERSION for display."
  (if (or (null version) (string-empty-p version))
      "-"
    (propertize version 'face 'beads-daemons-version-face)))

(defun beads-daemons-list--format-uptime (seconds)
  "Format SECONDS as uptime string."
  (if (or (null seconds) (< seconds 0))
      "-"
    (beads-daemon--format-uptime seconds)))

(defun beads-daemons-list--format-activity (last-activity-time)
  "Format LAST-ACTIVITY-TIME as relative time."
  (if (or (null last-activity-time) (string-empty-p last-activity-time))
      "-"
    (condition-case nil
        (let* ((decoded (iso8601-parse last-activity-time))
               (time (encode-time decoded))
               (diff (float-time (time-subtract (current-time) time))))
          (cond
           ((< diff 60) "just now")
           ((< diff 3600) (format "%.0fm ago" (/ diff 60)))
           ((< diff 86400) (format "%.1fh ago" (/ diff 3600)))
           (t (format "%.1fd ago" (/ diff 86400)))))
      (error "-"))))

(defun beads-daemons-list--format-lock (active holder)
  "Format lock status from ACTIVE and HOLDER."
  (if active
      (propertize (format "locked: %s" (or holder "?"))
                  'face 'beads-daemons-lock-face)
    "-"))

(defun beads-daemons-list--make-entry (daemon)
  "Make tabulated-list entry from DAEMON info object."
  (let ((workspace (oref daemon workspace-path))
        (pid (oref daemon pid))
        (version (oref daemon version))
        (uptime (oref daemon uptime-seconds))
        (activity (oref daemon last-activity-time))
        (lock-active (oref daemon exclusive-lock-active))
        (lock-holder (oref daemon exclusive-lock-holder)))
    (list daemon
          (vector (beads-daemons-list--format-workspace workspace)
                  (beads-daemons-list--format-pid pid)
                  (beads-daemons-list--format-version version)
                  (beads-daemons-list--format-uptime uptime)
                  (beads-daemons-list--format-activity activity)
                  (beads-daemons-list--format-lock lock-active lock-holder)))))

;;; ============================================================
;;; List Buffer Commands
;;; ============================================================

(defun beads-daemons-list-refresh ()
  "Refresh the daemons list buffer."
  (interactive)
  (unless (derived-mode-p 'beads-daemons-list-mode)
    (user-error "Not in a daemons list buffer"))
  (let ((daemons (beads-daemons--list)))
    (setq beads-daemons-list--data daemons)
    (setq tabulated-list-entries
          (mapcar #'beads-daemons-list--make-entry (or daemons nil)))
    (tabulated-list-print t)
    (message "Found %d daemon(s)" (length (or daemons nil)))))

(defun beads-daemons-list--get-daemon-at-point ()
  "Get the daemon info object at point."
  (tabulated-list-get-id))

(defun beads-daemons-list--get-target-at-point ()
  "Get the target (workspace path) for daemon at point."
  (when-let ((daemon (beads-daemons-list--get-daemon-at-point)))
    (or (oref daemon workspace-path)
        (when-let ((pid (oref daemon pid)))
          (number-to-string pid)))))

(defun beads-daemons-list-stop ()
  "Stop the daemon at point."
  (interactive)
  (let ((target (beads-daemons-list--get-target-at-point)))
    (if (null target)
        (user-error "No daemon at point")
      (when (y-or-n-p (format "Stop daemon for %s? " target))
        (condition-case err
            (let ((result (beads-daemons--stop target)))
              (if (oref result stopped)
                  (progn
                    (message "Stopped daemon for %s (PID %s)"
                             (or (oref result workspace) target)
                             (or (oref result pid) "?"))
                    (beads-daemons-list-refresh))
                (message "Failed to stop daemon")))
          (error
           (message "Error stopping daemon: %s" (error-message-string err))))))))

(defun beads-daemons-list-restart ()
  "Restart the daemon at point."
  (interactive)
  (let ((target (beads-daemons-list--get-target-at-point)))
    (if (null target)
        (user-error "No daemon at point")
      (when (y-or-n-p (format "Restart daemon for %s? " target))
        (condition-case err
            (let ((result (beads-daemons--restart target)))
              (message "Restarted daemon for %s"
                       (or (oref result workspace) target))
              (sit-for 1)
              (beads-daemons-list-refresh))
          (error
           (message "Error restarting daemon: %s" (error-message-string err))))))))

(defun beads-daemons-list-view-log ()
  "View logs for the daemon at point."
  (interactive)
  (let ((target (beads-daemons-list--get-target-at-point)))
    (if (null target)
        (user-error "No daemon at point")
      (condition-case err
          (let* ((result (beads-daemons--logs target 100))
                 (content (oref result content))
                 (log-path (oref result log-path))
                 (workspace (oref result workspace))
                 (buffer-name (format "*beads-daemon-log: %s*"
                                      (or workspace target))))
            (with-current-buffer (get-buffer-create buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (or content "No log content"))
                (goto-char (point-min)))
              (special-mode)
              (setq-local header-line-format
                          (format " Log: %s" (or log-path "?"))))
            (switch-to-buffer buffer-name))
        (error
         (message "Error fetching logs: %s" (error-message-string err)))))))

(defun beads-daemons-list-killall ()
  "Kill all running daemons."
  (interactive)
  (let ((count (length beads-daemons-list--data)))
    (when (y-or-n-p (format "Kill all %d daemon(s)? " count))
      (condition-case err
          (let ((result (beads-daemons--killall)))
            (message "Stopped: %d, Failed: %d"
                     (or (oref result stopped) 0)
                     (or (oref result failed) 0))
            (sit-for 1)
            (beads-daemons-list-refresh))
        (error
         (message "Error killing daemons: %s" (error-message-string err)))))))

(defun beads-daemons-list-health ()
  "Show health report for all daemons."
  (interactive)
  (condition-case err
      (let ((report (beads-daemons--health)))
        (message "Total: %d | Healthy: %d | Stale: %d | Mismatched: %d"
                 (or (oref report total) 0)
                 (or (oref report healthy) 0)
                 (or (oref report stale) 0)
                 (or (oref report mismatched) 0)))
    (error
     (message "Error fetching health: %s" (error-message-string err)))))

(defun beads-daemons-list-help ()
  "Show help for daemons list mode."
  (interactive)
  (message "g:refresh  s:stop  r:restart  l:log  K:killall  H:health  q:quit"))

;;;###autoload
(defun beads-daemons-list ()
  "Display list of all running bd daemons."
  (interactive)
  (let ((buffer (get-buffer-create "*beads-daemons*")))
    (with-current-buffer buffer
      (beads-daemons-list-mode)
      (beads-daemons-list-refresh))
    (switch-to-buffer buffer)))

;;; ============================================================
;;; Transient State Variables
;;; ============================================================

(defvar beads-daemons--search nil
  "Search directories for daemon discovery.")

(defvar beads-daemons--target nil
  "Target workspace path or PID for stop/restart/logs.")

(defvar beads-daemons--lines nil
  "Number of log lines to show.")

(defvar beads-daemons--force nil
  "Force flag for killall.")

;;; ============================================================
;;; Transient Infix Commands
;;; ============================================================

(transient-define-infix beads-daemons--infix-search ()
  "Set search directories."
  :class 'transient-option
  :key "-s"
  :description "Search directory"
  :argument "--search="
  :multi-value t
  :reader (lambda (prompt _initial _history)
            (read-directory-name prompt)))

(transient-define-infix beads-daemons--infix-target ()
  "Set target workspace or PID."
  :class 'transient-option
  :key "-t"
  :description "Target (workspace/PID)"
  :argument "--target="
  :reader (lambda (prompt _initial _history)
            (read-string prompt)))

(transient-define-infix beads-daemons--infix-lines ()
  "Set number of log lines."
  :class 'transient-option
  :key "-n"
  :description "Log lines"
  :argument "--lines="
  :reader (lambda (prompt _initial _history)
            (read-number prompt 50)))

(transient-define-infix beads-daemons--infix-force ()
  "Toggle force flag for killall."
  :class 'transient-lisp-variable
  :variable 'beads-daemons--force
  :key "-f"
  :description "Force (SIGKILL)"
  :argument "--force")

;;; ============================================================
;;; Transient Suffix Commands
;;; ============================================================

(defun beads-daemons--reset-state ()
  "Reset all daemons transient state variables."
  (setq beads-daemons--search nil
        beads-daemons--target nil
        beads-daemons--lines nil
        beads-daemons--force nil))

(transient-define-suffix beads-daemons--list-all ()
  "List all running daemons in a buffer."
  :key "l"
  :description "List daemons"
  (interactive)
  (beads-daemons-list))

(transient-define-suffix beads-daemons--show-health ()
  "Show health report for all daemons."
  :key "h"
  :description "Health check"
  :transient t
  (interactive)
  (condition-case err
      (let ((report (beads-daemons--health)))
        (message "Total: %d | Healthy: %d | Stale: %d | Mismatched: %d"
                 (or (oref report total) 0)
                 (or (oref report healthy) 0)
                 (or (oref report stale) 0)
                 (or (oref report mismatched) 0)))
    (error
     (message "Error: %s" (error-message-string err)))))

(transient-define-suffix beads-daemons--do-stop ()
  "Stop a specific daemon."
  :key "s"
  :description "Stop daemon"
  (interactive)
  (let* ((args (transient-args 'beads-daemons))
         (target (transient-arg-value "--target=" args)))
    (if (or (null target) (string-empty-p target))
        (user-error "Target is required (use -t to set)")
      (when (y-or-n-p (format "Stop daemon for %s? " target))
        (condition-case err
            (let ((result (beads-daemons--stop target)))
              (if (oref result stopped)
                  (message "Stopped daemon for %s (PID %s)"
                           (or (oref result workspace) target)
                           (or (oref result pid) "?"))
                (message "Failed to stop daemon")))
          (error
           (message "Error: %s" (error-message-string err))))))))

(transient-define-suffix beads-daemons--do-restart ()
  "Restart a specific daemon."
  :key "r"
  :description "Restart daemon"
  (interactive)
  (let* ((args (transient-args 'beads-daemons))
         (target (transient-arg-value "--target=" args)))
    (if (or (null target) (string-empty-p target))
        (user-error "Target is required (use -t to set)")
      (when (y-or-n-p (format "Restart daemon for %s? " target))
        (condition-case err
            (let ((result (beads-daemons--restart target)))
              (message "Restarted daemon for %s"
                       (or (oref result workspace) target)))
          (error
           (message "Error: %s" (error-message-string err))))))))

(transient-define-suffix beads-daemons--do-logs ()
  "View logs for a specific daemon."
  :key "L"
  :description "View logs"
  (interactive)
  (let* ((args (transient-args 'beads-daemons))
         (target (transient-arg-value "--target=" args))
         (lines-str (transient-arg-value "--lines=" args))
         (lines (and lines-str (string-to-number lines-str))))
    (if (or (null target) (string-empty-p target))
        (user-error "Target is required (use -t to set)")
      (condition-case err
          (let* ((result (beads-daemons--logs target (or lines 100)))
                 (content (oref result content))
                 (log-path (oref result log-path))
                 (workspace (oref result workspace))
                 (buffer-name (format "*beads-daemon-log: %s*"
                                      (or workspace target))))
            (with-current-buffer (get-buffer-create buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (or content "No log content"))
                (goto-char (point-min)))
              (special-mode)
              (setq-local header-line-format
                          (format " Log: %s" (or log-path "?"))))
            (switch-to-buffer buffer-name))
        (error
         (message "Error: %s" (error-message-string err)))))))

(transient-define-suffix beads-daemons--do-killall ()
  "Kill all running daemons."
  :key "K"
  :description "Kill all daemons"
  (interactive)
  (when (y-or-n-p "Kill ALL running daemons? ")
    (let ((force beads-daemons--force))
      (condition-case err
          (let ((result (beads-daemons--killall force)))
            (message "Stopped: %d, Failed: %d"
                     (or (oref result stopped) 0)
                     (or (oref result failed) 0)))
        (error
         (message "Error: %s" (error-message-string err)))))))

;;; ============================================================
;;; Transient Menu
;;; ============================================================

(defun beads-daemons--format-header ()
  "Format header showing daemon count for transient menu."
  (condition-case nil
      (let ((daemons (beads-daemons--list)))
        (concat
         (propertize "Daemons: " 'face 'bold)
         (propertize (number-to-string (length (or daemons nil)))
                     'face 'font-lock-constant-face)
         (propertize " running" 'face 'shadow)))
    (error
     (concat
      (propertize "Daemons: " 'face 'bold)
      (propertize "?" 'face 'shadow)))))

;;;###autoload (autoload 'beads-daemons "beads-daemons" nil t)
(transient-define-prefix beads-daemons ()
  "Manage all bd daemons across repositories.

This menu provides system-wide daemon management, unlike
`beads-daemon' which manages only the current project's daemon."
  [:description
   (lambda () (beads-daemons--format-header))]
  ["Options"
   (beads-daemons--infix-target)
   (beads-daemons--infix-lines)
   (beads-daemons--infix-force)]
  ["Browse"
   (beads-daemons--list-all)
   (beads-daemons--show-health)]
  ["Actions"
   (beads-daemons--do-stop)
   (beads-daemons--do-restart)
   (beads-daemons--do-logs)
   (beads-daemons--do-killall)]
  ["Other"
   ("q" "Quit" transient-quit-one)])

(provide 'beads-daemons)
;;; beads-daemons.el ends here
