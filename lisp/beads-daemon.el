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
;;; Customization
;;; ============================================================

(defgroup beads-daemon nil
  "Daemon management settings."
  :group 'beads
  :prefix "beads-daemon-")

(defcustom beads-daemon-status-auto-refresh nil
  "If non-nil, automatically refresh daemon status buffer.
When set to a number, refresh every N seconds."
  :type '(choice (const :tag "No auto-refresh" nil)
                 (integer :tag "Refresh interval (seconds)"))
  :group 'beads-daemon)

;;; ============================================================
;;; Faces
;;; ============================================================

(defface beads-daemon-running-face
  '((t :inherit success :weight bold))
  "Face for running daemon state."
  :group 'beads-daemon)

(defface beads-daemon-stopped-face
  '((t :inherit error :weight bold))
  "Face for stopped daemon state."
  :group 'beads-daemon)

(defface beads-daemon-healthy-face
  '((t :inherit success))
  "Face for healthy daemon status."
  :group 'beads-daemon)

(defface beads-daemon-unhealthy-face
  '((t :inherit error))
  "Face for unhealthy daemon status."
  :group 'beads-daemon)

(defface beads-daemon-section-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headers in daemon status buffer."
  :group 'beads-daemon)

(defface beads-daemon-label-face
  '((t :inherit font-lock-constant-face))
  "Face for labels in daemon status buffer."
  :group 'beads-daemon)

(defface beads-daemon-value-face
  '((t :inherit default))
  "Face for values in daemon status buffer."
  :group 'beads-daemon)

(defface beads-daemon-operation-face
  '((t :inherit font-lock-function-name-face))
  "Face for operation names in metrics table."
  :group 'beads-daemon)

(defface beads-daemon-error-count-face
  '((t :inherit error))
  "Face for non-zero error counts."
  :group 'beads-daemon)

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
;;; Status Buffer Mode
;;; ============================================================

(defvar-local beads-daemon-status--data nil
  "Cached daemon data for current status buffer.
An alist with keys `status', `health', and `metrics'.")

(defvar-local beads-daemon-status--refresh-timer nil
  "Timer for auto-refresh in daemon status buffer.")

(defvar beads-daemon-status-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Refresh/quit
    (define-key map (kbd "g") #'beads-daemon-status-refresh)
    (define-key map (kbd "q") #'quit-window)
    ;; Actions
    (define-key map (kbd "s") #'beads-daemon-start)
    (define-key map (kbd "S") #'beads-daemon-status--stop)
    (define-key map (kbd "r") #'beads-daemon-status--restart)
    ;; Log viewing
    (define-key map (kbd "l") #'beads-daemon-status--view-log)
    (define-key map (kbd "L") #'beads-daemon-status--tail-log)
    ;; Navigation
    (define-key map (kbd "n") #'beads-daemon-status-next-section)
    (define-key map (kbd "p") #'beads-daemon-status-previous-section)
    ;; Help
    (define-key map (kbd "?") #'beads-daemon-status-help)
    map)
  "Keymap for `beads-daemon-status-mode'.")

(define-derived-mode beads-daemon-status-mode special-mode "Beads-Daemon"
  "Major mode for displaying daemon status information.

\\{beads-daemon-status-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  ;; Set up auto-refresh timer if configured
  (when (and beads-daemon-status-auto-refresh
             (numberp beads-daemon-status-auto-refresh))
    (setq beads-daemon-status--refresh-timer
          (run-with-timer beads-daemon-status-auto-refresh
                          beads-daemon-status-auto-refresh
                          #'beads-daemon-status--auto-refresh
                          (current-buffer))))
  ;; Clean up timer on buffer kill
  (add-hook 'kill-buffer-hook #'beads-daemon-status--cleanup nil t))

(defun beads-daemon-status--cleanup ()
  "Clean up resources when daemon status buffer is killed."
  (when beads-daemon-status--refresh-timer
    (cancel-timer beads-daemon-status--refresh-timer)
    (setq beads-daemon-status--refresh-timer nil)))

(defun beads-daemon-status--auto-refresh (buffer)
  "Auto-refresh daemon status in BUFFER if still visible."
  (when (and (buffer-live-p buffer)
             (get-buffer-window buffer))
    (with-current-buffer buffer
      (beads-daemon-status-refresh))))

;;; ============================================================
;;; Status Buffer Rendering
;;; ============================================================

(defun beads-daemon-status--insert-header (label value &optional value-face)
  "Insert a header line with LABEL and VALUE.
Optional VALUE-FACE overrides the default face for VALUE."
  (insert (propertize (format "%-16s" label) 'face 'beads-daemon-label-face))
  (when value
    (insert (propertize (format "%s" value)
                        'face (or value-face 'beads-daemon-value-face))))
  (insert "\n"))

(defun beads-daemon-status--insert-section-header (title)
  "Insert a section header with TITLE."
  (insert "\n")
  (insert (propertize title 'face 'beads-daemon-section-header-face))
  (insert "\n")
  (insert (propertize (make-string (length title) ?─)
                      'face 'beads-daemon-section-header-face))
  (insert "\n"))

(defun beads-daemon-status--format-state (status health)
  "Format daemon state from STATUS and HEALTH data.
Returns a propertized string showing running state and health."
  (if (and status (oref status running))
      (let* ((health-status (and health (oref health status)))
             (state-text "Running")
             (health-text (if (equal health-status "healthy")
                              " (healthy)"
                            (format " (%s)" (or health-status "unknown")))))
        (concat
         (propertize state-text 'face 'beads-daemon-running-face)
         (propertize health-text
                     'face (if (equal health-status "healthy")
                               'beads-daemon-healthy-face
                             'beads-daemon-unhealthy-face))))
    (propertize "Stopped" 'face 'beads-daemon-stopped-face)))

(defun beads-daemon-status--render-status-section (status health)
  "Render the status section from STATUS and HEALTH data."
  (beads-daemon-status--insert-section-header "Status")
  (beads-daemon-status--insert-header
   "State:"
   (beads-daemon-status--format-state status health))
  (when (and status (oref status running))
    (beads-daemon-status--insert-header
     "PID:"
     (number-to-string (or (oref status pid) 0)))
    (beads-daemon-status--insert-header
     "Started:"
     (or (oref status started) "N/A"))
    (when health
      (beads-daemon-status--insert-header
       "Uptime:"
       (beads-daemon--format-uptime (oref health uptime-seconds))))))

(defun beads-daemon-status--render-config-section (status health)
  "Render the configuration section from STATUS and HEALTH data."
  (beads-daemon-status--insert-section-header "Configuration")
  (when status
    (let ((log-path (oref status log-path)))
      (beads-daemon-status--insert-header "Log file:" nil)
      (when log-path
        ;; Make log path clickable
        (delete-char -1)  ; Remove newline
        (insert " ")
        (insert-text-button
         log-path
         'action (lambda (_btn) (view-file log-path))
         'follow-link t
         'help-echo "Click to view log file")
        (insert "\n"))))
  (when health
    (beads-daemon-status--insert-header
     "Version:"
     (or (oref health version) "N/A"))))

(defun beads-daemon-status--render-health-section (health)
  "Render the health section from HEALTH data."
  (beads-daemon-status--insert-section-header "Health")
  (if (null health)
      (insert (propertize "  No health data available\n" 'face 'shadow))
    (beads-daemon-status--insert-header
     "DB Response:"
     (format "%.2f ms" (or (oref health db-response-ms) 0)))
    (beads-daemon-status--insert-header
     "Connections:"
     (format "%d / %d (active/max)"
             (or (oref health active-connections) 0)
             (or (oref health max-connections) 0)))
    (beads-daemon-status--insert-header
     "Memory:"
     (format "%s allocated"
             (beads-daemon--format-bytes (oref health memory-alloc-mb))))))

(defun beads-daemon-status--render-operations-table (metrics)
  "Render operations table from METRICS data."
  (beads-daemon-status--insert-section-header "Operations")
  (if (or (null metrics) (null (oref metrics operations)))
      (insert (propertize "  No operation data available\n" 'face 'shadow))
    (let ((ops (oref metrics operations)))
      ;; Table header
      (insert (propertize
               (format "  %-14s %7s %7s %7s %9s %9s\n"
                       "Operation" "Count" "Success" "Errors" "Avg ms" "P99 ms")
               'face 'bold))
      (insert (propertize
               (format "  %s\n" (make-string 62 ?─))
               'face 'shadow))
      ;; Table rows
      (dolist (op ops)
        (let* ((name (oref op operation))
               (total (or (oref op total-count) 0))
               (success (or (oref op success-count) 0))
               (errors (or (oref op error-count) 0))
               (latency (oref op latency))
               (avg-ms (or (alist-get 'avg_ms latency) 0))
               (p99-ms (or (alist-get 'p99_ms latency) 0)))
          (insert "  ")
          (insert (propertize (format "%-14s" name)
                              'face 'beads-daemon-operation-face))
          (insert (format " %7d %7d " total success))
          (insert (propertize (format "%7d" errors)
                              'face (if (> errors 0)
                                        'beads-daemon-error-count-face
                                      'default)))
          (insert (format " %9.2f %9.2f\n" avg-ms p99-ms))))
      ;; Summary line
      (insert (propertize
               (format "  %s\n" (make-string 62 ?─))
               'face 'shadow))
      (let ((total-ops (seq-reduce (lambda (acc op)
                                     (+ acc (or (oref op total-count) 0)))
                                   ops 0))
            (total-errors (seq-reduce (lambda (acc op)
                                        (+ acc (or (oref op error-count) 0)))
                                      ops 0)))
        (insert (format "  Total: %d operations, %d errors\n"
                        total-ops total-errors))))))

(defun beads-daemon-status--render-resources-section (metrics)
  "Render resources section from METRICS data."
  (beads-daemon-status--insert-section-header "Resources")
  (if (null metrics)
      (insert (propertize "  No resource data available\n" 'face 'shadow))
    (beads-daemon-status--insert-header
     "Memory:"
     (format "%s allocated, %s system"
             (beads-daemon--format-bytes (oref metrics memory-alloc-mb))
             (beads-daemon--format-bytes (oref metrics memory-sys-mb))))
    (beads-daemon-status--insert-header
     "Goroutines:"
     (number-to-string (or (oref metrics goroutine-count) 0)))
    (beads-daemon-status--insert-header
     "Connections:"
     (format "%d total, %d active, %d rejected"
             (or (oref metrics total-connections) 0)
             (or (oref metrics active-connections) 0)
             (or (oref metrics rejected-connections) 0)))))

(defun beads-daemon-status--render-buffer (status health metrics)
  "Render the daemon status buffer with STATUS, HEALTH, and METRICS."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Title
    (insert (propertize "Daemon Status" 'face '(:inherit bold :height 1.3)))
    (insert "\n")
    (insert (propertize (make-string 13 ?═) 'face 'beads-daemon-section-header-face))
    (insert "\n")
    ;; Sections
    (beads-daemon-status--render-status-section status health)
    (when (and status (oref status running))
      (beads-daemon-status--render-config-section status health)
      (beads-daemon-status--render-health-section health)
      (beads-daemon-status--render-operations-table metrics)
      (beads-daemon-status--render-resources-section metrics))
    ;; Footer
    (insert "\n")
    (insert (propertize "Press 'g' to refresh, 'q' to quit, '?' for help"
                        'face 'shadow))
    (insert "\n")
    (goto-char (point-min))))

;;; ============================================================
;;; Status Buffer Commands
;;; ============================================================

(defun beads-daemon-status-refresh ()
  "Refresh the daemon status buffer."
  (interactive)
  (unless (derived-mode-p 'beads-daemon-status-mode)
    (user-error "Not in a daemon status buffer"))
  (let ((status (beads-daemon--get-status))
        (health (beads-daemon--get-health))
        (metrics (beads-daemon--get-metrics)))
    (setq beads-daemon-status--data
          `((status . ,status)
            (health . ,health)
            (metrics . ,metrics)))
    (beads-daemon-status--render-buffer status health metrics)
    (message "Daemon status refreshed")))

(defun beads-daemon-status--stop ()
  "Stop the daemon from status buffer."
  (interactive)
  (if (not (beads-daemon--running-p))
      (message "Daemon is not running")
    (when (y-or-n-p "Stop the daemon? ")
      (condition-case err
          (progn
            (beads-command-execute (beads-daemon-command-stop))
            (message "Daemon stopped")
            (beads-daemon-status-refresh))
        (error
         (message "Failed to stop daemon: %s" (error-message-string err)))))))

(defun beads-daemon-status--restart ()
  "Restart the daemon from status buffer."
  (interactive)
  (condition-case err
      (progn
        (when (beads-daemon--running-p)
          (beads-command-execute (beads-daemon-command-stop))
          (sit-for 0.5))
        (beads-command-execute (beads-daemon-command-start))
        (message "Daemon restarted")
        (sit-for 0.5)
        (beads-daemon-status-refresh))
    (error
     (message "Failed to restart daemon: %s" (error-message-string err)))))

(defun beads-daemon-status--view-log ()
  "View daemon log file from status buffer."
  (interactive)
  (let ((log-path (beads-daemon--get-log-path)))
    (if (null log-path)
        (message "Log path not available")
      (if (file-exists-p log-path)
          (view-file log-path)
        (message "Log file does not exist: %s" log-path)))))

(defun beads-daemon-status--tail-log ()
  "Tail daemon log file from status buffer."
  (interactive)
  (let ((log-path (beads-daemon--get-log-path)))
    (if (null log-path)
        (message "Log path not available")
      (if (file-exists-p log-path)
          (progn
            (find-file log-path)
            (goto-char (point-max))
            (auto-revert-tail-mode 1)
            (message "Following log file"))
        (message "Log file does not exist: %s" log-path)))))

(defun beads-daemon-status-next-section ()
  "Move to the next section in the status buffer."
  (interactive)
  (let ((section-regexp "^[A-Z][a-zA-Z ]+\n─+$"))
    (if (re-search-forward section-regexp nil t)
        (progn
          (forward-line 1)
          (recenter-top-bottom 0))
      (message "No next section"))))

(defun beads-daemon-status-previous-section ()
  "Move to the previous section in the status buffer."
  (interactive)
  (let ((section-regexp "^[A-Z][a-zA-Z ]+\n─+$"))
    (if (re-search-backward section-regexp nil t)
        (progn
          (forward-line 1)
          (recenter-top-bottom 0))
      (message "No previous section"))))

(defun beads-daemon-status-help ()
  "Show help for daemon status mode."
  (interactive)
  (message "g:refresh  s:start  S:stop  r:restart  l:log  L:tail  n/p:navigate  q:quit"))

;;;###autoload
(defun beads-daemon-dashboard ()
  "Display daemon status in a dedicated buffer.
Shows comprehensive daemon information including status, health,
metrics, and operations."
  (interactive)
  (let* ((buffer-name "*beads-daemon*")
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (beads-daemon-status-mode)
      (beads-daemon-status-refresh))
    (switch-to-buffer buffer)))

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

(transient-define-suffix beads-daemon--open-status-buffer ()
  "Open the daemon status buffer."
  :key "d"
  :description "Dashboard"
  (interactive)
  (beads-daemon-dashboard))

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
   (beads-daemon--open-status-buffer)
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
