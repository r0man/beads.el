;;; beads-daemon.el --- Daemon management for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

;;; Commentary:

;; Provides transient menu interface for managing the Beads daemon.
;;
;; The bd daemon command runs a background sync daemon that
;; automatically synchronizes issues with git remote at configurable
;; intervals.
;;
;; Features:
;; - Start daemon with auto-commit/push capabilities
;; - Stop running daemon
;; - Check daemon status
;; - Configurable sync interval
;; - Custom log file path
;;
;; All commands follow the patterns established in beads-misc.el.

;;; Code:

(require 'beads)
(require 'transient)

;;; ============================================================
;;; bd daemon
;;; ============================================================

;;; Transient State Variables

(defvar beads-daemon--auto-commit nil
  "Whether to enable auto-commit.")

(defvar beads-daemon--auto-push nil
  "Whether to enable auto-push.")

(defvar beads-daemon--interval nil
  "Sync interval duration (e.g., '5m', '10m').")

(defvar beads-daemon--log-path nil
  "Custom log file path.")

;;; Utility Functions

(defun beads-daemon--reset-state ()
  "Reset daemon transient state."
  (setq beads-daemon--auto-commit nil
        beads-daemon--auto-push nil
        beads-daemon--interval nil
        beads-daemon--log-path nil))

(defun beads-daemon--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-daemon--get-default-log-path ()
  "Get default log path (.beads/daemon.log)."
  (when-let ((beads-dir (beads--find-beads-dir)))
    (expand-file-name "daemon.log" beads-dir)))

;;; Infix Commands

(transient-define-infix beads-daemon--infix-auto-commit ()
  "Toggle auto-commit flag."
  :class 'transient-switch
  :description "Auto-commit changes (--auto-commit)"
  :key "c"
  :argument "--auto-commit"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-daemon--auto-commit
                  (not beads-daemon--auto-commit))
            beads-daemon--auto-commit))

(transient-define-infix beads-daemon--infix-auto-push ()
  "Toggle auto-push flag."
  :class 'transient-switch
  :description "Auto-push commits (--auto-push)"
  :key "p"
  :argument "--auto-push"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-daemon--auto-push
                  (not beads-daemon--auto-push))
            beads-daemon--auto-push))

(transient-define-infix beads-daemon--infix-interval ()
  "Set the sync interval."
  :class 'transient-option
  :description (lambda ()
                 (concat "Sync interval (--interval)"
                         (beads-daemon--format-value
                          beads-daemon--interval)))
  :key "i"
  :argument "interval="
  :prompt "Sync interval (e.g., 5m, 10m): "
  :reader (lambda (_prompt _initial-input _history)
            (let ((interval (read-string
                             "Sync interval (e.g., 5m, 10m): "
                             beads-daemon--interval)))
              (setq beads-daemon--interval interval)
              interval)))

(transient-define-infix beads-daemon--infix-log ()
  "Set the log file path."
  :class 'transient-option
  :description (lambda ()
                 (concat "Log file (--log)"
                         (beads-daemon--format-value
                          (or beads-daemon--log-path
                              (beads-daemon--get-default-log-path)))))
  :key "l"
  :argument "log="
  :prompt "Log file path: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((path (read-file-name
                         "Log file path: "
                         nil
                         (or beads-daemon--log-path
                             (beads-daemon--get-default-log-path)))))
              (setq beads-daemon--log-path path)
              path)))

;;; Suffix Commands

(defun beads-daemon--execute-start (auto-commit auto-push interval
                                                 log-path)
  "Execute bd daemon start with flags.
AUTO-COMMIT, AUTO-PUSH, INTERVAL, and LOG-PATH control behavior."
  (condition-case err
      (let ((args (list "daemon")))
        (when auto-commit
          (setq args (append args (list "--auto-commit"))))
        (when auto-push
          (setq args (append args (list "--auto-push"))))
        (when (and interval (not (string-empty-p (string-trim interval))))
          (setq args (append args (list "--interval" interval))))
        (when (and log-path (not (string-empty-p (string-trim log-path))))
          (setq args (append args (list "--log" log-path))))
        ;; Start daemon in background
        (let* ((buffer (generate-new-buffer " *beads-daemon*"))
               (cmd (beads--build-command (car args)))
               ;; Remove --json from daemon command as it doesn't output JSON
               (cmd (seq-remove (lambda (x) (string= x "--json")) cmd))
               ;; Add remaining daemon args
               (cmd (append cmd (cdr args))))
          (let ((proc (apply #'start-file-process
                             "beads-daemon" buffer
                             (car cmd) (cdr cmd))))
            (set-process-sentinel
             proc
             (lambda (process event)
               (when (string-match-p "\\(finished\\|exited\\)" event)
                 (message "Beads daemon stopped"))
               (when (string-match-p "failed" event)
                 (message "Beads daemon failed: %s" event))))
            (message "Beads daemon started (PID: %d)" (process-id proc))
            nil)))
    (error
     (beads--error "Failed to start daemon: %s"
                   (error-message-string err)))))

(defun beads-daemon--execute-stop ()
  "Execute bd daemon --stop to stop running daemon."
  (condition-case err
      (let ((output (with-temp-buffer
                      ;; Use bd daemon --stop without --json
                      (let* ((cmd (list beads-executable "daemon" "--stop"))
                             (exit-code (apply #'call-process
                                               (car cmd) nil t nil
                                               (cdr cmd))))
                        (unless (zerop exit-code)
                          (error "Stop failed: %s" (buffer-string)))
                        (buffer-string)))))
        (message "Daemon stopped: %s" (string-trim output))
        nil)
    (error
     (beads--error "Failed to stop daemon: %s"
                   (error-message-string err)))))

(defun beads-daemon--execute-status ()
  "Execute bd daemon --status and display in buffer."
  (condition-case err
      (let ((output (with-temp-buffer
                      ;; Use bd daemon --status without --json
                      (let* ((cmd (list beads-executable "daemon" "--status"))
                             (exit-code (apply #'call-process
                                               (car cmd) nil t nil
                                               (cdr cmd))))
                        (unless (zerop exit-code)
                          (error "Status check failed: %s" (buffer-string)))
                        (buffer-string))))
            (buf (get-buffer-create "*beads-daemon-status*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output)
            (goto-char (point-min))
            (special-mode)
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "g")
                           (lambda ()
                             (interactive)
                             (beads-daemon--execute-status)))))
        (display-buffer buf)
        (message "Daemon status")
        nil)
    (error
     (beads--error "Failed to get daemon status: %s"
                   (error-message-string err)))))

(defun beads-daemon--view-log ()
  "View daemon log file in buffer."
  (condition-case err
      (let* ((log-path (or beads-daemon--log-path
                           (beads-daemon--get-default-log-path)))
             (buf (get-buffer-create "*beads-daemon-log*")))
        (unless log-path
          (user-error "Cannot determine log file path"))
        (unless (file-exists-p log-path)
          (user-error "Log file does not exist: %s" log-path))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-file-contents log-path)
            (goto-char (point-max))
            (special-mode)
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "g")
                           (lambda ()
                             (interactive)
                             (beads-daemon--view-log)))
            (setq header-line-format
                  (format "Daemon Log: %s - Press 'q' to quit, 'g' to refresh"
                          log-path))))
        (display-buffer buf)
        (message "Viewing daemon log: %s" log-path)
        nil)
    (error
     (beads--error "Failed to view log: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-daemon--start-command ()
  "Start the Beads daemon."
  :key "s"
  :description "Start daemon"
  (interactive)
  (beads-daemon--execute-start beads-daemon--auto-commit
                               beads-daemon--auto-push
                               beads-daemon--interval
                               beads-daemon--log-path)
  (beads-daemon--reset-state))

(transient-define-suffix beads-daemon--stop-command ()
  "Stop the Beads daemon."
  :key "S"
  :description "Stop daemon"
  (interactive)
  (when (y-or-n-p "Stop running daemon? ")
    (beads-daemon--execute-stop)))

(transient-define-suffix beads-daemon--status-command ()
  "Show daemon status."
  :key "t"
  :description "Show status"
  :transient t
  (interactive)
  (beads-daemon--execute-status))

(transient-define-suffix beads-daemon--view-log-command ()
  "View daemon log file."
  :key "L"
  :description "View log file"
  :transient t
  (interactive)
  (beads-daemon--view-log))

(transient-define-suffix beads-daemon--reset ()
  "Reset all daemon parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-daemon--reset-state)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-daemon "beads-daemon" nil t)
(transient-define-prefix beads-daemon ()
  "Manage the Beads sync daemon.

This transient menu provides an interface for managing the Beads
daemon, which automatically synchronizes issues with git remote
at configurable intervals."
  :value (lambda () nil)
  (interactive)
  (beads-check-executable)
  ["Daemon Options"
   ["Sync Behavior"
    (beads-daemon--infix-auto-commit)
    (beads-daemon--infix-auto-push)
    (beads-daemon--infix-interval)]
   ["Configuration"
    (beads-daemon--infix-log)]]
  ["Actions"
   ["Control"
    ("s" "Start daemon" beads-daemon--start-command)
    ("S" "Stop daemon" beads-daemon--stop-command)]
   ["View"
    ("t" "Show status" beads-daemon--status-command)
    ("L" "View log" beads-daemon--view-log-command)]
   ["Other"
    ("R" "Reset fields" beads-daemon--reset)
    ("q" "Quit" transient-quit-one)]])

;;; Standalone Commands

;;;###autoload
(defun beads-daemon-start ()
  "Start the Beads daemon interactively."
  (interactive)
  (beads-check-executable)
  (call-interactively 'beads-daemon))

;;;###autoload
(defun beads-daemon-stop ()
  "Stop the Beads daemon."
  (interactive)
  (beads-check-executable)
  (beads-daemon--execute-stop))

;;;###autoload
(defun beads-daemon-status ()
  "Show Beads daemon status."
  (interactive)
  (beads-check-executable)
  (beads-daemon--execute-status))

;;; Footer

(provide 'beads-daemon)
;;; beads-daemon.el ends here
