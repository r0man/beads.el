;;; beads-modeline.el --- Mode-line indicator for Beads daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides a non-intrusive mode-line indicator showing
;; the daemon status.  The indicator displays:
;;   - (green) running
;;   - (gray) stopped
;;   (yellow) degraded
;;
;; Clicking the indicator opens the daemon transient menu.  The
;; indicator is only shown in beads-related buffers.
;;
;; The daemon status is cached with a configurable refresh interval
;; to avoid excessive polling.
;;
;; Inspired by eglot/lsp-mode mode-line patterns.
;;
;; Usage:
;;   (beads-modeline-mode 1)    ; Enable globally
;;   (beads-modeline-mode -1)   ; Disable globally
;;
;; Or add to individual buffers:
;;   (add-hook 'beads-list-mode-hook #'beads-modeline-setup)

;;; Code:

(require 'cl-lib)
(require 'beads-command)  ; For daemon status/health EIEIO classes
(require 'beads-daemon)   ; For beads-daemon--get-status/health

;; Forward declarations
(defvar beads-executable)
(declare-function beads-daemon "beads-daemon")

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup beads-modeline nil
  "Mode-line indicator for Beads daemon status."
  :group 'beads
  :prefix "beads-modeline-")

(defcustom beads-modeline-refresh-interval 30
  "Seconds between daemon status refreshes.
Set to nil to disable automatic refresh (status only updates on
mode-line redisplay)."
  :type '(choice (integer :tag "Refresh interval (seconds)")
                 (const :tag "No automatic refresh" nil))
  :group 'beads-modeline)

(defcustom beads-modeline-position 'after-mode-name
  "Position of the daemon indicator in the mode-line.
Possible values:
- `after-mode-name': After the major mode name
- `end': At the end of the mode-line"
  :type '(choice (const :tag "After mode name" after-mode-name)
                 (const :tag "End of mode-line" end))
  :group 'beads-modeline)

(defcustom beads-modeline-prefix " bd"
  "Prefix string shown before the status indicator.
Set to empty string to show only the status icon."
  :type 'string
  :group 'beads-modeline)

;;; ============================================================
;;; Faces
;;; ============================================================

(defface beads-modeline-running
  '((t :inherit success))
  "Face for running daemon indicator."
  :group 'beads-modeline)

(defface beads-modeline-stopped
  '((t :inherit shadow))
  "Face for stopped daemon indicator."
  :group 'beads-modeline)

(defface beads-modeline-degraded
  '((t :inherit warning))
  "Face for degraded daemon indicator."
  :group 'beads-modeline)

(defface beads-modeline-prefix
  '((t :inherit mode-line))
  "Face for the mode-line prefix."
  :group 'beads-modeline)

;;; ============================================================
;;; Status Cache
;;; ============================================================

(defvar beads-modeline--status-cache nil
  "Cached daemon status.
A cons cell (TIMESTAMP . STATUS) where STATUS is one of:
`running', `stopped', or `degraded'.")

(defvar beads-modeline--refresh-timer nil
  "Timer for automatic status refresh.")

(defun beads-modeline--cache-valid-p ()
  "Return non-nil if the status cache is still valid."
  (and beads-modeline--status-cache
       beads-modeline-refresh-interval
       (< (- (float-time) (car beads-modeline--status-cache))
          beads-modeline-refresh-interval)))

(defun beads-modeline--get-status ()
  "Get the current daemon status, using cache if valid.
Returns one of: `running', `stopped', `degraded', or `unknown'."
  (if (beads-modeline--cache-valid-p)
      (cdr beads-modeline--status-cache)
    (beads-modeline--refresh-status)))

(defun beads-modeline--refresh-status ()
  "Refresh the daemon status cache.
Returns one of: `running', `stopped', `degraded', or `unknown'."
  (let ((status (condition-case nil
                    (beads-modeline--fetch-status)
                  (error 'unknown))))
    (setq beads-modeline--status-cache (cons (float-time) status))
    status))

(defun beads-modeline--fetch-status ()
  "Fetch daemon status from bd.
Returns one of: `running', `stopped', `degraded'."
  (let ((daemon-status (beads-daemon--get-status)))
    (if (or (null daemon-status)
            (not (oref daemon-status running)))
        'stopped
      ;; Check health
      (let ((health (beads-daemon--get-health)))
        (cond
         ((null health) 'degraded)
         ((not (equal (oref health status) "healthy")) 'degraded)
         (t 'running))))))

(defun beads-modeline-invalidate-cache ()
  "Invalidate the status cache, forcing a refresh on next check."
  (setq beads-modeline--status-cache nil))

;;; ============================================================
;;; Mode-line Segment
;;; ============================================================

(defvar beads-modeline--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'beads-modeline--click)
    (define-key map [mode-line mouse-3] #'beads-modeline--refresh-click)
    map)
  "Keymap for the mode-line indicator.")

(defun beads-modeline--click (_event)
  "Handle mouse click on mode-line indicator.
Opens the daemon transient menu."
  (interactive "e")
  (call-interactively #'beads-daemon))

(defun beads-modeline--refresh-click (_event)
  "Handle right-click on mode-line indicator.
Refreshes the status cache."
  (interactive "e")
  (beads-modeline-invalidate-cache)
  (beads-modeline--refresh-status)
  (force-mode-line-update)
  (message "Daemon status refreshed"))

(defun beads-modeline--status-icon (status)
  "Return the icon string for STATUS."
  (pcase status
    ('running (propertize "\u25cf" 'face 'beads-modeline-running))  ; ●
    ('stopped (propertize "\u25cb" 'face 'beads-modeline-stopped))  ; ○
    ('degraded (propertize "\u26a0" 'face 'beads-modeline-degraded)) ; ⚠
    (_ (propertize "?" 'face 'beads-modeline-stopped))))

(defun beads-modeline--help-echo (status)
  "Return help-echo text for STATUS."
  (format "Beads daemon: %s\nmouse-1: Open daemon menu\nmouse-3: Refresh status"
          (pcase status
            ('running "Running")
            ('stopped "Stopped")
            ('degraded "Degraded")
            (_ "Unknown"))))

(defun beads-modeline-segment ()
  "Return the mode-line segment for daemon status.
Returns nil if not in a beads-related buffer."
  (when (beads-modeline--in-beads-buffer-p)
    (let ((status (beads-modeline--get-status)))
      (propertize
       (concat (propertize beads-modeline-prefix 'face 'beads-modeline-prefix)
               " "
               (beads-modeline--status-icon status))
       'help-echo (beads-modeline--help-echo status)
       'mouse-face 'mode-line-highlight
       'local-map beads-modeline--keymap))))

(defun beads-modeline--in-beads-buffer-p ()
  "Return non-nil if current buffer is a beads-related buffer."
  (or (derived-mode-p 'beads-list-mode)
      (derived-mode-p 'beads-show-mode)
      (derived-mode-p 'beads-stats-mode)
      (derived-mode-p 'beads-graph-mode)
      (derived-mode-p 'beads-daemon-status-mode)
      (derived-mode-p 'beads-daemons-list-mode)
      (derived-mode-p 'beads-label-list-all-mode)
      ;; Also match buffer names
      (string-match-p "\\*beads-" (buffer-name))))

;;; ============================================================
;;; Minor Mode
;;; ============================================================

(defvar beads-modeline--mode-line-construct
  '(:eval (beads-modeline-segment))
  "Mode-line construct for the daemon indicator.")

(defun beads-modeline--install ()
  "Install the mode-line segment."
  (unless (memq beads-modeline--mode-line-construct mode-line-misc-info)
    (pcase beads-modeline-position
      ('end
       (setq-default mode-line-misc-info
                     (append mode-line-misc-info
                             (list beads-modeline--mode-line-construct))))
      ('after-mode-name
       (setq-default mode-line-misc-info
                     (cons beads-modeline--mode-line-construct
                           mode-line-misc-info)))))
  ;; Start refresh timer if configured
  (beads-modeline--start-timer))

(defun beads-modeline--uninstall ()
  "Uninstall the mode-line segment."
  (setq-default mode-line-misc-info
                (delq beads-modeline--mode-line-construct
                      (default-value 'mode-line-misc-info)))
  ;; Stop refresh timer
  (beads-modeline--stop-timer)
  ;; Clear cache
  (setq beads-modeline--status-cache nil))

(defun beads-modeline--start-timer ()
  "Start the automatic refresh timer."
  (beads-modeline--stop-timer)
  (when beads-modeline-refresh-interval
    (setq beads-modeline--refresh-timer
          (run-with-timer beads-modeline-refresh-interval
                          beads-modeline-refresh-interval
                          #'beads-modeline--timer-refresh))))

(defun beads-modeline--stop-timer ()
  "Stop the automatic refresh timer."
  (when beads-modeline--refresh-timer
    (cancel-timer beads-modeline--refresh-timer)
    (setq beads-modeline--refresh-timer nil)))

(defun beads-modeline--timer-refresh ()
  "Refresh status from timer callback."
  ;; Only refresh if there are beads buffers visible
  (when (cl-some (lambda (win)
                   (with-current-buffer (window-buffer win)
                     (beads-modeline--in-beads-buffer-p)))
                 (window-list))
    (beads-modeline--refresh-status)
    (force-mode-line-update t)))

;;;###autoload
(define-minor-mode beads-modeline-mode
  "Toggle daemon status indicator in the mode-line.
When enabled, beads-related buffers show a small indicator
displaying the daemon status: running, stopped, or degraded.
Clicking the indicator opens the daemon transient menu."
  :global t
  :lighter nil
  :group 'beads-modeline
  (if beads-modeline-mode
      (beads-modeline--install)
    (beads-modeline--uninstall)))

;;;###autoload
(defun beads-modeline-setup ()
  "Set up mode-line indicator for the current buffer.
Call this from mode hooks to enable the indicator for specific modes."
  (interactive)
  (unless beads-modeline-mode
    (beads-modeline-mode 1)))

(provide 'beads-modeline)
;;; beads-modeline.el ends here
