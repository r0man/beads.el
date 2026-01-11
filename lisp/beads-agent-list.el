;;; beads-agent-list.el --- Tabulated list mode for Beads agent sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides a tabulated-list-mode buffer for displaying
;; AI agent sessions in beads.el.  It replaces the simple help-window
;; display from `beads-agent-list-sessions' with a proper tabulated
;; list including sorting, keybindings, and visual status indicators.
;;
;; Entry Point:
;;   M-x beads-agent-list    ; Open *beads-agents* buffer
;;
;; Key bindings:
;;   RET/j - Jump to agent buffer
;;   i     - Show issue details
;;   s     - Stop session at point
;;   S     - Stop all sessions
;;   r     - Restart session
;;   g     - Refresh buffer
;;   q     - Quit buffer
;;   c     - Cleanup stale sessions
;;   n/p   - Navigate next/previous
;;   w     - Copy session ID
;;   W     - Copy issue ID
;;   C-c C-s - Sesman commands

;;; Code:

(require 'beads)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-agent-backend)
(require 'beads-sesman)

;;; Forward Declarations

(declare-function beads-agent-start "beads-agent")
(declare-function beads-agent-stop "beads-agent")
(declare-function beads-agent-jump "beads-agent")
(declare-function beads-agent-cleanup-stale-sessions "beads-agent")
(declare-function beads-show "beads-show")

;;; Customization

(defgroup beads-agent-list nil
  "Tabulated list display for Beads agent sessions."
  :group 'beads-agent
  :prefix "beads-agent-list-")

(defcustom beads-agent-list-issue-width 16
  "Width of Issue ID column in agent list."
  :type 'integer
  :group 'beads-agent-list)

(defcustom beads-agent-list-title-width 30
  "Width of Title column in agent list."
  :type 'integer
  :group 'beads-agent-list)

(defcustom beads-agent-list-backend-width 15
  "Width of Backend column in agent list."
  :type 'integer
  :group 'beads-agent-list)

(defcustom beads-agent-list-status-width 10
  "Width of Status column in agent list."
  :type 'integer
  :group 'beads-agent-list)

(defcustom beads-agent-list-duration-width 10
  "Width of Duration column in agent list."
  :type 'integer
  :group 'beads-agent-list)

(defcustom beads-agent-list-directory-width 30
  "Width of Directory column in agent list."
  :type 'integer
  :group 'beads-agent-list)

;;; Faces

(defface beads-agent-list-running
  '((t :inherit success :weight bold))
  "Face for running agent status indicator."
  :group 'beads-agent-list)

(defface beads-agent-list-stale
  '((t :inherit shadow))
  "Face for stale agent status indicator."
  :group 'beads-agent-list)

(defface beads-agent-list-finished
  '((t :inherit success))
  "Face for finished agent status indicator."
  :group 'beads-agent-list)

(defface beads-agent-list-failed
  '((t :inherit error))
  "Face for failed agent status indicator."
  :group 'beads-agent-list)

;;; Variables

(defvar-local beads-agent-list--title-cache nil
  "Hash table mapping issue IDs to titles for the current buffer.")

;;; Utilities

(defun beads-agent-list--status-face (status)
  "Return face for STATUS symbol."
  (pcase status
    ('running 'beads-agent-list-running)
    ('stale 'beads-agent-list-stale)
    ('finished 'beads-agent-list-finished)
    ('failed 'beads-agent-list-failed)
    (_ 'default)))

(defun beads-agent-list--format-status (session)
  "Format status for SESSION with appropriate face."
  (let* ((active (beads-agent--session-active-p session))
         (outcome (beads-agent--get-issue-outcome (oref session issue-id)))
         (status (cond
                  (active 'running)
                  ((eq outcome 'finished) 'finished)
                  ((eq outcome 'failed) 'failed)
                  (t 'stale)))
         (status-str (symbol-name status)))
    (propertize status-str
                'face (beads-agent-list--status-face status)
                'help-echo (format "Session status: %s" status-str))))

(defun beads-agent-list--format-duration (started-at)
  "Format duration since STARTED-AT timestamp as human-readable string."
  (if (or (not started-at) (string-empty-p started-at))
      ""
    (condition-case nil
        (let* ((start-time (date-to-time started-at))
               (now (current-time))
               (diff (time-subtract now start-time))
               (seconds (time-to-seconds diff)))
          (cond
           ((< seconds 60)
            (format "%ds" (floor seconds)))
           ((< seconds 3600)
            (format "%dm" (floor (/ seconds 60))))
           ((< seconds 86400)
            (let* ((hours (floor (/ seconds 3600)))
                   (mins (floor (/ (mod seconds 3600) 60))))
              (if (> mins 0)
                  (format "%dh %dm" hours mins)
                (format "%dh" hours))))
           (t
            (let* ((days (floor (/ seconds 86400)))
                   (hours (floor (/ (mod seconds 86400) 3600))))
              (if (> hours 0)
                  (format "%dd %dh" days hours)
                (format "%dd" days))))))
      (error ""))))

(defun beads-agent-list--format-directory (session)
  "Format working directory for SESSION."
  (let ((dir (or (oref session worktree-dir)
                 (oref session project-dir)
                 "")))
    (abbreviate-file-name dir)))

(defun beads-agent-list--fetch-titles (issue-ids)
  "Fetch titles for ISSUE-IDS and return a hash table mapping ID to title."
  (let ((cache (make-hash-table :test #'equal)))
    (when issue-ids
      (condition-case nil
          (let ((issues (beads-command-show! :issue-ids issue-ids)))
            (dolist (issue (if (listp issues) issues (list issues)))
              (when issue
                (puthash (oref issue id)
                         (or (oref issue title) "")
                         cache))))
        (error nil)))
    cache))

(defun beads-agent-list--get-title (issue-id)
  "Get title for ISSUE-ID from cache or return empty string."
  (if beads-agent-list--title-cache
      (gethash issue-id beads-agent-list--title-cache "")
    ""))

(defun beads-agent-list--session-to-entry (session)
  "Convert SESSION (beads-agent-session object) to tabulated-list entry."
  (let* ((session-id (oref session id))
         (issue-id (oref session issue-id))
         (title (beads-agent-list--get-title issue-id))
         (backend-name (oref session backend-name))
         (status-str (beads-agent-list--format-status session))
         (duration-str (beads-agent-list--format-duration
                        (oref session started-at)))
         (dir-str (beads-agent-list--format-directory session)))
    (list session-id
          (vector issue-id
                  title
                  backend-name
                  status-str
                  duration-str
                  dir-str))))

(defun beads-agent-list--populate-buffer ()
  "Populate current buffer with agent sessions."
  (let* ((sessions (beads-agent--get-all-sessions))
         (issue-ids (mapcar (lambda (s) (oref s issue-id)) sessions)))
    ;; Batch-fetch titles
    (setq beads-agent-list--title-cache
          (beads-agent-list--fetch-titles issue-ids))
    ;; Build entries
    (setq tabulated-list-entries
          (mapcar #'beads-agent-list--session-to-entry sessions))
    (tabulated-list-print t)))

(defun beads-agent-list--current-session-id ()
  "Return the session ID at point, or nil."
  (tabulated-list-get-id))

(defun beads-agent-list--current-issue-id ()
  "Return the issue ID for the session at point, or nil."
  (when-let ((session-id (beads-agent-list--current-session-id)))
    (when-let ((session (beads-agent--get-session session-id)))
      (oref session issue-id))))

;;; Commands

(defun beads-agent-list-refresh ()
  "Refresh the agent list buffer."
  (interactive)
  (beads-agent-list--populate-buffer)
  (message "Refreshed %d session%s"
           (length tabulated-list-entries)
           (if (= (length tabulated-list-entries) 1) "" "s")))

(defun beads-agent-list-jump ()
  "Jump to the agent buffer for session at point."
  (interactive)
  (if-let ((session-id (beads-agent-list--current-session-id)))
      (beads-agent-jump session-id)
    (user-error "No session at point")))

(defun beads-agent-list-show-issue ()
  "Show details for the issue associated with session at point."
  (interactive)
  (if-let ((issue-id (beads-agent-list--current-issue-id)))
      (beads-show issue-id)
    (user-error "No session at point")))

(defun beads-agent-list-stop ()
  "Stop the agent session at point."
  (interactive)
  (if-let ((session-id (beads-agent-list--current-session-id)))
      (progn
        (beads-agent-stop session-id)
        (beads-agent-list-refresh))
    (user-error "No session at point")))

(defun beads-agent-list-stop-all ()
  "Stop all agent sessions."
  (interactive)
  (let ((sessions (beads-agent--get-all-sessions)))
    (if (null sessions)
        (message "No sessions to stop")
      (when (y-or-n-p (format "Stop all %d session(s)? " (length sessions)))
        (dolist (session sessions)
          (condition-case nil
              (beads-agent-stop (oref session id))
            (error nil)))
        (beads-agent-list-refresh)
        (message "Stopped all sessions")))))

(defun beads-agent-list-restart ()
  "Restart the agent session at point.
Stops the current session and starts a new one for the same issue."
  (interactive)
  (if-let ((session-id (beads-agent-list--current-session-id)))
      (let* ((session (beads-agent--get-session session-id))
             (issue-id (and session (oref session issue-id))))
        (if (not issue-id)
            (user-error "Cannot find issue ID for session")
          (beads-agent-stop session-id)
          (beads-agent-start issue-id)
          (run-with-timer 0.5 nil #'beads-agent-list-refresh)))
    (user-error "No session at point")))

(defun beads-agent-list-cleanup ()
  "Clean up stale sessions."
  (interactive)
  (beads-agent-cleanup-stale-sessions)
  (beads-agent-list-refresh))

(defun beads-agent-list-copy-session-id ()
  "Copy the session ID at point to the kill ring."
  (interactive)
  (if-let ((session-id (beads-agent-list--current-session-id)))
      (progn
        (kill-new session-id)
        (message "Copied session ID: %s" session-id))
    (user-error "No session at point")))

(defun beads-agent-list-copy-issue-id ()
  "Copy the issue ID at point to the kill ring."
  (interactive)
  (if-let ((issue-id (beads-agent-list--current-issue-id)))
      (progn
        (kill-new issue-id)
        (message "Copied issue ID: %s" issue-id))
    (user-error "No session at point")))

(defun beads-agent-list-quit ()
  "Quit the agent list buffer."
  (interactive)
  (quit-window t))

(defun beads-agent-list-next ()
  "Move to next session."
  (interactive)
  (forward-line 1))

(defun beads-agent-list-previous ()
  "Move to previous session."
  (interactive)
  (forward-line -1))

;;; Mode Definition

(defvar beads-agent-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; Navigation
    (define-key map (kbd "n") #'beads-agent-list-next)
    (define-key map (kbd "p") #'beads-agent-list-previous)
    (define-key map (kbd "RET") #'beads-agent-list-jump)
    (define-key map (kbd "j") #'beads-agent-list-jump)
    ;; Actions
    (define-key map (kbd "i") #'beads-agent-list-show-issue)
    (define-key map (kbd "s") #'beads-agent-list-stop)
    (define-key map (kbd "S") #'beads-agent-list-stop-all)
    (define-key map (kbd "r") #'beads-agent-list-restart)
    (define-key map (kbd "c") #'beads-agent-list-cleanup)
    ;; Utilities
    (define-key map (kbd "g") #'beads-agent-list-refresh)
    (define-key map (kbd "q") #'beads-agent-list-quit)
    (define-key map (kbd "w") #'beads-agent-list-copy-session-id)
    (define-key map (kbd "W") #'beads-agent-list-copy-issue-id)
    ;; Sesman session management
    (define-key map (kbd "C-c C-s") beads-sesman-map)
    map)
  "Keymap for `beads-agent-list-mode'.")

(define-derived-mode beads-agent-list-mode tabulated-list-mode "Beads-Agents"
  "Major mode for displaying Beads agent sessions in a tabulated list.

\\{beads-agent-list-mode-map}"
  (setq tabulated-list-format
        (vector (list "Issue" beads-agent-list-issue-width t)
                (list "Title" beads-agent-list-title-width t)
                (list "Backend" beads-agent-list-backend-width t)
                (list "Status" beads-agent-list-status-width t)
                (list "Duration" beads-agent-list-duration-width t)
                (list "Directory" beads-agent-list-directory-width t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (hl-line-mode 1))

;;; Public Entry Point

;;;###autoload
(defun beads-agent-list ()
  "Display agent sessions in a tabulated list buffer.
Opens a beads-agents buffer showing all active AI agent sessions
with their status, duration, and working directory."
  (interactive)
  (let ((buffer (get-buffer-create (beads-buffer-name-utility "agents"))))
    (with-current-buffer buffer
      (beads-agent-list-mode)
      (beads-agent-list--populate-buffer)
      (setq mode-line-format
            '("%e" mode-line-front-space
              mode-line-buffer-identification
              (:eval (format "  %d session%s"
                             (length tabulated-list-entries)
                             (if (= (length tabulated-list-entries) 1) "" "s"))))))
    (pop-to-buffer buffer)))

;;; Hook Integration

(defun beads-agent-list--on-state-change (_action _session)
  "Handle agent state change by refreshing all agent list buffers.
ACTION and SESSION are provided by `beads-agent-state-change-hook'."
  (dolist (buffer (beads-buffer-name-find-utility-buffers nil "agents"))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (condition-case nil
            (beads-agent-list--populate-buffer)
          (error nil))))))

;; Register hook for auto-refresh
(add-hook 'beads-agent-state-change-hook #'beads-agent-list--on-state-change t)

(provide 'beads-agent-list)

;;; beads-agent-list.el ends here
