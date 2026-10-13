;;; beads-agent-session.el --- Session commands for beads AI agents -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the user-facing session commands and keymap for
;; beads AI agent sessions.
;;
;; The commands are context-aware: they locate the session for the
;; current buffer or working directory in the in-house session registry
;; (`beads-agent--sessions', see `beads-agent-backend.el') instead of
;; relying on an external session manager's bound context.
;;
;; Sessions are started with the regular `beads-agent-start' path; the
;; map only exists so views can offer the CIDER/ESS-style `C-c C-s'
;; prefix.

;;; Code:

(require 'beads-agent-backend)

;; Forward declarations (beads-agent.el requires this module transitively
;; through beads-agent-list.el, so we must not require it here).
(declare-function beads-agent-start "beads-agent")
(declare-function beads-agent-stop "beads-agent")
(declare-function beads-agent-list "beads-agent-list")

;;; Customization

(defcustom beads-agent-session-restart-delay 0.5
  "Delay in seconds before restarting a session.
This allows cleanup to complete before starting a new agent."
  :type 'number
  :group 'beads-agent)

;;; Context Lookup

(defun beads-agent--session-for-buffer (&optional buffer)
  "Return the agent session associated with BUFFER, or nil.
Checks the session registry for a session whose stored buffer is
BUFFER, then falls back to the buffer-local session id set when the
agent buffer was linked."
  (let ((buffer (or buffer (current-buffer))))
    (or (cl-find-if (lambda (session)
                      (eq (beads-agent-session-buffer session) buffer))
                    (beads-agent--all-sessions))
        (with-current-buffer buffer
          (when (bound-and-true-p beads-agent--buffer-session-id)
            (beads-agent--get-session beads-agent--buffer-session-id))))))

(defun beads-agent--session-for-directory (directory)
  "Return the agent session whose working directory is DIRECTORY, or nil."
  (let ((target (file-truename (expand-file-name directory))))
    (cl-find-if
     (lambda (session)
       (equal (file-truename
               (expand-file-name (beads-agent-session-working-dir session)))
              target))
     (beads-agent--all-sessions))))

(defun beads-agent--session-for-context (&optional directory)
  "Return the agent session for the current buffer or DIRECTORY.
DIRECTORY defaults to `default-directory'."
  (or (beads-agent--session-for-buffer (current-buffer))
      (beads-agent--session-for-directory (or directory default-directory))))

;;; User-Facing Commands

;;;###autoload
(defun beads-agent-session-start ()
  "Start a new beads agent session.
Delegates to `beads-agent-start' (issue at point or prompt)."
  (interactive)
  (call-interactively #'beads-agent-start))

;;;###autoload
(defun beads-agent-session-stop ()
  "Stop the beads agent session for the current buffer/directory."
  (interactive)
  (if-let* ((session (beads-agent--session-for-context)))
      (beads-agent-stop (oref session id))
    (user-error "No current beads agent session")))

;;;###autoload
(defun beads-agent-session-restart ()
  "Restart the beads agent session for the current buffer/directory."
  (interactive)
  (if-let* ((session (beads-agent--session-for-context)))
      (let ((issue-id (beads-agent-session-issue-id session)))
        (beads-agent-stop (oref session id))
        (run-at-time beads-agent-session-restart-delay nil
                     (lambda ()
                       (beads-agent-start issue-id))))
    (user-error "No current beads agent session")))

;;; Keymap

(defvar beads-agent-session-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'beads-agent-session-start)
    (define-key map (kbd "q") #'beads-agent-session-stop)
    (define-key map (kbd "r") #'beads-agent-session-restart)
    (define-key map (kbd "b") #'beads-agent-list)
    map)
  "Keymap for beads agent session commands.
Intended to be bound under a global prefix key (C-c C-s).")

(provide 'beads-agent-session)
;;; beads-agent-session.el ends here
