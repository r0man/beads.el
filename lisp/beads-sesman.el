;;; beads-sesman.el --- Sesman integration for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides sesman integration for beads, enabling
;; context-aware session selection using the sesman library.
;;
;; Sesman (Session Manager) provides:
;; - Automatic session selection based on buffer, directory, or project
;; - Standard keybindings for session management
;; - Session browser UI
;; - Context linking for sessions
;;
;; Session Structure:
;;   (session-name backend-handle beads-agent-session)
;;   - session-name: "<issue-id>@<worktree-path>" or "<issue-id>@<project>"
;;   - backend-handle: Opaque handle from the AI backend
;;   - beads-agent-session: EIEIO object with full metadata
;;
;; Dual Context Linking:
;;   Sessions are linked to both:
;;   1. Worktree directory (primary) - for context in worktree buffers
;;   2. Main project directory (fallback) - for context from main repo
;;
;; Usage:
;;   ;; Start a session (will be registered with sesman)
;;   (sesman-start-session 'beads)
;;
;;   ;; Get current session for buffer context
;;   (sesman-current-session 'beads)
;;
;;   ;; Open session browser
;;   M-x beads-sesman-browser

;;; Code:

(require 'sesman)
(require 'beads-agent-backend)

;; Forward declarations
(declare-function beads--find-project-root "beads")
(declare-function beads-agent-start "beads-agent")
(declare-function beads-agent-stop "beads-agent")
(declare-function beads-agent--read-issue-id "beads-agent")

;;; Session Naming

(defun beads-sesman--session-name (session)
  "Generate sesman session name from SESSION.
Delegates to `beads-agent-backend-session-name' for backend-specific naming.
Falls back to \"<issue-id>@<working-dir>\" if backend not found."
  (if-let ((backend (beads-agent--get-backend (oref session backend-name))))
      (beads-agent-backend-session-name backend session)
    ;; Fallback if backend not found (defensive)
    (let ((issue-id (oref session issue-id))
          (working-dir (or (oref session worktree-dir)
                           (oref session project-dir))))
      (format "%s@%s" issue-id (abbreviate-file-name working-dir)))))

;;; Sesman Generics Implementation

(cl-defmethod sesman-start-session ((_system (eql beads)))
  "Start a new beads session asynchronously.
Prompt for issue ID and backend, then start the agent.

Note: This method returns nil because `beads-agent-start' is async.
The session is registered with sesman via `beads-agent-state-change-hook'
when the agent actually starts.  Use `sesman-current-session' after
the agent has started to get the session."
  (let ((issue-id (beads-agent--read-issue-id)))
    (beads-agent-start issue-id)
    ;; Return nil - session registration happens via hook when agent starts
    nil))

(cl-defmethod sesman-quit-session ((_system (eql beads)) session)
  "Quit beads SESSION.
SESSION is a sesman session list (name backend-handle beads-agent-session)."
  (when-let ((beads-session (nth 2 session)))
    (beads-agent-stop (oref beads-session id))))

(cl-defmethod sesman-restart-session ((_system (eql beads)) session)
  "Restart beads SESSION.
Stop the session then start a new one for the same issue."
  (when-let ((beads-session (nth 2 session)))
    (let ((issue-id (oref beads-session issue-id)))
      (sesman-quit-session 'beads session)
      ;; Small delay to allow cleanup
      (run-at-time 0.5 nil
                   (lambda ()
                     (beads-agent-start issue-id))))))

(cl-defmethod sesman-project ((_system (eql beads)))
  "Return project root for current directory.
Used by sesman for project-based context linking."
  (beads--find-project-root))

(cl-defmethod sesman-context-types ((_system (eql beads)))
  "Return context types understood by beads.
Sessions can be linked to buffers, directories, or projects."
  '(buffer directory project))

(cl-defmethod sesman-more-relevant-p ((_system (eql beads)) session1 session2)
  "Compare SESSION1 and SESSION2 for relevance.
Use recency-based comparison (more recent = more relevant)."
  (let ((s1 (nth 2 session1))
        (s2 (nth 2 session2)))
    (when (and s1 s2)
      (string> (oref s1 started-at) (oref s2 started-at)))))

(cl-defmethod sesman-session-info ((_system (eql beads)) session)
  "Return display info for SESSION.
Return plist with :objects, :strings for sesman-browser display."
  (let* ((beads-session (nth 2 session))
         (backend-handle (nth 1 session)))
    (when beads-session
      (list
       :objects (list backend-handle)
       :strings (delq nil
                      (list
                       (format "Issue: %s" (oref beads-session issue-id))
                       (format "Backend: %s" (oref beads-session backend-name))
                       (format "Started: %s" (oref beads-session started-at))
                       (when-let ((worktree (oref beads-session worktree-dir)))
                         (format "Worktree: %s"
                                 (abbreviate-file-name worktree)))))))))

;;; Session Registration Helpers

(defun beads-sesman--make-sesman-session (beads-session)
  "Create sesman session list from BEADS-SESSION.
Return (name backend-handle beads-agent-session)."
  (list (beads-sesman--session-name beads-session)
        (oref beads-session backend-session)
        beads-session))

(defun beads-sesman--register-session (beads-session)
  "Register BEADS-SESSION with sesman and link to contexts.
Link to both worktree directory (primary) and main project (fallback)."
  (let ((sesman-session (beads-sesman--make-sesman-session beads-session)))
    ;; Register with sesman
    (sesman-register 'beads sesman-session)
    ;; Link to worktree directory (primary context)
    (when-let ((worktree (oref beads-session worktree-dir)))
      (sesman-link-session 'beads sesman-session 'directory worktree))
    ;; Link to main project (fallback context)
    (sesman-link-session 'beads sesman-session 'project
                         (oref beads-session project-dir))))

(defun beads-sesman--unregister-session (beads-session)
  "Unregister BEADS-SESSION from sesman."
  (let ((name (beads-sesman--session-name beads-session)))
    (sesman-unregister 'beads
                       (sesman-session 'beads name))))

;;; Hook Integration

(defun beads-sesman--state-change-handler (action session)
  "Handle session state change for sesman integration.
ACTION is `started', `stopped', or `failed'.
SESSION is the beads-agent-session object."
  (pcase action
    ('started (beads-sesman--register-session session))
    ('stopped (beads-sesman--unregister-session session))))

;;; Minor Mode

;;;###autoload
(define-minor-mode beads-sesman-mode
  "Toggle sesman integration for beads agent sessions.

When enabled, beads agent sessions are automatically registered with
sesman, providing context-aware session selection based on buffer,
directory, or project.

This mode controls whether the state change hook is active.  When
disabled, beads works without sesman integration."
  :global t
  :group 'beads
  (if beads-sesman-mode
      (add-hook 'beads-agent-state-change-hook
                #'beads-sesman--state-change-handler)
    (remove-hook 'beads-agent-state-change-hook
                 #'beads-sesman--state-change-handler)))

;;; User-Facing Commands

;;;###autoload
(defun beads-sesman-start ()
  "Start a new beads session via sesman."
  (interactive)
  (sesman-start-session 'beads))

;;;###autoload
(defun beads-sesman-quit ()
  "Quit the current beads session."
  (interactive)
  (if-let ((session (sesman-current-session 'beads)))
      (sesman-quit-session 'beads session)
    (user-error "No current beads session")))

;;;###autoload
(defun beads-sesman-restart ()
  "Restart the current beads session."
  (interactive)
  (if-let ((session (sesman-current-session 'beads)))
      (sesman-restart-session 'beads session)
    (user-error "No current beads session")))

;;;###autoload
(defun beads-sesman-browser ()
  "Open the sesman browser for beads sessions."
  (interactive)
  (let ((sesman-system 'beads))
    (sesman-browser)))

;;;###autoload
(defun beads-sesman-info ()
  "Display info about current beads sessions."
  (interactive)
  (let ((sesman-system 'beads))
    (sesman-info)))

;;;###autoload
(defun beads-sesman-link ()
  "Link a beads session to the current context."
  (interactive)
  (let ((sesman-system 'beads))
    (call-interactively #'sesman-link-with-buffer)))

;;; Keymap

(defvar beads-sesman-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'beads-sesman-start)
    (define-key map (kbd "q") #'beads-sesman-quit)
    (define-key map (kbd "r") #'beads-sesman-restart)
    (define-key map (kbd "b") #'beads-sesman-browser)
    (define-key map (kbd "i") #'beads-sesman-info)
    (define-key map (kbd "l") #'beads-sesman-link)
    map)
  "Keymap for beads sesman commands.
Intended to be bound under a global prefix key.")

(provide 'beads-sesman)
;;; beads-sesman.el ends here
