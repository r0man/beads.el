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
;; Context Linking:
;;   Sessions are linked to three context types:
;;   1. Buffer (agent buffer) - for context when in the agent's terminal
;;   2. Worktree directory (primary) - for context in worktree buffers
;;   3. Main project directory (fallback) - for context from main repo
;;
;;   This enables `sesman-current-session' to find the right session
;;   whether you're in the agent buffer, editing files in the worktree,
;;   or working in the main repository.
;;
;; Usage:
;;   ;; Start a session (will be registered with sesman)
;;   (sesman-start-session 'Beads)
;;
;;   ;; Get current session for buffer context
;;   (sesman-current-session 'Beads)
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

;;; Constants

(defconst beads-sesman-system 'Beads
  "Sesman system name for beads sessions.
This symbol identifies beads sessions in sesman's registry.")

;;; Customization

(defcustom beads-sesman-restart-delay 0.5
  "Delay in seconds before restarting a session.
This allows cleanup to complete before starting a new agent."
  :type 'number
  :group 'beads)

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

(cl-defmethod sesman-start-session ((_system (eql Beads)))
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

(cl-defmethod sesman-quit-session ((_system (eql Beads)) session)
  "Quit beads SESSION.
SESSION is a sesman session list (name backend-handle beads-agent-session)."
  (when-let ((beads-session (nth 2 session)))
    (beads-agent-stop (oref beads-session id))))

(cl-defmethod sesman-restart-session ((_system (eql Beads)) session)
  "Restart beads SESSION.
Stop the session then start a new one for the same issue."
  (when-let ((beads-session (nth 2 session)))
    (let ((issue-id (oref beads-session issue-id)))
      (sesman-quit-session beads-sesman-system session)
      ;; Small delay to allow cleanup
      (run-at-time beads-sesman-restart-delay nil
                   (lambda ()
                     (beads-agent-start issue-id))))))

(cl-defmethod sesman-project ((_system (eql Beads)))
  "Return project root for current directory.
Used by sesman for project-based context linking."
  (beads--find-project-root))

(cl-defmethod sesman-context-types ((_system (eql Beads)))
  "Return context types understood by beads.
Sessions can be linked to buffers, directories, or projects."
  '(buffer directory project))

(cl-defmethod sesman-more-relevant-p ((_system (eql Beads)) session1 session2)
  "Compare SESSION1 and SESSION2 for relevance.
Use recency-based comparison (more recent = more relevant)."
  (let ((s1 (nth 2 session1))
        (s2 (nth 2 session2)))
    (when (and s1 s2)
      (string> (oref s1 started-at) (oref s2 started-at)))))

(cl-defmethod sesman-session-info ((_system (eql Beads)) session)
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
Link to worktree directory (primary), main project (fallback),
and the agent buffer (if available)."
  (let ((sesman-session (beads-sesman--make-sesman-session beads-session)))
    ;; Register with sesman
    (sesman-register beads-sesman-system sesman-session)
    ;; Link to worktree directory (primary context)
    (when-let ((worktree (oref beads-session worktree-dir)))
      (sesman-link-session beads-sesman-system sesman-session 'directory worktree))
    ;; Link to main project (fallback context)
    (sesman-link-session beads-sesman-system sesman-session 'project
                         (oref beads-session project-dir))
    ;; Link to agent buffer if available
    (when-let* ((backend (beads-agent--get-backend (oref beads-session backend-name)))
                (buffer (beads-agent-backend-get-buffer backend beads-session)))
      (when (buffer-live-p buffer)
        (sesman-link-session beads-sesman-system sesman-session 'buffer buffer)
        ;; Set sesman-system in the buffer so sesman commands work there
        (with-current-buffer buffer
          (setq-local sesman-system beads-sesman-system))))))

(defun beads-sesman--unregister-session (beads-session)
  "Unregister BEADS-SESSION from sesman."
  (let ((name (beads-sesman--session-name beads-session)))
    (sesman-unregister beads-sesman-system
                       (sesman-session beads-sesman-system name))))

;;; Hook Integration

(defun beads-sesman--state-change-handler (action session)
  "Handle session state change for sesman integration.
ACTION is `started', `stopped', or `failed'.
SESSION is the beads-agent-session object."
  (pcase action
    ('started (beads-sesman--register-session session))
    ('stopped (beads-sesman--unregister-session session))))

;;; User-Facing Commands

;;;###autoload
(defun beads-sesman-start ()
  "Start a new beads session via sesman."
  (interactive)
  (sesman-start-session beads-sesman-system))

;;;###autoload
(defun beads-sesman-quit ()
  "Quit the current beads session."
  (interactive)
  (if-let ((session (sesman-current-session beads-sesman-system)))
      (sesman-quit-session beads-sesman-system session)
    (user-error "No current beads session")))

;;;###autoload
(defun beads-sesman-restart ()
  "Restart the current beads session."
  (interactive)
  (if-let ((session (sesman-current-session beads-sesman-system)))
      (sesman-restart-session beads-sesman-system session)
    (user-error "No current beads session")))

;;;###autoload
(defun beads-sesman-browser ()
  "Open the sesman browser for beads sessions."
  (interactive)
  (let ((sesman-system beads-sesman-system))
    (sesman-browser)))

;;;###autoload
(defun beads-sesman-link ()
  "Link a beads session to the current context."
  (interactive)
  (let ((sesman-system beads-sesman-system))
    (call-interactively #'sesman-link-with-buffer)))

;;; Keymap

(defvar beads-sesman-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'beads-sesman-start)
    (define-key map (kbd "q") #'beads-sesman-quit)
    (define-key map (kbd "r") #'beads-sesman-restart)
    (define-key map (kbd "b") #'beads-sesman-browser)
    (define-key map (kbd "l") #'beads-sesman-link)
    map)
  "Keymap for beads sesman commands.
Intended to be bound under a global prefix key.")

;;; Hook Registration
;;
;; Register the state change handler unconditionally when this file is loaded.
;; This ensures sesman integration is always active when beads-sesman is required.

(add-hook 'beads-agent-state-change-hook #'beads-sesman--state-change-handler)

(provide 'beads-sesman)
;;; beads-sesman.el ends here
