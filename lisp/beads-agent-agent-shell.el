;;; beads-agent-agent-shell.el --- agent-shell backend for beads-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the agent-shell backend for beads-agent.
;; It integrates with xenodium's agent-shell package to enable
;; AI-assisted issue work through various AI agents.
;;
;; agent-shell is an Emacs package that provides AI agent capabilities
;; through shell integration.  It supports multiple AI backends and
;; features like file capabilities, session modes, and MCP servers.
;; See: https://github.com/xenodium/agent-shell
;;
;; The backend implements all required protocol methods:
;; - beads-agent-backend-available-p
;; - beads-agent-backend-start
;; - beads-agent-backend-stop
;; - beads-agent-backend-session-active-p
;; - beads-agent-backend-switch-to-buffer
;; - beads-agent-backend-send-prompt
;; - beads-agent-backend-get-buffer
;;
;; Usage:
;;   The backend is automatically registered when this file is loaded.
;;   Ensure agent-shell is installed from:
;;   https://github.com/xenodium/agent-shell
;;
;; Buffer identification:
;;   agent-shell buffers are identified by having `agent-shell-mode' and
;;   a matching `default-directory'.

;;; Code:

(require 'beads-agent-backend)
(require 'seq)

;;; External Function Declarations (PUBLIC API ONLY)

;; From agent-shell.el - session management
(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-project-buffers "agent-shell")
(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")

;; From acp.el - client shutdown
(declare-function acp-shutdown "acp")

;; Declare external variables
(defvar agent-shell-agent-configs)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell--state)

;;; Customization

(defgroup beads-agent-agent-shell nil
  "Agent-shell backend for beads-agent."
  :group 'beads-agent
  :prefix "beads-agent-agent-shell-")

(defcustom beads-agent-agent-shell-config nil
  "Agent config to use for beads-agent sessions.
If nil, uses `agent-shell-preferred-agent-config' if set,
otherwise prompts user to select from `agent-shell-agent-configs'.
See `agent-shell-make-agent-config' for config format."
  :type '(choice (const :tag "Use preferred or prompt" nil)
                 (alist :tag "Agent config"))
  :group 'beads-agent-agent-shell)

;;; Helper Functions

(defun beads-agent-agent-shell--find-buffers (dir)
  "Find agent-shell buffers for directory DIR.
Returns list of buffers in `agent-shell-mode' with matching `default-directory'."
  (let ((normalized-dir (directory-file-name
                         (file-truename (expand-file-name dir)))))
    (seq-filter
     (lambda (buf)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (and (derived-mode-p 'agent-shell-mode)
                (string= (directory-file-name
                          (file-truename (expand-file-name default-directory)))
                         normalized-dir)))))
     (buffer-list))))

(defun beads-agent-agent-shell--buffer-active-p (buf)
  "Return non-nil if BUF is an active agent-shell session."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (and (derived-mode-p 'agent-shell-mode)
              (get-buffer-process buf)))))

;;; Backend Class

(defclass beads-agent-backend-agent-shell (beads-agent-backend)
  ((name :initform "agent-shell")
   (priority :initform 15)
   (description :initform "AI agent via shell integration"))
  :documentation "Backend for agent-shell integration.
Uses xenodium's agent-shell package for AI agent sessions.")

;;; Protocol Implementation

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-agent-shell))
  "Check if agent-shell is available.
Verifies the package is loaded and key functions exist."
  (and ;; Check agent-shell package
       (or (featurep 'agent-shell)
           (require 'agent-shell nil t))
       (fboundp 'agent-shell-start)
       (fboundp 'agent-shell-insert)
       ;; Check agent configs exist
       (boundp 'agent-shell-agent-configs)
       agent-shell-agent-configs))

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-agent-shell) _issue prompt)
  "Start agent-shell session with PROMPT.
ISSUE is ignored as agent-shell works per-project/workspace.
The working directory is determined by the caller (may be a worktree).
Returns the agent-shell buffer as the session handle."
  ;; Pre-flight checks with helpful error messages
  (unless (or (featurep 'agent-shell) (require 'agent-shell nil t))
    (error "Agent-shell not found.  Install from: https://github.com/xenodium/agent-shell"))
  (require 'agent-shell)
  (unless (and (boundp 'agent-shell-agent-configs) agent-shell-agent-configs)
    (error "No agent-shell configs defined.  Configure `agent-shell-agent-configs'"))
  ;; default-directory is set by beads-agent-start (may be worktree)
  ;; Bind BD_NO_DAEMON=1 to disable bd daemon (not supported in worktrees)
  (let* ((process-environment (cons "BD_NO_DAEMON=1" process-environment))
         (config (or beads-agent-agent-shell-config
                     (and (boundp 'agent-shell-preferred-agent-config)
                          agent-shell-preferred-agent-config)
                     (agent-shell-select-config :prompt "Select agent: ")))
         buffer)
    (unless config
      (error "No agent-shell config selected"))
    ;; Start agent-shell session with the config
    (condition-case err
        (progn
          (setq buffer (agent-shell-start :config config))
          (unless buffer
            (error "Agent-shell session started but no buffer returned"))
          ;; Send the initial prompt
          (with-current-buffer buffer
            (agent-shell-insert :text prompt :submit t))
          buffer)
      (error
       (error "Failed to start agent-shell: %s"
              (error-message-string err))))))

(cl-defmethod beads-agent-backend-stop
    ((_backend beads-agent-backend-agent-shell) session)
  "Stop agent-shell SESSION.
Uses standard buffer killing which triggers `agent-shell--clean-up'
via the buffer-local `kill-buffer-hook'.  The cleanup function handles
ACP client shutdown, heartbeat termination, and viewport buffer cleanup."
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-agent-shell--find-buffers working-dir)))
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          ;; Optionally interrupt any running request for better UX
          (condition-case nil
              (when (and (get-buffer-process buf)
                         (fboundp 'agent-shell-interrupt))
                (agent-shell-interrupt t))
            (error nil))
          ;; Kill buffer while it's current - agent-shell--clean-up checks
          ;; (derived-mode-p 'agent-shell-mode) which requires current buffer.
          (kill-buffer buf))))))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-agent-shell) session)
  "Check if agent-shell SESSION is active.
Returns non-nil if an agent-shell buffer for the session's directory exists
and has a live process."
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-agent-shell--find-buffers working-dir)))
    (and buffers
         (cl-some #'beads-agent-agent-shell--buffer-active-p buffers))))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((_backend beads-agent-backend-agent-shell) session)
  "Switch to agent-shell buffer for SESSION.
If no agent-shell session exists, starts a new one automatically."
  ;; First try the session's stored buffer (renamed to beads format)
  (let ((stored-buffer (beads-agent-session-buffer session)))
    (if (and stored-buffer (buffer-live-p stored-buffer))
        (pop-to-buffer stored-buffer)
      ;; Fall back to agent-shell's normal switching behavior
      (require 'agent-shell)
      (let* ((working-dir (beads-agent-session-working-dir session))
             (buffers (beads-agent-agent-shell--find-buffers working-dir))
             (active-buf (cl-find-if #'beads-agent-agent-shell--buffer-active-p
                                     buffers)))
        (if active-buf
            ;; Session exists - switch to it
            (pop-to-buffer active-buf)
          ;; No active session - start a new one
          (message "Agent-shell session expired, starting new one...")
          (let ((default-directory working-dir)
                (config (or beads-agent-agent-shell-config
                            (and (boundp 'agent-shell-preferred-agent-config)
                                 agent-shell-preferred-agent-config)
                            (car agent-shell-agent-configs))))
            (agent-shell-start :config config)))))))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-agent-shell) session prompt)
  "Send PROMPT to agent-shell for SESSION."
  (require 'agent-shell)
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-agent-shell--find-buffers working-dir))
         (active-buf (cl-find-if #'beads-agent-agent-shell--buffer-active-p
                                 buffers)))
    (if active-buf
        (with-current-buffer active-buf
          (agent-shell-insert :text prompt :submit t))
      (error "No active agent-shell session for %s" working-dir))))

(cl-defmethod beads-agent-backend-get-buffer
    ((_backend beads-agent-backend-agent-shell) session)
  "Return the agent-shell buffer for SESSION, or nil if not available.
First checks if the session has a stored buffer (after renaming),
then falls back to pattern-based buffer lookup."
  ;; First check the session's stored buffer (set after renaming)
  (let ((stored-buffer (beads-agent-session-buffer session)))
    (if (and stored-buffer (buffer-live-p stored-buffer))
        stored-buffer
      ;; Fall back to pattern-based lookup
      (let* ((working-dir (beads-agent-session-working-dir session))
             (buffers (beads-agent-agent-shell--find-buffers working-dir)))
        (cl-find-if #'beads-agent-agent-shell--buffer-active-p buffers)))))

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-agent-shell))

(provide 'beads-agent-agent-shell)
;;; beads-agent-agent-shell.el ends here
