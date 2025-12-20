;;; beads-agent-claude-code-ide.el --- claude-code-ide.el backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the claude-code-ide.el backend for beads-agent.
;; It integrates with the claude-code-ide package to enable AI-assisted
;; issue work through Claude Code.
;;
;; The backend implements all required protocol methods:
;; - beads-agent-backend-available-p
;; - beads-agent-backend-start
;; - beads-agent-backend-stop
;; - beads-agent-backend-session-active-p
;; - beads-agent-backend-switch-to-buffer
;; - beads-agent-backend-send-prompt
;;
;; Usage:
;;   The backend is automatically registered when this file is loaded.
;;   Ensure claude-code-ide is installed and configured.

;;; Code:

(require 'beads-agent-backend)
(require 'seq)

;; Declare external functions to avoid compiler warnings
(declare-function ws-process "web-server")
(declare-function claude-code-ide "claude-code-ide")
(declare-function claude-code-ide-stop "claude-code-ide")
(declare-function claude-code-ide-switch-to-buffer "claude-code-ide")
(declare-function claude-code-ide-send-prompt "claude-code-ide")
(declare-function claude-code-ide-mcp--get-session-for-project "claude-code-ide-mcp")
(declare-function claude-code-ide-emacs-tools-setup "claude-code-ide-emacs-tools")

;; Declare external variables
(defvar claude-code-ide-cli-extra-flags)
(defvar claude-code-ide-enable-mcp-server)

;;; Helper Functions

(defun beads-agent-claude-code-ide--find-buffers (dir)
  "Find Claude Code IDE buffers for directory DIR.
Claude Code IDE buffers are named `*Claude Code: DIRECTORY_NAME*' where
DIRECTORY_NAME is the directory basename.

NOTE: This reimplements claude-code-ide's buffer naming convention.
If upstream changes the format, this function may need updating."
  (let* ((normalized-dir (file-truename (expand-file-name dir)))
         (dir-name (file-name-nondirectory (directory-file-name normalized-dir)))
         (expected-name (format "*Claude Code: %s*" dir-name)))
    (seq-filter
     (lambda (buf)
       (string= (buffer-name buf) expected-name))
     (buffer-list))))

;;; Backend Class

(defclass beads-agent-backend-claude-code-ide (beads-agent-backend)
  ((name :initform "claude-code-ide")
   (priority :initform 10)
   (description :initform "MCP-based Claude Code integration"))
  :documentation "Backend for claude-code-ide.el integration.
This is the primary, fully-implemented backend.")

;;; Protocol Implementation

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-claude-code-ide))
  "Check if claude-code-ide is available.
Verifies the package is loaded, web-server is available, and claude exists."
  (and ;; Check web-server.el is available (required for MCP)
       (or (featurep 'web-server)
           (require 'web-server nil t))
       ;; Check claude-code-ide package
       (or (featurep 'claude-code-ide)
           (require 'claude-code-ide nil t))
       (fboundp 'claude-code-ide)
       (fboundp 'claude-code-ide-stop)
       (fboundp 'claude-code-ide-send-prompt)
       (fboundp 'claude-code-ide-switch-to-buffer)
       ;; Verify claude command is available in PATH
       (executable-find "claude")))

;; Declare vterm-environment to avoid compiler warnings
(defvar vterm-environment)

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-claude-code-ide) _issue prompt)
  "Start claude-code-ide session with PROMPT.
ISSUE is ignored as claude-code-ide works per-project.
The working directory is determined by the caller (may be a worktree).
Returns cons cell (BACKEND-SESSION . BUFFER)."
  ;; Pre-flight checks with helpful error messages
  (unless (or (featurep 'web-server) (require 'web-server nil t))
    (error "Web-server.el not found.  Install it via: M-x package-install RET web-server RET"))
  (unless (or (featurep 'claude-code-ide) (require 'claude-code-ide nil t))
    (error "Claude-code-ide.el not found.  See: https://github.com/anthropics/claude-code"))
  (unless (executable-find "claude")
    (error "Claude command not found in PATH.  Install: npm install -g @anthropic-ai/claude-code"))
  (require 'claude-code-ide)
  ;; Ensure Emacs MCP tools are set up (idempotent check via enable flag)
  (when (and (not (bound-and-true-p claude-code-ide-enable-mcp-server))
             (require 'claude-code-ide-emacs-tools nil t)
             (fboundp 'claude-code-ide-emacs-tools-setup))
    (claude-code-ide-emacs-tools-setup))
  ;; default-directory is set by beads-agent-start (may be worktree)
  ;; Bind BD_NO_DAEMON=1 to disable bd daemon (not supported in worktrees)
  ;; We bind both process-environment and vterm-environment to ensure the
  ;; variable is available regardless of how the terminal is launched:
  ;; - process-environment: standard Emacs process spawning ("VAR=value")
  ;; - vterm-environment: vterm terminal emulator (also "VAR=value" strings)
  ;; Pass initial prompt as positional CLI argument via extra-flags
  (let* ((working-dir default-directory)
         (process-environment (cons "BD_NO_DAEMON=1" process-environment))
         ;; vterm-environment expects strings in "VAR=value" format
         (vterm-environment (if (boundp 'vterm-environment)
                                (cons "BD_NO_DAEMON=1" vterm-environment)
                              (list "BD_NO_DAEMON=1")))
         ;; Append initial prompt to user's extra flags (preserve user settings)
         (claude-code-ide-cli-extra-flags
          (string-trim
           (concat (or claude-code-ide-cli-extra-flags "")
                   " "
                   (shell-quote-argument prompt))))
         backend-session
         buffer)
    ;; Start claude-code-ide with the prompt as CLI argument
    (condition-case err
        (claude-code-ide)
      (error
       (error "Failed to start claude-code-ide: %s"
              (error-message-string err))))
    ;; Get the buffer that was just created (before any renaming)
    (setq buffer (car (beads-agent-claude-code-ide--find-buffers working-dir)))
    ;; Get MCP session handle (may be nil if not yet connected)
    (when (fboundp 'claude-code-ide-mcp--get-session-for-project)
      (condition-case nil
          (setq backend-session
                (claude-code-ide-mcp--get-session-for-project working-dir))
        (error nil)))
    ;; Return (backend-session . buffer)
    (cons backend-session buffer)))

(cl-defmethod beads-agent-backend-stop
    ((backend beads-agent-backend-claude-code-ide) session)
  "Stop claude-code-ide SESSION using BACKEND.
Explicitly kills the session buffer since beads renames it and
`claude-code-ide-stop' won't find it by the original name."
  ;; Kill the buffer directly - claude-code-ide-stop won't find it
  ;; because we renamed it from *Claude Code: ...* to *beads-agent[...]*
  (when-let ((buffer (beads-agent-backend-get-buffer backend session)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-claude-code-ide) session)
  "Check if claude-code-ide SESSION is active."
  (require 'claude-code-ide)
  (let ((backend-session (oref session backend-session))
        (working-dir (beads-agent-session-working-dir session)))
    (and backend-session
         ;; Check if we can get the session for the project/worktree
         (when (fboundp 'claude-code-ide-mcp--get-session-for-project)
           (claude-code-ide-mcp--get-session-for-project working-dir)))))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((backend beads-agent-backend-claude-code-ide) session)
  "Switch to claude-code-ide buffer for SESSION using BACKEND."
  (let ((buffer (or (beads-agent-backend-get-buffer backend session)
                    ;; Try to find by expected name (recovery for old sessions)
                    (get-buffer (beads-agent--generate-buffer-name-for-session
                                 session)))))
    (cond
     ((and buffer (buffer-live-p buffer))
      ;; Store buffer if we recovered it by name
      (unless (beads-agent-session-buffer session)
        (beads-agent-session-set-buffer session buffer))
      (beads-agent--pop-to-buffer-other-window buffer))
     (buffer
      (user-error "Agent buffer has been killed"))
     (t
      (user-error "No buffer found for session %s" (oref session id))))))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-claude-code-ide) session prompt)
  "Send PROMPT to claude-code-ide for SESSION."
  (require 'claude-code-ide)
  (let ((default-directory (beads-agent-session-working-dir session)))
    (claude-code-ide-send-prompt prompt)))

;; Note: beads-agent-backend-get-buffer uses the default implementation
;; from beads-agent-backend.el which returns the stored session buffer.

;;; Registration

;; Register the backend when this file is loaded
(beads-agent--register-backend
 (beads-agent-backend-claude-code-ide))


(provide 'beads-agent-claude-code-ide)
;;; beads-agent-claude-code-ide.el ends here
