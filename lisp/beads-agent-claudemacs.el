;;; beads-agent-claudemacs.el --- claudemacs backend for beads-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the claudemacs backend for beads-agent.
;; It integrates with the claudemacs package (cpoile/claudemacs) to enable
;; AI-assisted issue work through Claude Code.
;;
;; claudemacs is an AI pair programming package for Emacs that uses the
;; `eat' terminal emulator and supports multiple AI tools (Claude, Codex,
;; Gemini).  It features system notifications, session resume, transient
;; menus, and context-aware file references.
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
;;   Ensure claudemacs is installed from:
;;   https://github.com/cpoile/claudemacs
;;
;; Buffer naming convention:
;;   Claudemacs uses *claudemacs:TOOL:SESSION-ID* or *claudemacs:TOOL-N:SESSION-ID*
;;   where TOOL is claude/codex/gemini, N is instance number, and SESSION-ID
;;   is the workspace/project name.

;;; Code:

(require 'beads-agent-backend)
(require 'seq)

;;; External Function Declarations

;; From claudemacs.el - session management
(declare-function claudemacs--start "claudemacs")
(declare-function claudemacs--project-root "claudemacs")
(declare-function claudemacs--list-sessions-for-workspace "claudemacs")
(declare-function claudemacs--get-current-session-buffer "claudemacs")
(declare-function claudemacs--is-claudemacs-buffer-p "claudemacs")

;; From claudemacs.el - user commands
(declare-function claudemacs-kill "claudemacs")
(declare-function claudemacs-switch-to-session "claudemacs")

;; From claudemacs.el - messaging
(declare-function claudemacs--send-message-to-claude "claudemacs")

;; From eat.el - terminal process management
(declare-function eat-kill-process "eat")

;; Declare external variables to avoid compiler warnings
(defvar claudemacs-default-tool)
(defvar claudemacs--cwd)

;;; Customization

(defgroup beads-agent-claudemacs nil
  "Claudemacs backend for beads-agent."
  :group 'beads-agent
  :prefix "beads-agent-claudemacs-")

(defcustom beads-agent-claudemacs-tool 'claude
  "Which claudemacs tool to use for agent sessions.
Must be a symbol registered in `claudemacs-tool-registry'.
Common values are `claude', `codex', or `gemini'."
  :type '(choice (const :tag "Claude" claude)
                 (const :tag "Codex" codex)
                 (const :tag "Gemini" gemini)
                 (symbol :tag "Other tool"))
  :group 'beads-agent-claudemacs)

;;; Helper Functions

(defun beads-agent-claudemacs--find-buffers (dir)
  "Find claudemacs buffers for directory DIR.
Claudemacs buffers are named `*claudemacs:TOOL:SESSION-ID*' or
`*claudemacs:TOOL-N:SESSION-ID*' where SESSION-ID is derived from
the workspace/project name.

Returns a list of matching buffers, most recent first."
  ;; Normalize directory: expand, resolve symlinks, strip trailing slash
  (let* ((normalized-dir (directory-file-name
                          (file-truename (expand-file-name dir))))
         (result nil))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (boundp 'claudemacs--cwd)
                     claudemacs--cwd
                     (string= (directory-file-name
                               (file-truename (expand-file-name claudemacs--cwd)))
                              normalized-dir))
            (push buf result)))))
    (nreverse result)))

(defun beads-agent-claudemacs--buffer-has-process-p (buf)
  "Return non-nil if buffer BUF has an active process."
  (and (buffer-live-p buf)
       (get-buffer-process buf)))

;;; Backend Class

(defclass beads-agent-backend-claudemacs (beads-agent-backend)
  ((name :initform "claudemacs")
   (priority :initform 35))
  :documentation "Backend for claudemacs integration.
Uses cpoile's claudemacs package for AI pair programming sessions.")

;;; Protocol Implementation

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-claudemacs))
  "Check if claudemacs is available.
Verifies the package is loaded and key functions exist."
  (and ;; Check claudemacs package
       (or (featurep 'claudemacs)
           (require 'claudemacs nil t))
       (fboundp 'claudemacs--start)
       (fboundp 'claudemacs-kill)
       (fboundp 'claudemacs--send-message-to-claude)
       ;; Check eat terminal emulator
       (or (featurep 'eat)
           (require 'eat nil t))
       ;; Verify claude command is available in PATH
       (executable-find "claude")))

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-claudemacs) _issue prompt)
  "Start claudemacs session with PROMPT.
ISSUE is ignored as claudemacs works per-project/workspace.
The working directory is determined by the caller (may be a worktree).
Returns the claudemacs buffer as the session handle."
  ;; Pre-flight checks with helpful error messages
  (unless (or (featurep 'claudemacs) (require 'claudemacs nil t))
    (error "Claudemacs not found.  Install from: https://github.com/cpoile/claudemacs"))
  (unless (or (featurep 'eat) (require 'eat nil t))
    (error "Eat terminal emulator not found.  Install from MELPA: M-x package-install RET eat"))
  (unless (executable-find "claude")
    (error "Claude command not found in PATH.  Install: npm install -g @anthropic-ai/claude-code"))
  (require 'claudemacs)
  ;; default-directory is set by beads-agent-start (may be worktree)
  ;; Bind BD_NO_DAEMON=1 to disable bd daemon (not supported in worktrees)
  (let* ((working-dir default-directory)
         (process-environment (cons "BD_NO_DAEMON=1" process-environment))
         buffer)
    ;; Start claudemacs session in the working directory
    ;; Pass prompt as CLI argument via the &rest args parameter.
    ;; claudemacs--start signature: (work-dir &optional tool instance-num &rest args)
    ;; The claude CLI accepts: claude [options] [prompt]
    (condition-case err
        (progn
          ;; Pass nil for tool (use default), nil for instance-num (auto),
          ;; then "--" and prompt as args to pass to the CLI
          (claudemacs--start working-dir nil nil "--" prompt)
          ;; Get the buffer that was just created
          (setq buffer (car (beads-agent-claudemacs--find-buffers working-dir)))
          (unless buffer
            (error "Claudemacs session started but buffer not found"))
          buffer)
      (error
       (error "Failed to start claudemacs: %s"
              (error-message-string err))))))

(cl-defmethod beads-agent-backend-stop
    ((_backend beads-agent-backend-claudemacs) session)
  "Stop claudemacs SESSION."
  (require 'claudemacs)
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-claudemacs--find-buffers working-dir)))
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          ;; Kill the terminal process first
          (when (get-buffer-process buf)
            (condition-case nil
                (eat-kill-process)
              (error nil)))
          ;; Then kill the buffer
          (kill-buffer buf))))))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-claudemacs) session)
  "Check if claudemacs SESSION is active.
Returns non-nil if a claudemacs buffer for the session's directory exists
and has a live process."
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-claudemacs--find-buffers working-dir)))
    (and buffers
         (cl-some #'beads-agent-claudemacs--buffer-has-process-p buffers))))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((_backend beads-agent-backend-claudemacs) session)
  "Switch to claudemacs buffer for SESSION.
If no claudemacs session exists, starts a new one automatically."
  (require 'claudemacs)
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-claudemacs--find-buffers working-dir))
         (active-buf (cl-find-if #'beads-agent-claudemacs--buffer-has-process-p
                                 buffers)))
    (if active-buf
        ;; Session exists - switch to it
        (pop-to-buffer active-buf)
      ;; No active session - start a new one
      (message "Claudemacs session expired, starting new one...")
      (let ((default-directory working-dir))
        (claudemacs--start working-dir)))))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-claudemacs) session prompt)
  "Send PROMPT to claudemacs for SESSION."
  (require 'claudemacs)
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-claudemacs--find-buffers working-dir))
         (active-buf (cl-find-if #'beads-agent-claudemacs--buffer-has-process-p
                                 buffers)))
    (if active-buf
        (with-current-buffer active-buf
          (claudemacs--send-message-to-claude prompt))
      (error "No active claudemacs session for %s" working-dir))))

(cl-defmethod beads-agent-backend-get-buffer
    ((_backend beads-agent-backend-claudemacs) session)
  "Return the claudemacs buffer for SESSION, or nil if not available."
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-claudemacs--find-buffers working-dir)))
    (cl-find-if #'beads-agent-claudemacs--buffer-has-process-p buffers)))

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-claudemacs))

(provide 'beads-agent-claudemacs)
;;; beads-agent-claudemacs.el ends here
