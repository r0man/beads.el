;;; beads-agent-claude-code.el --- claude-code.el backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the claude-code.el backend for beads-agent.
;; It integrates with the claude-code package by stevemolitor to enable
;; AI-assisted issue work through Claude Code.
;;
;; claude-code.el is a lightweight Claude Code integration that supports
;; both vterm and eat terminal backends, with features like session
;; resume, read-only mode, and desktop notifications.
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
;;   Ensure claude-code is installed from:
;;   https://github.com/stevemolitor/claude-code.el

;;; Code:

(require 'beads-agent-backend)
(require 'seq)

;;; External Function Declarations

;; From claude-code.el
(declare-function claude-code "claude-code")
(declare-function claude-code-kill "claude-code")
(declare-function claude-code-switch-to-buffer "claude-code")
(declare-function claude-code-send-command "claude-code")

;; Declare external variables to avoid compiler warnings
(defvar vterm-environment)
(defvar claude-code-program-switches)

;;; Helper Functions

(defun beads-agent-claude-code--find-buffers (dir)
  "Find Claude buffers for directory DIR.
Claude buffers are named `*claude:DIRECTORY/*' or
`*claude:DIRECTORY/:INSTANCE*' where DIRECTORY is the abbreviated
truename of the directory with trailing slash.

NOTE: This reimplements claude-code's buffer naming convention.
If upstream changes the format, this function may need updating."
  ;; Use file-name-as-directory to ensure trailing slash, matching claude-code's
  ;; buffer naming convention (it uses abbreviate-file-name on directory which
  ;; preserves trailing slash from project-root/file-name-directory)
  (let* ((normalized-dir (file-name-as-directory
                          (abbreviate-file-name (file-truename dir))))
         (prefix (concat "*claude:" normalized-dir)))
    (seq-filter
     (lambda (buf)
       (let ((name (buffer-name buf)))
         (and (string-prefix-p prefix name)
              ;; Must end with * (main buffer) or :INSTANCE* (named instance)
              (or (string= name (concat prefix "*"))
                  (and (> (length name) (length prefix))
                       (eq (aref name (length prefix)) ?:)
                       (string-suffix-p "*" name))))))
     (buffer-list))))

;;; Backend Class

(defclass beads-agent-backend-claude-code (beads-agent-backend)
  ((name :initform "claude-code")
   (priority :initform 40)
   (description :initform "Lightweight Claude Code via vterm/eat"))
  :documentation "Backend for claude-code.el integration.
Uses stevemolitor's claude-code package for terminal-based Claude sessions.")

;;; Protocol Implementation

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-claude-code))
  "Check if claude-code is available.
Verifies the package is loaded and key functions exist."
  (and ;; Check claude-code package
       (or (featurep 'claude-code)
           (require 'claude-code nil t))
       (fboundp 'claude-code)
       (fboundp 'claude-code-kill)
       (fboundp 'claude-code-send-command)
       (fboundp 'claude-code-switch-to-buffer)
       ;; Verify claude command is available in PATH
       (executable-find "claude")))

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-claude-code) _issue prompt)
  "Start claude-code session with PROMPT.
ISSUE is ignored as claude-code works per-project.
The working directory is determined by the caller (may be a worktree).
Returns cons cell (BACKEND-SESSION . BUFFER)."
  ;; Pre-flight checks with helpful error messages
  (unless (or (featurep 'claude-code) (require 'claude-code nil t))
    (error "Claude-code.el not found.  Install from: https://github.com/stevemolitor/claude-code.el"))
  (unless (executable-find "claude")
    (error "Claude command not found in PATH.  Install: npm install -g @anthropic-ai/claude-code"))
  (require 'claude-code)
  ;; default-directory is set by beads-agent-start (may be worktree)
  ;; Bind BD_NO_DAEMON=1 to disable bd daemon (not supported in worktrees)
  ;; We bind both process-environment and vterm-environment to ensure the
  ;; variable is available regardless of terminal backend (vterm or eat):
  ;; Pass prompt as CLI argument by appending to claude-code-program-switches
  (let* ((working-dir default-directory)
         (process-environment (cons "BD_NO_DAEMON=1" process-environment))
         ;; vterm-environment expects strings in "VAR=value" format
         (vterm-environment (if (boundp 'vterm-environment)
                                (cons "BD_NO_DAEMON=1" vterm-environment)
                              (list "BD_NO_DAEMON=1")))
         ;; Append prompt as positional argument to CLI switches
         ;; The claude CLI accepts: claude [options] [prompt]
         (claude-code-program-switches
          (append (and (boundp 'claude-code-program-switches)
                       claude-code-program-switches)
                  (list "--" prompt)))
         buffer)
    ;; Start claude-code in the working directory with prompt as CLI arg
    (condition-case err
        (progn
          (claude-code)
          ;; Get the buffer that was just created
          (setq buffer (car (beads-agent-claude-code--find-buffers working-dir))))
      (error
       (error "Failed to start claude-code: %s"
              (error-message-string err))))
    ;; Return (nil . buffer) - claude-code has no separate session handle
    (cons nil buffer)))

(cl-defmethod beads-agent-backend-stop
    ((backend beads-agent-backend-claude-code) session)
  "Stop claude-code SESSION using BACKEND.
Explicitly kills the session buffer since beads renames it and
`claude-code-kill' won't find it by the original name."
  ;; Kill the buffer directly - claude-code-kill won't find it
  ;; because we renamed it from *Claude: ...* to *beads-agent[...]*
  (when-let ((buffer (beads-agent-backend-get-buffer backend session)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-claude-code) session)
  "Check if claude-code SESSION is active.
Returns non-nil if the Claude buffer for the session's directory exists
and has a live process."
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-claude-code--find-buffers working-dir)))
    (and buffers
         (buffer-live-p (car buffers))
         ;; Check if buffer has an active process
         (get-buffer-process (car buffers)))))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((backend beads-agent-backend-claude-code) session)
  "Switch to claude-code buffer for SESSION using BACKEND."
  (if-let ((buffer (beads-agent-backend-get-buffer backend session)))
      (if (buffer-live-p buffer)
          (beads-agent--pop-to-buffer-other-window buffer)
        (user-error "Agent buffer has been killed"))
    (user-error "No buffer found for session %s" (oref session id))))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-claude-code) session prompt)
  "Send PROMPT to claude-code for SESSION."
  (require 'claude-code)
  (let ((default-directory (beads-agent-session-working-dir session)))
    (claude-code-send-command prompt)))

;; Note: beads-agent-backend-get-buffer uses the default implementation
;; from beads-agent-backend.el which returns the stored session buffer.

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-claude-code))

(provide 'beads-agent-claude-code)
;;; beads-agent-claude-code.el ends here
