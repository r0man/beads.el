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
Claude buffers are named `*claude:DIRECTORY*' or `*claude:DIRECTORY:INSTANCE*'
where DIRECTORY is the abbreviated truename of the directory.

NOTE: This reimplements claude-code's buffer naming convention.
If upstream changes the format, this function may need updating."
  ;; Use directory-file-name to strip trailing slash for consistent matching
  (let* ((normalized-dir (directory-file-name
                          (abbreviate-file-name (file-truename dir))))
         (prefix (concat "*claude:" normalized-dir)))
    (seq-filter
     (lambda (buf)
       (let ((name (buffer-name buf)))
         (and (string-prefix-p prefix name)
              ;; Must end with * or :INSTANCE*
              (or (string= name (concat prefix "*"))
                  (and (> (length name) (length prefix))
                       (eq (aref name (length prefix)) ?:)
                       (string-suffix-p "*" name))))))
     (buffer-list))))

;;; Backend Class

(defclass beads-agent-backend-claude-code (beads-agent-backend)
  ((name :initform "claude-code")
   (priority :initform 40))
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
Returns the Claude buffer as the session handle."
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
                  (list "--" prompt))))
    ;; Start claude-code in the working directory with prompt as CLI arg
    (condition-case err
        (progn
          (claude-code)
          ;; Get the buffer that was just created
          (car (beads-agent-claude-code--find-buffers working-dir)))
      (error
       (error "Failed to start claude-code: %s"
              (error-message-string err))))))

(cl-defmethod beads-agent-backend-stop
    ((_backend beads-agent-backend-claude-code) session)
  "Stop claude-code SESSION."
  (require 'claude-code)
  (let ((default-directory (beads-agent-session-working-dir session)))
    (claude-code-kill)))

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
  "Switch to claude-code buffer for SESSION using BACKEND.
If no Claude Code session exists, starts a new one automatically."
  (require 'claude-code)
  (let ((default-directory (beads-agent-session-working-dir session)))
    (if (beads-agent-backend-session-active-p backend session)
        ;; Session exists - switch to it
        (condition-case nil
            (claude-code-switch-to-buffer)
          (error
           (message "Claude Code session expired, starting new one...")
           (claude-code)))
      ;; No active session - start a new one
      (message "Claude Code session expired, starting new one...")
      (claude-code))))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-claude-code) session prompt)
  "Send PROMPT to claude-code for SESSION."
  (require 'claude-code)
  (let ((default-directory (beads-agent-session-working-dir session)))
    (claude-code-send-command prompt)))

(cl-defmethod beads-agent-backend-get-buffer
    ((_backend beads-agent-backend-claude-code) session)
  "Return the claude-code buffer for SESSION, or nil if not available."
  (let ((working-dir (beads-agent-session-working-dir session)))
    (car (beads-agent-claude-code--find-buffers working-dir))))

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-claude-code))

(provide 'beads-agent-claude-code)
;;; beads-agent-claude-code.el ends here
