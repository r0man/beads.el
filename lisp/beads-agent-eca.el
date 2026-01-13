;;; beads-agent-eca.el --- ECA (Editor Code Assistant) backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the ECA (Editor Code Assistant) backend for
;; beads-agent.  It integrates with eca-emacs to enable AI-assisted
;; issue work through ECA's JSON-RPC API.
;;
;; ECA is an Emacs AI assistant that supports multiple concurrent
;; sessions per workspace, making it well-suited for beads.el's
;; directory-bound session model.
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
;;   Ensure eca-emacs is installed from:
;;   https://github.com/editor-code-assistant/eca-emacs

;;; Code:

(require 'beads-agent-backend)
(require 'seq)

;;; External Function Declarations

;; From eca.el (main entry point)
(declare-function eca "eca")
(declare-function eca-stop "eca")
(declare-function eca-restart "eca")
(declare-function eca-session "eca")
(declare-function eca--session-id "eca")
(declare-function eca-create-session "eca")

;; From eca-process.el
(declare-function eca-process-start "eca-process")
(declare-function eca-process-stop "eca-process")
(declare-function eca-process-running-p "eca-process")

;; From eca-chat.el
(declare-function eca-chat-send-prompt "eca-chat")
(declare-function eca-chat-open "eca-chat")
(declare-function eca-chat-exit "eca-chat")
(declare-function eca-chat-stop-prompt "eca-chat")

;; From eca-install.el (optional - server installation)
(declare-function eca-install-server "eca-install")

;;; Helper Functions

(defun beads-agent-eca--find-chat-buffer (eca-session)
  "Find the ECA chat buffer for ECA-SESSION.
ECA chat buffers are typically named with the session context.
Returns the buffer if found, nil otherwise."
  ;; ECA chat buffers follow a naming convention that includes the session ID
  ;; We search for buffers that match the ECA chat pattern for this session
  (when eca-session
    (let* ((session-id (and (fboundp 'eca--session-id)
                            (eca--session-id eca-session)))
           ;; ECA chat buffer names use angle brackets: <eca-chat:N:M>
           (chat-prefix (format "<eca-chat%s"
                               (if session-id
                                   (format ":%s" session-id)
                                 ""))))
      (seq-find (lambda (buf)
                  (string-prefix-p chat-prefix (buffer-name buf)))
                (buffer-list)))))

(defun beads-agent-eca--get-session-for-dir (dir)
  "Get the ECA session for directory DIR.
Uses ECA's workspace-based session lookup.
Returns the ECA session object if found, nil otherwise."
  (when (and (featurep 'eca)
             (fboundp 'eca-session))
    (let ((default-directory (expand-file-name dir)))
      (eca-session))))

(defun beads-agent-eca--wait-for-session (&optional timeout)
  "Wait for ECA session to be ready.
TIMEOUT is the maximum time to wait in seconds (default 5).
Returns the session if ready, nil if timed out."
  (let* ((timeout-secs (or timeout 5.0))
         (deadline (+ (float-time) timeout-secs))
         (session nil))
    (while (and (null session) (< (float-time) deadline))
      (setq session (and (fboundp 'eca-session) (eca-session)))
      (unless session
        (sit-for 0.1)))
    session))

;;; Backend Class

(defclass beads-agent-backend-eca (beads-agent-backend)
  ((name :initform "eca")
   (priority :initform 20)
   (description :initform "AI pair-programming via ECA server"))
  :documentation "Backend for ECA (Editor Code Assistant) integration.
Uses eca-emacs for AI-assisted development with multi-session support.")

;;; Protocol Implementation

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-eca))
  "Check if ECA is available.
Verifies the eca-emacs package is loaded and key functions exist."
  (and ;; Check eca package can be loaded
       (or (featurep 'eca)
           (require 'eca nil t))
       ;; Verify core functions exist
       (fboundp 'eca)
       (fboundp 'eca-stop)
       ;; Chat functions for interaction
       (or (featurep 'eca-chat)
           (require 'eca-chat nil t))
       (fboundp 'eca-chat-send-prompt)
       ;; Process functions for status checks
       (or (featurep 'eca-process)
           (require 'eca-process nil t))
       (fboundp 'eca-process-running-p)
       ;; ECA server should be available (either in PATH or auto-downloadable)
       (or (executable-find "eca")
           (fboundp 'eca-install-server))))

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-eca) _issue prompt)
  "Start ECA session with PROMPT.
ISSUE is unused as ECA works per-workspace via `default-directory'.
Returns cons cell (BACKEND-SESSION . BUFFER)."
  ;; Pre-flight checks with helpful error messages
  (unless (or (featurep 'eca) (require 'eca nil t))
    (error "ECA-emacs not found.  Install eca-emacs package"))
  (unless (or (featurep 'eca-chat) (require 'eca-chat nil t))
    (error "ECA chat module not found"))
  (unless (or (featurep 'eca-process) (require 'eca-process nil t))
    (error "ECA process module not found"))
  (require 'eca)
  (require 'eca-chat)
  (require 'eca-process)
  ;; default-directory is set by beads-agent-start to the project/worktree
  ;; ECA's session lookup uses this for workspace matching
  (let (eca-session
        buffer)
    (condition-case err
        (progn
          ;; Start ECA session - this may prompt for workspace
          (eca)
          ;; Wait for session to be ready
          (setq eca-session (beads-agent-eca--wait-for-session 5.0))
          (unless eca-session
            (error "Timeout waiting for ECA session"))
          ;; Wait for chat buffer BEFORE sending prompt
          ;; ECA's initialization is async - the chat buffer may not exist yet
          (setq buffer (or (beads-agent-eca--find-chat-buffer eca-session)
                          ;; If not found, try opening it
                          (when (fboundp 'eca-chat-open)
                            (eca-chat-open eca-session)
                            (beads-agent--wait-for-buffer
                             (lambda ()
                               (beads-agent-eca--find-chat-buffer eca-session))
                             5.0))))
          (unless buffer
            (error "Could not find ECA chat buffer"))
          ;; Now send initial prompt - buffer exists
          (eca-chat-send-prompt prompt))
      (error
       (error "Failed to start ECA: %s" (error-message-string err))))
    ;; Return (eca-session . buffer)
    (cons eca-session buffer)))

(cl-defmethod beads-agent-backend-stop
    ((backend beads-agent-backend-eca) session)
  "Stop ECA SESSION using BACKEND.
Since beads renames the buffer, we cannot use `eca-stop' directly.
We kill the buffer directly and terminate the process explicitly."
  ;; Get the renamed buffer from the session
  (when-let ((buffer (beads-agent-backend-get-buffer backend session)))
    (when (buffer-live-p buffer)
      ;; Kill process first to avoid zombies
      (when-let ((proc (get-buffer-process buffer)))
        (delete-process proc))
      (kill-buffer buffer)))
  ;; Also try to clean up via ECA's native stop if the backend-session is valid
  (when-let ((eca-session (oref session backend-session)))
    (ignore-errors
      (when (and (featurep 'eca) (fboundp 'eca-stop))
        ;; This may fail if the session is already gone, which is fine
        (eca-stop)))))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-eca) session)
  "Check if ECA SESSION is active.
Returns non-nil if the ECA server process is running for this session."
  (when-let ((eca-session (oref session backend-session)))
    (and (or (featurep 'eca-process)
             (require 'eca-process nil t))
         (fboundp 'eca-process-running-p)
         (eca-process-running-p eca-session))))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((backend beads-agent-backend-eca) session)
  "Switch to ECA buffer for SESSION using BACKEND."
  (if-let ((buffer (beads-agent-backend-get-buffer backend session)))
      (if (buffer-live-p buffer)
          (beads-agent--pop-to-buffer-other-window buffer)
        (user-error "Agent buffer has been killed"))
    ;; Fallback: try to open via ECA's native function
    (if-let ((eca-session (oref session backend-session)))
        (when (and (featurep 'eca-chat) (fboundp 'eca-chat-open))
          (eca-chat-open eca-session))
      (user-error "No buffer found for session %s" (oref session id)))))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-eca) session prompt)
  "Send PROMPT to active ECA session.
Uses ECA's chat module to deliver the prompt.
Sets `default-directory' from SESSION for workspace-based session lookup."
  (unless (and (featurep 'eca-chat) (fboundp 'eca-chat-send-prompt))
    (require 'eca-chat))
  ;; ECA uses workspace-based session lookup via default-directory
  (let ((default-directory (beads-agent-session-working-dir session)))
    (eca-chat-send-prompt prompt)))

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-eca))

(provide 'beads-agent-eca)
;;; beads-agent-eca.el ends here
