;;; beads-agent-efrit.el --- efrit backend for beads-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the efrit backend for beads-agent.
;; Efrit is an Emacs-native AI agent framework that uses Claude.
;;
;; The backend supports two efrit interfaces:
;; - efrit-do: Async command execution with progress buffer
;; - efrit-agent: REPL-style interactive session (not yet implemented)
;;
;; Public efrit APIs used:
;; - `efrit-do' - Start async execution, returns session
;; - `efrit-do-show-progress' - Show progress buffer
;; - `efrit-session-active' - Get active session
;; - `efrit-session-cancel' - Cancel session
;; - `efrit-session-id' - Get session ID (struct accessor)
;; - `efrit-session-buffer' - Get session buffer (struct accessor)
;; - `efrit-session-status' - Get session status (struct accessor)
;; - `efrit-progress-inject' - Send follow-up guidance

;;; Code:

(require 'beads-agent-backend)

;;; External Function Declarations (Public APIs Only)

;; From efrit-do.el
(declare-function efrit-do "efrit-do")
(declare-function efrit-do-show-progress "efrit-do")

;; From efrit-session-core.el (struct accessors)
(declare-function efrit-session-active "efrit-session-core")
(declare-function efrit-session-cancel "efrit-session-core")
(declare-function efrit-session-id "efrit-session-core")
(declare-function efrit-session-buffer "efrit-session-core")
(declare-function efrit-session-status "efrit-session-core")

;; From efrit-progress.el
(declare-function efrit-progress-inject "efrit-progress")
(declare-function efrit-progress-get-buffer "efrit-progress")

;;; Customization

(defgroup beads-agent-efrit nil
  "Efrit backend for beads-agent."
  :group 'beads-agent
  :prefix "beads-agent-efrit-")

(defcustom beads-agent-efrit-interface 'efrit-do
  "Which efrit interface to use for agent sessions.
Currently only `efrit-do' is supported."
  :type '(choice (const :tag "efrit-do (async)" efrit-do)
                 (const :tag "efrit-agent (REPL, not yet implemented)" efrit-agent))
  :group 'beads-agent-efrit)

;;; Backend Class

(defclass beads-agent-backend-efrit (beads-agent-backend)
  ((name :initform "efrit")
   (priority :initform 20)
   (description :initform "Emacs-native AI agent framework"))
  :documentation "Backend for efrit integration.
Provides async command execution via efrit-do with progress tracking.")

;;; Protocol Implementation

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-efrit))
  "Check if efrit is available.
Verifies efrit-do package is loadable and key functions exist."
  (or (featurep 'efrit)
      (require 'efrit nil t)))

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-efrit) _issue prompt)
  "Start efrit session with PROMPT.
ISSUE is ignored as efrit works per-project.
The working directory is determined by the caller (may be a worktree).
Returns cons cell (BACKEND-SESSION . BUFFER)."
  ;; Pre-flight checks
  (unless (or (featurep 'efrit-do) (require 'efrit-do nil t))
    (error "Efrit not found.  Install from: https://github.com/steveyegge/efrit"))
  (require 'efrit-do)
  (require 'efrit-session-core)
  (require 'efrit-progress)
  ;; Bind BD_NO_DAEMON=1 to disable bd daemon (not supported in worktrees)
  (let ((process-environment (cons "BD_NO_DAEMON=1" process-environment)))
    ;; Start efrit-do with the prompt
    (condition-case err
        (let* ((efrit-session (efrit-do prompt))
               (buffer (when efrit-session
                         (efrit-progress-get-buffer (efrit-session-id efrit-session)))))
          ;; Return (efrit-session . buffer)
          (cons efrit-session buffer))
      (error
       (error "Failed to start efrit: %s" (error-message-string err))))))

(cl-defmethod beads-agent-backend-stop
    ((_backend beads-agent-backend-efrit) session)
  "Stop efrit SESSION."
  (require 'efrit-session-core)
  (let ((efrit-session (oref session backend-session)))
    (when efrit-session
      (efrit-session-cancel efrit-session))))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-efrit) session)
  "Check if efrit SESSION is still active."
  (require 'efrit-session-core)
  (let ((efrit-session (oref session backend-session)))
    (and efrit-session
         ;; Check if this is still the active session
         (when-let ((active (efrit-session-active)))
           (equal (efrit-session-id active)
                  (efrit-session-id efrit-session)))
         ;; Check session status is running
         (memq (efrit-session-status efrit-session) '(active running)))))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((backend beads-agent-backend-efrit) session)
  "Switch to efrit progress buffer for SESSION using BACKEND."
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
    ((_backend beads-agent-backend-efrit) session prompt)
  "Send PROMPT to efrit SESSION as follow-up guidance.
Uses efrit's injection mechanism to add guidance to the active session."
  (require 'efrit-progress)
  (let ((efrit-session (oref session backend-session)))
    (if efrit-session
        ;; Inject guidance message into the session
        (efrit-progress-inject (efrit-session-id efrit-session)
                               'guidance
                               prompt)
      ;; If no session, queue a new command
      (require 'efrit-do)
      (efrit-do prompt))))

;; Note: beads-agent-backend-get-buffer uses the default implementation
;; from beads-agent-backend.el which returns the stored session buffer.

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-efrit))

(provide 'beads-agent-efrit)
;;; beads-agent-efrit.el ends here
