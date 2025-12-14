;;; beads-agent-claude-code.el --- claude-code.el backend (placeholder) -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the claude-code.el backend for beads-agent.
;; Currently a placeholder - full implementation pending.
;;
;; claude-code.el is an alternative Claude Code integration for Emacs.

;;; Code:

(require 'beads-agent-backend)

;;; Backend Class

(defclass beads-agent-backend-claude-code (beads-agent-backend)
  ((name :initform "claude-code")
   (priority :initform 40))
  :documentation "Backend for claude-code.el integration (placeholder).")

;;; Protocol Implementation (Stubs)

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-claude-code))
  "Check if claude-code is available.
Returns nil because this backend is not yet implemented."
  ;; Placeholder: not implemented yet
  nil)

;; Declare vterm-environment to avoid compiler warnings
(defvar vterm-environment)

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-claude-code) _issue _prompt)
  "Start claude-code session (not yet implemented)."
  ;; When implemented, bind BD_NO_DAEMON=1 for both process-environment
  ;; and terminal emulators (vterm/eat) to disable bd daemon
  ;; (not supported in worktrees):
  ;;
  ;; (let* ((process-environment (cons "BD_NO_DAEMON=1" process-environment))
  ;;        ;; vterm uses alist format ("VAR" . "value")
  ;;        (vterm-environment (if (boundp 'vterm-environment)
  ;;                               (cons '("BD_NO_DAEMON" . "1")
  ;;                                     vterm-environment)
  ;;                             nil)))
  ;;   ...)
  (user-error "Claude-code backend not yet implemented"))

(cl-defmethod beads-agent-backend-stop
    ((_backend beads-agent-backend-claude-code) _session)
  "Stop claude-code session (not yet implemented)."
  (user-error "Claude-code backend not yet implemented"))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-claude-code) _session)
  "Check if claude-code session is active (not yet implemented)."
  nil)

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((_backend beads-agent-backend-claude-code) _session)
  "Switch to claude-code buffer (not yet implemented)."
  (user-error "Claude-code backend not yet implemented"))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-claude-code) _session _prompt)
  "Send prompt to claude-code (not yet implemented)."
  (user-error "Claude-code backend not yet implemented"))

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-claude-code))

(provide 'beads-agent-claude-code)
;;; beads-agent-claude-code.el ends here
