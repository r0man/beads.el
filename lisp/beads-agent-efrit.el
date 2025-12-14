;;; beads-agent-efrit.el --- efrit backend (placeholder) -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the efrit backend for beads-agent.
;; Currently a placeholder - full implementation pending.
;;
;; efrit is an Emacs-native AI agent framework.

;;; Code:

(require 'beads-agent-backend)

;;; Backend Class

(defclass beads-agent-backend-efrit (beads-agent-backend)
  ((name :initform "efrit")
   (priority :initform 20))
  :documentation "Backend for efrit integration (placeholder).")

;;; Protocol Implementation (Stubs)

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-efrit))
  "Check if efrit is available.
Returns nil because this backend is not yet implemented."
  ;; Placeholder: not implemented yet
  nil)

;; Declare vterm-environment to avoid compiler warnings
(defvar vterm-environment)

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-efrit) _issue _prompt)
  "Start efrit session (not yet implemented)."
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
  (user-error "Efrit backend not yet implemented"))

(cl-defmethod beads-agent-backend-stop
    ((_backend beads-agent-backend-efrit) _session)
  "Stop efrit session (not yet implemented)."
  (user-error "Efrit backend not yet implemented"))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-efrit) _session)
  "Check if efrit session is active (not yet implemented)."
  nil)

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((_backend beads-agent-backend-efrit) _session)
  "Switch to efrit buffer (not yet implemented)."
  (user-error "Efrit backend not yet implemented"))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-efrit) _session _prompt)
  "Send prompt to efrit (not yet implemented)."
  (user-error "Efrit backend not yet implemented"))

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-efrit))

(provide 'beads-agent-efrit)
;;; beads-agent-efrit.el ends here
