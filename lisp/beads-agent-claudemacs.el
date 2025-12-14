;;; beads-agent-claudemacs.el --- claudemacs backend (placeholder) -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the claudemacs backend for beads-agent.
;; Currently a placeholder - full implementation pending.
;;
;; claudemacs is a Claude integration for Emacs.

;;; Code:

(require 'beads-agent-backend)

;;; Backend Class

(defclass beads-agent-backend-claudemacs (beads-agent-backend)
  ((name :initform "claudemacs")
   (priority :initform 30))
  :documentation "Backend for claudemacs integration (placeholder).")

;;; Protocol Implementation (Stubs)

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-claudemacs))
  "Check if claudemacs is available.
Returns nil because this backend is not yet implemented."
  ;; Placeholder: not implemented yet
  nil)

;; Declare vterm-environment to avoid compiler warnings
(defvar vterm-environment)

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-claudemacs) _issue _prompt)
  "Start claudemacs session (not yet implemented)."
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
  (user-error "Claudemacs backend not yet implemented"))

(cl-defmethod beads-agent-backend-stop
    ((_backend beads-agent-backend-claudemacs) _session)
  "Stop claudemacs session (not yet implemented)."
  (user-error "Claudemacs backend not yet implemented"))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-claudemacs) _session)
  "Check if claudemacs session is active (not yet implemented)."
  nil)

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((_backend beads-agent-backend-claudemacs) _session)
  "Switch to claudemacs buffer (not yet implemented)."
  (user-error "Claudemacs backend not yet implemented"))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-claudemacs) _session _prompt)
  "Send prompt to claudemacs (not yet implemented)."
  (user-error "Claudemacs backend not yet implemented"))

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-claudemacs))

(provide 'beads-agent-claudemacs)
;;; beads-agent-claudemacs.el ends here
