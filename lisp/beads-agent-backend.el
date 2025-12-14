;;; beads-agent-backend.el --- Core infrastructure for AI agent backends -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the core infrastructure for AI agent integration
;; in Beads.  It defines:
;;
;; - Abstract base class for backends (beads-agent-backend)
;; - Session class for tracking active sessions (beads-agent-session)
;; - Protocol (generic methods) that backends must implement
;; - Backend registry for registration and lookup
;; - Session storage and management
;;
;; Backend implementations should require this module and implement
;; the protocol methods.  They should NOT require beads-agent.el to
;; avoid circular dependencies.
;;
;; The user-facing API is in beads-agent.el, which requires this module.

;;; Code:

(require 'eieio)
(require 'cl-lib)

;;; EIEIO Classes

(defclass beads-agent-backend ()
  ((name
    :initarg :name
    :type string
    :documentation "Human-readable backend name (e.g., \"claude-code-ide\").")
   (priority
    :initarg :priority
    :type integer
    :initform 50
    :documentation "Priority for auto-selection (lower = preferred)."))
  :abstract t
  :documentation "Abstract base class for AI agent backends.
Subclasses must implement all generic methods defined below.")

(defclass beads-agent-session ()
  ((id
    :initarg :id
    :type string
    :documentation "Unique session identifier (UUID).")
   (issue-id
    :initarg :issue-id
    :type string
    :documentation "Issue ID this session is working on.")
   (backend-name
    :initarg :backend-name
    :type string
    :documentation "Name of the backend running this session.")
   (project-dir
    :initarg :project-dir
    :type string
    :documentation "Project directory for this session.")
   (worktree-dir
    :initarg :worktree-dir
    :initform nil
    :documentation "Git worktree directory if using worktrees.
When non-nil, this is the directory where work is performed.
The project-dir then refers to the main repo.")
   (started-at
    :initarg :started-at
    :type string
    :documentation "ISO 8601 timestamp when session started.")
   (backend-session
    :initarg :backend-session
    :initform nil
    :documentation "Backend-specific session object/handle."))
  :documentation "Represents an active AI agent session.")

;;; Session Accessors (for use by other modules without requiring EIEIO)

(defun beads-agent-session-backend-name (session)
  "Return the backend name for SESSION."
  (oref session backend-name))

(defun beads-agent-session-started-at (session)
  "Return the started-at timestamp for SESSION."
  (oref session started-at))

(defun beads-agent-session-worktree-dir (session)
  "Return the worktree directory for SESSION, or nil if not using worktrees."
  (oref session worktree-dir))

(defun beads-agent-session-working-dir (session)
  "Return the working directory for SESSION.
Returns worktree-dir if set, otherwise project-dir."
  (or (oref session worktree-dir)
      (oref session project-dir)))

;;; Backend Protocol (Generic Methods)

(cl-defgeneric beads-agent-backend-available-p (backend)
  "Return non-nil if BACKEND is available for use.
This should check if the required package is loaded and functional.")

(cl-defgeneric beads-agent-backend-start (backend issue prompt)
  "Start BACKEND working on ISSUE with PROMPT.
ISSUE is a beads-issue object.
PROMPT is a string to send to the agent.
Returns a backend-specific session handle or signals an error.")

(cl-defgeneric beads-agent-backend-stop (backend session)
  "Stop SESSION running on BACKEND.
SESSION is a beads-agent-session object.")

(cl-defgeneric beads-agent-backend-session-active-p (backend session)
  "Return non-nil if SESSION is still active on BACKEND.
SESSION is a beads-agent-session object.")

(cl-defgeneric beads-agent-backend-switch-to-buffer (backend session)
  "Switch to the buffer for SESSION on BACKEND.
SESSION is a beads-agent-session object.")

(cl-defgeneric beads-agent-backend-send-prompt (backend session prompt)
  "Send PROMPT to active SESSION on BACKEND.
SESSION is a beads-agent-session object.
PROMPT is a string to send to the agent.")

;;; Session Storage (In-Memory)

(defvar beads-agent--sessions (make-hash-table :test 'equal)
  "Hash table of active sessions, keyed by session ID.
Values are beads-agent-session objects.")

(defvar beads-agent--issue-sessions (make-hash-table :test 'equal)
  "Hash table mapping issue-id to list of session IDs.
Used for quick lookup of sessions by issue.")

(defvar beads-agent--backends nil
  "List of registered beads-agent-backend instances.
Backends are sorted by priority (lower = preferred).")

;;; State Change Hook

(defvar beads-agent-state-change-hook nil
  "Hook run when agent session state changes.
Each function is called with two arguments:
  ACTION  - Symbol: `started', `stopped', or `failed'
  SESSION - The `beads-agent-session' object (or partial info for `failed')

This hook is called after the state change is complete.
Use this to refresh UI elements like `beads-list' buffers.")

(defun beads-agent--run-state-change-hook (action session)
  "Run `beads-agent-state-change-hook' with ACTION and SESSION."
  (run-hook-with-args 'beads-agent-state-change-hook action session))

;;; Backend Registry Functions

(defun beads-agent--register-backend (backend)
  "Register BACKEND for use with beads-agent.
BACKEND must be an instance of a beads-agent-backend subclass."
  (unless (object-of-class-p backend 'beads-agent-backend)
    (error "Backend must be a beads-agent-backend instance"))
  ;; Remove existing backend with same name
  (setq beads-agent--backends
        (cl-remove-if (lambda (b)
                        (equal (oref b name) (oref backend name)))
                      beads-agent--backends))
  ;; Add new backend and sort by priority
  (push backend beads-agent--backends)
  (setq beads-agent--backends
        (sort beads-agent--backends
              (lambda (a b)
                (< (oref a priority) (oref b priority))))))

(defun beads-agent--get-backend (name)
  "Get backend by NAME, or nil if not found."
  (cl-find-if (lambda (b) (equal (oref b name) name))
              beads-agent--backends))

(defun beads-agent--get-available-backends ()
  "Return list of available backends, sorted by priority."
  (cl-remove-if-not #'beads-agent-backend-available-p
                    beads-agent--backends))

(defun beads-agent--backend-names ()
  "Return list of available backend names."
  (mapcar (lambda (b) (oref b name))
          (beads-agent--get-available-backends)))

;;; Session Management Functions

(defun beads-agent--generate-session-id ()
  "Generate unique session ID using timestamp and random."
  (format "session-%s-%04x"
          (format-time-string "%Y%m%d%H%M%S")
          (random 65536)))

(defun beads-agent--create-session (issue-id backend-name project-dir
                                             backend-session
                                             &optional worktree-dir)
  "Create and register a new session.
ISSUE-ID is the issue being worked on.
BACKEND-NAME is the name of the backend.
PROJECT-DIR is the main project directory.
BACKEND-SESSION is the backend-specific session handle.
WORKTREE-DIR is the git worktree directory, if using worktrees.
Returns the created beads-agent-session object."
  (let* ((session-id (beads-agent--generate-session-id))
         (session (beads-agent-session
                   :id session-id
                   :issue-id issue-id
                   :backend-name backend-name
                   :project-dir project-dir
                   :worktree-dir worktree-dir
                   :started-at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                   :backend-session backend-session)))
    ;; Store in sessions hash
    (puthash session-id session beads-agent--sessions)
    ;; Update issue-sessions mapping
    (let ((existing (gethash issue-id beads-agent--issue-sessions)))
      (puthash issue-id (cons session-id existing)
               beads-agent--issue-sessions))
    ;; Run state change hook
    (beads-agent--run-state-change-hook 'started session)
    session))

(defun beads-agent--destroy-session (session-id)
  "Remove session SESSION-ID from all tracking structures."
  (when-let ((session (gethash session-id beads-agent--sessions)))
    (let* ((issue-id (oref session issue-id))
           (issue-sessions (gethash issue-id beads-agent--issue-sessions)))
      ;; Remove from issue mapping
      (puthash issue-id (delete session-id issue-sessions)
               beads-agent--issue-sessions)
      ;; Clean up empty list
      (when (null (gethash issue-id beads-agent--issue-sessions))
        (remhash issue-id beads-agent--issue-sessions)))
    ;; Remove from sessions hash
    (remhash session-id beads-agent--sessions)
    ;; Run state change hook (after removal, but with session data)
    (beads-agent--run-state-change-hook 'stopped session)))

(defun beads-agent--get-session (session-id)
  "Get session by SESSION-ID, or nil if not found."
  (gethash session-id beads-agent--sessions))

(defun beads-agent--get-sessions-for-issue (issue-id)
  "Get all sessions for ISSUE-ID as a list of session objects."
  (let ((session-ids (gethash issue-id beads-agent--issue-sessions)))
    (delq nil (mapcar #'beads-agent--get-session session-ids))))

(defun beads-agent--get-all-sessions ()
  "Get all active sessions as a list."
  (let (sessions)
    (maphash (lambda (_k v) (push v sessions))
             beads-agent--sessions)
    sessions))

(defun beads-agent--session-active-p (session)
  "Check if SESSION is still active."
  (when-let ((backend (beads-agent--get-backend
                       (oref session backend-name))))
    (beads-agent-backend-session-active-p backend session)))

(provide 'beads-agent-backend)
;;; beads-agent-backend.el ends here
