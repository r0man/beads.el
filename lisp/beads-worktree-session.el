;;; beads-worktree-session.el --- Worktree buffer grouping for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module groups beads buffers (list, show) and agent sessions by
;; project directory.  It never touches an external session manager.
;;
;; Key principle: directory is identity, branch is metadata.
;;
;; Agent sessions are tracked in the in-house registry in
;; `beads-agent-backend.el' (`beads-agent--sessions'); this module groups
;; them, and the show/list buffers, under a per-directory
;; `beads-worktree-session' object for lifecycle management.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'beads-agent-backend)
(require 'beads-custom)

;; Forward declarations
(declare-function beads-git-find-project-root "beads-git")
(declare-function beads-git-get-branch "beads-git")

;;; Worktree Session Class (for buffer grouping)
;;
;; This class groups beads buffers (list, show) by project directory.
;; It is separate from agent sessions which are tracked via
;; beads-agent-session in beads-agent-backend.el.
;;
;; Key principle: Directory is identity, branch is metadata.

(defclass beads-worktree-session ()
  ((id
    :initarg :id
    :type string
    :documentation "Unique ID: abbreviated project path (e.g., ~/code/beads.el).")
   (project-dir
    :initarg :project-dir
    :type string
    :documentation "Project root directory (absolute).  THIS IS THE IDENTITY.
The session is keyed by this directory - same directory means same
logical session context, regardless of git branch.")
   (proj-name
    :initarg :proj-name
    :type string
    :documentation "Project name (basename of project-dir).
Used for display and buffer naming.")
   (branch
    :initarg :branch
    :initform nil
    :type (or string null)
    :documentation "Current git branch.  METADATA only - can change.
This is refreshed on access and does NOT affect session identity.
Different branches in the same directory share the same session.")
   (buffers
    :initarg :buffers
    :initform nil
    :type list
    :documentation "List of beads buffers (list, show) in this session.
These are buffer objects, not buffer names.")
   (agent-sessions
    :initarg :agent-sessions
    :initform nil
    :type list
    :documentation "List of agent sessions in this project.
These are beads-agent-session objects from beads-agent-backend.el."))
  :documentation "Represents a beads project/worktree context for buffer grouping.

Session identity is the project root directory (stable, unique).
Git branch is metadata that can change without affecting identity.

This enables:
- Same buffer reused when switching branches in same directory
- Different buffers for different worktrees (different directories)
- Agent sessions grouped with their project's buffers")

;;; Worktree Session Storage

(defvar beads-worktree-session--sessions nil
  "List of active beads-worktree-session objects.
Sessions are keyed by project directory (normalized path).")

;;; Worktree Session Accessors

(defun beads-worktree-session-empty-p (session)
  "Return non-nil if SESSION has no buffers and no agents.
An empty session can be cleaned up."
  (and (null (oref session buffers))
       (null (oref session agent-sessions))))

(defun beads-worktree-session-refresh-branch (session)
  "Refresh the branch metadata for SESSION.
Updates the branch slot with the current git branch.
Does NOT affect session identity."
  (let ((default-directory (oref session project-dir)))
    (oset session branch (beads-git-get-branch))))

(defun beads-worktree-session-add-buffer (session buffer)
  "Add BUFFER to SESSION's buffer list if not already present.
Returns non-nil if buffer was added, nil if already present."
  (unless (memq buffer (oref session buffers))
    (oset session buffers (cons buffer (oref session buffers)))
    t))

(defun beads-worktree-session-remove-buffer (session buffer)
  "Remove BUFFER from SESSION's buffer list.
Returns non-nil if buffer was removed, nil if not present."
  (when (memq buffer (oref session buffers))
    (oset session buffers (delq buffer (oref session buffers)))
    t))

;;; Worktree Session Management

(defun beads-worktree-session--normalize-directory (dir)
  "Normalize DIR for consistent comparison.
Returns expanded, truename path."
  (file-truename (expand-file-name dir)))

(defun beads-worktree-session--for-directory (dir)
  "Find worktree session for DIR.
Returns beads-worktree-session or nil."
  (let ((normalized-dir (beads-worktree-session--normalize-directory dir)))
    (cl-find-if
     (lambda (session)
       (equal (beads-worktree-session--normalize-directory
               (oref session project-dir))
              normalized-dir))
     beads-worktree-session--sessions)))

(defun beads-worktree-session--create (project-dir)
  "Create a new worktree session for PROJECT-DIR.
Returns the created beads-worktree-session object.
Gracefully handles the case where PROJECT-DIR does not exist
or is not a git repository."
  (let* ((normalized-dir (beads-worktree-session--normalize-directory project-dir))
         (proj-name (file-name-nondirectory
                     (directory-file-name normalized-dir)))
         (session-id (abbreviate-file-name normalized-dir))
         ;; Gracefully handle non-existent directories or non-git repos
         (branch (condition-case nil
                     (let ((default-directory normalized-dir))
                       (beads-git-get-branch))
                   (error nil)))
         (session (beads-worktree-session
                   :id session-id
                   :project-dir normalized-dir
                   :proj-name proj-name
                   :branch branch)))
    (push session beads-worktree-session--sessions)
    session))

(defun beads-worktree-session-ensure ()
  "Get or create worktree session for current context.
Uses project root directory as identity."
  (let ((project-dir (or (beads-git-find-project-root)
                         default-directory)))
    (or (beads-worktree-session--for-directory project-dir)
        (beads-worktree-session--create project-dir))))

(defun beads-worktree-session-cleanup (session)
  "Clean up SESSION if it's empty.
Removes the session from the global list if it has no buffers
and no agent sessions."
  (when (beads-worktree-session-empty-p session)
    (setq beads-worktree-session--sessions
          (delq session beads-worktree-session--sessions))))

(defun beads-worktree-session-for-buffer (buffer)
  "Find the worktree session containing BUFFER.
Return beads-worktree-session or nil."
  (cl-find-if
   (lambda (session)
     (memq buffer (oref session buffers)))
   beads-worktree-session--sessions))

;;; Agent Session Integration with Worktree Sessions
;;
;; Agent sessions are tracked within their parent worktree session.
;; This provides unified lifecycle management for all beads entities
;; in a project directory.

(defun beads-worktree-session--add-agent (agent-session)
  "Add AGENT-SESSION to appropriate worktree session.
Creates the worktree session if needed.  Uses agent's project-dir
for lookup."
  (when-let* ((project-dir (oref agent-session project-dir)))
    (let ((worktree-session
           (or (beads-worktree-session--for-directory project-dir)
               (beads-worktree-session--create project-dir))))
      (unless (memq agent-session (oref worktree-session agent-sessions))
        (oset worktree-session agent-sessions
              (cons agent-session (oref worktree-session agent-sessions)))))))

(defun beads-worktree-session--remove-agent (agent-session)
  "Remove AGENT-SESSION from its worktree session.
Also cleans up the worktree session if it becomes empty."
  (when-let* ((project-dir (oref agent-session project-dir))
              (worktree-session
               (beads-worktree-session--for-directory project-dir)))
    (oset worktree-session agent-sessions
          (delq agent-session (oref worktree-session agent-sessions)))
    (beads-worktree-session-cleanup worktree-session)))

(defun beads-worktree-session--agents-for-project (project-dir)
  "Return list of agent sessions for PROJECT-DIR.
Returns nil if no agents found."
  (when-let* ((worktree-session
               (beads-worktree-session--for-directory project-dir)))
    (oref worktree-session agent-sessions)))

(defun beads-worktree-session--for-agent (agent-session)
  "Find the worktree session containing AGENT-SESSION.
Return beads-worktree-session or nil."
  (cl-find-if
   (lambda (session)
     (memq agent-session (oref session agent-sessions)))
   beads-worktree-session--sessions))

;;; Hook Integration

(defun beads-worktree-session--on-agent-state-change (action session)
  "Group SESSION under its worktree session as agent state changes.
ACTION is `started', `stopped', or `failed'."
  (pcase action
    ('started (beads-worktree-session--add-agent session))
    ('stopped (beads-worktree-session--remove-agent session))))

(add-hook 'beads-agent-state-change-hook
          #'beads-worktree-session--on-agent-state-change)

(provide 'beads-worktree-session)
;;; beads-worktree-session.el ends here
