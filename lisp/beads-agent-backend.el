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
;; - Session query functions (storage delegated to sesman)
;;
;; Session Storage Architecture:
;;
;; Sessions are stored in sesman (Session Manager) rather than local
;; hash tables.  This provides context-aware session selection and
;; standard Emacs session management UI.  The flow is:
;;
;; 1. `beads-agent--create-session' creates session object and runs hook
;; 2. Hook handler in beads-sesman.el registers session with sesman
;; 3. Query functions iterate `(sesman-sessions beads-sesman-system)' to find sessions
;; 4. `beads-agent--destroy-session' runs hook to unregister from sesman
;;
;; Backend implementations should require this module and implement
;; the protocol methods.  They should NOT require beads-agent.el to
;; avoid circular dependencies.
;;
;; The user-facing API is in beads-agent.el, which requires this module.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'sesman)
(require 'beads-buffer)

;; Forward declaration - defined in beads-sesman.el (required at end of file)
(defvar beads-sesman-system)

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
    :documentation "Priority for auto-selection (lower = preferred).")
   (description
    :initarg :description
    :type string
    :initform ""
    :documentation "Brief description of what this backend does.
Shown in completion annotations to help users choose backends."))
  :abstract t
  :documentation "Abstract base class for AI agent backends.
Subclasses must implement all generic methods defined below.")

(defclass beads-agent-session ()
  ((id
    :initarg :id
    :type string
    :documentation "Unique session identifier.
Format: `proj-name#N' (e.g., beads.el#1) for directory-bound sessions.
Legacy format: `issue-id#N' for issue-bound sessions (deprecated).")
   (project-dir
    :initarg :project-dir
    :type string
    :documentation "Project root directory.  THIS IS THE SESSION IDENTITY.
The session is keyed by this directory - same directory means same
logical session context, regardless of git branch or current issue.
Different directories (including worktrees) are different sessions.")
   (proj-name
    :initarg :proj-name
    :initform nil
    :type (or string null)
    :documentation "Project name for display and session ID.
Typically the basename of `project-dir' (e.g., \"beads.el\").
Used in buffer names and session IDs for human readability.")
   (instance-number
    :initarg :instance-number
    :initform 1
    :type integer
    :documentation "Instance number for this project (1, 2, 3...).
Multiple agents can run in the same project; this distinguishes them.
The session ID is `proj-name#instance-number'.")
   (current-issue
    :initarg :current-issue
    :initform nil
    :type (or string null)
    :documentation "Current focus issue ID (can change mid-session).
This is the issue the agent is currently focused on.  Unlike the old
`issue-id' slot which was the session identity, this is just metadata
that can change as the agent works on different issues.")
   (touched-issues
    :initarg :touched-issues
    :initform nil
    :type list
    :documentation "List of issue IDs worked on this session.
Tracks all issues that have been the `current-issue' focus.
Useful for understanding what work happened in a session.")
   ;; DEPRECATED: issue-id slot - use current-issue instead
   ;; Kept for backward compatibility during migration
   (issue-id
    :initarg :issue-id
    :initform nil
    :type (or string null)
    :documentation "DEPRECATED: Use `current-issue' instead.
This slot is kept for backward compatibility during migration to
directory-bound sessions.  New code should use `current-issue' for
focus and `project-dir' for identity.")
   (issue-title
    :initarg :issue-title
    :initform nil
    :type (or string null)
    :documentation "Title of the issue being worked on.
Used in buffer names to help identify the session's purpose.")
   (backend-name
    :initarg :backend-name
    :type string
    :documentation "Name of the backend running this session.")
   (agent-type-name
    :initarg :agent-type-name
    :initform nil
    :documentation "Name of the agent type for this session (e.g., \"Task\", \"Review\").
When nil, the session was started without specifying an agent type.")
   ;; DEPRECATED: worktree-dir slot - directory is now identity
   ;; Different worktrees are simply different project-dir values
   (worktree-dir
    :initarg :worktree-dir
    :initform nil
    :documentation "DEPRECATED: Use `project-dir' as the directory identity.
In the new directory-bound model, worktrees are just different
directories and thus have different `project-dir' values.
This slot is kept for backward compatibility.")
   (started-at
    :initarg :started-at
    :type string
    :documentation "ISO 8601 timestamp when session started.")
   (backend-session
    :initarg :backend-session
    :initform nil
    :documentation "Backend-specific session object/handle.
This is the return value from `beads-agent-backend-start'.
In sesman sessions, this becomes the second element of the tuple:
  (session-name BACKEND-SESSION beads-agent-session)
Backends use this to track their internal session state.
The value is opaque to beads-sesman and passed through unchanged.

This slot MAY BE NIL in the following cases:
- During async session startup before the backend connects
- If the backend doesn't maintain connection state (e.g., terminal-based)
- After session cleanup if the backend was already disconnected
Code that accesses this slot should handle nil gracefully.")
   (buffer
    :initarg :buffer
    :initform nil
    :documentation "The Emacs buffer for this session.
After the backend creates its buffer, it is renamed to the beads format
and stored here for efficient lookup.  Buffer naming follows the format:
  *beads-agent[PROJECT-NAME][TYPE#N]* (directory-bound)
  *beads-agent[ISSUE-ID][TYPE#N]* (legacy, deprecated)
This slot is set after session creation when the buffer is renamed."))
  :documentation "Represents an active AI agent session.

In the directory-bound model (new):
  - `project-dir' is THE IDENTITY (same dir = same session context)
  - `current-issue' is the focus (can change mid-session)
  - `touched-issues' tracks all issues worked on
  - Session ID format: `proj-name#N'

In the issue-bound model (deprecated):
  - `issue-id' was the identity
  - Session ID format: `issue-id#N'

The directory-bound model allows agents to persist across issue
switches, enabling natural multi-issue work within a single session.")

;;; Session Accessors (for use by other modules without requiring EIEIO)

(defun beads-agent-session-backend-name (session)
  "Return the backend name for SESSION."
  (oref session backend-name))

(defun beads-agent-session-type-name (session)
  "Return the agent type name for SESSION, or nil if not set."
  (oref session agent-type-name))

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

(defun beads-agent-session-buffer (session)
  "Return the Emacs buffer for SESSION, or nil if not set.
This returns the buffer stored in the session's buffer slot,
which is set after the backend buffer is renamed to beads format."
  (oref session buffer))

(defun beads-agent-session-set-buffer (session buffer)
  "Set the Emacs buffer for SESSION to BUFFER.
This should be called after renaming the backend buffer to beads format."
  (oset session buffer buffer))

;;; Directory-Bound Session Accessors
;;
;; These accessors support the new directory-bound session model where
;; project-dir is the identity and current-issue is changeable focus.

(defun beads-agent-session-project-dir (session)
  "Return the project directory for SESSION.
This is THE IDENTITY of the session in the directory-bound model."
  (oref session project-dir))

(defun beads-agent-session-project-name (session)
  "Return the project name for SESSION, or nil if not set.
This is a short display name derived from `project-dir'."
  (oref session proj-name))

(defun beads-agent-session-instance-number (session)
  "Return the instance number for SESSION.
Multiple agents in the same project have different instance numbers."
  (oref session instance-number))

(defun beads-agent-session-current-issue (session)
  "Return the current focus issue for SESSION, or nil.
This can change mid-session as the agent works on different issues."
  (oref session current-issue))

(defun beads-agent-session-set-current-issue (session issue-id)
  "Set SESSION's current focus to ISSUE-ID.
Also adds ISSUE-ID to touched-issues if not already present.
ISSUE-ID can be nil to clear the focus."
  (oset session current-issue issue-id)
  (when issue-id
    (beads-agent-session-add-touched-issue session issue-id)))

(defun beads-agent-session-touched-issues (session)
  "Return list of issue IDs that SESSION has worked on."
  (oref session touched-issues))

(defun beads-agent-session-add-touched-issue (session issue-id)
  "Add ISSUE-ID to SESSION's touched-issues if not already present.
Returns non-nil if ISSUE-ID was added, nil if already present."
  (unless (member issue-id (oref session touched-issues))
    (oset session touched-issues
          (cons issue-id (oref session touched-issues)))
    t))

(defun beads-agent-session-issue-id (session)
  "Return the issue ID for SESSION (deprecated).
For backward compatibility.  New code should use
`beads-agent-session-current-issue' instead."
  (or (oref session current-issue)
      (oref session issue-id)))

(defun beads-agent-session-issue-title (session)
  "Return the issue title for SESSION, or nil if not set."
  (oref session issue-title))

;;; Backend Protocol (Generic Methods)

(cl-defgeneric beads-agent-backend-available-p (backend)
  "Return non-nil if BACKEND is available for use.
This should check if the required package is loaded and functional.")

(cl-defgeneric beads-agent-backend-start (backend issue prompt)
  "Start BACKEND working on ISSUE with PROMPT.
ISSUE is a beads-issue object.
PROMPT is a string to send to the agent.

Returns a cons cell (BACKEND-SESSION . BUFFER) where:
- BACKEND-SESSION is a backend-specific session handle (can be nil)
- BUFFER is the Emacs buffer for the agent session

The BACKEND-SESSION is stored in the `backend-session' slot of the
`beads-agent-session' object.  The BUFFER is stored in the `buffer'
slot after being renamed to the beads naming convention.

If BUFFER is nil, the caller will not be able to find the agent's
buffer.  Most backends should return the buffer they create.

Signals an error on failure.")

(cl-defgeneric beads-agent-backend-stop (backend session)
  "Stop SESSION running on BACKEND.
SESSION is a beads-agent-session object.")

(cl-defgeneric beads-agent-backend-stop-async (backend session callback)
  "Stop SESSION running on BACKEND asynchronously.
SESSION is a beads-agent-session object.
CALLBACK is called with no arguments when stop completes.

This method should return immediately without blocking.
The actual stop operation may happen asynchronously.

Default implementation calls sync `beads-agent-backend-stop' via
`run-at-time' to avoid blocking the UI, then invokes CALLBACK."
  ;; Default implementation uses run-at-time to make sync stop non-blocking
  (let ((session-id (oref session id)))
    (run-at-time
     0 nil
     (lambda ()
       ;; Defensive check: verify session is still registered before stopping.
       ;; This prevents errors when timer fires after session is destroyed.
       ;; We re-lookup by ID rather than using the captured session object
       ;; to ensure we have the current state.
       (when (beads-agent--get-session session-id)
         (beads-agent-backend-stop backend session))
       (when callback
         (funcall callback))))))

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

(cl-defgeneric beads-agent-backend-session-name (backend session)
  "Return display name for SESSION on BACKEND.
SESSION is a beads-agent-session object.
The returned name is used as the sesman session identifier.

Default implementation returns \"<session-id>@<working-dir>\" format,
where session-id is in `issue-id#N' format for numbered sessions.
Backends may override this to use custom naming schemes."
  ;; Default implementation uses session-id@working-dir format
  ;; backend arg exists for method dispatch but isn't used in default
  (ignore backend)
  (let ((session-id (oref session id))
        (working-dir (or (oref session worktree-dir)
                         (oref session project-dir))))
    (format "%s@%s" session-id (abbreviate-file-name working-dir))))

(cl-defgeneric beads-agent-backend-get-buffer (backend session)
  "Return the buffer for SESSION on BACKEND, or nil if not available.
SESSION is a beads-agent-session object.
Used by sesman to link the session buffer for context-aware selection.

Default implementation returns the buffer stored in the session's
`buffer' slot.  This is set automatically when the session is created
from the return value of `beads-agent-backend-start'.

Backends generally don't need to override this method."
  ;; Default: return stored buffer
  (ignore backend)
  (beads-agent-session-buffer session))

;;; Backend Registry

(defvar beads-agent--backends nil
  "List of registered beads-agent-backend instances.
Backends are sorted by priority (lower = preferred).")

(defvar beads-agent--issue-outcomes (make-hash-table :test #'equal)
  "Hash table mapping issue-id to last agent outcome.
Values are symbols: `finished', `failed', or nil.
This is used by `beads-list' to show colored status indicators.")

(defun beads-agent--get-issue-outcome (issue-id)
  "Get the last agent outcome for ISSUE-ID.
Returns `finished', `failed', or nil if no outcome recorded."
  (gethash issue-id beads-agent--issue-outcomes))

;;; State Change Hook

(defvar beads-agent-state-change-hook nil
  "Hook run when agent session state changes.
Each function is called with two arguments:
  ACTION  - Symbol: `started', `stopped', or `failed'
  SESSION - The `beads-agent-session' object (or partial info for `failed')

This hook is called after the state change is complete.
Use this to refresh UI elements like `beads-list' buffers.")

(defun beads-agent--run-state-change-hook (action session)
  "Run `beads-agent-state-change-hook' with ACTION and SESSION.
Also records the outcome in `beads-agent--issue-outcomes' for UI display.
Outcomes are stored as (letter . outcome) cons cells where letter is the
first character of the agent type name (T/R/P/Q/C) and outcome is `finished'
or `failed'.  For backward compatibility, outcome may be just the symbol."
  ;; Record outcome for UI display
  (when-let ((issue-id (and session (oref session issue-id))))
    (let* ((type-name (oref session agent-type-name))
           (letter (if type-name
                       (substring type-name 0 1)
                     "●")))
      (pcase action
        ('started
         ;; Clear previous outcome when starting fresh
         (remhash issue-id beads-agent--issue-outcomes))
        ('stopped
         ;; Clear outcome when stopped - indicators only show for active agents
         (remhash issue-id beads-agent--issue-outcomes))
        ('failed
         (puthash issue-id (cons letter 'failed) beads-agent--issue-outcomes)))))
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

(defun beads-agent--get-all-backends ()
  "Return list of all registered backends, sorted by priority.
Unlike `beads-agent--get-available-backends', this returns all
backends including those that are currently unavailable."
  beads-agent--backends)

(defun beads-agent--backend-names ()
  "Return list of available backend names."
  (mapcar (lambda (b) (oref b name))
          (beads-agent--get-available-backends)))

;;; Buffer Acquisition Helpers

(defun beads-agent--wait-for-buffer (finder &optional timeout interval)
  "Wait for FINDER function to return a non-nil buffer.
FINDER is called repeatedly until it returns a buffer or TIMEOUT
seconds elapse.  TIMEOUT defaults to 5 seconds.  INTERVAL is the
delay between retries, defaulting to 0.1 seconds.

Returns the buffer if found, nil if timed out.

This handles the race condition where backend processes may not
have created their buffer immediately after being spawned."
  (let* ((timeout-secs (or timeout 5.0))
         (interval-secs (or interval 0.1))
         (deadline (+ (float-time) timeout-secs))
         (buffer nil))
    (while (and (null buffer) (< (float-time) deadline))
      (setq buffer (funcall finder))
      (unless buffer
        (sit-for interval-secs)))
    buffer))

;;; Session Management Functions
;;
;; Sessions are stored in sesman (via beads-sesman.el hook handler).
;; The hook `beads-agent-state-change-hook' triggers registration/unregistration.
;; Query functions iterate sesman sessions to find beads-agent-session objects.

(defun beads-agent--session-number-from-id (session-id)
  "Extract session number from SESSION-ID in `issue-id#N' format.
Returns N as an integer, or nil if SESSION-ID doesn't match the format."
  (when (and session-id (string-match "#\\([0-9]+\\)\\'" session-id))
    (string-to-number (match-string 1 session-id))))

(defun beads-agent--next-session-number (issue-id)
  "Find the next available session number for ISSUE-ID.
Scans all existing sessions for ISSUE-ID and returns max+1.
Returns 1 if no sessions exist for the issue.
Session numbers never reuse - gaps in numbering are ignored."
  (let ((max-num 0))
    (dolist (session (beads-agent--get-sessions-for-issue issue-id))
      (when-let ((num (beads-agent--session-number-from-id (oref session id))))
        (setq max-num (max max-num num))))
    (1+ max-num)))

(defun beads-agent--generate-session-id (issue-id)
  "Generate unique session ID for ISSUE-ID.
Returns ID in `issue-id#N' format where N is the next available number.
DEPRECATED: Use `beads-agent--generate-project-session-id' instead."
  (format "%s#%d" issue-id (beads-agent--next-session-number issue-id)))

(defun beads-agent--generate-project-session-id (project-dir)
  "Generate unique session ID for PROJECT-DIR.
Returns ID in `project-name#N' format where N is the next available number."
  (let ((project-name (beads-agent--derive-project-name project-dir)))
    (format "%s#%d" project-name (beads-agent--next-project-instance-number project-dir))))

(defun beads-agent--create-session (issue-id backend-name project-dir
                                             backend-session
                                             &optional worktree-dir agent-type-name
                                             issue-title)
  "Create and register a new session (legacy issue-bound).
ISSUE-ID is the issue being worked on.
BACKEND-NAME is the name of the backend.
PROJECT-DIR is the main project directory.
BACKEND-SESSION is the backend-specific session handle.
WORKTREE-DIR is the git worktree directory, if using worktrees.
AGENT-TYPE-NAME is the name of the agent type (e.g., \"Task\", \"Review\").
ISSUE-TITLE is the title of the issue (used in buffer names).
Returns the created beads-agent-session object.

DEPRECATED: Use `beads-agent--create-project-session' for new code.

Note: Session storage is handled by `beads-agent-state-change-hook'.
The hook handler in beads-sesman.el registers the session with sesman."
  (let* ((session-id (beads-agent--generate-session-id issue-id))
         (normalized-dir (expand-file-name project-dir))
         (project-name (beads-agent--derive-project-name normalized-dir))
         (instance-num (beads-agent--next-project-instance-number normalized-dir))
         (session (beads-agent-session
                   :id session-id
                   :issue-id issue-id
                   :issue-title issue-title
                   :backend-name backend-name
                   :agent-type-name agent-type-name
                   :project-dir normalized-dir
                   :proj-name project-name
                   :instance-number instance-num
                   :worktree-dir worktree-dir
                   :started-at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                   :backend-session backend-session)))
    ;; Run state change hook - this triggers sesman registration
    (beads-agent--run-state-change-hook 'started session)
    session))

(defun beads-agent--create-project-session (project-dir backend-name backend-session
                                                        &optional initial-issue agent-type-name)
  "Create and register a new directory-bound session.
PROJECT-DIR is the project root directory (THE IDENTITY).
BACKEND-NAME is the name of the backend.
BACKEND-SESSION is the backend-specific session handle.
INITIAL-ISSUE is the optional initial focus issue ID.
AGENT-TYPE-NAME is the name of the agent type (e.g., \"Task\", \"Review\").
Returns the created beads-agent-session object.

The session is keyed by PROJECT-DIR, not issue.  Multiple issues can be
worked on within a single session.  INITIAL-ISSUE (if provided) becomes
the current focus and is added to touched-issues.

Note: Session storage is handled by `beads-agent-state-change-hook'.
The hook handler in beads-sesman.el registers the session with sesman."
  (let* ((normalized-dir (expand-file-name project-dir))
         (project-name (beads-agent--derive-project-name normalized-dir))
         (instance-num (beads-agent--next-project-instance-number normalized-dir))
         (session-id (format "%s#%d" project-name instance-num))
         (session (beads-agent-session
                   :id session-id
                   :project-dir normalized-dir
                   :proj-name project-name
                   :instance-number instance-num
                   :current-issue initial-issue
                   :touched-issues (when initial-issue (list initial-issue))
                   ;; Backward compatibility: set issue-id to initial-issue
                   :issue-id initial-issue
                   :backend-name backend-name
                   :agent-type-name agent-type-name
                   :started-at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                   :backend-session backend-session)))
    ;; Run state change hook - this triggers sesman registration
    (beads-agent--run-state-change-hook 'started session)
    session))

(defun beads-agent--destroy-session (session-id)
  "Remove session SESSION-ID from tracking.
Looks up the session from sesman and runs the state change hook,
which triggers unregistration from sesman."
  (when-let ((session (beads-agent--get-session session-id)))
    ;; Run state change hook - this triggers sesman unregistration
    (beads-agent--run-state-change-hook 'stopped session)))

(defun beads-agent--get-session (session-id)
  "Get session by SESSION-ID, or nil if not found.
Searches all sesman sessions for a beads-agent-session with matching ID."
  (cl-loop for sesman-session in (sesman-sessions beads-sesman-system)
           for beads-session = (nth 2 sesman-session)
           when (and beads-session
                     (equal (oref beads-session id) session-id))
           return beads-session))

(defun beads-agent--get-sessions-for-issue (issue-id)
  "Get all sessions for ISSUE-ID as a list of session objects."
  (cl-loop for sesman-session in (sesman-sessions beads-sesman-system)
           for beads-session = (nth 2 sesman-session)
           when (and beads-session
                     (equal (oref beads-session issue-id) issue-id))
           collect beads-session))

(defun beads-agent--get-all-sessions ()
  "Get all active sessions as a list of beads-agent-session objects."
  (cl-loop for sesman-session in (sesman-sessions beads-sesman-system)
           for beads-session = (nth 2 sesman-session)
           when beads-session
           collect beads-session))

(defun beads-agent--current-session ()
  "Get current session based on buffer context.
Uses sesman's context-aware session selection."
  (when-let ((sesman-session (sesman-current-session beads-sesman-system)))
    (nth 2 sesman-session)))

(defun beads-agent--session-active-p (session)
  "Check if SESSION is still active."
  (when-let ((backend (beads-agent--get-backend
                       (oref session backend-name))))
    (beads-agent-backend-session-active-p backend session)))

;;; Directory-Bound Session Management
;;
;; These functions support the new directory-bound session model where
;; sessions are keyed by project directory rather than issue ID.

(defun beads-agent--derive-project-name (project-dir)
  "Derive a project name from PROJECT-DIR.
Returns the basename of the directory (e.g., \"beads.el\" from
\"/home/user/code/beads.el\")."
  (file-name-nondirectory (directory-file-name (expand-file-name project-dir))))

(defun beads-agent--get-sessions-for-project (project-dir)
  "Get all sessions for PROJECT-DIR as a list of session objects.
PROJECT-DIR is normalized for comparison."
  (let ((normalized-dir (file-truename (expand-file-name project-dir))))
    (cl-loop for sesman-session in (sesman-sessions beads-sesman-system)
             for beads-session = (nth 2 sesman-session)
             when (and beads-session
                       (equal (file-truename
                               (expand-file-name (oref beads-session project-dir)))
                              normalized-dir))
             collect beads-session)))

(defun beads-agent--next-project-instance-number (project-dir)
  "Find the next available instance number for PROJECT-DIR.
Scans all existing sessions for PROJECT-DIR and returns max+1.
Returns 1 if no sessions exist for the project.
Instance numbers never reuse - gaps in numbering are ignored."
  (let ((max-num 0))
    (dolist (session (beads-agent--get-sessions-for-project project-dir))
      (let ((num (oref session instance-number)))
        (when (and num (integerp num))
          (setq max-num (max max-num num)))))
    (1+ max-num)))

(defun beads-agent--get-sessions-touching-issue (issue-id)
  "Get sessions that have touched ISSUE-ID.
Returns sessions where ISSUE-ID is in `touched-issues'."
  (cl-loop for sesman-session in (sesman-sessions beads-sesman-system)
           for beads-session = (nth 2 sesman-session)
           when (and beads-session
                     (member issue-id (oref beads-session touched-issues)))
           collect beads-session))

(defun beads-agent--get-sessions-focused-on-issue (issue-id)
  "Get sessions currently focused on ISSUE-ID.
Returns sessions where `current-issue' equals ISSUE-ID."
  (cl-loop for sesman-session in (sesman-sessions beads-sesman-system)
           for beads-session = (nth 2 sesman-session)
           when (and beads-session
                     (equal (oref beads-session current-issue) issue-id))
           collect beads-session))

;;; Buffer Naming
;;
;; Directory-bound buffer names (new):
;;   *beads-agent[PROJECT-NAME][TYPE#N]*
;;
;; Where:
;;   PROJECT-NAME - The project name (e.g., "beads.el")
;;   TYPE - The agent type name (e.g., "Task", "Review", "Plan")
;;   N - Instance number for this (project, type) combination
;;
;; Examples:
;;   *beads-agent[beads.el][Task#1]*
;;   *beads-agent[beads.el][Plan#2]*
;;
;; Legacy issue-bound buffer names (deprecated):
;;   *beads-agent[ISSUE-ID][TYPE#N]*
;;
;; Where:
;;   ISSUE-ID - The issue being worked on (e.g., "beads.el-xrrt")
;;   TYPE - The agent type name (e.g., "Task", "Review", "Plan")
;;   N - Instance number per (issue, type) combination
;;
;; Examples (legacy):
;;   *beads-agent[beads.el-xrrt][Task#1]*
;;   *beads-agent[beads.el-xrrt][Plan#1]*
;;
;; Note: Both formats are syntactically identical ([NAME][TYPE#N]).
;; The distinction is semantic: PROJECT-NAME is short (e.g., "beads.el")
;; while ISSUE-ID includes issue suffix (e.g., "beads.el-xrrt").
;;
;; Instance Number Schemes:
;;
;; There are TWO independent instance numbering schemes:
;;
;; 1. Session Instance Number (stored in session's `instance-number' slot):
;;    - Counter: beads-agent--project-instance-counters
;;    - Key: normalized project-dir path
;;    - Used for: Session IDs (e.g., "beads.el#1", "beads.el#2")
;;    - Increments: Per project, regardless of agent type
;;
;; 2. Buffer Instance Number (in buffer name's TYPE#N):
;;    - Counter: beads-agent--typed-instance-counters
;;    - Key: (project-name . type-name) cons cell
;;    - Used for: Buffer names (e.g., "*beads-agent[beads.el][Task#1]*")
;;    - Increments: Per (project, type) combination
;;
;; Example scenario with 3 sessions in same project:
;;   Session "beads.el#1" (Task)  -> buffer "*beads-agent[beads.el][Task#1]*"
;;   Session "beads.el#2" (Plan)  -> buffer "*beads-agent[beads.el][Plan#1]*"
;;   Session "beads.el#3" (Task)  -> buffer "*beads-agent[beads.el][Task#2]*"
;;
;; The session numbers (1, 2, 3) are sequential per-project.
;; The buffer numbers (Task#1, Plan#1, Task#2) are sequential per-type.

(defvar beads-agent--typed-instance-counters (make-hash-table :test #'equal)
  "Hash table mapping (name . type-name) to next instance number.
Used to track instance numbers per (name, type) combination, where
name can be either an issue-id (legacy) or project-name (directory-bound).")

(defun beads-agent--reset-typed-instance-counters ()
  "Reset all typed instance counters.
Primarily used for testing."
  (clrhash beads-agent--typed-instance-counters))

(defun beads-agent--next-typed-instance-number (name type-name)
  "Get the next instance number for NAME and TYPE-NAME combination.
NAME can be an issue-id (legacy) or project-name (directory-bound).
Returns 1 for the first instance, 2 for the second, etc.
Instance numbers are tracked independently for each (name, type) pair."
  (let* ((key (cons name type-name))
         (current (gethash key beads-agent--typed-instance-counters 0))
         (next (1+ current)))
    (puthash key next beads-agent--typed-instance-counters)
    next))

(defun beads-agent--peek-typed-instance-number (name type-name)
  "Peek at the next instance number without incrementing.
Returns what the next call to `beads-agent--next-typed-instance-number'
would return for NAME and TYPE-NAME."
  (1+ (gethash (cons name type-name)
               beads-agent--typed-instance-counters 0)))

(defun beads-agent--generate-buffer-name (proj-name type-name
                                          instance-n &optional issue-id title)
  "Generate buffer name for agent session.
PROJ-NAME is the project name (e.g., \"beads.el\").
TYPE-NAME is the agent type name (e.g., \"Task\", \"Plan\").
INSTANCE-N is the instance number for this (project, type) combination.
ISSUE-ID and TITLE are optional issue context.

Returns buffer name using centralized format:
  *beads-agent[PROJECT]/TYPE#N*
  *beads-agent[PROJECT]/TYPE#N ISSUE-ID TITLE*"
  (beads-buffer-name-agent type-name instance-n
                           issue-id title proj-name nil))

(defun beads-agent--generate-buffer-name-for-session (session)
  "Generate buffer name for SESSION object.
Uses the session's proj-name and agent-type-name.
If agent-type-name is nil, uses \"Agent\" as default."
  (let ((proj-name (or (oref session proj-name)
                       (beads-agent--derive-project-name
                        (oref session project-dir))))
        (type-name (or (oref session agent-type-name) "Agent"))
        (instance-n (beads-agent--session-instance-number session))
        (issue-id (oref session current-issue)))
    (beads-agent--generate-buffer-name proj-name type-name
                                        instance-n issue-id nil)))

(defun beads-agent--session-instance-number (session)
  "Extract instance number from SESSION.
Parses the session ID which is in `issue-id#N' format and returns N.
If the format doesn't match, returns 1 as default."
  (or (beads-agent--session-number-from-id (oref session id)) 1))

(defun beads-agent--parse-buffer-name (buffer-name)
  "Parse a beads agent buffer name into its components.
BUFFER-NAME should be in centralized format:
  *beads-agent[PROJECT]/TYPE#N*
  *beads-agent[PROJECT]/TYPE#N ISSUE-ID TITLE*

Returns plist with keys from `beads-buffer-parse-agent':
  :project, :branch, :type, :instance, :issue-id, :title
or nil if the buffer name doesn't match the expected format."
  (beads-buffer-parse-agent buffer-name))

(defun beads-agent--buffer-name-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a valid beads agent buffer name.
Matches the centralized format: *beads-agent[PROJECT]/TYPE#N*

Use this predicate when:
- Filtering agent buffers
- Validating buffer names before parsing
- Implementing buffer cleanup or listing functions"
  (beads-buffer-agent-p buffer-name))

;;; Directory-Bound Buffer Naming
;;
;; Buffer names use the centralized beads-buffer module with format:
;;   *beads-agent[PROJECT]/TYPE#N*
;;   *beads-agent[PROJECT@BRANCH]/TYPE#N*  (on feature branch)
;;   *beads-agent[PROJECT]/TYPE#N ISSUE-ID TITLE*   (with issue)

(defun beads-agent--generate-project-buffer-name
    (proj-name type-name instance-n
     &optional issue-id title branch)
  "Generate buffer name for directory-bound session.
PROJ-NAME is the project name (e.g., \"beads.el\").
TYPE-NAME is the agent type name (e.g., \"Task\", \"Plan\").
INSTANCE-N is the instance number for this (project, type) combination.
ISSUE-ID and TITLE are optional issue context.
BRANCH is optional feature branch context.

Returns buffer name in format: *beads-agent[PROJECT]/TYPE#N*"
  (beads-buffer-name-agent type-name instance-n
                           issue-id title
                           proj-name branch))

(defun beads-agent--generate-buffer-name-for-project-session (session)
  "Generate buffer name for directory-bound SESSION object.
Uses the worktree directory name if available, otherwise falls back
to the session's proj-name or project-dir.  Instance number is
determined by a typed counter keyed by (display-name, type-name),
ensuring each type has independent numbering within a directory.

The buffer name includes the issue ID and title (truncated) when available,
making it easy to identify which issue the agent is working on.

This function is idempotent: if SESSION already has a buffer stored,
returns that buffer's name instead of generating a new one.  This
prevents the typed instance counter from being incremented multiple
times for the same session."
  ;; Idempotent: return existing buffer name if session already has one
  (if-let ((existing-buffer (oref session buffer)))
      (buffer-name existing-buffer)
    ;; Generate new name (increments typed counter)
    ;; Prefer worktree-dir for display name when available
    (let* ((wt-dir (oref session worktree-dir))
           (display-name (if (and wt-dir (not (string-empty-p wt-dir)))
                             (beads-agent--derive-project-name wt-dir)
                           (or (oref session proj-name)
                               (beads-agent--derive-project-name
                                (oref session project-dir)))))
           (type-name (or (oref session agent-type-name) "Agent"))
           (instance-n (beads-agent--next-typed-instance-number
                        display-name type-name))
           (issue-id (beads-agent-session-issue-id session))
           (issue-title (beads-agent-session-issue-title session)))
      (beads-agent--generate-project-buffer-name
       display-name type-name instance-n issue-id issue-title))))

(defun beads-agent--parse-project-buffer-name (buffer-name)
  "Parse a directory-bound buffer name into its components.
BUFFER-NAME should be in format *beads-agent[PROJECT]/TYPE#N*.
Returns plist with :project-name, :branch, :type-name,
:instance-n, :issue-id, :title, or nil if format doesn't match.

This wraps the centralized `beads-buffer-parse-agent' function,
mapping its return keys to the legacy names used in this module."
  (when-let ((parsed (beads-buffer-parse-agent buffer-name)))
    ;; Map centralized keys to legacy keys for compatibility
    (list :project-name (plist-get parsed :project)
          :branch (plist-get parsed :branch)
          :type-name (plist-get parsed :type)
          :instance-n (plist-get parsed :instance)
          :issue-id (plist-get parsed :issue-id)
          :title (plist-get parsed :title))))

(defun beads-agent--project-buffer-name-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a directory-bound buffer name.
Directory-bound format: *beads-agent[PROJECT]/TYPE#N*

Use this predicate when:
- Working with directory-bound session creation code
- Parsing buffer names with `beads-agent--parse-project-buffer-name'
- Code paths that only handle the modern project-based workflow

For general buffer validation that accepts any agent buffer format,
use `beads-agent--buffer-name-p' instead."
  (beads-buffer-agent-p buffer-name))

;;; Window Management
;;
;; Agent buffers use Emacs's native `display-buffer-alist' for window
;; management.  When starting agents, beads-agent.el uses
;; `display-buffer-overriding-action' with built-in display actions
;; to keep the current window visible while showing the agent elsewhere.

(defun beads-agent--pop-to-buffer-other-window (buffer)
  "Switch to BUFFER using the configured display action.
This respects `display-buffer-alist' rules set up for agent buffers,
which by default display agents in a window to the right.

This is the recommended way to display agent buffers."
  (pop-to-buffer buffer))

(defun beads-agent--find-buffers-by-issue (issue-id)
  "Find all beads agent buffers for ISSUE-ID.
Returns a list of buffer objects."
  (cl-loop for buf in (buffer-list)
           for name = (buffer-name buf)
           for parsed = (beads-agent--parse-buffer-name name)
           when (and parsed (equal (plist-get parsed :issue-id) issue-id))
           collect buf))

(defun beads-agent--find-buffers-by-type (type-name)
  "Find all beads agent buffers with TYPE-NAME.
Returns a list of buffer objects."
  (cl-loop for buf in (buffer-list)
           for name = (buffer-name buf)
           for parsed = (beads-agent--parse-buffer-name name)
           when (and parsed (equal (plist-get parsed :type) type-name))
           collect buf))

(provide 'beads-agent-backend)

;;; Load Sesman Integration
;;
;; Session storage is delegated to sesman via beads-sesman.el.
;; This require ensures the hook handler is registered.
;; Must come after `provide' to avoid circular dependency.
(require 'beads-sesman)

;;; beads-agent-backend.el ends here
