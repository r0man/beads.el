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
    :documentation "Unique session identifier in `issue-id#N' format.")
   (issue-id
    :initarg :issue-id
    :type string
    :documentation "Issue ID this session is working on.")
   (backend-name
    :initarg :backend-name
    :type string
    :documentation "Name of the backend running this session.")
   (agent-type-name
    :initarg :agent-type-name
    :initform nil
    :documentation "Name of the agent type for this session (e.g., \"Task\", \"Review\").
When nil, the session was started without specifying an agent type.")
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
    :documentation "Backend-specific session object/handle.
This is the return value from `beads-agent-backend-start'.
In sesman sessions, this becomes the second element of the tuple:
  (session-name BACKEND-SESSION beads-agent-session)
Backends use this to track their internal session state.
The value is opaque to beads-sesman and passed through unchanged.")
   (buffer
    :initarg :buffer
    :initform nil
    :documentation "The Emacs buffer for this session.
After the backend creates its buffer, it is renamed to the beads format
and stored here for efficient lookup.  Buffer naming follows the format:
  *beads-agent[ISSUE-ID][TYPE#N]*
This slot is set after session creation when the buffer is renamed."))
  :documentation "Represents an active AI agent session.")

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

;;; Backend Protocol (Generic Methods)

(cl-defgeneric beads-agent-backend-available-p (backend)
  "Return non-nil if BACKEND is available for use.
This should check if the required package is loaded and functional.")

(cl-defgeneric beads-agent-backend-start (backend issue prompt)
  "Start BACKEND working on ISSUE with PROMPT.
ISSUE is a beads-issue object.
PROMPT is a string to send to the agent.

Returns a backend-specific session handle or signals an error.
The handle is stored in the `backend-session' slot of the
`beads-agent-session' object and used as part of the sesman
session tuple: (session-name backend-handle beads-agent-session).

The handle should be suitable for display in sesman's session
browser and for identifying the backend's notion of the session.
For example, claude-code-ide returns an MCP session handle.")

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
  (run-at-time
   0 nil
   (lambda ()
     (beads-agent-backend-stop backend session)
     (when callback
       (funcall callback)))))

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

Default implementation returns nil.  Backends should override this
to return their agent buffer if available."
  ;; Default: no buffer available
  (ignore backend session)
  nil)

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
Returns ID in `issue-id#N' format where N is the next available number."
  (format "%s#%d" issue-id (beads-agent--next-session-number issue-id)))

(defun beads-agent--create-session (issue-id backend-name project-dir
                                             backend-session
                                             &optional worktree-dir agent-type-name)
  "Create and register a new session.
ISSUE-ID is the issue being worked on.
BACKEND-NAME is the name of the backend.
PROJECT-DIR is the main project directory.
BACKEND-SESSION is the backend-specific session handle.
WORKTREE-DIR is the git worktree directory, if using worktrees.
AGENT-TYPE-NAME is the name of the agent type (e.g., \"Task\", \"Review\").
Returns the created beads-agent-session object.

Note: Session storage is handled by `beads-agent-state-change-hook'.
The hook handler in beads-sesman.el registers the session with sesman."
  (let* ((session-id (beads-agent--generate-session-id issue-id))
         (session (beads-agent-session
                   :id session-id
                   :issue-id issue-id
                   :backend-name backend-name
                   :agent-type-name agent-type-name
                   :project-dir project-dir
                   :worktree-dir worktree-dir
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

;;; Buffer Naming
;;
;; Buffer names follow the format:
;;   *beads-agent[ISSUE-ID][TYPE#N]*
;;
;; Where:
;;   ISSUE-ID - The issue being worked on (e.g., "beads.el-xrrt")
;;   TYPE - The agent type name (e.g., "Task", "Review", "Plan")
;;   N - Instance number per (issue, type) combination
;;
;; Examples:
;;   *beads-agent[beads.el-xrrt][Task#1]*
;;   *beads-agent[beads.el-xrrt][Plan#1]*
;;   *beads-agent[beads.el-xrrt][Task#2]*

(defvar beads-agent--typed-instance-counters (make-hash-table :test #'equal)
  "Hash table mapping (issue-id . type-name) to next instance number.
Used to track instance numbers per (issue, type) combination.")

(defun beads-agent--reset-typed-instance-counters ()
  "Reset all typed instance counters.
Primarily used for testing."
  (clrhash beads-agent--typed-instance-counters))

(defun beads-agent--next-typed-instance-number (issue-id type-name)
  "Get the next instance number for ISSUE-ID and TYPE-NAME combination.
Returns 1 for the first instance, 2 for the second, etc.
Instance numbers are tracked independently for each (issue, type) pair."
  (let* ((key (cons issue-id type-name))
         (current (gethash key beads-agent--typed-instance-counters 0))
         (next (1+ current)))
    (puthash key next beads-agent--typed-instance-counters)
    next))

(defun beads-agent--peek-typed-instance-number (issue-id type-name)
  "Peek at the next instance number without incrementing.
Returns what the next call to `beads-agent--next-typed-instance-number'
would return for ISSUE-ID and TYPE-NAME."
  (1+ (gethash (cons issue-id type-name)
               beads-agent--typed-instance-counters 0)))

(defun beads-agent--generate-buffer-name (issue-id type-name instance-n)
  "Generate buffer name in beads format.
ISSUE-ID is the issue identifier (e.g., \"beads.el-xrrt\").
TYPE-NAME is the agent type name (e.g., \"Task\", \"Plan\").
INSTANCE-N is the instance number for this (issue, type) combination.

Returns buffer name in format: *beads-agent[ISSUE-ID][TYPE#N]*"
  (format "*beads-agent[%s][%s#%d]*" issue-id type-name instance-n))

(defun beads-agent--generate-buffer-name-for-session (session)
  "Generate buffer name for SESSION object.
Uses the session's issue-id and agent-type-name to build the name.
If agent-type-name is nil, uses \"Agent\" as default."
  (let ((issue-id (oref session issue-id))
        (type-name (or (oref session agent-type-name) "Agent"))
        (instance-n (beads-agent--session-instance-number session)))
    (beads-agent--generate-buffer-name issue-id type-name instance-n)))

(defun beads-agent--session-instance-number (session)
  "Extract instance number from SESSION.
Parses the session ID which is in `issue-id#N' format and returns N.
If the format doesn't match, returns 1 as default."
  (or (beads-agent--session-number-from-id (oref session id)) 1))

(defun beads-agent--parse-buffer-name (buffer-name)
  "Parse a beads buffer name into its components.
BUFFER-NAME should be in format *beads-agent[ISSUE-ID][TYPE#N]*.
Returns plist (:issue-id ISSUE-ID :type-name TYPE :instance-n N)
or nil if the buffer name doesn't match the expected format."
  (when (string-match
         "\\*beads-agent\\[\\([^]]+\\)\\]\\[\\([^#]+\\)#\\([0-9]+\\)\\]\\*"
         buffer-name)
    (list :issue-id (match-string 1 buffer-name)
          :type-name (match-string 2 buffer-name)
          :instance-n (string-to-number (match-string 3 buffer-name)))))

(defun beads-agent--buffer-name-p (buffer-name)
  "Return non-nil if BUFFER-NAME is a valid beads agent buffer name."
  (and buffer-name
       (string-match-p
        "\\*beads-agent\\[.+\\]\\[.+#[0-9]+\\]\\*"
        buffer-name)))

;;; Window Management
;;
;; Agent buffers use Emacs's native `display-buffer-alist' for window
;; management.  The display action is configured via the customizable
;; variable `beads-agent-display-buffer-action' in beads-agent.el.

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
           when (and parsed (equal (plist-get parsed :type-name) type-name))
           collect buf))

(provide 'beads-agent-backend)

;;; Load Sesman Integration
;;
;; Session storage is delegated to sesman via beads-sesman.el.
;; This require ensures the hook handler is registered.
;; Must come after `provide' to avoid circular dependency.
(require 'beads-sesman)

;;; beads-agent-backend.el ends here
