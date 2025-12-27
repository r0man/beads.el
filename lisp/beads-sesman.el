;;; beads-sesman.el --- Sesman integration for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides sesman integration for beads, enabling
;; context-aware session selection using the sesman library.
;;
;; Sesman (Session Manager) provides:
;; - Automatic session selection based on buffer, directory, or project
;; - Standard keybindings for session management
;; - Session browser UI
;; - Context linking for sessions
;;
;; Session Structure:
;;   (session-name backend-handle beads-agent-session)
;;   - session-name: "<issue-id>@<worktree-path>" or "<issue-id>@<project>"
;;   - backend-handle: Opaque handle from the AI backend
;;   - beads-agent-session: EIEIO object with full metadata
;;
;; Context Linking:
;;   Sessions are linked to two context types:
;;   1. Buffer (agent buffer) - for context when in the agent's terminal
;;   2. Working directory - the directory where the agent operates
;;      (worktree if using worktrees, otherwise project root)
;;
;;   This enables `sesman-current-session' to find the right session
;;   when you're in the agent buffer or editing files in its working
;;   directory.  Different directories (including worktrees) are
;;   separate contexts with their own sessions.
;;
;; Usage:
;;   ;; Start a session (will be registered with sesman)
;;   (sesman-start-session 'Beads)
;;
;;   ;; Get current session for buffer context
;;   (sesman-current-session 'Beads)
;;
;;   ;; Open session browser
;;   M-x beads-sesman-browser

;;; Code:

(require 'eieio)
(require 'sesman)
(require 'beads-agent-backend)
(require 'beads-custom)

;; Forward declarations
(declare-function beads-git-find-project-root "beads-git")
(declare-function beads-git-get-project-name "beads-git")
(declare-function beads-git-get-branch "beads-git")
(declare-function beads-agent-start "beads-agent")
(declare-function beads-agent-stop "beads-agent")
(declare-function beads-agent--read-issue-id "beads-agent")

;;; Constants

(defconst beads-sesman-system 'Beads
  "Sesman system name for beads sessions.
This symbol identifies beads sessions in sesman's registry.")

;;; Customization

(defcustom beads-sesman-restart-delay 0.5
  "Delay in seconds before restarting a session.
This allows cleanup to complete before starting a new agent."
  :type 'number
  :group 'beads)

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

(defvar beads-sesman--worktree-sessions nil
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

(defun beads-sesman--normalize-directory (dir)
  "Normalize DIR for consistent comparison.
Returns expanded, truename path."
  (file-truename (expand-file-name dir)))

(defun beads-sesman--session-for-directory (dir)
  "Find worktree session for DIR.
Returns beads-worktree-session or nil."
  (let ((normalized-dir (beads-sesman--normalize-directory dir)))
    (cl-find-if
     (lambda (session)
       (equal (beads-sesman--normalize-directory (oref session project-dir))
              normalized-dir))
     beads-sesman--worktree-sessions)))

(defun beads-sesman--create-worktree-session (project-dir)
  "Create a new worktree session for PROJECT-DIR.
Returns the created beads-worktree-session object.
Gracefully handles the case where PROJECT-DIR does not exist
or is not a git repository."
  (let* ((normalized-dir (beads-sesman--normalize-directory project-dir))
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
    (push session beads-sesman--worktree-sessions)
    session))

(defun beads-sesman--ensure-worktree-session ()
  "Get or create worktree session for current context.
Uses project root directory as identity."
  (let ((project-dir (or (beads-git-find-project-root)
                         default-directory)))
    (or (beads-sesman--session-for-directory project-dir)
        (beads-sesman--create-worktree-session project-dir))))

(defun beads-sesman--maybe-cleanup-worktree-session (session)
  "Clean up SESSION if it's empty.
Removes the session from the global list if it has no buffers
and no agent sessions."
  (when (beads-worktree-session-empty-p session)
    (setq beads-sesman--worktree-sessions
          (delq session beads-sesman--worktree-sessions))))

(defun beads-sesman--buffer-worktree-session (buffer)
  "Find the worktree session containing BUFFER.
Return beads-worktree-session or nil."
  (cl-find-if
   (lambda (session)
     (memq buffer (oref session buffers)))
   beads-sesman--worktree-sessions))

;;; Agent Session Integration with Worktree Sessions
;;
;; Agent sessions are tracked within their parent worktree session.
;; This provides unified lifecycle management for all beads entities
;; in a project directory.

(defun beads-sesman--add-agent-to-worktree (agent-session)
  "Add AGENT-SESSION to appropriate worktree session.
Creates the worktree session if needed.  Uses agent's project-dir
for lookup."
  (when-let ((project-dir (oref agent-session project-dir)))
    (let ((worktree-session (or (beads-sesman--session-for-directory project-dir)
                                (beads-sesman--create-worktree-session project-dir))))
      (unless (memq agent-session (oref worktree-session agent-sessions))
        (oset worktree-session agent-sessions
              (cons agent-session (oref worktree-session agent-sessions)))))))

(defun beads-sesman--remove-agent-from-worktree (agent-session)
  "Remove AGENT-SESSION from its worktree session.
Also cleans up the worktree session if it becomes empty."
  (when-let* ((project-dir (oref agent-session project-dir))
              (worktree-session (beads-sesman--session-for-directory project-dir)))
    (oset worktree-session agent-sessions
          (delq agent-session (oref worktree-session agent-sessions)))
    (beads-sesman--maybe-cleanup-worktree-session worktree-session)))

(defun beads-sesman--get-agents-for-project (project-dir)
  "Return list of agent sessions for PROJECT-DIR.
Returns nil if no agents found."
  (when-let ((worktree-session (beads-sesman--session-for-directory project-dir)))
    (oref worktree-session agent-sessions)))

(defun beads-sesman--agent-worktree-session (agent-session)
  "Find the worktree session containing AGENT-SESSION.
Return beads-worktree-session or nil."
  (cl-find-if
   (lambda (session)
     (memq agent-session (oref session agent-sessions)))
   beads-sesman--worktree-sessions))

;;; Session Naming

(defun beads-sesman--session-name (session)
  "Generate sesman session name from SESSION.
Delegates to `beads-agent-backend-session-name' for backend-specific naming.
Falls back to \"<session-id>@<working-dir>\" if backend not found,
where session-id is in `issue-id#N' format."
  (if-let ((backend (beads-agent--get-backend (oref session backend-name))))
      (beads-agent-backend-session-name backend session)
    ;; Fallback if backend not found (defensive)
    (let ((session-id (oref session id))
          (working-dir (or (oref session worktree-dir)
                           (oref session project-dir))))
      (format "%s@%s" session-id (abbreviate-file-name working-dir)))))

;;; Sesman Generics Implementation

(cl-defmethod sesman-start-session ((_system (eql Beads)))
  "Start a new beads session asynchronously.
Prompt for issue ID and backend, then start the agent.

Note: This method returns nil because `beads-agent-start' is async.
The session is registered with sesman via `beads-agent-state-change-hook'
when the agent actually starts.  Use `sesman-current-session' after
the agent has started to get the session."
  (let ((issue-id (beads-agent--read-issue-id)))
    (beads-agent-start issue-id)
    ;; Return nil - session registration happens via hook when agent starts
    nil))

(cl-defmethod sesman-quit-session ((_system (eql Beads)) session)
  "Quit beads SESSION.
SESSION is a sesman session list (name backend-handle beads-agent-session)."
  (when-let ((beads-session (nth 2 session)))
    (beads-agent-stop (oref beads-session id))))

(cl-defmethod sesman-restart-session ((_system (eql Beads)) session)
  "Restart beads SESSION.
Stop the session then start a new one for the same issue."
  (when-let ((beads-session (nth 2 session)))
    (let ((issue-id (oref beads-session issue-id)))
      (sesman-quit-session beads-sesman-system session)
      ;; Small delay to allow cleanup
      (run-at-time beads-sesman-restart-delay nil
                   (lambda ()
                     (beads-agent-start issue-id))))))

(cl-defmethod sesman-project ((_system (eql Beads)))
  "Return project root for current directory.
Used by sesman for project-based context linking."
  (beads-git-find-project-root))

(cl-defmethod sesman-context-types ((_system (eql Beads)))
  "Return context types understood by beads.
Sessions can be linked to buffers, directories, or projects."
  '(buffer directory project))

(cl-defmethod sesman-more-relevant-p ((_system (eql Beads)) session1 session2)
  "Compare SESSION1 and SESSION2 for relevance.
Use recency-based comparison (more recent = more relevant)."
  (let ((s1 (nth 2 session1))
        (s2 (nth 2 session2)))
    (when (and s1 s2)
      (string> (oref s1 started-at) (oref s2 started-at)))))

(defun beads-sesman--format-timestamp (iso-timestamp)
  "Format ISO-TIMESTAMP for human-readable display.
Converts ISO 8601 format to a friendlier format like \"Dec 25, 14:30\".
For today's timestamps, shows just time. For other days, shows date and time."
  (when (and iso-timestamp (stringp iso-timestamp) (> (length iso-timestamp) 0))
    (condition-case nil
        ;; Parse using encode-time with manual extraction
        ;; Format: 2025-12-26T16:11:39+0100
        (if (string-match
             "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
             iso-timestamp)
            (let* ((year (string-to-number (match-string 1 iso-timestamp)))
                   (month (string-to-number (match-string 2 iso-timestamp)))
                   (day (string-to-number (match-string 3 iso-timestamp)))
                   (hour (string-to-number (match-string 4 iso-timestamp)))
                   (minute (string-to-number (match-string 5 iso-timestamp)))
                   (time (encode-time 0 minute hour day month year))
                   (now (decode-time))
                   (today-p (and (= year (decoded-time-year now))
                                 (= month (decoded-time-month now))
                                 (= day (decoded-time-day now)))))
              (if today-p
                  ;; Today: just show time
                  (format "%02d:%02d" hour minute)
                ;; Other day: show date and time
                (format-time-string "%b %d, %H:%M" time)))
          ;; Regex didn't match, return as-is
          iso-timestamp)
      (error iso-timestamp))))

(defun beads-sesman--format-touched-issues (issues)
  "Format list of ISSUES for display.
Returns a comma-separated string, or nil if empty."
  (when (and issues (listp issues) (not (null issues)))
    (mapconcat #'identity (seq-take issues 5) ", ")))

(cl-defmethod sesman-session-info ((_system (eql Beads)) session)
  "Return display info for SESSION.
Return plist with :objects, :strings, :buffers for sesman-browser display.
The :buffers key provides the agent buffer for `sesman-goto' to jump to.

Display includes:
- Session ID (project-name#instance)
- Agent type and instance
- Project name
- Worktree path with git branch
- Current issue focus
- Touched issues list
- Human-readable start time"
  (let* ((beads-session (nth 2 session))
         (backend-handle (nth 1 session)))
    (when beads-session
      ;; Get the agent buffer for :buffers (used by sesman-goto)
      (let* ((agent-buffer
              (when-let ((backend (beads-agent--get-backend
                                   (oref beads-session backend-name))))
                (beads-agent-backend-get-buffer backend beads-session)))
             ;; Extract session info
             (session-id (oref beads-session id))
             (proj-name (oref beads-session proj-name))
             (instance-num (oref beads-session instance-number))
             (type-name (oref beads-session agent-type-name))
             (project-dir (oref beads-session project-dir))
             (worktree-dir (oref beads-session worktree-dir))
             (current-issue (oref beads-session current-issue))
             (touched-issues (oref beads-session touched-issues))
             (started-at (oref beads-session started-at))
             ;; Get git branch for worktree or project
             (working-dir (or worktree-dir project-dir))
             (git-branch (when (and working-dir (file-directory-p working-dir))
                           (let ((default-directory working-dir))
                             (ignore-errors (beads-git-get-branch))))))
        (list
         ;; :buffers is checked first by sesman-goto for jumping
         :buffers (when (and agent-buffer (buffer-live-p agent-buffer))
                    (list agent-buffer))
         :objects (list backend-handle)
         :strings (delq nil
                        (list
                         ;; Session ID
                         (format "Session: %s" session-id)
                         ;; Agent type with instance
                         (when type-name
                           (format "Agent: %s#%d" type-name (or instance-num 1)))
                         ;; Project name
                         (when proj-name
                           (format "Project: %s" proj-name))
                         ;; Worktree with branch
                         (cond
                          ((and worktree-dir git-branch)
                           (format "Worktree: %s [%s]"
                                   (abbreviate-file-name worktree-dir)
                                   git-branch))
                          (worktree-dir
                           (format "Worktree: %s"
                                   (abbreviate-file-name worktree-dir)))
                          ;; Show project dir with branch if no worktree
                          ((and project-dir git-branch)
                           (format "Dir: %s [%s]"
                                   (abbreviate-file-name project-dir)
                                   git-branch)))
                         ;; Current focus issue
                         (when current-issue
                           (format "Focus: %s" current-issue))
                         ;; Touched issues
                         (when-let ((touched-str (beads-sesman--format-touched-issues touched-issues)))
                           (format "Touched: %s" touched-str))
                         ;; Human-readable start time
                         (format "Started: %s"
                                 (beads-sesman--format-timestamp started-at)))))))))

;;; Session Registration Helpers

(defvar-local beads-sesman--buffer-session-id nil
  "Session ID for the agent session associated with this buffer.
Set when an agent buffer is linked to a sesman session.
Used by `beads-sesman--buffer-kill-handler' to clean up the session
when the buffer is killed manually.")

(defun beads-sesman--make-sesman-session (beads-session)
  "Create sesman session list from BEADS-SESSION.
Return (name backend-handle beads-agent-session)."
  (list (beads-sesman--session-name beads-session)
        (oref beads-session backend-session)
        beads-session))

(defun beads-sesman--buffer-kill-handler ()
  "Handle agent buffer being killed.
Cleans up the associated session when an agent buffer is killed
manually (e.g., via `kill-buffer' or \\`C-x k').

This ensures the beads-list view reflects the actual session state
rather than showing stale agent indicators."
  (when beads-sesman--buffer-session-id
    ;; Use beads-agent-stop to properly clean up the session.
    ;; This triggers the state-change hook which unregisters from sesman.
    (condition-case nil
        (when (fboundp 'beads-agent-stop)
          (beads-agent-stop beads-sesman--buffer-session-id))
      ;; Ignore errors during cleanup - buffer is already being killed
      (error nil))))

(defun beads-sesman--register-session (beads-session)
  "Register BEADS-SESSION with sesman and link to contexts.
Link to working directory (worktree or project root) and the
agent buffer (if available).

When linking to an agent buffer, also sets up a `kill-buffer-hook'
to clean up the session if the buffer is killed manually."
  (let ((sesman-session (beads-sesman--make-sesman-session beads-session)))
    ;; Register with sesman
    (sesman-register beads-sesman-system sesman-session)
    ;; Link to working directory (worktree if set, otherwise project root)
    (let ((working-dir (or (oref beads-session worktree-dir)
                           (oref beads-session project-dir))))
      (sesman-link-session beads-sesman-system sesman-session 'directory working-dir))
    ;; Link to agent buffer if available
    ;; Note: We check buffer-live-p twice to handle potential race conditions
    ;; where the buffer could be killed between checks and operations.
    (when-let* ((backend (beads-agent--get-backend (oref beads-session backend-name)))
                (buffer (beads-agent-backend-get-buffer backend beads-session)))
      (when (buffer-live-p buffer)
        (sesman-link-session beads-sesman-system sesman-session 'buffer buffer)
        ;; Set up buffer for session management
        ;; Re-check buffer-live-p to handle race where buffer is killed
        ;; between sesman-link-session and with-current-buffer
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local sesman-system beads-sesman-system)
            ;; Track session ID for cleanup on buffer kill
            (setq-local beads-sesman--buffer-session-id (oref beads-session id))
            ;; Add kill-buffer-hook to clean up session when buffer is killed
            (add-hook 'kill-buffer-hook #'beads-sesman--buffer-kill-handler nil t)))))))

(defun beads-sesman--link-session-buffer (beads-session buffer)
  "Link BUFFER to the sesman session for BEADS-SESSION.
This should be called after the buffer is stored in the session,
to set up `sesman-system' and the `kill-buffer-hook'.

This fixes a timing issue where `beads-sesman--register-session' is called
before the buffer is available (during session creation), so the buffer
linking is deferred until after `beads-agent--rename-and-store-buffer'."
  (when (and buffer (buffer-live-p buffer))
    (let ((name (beads-sesman--session-name beads-session)))
      (when-let ((sesman-session (sesman-session beads-sesman-system name)))
        ;; Link buffer to session
        (sesman-link-session beads-sesman-system sesman-session 'buffer buffer)
        ;; Set up buffer for session management
        (with-current-buffer buffer
          (setq-local sesman-system beads-sesman-system)
          ;; Track session ID for cleanup on buffer kill
          (setq-local beads-sesman--buffer-session-id (oref beads-session id))
          ;; Add kill-buffer-hook to clean up session when buffer is killed
          (add-hook 'kill-buffer-hook #'beads-sesman--buffer-kill-handler nil t))))))

(defun beads-sesman--unregister-session (beads-session)
  "Unregister BEADS-SESSION from sesman.
Also clears the buffer-local session ID to prevent the `kill-buffer-hook'
from attempting a redundant cleanup when the buffer is eventually killed."
  (let ((name (beads-sesman--session-name beads-session)))
    ;; Clear buffer-local session ID and remove hook to prevent orphaned cleanup
    (when-let* ((backend (beads-agent--get-backend (oref beads-session backend-name)))
                (buffer (beads-agent-backend-get-buffer backend beads-session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local beads-sesman--buffer-session-id nil)
          ;; Remove the kill-buffer-hook to prevent orphaned cleanup attempts
          (remove-hook 'kill-buffer-hook #'beads-sesman--buffer-kill-handler t))))
    ;; Unregister from sesman
    ;; Note: sesman-session returns nil if session not found by name.
    ;; This could happen if the session name changed between registration
    ;; and unregistration (e.g., path normalization differences).
    (if-let ((ses (sesman-session beads-sesman-system name)))
        (sesman-unregister beads-sesman-system ses)
      ;; Session not found by name - this is unexpected but not fatal.
      ;; Only log in debug mode to avoid noise in normal usage.
      (when beads-enable-debug
        (message "beads-sesman: session not found for unregistration: %s"
                 name)))))

;;; Hook Integration

(defun beads-sesman--state-change-handler (action session)
  "Handle session state change for sesman integration.
ACTION is `started', `stopped', or `failed'.
SESSION is the beads-agent-session object.

This handler:
1. Registers/unregisters with sesman for context-aware session selection
2. Adds/removes agent session from worktree session for lifecycle tracking"
  (pcase action
    ('started
     (beads-sesman--register-session session)
     (beads-sesman--add-agent-to-worktree session))
    ('stopped
     (beads-sesman--remove-agent-from-worktree session)
     (beads-sesman--unregister-session session))))

;;; User-Facing Commands

;;;###autoload
(defun beads-sesman-start ()
  "Start a new beads session via sesman."
  (interactive)
  (sesman-start-session beads-sesman-system))

;;;###autoload
(defun beads-sesman-quit ()
  "Quit the current beads session."
  (interactive)
  (if-let ((session (sesman-current-session beads-sesman-system)))
      (sesman-quit-session beads-sesman-system session)
    (user-error "No current beads session")))

;;;###autoload
(defun beads-sesman-restart ()
  "Restart the current beads session."
  (interactive)
  (if-let ((session (sesman-current-session beads-sesman-system)))
      (sesman-restart-session beads-sesman-system session)
    (user-error "No current beads session")))

;;;###autoload
(defun beads-sesman-browser ()
  "Open the sesman browser for beads sessions."
  (interactive)
  (let ((sesman-system beads-sesman-system))
    (sesman-browser)))

;;;###autoload
(defun beads-sesman-link ()
  "Link a beads session to the current context."
  (interactive)
  (let ((sesman-system beads-sesman-system))
    (call-interactively #'sesman-link-with-buffer)))

;;; Keymap

(defvar beads-sesman-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'beads-sesman-start)
    (define-key map (kbd "q") #'beads-sesman-quit)
    (define-key map (kbd "r") #'beads-sesman-restart)
    (define-key map (kbd "b") #'beads-sesman-browser)
    (define-key map (kbd "l") #'beads-sesman-link)
    map)
  "Keymap for beads sesman commands.
Intended to be bound under a global prefix key.")

;;; Hook Registration
;;
;; Register the state change handler unconditionally when this file is loaded.
;; This ensures sesman integration is always active when beads-sesman is required.

(add-hook 'beads-agent-state-change-hook #'beads-sesman--state-change-handler)

(provide 'beads-sesman)
;;; beads-sesman.el ends here
