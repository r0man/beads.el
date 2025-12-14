;;; beads-agent.el --- AI agent integration for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides AI agent integration for Beads, enabling users
;; to start/stop AI agents working on issues, track sessions, and
;; navigate between issues and agent buffers.
;;
;; Architecture:
;;
;; The architecture uses an abstract backend system that allows
;; different AI agent implementations to be plugged in:
;; - claude-code-ide.el (primary, fully implemented)
;; - efrit (placeholder)
;; - claudemacs (placeholder)
;; - claude-code.el (placeholder)
;;
;; See `beads-agent-backend.el' for the backend protocol definition.
;;
;; Session Management (via Sesman):
;;
;; Sessions are managed using sesman (Session Manager), which provides:
;; - Context-aware session selection based on buffer/directory/project
;; - Standard keybindings for session management (C-c C-s prefix)
;; - Interactive session browser (M-x beads-sesman-browser)
;; - Automatic session linking to worktree and project directories
;;
;; Session Naming Convention:
;;   Sessions are named "<issue-id>@<directory>", e.g.:
;;   - beads.el-42@~/projects/beads.el/        (main repo)
;;   - beads.el-42@~/projects/beads.el-42/     (worktree)
;;
;; Context Linking:
;;   Each session is linked to three context types:
;;   1. Agent buffer - the terminal where the AI agent runs
;;   2. Worktree directory (primary) - for context in worktree buffers
;;   3. Main project (fallback) - for context from anywhere in project
;;
;;   This triple linking enables sesman to automatically select the
;;   correct session whether you're in the agent buffer, editing files
;;   in a worktree, or working anywhere in the main repository.
;;
;; See `beads-sesman.el' for the sesman integration implementation.
;;
;; Git Worktree Support:
;;
;; When `beads-agent-use-worktrees' is non-nil (default), each agent
;; session gets its own git worktree for isolation:
;; - Worktrees are created as siblings to the main repo
;; - Named after the issue ID (e.g., ~/projects/beads.el-42/)
;; - Branch is created with the same name as the issue ID
;; - Issues are automatically imported from the main repo's JSONL
;;
;; Usage:
;;
;;   ;; Start agent on an issue
;;   M-x beads-agent-start RET
;;
;;   ;; Or from beads-list/beads-show buffers, press A
;;
;;   ;; Jump to agent buffer
;;   M-x beads-agent-jump RET
;;
;;   ;; Stop agent session
;;   M-x beads-agent-stop RET
;;
;;   ;; Open agent menu
;;   M-x beads-agent RET
;;
;;   ;; Sesman session management (bind beads-sesman-map to a prefix)
;;   C-c C-s s - Start new session
;;   C-c C-s q - Quit current session
;;   C-c C-s r - Restart current session
;;   C-c C-s b - Open session browser
;;   C-c C-s l - Link session to current context

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'json)
(require 'beads)
(require 'beads-command)
(require 'beads-agent-backend)
(require 'transient)

;; Forward declarations
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-show--issue-id "beads-show")
(defvar beads-show--issue-id)

;;; Customization

(defgroup beads-agent nil
  "AI agent integration for Beads."
  :group 'beads
  :prefix "beads-agent-")

(defcustom beads-agent-auto-set-in-progress t
  "Automatically set issue status to in_progress when agent starts.
When non-nil, starting an agent on an issue will update the issue
status to in_progress if it is currently open."
  :type 'boolean
  :group 'beads-agent)

(defcustom beads-agent-default-backend nil
  "Default backend to use for agent sessions.
If nil, prompt user to select from available backends.
If set to a backend name string, use that backend automatically."
  :type '(choice (const :tag "Prompt" nil)
                 (string :tag "Backend name"))
  :group 'beads-agent)

(defcustom beads-agent-use-worktrees t
  "When non-nil, agent sessions use git worktrees for isolation.
Each issue gets its own worktree, named after the issue ID.
Worktrees are created in a sibling directory to the main repo."
  :type 'boolean
  :group 'beads-agent)

(defcustom beads-agent-worktree-parent nil
  "Parent directory for agent worktrees.
If nil, worktrees are created as siblings to the main repository.
For example, if the repo is at ~/projects/myrepo, worktrees are
created at ~/projects/issue-id/.

If set to a path, worktrees are created under that directory."
  :type '(choice (const :tag "Sibling to repo" nil)
                 (directory :tag "Custom directory"))
  :group 'beads-agent)

;;; JSON Parsing Utilities

(defun beads-agent--extract-json (output)
  "Extract JSON from OUTPUT string.
The bd command may output non-JSON content before the actual JSON
\(e.g., daemon connection messages, progress indicators).  This
function finds the first JSON array or object in OUTPUT and returns
just that portion.

Returns the JSON substring, or the original OUTPUT if no JSON
delimiter is found."
  (if-let ((json-start (or (string-match "^\\[" output)
                           (string-match "^{" output)
                           (string-match "\n\\[" output)
                           (string-match "\n{" output))))
      ;; Found JSON start - extract from there
      ;; Skip the newline if we matched on \n[ or \n{
      (let ((actual-start (if (or (string-match "^\n" (substring output json-start))
                                  (= json-start 0))
                              (if (= json-start 0)
                                  0
                                (1+ json-start))
                            json-start)))
        (substring output actual-start))
    ;; No JSON found, return original
    output))

;;; Git Worktree Support

(defun beads-agent--git-command (&rest args)
  "Run git with ARGS and return trimmed output, or nil on error."
  (let ((default-directory (or (beads--find-project-root)
                               default-directory)))
    (with-temp-buffer
      (when (zerop (apply #'call-process "git" nil t nil args))
        (string-trim (buffer-string))))))

(defun beads-agent--git-command-async (callback &rest args)
  "Run git with ARGS asynchronously and call CALLBACK with result.
CALLBACK receives (success output) where success is t/nil."
  (let* ((default-directory (or (beads--find-project-root)
                                default-directory))
         (output-buffer (generate-new-buffer " *beads-git-async*"))
         (process (apply #'start-process
                         "beads-git" output-buffer
                         "git" args)))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((success (zerop (process-exit-status proc)))
               (output (with-current-buffer output-buffer
                         (string-trim (buffer-string)))))
           (kill-buffer output-buffer)
           (funcall callback success output)))))))

(defun beads-agent--main-repo-root ()
  "Find the main repository root, even from within a worktree.
Returns the path to the main repo, not the worktree."
  (when-let* ((git-dir (beads-agent--git-command
                        "rev-parse" "--path-format=absolute" "--git-common-dir")))
    ;; git-common-dir returns path to .git directory of main repo
    ;; Strip trailing .git or /worktrees/... to get repo root
    (let ((dir (file-name-directory (directory-file-name git-dir))))
      (if (string-suffix-p "/.git/" dir)
          (substring dir 0 -6)  ; Remove /.git/
        dir))))

(defun beads-agent--in-worktree-p ()
  "Return non-nil if current directory is inside a git worktree.
Distinguishes between the main working tree and linked worktrees."
  (let ((toplevel (beads-agent--git-command "rev-parse" "--show-toplevel"))
        (main-root (beads-agent--main-repo-root)))
    (and toplevel main-root
         (not (file-equal-p toplevel main-root)))))

(defun beads-agent--list-worktrees ()
  "Return list of (path branch) pairs for all worktrees."
  (when-let* ((output (beads-agent--git-command "worktree" "list" "--porcelain")))
    (let ((worktrees nil)
          (current-path nil)
          (current-branch nil))
      (dolist (line (split-string output "\n" t))
        (cond
         ((string-prefix-p "worktree " line)
          ;; Save previous worktree if we have one
          (when current-path
            (push (list current-path current-branch) worktrees))
          (setq current-path (substring line 9))
          (setq current-branch nil))
         ((string-prefix-p "branch " line)
          (setq current-branch
                (replace-regexp-in-string
                 "^refs/heads/" "" (substring line 7))))))
      ;; Save last worktree
      (when current-path
        (push (list current-path current-branch) worktrees))
      (nreverse worktrees))))

(defun beads-agent--find-worktree-for-issue (issue-id)
  "Find existing worktree for ISSUE-ID.
Returns the worktree path or nil if not found."
  (let ((worktrees (beads-agent--list-worktrees)))
    (cl-loop for (path branch) in worktrees
             when (or (string-suffix-p (concat "/" issue-id) path)
                      (equal branch issue-id))
             return path)))

(defun beads-agent--worktree-path-for-issue (issue-id)
  "Calculate the worktree path for ISSUE-ID.
Does not check if it exists."
  (let* ((main-root (beads-agent--main-repo-root))
         (parent (or beads-agent-worktree-parent
                     (file-name-directory (directory-file-name main-root)))))
    (expand-file-name issue-id parent)))

(defun beads-agent--create-worktree (issue-id)
  "Create a git worktree for ISSUE-ID.
Creates a new branch based on the current HEAD.
Returns the worktree path on success, signals error on failure."
  (let* ((worktree-path (beads-agent--worktree-path-for-issue issue-id))
         (main-root (beads-agent--main-repo-root))
         (default-directory main-root))
    ;; Check if path already exists
    (when (file-exists-p worktree-path)
      (error "Worktree path already exists: %s" worktree-path))
    ;; Create worktree with new branch
    (with-temp-buffer
      (let ((exit-code (call-process "git" nil t nil
                                     "worktree" "add"
                                     "-b" issue-id
                                     worktree-path)))
        (unless (zerop exit-code)
          ;; Branch might already exist, try without -b
          (erase-buffer)
          (setq exit-code (call-process "git" nil t nil
                                        "worktree" "add"
                                        worktree-path
                                        issue-id))
          (unless (zerop exit-code)
            (error "Failed to create worktree: %s" (buffer-string))))))
    (message "Created worktree for %s at %s" issue-id worktree-path)
    worktree-path))

(defun beads-agent--ensure-worktree (issue-id)
  "Ensure a worktree exists for ISSUE-ID.
Creates one if it doesn't exist.  Returns the worktree path."
  (or (beads-agent--find-worktree-for-issue issue-id)
      (beads-agent--create-worktree issue-id)))

(defun beads-agent--ensure-worktree-async (issue-id callback)
  "Ensure a worktree exists for ISSUE-ID asynchronously.
CALLBACK receives (success worktree-path-or-error)."
  (if-let ((existing (beads-agent--find-worktree-for-issue issue-id)))
      ;; Worktree already exists
      (funcall callback t existing)
    ;; Need to create worktree
    (beads-agent--create-worktree-async issue-id callback)))

(defun beads-agent--create-worktree-async (issue-id callback)
  "Create a git worktree for ISSUE-ID asynchronously.
CALLBACK receives (success worktree-path-or-error)."
  (let* ((worktree-path (beads-agent--worktree-path-for-issue issue-id))
         (main-root (beads-agent--main-repo-root))
         (default-directory main-root))
    ;; Check if path already exists
    (if (file-exists-p worktree-path)
        (funcall callback nil (format "Worktree path already exists: %s" worktree-path))
      ;; Create worktree with new branch
      (let* ((output-buffer (generate-new-buffer " *beads-worktree*"))
             (process (start-process
                       "beads-worktree" output-buffer
                       "git" "worktree" "add"
                       "-b" issue-id
                       worktree-path)))
        (set-process-sentinel
         process
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let ((success (zerop (process-exit-status proc))))
               (kill-buffer output-buffer)
               (if success
                   ;; Worktree created, now initialize beads database
                   (beads-agent--init-worktree-beads-async
                    issue-id worktree-path callback)
                 ;; Branch might already exist, try without -b
                 (beads-agent--create-worktree-existing-branch-async
                  issue-id worktree-path callback))))))))))

(defun beads-agent--extract-prefix (issue-id)
  "Extract the prefix from ISSUE-ID.
For example, \"beads.el-qghm\" returns \"beads.el\"."
  (if (string-match "\\(.*\\)-[^-]+$" issue-id)
      (match-string 1 issue-id)
    issue-id))

(defun beads-agent--init-worktree-beads-async (issue-id worktree-path callback)
  "Initialize beads database in WORKTREE-PATH asynchronously.
First initializes bd with the correct prefix from ISSUE-ID, then imports
issues from the main repo's JSONL file.
CALLBACK receives (success path-or-error)."
  (let* ((default-directory worktree-path)
         (main-root (beads-agent--main-repo-root))
         (jsonl-path (expand-file-name ".beads/issues.jsonl" main-root))
         (prefix (beads-agent--extract-prefix issue-id))
         (process-environment (cons "BD_NO_DAEMON=1" process-environment)))
    ;; Check if JSONL exists in main repo
    (if (not (file-exists-p jsonl-path))
        ;; No JSONL, just proceed (fresh repo perhaps)
        (progn
          (message "Created worktree for %s at %s (no issues to import)"
                   issue-id worktree-path)
          (funcall callback t worktree-path))
      ;; First initialize bd with the correct prefix, then import
      (let* ((output-buffer (generate-new-buffer " *beads-init*"))
             (process (start-process
                       "beads-init" output-buffer
                       "bd" "--no-daemon" "init" "-p" prefix "-q")))
        (set-process-sentinel
         process
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let ((success (zerop (process-exit-status proc))))
               (kill-buffer output-buffer)
               (if success
                   ;; Init succeeded, now import
                   (beads-agent--import-worktree-issues-async
                    issue-id worktree-path jsonl-path callback)
                 ;; Init failed - warn and continue
                 (message "Warning: Failed to init beads in worktree")
                 (funcall callback t worktree-path))))))))))

(defun beads-agent--import-worktree-issues-async (issue-id worktree-path jsonl-path callback)
  "Import issues into worktree from JSONL-PATH asynchronously.
ISSUE-ID and WORKTREE-PATH are for logging.
CALLBACK receives (success path-or-error)."
  (let* ((default-directory worktree-path)
         (process-environment (cons "BD_NO_DAEMON=1" process-environment))
         (output-buffer (generate-new-buffer " *beads-import*"))
         (process (start-process
                   "beads-import" output-buffer
                   "bd" "--no-daemon" "import" "-i" jsonl-path)))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((success (zerop (process-exit-status proc)))
               (output (with-current-buffer output-buffer
                         (string-trim (buffer-string)))))
           (kill-buffer output-buffer)
           (if success
               (progn
                 (message "Created worktree for %s at %s (imported issues)"
                          issue-id worktree-path)
                 (funcall callback t worktree-path))
             ;; Import failed, but worktree exists - warn and continue
             (message "Warning: Failed to import issues in worktree: %s" output)
             (funcall callback t worktree-path))))))))

(defun beads-agent--create-worktree-existing-branch-async (issue-id worktree-path callback)
  "Create worktree for existing branch ISSUE-ID at WORKTREE-PATH.
CALLBACK receives (success worktree-path-or-error)."
  (let* ((main-root (beads-agent--main-repo-root))
         (default-directory main-root)
         (output-buffer (generate-new-buffer " *beads-worktree*"))
         (process (start-process
                   "beads-worktree" output-buffer
                   "git" "worktree" "add"
                   worktree-path
                   issue-id)))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((success (zerop (process-exit-status proc)))
               (output (with-current-buffer output-buffer
                         (string-trim (buffer-string)))))
           (kill-buffer output-buffer)
           (if success
               ;; Worktree created, now initialize beads database
               (beads-agent--init-worktree-beads-async
                issue-id worktree-path callback)
             (funcall callback nil (format "Failed to create worktree: %s" output)))))))))

(defun beads-agent--setup-worktree-environment ()
  "Return `process-environment' with BD_NO_DAEMON=1 set.
This is needed because bd daemon doesn't work well across worktrees."
  (cons "BD_NO_DAEMON=1" process-environment))

;;; Backend Selection

(defun beads-agent--select-backend ()
  "Select backend: use default if set, else prompt from available.
Returns a beads-agent-backend instance or signals an error."
  (let ((available (beads-agent--get-available-backends)))
    (unless available
      (user-error "No AI agent backends available.

To use AI agents, you need:
2. Install claude-code-ide.el package
3. Install Claude Code CLI: npm install -g @anthropic-ai/claude-code
4. Ensure `claude' command is in your PATH

See: https://github.com/anthropics/claude-code"))
    (cond
     ;; Use default if set and available
     ((and beads-agent-default-backend
           (beads-agent--get-backend beads-agent-default-backend)
           (beads-agent-backend-available-p
            (beads-agent--get-backend beads-agent-default-backend)))
      (beads-agent--get-backend beads-agent-default-backend))
     ;; Single backend available
     ((= (length available) 1)
      (car available))
     ;; Multiple backends - prompt user
     (t
      (let* ((names (mapcar (lambda (b) (oref b name)) available))
             (choice (completing-read "Select backend: " names nil t)))
        (beads-agent--get-backend choice))))))

;;; Context Building

(defun beads-agent--build-prompt (issue)
  "Build initial prompt from ISSUE object.
ISSUE is a beads-issue instance."
  (let ((title (oref issue title))
        (description (oref issue description))
        (acceptance (oref issue acceptance-criteria))
        (id (oref issue id)))
    (concat
     (format "Please work on beads issue %s:\n\n" id)
     (when (and title (not (string-empty-p title)))
       (format "Title: %s\n" title))
     (when (and description (not (string-empty-p description)))
       (format "\nDescription:\n%s\n" description))
     (when (and acceptance (not (string-empty-p acceptance)))
       (format "\nAcceptance Criteria:\n%s\n" acceptance)))))

;;; Status Update

(defun beads-agent--maybe-update-status (issue-id)
  "Update ISSUE-ID to in_progress if configured and currently open."
  (when beads-agent-auto-set-in-progress
    (condition-case nil
        (let ((issue (beads-command-show! :issue-ids (list issue-id))))
          (when (equal (oref issue status) "open")
            (beads-command-update!
             :issue-ids (list issue-id)
             :status "in_progress")
            (beads--invalidate-completion-cache)
            (message "Set %s to in_progress" issue-id)))
      (error nil))))  ; Silently ignore errors

(defun beads-agent--maybe-update-status-async (issue-id callback)
  "Update ISSUE-ID to in_progress asynchronously.
CALLBACK receives no arguments when done."
  (if (not beads-agent-auto-set-in-progress)
      (funcall callback)
    ;; Get issue status first
    (let* ((cmd (beads-command-show :issue-ids (list issue-id)))
           ;; beads-command-line already includes beads-executable
           (full-cmd (beads-command-line cmd))
           (output-buffer (generate-new-buffer " *beads-status*"))
           (process (apply #'start-process
                           "beads-show" output-buffer
                           full-cmd)))
      (set-process-sentinel
       process
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let ((success (zerop (process-exit-status proc)))
                 (output (with-current-buffer output-buffer
                           (buffer-string))))
             (kill-buffer output-buffer)
             (if (not success)
                 (funcall callback)  ; Skip on error
               ;; Parse and check status
               (condition-case nil
                   (let* ((json-object-type 'alist)
                          (json-array-type 'list)
                          (json-str (beads-agent--extract-json output))
                          (data (json-read-from-string json-str))
                          (status (alist-get 'status data)))
                     (if (equal status "open")
                         ;; Update to in_progress
                         (beads-agent--update-status-async issue-id callback)
                       (funcall callback)))
                 (error (funcall callback)))))))))))

(defun beads-agent--update-status-async (issue-id callback)
  "Set ISSUE-ID to in_progress asynchronously.
CALLBACK receives no arguments when done."
  (let* ((cmd (beads-command-update
               :issue-ids (list issue-id)
               :status "in_progress"))
         ;; beads-command-line already includes beads-executable
         (full-cmd (beads-command-line cmd))
         (output-buffer (generate-new-buffer " *beads-update*"))
         (process (apply #'start-process
                         "beads-update" output-buffer
                         full-cmd)))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((success (zerop (process-exit-status proc))))
           (kill-buffer output-buffer)
           (when success
             (beads--invalidate-completion-cache)
             (message "Set %s to in_progress" issue-id))
           (funcall callback)))))))

;;; Context Detection

(defun beads-agent--detect-issue-id ()
  "Detect issue ID from current context.
Returns issue ID string or nil if not found."
  (or
   ;; From beads-list buffer
   (when (derived-mode-p 'beads-list-mode)
     (beads-list--current-issue-id))
   ;; From beads-show buffer
   (when (derived-mode-p 'beads-show-mode)
     beads-show--issue-id)
   ;; From buffer name (*beads-show: bd-N*)
   (when (string-match "\\*beads-show: \\([a-zA-Z0-9._-]+\\)\\*"
                       (buffer-name))
     (match-string 1 (buffer-name)))))

(defun beads-agent--read-issue-id ()
  "Read issue ID with completion, using context if available."
  (or (beads-agent--detect-issue-id)
      (completing-read
       "Issue: "
       (beads--issue-completion-table)
       nil t nil 'beads--issue-id-history)))

;;; Public API Functions

;;;###autoload
(defun beads-agent-start (&optional issue-id backend-name prompt)
  "Start an AI agent working on ISSUE-ID asynchronously.
ISSUE-ID defaults to issue at point or prompts for selection.
BACKEND-NAME defaults to configured default or prompts for selection.
PROMPT defaults to auto-generated from issue.

When `beads-agent-use-worktrees' is non-nil, the agent will work
in a git worktree named after the issue ID.  The worktree is
created automatically if it doesn't exist.

This function returns immediately.  The agent is started in the
background with progress messages displayed in the echo area."
  (interactive)
  (beads-check-executable)
  (let* ((issue-id (or issue-id (beads-agent--read-issue-id)))
         (backend (if backend-name
                      (or (beads-agent--get-backend backend-name)
                          (user-error "Backend not found: %s" backend-name))
                    (beads-agent--select-backend)))
         (project-dir (beads--find-project-root)))
    ;; Check for existing session first (sync - fast)
    (when-let ((existing (beads-agent--get-sessions-for-issue issue-id)))
      (unless (y-or-n-p (format "Session exists for %s. Start another? " issue-id))
        (user-error "Aborted")))
    ;; Start the async workflow
    (message "Starting agent for %s..." issue-id)
    (beads-agent--start-async issue-id backend project-dir prompt)))

(defun beads-agent--start-async (issue-id backend project-dir prompt)
  "Async implementation of agent start.
ISSUE-ID is the issue to work on.
BACKEND is the beads-agent-backend instance.
PROJECT-DIR is the project root directory.
PROMPT is the optional pre-built prompt."
  ;; Step 1: Fetch issue info (async)
  (beads-agent--fetch-issue-async
   issue-id
   (lambda (issue)
     (let ((prompt (or prompt (beads-agent--build-prompt issue))))
       ;; Step 2: Setup worktree if needed (async)
       (if beads-agent-use-worktrees
           (beads-agent--ensure-worktree-async
            issue-id
            (lambda (success result)
              (if success
                  ;; Step 3: Update status (async)
                  (beads-agent--continue-start
                   issue-id backend project-dir result prompt issue)
                (message "Failed to create worktree: %s" result))))
         ;; No worktree, continue directly
         (beads-agent--continue-start
          issue-id backend project-dir nil prompt issue))))))

(defun beads-agent--continue-start (issue-id backend project-dir worktree-dir prompt issue)
  "Continue agent start after worktree is ready.
ISSUE-ID, BACKEND, PROJECT-DIR, WORKTREE-DIR, PROMPT, ISSUE are context."
  ;; Update status async, then start backend
  (beads-agent--maybe-update-status-async
   issue-id
   (lambda ()
     ;; Step 4: Start the backend
     (beads-agent--start-backend-async
      issue-id backend project-dir worktree-dir prompt issue))))

(defun beads-agent--fetch-issue-async (issue-id callback)
  "Fetch ISSUE-ID asynchronously and call CALLBACK with result.
CALLBACK receives a beads-issue object."
  (let* ((cmd (beads-command-show :issue-ids (list issue-id)))
         ;; beads-command-line already includes beads-executable
         (full-cmd (beads-command-line cmd))
         (output-buffer (generate-new-buffer " *beads-fetch*"))
         (process (apply #'start-process
                         "beads-fetch" output-buffer
                         full-cmd)))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((exit-code (process-exit-status proc))
               (output (with-current-buffer output-buffer
                         (buffer-string))))
           (kill-buffer output-buffer)
           (if (not (zerop exit-code))
               (message "Failed to fetch issue %s (exit %d): %s"
                        issue-id exit-code (string-trim output))
             (condition-case err
                 (let* ((json-object-type 'alist)
                        (json-array-type 'vector)
                        (json-str (beads-agent--extract-json output))
                        (json-data (json-read-from-string json-str))
                        (issue (beads--parse-issue json-data)))
                   (funcall callback issue))
               (error
                (message "Failed to parse issue: %s" (error-message-string err)))))))))))

(defun beads-agent--start-backend-async (issue-id backend project-dir worktree-dir prompt issue)
  "Start the backend asynchronously.
ISSUE-ID, BACKEND, PROJECT-DIR, WORKTREE-DIR, PROMPT, ISSUE are context."
  (let* ((working-dir (or worktree-dir project-dir))
         (process-environment (if worktree-dir
                                  (beads-agent--setup-worktree-environment)
                                process-environment))
         (default-directory working-dir))
    (condition-case err
        (let* ((backend-session (beads-agent-backend-start backend issue prompt))
               (session (beads-agent--create-session
                         issue-id
                         (oref backend name)
                         project-dir
                         backend-session
                         worktree-dir)))
          (message "Started agent session %s on %s%s"
                   (oref session id) issue-id
                   (if worktree-dir
                       (format " (worktree: %s)" worktree-dir)
                     "")))
      (error
       (message "Failed to start agent: %s" (error-message-string err))))))

;;;###autoload
(defun beads-agent-stop (&optional session-id)
  "Stop agent session SESSION-ID.
If SESSION-ID is nil, prompts for selection from active sessions."
  (interactive)
  (let* ((sessions (beads-agent--get-all-sessions))
         (session-id (or session-id
                         (if (null sessions)
                             (user-error "No active sessions")
                           (completing-read
                            "Stop session: "
                            (mapcar (lambda (s)
                                      (format "%s (%s)"
                                              (oref s id)
                                              (oref s issue-id)))
                                    sessions)
                            nil t))))
         ;; Extract session-id if it includes issue-id annotation
         (session-id (if (string-match "^\\([^ ]+\\)" session-id)
                         (match-string 1 session-id)
                       session-id))
         (session (beads-agent--get-session session-id)))
    (unless session
      (user-error "Session not found: %s" session-id))
    (when-let ((backend (beads-agent--get-backend (oref session backend-name))))
      (beads-agent-backend-stop backend session))
    (beads-agent--destroy-session session-id)
    (message "Stopped session %s" session-id)))

(defun beads-agent-stop-async (session-id &optional callback)
  "Stop agent session SESSION-ID asynchronously.
CALLBACK is called with no arguments when stop completes.
This function returns immediately without blocking."
  (let ((session (beads-agent--get-session session-id)))
    (if (not session)
        ;; Session not found - call callback and return
        (when callback (funcall callback))
      ;; Session exists - stop it
      (let ((backend (beads-agent--get-backend (oref session backend-name))))
        (if backend
            (beads-agent-backend-stop-async
             backend session
             (lambda ()
               (beads-agent--destroy-session session-id)
               (message "Stopped session %s" session-id)
               (when callback (funcall callback))))
          ;; No backend - just destroy the session
          (beads-agent--destroy-session session-id)
          (message "Stopped session %s" session-id)
          (when callback (funcall callback)))))))

(defun beads-agent-stop-all-async (&optional callback)
  "Stop all agent sessions asynchronously.
CALLBACK is called with no arguments when all sessions are stopped.
This function returns immediately without blocking."
  (let ((sessions (beads-agent--get-all-sessions)))
    (if (null sessions)
        (when callback (funcall callback))
      (let ((remaining (length sessions)))
        (dolist (session sessions)
          (beads-agent-stop-async
           (oref session id)
           (lambda ()
             (cl-decf remaining)
             (when (zerop remaining)
               (when callback (funcall callback))))))))))

;;;###autoload
(defun beads-agent-jump (&optional session-id)
  "Jump to buffer for agent session SESSION-ID.
If SESSION-ID is nil, prompts for selection from active sessions."
  (interactive)
  (let* ((sessions (beads-agent--get-all-sessions))
         (session-id (or session-id
                         (if (null sessions)
                             (user-error "No active sessions")
                           (completing-read
                            "Jump to session: "
                            (mapcar (lambda (s)
                                      (format "%s (%s)"
                                              (oref s id)
                                              (oref s issue-id)))
                                    sessions)
                            nil t))))
         ;; Extract session-id if it includes issue-id annotation
         (session-id (if (string-match "^\\([^ ]+\\)" session-id)
                         (match-string 1 session-id)
                       session-id))
         (session (beads-agent--get-session session-id)))
    (unless session
      (user-error "Session not found: %s" session-id))
    (when-let ((backend (beads-agent--get-backend (oref session backend-name))))
      (beads-agent-backend-switch-to-buffer backend session))))

;;;###autoload
(defun beads-agent-send-prompt (prompt &optional session-id)
  "Send PROMPT to agent session SESSION-ID.
If SESSION-ID is nil, uses session for current issue or prompts."
  (interactive "sPrompt: ")
  (let* ((sessions (beads-agent--get-all-sessions))
         (session-id (or session-id
                         ;; Try to find session for current issue
                         (when-let ((issue-id (beads-agent--detect-issue-id)))
                           (when-let ((issue-sessions
                                       (beads-agent--get-sessions-for-issue
                                        issue-id)))
                             (oref (car issue-sessions) id)))
                         ;; Prompt
                         (if (null sessions)
                             (user-error "No active sessions")
                           (completing-read
                            "Send to session: "
                            (mapcar (lambda (s)
                                      (format "%s (%s)"
                                              (oref s id)
                                              (oref s issue-id)))
                                    sessions)
                            nil t))))
         ;; Extract session-id if it includes issue-id annotation
         (session-id (if (string-match "^\\([^ ]+\\)" session-id)
                         (match-string 1 session-id)
                       session-id))
         (session (beads-agent--get-session session-id)))
    (unless session
      (user-error "Session not found: %s" session-id))
    (when-let ((backend (beads-agent--get-backend (oref session backend-name))))
      (beads-agent-backend-send-prompt backend session prompt)
      (message "Sent prompt to session %s" session-id))))

;;;###autoload
(defun beads-agent-list-sessions ()
  "List all active agent sessions."
  (interactive)
  (let ((sessions (beads-agent--get-all-sessions)))
    (if (null sessions)
        (message "No active sessions")
      (with-help-window "*beads-agent-sessions*"
        (with-current-buffer "*beads-agent-sessions*"
          (insert "Active Agent Sessions\n")
          (insert "=====================\n\n")
          (dolist (session sessions)
            (insert (format "Session: %s\n" (oref session id)))
            (insert (format "  Issue:    %s\n" (oref session issue-id)))
            (insert (format "  Backend:  %s\n" (oref session backend-name)))
            (insert (format "  Started:  %s\n" (oref session started-at)))
            (insert (format "  Active:   %s\n"
                            (if (beads-agent--session-active-p session)
                                "yes" "no")))
            (when-let ((worktree (oref session worktree-dir)))
              (insert (format "  Worktree: %s\n" worktree)))
            (insert "\n")))))))

;;;###autoload
(defun beads-agent-cleanup-stale-sessions ()
  "Remove sessions that are no longer active.
This cleans up sessions where the underlying backend process has
died or the buffer has been killed.  Returns the number of
sessions cleaned up."
  (interactive)
  (let ((sessions (beads-agent--get-all-sessions))
        (cleaned 0))
    (dolist (session sessions)
      (unless (beads-agent--session-active-p session)
        (beads-agent--destroy-session (oref session id))
        (cl-incf cleaned)))
    (if (zerop cleaned)
        (message "No stale sessions found")
      (message "Cleaned up %d stale session%s"
               cleaned (if (= cleaned 1) "" "s")))
    cleaned))

;;; Transient Menu

(defun beads-agent--format-header ()
  "Format header for agent transient menu."
  (let* ((sessions (beads-agent--get-all-sessions))
         (backends (beads-agent--get-available-backends))
         (session-count (length sessions))
         (backend-count (length backends)))
    (format "AI Agent Integration [%d session%s, %d backend%s]"
            session-count (if (= session-count 1) "" "s")
            backend-count (if (= backend-count 1) "" "s"))))

(transient-define-suffix beads-agent--start-suffix ()
  "Start agent on an issue."
  :key "s"
  :description "Start agent on issue"
  (interactive)
  (call-interactively #'beads-agent-start))

(transient-define-suffix beads-agent--stop-suffix ()
  "Stop an agent session."
  :key "S"
  :description "Stop agent session"
  (interactive)
  (call-interactively #'beads-agent-stop))

(transient-define-suffix beads-agent--jump-suffix ()
  "Jump to agent buffer."
  :key "j"
  :description "Jump to agent buffer"
  (interactive)
  (call-interactively #'beads-agent-jump))

(transient-define-suffix beads-agent--send-prompt-suffix ()
  "Send prompt to agent."
  :key "p"
  :description "Send prompt to agent"
  (interactive)
  (call-interactively #'beads-agent-send-prompt))

(transient-define-suffix beads-agent--list-suffix ()
  "List active sessions."
  :key "l"
  :description "List active sessions"
  (interactive)
  (call-interactively #'beads-agent-list-sessions))

(transient-define-suffix beads-agent--cleanup-suffix ()
  "Clean up stale sessions."
  :key "c"
  :description "Cleanup stale sessions"
  :transient t
  (interactive)
  (beads-agent-cleanup-stale-sessions)
  (transient--redisplay))

(transient-define-suffix beads-agent--refresh-suffix ()
  "Refresh the menu."
  :key "g"
  :description "Refresh"
  :transient t
  (interactive)
  (transient--redisplay))

;;;###autoload (autoload 'beads-agent "beads-agent" nil t)
(transient-define-prefix beads-agent ()
  "AI Agent integration for Beads issues."
  [:description
   (lambda () (beads-agent--format-header))
   :pad-keys t]
  ["Agent Actions"
   (beads-agent--start-suffix)
   (beads-agent--stop-suffix)
   (beads-agent--jump-suffix)
   (beads-agent--send-prompt-suffix)]
  ["Session Management"
   (beads-agent--list-suffix)
   (beads-agent--cleanup-suffix)]
  ["Other"
   (beads-agent--refresh-suffix)
   ("q" "Quit" transient-quit-one)])

;;; Context-Aware Commands (for list/show buffers)

;;;###autoload
(defun beads-agent-start-at-point ()
  "Start AI agent for issue at point, or jump to existing session.
If a session already exists for this issue, jump to it instead of
starting a new one.

When called from `beads-list-mode' or `beads-show-mode', the list/show
buffer is kept visible and the agent buffer opens in the other window."
  (interactive)
  (if-let ((id (beads-agent--detect-issue-id)))
      ;; Check for existing session first
      (if-let ((sessions (beads-agent--get-sessions-for-issue id)))
          ;; Session exists - jump to it in other window if in list/show mode
          (beads-agent--jump-other-window-if-applicable
           (oref (car sessions) id))
        ;; No session - start new one
        ;; If we're in list/show mode, arrange windows to keep list visible
        (beads-agent--start-preserving-list-buffer id))
    (call-interactively #'beads-agent-start)))

(defun beads-agent--in-list-or-show-mode-p ()
  "Return non-nil if current buffer is in `beads-list-mode' or `beads-show-mode'."
  (or (derived-mode-p 'beads-list-mode)
      (derived-mode-p 'beads-show-mode)))

(defun beads-agent--jump-other-window-if-applicable (session-id)
  "Jump to SESSION-ID buffer, using other window if in list/show mode."
  (if (beads-agent--in-list-or-show-mode-p)
      (let ((list-buffer (current-buffer)))
        (beads-agent-jump session-id)
        ;; Ensure list buffer is still visible
        (unless (get-buffer-window list-buffer)
          (display-buffer list-buffer '(display-buffer-reuse-window))))
    (beads-agent-jump session-id)))

(defun beads-agent--start-preserving-list-buffer (issue-id)
  "Start agent for ISSUE-ID, keeping list/show buffer visible.
When called from `beads-list-mode' or `beads-show-mode', splits the
window to show the agent in the other window while keeping the
list/show buffer visible."
  (if (beads-agent--in-list-or-show-mode-p)
      (let ((list-buffer (current-buffer)))
        ;; Split window if there's only one
        (when (= (length (window-list)) 1)
          (split-window-right))
        ;; Start the agent (async) - it will take over a window
        (beads-agent-start issue-id)
        ;; Schedule a check to restore list visibility after agent starts
        (run-with-timer
         0.5 nil
         (lambda ()
           (when (buffer-live-p list-buffer)
             ;; If list buffer is not visible, display it
             (unless (get-buffer-window list-buffer)
               (display-buffer list-buffer
                               '(display-buffer-reuse-window
                                 (inhibit-same-window . t))))))))
    ;; Not in list/show mode - just start normally
    (beads-agent-start issue-id)))

;;;###autoload
(defun beads-agent-jump-at-point ()
  "Jump to agent buffer for issue at point, starting one if needed.
If no session exists for the current issue, starts a new agent session."
  (interactive)
  (if-let ((id (beads-agent--detect-issue-id)))
      (if-let ((sessions (beads-agent--get-sessions-for-issue id)))
          ;; Session exists - jump to it
          (beads-agent-jump (oref (car sessions) id))
        ;; No session - start a new one
        (beads-agent-start id))
    (call-interactively #'beads-agent-jump)))

(provide 'beads-agent)

;;; Load Available Backends

;; Load all available backend implementations.
;; Each backend self-registers when loaded.
;; Using (require 'foo nil t) allows graceful handling of missing backends.
(require 'beads-agent-claude-code-ide nil t)
(require 'beads-agent-claude-code nil t)
(require 'beads-agent-claudemacs nil t)
(require 'beads-agent-efrit nil t)

;;; beads-agent.el ends here
