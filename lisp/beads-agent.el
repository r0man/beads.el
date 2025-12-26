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
;; - claude-code-ide.el (MCP-based integration)
;; - claude-code.el (terminal-based integration)
;; - claudemacs (AI pair programming with eat terminal)
;; - efrit (Emacs-native AI agent framework)
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
(require 'beads-completion)
(require 'beads-agent-backend)
(require 'beads-agent-type)
(require 'beads-agent-types)
(require 'transient)

;; Forward declarations
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-show--issue-id "beads-show")
(declare-function beads-sesman--link-session-buffer "beads-sesman")
(defvar beads-show--issue-id)
(defvar beads-sesman--buffer-session-id)

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
CALLBACK receives (success output) where success is t/nil.
Returns the process object, which can be used to cancel the operation."
  (let* ((default-directory (or (beads--find-project-root)
                                default-directory))
         (output-buffer (generate-new-buffer " *beads-git-async*")))
    (make-process
     :name "beads-git"
     :buffer output-buffer
     :command (cons "git" args)
     :connection-type 'pipe
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (let ((success (zerop (process-exit-status proc)))
                   (output (when (buffer-live-p output-buffer)
                             (with-current-buffer output-buffer
                               (string-trim (buffer-string))))))
               (funcall callback success (or output "")))
           ;; Always kill buffer, even on error
           (when (buffer-live-p output-buffer)
             (kill-buffer output-buffer))))))))

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
      (let ((output-buffer (generate-new-buffer " *beads-worktree*")))
        (make-process
         :name "beads-worktree"
         :buffer output-buffer
         :command (list "git" "worktree" "add" "-b" issue-id worktree-path)
         :connection-type 'pipe
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (unwind-protect
                 (let ((success (zerop (process-exit-status proc))))
                   (if success
                       ;; Worktree created - uses shared database from main repo
                       (progn
                         (message "Created worktree for %s at %s" issue-id worktree-path)
                         (funcall callback t worktree-path))
                     ;; Branch might already exist, try without -b
                     (beads-agent--create-worktree-existing-branch-async
                      issue-id worktree-path callback)))
               ;; Always kill buffer, even on error
               (when (buffer-live-p output-buffer)
                 (kill-buffer output-buffer))))))))))

(defun beads-agent--create-worktree-existing-branch-async (issue-id worktree-path callback)
  "Create worktree for existing branch ISSUE-ID at WORKTREE-PATH.
CALLBACK receives (success worktree-path-or-error)."
  (let* ((main-root (beads-agent--main-repo-root))
         (default-directory main-root)
         (output-buffer (generate-new-buffer " *beads-worktree*")))
    (make-process
     :name "beads-worktree"
     :buffer output-buffer
     :command (list "git" "worktree" "add" worktree-path issue-id)
     :connection-type 'pipe
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (let ((success (zerop (process-exit-status proc)))
                   (output (when (buffer-live-p output-buffer)
                             (with-current-buffer output-buffer
                               (string-trim (buffer-string))))))
               (if success
                   ;; Worktree created - uses shared database from main repo
                   (progn
                     (message "Created worktree for %s at %s" issue-id worktree-path)
                     (funcall callback t worktree-path))
                 (funcall callback nil (format "Failed to create worktree: %s"
                                               (or output "unknown error")))))
           ;; Always kill buffer, even on error
           (when (buffer-live-p output-buffer)
             (kill-buffer output-buffer))))))))

;;; Backend Selection

(defun beads-agent--backend-available-and-get (name)
  "Return backend with NAME if available, nil otherwise.
NAME is a backend name string.  Returns the backend object if
it exists and is available, nil otherwise."
  (when-let ((backend (beads-agent--get-backend name)))
    (when (beads-agent-backend-available-p backend)
      backend)))

(defun beads-agent--select-backend (&optional agent-type)
  "Select backend using type preference, default, or prompt.
AGENT-TYPE is an optional `beads-agent-type' object.

Backend selection order:
1. Type-specific backend (from AGENT-TYPE's preferred-backend method)
2. Global default (`beads-agent-default-backend')
3. Prompt user to select from available backends

Returns a beads-agent-backend instance or signals an error.

When prompting, uses rich completion with annotations showing
priority and availability status."
  (let ((available (beads-agent--get-available-backends)))
    (unless available
      (user-error "No AI agent backends available.

To use AI agents, you need:
2. Install claude-code-ide.el package
3. Install Claude Code CLI: npm install -g @anthropic-ai/claude-code
4. Ensure `claude' command is in your PATH

See: https://github.com/anthropics/claude-code"))
    (or
     ;; 1. Try type-specific backend if agent-type provided
     (when agent-type
       (when-let ((type-pref (beads-agent-type-preferred-backend agent-type)))
         (beads-agent--backend-available-and-get type-pref)))
     ;; 2. Try global default backend
     (when beads-agent-default-backend
       (beads-agent--backend-available-and-get beads-agent-default-backend))
     ;; 3. Prompt user to select
     (progn
       (require 'beads-completion)
       (require 'beads-custom)
       (let* ((available-names (mapcar (lambda (b) (oref b name)) available))
              ;; When showing unavailable backends, don't filter the list
              ;; (they appear grayed out).  Validate selection after.
              (predicate (unless beads-completion-show-unavailable-backends
                           (lambda (cand)
                             (member (if (consp cand) (car cand) cand)
                                     available-names))))
              (choice (beads-completion-read-backend "Select backend: "
                                                     predicate
                                                     t))
              (backend (beads-agent--get-backend choice)))
         ;; Validate selection when unavailable backends were shown
         (when (and beads-completion-show-unavailable-backends
                    (not (beads-agent-backend-available-p backend)))
           (user-error "Backend '%s' is not available.  %s"
                       choice
                       (or (oref backend description)
                           "Install the required package")))
         backend)))))

;;;###autoload
(defun beads-agent-switch-backend (&optional save)
  "Switch default backend by selecting from available backends.
Uses rich completion with annotations showing priority and descriptions.

When called interactively with a prefix argument (\\[universal-argument]),
or when SAVE is non-nil, persist the selection to the custom file.

This sets `beads-agent-default-backend' so that future agent sessions
will automatically use the selected backend without prompting."
  (interactive "P")
  (require 'beads-completion)
  (require 'beads-custom)
  (let* ((available (beads-agent--get-available-backends)))
    (unless available
      (user-error "No AI agent backends available"))
    (let* ((available-names (mapcar (lambda (b) (oref b name)) available))
           (current beads-agent-default-backend)
           (prompt (if current
                       (format "Switch backend (current: %s): " current)
                     "Select default backend: "))
           ;; When showing unavailable backends, don't filter the list.
           (predicate (unless beads-completion-show-unavailable-backends
                        (lambda (cand)
                          (member (if (consp cand) (car cand) cand)
                                  available-names))))
           (choice (beads-completion-read-backend prompt
                                                predicate
                                                t))
           (backend (beads-agent--get-backend choice)))
      ;; Validate selection when unavailable backends were shown
      (when (and beads-completion-show-unavailable-backends
                 (not (beads-agent-backend-available-p backend)))
        (user-error "Backend '%s' is not available.  %s"
                    choice
                    (or (oref backend description)
                        "Install the required package")))
      (setq beads-agent-default-backend choice)
      (when save
        (customize-save-variable 'beads-agent-default-backend choice))
      (message "Default backend set to %s%s"
               choice
               (if save " (saved)" "")))))

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
  "Update ISSUE-ID to in_progress if currently open.
Only updates when `beads-agent-auto-set-in-progress' is non-nil.
CALLBACK receives no arguments when done."
  (if (not beads-agent-auto-set-in-progress)
      (funcall callback)
    ;; Get issue status first
    (let ((cmd (beads-command-show :issue-ids (list issue-id))))
      (beads-command-execute-async
       cmd
       (lambda (result)
         (if (not (zerop (oref result exit-code)))
             (funcall callback)  ; Skip on error
           ;; Parse and check status - data slot contains parsed issue(s)
           (condition-case nil
               (let* ((data (oref result data))
                      ;; data may be a single issue or vector of issues
                      (issue (if (vectorp data) (aref data 0) data))
                      (status (oref issue status)))
                 (if (equal status "open")
                     ;; Update to in_progress
                     (beads-agent--update-status-async issue-id callback)
                   (funcall callback)))
             (error (funcall callback)))))))))

(defun beads-agent--update-status-async (issue-id callback)
  "Set ISSUE-ID status to in_progress asynchronously.
CALLBACK receives no arguments when done."
  (let ((cmd (beads-command-update
              :issue-ids (list issue-id)
              :status "in_progress")))
    (beads-command-execute-async
     cmd
     (lambda (result)
       (when (zerop (oref result exit-code))
         (beads--invalidate-completion-cache)
         (message "Set %s to in_progress" issue-id))
       (funcall callback)))))

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
      (beads-completion-read-issue
       "Issue: " nil t nil 'beads--issue-id-history)))

;;; Public API Functions

;;;###autoload
(defun beads-agent-start (&optional issue-id backend-name prompt agent-type-name)
  "Start an AI agent working on ISSUE-ID asynchronously.
ISSUE-ID defaults to issue at point or prompts for selection.
BACKEND-NAME defaults to configured default or prompts for selection.
PROMPT defaults to auto-generated from issue (overridden by agent type).
AGENT-TYPE-NAME is the name of the agent type (e.g., \"Task\", \"Review\").
When not specified, defaults to \"Task\".  The agent type's prompt
template is used instead of PROMPT.

When `beads-agent-use-worktrees' is non-nil, the agent will work
in a git worktree named after the issue ID.  The worktree is
created automatically if it doesn't exist.

This function returns immediately.  The agent is started in the
background with progress messages displayed in the echo area."
  (interactive)
  (beads-check-executable)
  (let* ((issue-id (or issue-id (beads-agent--read-issue-id)))
         ;; Default to "Task" type when not specified
         (effective-type-name (or agent-type-name "Task"))
         (agent-type (or (beads-agent-type-get effective-type-name)
                         (user-error "Unknown agent type: %s" effective-type-name)))
         (backend (if backend-name
                      (or (beads-agent--get-backend backend-name)
                          (user-error "Backend not found: %s" backend-name))
                    (beads-agent--select-backend agent-type)))
         (project-dir (beads--find-project-root)))
    ;; Start the async workflow
    (message "Starting %s agent for %s..."
             (oref agent-type name)
             issue-id)
    (beads-agent--start-async issue-id backend project-dir prompt agent-type)))

(defun beads-agent--start-async (issue-id backend project-dir prompt agent-type)
  "Async implementation of agent start.
ISSUE-ID is the issue to work on.
BACKEND is the beads-agent-backend instance.
PROJECT-DIR is the project root directory.
PROMPT is the optional pre-built prompt (overridden by AGENT-TYPE).
AGENT-TYPE is an optional `beads-agent-type' instance."
  ;; Step 1: Fetch issue info (async)
  (beads-agent--fetch-issue-async
   issue-id
   (lambda (issue)
     ;; Handle fetch failure - issue is nil when fetch or parse failed
     (if (null issue)
         (message "Cannot start agent: failed to fetch issue %s" issue-id)
       ;; Restore default-directory for async callbacks.
       ;; Process sentinels run with arbitrary default-directory, but
       ;; worktree functions need the project directory for git commands.
       (let* ((default-directory project-dir)
              ;; Build prompt: agent-type takes precedence, then explicit prompt,
              ;; then default issue-based prompt
              (effective-prompt
               (cond
                ;; Agent type builds its own prompt
                (agent-type
                 (beads-agent-type-build-prompt agent-type issue))
                ;; Explicit prompt provided
                (prompt prompt)
                ;; Default: build from issue
                (t (beads-agent--build-prompt issue)))))
         ;; Step 2: Setup worktree if needed (async)
         (if beads-agent-use-worktrees
             (beads-agent--ensure-worktree-async
              issue-id
              (lambda (success result)
                ;; Also restore here for nested async callbacks
                (let ((default-directory project-dir))
                  (if success
                      ;; Step 3: Update status (async)
                      (beads-agent--continue-start
                       issue-id backend project-dir result effective-prompt issue agent-type)
                    (message "Failed to create worktree: %s" result)))))
           ;; No worktree, continue directly
           (beads-agent--continue-start
            issue-id backend project-dir nil effective-prompt issue agent-type)))))))

(defun beads-agent--continue-start (issue-id backend project-dir worktree-dir
                                            prompt issue agent-type)
  "Continue agent start after worktree is ready.
ISSUE-ID, BACKEND, PROJECT-DIR, WORKTREE-DIR, PROMPT, ISSUE, AGENT-TYPE
are passed through from `beads-agent--start-async'."
  ;; Update status async, then start backend
  (beads-agent--maybe-update-status-async
   issue-id
   (lambda ()
     ;; Step 4: Start the backend
     (beads-agent--start-backend-async
      issue-id backend project-dir worktree-dir prompt issue agent-type))))

(defun beads-agent--fetch-issue-async (issue-id callback)
  "Fetch ISSUE-ID asynchronously and call CALLBACK with result.
CALLBACK receives a beads-issue object, or nil on error."
  (let ((cmd (beads-command-show :issue-ids (list issue-id))))
    (beads-command-execute-async
     cmd
     (lambda (result)
       (if (not (zerop (oref result exit-code)))
           (progn
             (message "Failed to fetch issue %s (exit %d): %s"
                      issue-id (oref result exit-code)
                      (string-trim (oref result stderr)))
             (funcall callback nil))
         ;; data slot contains parsed issue(s) - a vector from bd show
         (condition-case err
             (let* ((data (oref result data))
                    ;; bd show returns a vector, extract first element
                    (issue (if (vectorp data) (aref data 0) data)))
               (funcall callback issue))
           (error
            (message "Failed to parse issue: %s" (error-message-string err))
            (funcall callback nil))))))))

(defun beads-agent--rename-and-store-buffer (session buffer)
  "Rename BUFFER to beads format and store in SESSION.
Renames the buffer to the beads naming convention and stores it in
the session's buffer slot for efficient lookup.
The buffer is renamed to format: *beads-agent[PROJECT-NAME][TYPE#N]*
Also sets the buffer's `default-directory' to the session's working
directory (worktree or project) for correct beads database discovery.

This function is idempotent: if SESSION already has a buffer stored,
does nothing to avoid renaming an already-renamed buffer."
  (when (and buffer (buffer-live-p buffer))
    ;; Skip if session already has a buffer (idempotent)
    (unless (oref session buffer)
      (let* ((new-name (beads-agent--generate-buffer-name-for-project-session session))
             ;; Get working directory: worktree takes precedence over project
             (working-dir (or (oref session worktree-dir)
                              (oref session project-dir))))
        ;; Rename the buffer and set its working directory
        (with-current-buffer buffer
          (rename-buffer new-name t)
          ;; Set default-directory for worktree support - ensures beads commands
          ;; find the correct .beads database when invoked from this buffer
          (setq-local default-directory working-dir))
        ;; Store in session
        (beads-agent-session-set-buffer session buffer)
        ;; Link buffer to sesman session (deferred from session creation)
        ;; This sets sesman-system and kill-buffer-hook in the buffer
        (beads-sesman--link-session-buffer session buffer)))))

(defun beads-agent--start-backend-async (issue-id backend project-dir worktree-dir
                                                  prompt issue agent-type)
  "Start the backend asynchronously.
ISSUE-ID, BACKEND, PROJECT-DIR, WORKTREE-DIR, PROMPT, ISSUE, AGENT-TYPE
are passed through from `beads-agent--continue-start'."
  (let* ((working-dir (or worktree-dir project-dir))
         ;; In worktrees, add BD_NO_DAEMON=1 for worktree safety
         ;; (prevents daemon from being started in worktree context)
         (process-environment (if worktree-dir
                                  (cons "BD_NO_DAEMON=1" process-environment)
                                process-environment))
         (default-directory working-dir)
         (agent-type-name (when agent-type (oref agent-type name))))
    (condition-case err
        ;; Bind display-buffer-overriding-action to force agent buffers
        ;; to open in a window to the right, keeping list/show buffers visible.
        ;; Uses display-buffer-in-direction (regular window) rather than
        ;; display-buffer-in-side-window (side window with special behavior).
        (let* ((display-buffer-overriding-action
                '((display-buffer-reuse-window
                   display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.5)))
               ;; backend-start returns (backend-session . buffer)
               (start-result (beads-agent-backend-start backend issue prompt))
               (backend-session (car start-result))
               (buffer (cdr start-result))
               (session (beads-agent--create-session
                         issue-id
                         (oref backend name)
                         project-dir
                         backend-session
                         worktree-dir
                         agent-type-name)))
          ;; Rename backend buffer to beads format and store in session
          (beads-agent--rename-and-store-buffer session buffer)
          (message "Started %s agent session %s on %s%s"
                   (or agent-type-name "default")
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
                                      (cons (format "%s %s"
                                                    (oref s issue-id)
                                                    (beads-agent--session-display-name s))
                                            (oref s id)))
                                    sessions)
                            nil t))))
         ;; Extract session-id from alist choice or string
         (session-id (cond
                      ((consp session-id) (cdr session-id))
                      ((string-match "^\\([^ ]+\\)" session-id)
                       (match-string 1 session-id))
                      (t session-id)))
         (session (beads-agent--get-session session-id)))
    (unless session
      (user-error "Session not found: %s" session-id))
    (when-let ((backend (beads-agent--get-backend (oref session backend-name))))
      ;; Clear buffer-local session ID BEFORE killing buffer to prevent
      ;; kill-buffer-hook from calling beads-agent-stop recursively.
      (when-let ((buffer (beads-agent-backend-get-buffer backend session)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local beads-sesman--buffer-session-id nil))))
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
            (progn
              ;; Clear buffer-local session ID BEFORE killing buffer to prevent
              ;; kill-buffer-hook from calling beads-agent-stop recursively.
              (when-let ((buffer (beads-agent-backend-get-buffer backend session)))
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (setq-local beads-sesman--buffer-session-id nil))))
              (beads-agent-backend-stop-async
               backend session
               (lambda ()
                 (beads-agent--destroy-session session-id)
                 (message "Stopped session %s" session-id)
                 (when callback (funcall callback)))))
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
                                      (cons (format "%s %s"
                                                    (oref s issue-id)
                                                    (beads-agent--session-display-name s))
                                            (oref s id)))
                                    sessions)
                            nil t))))
         ;; Extract session-id from alist choice or string
         (session-id (cond
                      ((consp session-id) (cdr session-id))
                      ((string-match "^\\([^ ]+\\)" session-id)
                       (match-string 1 session-id))
                      (t session-id)))
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
                                      (cons (format "%s %s"
                                                    (oref s issue-id)
                                                    (beads-agent--session-display-name s))
                                            (oref s id)))
                                    sessions)
                            nil t))))
         ;; Extract session-id from alist choice or string
         (session-id (cond
                      ((consp session-id) (cdr session-id))
                      ((string-match "^\\([^ ]+\\)" session-id)
                       (match-string 1 session-id))
                      (t session-id)))
         (session (beads-agent--get-session session-id)))
    (unless session
      (user-error "Session not found: %s" session-id))
    (when-let ((backend (beads-agent--get-backend (oref session backend-name))))
      (beads-agent-backend-send-prompt backend session prompt)
      (message "Sent prompt to session %s" session-id))))

;;; Forward Declaration for Agent List

(declare-function beads-agent-list "beads-agent-list")

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

;;; Per-Issue Agent Menu

(defvar beads-agent-issue--current-issue-id nil
  "Current issue ID for the per-issue agent menu.
Set by `beads-agent-issue' before displaying the menu.")

(defun beads-agent--session-display-name (session)
  "Return display name for SESSION in per-issue menu.
Format: \"Type#N (backend-name)\" where Type is the agent type
and N is the instance number."
  (let* ((type-name (or (beads-agent-session-type-name session) "Agent"))
         (instance-n (beads-agent--session-instance-number session))
         (backend (oref session backend-name)))
    (format "%s#%d (%s)" type-name instance-n backend)))

(defun beads-agent-issue--format-header ()
  "Format header for per-issue agent menu."
  (let* ((issue-id beads-agent-issue--current-issue-id)
         (sessions (beads-agent--get-sessions-for-issue issue-id))
         (count (length sessions)))
    (format "Agents for %s [%d active]" issue-id count)))

(defun beads-agent-issue--get-sessions ()
  "Get sessions for current issue, sorted by session number."
  (let ((sessions (beads-agent--get-sessions-for-issue
                   beads-agent-issue--current-issue-id)))
    (sort sessions
          (lambda (a b)
            (let ((num-a (or (beads-agent--session-number-from-id (oref a id)) 0))
                  (num-b (or (beads-agent--session-number-from-id (oref b id)) 0)))
              (< num-a num-b))))))

(transient-define-suffix beads-agent-issue--start-new ()
  "Start a new agent for the current issue."
  :key "s"
  :description "Start new agent"
  (interactive)
  (beads-agent-start beads-agent-issue--current-issue-id))

(transient-define-suffix beads-agent-issue--stop-one ()
  "Stop one agent session for the current issue."
  :key "k"
  :description "Stop one agent"
  (interactive)
  (let* ((sessions (beads-agent-issue--get-sessions)))
    (if (null sessions)
        (message "No active agents for %s" beads-agent-issue--current-issue-id)
      (let* ((choices (mapcar (lambda (s)
                                (cons (beads-agent--session-display-name s)
                                      (oref s id)))
                              sessions))
             (choice (completing-read "Stop agent: " choices nil t))
             (session-id (cdr (assoc choice choices))))
        (beads-agent-stop session-id)))))

(transient-define-suffix beads-agent-issue--stop-all ()
  "Stop all agents for the current issue."
  :key "K"
  :description "Stop all agents"
  (interactive)
  (let* ((sessions (beads-agent-issue--get-sessions))
         (count (length sessions)))
    (if (zerop count)
        (message "No active agents for %s" beads-agent-issue--current-issue-id)
      (when (y-or-n-p (format "Stop all %d agent%s for %s? "
                              count (if (= count 1) "" "s")
                              beads-agent-issue--current-issue-id))
        (dolist (session sessions)
          (beads-agent-stop (oref session id)))
        (message "Stopped %d agent%s" count (if (= count 1) "" "s"))))))

(transient-define-suffix beads-agent-issue--refresh ()
  "Refresh the per-issue menu."
  :key "g"
  :description "Refresh"
  :transient t
  (interactive)
  (transient--redisplay))

(defun beads-agent-issue--make-jump-suffix (session index)
  "Create a jump suffix for SESSION at INDEX (1-based)."
  (let ((key (format "j%d" index))
        (desc (beads-agent--session-display-name session))
        (session-id (oref session id)))
    `(,key ,desc
           (lambda ()
             (interactive)
             (beads-agent-jump ,session-id)))))

(defun beads-agent-issue--setup-agents ()
  "Return vector of jump suffixes for active agents."
  (let* ((sessions (beads-agent-issue--get-sessions))
         (suffixes nil)
         (index 1))
    (dolist (session sessions)
      (push (beads-agent-issue--make-jump-suffix session index) suffixes)
      (cl-incf index))
    (if suffixes
        (vconcat (nreverse suffixes))
      [("" "No active agents" ignore)])))

;;;###autoload (autoload 'beads-agent-issue "beads-agent" nil t)
(transient-define-prefix beads-agent-issue (issue-id)
  "Manage AI agents for a specific issue.

Shows all active agents for ISSUE-ID with jump keys to switch
to each agent's buffer.

ISSUE-ID is required; detected from context or prompted."
  [:description
   (lambda () (beads-agent-issue--format-header))]
  ["Active Agents"
   :class transient-column
   :setup-children
   (lambda (_)
     (mapcar (lambda (spec)
               (transient-parse-suffix
                'beads-agent-issue spec))
             (append (beads-agent-issue--setup-agents) nil)))]
  ["Actions"
   (beads-agent-issue--start-new)
   (beads-agent-issue--stop-one)
   (beads-agent-issue--stop-all)]
  ["Other"
   (beads-agent-issue--refresh)
   ("q" "Quit" transient-quit-one)]
  (interactive (list (or (beads-agent--detect-issue-id)
                         (beads-agent--read-issue-id))))
  (setq beads-agent-issue--current-issue-id issue-id)
  (transient-setup 'beads-agent-issue))

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

;;; beads-agent-start-menu - Sub-menu for starting agents

;; Forward declare reader functions (defined in beads-reader.el)
(declare-function beads-reader-issue-id "beads-reader")
(declare-function beads-reader-agent-backend "beads-reader")

(defun beads-agent-start--format-header ()
  "Format header for agent start menu."
  (let* ((backends (beads-agent--get-available-backends))
         (backend-count (length backends))
         (context-id (beads-agent--detect-issue-id)))
    (concat "Start AI Agent"
            (when context-id
              (format " [context: %s]" context-id))
            (format " (%d backend%s available)"
                    backend-count (if (= backend-count 1) "" "s")))))

(transient-define-infix beads-agent-start--infix-issue-id ()
  "Set the issue ID to start agent on."
  :class 'transient-option
  :description "Issue"
  :key "i"
  :argument "--issue="
  :prompt "Issue: "
  :reader #'beads-reader-issue-id)

(transient-define-infix beads-agent-start--infix-backend ()
  "Set the backend to use for the agent."
  :class 'transient-option
  :description "Backend"
  :key "b"
  :argument "--backend="
  :prompt "Backend: "
  :reader #'beads-reader-agent-backend)

(transient-define-suffix beads-agent-start--execute ()
  "Execute agent start with current parameters."
  :key "x"
  :description "Start agent"
  (interactive)
  (let* ((args (transient-args 'beads-agent-start-menu))
         (issue-id (transient-arg-value "--issue=" args))
         (backend-name (transient-arg-value "--backend=" args)))
    (unless issue-id
      (user-error "Issue ID is required"))
    (beads-agent-start issue-id backend-name)))

(transient-define-suffix beads-agent-start--preview ()
  "Preview the agent start configuration."
  :key "v"
  :description "Preview"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-agent-start-menu))
         (issue-id (transient-arg-value "--issue=" args))
         (backend-name (transient-arg-value "--backend=" args))
         (worktree-status (if beads-agent-use-worktrees "yes" "no")))
    (message "Start agent: issue=%s backend=%s worktree=%s"
             (or issue-id "[not set]")
             (or backend-name "[auto-select]")
             worktree-status)))

(transient-define-suffix beads-agent-start--reset ()
  "Reset all parameters to defaults."
  :key "R"
  :description "Reset"
  :transient t
  (interactive)
  (transient-reset)
  (message "Parameters reset"))

;;;###autoload (autoload 'beads-agent-start-menu "beads-agent" nil t)
(transient-define-prefix beads-agent-start-menu ()
  "Start an AI agent on an issue.

This menu allows configuring the agent start parameters:
- Issue ID: The issue to work on (auto-detected from context)
- Backend: The AI backend to use (auto-selected if not specified)

The agent will work in a git worktree if `beads-agent-use-worktrees'
is enabled (default)."
  [:description
   (lambda () (beads-agent-start--format-header))]
  ["Options"
   (beads-agent-start--infix-issue-id)
   (beads-agent-start--infix-backend)]
  ["Actions"
   (beads-agent-start--execute)
   (beads-agent-start--preview)
   (beads-agent-start--reset)
   ("q" "Quit" transient-quit-one)]
  (interactive)
  ;; Pre-populate issue-id from context if available
  (let ((context-id (beads-agent--detect-issue-id)))
    (when context-id
      (transient-set-value 'beads-agent-start-menu
                           (list (concat "--issue=" context-id)))))
  (transient-setup 'beads-agent-start-menu))

(transient-define-suffix beads-agent--start-suffix ()
  "Start agent on an issue."
  :key "s"
  :description "Start agent on issue"
  (interactive)
  (call-interactively #'beads-agent-start-menu))

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
  (call-interactively #'beads-agent-list))

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

(transient-define-suffix beads-agent--switch-backend-suffix ()
  "Switch the default backend."
  :key "B"
  :description (lambda ()
                 (if beads-agent-default-backend
                     (format "Switch backend (current: %s)"
                             beads-agent-default-backend)
                   "Set default backend"))
  :transient t
  (interactive)
  (call-interactively #'beads-agent-switch-backend)
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
  ["Configuration"
   (beads-agent--switch-backend-suffix)]
  ["Other"
   (beads-agent--refresh-suffix)
   ("q" "Quit" transient-quit-one)])

;;; Context-Aware Commands (for list/show buffers)

;;;###autoload
(defun beads-agent-start-at-point ()
  "Start AI agent for issue at point, or manage existing agents.
If agents exist for this issue, show the management menu.
Otherwise, start an agent directly.

When called from `beads-list-mode' or `beads-show-mode', the list/show
buffer is kept visible and the agent buffer opens in the other window."
  (interactive)
  (if-let ((id (beads-agent--detect-issue-id)))
      ;; Check for existing sessions first
      (if (beads-agent--get-sessions-for-issue id)
          ;; Sessions exist - show management menu
          (beads-agent-issue id)
        ;; No sessions - start new agent directly
        ;; Each backend's display-buffer-overriding-action handles window placement
        (beads-agent-start id))
    (call-interactively #'beads-agent-start)))

(defun beads-agent--select-session-completing-read (sessions prompt)
  "Select a session from SESSIONS using `completing-read' with PROMPT.
Returns the selected session object, or nil if SESSIONS is empty.
The completion shows format \"Type#N (backend)\" for each session."
  (when sessions
    (let* ((choices
            (mapcar
             (lambda (s)
               (let* ((type-name (or (beads-agent-session-type-name s) "Agent"))
                      (instance-n (beads-agent--session-instance-number s))
                      (backend (oref s backend-name))
                      (label (format "%s#%d (%s)" type-name instance-n backend)))
                 (cons label s)))
             sessions))
           (selection (completing-read prompt choices nil t)))
      (cdr (assoc selection choices)))))

;;;###autoload
(defun beads-agent-jump-at-point ()
  "Jump to agent buffer for issue at point, starting one if needed.
If no session exists for the current issue, starts a new agent session.
If multiple sessions exist, prompts for which one to jump to."
  (interactive)
  (if-let ((id (beads-agent--detect-issue-id)))
      (if-let ((sessions (beads-agent--get-sessions-for-issue id)))
          (cond
           ;; Single session - jump directly
           ((= (length sessions) 1)
            (beads-agent-jump (oref (car sessions) id)))
           ;; Multiple sessions - prompt for selection
           (t
            (if-let ((selected (beads-agent--select-session-completing-read
                                sessions
                                (format "Jump to agent for %s: " id))))
                (beads-agent-jump (oref selected id))
              (message "No agent selected"))))
        ;; No session - start a new one
        (beads-agent-start id))
    (call-interactively #'beads-agent-jump)))

;;; Agent Type Start Commands
;;
;; These commands implement smart jump-or-start logic for each agent type.
;; If no C-u prefix and an active agent of the same type exists for the issue,
;; they jump to its buffer.  Otherwise, they start a new agent.

(defun beads-agent--get-sessions-for-issue-type (issue-id type-name)
  "Get sessions for ISSUE-ID that match TYPE-NAME.
TYPE-NAME is the agent type name (e.g., \"Task\", \"Review\").
Returns list of matching sessions."
  (cl-remove-if-not
   (lambda (session)
     (equal (beads-agent-session-type-name session) type-name))
   (beads-agent--get-sessions-for-issue issue-id)))

(defun beads-agent--start-typed (type-name &optional force-new)
  "Start or jump to agent of TYPE-NAME for issue at point.
When FORCE-NEW is non-nil, always start a new agent even if one exists.
When existing sessions of the same type exist and FORCE-NEW is nil:
- If one session exists, jumps to it directly.
- If multiple sessions exist, prompts for which one to jump to.
This is the core implementation for all type-specific start commands."
  (if-let ((id (beads-agent--detect-issue-id)))
      (let ((existing (beads-agent--get-sessions-for-issue-type id type-name)))
        (if (and existing (not force-new))
            ;; Jump to existing session of this type
            (cond
             ;; Single session - jump directly
             ((= (length existing) 1)
              (beads-agent-jump (oref (car existing) id)))
             ;; Multiple sessions - prompt for selection
             (t
              (if-let ((selected (beads-agent--select-session-completing-read
                                  existing
                                  (format "Jump to %s agent for %s: " type-name id))))
                  (beads-agent-jump (oref selected id))
                (message "No agent selected"))))
          ;; Start new agent of this type
          ;; Note: Window splitting is handled in beads-agent--start-backend-async
          ;; to occur after backend selection, not before
          (beads-agent-start id nil nil type-name)))
    (user-error "No issue at point")))

;;;###autoload
(defun beads-agent-start-task (&optional arg)
  "Start or jump to Task agent for issue at point.
With prefix ARG, always start a new agent even if one exists.
Without prefix, jumps to existing Task agent if one is running."
  (interactive "P")
  (beads-agent--start-typed "Task" arg))

;;;###autoload
(defun beads-agent-start-review (&optional arg)
  "Start or jump to Review agent for issue at point.
With prefix ARG, always start a new agent even if one exists.
Without prefix, jumps to existing Review agent if one is running."
  (interactive "P")
  (beads-agent--start-typed "Review" arg))

;;;###autoload
(defun beads-agent-start-plan (&optional arg)
  "Start or jump to Plan agent for issue at point.
With prefix ARG, always start a new agent even if one exists.
Without prefix, jumps to existing Plan agent if one is running.
The Plan agent analyzes and creates implementation plans without
making changes, working with any backend."
  (interactive "P")
  (beads-agent--start-typed "Plan" arg))

;;;###autoload
(defun beads-agent-start-qa (&optional arg)
  "Start or jump to QA agent for issue at point.
With prefix ARG, always start a new agent even if one exists.
Without prefix, jumps to existing QA agent if one is running."
  (interactive "P")
  (beads-agent--start-typed "QA" arg))

;;;###autoload
(defun beads-agent-start-custom (&optional arg)
  "Start or jump to Custom agent for issue at point.
With prefix ARG, always start a new agent even if one exists.
Without prefix, jumps to existing Custom agent if one is running.
Prompts for a custom prompt string to send to the agent."
  (interactive "P")
  (beads-agent--start-typed "Custom" arg))

;;;###autoload
(defun beads-agent-stop-at-point ()
  "Stop agent for issue at point.
If multiple agents exist, prompts for which one to stop.
If no agent exists, shows a message."
  (interactive)
  (if-let ((id (beads-agent--detect-issue-id)))
      (if-let ((sessions (beads-agent--get-sessions-for-issue id)))
          (cond
           ;; Single session - stop it directly
           ((= (length sessions) 1)
            (beads-agent-stop (oref (car sessions) id)))
           ;; Multiple sessions - prompt for selection
           (t
            (if-let ((selected (beads-agent--select-session-completing-read
                                sessions
                                (format "Stop agent for %s: " id))))
                (beads-agent-stop (oref selected id))
              (message "No agent selected"))))
        (message "No agents running for %s" id))
    (user-error "No issue at point")))

;;; Directory-Bound Session Focus Commands
;;
;; Commands to switch issue focus within a directory-bound session.
;; Since agents now persist across issues, users need to switch focus
;; without restarting the agent.

(defun beads-agent--get-current-project-session ()
  "Get the current directory-bound session for the project.
Looks for sessions matching the current project directory.
Returns the most recent session if multiple exist, or nil if none."
  (when-let ((project-dir (or (beads--find-project-root)
                               default-directory)))
    (let ((sessions (beads-agent--get-sessions-for-project project-dir)))
      (car sessions))))  ; Most recent session

;;;###autoload
(defun beads-agent-focus-issue (issue-id)
  "Set current agent focus to ISSUE-ID.
Adds ISSUE-ID to touched-issues if not already present.
Uses the current directory-bound session for this project."
  (interactive (list (beads-completion-read-issue "Focus on issue: ")))
  (if-let ((session (beads-agent--get-current-project-session)))
      (progn
        (beads-agent-session-set-current-issue session issue-id)
        (message "Agent now focused on %s" issue-id))
    (user-error "No active agent session for current project")))

;;;###autoload
(defun beads-agent-clear-focus ()
  "Clear current issue focus.
Agent continues in general project context without focus.
Uses the current directory-bound session for this project."
  (interactive)
  (if-let ((session (beads-agent--get-current-project-session)))
      (progn
        (oset session current-issue nil)
        (message "Agent focus cleared (project context)"))
    (user-error "No active agent session for current project")))

;;;###autoload
(defun beads-agent-show-touched ()
  "Show all issues touched by the current agent session.
Displays the list of issues that have been focused on during this session."
  (interactive)
  (if-let ((session (beads-agent--get-current-project-session)))
      (let ((touched (beads-agent-session-touched-issues session))
            (current (beads-agent-session-current-issue session)))
        (if touched
            (message "Touched issues: %s%s"
                     (string-join touched ", ")
                     (if current
                         (format " (current: %s)" current)
                       ""))
          (message "No issues touched yet%s"
                   (if current
                       (format " (current: %s)" current)
                     ""))))
    (user-error "No active agent session for current project")))

;;; Mode-Line Indicator
;;
;; Provides a mode-line segment showing project/agent context.
;; Example displays:
;;   [beads.el:Task#1@wt]  -- in worktree with active Task agent
;;   [beads.el:main]       -- in main repo, no agent

(defcustom beads-agent-mode-line-format 'default
  "Format for the mode-line indicator.
The value can be one of:
  - `default': Show [project:agent@context] format
  - `compact': Show just [P:T#1] minimal format
  - `full': Show full project path and agent details
  - A function: Called with no arguments, returns a string or nil"
  :type '(choice (const :tag "Default format" default)
                 (const :tag "Compact format" compact)
                 (const :tag "Full format" full)
                 (function :tag "Custom function"))
  :group 'beads-agent)

(defcustom beads-agent-mode-line-faces t
  "When non-nil, use faces for mode-line indicator.
When nil, use plain text without face properties."
  :type 'boolean
  :group 'beads-agent)

(defface beads-agent-mode-line-project
  '((t :inherit mode-line-buffer-id))
  "Face for project name in mode-line indicator."
  :group 'beads-agent)

(defface beads-agent-mode-line-agent
  '((t :inherit success))
  "Face for active agent indicator in mode-line."
  :group 'beads-agent)

(defface beads-agent-mode-line-worktree
  '((t :inherit warning))
  "Face for worktree indicator in mode-line."
  :group 'beads-agent)

;;; Mode-Line Context Cache
;;
;; Cache git branch and worktree status to avoid shelling out on every
;; mode-line redraw.  Cache is per-directory with a short TTL.

(defvar beads-agent--mode-line-cache nil
  "Cache for mode-line context: (directory branch in-worktree timestamp).")

(defconst beads-agent--mode-line-cache-ttl 5.0
  "Time-to-live for mode-line cache in seconds.")

(defun beads-agent--mode-line-cache-valid-p ()
  "Return non-nil if the mode-line cache is still valid."
  (and beads-agent--mode-line-cache
       (equal (car beads-agent--mode-line-cache) default-directory)
       (let ((timestamp (nth 3 beads-agent--mode-line-cache)))
         (< (- (float-time) timestamp) beads-agent--mode-line-cache-ttl))))

(defun beads-agent--mode-line-cached-git-info ()
  "Return cached (branch . in-worktree) or fetch and cache.
Returns a cons cell (BRANCH . IN-WORKTREE)."
  (if (beads-agent--mode-line-cache-valid-p)
      (cons (nth 1 beads-agent--mode-line-cache)
            (nth 2 beads-agent--mode-line-cache))
    ;; Cache miss - fetch and store
    (let ((branch (beads--get-git-branch))
          (in-worktree (beads--in-git-worktree-p)))
      (setq beads-agent--mode-line-cache
            (list default-directory branch in-worktree (float-time)))
      (cons branch in-worktree))))

(defun beads-agent--mode-line-context ()
  "Get the current context for mode-line display.
Returns a plist with keys:
  :project-name  - Project name string or nil
  :branch        - Git branch name or nil
  :in-worktree   - Non-nil if in a git worktree
  :agent-session - Active beads-agent-session or nil
  :agent-type    - Agent type name (e.g., \"Task\") or nil
  :agent-instance - Agent instance number or nil

Git branch and worktree status are cached for performance."
  (let* ((project-name (beads--get-project-name))
         (git-info (beads-agent--mode-line-cached-git-info))
         (branch (car git-info))
         (in-worktree (cdr git-info))
         (session (beads-agent--get-current-project-session))
         (agent-type (when session
                       (beads-agent-session-type-name session)))
         (agent-instance (when session
                           (beads-agent--session-instance-number session))))
    (list :project-name project-name
          :branch branch
          :in-worktree in-worktree
          :agent-session session
          :agent-type agent-type
          :agent-instance agent-instance)))

(defun beads-agent--mode-line-format-default (ctx)
  "Format mode-line using default format from context CTX.
CTX is a plist from `beads-agent--mode-line-context'.
Returns a string like [beads.el:Task#1@wt] or [beads.el:main]."
  (let* ((project-name (plist-get ctx :project-name))
         (branch (plist-get ctx :branch))
         (in-worktree (plist-get ctx :in-worktree))
         (agent-type (plist-get ctx :agent-type))
         (agent-instance (plist-get ctx :agent-instance))
         (use-faces beads-agent-mode-line-faces))
    (when project-name
      (let* ((proj-str (if use-faces
                           (propertize project-name
                                       'face 'beads-agent-mode-line-project)
                         project-name))
             (agent-str (when agent-type
                          (let ((str (format "%s#%d"
                                             agent-type
                                             (or agent-instance 1))))
                            (if use-faces
                                (propertize str
                                            'face 'beads-agent-mode-line-agent)
                              str))))
             (context-str (if in-worktree
                              (if use-faces
                                  (propertize "wt"
                                              'face 'beads-agent-mode-line-worktree)
                                "wt")
                            (or branch "main"))))
        (concat "["
                proj-str
                ":"
                (if agent-str
                    (concat agent-str "@" context-str)
                  context-str)
                "]")))))

(defun beads-agent--mode-line-format-compact (ctx)
  "Format mode-line using compact format from context CTX.
CTX is a plist from `beads-agent--mode-line-context'.
Returns a string like [P:T#1] where P is first char of project."
  (let* ((project-name (plist-get ctx :project-name))
         (agent-type (plist-get ctx :agent-type))
         (agent-instance (plist-get ctx :agent-instance))
         (in-worktree (plist-get ctx :in-worktree)))
    (when (and project-name (> (length project-name) 0))
      (let* ((p-char (substring project-name 0 1))
             (agent-str (when (and agent-type (> (length agent-type) 0))
                          (format "%s#%d"
                                  (substring agent-type 0 1)
                                  (or agent-instance 1))))
             (wt-str (if in-worktree "*" "")))
        (concat "[" p-char
                (when agent-str (concat ":" agent-str))
                wt-str "]")))))

(defun beads-agent--mode-line-format-full (ctx)
  "Format mode-line using full format from context CTX.
CTX is a plist from `beads-agent--mode-line-context'.
Returns a string with full project path and agent details."
  (let* ((project-name (plist-get ctx :project-name))
         (branch (plist-get ctx :branch))
         (in-worktree (plist-get ctx :in-worktree))
         (session (plist-get ctx :agent-session))
         (agent-type (plist-get ctx :agent-type))
         (agent-instance (plist-get ctx :agent-instance))
         (use-faces beads-agent-mode-line-faces))
    (when project-name
      (let* ((proj-str (if use-faces
                           (propertize project-name
                                       'face 'beads-agent-mode-line-project)
                         project-name))
             (branch-str (or branch "main"))
             (wt-str (if in-worktree
                         (if use-faces
                             (propertize " [worktree]"
                                         'face 'beads-agent-mode-line-worktree)
                           " [worktree]")
                       ""))
             (agent-str (when session
                          (let* ((current-issue
                                  (beads-agent-session-current-issue session))
                                 (str (format " %s#%d%s"
                                              agent-type
                                              (or agent-instance 1)
                                              (if current-issue
                                                  (format " (%s)" current-issue)
                                                ""))))
                            (if use-faces
                                (propertize str
                                            'face 'beads-agent-mode-line-agent)
                              str)))))
        (concat "[" proj-str ":" branch-str wt-str
                (or agent-str "") "]")))))

(defun beads-agent--mode-line-indicator ()
  "Return mode-line string for current beads context.
Uses `beads-agent-mode-line-format' to determine the display format.
Returns nil if not in a beads project."
  (let ((ctx (beads-agent--mode-line-context)))
    (pcase beads-agent-mode-line-format
      ('default (beads-agent--mode-line-format-default ctx))
      ('compact (beads-agent--mode-line-format-compact ctx))
      ('full (beads-agent--mode-line-format-full ctx))
      ((pred functionp) (funcall beads-agent-mode-line-format))
      (_ (beads-agent--mode-line-format-default ctx)))))

(defvar beads-agent-mode-line
  '(:eval (beads-agent--mode-line-indicator))
  "Mode line construct for beads agent context indicator.
Add this to `mode-line-misc-info' or your mode-line format to display
the current project/agent context.

Example usage:
  (add-to-list \\='mode-line-misc-info \\='beads-agent-mode-line)")

;;;###autoload
(put 'beads-agent-mode-line 'risky-local-variable t)

(defconst beads-agent--mode-line-misc-info-entry
  '(beads-agent-mode-line-mode (" " beads-agent-mode-line))
  "Entry added to `mode-line-misc-info' by `beads-agent-mode-line-mode'.
This uses the minor-mode conditional format so the indicator only
appears when the mode is enabled, and we can identify it precisely
for removal.")

;;;###autoload
(define-minor-mode beads-agent-mode-line-mode
  "Minor mode to display beads agent context in the mode-line.
When enabled, shows project name, agent status, and worktree
indicator in the mode-line.

Example displays:
  [beads.el:Task#1@wt]  -- in worktree with active Task agent
  [beads.el:main]       -- in main repo, no agent

Customize `beads-agent-mode-line-format' to change the display style."
  :global t
  :lighter nil
  :group 'beads-agent
  (if beads-agent-mode-line-mode
      ;; Enable: add our entry to mode-line-misc-info if not present
      (unless (member beads-agent--mode-line-misc-info-entry mode-line-misc-info)
        (setq mode-line-misc-info
              (append mode-line-misc-info
                      (list beads-agent--mode-line-misc-info-entry))))
    ;; Disable: remove only our specific entry
    (setq mode-line-misc-info
          (delete beads-agent--mode-line-misc-info-entry mode-line-misc-info))))

(provide 'beads-agent)

;;; Load Available Backends

;; Load all available backend implementations.
;; Each backend self-registers when loaded.
;; Using (require 'foo nil t) allows graceful handling of missing backends.
(require 'beads-agent-claude-code-ide nil t)
(require 'beads-agent-claude-code nil t)
(require 'beads-agent-claudemacs nil t)
(require 'beads-agent-agent-shell nil t)
(require 'beads-agent-efrit nil t)

;;; Load Agent List Module

;; Load the tabulated-list UI for agent sessions.
(require 'beads-agent-list)

;;; beads-agent.el ends here
