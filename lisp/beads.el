;;; beads.el --- Magit-like interface for Beads issue tracker -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; Keywords: tools, project, issues
;; URL: https://github.com/josephburnett/beads

;;; Commentary:

;; beads.el provides a Magit-like Emacs interface for the Beads issue
;; tracker.  It offers:
;;
;; - Transient menus for all bd CLI commands
;; - Tabulated-list buffers for viewing issues
;; - Issue detail view with markdown rendering
;; - Context-aware commands
;; - Full integration with project.el
;;
;; Usage:
;;
;;   M-x beads RET        ; Open main transient menu
;;   M-x beads-list RET   ; List all issues
;;   M-x beads-ready RET  ; Show ready work
;;
;; See README.md for full documentation.

;;; Code:

(require 'json)
(require 'project)
(require 'transient)

;;; Customization

(defgroup beads nil
  "Magit-like interface for Beads issue tracker."
  :group 'tools
  :prefix "beads-")

(defcustom beads-executable "bd"
  "Path to the bd executable."
  :type 'string
  :group 'beads)

(defcustom beads-database-path nil
  "Path to the beads database.
If nil, bd will auto-discover the database."
  :type '(choice (const :tag "Auto-discover" nil)
                 (file :tag "Database path"))
  :group 'beads)

(defcustom beads-actor nil
  "Actor name for audit trail.
If nil, uses $USER environment variable."
  :type '(choice (const :tag "Use $USER" nil)
                 (string :tag "Actor name"))
  :group 'beads)

(defcustom beads-enable-debug nil
  "Enable debug logging to *beads-debug* buffer."
  :type 'boolean
  :group 'beads)

(defcustom beads-debug-level 'info
  "Debug logging level.
- `error': Only log errors
- `info': Log commands and important events (default)
- `verbose': Log everything including command output"
  :type '(choice (const :tag "Error only" error)
                 (const :tag "Info (commands and events)" info)
                 (const :tag "Verbose (all output)" verbose))
  :group 'beads)

(defcustom beads-auto-refresh t
  "Automatically refresh buffers after mutations."
  :type 'boolean
  :group 'beads)

;;; Variables

(defvar beads--project-cache (make-hash-table :test 'equal)
  "Cache of project roots to .beads directories.")

;;; Utilities

(defun beads--log (level format-string &rest args)
  "Log message to *beads-debug* buffer if debug is enabled.
LEVEL is one of `error', `info', or `verbose'.
FORMAT-STRING and ARGS are passed to `format'."
  (when beads-enable-debug
    ;; Check if this message should be logged based on level
    (when (or (eq level 'error)
              (and (eq beads-debug-level 'info)
                   (memq level '(error info)))
              (eq beads-debug-level 'verbose))
      (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
             (level-str (upcase (symbol-name level)))
             (msg (apply #'format format-string args))
             (log-line (format "[%s] [%s] %s\n" timestamp level-str msg)))
        ;; Log to buffer
        (with-current-buffer (get-buffer-create "*beads-debug*")
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert log-line))
          ;; Auto-scroll if buffer is visible
          (when (get-buffer-window (current-buffer))
            (goto-char (point-max))
            (recenter -1)))))))

(defun beads--error (format-string &rest args)
  "Display error message to user.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((msg (apply #'format format-string args)))
    (apply #'beads--log 'error "ERROR: %s" (list msg))
    (user-error "Beads: %s" msg)))

;;;###autoload
(defun beads-show-debug-buffer ()
  "Show the *beads-debug* buffer in another window.
Enables debug logging if not already enabled."
  (interactive)
  (unless beads-enable-debug
    (setq beads-enable-debug t)
    (message "Debug logging enabled"))
  (let ((buf (get-buffer-create "*beads-debug*")))
    (with-current-buffer buf
      (unless (eq major-mode 'beads-debug-mode)
        (beads-debug-mode)))
    (display-buffer buf)))

;;;###autoload
(defun beads-clear-debug-buffer ()
  "Clear the *beads-debug* buffer."
  (interactive)
  (when-let ((buf (get-buffer "*beads-debug*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message "Debug buffer cleared")))

(defvar beads-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'beads-show-debug-buffer)
    (define-key map (kbd "c") #'beads-clear-debug-buffer)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `beads-debug-mode'.")

(define-derived-mode beads-debug-mode special-mode "Beads-Debug"
  "Major mode for viewing Beads debug logs.

\\{beads-debug-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil))

;;;###autoload
(defun beads-toggle-debug ()
  "Toggle debug logging on/off."
  (interactive)
  (setq beads-enable-debug (not beads-enable-debug))
  (message "Beads debug logging %s"
           (if beads-enable-debug "enabled" "disabled"))
  (when beads-enable-debug
    (beads-show-debug-buffer)))

;;; Project Integration

(defun beads--find-project-root ()
  "Find the project root directory.
Returns nil if not in a project."
  (when-let* ((proj (project-current)))
    (if (fboundp 'project-root)
        (project-root proj)
      ;; Emacs 27 compatibility - project-roots is obsolete but needed for old Emacs
      (with-no-warnings
        (car (project-roots proj))))))

(defun beads--find-beads-dir (&optional directory)
  "Find .beads directory starting from DIRECTORY.
If DIRECTORY is nil, uses current buffer's directory or project root.
Returns the path to .beads directory or nil if not found."
  (let* ((start-dir (or directory
                        (beads--find-project-root)
                        default-directory))
         (cached (gethash start-dir beads--project-cache)))
    (if cached
        cached
      (let ((beads-dir (locate-dominating-file start-dir ".beads")))
        (when beads-dir
          (let ((full-path (expand-file-name ".beads" beads-dir)))
            (puthash start-dir full-path beads--project-cache)
            full-path))))))

(defun beads--get-database-path ()
  "Get the database path for bd commands.
Returns nil if auto-discovery should be used."
  (or beads-database-path
      (when-let* ((beads-dir (beads--find-beads-dir)))
        (car (directory-files beads-dir t "\\.db\\'")))))

;;; Process Execution

(defun beads--build-command (subcommand &rest args)
  "Build bd command with SUBCOMMAND and ARGS.
Automatically adds global flags based on customization."
  (let ((cmd (list beads-executable)))
    ;; Add global flags
    (when beads-actor
      (setq cmd (append cmd (list "--actor" beads-actor))))
    (when-let* ((db (beads--get-database-path)))
      ;; Strip Tramp prefix for remote paths so bd can understand the path
      (setq cmd (append cmd (list "--db" (file-local-name db)))))
    ;; Add subcommand and args
    (append cmd (list subcommand) args (list "--json"))))

(defun beads--run-command (subcommand &rest args)
  "Run bd SUBCOMMAND with ARGS synchronously.
Returns parsed JSON output or signals error.
Works over Tramp when `default-directory' is a remote path."
  (let* ((cmd (apply #'beads--build-command subcommand args))
         (cmd-string (mapconcat #'shell-quote-argument cmd " ")))
    (beads--log 'info "Running: %s" cmd-string)
    (beads--log 'verbose "In directory: %s" default-directory)
    (with-temp-buffer
      (let* ((exit-code (apply #'process-file
                               (car cmd) nil t nil (cdr cmd)))
             (output (buffer-string)))
        (beads--log 'verbose "Exit code: %d" exit-code)
        (beads--log 'verbose "Output: %s" output)
        (if (zerop exit-code)
            (condition-case err
                (json-read-from-string output)
              (error
               (beads--error "Failed to parse JSON: %s" (error-message-string err))))
          (beads--error "Command failed (exit %d): %s" exit-code output))))))

(defun beads--run-command-async (callback subcommand &rest args)
  "Run bd SUBCOMMAND with ARGS asynchronously.
Call CALLBACK with parsed JSON output when complete.
Works over Tramp when `default-directory' is a remote path."
  (let* ((cmd (apply #'beads--build-command subcommand args))
         (cmd-string (mapconcat #'shell-quote-argument cmd " "))
         (buffer (generate-new-buffer " *beads-async*")))
    (beads--log 'info "Running async: %s" cmd-string)
    (beads--log 'verbose "In directory: %s" default-directory)
    (let ((proc (apply #'start-file-process
                       "beads-async" buffer
                       (car cmd) (cdr cmd))))
      (set-process-sentinel
       proc
       (lambda (process _event)
         (when (eq (process-status process) 'exit)
           (let ((exit-code (process-exit-status process)))
             (with-current-buffer (process-buffer process)
               (let ((output (buffer-string)))
                 (beads--log 'verbose "Async exit code: %d" exit-code)
                 (beads--log 'verbose "Async output: %s" output)
                 (if (zerop exit-code)
                     (condition-case err
                         (funcall callback (json-read-from-string output))
                       (error
                        (beads--error "Failed to parse JSON: %s"
                                      (error-message-string err))))
                   (beads--error "Async command failed (exit %d): %s"
                                 exit-code output))))
             (kill-buffer (process-buffer process))))))
      proc)))

;;; Completion Support

(defvar beads--completion-cache nil
  "Cache for issue list used in completion.
Format: (TIMESTAMP . ISSUES-LIST)")

(defvar beads--completion-cache-ttl 5
  "Time-to-live for completion cache in seconds.")

(defun beads--get-cached-issues ()
  "Get cached issue list, refreshing if stale.
Returns list of issues or nil on error."
  (let ((now (float-time)))
    (when (or (null beads--completion-cache)
              (> (- now (car beads--completion-cache))
                 beads--completion-cache-ttl))
      (condition-case nil
          (setq beads--completion-cache
                (cons now (beads--parse-issues (beads--run-command "list"))))
        (error
         (setq beads--completion-cache nil))))
    (cdr beads--completion-cache)))

(defun beads--invalidate-completion-cache ()
  "Invalidate the completion cache.
Call this after creating, updating, or deleting issues."
  (setq beads--completion-cache nil))

(defun beads--issue-completion-table ()
  "Return completion table for issue IDs with annotations."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . beads-issue)
          (annotation-function . beads--annotate-issue)
          (group-function . beads--group-issue))
      (let ((issues (beads--get-cached-issues)))
        (complete-with-action
         action
         (mapcar (lambda (i) (alist-get 'id i)) issues)
         string pred)))))

(defun beads--annotate-issue (issue-id)
  "Annotate ISSUE-ID with status and title for completion."
  (condition-case nil
      (let* ((issues (beads--get-cached-issues))
             (issue (seq-find (lambda (i)
                               (string= (alist-get 'id i) issue-id))
                             issues)))
        (when issue
          (let ((status (alist-get 'status issue))
                (title (alist-get 'title issue))
                (priority (alist-get 'priority issue)))
            (format " %s [P%s] %s"
                    (propertize (upcase status)
                              'face (pcase status
                                     ("open" 'success)
                                     ("in_progress" 'warning)
                                     ("blocked" 'error)
                                     ("closed" 'shadow)
                                     (_ 'default)))
                    priority
                    (if (> (length title) 40)
                        (concat (substring title 0 37) "...")
                      title)))))
    (error "")))

(defun beads--group-issue (issue-id transform)
  "Group ISSUE-ID by status for completion.
If TRANSFORM is non-nil, return the transformed issue ID."
  (if transform
      issue-id
    (condition-case nil
        (let* ((issues (beads--get-cached-issues))
               (issue (seq-find (lambda (i)
                                 (string= (alist-get 'id i) issue-id))
                               issues))
               (status (when issue (alist-get 'status issue))))
          (pcase status
            ("open" "Open")
            ("in_progress" "In Progress")
            ("blocked" "Blocked")
            ("closed" "Closed")
            (_ "Other")))
      (error "Other"))))

(defvar beads--issue-id-history nil
  "History list for issue ID completion.")

(defvar beads--dependency-type-history nil
  "History list for dependency type completion.")

;;; JSON Parsing

(defun beads--parse-issue (json)
  "Parse issue from JSON object.
Returns an alist with issue fields."
  (let ((issue (if (vectorp json) (aref json 0) json)))
    `((id . ,(alist-get 'id issue))
      (title . ,(alist-get 'title issue))
      (description . ,(alist-get 'description issue))
      (status . ,(alist-get 'status issue))
      (priority . ,(alist-get 'priority issue))
      (issue-type . ,(alist-get 'issue_type issue))
      (created-at . ,(alist-get 'created_at issue))
      (updated-at . ,(alist-get 'updated_at issue))
      (closed-at . ,(alist-get 'closed_at issue))
      (acceptance-criteria . ,(alist-get 'acceptance_criteria issue))
      (design . ,(alist-get 'design issue))
      (notes . ,(alist-get 'notes issue))
      (assignee . ,(alist-get 'assignee issue))
      (external-ref . ,(alist-get 'external_ref issue)))))

(defun beads--parse-issues (json)
  "Parse list of issues from JSON array.
Returns a list of issue alists."
  (when (and json (vectorp json))
    (mapcar #'beads--parse-issue (append json nil))))

;;; Public API

;;;###autoload
(defun beads-check-executable ()
  "Check if bd executable is available.
Returns t if found, signals error otherwise."
  (interactive)
  (if (executable-find beads-executable)
      (progn
        (when (called-interactively-p 'interactive)
          (message "Found bd executable: %s" beads-executable))
        t)
    (beads--error "Cannot find bd executable: %s" beads-executable)))

;;;###autoload
(autoload 'beads "beads-main" "Open the main Beads transient menu." t)

;;;###autoload
(autoload 'beads-list "beads-list" "Display all Beads issues in a tabulated list." t)

;;;###autoload
(autoload 'beads-ready "beads-list" "Display ready Beads issues in a tabulated list." t)

;;;###autoload
(autoload 'beads-blocked "beads-list" "Display blocked Beads issues in a tabulated list." t)

;;;###autoload
(autoload 'beads-show "beads-show" "Show details for a Beads issue." t)

;;;###autoload
(autoload 'beads-create "beads-create" "Create a new Beads issue using transient menu." t)

;;;###autoload
(autoload 'beads-close "beads-close" "Close a Beads issue with optional reason." t)

;;;###autoload
(autoload 'beads-delete "beads-delete" "Delete a Beads issue permanently." t)

;;;###autoload
(autoload 'beads-stats "beads-stats" "Display Beads issue statistics." t)

;;;###autoload
(autoload 'beads-dep "beads-dep" "Manage dependencies in Beads." t)

;;;###autoload
(autoload 'beads-dep-add "beads-dep" "Add a dependency to an issue." t)

;;;###autoload
(autoload 'beads-dep-remove "beads-dep" "Remove a dependency from an issue." t)

;;;###autoload
(autoload 'beads-dep-tree "beads-dep" "Display dependency tree for an issue." t)

;;;###autoload
(autoload 'beads-dep-cycles "beads-dep" "Check for dependency cycles." t)

;;;###autoload
(autoload 'beads-quickstart "beads-misc" "Show Beads quickstart guide." t)

;;;###autoload
(autoload 'beads-graph-all "beads-graph" "Show dependency graph for all issues." t)

;;;###autoload
(autoload 'beads-graph-issue "beads-graph" "Show dependency graph focused on issue." t)

;;; Footer

(provide 'beads)
;;; beads.el ends here
