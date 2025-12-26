;;; beads.el --- Magit-like interface for Beads issue tracker -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.10.1") (sesman "0.3.2"))
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

(require 'beads-command)
(require 'beads-completion)
(require 'beads-custom)
(require 'json)
(require 'project)
(require 'transient)

;;; Variables

(defvar beads--project-cache (make-hash-table :test 'equal)
  "Cache of project roots to .beads directories.")

;; Forward declarations for global option variables (defined in beads-option.el)
(defvar beads-global-actor nil)
(defvar beads-global-db nil)
(defvar beads-global-json nil)
(defvar beads-global-no-auto-flush nil)
(defvar beads-global-no-auto-import nil)
(defvar beads-global-no-daemon nil)
(defvar beads-global-no-db nil)
(defvar beads-global-sandbox nil)

;;; Constants

(defconst beads-display-value-max-length 40
  "Maximum length for displaying values in transient menus.
Values longer than this will be truncated with \"...\" appended.")

(defconst beads-separator-line-width 40
  "Width of separator lines in error and debug buffers.")

(defconst beads-stats-separator-width 50
  "Width of separator lines in statistics display.")

(defconst beads-graph-label-max-length 30
  "Maximum length for issue titles in dependency graphs.
Longer titles will be truncated for graph display.")

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
             (log-line (format "[%s] [%s] %s\n" timestamp level-str msg))
             (buf (get-buffer-create "*beads-debug*")))
        ;; Log to buffer
        (with-current-buffer buf
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert log-line)))
        ;; Auto-scroll if buffer is visible in a window
        (when-let ((win (get-buffer-window buf)))
          (with-selected-window win
            (goto-char (point-max))
            (recenter -1)))))))

(defun beads--error (format-string &rest args)
  "Display error message to user.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((msg (apply #'format format-string args)))
    (apply #'beads--log 'error "ERROR: %s" (list msg))
    (user-error "Beads: %s" msg)))

(defun beads--display-error-buffer (command exit-code stdout stderr)
  "Display detailed error information in *beads-errors* buffer.
COMMAND is the command string that was executed.
EXIT-CODE is the process exit code.
STDOUT is the standard output from the process.
STDERR is the standard error output from the process."
  (let ((buf (get-buffer-create "*beads-errors*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (special-mode)
      (setq buffer-read-only nil)
      (insert (propertize "Beads Command Error\n"
                          'face '(:weight bold :height 1.2))
              (propertize (make-string 60 ?=) 'face 'shadow)
              "\n\n")

      ;; Timestamp
      (insert (propertize "Time: " 'face 'bold)
              (format-time-string "%Y-%m-%d %H:%M:%S")
              "\n\n")

      ;; Command
      (insert (propertize "Command:\n" 'face 'bold)
              (propertize command 'face 'font-lock-string-face)
              "\n\n")

      ;; Exit code
      (insert (propertize "Exit Code: " 'face 'bold)
              (propertize (format "%d" exit-code)
                          'face 'error)
              "\n\n")

      ;; Stdout
      (insert (propertize "Standard Output:\n" 'face 'bold)
              (propertize (make-string beads-separator-line-width ?-)
                          'face 'shadow)
              "\n")
      (if (and stdout (not (string-empty-p (string-trim stdout))))
          (insert stdout "\n")
        (insert (propertize "(empty)\n" 'face 'shadow)))
      (insert "\n")

      ;; Stderr
      (insert (propertize "Standard Error:\n" 'face 'bold)
              (propertize (make-string beads-separator-line-width ?-)
                          'face 'shadow)
              "\n")
      (if (and stderr (not (string-empty-p (string-trim stderr))))
          (insert stderr "\n")
        (insert (propertize "(empty)\n" 'face 'shadow)))

      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer buf)))

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

(defun beads--get-project-name ()
  "Return project name for current context.
Uses the basename of the project root directory.
Returns nil if not in a project."
  (when-let ((root (beads--find-project-root)))
    (file-name-nondirectory (directory-file-name root))))

(defun beads--get-git-branch ()
  "Return current git branch name, or nil if not in a git repo.
This is METADATA for display, not identity.  Works over Tramp."
  (let ((default-directory (or (beads--find-project-root) default-directory)))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil
                                 "rev-parse" "--abbrev-ref" "HEAD"))
        (let ((branch (string-trim (buffer-string))))
          (unless (string= branch "HEAD")  ; detached HEAD
            branch))))))

;;; Git Worktree Support

(defun beads--in-git-worktree-p ()
  "Return non-nil if current directory is in a git worktree.
In worktrees, .git is a file containing `gitdir: ...' instead of a directory.
Works from nested directories within the worktree."
  (when-let ((git-dir (locate-dominating-file default-directory ".git")))
    (let ((dot-git (expand-file-name ".git" git-dir)))
      (and (file-exists-p dot-git)
           (not (file-directory-p dot-git))))))

(defun beads--find-main-repo-from-worktree ()
  "Find the main git repository path when in a worktree.
Uses `git rev-parse --git-common-dir' which returns the shared .git directory.
Returns the main repository path, or nil if not in a worktree or on error."
  (when (beads--in-git-worktree-p)
    (let ((default-directory (or (beads--find-project-root) default-directory)))
      (with-temp-buffer
        (when (zerop (process-file "git" nil t nil
                                   "rev-parse" "--git-common-dir"))
          (let ((git-common-dir (string-trim (buffer-string))))
            (when (and git-common-dir
                       (not (string-empty-p git-common-dir))
                       (not (string-prefix-p "fatal:" git-common-dir)))
              ;; git-common-dir is the .git directory, we need its parent
              (file-name-directory
               (directory-file-name (expand-file-name git-common-dir))))))))))

(defun beads--find-beads-dir (&optional directory)
  "Find .beads directory starting from DIRECTORY.
If DIRECTORY is nil, uses `default-directory'.
Returns the path to .beads directory or nil if not found.

Search order:
1. Walk up from DIRECTORY/`default-directory' looking for .beads
   (ensures worktree-local .beads is found before main repo's)
2. If not found and in a git worktree, check the main repository"
  (let* ((start-dir (or directory default-directory))
         (cached (gethash start-dir beads--project-cache)))
    (if cached
        cached
      ;; Try local discovery first
      (let ((beads-dir (locate-dominating-file start-dir ".beads")))
        ;; If not found locally, check if we're in a worktree
        (unless beads-dir
          (when-let ((main-repo (beads--find-main-repo-from-worktree)))
            (let ((main-beads (expand-file-name ".beads" main-repo)))
              (when (file-directory-p main-beads)
                (setq beads-dir main-repo)))))
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
Automatically adds global flags based on customization and
global transient variables (beads-global-*).

Global transient variables (set via beads-option-global options)
take precedence over defcustom settings."
  ;; Use push/nreverse for O(n) performance instead of repeated append (O(n²))
  (let ((parts nil))
    ;; Build arguments by pushing in desired final order, then reverse at end
    ;; Final: (executable [--actor actor] [--db db] [flags...] subcommand args... --json)

    ;; Push executable first (will be first after nreverse)
    (push beads-executable parts)

    ;; Actor: beads-global-actor > beads-actor > $USER
    (when-let ((actor (or beads-global-actor beads-actor)))
      ;; Convert to string in case it's a symbol
      (let ((actor-str (if (stringp actor) actor (format "%s" actor))))
        (unless (string-empty-p (string-trim actor-str))
          (push "--actor" parts)
          (push actor-str parts))))

    ;; Database: beads-global-db > beads--get-database-path
    (when-let ((db (or beads-global-db (beads--get-database-path))))
      ;; Convert to string in case it's a symbol
      (let ((db-str (if (stringp db) db (format "%s" db))))
        (unless (string-empty-p (string-trim db-str))
          ;; Strip Tramp prefix for remote paths so bd can understand the path
          (push "--db" parts)
          (push (file-local-name db-str) parts))))

    ;; Boolean global flags (only if set via transient)
    (when beads-global-no-auto-flush
      (push "--no-auto-flush" parts))
    (when beads-global-no-auto-import
      (push "--no-auto-import" parts))
    (when beads-global-no-daemon
      (push "--no-daemon" parts))
    (when beads-global-no-db
      (push "--no-db" parts))
    (when beads-global-sandbox
      (push "--sandbox" parts))

    ;; Add subcommand
    (push subcommand parts)

    ;; Add command-specific args
    (dolist (arg args)
      (push arg parts))

    ;; --json flag goes at the end
    (push "--json" parts)

    ;; Reverse to get correct order
    (nreverse parts)))

;;; Completion Support (aliases for backward compatibility)

;; The completion implementation is in beads-completion.el.
;; These aliases maintain backward compatibility with existing code.

(defalias 'beads--issue-completion-table #'beads-completion-issue-table
  "Return completion table for issue IDs with title-aware matching.")

(defalias 'beads--invalidate-completion-cache #'beads-completion-invalidate-cache
  "Invalidate the completion cache.")

(defalias 'beads--get-cached-issues #'beads-completion--get-cached-issues
  "Get cached issue list, refreshing if stale.")

(defvar beads--issue-id-history nil
  "History list for issue ID completion.")

(defvar beads--dependency-type-history nil
  "History list for dependency type completion.")

;;; JSON Parsing

(defun beads--parse-issue (json)
  "Parse issue from JSON object.
Returns a beads-issue EIEIO instance."
  (let ((issue (if (vectorp json) (aref json 0) json)))
    (beads-issue-from-json issue)))

(defun beads--parse-issues (json)
  "Parse list of issues from JSON array.
Returns a list of beads-issue EIEIO instances."
  (when (and json (vectorp json))
    (mapcar #'beads-issue-from-json (append json nil))))

;;; Info/Debug Command

;;;###autoload
(defun beads-info ()
  "Display information about beads configuration in current context.
Shows worktree status, database path, and --no-daemon settings.
Useful for debugging configuration issues."
  (interactive)
  (let* ((in-worktree (beads--in-git-worktree-p))
         (main-repo (when in-worktree (beads--find-main-repo-from-worktree)))
         (beads-dir (beads--find-beads-dir))
         (db-path (beads--get-database-path)))
    (message "Beads Info:
  In worktree: %s
  Main repo: %s
  .beads dir: %s
  Database: %s
  --no-daemon: %s%s"
             (if in-worktree "yes" "no")
             (or main-repo "N/A")
             (or beads-dir "NOT FOUND")
             (or db-path "NOT FOUND")
             (if beads-global-no-daemon "enabled" "disabled")
             (if beads-global-no-daemon " (via transient)" ""))))

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
(autoload 'beads-update "beads-update" "Update a Beads issue using transient menu." t)

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
(autoload 'beads-quickstart "beads-quickstart" "Show Beads quickstart guide." t)

;;;###autoload
(autoload 'beads-import "beads-import" "Import issues from JSONL." t)

;;;###autoload
(autoload 'beads-export "beads-export" "Export issues to JSONL format." t)

;;;###autoload
(autoload 'beads-sync "beads-sync" "Synchronize issues with git remote." t)

;;;###autoload
(autoload 'beads-graph-all "beads-graph" "Show dependency graph for all issues." t)

;;;###autoload
(autoload 'beads-graph-issue "beads-graph" "Show dependency graph focused on issue." t)

;;;###autoload
(autoload 'beads-label "beads-label" "Manage labels for issues." t)

;;;###autoload
(autoload 'beads-label-add "beads-label" "Add a label to one or more issues." t)

;;;###autoload
(autoload 'beads-label-remove "beads-label" "Remove a label from one or more issues." t)

;;;###autoload
(autoload 'beads-label-list-interactive "beads-label" "List labels for an issue." t)

;;;###autoload
(autoload 'beads-label-list-all-view "beads-label" "Display all labels in a tabulated list buffer." t)

;;;###autoload
(autoload 'beads-label-list-all "beads-label" "Return a list of all labels from bd label list-all.")

;;;###autoload
(autoload 'beads--get-cached-labels "beads-label" "Get labels from cache or fetch if needed.")

;;;###autoload
(autoload 'beads--invalidate-label-cache "beads-label" "Invalidate the label cache.")

;;;###autoload
(autoload 'beads--label-completion-table "beads-label" "Return completion table for labels.")

;;;###autoload
(autoload 'beads-eldoc-mode "beads-eldoc"
  "Global minor mode to enable eldoc support for beads issue references." t)

;;; Footer

(provide 'beads)
;;; beads.el ends here
