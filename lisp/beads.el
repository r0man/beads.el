;;; beads.el --- Magit-like interface for Beads issue tracker -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.10.1") (sesman "0.3.2") (magit-section "3.0.0"))
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

(require 'beads-buffer)
(require 'beads-command)
(require 'beads-completion)
(require 'beads-custom)
(require 'beads-git)
(require 'json)
(require 'project)
(require 'transient)

;;; Forward Declarations (for context detection without hard deps)

;; beads-command-list.el provides this
(declare-function beads-list--current-issue-id "beads-command-list")
;; beads-section.el provides this
(declare-function beads-section-issue-id-at-point "beads-section")
;; beads-command-show.el defines this buffer-local var
(defvar beads-show--issue-id)

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
FORMAT-STRING and ARGS are passed to `format'.

The log format is compatible with `log-view-mode':
  TIMESTAMP [LEVEL] message"
  (when beads-enable-debug
    ;; Check if this message should be logged based on level
    (when (or (eq level 'error)
              (and (eq beads-debug-level 'info)
                   (memq level '(error info)))
              (eq beads-debug-level 'verbose))
      (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
             ;; Map verbose -> DEBUG for log-view-mode compatibility
             (level-str (if (eq level 'verbose) "DEBUG" (upcase (symbol-name level))))
             (msg (apply #'format format-string args))
             (log-line (format "%s [%-5s] %s\n" timestamp level-str msg))
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

(defun beads--string-blank-p (value)
  "Return non-nil if VALUE is blank.
A value is considered blank if it is nil, not a string, or an empty string.
This function safely handles non-string values without signaling an error,
which is useful when validating transient arguments that may return a
non-string truthy value instead of a string in some transient versions."
  (or (null value)
      (not (stringp value))
      (string-empty-p (string-trim value))))

(defun beads--sanitize-string (value)
  "Return VALUE if it is a non-blank string, otherwise nil.
This ensures that non-string values (like t) and empty strings are
converted to nil, which is useful for processing transient arguments."
  (when (and (stringp value)
             (not (string-empty-p (string-trim value))))
    value))

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
;;
;; These functions delegate to beads-git.el for git operations.
;; They remain here for the beads-specific caching logic.

(defun beads--find-project-root ()
  "Find the project root directory.
Returns nil if not in a project.
This is an alias for `beads-git-find-project-root'."
  (beads-git-find-project-root))

(defun beads--get-project-name ()
  "Return project name for current context.
This is an alias for `beads-git-get-project-name'."
  (beads-git-get-project-name))

(defun beads--get-git-branch ()
  "Return current git branch name, or nil if not in a git repo.
This is an alias for `beads-git-get-branch'."
  (beads-git-get-branch))

(defun beads--in-git-worktree-p ()
  "Return non-nil if current directory is in a git worktree.
This is an alias for `beads-git-in-worktree-p'."
  (beads-git-in-worktree-p))

(defun beads--find-main-repo-from-worktree ()
  "Find the main git repository path when in a worktree.
This is an alias for `beads-git-find-main-repo'."
  (beads-git-find-main-repo))

;;; Beads Directory Discovery

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
          (when-let ((main-repo (beads-git-find-main-repo)))
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

;;; Context Functions — Public API

(defun beads--issue-id-at-text-point ()
  "Return a beads issue ID at point from button or regexp, or nil.
Checks for a button with an `issue-id' property first, then
scans the current line for an issue ID pattern overlapping point."
  (let ((original-point (point))
        (case-fold-search nil))
    (or
     ;; Button with issue-id property
     (when-let ((button (button-at original-point)))
       (button-get button 'issue-id))
     ;; Issue ID pattern on current line, overlapping point
     (save-excursion
       (let ((line-start (line-beginning-position))
             (line-end   (line-end-position)))
         (goto-char line-start)
         (catch 'found
           (while (re-search-forward
                   (concat "\\b\\([a-zA-Z][a-zA-Z0-9._-]*"
                           "-[0-9a-fA-F]+\\(?:\\.[0-9]+\\)*\\)\\b")
                   line-end t)
             (when (and (>= original-point (match-beginning 1))
                        (<= original-point (match-end 1)))
               (throw 'found (match-string 1))))))))))

;;;###autoload
(defun beads-issue-at-point ()
  "Return the beads issue ID at point, or nil.

Checks contexts in order:
1. `beads-list-mode' — issue ID from current tabulated-list row
2. `beads-show-mode' — issue ID stored in the show buffer
3. `beads-section-mode' — issue ID from magit-section at point
4. Buffer name   — beads-show buffer naming convention
5. Text at point — button or issue ID regexp match"
  (or
   ;; 1. Tabulated-list buffer (beads-list-mode)
   (when (and (derived-mode-p 'beads-list-mode)
              (fboundp 'beads-list--current-issue-id))
     (beads-list--current-issue-id))
   ;; 2. Show buffer (beads-show-mode)
   (when (and (derived-mode-p 'beads-show-mode)
              (boundp 'beads-show--issue-id))
     beads-show--issue-id)
   ;; 3. Magit-section buffer — look for beads-issue-section at point
   (when (and (derived-mode-p 'magit-section-mode)
              (fboundp 'beads-section-issue-id-at-point))
     (beads-section-issue-id-at-point))
   ;; 4. Buffer name parsing (*beads-show[PROJECT]/ISSUE-ID*)
   (when-let ((parsed (beads-buffer-parse-show (buffer-name))))
     (plist-get parsed :issue-id))
   ;; 5. Text at point (button or regexp)
   (beads--issue-id-at-text-point)))

;;;###autoload
(defun beads-current-project-root ()
  "Return the beads project root directory for the current buffer, or nil.
The project root is the directory that contains the .beads directory.
Returns nil when not inside a beads project."
  (when-let ((beads-dir (beads--find-beads-dir)))
    (file-name-directory (directory-file-name beads-dir))))

;;;###autoload
(defun beads-current-database-path ()
  "Return the beads database path for the current project, or nil.
Locates the .beads/*.db file for the current project.
Returns nil when no database is found."
  (beads--get-database-path))

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

(defvar beads--worktree-name-history nil
  "History list for worktree name completion.")

(defvar beads--worktree-branch-history nil
  "History list for worktree branch completion.")

(defvar beads--worktree-existing-history nil
  "History list for existing worktree completion.")

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

;;; Main Menu Variables (formerly beads-main.el)

(defvar beads-main--cached-version nil
  "Cached version string from bd CLI.")

(defvar beads-main--cached-project-info nil
  "Cached project info (root and db path).")

;;; Main Menu Utility Functions

(defun beads-main--get-version ()
  "Get beads version from bd CLI.
Returns cached version if available, otherwise queries bd."
  (or beads-main--cached-version
      (condition-case nil
          (let* ((output (with-temp-buffer
                           (call-process beads-executable nil t nil
                                        "version")
                           (buffer-string)))
                 (version (if (string-match "bd version \\([^ \n]+\\)" output)
                             (match-string 1 output)
                           "unknown")))
            (setq beads-main--cached-version version)
            version)
        (error "Unknown version"))))

(defun beads-main--get-project-info ()
  "Get current project root and database path.
Returns cons cell (PROJECT-ROOT . DB-PATH) or nil if not in project."
  (or beads-main--cached-project-info
      (let ((root (beads-git-find-project-root))
            (db (beads--get-database-path)))
        (when root
          (let ((info (cons root db)))
            (setq beads-main--cached-project-info info)
            info)))))

(defun beads-main--clear-cache ()
  "Clear cached project and version information."
  (setq beads-main--cached-version nil
        beads-main--cached-project-info nil))

(defun beads-main--format-project-header ()
  "Format project header for transient menu.
Returns a propertized string showing project and database info."
  (let ((info (beads-main--get-project-info)))
    (if info
        (let* ((root (car info))
               (db (cdr info))
               (project-name (file-name-nondirectory
                              (directory-file-name root)))
               (db-display (if db
                              (file-name-nondirectory db)
                            "auto-discover")))
          (concat
           (propertize "Project: " 'face 'bold)
           (propertize project-name 'face 'font-lock-constant-face)
           (propertize " (" 'face 'shadow)
           (propertize root 'face 'shadow)
           (propertize ")" 'face 'shadow)
           "\n"
           (propertize "Database: " 'face 'bold)
           (propertize db-display 'face 'font-lock-string-face)
           "\n"
           (propertize "Version: " 'face 'bold)
           (propertize (beads-main--get-version)
                      'face 'font-lock-keyword-face)
           "\n"
           (propertize "↓ more commands below — scroll transient window"
                      'face 'shadow)))
      (propertize "No beads project found in current directory"
                 'face 'warning))))

;;; Menu Refresh

(transient-define-suffix beads-refresh-menu ()
  "Refresh the beads menu (clear cache and redisplay)."
  :description "Refresh menu"
  :transient t
  (interactive)
  (beads-main--clear-cache)
  (message "Menu refreshed"))

;;; More Commands Sub-dispatch

;;;###autoload
(transient-define-prefix beads-more-menu ()
  "Additional beads commands not in the main dispatch.

This menu contains less frequently used commands organized
into logical groups for easy access."
  ["More Issue Operations"
   ("o" "Reopen issue" beads-reopen)
   ("D" "Delete issue" beads-delete)
   ("e" "Edit field" beads-edit)
   ("I" "Create (form)" beads-create-form)
   ("Q" "Quick capture (q)" beads-q)
   ("O" "Move issue" beads-move)
   ("B" "Refile issue" beads-refile)
   ("a" "Children" beads-children)
   (">" "Promote wisp" beads-promote)
   ("X" "Query issues" beads-query)
   ("[" "Todo items" beads-todo)
   ("nr" "Rename issue" beads-rename)]
  ["Workflow & Collaboration"
   ("K" "Cook formula" beads-cook)
   ("g" "Gate menu" beads-gate)
   ("M" "Merge slot" beads-merge-slot)
   ("f" "Defer issue" beads-defer)
   ("U" "Undefer issue" beads-undefer)
   ("H" "Ship capability" beads-ship)]
  ["Views & Reports"
   ("t" "Stats/Status" beads-stats)
   ("C" "Count issues" beads-count)
   ("S" "Stale issues" beads-stale)
   ("/" "Search" beads-search)
   ("T" "Lint issues" beads-lint)
   ("Y" "Orphans" beads-orphans)
   ("]" "Issue types" beads-types)
   ("<" "Find duplicates (AI)" beads-find-duplicates)
   ("v" "Graph (visual)" beads-graph-all)
   ("E" "Epic status" beads-epic)]
  ["Agent, Slots & Audit"
   ("@" "Slot menu" beads-slot)
   ("=" "Comments" beads-comments-menu)
   ("~" "Audit log" beads-audit)
   ("w" "Swarm menu" beads-swarm)]
  ["Maintenance"
   ("+" "Doctor" beads-doctor)
   ("^" "Migrate menu" beads-migrate-menu)
   ("W" "Worktree menu" beads-worktree-menu)
   ("&" "Admin menu" beads-admin)
   ("0" "Preflight check" beads-preflight)
   ("-" "Upgrade bd" beads-upgrade)
   ("P" "Rename prefix" beads-rename-prefix)
   (":" "Repair database" beads-repair)
   (";" "Resolve conflicts" beads-resolve-conflicts)
   ("nc" "Compact menu" beads-compact)
   ("nf" "Flatten Dolt" beads-flatten)
   ("ng" "Garbage collect" beads-gc)
   ("np" "Purge ephemeral" beads-purge)]
  ["Dolt & Version Control"
   ("#" "VC menu" beads-vc)
   ("p" "Branch" beads-branch)
   ("`" "Diff" beads-diff)
   ("%" "History" beads-history)
   ("$" "Federation menu" beads-federation)]
  ["Structure & Data"
   ("1" "Mark duplicate" beads-duplicate)
   ("2" "Find duplicates" beads-duplicates)
   ("3" "Supersede issue" beads-supersede)
   ("4" "Restore issue" beads-restore)
   ("J" "SQL query" beads-sql)
   ("nb" "Backup database" beads-backup)
   ("ne" "Export to JSONL" beads-export)]
  ["Integrations"
   ("j" "Jira" beads-jira)
   ("N" "Linear" beads-linear)
   ("yl" "GitLab" beads-gitlab)
   ("R" "Repo" beads-repo)
   ("*" "Mail delegate" beads-mail)]
  ["Setup & Config"
   ("i" "Init project" beads-init)
   ("?" "Quickstart" beads-quickstart)
   ("h" "Hooks menu" beads-hooks)
   ("!" "Info/Debug" beads-info)
   ("5" "Where (location)" beads-where)
   ("6" "Human commands" beads-human)
   ("7" "Onboard snippet" beads-onboard)
   ("8" "Prime context" beads-prime)
   ("9" "Setup integrations" beads-setup)
   ("_" "Forget memory" beads-forget)
   ("\\" "KV store" beads-kv)
   ("|" "Memories" beads-memories)
   ("{" "Recall memory" beads-recall)
   ("}" "Remember" beads-remember)]
  ["State & Info"
   ("z" "Set state" beads-set-state)
   ("Z" "State menu" beads-state-menu)
   ("V" "Version" beads-version)]
  ["Actions"
   ("q" "Quit" transient-quit-one)])

;;; Main Transient Menu

;;;###autoload
(transient-define-prefix beads ()
  "Main transient menu for Beads issue tracker.

This is the primary entry point for beads.el, providing a Magit-like
interface for all issue tracking operations.  The menu is organized
into a compact hierarchical structure with sub-dispatches."
  [:description
   (lambda () (beads-main--format-project-header))
   :class transient-row
   ("" "" ignore :if (lambda () nil))]
  [["Issues"
    ("l" "List" beads-list)
    ("c" "Create" beads-create)
    ("s" "Status" beads-status)
    ("S" "Show" beads-show)
    ("u" "Update" beads-update)
    ("x" "Close" beads-close)]
   ["Workflow"
    ("r" "Ready" beads-ready)
    ("b" "Blocked" beads-blocked)
    ("d" "Dependencies" beads-dep)
    ("F" "Formula" beads-formula-menu)
    ("m" "Molecule" beads-mol)]
   ["Manage"
    ("L" "Labels" beads-label)
    ("A" "Agents" beads-agent-menu)
    ("." "Config" beads-config)
    ("k" "Dolt" beads-dolt)]
   ["Extensions"
    ("G" "Gas Town" gastown :if (lambda () (featurep 'gastown)))]
   ["Actions"
    ("!" "More..." beads-more-menu)
    ("g" "Refresh" beads-refresh-menu)
    ("q" "Quit" transient-quit-one)]])

;;; Info/Debug Command

;;;###autoload
(defun beads-emacs-info ()
  "Display Emacs-specific beads configuration information.
Shows worktree status, database path, and --no-daemon settings.
Useful for debugging Emacs configuration issues.

Note: Use `beads-info' (M-x beads-info) for bd CLI info command
which shows daemon status and database statistics."
  (interactive)
  (let* ((in-worktree (beads-git-in-worktree-p))
         (main-repo (when in-worktree (beads-git-find-main-repo)))
         (beads-dir (beads--find-beads-dir))
         (db-path (beads--get-database-path)))
    (message "Beads Emacs Info:
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
    (beads--error "Cannot find bd executable '%s'.
Install bd CLI from https://github.com/steveyegge/beads
or set `beads-executable' to the full path" beads-executable)))

;; Main menu command autoloads — ensures all commands referenced in the
;; `beads' transient menu are available when the menu renders, without
;; eagerly requiring every module.  When beads.el is loaded directly
;; (e.g. -l beads or (require 'beads)) these forms execute immediately.
;; When the package is installed via ELPA the generated loaddefs file takes
;; precedence, but these forms are harmless no-ops in that case.

;; beads-command-list
;;;###autoload
(autoload 'beads-list "beads-command-list" nil t)
;;;###autoload
(autoload 'beads-ready "beads-command-list" nil t)
;;;###autoload
(autoload 'beads-blocked "beads-command-list" nil t)

;; beads-command-create
;;;###autoload
(autoload 'beads-create "beads-command-create" nil t)

;; beads-command-update
;;;###autoload
(autoload 'beads-update "beads-command-update" nil t)

;; beads-command-close
;;;###autoload
(autoload 'beads-close "beads-command-close" nil t)

;; beads-command-reopen
;;;###autoload
(autoload 'beads-reopen "beads-command-reopen" nil t)

;; beads-command-delete
;;;###autoload
(autoload 'beads-delete "beads-command-delete" nil t)

;; beads-status
;;;###autoload
(autoload 'beads-status "beads-status" nil t)

;; beads-command-show
;;;###autoload
(autoload 'beads-show "beads-command-show" nil t)

;; beads-command-edit
;;;###autoload
(autoload 'beads-edit "beads-command-edit" nil t)

;; beads-command-dep
;;;###autoload
(autoload 'beads-dep "beads-command-dep" nil t)

;; beads-command-formula
;;;###autoload
(autoload 'beads-formula-menu "beads-command-formula" nil t)

;; beads-command-mol
;;;###autoload
(autoload 'beads-mol "beads-command-mol" nil t)

;; beads-command-gate
;;;###autoload
(autoload 'beads-gate "beads-command-gate" nil t)

;; beads-command-merge-slot
;;;###autoload
(autoload 'beads-merge-slot "beads-command-merge-slot" nil t)

;; beads-command-defer
;;;###autoload
(autoload 'beads-defer "beads-command-defer" nil t)
;;;###autoload
(autoload 'beads-undefer "beads-command-defer" nil t)

;; beads-command-init
;;;###autoload
(autoload 'beads-init "beads-command-init" nil t)

;; beads-command-quickstart
;;;###autoload
(autoload 'beads-quickstart "beads-command-quickstart" nil t)

;; beads-command-config
;;;###autoload
(autoload 'beads-config "beads-command-config" nil t)

;; beads-command-hooks
;;;###autoload
(autoload 'beads-hooks "beads-command-hooks" nil t)

;; beads-command-info
;;;###autoload
(autoload 'beads-info "beads-command-info" nil t)

;; beads-command-stats
;;;###autoload
(autoload 'beads-stats "beads-command-stats" nil t)

;; beads-command-count
;;;###autoload
(autoload 'beads-count "beads-command-count" nil t)

;; beads-command-stale
;;;###autoload
(autoload 'beads-stale "beads-command-stale" nil t)

;; beads-command-search
;;;###autoload
(autoload 'beads-search "beads-command-search" nil t)

;; beads-command-agent
;;;###autoload
(autoload 'beads-agent-menu "beads-command-agent" nil t)

;; beads-command-slot
;;;###autoload
(autoload 'beads-slot "beads-command-slot" nil t)

;; beads-command-comments
;;;###autoload
(autoload 'beads-comments-menu "beads-command-comments" nil t)

;; beads-command-audit
;;;###autoload
(autoload 'beads-audit "beads-command-audit" nil t)

;; beads-command-doctor
;;;###autoload
(autoload 'beads-doctor "beads-command-doctor" nil t)

;; beads-command-migrate
;;;###autoload
(autoload 'beads-migrate-menu "beads-command-migrate" nil t)

;; beads-command-worktree
;;;###autoload
(autoload 'beads-worktree-menu "beads-command-worktree" nil t)

;; beads-command-admin
;;;###autoload
(autoload 'beads-admin "beads-command-admin" nil t)

;; beads-command-compact
;;;###autoload
(autoload 'beads-compact "beads-command-compact" nil t)

;; beads-command-dolt
;;;###autoload
(autoload 'beads-dolt "beads-command-dolt" nil t)

;; beads-command-vc
;;;###autoload
(autoload 'beads-vc "beads-command-vc" nil t)

;; beads-command-branch
;;;###autoload
(autoload 'beads-branch "beads-command-branch" nil t)

;; beads-command-diff
;;;###autoload
(autoload 'beads-diff "beads-command-diff" nil t)

;; beads-command-history
;;;###autoload
(autoload 'beads-history "beads-command-history" nil t)

;; beads-command-federation
;;;###autoload
(autoload 'beads-federation "beads-command-federation" nil t)

;; beads-command-graph
;;;###autoload
(autoload 'beads-graph-all "beads-command-graph" nil t)

;; beads-command-epic
;;;###autoload
(autoload 'beads-epic "beads-command-epic" nil t)

;; beads-command-swarm
;;;###autoload
(autoload 'beads-swarm "beads-command-swarm" nil t)

;; beads-command-restore
;;;###autoload
(autoload 'beads-restore "beads-command-restore" nil t)

;; beads-command-sql
;;;###autoload
(autoload 'beads-sql "beads-command-sql" nil t)

;; beads-command-integrations
;;;###autoload
(autoload 'beads-jira "beads-command-integrations" nil t)
;;;###autoload
(autoload 'beads-linear "beads-command-integrations" nil t)
;;;###autoload
(autoload 'beads-gitlab "beads-command-integrations" nil t)
;;;###autoload
(autoload 'beads-repo "beads-command-integrations" nil t)

;; beads-command-state
;;;###autoload
(autoload 'beads-set-state "beads-command-state" nil t)
;;;###autoload
(autoload 'beads-state-menu "beads-command-state" nil t)

;; beads-command-label
;;;###autoload
(autoload 'beads-label "beads-command-label" nil t)

;; beads-command-misc — miscellaneous commands referenced from main menu
;;;###autoload
(autoload 'beads-create-form "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-q "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-move "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-refile "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-children "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-promote "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-query "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-todo "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-rename "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-cook "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-ship "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-where "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-human "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-onboard "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-prime "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-setup "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-forget "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-kv "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-memories "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-recall "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-remember "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-lint "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-orphans "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-types "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-find-duplicates "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-flatten "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-gc "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-mail "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-preflight "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-upgrade "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-rename-prefix "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-repair "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-resolve-conflicts "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-purge "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-duplicate "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-duplicates "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-supersede "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-backup "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-export "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-version "beads-command-misc" nil t)

;; Label API autoloads — these are used by other modules (e.g., readers,
;; completion) that depend on beads.el, so they must be available early.
;;;###autoload
(autoload 'beads-label-list-all "beads-command-label" "Return a list of all labels from bd label list-all.")

;;;###autoload
(autoload 'beads--get-cached-labels "beads-command-label" "Get labels from cache or fetch if needed.")

;;;###autoload
(autoload 'beads--invalidate-label-cache "beads-command-label" "Invalidate the label cache.")

;;;###autoload
(autoload 'beads--label-completion-table "beads-command-label" "Return completion table for labels.")

;;;###autoload
(autoload 'beads-eldoc-mode "beads-eldoc"
  "Global minor mode to enable eldoc support for beads issue references." t)

;;; Footer

(provide 'beads)
;;; beads.el ends here
