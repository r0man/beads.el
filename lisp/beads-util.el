;;; beads-util.el --- Low-level utilities for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; Low-level utility functions used throughout beads.el modules.
;; This module exists so that ~30 modules can require just the
;; utilities without pulling in the full beads.el entry point,
;; breaking the circular dependency where beads.el requires
;; beads-command which needs beads utilities.

;;; Code:

(require 'beads-custom)
(require 'beads-git)

;; Forward declarations for optional dependencies
(declare-function beads-from-json "beads-types")
(declare-function beads-completion-invalidate-cache "beads-completion")
(declare-function beads-completion-issue-table "beads-completion")
(declare-function beads-completion--get-cached-issues "beads-completion")

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

(defun beads--resolve-beads-dir (beads-dir)
  "Resolve BEADS-DIR, following a redirect file if present.
If BEADS-DIR contains a `redirect' file, reads its content as a
path relative to the project root (parent of BEADS-DIR) and
returns the resolved target directory.  If the target does not
exist or there is no redirect file, returns BEADS-DIR unchanged."
  (let ((redirect-file (expand-file-name "redirect" beads-dir)))
    (if (file-readable-p redirect-file)
        (let* ((project-root
                (file-name-directory (directory-file-name beads-dir)))
               (target (string-trim
                        (with-temp-buffer
                          (insert-file-contents redirect-file)
                          (buffer-string))))
               (resolved (expand-file-name target project-root)))
          (if (file-directory-p resolved)
              resolved
            beads-dir))
      beads-dir)))

(defun beads--get-database-path ()
  "Get the database path for bd commands.
Returns nil if auto-discovery should be used.
Follows .beads/redirect files to find the actual database
directory, then looks for a SQLite .db file or a Dolt database
subdirectory."
  (or beads-database-path
      (when-let* ((beads-dir (beads--find-beads-dir)))
        (let* ((resolved-dir (beads--resolve-beads-dir beads-dir)))
          (or
           ;; Legacy: SQLite .db file
           (car (directory-files resolved-dir t "\\.db\\'"))
           ;; Modern: Dolt database directory
           (let ((dolt-dir (expand-file-name "dolt" resolved-dir)))
             (when (file-directory-p dolt-dir)
               dolt-dir)))))))

;;; Process Execution

(defun beads--build-command (subcommand &rest args)
  "Build bd command with SUBCOMMAND and ARGS.
Automatically adds global flags based on customization and
global transient variables (beads-global-*).

Global transient variables (set via beads-option-global options)
take precedence over defcustom settings."
  ;; Use push/nreverse for O(n) performance instead of repeated append (O(n^2))
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

;;; Public API

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

;;; Completion Support (aliases for backward compatibility)

;; The completion implementation is in beads-completion.el.
;; These aliases maintain backward compatibility with existing code.

(defalias 'beads--issue-completion-table #'beads-completion-issue-table
  "Return completion table for issue IDs with title-aware matching.")

(defalias 'beads--invalidate-completion-cache #'beads-completion-invalidate-cache
  "Invalidate the completion cache.")

(defalias 'beads--get-cached-issues #'beads-completion--get-cached-issues
  "Get cached issue list, refreshing if stale.")

;;; History Variables

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
    (beads-from-json 'beads-issue issue)))

(defun beads--parse-issues (json)
  "Parse list of issues from JSON array.
Returns a list of beads-issue EIEIO instances."
  (when (and json (vectorp json))
    (mapcar (lambda (j) (beads-from-json 'beads-issue j)) (append json nil))))

;;; Footer

(provide 'beads-util)
;;; beads-util.el ends here
