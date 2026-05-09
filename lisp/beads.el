;;; beads.el --- Magit-like interface for Beads issue tracker -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.10.1") (sesman "0.3.2") (vui "1.0.0"))
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
(require 'beads-util)
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
3. `beads-section-mode' — issue ID from vui text property at point
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
   ;; 3. vui-mode buffer — look for beads-issue-section at point
   (when (and (derived-mode-p 'vui-mode)
              (fboundp 'beads-section-issue-id-at-point))
     (beads-section-issue-id-at-point))
   ;; 4. Buffer name parsing (*beads-show[PROJECT]/ISSUE-ID*)
   (when-let ((parsed (beads-buffer-parse-show (buffer-name))))
     (plist-get parsed :issue-id))
   ;; 5. Text at point (button or regexp)
   (beads--issue-id-at-text-point)))

(defun beads-issue-at-point-or-read (prompt)
  "Return the beads issue ID at point, or prompt with PROMPT.
Try `beads-issue-at-point' first; fall back to `completing-read'
using `beads-completion-read-issue'."
  (or (beads-issue-at-point)
      (beads-completion-read-issue prompt nil t)))

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


;;; Main Menu Variables (formerly beads-main.el)

(defvar beads-main--cached-version nil
  "Cached version string from bd CLI.")

(defvar beads-main--cached-project-info nil
  "Cached project info.  Format: (DIRECTORY ROOT . DB-PATH) or nil.")

;;; Main Menu Utility Functions

(defun beads-main--get-version ()
  "Get beads version from bd CLI.
Returns cached version if available, otherwise queries bd."
  (or beads-main--cached-version
      (condition-case nil
          (let* ((output (with-temp-buffer
                           (process-file beads-executable nil t nil
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
Returns cons cell (PROJECT-ROOT . DB-PATH) or nil if not in project.
Cache is keyed by directory to avoid stale data when switching projects."
  (if (and beads-main--cached-project-info
           (equal default-directory
                  (car beads-main--cached-project-info)))
      (cdr beads-main--cached-project-info)
    (let ((root (beads-git-find-project-root))
          (db (beads--get-database-path)))
      (when root
        (let ((info (cons root db)))
          (setq beads-main--cached-project-info
                (cons default-directory info))
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
               (db-display
                (cond
                 ;; .db file: show just the filename
                 ((and db (file-regular-p db))
                  (file-name-nondirectory db))
                 ;; Dolt directory or any other path: show full path
                 (db db)
                 ;; Nothing found
                 (t "not found"))))
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

;;; Context Predicates

(defun beads--in-beads-buffer-p ()
  "Return non-nil when current buffer is a beads buffer.
Used as :if predicate for context-aware transient groups."
  (derived-mode-p 'beads-list-mode
                  'beads-show-mode
                  'beads-section-mode
                  'beads-epic-status-mode
                  'beads-formula-list-mode
                  'beads-formula-show-mode))

;;; Menu Refresh

(transient-define-suffix beads-refresh-menu ()
  "Refresh the beads menu (clear cache and redisplay)."
  :description "Refresh menu"
  :transient t
  (interactive)
  (beads-main--clear-cache)
  (message "Menu refreshed"))

;;; Legacy More Commands Sub-dispatch (deprecated)
;; All commands are now in beads-ops-menu and beads-advanced-menu.
;; This is kept temporarily for backwards compatibility.

;;;###autoload
(transient-define-prefix beads-more-menu ()
  "Additional beads commands not in the main dispatch.

This menu contains less frequently used commands organized
into logical groups for easy access."
  ["More Issue Operations"
   ("o" "Reopen issue" beads-reopen)
   ("D" "Delete issue" beads-delete)
   ("e" "Edit field" beads-edit)
   ("I" "Create (transient)" beads-create)
   ("Q" "Quick capture (q)" beads-q)
("a" "Children" beads-children)
   (">" "Promote wisp" beads-promote)
   ("X" "Query issues" beads-query)
   ("[" "Todo items" beads-todo)
   ("nr" "Rename issue" beads-rename)]
  ["Workflow & Collaboration"
   ("K" "Cook formula" beads-cook)
   ("g" "Gate menu" beads-gate)
   ("f" "Defer issue" beads-defer)
   ("U" "Undefer issue" beads-undefer)
   ("H" "Ship capability" beads-ship)]
  ["Views & Reports"
   ("l" "List (advanced)" beads-list-advanced)
   ("t" "Stats/Status" beads-stats)
   ("C" "Count issues" beads-count)
   ("S" "Stale issues" beads-stale)
   ("/" "Search" beads-search)
   ("T" "Lint issues" beads-lint)
   ("Y" "Orphans" beads-orphans)
   ("]" "Issue types" beads-types)
   ("<" "Find duplicates (AI)" beads-find-duplicates)
   ("v" "Graph (visual)" beads-graph-all)
   ("E" "Epic menu" beads-epic-menu)]
  ["Agent & Audit"
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
   ("nc" "Admin compact menu" beads-compact)
   ("nC" "Compact Dolt commits" beads-compact-commits)
   ("nf" "Flatten Dolt" beads-flatten)
   ("ng" "Garbage collect" beads-gc)
   ("np" "Purge ephemeral" beads-purge)
   ("nP" "Prune closed beads" beads-prune)
   ("nB" "Batch ops (transactional)" beads-batch)
   ("ni" "Ping database" beads-ping)]
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
   ("G" "GitHub" beads-github)
   ("yl" "GitLab" beads-gitlab)
   ("R" "Repo" beads-repo)
   ("A" "ADO" beads-ado)
   ("*" "Mail delegate" beads-mail)]
  ["Setup & Config"
   ("ii" "Init project" beads-init)
   ("is" "Init safety" beads-init-safety)
   ("ib" "Bootstrap" beads-bootstrap)
   ("ic" "Context" beads-context)
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
    ("c" "Create" beads-compose-create)
    ("/" "Search" beads-search)
    ("i" "Show" beads-show)
    ("u" "Update" beads-update)
    ("x" "Close" beads-close)]
   ["Workflow"
    ("r" "Ready" beads-ready)
    ("b" "Blocked" beads-blocked)
    ("d" "Dependencies" beads-dep)
    ("e" "Edit" beads-edit)
    ("o" "Reopen" beads-reopen)]
   ["Views"
    ("s" "Dashboard" beads-dashboard)
    ("S" "Stats" beads-stats)
    ("v" "Graph" beads-graph-all)
    ("E" "Epic" beads-epic-menu)
    ("H" "History" beads-history)
    ("D" "Diff" beads-diff)]
   ["Manage"
    ("L" "Labels" beads-label-menu)
    ("F" "Formula" beads-formula-menu)
    ("m" "Molecule" beads-mol)
    ("k" "Dolt" beads-dolt)
    ("." "Config" beads-config)]]
  [["Context"
    :if beads--in-beads-buffer-p
    ("#" "Set priority" beads-actions-set-priority)
    ("C" "Claim" beads-actions-claim)
    ("?" "Actions..." beads-show-actions)]
   ["Actions"
    ("!" "Ops..." beads-ops-menu)
    (">" "Advanced..." beads-advanced-menu)
    ("g" "Refresh" beads-refresh-menu)
    ("q" "Quit" transient-quit-one)]])

;;; Info/Debug Command

;;;###autoload
(defun beads-emacs-info ()
  "Display Emacs-specific beads configuration information.
Shows worktree status and database path.
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
  Database: %s"
             (if in-worktree "yes" "no")
             (or main-repo "N/A")
             (or beads-dir "NOT FOUND")
             (or db-path "NOT FOUND"))))

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
(autoload 'beads-list-advanced "beads-command-list" nil t)
;;;###autoload
(autoload 'beads-ready "beads-command-list" nil t)
;;;###autoload
(autoload 'beads-blocked "beads-command-list" nil t)

;; beads-ops-menu
;;;###autoload
(autoload 'beads-ops-menu "beads-ops-menu" nil t)

;; beads-advanced-menu
;;;###autoload
(autoload 'beads-advanced-menu "beads-advanced-menu" nil t)

;; beads-compose
;;;###autoload
(autoload 'beads-compose-create "beads-compose" nil t)
;;;###autoload
(autoload 'beads-compose-edit "beads-compose" nil t)
;;;###autoload
(autoload 'beads-compose-comment "beads-compose" nil t)

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

;; beads-status (deprecated compat shim)
;;;###autoload
(autoload 'beads-status "beads-status" nil t)

;; beads-dashboard
;;;###autoload
(autoload 'beads-dashboard "beads-dashboard" nil t)

;; beads-command-show
;;;###autoload
(autoload 'beads-show "beads-command-show" nil t)
;;;###autoload
(autoload 'beads-show-actions "beads-command-show" nil t)

;; beads-actions
;;;###autoload
(autoload 'beads-actions-claim "beads-actions" nil t)
;;;###autoload
(autoload 'beads-actions-set-status "beads-actions" nil t)
;;;###autoload
(autoload 'beads-actions-set-priority "beads-actions" nil t)

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

;; beads-command-status
;;;###autoload
(autoload 'beads-stats "beads-command-status" nil t)

;; beads-command-count
;;;###autoload
(autoload 'beads-count "beads-command-count" nil t)

;; beads-command-stale
;;;###autoload
(autoload 'beads-stale "beads-command-stale" nil t)

;; beads-command-search
;;;###autoload
(autoload 'beads-search "beads-command-search" nil t)

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
;;;###autoload
(autoload 'beads-compact-commits "beads-command-compact" nil t)

;; beads-command-batch
;;;###autoload
(autoload 'beads-batch "beads-command-batch" nil t)

;; beads-command-prune
;;;###autoload
(autoload 'beads-prune "beads-command-prune" nil t)

;; beads-command-ping
;;;###autoload
(autoload 'beads-ping "beads-command-ping" nil t)

;; beads-command-init (init-safety)
;;;###autoload
(autoload 'beads-init-safety "beads-command-init" nil t)

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
;;;###autoload
(autoload 'beads-epic-menu "beads-command-epic" nil t)

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
;;;###autoload
(autoload 'beads-ado "beads-command-integrations" nil t)

;; beads-command-state
;;;###autoload
(autoload 'beads-set-state "beads-command-state" nil t)
;;;###autoload
(autoload 'beads-state-menu "beads-command-state" nil t)

;; beads-command-label
;;;###autoload
(autoload 'beads-label-menu "beads-command-label" nil t)

;; beads-command-misc — miscellaneous commands referenced from main menu
;;;###autoload
(autoload 'beads-create-form "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-q "beads-command-misc" nil t)

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
(autoload 'beads-import "beads-command-misc" nil t)
;;;###autoload
(autoload 'beads-note "beads-command-misc" nil t)
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
