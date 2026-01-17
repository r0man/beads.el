;;; beads-main.el --- Main transient menu for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides the main entry point and root transient menu
;; for beads.el.  The `beads' command opens a Magit-like menu interface
;; that provides access to all beads functionality organized into
;; logical groups.
;;
;; Usage:
;;   M-x beads RET    ; Open main transient menu
;;
;; The menu is organized into groups:
;; - View: Commands for viewing issues (list, ready, blocked, show, stats)
;; - Create/Edit: Commands for creating/modifying (create, update, close)
;; - Dependencies: Dependency management (dep submenu)
;; - Admin: Project administration (init, export, import)
;;
;; The menu header displays:
;; - Current project name and root directory
;; - Database path being used
;; - Beads version from bd CLI

;;; Code:

(require 'beads)
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-command-create)
(require 'beads-command-update)
(require 'beads-command-close)
(require 'beads-command-admin)
(require 'beads-command-agent)
(require 'beads-command-blocked)
(require 'beads-command-graph)
(require 'beads-command-ready)
(require 'beads-command-sync)
(require 'beads-command-worktree)
(require 'beads-command-reopen)
(require 'beads-command-delete)
(require 'beads-command-stats)
(require 'beads-command-dep)
(require 'beads-command-init)
(require 'beads-command-export)
(require 'beads-command-import)
(require 'beads-command-quickstart)
(require 'beads-command-epic)
(require 'beads-agent)
(require 'beads-command-worktree)
(require 'beads-command-info)
(require 'beads-command-formula)
(require 'beads-command-activity)
(require 'beads-command-audit)
(require 'beads-command-comments)
(require 'beads-command-config)
(require 'beads-command-count)
(require 'beads-command-daemon)
(require 'beads-command-defer)
(require 'beads-command-doctor)
(require 'beads-command-edit)
(require 'beads-command-gate)
(require 'beads-command-hooks)
(require 'beads-command-integrations)
(require 'beads-command-label)
(require 'beads-command-merge-slot)
(require 'beads-command-migrate)
(require 'beads-command-misc)
(require 'beads-command-mol)
(require 'beads-command-search)
(require 'beads-command-slot)
(require 'beads-command-stale)
(require 'beads-command-state)
(require 'beads-command-status)
(require 'beads-command-swarm)
(require 'transient)

;;; Variables

(defvar beads-main--cached-version nil
  "Cached version string from bd CLI.")

(defvar beads-main--cached-project-info nil
  "Cached project info (root and db path).")

;;; Utility Functions

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
                      'face 'font-lock-keyword-face)))
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

;;; Main Transient Menu

;;;###autoload (autoload 'beads "beads-main" nil t)
(transient-define-prefix beads ()
  "Main transient menu for Beads issue tracker.

This is the primary entry point for beads.el, providing a Magit-like
interface for all issue tracking operations.  The menu is organized
into logical groups matching bd CLI structure."
  [:description
   (lambda () (beads-main--format-project-header))
   :class transient-row
   ("" "" ignore :if (lambda () nil))]
  ;; Row 1: Issues | Workflow | Setup
  [["Working With Issues"
    ("l" "List issues" beads-list)
    ("c" "Create issue" beads-create)
    ("Q" "Quick capture (q)" beads-q)
    ("u" "Update issue" beads-update)
    ("x" "Close issue" beads-close)
    ("o" "Reopen issue" beads-reopen)
    ("D" "Delete issue" beads-delete)
    ("s" "Show issue" beads-show)
    ("e" "Edit field" beads-edit)
    ("O" "Move issue" beads-move)
    ("B" "Refile issue" beads-refile)]
   ["Workflow & Collaboration"
    ("F" "Formula menu" beads-formula-menu)
    ("K" "Cook formula" beads-cook)
    ("m" "Molecule menu" beads-mol)
    ("g" "Gate menu" beads-gate)
    ("M" "Merge slot" beads-merge-slot)
    ("f" "Defer issue" beads-defer)
    ("U" "Undefer issue" beads-undefer)
    ("H" "Ship capability" beads-ship)]
   ["Setup & Config"
    ("i" "Init project" beads-init)
    ("?" "Quickstart" beads-quickstart)
    ("." "Config menu" beads-config)
    ("h" "Hooks menu" beads-hooks)
    ("!" "Info/Debug" beads-info)
    ("5" "Where (location)" beads-where)
    ("6" "Human commands" beads-human)
    ("7" "Onboard snippet" beads-onboard)
    ("8" "Prime context" beads-prime)
    ("9" "Setup integrations" beads-setup)]]
  ;; Row 2: Views | Agent | Maintenance
  [["Views & Reports"
    ("r" "Ready work" beads-ready)
    ("b" "Blocked issues" beads-blocked)
    ("t" "Stats/Status" beads-stats)
    ("a" "Activity feed" beads-activity)
    ("C" "Count issues" beads-count)
    ("S" "Stale issues" beads-stale)
    ("/" "Search" beads-search)
    ("T" "Lint issues" beads-lint)
    ("Y" "Orphans" beads-orphans)]
   ["Agent & Slots"
    ("A" "Agent menu" beads-agent-menu)
    ("@" "Slot menu" beads-slot)
    ("=" "Comments" beads-comments-menu)
    ("~" "Audit log" beads-audit)]
   ["Maintenance"
    ("+" "Doctor" beads-doctor)
    ("^" "Migrate menu" beads-migrate-menu)
    ("W" "Worktree menu" beads-worktree-menu)
    ("&" "Admin menu" beads-admin)
    ("0" "Preflight check" beads-preflight)
    ("-" "Upgrade bd" beads-upgrade)
    ("P" "Rename prefix" beads-rename-prefix)
    (":" "Repair database" beads-repair)
    (";" "Resolve conflicts" beads-resolve-conflicts)]]
  ;; Row 3: Dependencies | Sync | Integrations
  [["Dependencies & Structure"
    ("d" "Dependencies" beads-dep)
    ("v" "Graph (visual)" beads-graph-all)
    ("E" "Epic status" beads-epic)
    ("w" "Swarm menu" beads-swarm)
    ("1" "Mark duplicate" beads-duplicate)
    ("2" "Find duplicates" beads-duplicates)
    ("3" "Supersede issue" beads-supersede)]
   ["Sync & Data"
    ("y" "Sync with remote" beads-sync)
    ("X" "Export to JSONL" beads-export)
    ("I" "Import from JSONL" beads-import)
    ("n" "Daemon menu" beads-daemon)
    ("4" "Restore from git" beads-restore)]
   ["Integrations"
    ("j" "Jira" beads-jira)
    ("N" "Linear" beads-linear)
    ("R" "Repo" beads-repo)
    ("*" "Mail delegate" beads-mail)]]
  ;; Row 4: Labels | Actions
  [["Labels & State"
    ("L" "Label menu" beads-label)
    ("z" "Set state" beads-set-state)
    ("Z" "State menu" beads-state-menu)]
   ["Actions"
    ("G" "Refresh menu" beads-refresh-menu)
    ("V" "Version" beads-version)
    ("q" "Quit" transient-quit-one)]])

;;; Footer

(provide 'beads-main)
;;; beads-main.el ends here
