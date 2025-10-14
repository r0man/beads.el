;;; beads-main.el --- Main transient menu for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

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
(require 'beads-list)
(require 'beads-show)
(require 'beads-create)
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
        (error "unknown"))))

(defun beads-main--get-project-info ()
  "Get current project root and database path.
Returns cons cell (PROJECT-ROOT . DB-PATH) or nil if not in project."
  (or beads-main--cached-project-info
      (let ((root (beads--find-project-root))
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

;;; Placeholder Commands

(defun beads-main--not-implemented (feature)
  "Show not-implemented message for FEATURE."
  (message "Feature '%s' not yet implemented" feature))

(transient-define-suffix beads-update ()
  "Update an existing issue (placeholder)."
  :description "Update issue"
  (interactive)
  (beads-main--not-implemented "update"))

(transient-define-suffix beads-close ()
  "Close an issue (placeholder)."
  :description "Close issue"
  (interactive)
  (beads-main--not-implemented "close"))

(transient-define-suffix beads-stats ()
  "Show project statistics (placeholder)."
  :description "Show statistics"
  (interactive)
  (beads-main--not-implemented "stats"))

(transient-define-suffix beads-dep ()
  "Manage dependencies (placeholder submenu)."
  :description "Manage dependencies"
  (interactive)
  (beads-main--not-implemented "dep"))

(transient-define-suffix beads-init ()
  "Initialize beads in current directory (placeholder)."
  :description "Initialize beads project"
  (interactive)
  (beads-main--not-implemented "init"))

(transient-define-suffix beads-export ()
  "Export issues to JSONL (placeholder)."
  :description "Export to JSONL"
  (interactive)
  (beads-main--not-implemented "export"))

(transient-define-suffix beads-import ()
  "Import issues from JSONL (placeholder)."
  :description "Import from JSONL"
  (interactive)
  (beads-main--not-implemented "import"))

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
into logical groups for easy navigation.

Key bindings:
  View issues:        l (list), r (ready), b (blocked), s (show)
  Create/Edit:        c (create), u (update), x (close)
  Dependencies:       d (dep submenu)
  Admin:              i (init), e (export), I (import)
  Other:              g (refresh), q (quit)"
  [:description
   (lambda () (beads-main--format-project-header))
   :pad-keys t]
  ["View"
   :description "View issues"
   ("l" "List all issues" beads-list)
   ("r" "Ready work" beads-ready)
   ("b" "Blocked issues" beads-blocked)
   ("s" "Show issue" beads-show)
   ("t" "Stats" beads-stats)]
  ["Create/Edit"
   :description "Create and modify issues"
   ("c" "Create issue" beads-create)
   ("u" "Update issue" beads-update)
   ("x" "Close issue" beads-close)]
  ["Dependencies"
   :description "Manage dependencies"
   ("d" "Dependencies menu" beads-dep)]
  ["Admin"
   :description "Project administration"
   ("i" "Init project" beads-init)
   ("e" "Export to JSONL" beads-export)
   ("I" "Import from JSONL" beads-import)]
  ["Other"
   :description "Other commands"
   ("g" "Refresh menu" beads-refresh-menu)
   ("q" "Quit" transient-quit-one)])

;;; Integration with beads-list

;; Update beads-list-show to use beads-show when available
(defun beads-list-show ()
  "Show details for the issue at point."
  (interactive)
  (if-let ((id (beads-list--current-issue-id)))
      (if (fboundp 'beads-show)
          (beads-show id)
        (message "Show issue %s" id))
    (user-error "No issue at point")))

;;; Footer

(provide 'beads-main)
;;; beads-main.el ends here
