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
(require 'beads-update)
(require 'beads-close)
(require 'beads-stats)
(require 'beads-dep)
(require 'beads-misc)
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

;;; beads-init command

(defvar beads-init--prefix nil
  "Custom prefix for bd init command.")

(defun beads-init--reset-state ()
  "Reset init state."
  (setq beads-init--prefix nil))

(transient-define-infix beads-init--infix-prefix ()
  "Set custom issue prefix."
  :class 'transient-option
  :description "Issue prefix (--prefix)"
  :key "p"
  :argument "prefix="
  :prompt "Issue prefix: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((prefix (read-string "Issue prefix (default: dir name): "
                                      beads-init--prefix)))
              (setq beads-init--prefix prefix)
              prefix)))

(transient-define-suffix beads-init--execute ()
  "Execute bd init command."
  :key "i"
  :description "Initialize project"
  (interactive)
  (condition-case err
      (let* ((args (when (and beads-init--prefix
                             (not (string-empty-p
                                   (string-trim beads-init--prefix))))
                    (list "--prefix" beads-init--prefix)))
             (result (apply #'beads--run-command "init" args)))
        (beads-init--reset-state)
        (beads-main--clear-cache)
        (message "Beads project initialized successfully")
        ;; Refresh menu to show new project info
        (beads))
    (error
     (message "Failed to initialize: %s" (error-message-string err)))))

(transient-define-prefix beads-init ()
  "Initialize a new Beads project in the current directory."
  ["Options"
   (beads-init--infix-prefix)]
  ["Actions"
   ("i" "Initialize" beads-init--execute)
   ("q" "Quit" transient-quit-one)])

;;; beads-export command

(defvar beads-export--output nil
  "Output file for bd export command.")

(defvar beads-export--status nil
  "Status filter for bd export command.")

(defun beads-export--reset-state ()
  "Reset export state."
  (setq beads-export--output nil
        beads-export--status nil))

(transient-define-infix beads-export--infix-output ()
  "Set output file."
  :class 'transient-option
  :description "Output file (-o)"
  :key "o"
  :argument "output="
  :prompt "Output file: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((file (read-file-name "Output file: "
                                       nil nil nil
                                       beads-export--output)))
              (setq beads-export--output file)
              file)))

(transient-define-infix beads-export--infix-status ()
  "Set status filter."
  :class 'transient-option
  :description "Status filter (-s)"
  :key "s"
  :argument "status="
  :prompt "Status: "
  :choices '("open" "in_progress" "blocked" "closed")
  :reader (lambda (_prompt _initial-input _history)
            (let ((status (completing-read
                          "Status filter: "
                          '("open" "in_progress" "blocked" "closed")
                          nil t beads-export--status)))
              (setq beads-export--status status)
              status)))

(transient-define-suffix beads-export--execute ()
  "Execute bd export command."
  :key "e"
  :description "Export to JSONL"
  (interactive)
  (condition-case err
      (let ((args nil))
        ;; Add output file
        (when (and beads-export--output
                  (not (string-empty-p (string-trim beads-export--output))))
          (setq args (append args (list "-o" beads-export--output))))
        ;; Add status filter
        (when (and beads-export--status
                  (not (string-empty-p (string-trim beads-export--status))))
          (setq args (append args (list "-s" beads-export--status))))
        ;; Run command
        (apply #'beads--run-command "export" args)
        (if beads-export--output
            (message "Exported issues to %s" beads-export--output)
          (message "Exported issues to stdout"))
        (beads-export--reset-state))
    (error
     (message "Failed to export: %s" (error-message-string err)))))

(transient-define-prefix beads-export ()
  "Export Beads issues to JSONL format."
  ["Options"
   (beads-export--infix-output)
   (beads-export--infix-status)]
  ["Actions"
   ("e" "Export" beads-export--execute)
   ("q" "Quit" transient-quit-one)])

;;; beads-import command

(defvar beads-import--input nil
  "Input file for bd import command.")

(defvar beads-import--dry-run nil
  "Dry-run flag for bd import command.")

(defvar beads-import--resolve-collisions nil
  "Resolve-collisions flag for bd import command.")

(defvar beads-import--skip-existing nil
  "Skip-existing flag for bd import command.")

(defvar beads-import--strict nil
  "Strict flag for bd import command.")

(defun beads-import--reset-state ()
  "Reset import state."
  (setq beads-import--input nil
        beads-import--dry-run nil
        beads-import--resolve-collisions nil
        beads-import--skip-existing nil
        beads-import--strict nil))

(transient-define-infix beads-import--infix-input ()
  "Set input file."
  :class 'transient-option
  :description "Input file (-i)"
  :key "i"
  :argument "input="
  :prompt "Input file: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((file (read-file-name "Input file: "
                                       nil nil t
                                       beads-import--input)))
              (setq beads-import--input file)
              file)))

(transient-define-infix beads-import--infix-dry-run ()
  "Toggle dry-run mode."
  :class 'transient-switch
  :description "Dry run (--dry-run)"
  :key "d"
  :argument "--dry-run"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-import--dry-run (not beads-import--dry-run))
            (if beads-import--dry-run "--dry-run" nil)))

(transient-define-infix beads-import--infix-resolve-collisions ()
  "Toggle resolve-collisions mode."
  :class 'transient-switch
  :description "Resolve collisions (--resolve-collisions)"
  :key "r"
  :argument "--resolve-collisions"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-import--resolve-collisions
                  (not beads-import--resolve-collisions))
            (if beads-import--resolve-collisions
                "--resolve-collisions" nil)))

(transient-define-infix beads-import--infix-skip-existing ()
  "Toggle skip-existing mode."
  :class 'transient-switch
  :description "Skip existing (-s)"
  :key "s"
  :argument "-s"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-import--skip-existing
                  (not beads-import--skip-existing))
            (if beads-import--skip-existing "-s" nil)))

(transient-define-infix beads-import--infix-strict ()
  "Toggle strict mode."
  :class 'transient-switch
  :description "Strict mode (--strict)"
  :key "S"
  :argument "--strict"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-import--strict (not beads-import--strict))
            (if beads-import--strict "--strict" nil)))

(transient-define-suffix beads-import--execute ()
  "Execute bd import command."
  :key "I"
  :description "Import from JSONL"
  (interactive)
  (condition-case err
      (let ((args nil))
        ;; Add input file
        (when (and beads-import--input
                  (not (string-empty-p (string-trim beads-import--input))))
          (setq args (append args (list "-i" beads-import--input))))
        ;; Add flags
        (when beads-import--dry-run
          (setq args (append args (list "--dry-run"))))
        (when beads-import--resolve-collisions
          (setq args (append args (list "--resolve-collisions"))))
        (when beads-import--skip-existing
          (setq args (append args (list "-s"))))
        (when beads-import--strict
          (setq args (append args (list "--strict"))))
        ;; Run command
        (apply #'beads--run-command "import" args)
        (beads--invalidate-completion-cache)
        (beads-main--clear-cache)
        (if beads-import--input
            (message "Imported issues from %s" beads-import--input)
          (message "Imported issues from stdin"))
        (beads-import--reset-state))
    (error
     (message "Failed to import: %s" (error-message-string err)))))

(transient-define-prefix beads-import ()
  "Import Beads issues from JSONL format."
  ["Options"
   (beads-import--infix-input)]
  ["Flags"
   (beads-import--infix-dry-run)
   (beads-import--infix-resolve-collisions)
   (beads-import--infix-skip-existing)
   (beads-import--infix-strict)]
  ["Actions"
   ("I" "Import" beads-import--execute)
   ("q" "Quit" transient-quit-one)])

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
   ("d" "Dependencies menu" beads-dep)
   ("v" "Visual graph" beads-graph-all)]
  ["Admin"
   :description "Project administration"
   ("i" "Init project" beads-init)
   ("e" "Export to JSONL" beads-export)
   ("I" "Import from JSONL" beads-import)]
  ["Other"
   :description "Other commands"
   ("?" "Quickstart guide" beads-quickstart)
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
