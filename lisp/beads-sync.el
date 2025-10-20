;;; beads-sync.el --- Transient menu for bd sync command -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides a transient menu interface for the bd sync command.
;; The sync command synchronizes issues with git remote in a single
;; operation:
;; 1. Export pending changes to JSONL
;; 2. Commit changes to git
;; 3. Pull from remote (with conflict resolution)
;; 4. Import updated JSONL
;; 5. Push local commits to remote
;;
;; Usage:
;;   M-x beads-sync RET
;;
;; The menu allows setting:
;; - --dry-run: Preview without making changes
;; - -m/--message: Custom commit message
;; - --no-pull: Skip pulling from remote
;; - --no-push: Skip pushing to remote
;;
;; After execution, displays sync progress and results (commits
;; pulled/pushed) in a compilation buffer.

;;; Code:

(require 'beads)
(require 'transient)
(require 'compile)

;;; Forward declarations
(declare-function beads-list-refresh "beads-list")
(declare-function beads-refresh-show "beads-show")

;;; Transient State Variables

(defvar beads-sync--dry-run nil
  "Whether to run in dry-run mode (preview without changes).")

(defvar beads-sync--message nil
  "Custom commit message for sync operation.")

(defvar beads-sync--no-pull nil
  "Whether to skip pulling from remote.")

(defvar beads-sync--no-push nil
  "Whether to skip pushing to remote.")

;;; Utility Functions

(defun beads-sync--reset-state ()
  "Reset all sync transient state variables."
  (setq beads-sync--dry-run nil
        beads-sync--message nil
        beads-sync--no-pull nil
        beads-sync--no-push nil))

(defun beads-sync--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-sync--refresh-all-buffers ()
  "Refresh all beads buffers after sync."
  ;; Refresh all beads-list buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'beads-list-mode)
        (ignore-errors
          (beads-list-refresh)))))
  ;; Refresh all beads-show buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (boundp 'beads-show-mode)
                 beads-show-mode)
        (ignore-errors
          (beads-refresh-show))))))

;;; Infix Commands

(transient-define-infix beads-sync--infix-dry-run ()
  "Toggle dry-run mode."
  :class 'transient-switch
  :description "Dry run (--dry-run)"
  :key "d"
  :argument "--dry-run")

(transient-define-infix beads-sync--infix-message ()
  "Set custom commit message."
  :class 'transient-option
  :description "Message (-m)"
  :key "m"
  :argument "-m="
  :prompt "Commit message: "
  :reader (lambda (_prompt _initial-input _history)
            (read-string "Commit message: ")))

(transient-define-infix beads-sync--infix-no-pull ()
  "Toggle skip pull flag."
  :class 'transient-switch
  :description "Skip pull (--no-pull)"
  :key "P"
  :argument "--no-pull")

(transient-define-infix beads-sync--infix-no-push ()
  "Toggle skip push flag."
  :class 'transient-switch
  :description "Skip push (--no-push)"
  :key "p"
  :argument "--no-push")

;;; Suffix Commands

(defun beads-sync--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:dry-run BOOL :message STRING :no-pull BOOL :no-push BOOL)."
  (let ((dry-run nil)
        (message nil)
        (no-pull nil)
        (no-push nil))
    (while args
      (let ((arg (pop args)))
        (cond
         ((string= arg "--dry-run")
          (setq dry-run t))
         ((string= arg "--no-pull")
          (setq no-pull t))
         ((string= arg "--no-push")
          (setq no-push t))
         ((string-prefix-p "-m=" arg)
          (setq message (substring arg 3))))))
    (list :dry-run dry-run
          :message message
          :no-pull no-pull
          :no-push no-push)))

(defun beads-sync--build-command-line (dry-run message no-pull no-push)
  "Build command line for bd sync.
DRY-RUN: preview without making changes
MESSAGE: custom commit message
NO-PULL: skip pulling from remote
NO-PUSH: skip pushing to remote"
  (let ((cmd (list beads-executable "sync")))
    ;; Add flags
    (when dry-run
      (setq cmd (append cmd (list "--dry-run"))))
    (when (and message (not (string-empty-p (string-trim message))))
      (setq cmd (append cmd (list "-m" message))))
    (when no-pull
      (setq cmd (append cmd (list "--no-pull"))))
    (when no-push
      (setq cmd (append cmd (list "--no-push"))))
    ;; Return as shell command string
    (mapconcat #'shell-quote-argument cmd " ")))

(defvar beads-sync--dry-run-active nil
  "Track if current sync is a dry-run.")

(defun beads-sync--compilation-finish (buffer status)
  "Finish function for beads sync compilation.
BUFFER is the compilation buffer.
STATUS is the exit status string."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n")
      (if (string-match "^finished" status)
          (progn
            (insert (propertize "Sync completed successfully!\n"
                               'face 'success))
            ;; Refresh buffers if not dry-run
            (unless beads-sync--dry-run-active
              (beads-sync--refresh-all-buffers)
              (message "Sync completed - buffers refreshed")))
        (insert (propertize (format "Sync failed: %s" status)
                           'face 'error))))))

(defun beads-sync--execute (dry-run message no-pull no-push)
  "Execute bd sync command in a compilation buffer.
DRY-RUN: preview without making changes
MESSAGE: custom commit message
NO-PULL: skip pulling from remote
NO-PUSH: skip pushing to remote"
  (let* ((command (beads-sync--build-command-line dry-run message
                                                   no-pull no-push))
         (buffer-name (if dry-run "*beads-sync (dry-run)*" "*beads-sync*")))
    ;; Store dry-run state for finish function
    (setq beads-sync--dry-run-active dry-run)

    ;; Kill existing buffer if it exists
    (when-let ((old-buf (get-buffer buffer-name)))
      (kill-buffer old-buf))

    ;; Start compilation
    (let ((compilation-buffer-name-function
           (lambda (_mode) buffer-name))
          (compilation-finish-functions
           '(beads-sync--compilation-finish)))
      (compilation-start command nil))

    (message "Running: %s" command)
    nil))

(transient-define-suffix beads-sync--execute-command ()
  "Execute the bd sync command."
  :key "s"
  :description "Sync with remote"
  (interactive)
  (beads-check-executable)
  (let* ((args (transient-args 'beads-sync))
         (parsed (beads-sync--parse-transient-args args))
         (dry-run (plist-get parsed :dry-run))
         (message (plist-get parsed :message))
         (no-pull (plist-get parsed :no-pull))
         (no-push (plist-get parsed :no-push)))
    ;; Update state variables for potential subsequent calls
    (setq beads-sync--dry-run dry-run
          beads-sync--message message
          beads-sync--no-pull no-pull
          beads-sync--no-push no-push)
    (beads-sync--execute dry-run message no-pull no-push)
    (unless dry-run
      (beads-sync--reset-state))))

(transient-define-suffix beads-sync--reset ()
  "Reset all sync parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-sync--reset-state)
    ;; Clear transient's argument state
    (transient-set)
    (message "Fields reset")))

(transient-define-suffix beads-sync--preview ()
  "Preview the bd sync command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((transient-args (transient-args 'beads-sync))
         (parsed (beads-sync--parse-transient-args transient-args))
         (dry-run (plist-get parsed :dry-run))
         (message (plist-get parsed :message))
         (no-pull (plist-get parsed :no-pull))
         (no-push (plist-get parsed :no-push))
         (args nil))
    ;; Build args list (without the subcommand)
    (when dry-run
      (setq args (append args (list "--dry-run"))))
    (when (and message (not (string-empty-p (string-trim message))))
      (setq args (append args (list "-m" message))))
    (when no-pull
      (setq args (append args (list "--no-pull"))))
    (when no-push
      (setq args (append args (list "--no-push"))))

    (let* ((cmd (apply #'beads--build-command "sync" args))
           (cmd-string (mapconcat #'shell-quote-argument cmd " ")))
      (message "Command: %s" cmd-string))))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-sync "beads-sync" nil t)
(transient-define-prefix beads-sync ()
  "Synchronize issues with git remote.

This transient menu provides an interface for the bd sync command,
which performs a complete git-based sync workflow:
1. Export pending changes to JSONL
2. Commit changes to git
3. Pull from remote (with conflict resolution)
4. Import updated JSONL
5. Push local commits to remote

Use --dry-run to preview changes without making them."
  ["Sync Options"
   (beads-sync--infix-dry-run)
   (beads-sync--infix-message)
   (beads-sync--infix-no-pull)
   (beads-sync--infix-no-push)]
  ["Actions"
   ("s" "Sync" beads-sync--execute-command)
   ("P" "Preview command" beads-sync--preview)
   ("R" "Reset fields" beads-sync--reset)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-sync)
;;; beads-sync.el ends here
