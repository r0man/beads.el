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
(require 'beads-option)
(require 'transient)
(require 'compile)

;;; Forward declarations
(declare-function beads-list-refresh "beads-list")
(declare-function beads-refresh-show "beads-show")

;;; Utility Functions

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
      (when (eq major-mode 'beads-show-mode)
        (ignore-errors
          (beads-refresh-show))))))

;;; Suffix Commands

(defun beads-sync--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:dry-run BOOL :message STRING :no-pull BOOL :no-push BOOL)."
  ;; Note: transient 0.12.0 can return `t' instead of "" for empty option values.
  ;; We use beads--sanitize-string to convert non-string values to nil.
  (let* ((dry-run (transient-arg-value "--dry-run" args))
         (message (beads--sanitize-string (transient-arg-value "--message=" args)))
         (no-pull (transient-arg-value "--no-pull" args))
         (no-push (transient-arg-value "--no-push" args)))
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
  ;; Use push/nreverse for O(n) performance instead of repeated append (O(n²))
  (let ((parts nil))
    ;; Build arguments in reverse order (push prepends to list)
    (when no-push
      (push "--no-push" parts))
    (when no-pull
      (push "--no-pull" parts))
    (when message
      (let ((trimmed (string-trim message)))
        (unless (string-empty-p trimmed)
          (push trimmed parts)
          (push "-m" parts))))
    (when dry-run
      (push "--dry-run" parts))
    (push "sync" parts)
    (push beads-executable parts)
    ;; Reverse to get correct order and return as shell command string
    (mapconcat #'shell-quote-argument (nreverse parts) " ")))

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
  :key "x"
  :description "Sync with remote"
  (interactive)
  (beads-check-executable)
  (let* ((args (transient-args 'beads-sync))
         (parsed (beads-sync--parse-transient-args args))
         (dry-run (plist-get parsed :dry-run))
         (message (plist-get parsed :message))
         (no-pull (plist-get parsed :no-pull))
         (no-push (plist-get parsed :no-push)))
    (beads-sync--execute dry-run message no-pull no-push)))

(transient-define-suffix beads-sync--reset ()
  "Reset all sync parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    ;; Clear transient's argument state using transient-reset
    (transient-reset)
    ;; Refresh the transient display to show cleared state
    (transient--redisplay)
    (message "All fields reset")))

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
         args)
    ;; Build args list in reverse order for push/nreverse
    (when no-push
      (push "--no-push" args))
    (when no-pull
      (push "--no-pull" args))
    (when message
      (let ((trimmed (string-trim message)))
        (unless (string-empty-p trimmed)
          (push trimmed args)
          (push "-m" args))))
    (when dry-run
      (push "--dry-run" args))
    (setq args (nreverse args))

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
   (beads-option-sync-dry-run)
   (beads-option-sync-message)
   (beads-option-sync-no-pull)
   (beads-option-sync-no-push)]
  ["Actions"
   (beads-sync--execute-command)
   (beads-sync--preview)
   (beads-sync--reset)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-sync)
;;; beads-sync.el ends here
