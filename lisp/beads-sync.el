;;; beads-sync.el --- Transient menu for bd sync command -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

;;; Commentary:

;; Provides transient menu interface for the Beads sync command.
;;
;; The bd sync command synchronizes issues with git remote in a single
;; operation:
;; 1. Export pending changes to JSONL
;; 2. Commit changes to git
;; 3. Pull from remote (with conflict resolution)
;; 4. Import updated JSONL
;; 5. Push local commits to remote
;;
;; This module provides a transient menu with flags for:
;; - --dry-run: Preview without making changes
;; - -m/--message: Custom commit message
;; - --no-pull: Skip pulling from remote
;; - --no-push: Skip pushing to remote
;;
;; The command displays sync progress and results, handles conflicts,
;; and refreshes all beads buffers after successful sync.

;;; Code:

(require 'beads)
(require 'transient)

;;; Forward declarations
(declare-function beads-list-refresh "beads-list")
(declare-function beads-refresh-show "beads-show")

;;; ============================================================
;;; bd sync
;;; ============================================================

;;; Transient State Variables

(defvar beads-sync--message nil
  "Custom commit message for sync.")

(defvar beads-sync--dry-run nil
  "Whether to run in dry-run mode (preview only).")

(defvar beads-sync--no-pull nil
  "Whether to skip pulling from remote.")

(defvar beads-sync--no-push nil
  "Whether to skip pushing to remote.")

;;; Utility Functions

(defun beads-sync--reset-state ()
  "Reset sync transient state."
  (setq beads-sync--message nil
        beads-sync--dry-run nil
        beads-sync--no-pull nil
        beads-sync--no-push nil))

(defun beads-sync--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-sync--format-flag (flag-set)
  "Format FLAG-SET for display in transient menu."
  (if flag-set
      (propertize " [enabled]" 'face 'transient-value)
    (propertize " [disabled]" 'face 'transient-inactive-value)))

;;; Infix Commands

(transient-define-infix beads-sync--infix-message ()
  "Set the commit message for sync."
  :class 'transient-option
  :description (lambda ()
                 (concat "Message (-m/--message)"
                         (beads-sync--format-value
                          beads-sync--message)))
  :key "m"
  :argument "message="
  :prompt "Commit message: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((msg (read-string "Commit message: "
                                    beads-sync--message)))
              (setq beads-sync--message msg)
              msg)))

(transient-define-infix beads-sync--infix-dry-run ()
  "Toggle dry-run flag."
  :class 'transient-switch
  :description "Dry run (--dry-run)"
  :key "d"
  :argument "--dry-run"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-sync--dry-run
                  (not beads-sync--dry-run))
            beads-sync--dry-run))

(transient-define-infix beads-sync--infix-no-pull ()
  "Toggle no-pull flag."
  :class 'transient-switch
  :description "Skip pull (--no-pull)"
  :key "p"
  :argument "--no-pull"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-sync--no-pull
                  (not beads-sync--no-pull))
            beads-sync--no-pull))

(transient-define-infix beads-sync--infix-no-push ()
  "Toggle no-push flag."
  :class 'transient-switch
  :description "Skip push (--no-push)"
  :key "P"
  :argument "--no-push"
  :reader (lambda (_prompt _initial-input _history)
            (setq beads-sync--no-push
                  (not beads-sync--no-push))
            beads-sync--no-push))

;;; Suffix Commands

(defun beads-sync--execute (message dry-run no-pull no-push)
  "Execute bd sync with given parameters.
MESSAGE is the custom commit message.
DRY-RUN previews without making changes.
NO-PULL skips pulling from remote.
NO-PUSH skips pushing to remote."
  (condition-case err
      (let ((args (list "sync")))
        ;; Add flags
        (when dry-run
          (setq args (append args (list "--dry-run"))))
        (when no-pull
          (setq args (append args (list "--no-pull"))))
        (when no-push
          (setq args (append args (list "--no-push"))))
        (when (and message (not (string-empty-p (string-trim message))))
          (setq args (append args (list "-m" message))))

        ;; Execute sync command
        (let ((output (with-temp-buffer
                        (let ((exit-code
                               (apply #'call-process
                                      beads-executable nil t nil args)))
                          (unless (zerop exit-code)
                            (error "Sync failed: %s" (buffer-string)))
                          (buffer-string)))))

          ;; Display output in dedicated buffer
          (let ((buf (get-buffer-create "*beads-sync*")))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "=== Beads Sync Results ===\n\n")
                (insert output)
                (goto-char (point-min))
                (special-mode)
                (local-set-key (kbd "q") 'quit-window)
                (local-set-key (kbd "g")
                               (lambda ()
                                 (interactive)
                                 (beads-sync--execute message dry-run
                                                      no-pull no-push)))
                (visual-line-mode 1)))
            (display-buffer buf))

          ;; Refresh all beads buffers if not dry-run
          (unless dry-run
            (when beads-auto-refresh
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (cond
                   ((and (fboundp 'beads-list-refresh)
                         (derived-mode-p 'beads-list-mode))
                    (beads-list-refresh))
                   ((and (fboundp 'beads-refresh-show)
                         (derived-mode-p 'beads-show-mode))
                    (beads-refresh-show)))))))

          ;; Show appropriate message
          (if dry-run
              (message "Sync dry-run completed (see *beads-sync* buffer)")
            (message "Sync completed successfully"))
          nil))
    (error
     (beads--error "Failed to sync: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-sync--execute-command ()
  "Execute the bd sync command."
  :key "s"
  :description "Sync with remote"
  (interactive)
  (beads-sync--execute beads-sync--message
                       beads-sync--dry-run
                       beads-sync--no-pull
                       beads-sync--no-push)
  (unless beads-sync--dry-run
    (beads-sync--reset-state)))

(transient-define-suffix beads-sync--preview ()
  "Preview the sync command without executing."
  :key "v"
  :description "Preview command"
  :transient t
  (interactive)
  (let ((args (list "bd sync")))
    (when beads-sync--dry-run
      (setq args (append args (list "--dry-run"))))
    (when beads-sync--no-pull
      (setq args (append args (list "--no-pull"))))
    (when beads-sync--no-push
      (setq args (append args (list "--no-push"))))
    (when (and beads-sync--message
               (not (string-empty-p (string-trim beads-sync--message))))
      (setq args (append args (list "-m" (format "\"%s\""
                                                 beads-sync--message)))))
    (message "Command: %s" (string-join args " "))))

(transient-define-suffix beads-sync--reset ()
  "Reset all sync parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-sync--reset-state)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-sync "beads-sync" nil t)
(transient-define-prefix beads-sync ()
  "Synchronize issues with git remote.

This transient menu provides an interface for the bd sync command,
which synchronizes issues with the git remote in a single operation:

1. Export pending changes to JSONL
2. Commit changes to git
3. Pull from remote (with conflict resolution)
4. Import updated JSONL
5. Push local commits to remote

Flags:
  --dry-run: Preview sync without making changes
  -m/--message: Custom commit message (auto-generated if not set)
  --no-pull: Skip pulling from remote
  --no-push: Skip pushing to remote

The sync results are displayed in a dedicated buffer, and all beads
buffers are refreshed after a successful sync."
  :value (lambda () nil)
  (interactive)
  (beads-check-executable)
  ["Sync Options"
   ["Message"
    (beads-sync--infix-message)]
   ["Flags"
    (beads-sync--infix-dry-run)
    (beads-sync--infix-no-pull)
    (beads-sync--infix-no-push)]]
  ["Actions"
   ("s" "Sync" beads-sync--execute-command)
   ("v" "Preview command" beads-sync--preview)
   ("R" "Reset fields" beads-sync--reset)
   ("q" "Quit" transient-quit-one)])

;;; Footer

(provide 'beads-sync)
;;; beads-sync.el ends here
