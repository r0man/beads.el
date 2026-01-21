;;; beads-command-sync.el --- Sync command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the EIEIO command class for `bd sync' operation.
;; Sync synchronizes issues with git remote through pull, merge, export,
;; commit, and push operations.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)
(require 'compile)

;; Forward declarations
(declare-function beads-list-refresh "beads-list")
(declare-function beads-refresh-show "beads-show")
(declare-function beads--build-command "beads")
(declare-function beads--sanitize-string "beads")
(declare-function beads-check-executable "beads")
(defvar beads-executable)

;;; ============================================================
;;; Command Class: beads-command-sync
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-sync (beads-command-json)
  ((dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview sync without making changes."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1)
   (no-pull
    :initarg :no-pull
    :type boolean
    :initform nil
    :documentation "Skip pulling from remote."
    :long-option "no-pull"
    :option-type :boolean
    :key "P"
    :transient "--no-pull"
    :class transient-switch
    :argument "--no-pull"
    :transient-group "Options"
    :level 1
    :order 2)
   (no-push
    :initarg :no-push
    :type boolean
    :initform nil
    :documentation "Skip pushing to remote."
    :long-option "no-push"
    :option-type :boolean
    :key "p"
    :transient "--no-push"
    :class transient-switch
    :argument "--no-push"
    :transient-group "Options"
    :level 1
    :order 3)
   (flush-only
    :initarg :flush-only
    :type boolean
    :initform nil
    :documentation "Only export pending changes to JSONL."
    :long-option "flush-only"
    :option-type :boolean
    :key "f"
    :transient "--flush-only"
    :class transient-switch
    :argument "--flush-only"
    :transient-group "Options"
    :level 2
    :order 4)
   (import-only
    :initarg :import-only
    :type boolean
    :initform nil
    :documentation "Only import from JSONL."
    :long-option "import-only"
    :option-type :boolean
    :key "i"
    :transient "--import-only"
    :class transient-switch
    :argument "--import-only"
    :transient-group "Options"
    :level 2
    :order 5)
   (squash
    :initarg :squash
    :type boolean
    :initform nil
    :documentation "Accumulate changes without committing."
    :long-option "squash"
    :option-type :boolean
    :key "s"
    :transient "--squash"
    :class transient-switch
    :argument "--squash"
    :transient-group "Options"
    :level 2
    :order 6)
   (message
    :initarg :message
    :type (or null string)
    :initform nil
    :documentation "Commit message."
    :long-option "message"
    :short-option "m"
    :option-type :string
    :key "m"
    :transient "--message"
    :class transient-option
    :argument "--message="
    :prompt "Commit message: "
    :transient-group "Options"
    :level 2
    :order 7)
   (status-flag
    :initarg :status-flag
    :type boolean
    :initform nil
    :documentation "Show diff between sync branch and main."
    :long-option "status"
    :option-type :boolean
    :key "S"
    :transient "--status"
    :class transient-switch
    :argument "--status"
    :transient-group "Options"
    :level 2
    :order 8)
   (merge-flag
    :initarg :merge-flag
    :type boolean
    :initform nil
    :documentation "Merge sync branch back to main."
    :long-option "merge"
    :option-type :boolean
    :key "M"
    :transient "--merge"
    :class transient-switch
    :argument "--merge"
    :transient-group "Options"
    :level 2
    :order 9)
   (check
    :initarg :check
    :type boolean
    :initform nil
    :documentation "Pre-sync integrity check."
    :long-option "check"
    :option-type :boolean
    :key "c"
    :transient "--check"
    :class transient-switch
    :argument "--check"
    :transient-group "Options"
    :level 2
    :order 10)
   (accept-rebase
    :initarg :accept-rebase
    :type boolean
    :initform nil
    :documentation "Accept rebased history from remote."
    :long-option "accept-rebase"
    :option-type :boolean
    :key "r"
    :transient "--accept-rebase"
    :class transient-switch
    :argument "--accept-rebase"
    :transient-group "Options"
    :level 3
    :order 11)
   (from-main
    :initarg :from-main
    :type boolean
    :initform nil
    :documentation "Merge from main branch instead of sync branch."
    :long-option "from-main"
    :option-type :boolean
    :key "F"
    :transient "--from-main"
    :class transient-switch
    :argument "--from-main"
    :transient-group "Options"
    :level 3
    :order 12)
   (no-git-history
    :initarg :no-git-history
    :type boolean
    :initform nil
    :documentation "Disable git history tracking in sync."
    :long-option "no-git-history"
    :option-type :boolean
    :key "H"
    :transient "--no-git-history"
    :class transient-switch
    :argument "--no-git-history"
    :transient-group "Options"
    :level 3
    :order 13)
   (rename-on-import
    :initarg :rename-on-import
    :type boolean
    :initform nil
    :documentation "Rename issues to match repo on import."
    :long-option "rename-on-import"
    :option-type :boolean
    :key "R"
    :transient "--rename-on-import"
    :class transient-switch
    :argument "--rename-on-import"
    :transient-group "Options"
    :level 3
    :order 14))
  :documentation "Represents bd sync command.
Synchronizes issues with git remote."))

(cl-defmethod beads-command-subcommand ((_command beads-command-sync))
  "Return \"sync\" as the CLI subcommand."
  "sync")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-sync))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menu

;;;###autoload (autoload 'beads-sync-transient "beads-command-sync" nil t)
(beads-meta-define-transient beads-command-sync "beads-sync-transient"
  "Synchronize issues with git remote (auto-generated menu).

See `beads-sync' for the full user-facing transient menu with
compilation buffer output."
  beads-option-global-section)

;;; Interactive Sync Workflow

(defun beads-sync--refresh-all-buffers ()
  "Refresh all beads buffers after sync."
  ;; Refresh all beads-list buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (eq major-mode 'beads-list-mode)
                 (bound-and-true-p beads-list--command))
        (ignore-errors
          (beads-list-refresh t)))))
  ;; Refresh all beads-show buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'beads-show-mode)
        (ignore-errors
          (beads-refresh-show))))))

(defun beads-sync--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:dry-run BOOL :message STRING :no-pull BOOL :no-push BOOL)."
  ;; Note: transient 0.12.0 can return `t' instead of "" for empty option
  ;; values.  We use beads--sanitize-string to convert non-string values
  ;; to nil.
  (let* ((dry-run (transient-arg-value "--dry-run" args))
         (message (beads--sanitize-string
                   (transient-arg-value "--message=" args)))
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
  ;; Use push/nreverse for O(n) performance instead of repeated append (O(n^2))
  ;; Push in reverse of desired order, then nreverse to get: bd sync [flags]
  (let ((parts nil))
    (push beads-executable parts)
    (push "sync" parts)
    (when dry-run
      (push "--dry-run" parts))
    (when message
      (let ((trimmed (string-trim message)))
        (unless (string-empty-p trimmed)
          (push "-m" parts)
          (push trimmed parts))))
    (when no-pull
      (push "--no-pull" parts))
    (when no-push
      (push "--no-push" parts))
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

;;; Suffix Commands

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

;;;###autoload (autoload 'beads-sync "beads-command-sync" nil t)
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

(provide 'beads-command-sync)
;;; beads-command-sync.el ends here
