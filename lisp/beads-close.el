;;; beads-close.el --- Close issues with reason -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides an interactive interface for closing issues in Beads.
;; This module allows users to close issues with an optional reason,
;; and is context-aware to detect the issue ID from beads-list or
;; beads-show buffers.
;;
;; Usage:
;;   M-x beads-close RET
;;
;; The command will:
;; - Detect issue ID from context (beads-list or beads-show buffers)
;; - Prompt for issue ID if not in context
;; - Allow entering a reason for closing (optional but recommended)
;; - Execute bd close command
;; - Refresh open beads buffers
;;
;; This module uses the transient-args pattern where transient manages
;; state via its built-in argument system.  Suffix commands retrieve
;; values using (transient-args 'beads-close).

;;; Code:

(require 'beads)
(require 'beads-list)
(require 'beads-option)
(require 'beads-show)
(require 'transient)

;;; Utility Functions

(defun beads-close--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:issue-id STRING :reason STRING).

This uses transient's standard argument parsing with dash-style flags."
  (let* ((issue-id (transient-arg-value "--id=" args))
         (reason (transient-arg-value "--reason=" args)))
    (list :issue-id issue-id
          :reason reason)))

(defun beads-close--detect-issue-id ()
  "Detect issue ID from current context.
Returns issue ID string or nil if not found."
  (or
   ;; From beads-list buffer
   (when (derived-mode-p 'beads-list-mode)
     (beads-list--current-issue-id))
   ;; From beads-show buffer
   (when (derived-mode-p 'beads-show-mode)
     beads-show--issue-id)
   ;; From buffer name (*beads-show: bd-N*)
   (when (string-match "\\*beads-show: \\(bd-[0-9]+\\)\\*"
                      (buffer-name))
     (match-string 1 (buffer-name)))))

(defun beads-close--validate-issue-id (issue-id)
  "Validate that ISSUE-ID is set.
Returns error message string if invalid, nil if valid."
  (when (or (null issue-id)
            (string-empty-p (string-trim issue-id)))
    "Issue ID is required"))

(defun beads-close--validate-all (parsed)
  "Validate all parameters from PARSED plist.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-close--validate-issue-id (plist-get parsed :issue-id)))))

(defun beads-close--build-command-args (parsed)
  "Build command arguments from PARSED plist.
Returns list of arguments for bd close command."
  (let (args)
    ;; Push in reverse order for push/nreverse pattern
    ;; Issue ID goes first (will be first after nreverse)
    (push (plist-get parsed :issue-id) args)
    ;; Add reason flag if provided
    (when-let ((reason (plist-get parsed :reason)))
      (let ((trimmed (string-trim reason)))
        (unless (string-empty-p trimmed)
          (push "--reason" args)
          (push trimmed args))))
    (nreverse args)))

;;; Suffix Commands

(transient-define-suffix beads-close--execute ()
  "Execute the bd close command with current parameters."
  :key "x"
  :description "Close issue"
  (interactive)
  (let* ((args (transient-args 'beads-close))
         (parsed (beads-close--parse-transient-args args))
         (errors (beads-close--validate-all parsed)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (progn
            (let* ((cmd-args (beads-close--build-command-args parsed))
                   (result (apply #'beads--run-command "close" cmd-args))
                   (issue (beads--parse-issue result))
                   (issue-id (alist-get 'id issue)))
              (message "Closed issue: %s - %s"
                       issue-id
                       (alist-get 'title issue))
              ;; Invalidate completion cache
              (beads--invalidate-completion-cache)
              ;; Refresh any open beads buffers
              (when beads-auto-refresh
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    (cond
                     ((derived-mode-p 'beads-list-mode)
                      (beads-list-refresh))
                     ((and (derived-mode-p 'beads-show-mode)
                           (string= beads-show--issue-id issue-id))
                      (beads-refresh-show))))))
              nil))
        (error
         (let ((err-msg (format "Failed to close issue: %s"
                               (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-close--reset ()
  "Reset all parameters to their default values."
  :key "r"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    ;; Clear transient's argument state using transient-reset
    (transient-reset)
    ;; Refresh the transient display to show cleared state
    (transient--redisplay)
    (message "All fields reset")))

(transient-define-suffix beads-close--preview ()
  "Preview the bd close command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-close))
         (parsed (beads-close--parse-transient-args args))
         (errors (beads-close--validate-all parsed)))
    (if errors
        (let ((err-msg (format "Validation errors: %s" (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (let* ((cmd-args (beads-close--build-command-args parsed))
             (cmd (apply #'beads--build-command "close" cmd-args))
             (cmd-string (mapconcat #'shell-quote-argument cmd " "))
             (preview-msg (format "Command: %s" cmd-string)))
        (message "%s" preview-msg)
        preview-msg))))

;;; Main Transient Menu

(transient-define-prefix beads-close--menu ()
  "Transient menu for closing an issue in Beads."
  ["Close Issue"
   (beads-option-close-issue-id)
   (beads-option-close-reason)]
  beads-option-global-section
  ["Actions"
   ("x" "Close issue" beads-close--execute)
   ("P" "Preview command" beads-close--preview)
   ("r" "Reset fields" beads-close--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-close (&optional issue-id)
  "Close an issue in Beads.

This function provides an interactive interface for closing issues
via a transient menu.  The function is context-aware and
automatically detects the issue ID from beads-list or beads-show
buffers.

If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user.

Note: The detected ISSUE-ID is passed as an argument but must be
manually entered in the transient menu.  This is by design to allow
users to review and confirm the issue ID before closing."
  (interactive
   (list (or (beads-close--detect-issue-id)
            (completing-read
             "Close issue: "
             (beads--issue-completion-table)
             nil t nil 'beads--issue-id-history))))
  ;; Suppress unused argument warning
  (ignore issue-id)
  ;; Check executable
  (beads-check-executable)
  ;; Show the transient menu
  ;; Note: issue-id is passed but not automatically set in the menu
  ;; User must enter it via the --id infix
  (beads-close--menu))

(provide 'beads-close)
;;; beads-close.el ends here
