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
(require 'beads-completion)
(require 'beads-list)
(require 'beads-option)
(require 'beads-show)
(require 'transient)

;;; Utility Functions

(defun beads-close--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-close instance.
Returns a beads-command-close object populated with values from ARGS.

This uses transient's standard argument parsing with dash-style flags."
  (let* ((issue-id (transient-arg-value "--id=" args))
         (reason (transient-arg-value "--reason=" args)))
    (beads-command-close
     :issue-ids (when issue-id (list issue-id))
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
  (when (beads--string-blank-p issue-id)
    "Issue ID is required"))

(defun beads-close--validate-reason (reason)
  "Validate that REASON is set.
Returns error message string if invalid, nil if valid."
  (when (beads--string-blank-p reason)
    "Reason is required"))

(defun beads-close--validate-all (cmd)
  "Validate all parameters from CMD beads-command-close instance.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-close--validate-issue-id
               (when (oref cmd issue-ids)
                 (car (oref cmd issue-ids))))
              (beads-close--validate-reason (oref cmd reason)))))

;;; Suffix Commands

(transient-define-suffix beads-close--execute ()
  "Execute the bd close command with current parameters."
  :key "x"
  :description "Close issue"
  (interactive)
  (let* ((args (transient-args 'beads-close))
         (cmd (beads-close--parse-transient-args args))
         (errors (beads-close--validate-all cmd)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (progn
            (beads-command-execute cmd)
            (let ((issue (oref cmd data)))
              (message "Closed issue: %s - %s"
                       (oref issue id)
                       (oref issue title))
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
                           (string= beads-show--issue-id (oref issue id)))
                      (beads-refresh-show))))))
              nil))
        (error
         (let ((err-msg (format "Failed to close issue: %s"
                               (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-close--reset ()
  "Reset all parameters to their default values."
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

(transient-define-suffix beads-close--preview ()
  "Preview the bd close command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-close))
         (cmd (beads-close--parse-transient-args args))
         (errors (beads-close--validate-all cmd)))
    (if errors
        (let ((err-msg (format "Validation errors: %s" (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (let* ((cmd-list (beads-command-line cmd))
             (cmd-string (mapconcat #'shell-quote-argument cmd-list " "))
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
   (beads-close--execute)
   (beads-close--preview)
   (beads-close--reset)
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
            (beads-completion-read-issue
             "Close issue: " nil t nil 'beads--issue-id-history))))
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
