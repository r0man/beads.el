;;; beads-reopen.el --- Reopen closed issues -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides an interactive interface for reopening closed issues in
;; Beads.  This module allows users to reopen issues with an optional
;; reason, and is context-aware to detect the issue ID from beads-list
;; or beads-show buffers.
;;
;; Usage:
;;   M-x beads-reopen RET
;;
;; The command will:
;; - Detect issue ID from context (beads-list or beads-show buffers)
;; - Prompt for issue ID if not in context
;; - Allow entering a reason for reopening (optional)
;; - Execute bd reopen command
;; - Refresh open beads buffers
;;
;; This module uses the transient-args pattern where transient manages
;; state via its built-in argument system.  Suffix commands retrieve
;; values using (transient-args 'beads-reopen--menu).

;;; Code:

(require 'beads)
(require 'beads-list)
(require 'beads-option)
(require 'beads-show)
(require 'transient)

;;; Utility Functions

(defun beads-reopen--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:issue-id STRING :reason STRING).

This uses transient's standard argument parsing with dash-style flags."
  (let* ((issue-id (transient-arg-value "--id=" args))
         (reason (transient-arg-value "--reason=" args)))
    (list :issue-id issue-id
          :reason reason)))

(defun beads-reopen--detect-issue-id ()
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

(defun beads-reopen--validate-issue-id (issue-id)
  "Validate that ISSUE-ID is set.
Returns error message string if invalid, nil if valid."
  (when (or (null issue-id)
            (string-empty-p (string-trim issue-id)))
    "Issue ID is required"))

(defun beads-reopen--validate-all (parsed)
  "Validate all parameters from PARSED plist.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-reopen--validate-issue-id
               (plist-get parsed :issue-id)))))

(defun beads-reopen--build-command-args (parsed)
  "Build command arguments from PARSED plist.
Returns list of arguments for bd reopen command."
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

(transient-define-suffix beads-reopen--execute ()
  "Execute the bd reopen command with current parameters."
  :key "x"
  :description "Reopen issue"
  (interactive)
  (let* ((args (transient-args 'beads-reopen--menu))
         (parsed (beads-reopen--parse-transient-args args))
         (errors (beads-reopen--validate-all parsed)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (progn
            (let* ((cmd-args (beads-reopen--build-command-args parsed))
                   (result (apply #'beads--run-command "reopen" cmd-args))
                   (issue (beads--parse-issue result))
                   (issue-id (alist-get 'id issue)))
              (message "Reopened issue: %s - %s"
                       issue-id
                       (alist-get 'title issue))
              ;; Invalidate completion cache
              (beads--invalidate-completion-cache)
              ;; Refresh any open beads buffers
              (when beads-auto-refresh
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    (cond
                     ((and (derived-mode-p 'beads-list-mode)
                           (bound-and-true-p beads-list--command))
                      (beads-list-refresh))
                     ((and (derived-mode-p 'beads-show-mode)
                           (string= beads-show--issue-id issue-id))
                      (beads-refresh-show))))))
              nil))
        (error
         (let ((err-msg (format "Failed to reopen issue: %s"
                               (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-reopen--reset ()
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

(transient-define-suffix beads-reopen--preview ()
  "Preview the bd reopen command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-reopen--menu))
         (parsed (beads-reopen--parse-transient-args args))
         (errors (beads-reopen--validate-all parsed)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                              (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (let* ((cmd-args (beads-reopen--build-command-args parsed))
             (cmd (apply #'beads--build-command "reopen" cmd-args))
             (cmd-string (mapconcat #'shell-quote-argument cmd " "))
             (preview-msg (format "Command: %s" cmd-string)))
        (message "%s" preview-msg)
        preview-msg))))

;;; Main Transient Menu

(transient-define-prefix beads-reopen--menu ()
  "Transient menu for reopening an issue in Beads."
  ["Reopen Issue"
   (beads-option-reopen-issue-id)
   (beads-option-reopen-reason)]
  ["Actions"
   ("x" "Reopen issue" beads-reopen--execute)
   ("P" "Preview command" beads-reopen--preview)
   ("R" "Reset fields" beads-reopen--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-reopen (&optional issue-id)
  "Reopen a closed issue in Beads.

This function provides an interactive interface for reopening closed
issues via a transient menu.  The function is context-aware and
automatically detects the issue ID from beads-list or beads-show
buffers.

If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user."
  (interactive
   (list (or (beads-reopen--detect-issue-id)
            (completing-read
             "Reopen issue: "
             (beads--issue-completion-table)
             nil t nil 'beads--issue-id-history))))
  ;; Check executable
  (beads-check-executable)
  ;; Show the transient menu with initial issue ID if provided
  (if issue-id
      ;; Set initial value using transient-args
      (transient-setup 'beads-reopen--menu nil nil
                       :value (list (concat "--id=" issue-id)))
    (transient-setup 'beads-reopen--menu)))

(provide 'beads-reopen)
;;; beads-reopen.el ends here
