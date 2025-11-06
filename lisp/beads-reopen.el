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

;;; Code:

(require 'beads)
(require 'beads-list)
(require 'beads-option)
(require 'beads-show)
(require 'transient)

;;; Utility Functions

(defun beads-reopen--reset-state ()
  "Reset all transient state variables to nil."
  (setq beads-reopen--issue-id nil
        beads-reopen--reason nil))

(defun beads-reopen--format-current-value (value)
  "Format VALUE for display in transient menu.
Returns a propertized string showing the current value."
  (if (and value (not (string-empty-p (string-trim value))))
      (let ((display (if (> (length value) 40)
                        (concat (substring value 0 40) "...")
                      value)))
        (propertize (format " [%s]" display) 'face 'transient-value))
    (propertize " [not set]" 'face 'transient-inactive-value)))

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

(defun beads-reopen--validate-issue-id ()
  "Validate that issue ID is set.
Returns error message string if invalid, nil if valid."
  (when (or (null beads-reopen--issue-id)
            (string-empty-p (string-trim beads-reopen--issue-id)))
    "Issue ID is required"))

(defun beads-reopen--validate-all ()
  "Validate all parameters.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-reopen--validate-issue-id))))

(defun beads-reopen--build-command-args ()
  "Build command arguments from current transient state.
Returns list of arguments for bd reopen command."
  (let (args)
    ;; Push in reverse order for push/nreverse pattern
    ;; Issue ID goes first (will be first after nreverse)
    (push beads-reopen--issue-id args)
    ;; Add reason flag if provided
    (when beads-reopen--reason
      (let ((trimmed (string-trim beads-reopen--reason)))
        (unless (string-empty-p trimmed)
          (push "--reason" args)
          (push trimmed args))))
    (nreverse args)))

(defun beads-reopen--edit-reason-multiline (current-value callback)
  "Edit reason in a multiline buffer.
CURRENT-VALUE is the initial text, CALLBACK is called with the result.
After editing, the transient menu is re-displayed."
  (let* ((buffer-name "*beads-reopen-reason*")
         (buffer (generate-new-buffer buffer-name))
         (parent-buffer (current-buffer)))
    (switch-to-buffer buffer)
    (when current-value
      (insert current-value))
    ;; Use text-mode
    (text-mode)
    ;; Enable visual-line-mode for better editing
    (visual-line-mode 1)
    (setq header-line-format
          "Edit reopen reason: C-c C-c to finish, C-c C-k to cancel")
    ;; Set up keybindings
    (let ((finish-func (lambda ()
                        (interactive)
                        (let ((text (buffer-substring-no-properties
                                   (point-min) (point-max))))
                          (kill-buffer)
                          (switch-to-buffer parent-buffer)
                          (funcall callback text)
                          (message "Reason saved")
                          ;; Re-show the transient menu
                          (beads-reopen))))
          (cancel-func (lambda ()
                        (interactive)
                        (kill-buffer)
                        (switch-to-buffer parent-buffer)
                        (message "Reason edit cancelled")
                        ;; Re-show the transient menu
                        (beads-reopen))))
      (local-set-key (kbd "C-c C-c") finish-func)
      (local-set-key (kbd "C-c C-k") cancel-func))
    (message "Edit reason. C-c C-c to finish, C-c C-k to cancel.")))

;;; Suffix Commands - Reason

(transient-define-suffix beads-reopen--infix-reason ()
  "Set the reason for reopening using a multiline editor."
  :description "Reason (--reason)"
  :key "r"
  (interactive)
  (beads-reopen--edit-reason-multiline
   beads-reopen--reason
   (lambda (text) (setq beads-reopen--reason text))))

;;; Suffix Commands

(transient-define-suffix beads-reopen--execute ()
  "Execute the bd reopen command with current parameters."
  :key "x"
  :description "Reopen issue"
  (interactive)
  (let ((errors (beads-reopen--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (progn
            (let* ((args (beads-reopen--build-command-args))
                   (result (apply #'beads--run-command "reopen" args))
                   (issue (beads--parse-issue result))
                   (issue-id (alist-get 'id issue)))
              (beads-reopen--reset-state)
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
                     ((derived-mode-p 'beads-list-mode)
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
    (beads-reopen--reset-state)
    (message "All fields reset")))

(transient-define-suffix beads-reopen--preview ()
  "Preview the bd reopen command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let ((errors (beads-reopen--validate-all)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                              (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (let* ((args (beads-reopen--build-command-args))
             (cmd (apply #'beads--build-command "reopen" args))
             (cmd-string (mapconcat #'shell-quote-argument cmd " "))
             (preview-msg (format "Command: %s" cmd-string)))
        (message "%s" preview-msg)
        preview-msg))))

;;; Main Transient Menu

(transient-define-prefix beads-reopen--menu ()
  "Transient menu for reopening an issue in Beads."
  ["Reopen Issue"
   (beads-option-reopen-issue-id)
   (beads-reopen--infix-reason)]
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
  ;; Set initial issue ID if provided
  (when issue-id
    (setq beads-reopen--issue-id issue-id))
  ;; Show the transient menu
  (transient-setup 'beads-reopen--menu))

(provide 'beads-reopen)
;;; beads-reopen.el ends here
