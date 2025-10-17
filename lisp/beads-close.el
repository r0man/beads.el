;;; beads-close.el --- Close issues with reason -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

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

;;; Code:

(require 'beads)
(require 'beads-list)
(require 'beads-show)
(require 'transient)

;;; Transient State Variables

(defvar beads-close--issue-id nil
  "Issue ID to close.")

(defvar beads-close--reason nil
  "Reason for closing the issue.")

;;; Utility Functions

(defun beads-close--reset-state ()
  "Reset all transient state variables to nil."
  (setq beads-close--issue-id nil
        beads-close--reason nil))

(defun beads-close--format-current-value (value)
  "Format VALUE for display in transient menu.
Returns a propertized string showing the current value."
  (if (and value (not (string-empty-p (string-trim value))))
      (let ((display (if (> (length value) 40)
                        (concat (substring value 0 40) "...")
                      value)))
        (propertize (format " [%s]" display) 'face 'transient-value))
    (propertize " [not set]" 'face 'transient-inactive-value)))

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

(defun beads-close--validate-issue-id ()
  "Validate that issue ID is set.
Returns error message string if invalid, nil if valid."
  (when (or (null beads-close--issue-id)
            (string-empty-p (string-trim beads-close--issue-id)))
    "Issue ID is required"))

(defun beads-close--validate-all ()
  "Validate all parameters.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-close--validate-issue-id))))

(defun beads-close--build-command-args ()
  "Build command arguments from current transient state.
Returns list of arguments for bd close command."
  (let ((args (list beads-close--issue-id)))
    ;; Add reason flag if provided
    (when (and beads-close--reason
               (not (string-empty-p (string-trim beads-close--reason))))
      (setq args (append args (list "--reason" beads-close--reason))))
    args))

(defun beads-close--edit-reason-multiline (current-value callback)
  "Edit reason in a multiline buffer.
CURRENT-VALUE is the initial text, CALLBACK is called with the result.
After editing, the transient menu is re-displayed."
  (let* ((buffer-name "*beads-close-reason*")
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
          "Edit close reason: C-c C-c to finish, C-c C-k to cancel")
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
                          (beads-close))))
          (cancel-func (lambda ()
                        (interactive)
                        (kill-buffer)
                        (switch-to-buffer parent-buffer)
                        (message "Reason edit cancelled")
                        ;; Re-show the transient menu
                        (beads-close))))
      (local-set-key (kbd "C-c C-c") finish-func)
      (local-set-key (kbd "C-c C-k") cancel-func))
    (message "Edit reason. C-c C-c to finish, C-c C-k to cancel.")))

;;; Infix Commands

(transient-define-infix beads-close--infix-issue-id ()
  "Set the issue ID to close."
  :class 'transient-option
  :description (lambda ()
                 (concat "Issue ID (required)"
                         (beads-close--format-current-value
                          beads-close--issue-id)))
  :key "i"
  :argument "id="
  :prompt "Issue ID: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((id (completing-read
                      "Issue ID to close: "
                      (beads--issue-completion-table)
                      nil t beads-close--issue-id
                      'beads--issue-id-history)))
              (setq beads-close--issue-id id)
              id)))

(transient-define-suffix beads-close--infix-reason ()
  "Set the reason for closing using a multiline editor."
  :description "Reason (--reason)"
  :key "r"
  (interactive)
  (beads-close--edit-reason-multiline
   beads-close--reason
   (lambda (text) (setq beads-close--reason text))))

;;; Suffix Commands

(transient-define-suffix beads-close--execute ()
  "Execute the bd close command with current parameters."
  :key "x"
  :description "Close issue"
  (interactive)
  (let ((errors (beads-close--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (let* ((args (beads-close--build-command-args))
                 (result (apply #'beads--run-command "close" args))
                 (issue (beads--parse-issue result))
                 (issue-id (alist-get 'id issue)))
            (beads-close--reset-state)
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
                    (beads-refresh-show)))))))
        (error
         (message "Failed to close issue: %s"
                  (error-message-string err)))))))

(transient-define-suffix beads-close--reset ()
  "Reset all parameters to their default values."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-close--reset-state)
    (message "All fields reset")))

(transient-define-suffix beads-close--preview ()
  "Preview the bd close command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let ((errors (beads-close--validate-all)))
    (if errors
        (message "Validation errors: %s" (string-join errors "; "))
      (let* ((args (beads-close--build-command-args))
             (cmd (apply #'beads--build-command "close" args))
             (cmd-string (mapconcat #'shell-quote-argument cmd " ")))
        (message "Command: %s" cmd-string)))))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-close "beads-close" nil t)
(transient-define-prefix beads-close (&optional issue-id)
  "Close an issue in Beads with an optional reason.

This transient menu provides an interactive interface for closing
issues. The issue ID is detected from context (beads-list or
beads-show buffers) or prompted for completion.

Optional argument ISSUE-ID pre-fills the issue to close."
  :value (lambda () nil)
  (interactive
   (list (beads-close--detect-issue-id)))
  ;; Set initial issue ID if provided or detected
  (when issue-id
    (setq beads-close--issue-id issue-id))
  ;; Check executable
  (beads-check-executable)
  ;; Show menu
  ["Close Issue"
   (beads-close--infix-issue-id)
   (beads-close--infix-reason)]
  ["Actions"
   ("x" "Close issue" beads-close--execute)
   ("P" "Preview command" beads-close--preview)
   ("R" "Reset fields" beads-close--reset)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-close)
;;; beads-close.el ends here
