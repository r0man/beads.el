;;; beads-reopen.el --- Reopen issues with reason -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides an interactive interface for reopening issues in Beads.
;; This module allows users to reopen issues with an optional reason,
;; and is context-aware to detect the issue ID from beads-list or
;; beads-show buffers.
;;
;; Usage:
;;   M-x beads-reopen RET
;;
;; The command will:
;; - Detect issue ID from context (beads-list or beads-show buffers)
;; - Prompt for issue ID if not in context
;; - Allow entering a reason for reopening (optional but recommended)
;; - Execute bd reopen command
;; - Refresh open beads buffers

;;; Code:

(require 'beads)
(require 'beads-list)
(require 'beads-show)
(require 'transient)

;;; Transient State Variables

(defvar beads-reopen--issue-id nil
  "Issue ID to reopen.")

(defvar beads-reopen--reason nil
  "Reason for reopening the issue.")

;;; Utility Functions

(defun beads-reopen--reset-state ()
  "Reset all transient state variables to nil."
  (setq beads-reopen--issue-id nil
        beads-reopen--reason nil))

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

(defun beads-reopen--validate-issue-id (id)
  "Validate that issue ID is set.
Returns error message string if invalid, nil if valid."
  (when (or (null id)
            (string-empty-p (string-trim id)))
    "Issue ID is required"))

(defun beads-reopen--validate-all (parsed)
  "Validate all parameters in PARSED plist.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-reopen--validate-issue-id (plist-get parsed :id)))))

(defun beads-reopen--build-command-args (parsed)
  "Build command arguments from PARSED plist.
Returns list of arguments for bd reopen command."
  (let* ((id (plist-get parsed :id))
         (reason (plist-get parsed :reason))
         (args (list id)))
    ;; Add reason flag if provided
    (when (and reason (not (string-empty-p (string-trim reason))))
      (setq args (append args (list "--reason" reason))))
    args))

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

;;; Infix Commands

(transient-define-infix beads-reopen--infix-issue-id ()
  "Set the issue ID to reopen."
  :class 'transient-option
  :description "Issue ID (required)"
  :key "i"
  :argument "id="
  :prompt "Issue ID to reopen: "
  :reader (lambda (_prompt _initial-input _history)
            (completing-read "Issue ID to reopen: "
                           (beads--issue-completion-table)
                           nil t beads-reopen--issue-id
                           'beads--issue-id-history)))

(transient-define-suffix beads-reopen--infix-reason ()
  "Set the reason for reopening using a multiline editor."
  :description "Reason (--reason)"
  :key "r"
  (interactive)
  (beads-reopen--edit-reason-multiline
   beads-reopen--reason
   (lambda (text) (setq beads-reopen--reason text))))

;;; Suffix Commands

(defun beads-reopen--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:id STRING :reason STRING).
Note: Reason is handled via state variable from multiline editor."
  (let ((id nil))
    (while args
      (let ((arg (pop args)))
        (when (string-prefix-p "id=" arg)
          (setq id (substring arg 3)))))
    (list :id id
          :reason beads-reopen--reason)))

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
              ;; Update state variables for potential subsequent calls
              (setq beads-reopen--issue-id (plist-get parsed :id))
              (beads-reopen--reset-state)
              (message "Reopend issue: %s - %s"
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
    ;; Clear transient's argument state
    (transient-set)
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
   (beads-reopen--infix-issue-id)
   (beads-reopen--infix-reason)]
  ["Actions"
   ("x" "Reopen issue" beads-reopen--execute)
   ("P" "Preview command" beads-reopen--preview)
   ("R" "Reset fields" beads-reopen--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-reopen (&optional issue-id)
  "Reopen an issue in Beads.

This function provides an interactive interface for reopening issues
via a transient menu.  The function is context-aware and
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
