;;; beads-update.el --- Transient menu for updating issues -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides a transient menu interface for updating existing issues in
;; Beads.  This module uses the transient library to create an
;; interactive menu that allows users to modify any field of an
;; existing issue.
;;
;; Usage:
;;   M-x beads-update RET
;;
;; The menu is context-aware and detects the issue ID from:
;; - beads-list buffers (from current line)
;; - beads-show buffers (from buffer name)
;; - Otherwise prompts for issue ID with completion
;;
;; The menu automatically fetches the current issue data and
;; pre-populates fields.  Only changed fields are sent to bd update.
;;
;; Available fields:
;; - Status (open, in_progress, blocked, closed)
;; - Priority (0-4)
;; - Type (bug, feature, task, epic, chore)
;; - Title
;; - Description
;; - Acceptance Criteria
;; - Design
;; - Notes
;; - Assignee
;; - External Reference
;;
;; This module uses the transient-args pattern where transient manages
;; state via its built-in argument system.  Suffix commands retrieve
;; values using (transient-args 'beads-update).

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-option)
(require 'beads-list)
(require 'beads-show)
(require 'transient)

;;; State Management

(defvar beads-update--issue-id nil
  "Issue ID being updated (detected from context).")

(defvar beads-update--original-data nil
  "Original issue data alist (for showing current values in menu).")

;;; Utility Functions

(defun beads-update--detect-issue-id ()
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

(defun beads-update--fetch-issue (issue-id)
  "Fetch issue data for ISSUE-ID from bd.
Returns parsed issue alist or signals error."
  (condition-case err
      (beads-command-show! :json t :issue-ids (list issue-id))
    (error
     (beads--error "Failed to fetch issue %s: %s"
                   issue-id
                   (error-message-string err)))))

(defun beads-update--load-issue-data (issue-id)
  "Load issue data for ISSUE-ID and populate state.
Sets beads-update--issue-id and beads-update--original-data."
  (setq beads-update--issue-id issue-id)
  (setq beads-update--original-data
        (beads-update--fetch-issue issue-id)))

(defun beads-update--get-original (field)
  "Get original value of FIELD from original-data.
FIELD is a symbol like `status', `priority', etc.
Original-data is a beads-issue instance."
  (when beads-update--original-data
    (condition-case nil
        (eieio-oref beads-update--original-data field)
      (invalid-slot-name nil))))

(defun beads-update--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-update instance.
Returns a beads-command-update command object with populated slots
from the parsed transient arguments.

This uses transient's standard argument parsing with dash-style flags."
  (let* ((status (transient-arg-value "--status=" args))
         (priority-str (transient-arg-value "--priority=" args))
         (priority (when priority-str (string-to-number priority-str)))
         (title (transient-arg-value "--title=" args))
         (description (transient-arg-value "--description=" args))
         (acceptance (transient-arg-value "--acceptance=" args))
         (design (transient-arg-value "--design=" args))
         (notes (transient-arg-value "--notes=" args))
         (assignee (transient-arg-value "--assignee=" args))
         (external-ref (transient-arg-value "--external-ref=" args)))
    (beads-command-update
     :json t
     :issue-ids (when beads-update--issue-id
                  (list beads-update--issue-id))
     :status status
     :priority priority
     :title title
     :description description
     :acceptance acceptance
     :design design
     :notes notes
     :assignee assignee
     :external-ref external-ref)))

(defun beads-update--get-changed-fields (cmd)
  "Return alist of fields that have been changed in CMD.
Only includes fields where current value differs from original."
  (let ((changes nil))
    (with-slots (status priority title description acceptance design
                       notes assignee external-ref) cmd
      (when status
        (unless (equal status (beads-update--get-original 'status))
          (push (cons 'status status) changes)))
      (when priority
        (unless (equal priority (beads-update--get-original 'priority))
          (push (cons 'priority priority) changes)))
      (when title
        (unless (equal title (beads-update--get-original 'title))
          (push (cons 'title title) changes)))
      (when description
        (unless (equal description
                       (beads-update--get-original 'description))
          (push (cons 'description description) changes)))
      (when acceptance
        (unless (equal acceptance
                       (beads-update--get-original 'acceptance-criteria))
          (push (cons 'acceptance acceptance) changes)))
      (when design
        (unless (equal design (beads-update--get-original 'design))
          (push (cons 'design design) changes)))
      (when notes
        (unless (equal notes (beads-update--get-original 'notes))
          (push (cons 'notes notes) changes)))
      (when assignee
        (unless (equal assignee (beads-update--get-original 'assignee))
          (push (cons 'assignee assignee) changes)))
      (when external-ref
        (unless (equal external-ref
                       (beads-update--get-original 'external-ref))
          (push (cons 'external-ref external-ref) changes)))
      (nreverse changes))))

(defun beads-update--validate-all (cmd)
  "Validate all parameters from CMD command instance.
Returns list of error messages, or nil if all valid."
  (let ((error-msg (beads-command-validate cmd))
        (changes (beads-update--get-changed-fields cmd)))
    (delq nil
          (list
           error-msg
           (when (null changes) "No fields have been changed")))))

;;; Suffix Commands

(transient-define-suffix beads-update--execute ()
  "Execute the bd update command with changed parameters."
  :key "x"
  :description "Update issue"
  (interactive)
  (let* ((args (transient-args 'beads-update))
         (cmd (beads-update--parse-transient-args args))
         (errors (beads-update--validate-all cmd))
         (changes (beads-update--get-changed-fields cmd)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (progn
            (let ((_issue (beads-command-execute cmd)))
              (message "Updated issue: %s (changed %d field%s)"
                       beads-update--issue-id
                       (length changes)
                       (if (= (length changes) 1) "" "s"))
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
                           (string= beads-show--issue-id
                                   beads-update--issue-id))
                      (beads-refresh-show))))))
              ;; Reset state
              (setq beads-update--issue-id nil
                    beads-update--original-data nil))
            nil)
        (error
         (let ((err-msg (format "Failed to update issue: %s"
                               (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-update--reset ()
  "Reset all changed parameters to their original values."
  :key "R"
  :description "Reset all changes"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all changes? ")
    ;; Clear transient's argument state using transient-reset
    (transient-reset)
    ;; Refresh the transient display to show cleared state
    (transient--redisplay)
    (message "All changes reset")))

(transient-define-suffix beads-update--preview ()
  "Preview the bd update command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-update))
         (cmd (beads-update--parse-transient-args args))
         (errors (beads-update--validate-all cmd)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                               (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (condition-case err
          (let* ((cmd-line (beads-command-line cmd))
                 (full-cmd (cons beads-executable cmd-line))
                 (cmd-string (mapconcat #'shell-quote-argument full-cmd " "))
                 (changes (beads-update--get-changed-fields cmd))
                 (preview-msg (format "Command: %s\nChanges: %s"
                                     cmd-string
                                     (mapconcat (lambda (c)
                                                 (format "%s=%s"
                                                         (car c)
                                                         (cdr c)))
                                               changes ", "))))
            (message "%s" preview-msg)
            preview-msg)
        (error
         (let ((err-msg (format "Error: %s" (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

;;; Main Transient Menu

(transient-define-prefix beads-update--menu ()
  "Transient menu for updating an issue in Beads."
  ["Issue Details"
   ["Status & Priority"
    (beads-option-update-status)
    (beads-option-issue-priority)]
   ["Basic Info"
    (beads-option-issue-title)
    (beads-option-issue-assignee)
    (beads-option-issue-external-ref)]]
  ["Content Fields"
   (beads-option-issue-description)
   (beads-option-issue-acceptance)
   (beads-option-issue-design)
   (beads-option-update-notes-multiline)]
  beads-option-global-section
  ["Actions"
   (beads-update--execute)
   (beads-update--preview)
   (beads-update--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-update (&optional issue-id)
  "Update an existing issue in Beads.

This function provides an interactive interface for updating all fields
of an existing issue via a transient menu.  The function is context-aware
and automatically detects the issue ID from beads-list or beads-show
buffers.

If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user."
  (interactive
   (list (or (beads-update--detect-issue-id)
            (completing-read
             "Update issue: "
             (beads--issue-completion-table)
             nil t nil 'beads--issue-id-history))))
  ;; Load issue data before showing menu
  (beads-check-executable)
  (unless issue-id
    (user-error "No issue ID specified"))
  (beads-update--load-issue-data issue-id)
  ;; Show the transient menu
  (beads-update--menu))

(provide 'beads-update)
;;; beads-update.el ends here
