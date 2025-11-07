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

;;; Code:

(require 'beads)
(require 'beads-option)
(require 'beads-list)
(require 'beads-show)
(require 'transient)

;;; Utility Functions

(defun beads-update--reset-state ()
  "Reset all transient state variables to nil."
  (setq beads-update--issue-id nil
        beads-update--original-data nil
        beads-update--status nil
        beads-update--priority nil
        beads-update--type nil
        beads-update--title nil
        beads-update--description nil
        beads-update--acceptance-criteria nil
        beads-update--design nil
        beads-update--notes nil
        beads-update--assignee nil
        beads-update--external-ref nil))

(defun beads-update--format-current-value (new-value original-value)
  "Format value for display in transient menu.
Shows NEW-VALUE if set, otherwise ORIGINAL-VALUE.
Returns a propertized string."
  (let* ((current (or new-value original-value))
         (changed (and new-value (not (equal new-value original-value))))
         (display (if current
                     (if (> (length (format "%s" current))
                            beads-display-value-max-length)
                         (concat (substring (format "%s" current) 0
                                           beads-display-value-max-length)
                                "...")
                       (format "%s" current))
                   "unset"))
         (face (cond
                (changed 'transient-value)
                (current 'transient-inactive-value)
                (t 'transient-inactive-value))))
    (propertize (format " [%s]" display) 'face face)))

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
      (let ((json (beads--run-command "show" issue-id)))
        (beads--parse-issue json))
    (error
     (beads--error "Failed to fetch issue %s: %s"
                   issue-id
                   (error-message-string err)))))

(defun beads-update--load-issue-data (issue-id)
  "Load issue data for ISSUE-ID and populate state.
Sets beads-update--issue-id and beads-update--original-data."
  (setq beads-update--issue-id issue-id)
  (setq beads-update--original-data
        (beads-update--fetch-issue issue-id))
  ;; Reset current values to nil (user hasn't changed anything yet)
  (setq beads-update--status nil
        beads-update--priority nil
        beads-update--type nil
        beads-update--title nil
        beads-update--description nil
        beads-update--acceptance-criteria nil
        beads-update--design nil
        beads-update--notes nil
        beads-update--assignee nil
        beads-update--external-ref nil))

(defun beads-update--get-original (field)
  "Get original value of FIELD from original-data."
  (alist-get field beads-update--original-data))

(defun beads-update--get-changed-fields ()
  "Return alist of fields that have been changed.
Only includes fields where current value differs from original."
  (let ((changes nil))
    (when (and beads-update--status
              (not (equal beads-update--status
                         (beads-update--get-original 'status))))
      (push (cons 'status beads-update--status) changes))
    (when (and beads-update--priority
              (not (equal beads-update--priority
                         (beads-update--get-original 'priority))))
      (push (cons 'priority beads-update--priority) changes))
    (when (and beads-update--type
              (not (equal beads-update--type
                         (beads-update--get-original 'issue-type))))
      (push (cons 'type beads-update--type) changes))
    (when (and beads-update--title
              (not (equal beads-update--title
                         (beads-update--get-original 'title))))
      (push (cons 'title beads-update--title) changes))
    (when (and beads-update--description
              (not (equal beads-update--description
                         (beads-update--get-original 'description))))
      (push (cons 'description beads-update--description) changes))
    (when (and beads-update--acceptance-criteria
              (not (equal beads-update--acceptance-criteria
                         (beads-update--get-original
                          'acceptance-criteria))))
      (push (cons 'acceptance-criteria
                 beads-update--acceptance-criteria)
            changes))
    (when (and beads-update--design
              (not (equal beads-update--design
                         (beads-update--get-original 'design))))
      (push (cons 'design beads-update--design) changes))
    (when (and beads-update--notes
              (not (equal beads-update--notes
                         (beads-update--get-original 'notes))))
      (push (cons 'notes beads-update--notes) changes))
    (when (and beads-update--assignee
              (not (equal beads-update--assignee
                         (beads-update--get-original 'assignee))))
      (push (cons 'assignee beads-update--assignee) changes))
    (when (and beads-update--external-ref
              (not (equal beads-update--external-ref
                         (beads-update--get-original 'external-ref))))
      (push (cons 'external-ref beads-update--external-ref) changes))
    (nreverse changes)))

(defun beads-update--validate-status ()
  "Validate that status is valid.
Returns error message string if invalid, nil if valid."
  (when (and beads-update--status
            (not (member beads-update--status
                        '("open" "in_progress" "blocked" "closed"))))
    "Status must be one of: open, in_progress, blocked, closed"))

(defun beads-update--validate-type ()
  "Validate that type is valid.
Returns error message string if invalid, nil if valid."
  (when (and beads-update--type
            (not (member beads-update--type
                        '("bug" "feature" "task" "epic" "chore"))))
    "Type must be one of: bug, feature, task, epic, chore"))

(defun beads-update--validate-priority ()
  "Validate that priority is valid.
Returns error message string if invalid, nil if valid."
  (when (and beads-update--priority
            (not (and (numberp beads-update--priority)
                     (>= beads-update--priority 0)
                     (<= beads-update--priority 4))))
    "Priority must be a number between 0 and 4"))

(defun beads-update--validate-all ()
  "Validate all parameters.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-update--validate-status)
              (beads-update--validate-type)
              (beads-update--validate-priority))))

(defun beads-update--build-command-args ()
  "Build command arguments from changed fields.
Returns list of arguments for bd update command."
  (unless beads-update--issue-id
    (user-error "No issue ID set"))
  (let (args
        (changes (beads-update--get-changed-fields)))
    (when (null changes)
      (user-error "No fields have been changed"))
    ;; Push in reverse order for push/nreverse pattern
    ;; Issue ID goes first (will be first after nreverse)
    (push beads-update--issue-id args)
    ;; Collect flag arguments (process changes in original order)
    (dolist (change changes)
      (pcase (car change)
        ('status
         (push "-s" args)
         (push (cdr change) args))
        ('priority
         (push "-p" args)
         (push (number-to-string (cdr change)) args))
        ('type
         (push "-t" args)
         (push (cdr change) args))
        ('title
         (push "--title" args)
         (push (cdr change) args))
        ('description
         (push "--description" args)
         (push (cdr change) args))
        ('acceptance-criteria
         (push "--acceptance-criteria" args)
         (push (cdr change) args))
        ('design
         (push "--design" args)
         (push (cdr change) args))
        ('notes
         (push "--notes" args)
         (push (cdr change) args))
        ('assignee
         (push "-a" args)
         (push (cdr change) args))
        ('external-ref
         (push "--external-ref" args)
         (push (cdr change) args))))
    (nreverse args)))

;;; Multiline Editor Functions

(defun beads-update--edit-text-multiline (current-value fallback-value callback field-name)
  "Edit text in a multiline buffer.
CURRENT-VALUE is the modified text, FALLBACK-VALUE is the original,
CALLBACK is called with the result, FIELD-NAME is shown in messages.
After editing, the transient menu is re-displayed."
  (let* ((buffer-name (format "*beads-%s*" (downcase field-name)))
         (buffer (generate-new-buffer buffer-name))
         (parent-buffer (current-buffer)))
    (switch-to-buffer buffer)
    (insert (or current-value fallback-value ""))
    ;; Use markdown-mode if available, otherwise text-mode
    (if (fboundp 'markdown-mode)
        (markdown-mode)
      (text-mode))
    ;; Enable visual-line-mode for better editing
    (visual-line-mode 1)
    (setq header-line-format
          (format "Edit %s: C-c C-c to finish, C-c C-k to cancel"
                  field-name))
    ;; Set up keybindings
    (let ((finish-func (lambda ()
                        (interactive)
                        (let ((text (buffer-substring-no-properties
                                   (point-min) (point-max))))
                          (kill-buffer)
                          (switch-to-buffer parent-buffer)
                          (funcall callback text)
                          (message "%s saved" field-name)
                          ;; Re-show the transient menu
                          (beads-update--menu))))
          (cancel-func (lambda ()
                        (interactive)
                        (kill-buffer)
                        (switch-to-buffer parent-buffer)
                        (message "%s edit cancelled" field-name)
                        ;; Re-show the transient menu
                        (beads-update--menu))))
      (local-set-key (kbd "C-c C-c") finish-func)
      (local-set-key (kbd "C-c C-k") cancel-func))
    (message "Edit %s. C-c C-c to finish, C-c C-k to cancel." field-name)))

(transient-define-suffix beads-update--infix-description ()
  "Set the description using a multiline editor."
  :description "Description (--description)"
  :key "d"
  (interactive)
  (beads-update--edit-text-multiline
   beads-update--description
   (beads-update--get-original 'description)
   (lambda (text) (setq beads-update--description text))
   "Description"))

(transient-define-suffix beads-update--infix-acceptance-multiline ()
  "Set the acceptance criteria using a multiline editor."
  :description "Acceptance Criteria (multiline)"
  :key "A"
  (interactive)
  (beads-update--edit-text-multiline
   beads-update--acceptance-criteria
   (beads-update--get-original 'acceptance-criteria)
   (lambda (text) (setq beads-update--acceptance-criteria text))
   "Acceptance Criteria"))

(transient-define-suffix beads-update--infix-design-multiline ()
  "Set the design notes using a multiline editor."
  :description "Design (multiline)"
  :key "E"  ; Changed from G to avoid conflict with Magit refresh
  (interactive)
  (beads-update--edit-text-multiline
   beads-update--design
   (beads-update--get-original 'design)
   (lambda (text) (setq beads-update--design text))
   "Design"))

(transient-define-suffix beads-update--infix-notes-multiline ()
  "Set the notes using a multiline editor."
  :description "Notes (multiline)"
  :key "N"
  (interactive)
  (beads-update--edit-text-multiline
   beads-update--notes
   (beads-update--get-original 'notes)
   (lambda (text) (setq beads-update--notes text))
   "Notes"))

;;; Suffix Commands

(transient-define-suffix beads-update--execute ()
  "Execute the bd update command with changed parameters."
  :key "u"
  :description "Update issue"
  (interactive)
  (let* ((errors (beads-update--validate-all))
         (changes (beads-update--get-changed-fields)))
    (when (null changes)
      (user-error "No fields have been changed"))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (progn
            (let* ((args (beads-update--build-command-args))
                   (result (apply #'beads--run-command "update" args))
                   (_issue (beads--parse-issue result)))
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
              (beads-update--reset-state))
            nil)
        (error
         (let ((err-msg (format "Failed to update issue: %s"
                               (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-update--reset ()
  "Reset all changed parameters to their original values."
  :key "r"
  :description "Reset all changes"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all changes? ")
    (setq beads-update--status nil
          beads-update--priority nil
          beads-update--type nil
          beads-update--title nil
          beads-update--description nil
          beads-update--acceptance-criteria nil
          beads-update--design nil
          beads-update--notes nil
          beads-update--assignee nil
          beads-update--external-ref nil)
    (message "All changes reset")))

(transient-define-suffix beads-update--preview ()
  "Preview the bd update command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let ((errors (beads-update--validate-all)))
    (if errors
        (let ((err-msg (format "Validation errors: %s" (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (condition-case err
          (let* ((args (beads-update--build-command-args))
                 (cmd (apply #'beads--build-command "update" args))
                 (cmd-string (mapconcat #'shell-quote-argument cmd " "))
                 (changes (beads-update--get-changed-fields))
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
    (beads-option-update-priority)
    (beads-option-update-type)]
   ["Basic Info"
    (beads-option-update-title)
    (beads-option-update-assignee)
    (beads-option-update-external-ref)]]
  ["Content Fields"
   (beads-update--infix-description)
   (beads-update--infix-acceptance-multiline)
   (beads-update--infix-design-multiline)
   (beads-update--infix-notes-multiline)]
  ["Actions"
   ("u" "Update issue" beads-update--execute)
   ("P" "Preview command" beads-update--preview)
   ("r" "Reset all changes" beads-update--reset)
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
