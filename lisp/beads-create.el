;;; beads-create.el --- Transient menu for creating issues -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides a transient menu interface for creating new issues in Beads.
;; This module uses the transient library to create an interactive menu
;; that allows users to set all parameters for `bd create` command.
;;
;; Usage:
;;   M-x beads-create RET
;;
;; The menu allows setting:
;; - Issue type (bug, feature, task, epic, chore)
;; - Priority (0-4)
;; - Description (multiline)
;; - Custom ID (optional, for parallel workers)
;; - Dependencies (optional, format: type:issue-id)
;;
;; After setting parameters, the suffix command executes bd create
;; and displays the created issue.

;;; Code:

(require 'beads)
(require 'transient)

;;; Transient State Variables

(defvar beads-create--title nil
  "Title for the issue being created.")

(defvar beads-create--type nil
  "Type for the issue being created.")

(defvar beads-create--priority nil
  "Priority for the issue being created.")

(defvar beads-create--description nil
  "Description for the issue being created.")

(defvar beads-create--custom-id nil
  "Custom ID for the issue being created.")

(defvar beads-create--dependencies nil
  "Dependencies for the issue being created.")

;;; Utility Functions

(defun beads-create--reset-state ()
  "Reset all transient state variables to nil."
  (setq beads-create--title nil
        beads-create--type nil
        beads-create--priority nil
        beads-create--description nil
        beads-create--custom-id nil
        beads-create--dependencies nil))

(defun beads-create--validate-title (title)
  "Validate that TITLE is set.
Returns error message string if invalid, nil if valid."
  (when (or (null title)
            (string-empty-p (string-trim title)))
    "Title is required"))

(defun beads-create--validate-type (type)
  "Validate that TYPE is valid.
Returns error message string if invalid, nil if valid."
  (when (and type
             (not (member type
                          '("bug" "feature" "task" "epic" "chore"))))
    "Type must be one of: bug, feature, task, epic, chore"))

(defun beads-create--validate-priority (priority)
  "Validate that PRIORITY is valid.
Returns error message string if invalid, nil if valid."
  (when (and priority
             (not (and (numberp priority)
                      (>= priority 0)
                      (<= priority 4))))
    "Priority must be a number between 0 and 4"))

(defun beads-create--validate-dependencies (deps)
  "Validate DEPS format.
Returns error message string if invalid, nil if valid."
  (when deps
    (unless (string-match-p
             "^[a-z-]+:[a-z0-9-]+\\(,[a-z-]+:[a-z0-9-]+\\)*$"
             deps)
      "Dependencies must be in format: type:issue-id (e.g., blocks:bd-1)")))

(defun beads-create--validate-all (parsed)
  "Validate all parameters in PARSED plist.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-create--validate-title (plist-get parsed :title))
              (beads-create--validate-type (plist-get parsed :type))
              (beads-create--validate-priority (plist-get parsed :priority))
              (beads-create--validate-dependencies (plist-get parsed :deps)))))

(defun beads-create--build-command-args (parsed)
  "Build command arguments from PARSED plist.
Returns list of arguments for bd create command."
  (let* ((title (plist-get parsed :title))
         (type (plist-get parsed :type))
         (priority (plist-get parsed :priority))
         (description (plist-get parsed :description))
         (id (plist-get parsed :id))
         (deps (plist-get parsed :deps))
         (args (list title)))
    ;; Add type flag
    (when type
      (setq args (append args (list "-t" type))))
    ;; Add priority flag
    (when priority
      (setq args (append args (list "-p" (number-to-string priority)))))
    ;; Add description flag
    (when (and description
               (not (string-empty-p (string-trim description))))
      (setq args (append args (list "-d" description))))
    ;; Add custom ID flag
    (when (and id (not (string-empty-p (string-trim id))))
      (setq args (append args (list "--id" id))))
    ;; Add dependencies flag
    (when (and deps (not (string-empty-p (string-trim deps))))
      (setq args (append args (list "--deps" deps))))
    args))

;;; Infix Commands

(transient-define-infix beads-create--infix-title ()
  "Set the title of the issue."
  :class 'transient-option
  :description "Title (required)"
  :key "t"
  :argument "title="
  :prompt "Issue title: "
  :reader (lambda (_prompt _initial-input _history)
            (read-string "Issue title: ")))

(transient-define-infix beads-create--infix-type ()
  "Set the type of the issue."
  :class 'transient-option
  :description "Type (-t)"
  :key "T"
  :argument "type="
  :prompt "Type: "
  :choices '("bug" "feature" "task" "epic" "chore")
  :reader (lambda (_prompt _initial-input _history)
            (completing-read "Type: "
                           '("bug" "feature" "task" "epic" "chore")
                           nil t)))

(transient-define-infix beads-create--infix-priority ()
  "Set the priority of the issue."
  :class 'transient-option
  :description "Priority (-p)"
  :key "p"
  :argument "priority="
  :prompt "Priority: "
  :reader (lambda (_prompt _initial-input _history)
            (let* ((choices '(("0 - Critical" . 0)
                             ("1 - High" . 1)
                             ("2 - Medium" . 2)
                             ("3 - Low" . 3)
                             ("4 - Backlog" . 4)))
                   (selection (completing-read "Priority: " choices nil t))
                   (priority (cdr (assoc selection choices))))
              (number-to-string priority))))

(defun beads-create--edit-text-multiline (current-value callback field-name)
  "Edit text in a multiline buffer.
CURRENT-VALUE is the initial text, CALLBACK is called with the result,
FIELD-NAME is shown in the buffer name and messages.
After editing, the transient menu is re-displayed."
  (let* ((buffer-name (format "*beads-%s*" (downcase field-name)))
         (buffer (generate-new-buffer buffer-name))
         (parent-buffer (current-buffer)))
    (switch-to-buffer buffer)
    (when current-value
      (insert current-value))
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
                          (transient-setup 'beads-create))))
          (cancel-func (lambda ()
                        (interactive)
                        (kill-buffer)
                        (switch-to-buffer parent-buffer)
                        (message "%s edit cancelled" field-name)
                        ;; Re-show the transient menu
                        (transient-setup 'beads-create))))
      (local-set-key (kbd "C-c C-c") finish-func)
      (local-set-key (kbd "C-c C-k") cancel-func))
    (message "Edit %s. C-c C-c to finish, C-c C-k to cancel." field-name)))

(transient-define-suffix beads-create--infix-description ()
  "Set the description using a multiline editor."
  :description "Description (-d)"
  :key "d"
  (interactive)
  (beads-create--edit-text-multiline
   beads-create--description
   (lambda (text) (setq beads-create--description text))
   "Description"))

(transient-define-infix beads-create--infix-custom-id ()
  "Set a custom ID for the issue."
  :class 'transient-option
  :description "Custom ID (--id)"
  :key "i"
  :argument "id="
  :prompt "Custom ID (e.g., worker1-100): "
  :reader (lambda (_prompt _initial-input _history)
            (read-string "Custom ID (e.g., worker1-100): ")))

(transient-define-infix beads-create--infix-dependencies ()
  "Set dependencies for the issue."
  :class 'transient-option
  :description "Dependencies (--deps)"
  :key "D"
  :argument "deps="
  :prompt "Dependencies (type:id, e.g., blocks:bd-1): "
  :reader (lambda (_prompt _initial-input _history)
            (read-string "Dependencies (type:id, e.g., blocks:bd-1): ")))

;;; Suffix Commands

(defun beads-create--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:title STRING :type STRING :priority NUMBER :description STRING
:id STRING :deps STRING)."
  (let ((title nil)
        (type nil)
        (priority nil)
        (id nil)
        (deps nil))
    (while args
      (let ((arg (pop args)))
        (cond
         ((string-prefix-p "title=" arg)
          (setq title (substring arg 6)))
         ((string-prefix-p "type=" arg)
          (setq type (substring arg 5)))
         ((string-prefix-p "priority=" arg)
          (setq priority (string-to-number (substring arg 9))))
         ((string-prefix-p "id=" arg)
          (setq id (substring arg 3)))
         ((string-prefix-p "deps=" arg)
          (setq deps (substring arg 5))))))
    (list :title title
          :type type
          :priority priority
          :description beads-create--description ; from multiline editor
          :id id
          :deps deps)))

(transient-define-suffix beads-create--execute ()
  "Execute the bd create command with current parameters."
  :key "c"
  :description "Create issue"
  (interactive)
  (let* ((args (transient-args 'beads-create))
         (parsed (beads-create--parse-transient-args args))
         (errors (beads-create--validate-all parsed)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (progn
            (let* ((cmd-args (beads-create--build-command-args parsed))
                   (result (apply #'beads--run-command "create" cmd-args))
                   (issue (beads--parse-issue result))
                   (issue-id (alist-get 'id issue)))
              ;; Update state variables for potential subsequent calls
              (setq beads-create--title (plist-get parsed :title)
                    beads-create--type (plist-get parsed :type)
                    beads-create--priority (plist-get parsed :priority)
                    beads-create--custom-id (plist-get parsed :id)
                    beads-create--dependencies (plist-get parsed :deps))
              (beads-create--reset-state)
              (message "Created issue: %s - %s"
                       issue-id
                       (alist-get 'title issue))
              ;; Optionally show the created issue
              (when (y-or-n-p (format "Show issue %s? " issue-id))
                ;; This would call beads-show if it existed
                (message "Issue details: %s" issue)))
            nil)
        (error
         (let ((err-msg (format "Failed to create issue: %s"
                               (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-create--reset ()
  "Reset all parameters to their default values."
  :key "R"
  :description "Reset all fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-create--reset-state)
    ;; Reset transient's argument state and refresh
    (transient-reset)
    (message "All fields reset")))

(transient-define-suffix beads-create--preview ()
  "Preview the bd create command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-create))
         (parsed (beads-create--parse-transient-args args))
         (errors (beads-create--validate-all parsed)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                              (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (let* ((cmd-args (beads-create--build-command-args parsed))
             (cmd (apply #'beads--build-command "create" cmd-args))
             (cmd-string (mapconcat #'shell-quote-argument cmd " "))
             (preview-msg (format "Command: %s" cmd-string)))
        (message "%s" preview-msg)
        preview-msg))))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-create "beads-create" nil t)
(transient-define-prefix beads-create ()
  "Create a new issue in Beads.

This transient menu provides an interactive interface for setting
all parameters of the bd create command.  Required fields are
validated before execution."
  :value (lambda () nil)
  ["Issue Details"
   ["Basic"
    (beads-create--infix-title)
    (beads-create--infix-type)
    (beads-create--infix-priority)]
   ["Content"
    (beads-create--infix-description)]]
  ["Advanced"
   (beads-create--infix-custom-id)
   (beads-create--infix-dependencies)]
  ["Actions"
   ("c" "Create issue" beads-create--execute)
   ("P" "Preview command" beads-create--preview)
   ("R" "Reset all fields" beads-create--reset)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-create)
;;; beads-create.el ends here
