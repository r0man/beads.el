;;; beads-create.el --- Transient menu for creating issues -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

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

(defun beads-create--format-current-value (value)
  "Format VALUE for display in transient menu.
Returns a propertized string showing the current value."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-create--validate-title ()
  "Validate that title is set.
Returns error message string if invalid, nil if valid."
  (when (or (null beads-create--title)
            (string-empty-p (string-trim beads-create--title)))
    "Title is required"))

(defun beads-create--validate-type ()
  "Validate that type is valid.
Returns error message string if invalid, nil if valid."
  (when (and beads-create--type
             (not (member beads-create--type
                          '("bug" "feature" "task" "epic" "chore"))))
    "Type must be one of: bug, feature, task, epic, chore"))

(defun beads-create--validate-priority ()
  "Validate that priority is valid.
Returns error message string if invalid, nil if valid."
  (when (and beads-create--priority
             (not (and (numberp beads-create--priority)
                      (>= beads-create--priority 0)
                      (<= beads-create--priority 4))))
    "Priority must be a number between 0 and 4"))

(defun beads-create--validate-dependencies ()
  "Validate dependency format.
Returns error message string if invalid, nil if valid."
  (when beads-create--dependencies
    (unless (string-match-p
             "^[a-z-]+:[a-z0-9-]+\\(,[a-z-]+:[a-z0-9-]+\\)*$"
             beads-create--dependencies)
      "Dependencies must be in format: type:issue-id (e.g., blocks:bd-1)")))

(defun beads-create--validate-all ()
  "Validate all parameters.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-create--validate-title)
              (beads-create--validate-type)
              (beads-create--validate-priority)
              (beads-create--validate-dependencies))))

(defun beads-create--build-command-args ()
  "Build command arguments from current transient state.
Returns list of arguments for bd create command."
  (let ((args (list beads-create--title)))
    ;; Add type flag
    (when beads-create--type
      (setq args (append args (list "-t" beads-create--type))))
    ;; Add priority flag
    (when beads-create--priority
      (setq args (append args (list "-p"
                                     (number-to-string
                                      beads-create--priority)))))
    ;; Add description flag
    (when (and beads-create--description
               (not (string-empty-p (string-trim beads-create--description))))
      (setq args (append args (list "-d" beads-create--description))))
    ;; Add custom ID flag
    (when (and beads-create--custom-id
               (not (string-empty-p (string-trim beads-create--custom-id))))
      (setq args (append args (list "--id" beads-create--custom-id))))
    ;; Add dependencies flag
    (when (and beads-create--dependencies
               (not (string-empty-p (string-trim
                                     beads-create--dependencies))))
      (setq args (append args (list "--deps" beads-create--dependencies))))
    args))

;;; Infix Commands

(transient-define-infix beads-create--infix-title ()
  "Set the title of the issue."
  :class 'transient-option
  :description (lambda ()
                 (concat "Title (required)"
                         (beads-create--format-current-value
                          beads-create--title)))
  :key "t"
  :argument "title="
  :prompt "Issue title: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((title (read-string "Issue title: "
                                      beads-create--title)))
              (setq beads-create--title title)
              title)))

(transient-define-infix beads-create--infix-type ()
  "Set the type of the issue."
  :class 'transient-option
  :description (lambda ()
                 (concat "Type (-t)"
                         (beads-create--format-current-value
                          beads-create--type)))
  :key "T"
  :argument "type="
  :prompt "Type: "
  :choices '("bug" "feature" "task" "epic" "chore")
  :reader (lambda (_prompt _initial-input _history)
            (let ((type (completing-read
                        "Type: "
                        '("bug" "feature" "task" "epic" "chore")
                        nil t beads-create--type)))
              (setq beads-create--type type)
              type)))

(transient-define-infix beads-create--infix-priority ()
  "Set the priority of the issue."
  :class 'transient-option
  :description (lambda ()
                 (concat "Priority (-p)"
                         (beads-create--format-current-value
                          (when beads-create--priority
                            (number-to-string beads-create--priority)))))
  :key "p"
  :argument "priority="
  :prompt "Priority: "
  :reader (lambda (_prompt _initial-input _history)
            (let* ((choices '(("0 - Critical" . 0)
                             ("1 - High" . 1)
                             ("2 - Medium" . 2)
                             ("3 - Low" . 3)
                             ("4 - Backlog" . 4)))
                   (selection (completing-read
                              "Priority: "
                              choices
                              nil t
                              (when beads-create--priority
                                (car (rassoc beads-create--priority
                                            choices)))))
                   (priority (cdr (assoc selection choices))))
              (setq beads-create--priority priority)
              (number-to-string priority))))

(transient-define-infix beads-create--infix-description ()
  "Set the description of the issue."
  :class 'transient-option
  :description (lambda ()
                 (concat "Description (-d)"
                         (beads-create--format-current-value
                          (when beads-create--description
                            (if (> (length beads-create--description) 20)
                                (concat (substring
                                         beads-create--description 0 20)
                                        "...")
                              beads-create--description)))))
  :key "d"
  :argument "description="
  :prompt "Description: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((desc (read-string "Description (single line): "
                                     beads-create--description)))
              (setq beads-create--description desc)
              desc)))

(defun beads-create--edit-text-multiline (current-value callback field-name)
  "Edit text in a multiline buffer.
CURRENT-VALUE is the initial text, CALLBACK is called with the result,
FIELD-NAME is shown in the buffer name and messages."
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
                          (message "%s saved" field-name))))
          (cancel-func (lambda ()
                        (interactive)
                        (kill-buffer)
                        (switch-to-buffer parent-buffer)
                        (message "%s edit cancelled" field-name))))
      (local-set-key (kbd "C-c C-c") finish-func)
      (local-set-key (kbd "C-c C-k") cancel-func))
    (message "Edit %s. C-c C-c to finish, C-c C-k to cancel." field-name)))

(transient-define-suffix beads-create--infix-description-multiline ()
  "Set the description using a multiline editor."
  :description "Description (multiline)"
  :key "M"
  :transient t
  (interactive)
  (beads-create--edit-text-multiline
   beads-create--description
   (lambda (text) (setq beads-create--description text))
   "Description"))

(transient-define-infix beads-create--infix-custom-id ()
  "Set a custom ID for the issue."
  :class 'transient-option
  :description (lambda ()
                 (concat "Custom ID (--id)"
                         (beads-create--format-current-value
                          beads-create--custom-id)))
  :key "i"
  :argument "id="
  :prompt "Custom ID: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((id (read-string "Custom ID (e.g., worker1-100): "
                                   beads-create--custom-id)))
              (setq beads-create--custom-id id)
              id)))

(transient-define-infix beads-create--infix-dependencies ()
  "Set dependencies for the issue."
  :class 'transient-option
  :description (lambda ()
                 (concat "Dependencies (--deps)"
                         (beads-create--format-current-value
                          beads-create--dependencies)))
  :key "D"
  :argument "deps="
  :prompt "Dependencies: "
  :reader (lambda (_prompt _initial-input _history)
            (let ((deps (read-string
                        "Dependencies (type:id, e.g., blocks:bd-1): "
                        beads-create--dependencies)))
              (setq beads-create--dependencies deps)
              deps)))

;;; Suffix Commands

(transient-define-suffix beads-create--execute ()
  "Execute the bd create command with current parameters."
  :key "c"
  :description "Create issue"
  (interactive)
  (let ((errors (beads-create--validate-all)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (let* ((args (beads-create--build-command-args))
                 (result (apply #'beads--run-command "create" args))
                 (issue (beads--parse-issue result))
                 (issue-id (alist-get 'id issue)))
            (beads-create--reset-state)
            (message "Created issue: %s - %s"
                     issue-id
                     (alist-get 'title issue))
            ;; Optionally show the created issue
            (when (y-or-n-p (format "Show issue %s? " issue-id))
              ;; This would call beads-show if it existed
              (message "Issue details: %s" issue)))
        (error
         (message "Failed to create issue: %s"
                  (error-message-string err)))))))

(transient-define-suffix beads-create--reset ()
  "Reset all parameters to their default values."
  :key "r"
  :description "Reset all fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-create--reset-state)
    (message "All fields reset")))

(transient-define-suffix beads-create--preview ()
  "Preview the bd create command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let ((errors (beads-create--validate-all)))
    (if errors
        (message "Validation errors: %s" (string-join errors "; "))
      (let* ((args (beads-create--build-command-args))
             (cmd (apply #'beads--build-command "create" args))
             (cmd-string (mapconcat #'shell-quote-argument cmd " ")))
        (message "Command: %s" cmd-string)))))

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
    (beads-create--infix-description)
    ("M" "Description (multiline)" beads-create--infix-description-multiline)]]
  ["Advanced"
   (beads-create--infix-custom-id)
   (beads-create--infix-dependencies)]
  ["Actions"
   ("c" "Create issue" beads-create--execute)
   ("P" "Preview command" beads-create--preview)
   ("r" "Reset all fields" beads-create--reset)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-create)
;;; beads-create.el ends here
