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

;;; Custom Transient Classes

(defclass beads-create-multiline-class (transient-option)
  ((always-read :initform t)
   (multiline :initform t :initarg :multiline))
  "A transient infix class for multiline string input.")

(cl-defmethod transient-format-value ((obj beads-create-multiline-class))
  "Format the value of multiline OBJ for display."
  (let ((value (oref obj value)))
    (if (and value (not (string-empty-p value)))
        (propertize (format " [%s]"
                            (truncate-string-to-width
                             (replace-regexp-in-string "\n" "↵" value)
                             20 nil nil "..."))
                    'face 'transient-value)
      (propertize " [unset]" 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-read ((obj beads-create-multiline-class))
  "Read a multiline value for OBJ using a temporary buffer."
  (let* ((prompt (oref obj prompt))
         (value (oref obj value))
         (buffer-name "*beads-multiline-input*")
         (result nil))
    (save-window-excursion
      (pop-to-buffer (get-buffer-create buffer-name))
      (erase-buffer)
      (when (and value (not (string-empty-p value)))
        (insert value))
      (if (fboundp 'markdown-mode)
          (markdown-mode)
        (text-mode))
      (visual-line-mode 1)
      (setq header-line-format
            (format "%s (C-c C-c to finish, C-c C-k to cancel)" prompt))
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map (kbd "C-c C-c")
          (lambda ()
            (interactive)
            (setq result (buffer-substring-no-properties
                          (point-min) (point-max)))
            (kill-buffer)
            (exit-recursive-edit)))
        (define-key map (kbd "C-c C-k")
          (lambda ()
            (interactive)
            (setq result value) ;; Keep old value on cancel
            (kill-buffer)
            (exit-recursive-edit)))
        (use-local-map map))
      (recursive-edit))
    result))

;;; Utility Functions

(defun beads-create--format-current-value (value)
  "Format VALUE for display in transient menu.
Returns a propertized string showing the current value."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-create--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:title STRING :type STRING :priority NUMBER
         :description STRING :custom-id STRING :dependencies STRING)."
  (let ((title nil)
        (type nil)
        (priority nil)
        (description nil)
        (custom-id nil)
        (dependencies nil))
    (while args
      (let ((arg (pop args)))
        (cond
         ((string-prefix-p "title=" arg)
          (setq title (substring arg 6)))
         ((string-prefix-p "-t=" arg)
          (setq type (substring arg 3)))
         ((string-prefix-p "-p=" arg)
          (setq priority (string-to-number (substring arg 3))))
         ((string-prefix-p "-d=" arg)
          (setq description (substring arg 3)))
         ((string-prefix-p "--id=" arg)
          (setq custom-id (substring arg 5)))
         ((string-prefix-p "--deps=" arg)
          (setq dependencies (substring arg 7))))))
    (list :title title
          :type type
          :priority priority
          :description description
          :custom-id custom-id
          :dependencies dependencies)))

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

(defun beads-create--validate-dependencies (dependencies)
  "Validate DEPENDENCIES format.
Returns error message string if invalid, nil if valid."
  (when dependencies
    (unless (string-match-p
             "^[a-z-]+:[a-z0-9-]+\\(,[a-z-]+:[a-z0-9-]+\\)*$"
             dependencies)
      "Dependencies must be in format: type:issue-id (e.g., blocks:bd-1)")))

(defun beads-create--validate-all (parsed)
  "Validate all parameters in PARSED plist.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-create--validate-title (plist-get parsed :title))
              (beads-create--validate-type (plist-get parsed :type))
              (beads-create--validate-priority (plist-get parsed :priority))
              (beads-create--validate-dependencies (plist-get parsed :dependencies)))))

(defun beads-create--build-command-args (parsed)
  "Build command arguments from PARSED plist.
Returns list of arguments for bd create command."
  (let* ((title (plist-get parsed :title))
         (type (plist-get parsed :type))
         (priority (plist-get parsed :priority))
         (description (plist-get parsed :description))
         (custom-id (plist-get parsed :custom-id))
         (dependencies (plist-get parsed :dependencies))
         (args (list title)))
    ;; Add type flag
    (when type
      (setq args (append args (list "-t" type))))
    ;; Add priority flag
    (when priority
      (setq args (append args (list "-p"
                                     (number-to-string priority)))))
    ;; Add description flag
    (when (and description
               (not (string-empty-p (string-trim description))))
      (setq args (append args (list "-d" description))))
    ;; Add custom ID flag
    (when (and custom-id
               (not (string-empty-p (string-trim custom-id))))
      (setq args (append args (list "--id" custom-id))))
    ;; Add dependencies flag
    (when (and dependencies
               (not (string-empty-p (string-trim dependencies))))
      (setq args (append args (list "--deps" dependencies))))
    args))

;;; Infix Commands

(transient-define-infix beads-create--infix-title ()
  "Set the title of the issue."
  :class 'transient-option
  :description "Title (required)"
  :key "t"
  :argument "title="
  :prompt "Issue title: ")

(transient-define-infix beads-create--infix-type ()
  "Set the type of the issue."
  :class 'transient-option
  :description "Type (-t)"
  :key "T"
  :argument "-t="
  :prompt "Type: "
  :choices '("bug" "feature" "task" "epic" "chore"))

(transient-define-infix beads-create--infix-priority ()
  "Set the priority of the issue."
  :class 'transient-option
  :description "Priority (-p)"
  :key "p"
  :argument "-p="
  :prompt "Priority (0-4): "
  :reader (lambda (prompt _initial-input _history)
            (let* ((choices '(("0 - Critical" . "0")
                             ("1 - High" . "1")
                             ("2 - Medium" . "2")
                             ("3 - Low" . "3")
                             ("4 - Backlog" . "4")))
                   (selection (completing-read
                              prompt
                              choices
                              nil t))
                   (priority (cdr (assoc selection choices))))
              priority)))

(transient-define-infix beads-create--infix-description ()
  "Set the description using a multiline editor."
  :class 'beads-create-multiline-class
  :description "Description (-d)"
  :key "d"
  :argument "-d="
  :prompt "Issue description")

(transient-define-infix beads-create--infix-custom-id ()
  "Set a custom ID for the issue."
  :class 'transient-option
  :description "Custom ID (--id)"
  :key "i"
  :argument "--id="
  :prompt "Custom ID (e.g., worker1-100): ")

(transient-define-infix beads-create--infix-dependencies ()
  "Set dependencies for the issue."
  :class 'transient-option
  :description "Dependencies (--deps)"
  :key "D"
  :argument "--deps="
  :prompt "Dependencies (type:id, e.g., blocks:bd-1): ")

;;; Suffix Commands

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
              ;; Reset transient state after successful creation
              (transient-reset)
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
  :key "r"
  :description "Reset all fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
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
        (let ((err-msg (format "Validation errors: %s" (string-join errors "; "))))
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
   ("r" "Reset all fields" beads-create--reset)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-create)
;;; beads-create.el ends here