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
;;
;; This module uses the transient-args pattern where transient manages
;; state via its built-in argument system.  Suffix commands retrieve
;; values using (transient-args 'beads-create).

;;; Code:

(require 'beads)
(require 'beads-option)
(require 'transient)

;;; Utility Functions

(defun beads-create--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:title STRING :type STRING :priority NUMBER
         :description STRING :custom-id STRING :dependencies STRING
         :acceptance STRING :assignee STRING :design STRING
         :external-ref STRING :labels STRING :force BOOLEAN)."
  (let ((title nil)
        (type nil)
        (priority nil)
        (description nil)
        (custom-id nil)
        (dependencies nil)
        (acceptance nil)
        (assignee nil)
        (design nil)
        (external-ref nil)
        (labels nil)
        (force nil))
    (while args
      (let ((arg (pop args)))
        (cond
         ((string-prefix-p "title=" arg)
          (setq title (substring arg 6)))
         ((string-prefix-p "type=" arg)
          (setq type (substring arg 5)))
         ((string-prefix-p "priority=" arg)
          (setq priority (string-to-number (substring arg 9))))
         ((string-prefix-p "description=" arg)
          (setq description (substring arg 12)))
         ((string-prefix-p "id=" arg)
          (setq custom-id (substring arg 3)))
         ((string-prefix-p "deps=" arg)
          (setq dependencies (substring arg 5)))
         ((string-prefix-p "acceptance=" arg)
          (setq acceptance (substring arg 11)))
         ((string-prefix-p "assignee=" arg)
          (setq assignee (substring arg 9)))
         ((string-prefix-p "design=" arg)
          (setq design (substring arg 7)))
         ((string-prefix-p "external-ref=" arg)
          (setq external-ref (substring arg 13)))
         ((string-prefix-p "labels=" arg)
          (setq labels (substring arg 7)))
         ((string= "--force" arg)
          (setq force t)))))
    (list :title title
          :type type
          :priority priority
          :description description
          :custom-id custom-id
          :dependencies dependencies
          :acceptance acceptance
          :assignee assignee
          :design design
          :external-ref external-ref
          :labels labels
          :force force)))

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
  "Validate dependency format in DEPENDENCIES.
Returns error message string if invalid, nil if valid."
  (when (and dependencies
             (not (string-empty-p (string-trim dependencies))))
    (unless (string-match-p
             "^[a-z-]+:[a-z0-9.-]+\\(,[a-z-]+:[a-z0-9.-]+\\)*$"
             dependencies)
      "Dependencies must be in format: type:issue-id (e.g., blocks:bd-a1b2)")))

(defun beads-create--validate-all (parsed)
  "Validate all parameters from PARSED plist.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-create--validate-title (plist-get parsed :title))
              (beads-create--validate-type (plist-get parsed :type))
              (beads-create--validate-priority
               (plist-get parsed :priority))
              (beads-create--validate-dependencies
               (plist-get parsed :dependencies)))))

(defun beads-create--build-command-args (parsed)
  "Build command arguments from PARSED plist.
Returns list of arguments for bd create command."
  (let ((args (list (plist-get parsed :title))))
    ;; Add type flag
    (when-let ((type (plist-get parsed :type)))
      (setq args (append args (list "-t" type))))
    ;; Add priority flag
    (when-let ((priority (plist-get parsed :priority)))
      (setq args (append args (list "-p" (number-to-string priority)))))
    ;; Add description flag
    (when-let ((desc (plist-get parsed :description)))
      (unless (string-empty-p (string-trim desc))
        (setq args (append args (list "-d" desc)))))
    ;; Add acceptance flag
    (when-let ((acceptance (plist-get parsed :acceptance)))
      (unless (string-empty-p (string-trim acceptance))
        (setq args (append args (list "--acceptance" acceptance)))))
    ;; Add assignee flag
    (when-let ((assignee (plist-get parsed :assignee)))
      (unless (string-empty-p (string-trim assignee))
        (setq args (append args (list "-a" assignee)))))
    ;; Add design flag
    (when-let ((design (plist-get parsed :design)))
      (unless (string-empty-p (string-trim design))
        (setq args (append args (list "--design" design)))))
    ;; Add external-ref flag
    (when-let ((external-ref (plist-get parsed :external-ref)))
      (unless (string-empty-p (string-trim external-ref))
        (setq args (append args (list "--external-ref" external-ref)))))
    ;; Add labels flag
    (when-let ((labels (plist-get parsed :labels)))
      (unless (string-empty-p (string-trim labels))
        (setq args (append args (list "-l" labels)))))
    ;; Add custom ID flag
    (when-let ((id (plist-get parsed :custom-id)))
      (unless (string-empty-p (string-trim id))
        (setq args (append args (list "--id" id)))))
    ;; Add dependencies flag
    (when-let ((deps (plist-get parsed :dependencies)))
      (unless (string-empty-p (string-trim deps))
        (setq args (append args (list "--deps" deps)))))
    ;; Add force flag
    (when (plist-get parsed :force)
      (setq args (append args (list "--force"))))
    args))

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
          (let* ((cmd-args (beads-create--build-command-args parsed))
                 (result (apply #'beads--run-command "create" cmd-args))
                 (issue (beads--parse-issue result))
                 (issue-id (alist-get 'id issue)))
            (message "Created issue: %s - %s"
                     issue-id
                     (alist-get 'title issue))
            ;; Invalidate completion cache
            (beads--invalidate-completion-cache)
            ;; Optionally show the created issue
            (when (y-or-n-p (format "Show issue %s? " issue-id))
              (message "Issue details: %s" issue))
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
    ;; Clear transient's argument state
    (transient-set)
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
    (beads-option-create-title)
    (beads-option-create-type)
    (beads-option-create-priority)]
   ["Content"
    (beads-option-create-description)
    (beads-option-create-acceptance)
    (beads-option-create-design)]
   ["Assignment"
    (beads-option-create-assignee)
    (beads-option-create-labels)
    (beads-option-create-external-ref)]]
  ["Advanced"
   ["IDs & Deps"
    (beads-option-create-custom-id)
    (beads-option-create-dependencies)]
   ["Options"
    (beads-option-create-force)]]
  ["Actions"
   ("c" "Create issue" beads-create--execute)
   ("P" "Preview command" beads-create--preview)
   ("r" "Reset all fields" beads-create--reset)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-create)
;;; beads-create.el ends here
