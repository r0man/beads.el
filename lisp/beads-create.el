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

;; Forward declarations
(declare-function beads-show "beads-show")

;;; Utility Functions

(defun beads-create--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:title STRING :type STRING :priority NUMBER
         :description STRING :custom-id STRING :dependencies STRING
         :acceptance STRING :assignee STRING :design STRING
         :external-ref STRING :labels STRING :force BOOLEAN
         :parent STRING :repo STRING :from-template STRING :file STRING).

This uses transient's standard argument parsing with dash-style flags."
  (let* ((title (transient-arg-value "--title=" args))
         (type (transient-arg-value "--type=" args))
         (priority-str (transient-arg-value "--priority=" args))
         (priority (when priority-str (string-to-number priority-str)))
         (description (transient-arg-value "--description=" args))
        (custom-id (transient-arg-value "--id=" args))
        (dependencies (transient-arg-value "--deps=" args))
        (acceptance (transient-arg-value "--acceptance=" args))
        (assignee (transient-arg-value "--assignee=" args))
        (design (transient-arg-value "--design=" args))
        (external-ref (transient-arg-value "--external-ref=" args))
        (labels (transient-arg-value "--labels=" args))
        (force (transient-arg-value "--force" args))
        (parent (transient-arg-value "--parent=" args))
        (repo (transient-arg-value "--repo=" args))
        (from-template (transient-arg-value "--from-template=" args))
        (file (transient-arg-value "--file=" args)))
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
          :force force
          :parent parent
          :repo repo
          :from-template from-template
          :file file)))

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
Returns list of arguments for bd create command.

Title is passed as positional argument, other flags use standard
dash-style syntax matching bd CLI."
  (let ((args (list (plist-get parsed :title))))
    ;; Add flags in consistent order (flag then value)
    (when-let ((type (plist-get parsed :type)))
      (setq args (append args (list "-t" type))))
    (when-let ((priority (plist-get parsed :priority)))
      (setq args (append args (list "-p" (number-to-string priority)))))
    (when-let ((desc (plist-get parsed :description)))
      (unless (string-empty-p (string-trim desc))
        (setq args (append args (list "-d" desc)))))
    (when-let ((acceptance (plist-get parsed :acceptance)))
      (unless (string-empty-p (string-trim acceptance))
        (setq args (append args (list "--acceptance" acceptance)))))
    (when-let ((assignee (plist-get parsed :assignee)))
      (unless (string-empty-p (string-trim assignee))
        (setq args (append args (list "-a" assignee)))))
    (when-let ((design (plist-get parsed :design)))
      (unless (string-empty-p (string-trim design))
        (setq args (append args (list "--design" design)))))
    (when-let ((external-ref (plist-get parsed :external-ref)))
      (unless (string-empty-p (string-trim external-ref))
        (setq args (append args (list "--external-ref" external-ref)))))
    (when-let ((labels (plist-get parsed :labels)))
      (unless (string-empty-p (string-trim labels))
        (setq args (append args (list "-l" labels)))))
    (when-let ((id (plist-get parsed :custom-id)))
      (unless (string-empty-p (string-trim id))
        (setq args (append args (list "--id" id)))))
    (when-let ((deps (plist-get parsed :dependencies)))
      (unless (string-empty-p (string-trim deps))
        (setq args (append args (list "--deps" deps)))))
    (when-let ((parent (plist-get parsed :parent)))
      (unless (string-empty-p (string-trim parent))
        (setq args (append args (list "--parent" parent)))))
    (when-let ((repo (plist-get parsed :repo)))
      (unless (string-empty-p (string-trim repo))
        (setq args (append args (list "--repo" repo)))))
    (when-let ((from-template (plist-get parsed :from-template)))
      (unless (string-empty-p (string-trim from-template))
        (setq args (append args (list "--from-template" from-template)))))
    (when-let ((file (plist-get parsed :file)))
      (unless (string-empty-p (string-trim file))
        (setq args (append args (list "-f" file)))))
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
            ;; Optionally show the created issue in a proper buffer
            (when (y-or-n-p (format "Show issue %s? " issue-id))
              (beads-show issue-id))
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
    ;; Clear transient's argument state using transient-reset
    (transient-reset)
    ;; Refresh the transient display to show cleared state
    (transient--redisplay)
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

;;; Transient Groups

(transient-define-group beads-create-infix-arguments
  ;; Options for creating a new issue.  Grouped logically by purpose:
  ;; Required fields, Issue attributes, Content, and Advanced options.
  ["Required"
   (beads-option-create-title)]
  ["Issue attributes"
   (beads-option-create-type)
   (beads-option-create-priority)
   (beads-option-create-assignee)
   (beads-option-create-labels)]
  ["Content"
   (beads-option-create-description)
   (7 beads-option-create-acceptance)
   (7 beads-option-create-design)]
  ["Advanced"
   (7 beads-option-create-external-ref)
   (7 beads-option-create-custom-id)
   (7 beads-option-create-dependencies)
   (7 beads-option-create-parent)
   (7 beads-option-create-repo)
   (7 beads-option-create-from-template)
   (7 beads-option-create-file)
   (7 "-f" "Force creation" "--force")]
  ["Global Options"
   (7 beads-option-global-actor)
   (7 beads-option-global-db)
   (7 beads-option-global-json)
   (7 beads-option-global-no-auto-flush)
   (7 beads-option-global-no-auto-import)
   (7 beads-option-global-no-daemon)
   (7 beads-option-global-no-db)
   (7 beads-option-global-sandbox)])

;;; Main Transient Menu

;;;###autoload (autoload 'beads-create "beads-create" nil t)
(transient-define-prefix beads-create ()
  "Create a new issue in Beads.

This transient menu provides an interactive interface for setting
all parameters of the bd create command.  Required fields are
validated before execution."
  beads-create-infix-arguments
  ["Actions"
   ("x" "Create issue" beads-create--execute)
   ("P" "Preview command" beads-create--preview)
   ("R" "Reset all fields" beads-create--reset)])

(provide 'beads-create)
;;; beads-create.el ends here
