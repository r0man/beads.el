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
  "Parse transient ARGS list into a beads-command-create instance.
Returns a beads-command-create object populated with values from ARGS.

This uses transient's standard argument parsing with dash-style flags."
  (let* ((title (transient-arg-value "--title=" args))
         (type (transient-arg-value "--type=" args))
         (priority-str (transient-arg-value "--priority=" args))
         (priority (when priority-str (string-to-number priority-str)))
         (description (transient-arg-value "--description=" args))
         (custom-id (transient-arg-value "--id=" args))
         (dependencies-str (transient-arg-value "--deps=" args))
         (dependencies (when (and dependencies-str
                                  (not (string-empty-p (string-trim dependencies-str))))
                         (split-string (string-trim dependencies-str) "," t "[ \t]+")))
         (acceptance (transient-arg-value "--acceptance=" args))
         (assignee (transient-arg-value "--assignee=" args))
         (design (transient-arg-value "--design=" args))
         (external-ref (transient-arg-value "--external-ref=" args))
         (labels-str (transient-arg-value "--labels=" args))
         (labels (when (and labels-str
                           (not (string-empty-p (string-trim labels-str))))
                   (split-string (string-trim labels-str) "," t "[ \t]+")))
         (force (transient-arg-value "--force" args))
         (parent (transient-arg-value "--parent=" args))
         (repo (transient-arg-value "--repo=" args))
         (from-template (transient-arg-value "--from-template=" args))
         (file (transient-arg-value "--file=" args)))
    (beads-command-create
     :title title
     :issue-type type
     :priority priority
     :description description
     :id custom-id
     :deps dependencies
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
             "^[a-z-]+:[A-Za-z0-9._-]+\\(,[a-z-]+:[A-Za-z0-9._-]+\\)*$"
             dependencies)
      "Dependencies must be in format: type:issue-id (e.g., blocks:bd-a1b2)")))

(defun beads-create--validate-all (cmd)
  "Validate all parameters from CMD beads-command-create instance.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-create--validate-title (oref cmd title))
              (beads-create--validate-type (oref cmd issue-type))
              (beads-create--validate-priority (oref cmd priority))
              (beads-create--validate-dependencies
               ;; Convert deps list back to string format for validation
               (when (oref cmd deps)
                 (string-join (oref cmd deps) ","))))))

;;; Suffix Commands

(transient-define-suffix beads-create--execute ()
  "Execute the bd create command with current parameters."
  :key "c"
  :description "Create issue"
  (interactive)
  (let* ((args (transient-args 'beads-create))
         (cmd (beads-create--parse-transient-args args))
         (errors (beads-create--validate-all cmd)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (let ((issue (beads-command-execute cmd)))
            (message "Created issue: %s - %s"
                     (oref issue id)
                     (oref issue title))
            ;; Invalidate completion cache
            (beads--invalidate-completion-cache)
            ;; Optionally show the created issue in a proper buffer
            (when (y-or-n-p (format "Show issue %s? " (oref issue id)))
              (beads-show (oref issue id)))
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
         (cmd (beads-create--parse-transient-args args))
         (errors (beads-create--validate-all cmd)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                               (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (let* ((cmd-list (beads-command-line cmd))
             (cmd-string (mapconcat #'shell-quote-argument cmd-list " "))
             (preview-msg (format "Command: %s" cmd-string)))
        (message "%s" preview-msg)
        preview-msg))))

;;; Transient Groups

(transient-define-group beads-create--required-section
  [:level 1 "Required"
          (beads-option-issue-title)])

(transient-define-group beads-create--issue-attributes-section
  [:level 2 "Issue attributes"
          (beads-option-issue-type)
          (beads-option-issue-priority)
          (beads-option-issue-assignee)
          (beads-option-issue-labels)])

(transient-define-group beads-create--content-section
  [:level 3 "Content"
          (beads-option-issue-description)
          (beads-option-issue-acceptance)
          (beads-option-issue-design)])

(transient-define-group beads-create--advanced-section
  [:level 4 "Advanced"
          (beads-option-issue-external-ref)
          (beads-option-create-custom-id)
          (beads-option-create-dependencies)
          (beads-option-create-parent)
          (beads-option-create-repo)
          (beads-option-create-from-template)
          (beads-option-create-file)
          (beads-option-create-force)])

;;; Main Transient Menu

;;;###autoload (autoload 'beads-create "beads-create" nil t)
(transient-define-prefix beads-create ()
  "Create a new issue in Beads.

This transient menu provides an interactive interface for setting
all parameters of the bd create command.  Required fields are
validated before execution.

Transient levels control which field groups are visible (cycle with C-x l):
  Level 1: Required (title)
  Level 2: Issue attributes (type, priority, assignee, labels)
  Level 3: Content (description, acceptance, design)  [default]
  Level 4: Advanced (external-ref, custom-id, dependencies, etc.)
  Level 7: Global options (actor, db, json flags, etc.)"
  beads-create--required-section
  beads-create--issue-attributes-section
  beads-create--content-section
  beads-create--advanced-section
  beads-option-global-section
  ["Actions"
   ("x" "Create issue" beads-create--execute)
   ("P" "Preview command" beads-create--preview)
   ("R" "Reset all fields" beads-create--reset)])

(provide 'beads-create)
;;; beads-create.el ends here
