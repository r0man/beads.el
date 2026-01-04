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
  ;; Note: transient 0.12.0 can return `t' instead of "" for empty option values.
  ;; We use beads--sanitize-string to convert non-string values to nil.
  (let* ((title (beads--sanitize-string (transient-arg-value "--title=" args)))
         (type (beads--sanitize-string (transient-arg-value "--type=" args)))
         (priority-str (beads--sanitize-string (transient-arg-value "--priority=" args)))
         (priority (when priority-str (string-to-number priority-str)))
         (description (beads--sanitize-string (transient-arg-value "--description=" args)))
         (custom-id (beads--sanitize-string (transient-arg-value "--id=" args)))
         (dependencies-str (beads--sanitize-string (transient-arg-value "--deps=" args)))
         (dependencies (when dependencies-str
                         (split-string (string-trim dependencies-str) "," t "[ \t]+")))
         (acceptance (beads--sanitize-string (transient-arg-value "--acceptance=" args)))
         (assignee (beads--sanitize-string (transient-arg-value "--assignee=" args)))
         (design (beads--sanitize-string (transient-arg-value "--design=" args)))
         (external-ref (beads--sanitize-string (transient-arg-value "--external-ref=" args)))
         (labels-str (beads--sanitize-string (transient-arg-value "--labels=" args)))
         (labels (when labels-str
                   (split-string (string-trim labels-str) "," t "[ \t]+")))
         (force (transient-arg-value "--force" args))
         (parent (beads--sanitize-string (transient-arg-value "--parent=" args)))
         (repo (beads--sanitize-string (transient-arg-value "--repo=" args)))
         (from-template (beads--sanitize-string (transient-arg-value "--from-template=" args)))
         (file (beads--sanitize-string (transient-arg-value "--file=" args))))
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

;;; Suffix Commands

(transient-define-suffix beads-create--execute ()
  "Execute the bd create command with current parameters."
  :key "x"
  :description "Create issue"
  (interactive)
  (let* ((args (transient-args 'beads-create))
         (cmd (beads-create--parse-transient-args args))
         (error-msg (beads-command-validate cmd)))
    (if error-msg
        (user-error "Validation failed: %s" error-msg)
      (condition-case err
          (let* ((result (oref (beads-command-execute cmd) data))
                 ;; Handle both single-issue and multi-issue responses:
                 ;; - With --title flag: returns one beads-issue object
                 ;; - With --file flag: returns list of beads-issue objects
                 (issues (cond
                          ((null result) nil)
                          ((cl-typep result 'beads-issue) (list result))
                          ((and (listp result)
                                (not (null result))
                                (cl-typep (car result) 'beads-issue))
                           result)
                          (t
                           (error "Unexpected result type from bd create: %S"
                                  result))))
                 (first-issue (car issues)))
            (cond
             ((null first-issue)
              (message "No issues created"))
             ((= (length issues) 1)
              (message "Created issue: %s - %s"
                       (oref first-issue id)
                       (oref first-issue title)))
             (t
              (message "Created %d issues from file (first: %s)"
                       (length issues) (oref first-issue id))))
            ;; Invalidate completion cache (even if no issues, cache state may have changed)
            (beads--invalidate-completion-cache)
            ;; Optionally show the first created issue in a proper buffer
            (when (and first-issue
                       (y-or-n-p (format "Show issue %s? " (oref first-issue id))))
              (beads-show (oref first-issue id)))
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
         (error-msg (beads-command-validate cmd)))
    (if error-msg
        (let ((msg (format "Validation errors: %s" error-msg)))
          (message "%s" msg)
          msg)
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
   (beads-create--execute)
   (beads-create--preview)
   (beads-create--reset)])

(provide 'beads-create)
;;; beads-create.el ends here
