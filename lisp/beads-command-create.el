;;; beads-command-create.el --- Create command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-create' EIEIO class for the
;; `bd create' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd create command creates a new issue (or multiple issues from
;; markdown file).  It supports various issue types including bugs,
;; features, tasks, epics, molecules, gates, agents, and events.
;;
;; Features:
;; - Create single issues with title or multiple from file
;; - Rich metadata: priority, type, labels, assignee
;; - Dependencies and parent relationships
;; - Due dates and deferral
;; - Template-based creation
;; - Advanced options for molecules, agents, and events
;;
;; Usage:
;;   (beads-command-execute (beads-command-create :title "My task"))
;;   (beads-command-create!)  ; convenience function

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'transient)

;; Forward declaration
(declare-function beads-show "beads-command-show")

;;; Create Command

(beads-defcommand beads-command-create (beads-command-global-options)
  ((title
    :positional 1
    :key "t"
    :prompt "Issue title: "
    :transient-reader beads-reader-issue-title
    :transient-group "Required"
    :level 1
    :order 1
    :required t)

   ;; Issue Attributes
   (issue-type
    :long-option "type"
    :short-option "t"
    :option-type :string
    :key "y"
    :prompt "Type: "
    :choices ("bug" "feature" "task" "epic" "chore"
                        "merge-request" "molecule" "gate" "agent"
                        "role" "rig" "convoy" "event")
    :transient-reader beads-reader-issue-type
    :transient-group "Issue Attributes"
    :level 2
    :order 1)
   (priority
    :type (or null string integer)
    :short-option "p"
    :option-type :string
    :key "p"
    :prompt "Priority: "
    :transient-reader beads-reader-issue-priority
    :transient-group "Issue Attributes"
    :level 2
    :order 2)
   (assignee
    :short-option "a"
    :option-type :string
    :key "a"
    :prompt "Assignee: "
    :transient-reader beads-reader-issue-assignee
    :transient-group "Issue Attributes"
    :level 2
    :order 3)
   (labels
    :short-option "l"
    :option-type :list
    :option-separator ","
    :key "l"
    :prompt "Labels (comma-separated): "
    :transient-reader beads-reader-issue-labels
    :transient-group "Issue Attributes"
    :level 2
    :order 4)

   ;; Content
   (description
    :short-option "d"
    :option-type :string
    :key "d"
    :transient beads-transient-multiline
    :field-name "Description"
    :transient-group "Content"
    :level 3
    :order 1)
   (acceptance
    :option-type :string
    :key "A"
    :transient beads-transient-multiline
    :field-name "Acceptance Criteria"
    :transient-group "Content"
    :level 3
    :order 2)
   (design
    :option-type :string
    :key "G"
    :transient beads-transient-multiline
    :field-name "Design"
    :transient-group "Content"
    :level 3
    :order 3)
   (notes
    :option-type :string
    :key "N"
    :transient beads-transient-multiline
    :field-name "Notes"
    :transient-group "Content"
    :level 3
    :order 4)
   (body-file
    :option-type :string
    :key "B"
    :prompt "Body file: "
    :transient-reader transient-read-file
    :transient-group "Content"
    :level 4
    :order 5)

   ;; Time Management
   (estimate
    :short-option "e"
    :option-type :integer
    :key "e"
    :prompt "Estimate (minutes): "
    :transient-group "Time"
    :level 3
    :order 1)
   (due
    :option-type :string
    :key "D"
    :prompt "Due date: "
    :transient-group "Time"
    :level 3
    :order 2)
   (defer
    :option-type :string
    :key "E"
    :prompt "Defer until: "
    :transient-group "Time"
    :level 3
    :order 3)

   ;; Relationships
   (parent
    :option-type :string
    :key "P"
    :prompt "Parent issue ID: "
    :transient-reader beads-reader-create-parent
    :transient-group "Relationships"
    :level 4
    :order 1)
   (deps
    :option-type :list
    :option-separator ","
    :key "R"
    :prompt "Dependencies (type:id,...): "
    :transient-reader beads-reader-create-dependencies
    :transient-group "Relationships"
    :level 4
    :order 2)
   (waits-for
    :option-type :string
    :key "W"
    :prompt "Waits for issue ID: "
    :transient-group "Relationships"
    :level 5
    :order 3)
   (waits-for-gate
    :option-type :string
    :key "g"
    :prompt "Gate type: "
    :choices ("all-children" "any-children")
    :transient-group "Relationships"
    :level 5
    :order 4)

   ;; Advanced Options
   (external-ref
    :option-type :string
    :key "x"
    :prompt "External reference: "
    :transient-reader beads-reader-issue-external-ref
    :transient-group "Advanced"
    :level 4
    :order 1)
   (id
    :option-type :string
    :key "i"
    :prompt "Custom ID: "
    :transient-reader beads-reader-create-custom-id
    :transient-group "Advanced"
    :level 4
    :order 2)
   (prefix-arg
    :long-option "prefix"
    :option-type :string
    :key "r"
    :prompt "Prefix: "
    :transient-group "Advanced"
    :level 4
    :order 3)
   (rig
    :option-type :string
    :key "I"
    :prompt "Rig: "
    :transient-group "Advanced"
    :level 4
    :order 4)
   (repo
    :option-type :string
    :key "o"
    :prompt "Target repository: "
    :transient-reader beads-reader-create-repo
    :transient-group "Advanced"
    :level 4
    :order 5)

   ;; Batch/File Creation
   (file
    :short-option "f"
    :option-type :string
    :key "F"
    :prompt "Markdown file: "
    :transient-reader beads-reader-create-file
    :transient-group "Batch"
    :level 5
    :order 1)
   (from-template
    :option-type :string
    :key "T"
    :prompt "Template: "
    :transient-reader beads-reader-create-from-template
    :transient-group "Batch"
    :level 5
    :order 2)

   ;; Flags
   (dry-run
    :option-type :boolean
    :key "n"
    :transient-group "Flags"
    :level 2
    :order 1)
   (force
    :option-type :boolean
    :key "!"
    :transient-group "Flags"
    :level 5
    :order 1)
   (ephemeral
    :option-type :boolean
    :key "@"
    :transient-group "Flags"
    :level 5
    :order 2)
   (silent
    :option-type :boolean
    :key "s"
    :transient-group "Flags"
    :level 5
    :order 3)
   (validate
    :option-type :boolean
    :key "V"
    :transient-group "Flags"
    :level 5
    :order 4)

   ;; Molecule-specific
   (mol-type
    :option-type :string
    :key "mt"
    :prompt "Molecule type: "
    :choices ("swarm" "patrol" "work")
    :transient-group "Molecule"
    :level 6
    :order 1)

   ;; Agent-specific
   (agent-rig
    :option-type :string
    :key "ar"
    :prompt "Agent rig: "
    :transient-group "Agent"
    :level 6
    :order 1)
   (role-type
    :option-type :string
    :key "rt"
    :prompt "Role type: "
    :choices ("polecat" "crew" "witness" "refinery" "mayor" "deacon")
    :transient-group "Agent"
    :level 6
    :order 2)

   ;; Event-specific
   (event-actor
    :option-type :string
    :key "ea"
    :prompt "Event actor: "
    :transient-group "Event"
    :level 6
    :order 1)
   (event-category
    :option-type :string
    :key "ec"
    :prompt "Event category: "
    :transient-group "Event"
    :level 6
    :order 2)
   (event-payload
    :option-type :string
    :key "ep"
    :prompt "Event payload (JSON): "
    :transient-group "Event"
    :level 6
    :order 3)
   (event-target
    :option-type :string
    :key "et"
    :prompt "Event target: "
    :transient-group "Event"
    :level 6
    :order 4))
  :documentation "Represents bd create command.
Creates a new issue (or multiple issues from markdown file).
When executed with :json t, returns the created beads-issue instance(s).")


(cl-defmethod beads-command-validate ((command beads-command-create))
  "Validate create COMMAND.
Checks for required fields and conflicts between options.
Returns error string or nil if valid."
  (with-slots (title file deps labels issue-type priority) command
    (or
     ;; Must have either title or file
     (and (not title) (not file)
          "Must provide either title or --file")
     ;; Can't use both title and file
     (and title file
          "Cannot use both title and --file")
     ;; Title validation (if provided, cannot be empty)
     (and title (beads--string-blank-p title)
          "Title cannot be empty")
     ;; Type validation
     (and issue-type
          (not (member issue-type '("bug" "feature" "task" "epic" "chore"
                                    "merge-request" "molecule" "gate"
                                    "agent" "role" "rig" "convoy" "event")))
          "Invalid issue type")
     ;; Priority validation (accepts number or string)
     (and priority
          (let ((p (if (stringp priority) (string-to-number priority) priority)))
            (not (and (numberp p) (>= p 0) (<= p 4))))
          "Priority must be a number between 0 and 4")
     ;; Validate list content types
     (beads-command--validate-string-list deps "deps")
     (beads-command--validate-string-list labels "labels")
     ;; Dependency format validation
     (and deps
          (not (seq-every-p
                (lambda (dep)
                  (string-match-p
                   "^\\([a-z-]+:\\)?[A-Za-z0-9._-]+$" dep))
                deps))
          "Dependencies must be in format: type:issue-id or plain issue-id"))))

(cl-defmethod beads-command-parse ((command beads-command-create) execution)
  "Parse create COMMAND output from EXECUTION.
Returns created issue(s).
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, returns beads-issue instance (or list when creating
from file).
Does not modify any slots."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON, then convert to beads-issue instance(s)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (cond
             ;; Single issue (JSON object, including non-empty alists)
             ((and (listp parsed-json) (not (null parsed-json)))
              (beads-issue-from-json parsed-json))
             ;; Multiple issues from file (JSON array)
             ((eq (type-of parsed-json) 'vector)
              (mapcar #'beads-issue-from-json
                      (append parsed-json nil)))
             ;; Unexpected JSON structure
             (t
              (signal 'beads-json-parse-error
                      (list "Unexpected JSON structure from bd create"
                            :exit-code (oref execution exit-code)
                            :parsed-json parsed-json
                            :stderr (oref execution stderr)))))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instance: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :parsed-json parsed-json
                         :stderr (oref execution stderr)
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-create))
  "Execute CMD to create issue and offer to show it.
Overrides default `compilation-mode' behavior with issue-specific UX."
  (oset cmd json t)
  (let* ((result (oref (beads-command-execute cmd) result))
         ;; Handle both single-issue and multi-issue responses
         (issues (cond
                  ((null result) nil)
                  ((cl-typep result 'beads-issue) (list result))
                  ((and (listp result)
                        (not (null result))
                        (cl-typep (car result) 'beads-issue))
                   result)
                  (t nil)))
         (first-issue (car issues)))
    ;; Invalidate completion cache after creating issues
    (beads--invalidate-completion-cache)
    (cond
     ((null first-issue)
      (message "No issues created"))
     ((= (length issues) 1)
      (message "Created issue: %s - %s"
               (oref first-issue id)
               (oref first-issue title))
      (when (y-or-n-p (format "Show issue %s? " (oref first-issue id)))
        (beads-show (oref first-issue id))))
     (t
      (message "Created %d issues from file (first: %s)"
               (length issues) (oref first-issue id))))))

;;; Transient Menu - Parse Function

(defun beads-create--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-create instance.
Returns a beads-command-create object populated with values from ARGS.

This uses transient's standard argument parsing with dash-style flags."
  ;; Note: transient 0.12.0 can return `t' instead of "" for empty option values.
  ;; We use beads--sanitize-string to convert non-string values to nil.
  (let* ((title (beads--sanitize-string (transient-arg-value "--title=" args)))
         (type (beads--sanitize-string (transient-arg-value "--type=" args)))
         (priority-str (beads--sanitize-string
                        (transient-arg-value "--priority=" args)))
         (priority (when priority-str (string-to-number priority-str)))
         (description (beads--sanitize-string
                       (transient-arg-value "--description=" args)))
         (custom-id (beads--sanitize-string (transient-arg-value "--id=" args)))
         (dependencies-str (beads--sanitize-string
                            (transient-arg-value "--deps=" args)))
         (dependencies (when dependencies-str
                         (split-string (string-trim dependencies-str)
                                       "," t "[ \t]+")))
         (acceptance (beads--sanitize-string
                      (transient-arg-value "--acceptance=" args)))
         (assignee (beads--sanitize-string
                    (transient-arg-value "--assignee=" args)))
         (design (beads--sanitize-string
                  (transient-arg-value "--design=" args)))
         (external-ref (beads--sanitize-string
                        (transient-arg-value "--external-ref=" args)))
         (labels-str (beads--sanitize-string
                      (transient-arg-value "--labels=" args)))
         (labels (when labels-str
                   (split-string (string-trim labels-str) "," t "[ \t]+")))
         (force (transient-arg-value "--force" args))
         (parent (beads--sanitize-string
                  (transient-arg-value "--parent=" args)))
         (repo (beads--sanitize-string (transient-arg-value "--repo=" args)))
         (from-template (beads--sanitize-string
                         (transient-arg-value "--from-template=" args)))
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

;;; Transient Menu - Suffix Commands

(transient-define-suffix beads-create--execute ()
  "Execute the bd create command with current parameters."
  :key "x"
  :description "Create issue"
  (interactive)
  (let* ((args (transient-args 'beads-create))
         (cmd (beads-create--parse-transient-args args)))
    (unless cmd
      (user-error "Failed to parse transient arguments"))
    (let ((error-msg (beads-command-validate cmd)))
      (if error-msg
          (user-error "Validation failed: %s" error-msg)
        (oset cmd json t)
        (condition-case err
            (let* ((result (oref (beads-command-execute cmd) result))
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
              ;; Invalidate completion cache
              (beads--invalidate-completion-cache)
              ;; Optionally show the first created issue in a proper buffer
              (when (and first-issue
                         (y-or-n-p (format "Show issue %s? "
                                           (oref first-issue id))))
                (beads-show (oref first-issue id)))
              nil)
          (error
           (let ((err-msg (format "Failed to create issue: %s"
                                  (error-message-string err))))
             (message "%s" err-msg)
             err-msg)))))))

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
         (cmd (beads-create--parse-transient-args args)))
    (if (not cmd)
        (let ((msg "Validation errors: Failed to parse transient arguments"))
          (message "%s" msg)
          msg)
      (let ((error-msg (beads-command-validate cmd)))
        (if error-msg
            (let ((msg (format "Validation errors: %s" error-msg)))
              (message "%s" msg)
              msg)
          (let* ((cmd-list (beads-command-line cmd))
                 (cmd-string (mapconcat #'shell-quote-argument cmd-list " "))
                 (preview-msg (format "Command: %s" cmd-string)))
            (message "%s" preview-msg)
            preview-msg))))))

;;; Transient Menu - Groups

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

;;; Transient Menu - Main Menu

;;;###autoload (autoload 'beads-create "beads-command-create" nil t)
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

;; Auto-generated transient for testing/internal use
(beads-meta-define-transient beads-command-create "beads-create-transient"
  "Create a new issue (auto-generated menu).

See `beads-create' for the full user-facing transient menu."
  beads-option-global-section)

(provide 'beads-command-create)
;;; beads-command-create.el ends here
