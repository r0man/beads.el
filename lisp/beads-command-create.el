;;; beads-command-create.el --- Create command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-create' EIEIO class for the
;; `bd create' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-defcommand'.
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
;;   (beads-execute 'beads-command-create)  ; convenience function

;;; Code:

(require 'beads-util)
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
    :short-option "t"
    :prompt "Issue title: "
    :reader beads-reader-issue-title
    :group "Required"
    :level 1
    :order 1
    :required t)

   ;; Issue Attributes
   (issue-type
    :long-option "type"
    :short-option "t"
    :type (or null string)
    :transient-key "y"
    :prompt "Type: "
    :choices ("bug" "feature" "task" "epic" "chore"
                        "merge-request" "molecule" "gate" "agent"
                        "role" "rig" "convoy" "event")
    :reader beads-reader-issue-type
    :group "Issue Attributes"
    :level 2
    :order 1)
   (priority
    :type (or null string integer)
    :short-option "p"
    :type (or null string)
    :prompt "Priority: "
    :reader beads-reader-issue-priority
    :group "Issue Attributes"
    :level 2
    :order 2)
   (assignee
    :short-option "a"
    :type (or null string)
    :prompt "Assignee: "
    :reader beads-reader-issue-assignee
    :group "Issue Attributes"
    :level 2
    :order 3)
   (labels
    :short-option "l"
    :type (list-of string)
    :separator ","
    :prompt "Labels (comma-separated): "
    :reader beads-reader-issue-labels
    :group "Issue Attributes"
    :level 2
    :order 4)

   ;; Content
   (description
    :short-option "d"
    :type (or null string)
    :transient beads-transient-multiline
    :documentation "Description"
    :group "Content"
    :level 3
    :order 1)
   (acceptance
    :type (or null string)
    :short-option "A"
    :transient beads-transient-multiline
    :documentation "Acceptance Criteria"
    :group "Content"
    :level 3
    :order 2)
   (design
    :type (or null string)
    :short-option "G"
    :transient beads-transient-multiline
    :documentation "Design"
    :group "Content"
    :level 3
    :order 3)
   (notes
    :type (or null string)
    :short-option "N"
    :transient beads-transient-multiline
    :documentation "Notes"
    :group "Content"
    :level 3
    :order 4)
   (body-file
    :type (or null string)
    :short-option "B"
    :prompt "Body file: "
    :reader transient-read-file
    :group "Content"
    :level 4
    :order 5)

   ;; Time Management
   (estimate
    :short-option "e"
    :type (or null string integer)
    :prompt "Estimate (minutes): "
    :group "Time"
    :level 3
    :order 1)
   (due
    :type (or null string)
    :short-option "D"
    :prompt "Due date: "
    :group "Time"
    :level 3
    :order 2)
   (defer
    :type (or null string)
    :short-option "E"
    :prompt "Defer until: "
    :group "Time"
    :level 3
    :order 3)

   ;; Relationships
   (parent
    :type (or null string)
    :short-option "P"
    :prompt "Parent issue ID: "
    :reader beads-reader-create-parent
    :group "Relationships"
    :level 4
    :order 1)
   (deps
    :type (list-of string)
    :separator ","
    :short-option "R"
    :prompt "Dependencies (type:id,...): "
    :reader beads-reader-create-dependencies
    :group "Relationships"
    :level 4
    :order 2)
   (waits-for
    :type (or null string)
    :short-option "W"
    :prompt "Waits for issue ID: "
    :group "Relationships"
    :level 5
    :order 3)
   (waits-for-gate
    :type (or null string)
    :short-option "g"
    :prompt "Gate type: "
    :choices ("all-children" "any-children")
    :group "Relationships"
    :level 5
    :order 4)

   ;; Advanced Options
   (external-ref
    :type (or null string)
    :short-option "x"
    :prompt "External reference: "
    :reader beads-reader-issue-external-ref
    :group "Advanced"
    :level 4
    :order 1)
   (id
    :type (or null string)
    :short-option "i"
    :prompt "Custom ID: "
    :reader beads-reader-create-custom-id
    :group "Advanced"
    :level 4
    :order 2)
   (prefix-arg
    :long-option "prefix"
    :type (or null string)
    :short-option "r"
    :prompt "Prefix: "
    :group "Advanced"
    :level 4
    :order 3)
   (rig
    :type (or null string)
    :short-option "I"
    :prompt "Rig: "
    :group "Advanced"
    :level 4
    :order 4)
   (repo
    :type (or null string)
    :short-option "o"
    :prompt "Target repository: "
    :reader beads-reader-create-repo
    :group "Advanced"
    :level 4
    :order 5)

   ;; Batch/File Creation
   (file
    :short-option "f"
    :type (or null string)
    :transient-key "F"
    :prompt "Markdown file: "
    :reader beads-reader-create-file
    :group "Batch"
    :level 5
    :order 1)
   (from-template
    :type (or null string)
    :short-option "T"
    :prompt "Template: "
    :reader beads-reader-create-from-template
    :group "Batch"
    :level 5
    :order 2)

   ;; Flags
   (dry-run
    :type boolean
    :short-option "n"
    :group "Flags"
    :level 2
    :order 1)
   (force
    :type boolean
    :short-option "!"
    :group "Flags"
    :level 5
    :order 1)
   (ephemeral
    :type boolean
    :short-option "@"
    :group "Flags"
    :level 5
    :order 2)
   (silent
    :type boolean
    :short-option "s"
    :group "Flags"
    :level 5
    :order 3)
   (validate
    :type boolean
    :short-option "V"
    :group "Flags"
    :level 5
    :order 4)

   ;; Molecule-specific
   (mol-type
    :type (or null string)
    :short-option "mt"
    :prompt "Molecule type: "
    :choices ("swarm" "patrol" "work")
    :group "Molecule"
    :level 6
    :order 1)

   ;; Agent-specific
   (agent-rig
    :type (or null string)
    :short-option "ar"
    :prompt "Agent rig: "
    :group "Agent"
    :level 6
    :order 1)
   (role-type
    :type (or null string)
    :short-option "rt"
    :prompt "Role type: "
    :choices ("polecat" "crew" "witness" "refinery" "mayor" "deacon")
    :group "Agent"
    :level 6
    :order 2)

   ;; Event-specific
   (event-actor
    :type (or null string)
    :short-option "ea"
    :prompt "Event actor: "
    :group "Event"
    :level 6
    :order 1)
   (event-category
    :type (or null string)
    :short-option "ec"
    :prompt "Event category: "
    :group "Event"
    :level 6
    :order 2)
   (event-payload
    :type (or null string)
    :short-option "ep"
    :prompt "Event payload (JSON): "
    :group "Event"
    :level 6
    :order 3)
   (event-target
    :type (or null string)
    :short-option "et"
    :prompt "Event target: "
    :group "Event"
    :level 6
    :order 4)

   ;; Additional flags from CLI
   (append-notes
    :type (or null string)
    :long-option "append-notes"
    :transient beads-transient-multiline
    :documentation "Append Notes"
    :group "Content"
    :level 4
    :order 6)
   (context
    :type (or null string)
    :long-option "context"
    :transient beads-transient-multiline
    :documentation "Context"
    :group "Content"
    :level 4
    :order 7)
   (design-file
    :type (or null string)
    :long-option "design-file"
    :prompt "Design file: "
    :reader transient-read-file
    :group "Content"
    :level 4
    :order 8)
   (graph
    :type (or null string)
    :long-option "graph"
    :prompt "Graph JSON file: "
    :reader transient-read-file
    :group "Batch"
    :level 5
    :order 3)
   (metadata
    :type (or null string)
    :long-option "metadata"
    :prompt "Metadata (JSON or @file.json): "
    :group "Advanced"
    :level 4
    :order 6)
   (no-history
    :type boolean
    :long-option "no-history"
    :short-option "H"
    :group "Flags"
    :level 5
    :order 5)
   (no-inherit-labels
    :type boolean
    :long-option "no-inherit-labels"
    :short-option "N"
    :group "Flags"
    :level 4
    :order 6)
   (skills
    :type (or null string)
    :long-option "skills"
    :prompt "Required skills: "
    :group "Advanced"
    :level 4
    :order 7)
   (spec-id
    :type (or null string)
    :long-option "spec-id"
    :prompt "Spec ID: "
    :group "Advanced"
    :level 4
    :order 8)
   (stdin
    :type boolean
    :long-option "stdin"
    :group "Content"
    :level 5
    :order 9)
   (wisp-type
    :type (or null string)
    :long-option "wisp-type"
    :prompt "Wisp type: "
    :choices ("heartbeat" "ping" "patrol" "gc_report"
              "recovery" "error" "escalation")
    :group "Advanced"
    :level 5
    :order 9))
  :documentation "Represents bd create command.
Creates a new issue (or multiple issues from markdown file).
When executed with :json t, returns the created beads-issue instance(s)."
  :transient :manual)


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

(cl-defmethod beads-command-parse ((command beads-command-create) stdout)
  "Parse create COMMAND output from STDOUT.
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
              (beads-from-json 'beads-issue parsed-json))
             ;; Multiple issues from file (JSON array)
             ((eq (type-of parsed-json) 'vector)
              (mapcar (lambda (j) (beads-from-json 'beads-issue j))
                      (append parsed-json nil)))
             ;; Unexpected JSON structure
             (t
              (signal 'beads-json-parse-error
                      (list "Unexpected JSON structure from bd create"
                            :stdout stdout
                            :parsed-json parsed-json))))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instance: %s"
                                 (error-message-string err))
                         :stdout stdout
                         :parsed-json parsed-json
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-create))
  "Execute CMD to create issue and offer to show it.
Overrides default `compilation-mode' behavior with issue-specific UX."
  (oset cmd json t)
  (let* ((result (beads-command-execute cmd))
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
            (let* ((result (beads-command-execute cmd))
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
