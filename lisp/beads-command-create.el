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
(declare-function beads-show "beads-show")

;;; Create Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
  (beads-defcommand beads-command-create (beads-command-json)
    (;; Required - Title
     (title
      :initarg :title
      :type (or null string)
      :initform nil
      :documentation "Issue title (positional or --title).
  First positional argument or explicit --title flag."
      ;; CLI properties - title is a positional argument
      :positional 1
      ;; Transient properties
      :key "t"
      :transient "Title (required)"
      :class transient-option
      :argument "--title="
      :prompt "Issue title: "
      :reader beads-reader-issue-title
      :transient-group "Required"
      :level 1
      :order 1
      ;; Validation
      :required t)

     ;; Issue Attributes
     (issue-type
      :initarg :issue-type
      :type (or null string)
      :initform nil
      :documentation "Issue type (-t, --type).
  Values: bug, feature, task, epic, chore, merge-request, molecule,
  gate, agent, role, rig, convoy, event. Default: 'task'."
      ;; CLI properties
      :long-option "type"
      :short-option "t"
      :option-type :string
      ;; Transient properties
      :key "t"
      :transient "--type"
      :class transient-option
      :argument "--type="
      :prompt "Type: "
      :choices ("bug" "feature" "task" "epic" "chore"
                          "merge-request" "molecule" "gate" "agent"
                          "role" "rig" "convoy" "event")
      :reader beads-reader-issue-type
      :transient-group "Issue Attributes"
      :level 2
      :order 1)
     (priority
      :initarg :priority
      :type (or null string integer)
      :initform nil
      :documentation "Priority (-p, --priority).
  Values: 0-4 or P0-P4 (0=highest). Default: '2'."
      ;; CLI properties
      :long-option "priority"
      :short-option "p"
      :option-type :string
      ;; Transient properties
      :key "p"
      :transient "--priority"
      :class transient-option
      :argument "--priority="
      :prompt "Priority: "
      :reader beads-reader-issue-priority
      :transient-group "Issue Attributes"
      :level 2
      :order 2)
     (assignee
      :initarg :assignee
      :type (or null string)
      :initform nil
      :documentation "Assignee (-a, --assignee)."
      ;; CLI properties
      :long-option "assignee"
      :short-option "a"
      :option-type :string
      ;; Transient properties
      :key "a"
      :transient "--assignee"
      :class transient-option
      :argument "--assignee="
      :prompt "Assignee: "
      :reader beads-reader-issue-assignee
      :transient-group "Issue Attributes"
      :level 2
      :order 3)
     (labels
      :initarg :labels
      :type (or null list)
      :initform nil
      :documentation "Labels (-l, --labels).
  List of label strings, comma-separated."
      ;; CLI properties
      :long-option "labels"
      :short-option "l"
      :option-type :list
      :option-separator ","
      ;; Transient properties
      :key "l"
      :transient "--labels"
      :class transient-option
      :argument "--labels="
      :prompt "Labels (comma-separated): "
      :reader beads-reader-issue-labels
      :transient-group "Issue Attributes"
      :level 2
      :order 4)

     ;; Content
     (description
      :initarg :description
      :type (or null string)
      :initform nil
      :documentation "Issue description (-d, --description)."
      ;; CLI properties
      :long-option "description"
      :short-option "d"
      :option-type :string
      ;; Transient properties
      :key "d"
      :transient "--description"
      :class beads-transient-multiline
      :argument "--description="
      :field-name "Description"
      :transient-group "Content"
      :level 3
      :order 1)
     (acceptance
      :initarg :acceptance
      :type (or null string)
      :initform nil
      :documentation "Acceptance criteria (--acceptance)."
      ;; CLI properties
      :long-option "acceptance"
      :option-type :string
      ;; Transient properties
      :key "A"
      :transient "--acceptance"
      :class beads-transient-multiline
      :argument "--acceptance="
      :field-name "Acceptance Criteria"
      :transient-group "Content"
      :level 3
      :order 2)
     (design
      :initarg :design
      :type (or null string)
      :initform nil
      :documentation "Design notes (--design)."
      ;; CLI properties
      :long-option "design"
      :option-type :string
      ;; Transient properties
      :key "G"
      :transient "--design"
      :class beads-transient-multiline
      :argument "--design="
      :field-name "Design"
      :transient-group "Content"
      :level 3
      :order 3)
     (notes
      :initarg :notes
      :type (or null string)
      :initform nil
      :documentation "Additional notes (--notes)."
      ;; CLI properties
      :long-option "notes"
      :option-type :string
      ;; Transient properties
      :key "N"
      :transient "--notes"
      :class beads-transient-multiline
      :argument "--notes="
      :field-name "Notes"
      :transient-group "Content"
      :level 3
      :order 4)
     (body-file
      :initarg :body-file
      :type (or null string)
      :initform nil
      :documentation "Read description from file (--body-file).
  Use - for stdin."
      ;; CLI properties
      :long-option "body-file"
      :option-type :string
      ;; Transient properties
      :key "B"
      :transient "--body-file"
      :class transient-option
      :argument "--body-file="
      :prompt "Body file: "
      :reader transient-read-file
      :transient-group "Content"
      :level 4
      :order 5)

     ;; Time Management
     (estimate
      :initarg :estimate
      :type (or null integer)
      :initform nil
      :documentation "Time estimate in minutes (-e, --estimate).
  Example: 60 for 1 hour."
      ;; CLI properties
      :long-option "estimate"
      :short-option "e"
      :option-type :integer
      ;; Transient properties
      :key "e"
      :transient "--estimate (minutes)"
      :class transient-option
      :argument "--estimate="
      :prompt "Estimate (minutes): "
      :transient-group "Time"
      :level 3
      :order 1)
     (due
      :initarg :due
      :type (or null string)
      :initform nil
      :documentation "Due date/time (--due).
  Formats: +6h, +1d, +2w, tomorrow, next monday, 2025-01-15."
      ;; CLI properties
      :long-option "due"
      :option-type :string
      ;; Transient properties
      :key "D"
      :transient "--due"
      :class transient-option
      :argument "--due="
      :prompt "Due date: "
      :transient-group "Time"
      :level 3
      :order 2)
     (defer
      :initarg :defer
      :type (or null string)
      :initform nil
      :documentation "Defer until date (--defer).
  Issue hidden from bd ready until then. Same formats as --due."
      ;; CLI properties
      :long-option "defer"
      :option-type :string
      ;; Transient properties
      :key "E"
      :transient "--defer"
      :class transient-option
      :argument "--defer="
      :prompt "Defer until: "
      :transient-group "Time"
      :level 3
      :order 3)

     ;; Relationships
     (parent
      :initarg :parent
      :type (or null string)
      :initform nil
      :documentation "Parent issue ID for hierarchical child (--parent).
  Example: 'bd-a3f8e9'."
      ;; CLI properties
      :long-option "parent"
      :option-type :string
      ;; Transient properties
      :key "P"
      :transient "--parent"
      :class transient-option
      :argument "--parent="
      :prompt "Parent issue ID: "
      :reader beads-reader-create-parent
      :transient-group "Relationships"
      :level 4
      :order 1)
     (deps
      :initarg :deps
      :type (or null list)
      :initform nil
      :documentation "Dependencies (--deps).
  List of strings in format 'type:id' or 'id'.
  Examples: 'discovered-from:bd-20', 'blocks:bd-15', 'bd-20'."
      ;; CLI properties
      :long-option "deps"
      :option-type :list
      :option-separator ","
      ;; Transient properties
      :key "R"
      :transient "--deps"
      :class transient-option
      :argument "--deps="
      :prompt "Dependencies (type:id,...): "
      :reader beads-reader-create-dependencies
      :transient-group "Relationships"
      :level 4
      :order 2)
     (waits-for
      :initarg :waits-for
      :type (or null string)
      :initform nil
      :documentation "Spawner issue ID to wait for (--waits-for).
  Creates waits-for dependency for fanout gate."
      ;; CLI properties
      :long-option "waits-for"
      :option-type :string
      ;; Transient properties
      :key "W"
      :transient "--waits-for"
      :class transient-option
      :argument "--waits-for="
      :prompt "Waits for issue ID: "
      :transient-group "Relationships"
      :level 5
      :order 3)
     (waits-for-gate
      :initarg :waits-for-gate
      :type (or null string)
      :initform nil
      :documentation "Gate type for waits-for (--waits-for-gate).
  Values: all-children (wait for all) or any-children (wait for first).
  Default: all-children."
      ;; CLI properties
      :long-option "waits-for-gate"
      :option-type :string
      ;; Transient properties
      :key "g"
      :transient "--waits-for-gate"
      :class transient-option
      :argument "--waits-for-gate="
      :prompt "Gate type: "
      :choices ("all-children" "any-children")
      :transient-group "Relationships"
      :level 5
      :order 4)

     ;; Advanced Options
     (external-ref
      :initarg :external-ref
      :type (or null string)
      :initform nil
      :documentation "External reference (--external-ref).
  Examples: 'gh-9', 'jira-ABC'."
      ;; CLI properties
      :long-option "external-ref"
      :option-type :string
      ;; Transient properties
      :key "x"
      :transient "--external-ref"
      :class transient-option
      :argument "--external-ref="
      :prompt "External reference: "
      :reader beads-reader-issue-external-ref
      :transient-group "Advanced"
      :level 4
      :order 1)
     (id
      :initarg :id
      :type (or null string)
      :initform nil
      :documentation "Explicit issue ID (--id).
  Example: 'bd-42' for partitioning."
      ;; CLI properties
      :long-option "id"
      :option-type :string
      ;; Transient properties
      :key "i"
      :transient "--id"
      :class transient-option
      :argument "--id="
      :prompt "Custom ID: "
      :reader beads-reader-create-custom-id
      :transient-group "Advanced"
      :level 4
      :order 2)
     (prefix-arg
      :initarg :prefix-arg
      :type (or null string)
      :initform nil
      :documentation "Create issue in rig by prefix (--prefix).
  Example: --prefix bd- or --prefix bd."
      ;; CLI properties
      :long-option "prefix"
      :option-type :string
      ;; Transient properties
      :key "r"
      :transient "--prefix"
      :class transient-option
      :argument "--prefix="
      :prompt "Prefix: "
      :transient-group "Advanced"
      :level 4
      :order 3)
     (rig
      :initarg :rig
      :type (or null string)
      :initform nil
      :documentation "Create issue in a different rig (--rig).
  Example: --rig beads."
      ;; CLI properties
      :long-option "rig"
      :option-type :string
      ;; Transient properties
      :key "I"
      :transient "--rig"
      :class transient-option
      :argument "--rig="
      :prompt "Rig: "
      :transient-group "Advanced"
      :level 4
      :order 4)
     (repo
      :initarg :repo
      :type (or null string)
      :initform nil
      :documentation "Target repository for issue (--repo).
  Overrides auto-routing."
      ;; CLI properties
      :long-option "repo"
      :option-type :string
      ;; Transient properties
      :key "o"
      :transient "--repo"
      :class transient-option
      :argument "--repo="
      :prompt "Target repository: "
      :reader beads-reader-create-repo
      :transient-group "Advanced"
      :level 4
      :order 5)

     ;; Batch/File Creation
     (file
      :initarg :file
      :type (or null string)
      :initform nil
      :documentation "Create multiple issues from markdown file (-f, --file)."
      ;; CLI properties
      :long-option "file"
      :short-option "f"
      :option-type :string
      ;; Transient properties
      :key "F"
      :transient "--file"
      :class transient-option
      :argument "--file="
      :prompt "Markdown file: "
      :reader beads-reader-create-file
      :transient-group "Batch"
      :level 5
      :order 1)
     (from-template
      :initarg :from-template
      :type (or null string)
      :initform nil
      :documentation "Create issue from template (--from-template).
  Examples: 'epic', 'bug', 'feature'."
      ;; CLI properties
      :long-option "from-template"
      :option-type :string
      ;; Transient properties
      :key "T"
      :transient "--from-template"
      :class transient-option
      :argument "--from-template="
      :prompt "Template: "
      :reader beads-reader-create-from-template
      :transient-group "Batch"
      :level 5
      :order 2)

     ;; Flags
     (dry-run
      :initarg :dry-run
      :type boolean
      :initform nil
      :documentation "Preview what would be created (--dry-run)."
      ;; CLI properties
      :long-option "dry-run"
      :option-type :boolean
      ;; Transient properties
      :key "n"
      :transient "--dry-run"
      :class transient-switch
      :argument "--dry-run"
      :transient-group "Flags"
      :level 2
      :order 1)
     (force
      :initarg :force
      :type boolean
      :initform nil
      :documentation "Force creation even if prefix doesn't match (--force)."
      ;; CLI properties
      :long-option "force"
      :option-type :boolean
      ;; Transient properties
      :key "!"
      :transient "--force"
      :class transient-switch
      :argument "--force"
      :transient-group "Flags"
      :level 5
      :order 1)
     (ephemeral
      :initarg :ephemeral
      :type boolean
      :initform nil
      :documentation "Create as ephemeral, not exported to JSONL (--ephemeral)."
      ;; CLI properties
      :long-option "ephemeral"
      :option-type :boolean
      ;; Transient properties
      :key "@"
      :transient "--ephemeral"
      :class transient-switch
      :argument "--ephemeral"
      :transient-group "Flags"
      :level 5
      :order 2)
     (silent
      :initarg :silent
      :type boolean
      :initform nil
      :documentation "Output only the issue ID (--silent).
  For scripting."
      ;; CLI properties
      :long-option "silent"
      :option-type :boolean
      ;; Transient properties
      :key "s"
      :transient "--silent"
      :class transient-switch
      :argument "--silent"
      :transient-group "Flags"
      :level 5
      :order 3)
     (validate
      :initarg :validate
      :type boolean
      :initform nil
      :documentation "Validate description contains required sections (--validate)."
      ;; CLI properties
      :long-option "validate"
      :option-type :boolean
      ;; Transient properties
      :key "V"
      :transient "--validate"
      :class transient-switch
      :argument "--validate"
      :transient-group "Flags"
      :level 5
      :order 4)

     ;; Molecule-specific
     (mol-type
      :initarg :mol-type
      :type (or null string)
      :initform nil
      :documentation "Molecule type (--mol-type).
  Values: swarm (multi-polecat), patrol (recurring ops), work (default).
  Requires --type=molecule."
      ;; CLI properties
      :long-option "mol-type"
      :option-type :string
      ;; Transient properties
      :key "mt"
      :transient "--mol-type"
      :class transient-option
      :argument "--mol-type="
      :prompt "Molecule type: "
      :choices ("swarm" "patrol" "work")
      :transient-group "Molecule"
      :level 6
      :order 1)

     ;; Agent-specific
     (agent-rig
      :initarg :agent-rig
      :type (or null string)
      :initform nil
      :documentation "Agent's rig name (--agent-rig).
  Requires --type=agent."
      ;; CLI properties
      :long-option "agent-rig"
      :option-type :string
      ;; Transient properties
      :key "ar"
      :transient "--agent-rig"
      :class transient-option
      :argument "--agent-rig="
      :prompt "Agent rig: "
      :transient-group "Agent"
      :level 6
      :order 1)
     (role-type
      :initarg :role-type
      :type (or null string)
      :initform nil
      :documentation "Agent role type (--role-type).
  Values: polecat, crew, witness, refinery, mayor, deacon.
  Requires --type=agent."
      ;; CLI properties
      :long-option "role-type"
      :option-type :string
      ;; Transient properties
      :key "rt"
      :transient "--role-type"
      :class transient-option
      :argument "--role-type="
      :prompt "Role type: "
      :choices ("polecat" "crew" "witness" "refinery" "mayor" "deacon")
      :transient-group "Agent"
      :level 6
      :order 2)

     ;; Event-specific
     (event-actor
      :initarg :event-actor
      :type (or null string)
      :initform nil
      :documentation "Entity URI who caused this event (--event-actor).
  Requires --type=event."
      ;; CLI properties
      :long-option "event-actor"
      :option-type :string
      ;; Transient properties
      :key "ea"
      :transient "--event-actor"
      :class transient-option
      :argument "--event-actor="
      :prompt "Event actor: "
      :transient-group "Event"
      :level 6
      :order 1)
     (event-category
      :initarg :event-category
      :type (or null string)
      :initform nil
      :documentation "Event category (--event-category).
  Examples: patrol.muted, agent.started. Requires --type=event."
      ;; CLI properties
      :long-option "event-category"
      :option-type :string
      ;; Transient properties
      :key "ec"
      :transient "--event-category"
      :class transient-option
      :argument "--event-category="
      :prompt "Event category: "
      :transient-group "Event"
      :level 6
      :order 2)
     (event-payload
      :initarg :event-payload
      :type (or null string)
      :initform nil
      :documentation "Event-specific JSON data (--event-payload).
  Requires --type=event."
      ;; CLI properties
      :long-option "event-payload"
      :option-type :string
      ;; Transient properties
      :key "ep"
      :transient "--event-payload"
      :class transient-option
      :argument "--event-payload="
      :prompt "Event payload (JSON): "
      :transient-group "Event"
      :level 6
      :order 3)
     (event-target
      :initarg :event-target
      :type (or null string)
      :initform nil
      :documentation "Entity URI or bead ID affected (--event-target).
  Requires --type=event."
      ;; CLI properties
      :long-option "event-target"
      :option-type :string
      ;; Transient properties
      :key "et"
      :transient "--event-target"
      :class transient-option
      :argument "--event-target="
      :prompt "Event target: "
      :transient-group "Event"
      :level 6
      :order 4))
    :documentation "Represents bd create command.
  Creates a new issue (or multiple issues from markdown file).
  When executed with :json t, returns the created beads-issue instance(s)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-create))
  "Return \"create\" as the CLI subcommand name."
  "create")

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
                  (string-match-p "^[a-z-]+:[A-Za-z0-9._-]+$" dep))
                deps))
          "Dependencies must be in format: type:issue-id"))))

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
             ;; Single issue (JSON object)
             ((eq (type-of parsed-json) 'cons)
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
