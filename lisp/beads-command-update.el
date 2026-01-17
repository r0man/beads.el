;;; beads-command-update.el --- Update command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-update' EIEIO class for the
;; `bd update' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd update command updates one or more issues with new field
;; values.  It supports modifying status, priority, title, description,
;; labels, and many other fields.
;;
;; Features:
;; - Update single or multiple issues at once
;; - Status transitions
;; - Label management (add, remove, set)
;; - Due dates and deferral
;; - Atomic claim operation
;; - Time estimates
;;
;; Usage:
;;   (beads-command-execute (beads-command-update :issue-ids '("bd-1")
;;                                                 :status "closed"))
;;   (beads-command-update!)  ; convenience function

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'transient)

;; Forward declaration
(declare-function beads--invalidate-completion-cache "beads")

;;; Update Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-update (beads-command-json)
  (;; Issue IDs
   (issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs to update (positional arguments).
Example: '(\"bd-1\" \"bd-2\").  If not provided, updates the last
touched issue."
    ;; CLI properties
    :positional 1
    :option-type :list
    :option-separator " "
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue IDs"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue ID(s): "
    :transient-reader beads-reader-issue-id
    :transient-group "Issue"
    :transient-level 1
    :transient-order 1)

   ;; Status & Priority
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "New status (-s, --status).
Values: open, in_progress, blocked, closed."
    ;; CLI properties
    :long-option "status"
    :short-option "s"
    :option-type :string
    ;; Transient properties
    :transient-key "s"
    :transient-description "--status"
    :transient-class transient-option
    :transient-argument "--status="
    :transient-prompt "Status: "
    :transient-choices ("open" "in_progress" "blocked" "closed")
    :transient-reader beads-reader-update-status
    :transient-group "Status & Priority"
    :transient-level 1
    :transient-order 1)
   (priority
    :initarg :priority
    :type (or null string integer)
    :initform nil
    :documentation "New priority (-p, --priority).
Values: 0-4 or P0-P4 (0=highest)."
    ;; CLI properties
    :long-option "priority"
    :short-option "p"
    :option-type :string
    ;; Transient properties
    :transient-key "p"
    :transient-description "--priority"
    :transient-class transient-option
    :transient-argument "--priority="
    :transient-prompt "Priority: "
    :transient-reader beads-reader-issue-priority
    :transient-group "Status & Priority"
    :transient-level 1
    :transient-order 2)
   (claim
    :initarg :claim
    :type boolean
    :initform nil
    :documentation "Atomically claim the issue (--claim).
Sets assignee to you and status to in_progress.
Fails if already claimed."
    ;; CLI properties
    :long-option "claim"
    :option-type :boolean
    ;; Transient properties
    :transient-key "c"
    :transient-description "--claim"
    :transient-class transient-switch
    :transient-argument "--claim"
    :transient-group "Status & Priority"
    :transient-level 2
    :transient-order 3)

   ;; Basic Info
   (title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "New title (--title)."
    ;; CLI properties
    :long-option "title"
    :option-type :string
    ;; Transient properties
    :transient-key "t"
    :transient-description "--title"
    :transient-class transient-option
    :transient-argument "--title="
    :transient-prompt "Issue title: "
    :transient-reader beads-reader-issue-title
    :transient-group "Basic Info"
    :transient-level 2
    :transient-order 1)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "New type (-t, --type).
Values: bug, feature, task, epic, chore, merge-request, molecule,
gate, agent, role, rig, convoy, event, slot."
    ;; CLI properties
    :long-option "type"
    :short-option "t"
    :option-type :string
    ;; Transient properties
    :transient-key "t"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Type: "
    :transient-choices ("bug" "feature" "task" "epic" "chore"
                        "merge-request" "molecule" "gate" "agent"
                        "role" "rig" "convoy" "event" "slot")
    :transient-reader beads-reader-issue-type
    :transient-group "Basic Info"
    :transient-level 2
    :transient-order 2)
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "New assignee (-a, --assignee)."
    ;; CLI properties
    :long-option "assignee"
    :short-option "a"
    :option-type :string
    ;; Transient properties
    :transient-key "a"
    :transient-description "--assignee"
    :transient-class transient-option
    :transient-argument "--assignee="
    :transient-prompt "Assignee: "
    :transient-reader beads-reader-issue-assignee
    :transient-group "Basic Info"
    :transient-level 2
    :transient-order 3)
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
    :transient-key "x"
    :transient-description "--external-ref"
    :transient-class transient-option
    :transient-argument "--external-ref="
    :transient-prompt "External reference: "
    :transient-reader beads-reader-issue-external-ref
    :transient-group "Basic Info"
    :transient-level 3
    :transient-order 4)
   (parent
    :initarg :parent
    :type (or null string)
    :initform nil
    :documentation "New parent issue ID (--parent).
Reparents the issue.  Use empty string to remove parent."
    ;; CLI properties
    :long-option "parent"
    :option-type :string
    ;; Transient properties
    :transient-key "P"
    :transient-description "--parent"
    :transient-class transient-option
    :transient-argument "--parent="
    :transient-prompt "Parent issue ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Basic Info"
    :transient-level 3
    :transient-order 5)

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
    :transient-key "d"
    :transient-description "--description"
    :transient-class beads-create-transient-multiline
    :transient-argument "--description="
    :transient-field-name "Description"
    :transient-group "Content"
    :transient-level 3
    :transient-order 1)
   (acceptance
    :initarg :acceptance
    :type (or null string)
    :initform nil
    :documentation "Acceptance criteria (--acceptance)."
    ;; CLI properties
    :long-option "acceptance"
    :option-type :string
    ;; Transient properties
    :transient-key "A"
    :transient-description "--acceptance"
    :transient-class beads-create-transient-multiline
    :transient-argument "--acceptance="
    :transient-field-name "Acceptance Criteria"
    :transient-group "Content"
    :transient-level 3
    :transient-order 2)
   (design
    :initarg :design
    :type (or null string)
    :initform nil
    :documentation "Design notes (--design)."
    ;; CLI properties
    :long-option "design"
    :option-type :string
    ;; Transient properties
    :transient-key "G"
    :transient-description "--design"
    :transient-class beads-create-transient-multiline
    :transient-argument "--design="
    :transient-field-name "Design"
    :transient-group "Content"
    :transient-level 3
    :transient-order 3)
   (notes
    :initarg :notes
    :type (or null string)
    :initform nil
    :documentation "Additional notes (--notes)."
    ;; CLI properties
    :long-option "notes"
    :option-type :string
    ;; Transient properties
    :transient-key "N"
    :transient-description "--notes"
    :transient-class beads-create-transient-multiline
    :transient-argument "--notes="
    :transient-field-name "Notes"
    :transient-group "Content"
    :transient-level 3
    :transient-order 4)
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
    :transient-key "B"
    :transient-description "--body-file"
    :transient-class transient-option
    :transient-argument "--body-file="
    :transient-prompt "Body file: "
    :transient-reader transient-read-file
    :transient-group "Content"
    :transient-level 4
    :transient-order 5)

   ;; Labels
   (add-label
    :initarg :add-label
    :type (or null list)
    :initform nil
    :documentation "Add labels (--add-label).
Repeatable.  Existing labels are preserved."
    ;; CLI properties
    :long-option "add-label"
    :option-type :list
    :option-separator nil  ; Each label is a separate --add-label arg
    ;; Transient properties
    :transient-key "l"
    :transient-description "--add-label"
    :transient-class transient-option
    :transient-argument "--add-label="
    :transient-prompt "Add label: "
    :transient-reader beads-reader-issue-labels
    :transient-group "Labels"
    :transient-level 2
    :transient-order 1)
   (remove-label
    :initarg :remove-label
    :type (or null list)
    :initform nil
    :documentation "Remove labels (--remove-label).
Repeatable."
    ;; CLI properties
    :long-option "remove-label"
    :option-type :list
    :option-separator nil  ; Each label is a separate --remove-label arg
    ;; Transient properties
    :transient-key "L"
    :transient-description "--remove-label"
    :transient-class transient-option
    :transient-argument "--remove-label="
    :transient-prompt "Remove label: "
    :transient-reader beads-reader-issue-labels
    :transient-group "Labels"
    :transient-level 2
    :transient-order 2)
   (set-labels
    :initarg :set-labels
    :type (or null list)
    :initform nil
    :documentation "Set labels, replacing all existing (--set-labels).
Repeatable."
    ;; CLI properties
    :long-option "set-labels"
    :option-type :list
    :option-separator nil  ; Each label is a separate --set-labels arg
    ;; Transient properties
    :transient-key "S"
    :transient-description "--set-labels"
    :transient-class transient-option
    :transient-argument "--set-labels="
    :transient-prompt "Set labels: "
    :transient-reader beads-reader-issue-labels
    :transient-group "Labels"
    :transient-level 3
    :transient-order 3)

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
    :transient-key "e"
    :transient-description "--estimate (minutes)"
    :transient-class transient-option
    :transient-argument "--estimate="
    :transient-prompt "Estimate (minutes): "
    :transient-group "Time"
    :transient-level 3
    :transient-order 1)
   (due
    :initarg :due
    :type (or null string)
    :initform nil
    :documentation "Due date/time (--due).
Use empty to clear.  Formats: +6h, +1d, +2w, tomorrow, next monday,
2025-01-15."
    ;; CLI properties
    :long-option "due"
    :option-type :string
    ;; Transient properties
    :transient-key "D"
    :transient-description "--due"
    :transient-class transient-option
    :transient-argument "--due="
    :transient-prompt "Due date: "
    :transient-group "Time"
    :transient-level 3
    :transient-order 2)
   (defer
    :initarg :defer
    :type (or null string)
    :initform nil
    :documentation "Defer until date (--defer).
Issue hidden from bd ready until then.  Use empty to clear."
    ;; CLI properties
    :long-option "defer"
    :option-type :string
    ;; Transient properties
    :transient-key "E"
    :transient-description "--defer"
    :transient-class transient-option
    :transient-argument "--defer="
    :transient-prompt "Defer until: "
    :transient-group "Time"
    :transient-level 3
    :transient-order 3)

   ;; Advanced
   (await-id
    :initarg :await-id
    :type (or null string)
    :initform nil
    :documentation "Set gate await_id (--await-id).
E.g., GitHub run ID for gh:run gates."
    ;; CLI properties
    :long-option "await-id"
    :option-type :string
    ;; Transient properties
    :transient-key "W"
    :transient-description "--await-id"
    :transient-class transient-option
    :transient-argument "--await-id="
    :transient-prompt "Await ID: "
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 1)
   (session
    :initarg :session
    :type (or null string)
    :initform nil
    :documentation "Claude Code session ID (--session).
For status=closed.  Or set CLAUDE_SESSION_ID env var."
    ;; CLI properties
    :long-option "session"
    :option-type :string
    ;; Transient properties
    :transient-key "I"
    :transient-description "--session"
    :transient-class transient-option
    :transient-argument "--session="
    :transient-prompt "Session ID: "
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 2))
  :documentation "Represents bd update command.
Updates one or more issues with new field values.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-update))
  "Return \"update\" as the CLI subcommand name."
  "update")

(cl-defmethod beads-command-validate ((command beads-command-update))
  "Validate update COMMAND.
Checks that at least one issue ID and one field to update is provided.
Returns error string or nil if valid."
  (with-slots (issue-ids acceptance assignee description design
                         external-ref notes priority status title
                         issue-type parent add-label remove-label
                         set-labels estimate due defer await-id
                         session body-file claim) command
    (or
     ;; Must have at least one issue ID
     (and (or (null issue-ids) (zerop (length issue-ids)))
          "Must provide at least one issue ID")
     ;; Must have at least one field to update
     (and (not (or acceptance assignee description design external-ref
                   notes priority status title issue-type parent
                   add-label remove-label set-labels estimate due
                   defer await-id session body-file claim))
          "Must provide at least one field to update")
     ;; Validate list content types
     (beads-command--validate-string-list issue-ids "issue-ids")
     (beads-command--validate-string-list add-label "add-label")
     (beads-command--validate-string-list remove-label "remove-label")
     (beads-command--validate-string-list set-labels "set-labels"))))

(cl-defmethod beads-command-parse ((command beads-command-update) execution)
  "Parse update COMMAND output from EXECUTION.
Returns updated issue(s).
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, returns beads-issue instance (or list when multiple IDs).
Does not modify any slots."
  (with-slots (json issue-ids) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON, then convert to beads-issue instance(s)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (if (eq (type-of parsed-json) 'vector)
                ;; bd update returns array - convert to issue objects
                (let ((issues (mapcar #'beads-issue-from-json
                                      (append parsed-json nil))))
                  ;; Return single issue if only one ID, list otherwise
                  (if (and issue-ids (= (length issue-ids) 1))
                      (car issues)
                    issues))
              ;; Unexpected JSON structure
              (signal 'beads-json-parse-error
                      (list "Unexpected JSON structure from bd update"
                            :exit-code (oref execution exit-code)
                            :parsed-json parsed-json
                            :stderr (oref execution stderr))))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instance: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :parsed-json parsed-json
                         :stderr (oref execution stderr)
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-update))
  "Execute CMD to update issue and show result.
Overrides default `compilation-mode' behavior."
  (let* ((result (oref (beads-command-execute cmd) result))
         (issues (cond
                  ((null result) nil)
                  ((cl-typep result 'beads-issue) (list result))
                  ((and (listp result)
                        (not (null result))
                        (cl-typep (car result) 'beads-issue))
                   result)
                  (t nil))))
    ;; Invalidate completion cache after updating
    (beads--invalidate-completion-cache)
    (if issues
        (message "Updated %d issue%s: %s"
                 (length issues)
                 (if (= (length issues) 1) "" "s")
                 (mapconcat (lambda (i) (oref i id)) issues ", "))
      (message "No issues updated"))))

;;; Transient Menu - State Management

(defvar beads-update--issue-id nil
  "Issue ID being updated (detected from context).")

(defvar beads-update--original-data nil
  "Original issue data alist (for showing current values in menu).")

;;; Transient Menu - Utility Functions

(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-buffer-parse-show "beads-buffer")
(declare-function beads-completion-read-issue "beads-completion")
(declare-function beads-check-executable "beads")
(declare-function beads-refresh-show "beads-show")
(declare-function beads-command-show! "beads-command-misc")
(defvar beads-show--issue-id)
(defvar beads-auto-refresh)

(defun beads-update--detect-issue-id ()
  "Detect issue ID from current context.
Returns issue ID string or nil if not found."
  (or
   ;; From beads-list buffer
   (when (derived-mode-p 'beads-list-mode)
     (beads-list--current-issue-id))
   ;; From beads-show buffer
   (when (derived-mode-p 'beads-show-mode)
     beads-show--issue-id)
   ;; From buffer name (*beads-show[PROJECT]/ISSUE-ID*)
   (when-let ((parsed (beads-buffer-parse-show (buffer-name))))
     (plist-get parsed :issue-id))))

(defun beads-update--fetch-issue (issue-id)
  "Fetch issue data for ISSUE-ID from bd.
Returns parsed issue alist or signals error."
  (condition-case err
      (beads-command-show! :json t :issue-ids (list issue-id))
    (error
     (beads--error "Failed to fetch issue %s: %s"
                   issue-id
                   (error-message-string err)))))

(defun beads-update--load-issue-data (issue-id)
  "Load issue data for ISSUE-ID and populate state.
Sets beads-update--issue-id and beads-update--original-data."
  (setq beads-update--issue-id issue-id)
  (setq beads-update--original-data
        (beads-update--fetch-issue issue-id)))

(defun beads-update--get-original (field)
  "Get original value of FIELD from original-data.
FIELD is a symbol like `status', `priority', etc.
Original-data is a beads-issue instance."
  (when beads-update--original-data
    (condition-case nil
        (eieio-oref beads-update--original-data field)
      (invalid-slot-name nil))))

(defun beads-update--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-update instance.
Returns a beads-command-update command object with populated slots
from the parsed transient arguments."
  ;; Note: transient 0.12.0 can return `t' instead of "" for empty option
  ;; values.  We use beads--sanitize-string to convert non-string values
  ;; to nil.
  (let* ((status (beads--sanitize-string
                  (transient-arg-value "--status=" args)))
         (priority-str (beads--sanitize-string
                        (transient-arg-value "--priority=" args)))
         (priority (when priority-str (string-to-number priority-str)))
         (title (beads--sanitize-string
                 (transient-arg-value "--title=" args)))
         (description (beads--sanitize-string
                       (transient-arg-value "--description=" args)))
         (acceptance (beads--sanitize-string
                      (transient-arg-value "--acceptance=" args)))
         (design (beads--sanitize-string
                  (transient-arg-value "--design=" args)))
         (notes (beads--sanitize-string
                 (transient-arg-value "--notes=" args)))
         (assignee (beads--sanitize-string
                    (transient-arg-value "--assignee=" args)))
         (external-ref (beads--sanitize-string
                        (transient-arg-value "--external-ref=" args))))
    (beads-command-update
     :json t
     :issue-ids (when beads-update--issue-id
                  (list beads-update--issue-id))
     :status status
     :priority priority
     :title title
     :description description
     :acceptance acceptance
     :design design
     :notes notes
     :assignee assignee
     :external-ref external-ref)))

(defun beads-update--get-changed-fields (cmd)
  "Return alist of fields that have been changed in CMD.
Only includes fields where current value differs from original."
  (let ((changes nil))
    (with-slots (status priority title description acceptance design
                        notes assignee external-ref) cmd
      (when status
        (unless (equal status (beads-update--get-original 'status))
          (push (cons 'status status) changes)))
      (when priority
        (unless (equal priority (beads-update--get-original 'priority))
          (push (cons 'priority priority) changes)))
      (when title
        (unless (equal title (beads-update--get-original 'title))
          (push (cons 'title title) changes)))
      (when description
        (unless (equal description
                       (beads-update--get-original 'description))
          (push (cons 'description description) changes)))
      (when acceptance
        (unless (equal acceptance
                       (beads-update--get-original 'acceptance-criteria))
          (push (cons 'acceptance acceptance) changes)))
      (when design
        (unless (equal design (beads-update--get-original 'design))
          (push (cons 'design design) changes)))
      (when notes
        (unless (equal notes (beads-update--get-original 'notes))
          (push (cons 'notes notes) changes)))
      (when assignee
        (unless (equal assignee (beads-update--get-original 'assignee))
          (push (cons 'assignee assignee) changes)))
      (when external-ref
        (unless (equal external-ref
                       (beads-update--get-original 'external-ref))
          (push (cons 'external-ref external-ref) changes)))
      (nreverse changes))))

(defun beads-update--validate-all (cmd)
  "Validate all parameters from CMD command instance.
Returns list of error messages, or nil if all valid."
  (let ((error-msg (beads-command-validate cmd))
        (changes (beads-update--get-changed-fields cmd)))
    (delq nil
          (list
           error-msg
           (when (null changes) "No fields have been changed")))))

;;; Transient Menu - Suffix Commands

(transient-define-suffix beads-update--execute ()
  "Execute the bd update command with changed parameters."
  :key "x"
  :description "Update issue"
  (interactive)
  (let* ((args (transient-args 'beads-update--menu))
         (cmd (beads-update--parse-transient-args args))
         (errors (beads-update--validate-all cmd))
         (changes (beads-update--get-changed-fields cmd)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (progn
            (let ((_issue (beads-command-execute cmd)))
              (message "Updated issue: %s (changed %d field%s)"
                       beads-update--issue-id
                       (length changes)
                       (if (= (length changes) 1) "" "s"))
              ;; Invalidate completion cache
              (beads--invalidate-completion-cache)
              ;; Refresh any open beads buffers
              (when beads-auto-refresh
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    (cond
                     ((derived-mode-p 'beads-list-mode)
                      (beads-list-refresh))
                     ((and (derived-mode-p 'beads-show-mode)
                           (string= beads-show--issue-id
                                    beads-update--issue-id))
                      (beads-refresh-show))))))
              ;; Reset state
              (setq beads-update--issue-id nil
                    beads-update--original-data nil))
            nil)
        (error
         (let ((err-msg (format "Failed to update issue: %s"
                                (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-update--reset ()
  "Reset all changed parameters to their original values."
  :key "R"
  :description "Reset all changes"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all changes? ")
    ;; Clear transient's argument state using transient-reset
    (transient-reset)
    ;; Refresh the transient display to show cleared state
    (transient--redisplay)
    (message "All changes reset")))

(transient-define-suffix beads-update--preview ()
  "Preview the bd update command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-update--menu))
         (cmd (beads-update--parse-transient-args args))
         (errors (beads-update--validate-all cmd)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                               (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (condition-case err
          (let* ((cmd-line (beads-command-line cmd))
                 (full-cmd (cons beads-executable cmd-line))
                 (cmd-string (mapconcat #'shell-quote-argument full-cmd " "))
                 (changes (beads-update--get-changed-fields cmd))
                 (preview-msg (format "Command: %s\nChanges: %s"
                                      cmd-string
                                      (mapconcat (lambda (c)
                                                   (format "%s=%s"
                                                           (car c)
                                                           (cdr c)))
                                                 changes ", "))))
            (message "%s" preview-msg)
            preview-msg)
        (error
         (let ((err-msg (format "Error: %s" (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

;;; Transient Menu - Main Menu

(transient-define-prefix beads-update--menu ()
  "Transient menu for updating an issue in Beads."
  ["Issue Details"
   ["Status & Priority"
    (beads-option-update-status)
    (beads-option-issue-priority)]
   ["Basic Info"
    (beads-option-issue-title)
    (beads-option-issue-assignee)
    (beads-option-issue-external-ref)]]
  ["Content Fields"
   (beads-option-issue-description)
   (beads-option-issue-acceptance)
   (beads-option-issue-design)
   (beads-option-update-notes-multiline)]
  beads-option-global-section
  ["Actions"
   (beads-update--execute)
   (beads-update--preview)
   (beads-update--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-update (&optional issue-id)
  "Update an existing issue in Beads.

This function provides an interactive interface for updating all fields
of an existing issue via a transient menu.  The function is context-aware
and automatically detects the issue ID from beads-list or beads-show
buffers.

If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user."
  (interactive
   (list (or (beads-update--detect-issue-id)
             (beads-completion-read-issue
              "Update issue: " nil t nil 'beads--issue-id-history))))
  ;; Load issue data before showing menu
  (beads-check-executable)
  (unless issue-id
    (user-error "No issue ID specified"))
  (beads-update--load-issue-data issue-id)
  ;; Show the transient menu
  (beads-update--menu))

;; Auto-generated transient for testing/internal use
(beads-meta-define-transient beads-command-update "beads-update-transient"
  "Update one or more issues (auto-generated menu).

See `beads-update' for the full user-facing transient menu."
  beads-option-global-section)

(provide 'beads-command-update)
;;; beads-command-update.el ends here
