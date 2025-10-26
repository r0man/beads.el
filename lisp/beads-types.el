;;; beads-types.el --- EIEIO types for Beads issue tracker -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; This module defines EIEIO classes for all Beads types, providing
;; an object-oriented interface to beads data structures.
;;
;; The classes mirror the Go structs defined in beads/internal/types
;; and provide:
;; - Type safety through EIEIO class system
;; - Conversion from JSON (bd --json output)
;; - Conversion to alist (for backwards compatibility)
;; - Validation methods
;; - Utility functions for working with beads objects
;;
;; Main classes:
;; - beads-issue: Core issue type
;; - beads-dependency: Issue dependency relationship
;; - beads-label: Issue label/tag
;; - beads-comment: Issue comment
;; - beads-event: Audit trail event
;; - beads-blocked-issue: Issue with blocking information
;; - beads-tree-node: Dependency tree node
;; - beads-statistics: Aggregate metrics
;; - beads-epic-status: Epic completion status
;;
;; Usage:
;;
;;   ;; Create from JSON
;;   (beads-issue-from-json json-data)
;;
;;   ;; Access slots
;;   (oref issue id)
;;   (oref issue title)
;;
;;   ;; Convert to alist (backwards compat)
;;   (beads-issue-to-alist issue)

;;; Code:

(require 'eieio)
(require 'cl-lib)

;;; Constants and Enumerations

;; Status constants
(defconst beads-status-open "open"
  "Issue status: open (not yet started).")

(defconst beads-status-in-progress "in_progress"
  "Issue status: in progress (actively being worked on).")

(defconst beads-status-blocked "blocked"
  "Issue status: blocked (waiting on dependencies).")

(defconst beads-status-closed "closed"
  "Issue status: closed (completed).")

(defconst beads-status-values
  (list beads-status-open
        beads-status-in-progress
        beads-status-blocked
        beads-status-closed)
  "List of all valid status values.")

;; Issue type constants
(defconst beads-type-bug "bug"
  "Issue type: bug fix.")

(defconst beads-type-feature "feature"
  "Issue type: new feature.")

(defconst beads-type-task "task"
  "Issue type: general task.")

(defconst beads-type-epic "epic"
  "Issue type: epic (collection of related issues).")

(defconst beads-type-chore "chore"
  "Issue type: chore (maintenance, refactoring).")

(defconst beads-issue-type-values
  (list beads-type-bug
        beads-type-feature
        beads-type-task
        beads-type-epic
        beads-type-chore)
  "List of all valid issue type values.")

;; Dependency type constants
(defconst beads-dep-blocks "blocks"
  "Dependency type: blocking relationship.")

(defconst beads-dep-related "related"
  "Dependency type: related (non-blocking) relationship.")

(defconst beads-dep-parent-child "parent-child"
  "Dependency type: parent-child (epic) relationship.")

(defconst beads-dep-discovered-from "discovered-from"
  "Dependency type: discovered from another issue.")

(defconst beads-dependency-type-values
  (list beads-dep-blocks
        beads-dep-related
        beads-dep-parent-child
        beads-dep-discovered-from)
  "List of all valid dependency type values.")

;; Event type constants
(defconst beads-event-created "created"
  "Event type: issue created.")

(defconst beads-event-updated "updated"
  "Event type: issue updated.")

(defconst beads-event-status-changed "status_changed"
  "Event type: status changed.")

(defconst beads-event-commented "commented"
  "Event type: comment added.")

(defconst beads-event-closed "closed"
  "Event type: issue closed.")

(defconst beads-event-reopened "reopened"
  "Event type: issue reopened.")

(defconst beads-event-dependency-added "dependency_added"
  "Event type: dependency added.")

(defconst beads-event-dependency-removed "dependency_removed"
  "Event type: dependency removed.")

(defconst beads-event-label-added "label_added"
  "Event type: label added.")

(defconst beads-event-label-removed "label_removed"
  "Event type: label removed.")

(defconst beads-event-compacted "compacted"
  "Event type: issue compacted.")

(defconst beads-event-type-values
  (list beads-event-created
        beads-event-updated
        beads-event-status-changed
        beads-event-commented
        beads-event-closed
        beads-event-reopened
        beads-event-dependency-added
        beads-event-dependency-removed
        beads-event-label-added
        beads-event-label-removed
        beads-event-compacted)
  "List of all valid event type values.")

;; Sort policy constants
(defconst beads-sort-hybrid "hybrid"
  "Sort policy: hybrid (recent by priority, old by age).")

(defconst beads-sort-priority "priority"
  "Sort policy: always by priority.")

(defconst beads-sort-oldest "oldest"
  "Sort policy: always by age (oldest first).")

(defconst beads-sort-policy-values
  (list beads-sort-hybrid
        beads-sort-priority
        beads-sort-oldest)
  "List of all valid sort policy values.")

;;; EIEIO Classes

(defclass beads-issue ()
  ((id
    :initarg :id
    :type string
    :documentation "Unique issue identifier (e.g., 'bd-123').")
   (title
    :initarg :title
    :type string
    :documentation "Issue title (max 500 characters).")
   (description
    :initarg :description
    :type string
    :initform ""
    :documentation "Detailed issue description.")
   (design
    :initarg :design
    :type (or null string)
    :initform nil
    :documentation "Design document or technical approach.")
   (acceptance-criteria
    :initarg :acceptance-criteria
    :type (or null string)
    :initform nil
    :documentation "Acceptance criteria for completion.")
   (notes
    :initarg :notes
    :type (or null string)
    :initform nil
    :documentation "Additional notes or progress updates.")
   (status
    :initarg :status
    :type string
    :documentation "Current status (open, in_progress, blocked, closed).")
   (priority
    :initarg :priority
    :type integer
    :initform 2
    :documentation "Priority level (0=lowest, 4=highest).")
   (issue-type
    :initarg :issue-type
    :type string
    :documentation "Issue type (bug, feature, task, epic, chore).")
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Assigned user.")
   (estimated-minutes
    :initarg :estimated-minutes
    :type (or null integer)
    :initform nil
    :documentation "Estimated time in minutes.")
   (created-at
    :initarg :created-at
    :type string
    :documentation "Creation timestamp (ISO 8601).")
   (updated-at
    :initarg :updated-at
    :type string
    :documentation "Last update timestamp (ISO 8601).")
   (closed-at
    :initarg :closed-at
    :type (or null string)
    :initform nil
    :documentation "Close timestamp (ISO 8601).")
   (external-ref
    :initarg :external-ref
    :type (or null string)
    :initform nil
    :documentation "External reference (e.g., 'gh-9', 'jira-ABC').")
   (compaction-level
    :initarg :compaction-level
    :type integer
    :initform 0
    :documentation "Compaction level (0=uncompacted).")
   (compacted-at
    :initarg :compacted-at
    :type (or null string)
    :initform nil
    :documentation "Compaction timestamp (ISO 8601).")
   (compacted-at-commit
    :initarg :compacted-at-commit
    :type (or null string)
    :initform nil
    :documentation "Git commit hash when compacted.")
   (original-size
    :initarg :original-size
    :type integer
    :initform 0
    :documentation "Original size before compaction.")
   (labels
    :initarg :labels
    :type list
    :initform nil
    :documentation "List of label strings.")
   (dependencies
    :initarg :dependencies
    :type list
    :initform nil
    :documentation "List of beads-dependency objects.")
   (comments
    :initarg :comments
    :type list
    :initform nil
    :documentation "List of beads-comment objects."))
  "Represents a Beads issue (trackable work item).")

(defclass beads-dependency ()
  ((issue-id
    :initarg :issue-id
    :type string
    :documentation "Issue that has the dependency.")
   (depends-on-id
    :initarg :depends-on-id
    :type string
    :documentation "Issue that is depended upon.")
   (type
    :initarg :type
    :type string
    :documentation "Dependency type (blocks, related, parent-child, etc).")
   (created-at
    :initarg :created-at
    :type string
    :documentation "Creation timestamp (ISO 8601).")
   (created-by
    :initarg :created-by
    :type string
    :documentation "User who created the dependency."))
  "Represents a dependency relationship between issues.")

(defclass beads-label ()
  ((issue-id
    :initarg :issue-id
    :type string
    :documentation "Issue ID.")
   (label
    :initarg :label
    :type string
    :documentation "Label text."))
  "Represents a label/tag on an issue.")

(defclass beads-comment ()
  ((id
    :initarg :id
    :type integer
    :documentation "Unique comment ID.")
   (issue-id
    :initarg :issue-id
    :type string
    :documentation "Issue ID.")
   (author
    :initarg :author
    :type string
    :documentation "Comment author.")
   (text
    :initarg :text
    :type string
    :documentation "Comment text.")
   (created-at
    :initarg :created-at
    :type string
    :documentation "Creation timestamp (ISO 8601)."))
  "Represents a comment on an issue.")

(defclass beads-event ()
  ((id
    :initarg :id
    :type integer
    :documentation "Unique event ID.")
   (issue-id
    :initarg :issue-id
    :type string
    :documentation "Issue ID.")
   (event-type
    :initarg :event-type
    :type string
    :documentation "Event type (created, updated, status_changed, etc).")
   (actor
    :initarg :actor
    :type string
    :documentation "User who triggered the event.")
   (old-value
    :initarg :old-value
    :type (or null string)
    :initform nil
    :documentation "Previous value (for updates).")
   (new-value
    :initarg :new-value
    :type (or null string)
    :initform nil
    :documentation "New value (for updates).")
   (comment
    :initarg :comment
    :type (or null string)
    :initform nil
    :documentation "Optional comment text.")
   (created-at
    :initarg :created-at
    :type string
    :documentation "Event timestamp (ISO 8601)."))
  "Represents an audit trail event.")

(defclass beads-blocked-issue (beads-issue)
  ((blocked-by-count
    :initarg :blocked-by-count
    :type integer
    :documentation "Number of blocking issues.")
   (blocked-by
    :initarg :blocked-by
    :type list
    :documentation "List of blocking issue IDs."))
  "Represents an issue with blocking information.")

(defclass beads-tree-node (beads-issue)
  ((depth
    :initarg :depth
    :type integer
    :documentation "Depth in dependency tree.")
   (truncated
    :initarg :truncated
    :type boolean
    :documentation "Whether this node's children are truncated."))
  "Represents a node in a dependency tree.")

(defclass beads-statistics ()
  ((total-issues
    :initarg :total-issues
    :type integer
    :documentation "Total number of issues.")
   (open-issues
    :initarg :open-issues
    :type integer
    :documentation "Number of open issues.")
   (in-progress-issues
    :initarg :in-progress-issues
    :type integer
    :documentation "Number of in-progress issues.")
   (closed-issues
    :initarg :closed-issues
    :type integer
    :documentation "Number of closed issues.")
   (blocked-issues
    :initarg :blocked-issues
    :type integer
    :documentation "Number of blocked issues.")
   (ready-issues
    :initarg :ready-issues
    :type integer
    :documentation "Number of ready issues.")
   (epics-eligible-for-closure
    :initarg :epics-eligible-for-closure
    :type integer
    :documentation "Number of epics eligible for closure.")
   (average-lead-time
    :initarg :average-lead-time
    :type float
    :documentation "Average lead time in hours."))
  "Represents aggregate statistics.")

(defclass beads-epic-status ()
  ((epic
    :initarg :epic
    :type (or null beads-issue)
    :documentation "The epic issue.")
   (total-children
    :initarg :total-children
    :type integer
    :documentation "Total number of child issues.")
   (closed-children
    :initarg :closed-children
    :type integer
    :documentation "Number of closed child issues.")
   (eligible-for-close
    :initarg :eligible-for-close
    :type boolean
    :documentation "Whether epic is eligible for closure."))
  "Represents an epic with completion status.")

(defclass beads-issue-filter ()
  ((status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status.")
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Filter by priority.")
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Filter by issue type.")
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee.")
   (labels
    :initarg :labels
    :type list
    :initform nil
    :documentation "Filter by labels (AND semantics).")
   (labels-any
    :initarg :labels-any
    :type list
    :initform nil
    :documentation "Filter by labels (OR semantics).")
   (title-search
    :initarg :title-search
    :type (or null string)
    :initform nil
    :documentation "Search in title.")
   (ids
    :initarg :ids
    :type list
    :initform nil
    :documentation "Filter by specific issue IDs.")
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Maximum number of results."))
  "Represents filters for issue queries.")

(defclass beads-work-filter ()
  ((status
    :initarg :status
    :type string
    :documentation "Filter by status.")
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Filter by priority.")
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee.")
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Maximum number of results.")
   (sort-policy
    :initarg :sort-policy
    :type (or null string)
    :initform nil
    :documentation "Sort policy (hybrid, priority, oldest)."))
  "Represents filters for ready work queries.")

;;; JSON Conversion Functions

(defun beads-issue-from-json (json)
  "Create a beads-issue object from JSON alist.
JSON should be the parsed JSON object from bd --json output."
  (beads-issue
   :id (alist-get 'id json)
   :title (alist-get 'title json)
   :description (or (alist-get 'description json) "")
   :design (alist-get 'design json)
   :acceptance-criteria (alist-get 'acceptance_criteria json)
   :notes (alist-get 'notes json)
   :status (alist-get 'status json)
   :priority (or (alist-get 'priority json) 2)
   :issue-type (alist-get 'issue_type json)
   :assignee (alist-get 'assignee json)
   :estimated-minutes (alist-get 'estimated_minutes json)
   :created-at (alist-get 'created_at json)
   :updated-at (alist-get 'updated_at json)
   :closed-at (alist-get 'closed_at json)
   :external-ref (alist-get 'external_ref json)
   :compaction-level (or (alist-get 'compaction_level json) 0)
   :compacted-at (alist-get 'compacted_at json)
   :compacted-at-commit (alist-get 'compacted_at_commit json)
   :original-size (or (alist-get 'original_size json) 0)
   :labels (append (alist-get 'labels json) nil)
   :dependencies (when-let ((deps (alist-get 'dependencies json)))
                   (mapcar #'beads-dependency-from-json (append deps nil)))
   :comments (when-let ((comments (alist-get 'comments json)))
               (mapcar #'beads-comment-from-json (append comments nil)))))

(defun beads-dependency-from-json (json)
  "Create a beads-dependency object from JSON alist."
  (beads-dependency
   :issue-id (alist-get 'issue_id json)
   :depends-on-id (alist-get 'depends_on_id json)
   :type (alist-get 'type json)
   :created-at (alist-get 'created_at json)
   :created-by (alist-get 'created_by json)))

(defun beads-label-from-json (json)
  "Create a beads-label object from JSON alist."
  (beads-label
   :issue-id (alist-get 'issue_id json)
   :label (alist-get 'label json)))

(defun beads-comment-from-json (json)
  "Create a beads-comment object from JSON alist."
  (beads-comment
   :id (alist-get 'id json)
   :issue-id (alist-get 'issue_id json)
   :author (alist-get 'author json)
   :text (alist-get 'text json)
   :created-at (alist-get 'created_at json)))

(defun beads-event-from-json (json)
  "Create a beads-event object from JSON alist."
  (beads-event
   :id (alist-get 'id json)
   :issue-id (alist-get 'issue_id json)
   :event-type (alist-get 'event_type json)
   :actor (alist-get 'actor json)
   :old-value (alist-get 'old_value json)
   :new-value (alist-get 'new_value json)
   :comment (alist-get 'comment json)
   :created-at (alist-get 'created_at json)))

(defun beads-blocked-issue-from-json (json)
  "Create a beads-blocked-issue object from JSON alist."
  (beads-blocked-issue
   :id (alist-get 'id json)
   :title (alist-get 'title json)
   :description (or (alist-get 'description json) "")
   :design (alist-get 'design json)
   :acceptance-criteria (alist-get 'acceptance_criteria json)
   :notes (alist-get 'notes json)
   :status (alist-get 'status json)
   :priority (or (alist-get 'priority json) 2)
   :issue-type (alist-get 'issue_type json)
   :assignee (alist-get 'assignee json)
   :estimated-minutes (alist-get 'estimated_minutes json)
   :created-at (alist-get 'created_at json)
   :updated-at (alist-get 'updated_at json)
   :closed-at (alist-get 'closed_at json)
   :external-ref (alist-get 'external_ref json)
   :compaction-level (or (alist-get 'compaction_level json) 0)
   :compacted-at (alist-get 'compacted_at json)
   :compacted-at-commit (alist-get 'compacted_at_commit json)
   :original-size (or (alist-get 'original_size json) 0)
   :labels (append (alist-get 'labels json) nil)
   :dependencies (when-let ((deps (alist-get 'dependencies json)))
                   (mapcar #'beads-dependency-from-json (append deps nil)))
   :comments (when-let ((comments (alist-get 'comments json)))
               (mapcar #'beads-comment-from-json (append comments nil)))
   :blocked-by-count (or (alist-get 'blocked_by_count json) 0)
   :blocked-by (append (alist-get 'blocked_by json) nil)))

(defun beads-tree-node-from-json (json)
  "Create a beads-tree-node object from JSON alist."
  (beads-tree-node
   :id (alist-get 'id json)
   :title (alist-get 'title json)
   :description (or (alist-get 'description json) "")
   :design (alist-get 'design json)
   :acceptance-criteria (alist-get 'acceptance_criteria json)
   :notes (alist-get 'notes json)
   :status (alist-get 'status json)
   :priority (or (alist-get 'priority json) 2)
   :issue-type (alist-get 'issue_type json)
   :assignee (alist-get 'assignee json)
   :estimated-minutes (alist-get 'estimated_minutes json)
   :created-at (alist-get 'created_at json)
   :updated-at (alist-get 'updated_at json)
   :closed-at (alist-get 'closed_at json)
   :external-ref (alist-get 'external_ref json)
   :compaction-level (or (alist-get 'compaction_level json) 0)
   :compacted-at (alist-get 'compacted_at json)
   :compacted-at-commit (alist-get 'compacted_at_commit json)
   :original-size (or (alist-get 'original_size json) 0)
   :labels (append (alist-get 'labels json) nil)
   :dependencies (when-let ((deps (alist-get 'dependencies json)))
                   (mapcar #'beads-dependency-from-json (append deps nil)))
   :comments (when-let ((comments (alist-get 'comments json)))
               (mapcar #'beads-comment-from-json (append comments nil)))
   :depth (or (alist-get 'depth json) 0)
   :truncated (eq (alist-get 'truncated json) t)))

(defun beads-statistics-from-json (json)
  "Create a beads-statistics object from JSON alist."
  (beads-statistics
   :total-issues (or (alist-get 'total_issues json) 0)
   :open-issues (or (alist-get 'open_issues json) 0)
   :in-progress-issues (or (alist-get 'in_progress_issues json) 0)
   :closed-issues (or (alist-get 'closed_issues json) 0)
   :blocked-issues (or (alist-get 'blocked_issues json) 0)
   :ready-issues (or (alist-get 'ready_issues json) 0)
   :epics-eligible-for-closure
   (or (alist-get 'epics_eligible_for_closure json) 0)
   :average-lead-time (or (alist-get 'average_lead_time_hours json) 0.0)))

(defun beads-epic-status-from-json (json)
  "Create a beads-epic-status object from JSON alist."
  (beads-epic-status
   :epic (when-let ((epic-json (alist-get 'epic json)))
           (beads-issue-from-json epic-json))
   :total-children (or (alist-get 'total_children json) 0)
   :closed-children (or (alist-get 'closed_children json) 0)
   :eligible-for-close (eq (alist-get 'eligible_for_close json) t)))

;;; Conversion to Alist (Backwards Compatibility)

(defun beads-issue-to-alist (issue)
  "Convert beads-issue object to alist format.
This provides backwards compatibility with existing code that expects
alists from beads--parse-issue."
  (with-slots (id title description design acceptance-criteria notes
                  status priority issue-type assignee estimated-minutes
                  created-at updated-at closed-at external-ref) issue
    `((id . ,id)
      (title . ,title)
      (description . ,description)
      (design . ,design)
      (acceptance-criteria . ,acceptance-criteria)
      (notes . ,notes)
      (status . ,status)
      (priority . ,priority)
      (issue-type . ,issue-type)
      (assignee . ,assignee)
      (estimated-minutes . ,estimated-minutes)
      (created-at . ,created-at)
      (updated-at . ,updated-at)
      (closed-at . ,closed-at)
      (external-ref . ,external-ref))))

;;; Validation Functions

(defun beads-status-valid-p (status)
  "Return t if STATUS is a valid status value."
  (member status beads-status-values))

(defun beads-issue-type-valid-p (type)
  "Return t if TYPE is a valid issue type value."
  (member type beads-issue-type-values))

(defun beads-dependency-type-valid-p (type)
  "Return t if TYPE is a valid dependency type value."
  (member type beads-dependency-type-values))

(defun beads-event-type-valid-p (type)
  "Return t if TYPE is a valid event type value."
  (member type beads-event-type-values))

(defun beads-sort-policy-valid-p (policy)
  "Return t if POLICY is a valid sort policy value."
  (or (null policy)
      (string-empty-p policy)
      (member policy beads-sort-policy-values)))

(cl-defmethod beads-validate ((issue beads-issue))
  "Validate ISSUE fields and return error string or nil if valid."
  (with-slots (title priority status issue-type estimated-minutes
                     closed-at) issue
    (cond
     ((string-empty-p title)
      "title is required")
     ((> (length title) 500)
      (format "title must be 500 characters or less (got %d)" (length title)))
     ((or (< priority 0) (> priority 4))
      (format "priority must be between 0 and 4 (got %d)" priority))
     ((not (beads-status-valid-p status))
      (format "invalid status: %s" status))
     ((not (beads-issue-type-valid-p issue-type))
      (format "invalid issue type: %s" issue-type))
     ((and estimated-minutes (< estimated-minutes 0))
      "estimated_minutes cannot be negative")
     ((and (string= status beads-status-closed) (not closed-at))
      "closed issues must have closed_at timestamp")
     ((and (not (string= status beads-status-closed)) closed-at)
      "non-closed issues cannot have closed_at timestamp")
     (t nil))))

;;; Utility Functions

(defun beads-issue-closed-p (issue)
  "Return t if ISSUE is closed."
  (string= (oref issue status) beads-status-closed))

(defun beads-issue-open-p (issue)
  "Return t if ISSUE is open."
  (string= (oref issue status) beads-status-open))

(defun beads-issue-in-progress-p (issue)
  "Return t if ISSUE is in progress."
  (string= (oref issue status) beads-status-in-progress))

(defun beads-issue-blocked-p (issue)
  "Return t if ISSUE is blocked."
  (string= (oref issue status) beads-status-blocked))

(defun beads-issue-epic-p (issue)
  "Return t if ISSUE is an epic."
  (string= (oref issue issue-type) beads-type-epic))

(defun beads-issue-has-label-p (issue label)
  "Return t if ISSUE has LABEL."
  (member label (oref issue labels)))

(defun beads-format-timestamp (timestamp)
  "Format TIMESTAMP string for display.
Converts ISO 8601 timestamp to human-readable format."
  (when (and timestamp (not (string-empty-p timestamp)))
    (condition-case nil
        (format-time-string "%Y-%m-%d %H:%M"
                            (date-to-time timestamp))
      (error timestamp))))

(defun beads-format-priority (priority)
  "Format PRIORITY for display."
  (pcase priority
    (0 "P0 (lowest)")
    (1 "P1")
    (2 "P2")
    (3 "P3")
    (4 "P4 (highest)")
    (_ (format "P%d" priority))))

(provide 'beads-types)
;;; beads-types.el ends here
