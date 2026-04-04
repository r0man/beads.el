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
;;   ;; Read a single issue by ID
;;   (let ((issue (beads-issue-read "bd-a1b2")))
;;     (message "Title: %s" (oref issue title)))
;;
;;   ;; List all issues
;;   (let ((issues (beads-issue-list)))
;;     (dolist (issue issues)
;;       (message "Issue: %s" (oref issue id))))
;;
;;   ;; List issues by status
;;   (beads-issue-list "open")
;;   (beads-issue-list "in_progress")
;;
;;   ;; Get ready work
;;   (beads-issue-ready)
;;   (beads-issue-ready 10)  ; limit to 10 results
;;
;;   ;; List blocked issues
;;   (beads-blocked-issue-list)
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
(require 'json)
(require 'beads-meta)

;; Forward declarations to avoid circular dependencies
(declare-function beads--parse-issue "beads")
(declare-function beads--parse-issues "beads")

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
    :type (or null string)
    :initform nil
    :documentation "Unique issue identifier (e.g., 'bd-a1b2').")
   (content-hash
    :initarg :content-hash
    :type (or null string)
    :initform nil
    :documentation "SHA256 hash of canonical content (for de-duplication).")
   (title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Issue title (max 500 characters).")
   (description
    :initarg :description
    :type (or null string)
    :initform nil
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
    :type (or null string)
    :initform nil
    :documentation "Current status (open, in_progress, blocked, closed).")
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Priority level (0=critical/highest, 4=backlog/lowest).")
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
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
    :type (or null string)
    :initform nil
    :documentation "Creation timestamp (ISO 8601).")
   (updated-at
    :initarg :updated-at
    :type (or null string)
    :initform nil
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
    :type (or null integer)
    :initform nil
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
    :type (or null integer)
    :initform nil
    :documentation "Original size before compaction.")
   (source-repo
    :initarg :source-repo
    :type (or null string)
    :initform nil
    :documentation "Which repo owns this issue (multi-repo support).")
   (created-by
    :initarg :created-by
    :type (or null string)
    :initform nil
    :documentation "User who created the issue (owner).")
   (labels
    :initarg :labels
    :type (list-of string)
    :initform nil
    :documentation "List of label strings.")
   (dependencies
    :initarg :dependencies
    :type (list-of beads-dependency)
    :initform nil
    :documentation "List of beads-dependency objects.")
   (dependents
    :initarg :dependents
    :type (list-of beads-dependency)
    :initform nil
    :documentation "List of beads-dependency objects for issues blocked by this one.")
   (comments
    :initarg :comments
    :type (list-of beads-comment)
    :initform nil
    :documentation "List of beads-comment objects."))
  "Represents a Beads issue (trackable work item).")

(defclass beads-dependency ()
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue that has the dependency.")
   (depends-on-id
    :initarg :depends-on-id
    :type (or null string)
    :initform nil
    :documentation "Issue that is depended upon.")
   (type
    :initarg :type
    :type (or null string)
    :initform nil
    :documentation "Dependency type (blocks, related, parent-child, etc).")
   (created-at
    :initarg :created-at
    :type (or null string)
    :initform nil
    :documentation "Creation timestamp (ISO 8601).")
   (created-by
    :initarg :created-by
    :type (or null string)
    :initform nil
    :documentation "User who created the dependency.")
   ;; Additional fields from IssueWithDependencyMetadata (bd show --json)
   ;; These are populated when the dependency comes from bd show output
   (title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Title of the depended-upon issue (from bd show).")
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Status of the depended-upon issue (from bd show).")
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Priority of the depended-upon issue (from bd show).")
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Type of the depended-upon issue (from bd show)."))
  "Represents a dependency relationship between issues.
When populated from bd show --json, includes full issue details.")

(defclass beads-label ()
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID.")
   (label
    :initarg :label
    :type (or null string)
    :initform nil
    :documentation "Label text."))
  "Represents a label/tag on an issue.")

(defclass beads-comment ()
  ((id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Unique comment ID (UUID string).")
   (issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID.")
   (author
    :initarg :author
    :type (or null string)
    :initform nil
    :documentation "Comment author.")
   (text
    :initarg :text
    :type (or null string)
    :initform nil
    :documentation "Comment text.")
   (created-at
    :initarg :created-at
    :type (or null string)
    :initform nil
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
    :type (list-of string)
    :documentation "List of blocking issue IDs."))
  "Represents an issue with blocking information.")

(defclass beads-tree-node (beads-issue)
  ((depth
    :initarg :depth
    :type integer
    :documentation "Depth in dependency tree.")
   (parent-id
    :initarg :parent-id
    :type (or null string)
    :initform nil
    :documentation "Parent issue ID in dependency tree.")
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
   (deferred-issues
    :initarg :deferred-issues
    :type integer
    :initform 0
    :documentation "Number of deferred issues.")
   (ready-issues
    :initarg :ready-issues
    :type integer
    :documentation "Number of ready issues.")
   (tombstone-issues
    :initarg :tombstone-issues
    :type integer
    :initform 0
    :documentation "Number of tombstone (deleted) issues.")
   (pinned-issues
    :initarg :pinned-issues
    :type integer
    :initform 0
    :documentation "Number of pinned issues.")
   (epics-eligible-for-closure
    :initarg :epics-eligible-for-closure
    :type integer
    :documentation "Number of epics eligible for closure.")
   (average-lead-time
    :initarg :average-lead-time
    :type float
    :json-key average_lead_time_hours
    :documentation "Average lead time in hours."))
  "Represents aggregate statistics.")

(defclass beads-recent-activity ()
  ((hours-tracked
    :initarg :hours-tracked
    :type integer
    :initform 24
    :documentation "Number of hours tracked for recent activity.")
   (commit-count
    :initarg :commit-count
    :type integer
    :initform 0
    :documentation "Number of commits in the tracking period.")
   (issues-created
    :initarg :issues-created
    :type integer
    :initform 0
    :documentation "Number of issues created.")
   (issues-closed
    :initarg :issues-closed
    :type integer
    :initform 0
    :documentation "Number of issues closed.")
   (issues-updated
    :initarg :issues-updated
    :type integer
    :initform 0
    :documentation "Number of issues updated.")
   (issues-reopened
    :initarg :issues-reopened
    :type integer
    :initform 0
    :documentation "Number of issues reopened.")
   (total-changes
    :initarg :total-changes
    :type integer
    :initform 0
    :documentation "Total number of changes."))
  "Represents recent activity metrics.")

(defclass beads-stats-data ()
  ((summary
    :initarg :summary
    :type beads-statistics
    :documentation "Summary statistics object.")
   (recent-activity
    :initarg :recent-activity
    :type (or null beads-recent-activity)
    :initform nil
    :documentation "Recent activity metrics (may be nil)."))
  "Container for stats command output with nested structure.")

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
    :type (list-of string)
    :initform nil
    :documentation "Filter by labels (AND semantics).")
   (labels-any
    :initarg :labels-any
    :type (list-of string)
    :initform nil
    :documentation "Filter by labels (OR semantics).")
   (title-search
    :initarg :title-search
    :type (or null string)
    :initform nil
    :documentation "Search in title.")
   (ids
    :initarg :ids
    :type (list-of string)
    :initform nil
    :documentation "Filter by specific issue IDs (comma-separated string).")
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Maximum number of results.")
   (title-contains
    :initarg :title-contains
    :type (or null string)
    :initform nil
    :documentation "Pattern matching in title (case-insensitive).")
   (description-contains
    :initarg :description-contains
    :type (or null string)
    :initform nil
    :documentation "Pattern matching in description (case-insensitive).")
   (notes-contains
    :initarg :notes-contains
    :type (or null string)
    :initform nil
    :documentation "Pattern matching in notes (case-insensitive).")
   (created-after
    :initarg :created-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues created after date (YYYY-MM-DD or RFC3339).")
   (created-before
    :initarg :created-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues created before date (YYYY-MM-DD or RFC3339).")
   (updated-after
    :initarg :updated-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated after date (YYYY-MM-DD or RFC3339).")
   (updated-before
    :initarg :updated-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated before date (YYYY-MM-DD or RFC3339).")
   (closed-after
    :initarg :closed-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed after date (YYYY-MM-DD or RFC3339).")
   (closed-before
    :initarg :closed-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed before date (YYYY-MM-DD or RFC3339).")
   (empty-description
    :initarg :empty-description
    :type boolean
    :initform nil
    :documentation "Filter issues with empty or missing description.")
   (no-assignee
    :initarg :no-assignee
    :type boolean
    :initform nil
    :documentation "Filter issues with no assignee.")
   (no-labels
    :initarg :no-labels
    :type boolean
    :initform nil
    :documentation "Filter issues with no labels.")
   (priority-min
    :initarg :priority-min
    :type (or null integer)
    :initform nil
    :documentation "Filter by minimum priority (inclusive).")
   (priority-max
    :initarg :priority-max
    :type (or null integer)
    :initform nil
    :documentation "Filter by maximum priority (inclusive).")
   (all
    :initarg :all
    :type boolean
    :initform nil
    :documentation "Show all issues (default behavior flag).")
   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Output format: digraph, dot, or Go template.")
   (long
    :initarg :long
    :type boolean
    :initform nil
    :documentation "Show detailed multi-line output for each issue."))
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
   (labels
    :initarg :labels
    :type (list-of string)
    :initform nil
    :documentation "Filter by labels (AND semantics).")
   (labels-any
    :initarg :labels-any
    :type (list-of string)
    :initform nil
    :documentation "Filter by labels (OR semantics).")
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
JSON should be the parsed JSON object from bd --json output.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-issue json))

(defun beads-dependency-from-json (json)
  "Create a beads-dependency object from JSON alist.
Delegates to `beads-from-json' which handles polymorphic keys."
  (beads-from-json 'beads-dependency json))

(cl-defmethod beads-from-json ((_class (eql 'beads-dependency)) json)
  "Construct beads-dependency from polymorphic JSON.
JSON can be either simple dependency format (issue_id, depends_on_id,
type) or IssueWithDependencyMetadata (id, dependency_type, plus full
issue fields).  Handles both key variants."
  (beads-dependency
   :issue-id (alist-get 'issue_id json)
   :depends-on-id (or (alist-get 'depends_on_id json)
                      (alist-get 'id json))
   :type (or (alist-get 'dependency_type json)
             (alist-get 'type json))
   :created-at (alist-get 'created_at json)
   :created-by (alist-get 'created_by json)
   :title (alist-get 'title json)
   :status (alist-get 'status json)
   :priority (alist-get 'priority json)
   :issue-type (alist-get 'issue_type json)))

(defun beads-label-from-json (json)
  "Create a beads-label object from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-label json))

(defun beads-comment-from-json (json)
  "Create a beads-comment object from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-comment json))

(defun beads-event-from-json (json)
  "Create a beads-event object from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-event json))

(defun beads-blocked-issue-from-json (json)
  "Create a beads-blocked-issue object from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-blocked-issue json))

(defun beads-tree-node-from-json (json)
  "Create a beads-tree-node object from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-tree-node json))

(defun beads-statistics-from-json (json)
  "Create a beads-statistics object from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-statistics json))

(defun beads-recent-activity-from-json (json)
  "Create a beads-recent-activity object from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-recent-activity json))

(cl-defmethod beads-from-json ((_class (eql 'beads-stats-data)) json)
  "Construct beads-stats-data from nested or flat JSON.
Falls back to treating the entire JSON as a statistics object
if the `summary' key is absent (backwards compatibility)."
  (let ((summary-json (alist-get 'summary json))
        (activity-json (alist-get 'recent_activity json)))
    (beads-stats-data
     :summary (beads-from-json 'beads-statistics
                               (or summary-json json))
     :recent-activity (when activity-json
                        (beads-from-json 'beads-recent-activity
                                         activity-json)))))

(defun beads-stats-data-from-json (json)
  "Create a beads-stats-data object from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-stats-data json))

(defun beads-epic-status-from-json (json)
  "Create a beads-epic-status object from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-epic-status json))

;;; beads-issue-filter Methods

(cl-defmethod beads-issue-filter-to-args ((filter beads-issue-filter))
  "Build bd command arguments from FILTER instance.
Returns a list of strings suitable for use with beads-command classes.
Only includes arguments for non-nil filter slots."
  (with-slots (status priority issue-type assignee labels labels-any
                      title-search ids limit title-contains
                      description-contains notes-contains created-after
                      created-before updated-after updated-before
                      closed-after closed-before empty-description
                      no-assignee no-labels priority-min priority-max
                      all format long) filter
    (let (args)
      ;; Boolean flags
      (when all (push "--all" args))
      (when no-assignee (push "--no-assignee" args))
      (when empty-description (push "--empty-description" args))
      (when no-labels (push "--no-labels" args))
      (when long (push "--long" args))
      ;; String filters
      (when assignee
        (push "--assignee" args)
        (push assignee args))
      (when closed-after
        (push "--closed-after" args)
        (push closed-after args))
      (when closed-before
        (push "--closed-before" args)
        (push closed-before args))
      (when created-after
        (push "--created-after" args)
        (push created-after args))
      (when created-before
        (push "--created-before" args)
        (push created-before args))
      (when description-contains
        (push "--desc-contains" args)
        (push description-contains args))
      (when format
        (push "--format" args)
        (push format args))
      (when ids
        (push "--id" args)
        (push ids args))
      (when notes-contains
        (push "--notes-contains" args)
        (push notes-contains args))
      (when status
        (push "--status" args)
        (push status args))
      (when title-search
        (push "--title" args)
        (push title-search args))
      (when title-contains
        (push "--title-contains" args)
        (push title-contains args))
      (when issue-type
        (push "--type" args)
        (push issue-type args))
      (when updated-after
        (push "--updated-after" args)
        (push updated-after args))
      (when updated-before
        (push "--updated-before" args)
        (push updated-before args))
      ;; List filters (repeatable flags)
      (when labels
        (dolist (label labels)
          (push "--label" args)
          (push label args)))
      (when labels-any
        (dolist (label labels-any)
          (push "--label-any" args)
          (push label args)))
      ;; Numeric filters
      (when limit
        (push "--limit" args)
        (push (number-to-string limit) args))
      (when priority
        (push "--priority" args)
        (push (number-to-string priority) args))
      (when priority-min
        (push "--priority-min" args)
        (push (number-to-string priority-min) args))
      (when priority-max
        (push "--priority-max" args)
        (push (number-to-string priority-max) args))
      (nreverse args))))

(cl-defmethod beads-issue-filter-is-empty ((filter beads-issue-filter))
  "Return t if FILTER has no active filters, nil otherwise."
  (null (beads-issue-filter-to-args filter)))

;;; Conversion to Alist (Backwards Compatibility)

(defun beads-issue-to-alist (issue)
  "Convert ISSUE (beads-issue object) to alist format.
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

;;; Dependency Operation Result

(defclass beads-dep-op-result ()
  ((op-status
    :initarg :op-status
    :type (or null string)
    :initform nil
    :json-key status
    :documentation "Operation status (\"added\" or \"removed\").")
   (issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue that has/had the dependency.")
   (depends-on-id
    :initarg :depends-on-id
    :type (or null string)
    :initform nil
    :documentation "Issue that is/was depended upon.")
   (dep-type
    :initarg :dep-type
    :type (or null string)
    :initform nil
    :json-key type
    :documentation "Dependency type (blocks, related, etc.)."))
  "Result of a bd dep add or dep remove operation.")

(defun beads-dep-op-result-from-json (json)
  "Create a beads-dep-op-result from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-dep-op-result json))

;;; Label Count (for bd label list-all)

(defclass beads-label-count ()
  ((label
    :initarg :label
    :type (or null string)
    :initform nil
    :documentation "Label text.")
   (count
    :initarg :count
    :type (or null integer)
    :initform nil
    :documentation "Number of issues with this label."))
  "A label with its usage count from bd label list-all.")

(defun beads-label-count-from-json (json)
  "Create a beads-label-count from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-label-count json))

;;; Formula Types (for bd formula commands)

(defclass beads-formula-var ()
  ((name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Variable name (key in vars map).")
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "What this variable is for.")
   (default
    :initarg :default
    :type (or null string)
    :initform nil
    :documentation "Default value if not provided.")
   (required
    :initarg :required
    :type boolean
    :initform nil
    :documentation "Whether the variable must be provided.")
   (enum
    :initarg :enum
    :type (list-of string)
    :initform nil
    :documentation "List of allowed values (if non-empty).")
   (pattern
    :initarg :pattern
    :type (or null string)
    :initform nil
    :documentation "Regex pattern the value must match.")
   (var-type
    :initarg :var-type
    :type (or null string)
    :initform nil
    :json-key type
    :documentation "Expected value type: string (default), int, bool."))
  "A formula variable definition (VarDef from formula package).")

(defun beads-formula-var-from-json (name json)
  "Create a beads-formula-var from NAME and JSON alist.
Delegates to `beads-from-json' after injecting name into JSON."
  (let ((json-with-name (cons (cons 'name (if (symbolp name) (symbol-name name) name)) json)))
    (beads-from-json 'beads-formula-var json-with-name)))

(defclass beads-formula-step ()
  ((id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Step ID, unique within the formula.")
   (title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Step title (issue title template).")
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Step description (issue description template).")
   (notes
    :initarg :notes
    :type (or null string)
    :initform nil
    :documentation "Additional notes for the step.")
   (needs
    :initarg :needs
    :type (list-of string)
    :initform nil
    :documentation "List of sibling step IDs that must complete first.")
   (depends-on
    :initarg :depends-on
    :type (list-of string)
    :initform nil
    :documentation "List of step IDs this step blocks on (alias for needs)."))
  "A formula step definition (Step from formula package).")

(defun beads-formula-step-from-json (json)
  "Create a beads-formula-step from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-formula-step json))

(defclass beads-formula-summary ()
  ((name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Formula name (e.g., \"emacs-lisp-dev\").")
   (formula-type
    :initarg :formula-type
    :type (or null string)
    :initform nil
    :json-key type
    :documentation "Formula type (workflow, expansion, aspect).")
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Brief description (may be truncated in list view).")
   (source
    :initarg :source
    :type (or null string)
    :initform nil
    :documentation "File path to the formula source.")
   (steps
    :initarg :steps
    :type (or null integer)
    :initform nil
    :documentation "Number of steps in the formula.")
   (vars
    :initarg :vars
    :type (or null integer)
    :initform nil
    :documentation "Number of variables in the formula."))
  :documentation "Summary of a formula for list display.")

(defun beads-formula-summary-from-json (json)
  "Create a beads-formula-summary from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-formula-summary json))

(defclass beads-formula ()
  ((name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Formula name.")
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Full description of the formula.")
   (version
    :initarg :version
    :type (or null integer)
    :initform nil
    :documentation "Formula version number.")
   (formula-type
    :initarg :formula-type
    :type (or null string)
    :initform nil
    :json-key type
    :documentation "Formula type (workflow, expansion, aspect).")
   (vars
    :initarg :vars
    :type (list-of beads-formula-var)
    :initform nil
    :documentation "Variables as a list of beads-formula-var objects.")
   (steps
    :initarg :steps
    :type (list-of beads-formula-step)
    :initform nil
    :documentation "List of step definitions as beads-formula-step objects.")
   (source
    :initarg :source
    :type (or null string)
    :initform nil
    :documentation "File path to the formula source."))
  :documentation "Full formula details from bd formula show.")

(defun beads-formula-from-json (json)
  "Create a beads-formula from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-formula json))

(cl-defmethod beads-from-json ((_class (eql 'beads-formula)) json)
  "Construct beads-formula from non-standard JSON.
The formula name is in the `formula' key (not `name'), and `vars'
is a map {name: def-alist, ...} rather than an array."
  (let* ((raw-vars (alist-get 'vars json))
         (vars (mapcar (lambda (entry)
                         (let ((var-json (cdr entry)))
                           (push (cons 'name (if (symbolp (car entry))
                                                 (symbol-name (car entry))
                                               (car entry)))
                                 var-json)
                           (beads-from-json 'beads-formula-var var-json)))
                       raw-vars))
         (raw-steps (alist-get 'steps json))
         (steps (mapcar (lambda (j)
                          (beads-from-json 'beads-formula-step j))
                        (append raw-steps nil))))
    (beads-formula
     :name (alist-get 'formula json)
     :description (alist-get 'description json)
     :version (alist-get 'version json)
     :formula-type (alist-get 'type json)
     :vars vars
     :steps steps
     :source (alist-get 'source json))))

;;; ============================================================
;;; Worktree Types
;;; ============================================================

(defclass beads-worktree ()
  ((name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Worktree name (directory name).")
   (path
    :initarg :path
    :type (or null string)
    :initform nil
    :documentation "Absolute path to the worktree directory.")
   (branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Git branch checked out in this worktree.")
   (is-main
    :initarg :is-main
    :type boolean
    :initform nil
    :documentation "Whether this is the main worktree (not a linked worktree).")
   (beads-state
    :initarg :beads-state
    :type (or null string)
    :initform nil
    :documentation "Beads configuration state.
Possible values:
- \"shared\": Main repository with .beads directory
- \"redirect\": Worktree with redirect to main .beads
- \"local\": Has its own .beads (not recommended)
- \"none\": No beads configuration"))
  :documentation "Represents a git worktree with beads configuration.")

(defun beads-worktree-from-json (json)
  "Create a beads-worktree instance from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-worktree json))

(cl-defmethod beads-from-json ((_class (eql 'beads-worktree)) json)
  "Construct beads-worktree from JSON.
Derives `name' from `path' when the name key is absent (e.g. in
bd worktree create output)."
  (let* ((wt (cl-call-next-method))
         (name (oref wt name))
         (path (oref wt path)))
    (unless name
      (when path
        (oset wt name (file-name-nondirectory
                       (directory-file-name path)))))
    wt))

(defclass beads-worktree-info ()
  ((is-worktree
    :initarg :is-worktree
    :type boolean
    :initform nil
    :documentation "Whether current directory is in a worktree.")
   (name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Worktree name (if in a worktree).")
   (path
    :initarg :path
    :type (or null string)
    :initform nil
    :documentation "Worktree path (if in a worktree).")
   (branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Branch name (if in a worktree).")
   (main-path
    :initarg :main-path
    :type (or null string)
    :initform nil
    :documentation "Path to main repository (if in a linked worktree).")
   (beads-state
    :initarg :beads-state
    :type (or null string)
    :initform nil
    :documentation "Beads configuration state."))
  :documentation "Information about the current worktree context.")

(defun beads-worktree-info-from-json (json)
  "Create a beads-worktree-info from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-worktree-info json))

;;; ============================================================
;;; Audit Types
;;; ============================================================

(defclass beads-audit-entry ()
  ((id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Unique audit entry ID.")
   (kind
    :initarg :kind
    :type (or null string)
    :initform nil
    :documentation "Entry type (llm_call, tool_call, label)."))
  "Result of a bd audit record command.")

(defun beads-audit-entry-from-json (json)
  "Create a beads-audit-entry from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-audit-entry json))

(defclass beads-audit-label-result ()
  ((id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "New label entry ID.")
   (parent-id
    :initarg :parent-id
    :type (or null string)
    :initform nil
    :documentation "ID of the interaction being labeled.")
   (label
    :initarg :label
    :type (or null string)
    :initform nil
    :documentation "Label value (good or bad)."))
  "Result of a bd audit label command.")

(defun beads-audit-label-result-from-json (json)
  "Create a beads-audit-label-result from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-audit-label-result json))

;;; ============================================================
;;; Config Types
;;; ============================================================

(defclass beads-config-entry ()
  ((key
    :initarg :key
    :type (or null string)
    :initform nil
    :documentation "Configuration key.")
   (value
    :initarg :value
    :type (or null string)
    :initform nil
    :documentation "Configuration value.")
   (location
    :initarg :location
    :type (or null string)
    :initform nil
    :documentation "Storage location (config.yaml, git config, etc)."))
  "A single configuration entry from bd config get/set.")

(defun beads-config-entry-from-json (json)
  "Create a beads-config-entry from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-config-entry json))

;;; ============================================================
;;; Count Types
;;; ============================================================

(defclass beads-count-result ()
  ((count
    :initarg :count
    :type integer
    :initform 0
    :documentation "Total count of matching issues."))
  "Result of a bd count command (ungrouped).")

(defun beads-count-result-from-json (json)
  "Create a beads-count-result from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-count-result json))

(defclass beads-count-group ()
  ((group
    :initarg :group
    :type (or null string)
    :initform nil
    :documentation "Group key (status, priority, type, assignee, or label).")
   (count
    :initarg :count
    :type integer
    :initform 0
    :documentation "Count for this group."))
  "A single group in a bd count --by-* result.")

(defun beads-count-group-from-json (json)
  "Create a beads-count-group from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-count-group json))

(defclass beads-count-grouped-result ()
  ((total
    :initarg :total
    :type integer
    :initform 0
    :documentation "Sum of all group counts.")
   (groups
    :initarg :groups
    :type (list-of beads-count-group)
    :initform nil
    :documentation "List of group/count pairs."))
  "Result of a bd count --by-* command (grouped).")

(defun beads-count-grouped-result-from-json (json)
  "Create a beads-count-grouped-result from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-count-grouped-result json))

;;; ============================================================
;;; Compact Types
;;; ============================================================

(defclass beads-compact-tier-stats ()
  ((candidates
    :initarg :candidates
    :type integer
    :initform 0
    :documentation "Number of compaction candidates.")
   (total-size
    :initarg :total-size
    :type integer
    :initform 0
    :documentation "Total size in bytes."))
  "Stats for a single compaction tier.")

(defun beads-compact-tier-stats-from-json (json)
  "Create a beads-compact-tier-stats from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-compact-tier-stats json))

(defclass beads-compact-stats ()
  ((tier1
    :initarg :tier1
    :type beads-compact-tier-stats
    :documentation "Tier 1 compaction stats.")
   (tier2
    :initarg :tier2
    :type beads-compact-tier-stats
    :documentation "Tier 2 compaction stats."))
  "Result of bd admin compact --stats.")

(defun beads-compact-stats-from-json (json)
  "Create a beads-compact-stats from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-compact-stats json))

(defclass beads-compact-candidate ()
  ((id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Issue ID.")
   (title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Issue title.")
   (size-bytes
    :initarg :size-bytes
    :type integer
    :initform 0
    :documentation "Total size of compactable fields in bytes.")
   (age-days
    :initarg :age-days
    :type integer
    :initform 0
    :documentation "Days since issue was closed.")
   (tier
    :initarg :tier
    :type integer
    :initform 1
    :documentation "Compaction tier (1 or 2).")
   (compacted
    :initarg :compacted
    :type boolean
    :initform nil
    :documentation "Whether already compacted."))
  "A compaction candidate from bd admin compact --analyze.")

(defun beads-compact-candidate-from-json (json)
  "Create a beads-compact-candidate from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-compact-candidate json))

(defclass beads-compact-result ()
  ((success
    :initarg :success
    :type boolean
    :initform nil
    :documentation "Whether compaction succeeded.")
   (tier
    :initarg :tier
    :type integer
    :initform 1
    :documentation "Compaction tier (1 or 2).")
   (issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID (for single-issue apply).")
   (original-size
    :initarg :original-size
    :type integer
    :initform 0
    :documentation "Original size in bytes.")
   (compacted-size
    :initarg :compacted-size
    :type integer
    :initform 0
    :documentation "Size after compaction in bytes.")
   (saved-bytes
    :initarg :saved-bytes
    :type integer
    :initform 0
    :documentation "Bytes saved.")
   (reduction-pct
    :initarg :reduction-pct
    :type float
    :initform 0.0
    :documentation "Reduction percentage (0-100).")
   (elapsed-ms
    :initarg :elapsed-ms
    :type integer
    :initform 0
    :documentation "Milliseconds elapsed.")
   (total
    :initarg :total
    :type (or null integer)
    :initform nil
    :documentation "Total candidates (for batch/auto mode).")
   (succeeded
    :initarg :succeeded
    :type (or null integer)
    :initform nil
    :documentation "Successful compactions (for batch/auto mode).")
   (failed
    :initarg :failed
    :type (or null integer)
    :initform nil
    :documentation "Failed compactions (for batch/auto mode)."))
  "Result of bd admin compact --apply or --auto.")

(defun beads-compact-result-from-json (json)
  "Create a beads-compact-result from JSON alist.
Delegates to `beads-from-json'."
  (beads-from-json 'beads-compact-result json))

;;; ============================================================
;;; Section Data Classes
;;; ============================================================

(defclass beads-issues-section ()
  nil
  "Data container for a group of open issues.")

(defclass beads-issue-section ()
  ((issue
    :initarg :issue
    :initform nil
    :documentation "The `beads-issue' object for this section line."))
  "Data container for a single beads issue.")

(defclass beads-blocked-section ()
  nil
  "Data container for blocked issues.")

(defclass beads-ready-section ()
  nil
  "Data container for ready (unblocked) issues.")

(provide 'beads-types)
;;; beads-types.el ends here
