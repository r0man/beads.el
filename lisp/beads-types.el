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
    :documentation "Priority level (0=lowest, 4=highest).")
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
    :type list
    :initform nil
    :documentation "List of label strings.")
   (dependencies
    :initarg :dependencies
    :type list
    :initform nil
    :documentation "List of beads-dependency objects.")
   (dependents
    :initarg :dependents
    :type list
    :initform nil
    :documentation "List of beads-dependency objects for issues blocked by this one.")
   (comments
    :initarg :comments
    :type list
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
    :type list
    :initform nil
    :documentation "Filter by labels (AND semantics).")
   (labels-any
    :initarg :labels-any
    :type list
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
JSON should be the parsed JSON object from bd --json output."
  (beads-issue
   :id (alist-get 'id json)
   :content-hash (alist-get 'content_hash json)
   :title (alist-get 'title json)
   :description (alist-get 'description json)
   :design (alist-get 'design json)
   :acceptance-criteria (alist-get 'acceptance_criteria json)
   :notes (alist-get 'notes json)
   :status (alist-get 'status json)
   :priority (alist-get 'priority json)
   :issue-type (alist-get 'issue_type json)
   :assignee (alist-get 'assignee json)
   :estimated-minutes (alist-get 'estimated_minutes json)
   :created-at (alist-get 'created_at json)
   :updated-at (alist-get 'updated_at json)
   :closed-at (alist-get 'closed_at json)
   :external-ref (alist-get 'external_ref json)
   :compaction-level (alist-get 'compaction_level json)
   :compacted-at (alist-get 'compacted_at json)
   :compacted-at-commit (alist-get 'compacted_at_commit json)
   :original-size (alist-get 'original_size json)
   :source-repo (alist-get 'source_repo json)
   :created-by (alist-get 'created_by json)
   :labels (append (alist-get 'labels json) nil)
   :dependencies (when-let ((deps (alist-get 'dependencies json)))
                   (mapcar #'beads-dependency-from-json (append deps nil)))
   :dependents (when-let ((deps (alist-get 'dependents json)))
                 (mapcar #'beads-dependency-from-json (append deps nil)))
   :comments (when-let ((comments (alist-get 'comments json)))
               (mapcar #'beads-comment-from-json (append comments nil)))))

(defun beads-dependency-from-json (json)
  "Create a beads-dependency object from JSON alist.
JSON can be either:
- Simple dependency format (from bd dep): issue_id, depends_on_id, type
- IssueWithDependencyMetadata format (from bd show --json):
  Full issue fields + dependency_type"
  ;; bd show --json returns IssueWithDependencyMetadata which has:
  ;; - All Issue fields (id, title, status, priority, etc.)
  ;; - dependency_type field
  ;; The 'id' is the depends_on_id (the issue being depended on)
  (let ((dep-type (or (alist-get 'dependency_type json)
                      (alist-get 'type json))))
    (beads-dependency
     :issue-id (alist-get 'issue_id json)
     :depends-on-id (or (alist-get 'depends_on_id json)
                        (alist-get 'id json))  ; bd show uses 'id'
     :type dep-type
     :created-at (alist-get 'created_at json)
     :created-by (alist-get 'created_by json)
     ;; Additional fields from IssueWithDependencyMetadata
     :title (alist-get 'title json)
     :status (alist-get 'status json)
     :priority (alist-get 'priority json)
     :issue-type (alist-get 'issue_type json))))

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
   :content-hash (alist-get 'content_hash json)
   :title (alist-get 'title json)
   :description (alist-get 'description json)
   :design (alist-get 'design json)
   :acceptance-criteria (alist-get 'acceptance_criteria json)
   :notes (alist-get 'notes json)
   :status (alist-get 'status json)
   :priority (alist-get 'priority json)
   :issue-type (alist-get 'issue_type json)
   :assignee (alist-get 'assignee json)
   :estimated-minutes (alist-get 'estimated_minutes json)
   :created-at (alist-get 'created_at json)
   :updated-at (alist-get 'updated_at json)
   :closed-at (alist-get 'closed_at json)
   :external-ref (alist-get 'external_ref json)
   :compaction-level (alist-get 'compaction_level json)
   :compacted-at (alist-get 'compacted_at json)
   :compacted-at-commit (alist-get 'compacted_at_commit json)
   :original-size (alist-get 'original_size json)
   :source-repo (alist-get 'source_repo json)
   :created-by (alist-get 'created_by json)
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
   :content-hash (alist-get 'content_hash json)
   :title (alist-get 'title json)
   :description (alist-get 'description json)
   :design (alist-get 'design json)
   :acceptance-criteria (alist-get 'acceptance_criteria json)
   :notes (alist-get 'notes json)
   :status (alist-get 'status json)
   :priority (alist-get 'priority json)
   :issue-type (alist-get 'issue_type json)
   :assignee (alist-get 'assignee json)
   :estimated-minutes (alist-get 'estimated_minutes json)
   :created-at (alist-get 'created_at json)
   :updated-at (alist-get 'updated_at json)
   :closed-at (alist-get 'closed_at json)
   :external-ref (alist-get 'external_ref json)
   :compaction-level (alist-get 'compaction_level json)
   :compacted-at (alist-get 'compacted_at json)
   :compacted-at-commit (alist-get 'compacted_at_commit json)
   :original-size (alist-get 'original_size json)
   :source-repo (alist-get 'source_repo json)
   :created-by (alist-get 'created_by json)
   :labels (append (alist-get 'labels json) nil)
   :dependencies (when-let ((deps (alist-get 'dependencies json)))
                   (mapcar #'beads-dependency-from-json (append deps nil)))
   :comments (when-let ((comments (alist-get 'comments json)))
               (mapcar #'beads-comment-from-json (append comments nil)))
   :depth (or (alist-get 'depth json) 0)
   :parent-id (alist-get 'parent_id json)
   :truncated (eq (alist-get 'truncated json) t)))

(defun beads-statistics-from-json (json)
  "Create a beads-statistics object from JSON alist."
  (beads-statistics
   :total-issues (or (alist-get 'total_issues json) 0)
   :open-issues (or (alist-get 'open_issues json) 0)
   :in-progress-issues (or (alist-get 'in_progress_issues json) 0)
   :closed-issues (or (alist-get 'closed_issues json) 0)
   :blocked-issues (or (alist-get 'blocked_issues json) 0)
   :deferred-issues (or (alist-get 'deferred_issues json) 0)
   :ready-issues (or (alist-get 'ready_issues json) 0)
   :tombstone-issues (or (alist-get 'tombstone_issues json) 0)
   :pinned-issues (or (alist-get 'pinned_issues json) 0)
   :epics-eligible-for-closure
   (or (alist-get 'epics_eligible_for_closure json) 0)
   :average-lead-time (or (alist-get 'average_lead_time_hours json) 0.0)))

(defun beads-recent-activity-from-json (json)
  "Create a beads-recent-activity object from JSON alist."
  (beads-recent-activity
   :hours-tracked (or (alist-get 'hours_tracked json) 24)
   :commit-count (or (alist-get 'commit_count json) 0)
   :issues-created (or (alist-get 'issues_created json) 0)
   :issues-closed (or (alist-get 'issues_closed json) 0)
   :issues-updated (or (alist-get 'issues_updated json) 0)
   :issues-reopened (or (alist-get 'issues_reopened json) 0)
   :total-changes (or (alist-get 'total_changes json) 0)))

(defun beads-stats-data-from-json (json)
  "Create a beads-stats-data object from JSON alist.
JSON should be the top-level stats response with summary and
recent_activity keys."
  (let ((summary-json (alist-get 'summary json))
        (activity-json (alist-get 'recent_activity json)))
    ;; Ensure average_lead_time_hours is a float
    (when summary-json
      (let ((lead-time (alist-get 'average_lead_time_hours summary-json)))
        (when (and lead-time (integerp lead-time))
          (setf (alist-get 'average_lead_time_hours summary-json)
                (float lead-time)))))
    (beads-stats-data
     :summary (if summary-json
                  (beads-statistics-from-json summary-json)
                ;; Fallback for flat structure (backwards compat)
                (beads-statistics-from-json json))
     :recent-activity (when activity-json
                        (beads-recent-activity-from-json activity-json)))))

(defun beads-epic-status-from-json (json)
  "Create a beads-epic-status object from JSON alist."
  (beads-epic-status
   :epic (when-let ((epic-json (alist-get 'epic json)))
           (beads-issue-from-json epic-json))
   :total-children (or (alist-get 'total_children json) 0)
   :closed-children (or (alist-get 'closed_children json) 0)
   :eligible-for-close (eq (alist-get 'eligible_for_close json) t)))

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

(provide 'beads-types)
;;; beads-types.el ends here
