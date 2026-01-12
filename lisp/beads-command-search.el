;;; beads-command-search.el --- Search command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-search' EIEIO class for the
;; `bd search' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd search command performs full-text search across issues.
;; It searches title, description, and ID fields.  It supports:
;; - Basic text search (positional query)
;; - Status, type, assignee, and label filtering
;; - Date range filtering (created, updated, closed)
;; - Priority range filtering
;; - Sort and limit options

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; Search Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-search (beads-command-json)
  ((query
    :initarg :query
    :type (or null string)
    :initform nil
    :documentation "Search query text (positional or --query).
Searches across title, description, and ID."
    :positional 1
    :transient-key "q"
    :transient-description "Search query"
    :transient-class transient-option
    :transient-argument "--query="
    :transient-prompt "Search query: "
    :transient-group "Query"
    :transient-level 1
    :transient-order 1)
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status (--status).
Valid values: open, in_progress, blocked, deferred, closed."
    :long-option "--status"
    :short-option "-s"
    :option-type :string
    :transient-key "s"
    :transient-description "Filter by status"
    :transient-class transient-option
    :transient-argument "--status="
    :transient-prompt "Status: "
    :transient-choices ("open" "in_progress" "blocked" "deferred" "closed")
    :transient-group "Filters"
    :transient-level 2
    :transient-order 1)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Filter by issue type (--type).
Valid values: bug, feature, task, epic, chore, merge-request, molecule, gate."
    :long-option "--type"
    :short-option "-t"
    :option-type :string
    :transient-key "t"
    :transient-description "Filter by type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Type: "
    :transient-choices ("bug" "feature" "task" "epic" "chore" "merge-request"
                        "molecule" "gate")
    :transient-group "Filters"
    :transient-level 2
    :transient-order 2)
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee (--assignee)."
    :long-option "--assignee"
    :short-option "-a"
    :option-type :string
    :transient-key "a"
    :transient-description "Filter by assignee"
    :transient-class transient-option
    :transient-argument "--assignee="
    :transient-prompt "Assignee: "
    :transient-group "Filters"
    :transient-level 2
    :transient-order 3)
   (label
    :initarg :label
    :type (or null string)
    :initform nil
    :documentation "Filter by labels (AND: must have ALL) (--label)."
    :long-option "--label"
    :short-option "-l"
    :option-type :string
    :transient-key "l"
    :transient-description "Filter by label (AND)"
    :transient-class transient-option
    :transient-argument "--label="
    :transient-prompt "Label: "
    :transient-group "Filters"
    :transient-level 2
    :transient-order 4)
   (label-any
    :initarg :label-any
    :type (or null string)
    :initform nil
    :documentation "Filter by labels (OR: must have ANY) (--label-any)."
    :long-option "--label-any"
    :option-type :string
    :transient-key "L"
    :transient-description "Filter by label (OR)"
    :transient-class transient-option
    :transient-argument "--label-any="
    :transient-prompt "Label (any): "
    :transient-group "Filters"
    :transient-level 3
    :transient-order 1)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit number of results (--limit).
Default is 50."
    :long-option "--limit"
    :short-option "-n"
    :option-type :integer
    :transient-key "n"
    :transient-description "Result limit"
    :transient-class transient-option
    :transient-argument "--limit="
    :transient-prompt "Limit (default 50): "
    :transient-group "Output"
    :transient-level 2
    :transient-order 1)
   (sort
    :initarg :sort
    :type (or null string)
    :initform nil
    :documentation "Sort field (--sort).
Valid values: priority, created, updated, closed, status, id, title, type, assignee."
    :long-option "--sort"
    :option-type :string
    :transient-key "o"
    :transient-description "Sort by field"
    :transient-class transient-option
    :transient-argument "--sort="
    :transient-prompt "Sort by: "
    :transient-choices ("priority" "created" "updated" "closed" "status" "id"
                        "title" "type" "assignee")
    :transient-group "Output"
    :transient-level 2
    :transient-order 2)
   (reverse
    :initarg :reverse
    :type boolean
    :initform nil
    :documentation "Reverse sort order (--reverse)."
    :long-option "--reverse"
    :short-option "-r"
    :option-type :boolean
    :transient-key "r"
    :transient-description "Reverse sort"
    :transient-class transient-switch
    :transient-argument "--reverse"
    :transient-group "Output"
    :transient-level 2
    :transient-order 3)
   (long
    :initarg :long
    :type boolean
    :initform nil
    :documentation "Show detailed multi-line output (--long)."
    :long-option "--long"
    :option-type :boolean
    :transient-key "g"
    :transient-description "Long output"
    :transient-class transient-switch
    :transient-argument "--long"
    :transient-group "Output"
    :transient-level 3
    :transient-order 1)
   (priority-min
    :initarg :priority-min
    :type (or null string)
    :initform nil
    :documentation "Filter by minimum priority (--priority-min).
Values: 0-4 or P0-P4."
    :long-option "--priority-min"
    :option-type :string
    :transient-key "pm"
    :transient-description "Min priority"
    :transient-class transient-option
    :transient-argument "--priority-min="
    :transient-prompt "Min priority (0-4): "
    :transient-group "Priority"
    :transient-level 3
    :transient-order 1)
   (priority-max
    :initarg :priority-max
    :type (or null string)
    :initform nil
    :documentation "Filter by maximum priority (--priority-max).
Values: 0-4 or P0-P4."
    :long-option "--priority-max"
    :option-type :string
    :transient-key "pM"
    :transient-description "Max priority"
    :transient-class transient-option
    :transient-argument "--priority-max="
    :transient-prompt "Max priority (0-4): "
    :transient-group "Priority"
    :transient-level 3
    :transient-order 2)
   (created-after
    :initarg :created-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues created after date (--created-after).
Format: YYYY-MM-DD or RFC3339."
    :long-option "--created-after"
    :option-type :string
    :transient-key "Ca"
    :transient-description "Created after"
    :transient-class transient-option
    :transient-argument "--created-after="
    :transient-prompt "Created after (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 1)
   (created-before
    :initarg :created-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues created before date (--created-before).
Format: YYYY-MM-DD or RFC3339."
    :long-option "--created-before"
    :option-type :string
    :transient-key "Cb"
    :transient-description "Created before"
    :transient-class transient-option
    :transient-argument "--created-before="
    :transient-prompt "Created before (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 2)
   (updated-after
    :initarg :updated-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated after date (--updated-after).
Format: YYYY-MM-DD or RFC3339."
    :long-option "--updated-after"
    :option-type :string
    :transient-key "Ua"
    :transient-description "Updated after"
    :transient-class transient-option
    :transient-argument "--updated-after="
    :transient-prompt "Updated after (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 3)
   (updated-before
    :initarg :updated-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated before date (--updated-before).
Format: YYYY-MM-DD or RFC3339."
    :long-option "--updated-before"
    :option-type :string
    :transient-key "Ub"
    :transient-description "Updated before"
    :transient-class transient-option
    :transient-argument "--updated-before="
    :transient-prompt "Updated before (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 4)
   (closed-after
    :initarg :closed-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed after date (--closed-after).
Format: YYYY-MM-DD or RFC3339."
    :long-option "--closed-after"
    :option-type :string
    :transient-key "ca"
    :transient-description "Closed after"
    :transient-class transient-option
    :transient-argument "--closed-after="
    :transient-prompt "Closed after (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 5)
   (closed-before
    :initarg :closed-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed before date (--closed-before).
Format: YYYY-MM-DD or RFC3339."
    :long-option "--closed-before"
    :option-type :string
    :transient-key "cb"
    :transient-description "Closed before"
    :transient-class transient-option
    :transient-argument "--closed-before="
    :transient-prompt "Closed before (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 6))
  :documentation "Represents bd search command.
Full-text search across issues.
When executed with :json t, returns matching issues as JSON."))

(cl-defmethod beads-command-subcommand ((_command beads-command-search))
  "Return subcommand name for search command."
  "search")

(cl-defmethod beads-command-validate ((_command beads-command-search))
  "Validate search COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-search))
  "Execute CMD in compilation buffer with human-readable output.
Disables JSON mode for interactive display with colors."
  ;; Set json to nil for human-readable colored output
  (oset cmd json nil)
  ;; Call the default implementation
  (cl-call-next-method))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-search "beads-command-search" nil t)
(beads-meta-define-transient beads-command-search "beads-search"
  "Search issues by text.

Full-text search across issue title, description, and ID.
Supports filtering by status, type, assignee, labels, dates, and priority.

Examples:
  Search for authentication bugs
  Search with status=open filter
  Search by partial ID (bd-5q)

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Query
  Level 2: Basic filters (status, type, assignee, label)
  Level 3: Advanced filters (dates, priority, output options)"
  beads-option-global-section)

(provide 'beads-command-search)
;;; beads-command-search.el ends here
