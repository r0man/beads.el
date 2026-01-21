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
      :key "q"
      :transient "Search query"
      :class transient-option
      :argument "--query="
      :prompt "Search query: "
      :transient-group "Query"
      :level 1
      :order 1)
     (status
      :initarg :status
      :type (or null string)
      :initform nil
      :documentation "Filter by status (--status).
  Valid values: open, in_progress, blocked, deferred, closed."
      :long-option "status"
      :short-option "s"
      :option-type :string
      :key "s"
      :transient "Filter by status"
      :class transient-option
      :argument "--status="
      :prompt "Status: "
      :choices ("open" "in_progress" "blocked" "deferred" "closed")
      :transient-group "Filters"
      :level 2
      :order 1)
     (issue-type
      :initarg :issue-type
      :type (or null string)
      :initform nil
      :documentation "Filter by issue type (--type).
  Valid values: bug, feature, task, epic, chore, merge-request, molecule, gate."
      :long-option "type"
      :short-option "t"
      :option-type :string
      :key "t"
      :transient "Filter by type"
      :class transient-option
      :argument "--type="
      :prompt "Type: "
      :choices ("bug" "feature" "task" "epic" "chore" "merge-request"
                          "molecule" "gate")
      :transient-group "Filters"
      :level 2
      :order 2)
     (assignee
      :initarg :assignee
      :type (or null string)
      :initform nil
      :documentation "Filter by assignee (--assignee)."
      :long-option "assignee"
      :short-option "a"
      :option-type :string
      :key "a"
      :transient "Filter by assignee"
      :class transient-option
      :argument "--assignee="
      :prompt "Assignee: "
      :transient-group "Filters"
      :level 2
      :order 3)
     (label
      :initarg :label
      :type (or null string)
      :initform nil
      :documentation "Filter by labels (AND: must have ALL) (--label)."
      :long-option "label"
      :short-option "l"
      :option-type :string
      :key "l"
      :transient "Filter by label (AND)"
      :class transient-option
      :argument "--label="
      :prompt "Label: "
      :transient-group "Filters"
      :level 2
      :order 4)
     (label-any
      :initarg :label-any
      :type (or null string)
      :initform nil
      :documentation "Filter by labels (OR: must have ANY) (--label-any)."
      :long-option "label-any"
      :option-type :string
      :key "L"
      :transient "Filter by label (OR)"
      :class transient-option
      :argument "--label-any="
      :prompt "Label (any): "
      :transient-group "Filters"
      :level 3
      :order 1)
     (limit
      :initarg :limit
      :type (or null integer)
      :initform nil
      :documentation "Limit number of results (--limit).
  Default is 50."
      :long-option "limit"
      :short-option "n"
      :option-type :integer
      :key "n"
      :transient "Result limit"
      :class transient-option
      :argument "--limit="
      :prompt "Limit (default 50): "
      :transient-group "Output"
      :level 2
      :order 1)
     (sort
      :initarg :sort
      :type (or null string)
      :initform nil
      :documentation "Sort field (--sort).
  Valid values: priority, created, updated, closed, status, id, title, type, assignee."
      :long-option "sort"
      :option-type :string
      :key "o"
      :transient "Sort by field"
      :class transient-option
      :argument "--sort="
      :prompt "Sort by: "
      :choices ("priority" "created" "updated" "closed" "status" "id"
                          "title" "type" "assignee")
      :transient-group "Output"
      :level 2
      :order 2)
     (reverse
      :initarg :reverse
      :type boolean
      :initform nil
      :documentation "Reverse sort order (--reverse)."
      :long-option "reverse"
      :short-option "r"
      :option-type :boolean
      :key "r"
      :transient "Reverse sort"
      :class transient-switch
      :argument "--reverse"
      :transient-group "Output"
      :level 2
      :order 3)
     (long
      :initarg :long
      :type boolean
      :initform nil
      :documentation "Show detailed multi-line output (--long)."
      :long-option "long"
      :option-type :boolean
      :key "g"
      :transient "Long output"
      :class transient-switch
      :argument "--long"
      :transient-group "Output"
      :level 3
      :order 1)
     (priority-min
      :initarg :priority-min
      :type (or null string)
      :initform nil
      :documentation "Filter by minimum priority (--priority-min).
  Values: 0-4 or P0-P4."
      :long-option "priority-min"
      :option-type :string
      :key "pm"
      :transient "Min priority"
      :class transient-option
      :argument "--priority-min="
      :prompt "Min priority (0-4): "
      :transient-group "Priority"
      :level 3
      :order 1)
     (priority-max
      :initarg :priority-max
      :type (or null string)
      :initform nil
      :documentation "Filter by maximum priority (--priority-max).
  Values: 0-4 or P0-P4."
      :long-option "priority-max"
      :option-type :string
      :key "pM"
      :transient "Max priority"
      :class transient-option
      :argument "--priority-max="
      :prompt "Max priority (0-4): "
      :transient-group "Priority"
      :level 3
      :order 2)
     (created-after
      :initarg :created-after
      :type (or null string)
      :initform nil
      :documentation "Filter issues created after date (--created-after).
  Format: YYYY-MM-DD or RFC3339."
      :long-option "created-after"
      :option-type :string
      :key "Ca"
      :transient "Created after"
      :class transient-option
      :argument "--created-after="
      :prompt "Created after (YYYY-MM-DD): "
      :transient-group "Date Filters"
      :level 3
      :order 1)
     (created-before
      :initarg :created-before
      :type (or null string)
      :initform nil
      :documentation "Filter issues created before date (--created-before).
  Format: YYYY-MM-DD or RFC3339."
      :long-option "created-before"
      :option-type :string
      :key "Cb"
      :transient "Created before"
      :class transient-option
      :argument "--created-before="
      :prompt "Created before (YYYY-MM-DD): "
      :transient-group "Date Filters"
      :level 3
      :order 2)
     (updated-after
      :initarg :updated-after
      :type (or null string)
      :initform nil
      :documentation "Filter issues updated after date (--updated-after).
  Format: YYYY-MM-DD or RFC3339."
      :long-option "updated-after"
      :option-type :string
      :key "Ua"
      :transient "Updated after"
      :class transient-option
      :argument "--updated-after="
      :prompt "Updated after (YYYY-MM-DD): "
      :transient-group "Date Filters"
      :level 3
      :order 3)
     (updated-before
      :initarg :updated-before
      :type (or null string)
      :initform nil
      :documentation "Filter issues updated before date (--updated-before).
  Format: YYYY-MM-DD or RFC3339."
      :long-option "updated-before"
      :option-type :string
      :key "Ub"
      :transient "Updated before"
      :class transient-option
      :argument "--updated-before="
      :prompt "Updated before (YYYY-MM-DD): "
      :transient-group "Date Filters"
      :level 3
      :order 4)
     (closed-after
      :initarg :closed-after
      :type (or null string)
      :initform nil
      :documentation "Filter issues closed after date (--closed-after).
  Format: YYYY-MM-DD or RFC3339."
      :long-option "closed-after"
      :option-type :string
      :key "ca"
      :transient "Closed after"
      :class transient-option
      :argument "--closed-after="
      :prompt "Closed after (YYYY-MM-DD): "
      :transient-group "Date Filters"
      :level 3
      :order 5)
     (closed-before
      :initarg :closed-before
      :type (or null string)
      :initform nil
      :documentation "Filter issues closed before date (--closed-before).
  Format: YYYY-MM-DD or RFC3339."
      :long-option "closed-before"
      :option-type :string
      :key "cb"
      :transient "Closed before"
      :class transient-option
      :argument "--closed-before="
      :prompt "Closed before (YYYY-MM-DD): "
      :transient-group "Date Filters"
      :level 3
      :order 6))
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
