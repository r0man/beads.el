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
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-command-list)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-reader)

;;; Search Command

(beads-defcommand beads-command-search (beads-command-global-options)
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
    :transient-reader beads-reader-list-label
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
    :transient-reader beads-reader-list-label
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
    :transient-reader beads-reader-list-priority
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
    :transient-reader beads-reader-list-priority
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
When executed with :json t, returns matching issues as JSON."
  :parse-as :issues)


(cl-defmethod beads-command-validate ((_command beads-command-search))
  "Validate search COMMAND.
No required fields, returns nil (valid)."
  nil)

;;; Interactive Execution — display results in tabulated-list-mode

(defvar-local beads-search--command-obj nil
  "The beads-command-search object used to populate this search buffer.
Used for refresh support.")

(cl-defmethod beads-command-execute-interactive
  ((cmd beads-command-search))
  "Execute search CMD and display results in `beads-list-mode'.
Runs with JSON enabled to parse results into beads-issue objects,
then displays them in a tabulated list buffer."
  ;; Force JSON on for structured output
  (oset cmd json t)
  (condition-case err
      (let* ((caller-dir default-directory)
             (project-dir (or (beads-git-find-project-root)
                              default-directory))
             (exec (beads-command-execute cmd))
             (issue-objects (oref exec result))
             (buffer (beads-list--get-or-create-buffer 'search)))
        (with-current-buffer buffer
          (unless (derived-mode-p 'beads-list-mode)
            (beads-list-mode))
          ;; Update directory-aware state
          (setq beads-list--project-dir project-dir)
          (setq beads-list--branch (beads-git-get-branch))
          (setq beads-list--proj-name (beads-git-get-project-name))
          (setq beads-search--command-obj cmd)
          (setq default-directory caller-dir)
          (if (not issue-objects)
              (progn
                (setq tabulated-list-entries nil)
                (tabulated-list-print t)
                (message "No issues found for search"))
            (beads-list--populate-buffer issue-objects 'search)
            (message "Found %d issue%s"
                     (length issue-objects)
                     (if (= (length issue-objects) 1) "" "s"))))
        (beads-list--display-buffer buffer))
    (error
     (message "Search failed: %s" (error-message-string err)))))


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
