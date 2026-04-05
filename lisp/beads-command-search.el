;;; beads-command-search.el --- Search command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-search' EIEIO class for the
;; `bd search' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-defcommand'.
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
    :positional 1
    :short-option "q"
    :prompt "Search query: "
    :group "Query"
    :level 1
    :order 1)
   (status
    :short-option "s"
    :type (or null string)
    :choices ("open" "in_progress" "blocked" "deferred" "closed")
    :group "Filters"
    :level 2
    :order 1)
   (issue-type
    :long-option "type"
    :short-option "t"
    :type (or null string)
    :choices ("bug" "feature" "task" "epic" "chore" "merge-request"
                        "molecule" "gate")
    :group "Filters"
    :level 2
    :order 2)
   (assignee
    :short-option "a"
    :type (or null string)
    :group "Filters"
    :level 2
    :order 3)
   (label
    :short-option "l"
    :type (or null string)
    :reader beads-reader-list-label
    :group "Filters"
    :level 2
    :order 4)
   (label-any
    :type (or null string)
    :short-option "L"
    :prompt "Label (any): "
    :reader beads-reader-list-label
    :group "Filters"
    :level 3
    :order 1)
   (limit
    :short-option "n"
    :type (or null string integer)
    :prompt "Limit (default 50): "
    :group "Output"
    :level 2
    :order 1)
   (sort
    :type (or null string)
    :short-option "o"
    :prompt "Sort by: "
    :choices ("priority" "created" "updated" "closed" "status" "id"
                        "title" "type" "assignee")
    :group "Output"
    :level 2
    :order 2)
   (reverse
    :short-option "r"
    :type boolean
    :group "Output"
    :level 2
    :order 3)
   (long
    :type boolean
    :short-option "g"
    :group "Output"
    :level 3
    :order 1)
   (priority-min
    :type (or null string)
    :short-option "pm"
    :prompt "Min priority (0-4): "
    :reader beads-reader-list-priority
    :group "Priority"
    :level 3
    :order 1)
   (priority-max
    :type (or null string)
    :short-option "pM"
    :prompt "Max priority (0-4): "
    :reader beads-reader-list-priority
    :group "Priority"
    :level 3
    :order 2)
   (created-after
    :type (or null string)
    :short-option "Ca"
    :prompt "Created after (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 1)
   (created-before
    :type (or null string)
    :short-option "Cb"
    :prompt "Created before (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 2)
   (updated-after
    :type (or null string)
    :short-option "Ua"
    :prompt "Updated after (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 3)
   (updated-before
    :type (or null string)
    :short-option "Ub"
    :prompt "Updated before (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 4)
   (closed-after
    :type (or null string)
    :short-option "ca"
    :prompt "Closed after (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 5)
   (closed-before
    :type (or null string)
    :short-option "cb"
    :prompt "Closed before (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 6)

   (desc-contains
    :type (or null string)
    :long-option "desc-contains"
    :group "Filter"
    :level 3)
   (empty-description
    :type boolean
    :long-option "empty-description"
    :group "Filter"
    :level 3)
   (external-contains
    :type (or null string)
    :long-option "external-contains"
    :group "Filter"
    :level 3)
   (has-metadata-key
    :type (or null string)
    :long-option "has-metadata-key"
    :group "Filter"
    :level 4)
   (metadata-field
    :type (or null string)
    :long-option "metadata-field"
    :group "Filter"
    :level 4)
   (no-assignee
    :type boolean
    :long-option "no-assignee"
    :group "Filter"
    :level 3)
   (no-labels
    :type boolean
    :long-option "no-labels"
    :group "Filter"
    :level 3)
   (notes-contains
    :type (or null string)
    :long-option "notes-contains"
    :group "Filter"
    :level 3))
  :documentation "Represents bd search command.
Full-text search across issues.
When executed with :json t, returns matching issues as JSON."
  :result (list-of beads-issue)
  :transient :manual)


;; beads-command-parse override removed: the base method now handles
;; JSON-to-domain parsing automatically via :result (list-of beads-issue).

;; Validate override removed: base handles slot-level validation.

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
             (issue-objects (beads-command-execute cmd))
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
