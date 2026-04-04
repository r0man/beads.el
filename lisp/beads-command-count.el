;;; beads-command-count.el --- Count command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-count' EIEIO class for the
;; `bd count' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd count command counts issues matching the specified filters.
;; By default, returns the total count of issues.
;; Use --by-* flags to group counts by different attributes.

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; Count Command

(beads-defcommand beads-command-count (beads-command-global-options)
  ((assignee
    :short-option "a"
    :type (or null string)
    :group "Filters"
    :level 2
    :order 1)
   (status
    :type (or null string)
    :short-option "s"
    :choices ("open" "in_progress" "blocked" "closed")
    :group "Filters"
    :level 2
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
   (closed-after
    :type (or null string)
    :short-option "ca"
    :prompt "Closed after (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 3)
   (closed-before
    :type (or null string)
    :short-option "cb"
    :prompt "Closed before (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 4)
   (by-status
    :type boolean
    :short-option "bs"
    :group "Grouping"
    :level 2
    :order 1)
   (by-priority
    :type boolean
    :short-option "bp"
    :group "Grouping"
    :level 2
    :order 2)
   (by-type
    :type boolean
    :short-option "bt"
    :group "Grouping"
    :level 2
    :order 3)
   (by-assignee
    :type boolean
    :short-option "ba"
    :group "Grouping"
    :level 2
    :order 4)
   (by-label
    :type boolean
    :short-option "bl"
    :group "Grouping"
    :level 2
    :order 5)
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
   (id
    :type (or null string)
    :long-option "id"
    :group "Filter"
    :level 3)
   (label
    :type (or null string)
    :long-option "label"
    :group "Filter"
    :level 3)
   (label-any
    :type (or null string)
    :long-option "label-any"
    :group "Filter"
    :level 3)
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
    :level 3)
   (priority-min
    :type (or null integer)
    :long-option "priority-min"
    :group "Filter"
    :level 3)
   (priority-max
    :type (or null integer)
    :long-option "priority-max"
    :group "Filter"
    :level 3)
   (title
    :type (or null string)
    :long-option "title"
    :group "Filter"
    :level 3)
   (title-contains
    :type (or null string)
    :long-option "title-contains"
    :group "Filter"
    :level 3))
  :documentation "Represents bd count command.
Counts issues matching the specified filters.
When executed with :json t, returns count data as JSON.")


(cl-defmethod beads-command-validate ((_command beads-command-count))
  "Validate count COMMAND.
No required fields, returns nil (valid)."
  nil)


;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-count "beads-command-count" nil t)
(beads-meta-define-transient beads-command-count "beads-count"
  "Count issues matching filters.

By default, returns the total count of issues matching the filters.
Use --by-* flags to group counts by different attributes.

Transient levels control which options are visible (cycle with C-x l):
  Level 2: Basic filters and grouping options
  Level 3: Date filters"
  beads-option-global-section)

(provide 'beads-command-count)
;;; beads-command-count.el ends here
