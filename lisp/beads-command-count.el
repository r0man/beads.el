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
    :option-type :string
    :key "a"
    :group "Filters"
    :level 2
    :order 1)
   (status
    :option-type :string
    :key "s"
    :choices ("open" "in_progress" "blocked" "closed")
    :group "Filters"
    :level 2
    :order 2)
   (created-after
    :option-type :string
    :key "Ca"
    :prompt "Created after (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 1)
   (created-before
    :option-type :string
    :key "Cb"
    :prompt "Created before (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 2)
   (closed-after
    :option-type :string
    :key "ca"
    :prompt "Closed after (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 3)
   (closed-before
    :option-type :string
    :key "cb"
    :prompt "Closed before (YYYY-MM-DD): "
    :group "Date Filters"
    :level 3
    :order 4)
   (by-status
    :option-type :boolean
    :key "bs"
    :group "Grouping"
    :level 2
    :order 1)
   (by-priority
    :option-type :boolean
    :key "bp"
    :group "Grouping"
    :level 2
    :order 2)
   (by-type
    :option-type :boolean
    :key "bt"
    :group "Grouping"
    :level 2
    :order 3)
   (by-assignee
    :option-type :boolean
    :key "ba"
    :group "Grouping"
    :level 2
    :order 4)
   (by-label
    :option-type :boolean
    :key "bl"
    :group "Grouping"
    :level 2
    :order 5))
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
