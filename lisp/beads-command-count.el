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

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-count (beads-command-json)
  ((assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee (--assignee)."
    :long-option "assignee"
    :short-option "a"
    :option-type :string
    :transient-key "a"
    :transient-description "Filter by assignee"
    :transient-class transient-option
    :transient-argument "--assignee="
    :transient-prompt "Assignee: "
    :transient-group "Filters"
    :transient-level 2
    :transient-order 1)
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status (--status).
Valid values: open, in_progress, blocked, closed."
    :long-option "status"
    :option-type :string
    :transient-key "s"
    :transient-description "Filter by status"
    :transient-class transient-option
    :transient-argument "--status="
    :transient-prompt "Status: "
    :transient-choices ("open" "in_progress" "blocked" "closed")
    :transient-group "Filters"
    :transient-level 2
    :transient-order 2)
   (created-after
    :initarg :created-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues created after date (--created-after).
Format: YYYY-MM-DD or RFC3339."
    :long-option "created-after"
    :option-type :string
    :transient-key "Ca"
    :transient-description "Created after date"
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
    :long-option "created-before"
    :option-type :string
    :transient-key "Cb"
    :transient-description "Created before date"
    :transient-class transient-option
    :transient-argument "--created-before="
    :transient-prompt "Created before (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 2)
   (closed-after
    :initarg :closed-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed after date (--closed-after).
Format: YYYY-MM-DD or RFC3339."
    :long-option "closed-after"
    :option-type :string
    :transient-key "ca"
    :transient-description "Closed after date"
    :transient-class transient-option
    :transient-argument "--closed-after="
    :transient-prompt "Closed after (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 3)
   (closed-before
    :initarg :closed-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed before date (--closed-before).
Format: YYYY-MM-DD or RFC3339."
    :long-option "closed-before"
    :option-type :string
    :transient-key "cb"
    :transient-description "Closed before date"
    :transient-class transient-option
    :transient-argument "--closed-before="
    :transient-prompt "Closed before (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 4)
   (by-status
    :initarg :by-status
    :type boolean
    :initform nil
    :documentation "Group count by status (--by-status)."
    :long-option "by-status"
    :option-type :boolean
    :transient-key "bs"
    :transient-description "Group by status"
    :transient-class transient-switch
    :transient-argument "--by-status"
    :transient-group "Grouping"
    :transient-level 2
    :transient-order 1)
   (by-priority
    :initarg :by-priority
    :type boolean
    :initform nil
    :documentation "Group count by priority (--by-priority)."
    :long-option "by-priority"
    :option-type :boolean
    :transient-key "bp"
    :transient-description "Group by priority"
    :transient-class transient-switch
    :transient-argument "--by-priority"
    :transient-group "Grouping"
    :transient-level 2
    :transient-order 2)
   (by-type
    :initarg :by-type
    :type boolean
    :initform nil
    :documentation "Group count by issue type (--by-type)."
    :long-option "by-type"
    :option-type :boolean
    :transient-key "bt"
    :transient-description "Group by type"
    :transient-class transient-switch
    :transient-argument "--by-type"
    :transient-group "Grouping"
    :transient-level 2
    :transient-order 3)
   (by-assignee
    :initarg :by-assignee
    :type boolean
    :initform nil
    :documentation "Group count by assignee (--by-assignee)."
    :long-option "by-assignee"
    :option-type :boolean
    :transient-key "ba"
    :transient-description "Group by assignee"
    :transient-class transient-switch
    :transient-argument "--by-assignee"
    :transient-group "Grouping"
    :transient-level 2
    :transient-order 4)
   (by-label
    :initarg :by-label
    :type boolean
    :initform nil
    :documentation "Group count by label (--by-label)."
    :long-option "by-label"
    :option-type :boolean
    :transient-key "bl"
    :transient-description "Group by label"
    :transient-class transient-switch
    :transient-argument "--by-label"
    :transient-group "Grouping"
    :transient-level 2
    :transient-order 5))
  :documentation "Represents bd count command.
Counts issues matching the specified filters.
When executed with :json t, returns count data as JSON."))

(cl-defmethod beads-command-subcommand ((_command beads-command-count))
  "Return subcommand name for count command."
  "count")

(cl-defmethod beads-command-validate ((_command beads-command-count))
  "Validate count COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-count))
  "Execute CMD in compilation buffer with human-readable output.
Disables JSON mode for interactive display."
  ;; Set json to nil for human-readable output
  (oset cmd json nil)
  ;; Call the default implementation
  (cl-call-next-method))

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
