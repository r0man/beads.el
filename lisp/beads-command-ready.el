;;; beads-command-ready.el --- Ready command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-ready' EIEIO class for the
;; `bd ready' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd ready command shows ready work (issues with no blockers that
;; are open or in_progress).  This is useful for finding the next task
;; to work on.
;;
;; Features:
;; - Filter by assignee, priority, type, labels
;; - Filter to molecule steps (--mol)
;; - Sort by hybrid, priority, or oldest
;; - Show unassigned issues only
;; - Include deferred issues
;;
;; Usage:
;;   (beads-command-execute (beads-command-ready))
;;   (beads-execute 'beads-command-ready)  ; convenience function

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-reader)
(require 'beads-types)

;;; Ready Command

(beads-defcommand beads-command-ready (beads-command-global-options)
  ((assignee
    :type (or null string)
    :short-option "a"
    :group "Filters"
    :level 1
    :order 1)
   (include-deferred
    :type boolean
    :short-option "D"
    :group "Filters"
    :level 2
    :order 8)
   (issue-type
    :long-option "type"
    :short-option "t"
    :type (or null string)
    :transient-key "T"
    :reader beads-reader-list-type
    :group "Filters"
    :level 1
    :order 4)
   (label
    :type (list-of string)
    :short-option "l"
    :prompt "Label (AND): "
    :reader beads-reader-list-label
    :group "Label Filters"
    :level 2
    :order 1)
   (label-any
    :type (list-of string)
    :short-option "L"
    :prompt "Label (OR): "
    :reader beads-reader-list-label
    :group "Label Filters"
    :level 2
    :order 2)
   (limit
    :type (or null string integer)
    :short-option "n"
    :group "Display"
    :level 1
    :order 1)
   (mol
    :type (or null string)
    :short-option "m"
    :prompt "Molecule ID: "
    :reader beads-reader-issue-id
    :group "Scope"
    :level 2
    :order 1)
   (mol-type
    :type (or null string)
    :short-option "M"
    :choices '("swarm" "patrol" "work")
    :group "Scope"
    :level 2
    :order 2)
   (parent
    :type (or null string)
    :short-option "P"
    :prompt "Parent ID: "
    :reader beads-reader-issue-id
    :group "Scope"
    :level 2
    :order 3)
   (pretty
    :type boolean
    :short-option "y"
    :group "Display"
    :level 2
    :order 2)
   (priority
    :type (or null string integer)
    :short-option "p"
    :reader beads-reader-list-priority
    :group "Filters"
    :level 1
    :order 2)
   (sort
    :type (or null string)
    :short-option "s"
    :choices '("hybrid" "priority" "oldest")
    :group "Display"
    :level 1
    :order 3)
   (unassigned
    :type boolean
    :short-option "u"
    :group "Filters"
    :level 1
    :order 3)
   (exclude-type
    :type (or null string)
    :long-option "exclude-type"
    :group "Filter"
    :level 3)
   (explain
    :type boolean
    :group "Options"
    :level 2)
   (gated
    :type boolean
    :group "Filter"
    :level 3)
   (has-metadata-key
    :type (or null string)
    :long-option "has-metadata-key"
    :group "Filter"
    :level 4)
   (include-ephemeral
    :type boolean
    :long-option "include-ephemeral"
    :group "Filter"
    :level 3)
   (metadata-field
    :type (or null string)
    :long-option "metadata-field"
    :group "Filter"
    :level 4)
   (plain
    :type boolean
    :group "Display"
    :level 3))
  :documentation "Represents bd ready command.
Shows ready work (no blockers, open or in-progress).
When executed with :json t, returns list of beads-issue instances.")


(cl-defmethod beads-command-validate ((command beads-command-ready))
  "Validate ready COMMAND.
Checks for valid sort, priority, and mol-type values.
Returns error string or nil if valid."
  (with-slots (sort priority mol-type label label-any) command
    (or
     ;; Validate sort value
     (and sort (not (member sort '("hybrid" "priority" "oldest")))
          "Sort must be one of: hybrid, priority, oldest")
     ;; Validate priority range
     (and priority (not (<= 0 priority 4))
          "Priority must be between 0 and 4")
     ;; Validate mol-type value
     (and mol-type (not (member mol-type '("swarm" "patrol" "work")))
          "Mol-type must be one of: swarm, patrol, work")
     ;; Validate list content types
     (beads-command--validate-string-list label "label")
     (beads-command--validate-string-list label-any "label-any"))))

(cl-defmethod beads-command-parse ((command beads-command-ready) stdout)
  "Parse ready COMMAND output from STDOUT.
Returns list of beads-issue instances.
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, converts parsed JSON to beads-issue instances.
Does not modify any slots."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON, then convert to beads-issue instances
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (mapcar (lambda (j) (beads-from-json 'beads-issue j)) (append parsed-json nil))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instances: %s"
                                 (error-message-string err))
                         :stdout stdout
                         :parsed-json parsed-json
                         :parse-error err))))))))


;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-ready-transient "beads-command-ready" nil t)
(beads-meta-define-transient beads-command-ready "beads-ready-transient"
  "Show ready work (issues with no blockers).

Ready issues are those that:
- Have status open or in_progress
- Have no unresolved blockers
- Are not deferred (unless --include-deferred)

Useful for finding the next task to work on.

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Common filters (assignee, priority, type, limit, sort, unassigned)
  Level 2: Label, scope, and display options"
  beads-option-global-section)

(provide 'beads-command-ready)
;;; beads-command-ready.el ends here
