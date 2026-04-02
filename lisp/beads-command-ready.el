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
;;   (beads-command-ready!)  ; convenience function

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
    :option-type :string
    :short-option "a"
    :key "a"
    :transient-group "Filters"
    :level 1
    :order 1)
   (include-deferred
    :option-type :boolean
    :key "D"
    :transient-group "Filters"
    :level 2
    :order 8)
   (issue-type
    :long-option "type"
    :short-option "t"
    :option-type :string
    :key "T"
    :transient-reader beads-reader-list-type
    :transient-group "Filters"
    :level 1
    :order 4)
   (label
    :option-type :list
    :short-option "l"
    :key "l"
    :prompt "Label (AND): "
    :transient-reader beads-reader-list-label
    :transient-group "Label Filters"
    :level 2
    :order 1)
   (label-any
    :option-type :list
    :key "L"
    :prompt "Label (OR): "
    :transient-reader beads-reader-list-label
    :transient-group "Label Filters"
    :level 2
    :order 2)
   (limit
    :option-type :integer
    :short-option "n"
    :key "n"
    :transient-group "Display"
    :level 1
    :order 1)
   (mol
    :option-type :string
    :key "m"
    :prompt "Molecule ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Scope"
    :level 2
    :order 1)
   (mol-type
    :option-type :string
    :key "M"
    :choices '("swarm" "patrol" "work")
    :transient-group "Scope"
    :level 2
    :order 2)
   (parent
    :option-type :string
    :key "P"
    :prompt "Parent ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Scope"
    :level 2
    :order 3)
   (pretty
    :option-type :boolean
    :key "y"
    :transient-group "Display"
    :level 2
    :order 2)
   (priority
    :option-type :integer
    :short-option "p"
    :key "p"
    :transient-reader beads-reader-list-priority
    :transient-group "Filters"
    :level 1
    :order 2)
   (sort
    :option-type :string
    :short-option "s"
    :key "s"
    :choices '("hybrid" "priority" "oldest")
    :transient-group "Display"
    :level 1
    :order 3)
   (unassigned
    :option-type :boolean
    :short-option "u"
    :key "u"
    :transient-group "Filters"
    :level 1
    :order 3))
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

(cl-defmethod beads-command-parse ((command beads-command-ready) execution)
  "Parse ready COMMAND output from EXECUTION.
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
            (mapcar #'beads-issue-from-json (append parsed-json nil))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instances: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :parsed-json parsed-json
                         :stderr (oref execution stderr)
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
