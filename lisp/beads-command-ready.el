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
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee (-a, --assignee)."
    ;; CLI properties
    :long-option "assignee"
    :short-option "a"
    :option-type :string
    ;; Transient properties
    :key "a"
    :transient "--assignee"
    :class transient-option
    :argument "--assignee="
    :prompt "Assignee: "
    :transient-group "Filters"
    :level 1
    :order 1)
   (include-deferred
    :initarg :include-deferred
    :type boolean
    :initform nil
    :documentation "Include issues with future defer_until timestamps
(--include-deferred)."
    ;; CLI properties
    :long-option "include-deferred"
    :option-type :boolean
    ;; Transient properties
    :key "D"
    :transient "--include-deferred"
    :class transient-switch
    :argument "--include-deferred"
    :transient-group "Filters"
    :level 2
    :order 8)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Filter by issue type (-t, --type).
Values: task, bug, feature, epic, merge-request.
Aliases: mr→merge-request, feat→feature, mol→molecule."
    ;; CLI properties
    :long-option "type"
    :short-option "t"
    :option-type :string
    ;; Transient properties
    :key "T"
    :transient "--type"
    :class transient-option
    :argument "--type="
    :prompt "Type: "
    :transient-reader beads-reader-list-type
    :transient-group "Filters"
    :level 1
    :order 4)
   (label
    :initarg :label
    :type (or null list)
    :initform nil
    :documentation "Filter by labels, AND logic (-l, --label).
Must have ALL labels. Can combine with --label-any."
    ;; CLI properties
    :long-option "label"
    :short-option "l"
    :option-type :list
    ;; Transient properties
    :key "l"
    :transient "--label (AND)"
    :class transient-option
    :argument "--label="
    :prompt "Label (AND): "
    :transient-reader beads-reader-list-label
    :transient-group "Label Filters"
    :level 2
    :order 1)
   (label-any
    :initarg :label-any
    :type (or null list)
    :initform nil
    :documentation "Filter by labels, OR logic (--label-any).
Must have AT LEAST ONE label. Can combine with --label."
    ;; CLI properties
    :long-option "label-any"
    :option-type :list
    ;; Transient properties
    :key "L"
    :transient "--label-any (OR)"
    :class transient-option
    :argument "--label-any="
    :prompt "Label (OR): "
    :transient-reader beads-reader-list-label
    :transient-group "Label Filters"
    :level 2
    :order 2)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Maximum issues to show (-n, --limit).
Default: 10."
    ;; CLI properties
    :long-option "limit"
    :short-option "n"
    :option-type :integer
    ;; Transient properties
    :key "n"
    :transient "--limit"
    :class transient-option
    :argument "--limit="
    :prompt "Limit: "
    :transient-group "Display"
    :level 1
    :order 1)
   (mol
    :initarg :mol
    :type (or null string)
    :initform nil
    :documentation "Filter to steps within a specific molecule (--mol).
Use for agents executing molecules to see which steps can run next."
    ;; CLI properties
    :long-option "mol"
    :option-type :string
    ;; Transient properties
    :key "m"
    :transient "--mol"
    :class transient-option
    :argument "--mol="
    :prompt "Molecule ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Scope"
    :level 2
    :order 1)
   (mol-type
    :initarg :mol-type
    :type (or null string)
    :initform nil
    :documentation "Filter by molecule type (--mol-type).
Values: swarm, patrol, work."
    ;; CLI properties
    :long-option "mol-type"
    :option-type :string
    ;; Transient properties
    :key "M"
    :transient "--mol-type"
    :class transient-option
    :argument "--mol-type="
    :prompt "Molecule type: "
    :choices '("swarm" "patrol" "work")
    :transient-group "Scope"
    :level 2
    :order 2)
   (parent
    :initarg :parent
    :type (or null string)
    :initform nil
    :documentation "Filter to descendants of this bead/epic (--parent)."
    ;; CLI properties
    :long-option "parent"
    :option-type :string
    ;; Transient properties
    :key "P"
    :transient "--parent"
    :class transient-option
    :argument "--parent="
    :prompt "Parent ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Scope"
    :level 2
    :order 3)
   (pretty
    :initarg :pretty
    :type boolean
    :initform nil
    :documentation "Display issues in tree format (--pretty)."
    ;; CLI properties
    :long-option "pretty"
    :option-type :boolean
    ;; Transient properties
    :key "y"
    :transient "--pretty"
    :class transient-switch
    :argument "--pretty"
    :transient-group "Display"
    :level 2
    :order 2)
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Filter by priority (-p, --priority).
Values: 0-4."
    ;; CLI properties
    :long-option "priority"
    :short-option "p"
    :option-type :integer
    ;; Transient properties
    :key "p"
    :transient "--priority"
    :class transient-option
    :argument "--priority="
    :prompt "Priority: "
    :transient-reader beads-reader-list-priority
    :transient-group "Filters"
    :level 1
    :order 2)
   (sort
    :initarg :sort
    :type (or null string)
    :initform nil
    :documentation "Sort policy (-s, --sort).
Values: hybrid (default), priority, oldest."
    ;; CLI properties
    :long-option "sort"
    :short-option "s"
    :option-type :string
    ;; Transient properties
    :key "s"
    :transient "--sort"
    :class transient-option
    :argument "--sort="
    :prompt "Sort: "
    :choices '("hybrid" "priority" "oldest")
    :transient-group "Display"
    :level 1
    :order 3)
   (unassigned
    :initarg :unassigned
    :type boolean
    :initform nil
    :documentation "Show only unassigned issues (-u, --unassigned)."
    ;; CLI properties
    :long-option "unassigned"
    :short-option "u"
    :option-type :boolean
    ;; Transient properties
    :key "u"
    :transient "--unassigned"
    :class transient-switch
    :argument "--unassigned"
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
