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

(require 'beads-command)
(require 'beads-types)

;;; Ready Command

(defclass beads-command-ready (beads-command-json)
  ((assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee (-a, --assignee)."
    ;; CLI properties
    :long-option "--assignee"
    :short-option "-a"
    :option-type :string
    ;; Transient properties
    :transient-key "-a"
    :transient-description "--assignee"
    :transient-class transient-option
    :transient-argument "--assignee="
    :transient-prompt "Assignee: "
    :transient-group "Filters"
    :transient-level 1
    :transient-order 1)
   (include-deferred
    :initarg :include-deferred
    :type boolean
    :initform nil
    :documentation "Include issues with future defer_until timestamps
(--include-deferred)."
    ;; CLI properties
    :long-option "--include-deferred"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-D"
    :transient-description "--include-deferred"
    :transient-class transient-switch
    :transient-argument "--include-deferred"
    :transient-group "Filters"
    :transient-level 2
    :transient-order 8)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Filter by issue type (-t, --type).
Values: task, bug, feature, epic, merge-request.
Aliases: mr→merge-request, feat→feature, mol→molecule."
    ;; CLI properties
    :long-option "--type"
    :short-option "-t"
    :option-type :string
    ;; Transient properties
    :transient-key "-T"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Type: "
    :transient-group "Filters"
    :transient-level 1
    :transient-order 4)
   (label
    :initarg :label
    :type (or null list)
    :initform nil
    :documentation "Filter by labels, AND logic (-l, --label).
Must have ALL labels. Can combine with --label-any."
    ;; CLI properties
    :long-option "--label"
    :short-option "-l"
    :option-type :list
    ;; Transient properties
    :transient-key "-l"
    :transient-description "--label (AND)"
    :transient-class transient-option
    :transient-argument "--label="
    :transient-prompt "Label (AND): "
    :transient-group "Label Filters"
    :transient-level 2
    :transient-order 1)
   (label-any
    :initarg :label-any
    :type (or null list)
    :initform nil
    :documentation "Filter by labels, OR logic (--label-any).
Must have AT LEAST ONE label. Can combine with --label."
    ;; CLI properties
    :long-option "--label-any"
    :option-type :list
    ;; Transient properties
    :transient-key "-L"
    :transient-description "--label-any (OR)"
    :transient-class transient-option
    :transient-argument "--label-any="
    :transient-prompt "Label (OR): "
    :transient-group "Label Filters"
    :transient-level 2
    :transient-order 2)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Maximum issues to show (-n, --limit).
Default: 10."
    ;; CLI properties
    :long-option "--limit"
    :short-option "-n"
    :option-type :integer
    ;; Transient properties
    :transient-key "-n"
    :transient-description "--limit"
    :transient-class transient-option
    :transient-argument "--limit="
    :transient-prompt "Limit: "
    :transient-group "Display"
    :transient-level 1
    :transient-order 1)
   (mol
    :initarg :mol
    :type (or null string)
    :initform nil
    :documentation "Filter to steps within a specific molecule (--mol).
Use for agents executing molecules to see which steps can run next."
    ;; CLI properties
    :long-option "--mol"
    :option-type :string
    ;; Transient properties
    :transient-key "-m"
    :transient-description "--mol"
    :transient-class transient-option
    :transient-argument "--mol="
    :transient-prompt "Molecule ID: "
    :transient-group "Scope"
    :transient-level 2
    :transient-order 1)
   (mol-type
    :initarg :mol-type
    :type (or null string)
    :initform nil
    :documentation "Filter by molecule type (--mol-type).
Values: swarm, patrol, work."
    ;; CLI properties
    :long-option "--mol-type"
    :option-type :string
    ;; Transient properties
    :transient-key "-M"
    :transient-description "--mol-type"
    :transient-class transient-option
    :transient-argument "--mol-type="
    :transient-prompt "Molecule type: "
    :transient-choices '("swarm" "patrol" "work")
    :transient-group "Scope"
    :transient-level 2
    :transient-order 2)
   (parent
    :initarg :parent
    :type (or null string)
    :initform nil
    :documentation "Filter to descendants of this bead/epic (--parent)."
    ;; CLI properties
    :long-option "--parent"
    :option-type :string
    ;; Transient properties
    :transient-key "-P"
    :transient-description "--parent"
    :transient-class transient-option
    :transient-argument "--parent="
    :transient-prompt "Parent ID: "
    :transient-group "Scope"
    :transient-level 2
    :transient-order 3)
   (pretty
    :initarg :pretty
    :type boolean
    :initform nil
    :documentation "Display issues in tree format (--pretty)."
    ;; CLI properties
    :long-option "--pretty"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-y"
    :transient-description "--pretty"
    :transient-class transient-switch
    :transient-argument "--pretty"
    :transient-group "Display"
    :transient-level 2
    :transient-order 2)
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Filter by priority (-p, --priority).
Values: 0-4."
    ;; CLI properties
    :long-option "--priority"
    :short-option "-p"
    :option-type :integer
    ;; Transient properties
    :transient-key "-p"
    :transient-description "--priority"
    :transient-class transient-option
    :transient-argument "--priority="
    :transient-prompt "Priority: "
    :transient-group "Filters"
    :transient-level 1
    :transient-order 2)
   (sort
    :initarg :sort
    :type (or null string)
    :initform nil
    :documentation "Sort policy (-s, --sort).
Values: hybrid (default), priority, oldest."
    ;; CLI properties
    :long-option "--sort"
    :short-option "-s"
    :option-type :string
    ;; Transient properties
    :transient-key "-s"
    :transient-description "--sort"
    :transient-class transient-option
    :transient-argument "--sort="
    :transient-prompt "Sort: "
    :transient-choices '("hybrid" "priority" "oldest")
    :transient-group "Display"
    :transient-level 1
    :transient-order 3)
   (unassigned
    :initarg :unassigned
    :type boolean
    :initform nil
    :documentation "Show only unassigned issues (-u, --unassigned)."
    ;; CLI properties
    :long-option "--unassigned"
    :short-option "-u"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-u"
    :transient-description "--unassigned"
    :transient-class transient-switch
    :transient-argument "--unassigned"
    :transient-group "Filters"
    :transient-level 1
    :transient-order 3))
  :documentation "Represents bd ready command.
Shows ready work (no blockers, open or in-progress).
When executed with :json t, returns list of beads-issue instances.")

(cl-defmethod beads-command-subcommand ((_command beads-command-ready))
  "Return \"ready\" as the CLI subcommand name."
  "ready")

;; Note: Manual beads-command-line method removed - now uses automatic
;; generation from slot metadata via beads-command-json base class.
;; List options (--label, --label-any) are correctly handled by repeating
;; the flag for each value.

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

(cl-defmethod beads-command-parse ((command beads-command-ready))
  "Parse ready COMMAND output and return list of beads-issue instances.
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, converts parsed JSON to beads-issue instances.
Does not modify command slots."
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
                         :exit-code (oref command exit-code)
                         :parsed-json parsed-json
                         :stderr (oref command stderr)
                         :parse-error err))))))))

;;; Convenience Function

(defun beads-command-ready! (&rest args)
  "Create and execute a beads-command-ready with ARGS.
Returns a list of parsed issue objects.
See `beads-command-ready' for available arguments."
  (oref (beads-command-execute (apply #'beads-command-ready args)) data))

(provide 'beads-command-ready)
;;; beads-command-ready.el ends here
