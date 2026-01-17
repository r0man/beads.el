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
(require 'beads-types)

;;; Ready Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-ready (beads-command-json)
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
    :transient-key "a"
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
    :long-option "include-deferred"
    :option-type :boolean
    ;; Transient properties
    :transient-key "D"
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
    :long-option "type"
    :short-option "t"
    :option-type :string
    ;; Transient properties
    :transient-key "T"
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
    :long-option "label"
    :short-option "l"
    :option-type :list
    ;; Transient properties
    :transient-key "l"
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
    :long-option "label-any"
    :option-type :list
    ;; Transient properties
    :transient-key "L"
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
    :long-option "limit"
    :short-option "n"
    :option-type :integer
    ;; Transient properties
    :transient-key "n"
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
    :long-option "mol"
    :option-type :string
    ;; Transient properties
    :transient-key "m"
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
    :long-option "mol-type"
    :option-type :string
    ;; Transient properties
    :transient-key "M"
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
    :long-option "parent"
    :option-type :string
    ;; Transient properties
    :transient-key "P"
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
    :long-option "pretty"
    :option-type :boolean
    ;; Transient properties
    :transient-key "y"
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
    :long-option "priority"
    :short-option "p"
    :option-type :integer
    ;; Transient properties
    :transient-key "p"
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
    :long-option "sort"
    :short-option "s"
    :option-type :string
    ;; Transient properties
    :transient-key "s"
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
    :long-option "unassigned"
    :short-option "u"
    :option-type :boolean
    ;; Transient properties
    :transient-key "u"
    :transient-description "--unassigned"
    :transient-class transient-switch
    :transient-argument "--unassigned"
    :transient-group "Filters"
    :transient-level 1
    :transient-order 3))
  :documentation "Represents bd ready command.
Shows ready work (no blockers, open or in-progress).
When executed with :json t, returns list of beads-issue instances."))

(cl-defmethod beads-command-subcommand ((_command beads-command-ready))
  "Return \"ready\" as the CLI subcommand name."
  "ready")

(cl-defmethod beads-command-line ((command beads-command-ready))
  "Build command arguments for ready COMMAND (without executable).
Returns list: (\"ready\" ...global-flags... ...flags...).

Note: This custom implementation is needed because list options
like --label need to be repeated for each value, not joined."
  (with-slots (assignee include-deferred issue-type label label-any
                        limit mol mol-type parent pretty priority
                        sort unassigned json) command
    (let ((args (list "ready"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Add --json if enabled (not handled by global-args for this class)
      (when json
        (setq args (append args (list "--json"))))

      ;; Boolean flags
      (when include-deferred
        (setq args (append args (list "--include-deferred"))))
      (when pretty
        (setq args (append args (list "--pretty"))))
      (when unassigned
        (setq args (append args (list "--unassigned"))))

      ;; String options
      (when assignee
        (setq args (append args (list "--assignee" assignee))))
      (when issue-type
        (setq args (append args (list "--type" issue-type))))
      (when mol
        (setq args (append args (list "--mol" mol))))
      (when mol-type
        (setq args (append args (list "--mol-type" mol-type))))
      (when parent
        (setq args (append args (list "--parent" parent))))
      (when sort
        (setq args (append args (list "--sort" sort))))

      ;; Integer options
      (when limit
        (setq args (append args (list "--limit" (number-to-string limit)))))
      (when priority
        (setq args (append args (list "--priority"
                                      (number-to-string priority)))))

      ;; List options (multiple values - repeat the flag for each)
      (when label
        (dolist (lbl label)
          (setq args (append args (list "--label" lbl)))))
      (when label-any
        (dolist (lbl label-any)
          (setq args (append args (list "--label-any" lbl)))))

      args)))

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

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-ready))
  "Execute CMD in compilation buffer with human-readable output.
Disables JSON mode for interactive display with colors."
  ;; Set json to nil for human-readable colored output
  (oset cmd json nil)
  ;; Call the default implementation
  (cl-call-next-method))

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
