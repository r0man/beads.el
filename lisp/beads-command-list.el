;;; beads-command-list.el --- List command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-list' EIEIO class for the
;; `bd list' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd list command lists issues with extensive filtering and
;; output formatting options.
;;
;; Features:
;; - Filter by status, type, assignee, priority, labels
;; - Date range filters (created, updated, closed, due, defer)
;; - Content search (title, description, notes)
;; - Special filters (ready, overdue, deferred, pinned)
;; - Multiple output formats (table, tree, long, dot, digraph)
;; - Sorting and pagination
;;
;; Usage:
;;   (beads-command-execute (beads-command-list :status "open"))
;;   (beads-command-list!)  ; convenience function

;;; Code:

(require 'beads)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-sesman)
(require 'beads-types)
(require 'transient)

;; Forward declarations for UI code
(declare-function beads-update "beads-command-update" (&optional issue-id))
(declare-function beads-reopen "beads-command-reopen" (&optional issue-id))
(declare-function beads-agent--get-sessions-for-issue "beads-agent")
(declare-function beads-agent--get-sessions-focused-on-issue "beads-agent-backend")
(declare-function beads-agent--get-sessions-touching-issue "beads-agent-backend")
(declare-function beads-agent-session-instance-number "beads-agent-backend")
(declare-function beads-agent-start-at-point "beads-agent")
(declare-function beads-agent-start-task "beads-agent" (&optional arg))
(declare-function beads-agent-start-review "beads-agent" (&optional arg))
(declare-function beads-agent-start-plan "beads-agent" (&optional arg))
(declare-function beads-agent-start-qa "beads-agent" (&optional arg))
(declare-function beads-agent-start-custom "beads-agent" (&optional arg))
(declare-function beads-agent-stop-at-point "beads-agent")
(declare-function beads-agent-jump-at-point "beads-agent")
(declare-function beads-agent--get-issue-outcome "beads-agent-backend")
(declare-function beads-agent-session-backend-name "beads-agent-backend")
(declare-function beads-agent-session-type-name "beads-agent-backend")
(declare-function beads-show--find-visible-buffer "beads-command-show"
                  (&optional project-dir))
(declare-function beads-show-update-buffer "beads-command-show"
                  (issue-id buffer))

;;; List Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-list (beads-command-json)
  (;; === Basic Filters ===
   (all
    :initarg :all
    :type boolean
    :initform nil
    :documentation "Show all issues including closed (--all).
Overrides default filter that excludes closed issues."
    ;; CLI properties
    :long-option "all"
    :option-type :boolean
    ;; Transient properties
    :transient-key "A"
    :transient-description "--all"
    :transient-class transient-switch
    :transient-argument "--all"
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 1)
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status (-s, --status).
Values: open, in_progress, blocked, deferred, closed."
    ;; CLI properties
    :long-option "status"
    :short-option "s"
    :option-type :string
    ;; Transient properties
    :transient-key "s"
    :transient-description "--status"
    :transient-class transient-option
    :transient-argument "--status="
    :transient-prompt "Status: "
    :transient-reader beads-reader-list-status
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 2)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Filter by type (-t, --type).
Values: bug, feature, task, epic, chore, merge-request, molecule,
gate, convoy.  Aliases: mr→merge-request, feat→feature, mol→molecule."
    ;; CLI properties
    :long-option "type"
    :short-option "t"
    :option-type :string
    ;; Transient properties
    :transient-key "t"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Type: "
    :transient-reader beads-reader-issue-type
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 3)
   (assignee
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
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 4)
   (priority
    :initarg :priority
    :type (or null string)
    :initform nil
    :documentation "Filter by priority (-p, --priority).
Values: 0-4 or P0-P4 (0=highest)."
    ;; CLI properties
    :long-option "priority"
    :short-option "p"
    :option-type :string
    ;; Transient properties
    :transient-key "p"
    :transient-description "--priority"
    :transient-class transient-option
    :transient-argument "--priority="
    :transient-prompt "Priority (0-4 or P0-P4): "
    :transient-reader beads-reader-priority
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 5)
   (label
    :initarg :label
    :type (or null list)
    :initform nil
    :documentation "Filter by labels - AND logic (-l, --label).
Must have ALL specified labels.  Can combine with --label-any."
    ;; CLI properties
    :long-option "label"
    :short-option "l"
    :option-type :list
    ;; Transient properties
    :transient-key "l"
    :transient-description "--label"
    :transient-class transient-option
    :transient-argument "--label="
    :transient-prompt "Labels (AND): "
    :transient-reader beads-reader-issue-labels
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 6)
   (label-any
    :initarg :label-any
    :type (or null list)
    :initform nil
    :documentation "Filter by labels - OR logic (--label-any).
Must have AT LEAST ONE of specified labels.  Can combine with --label."
    ;; CLI properties
    :long-option "label-any"
    :option-type :list
    ;; Transient properties
    :transient-key "L"
    :transient-description "--label-any"
    :transient-class transient-option
    :transient-argument "--label-any="
    :transient-prompt "Labels (OR): "
    :transient-reader beads-reader-issue-labels
    :transient-group "Basic Filters"
    :transient-level 2
    :transient-order 1)
   (id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Filter by specific issue IDs (--id).
Comma-separated, e.g., bd-1,bd-5,bd-10."
    ;; CLI properties
    :long-option "id"
    :option-type :string
    ;; Transient properties
    :transient-key "i"
    :transient-description "--id"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue IDs (comma-separated): "
    :transient-group "Basic Filters"
    :transient-level 2
    :transient-order 2)
   (parent
    :initarg :parent
    :type (or null string)
    :initform nil
    :documentation "Filter by parent issue ID (--parent).
Shows children of specified issue."
    ;; CLI properties
    :long-option "parent"
    :option-type :string
    ;; Transient properties
    :transient-key "P"
    :transient-description "--parent"
    :transient-class transient-option
    :transient-argument "--parent="
    :transient-prompt "Parent ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Basic Filters"
    :transient-level 2
    :transient-order 3)
   (ready
    :initarg :ready
    :type boolean
    :initform nil
    :documentation "Show only ready issues (--ready).
Status=open, excludes hooked/in_progress/blocked/deferred."
    ;; CLI properties
    :long-option "ready"
    :option-type :boolean
    ;; Transient properties
    :transient-key "R"
    :transient-description "--ready"
    :transient-class transient-switch
    :transient-argument "--ready"
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 7)

   ;; === Date Filters ===
   (created-after
    :initarg :created-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues created after date (--created-after).
Format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "created-after"
    :option-type :string
    ;; Transient properties
    :transient-key "ca"
    :transient-description "--created-after"
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
    ;; CLI properties
    :long-option "created-before"
    :option-type :string
    ;; Transient properties
    :transient-key "cb"
    :transient-description "--created-before"
    :transient-class transient-option
    :transient-argument "--created-before="
    :transient-prompt "Created before (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 2)
   (updated-after
    :initarg :updated-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated after date (--updated-after).
Format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "updated-after"
    :option-type :string
    ;; Transient properties
    :transient-key "ua"
    :transient-description "--updated-after"
    :transient-class transient-option
    :transient-argument "--updated-after="
    :transient-prompt "Updated after (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 3)
   (updated-before
    :initarg :updated-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated before date (--updated-before).
Format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "updated-before"
    :option-type :string
    ;; Transient properties
    :transient-key "ub"
    :transient-description "--updated-before"
    :transient-class transient-option
    :transient-argument "--updated-before="
    :transient-prompt "Updated before (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 4)
   (closed-after
    :initarg :closed-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed after date (--closed-after).
Format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "closed-after"
    :option-type :string
    ;; Transient properties
    :transient-key "xa"
    :transient-description "--closed-after"
    :transient-class transient-option
    :transient-argument "--closed-after="
    :transient-prompt "Closed after (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 5)
   (closed-before
    :initarg :closed-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed before date (--closed-before).
Format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "closed-before"
    :option-type :string
    ;; Transient properties
    :transient-key "xb"
    :transient-description "--closed-before"
    :transient-class transient-option
    :transient-argument "--closed-before="
    :transient-prompt "Closed before (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 6)
   (due-after
    :initarg :due-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues due after date (--due-after).
Supports relative: +6h, tomorrow."
    ;; CLI properties
    :long-option "due-after"
    :option-type :string
    ;; Transient properties
    :transient-key "da"
    :transient-description "--due-after"
    :transient-class transient-option
    :transient-argument "--due-after="
    :transient-prompt "Due after: "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 7)
   (due-before
    :initarg :due-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues due before date (--due-before).
Supports relative: +6h, tomorrow."
    ;; CLI properties
    :long-option "due-before"
    :option-type :string
    ;; Transient properties
    :transient-key "db"
    :transient-description "--due-before"
    :transient-class transient-option
    :transient-argument "--due-before="
    :transient-prompt "Due before: "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 8)
   (defer-after
    :initarg :defer-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues deferred after date (--defer-after).
Supports relative: +6h, tomorrow."
    ;; CLI properties
    :long-option "defer-after"
    :option-type :string
    ;; Transient properties
    :transient-key "fa"
    :transient-description "--defer-after"
    :transient-class transient-option
    :transient-argument "--defer-after="
    :transient-prompt "Deferred after: "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 9)
   (defer-before
    :initarg :defer-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues deferred before date (--defer-before).
Supports relative: +6h, tomorrow."
    ;; CLI properties
    :long-option "defer-before"
    :option-type :string
    ;; Transient properties
    :transient-key "fb"
    :transient-description "--defer-before"
    :transient-class transient-option
    :transient-argument "--defer-before="
    :transient-prompt "Deferred before: "
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 10)

   ;; === Content Filters ===
   (title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Filter by title text (--title).
Case-insensitive substring match."
    ;; CLI properties
    :long-option "title"
    :option-type :string
    ;; Transient properties
    :transient-key "T"
    :transient-description "--title"
    :transient-class transient-option
    :transient-argument "--title="
    :transient-prompt "Title contains: "
    :transient-group "Content Filters"
    :transient-level 2
    :transient-order 1)
   (title-contains
    :initarg :title-contains
    :type (or null string)
    :initform nil
    :documentation "Filter by title substring (--title-contains).
Case-insensitive."
    ;; CLI properties
    :long-option "title-contains"
    :option-type :string
    ;; Transient properties
    :transient-key "tc"
    :transient-description "--title-contains"
    :transient-class transient-option
    :transient-argument "--title-contains="
    :transient-prompt "Title contains: "
    :transient-group "Content Filters"
    :transient-level 3
    :transient-order 1)
   (desc-contains
    :initarg :desc-contains
    :type (or null string)
    :initform nil
    :documentation "Filter by description substring (--desc-contains).
Case-insensitive."
    ;; CLI properties
    :long-option "desc-contains"
    :option-type :string
    ;; Transient properties
    :transient-key "dc"
    :transient-description "--desc-contains"
    :transient-class transient-option
    :transient-argument "--desc-contains="
    :transient-prompt "Description contains: "
    :transient-group "Content Filters"
    :transient-level 2
    :transient-order 2)
   (notes-contains
    :initarg :notes-contains
    :type (or null string)
    :initform nil
    :documentation "Filter by notes substring (--notes-contains).
Case-insensitive."
    ;; CLI properties
    :long-option "notes-contains"
    :option-type :string
    ;; Transient properties
    :transient-key "nc"
    :transient-description "--notes-contains"
    :transient-class transient-option
    :transient-argument "--notes-contains="
    :transient-prompt "Notes contains: "
    :transient-group "Content Filters"
    :transient-level 3
    :transient-order 2)
   (empty-description
    :initarg :empty-description
    :type boolean
    :initform nil
    :documentation "Filter issues with empty/missing description
(--empty-description)."
    ;; CLI properties
    :long-option "empty-description"
    :option-type :boolean
    ;; Transient properties
    :transient-key "ed"
    :transient-description "--empty-description"
    :transient-class transient-switch
    :transient-argument "--empty-description"
    :transient-group "Content Filters"
    :transient-level 3
    :transient-order 3)

   ;; === Special Filters ===
   (no-assignee
    :initarg :no-assignee
    :type boolean
    :initform nil
    :documentation "Filter issues with no assignee (--no-assignee)."
    ;; CLI properties
    :long-option "no-assignee"
    :option-type :boolean
    ;; Transient properties
    :transient-key "na"
    :transient-description "--no-assignee"
    :transient-class transient-switch
    :transient-argument "--no-assignee"
    :transient-group "Special Filters"
    :transient-level 2
    :transient-order 1)
   (no-labels
    :initarg :no-labels
    :type boolean
    :initform nil
    :documentation "Filter issues with no labels (--no-labels)."
    ;; CLI properties
    :long-option "no-labels"
    :option-type :boolean
    ;; Transient properties
    :transient-key "nl"
    :transient-description "--no-labels"
    :transient-class transient-switch
    :transient-argument "--no-labels"
    :transient-group "Special Filters"
    :transient-level 2
    :transient-order 2)
   (pinned
    :initarg :pinned
    :type boolean
    :initform nil
    :documentation "Show only pinned issues (--pinned)."
    ;; CLI properties
    :long-option "pinned"
    :option-type :boolean
    ;; Transient properties
    :transient-key "pi"
    :transient-description "--pinned"
    :transient-class transient-switch
    :transient-argument "--pinned"
    :transient-group "Special Filters"
    :transient-level 2
    :transient-order 3)
   (no-pinned
    :initarg :no-pinned
    :type boolean
    :initform nil
    :documentation "Exclude pinned issues (--no-pinned)."
    ;; CLI properties
    :long-option "no-pinned"
    :option-type :boolean
    ;; Transient properties
    :transient-key "np"
    :transient-description "--no-pinned"
    :transient-class transient-switch
    :transient-argument "--no-pinned"
    :transient-group "Special Filters"
    :transient-level 3
    :transient-order 1)
   (overdue
    :initarg :overdue
    :type boolean
    :initform nil
    :documentation "Show only overdue issues (--overdue).
Due_at in the past and not closed."
    ;; CLI properties
    :long-option "overdue"
    :option-type :boolean
    ;; Transient properties
    :transient-key "od"
    :transient-description "--overdue"
    :transient-class transient-switch
    :transient-argument "--overdue"
    :transient-group "Special Filters"
    :transient-level 2
    :transient-order 4)
   (deferred
    :initarg :deferred
    :type boolean
    :initform nil
    :documentation "Show only issues with defer_until set (--deferred)."
    ;; CLI properties
    :long-option "deferred"
    :option-type :boolean
    ;; Transient properties
    :transient-key "df"
    :transient-description "--deferred"
    :transient-class transient-switch
    :transient-argument "--deferred"
    :transient-group "Special Filters"
    :transient-level 2
    :transient-order 5)
   (priority-min
    :initarg :priority-min
    :type (or null string)
    :initform nil
    :documentation "Filter by minimum priority (--priority-min).
Inclusive, 0-4 or P0-P4."
    ;; CLI properties
    :long-option "priority-min"
    :option-type :string
    ;; Transient properties
    :transient-key "pm"
    :transient-description "--priority-min"
    :transient-class transient-option
    :transient-argument "--priority-min="
    :transient-prompt "Min priority (0-4 or P0-P4): "
    :transient-reader beads-reader-priority
    :transient-group "Special Filters"
    :transient-level 3
    :transient-order 2)
   (priority-max
    :initarg :priority-max
    :type (or null string)
    :initform nil
    :documentation "Filter by maximum priority (--priority-max).
Inclusive, 0-4 or P0-P4."
    ;; CLI properties
    :long-option "priority-max"
    :option-type :string
    ;; Transient properties
    :transient-key "px"
    :transient-description "--priority-max"
    :transient-class transient-option
    :transient-argument "--priority-max="
    :transient-prompt "Max priority (0-4 or P0-P4): "
    :transient-reader beads-reader-priority
    :transient-group "Special Filters"
    :transient-level 3
    :transient-order 3)
   (mol-type
    :initarg :mol-type
    :type (or null string)
    :initform nil
    :documentation "Filter by molecule type (--mol-type).
Values: swarm, patrol, or work."
    ;; CLI properties
    :long-option "mol-type"
    :option-type :string
    ;; Transient properties
    :transient-key "mt"
    :transient-description "--mol-type"
    :transient-class transient-option
    :transient-argument "--mol-type="
    :transient-prompt "Molecule type: "
    :transient-group "Special Filters"
    :transient-level 3
    :transient-order 4)
   (include-gates
    :initarg :include-gates
    :type boolean
    :initform nil
    :documentation "Include gate issues in output (--include-gates).
Normally hidden."
    ;; CLI properties
    :long-option "include-gates"
    :option-type :boolean
    ;; Transient properties
    :transient-key "ig"
    :transient-description "--include-gates"
    :transient-class transient-switch
    :transient-argument "--include-gates"
    :transient-group "Special Filters"
    :transient-level 3
    :transient-order 5)
   (include-templates
    :initarg :include-templates
    :type boolean
    :initform nil
    :documentation "Include template molecules in output
(--include-templates)."
    ;; CLI properties
    :long-option "include-templates"
    :option-type :boolean
    ;; Transient properties
    :transient-key "it"
    :transient-description "--include-templates"
    :transient-class transient-switch
    :transient-argument "--include-templates"
    :transient-group "Special Filters"
    :transient-level 3
    :transient-order 6)

   ;; === Output Options ===
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit results (-n, --limit).
Default 50, use 0 for unlimited."
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
    :transient-group "Output Options"
    :transient-level 1
    :transient-order 1)
   (sort
    :initarg :sort
    :type (or null string)
    :initform nil
    :documentation "Sort by field (--sort).
Values: priority, created, updated, closed, status, id, title,
type, assignee."
    ;; CLI properties
    :long-option "sort"
    :option-type :string
    ;; Transient properties
    :transient-key "S"
    :transient-description "--sort"
    :transient-class transient-option
    :transient-argument "--sort="
    :transient-prompt "Sort by: "
    :transient-group "Output Options"
    :transient-level 1
    :transient-order 2)
   (reverse
    :initarg :reverse
    :type boolean
    :initform nil
    :documentation "Reverse sort order (-r, --reverse)."
    ;; CLI properties
    :long-option "reverse"
    :short-option "r"
    :option-type :boolean
    ;; Transient properties
    :transient-key "r"
    :transient-description "--reverse"
    :transient-class transient-switch
    :transient-argument "--reverse"
    :transient-group "Output Options"
    :transient-level 1
    :transient-order 3)
   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Output format (--format).
Values: 'digraph' (for golang.org/x/tools/cmd/digraph),
'dot' (Graphviz), or Go template."
    ;; CLI properties
    :long-option "format"
    :option-type :string
    ;; Transient properties
    :transient-key "F"
    :transient-description "--format"
    :transient-class transient-option
    :transient-argument "--format="
    :transient-prompt "Format: "
    :transient-group "Output Options"
    :transient-level 2
    :transient-order 1)
   (long
    :initarg :long
    :type boolean
    :initform nil
    :documentation "Show detailed multi-line output (--long)."
    ;; CLI properties
    :long-option "long"
    :option-type :boolean
    ;; Transient properties
    :transient-key "lo"
    :transient-description "--long"
    :transient-class transient-switch
    :transient-argument "--long"
    :transient-group "Output Options"
    :transient-level 2
    :transient-order 2)
   (pretty
    :initarg :pretty
    :type boolean
    :initform nil
    :documentation "Display issues in tree format (--pretty).
Shows status/priority symbols."
    ;; CLI properties
    :long-option "pretty"
    :option-type :boolean
    ;; Transient properties
    :transient-key "pr"
    :transient-description "--pretty"
    :transient-class transient-switch
    :transient-argument "--pretty"
    :transient-group "Output Options"
    :transient-level 2
    :transient-order 3)
   (tree
    :initarg :tree
    :type boolean
    :initform nil
    :documentation "Alias for --pretty: hierarchical tree format (--tree)."
    ;; CLI properties
    :long-option "tree"
    :option-type :boolean
    ;; Transient properties
    :transient-key "tr"
    :transient-description "--tree"
    :transient-class transient-switch
    :transient-argument "--tree"
    :transient-group "Output Options"
    :transient-level 3
    :transient-order 1)
   (no-pager
    :initarg :no-pager
    :type boolean
    :initform nil
    :documentation "Disable pager output (--no-pager)."
    ;; CLI properties
    :long-option "no-pager"
    :option-type :boolean
    ;; Transient properties
    :transient-key "nP"
    :transient-description "--no-pager"
    :transient-class transient-switch
    :transient-argument "--no-pager"
    :transient-group "Output Options"
    :transient-level 3
    :transient-order 2)
   (watch
    :initarg :watch
    :type boolean
    :initform nil
    :documentation "Watch for changes and auto-update (-w, --watch).
Implies --pretty."
    ;; CLI properties
    :long-option "watch"
    :short-option "w"
    :option-type :boolean
    ;; Transient properties
    :transient-key "w"
    :transient-description "--watch"
    :transient-class transient-switch
    :transient-argument "--watch"
    :transient-group "Output Options"
    :transient-level 2
    :transient-order 4))
  :documentation "Represents bd list command.
Lists issues with extensive filtering and output formatting options.
When executed with :json t, returns list of beads-issue instances."))

(cl-defmethod beads-command-subcommand ((_command beads-command-list))
  "Return \"list\" as the CLI subcommand name."
  "list")

(cl-defmethod beads-command-validate ((command beads-command-list))
  "Validate list COMMAND.
Checks for conflicts between options.
Returns error string or nil if valid."
  (with-slots (priority priority-max priority-min
                        assignee no-assignee
                        label label-any no-labels) command
    (cl-flet ((valid-priority-p (p)
                "Check if P is a valid priority (0-4).
P can be a number or string representation."
                (let ((n (if (stringp p) (string-to-number p) p)))
                  (and (numberp n) (<= 0 n 4)))))
      (or
       ;; Can't use --priority with --priority-min/max
       (and priority (or priority-max priority-min)
            "Cannot use --priority with --priority-min/--priority-max")
       ;; Can't use --assignee with --no-assignee
       (and assignee no-assignee
            "Cannot use both --assignee and --no-assignee")
       ;; Can't use --label/--label-any with --no-labels
       (and no-labels (or label label-any)
            "Cannot use --label/--label-any with --no-labels")
       ;; Validate priority range
       (and priority (not (valid-priority-p priority))
            "Priority must be between 0 and 4")
       (and priority-min (not (valid-priority-p priority-min))
            "Priority-min must be between 0 and 4")
       (and priority-max (not (valid-priority-p priority-max))
            "Priority-max must be between 0 and 4")
       ;; Validate list content types
       (beads-command--validate-string-list label "label")
       (beads-command--validate-string-list label-any "label-any")))))

(cl-defmethod beads-command-parse ((command beads-command-list) execution)
  "Parse list COMMAND output from EXECUTION.
Returns list of beads-issue instances.
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, returns list of beads-issue instances.
Does not modify any slots."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON, then convert to beads-issue instances
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (cond
             ;; Array result - convert to issue objects
             ((eq (type-of parsed-json) 'vector)
              (mapcar #'beads-issue-from-json (append parsed-json nil)))
             ;; Empty or null
             ((or (null parsed-json) (eq parsed-json :null))
              nil)
             ;; Unexpected structure
             (t
              (signal 'beads-json-parse-error
                      (list "Unexpected JSON structure from bd list"
                            :exit-code (oref execution exit-code)
                            :parsed-json parsed-json
                            :stderr (oref execution stderr)))))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to parse list result: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :parsed-json parsed-json
                         :stderr (oref execution stderr)
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-list))
  "Execute CMD in terminal buffer with human-readable output.
Disables JSON mode for interactive display with colors."
  ;; Set json to nil for human-readable colored output
  (oset cmd json nil)
  ;; Call the default implementation
  (cl-call-next-method))

;; Override auto-generated beads-command-list! to apply default limit.
;; Note: Using initialize-instance doesn't work well because:
;; 1. EIEIO validates :initform types at class definition time (symbol fails)
;; 2. :around/:after methods have complex slot argument handling
;; The function override is the cleanest solution that reliably works.
(defun beads-command-list! (&rest args)
  "Execute `beads-command-list' and return result data.

ARGS are passed to the constructor.  When :limit is not specified,
uses `beads-list-default-limit' as the default value.  Pass `:limit nil'
explicitly to disable the limit.

This function overrides the auto-generated version to support
the `beads-list-default-limit' customization variable."
  (unless (plist-member args :limit)
    (setq args (plist-put args :limit beads-list-default-limit)))
  (oref (beads-command-execute (apply #'beads-command-list args)) result))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-list-transient "beads-command-list" nil t)
(beads-meta-define-transient beads-command-list "beads-list-transient"
  "List issues with filtering and formatting options.

Supports extensive filtering by status, type, assignee, priority,
labels, dates, and content.  Output can be formatted as table, tree,
or graph formats.

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Basic filters (all, status, type, assignee, priority,
           label, ready), output (limit, sort, reverse)
  Level 2: Advanced filters (label-any, id, parent, title, desc,
           no-assignee, no-labels, pinned, overdue, deferred),
           output (format, long, pretty, watch)
  Level 3: Date filters, special filters, output options"
  beads-option-global-section)

;;; Customization

(defgroup beads-list nil
  "Tabulated list display for Beads issues."
  :group 'beads
  :prefix "beads-list-")

(defcustom beads-list-id-width 18
  "Width of ID column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-status-width 11
  "Width of Status column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-priority-width 8
  "Width of Priority column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-type-width 10
  "Width of Type column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-title-width 60
  "Width of Title column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-created-width 18
  "Width of Created column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-updated-width 18
  "Width of Updated column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-agent-width 15
  "Width of Agents column in issue lists.
Shows agent type and number for each active session (e.g., T#1 R#2).
Default width accommodates ~3 agents (5 chars each with spaces)."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-date-format 'absolute
  "Format for displaying creation dates in issue lists.

Options:
  `absolute'  - Absolute date and time (2025-10-20 16:36)
  `relative'  - Relative time (2 hours ago, 3 days ago)
  `iso'       - Full ISO 8601 timestamp (2025-10-20T16:36:52Z)
  `date-only' - Date without time (2025-10-20)
  string      - Custom format string for `format-time-string'

The `absolute' format sorts correctly in chronological order."
  :type '(choice (const :tag "Absolute (YYYY-MM-DD HH:MM)" absolute)
                 (const :tag "Relative (X hours/days ago)" relative)
                 (const :tag "ISO 8601 (full timestamp)" iso)
                 (const :tag "Date only (YYYY-MM-DD)" date-only)
                 (string :tag "Custom format-time-string"))
  :group 'beads-list)

(defcustom beads-list-update-show-delay 0.2
  "Delay in seconds before updating show buffer during navigation.
When holding down navigation keys, this prevents updating on every
intermediate position.  Modeled after `magit-update-other-window-delay'."
  :type 'number
  :group 'beads-list)

;;; Faces

(defface beads-list-status-open
  '((t :inherit font-lock-keyword-face))
  "Face for open status."
  :group 'beads-list)

(defface beads-list-status-in-progress
  '((t :inherit font-lock-warning-face))
  "Face for in_progress status."
  :group 'beads-list)

(defface beads-list-status-blocked
  '((t :inherit error))
  "Face for blocked status."
  :group 'beads-list)

(defface beads-list-status-closed
  '((t :inherit shadow))
  "Face for closed status."
  :group 'beads-list)

(defface beads-list-priority-critical
  '((t :inherit error :weight bold))
  "Face for priority 0 (critical)."
  :group 'beads-list)

(defface beads-list-priority-high
  '((t :inherit warning :weight bold))
  "Face for priority 1 (high)."
  :group 'beads-list)

(defface beads-list-priority-medium
  '((t :inherit default))
  "Face for priority 2 (medium)."
  :group 'beads-list)

(defface beads-list-priority-low
  '((t :inherit shadow))
  "Face for priority 3-4 (low/backlog)."
  :group 'beads-list)

(defface beads-list-agent-working
  '((t :inherit warning :weight bold))
  "Face for agent working indicator (yellow circle).
Inherits from `warning' face for theme consistency."
  :group 'beads-list)

(defface beads-list-agent-finished
  '((t :inherit success :weight bold))
  "Face for agent finished indicator (green circle).
Inherits from `success' face for theme consistency."
  :group 'beads-list)

(defface beads-list-agent-failed
  '((t :inherit error :weight bold))
  "Face for agent failed indicator (red circle).
Inherits from `error' face for theme consistency."
  :group 'beads-list)

;;; Variables

(defvar-local beads-list--command nil
  "The bd command used to populate this buffer (list, ready, or blocked).")

(defvar-local beads-list--raw-issues nil
  "List of beads-issue objects for the current buffer.")

(defvar-local beads-list--marked-issues nil
  "List of marked issue IDs.")

(defvar-local beads-list--command-obj nil
  "Current beads-command-list object (nil means no filter).
Use for client-side filtering in the buffer.")

;; Directory-aware buffer identity (beads.el-n3lv)
;; Key principle: Directory is identity, branch is metadata.

(defvar-local beads-list--project-dir nil
  "Project root directory for this buffer.
THIS IS THE IDENTITY - buffer lookup is by project-dir.")

(defvar-local beads-list--branch nil
  "Git branch name.
This is METADATA for display, not identity.
Updated on refresh to reflect current branch.")

(defvar-local beads-list--proj-name nil
  "Project name for buffer disambiguation.
Used when multiple projects have list buffers open.")

(defvar-local beads-list--pending-show-update nil
  "Pending show buffer update, or nil.
When non-nil, a cons cell (ISSUE-ID . BUFFER) indicating a scheduled
update.  Uses magit-style coalescing: rapid navigation updates the
pending target rather than scheduling multiple timers.")

(defvar-local beads-list--pending-show-timer nil
  "Timer for pending show buffer update, or nil.
Stored so we can cancel it when `beads-list-follow-mode' is disabled.")

;;; Buffer Lookup by Project Directory
;;
;; These functions find or create list buffers based on project directory,
;; NOT buffer name.  Same directory = same buffer, regardless of branch.

(defun beads-list--normalize-directory (dir)
  "Normalize DIR for consistent comparison.
Strips trailing slashes and expands to absolute path."
  (directory-file-name (expand-file-name dir)))

(defun beads-list--find-buffer-for-project (buffer-type project-dir)
  "Find existing buffer for BUFFER-TYPE and PROJECT-DIR.
BUFFER-TYPE is a symbol: `list', `ready', or `blocked'.
Return buffer or nil if not found."
  (let ((normalized-dir (beads-list--normalize-directory project-dir)))
    (cl-find-if
     (lambda (buf)
       (with-current-buffer buf
         (and (derived-mode-p 'beads-list-mode)
              beads-list--project-dir
              (equal (beads-list--normalize-directory beads-list--project-dir)
                     normalized-dir)
              (eq beads-list--command buffer-type))))
     (buffer-list))))

(defun beads-list--get-or-create-buffer (buffer-type)
  "Get or create list buffer for current project context.
BUFFER-TYPE is a symbol: `list', `ready', or `blocked'.
Reuses existing buffer for same project-dir (directory is identity)."
  (let* ((project-dir (or (beads-git-find-project-root) default-directory))
         (existing (beads-list--find-buffer-for-project buffer-type project-dir)))
    (or existing
        (let* ((proj-name (beads-git-get-project-name))
               (buf-name (beads-buffer-name-list
                          (symbol-name buffer-type)
                          nil
                          proj-name))
               (buffer (get-buffer-create buf-name)))
          (with-current-buffer buffer
            (setq beads-list--project-dir project-dir)
            (setq beads-list--branch (beads-git-get-branch))
            (setq beads-list--proj-name proj-name))
          buffer))))

(defun beads-list--display-buffer (buffer)
  "Display BUFFER in a sensible way.
If BUFFER is already visible in a window, select that window.
If not visible, display it in the current window without splitting."
  (beads-buffer-display-same-or-reuse buffer))

;;; Utilities

(defun beads-list--status-face (status)
  "Return face for STATUS."
  (pcase status
    ("open" 'beads-list-status-open)
    ("in_progress" 'beads-list-status-in-progress)
    ("blocked" 'beads-list-status-blocked)
    ("closed" 'beads-list-status-closed)
    (_ 'default)))

(defun beads-list--priority-face (priority)
  "Return face for PRIORITY."
  (pcase priority
    (0 'beads-list-priority-critical)
    (1 'beads-list-priority-high)
    (2 'beads-list-priority-medium)
    ((or 3 4) 'beads-list-priority-low)
    (_ 'default)))

(defun beads-list--format-status (status)
  "Format STATUS with appropriate face."
  (let ((status-str (or status "")))
    (propertize status-str 'face (beads-list--status-face status-str))))

(defun beads-list--format-priority (priority)
  "Format PRIORITY with appropriate face."
  (let ((priority-str (if priority (format "P%d" priority) "")))
    (propertize priority-str 'face (beads-list--priority-face priority))))

(defun beads-list--format-date (iso-timestamp)
  "Format ISO-TIMESTAMP according to `beads-list-date-format'.
ISO-TIMESTAMP should be an ISO 8601 string like
'2025-10-20T16:36:52.648609367Z'.  Returns a formatted string based on
the value of `beads-list-date-format'."
  (if (or (not iso-timestamp) (string-empty-p iso-timestamp))
      ""
    (let ((time (date-to-time iso-timestamp)))
      (pcase beads-list-date-format
        ('absolute
         (format-time-string "%Y-%m-%d %H:%M" time t))
        ('relative
         (let* ((now (current-time))
                (diff (time-subtract now time))
                (seconds (time-to-seconds diff)))
           (cond
            ((< seconds 60)
             (format "%ds ago" (floor seconds)))
            ((< seconds 3600)
             (format "%dm ago" (floor (/ seconds 60))))
            ((< seconds 86400)
             (let ((hours (floor (/ seconds 3600))))
               (format "%dh ago" hours)))
            ((< seconds 604800)
             (let ((days (floor (/ seconds 86400))))
               (format "%dd ago" days)))
            ((< seconds 2592000)
             (let ((weeks (floor (/ seconds 604800))))
               (format "%dw ago" weeks)))
            ((< seconds 31536000)
             (let ((months (floor (/ seconds 2592000))))
               (format "%dmo ago" months)))
            (t
             (let ((years (floor (/ seconds 31536000))))
               (format "%dy ago" years))))))
        ('iso
         (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))
        ('date-only
         (format-time-string "%Y-%m-%d" time))
        ((pred stringp)
         (format-time-string beads-list-date-format time))
        (_
         (format-time-string "%Y-%m-%d %H:%M" time))))))

(defun beads-list--format-agent-indicator (type-name instance-n face
                                                       &optional brief)
  "Format a single agent indicator.
TYPE-NAME is the agent type (e.g., \"Task\").
INSTANCE-N is the session instance number.
FACE is the face to apply.
If BRIEF is non-nil, show just the letter without instance number.
Returns a propertized string like \"T#1\" or \"T\" if BRIEF."
  (let* ((letter (if type-name (substring type-name 0 1) "●"))
         (indicator (if (or brief (not instance-n))
                        letter
                      (format "%s#%d" letter instance-n))))
    (propertize indicator 'face face)))

(defun beads-list--format-agent (issue-id)
  "Format agent status indicator for ISSUE-ID.
Uses directory-bound session model:
  - Shows focused sessions (current-issue = this issue) in yellow
  - Shows touched sessions (issue in touched-issues) in dim
  - Falls back to legacy issue-bound lookup for backward compatibility

Format: FOCUSED[/TOUCHED]
  - T#1 = Task agent #1 focused on this issue
  - ~P = Plan agent has touched this issue

Colors indicate status:
  - Yellow: agent currently focused on this issue
  - Dim: agent has touched this issue (not currently focused)
  - Green: agent finished successfully (legacy)
  - Red: agent failed (legacy)

Letters are: T=Task, R=Review, P=Plan, Q=QA, C=Custom."
  (let* ((focused (and (fboundp 'beads-agent--get-sessions-focused-on-issue)
                       (beads-agent--get-sessions-focused-on-issue issue-id)))
         (touched (and (fboundp 'beads-agent--get-sessions-touching-issue)
                       (beads-agent--get-sessions-touching-issue issue-id)))
         ;; Remove focused from touched for clean display
         (touched-only (cl-set-difference touched focused))
         ;; Legacy fallback
         (legacy-sessions (and (not focused) (not touched)
                               (fboundp 'beads-agent--get-sessions-for-issue)
                               (beads-agent--get-sessions-for-issue issue-id)))
         (outcome (and (fboundp 'beads-agent--get-issue-outcome)
                       (beads-agent--get-issue-outcome issue-id))))
    (cond
     ;; Focused or touched sessions - directory-bound model
     ((or focused touched-only)
      (let* ((separator (propertize "/" 'face 'shadow))
             ;; Format focused sessions (active work on this issue)
             (focused-indicators
              (mapcar
               (lambda (session)
                 (let* ((type-name (and (fboundp 'beads-agent-session-type-name)
                                        (beads-agent-session-type-name session)))
                        (instance-n (and (fboundp 'beads-agent-session-instance-number)
                                         (beads-agent-session-instance-number session)))
                        (letter (if type-name (substring type-name 0 1) "●"))
                        (indicator (if instance-n
                                       (format "%s#%d" letter instance-n)
                                     letter)))
                   (propertize indicator
                               'face 'beads-list-agent-working
                               'help-echo (format "Agent focused: %s"
                                                  (or type-name "unknown")))))
               focused))
             ;; Format touched sessions (past work in same agent session)
             (touched-indicators
              (mapcar
               (lambda (session)
                 (let* ((type-name (and (fboundp 'beads-agent-session-type-name)
                                        (beads-agent-session-type-name session)))
                        (letter (if type-name (substring type-name 0 1) "●"))
                        (indicator (format "~%s" letter)))
                   (propertize indicator
                               'face 'shadow
                               'help-echo (format "Agent touched: %s"
                                                  (or type-name "unknown")))))
               touched-only))
             (all-indicators (append focused-indicators touched-indicators)))
        (propertize (mapconcat #'identity all-indicators separator)
                    'help-echo (format "Focused: %d, Touched: %d"
                                       (length focused)
                                       (length touched-only)))))
     ;; Legacy issue-bound sessions (backward compatibility)
     (legacy-sessions
      (let* ((separator (propertize "/" 'face 'shadow))
             (type-counts
              (let ((counts (make-hash-table :test 'equal)))
                (dolist (session legacy-sessions)
                  (let ((type-name (and (fboundp 'beads-agent-session-type-name)
                                        (beads-agent-session-type-name session))))
                    (puthash type-name (1+ (gethash type-name counts 0)) counts)))
                counts))
             (type-instance-nums (make-hash-table :test 'equal))
             (indicators
              (mapcar
               (lambda (session)
                 (let* ((type-name (and (fboundp 'beads-agent-session-type-name)
                                        (beads-agent-session-type-name session)))
                        (current-num (1+ (gethash type-name type-instance-nums 0)))
                        (_ (puthash type-name current-num type-instance-nums))
                        (brief (= 1 (gethash type-name type-counts 1))))
                   (beads-list--format-agent-indicator
                    type-name current-num 'beads-list-agent-working brief)))
               legacy-sessions)))
        (propertize (if (= (length indicators) 1)
                        (car indicators)
                      (mapconcat #'identity indicators separator))
                    'help-echo (format "%d agent%s working"
                                       (length legacy-sessions)
                                       (if (= (length legacy-sessions) 1) "" "s")))))
     ;; Finished - show last indicator in green
     ((and (consp outcome) (eq (cdr outcome) 'finished))
      (propertize (car outcome)
                  'face 'beads-list-agent-finished
                  'help-echo "Agent finished successfully"))
     ((eq outcome 'finished)
      (propertize "●"
                  'face 'beads-list-agent-finished
                  'help-echo "Agent finished successfully"))
     ;; Failed - show last indicator in red
     ((and (consp outcome) (eq (cdr outcome) 'failed))
      (propertize (car outcome)
                  'face 'beads-list-agent-failed
                  'help-echo "Agent failed"))
     ((eq outcome 'failed)
      (propertize "●"
                  'face 'beads-list-agent-failed
                  'help-echo "Agent failed"))
     ;; No agent activity
     (t ""))))

(defun beads-list--issue-to-entry (issue)
  "Convert ISSUE (beads-issue object) to tabulated-list entry."
  (let* ((id (oref issue id))
         (title (or (oref issue title) ""))
         (status (oref issue status))
         (priority (oref issue priority))
         (type (or (oref issue issue-type) ""))
         (created (oref issue created-at))
         (created-str (beads-list--format-date created))
         (updated (oref issue updated-at))
         (updated-str (beads-list--format-date updated))
         (agent-str (beads-list--format-agent id)))
    (list id
          (vector id
                  type
                  (beads-list--format-status status)
                  (beads-list--format-priority priority)
                  agent-str
                  title
                  created-str
                  updated-str))))

(defun beads-list--populate-buffer (issues command &optional command-obj)
  "Populate current buffer with ISSUES using COMMAND for refresh.
Optional COMMAND-OBJ is a beads-command-list object for context."
  (setq beads-list--command command
        beads-list--raw-issues issues
        beads-list--command-obj command-obj)
  (setq tabulated-list-entries
        (mapcar #'beads-list--issue-to-entry issues))
  (tabulated-list-print t))

(defun beads-list--current-issue-id ()
  "Return the ID of the issue at point, or nil."
  (tabulated-list-get-id))

(defun beads-list--get-issue-by-id (id)
  "Return beads-issue object for ID from current buffer's raw issues."
  (seq-find (lambda (issue)
              (string= (oref issue id) id))
            beads-list--raw-issues))

;;; CLI Integration

(defun beads-issue-read (issue-id)
  "Read a beads issue by ISSUE-ID from the CLI.
Uses the bd show command with --json flag to fetch the issue.
Runs in the directory specified by `default-directory'.
Returns a beads-issue object or signals an error if not found."
  (interactive "sIssue ID: ")
  (let ((result (beads-command-show! :issue-ids (list issue-id))))
    ;; beads-command-show! returns a list, so unwrap single result
    (if (listp result)
        (car result)
      result)))

(defun beads-issue-list (&optional status)
  "List beads issues from the CLI.
Uses the bd list command with --json flag to fetch all issues.
If STATUS is provided, filters by that status.
Runs in the directory specified by `default-directory'.
Returns a list of beads-issue objects."
  (interactive)
  (if status
      (beads-command-list! :status status)
    (beads-command-list!)))

(defun beads-blocked-issue-list ()
  "List blocked beads issues from the CLI.
Uses the bd blocked command with --json flag.
Runs in the directory specified by `default-directory'.
Returns a list of beads-blocked-issue objects."
  (interactive)
  (beads-command-blocked!))

(defun beads-issue-ready (&optional limit)
  "Get ready work from the CLI.
Uses the bd ready command with --json flag.
If LIMIT is provided, limits the number of results.
Runs in the directory specified by `default-directory'.
Returns a list of beads-issue objects."
  (interactive)
  (if limit
      (beads-command-ready! :limit limit)
    (beads-command-ready!)))

;;; Transient Menu Integration

(defun beads-list--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-list object.
Returns a beads-command-list object with all applicable filters set."
  (let ((command (beads-command-list)))
    ;; Boolean switches
    (when (member "--all" args)
      (oset command all t))
    (when (member "--no-assignee" args)
      (oset command no-assignee t))
    (when (member "--empty-description" args)
      (oset command empty-description t))
    (when (member "--no-labels" args)
      (oset command no-labels t))
    (when (member "--long" args)
      (oset command long t))
    ;; String options
    (when-let ((assignee (transient-arg-value "--assignee=" args)))
      (oset command assignee assignee))
    (when-let ((closed-after (transient-arg-value "--closed-after=" args)))
      (oset command closed-after closed-after))
    (when-let ((closed-before (transient-arg-value "--closed-before=" args)))
      (oset command closed-before closed-before))
    (when-let ((created-after (transient-arg-value "--created-after=" args)))
      (oset command created-after created-after))
    (when-let ((created-before (transient-arg-value
                                 "--created-before=" args)))
      (oset command created-before created-before))
    (when-let ((desc-contains (transient-arg-value
                                "--desc-contains=" args)))
      (oset command desc-contains desc-contains))
    (when-let ((format (transient-arg-value "--format=" args)))
      (oset command format format))
    (when-let ((id (transient-arg-value "--id=" args)))
      (oset command id id))
    (when-let ((notes-contains (transient-arg-value
                                 "--notes-contains=" args)))
      (oset command notes-contains notes-contains))
    (when-let ((status (transient-arg-value "--status=" args)))
      (oset command status status))
    (when-let ((title (transient-arg-value "--title=" args)))
      (oset command title title))
    (when-let ((title-contains (transient-arg-value
                                 "--title-contains=" args)))
      (oset command title-contains title-contains))
    (when-let ((type (transient-arg-value "--type=" args)))
      (oset command issue-type type))
    (when-let ((updated-after (transient-arg-value
                                "--updated-after=" args)))
      (oset command updated-after updated-after))
    (when-let ((updated-before (transient-arg-value
                                 "--updated-before=" args)))
      (oset command updated-before updated-before))
    ;; Repeatable options (collect all values)
    (let ((label-values nil)
          (label-any-values nil))
      (dolist (arg args)
        (when (string-prefix-p "--label=" arg)
          (push (substring arg (length "--label=")) label-values))
        (when (string-prefix-p "--label-any=" arg)
          (push (substring arg (length "--label-any=")) label-any-values)))
      (when label-values
        (oset command label (nreverse label-values)))
      (when label-any-values
        (oset command label-any (nreverse label-any-values))))
    ;; Numeric options - apply default limit if not specified
    (oset command limit
          (if-let ((limit-str (transient-arg-value "--limit=" args)))
              (string-to-number limit-str)
            beads-list-default-limit))
    (when-let ((priority-str (transient-arg-value "--priority=" args)))
      (oset command priority priority-str))
    (when-let ((priority-min-str (transient-arg-value
                                    "--priority-min=" args)))
      (oset command priority-min priority-min-str))
    (when-let ((priority-max-str (transient-arg-value
                                    "--priority-max=" args)))
      (oset command priority-max priority-max-str))
    command))

;;; Transient Suffix Commands

(transient-define-suffix beads-list--transient-execute ()
  "Execute the bd list command with current filter parameters.
Uses directory-aware buffer identity: same project = same buffer."
  :key "x"
  :description "List issues"
  (interactive)
  (let* ((caller-dir default-directory)
         (project-dir (or (beads-git-find-project-root) default-directory))
         (args (transient-args 'beads-list))
         (command (beads-list--parse-transient-args args)))
    (condition-case err
        (let* ((exec (beads-command-execute command))
               (issue-objects (oref exec result))
               (buffer (beads-list--get-or-create-buffer 'list)))
          (with-current-buffer buffer
            (unless (derived-mode-p 'beads-list-mode)
              (beads-list-mode))
            ;; Update directory-aware state
            (setq beads-list--project-dir project-dir)
            (setq beads-list--branch (beads-git-get-branch))
            (setq beads-list--proj-name (beads-git-get-project-name))
            (setq default-directory caller-dir)
            (if (not issue-objects)
                (progn
                  (setq tabulated-list-entries nil)
                  (tabulated-list-print t)
                  (message "No issues found"))
              (beads-list--populate-buffer issue-objects 'list command)
              (message "Found %d issue%s"
                       (length issue-objects)
                       (if (= (length issue-objects) 1) "" "s"))))
          (beads-list--display-buffer buffer))
      (error
       (message "Failed to list issues: %s"
                (error-message-string err))))))

(transient-define-suffix beads-list--transient-reset ()
  "Reset all filter parameters to their default values."
  :key "R"
  :description "Reset all filters"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all filters? ")
    (transient-reset)
    (transient--redisplay)
    (message "All filters reset")))

(transient-define-suffix beads-list--transient-preview ()
  "Preview the bd list command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-list))
         (command (beads-list--parse-transient-args args))
         (cmd-line (beads-command-line command))
         (cmd-string (mapconcat #'shell-quote-argument cmd-line " ")))
    (message "Command: %s" cmd-string)))

;;; Transient Groups

(transient-define-group beads-list--basic-filters-section
  [:level 1 "Basic Filters"
          (beads-option-list-status)
          (beads-option-list-priority)
          (beads-option-list-type)
          (beads-option-list-assignee)])

(transient-define-group beads-list--text-search-section
  [:level 2 "Text Search"
          (beads-option-list-title)
          (beads-option-list-title-contains)
          (beads-option-list-desc-contains)
          (beads-option-list-notes-contains)])

(transient-define-group beads-list--date-filters-section
  [:level 3 "Date Filters"
          (beads-option-list-created-after)
          (beads-option-list-created-before)
          (beads-option-list-updated-after)
          (beads-option-list-updated-before)
          (beads-option-list-closed-after)
          (beads-option-list-closed-before)])

(transient-define-group beads-list--advanced-filters-section
  [:level 4 "Advanced Filters"
          (beads-option-list-priority-min)
          (beads-option-list-priority-max)
          (beads-option-list-label)
          (beads-option-list-label-any)
          (beads-option-list-id)
          (beads-option-list-no-assignee)
          (beads-option-list-empty-description)
          (beads-option-list-no-labels)])

(transient-define-group beads-list--output-options-section
  [:level 5 "Output Options"
          (beads-option-list-limit)
          (beads-option-list-long)
          (beads-option-list-format)
          (beads-option-list-all)])

;;; Main Transient

;;;###autoload (autoload 'beads-list "beads-list" nil t)
(transient-define-prefix beads-list ()
  "List issues in Beads with filter options.

This transient menu provides an interactive interface for setting
filter parameters for the bd list command.  All filters are
optional.

Transient levels control which filter groups are visible
(cycle with C-x l):
  Level 1: Basic filters (status, priority, type, assignee)
  Level 2: Text search (title, description, notes)
  Level 3: Date filters (created, updated, closed)        [default]
  Level 4: Advanced filters (priority ranges, labels, etc.)
  Level 5: Output options (limit, long format, etc.)
  Level 7: Global options (actor, db, json flags, etc.)"
  beads-list--basic-filters-section
  beads-list--text-search-section
  beads-list--date-filters-section
  beads-list--advanced-filters-section
  beads-list--output-options-section
  beads-option-global-section
  ["Actions"
   ("x" "List issues" beads-list--transient-execute)
   ("P" "Preview command" beads-list--transient-preview)
   ("R" "Reset all filters" beads-list--transient-reset)])

;;; Commands

(defun beads-list-refresh (&optional silent)
  "Refresh the current issue list buffer.
When SILENT is non-nil, suppress messages (for hook-triggered refreshes)."
  (interactive)
  (unless beads-list--command
    (user-error "No command associated with this buffer"))
  (let* ((issues (pcase beads-list--command
                   ('list
                    (if beads-list--command-obj
                        (oref (beads-command-execute beads-list--command-obj) result)
                      (beads-command-list!)))
                   ('ready
                    (beads-issue-ready))
                   ('blocked
                    (beads-blocked-issue-list))
                   (_ (error "Unknown command: %s" beads-list--command))))
         ;; Save window point if buffer is displayed, otherwise buffer point.
         ;; This ensures we preserve point correctly when called via
         ;; with-current-buffer from beads-list-refresh-all.
         (win (get-buffer-window (current-buffer)))
         (pos (if win (window-point win) (point))))
    (if (not issues)
        (progn
          (setq tabulated-list-entries nil)
          (tabulated-list-print t)
          (unless silent (message "No issues found")))
      (beads-list--populate-buffer issues beads-list--command beads-list--command-obj)
      ;; Restore point in window or buffer as appropriate
      (if win
          (set-window-point win pos)
        (goto-char pos))
      (unless silent
        (message "Refreshed %d issue%s"
                 (length issues)
                 (if (= (length issues) 1) "" "s"))))))

(defun beads-list-refresh-all ()
  "Refresh all visible beads-list buffers.
This is useful when agent state changes to update the AI indicator column."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (derived-mode-p 'beads-list-mode)))
      (with-current-buffer buffer
        (condition-case nil
            (beads-list-refresh 'silent)
          (error nil))))))  ; Ignore errors from individual buffer refreshes

(defun beads-list--on-agent-state-change (_action _session)
  "Handle agent state change by refreshing all beads-list buffers.
Update the AI indicator column when sessions start or stop.
ACTION and SESSION are provided by `beads-agent-state-change-hook'."
  (beads-list-refresh-all))

(defun beads-list-show ()
  "Show details for the issue at point in other window."
  (interactive)
  (if-let* ((id (beads-list--current-issue-id)))
      (beads-show id)
    (user-error "No issue at point")))

(defun beads-list-quit ()
  "Quit the current issue list buffer."
  (interactive)
  (quit-window t))

(defun beads-list-next ()
  "Move to next issue."
  (interactive)
  (forward-line 1))

(defun beads-list-previous ()
  "Move to previous issue."
  (interactive)
  (forward-line -1))

(defun beads-list-mark ()
  "Mark the issue at point."
  (interactive)
  (when-let* ((id (beads-list--current-issue-id)))
    (unless (member id beads-list--marked-issues)
      (push id beads-list--marked-issues))
    (tabulated-list-put-tag ">" t)))

(defun beads-list-unmark ()
  "Unmark the issue at point."
  (interactive)
  (when-let* ((id (beads-list--current-issue-id)))
    (setq beads-list--marked-issues
          (delete id beads-list--marked-issues))
    (tabulated-list-put-tag " " t)))

(defun beads-list-mark-all ()
  "Mark all issues in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (beads-list--current-issue-id)
        (beads-list-mark))
      (forward-line 0))))  ; Don't advance, beads-list-mark does that

(defun beads-list-unmark-all ()
  "Unmark all issues in the current buffer."
  (interactive)
  (setq beads-list--marked-issues nil)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (beads-list--current-issue-id)
        (tabulated-list-put-tag " "))
      (forward-line 1))))

(defun beads-list-create ()
  "Create a new issue using the beads-create transient menu."
  (interactive)
  (require 'beads-command-create)
  (call-interactively #'beads-create))

(defun beads-list-update ()
  "Update the issue at point using the beads-update transient menu."
  (interactive)
  (if-let* ((id (beads-list--current-issue-id)))
      (progn
        (require 'beads-command-update)
        ;; beads-update will auto-detect the issue ID from beads-list context
        (call-interactively #'beads-update))
    (user-error "No issue at point")))

(defun beads-list-close ()
  "Close the issue at point using the beads-close transient menu."
  (interactive)
  (if-let* ((id (beads-list--current-issue-id)))
      (progn
        (require 'beads-command-close)
        ;; beads-close will auto-detect the issue ID from beads-list context
        (call-interactively #'beads-close))
    (user-error "No issue at point")))

(defun beads-list-delete ()
  "Delete the issue at point using the beads-delete transient menu."
  (interactive)
  (let ((id (beads-list--current-issue-id)))
    (if id
        (progn
          (require 'beads-command-delete)
          ;; beads-delete will auto-detect the issue ID from beads-list context
          (beads-delete id))
      (user-error "No issue at point"))))

(defun beads-list-reopen ()
  "Reopen the issue at point using the beads-reopen transient menu."
  (interactive)
  (if-let* ((id (beads-list--current-issue-id)))
      (progn
        (require 'beads-command-reopen)
        ;; beads-reopen will auto-detect the issue ID from beads-list context
        (call-interactively #'beads-reopen))
    (user-error "No issue at point")))

(defun beads-list-copy-id ()
  "Copy the issue ID at point to the kill ring."
  (interactive)
  (if-let* ((id (beads-list--current-issue-id)))
      (progn
        (kill-new id)
        (message "Copied issue ID: %s" id))
    (user-error "No issue at point")))

(defun beads-list-sort ()
  "Sort the issue list by column.
Uses tabulated-list built-in sorting."
  (interactive)
  (call-interactively #'tabulated-list-sort))

(defun beads-list-filter ()
  "Open beads-list transient menu with current filter pre-selected.
If in a beads-list buffer, the current filter is used to pre-populate the
transient menu options."
  (interactive)
  (when (and (boundp 'beads-list--command-obj) beads-list--command-obj)
    ;; Convert current command to transient args
    (let ((cmd-line (beads-command-line beads-list--command-obj)))
      ;; Remove "list" from the beginning since transient will add it
      (when (and cmd-line (string= (car cmd-line) "list"))
        (setq cmd-line (cdr cmd-line)))
      ;; Set the transient value with current filter
      (put 'beads-list 'transient--value cmd-line)
      ;; Also add to history for persistence
      (put 'beads-list 'transient--history (list cmd-line))))
  ;; Open the transient menu
  (call-interactively #'beads-list))


;;; Bulk Operations

(defun beads-list-bulk-update-status ()
  "Update status for all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let ((status (completing-read
                   (format "Set status for %d issue(s): "
                          (length beads-list--marked-issues))
                   '("open" "in_progress" "blocked")
                   nil t)))
      (when (and (not (string-empty-p status))
                 (y-or-n-p (format "Update status to '%s' for %d issue(s)? "
                                  status (length beads-list--marked-issues))))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (beads-command-execute
                   (beads-command-update
                    :issue-ids (list id)
                    :status status))
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to update %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Updated %d issue(s), %d failed" success-count fail-count))))))

(defun beads-list-bulk-update-priority ()
  "Update priority for all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let* ((choices '(("0 - Critical" . 0)
                     ("1 - High" . 1)
                     ("2 - Medium" . 2)
                     ("3 - Low" . 3)
                     ("4 - Backlog" . 4)))
           (selection (completing-read
                      (format "Set priority for %d issue(s): "
                             (length beads-list--marked-issues))
                      choices nil t))
           (priority (cdr (assoc selection choices))))
      (when (and priority
                 (y-or-n-p (format "Update priority to %d for %d issue(s)? "
                                  priority (length beads-list--marked-issues))))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (beads-command-execute
                   (beads-command-update
                    :issue-ids (list id)
                    :priority priority))
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to update %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Updated %d issue(s), %d failed" success-count fail-count))))))

(defun beads-list-bulk-close ()
  "Close all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let ((reason (read-string
                   (format "Reason for closing %d issue(s): "
                          (length beads-list--marked-issues)))))
      (when (y-or-n-p (format "Close %d issue(s)? "
                             (length beads-list--marked-issues)))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (beads-command-execute
                   (beads-command-close
                    :issue-ids (list id)
                    :reason (when (and reason (not (string-empty-p (string-trim reason))))
                             reason)))
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to close %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Closed %d issue(s), %d failed"
                   success-count fail-count))))))

(defun beads-list-bulk-reopen ()
  "Reopen all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let ((reason (read-string
                   (format "Reason for reopening %d issue(s): "
                          (length beads-list--marked-issues)))))
      (when (y-or-n-p (format "Reopen %d issue(s)? "
                             (length beads-list--marked-issues)))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (if (and reason (not (string-empty-p (string-trim reason))))
                      (beads-command-reopen! :issue-ids (list id) :reason reason)
                    (beads-command-reopen! :issue-ids (list id)))
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to reopen %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Reopened %d issue(s), %d failed"
                   success-count fail-count))))))

;;; Follow Mode
;;
;; Automatically update beads-show buffer when navigating in beads-list.
;; Inspired by magit-log's revision buffer follow behavior.

;; Forward declaration - defined by `define-minor-mode' below
(defvar beads-list-follow-mode)

(defun beads-list--maybe-update-show-buffer ()
  "Schedule show buffer update if conditions are met.
Called from `post-command-hook' when `beads-list-follow-mode' is active.
Coalesces rapid updates by reusing pending timer rather than scheduling
multiple timers."
  (when (and beads-list-follow-mode
             (derived-mode-p 'beads-list-mode))
    (when-let* ((issue-id (beads-list--current-issue-id))
                (target-buf (beads-show--find-visible-buffer)))
      ;; Skip if already showing this issue
      (unless (with-current-buffer target-buf
                (equal beads-show--issue-id issue-id))
        ;; Coalescing: update pending target, schedule timer only if first
        (if beads-list--pending-show-update
            (setcar beads-list--pending-show-update issue-id)
          (setq beads-list--pending-show-update (cons issue-id target-buf))
          ;; Capture current buffer for timer callback - state is buffer-local
          (let ((list-buf (current-buffer)))
            (setq beads-list--pending-show-timer
                  (run-with-idle-timer
                   beads-list-update-show-delay nil
                   (lambda ()
                     (when (and (buffer-live-p list-buf)
                                (buffer-local-value
                                 'beads-list-follow-mode list-buf))
                       (with-current-buffer list-buf
                         (beads-list--do-update-show-buffer))))))))))))

(defun beads-list--do-update-show-buffer ()
  "Execute pending show buffer update."
  (when beads-list--pending-show-update
    (pcase-let ((`(,issue-id . ,buffer) beads-list--pending-show-update))
      (setq beads-list--pending-show-update nil)
      (setq beads-list--pending-show-timer nil)
      (when (and (buffer-live-p buffer)
                 (get-buffer-window buffer))
        (beads-show-update-buffer issue-id buffer)))))

(defun beads-list--cancel-pending-show-timer ()
  "Cancel any pending show buffer update timer.
Called from `kill-buffer-hook' to prevent resource leaks."
  (when beads-list--pending-show-timer
    (cancel-timer beads-list--pending-show-timer)
    (setq beads-list--pending-show-timer nil))
  (setq beads-list--pending-show-update nil))

(define-minor-mode beads-list-follow-mode
  "Automatically update beads-show buffer when navigating issues.
When this mode is enabled and a beads-show buffer is visible in
the current frame, navigating to a different issue with n/p will
automatically display that issue's details.

Uses an idle timer to debounce rapid navigation, similar to
`magit-update-other-window-delay'."
  :lighter " Follow"
  :keymap nil
  (if beads-list-follow-mode
      (progn
        (add-hook 'post-command-hook
                  #'beads-list--maybe-update-show-buffer nil t)
        (add-hook 'kill-buffer-hook
                  #'beads-list--cancel-pending-show-timer nil t))
    ;; Cancel any pending timer and clear state
    (beads-list--cancel-pending-show-timer)
    (remove-hook 'post-command-hook
                 #'beads-list--maybe-update-show-buffer t)
    (remove-hook 'kill-buffer-hook
                 #'beads-list--cancel-pending-show-timer t)))

;;; Mode Definition

(defvar beads-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; Navigation (following standard conventions)
    (define-key map (kbd "n") #'beads-list-next)
    (define-key map (kbd "p") #'beads-list-previous)
    (define-key map (kbd "RET") #'beads-list-show)

    ;; Refresh/quit (like Magit, dired)
    (define-key map (kbd "g") #'beads-list-refresh)
    (define-key map (kbd "q") #'beads-list-quit)

    ;; Marking (like dired/ibuffer)
    (define-key map (kbd "m") #'beads-list-mark)
    (define-key map (kbd "u") #'beads-list-unmark)
    (define-key map (kbd "U") #'beads-list-unmark-all)
    (define-key map (kbd "* !") #'beads-list-mark-all)     ; ibuffer-style
    (define-key map (kbd "* *") #'beads-list-mark-all)     ; alternative
    (define-key map (kbd "* u") #'beads-list-unmark-all)   ; ibuffer-style

    ;; CRUD operations (following Emacs conventions)
    (define-key map (kbd "c") #'beads-list-create)         ; create (like many modes)
    (define-key map (kbd "+") #'beads-list-create)         ; alternative
    (define-key map (kbd "e") #'beads-list-update)         ; edit (more intuitive)
    (define-key map (kbd "d") #'beads-list-close)          ; delete/done (mark for closing)
    (define-key map (kbd "k") #'beads-list-close)          ; kill (alternative)
    (define-key map (kbd "o") #'beads-list-reopen)         ; open/reopen closed issue
    (define-key map (kbd "D") #'beads-list-delete)         ; delete permanently (destructive)

    ;; Utilities
    (define-key map (kbd "w") #'beads-list-copy-id)        ; copy (like eww, info)
    (define-key map (kbd "C-w") #'beads-list-copy-id)      ; copy (override kill-region)
    (define-key map (kbd "S") #'beads-list-sort)           ; sort menu
    (define-key map (kbd "l") #'beads-list-filter)         ; filter (open transient with current filter)
    (define-key map (kbd "C-c C-f") #'beads-list-follow-mode) ; follow mode (like compilation)

    ;; AI Agent type commands
    (define-key map (kbd "T") #'beads-agent-start-task)     ; Task agent
    (define-key map (kbd "R") #'beads-agent-start-review)   ; Review agent
    (define-key map (kbd "P") #'beads-agent-start-plan)     ; Plan agent
    (define-key map (kbd "Q") #'beads-agent-start-qa)       ; QA agent
    (define-key map (kbd "C") #'beads-agent-start-custom)   ; Custom agent
    (define-key map (kbd "X") #'beads-agent-stop-at-point)  ; Stop agent
    (define-key map (kbd "J") #'beads-agent-jump-at-point)  ; Jump to agent
    (define-key map (kbd "A") #'beads-agent-start-at-point) ; Backward compat

    ;; Sesman session management (CIDER/ESS convention)
    (define-key map (kbd "C-c C-s") beads-sesman-map)

    ;; Bulk operations (like Magit) - create prefix map for B
    (let ((bulk-map (make-sparse-keymap)))
      (define-key bulk-map (kbd "s") #'beads-list-bulk-update-status)
      (define-key bulk-map (kbd "p") #'beads-list-bulk-update-priority)
      (define-key bulk-map (kbd "c") #'beads-list-bulk-close)
      (define-key bulk-map (kbd "o") #'beads-list-bulk-reopen)
      (define-key map (kbd "B") bulk-map))
    map)
  "Keymap for `beads-list-mode'.")

(define-derived-mode beads-list-mode tabulated-list-mode "Beads-List"
  "Major mode for displaying Beads issues in a tabulated list.

\\{beads-list-mode-map}"
  (setq tabulated-list-format
        (vector (list "ID" beads-list-id-width t)
                (list "Type" beads-list-type-width t)
                (list "Status" beads-list-status-width t)
                (list "Priority" beads-list-priority-width t
                      :right-align t)
                (list "Agents" beads-list-agent-width t)
                (list "Title" beads-list-title-width t)
                (list "Created" beads-list-created-width t)
                (list "Updated" beads-list-updated-width t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Created" t))
  (tabulated-list-init-header)
  (hl-line-mode 1))

;;; Public Commands

;;;###autoload
(defun beads-ready ()
  "Display ready Beads issues in a tabulated list.
Uses directory-aware buffer identity: same project = same buffer."
  (interactive)
  (beads-check-executable)
  (let* ((caller-dir default-directory)
         (project-dir (or (beads-git-find-project-root) default-directory))
         (buffer (beads-list--get-or-create-buffer 'ready))
         (issues (beads-issue-ready)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-list-mode)
        (beads-list-mode))
      ;; Update directory-aware state
      (setq beads-list--project-dir project-dir)
      (setq beads-list--branch (beads-git-get-branch))
      (setq beads-list--proj-name (beads-git-get-project-name))
      (setq default-directory caller-dir)
      (if (not issues)
          (progn
            (setq tabulated-list-entries nil)
            (tabulated-list-print t)
            (setq mode-line-format
                  '("%e" mode-line-front-space
                    mode-line-buffer-identification
                    "  No ready issues"))
            (message "No ready issues found"))
        (beads-list--populate-buffer issues 'ready)
        (setq mode-line-format
              '("%e" mode-line-front-space
                mode-line-buffer-identification
                (:eval (format "  %d ready issue%s%s"
                             (length tabulated-list-entries)
                             (if (= (length tabulated-list-entries) 1) "" "s")
                             (if beads-list--marked-issues
                                 (format " [%d marked]"
                                        (length beads-list--marked-issues))
                               "")))))))
    (beads-list--display-buffer buffer)))

;;;###autoload
(defun beads-blocked ()
  "Display blocked Beads issues in a tabulated list.
Uses directory-aware buffer identity: same project = same buffer."
  (interactive)
  (beads-check-executable)
  (let* ((caller-dir default-directory)
         (project-dir (or (beads-git-find-project-root) default-directory))
         (buffer (beads-list--get-or-create-buffer 'blocked))
         (issues (beads-blocked-issue-list)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-list-mode)
        (beads-list-mode))
      ;; Update directory-aware state
      (setq beads-list--project-dir project-dir)
      (setq beads-list--branch (beads-git-get-branch))
      (setq beads-list--proj-name (beads-git-get-project-name))
      (setq default-directory caller-dir)
      (if (not issues)
          (progn
            (setq tabulated-list-entries nil)
            (tabulated-list-print t)
            (setq mode-line-format
                  '("%e" mode-line-front-space
                    mode-line-buffer-identification
                    "  No blocked issues"))
            (message "No blocked issues found"))
        (beads-list--populate-buffer issues 'blocked)
        (setq mode-line-format
              '("%e" mode-line-front-space
                mode-line-buffer-identification
                (:eval (format "  %d blocked issue%s%s"
                             (length tabulated-list-entries)
                             (if (= (length tabulated-list-entries) 1) "" "s")
                             (if beads-list--marked-issues
                                 (format " [%d marked]"
                                        (length beads-list--marked-issues))
                               "")))))))
    (beads-list--display-buffer buffer)))

;;; Hook Registration

;; Register our hook function to refresh list buffers when agent state changes.
;; Declare the variable so we can add to it even if beads-agent-backend isn't
;; loaded yet.  When beads-agent-backend loads, it will use our hook.
(defvar beads-agent-state-change-hook)
;; Append to end of hook list so this runs AFTER sesman registers the session
(add-hook 'beads-agent-state-change-hook #'beads-list--on-agent-state-change t)

;;; Footer
(provide 'beads-command-list)
;;; beads-command-list.el ends here
