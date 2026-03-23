;;; beads-command-misc.el --- Misc command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for miscellaneous `bd' commands
;; that don't warrant their own module file.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-reader)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-duplicate
;;; ============================================================

(beads-defcommand beads-command-duplicate (beads-command-global-options)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to mark as duplicate."
    :positional 1)
   (of
    :initarg :of
    :type (or null string)
    :initform nil
    :documentation "Canonical issue ID."
    :long-option "of"
    :option-type :string
    :key "o"
    :transient "--of"
    :class transient-option
    :argument "--of="
    :prompt "Canonical issue: "
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd duplicate command.
Marks an issue as a duplicate of another.")


(cl-defmethod beads-command-validate ((command beads-command-duplicate))
  "Validate duplicate COMMAND."
  (with-slots (issue-id of) command
    (cond
     ((not issue-id) "Issue ID is required")
     ((not of) "Canonical issue ID (--of) is required")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-duplicates
;;; ============================================================

(beads-defcommand beads-command-duplicates (beads-command-global-options)
  ((merge
    :initarg :merge
    :type boolean
    :initform nil
    :documentation "Merge duplicates with confirmation."
    :long-option "merge"
    :option-type :boolean
    :key "m"
    :transient "--merge"
    :class transient-switch
    :argument "--merge"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd duplicates command.
Finds and optionally merges duplicate issues.")


;;; ============================================================
;;; Command Class: beads-command-supersede
;;; ============================================================

(beads-defcommand beads-command-supersede (beads-command-global-options)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to mark as superseded."
    :positional 1)
   (with-id
    :initarg :with-id
    :type (or null string)
    :initform nil
    :documentation "Replacement issue ID."
    :long-option "with"
    :option-type :string
    :key "w"
    :transient "--with"
    :class transient-option
    :argument "--with="
    :prompt "Replacement issue: "
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd supersede command.
Marks an issue as superseded by a newer one.")


(cl-defmethod beads-command-validate ((command beads-command-supersede))
  "Validate supersede COMMAND."
  (with-slots (issue-id with-id) command
    (cond
     ((not issue-id) "Issue ID is required")
     ((not with-id) "Replacement issue ID (--with) is required")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-orphans
;;; ============================================================

(beads-defcommand beads-command-orphans (beads-command-global-options)
  ((details
    :initarg :details
    :type boolean
    :initform nil
    :documentation "Show full commit information."
    :long-option "details"
    :option-type :boolean
    :key "d"
    :transient "--details"
    :class transient-switch
    :argument "--details"
    :transient-group "Options"
    :level 1
    :order 1)
   (fix
    :initarg :fix
    :type boolean
    :initform nil
    :documentation "Close orphaned issues with confirmation."
    :long-option "fix"
    :short-option "f"
    :option-type :boolean
    :key "f"
    :transient "--fix"
    :class transient-switch
    :argument "--fix"
    :transient-group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd orphans command.
Identifies orphaned issues referenced in commits but still open.")


;;; ============================================================
;;; Command Class: beads-command-lint
;;; ============================================================

(beads-defcommand beads-command-lint (beads-command-global-options)
  ((issue-ids
    :initarg :issue-ids
    :type list
    :initform nil
    :documentation "Issue IDs to lint (optional)."
    :positional 1
    :option-type :list
    :option-separator nil)
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status (default: open, 'all' for all)."
    :long-option "status"
    :short-option "s"
    :option-type :string
    :key "s"
    :transient "--status"
    :class transient-option
    :argument "--status="
    :prompt "Status: "
    :transient-group "Options"
    :level 1
    :order 1)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Filter by issue type."
    :long-option "type"
    :short-option "t"
    :option-type :string
    :key "t"
    :transient "--type"
    :class transient-option
    :argument "--type="
    :prompt "Type: "
    :choices ("bug" "task" "feature" "epic" "chore")
    :transient-group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd lint command.
Checks issues for missing template sections.")


;;; ============================================================
;;; Command Class: beads-command-move
;;; ============================================================

(beads-defcommand beads-command-move (beads-command-global-options)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to move."
    :positional 1
    ;; Transient properties for UI input
    :key "i"
    :transient "Issue ID (required)"
    :class transient-option
    :argument "--id="
    :transient-reader beads-reader-move-issue-id
    :prompt "Issue ID: "
    :transient-group "Move Issue"
    :level 1
    :order 0)
   (to
    :initarg :to
    :type (or null string)
    :initform nil
    :documentation "Target rig or prefix."
    :long-option "to"
    :option-type :string
    :key "t"
    :transient "--to (required)"
    :class transient-option
    :argument "--to="
    :prompt "Target rig: "
    :transient-group "Move Issue"
    :level 1
    :order 1)
   (keep-open
    :initarg :keep-open
    :type boolean
    :initform nil
    :documentation "Keep source issue open."
    :long-option "keep-open"
    :option-type :boolean
    :key "k"
    :transient "--keep-open"
    :class transient-switch
    :argument "--keep-open"
    :transient-group "Options"
    :level 2
    :order 2)
   (skip-deps
    :initarg :skip-deps
    :type boolean
    :initform nil
    :documentation "Skip dependency remapping."
    :long-option "skip-deps"
    :option-type :boolean
    :key "d"
    :transient "--skip-deps"
    :class transient-switch
    :argument "--skip-deps"
    :transient-group "Options"
    :level 2
    :order 3))
  :documentation "Represents bd move command.
Moves an issue to a different rig with dependency remapping.")


(cl-defmethod beads-command-validate ((command beads-command-move))
  "Validate move COMMAND."
  (with-slots (issue-id to) command
    (cond
     ((not issue-id) "Issue ID is required")
     ((not to) "Target rig (--to) is required")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-refile
;;; ============================================================

(beads-defcommand beads-command-refile (beads-command-global-options)
  ((source-id
    :initarg :source-id
    :type (or null string)
    :initform nil
    :documentation "Source issue ID."
    :positional 1)
   (target-rig
    :initarg :target-rig
    :type (or null string)
    :initform nil
    :documentation "Target rig name or prefix."
    :positional 2)
   (keep-open
    :initarg :keep-open
    :type boolean
    :initform nil
    :documentation "Keep source issue open."
    :long-option "keep-open"
    :option-type :boolean
    :key "k"
    :transient "--keep-open"
    :class transient-switch
    :argument "--keep-open"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd refile command.
Moves an issue to a different rig.")


(cl-defmethod beads-command-validate ((command beads-command-refile))
  "Validate refile COMMAND."
  (with-slots (source-id target-rig) command
    (cond
     ((not source-id) "Source issue ID is required")
     ((not target-rig) "Target rig is required")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-q (quick capture)
;;; ============================================================

(beads-defcommand beads-command-q (beads-command-global-options)
  ((title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Issue title."
    :positional 1
    ;; Transient properties for UI input
    :key "T"
    :transient "Title (required)"
    :class transient-option
    :argument "--title="
    :prompt "Title: "
    :transient-group "Quick Capture"
    :level 1
    :order 0)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Issue type (default: task)."
    :long-option "type"
    :short-option "t"
    :option-type :string
    :key "t"
    :transient "--type"
    :class transient-option
    :argument "--type="
    :prompt "Type: "
    :choices ("task" "bug" "feature" "epic" "chore")
    :transient-group "Options"
    :level 1
    :order 1)
   (priority
    :initarg :priority
    :type (or null string)
    :initform nil
    :documentation "Priority (0-4 or P0-P4)."
    :long-option "priority"
    :short-option "p"
    :option-type :string
    :key "p"
    :transient "--priority"
    :class transient-option
    :argument "--priority="
    :prompt "Priority: "
    :transient-group "Options"
    :level 1
    :order 2)
   (labels
    :initarg :labels
    :type list
    :initform nil
    :documentation "Labels for the issue."
    :long-option "labels"
    :short-option "l"
    :option-type :list
    :key "l"
    :transient "--labels"
    :class transient-option
    :argument "--labels="
    :prompt "Labels: "
    :transient-group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd q command.
Quick capture: creates issue and outputs only ID.")


(cl-defmethod beads-command-validate ((command beads-command-q))
  "Validate q COMMAND."
  (with-slots (title) command
    (if (not title) "Title is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-note
;;; ============================================================

(beads-defcommand beads-command-note (beads-command-global-options)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to append note to."
    :positional 1)
   (stdin
    :initarg :stdin
    :type boolean
    :initform nil
    :documentation "Read note text from stdin."
    :long-option "stdin"
    :option-type :boolean
    :key "s"
    :transient "--stdin"
    :class transient-switch
    :argument "--stdin"
    :transient-group "Input"
    :level 1
    :order 1)
   (file
    :initarg :file
    :type (or null string)
    :initform nil
    :documentation "Read note text from file."
    :long-option "file"
    :option-type :string
    :key "f"
    :transient "--file"
    :class transient-option
    :argument "--file="
    :prompt "File: "
    :transient-group "Input"
    :level 1
    :order 2))
  :documentation "Represents bd note command.
Appends a note to an issue's notes field.")


(cl-defmethod beads-command-validate ((command beads-command-note))
  "Validate note COMMAND."
  (with-slots (issue-id) command
    (if (not issue-id) "Issue ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-version
;;; ============================================================

(beads-defcommand beads-command-version (beads-command-global-options)
  ((daemon
    :initarg :daemon
    :type boolean
    :initform nil
    :documentation "Check daemon version and compatibility."
    :long-option "daemon"
    :option-type :boolean
    :key "d"
    :transient "--daemon"
    :class transient-switch
    :argument "--daemon"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd version command.
Prints version information.")


;;; ============================================================
;;; Command Class: beads-command-where
;;; ============================================================

(beads-defcommand beads-command-where (beads-command-global-options)
  ()
  :documentation "Represents bd where command.
Shows active beads location.")


;;; ============================================================
;;; Command Class: beads-command-human
;;; ============================================================

(beads-defcommand beads-command-human (beads-command-global-options)
  ()
  :documentation "Represents bd human command.
Shows essential commands for human users.")


;;; ============================================================
;;; Command Class: beads-command-onboard
;;; ============================================================

(beads-defcommand beads-command-onboard (beads-command-global-options)
  ()
  :documentation "Represents bd onboard command.
Displays minimal snippet for AGENTS.md.")


;;; ============================================================
;;; Command Class: beads-command-prime
;;; ============================================================

(beads-defcommand beads-command-prime (beads-command-global-options)
  ()
  :documentation "Represents bd prime command.
Outputs AI-optimized workflow context.")


;;; ============================================================
;;; Command Class: beads-command-preflight
;;; ============================================================

(beads-defcommand beads-command-preflight (beads-command-global-options)
  ()
  :documentation "Represents bd preflight command.
Shows PR readiness checklist.")


;;; ============================================================
;;; Command Class: beads-command-upgrade
;;; ============================================================

(beads-defcommand beads-command-upgrade (beads-command-global-options)
  ()
  :documentation "Represents bd upgrade command.
Check and manage bd version upgrades.")


;;; ============================================================
;;; Command Class: beads-command-rename-prefix
;;; ============================================================

(beads-defcommand beads-command-rename-prefix (beads-command-global-options)
  ((old-prefix
    :initarg :old-prefix
    :type (or null string)
    :initform nil
    :documentation "Old prefix to replace."
    :positional 1)
   (new-prefix
    :initarg :new-prefix
    :type (or null string)
    :initform nil
    :documentation "New prefix to use."
    :positional 2))
  :documentation "Represents bd rename-prefix command.
Renames the issue prefix for all issues in the database."
  :cli-command "rename-prefix")

(cl-defmethod beads-command-validate ((command beads-command-rename-prefix))
  "Validate rename-prefix COMMAND."
  (with-slots (old-prefix new-prefix) command
    (cond
     ((not old-prefix) "Old prefix is required")
     ((not new-prefix) "New prefix is required")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-setup
;;; ============================================================

(beads-defcommand beads-command-setup (beads-command-global-options)
  ((editor
    :initarg :editor
    :type (or null string)
    :initform nil
    :documentation "Editor to set up (cursor, vscode, claude, etc.)."
    :positional 1))
  :documentation "Represents bd setup command.
Setup integration with AI editors.")


;;; ============================================================
;;; Command Class: beads-command-ship
;;; ============================================================

(beads-defcommand beads-command-ship (beads-command-global-options)
  ((capability
    :initarg :capability
    :type (or null string)
    :initform nil
    :documentation "Capability to publish."
    :positional 1))
  :documentation "Represents bd ship command.
Publishes a capability for cross-project dependencies.")


;;; ============================================================
;;; Command Class: beads-command-cook
;;; ============================================================

(beads-defcommand beads-command-cook (beads-command-global-options)
  ((formula-id
    :initarg :formula-id
    :type (or null string)
    :initform nil
    :documentation "Formula ID to compile."
    :positional 1))
  :documentation "Represents bd cook command.
Compiles a formula into a proto (ephemeral by default).")


;;; ============================================================
;;; Command Class: beads-command-mail
;;; ============================================================

(beads-defcommand beads-command-mail (beads-command-global-options)
  ()
  :documentation "Represents bd mail command.
Delegates to mail provider.")


;;; Execute Interactive Methods

























;;; Transient Menus

;;;###autoload (autoload 'beads-duplicate "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-duplicate "beads-duplicate"
  "Mark an issue as duplicate of another."
  beads-option-global-section)

;;;###autoload (autoload 'beads-duplicates "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-duplicates "beads-duplicates"
  "Find and optionally merge duplicate issues."
  beads-option-global-section)

;;;###autoload (autoload 'beads-supersede "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-supersede "beads-supersede"
  "Mark an issue as superseded by another."
  beads-option-global-section)

;;;###autoload (autoload 'beads-orphans "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-orphans "beads-orphans"
  "Identify orphaned issues."
  beads-option-global-section)

;;;###autoload (autoload 'beads-lint "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-lint "beads-lint"
  "Check issues for missing template sections."
  beads-option-global-section)

;;;###autoload (autoload 'beads-move "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-move "beads-move"
  "Move an issue to a different rig."
  beads-option-global-section)

;;;###autoload (autoload 'beads-refile "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-refile "beads-refile"
  "Move an issue to a different rig (simple)."
  beads-option-global-section)

;;;###autoload (autoload 'beads-q "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-q "beads-q"
  "Quick capture: create issue and output ID."
  beads-option-global-section)

;;;###autoload (autoload 'beads-note "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-note "beads-note"
  "Append a note to an issue."
  beads-option-global-section)

;;;###autoload (autoload 'beads-version "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-version "beads-version"
  "Print version information."
  beads-option-global-section)

;;;###autoload (autoload 'beads-where "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-where "beads-where"
  "Show active beads location."
  beads-option-global-section)

;;;###autoload (autoload 'beads-human "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-human "beads-human"
  "Show essential commands for humans."
  beads-option-global-section)

;;;###autoload (autoload 'beads-onboard "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-onboard "beads-onboard"
  "Display minimal snippet for AGENTS.md."
  beads-option-global-section)

;;;###autoload (autoload 'beads-prime "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-prime "beads-prime"
  "Output AI-optimized workflow context."
  beads-option-global-section)

;;;###autoload (autoload 'beads-preflight "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-preflight "beads-preflight"
  "Show PR readiness checklist."
  beads-option-global-section)

;;;###autoload (autoload 'beads-upgrade "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-upgrade "beads-upgrade"
  "Check and manage version upgrades."
  beads-option-global-section)

;;;###autoload (autoload 'beads-rename-prefix "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-rename-prefix "beads-rename-prefix"
  "Rename issue prefix for all issues."
  beads-option-global-section)

;;;###autoload (autoload 'beads-setup "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-setup "beads-setup"
  "Setup integration with AI editors."
  beads-option-global-section)

;;;###autoload (autoload 'beads-ship "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-ship "beads-ship"
  "Publish capability for cross-project deps."
  beads-option-global-section)

;;;###autoload (autoload 'beads-cook "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-cook "beads-cook"
  "Compile formula into proto."
  beads-option-global-section)

;;;###autoload (autoload 'beads-mail "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-mail "beads-mail"
  "Delegate to mail provider."
  beads-option-global-section)

;;; Stub commands for bd --help coverage
;;
;; These commands wrap bd subcommands that don't yet have full
;; transient implementations.  They use beads-defcommand to define
;; minimal command classes and beads-meta-define-transient to create
;; a simple transient UI for each.

;;; Working With Issues — stubs

(beads-defcommand beads-command-children (beads-command-global-options)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Parent issue ID."
    :positional 1
    :key "i"
    :transient "Parent issue ID (required)"
    :class transient-option
    :argument "--id="
    :prompt "Parent issue ID: "
    :transient-group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd children command.
Lists child beads of a parent issue.")

;;;###autoload (autoload 'beads-children "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-children "beads-children"
  "List child beads of a parent issue."
  beads-option-global-section)

(beads-defcommand beads-command-create-form (beads-command-global-options)
  ()
  :documentation "Represents bd create-form command.
Creates a new issue using an interactive form."
  :cli-command "create-form")

;;;###autoload (autoload 'beads-create-form "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-create-form "beads-create-form"
  "Create a new issue using an interactive form."
  beads-option-global-section)

(beads-defcommand beads-command-promote (beads-command-global-options)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID (wisp) to promote."
    :positional 1
    :key "i"
    :transient "Issue ID (required)"
    :class transient-option
    :argument "--id="
    :prompt "Issue ID to promote: "
    :transient-group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd promote command.
Promotes a wisp to a permanent bead.")

;;;###autoload (autoload 'beads-promote "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-promote "beads-promote"
  "Promote a wisp to a permanent bead."
  beads-option-global-section)

(beads-defcommand beads-command-query (beads-command-global-options)
  ((query-string
    :initarg :query-string
    :type (or null string)
    :initform nil
    :documentation "Query string."
    :positional 1
    :key "q"
    :transient "Query (required)"
    :class transient-option
    :argument "--query="
    :prompt "Query: "
    :transient-group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd query command.
Queries issues using a simple query language.")

;;;###autoload (autoload 'beads-query "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-query "beads-query"
  "Query issues using a simple query language."
  beads-option-global-section)

(beads-defcommand beads-command-todo (beads-command-global-options)
  ()
  :documentation "Represents bd todo command.
Manages TODO items (convenience wrapper for task issues).")

;;;###autoload (autoload 'beads-todo "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-todo "beads-todo"
  "Manage TODO items."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-todo-add
;;; ============================================================

(beads-defcommand beads-command-todo-add (beads-command-global-options)
  ((title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Title of the TODO item (positional arg)."
    :positional 1
    :option-type :string
    :key "T"
    :transient "Title (required)"
    :class transient-option
    :argument "--title="
    :prompt "TODO title: "
    :transient-group "Add TODO"
    :level 1
    :order 1
    :required t)
   (priority
    :initarg :priority
    :type (or null string)
    :initform nil
    :documentation "Priority (-p, --priority).
Values: 0-4. Default: 2."
    :long-option "priority"
    :short-option "p"
    :option-type :string
    :key "p"
    :transient "--priority"
    :class transient-option
    :argument "--priority="
    :prompt "Priority (0-4): "
    :transient-reader beads-reader-issue-priority
    :transient-group "Add TODO"
    :level 2
    :order 2)
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Description (-d, --description)."
    :long-option "description"
    :short-option "d"
    :option-type :string
    :key "D"
    :transient "--description"
    :class transient-option
    :argument "--description="
    :prompt "Description: "
    :transient-group "Add TODO"
    :level 2
    :order 3))
  :documentation "Represents bd todo add command.
Adds a new TODO item (task issue with priority 2).")


(cl-defmethod beads-command-validate ((command beads-command-todo-add))
  "Validate todo add COMMAND.
Checks that title is provided.
Returns error string or nil if valid."
  (with-slots (title) command
    (if (or (null title) (string-empty-p title))
        "Must provide a title"
      nil)))

;;;###autoload (autoload 'beads-todo-add "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-todo-add "beads-todo-add"
  "Add a new TODO item."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-todo-list
;;; ============================================================

(beads-defcommand beads-command-todo-list (beads-command-global-options)
  ((all
    :initarg :all
    :type boolean
    :initform nil
    :documentation "Show all TODOs including completed (--all)."
    :long-option "all"
    :option-type :boolean
    :key "a"
    :transient "--all"
    :class transient-switch
    :argument "--all"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd todo list command.
Lists TODO items (open task issues).")

;;;###autoload (autoload 'beads-todo-list "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-todo-list "beads-todo-list"
  "List TODO items."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-todo-done
;;; ============================================================

(beads-defcommand beads-command-todo-done (beads-command-global-options)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more TODO issue IDs to mark done (positional)."
    :positional 1
    :option-type :list
    :option-separator " "
    :key "i"
    :transient "Issue ID (required)"
    :class transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Mark Done"
    :level 1
    :order 1
    :required t)
   (reason
    :initarg :reason
    :type (or null string)
    :initform nil
    :documentation "Reason for closing (--reason). Default: Completed."
    :long-option "reason"
    :option-type :string
    :key "r"
    :transient "--reason"
    :class transient-option
    :argument "--reason="
    :prompt "Reason: "
    :transient-group "Mark Done"
    :level 2
    :order 2))
  :documentation "Represents bd todo done command.
Marks one or more TODO items as done.")


(cl-defmethod beads-command-validate ((command beads-command-todo-done))
  "Validate todo done COMMAND.
Checks that at least one issue ID is provided.
Returns error string or nil if valid."
  (with-slots (issue-ids) command
    (if (or (null issue-ids) (zerop (length issue-ids)))
        "Must provide at least one issue ID"
      nil)))

;;;###autoload (autoload 'beads-todo-done "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-todo-done "beads-todo-done"
  "Mark TODO items as done."
  beads-option-global-section)

;;; Views & Reports — stubs

(beads-defcommand beads-command-types (beads-command-global-options)
  ()
  :documentation "Represents bd types command.
Lists valid issue types.")

;;;###autoload (autoload 'beads-types "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-types "beads-types"
  "List valid issue types."
  beads-option-global-section)

(beads-defcommand beads-command-find-duplicates (beads-command-global-options)
  ()
  :documentation "Represents bd find-duplicates command.
Finds semantically similar issues using text analysis or AI."
  :cli-command "find-duplicates")

;;;###autoload (autoload 'beads-find-duplicates "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-find-duplicates "beads-find-duplicates"
  "Find semantically similar issues using AI."
  beads-option-global-section)

;;; Sync & Data — stubs

;;; ============================================================
;;; Command Class: beads-command-backup (root)
;;; ============================================================

(beads-defcommand beads-command-backup (beads-command-global-options)
  ((force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Export even if nothing changed."
    :long-option "force"
    :option-type :boolean
    :key "f"
    :transient "--force"
    :class transient-switch
    :argument "--force"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd backup command.
Backs up your beads database by exporting all tables to JSONL.")

;;;###autoload (autoload 'beads-backup-root "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-backup "beads-backup-root"
  "Back up your beads database (export JSONL snapshot)."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-backup-init
;;; ============================================================

(beads-defcommand beads-command-backup-init (beads-command-global-options)
  ((path
    :initarg :path
    :type (or null string)
    :initform nil
    :documentation "Backup destination path or URL (required).
Local path: /mnt/usb/beads-backup or ~/Dropbox/beads-backup.
DoltHub: https://doltremoteapi.dolthub.com/user/repo"
    :positional 1
    :option-type :string
    :key "p"
    :transient "Path or URL (required)"
    :class transient-option
    :argument "--path="
    :prompt "Backup destination (path or DoltHub URL): "
    :transient-group "Options"
    :level 1
    :order 1
    :required t))
  :documentation "Represents bd backup init command.
Configures a Dolt backup destination (filesystem path or DoltHub URL).")

(cl-defmethod beads-command-validate ((command beads-command-backup-init))
  "Validate backup init COMMAND.  Requires path."
  (with-slots (path) command
    (cond
     ((not path) "Backup destination path is required")
     ((string-empty-p path) "Backup destination path cannot be empty")
     (t nil))))

;;;###autoload (autoload 'beads-backup-init "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-backup-init "beads-backup-init"
  "Configure a Dolt backup destination.

Set up a filesystem path or DoltHub URL as the backup destination.
After initializing, run 'bd backup sync' to push your data."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-backup-status
;;; ============================================================

(beads-defcommand beads-command-backup-status (beads-command-global-options)
  ()
  :documentation "Represents bd backup status command.
Shows last JSONL and Dolt backup status.")

;;;###autoload (autoload 'beads-backup-status "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-backup-status "beads-backup-status"
  "Show last backup status.

Displays JSONL backup timestamp, Dolt backup configuration,
and database size."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-backup-sync
;;; ============================================================

(beads-defcommand beads-command-backup-sync (beads-command-global-options)
  ()
  :documentation "Represents bd backup sync command.
Pushes the database to the configured Dolt backup destination.")

;;;###autoload (autoload 'beads-backup-sync "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-backup-sync "beads-backup-sync"
  "Push database to configured Dolt backup destination.

Syncs the full database state (all branches, full history) to the
backup location configured with 'bd backup init'.  Run 'bd backup
init <path>' first to configure a destination."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-backup-restore
;;; ============================================================

(beads-defcommand beads-command-backup-restore (beads-command-global-options)
  ((path
    :initarg :path
    :type (or null string)
    :initform nil
    :documentation "Path to directory containing JSONL backup files.
Defaults to .beads/backup/ if not specified."
    :positional 1
    :option-type :string
    :key "p"
    :transient "Backup directory path"
    :class transient-option
    :argument "--path="
    :prompt "Backup directory (leave empty for .beads/backup/): "
    :transient-group "Options"
    :level 1
    :order 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Show what would be restored without making changes."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd backup restore command.
Restores the database from JSONL backup files.")

;;;###autoload (autoload 'beads-backup-restore "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-backup-restore "beads-backup-restore"
  "Restore database from JSONL backup files.

Reads from .beads/backup/ by default, or from a specified path.
Use --dry-run to preview without making changes."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-backup-export-git
;;; ============================================================

(beads-defcommand beads-command-backup-export-git (beads-command-global-options)
  ((branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Target git branch for backup artifacts."
    :long-option "branch"
    :option-type :string
    :key "b"
    :transient "--branch"
    :class transient-option
    :argument "--branch="
    :prompt "Git branch (default: bd-backup): "
    :transient-group "Options"
    :level 1
    :order 1)
   (remote
    :initarg :remote
    :type (or null string)
    :initform nil
    :documentation "Git remote to push."
    :long-option "remote"
    :option-type :string
    :key "r"
    :transient "--remote"
    :class transient-option
    :argument "--remote="
    :prompt "Git remote (default: origin): "
    :transient-group "Options"
    :level 1
    :order 2)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Show what would happen without creating a worktree or pushing."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 3)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force a fresh backup export before comparing and copying."
    :long-option "force"
    :option-type :boolean
    :key "f"
    :transient "--force"
    :class transient-switch
    :argument "--force"
    :transient-group "Options"
    :level 1
    :order 4))
  :documentation "Represents bd backup export-git command.
Exports the current JSONL backup snapshot to a git branch."
  :cli-command "backup export-git")

;;;###autoload (autoload 'beads-backup-export-git "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-backup-export-git
  "beads-backup-export-git"
  "Export JSONL backup snapshot to a git branch.

Copies the backup snapshot into the specified git branch, commits
if changed, and pushes.  Use --dry-run to preview."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-backup-fetch-git
;;; ============================================================

(beads-defcommand beads-command-backup-fetch-git (beads-command-global-options)
  ((branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Git branch to fetch backup artifacts from."
    :long-option "branch"
    :option-type :string
    :key "b"
    :transient "--branch"
    :class transient-option
    :argument "--branch="
    :prompt "Git branch (default: bd-backup): "
    :transient-group "Options"
    :level 1
    :order 1)
   (remote
    :initarg :remote
    :type (or null string)
    :initform nil
    :documentation "Git remote to fetch from."
    :long-option "remote"
    :option-type :string
    :key "r"
    :transient "--remote"
    :class transient-option
    :argument "--remote="
    :prompt "Git remote (default: origin): "
    :transient-group "Options"
    :level 1
    :order 2)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Show what would happen without fetching or restoring."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 3))
  :documentation "Represents bd backup fetch-git command.
Fetches a JSONL backup snapshot from a git branch and restores it."
  :cli-command "backup fetch-git")

;;;###autoload (autoload 'beads-backup-fetch-git "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-backup-fetch-git
  "beads-backup-fetch-git"
  "Fetch JSONL backup snapshot from a git branch and restore it.

Companion to 'bd backup export-git'.  Fetches from a git branch
into a temporary worktree and restores.  Use --dry-run to preview."
  beads-option-global-section)

;;; ============================================================
;;; Parent Transient Menu: beads-backup
;;; ============================================================

;;;###autoload (autoload 'beads-backup "beads-command-misc" nil t)
(transient-define-prefix beads-backup ()
  "Back up and restore your beads database.

JSONL backup commands (portable snapshot):
  Backup: Export all tables to .beads/backup/*.jsonl
  Status: Show last backup status
  Restore: Import from JSONL files
  Export-git: Publish snapshot to a git branch
  Fetch-git: Restore from a git branch snapshot

Dolt-native backup commands (preserve full commit history):
  Init: Configure a backup destination (filesystem or DoltHub)
  Sync: Push to configured destination"
  ["JSONL Backup"
   ("B" "Backup now (export JSONL)" beads-backup-root)
   ("s" "Status" beads-backup-status)
   ("r" "Restore from JSONL" beads-backup-restore)
   ("e" "Export to git branch" beads-backup-export-git)
   ("F" "Fetch from git branch" beads-backup-fetch-git)]
  ["Dolt-native Backup"
   ("i" "Init destination" beads-backup-init)
   ("S" "Sync to backup" beads-backup-sync)]
  ["Quick Actions"
   ("q" "Quit" transient-quit-one)])

(beads-defcommand beads-command-export (beads-command-global-options)
  ()
  :documentation "Represents bd export command.
Exports issues to JSONL format.")

;;;###autoload (autoload 'beads-export "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-export "beads-export"
  "Export issues to JSONL format."
  beads-option-global-section)

(beads-defcommand beads-command-import (beads-command-global-options)
  ((file
    :initarg :file
    :type (or null string)
    :initform nil
    :documentation "JSONL file to import (default: .beads/issues.jsonl)."
    :positional 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Show what would be imported without importing."
    :long-option "dry-run"
    :option-type :boolean
    :key "n"
    :transient "--dry-run"
    :class transient-switch
    :argument "--dry-run"
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd import command.
Imports issues from a JSONL file into the database.")

;;;###autoload (autoload 'beads-import "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-import "beads-import"
  "Import issues from a JSONL file."
  beads-option-global-section)

;;; Maintenance — stubs

(beads-defcommand beads-command-flatten (beads-command-global-options)
  ()
  :documentation "Represents bd flatten command.
Squashes all Dolt history into a single commit.")

;;;###autoload (autoload 'beads-flatten "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-flatten "beads-flatten"
  "Squash all Dolt history into a single commit."
  beads-option-global-section)

(beads-defcommand beads-command-gc (beads-command-global-options)
  ()
  :documentation "Represents bd gc command.
Garbage collects: decays old issues, compacts Dolt, runs GC.")

;;;###autoload (autoload 'beads-gc "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-gc "beads-gc"
  "Garbage collect: decay issues, compact Dolt, run GC."
  beads-option-global-section)

(beads-defcommand beads-command-purge (beads-command-global-options)
  ()
  :documentation "Represents bd purge command.
Deletes closed ephemeral beads to reclaim space.")

;;;###autoload (autoload 'beads-purge "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-purge "beads-purge"
  "Delete closed ephemeral beads to reclaim space."
  beads-option-global-section)

;;; Setup & Configuration — memory stubs

(beads-defcommand beads-command-forget (beads-command-global-options)
  ((memory-id
    :initarg :memory-id
    :type (or null string)
    :initform nil
    :documentation "Memory ID to remove."
    :positional 1
    :key "m"
    :transient "Memory ID (required)"
    :class transient-option
    :argument "--id="
    :prompt "Memory ID: "
    :transient-group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd forget command.
Removes a persistent memory.")

;;;###autoload (autoload 'beads-forget "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-forget "beads-forget"
  "Remove a persistent memory."
  beads-option-global-section)

(beads-defcommand beads-command-kv (beads-command-global-options)
  ()
  :documentation "Represents bd kv command.
Key-value store commands.")

;;;###autoload (autoload 'beads-kv "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-kv "beads-kv"
  "Key-value store commands."
  beads-option-global-section)

(beads-defcommand beads-command-memories (beads-command-global-options)
  ()
  :documentation "Represents bd memories command.
Lists or searches persistent memories.")

;;;###autoload (autoload 'beads-memories "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-memories "beads-memories"
  "List or search persistent memories."
  beads-option-global-section)

(beads-defcommand beads-command-recall (beads-command-global-options)
  ((memory-id
    :initarg :memory-id
    :type (or null string)
    :initform nil
    :documentation "Memory ID to retrieve."
    :positional 1
    :key "m"
    :transient "Memory ID (required)"
    :class transient-option
    :argument "--id="
    :prompt "Memory ID: "
    :transient-group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd recall command.
Retrieves a specific memory.")

;;;###autoload (autoload 'beads-recall "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-recall "beads-recall"
  "Retrieve a specific memory."
  beads-option-global-section)

(beads-defcommand beads-command-remember (beads-command-global-options)
  ((content
    :initarg :content
    :type (or null string)
    :initform nil
    :documentation "Memory content to store."
    :positional 1
    :key "c"
    :transient "Content (required)"
    :class transient-option
    :argument "--content="
    :prompt "Memory content: "
    :transient-group "Options"
    :level 1
    :order 0))
  :documentation "Represents bd remember command.
Stores a persistent memory.")

;;;###autoload (autoload 'beads-remember "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-remember "beads-remember"
  "Store a persistent memory."
  beads-option-global-section)

;;; Additional Commands — rename stub

(beads-defcommand beads-command-rename (beads-command-global-options)
  ((old-id
    :initarg :old-id
    :type (or null string)
    :initform nil
    :documentation "Current issue ID to rename."
    :positional 1
    :key "o"
    :transient "Old issue ID (required)"
    :class transient-option
    :argument "--old-id="
    :prompt "Old issue ID: "
    :transient-group "Rename Issue"
    :level 1
    :order 0)
   (new-id
    :initarg :new-id
    :type (or null string)
    :initform nil
    :documentation "New issue ID to assign."
    :positional 2
    :key "n"
    :transient "New issue ID (required)"
    :class transient-option
    :argument "--new-id="
    :prompt "New issue ID: "
    :transient-group "Rename Issue"
    :level 1
    :order 1))
  :documentation "Represents bd rename command.
Renames an issue from one ID to another, updating all references.")


(cl-defmethod beads-command-validate ((command beads-command-rename))
  "Validate rename COMMAND."
  (with-slots (old-id new-id) command
    (cond
     ((not old-id) "Old issue ID is required")
     ((not new-id) "New issue ID is required")
     (t nil))))

;;;###autoload (autoload 'beads-rename "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-rename "beads-rename"
  "Rename an issue from one ID to another."
  beads-option-global-section)

;;; Context Command

(beads-defcommand beads-command-context (beads-command-global-options)
  ()
  :documentation "Represents bd context command.
Show effective backend identity and repository context.")

;;;###autoload (autoload 'beads-context "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-context "beads-context"
  "Show backend identity and repository context."
  beads-option-global-section)

(provide 'beads-command-misc)
;;; beads-command-misc.el ends here
