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

(beads-defcommand beads-command-duplicate (beads-command-json)
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

(beads-defcommand beads-command-duplicates (beads-command-json)
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

(beads-defcommand beads-command-supersede (beads-command-json)
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

(beads-defcommand beads-command-orphans (beads-command-json)
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

(beads-defcommand beads-command-lint (beads-command-json)
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

(beads-defcommand beads-command-move (beads-command-json)
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

(beads-defcommand beads-command-refile (beads-command-json)
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

(beads-defcommand beads-command-q (beads-command-json)
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
;;; Command Class: beads-command-version
;;; ============================================================

(beads-defcommand beads-command-version (beads-command-json)
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

(beads-defcommand beads-command-where (beads-command-json)
  ()
  :documentation "Represents bd where command.
Shows active beads location.")


;;; ============================================================
;;; Command Class: beads-command-human
;;; ============================================================

(beads-defcommand beads-command-human (beads-command-json)
  ()
  :documentation "Represents bd human command.
Shows essential commands for human users.")


;;; ============================================================
;;; Command Class: beads-command-onboard
;;; ============================================================

(beads-defcommand beads-command-onboard (beads-command-json)
  ()
  :documentation "Represents bd onboard command.
Displays minimal snippet for AGENTS.md.")


;;; ============================================================
;;; Command Class: beads-command-prime
;;; ============================================================

(beads-defcommand beads-command-prime (beads-command-json)
  ()
  :documentation "Represents bd prime command.
Outputs AI-optimized workflow context.")


;;; ============================================================
;;; Command Class: beads-command-preflight
;;; ============================================================

(beads-defcommand beads-command-preflight (beads-command-json)
  ()
  :documentation "Represents bd preflight command.
Shows PR readiness checklist.")


;;; ============================================================
;;; Command Class: beads-command-upgrade
;;; ============================================================

(beads-defcommand beads-command-upgrade (beads-command-json)
  ()
  :documentation "Represents bd upgrade command.
Check and manage bd version upgrades.")


;;; ============================================================
;;; Command Class: beads-command-rename-prefix
;;; ============================================================

(beads-defcommand beads-command-rename-prefix (beads-command-json)
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
Renames the issue prefix for all issues in the database.")

(cl-defmethod beads-command-subcommand ((_command beads-command-rename-prefix))
  "Return \"rename-prefix\" as the CLI subcommand."
  "rename-prefix")

(cl-defmethod beads-command-validate ((command beads-command-rename-prefix))
  "Validate rename-prefix COMMAND."
  (with-slots (old-prefix new-prefix) command
    (cond
     ((not old-prefix) "Old prefix is required")
     ((not new-prefix) "New prefix is required")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-repair
;;; ============================================================

(beads-defcommand beads-command-repair (beads-command-json)
  ()
  :documentation "Represents bd repair command.
Repairs corrupted database by cleaning orphaned references.")


;;; ============================================================
;;; Command Class: beads-command-resolve-conflicts
;;; ============================================================

(beads-defcommand beads-command-resolve-conflicts (beads-command-json)
  ()
  :documentation "Represents bd resolve-conflicts command.
Resolves git merge conflicts in JSONL files.")

(cl-defmethod beads-command-subcommand ((_command beads-command-resolve-conflicts))
  "Return \"resolve-conflicts\" as the CLI subcommand."
  "resolve-conflicts")

;;; ============================================================
;;; Command Class: beads-command-restore
;;; ============================================================

(beads-defcommand beads-command-restore (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to restore."
    :positional 1))
  :documentation "Represents bd restore command.
Restores full history of a compacted issue from git.")


(cl-defmethod beads-command-validate ((command beads-command-restore))
  "Validate restore COMMAND."
  (with-slots (issue-id) command
    (if (not issue-id) "Issue ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-merge
;;; ============================================================

(beads-defcommand beads-command-merge (beads-command-json)
  ((ancestor
    :initarg :ancestor
    :type (or null string)
    :initform nil
    :documentation "Ancestor file path."
    :positional 1)
   (current
    :initarg :current
    :type (or null string)
    :initform nil
    :documentation "Current file path."
    :positional 2)
   (other
    :initarg :other
    :type (or null string)
    :initform nil
    :documentation "Other file path."
    :positional 3))
  :documentation "Represents bd merge command.
Git merge driver for beads JSONL files.")


;;; ============================================================
;;; Command Class: beads-command-setup
;;; ============================================================

(beads-defcommand beads-command-setup (beads-command-json)
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

(beads-defcommand beads-command-ship (beads-command-json)
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

(beads-defcommand beads-command-cook (beads-command-json)
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

(beads-defcommand beads-command-mail (beads-command-json)
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

;;;###autoload (autoload 'beads-repair "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-repair "beads-repair"
  "Repair corrupted database."
  beads-option-global-section)

;;;###autoload (autoload 'beads-resolve-conflicts "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-resolve-conflicts "beads-resolve-conflicts"
  "Resolve git merge conflicts in JSONL files."
  beads-option-global-section)

;;;###autoload (autoload 'beads-restore "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-restore "beads-restore"
  "Restore full history of compacted issue."
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

(beads-defcommand beads-command-children (beads-command-json)
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

(beads-defcommand beads-command-create-form (beads-command-json)
  ()
  :documentation "Represents bd create-form command.
Creates a new issue using an interactive form.")

(cl-defmethod beads-command-subcommand ((_command beads-command-create-form))
  "Return \"create-form\" as the CLI subcommand."
  "create-form")

;;;###autoload (autoload 'beads-create-form "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-create-form "beads-create-form"
  "Create a new issue using an interactive form."
  beads-option-global-section)

(beads-defcommand beads-command-promote (beads-command-json)
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

(beads-defcommand beads-command-query (beads-command-json)
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

(beads-defcommand beads-command-todo (beads-command-json)
  ()
  :documentation "Represents bd todo command.
Manages TODO items (convenience wrapper for task issues).")

;;;###autoload (autoload 'beads-todo "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-todo "beads-todo"
  "Manage TODO items."
  beads-option-global-section)

;;; Views & Reports — stubs

(beads-defcommand beads-command-types (beads-command-json)
  ()
  :documentation "Represents bd types command.
Lists valid issue types.")

;;;###autoload (autoload 'beads-types "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-types "beads-types"
  "List valid issue types."
  beads-option-global-section)

(beads-defcommand beads-command-find-duplicates (beads-command-json)
  ()
  :documentation "Represents bd find-duplicates command.
Finds semantically similar issues using text analysis or AI.")

(cl-defmethod beads-command-subcommand ((_command beads-command-find-duplicates))
  "Return \"find-duplicates\" as the CLI subcommand."
  "find-duplicates")

;;;###autoload (autoload 'beads-find-duplicates "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-find-duplicates "beads-find-duplicates"
  "Find semantically similar issues using AI."
  beads-option-global-section)

;;; Sync & Data — stubs

(beads-defcommand beads-command-backup (beads-command-json)
  ()
  :documentation "Represents bd backup command.
Backs up your beads database.")

;;;###autoload (autoload 'beads-backup "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-backup "beads-backup"
  "Back up your beads database."
  beads-option-global-section)

(beads-defcommand beads-command-export (beads-command-json)
  ()
  :documentation "Represents bd export command.
Exports issues to JSONL format.")

;;;###autoload (autoload 'beads-export "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-export "beads-export"
  "Export issues to JSONL format."
  beads-option-global-section)

;;; Maintenance — stubs

(beads-defcommand beads-command-flatten (beads-command-json)
  ()
  :documentation "Represents bd flatten command.
Squashes all Dolt history into a single commit.")

;;;###autoload (autoload 'beads-flatten "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-flatten "beads-flatten"
  "Squash all Dolt history into a single commit."
  beads-option-global-section)

(beads-defcommand beads-command-gc (beads-command-json)
  ()
  :documentation "Represents bd gc command.
Garbage collects: decays old issues, compacts Dolt, runs GC.")

;;;###autoload (autoload 'beads-gc "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-gc "beads-gc"
  "Garbage collect: decay issues, compact Dolt, run GC."
  beads-option-global-section)

(beads-defcommand beads-command-purge (beads-command-json)
  ()
  :documentation "Represents bd purge command.
Deletes closed ephemeral beads to reclaim space.")

;;;###autoload (autoload 'beads-purge "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-purge "beads-purge"
  "Delete closed ephemeral beads to reclaim space."
  beads-option-global-section)

;;; Setup & Configuration — memory stubs

(beads-defcommand beads-command-forget (beads-command-json)
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

(beads-defcommand beads-command-kv (beads-command-json)
  ()
  :documentation "Represents bd kv command.
Key-value store commands.")

;;;###autoload (autoload 'beads-kv "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-kv "beads-kv"
  "Key-value store commands."
  beads-option-global-section)

(beads-defcommand beads-command-memories (beads-command-json)
  ()
  :documentation "Represents bd memories command.
Lists or searches persistent memories.")

;;;###autoload (autoload 'beads-memories "beads-command-misc" nil t)
(beads-meta-define-transient beads-command-memories "beads-memories"
  "List or search persistent memories."
  beads-option-global-section)

(beads-defcommand beads-command-recall (beads-command-json)
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

(beads-defcommand beads-command-remember (beads-command-json)
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

(provide 'beads-command-misc)
;;; beads-command-misc.el ends here
