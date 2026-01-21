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

(eval-and-compile
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
    :transient-key "o"
    :transient-description "--of"
    :transient-class transient-option
    :transient-argument "--of="
    :transient-prompt "Canonical issue: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd duplicate command.
Marks an issue as a duplicate of another."))

(cl-defmethod beads-command-subcommand ((_command beads-command-duplicate))
  "Return \"duplicate\" as the CLI subcommand."
  "duplicate")

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

(eval-and-compile
(beads-defcommand beads-command-duplicates (beads-command-json)
  ((merge
    :initarg :merge
    :type boolean
    :initform nil
    :documentation "Merge duplicates with confirmation."
    :long-option "merge"
    :option-type :boolean
    :transient-key "m"
    :transient-description "--merge"
    :transient-class transient-switch
    :transient-argument "--merge"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd duplicates command.
Finds and optionally merges duplicate issues."))

(cl-defmethod beads-command-subcommand ((_command beads-command-duplicates))
  "Return \"duplicates\" as the CLI subcommand."
  "duplicates")

;;; ============================================================
;;; Command Class: beads-command-supersede
;;; ============================================================

(eval-and-compile
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
    :transient-key "w"
    :transient-description "--with"
    :transient-class transient-option
    :transient-argument "--with="
    :transient-prompt "Replacement issue: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd supersede command.
Marks an issue as superseded by a newer one."))

(cl-defmethod beads-command-subcommand ((_command beads-command-supersede))
  "Return \"supersede\" as the CLI subcommand."
  "supersede")

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

(eval-and-compile
(beads-defcommand beads-command-orphans (beads-command-json)
  ((details
    :initarg :details
    :type boolean
    :initform nil
    :documentation "Show full commit information."
    :long-option "details"
    :option-type :boolean
    :transient-key "d"
    :transient-description "--details"
    :transient-class transient-switch
    :transient-argument "--details"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (fix
    :initarg :fix
    :type boolean
    :initform nil
    :documentation "Close orphaned issues with confirmation."
    :long-option "fix"
    :short-option "f"
    :option-type :boolean
    :transient-key "f"
    :transient-description "--fix"
    :transient-class transient-switch
    :transient-argument "--fix"
    :transient-group "Options"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd orphans command.
Identifies orphaned issues referenced in commits but still open."))

(cl-defmethod beads-command-subcommand ((_command beads-command-orphans))
  "Return \"orphans\" as the CLI subcommand."
  "orphans")

;;; ============================================================
;;; Command Class: beads-command-lint
;;; ============================================================

(eval-and-compile
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
    :transient-key "s"
    :transient-description "--status"
    :transient-class transient-option
    :transient-argument "--status="
    :transient-prompt "Status: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Filter by issue type."
    :long-option "type"
    :short-option "t"
    :option-type :string
    :transient-key "t"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Type: "
    :transient-choices ("bug" "task" "feature" "epic" "chore")
    :transient-group "Options"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd lint command.
Checks issues for missing template sections."))

(cl-defmethod beads-command-subcommand ((_command beads-command-lint))
  "Return \"lint\" as the CLI subcommand."
  "lint")

;;; ============================================================
;;; Command Class: beads-command-move
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-move (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to move."
    :positional 1
    ;; Transient properties for UI input
    :transient-key "i"
    :transient-description "Issue ID (required)"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-reader beads-reader-move-issue-id
    :transient-prompt "Issue ID: "
    :transient-group "Move Issue"
    :transient-level 1
    :transient-order 0)
   (to
    :initarg :to
    :type (or null string)
    :initform nil
    :documentation "Target rig or prefix."
    :long-option "to"
    :option-type :string
    :transient-key "t"
    :transient-description "--to (required)"
    :transient-class transient-option
    :transient-argument "--to="
    :transient-prompt "Target rig: "
    :transient-group "Move Issue"
    :transient-level 1
    :transient-order 1)
   (keep-open
    :initarg :keep-open
    :type boolean
    :initform nil
    :documentation "Keep source issue open."
    :long-option "keep-open"
    :option-type :boolean
    :transient-key "k"
    :transient-description "--keep-open"
    :transient-class transient-switch
    :transient-argument "--keep-open"
    :transient-group "Options"
    :transient-level 2
    :transient-order 2)
   (skip-deps
    :initarg :skip-deps
    :type boolean
    :initform nil
    :documentation "Skip dependency remapping."
    :long-option "skip-deps"
    :option-type :boolean
    :transient-key "d"
    :transient-description "--skip-deps"
    :transient-class transient-switch
    :transient-argument "--skip-deps"
    :transient-group "Options"
    :transient-level 2
    :transient-order 3))
  :documentation "Represents bd move command.
Moves an issue to a different rig with dependency remapping."))

(cl-defmethod beads-command-subcommand ((_command beads-command-move))
  "Return \"move\" as the CLI subcommand."
  "move")

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

(eval-and-compile
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
    :transient-key "k"
    :transient-description "--keep-open"
    :transient-class transient-switch
    :transient-argument "--keep-open"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd refile command.
Moves an issue to a different rig."))

(cl-defmethod beads-command-subcommand ((_command beads-command-refile))
  "Return \"refile\" as the CLI subcommand."
  "refile")

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

(eval-and-compile
(beads-defcommand beads-command-q (beads-command-json)
  ((title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Issue title."
    :positional 1)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Issue type (default: task)."
    :long-option "type"
    :short-option "t"
    :option-type :string
    :transient-key "t"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Type: "
    :transient-choices ("task" "bug" "feature" "epic" "chore")
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (priority
    :initarg :priority
    :type (or null string)
    :initform nil
    :documentation "Priority (0-4 or P0-P4)."
    :long-option "priority"
    :short-option "p"
    :option-type :string
    :transient-key "p"
    :transient-description "--priority"
    :transient-class transient-option
    :transient-argument "--priority="
    :transient-prompt "Priority: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (labels
    :initarg :labels
    :type list
    :initform nil
    :documentation "Labels for the issue."
    :long-option "labels"
    :short-option "l"
    :option-type :list
    :transient-key "l"
    :transient-description "--labels"
    :transient-class transient-option
    :transient-argument "--labels="
    :transient-prompt "Labels: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 3))
  :documentation "Represents bd q command.
Quick capture: creates issue and outputs only ID."))

(cl-defmethod beads-command-subcommand ((_command beads-command-q))
  "Return \"q\" as the CLI subcommand."
  "q")

(cl-defmethod beads-command-validate ((command beads-command-q))
  "Validate q COMMAND."
  (with-slots (title) command
    (if (not title) "Title is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-version
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-version (beads-command-json)
  ((daemon
    :initarg :daemon
    :type boolean
    :initform nil
    :documentation "Check daemon version and compatibility."
    :long-option "daemon"
    :option-type :boolean
    :transient-key "d"
    :transient-description "--daemon"
    :transient-class transient-switch
    :transient-argument "--daemon"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd version command.
Prints version information."))

(cl-defmethod beads-command-subcommand ((_command beads-command-version))
  "Return \"version\" as the CLI subcommand."
  "version")

;;; ============================================================
;;; Command Class: beads-command-where
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-where (beads-command-json)
  ()
  :documentation "Represents bd where command.
Shows active beads location."))

(cl-defmethod beads-command-subcommand ((_command beads-command-where))
  "Return \"where\" as the CLI subcommand."
  "where")

;;; ============================================================
;;; Command Class: beads-command-human
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-human (beads-command-json)
  ()
  :documentation "Represents bd human command.
Shows essential commands for human users."))

(cl-defmethod beads-command-subcommand ((_command beads-command-human))
  "Return \"human\" as the CLI subcommand."
  "human")

;;; ============================================================
;;; Command Class: beads-command-onboard
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-onboard (beads-command-json)
  ()
  :documentation "Represents bd onboard command.
Displays minimal snippet for AGENTS.md."))

(cl-defmethod beads-command-subcommand ((_command beads-command-onboard))
  "Return \"onboard\" as the CLI subcommand."
  "onboard")

;;; ============================================================
;;; Command Class: beads-command-prime
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-prime (beads-command-json)
  ()
  :documentation "Represents bd prime command.
Outputs AI-optimized workflow context."))

(cl-defmethod beads-command-subcommand ((_command beads-command-prime))
  "Return \"prime\" as the CLI subcommand."
  "prime")

;;; ============================================================
;;; Command Class: beads-command-preflight
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-preflight (beads-command-json)
  ()
  :documentation "Represents bd preflight command.
Shows PR readiness checklist."))

(cl-defmethod beads-command-subcommand ((_command beads-command-preflight))
  "Return \"preflight\" as the CLI subcommand."
  "preflight")

;;; ============================================================
;;; Command Class: beads-command-upgrade
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-upgrade (beads-command-json)
  ()
  :documentation "Represents bd upgrade command.
Check and manage bd version upgrades."))

(cl-defmethod beads-command-subcommand ((_command beads-command-upgrade))
  "Return \"upgrade\" as the CLI subcommand."
  "upgrade")

;;; ============================================================
;;; Command Class: beads-command-rename-prefix
;;; ============================================================

(eval-and-compile
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
Renames the issue prefix for all issues in the database."))

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

(eval-and-compile
(beads-defcommand beads-command-repair (beads-command-json)
  ()
  :documentation "Represents bd repair command.
Repairs corrupted database by cleaning orphaned references."))

(cl-defmethod beads-command-subcommand ((_command beads-command-repair))
  "Return \"repair\" as the CLI subcommand."
  "repair")

;;; ============================================================
;;; Command Class: beads-command-resolve-conflicts
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-resolve-conflicts (beads-command-json)
  ()
  :documentation "Represents bd resolve-conflicts command.
Resolves git merge conflicts in JSONL files."))

(cl-defmethod beads-command-subcommand ((_command beads-command-resolve-conflicts))
  "Return \"resolve-conflicts\" as the CLI subcommand."
  "resolve-conflicts")

;;; ============================================================
;;; Command Class: beads-command-restore
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-restore (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to restore."
    :positional 1))
  :documentation "Represents bd restore command.
Restores full history of a compacted issue from git."))

(cl-defmethod beads-command-subcommand ((_command beads-command-restore))
  "Return \"restore\" as the CLI subcommand."
  "restore")

(cl-defmethod beads-command-validate ((command beads-command-restore))
  "Validate restore COMMAND."
  (with-slots (issue-id) command
    (if (not issue-id) "Issue ID is required" nil)))

;;; ============================================================
;;; Command Class: beads-command-merge
;;; ============================================================

(eval-and-compile
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
Git merge driver for beads JSONL files."))

(cl-defmethod beads-command-subcommand ((_command beads-command-merge))
  "Return \"merge\" as the CLI subcommand."
  "merge")

;;; ============================================================
;;; Command Class: beads-command-setup
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-setup (beads-command-json)
  ((editor
    :initarg :editor
    :type (or null string)
    :initform nil
    :documentation "Editor to set up (cursor, vscode, claude, etc.)."
    :positional 1))
  :documentation "Represents bd setup command.
Setup integration with AI editors."))

(cl-defmethod beads-command-subcommand ((_command beads-command-setup))
  "Return \"setup\" as the CLI subcommand."
  "setup")

;;; ============================================================
;;; Command Class: beads-command-ship
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-ship (beads-command-json)
  ((capability
    :initarg :capability
    :type (or null string)
    :initform nil
    :documentation "Capability to publish."
    :positional 1))
  :documentation "Represents bd ship command.
Publishes a capability for cross-project dependencies."))

(cl-defmethod beads-command-subcommand ((_command beads-command-ship))
  "Return \"ship\" as the CLI subcommand."
  "ship")

;;; ============================================================
;;; Command Class: beads-command-cook
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-cook (beads-command-json)
  ((formula-id
    :initarg :formula-id
    :type (or null string)
    :initform nil
    :documentation "Formula ID to compile."
    :positional 1))
  :documentation "Represents bd cook command.
Compiles a formula into a proto (ephemeral by default)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-cook))
  "Return \"cook\" as the CLI subcommand."
  "cook")

;;; ============================================================
;;; Command Class: beads-command-mail
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-mail (beads-command-json)
  ()
  :documentation "Represents bd mail command.
Delegates to mail provider."))

(cl-defmethod beads-command-subcommand ((_command beads-command-mail))
  "Return \"mail\" as the CLI subcommand."
  "mail")

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-duplicate))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-duplicates))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-supersede))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-orphans))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-lint))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-move))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-refile))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-q))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-version))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-where))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-human))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-onboard))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-prime))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-preflight))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-upgrade))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-rename-prefix))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-repair))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-resolve-conflicts))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-restore))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-merge))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-setup))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-ship))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-cook))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-mail))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

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

(provide 'beads-command-misc)
;;; beads-command-misc.el ends here
