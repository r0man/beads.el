;;; beads-command-admin.el --- Admin command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd admin' operations.
;; Admin provides administrative commands for database maintenance.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-admin-cleanup
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-admin-cleanup (beads-command-json)
  ((dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview what would be deleted without making changes."
    :long-option "dry-run"
    :option-type :boolean
    :transient-key "n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Actually delete (required to perform deletion)."
    :long-option "force"
    :short-option "f"
    :option-type :boolean
    :transient-key "f"
    :transient-description "--force"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (cascade
    :initarg :cascade
    :type boolean
    :initform nil
    :documentation "Recursively delete all dependent issues."
    :long-option "cascade"
    :option-type :boolean
    :transient-key "C"
    :transient-description "--cascade"
    :transient-class transient-switch
    :transient-argument "--cascade"
    :transient-group "Options"
    :transient-level 1
    :transient-order 3)
   (ephemeral
    :initarg :ephemeral
    :type boolean
    :initform nil
    :documentation "Only delete closed wisps (transient molecules)."
    :long-option "ephemeral"
    :option-type :boolean
    :transient-key "e"
    :transient-description "--ephemeral"
    :transient-class transient-switch
    :transient-argument "--ephemeral"
    :transient-group "Filters"
    :transient-level 1
    :transient-order 1)
   (hard
    :initarg :hard
    :type boolean
    :initform nil
    :documentation "Bypass tombstone TTL safety; use --older-than days as cutoff."
    :long-option "hard"
    :option-type :boolean
    :transient-key "H"
    :transient-description "--hard"
    :transient-class transient-switch
    :transient-argument "--hard"
    :transient-group "Options"
    :transient-level 2
    :transient-order 4)
   (older-than
    :initarg :older-than
    :type (or null integer)
    :initform nil
    :documentation "Only delete issues closed more than N days ago (0 = all)."
    :long-option "older-than"
    :option-type :integer
    :transient-key "o"
    :transient-description "--older-than"
    :transient-class transient-option
    :transient-argument "--older-than="
    :transient-prompt "Days: "
    :transient-group "Filters"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd admin cleanup command.
Deletes closed issues and prunes expired tombstones."))

(cl-defmethod beads-command-subcommand ((_command beads-command-admin-cleanup))
  "Return \"admin cleanup\" as the CLI subcommand."
  "admin cleanup")

;;; ============================================================
;;; Command Class: beads-command-admin-compact
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-admin-compact (beads-command-json)
  ((dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview without compacting."
    :long-option "dry-run"
    :option-type :boolean
    :transient-key "n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (prune
    :initarg :prune
    :type boolean
    :initform nil
    :documentation "Prune mode: remove expired tombstones by age."
    :long-option "prune"
    :option-type :boolean
    :transient-key "p"
    :transient-description "--prune"
    :transient-class transient-switch
    :transient-argument "--prune"
    :transient-group "Mode"
    :transient-level 1
    :transient-order 1)
   (purge-tombstones
    :initarg :purge-tombstones
    :type boolean
    :initform nil
    :documentation "Purge mode: remove tombstones with no open deps."
    :long-option "purge-tombstones"
    :option-type :boolean
    :transient-key "P"
    :transient-description "--purge-tombstones"
    :transient-class transient-switch
    :transient-argument "--purge-tombstones"
    :transient-group "Mode"
    :transient-level 1
    :transient-order 2)
   (analyze
    :initarg :analyze
    :type boolean
    :initform nil
    :documentation "Analyze mode: export candidates for agent review."
    :long-option "analyze"
    :option-type :boolean
    :transient-key "a"
    :transient-description "--analyze"
    :transient-class transient-switch
    :transient-argument "--analyze"
    :transient-group "Mode"
    :transient-level 1
    :transient-order 3)
   (apply
    :initarg :apply
    :type boolean
    :initform nil
    :documentation "Apply mode: accept agent-provided summary."
    :long-option "apply"
    :option-type :boolean
    :transient-key "A"
    :transient-description "--apply"
    :transient-class transient-switch
    :transient-argument "--apply"
    :transient-group "Mode"
    :transient-level 1
    :transient-order 4)
   (auto
    :initarg :auto
    :type boolean
    :initform nil
    :documentation "Auto mode: AI-powered compaction (legacy)."
    :long-option "auto"
    :option-type :boolean
    :transient-key "u"
    :transient-description "--auto"
    :transient-class transient-switch
    :transient-argument "--auto"
    :transient-group "Mode"
    :transient-level 2
    :transient-order 5)
   (stats
    :initarg :stats
    :type boolean
    :initform nil
    :documentation "Show compaction statistics."
    :long-option "stats"
    :option-type :boolean
    :transient-key "s"
    :transient-description "--stats"
    :transient-class transient-switch
    :transient-argument "--stats"
    :transient-group "Mode"
    :transient-level 1
    :transient-order 6)
   (id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Compact specific issue."
    :long-option "id"
    :option-type :string
    :transient-key "i"
    :transient-description "--id"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue ID: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (all
    :initarg :all
    :type boolean
    :initform nil
    :documentation "Process all candidates."
    :long-option "all"
    :option-type :boolean
    :transient-key "l"
    :transient-description "--all"
    :transient-class transient-switch
    :transient-argument "--all"
    :transient-group "Options"
    :transient-level 1
    :transient-order 3)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force compact (bypass checks, requires --id)."
    :long-option "force"
    :option-type :boolean
    :transient-key "f"
    :transient-description "--force"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Options"
    :transient-level 2
    :transient-order 4)
   (older-than
    :initarg :older-than
    :type (or null integer)
    :initform nil
    :documentation "Prune tombstones older than N days (default: 30)."
    :long-option "older-than"
    :option-type :integer
    :transient-key "o"
    :transient-description "--older-than"
    :transient-class transient-option
    :transient-argument "--older-than="
    :transient-prompt "Days: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 5)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit number of candidates (0 = no limit)."
    :long-option "limit"
    :option-type :integer
    :transient-key "L"
    :transient-description "--limit"
    :transient-class transient-option
    :transient-argument "--limit="
    :transient-prompt "Limit: "
    :transient-group "Options"
    :transient-level 2
    :transient-order 6)
   (tier
    :initarg :tier
    :type (or null integer)
    :initform nil
    :documentation "Compaction tier (1 or 2)."
    :long-option "tier"
    :option-type :integer
    :transient-key "t"
    :transient-description "--tier"
    :transient-class transient-option
    :transient-argument "--tier="
    :transient-prompt "Tier (1 or 2): "
    :transient-group "Options"
    :transient-level 2
    :transient-order 7)
   (summary
    :initarg :summary
    :type (or null string)
    :initform nil
    :documentation "Path to summary file (use '-' for stdin)."
    :long-option "summary"
    :option-type :string
    :transient-key "S"
    :transient-description "--summary"
    :transient-class transient-option
    :transient-argument "--summary="
    :transient-prompt "Summary file: "
    :transient-group "Options"
    :transient-level 2
    :transient-order 8)
   (batch-size
    :initarg :batch-size
    :type (or null integer)
    :initform nil
    :documentation "Issues per batch (default 10)."
    :long-option "batch-size"
    :option-type :integer
    :transient-key "b"
    :transient-description "--batch-size"
    :transient-class transient-option
    :transient-argument "--batch-size="
    :transient-prompt "Batch size: "
    :transient-group "Options"
    :transient-level 3
    :transient-order 9)
   (workers
    :initarg :workers
    :type (or null integer)
    :initform nil
    :documentation "Parallel workers (default 5)."
    :long-option "workers"
    :option-type :integer
    :transient-key "w"
    :transient-description "--workers"
    :transient-class transient-option
    :transient-argument "--workers="
    :transient-prompt "Workers: "
    :transient-group "Options"
    :transient-level 3
    :transient-order 10))
  :documentation "Represents bd admin compact command.
Compacts old closed issues to save space."))

(cl-defmethod beads-command-subcommand ((_command beads-command-admin-compact))
  "Return \"admin compact\" as the CLI subcommand."
  "admin compact")

;;; ============================================================
;;; Command Class: beads-command-admin-reset
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-admin-reset (beads-command-json)
  ((force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Actually perform the reset (required)."
    :long-option "force"
    :option-type :boolean
    :transient-key "f"
    :transient-description "--force"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd admin reset command.
Removes all beads data and configuration. DANGEROUS!"))

(cl-defmethod beads-command-subcommand ((_command beads-command-admin-reset))
  "Return \"admin reset\" as the CLI subcommand."
  "admin reset")

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-admin-cleanup))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-admin-compact))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-admin-reset))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-admin-cleanup "beads-command-admin" nil t)
(beads-meta-define-transient beads-command-admin-cleanup "beads-admin-cleanup"
  "Delete closed issues and prune expired tombstones."
  beads-option-global-section)

;;;###autoload (autoload 'beads-admin-compact "beads-command-admin" nil t)
(beads-meta-define-transient beads-command-admin-compact "beads-admin-compact"
  "Compact old closed issues to save space."
  beads-option-global-section)

;;;###autoload (autoload 'beads-admin-reset "beads-command-admin" nil t)
(beads-meta-define-transient beads-command-admin-reset "beads-admin-reset"
  "Remove all beads data and configuration.

WARNING: This is destructive and cannot be undone!"
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-admin "beads-command-admin" nil t)
(transient-define-prefix beads-admin ()
  "Administrative commands for database maintenance.

Use with caution - prefer 'bd doctor --fix' for routine operations."
  ["Admin Commands"
   ("c" "Cleanup (delete closed)" beads-admin-cleanup)
   ("C" "Compact (save space)" beads-admin-compact)]
  ["Danger"
   ("R" "Reset (delete all)" beads-admin-reset)])

(provide 'beads-command-admin)
;;; beads-command-admin.el ends here
