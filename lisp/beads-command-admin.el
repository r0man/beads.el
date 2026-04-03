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
(require 'beads-reader)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-admin-cleanup
;;; ============================================================

(beads-defcommand beads-command-admin-cleanup (beads-command-global-options)
  ((dry-run
    :option-type :boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1)
   (force
    :short-option "f"
    :option-type :boolean
    :group "Options"
    :level 1
    :order 2)
   (cascade
    :option-type :boolean
    :short-option "C"
    :group "Options"
    :level 1
    :order 3)
   (ephemeral
    :option-type :boolean
    :short-option "e"
    :group "Filters"
    :level 1
    :order 1)
   (hard
    :option-type :boolean
    :short-option "H"
    :group "Options"
    :level 2
    :order 4)
   (older-than
    :option-type :integer
    :short-option "o"
    :prompt "Days: "
    :group "Filters"
    :level 1
    :order 2))
  :documentation "Represents bd admin cleanup command.
Deletes closed issues and prunes expired tombstones.")


;;; ============================================================
;;; Command Class: beads-command-admin-compact
;;; ============================================================

(beads-defcommand beads-command-admin-compact (beads-command-global-options)
  ((dry-run
    :option-type :boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1)
   (analyze
    :option-type :boolean
    :short-option "a"
    :group "Mode"
    :level 1
    :order 3)
   (apply
    :option-type :boolean
    :short-option "A"
    :group "Mode"
    :level 1
    :order 4)
   (auto
    :option-type :boolean
    :short-option "u"
    :group "Mode"
    :level 2
    :order 5)
   (stats
    :option-type :boolean
    :short-option "s"
    :group "Mode"
    :level 1
    :order 6)
   (id
    :option-type :string
    :short-option "i"
    :prompt "Issue ID: "
    :reader beads-reader-issue-id
    :group "Options"
    :level 1
    :order 2)
   (all
    :option-type :boolean
    :short-option "l"
    :group "Options"
    :level 1
    :order 3)
   (force
    :option-type :boolean
    :short-option "f"
    :group "Options"
    :level 2
    :order 4)
   (older-than
    :option-type :integer
    :short-option "o"
    :prompt "Days: "
    :group "Options"
    :level 1
    :order 5)
   (limit
    :option-type :integer
    :short-option "L"
    :group "Options"
    :level 2
    :order 6)
   (tier
    :option-type :integer
    :short-option "t"
    :prompt "Tier (1 or 2): "
    :group "Options"
    :level 2
    :order 7)
   (summary
    :option-type :string
    :short-option "S"
    :prompt "Summary file: "
    :group "Options"
    :level 2
    :order 8)
   (batch-size
    :option-type :integer
    :short-option "b"
    :prompt "Batch size: "
    :group "Options"
    :level 3
    :order 9)
   (workers
    :option-type :integer
    :short-option "w"
    :group "Options"
    :level 3
    :order 10))
  :documentation "Represents bd admin compact command.
Compacts old closed issues to save space.")


;;; ============================================================
;;; Command Class: beads-command-admin-reset
;;; ============================================================

(beads-defcommand beads-command-admin-reset (beads-command-global-options)
  ((force
    :option-type :boolean
    :short-option "f"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd admin reset command.
Removes all beads data and configuration. DANGEROUS!")


;;; Execute Interactive Methods




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
