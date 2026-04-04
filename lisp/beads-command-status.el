;;; beads-command-status.el --- Status command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-status' EIEIO class for the
;; `bd status' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd status command shows a quick snapshot of the issue database
;; state and statistics, similar to how 'git status' shows working tree
;; state.  It provides:
;; - Summary of issue counts by state (open, in_progress, blocked, closed)
;; - Ready work count
;; - Extended statistics (tombstones, pinned issues, average lead time)
;; - Recent activity over the last 24 hours from git history

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; Status Command

(beads-defcommand beads-command-status (beads-command-global-options)
  ((all-issues
    :long-option "all"
    :type boolean
    :short-option "a"
    :group "Options"
    :level 2
    :order 1)
   (assigned
    :type boolean
    :short-option "m"
    :group "Options"
    :level 2
    :order 2)
   (no-activity
    :type boolean
    :short-option "n"
    :group "Options"
    :level 2
    :order 3))
  :documentation "Represents bd status command.
Shows a quick snapshot of the issue database state and statistics.
When executed with :json t, returns status data as JSON.")


(cl-defmethod beads-command-validate ((_command beads-command-status))
  "Validate status COMMAND.
No required fields, returns nil (valid)."
  nil)


;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-status "beads-command-status" nil t)
(beads-meta-define-transient beads-command-status "beads-status"
  "Show issue database overview and statistics.

Similar to 'git status' for working tree state:
- Summary of issue counts by state
- Ready work and blockers
- Recent activity from git history

Use cases:
- Quick project health check
- Onboarding for new contributors
- Daily standup reference

Transient levels control which options are visible (cycle with C-x l):
  Level 2: All options"
  beads-option-global-section)

(provide 'beads-command-status)
;;; beads-command-status.el ends here
