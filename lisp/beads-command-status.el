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

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-status (beads-command-json)
  ((all-issues
    :initarg :all-issues
    :type boolean
    :initform nil
    :documentation "Show all issues (default behavior) (--all)."
    :long-option "all"
    :option-type :boolean
    :transient-key "a"
    :transient-description "Show all issues"
    :transient-class transient-switch
    :transient-argument "--all"
    :transient-group "Options"
    :transient-level 2
    :transient-order 1)
   (assigned
    :initarg :assigned
    :type boolean
    :initform nil
    :documentation "Show issues assigned to current user (--assigned)."
    :long-option "assigned"
    :option-type :boolean
    :transient-key "m"
    :transient-description "Show assigned to me"
    :transient-class transient-switch
    :transient-argument "--assigned"
    :transient-group "Options"
    :transient-level 2
    :transient-order 2)
   (no-activity
    :initarg :no-activity
    :type boolean
    :initform nil
    :documentation "Skip git activity analysis for faster output (--no-activity)."
    :long-option "no-activity"
    :option-type :boolean
    :transient-key "n"
    :transient-description "Skip activity (faster)"
    :transient-class transient-switch
    :transient-argument "--no-activity"
    :transient-group "Options"
    :transient-level 2
    :transient-order 3))
  :documentation "Represents bd status command.
Shows a quick snapshot of the issue database state and statistics.
When executed with :json t, returns status data as JSON."))

(cl-defmethod beads-command-subcommand ((_command beads-command-status))
  "Return subcommand name for status command."
  "status")

(cl-defmethod beads-command-validate ((_command beads-command-status))
  "Validate status COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-status))
  "Execute CMD in compilation buffer with human-readable output.
Disables JSON mode for interactive display with colors."
  ;; Set json to nil for human-readable colored output
  (oset cmd json nil)
  ;; Call the default implementation
  (cl-call-next-method))

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
