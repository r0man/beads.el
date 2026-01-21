;;; beads-command-stale.el --- Stale command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-stale' EIEIO class for the
;; `bd stale' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd stale command shows issues that haven't been updated recently
;; and may need attention.  This helps identify:
;; - In-progress issues with no recent activity (may be abandoned)
;; - Open issues that have been forgotten
;; - Issues that might be outdated or no longer relevant

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; Stale Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
  (beads-defcommand beads-command-stale (beads-command-json)
    ((days
      :initarg :days
      :type (or null integer)
      :initform nil
      :documentation "Issues not updated in this many days (--days).
  Default is 30 days."
      :long-option "days"
      :short-option "d"
      :option-type :integer
      :key "d"
      :transient "Days threshold"
      :class transient-option
      :argument "--days="
      :prompt "Days (default 30): "
      :transient-group "Filters"
      :level 2
      :order 1)
     (limit
      :initarg :limit
      :type (or null integer)
      :initform nil
      :documentation "Maximum issues to show (--limit).
  Default is 50."
      :long-option "limit"
      :short-option "n"
      :option-type :integer
      :key "n"
      :transient "Result limit"
      :class transient-option
      :argument "--limit="
      :prompt "Limit (default 50): "
      :transient-group "Filters"
      :level 2
      :order 2)
     (status
      :initarg :status
      :type (or null string)
      :initform nil
      :documentation "Filter by status (--status).
  Valid values: open, in_progress, blocked, deferred."
      :long-option "status"
      :short-option "s"
      :option-type :string
      :key "s"
      :transient "Filter by status"
      :class transient-option
      :argument "--status="
      :prompt "Status: "
      :choices ("open" "in_progress" "blocked" "deferred")
      :transient-group "Filters"
      :level 2
      :order 3))
    :documentation "Represents bd stale command.
  Shows issues that haven't been updated recently.
  When executed with :json t, returns issue list as JSON."))

(cl-defmethod beads-command-subcommand ((_command beads-command-stale))
  "Return subcommand name for stale command."
  "stale")

(cl-defmethod beads-command-validate ((_command beads-command-stale))
  "Validate stale COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-stale))
  "Execute CMD in compilation buffer with human-readable output.
Disables JSON mode for interactive display with colors."
  ;; Set json to nil for human-readable colored output
  (oset cmd json nil)
  ;; Call the default implementation
  (cl-call-next-method))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-stale "beads-command-stale" nil t)
(beads-meta-define-transient beads-command-stale "beads-stale"
  "Show stale issues (not updated recently).

Helps identify issues that may need attention:
- In-progress issues with no recent activity
- Open issues that have been forgotten
- Issues that might be outdated

Transient levels control which options are visible (cycle with C-x l):
  Level 2: All filters (days, limit, status)"
  beads-option-global-section)

(provide 'beads-command-stale)
;;; beads-command-stale.el ends here
