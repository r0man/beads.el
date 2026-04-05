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

(beads-defcommand beads-command-stale (beads-command-global-options)
  ((days
    :type (or null string integer)
    :short-option "d"
    :prompt "Days (default 30): "
    :group "Filters"
    :level 2
    :order 1)
   (limit
    :type (or null string integer)
    :short-option "n"
    :prompt "Limit (default 50): "
    :group "Filters"
    :level 2
    :order 2)
   (status
    :type (or null string)
    :short-option "s"
    :choices ("open" "in_progress" "blocked" "deferred")
    :group "Filters"
    :level 2
    :order 3))
  :documentation "Represents bd stale command.
Shows issues that haven't been updated recently.
When executed with :json t, returns issue list as JSON."
  :result (list-of beads-issue))


;; Validate override removed: base handles slot-level validation.


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
