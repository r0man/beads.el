;;; beads-command-stale.el --- Stale command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-stale' EIEIO class for the
;; `bd stale' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-defcommand'.
;;
;; The bd stale command shows issues that haven't been updated recently
;; and may need attention.  This helps identify:
;; - In-progress issues with no recent activity (may be abandoned)
;; - Open issues that have been forgotten
;; - Issues that might be outdated or no longer relevant

;;; Code:

(require 'beads-util)
(require 'beads-reader)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; Stale Command

;;;###autoload (autoload 'beads-stale "beads-command-stale" nil t)
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
    :order 3)
   (label
    :type (list-of string)
    :long-option "label"
    :short-option "l"
    :prompt "Labels (AND): "
    :reader beads-reader-issue-labels
    :group "Label Filters"
    :level 3
    :order 1
    :documentation "Filter by labels (AND: must have ALL).  Can combine with --label-any")
   (label-any
    :type (list-of string)
    :long-option "label-any"
    :prompt "Labels (OR): "
    :reader beads-reader-issue-labels
    :group "Label Filters"
    :level 3
    :order 2
    :documentation "Filter by labels (OR: must have AT LEAST ONE).  Can combine with --label")
   (exclude-label
    :type (list-of string)
    :long-option "exclude-label"
    :prompt "Exclude labels (any of): "
    :reader beads-reader-issue-labels
    :group "Label Filters"
    :level 3
    :order 3
    :documentation "Exclude issues that have ANY of these labels"))
  :documentation "Represents bd stale command.
Shows issues that haven't been updated recently.
When executed with :json t, returns issue list as JSON."
  :result (list-of beads-issue))

(provide 'beads-command-stale)
;;; beads-command-stale.el ends here
