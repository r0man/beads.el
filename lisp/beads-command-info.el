;;; beads-command-info.el --- Info command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-info' EIEIO class for the
;; `bd info' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd info command displays information about the current database
;; path and daemon status.  It helps debug issues where bd is using
;; an unexpected database or daemon connection.  It shows:
;; - The absolute path to the database file
;; - Daemon connection status (daemon or direct mode)
;; - If using daemon: socket path, health status, version
;; - Database statistics (issue count)
;; - Schema information (with --schema flag)
;; - What's new in recent versions (with --whats-new flag)

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; Info Command

(beads-defcommand beads-command-info (beads-command-global-options)
  ((schema
    :type boolean
    :short-option "s"
    :group "Options"
    :level 2
    :order 1)
   (whats-new
    :type boolean
    :short-option "w"
    :group "Options"
    :level 2
    :order 2)
   (thanks
    :type boolean
    :short-option "t"
    :group "Options"
    :level 2
    :order 3))
  :documentation "Represents bd info command.
Shows database and daemon information for debugging.
When executed with :json t, returns info data as JSON.")


;; Validate override removed: base handles slot-level validation.


;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-info "beads-command-info" nil t)
(beads-meta-define-transient beads-command-info "beads-info"
  "Show database and daemon information.

Displays:
- Database file path
- Daemon connection status
- Database statistics
- Schema information (with --schema)
- Recent version changes (with --whats-new)

Useful for debugging database or daemon issues.

Transient levels control which options are visible (cycle with C-x l):
  Level 2: All options"
  beads-option-global-section)

(provide 'beads-command-info)
;;; beads-command-info.el ends here
