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

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
  (beads-defcommand beads-command-info (beads-command-json)
    ((schema
      :initarg :schema
      :type boolean
      :initform nil
      :documentation "Include schema information in output (--schema)."
      :long-option "schema"
      :option-type :boolean
      :key "s"
      :transient "Show schema info"
      :class transient-switch
      :argument "--schema"
      :transient-group "Options"
      :level 2
      :order 1)
     (whats-new
      :initarg :whats-new
      :type boolean
      :initform nil
      :documentation "Show agent-relevant changes from recent versions (--whats-new)."
      :long-option "whats-new"
      :option-type :boolean
      :key "w"
      :transient "Show what's new"
      :class transient-switch
      :argument "--whats-new"
      :transient-group "Options"
      :level 2
      :order 2)
     (thanks
      :initarg :thanks
      :type boolean
      :initform nil
      :documentation "Show thank you page for contributors (--thanks)."
      :long-option "thanks"
      :option-type :boolean
      :key "t"
      :transient "Show contributors"
      :class transient-switch
      :argument "--thanks"
      :transient-group "Options"
      :level 2
      :order 3))
    :documentation "Represents bd info command.
  Shows database and daemon information for debugging.
  When executed with :json t, returns info data as JSON."))

(cl-defmethod beads-command-subcommand ((_command beads-command-info))
  "Return subcommand name for info command."
  "info")

(cl-defmethod beads-command-validate ((_command beads-command-info))
  "Validate info COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-info))
  "Execute CMD in compilation buffer with human-readable output.
Disables JSON mode for interactive display."
  ;; Set json to nil for human-readable output
  (oset cmd json nil)
  ;; Call the default implementation
  (cl-call-next-method))

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
