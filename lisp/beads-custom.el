;;; beads-custom.el --- Customization variables for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el

;;; Commentary:

;; This file contains all user-customizable variables for beads.el.
;; Users can configure these variables via M-x customize-group RET beads RET
;; or by setting them in their Emacs configuration.

;;; Code:

;;; Customization Group

(defgroup beads nil
  "Magit-like interface for Beads issue tracker."
  :group 'tools
  :prefix "beads-")

;;; Executable and Database

(defcustom beads-executable "bd"
  "Path to the bd executable.
This can be either a simple command name (e.g., \"bd\") if it's
in your PATH, or a full path to the executable."
  :type 'string
  :group 'beads)

(defcustom beads-database-path nil
  "Path to the beads database.
If nil, bd will auto-discover the database by searching for a
.beads directory in the project hierarchy."
  :type '(choice (const :tag "Auto-discover" nil)
                 (file :tag "Database path"))
  :group 'beads)

;;; Actor Configuration

(defcustom beads-actor nil
  "Actor name for audit trail.
If nil, uses the $USER environment variable.  This value is used
for tracking who performs actions in the issue tracker."
  :type '(choice (const :tag "Use $USER" nil)
                 (string :tag "Actor name"))
  :group 'beads)

;;; Debug Settings

(defcustom beads-enable-debug nil
  "Enable debug logging to *beads-debug* buffer.
When enabled, all bd commands and their output will be logged
for troubleshooting purposes."
  :type 'boolean
  :group 'beads)

(defcustom beads-debug-level 'info
  "Debug logging level.
- `error': Only log errors
- `info': Log commands and important events (default)
- `verbose': Log everything including command output"
  :type '(choice (const :tag "Error only" error)
                 (const :tag "Info (commands and events)" info)
                 (const :tag "Verbose (all output)" verbose))
  :group 'beads)

;;; UI Behavior

(defcustom beads-auto-refresh t
  "Automatically refresh buffers after mutations.
When enabled, beads list and show buffers will automatically
refresh after operations like create, update, or close."
  :type 'boolean
  :group 'beads)

(defcustom beads-list-default-limit 0
  "Default limit for issue list operations.
When 0, all issues are returned (no limit).
When set to a positive integer, limits the number of issues returned
in list operations such as `beads-command-list'.

This can be overridden per-command by explicitly setting the :limit
argument when calling list commands."
  :type '(integer :tag "Default limit (0 = no limit)")
  :group 'beads)

;;; Completion Behavior

(defcustom beads-completion-show-unavailable-backends t
  "Whether to show unavailable backends in completion lists.
When non-nil, unavailable backends are shown but cannot be selected.
They appear grayed out and are grouped separately from available backends.
When nil, only available backends appear in the completion list.

This affects `beads-agent-start' and related functions that prompt
for backend selection."
  :type 'boolean
  :group 'beads-agent)

;;; Provide

(provide 'beads-custom)
;;; beads-custom.el ends here
