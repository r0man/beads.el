;;; beads-command-history.el --- History command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-history' EIEIO class for the
;; `bd history' command.  Shows version history for an issue in the
;; Dolt-backed beads database.
;;
;; Usage:
;;   (beads-command-history! :issue-id "bd-42")
;;   (beads-history)  ; invoke transient menu

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; History Command

(beads-defcommand beads-command-history (beads-command-global-options)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to show history for (positional argument)."
    :positional 1
    :option-type :string
    :key "i"
    :transient "Issue ID (required)"
    :class transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :transient-reader beads-reader-close-issue-id
    :transient-group "History"
    :level 1
    :order 1
    :required t)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit number of history entries (--limit).
0 means show all entries."
    :long-option "limit"
    :option-type :integer
    :key "l"
    :transient "--limit"
    :class transient-option
    :argument "--limit="
    :prompt "Limit (0=all): "
    :transient-group "Options"
    :level 2
    :order 2))
  :documentation "Show version history for an issue.
Requires Dolt backend.")


(cl-defmethod beads-command-validate ((command beads-command-history))
  "Validate history COMMAND.
Issue ID is required."
  (with-slots (issue-id) command
    (when (or (null issue-id) (string-empty-p issue-id))
      "Must provide an issue ID")))


;;; Transient Menu

;;;###autoload (autoload 'beads-history "beads-command-history" nil t)
(beads-meta-define-transient beads-command-history "beads-history"
  "Show version history for an issue.

Displays all commits where the issue was modified.
Requires Dolt backend."
  beads-option-global-section)

(provide 'beads-command-history)
;;; beads-command-history.el ends here
