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
    :positional 1
    :option-type :string
    :key "i"
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads-reader-close-issue-id
    :group "History"
    :level 1
    :order 1
    :required t)
   (limit
    :option-type :integer
    :key "l"
    :prompt "Limit (0=all): "
    :group "Options"
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
