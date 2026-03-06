;;; beads-command-restore.el --- Restore command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-restore' EIEIO class for the
;; `bd restore' command.  Restores a compacted issue from Dolt version
;; history.
;;
;; Usage:
;;   (beads-command-restore! :issue-id "bd-42")
;;   (beads-restore)  ; invoke transient menu

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; Restore Command

(beads-defcommand beads-command-restore (beads-command)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to restore (positional argument)."
    :positional 1
    :option-type :string
    :key "i"
    :transient "Issue ID (required)"
    :class transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :transient-reader beads-reader-close-issue-id
    :transient-group "Restore"
    :level 1
    :order 1
    :required t))
  :documentation "Restore a compacted issue from Dolt history.
Requires Dolt backend.")


(cl-defmethod beads-command-validate ((command beads-command-restore))
  "Validate restore COMMAND.
Issue ID is required."
  (with-slots (issue-id) command
    (when (or (null issue-id) (string-empty-p issue-id))
      "Must provide an issue ID")))


;;; Transient Menu

;;;###autoload (autoload 'beads-restore "beads-command-restore" nil t)
(beads-meta-define-transient beads-command-restore "beads-restore"
  "Restore a compacted issue from Dolt history.

Retrieves the full content of a compacted issue from version history.
Requires Dolt backend."
  beads-option-global-section)

(provide 'beads-command-restore)
;;; beads-command-restore.el ends here
