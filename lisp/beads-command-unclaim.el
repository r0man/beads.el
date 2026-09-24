;;; beads-command-unclaim.el --- Unclaim command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-unclaim' EIEIO class for the
;; `bd unclaim' command, part of the bd 1.3.x worker-lease loop and
;; category 1 of the CLI sync audit (workflow be-j2b).
;;
;; Unclaim releases a claimed issue by clearing the assignee and
;; resetting the status to open — the inverse of `bd update --claim'.
;; With --if-assignee the release is an atomic compare-and-swap.

;;; Code:

(require 'beads-util)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'beads-reader)

;;; Unclaim Command

;;;###autoload (autoload 'beads-unclaim "beads-command-unclaim" nil t)
(beads-defcommand beads-command-unclaim (beads-command-global-options)
  ((issue-ids
    :positional 1
    :type (list-of string)
    :separator " "
    :prompt "Issue ID(s): "
    :reader beads-reader-issue-id
    :group "Unclaim"
    :level 1
    :order 1
    :required t
    :documentation "Issues to release")
   (reason
    :type (or null string)
    :long-option "reason"
    :short-option "r"
    :prompt "Reason: "
    :group "Options"
    :level 1
    :order 1
    :documentation "Reason for unclaiming")
   (force
    :type boolean
    :long-option "force"
    :group "Options"
    :level 2
    :order 1
    :documentation "Release the claim even if held by a different actor
(admin/reaper use)")
   (if-assignee
    :type (or null string)
    :long-option "if-assignee"
    :prompt "Only if assigned to: "
    :group "Options"
    :level 2
    :order 2
    :documentation "Only release if still assigned to this assignee
(atomic compare-and-swap; exits nonzero when the holder differs)"))
  :documentation "Release a claimed issue.
Clears the assignee and resets the status to open so other workers can
re-claim it.  Releasing another actor's claim requires --force; --if-assignee
performs an atomic compare-and-swap instead (cannot combine with --force)."
  :result (list-of beads-issue))

(cl-defmethod beads-command-validate ((command beads-command-unclaim))
  "Validate unclaim COMMAND.
Delegates to slot validation (issue IDs are required), then rejects
the contradictory --force + --if-assignee combination."
  (or (cl-call-next-method)
      (with-slots (force if-assignee) command
        (cond
         ((and force if-assignee (not (string-empty-p if-assignee)))
          "--if-assignee cannot be combined with --force")
         (t nil)))))

(provide 'beads-command-unclaim)
;;; beads-command-unclaim.el ends here
