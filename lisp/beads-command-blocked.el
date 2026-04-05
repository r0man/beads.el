;;; beads-command-blocked.el --- Blocked command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-blocked' EIEIO class for the
;; `bd blocked' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-defcommand'.
;;
;; The bd blocked command shows blocked issues (issues with unresolved
;; blockers).  This is useful for understanding what work is stalled
;; waiting on other issues.
;;
;; Features:
;; - Filter by parent epic/bead
;;
;; Usage:
;;   (beads-command-execute (beads-command-blocked))
;;   (beads-execute 'beads-command-blocked)  ; convenience function

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-reader)
(require 'beads-types)

;;; Blocked Command

(beads-defcommand beads-command-blocked (beads-command-global-options)
  ((parent
    :type (or null string)
    :short-option "P"
    :prompt "Parent ID: "
    :reader beads-reader-issue-id
    :group "Scope"
    :level 2
    :order 1))
  :documentation "Represents bd blocked command.
Shows blocked issues (issues with unresolved blockers).
When executed with :json t, returns list of beads-blocked-issue instances."
  :result (list-of beads-blocked-issue)
  :transient :manual)


;; Parse override removed: the base method handles JSON-to-domain
;; parsing automatically via :result (list-of beads-blocked-issue).


;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-blocked-transient "beads-command-blocked" nil t)
(beads-meta-define-transient beads-command-blocked "beads-blocked-transient"
  "Show blocked issues (issues with unresolved blockers).

Blocked issues are those that have dependencies that are not yet closed.
This helps understand what work is stalled waiting on other issues.

Transient levels control which options are visible (cycle with C-x l):
  Level 2: Scope filter (parent)"
  beads-option-global-section)

(provide 'beads-command-blocked)
;;; beads-command-blocked.el ends here
