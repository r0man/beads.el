;;; beads-command-migrate-personal.el --- Migrate-personal command class -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-migrate-personal' EIEIO
;; class for the `bd migrate-personal' command, category 1 of the CLI
;; sync audit (workflow be-j2b).
;;
;; The command is a one-time migration: it finds issues you created in
;; the project database and moves them to your personal planning
;; repository (~/.beads-planning by default), after asking for
;; confirmation (skip with --yes).

;;; Code:

(require 'beads-util)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)

;;; Migrate-Personal Command

;;;###autoload (autoload 'beads-migrate-personal "beads-command-migrate-personal" nil t)
(beads-defcommand beads-command-migrate-personal
    (beads-command-global-options)
  ((yes
    :type boolean
    :long-option "yes"
    :short-option "y"
    :group "Options"
    :level 1
    :order 1
    :documentation "Skip confirmation prompt"))
  :documentation "Move personal planning issues to your planning repo.
Finds issues you created in the project database and moves them to the
personal planning repository configured in routing.contributor — a
one-time migration for contributors who created personal planning
issues before contributor routing was configured."
  :cli-command "migrate-personal"
  :json nil)

(provide 'beads-command-migrate-personal)
;;; beads-command-migrate-personal.el ends here
