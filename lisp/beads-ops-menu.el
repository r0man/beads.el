;;; beads-ops-menu.el --- Mid-frequency operations menu -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Mid-frequency operations transient menu for beads.el, accessible
;; via ! from the main dispatch.  Contains issue lifecycle commands,
;; views/reports, issue details, and workflow coordination.
;;
;; This replaces the mid-frequency portion of the old beads-more-menu
;; (Phase 5 of the beads.el UX redesign).

;;; Code:

(require 'transient)

;; Forward declarations — all suffixes are autoloaded from their modules
(declare-function beads-defer "beads-command-defer" nil)
(declare-function beads-undefer "beads-command-defer" nil)
(declare-function beads-delete "beads-command-delete" (&optional issue-id))

(declare-function beads-promote "beads-command-misc" nil)
(declare-function beads-rename "beads-command-misc" nil)
(declare-function beads-count "beads-command-count" nil)
(declare-function beads-stats "beads-command-status" nil)
(declare-function beads-stale "beads-command-stale" nil)
(declare-function beads-types "beads-command-misc" nil)
(declare-function beads-lint "beads-command-misc" nil)
(declare-function beads-find-duplicates "beads-command-misc" nil)
(declare-function beads-orphans "beads-command-misc" nil)
(declare-function beads-children "beads-command-misc" nil)
(declare-function beads-comments-menu "beads-command-comments" nil)
(declare-function beads-todo "beads-command-misc" nil)
(declare-function beads-query "beads-command-misc" nil)
(declare-function beads-gate "beads-command-gate" nil)
(declare-function beads-swarm "beads-command-swarm" nil)
(declare-function beads-cook "beads-command-misc" nil)
(declare-function beads-ship "beads-command-misc" nil)
(declare-function beads-set-state "beads-command-state" nil)
(declare-function beads-state-menu "beads-command-state" nil)

;;;###autoload (autoload 'beads-ops-menu "beads-ops-menu" nil t)
(transient-define-prefix beads-ops-menu ()
  "Mid-frequency operations for beads issue tracking.

Accessible via ! from the main beads dispatch.  Contains issue
lifecycle commands, views and reports, issue details, and
workflow coordination."
  ["Issue Lifecycle"
   ("f" "Defer" beads-defer)
   ("F" "Undefer" beads-undefer)
   ("D" "Delete" beads-delete)
("p" "Promote wisp" beads-promote)
   ("R" "Rename" beads-rename)]
  ["Views & Reports"
   ("c" "Count" beads-count)
   ("t" "Stats" beads-stats)
   ("s" "Stale" beads-stale)
   ("T" "Types" beads-types)
   ("l" "Lint" beads-lint)
   ("O" "Find duplicates" beads-find-duplicates)
   ("o" "Orphans" beads-orphans)]
  ["Issue Details"
   ("a" "Children" beads-children)
   ("=" "Comments" beads-comments-menu)
   ("[" "Todo" beads-todo)
   ("q" "Query" beads-query)]
  ["Workflow"
   ("g" "Gate" beads-gate)
   ("w" "Swarm" beads-swarm)
   ("K" "Cook" beads-cook)
   ("H" "Ship" beads-ship)
   ("z" "Set state" beads-set-state)
   ("Z" "State menu" beads-state-menu)]
  ["Actions"
   ("Q" "Quit" transient-quit-one)])

(provide 'beads-ops-menu)
;;; beads-ops-menu.el ends here
