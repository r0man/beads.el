;;; beads-menu.el --- Hand-built dispatch and maintenance menus -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, menus

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Home of the hand-built porcelain menus (REQ-003, REQ-004):
;;
;; - `beads-dispatch' is the top-level dispatch menu opened with `?'
;;   from every porcelain view.  Groups are ordered by frequency and
;;   downstream packages append their own groups through
;;   `beads-menu-providers'.
;; - `beads-maintenance' is the single maintenance/infrastructure menu
;;   reached with `!'; it absorbs the former `beads-ops-menu' and
;;   `beads-advanced-menu' (REQ-023).
;;
;; The provider contract is deliberately narrow: a provider is a
;; function of no arguments that returns a list of transient group
;; vectors acceptable to `transient-define-prefix'.  Providers run on
;; every menu render, so they must be side-effect-free.  With no
;; providers installed the dispatch menu is exactly beads.el's own.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'beads)
(require 'beads-prefix)

;;; Menu Providers

(defvar beads-menu-providers nil
  "Hook of functions contributing groups to the dispatch menu.
Each function is called with no arguments and returns a list of
transient group vectors acceptable to `transient-define-prefix'.
Providers run on every menu render, so they must be side-effect-free;
they must not depend on the current buffer.  An empty hook (the
standalone default) contributes nothing.

Use `beads-menu-provider-groups' to collect the contributions.

Downstream use: Gas City appends a `[City]' group.")

(defun beads-menu-provider-groups ()
  "Return the combined transient groups from `beads-menu-providers'.
Calls each provider in order and appends its list.  An empty hook
returns nil, leaving the dispatch menu unchanged (standalone no-op)."
  (apply #'append
         (delq nil (mapcar #'funcall beads-menu-providers))))

(defun beads-menu--provider-children (children)
  "Return CHILDREN followed by every `beads-menu-providers' group.
Used as the `:setup-children' of the dispatch menu's provider group so
downstream groups are spliced in at menu render time.  With no
providers this is exactly CHILDREN (the standalone no-op)."
  (append children (beads-menu-provider-groups)))

;;; Dispatch Menu

;;;###autoload (autoload 'beads-dispatch "beads-menu" nil t)
(beads-define-prefix beads-dispatch ()
  "Hand-built dispatch menu for beads.el.

Opened with `?' from any porcelain view.  Groups are ordered by
frequency; `beads-menu-providers' appends downstream groups such as
gascity's `[City]' group when that package is present."
  [:description
   (lambda () (beads-main--format-project-header))
   :class transient-row
   ("" "" ignore :if (lambda () nil))]
  [["Issues"
    ("l" "List beads" beads-list-all)
    ("c" "Create bead" beads-compose-create)
    ("/" "Search" beads-search)
    ("i" "Show..." beads-show)
    ("u" "Update bead" beads-update)
    ("x" "Close bead" beads-close)
    ("o" "Reopen bead" beads-reopen)
    ("e" "Edit field" beads-edit)]
   ["Workflow"
    ("r" "Ready work" beads-ready)
    ("b" "Blocked work" beads-blocked)
    ("d" "Dependencies" beads-dep)
    ("#" "Priority" beads-actions-set-priority)
    ("C" "Claim" beads-actions-claim)
    ("s" "Set status" beads-actions-set-status)
    ("m" "Molecules" beads-mol)
    ("G" "Gates" beads-gate)
    ("w" "Wisps" beads-wisp-list)
    ("S" "Swarms" beads-swarm-list-view)
    ("T" "Types" beads-types)]
   ["Views"
    ("a" "Agents..." beads-agent)
    ("F" "Formulas..." beads-formula-menu)
    ("v" "Graph" beads-graph-all)
    ("D" "Dashboard" beads-dashboard)
    ("H" "History" beads-history)
    ("R" "Recent changes" beads-events-timeline)
    ("Y" "Orphans" beads-orphans)
    ("t" "Stats" beads-stats)
    ("X" "Context" beads-context)]
   ["Manage"
    ("L" "Labels..." beads-label-menu)
    ("k" "Dolt..." beads-dolt)
    ("." "Config" beads-config)
    ("W" "Worktrees..." beads-worktree-menu)]]
  [["Maintenance"
    ("!" "Maintenance..." beads-maintenance)
    ("*" "Command log" beads-log)
    ("g" "Refresh cache" beads-refresh-menu)
    ("q" "Quit" transient-quit-one)]
   ;; Downstream groups (e.g. gascity's `[City]') are spliced in here.
   ["" :class transient-column
    :setup-children beads-menu--provider-children]])

;;; Maintenance Menu

;;;###autoload (autoload 'beads-integrations-menu "beads-menu" nil t)
(beads-define-prefix beads-integrations-menu ()
  "Integration commands for beads.el.

Split out of `beads-maintenance' (F5) so the ~60-key maintenance wall
keeps only the daily operations at its default level; reach this
sub-dispatcher with `n' from maintenance."
  ["Integrations"
   ("n" "Jira" beads-jira)
   ("N" "Linear" beads-linear)
   ("y" "GitLab" beads-gitlab)
   ("9" "GitHub" beads-github)
   ("v" "Repo" beads-repo)
   ("A" "ADO" beads-ado)
   (">" "Federation" beads-federation)
   ("*" "Mail delegate" beads-mail)]
  ["Actions"
   ("q" "Quit" transient-quit-one)])

;;;###autoload (autoload 'beads-setup-menu "beads-menu" nil t)
(beads-define-prefix beads-setup-menu ()
  "Setup and migration commands for beads.el.

Split out of `beads-maintenance' (F5) so the rare setup/migration
commands sit one level away; reach this sub-dispatcher with `I' from
maintenance."
  ["Setup"
   ("I" "Init project" beads-init)
   ("x" "Init safety" beads-init-safety)
   (";" "Setup integrations" beads-setup)
   (":" "Info" beads-info)
   ("'" "Version" beads-version)
   ("L" "Bootstrap" beads-bootstrap)
   ("Q" "Hooks..." beads-hooks)
   ("W" "Quickstart" beads-quickstart)
   ("X" "Where" beads-where)
   ("0" "Context" beads-context)]
  ["Memory"
   ("|" "Memories" beads-memories)
   ("5" "KV store" beads-kv)
   ("~" "Prime" beads-prime)
   ("{" "Recall memory" beads-recall)
   ("}" "Remember" beads-remember)]
  ["Admin"
   ("@" "Audit" beads-audit)
   ("6" "Admin" beads-admin)
   ("7" "Worktrees..." beads-worktree-menu)]
  ["Actions"
   ("q" "Quit" transient-quit-one)])

;;;###autoload (autoload 'beads-maintenance "beads-menu" nil t)
(beads-define-prefix beads-maintenance ()
  "Maintenance and infrastructure commands for beads.el.

The single successor of the former `beads-ops-menu' and
`beads-advanced-menu' (REQ-023), reached with `!' from the dispatch
menu.  Every command the two menus held is reachable from here.

Transient levels control which groups are visible; cycle them with
`transient-set-level' (`C-x l' by default).  Level 1 holds the daily
operations, level 3 the database/structure/data groups, level 5 the
integrations sub-dispatcher, and level 7 setup/migration."
  :level 1
  [:level 1 "Issue Lifecycle"
   ("f" "Defer" beads-defer)
   ("F" "Undefer" beads-undefer)
   ("D" "Delete" beads-delete)
   ("p" "Promote wisp" beads-promote)
   ("R" "Rename" beads-rename)
   ("u" "Unclaim" beads-unclaim)]
  [:level 1 "Views & Reports"
   ("c" "Count" beads-count)
   ("t" "Stats" beads-stats)
   ("s" "Stale" beads-stale)
   ("T" "Types" beads-types)
   ("l" "Lint" beads-lint)
   ("O" "Find duplicates" beads-find-duplicates)
   ("o" "Orphans" beads-orphans)
   ("$" "Wisps" beads-wisp-list)
   ("*" "Command log" beads-log)]
  [:level 1 "Issue Details"
   ("a" "Children" beads-children)
   ("=" "Comments" beads-comments-menu)
   ("[" "Todo" beads-todo)
   ("e" "Events" beads-events)
   ("/" "Query" beads-query)]
  [:level 1 "Events (live)"
   ("," "Recent changes" beads-events-timeline)
   ("." "City timeline" beads-events-timeline-city)
   ("#" "Toggle live" beads-live-toggle)]
  [:level 1 "Events (time travel)"
   ("-" "Issue history" beads-events-history)
   ("^" "Rewind..." beads-events-rewind)]
  [:level 1 "Workflow"
   ("g" "Gate" beads-gate)
   ("w" "Swarm" beads-swarm)
   ("K" "Cook" beads-cook)
   ("H" "Ship" beads-ship)
   ("z" "Set state" beads-set-state)
   ("Z" "State menu" beads-state-menu)
   ("C" "Conflicts" beads-conflicts)
   ("r" "Reclaim" beads-reclaim)
   ("B" "Heartbeat" beads-heartbeat)]
  [:level 3 "Database"
   ("b" "Backup" beads-backup)
   ("E" "Export JSONL" beads-export)
   ("P" "Prune closed" beads-prune)
   ("G" "GC" beads-gc)
   ("d" "Batch ops" beads-batch)
   ("h" "Compact" beads-compact)
   ("i" "Flatten" beads-flatten)
   ("j" "Ping" beads-ping)
   ("+" "Doctor" beads-doctor)
   ("M" "Migrate..." beads-migrate-menu)
   ("U" "Upgrade" beads-upgrade)
   ("!" "Preflight" beads-preflight)
   ("8" "Purge" beads-purge)]
  [:level 3 "Structure"
   ("1" "Mark duplicate" beads-duplicate)
   ("2" "Find duplicates" beads-duplicates)
   ("3" "Supersede" beads-supersede)
   ("4" "Restore" beads-restore)
   ("J" "SQL query" beads-sql)
   ("k" "Rename prefix" beads-rename-prefix)
   ("_" "Forget memory" beads-forget)]
  [:level 3 "Data & Sync"
   ("V" "VC" beads-vc)
   ("Y" "Dolt sync" beads-sync)
   ("S" "Schema" beads-schema)
   ("m" "Migrate personal" beads-migrate-personal)
   ("<" "Branch" beads-branch)
   ("`" "Diff" beads-diff)
   ("%" "History" beads-history)
   ("&" "Provenance" beads-provenance)]
  [:level 5 "Integrations"
   ("n" "Integrations..." beads-integrations-menu)]
  [:level 7 "Setup"
   ("I" "Setup & migration..." beads-setup-menu)]
  ["Actions"
   ("q" "Quit" transient-quit-one)])

(provide 'beads-menu)
;;; beads-menu.el ends here
