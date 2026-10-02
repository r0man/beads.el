;;; beads-menu-reachability-test.el --- Retired-menu reachability -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; WI-1 removed the deprecated `beads-more-menu' dumping ground
;; (REQ-023, REQ-026).  Nothing it exposed may become unreachable: every
;; command it held must still be reachable from the primary dispatch
;; (`beads') or from the `beads-ops-menu' / `beads-advanced-menu'
;; temporary homes, until WI-9 collapses them into `beads-menu.el'.
;;
;; This test walks those three prefixes (recursing into sub-dispatch
;; prefixes, forcing autoloads so transient layouts are available) and
;; asserts the retired command set is covered.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-ops-menu)
(require 'beads-advanced-menu)

(defconst beads-menu-reachability-test--retired-commands
  '(beads-reopen beads-delete beads-edit beads-create beads-q
    beads-children beads-promote beads-query beads-todo beads-rename
    beads-cook beads-gate beads-defer beads-undefer beads-ship
    beads-list-advanced beads-stats beads-count beads-stale beads-search
    beads-lint beads-orphans beads-types beads-find-duplicates
    beads-graph-all beads-epic-menu beads-comments-menu beads-audit
    beads-swarm beads-doctor beads-migrate-menu beads-worktree-menu
    beads-admin beads-preflight beads-upgrade beads-rename-prefix
    beads-compact beads-compact-commits beads-flatten beads-gc
    beads-purge beads-prune beads-batch beads-ping beads-vc beads-branch
    beads-diff beads-history beads-federation beads-duplicate
    beads-duplicates beads-supersede beads-restore beads-sql beads-backup
    beads-export beads-jira beads-linear beads-github beads-gitlab
    beads-repo beads-ado beads-mail beads-init beads-init-safety
    beads-bootstrap beads-context beads-quickstart beads-hooks beads-info
    beads-where beads-human beads-onboard beads-prime beads-setup
    beads-forget beads-kv beads-memories beads-recall beads-remember
    beads-set-state beads-state-menu beads-version)
  "Commands the deleted `beads-more-menu' exposed.
Every one must remain reachable from the primary dispatch or the
ops/advanced temporary homes.")

(defconst beads-menu-reachability-test--roots
  '(beads beads-ops-menu beads-advanced-menu)
  "Transient prefixes the reachability walk starts from.")

(defun beads-menu-reachability-test--walk (spec)
  "Collect `:command' symbols from transient layout SPEC.
Descends into vectors, group forms and suffix plists."
  (cond
   ((vectorp spec)
    (seq-mapcat #'beads-menu-reachability-test--walk (append spec nil)))
   ((and (consp spec) (symbolp (car spec)))
    (let* ((rest (cdr spec))
           (props (if (keywordp (car rest)) rest (car rest)))
           (children (if (keywordp (car rest)) nil (cdr rest))))
      (append (when (and (listp props)
                         (plist-get props :command))
                (list (plist-get props :command)))
              (seq-mapcat #'beads-menu-reachability-test--walk children))))
   ((and (consp spec) (keywordp (car spec)))
    (let ((command (plist-get spec :command)))
      (when command (list command))))
   ((consp spec)
    (seq-mapcat #'beads-menu-reachability-test--walk spec))
   (t nil)))

(defun beads-menu-reachability-test--force-load (command)
  "Load COMMAND's file when it is still an autoload.
Does nothing for non-autoloaded or non-function symbols."
  (when (and (symbolp command)
             (fboundp command)
             (autoloadp (symbol-function command)))
    (ignore-errors (autoload-do-load (symbol-function command) command))))

(defun beads-menu-reachability-test--reachable-commands ()
  "Return every command reachable from the reachability roots.
Walks sub-dispatch prefixes recursively, forcing autoloads so their
transient layouts are defined."
  (let ((seen-menus nil)
        (commands nil))
    (cl-labels ((walk (menu)
                  (unless (memq menu seen-menus)
                    (push menu seen-menus)
                    (dolist (command
                             (beads-menu-reachability-test--walk
                              (get menu 'transient--layout)))
                      (push command commands)
                      (beads-menu-reachability-test--force-load command)
                      (when (get command 'transient--layout)
                        (walk command))))))
      (dolist (root beads-menu-reachability-test--roots)
        (beads-menu-reachability-test--force-load root)
        (walk root)))
    (delete-dups commands)))

(ert-deftest beads-menu-reachability-test-more-menu-undefined ()
  "`beads-more-menu' is deleted: no function, prefix, or layout remains."
  (should-not (fboundp 'beads-more-menu))
  (should-not (get 'beads-more-menu 'transient--prefix))
  (should-not (get 'beads-more-menu 'transient--layout)))

(ert-deftest beads-menu-reachability-test-retired-commands-reachable ()
  "Every command the deleted `beads-more-menu' held is still reachable."
  (let ((reachable (beads-menu-reachability-test--reachable-commands))
        (missing nil))
    (dolist (command beads-menu-reachability-test--retired-commands)
      (unless (memq command reachable)
        (push command missing)))
    (should-not missing)))

(provide 'beads-menu-reachability-test)
;;; beads-menu-reachability-test.el ends here
