;;; beads-menu-test.el --- Tests for the dispatch and maintenance menus -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; ERT tests for the hand-built menus in `beads-menu.el':
;; `beads-dispatch' (REQ-004) and `beads-maintenance' (REQ-023), plus
;; the `beads-menu-providers' extension seam.  Reachability is checked
;; by walking the parsed transient layout, so a command dropped from
;; the menu without a replacement fails the suite.

;;; Code:

(require 'ert)
(require 'beads-menu)

;;; Definition

(ert-deftest beads-menu-test-dispatch-defined ()
  "`beads-dispatch' is a transient prefix."
  (should (fboundp 'beads-dispatch))
  (should (get 'beads-dispatch 'transient--prefix)))

(ert-deftest beads-menu-test-maintenance-defined ()
  "`beads-maintenance' is a transient prefix (REQ-023)."
  (should (fboundp 'beads-maintenance))
  (should (get 'beads-maintenance 'transient--prefix)))

;;; Layout walker

(defun beads-menu-test--suffix-pairs (spec)
  "Collect (KEY . COMMAND) pairs reachable from transient layout SPEC.
Walks vectors and lists everywhere; a suffix spec is a keyword-plist
carrying :key/:command, optionally nested inside group forms."
  (cond
   ((vectorp spec)
    (seq-mapcat #'beads-menu-test--suffix-pairs (append spec nil)))
   ((and (consp spec) (symbolp (car spec)))
    (let* ((rest (cdr spec))
           (props (if (keywordp (car rest)) rest (car rest)))
           (children (if (keywordp (car rest)) nil (cdr rest))))
      (append (when (and (listp props)
                         (plist-get props :key)
                         (plist-get props :command))
                (list (cons (plist-get props :key)
                            (plist-get props :command))))
              (seq-mapcat #'beads-menu-test--suffix-pairs children))))
   ((and (consp spec) (keywordp (car spec)))
    (let ((key (plist-get spec :key))
          (command (plist-get spec :command)))
      (when (and key command) (list (cons key command)))))
   ((consp spec)
    (seq-mapcat #'beads-menu-test--suffix-pairs spec))
   (t nil)))

(defun beads-menu-test--layout-suffixes (menu)
  "Return the (KEY . COMMAND) pairs registered on transient MENU."
  (beads-menu-test--suffix-pairs (get menu 'transient--layout)))

(defun beads-menu-test--layout-commands (menu)
  "Return the command symbols registered on transient MENU."
  (mapcar #'cdr (beads-menu-test--layout-suffixes menu)))

;;; Reachability (every absorbed ops/advanced command)

;; The complete suffix inventory of the deleted `beads-ops-menu.el' and
;; `beads-advanced-menu.el' at the point of absorption.  Keep it literal
;; and exhaustive: if a work item drops a command instead of moving it,
;; this list (not the menu itself) must fail.  Do not trim an entry just
;; because the new menu lacks it.
(defconst beads-menu-test--absorbed-commands
  '(;; beads-ops-menu.el
    beads-defer beads-undefer beads-delete beads-promote beads-rename
    beads-unclaim beads-count beads-stats beads-stale beads-types
    beads-lint beads-find-duplicates beads-orphans beads-children
    beads-comments-menu beads-todo beads-events beads-query beads-gate
    beads-swarm beads-cook beads-ship beads-set-state beads-state-menu
    beads-conflicts beads-reclaim beads-heartbeat
    ;; beads-advanced-menu.el
    beads-doctor beads-preflight beads-upgrade beads-compact
    beads-flatten beads-gc beads-purge beads-rename-prefix beads-backup
    beads-export beads-restore beads-branch beads-vc beads-federation
    beads-sql beads-sync beads-schema beads-duplicate beads-duplicates
    beads-supersede beads-migrate-menu beads-migrate-personal beads-jira
    beads-linear beads-gitlab beads-github beads-repo beads-ado
    beads-mail beads-init beads-info beads-hooks beads-quickstart
    beads-where beads-version beads-context beads-setup beads-kv
    beads-prime beads-memories beads-recall beads-remember beads-forget
    beads-audit beads-admin beads-worktree-menu beads-diff beads-history
    beads-provenance)
  "Commands the former ops and advanced menus held.
Every one must remain reachable from `beads-maintenance' (REQ-023).")

(ert-deftest beads-menu-test-maintenance-reachability ()
  "Every absorbed command is registered on `beads-maintenance'."
  (let ((commands (beads-menu-test--layout-commands 'beads-maintenance)))
    (dolist (expected beads-menu-test--absorbed-commands)
      (should (memq expected commands)))))

(ert-deftest beads-menu-test-maintenance-keys-unique ()
  "Every keyed suffix on the maintenance menu has a unique key."
  (let ((keys (mapcar #'car
                      (beads-menu-test--layout-suffixes 'beads-maintenance))))
    (should (equal keys (delete-dups (copy-sequence keys))))))

(ert-deftest beads-menu-test-dispatch-keys-unique ()
  "Every keyed suffix on the dispatch menu has a unique key."
  (let ((keys (mapcar #'car
                      (beads-menu-test--layout-suffixes 'beads-dispatch))))
    (should (equal keys (delete-dups (copy-sequence keys))))))

(ert-deftest beads-menu-test-dispatch-core-commands ()
  "The dispatch menu carries the core issue commands and maintenance."
  (let ((commands (beads-menu-test--layout-commands 'beads-dispatch)))
    (dolist (expected '(beads-list beads-compose-create beads-search
                        beads-show beads-update beads-close beads-ready
                        beads-blocked beads-dashboard beads-stats
                        beads-maintenance))
      (should (memq expected commands)))))

;;; Providers

(ert-deftest beads-menu-test-provider-empty-is-noop ()
  "An empty provider hook contributes nothing."
  (should-not (beads-menu-provider-groups))
  (should (equal (beads-menu--provider-children '(["X" ignore]))
                 '(["X" ignore]))))

(ert-deftest beads-menu-test-provider-composition ()
  "Providers are collected and spliced into the dispatch provider group."
  (let* ((group ["City" ("c" "City status" ignore)])
         (beads-menu-providers (list (lambda () (list group)))))
    (should (equal (beads-menu-provider-groups) (list group)))
    (should (equal (beads-menu--provider-children nil) (list group)))
    ;; The dispatch layout wires the provider group to the seam.
    (should (beads-menu-test--dispatch-provider-group-p))))

(defun beads-menu-test--dispatch-provider-group-p ()
  "Return non-nil when the dispatch layout has a `:setup-children' group.
The group must use `beads-menu--provider-children' so downstream
providers are spliced in at render time."
  (let ((found nil))
    (beads-menu-test--walk (get 'beads-dispatch 'transient--layout)
                           (lambda (spec)
                             (when (and (vectorp spec)
                                        (>= (length spec) 2)
                                        (equal (plist-get (aref spec 1)
                                                          :setup-children)
                                               'beads-menu--provider-children))
                               (setq found t))))
    found))

(defun beads-menu-test--walk (spec fn)
  "Call FN on every vector in SPEC tree."
  (cond
   ((vectorp spec)
    (funcall fn spec)
    (mapc (lambda (s) (beads-menu-test--walk s fn)) (append spec nil)))
   ((listp spec)
    (mapc (lambda (s) (beads-menu-test--walk s fn)) spec))))

(provide 'beads-menu-test)
;;; beads-menu-test.el ends here
