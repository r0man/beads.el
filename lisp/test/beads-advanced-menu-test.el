;;; beads-advanced-menu-test.el --- Tests for beads-advanced-menu -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-advanced-menu: the low-frequency
;; maintenance/setup/integrations transient (Phase 6).

;;; Code:

(require 'ert)
(require 'beads-advanced-menu)

;;; Transient Definition Tests

(ert-deftest beads-advanced-menu-test-defined ()
  "Verify beads-advanced-menu is defined as a command."
  (should (fboundp 'beads-advanced-menu)))

(ert-deftest beads-advanced-menu-test-is-transient-prefix ()
  "Verify beads-advanced-menu is a transient prefix command."
  (should (get 'beads-advanced-menu 'transient--prefix)))

(ert-deftest beads-advanced-menu-test-command-symbol ()
  "Verify the prefix command symbol matches."
  (let ((prefix (get 'beads-advanced-menu 'transient--prefix)))
    (should (eq (oref prefix command) 'beads-advanced-menu))))

;;; Maintenance Suffixes

(ert-deftest beads-advanced-menu-test-doctor-defined ()
  "Verify beads-doctor is defined."
  (should (fboundp 'beads-doctor)))

(ert-deftest beads-advanced-menu-test-compact-defined ()
  "Verify beads-compact is defined."
  (should (fboundp 'beads-compact)))

(ert-deftest beads-advanced-menu-test-migrate-defined ()
  "Verify beads-migrate-menu is defined."
  (should (fboundp 'beads-migrate-menu)))

;;; Data & Sync Suffixes

(ert-deftest beads-advanced-menu-test-backup-defined ()
  "Verify beads-backup is defined."
  (should (fboundp 'beads-backup)))

(ert-deftest beads-advanced-menu-test-export-defined ()
  "Verify beads-export is defined."
  (should (fboundp 'beads-export)))

(ert-deftest beads-advanced-menu-test-restore-defined ()
  "Verify beads-restore is defined."
  (should (fboundp 'beads-restore)))

(ert-deftest beads-advanced-menu-test-vc-defined ()
  "Verify beads-vc is defined."
  (should (fboundp 'beads-vc)))

(ert-deftest beads-advanced-menu-test-federation-defined ()
  "Verify beads-federation is defined."
  (should (fboundp 'beads-federation)))

(ert-deftest beads-advanced-menu-test-sql-defined ()
  "Verify beads-sql is defined."
  (should (fboundp 'beads-sql)))

;;; Integration Suffixes

(ert-deftest beads-advanced-menu-test-jira-defined ()
  "Verify beads-jira is defined."
  (should (fboundp 'beads-jira)))

(ert-deftest beads-advanced-menu-test-github-defined ()
  "Verify beads-github is defined."
  (should (fboundp 'beads-github)))

(ert-deftest beads-advanced-menu-test-repo-defined ()
  "Verify beads-repo is defined."
  (should (fboundp 'beads-repo)))

;;; Setup Suffixes

(ert-deftest beads-advanced-menu-test-init-defined ()
  "Verify beads-init is defined."
  (should (fboundp 'beads-init)))

(ert-deftest beads-advanced-menu-test-hooks-defined ()
  "Verify beads-hooks is defined."
  (should (fboundp 'beads-hooks)))

(ert-deftest beads-advanced-menu-test-info-defined ()
  "Verify beads-info is defined."
  (should (fboundp 'beads-info)))

;;; Memory Suffixes

(ert-deftest beads-advanced-menu-test-kv-defined ()
  "Verify beads-kv is defined."
  (should (fboundp 'beads-kv)))

(ert-deftest beads-advanced-menu-test-memories-defined ()
  "Verify beads-memories is defined."
  (should (fboundp 'beads-memories)))

;;; Audit & Admin Suffixes

(ert-deftest beads-advanced-menu-test-audit-defined ()
  "Verify beads-audit is defined."
  (should (fboundp 'beads-audit)))

(ert-deftest beads-advanced-menu-test-admin-defined ()
  "Verify beads-admin is defined."
  (should (fboundp 'beads-admin)))

(ert-deftest beads-advanced-menu-test-worktree-defined ()
  "Verify beads-worktree-menu is defined."
  (should (fboundp 'beads-worktree-menu)))

;;; Suffix Reachability (layout walk)

(defun beads-advanced-menu-test--suffix-pairs (spec)
  "Collect (KEY . COMMAND) pairs reachable from transient layout SPEC.
Walks vectors, `(CLASS PLIST CHILDREN...)' group forms, and suffix
plists recursively."
  (cond
   ((vectorp spec)
    (seq-mapcat #'beads-advanced-menu-test--suffix-pairs (append spec nil)))
   ((and (consp spec) (symbolp (car spec)))
    (let* ((rest (cdr spec))
           (props (if (keywordp (car rest)) rest (car rest)))
           (children (if (keywordp (car rest)) nil (cdr rest))))
      (append (when (and (listp props)
                         (plist-get props :key)
                         (plist-get props :command))
                (list (cons (plist-get props :key)
                            (plist-get props :command))))
              (seq-mapcat #'beads-advanced-menu-test--suffix-pairs children))))
   ((and (consp spec) (keywordp (car spec)))
    (let ((key (plist-get spec :key))
          (command (plist-get spec :command)))
      (when (and key command) (list (cons key command)))))
   ((consp spec)
    (seq-mapcat #'beads-advanced-menu-test--suffix-pairs spec))
   (t nil)))

(defun beads-advanced-menu-test--layout-suffixes (menu)
  "Return the (KEY . COMMAND) pairs registered on transient MENU."
  (beads-advanced-menu-test--suffix-pairs
   (get menu 'transient--layout)))

(ert-deftest beads-advanced-menu-test-new-bd-1.3-leaves-reachable ()
  "The new category-1 command leaves are registered on the menu."
  (let ((suffixes (beads-advanced-menu-test--layout-suffixes
                   'beads-advanced-menu)))
    (dolist (expected '(("Y" . beads-sync)
                        ("s" . beads-schema)
                        ("5" . beads-migrate-personal)
                        ("H" . beads-provenance)))
      (should (member expected suffixes)))))

(ert-deftest beads-advanced-menu-test-keys-unique ()
  "Every suffix key on the advanced menu is unique."
  (let ((keys (mapcar #'car
                      (beads-advanced-menu-test--layout-suffixes
                       'beads-advanced-menu))))
    (should (equal keys (delete-dups (copy-sequence keys))))))

(provide 'beads-advanced-menu-test)
;;; beads-advanced-menu-test.el ends here
