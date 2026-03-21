;;; beads-ops-menu-test.el --- Tests for beads-ops-menu -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for beads-ops-menu: the mid-frequency operations transient
;; (Phase 5 of the beads.el UX redesign).

;;; Code:

(require 'ert)
(require 'beads-ops-menu)

;;; Transient Definition Tests

(ert-deftest beads-ops-menu-test-defined ()
  "Verify beads-ops-menu is defined as a command."
  (should (fboundp 'beads-ops-menu)))

(ert-deftest beads-ops-menu-test-is-transient-prefix ()
  "Verify beads-ops-menu is a transient prefix command."
  (should (get 'beads-ops-menu 'transient--prefix)))

(ert-deftest beads-ops-menu-test-command-symbol ()
  "Verify the prefix command symbol matches."
  (let ((prefix (get 'beads-ops-menu 'transient--prefix)))
    (should (eq (oref prefix command) 'beads-ops-menu))))

;;; Issue Lifecycle Suffixes

(ert-deftest beads-ops-menu-test-defer-defined ()
  "Verify beads-defer is defined."
  (should (fboundp 'beads-defer)))

(ert-deftest beads-ops-menu-test-undefer-defined ()
  "Verify beads-undefer is defined."
  (should (fboundp 'beads-undefer)))

(ert-deftest beads-ops-menu-test-delete-defined ()
  "Verify beads-delete is defined."
  (should (fboundp 'beads-delete)))

(ert-deftest beads-ops-menu-test-move-defined ()
  "Verify beads-move is defined."
  (should (fboundp 'beads-move)))

(ert-deftest beads-ops-menu-test-refile-defined ()
  "Verify beads-refile is defined."
  (should (fboundp 'beads-refile)))

(ert-deftest beads-ops-menu-test-rename-defined ()
  "Verify beads-rename is defined."
  (should (fboundp 'beads-rename)))

;;; Views & Reports Suffixes

(ert-deftest beads-ops-menu-test-count-defined ()
  "Verify beads-count is defined."
  (should (fboundp 'beads-count)))

(ert-deftest beads-ops-menu-test-stats-defined ()
  "Verify beads-stats is defined."
  (should (fboundp 'beads-stats)))

(ert-deftest beads-ops-menu-test-stale-defined ()
  "Verify beads-stale is defined."
  (should (fboundp 'beads-stale)))

(ert-deftest beads-ops-menu-test-types-defined ()
  "Verify beads-types is defined."
  (should (fboundp 'beads-types)))

(ert-deftest beads-ops-menu-test-lint-defined ()
  "Verify beads-lint is defined."
  (should (fboundp 'beads-lint)))

(ert-deftest beads-ops-menu-test-orphans-defined ()
  "Verify beads-orphans is defined."
  (should (fboundp 'beads-orphans)))

;;; Issue Details Suffixes

(ert-deftest beads-ops-menu-test-children-defined ()
  "Verify beads-children is defined."
  (should (fboundp 'beads-children)))

(ert-deftest beads-ops-menu-test-comments-menu-defined ()
  "Verify beads-comments-menu is defined."
  (should (fboundp 'beads-comments-menu)))

(ert-deftest beads-ops-menu-test-todo-defined ()
  "Verify beads-todo is defined."
  (should (fboundp 'beads-todo)))

(ert-deftest beads-ops-menu-test-query-defined ()
  "Verify beads-query is defined."
  (should (fboundp 'beads-query)))

;;; Workflow Suffixes

(ert-deftest beads-ops-menu-test-gate-defined ()
  "Verify beads-gate is defined as a transient prefix."
  (should (fboundp 'beads-gate))
  (should (get 'beads-gate 'transient--prefix)))


(ert-deftest beads-ops-menu-test-swarm-defined ()
  "Verify beads-swarm is defined as a transient prefix."
  (should (fboundp 'beads-swarm))
  (should (get 'beads-swarm 'transient--prefix)))

(ert-deftest beads-ops-menu-test-state-menu-defined ()
  "Verify beads-state-menu is defined as a transient prefix."
  (should (fboundp 'beads-state-menu))
  (should (get 'beads-state-menu 'transient--prefix)))

(ert-deftest beads-ops-menu-test-cook-defined ()
  "Verify beads-cook is defined."
  (should (fboundp 'beads-cook)))

(ert-deftest beads-ops-menu-test-ship-defined ()
  "Verify beads-ship is defined."
  (should (fboundp 'beads-ship)))

(ert-deftest beads-ops-menu-test-set-state-defined ()
  "Verify beads-set-state is defined."
  (should (fboundp 'beads-set-state)))

(provide 'beads-ops-menu-test)
;;; beads-ops-menu-test.el ends here
