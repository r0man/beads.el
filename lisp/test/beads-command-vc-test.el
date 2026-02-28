;;; beads-command-vc-test.el --- Tests for beads-command-vc -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-command-vc.el.

;;; Code:

(require 'ert)
(require 'beads-command-vc)

;;; Tests for VC Commit

(ert-deftest beads-vc-test-commit-class-exists ()
  "Test that beads-command-vc-commit class is defined."
  (should (cl-find-class 'beads-command-vc-commit)))

(ert-deftest beads-vc-test-commit-subcommand ()
  "Test that subcommand returns `vc commit'."
  (let ((cmd (beads-command-vc-commit)))
    (should (equal (beads-command-subcommand cmd) "vc commit"))))

(ert-deftest beads-vc-test-commit-command-line ()
  "Test commit command line generation."
  (let ((beads-executable "bd")
        (cmd (beads-command-vc-commit :message "test commit")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "vc" cmd-line))
      (should (member "commit" cmd-line))
      (should (member "--message" cmd-line)))))

;;; Tests for VC Merge

(ert-deftest beads-vc-test-merge-class-exists ()
  "Test that beads-command-vc-merge class is defined."
  (should (cl-find-class 'beads-command-vc-merge)))

(ert-deftest beads-vc-test-merge-subcommand ()
  "Test that subcommand returns `vc merge'."
  (let ((cmd (beads-command-vc-merge)))
    (should (equal (beads-command-subcommand cmd) "vc merge"))))

(ert-deftest beads-vc-test-merge-validate-missing-branch ()
  "Test validation when branch is missing."
  (let ((cmd (beads-command-vc-merge)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-vc-test-merge-validate-with-branch ()
  "Test validation with branch."
  (let ((cmd (beads-command-vc-merge :branch "feature")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-vc-test-merge-command-line ()
  "Test merge command line generation."
  (let ((beads-executable "bd")
        (cmd (beads-command-vc-merge :branch "feature")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "vc" cmd-line))
      (should (member "merge" cmd-line))
      (should (member "feature" cmd-line)))))

(ert-deftest beads-vc-test-merge-command-line-with-strategy ()
  "Test merge command line with strategy."
  (let ((beads-executable "bd")
        (cmd (beads-command-vc-merge :branch "feature" :strategy "ours")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "--strategy" cmd-line))
      (should (member "ours" cmd-line)))))

;;; Tests for VC Status

(ert-deftest beads-vc-test-status-class-exists ()
  "Test that beads-command-vc-status class is defined."
  (should (cl-find-class 'beads-command-vc-status)))

(ert-deftest beads-vc-test-status-subcommand ()
  "Test that subcommand returns `vc status'."
  (let ((cmd (beads-command-vc-status)))
    (should (equal (beads-command-subcommand cmd) "vc status"))))

(ert-deftest beads-vc-test-status-command-line ()
  "Test status command line generation."
  (let ((beads-executable "bd")
        (cmd (beads-command-vc-status)))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "vc" cmd-line))
      (should (member "status" cmd-line)))))

;;; Tests for VC Parent Menu

(ert-deftest beads-vc-test-parent-menu-defined ()
  "Test that beads-vc parent menu is defined."
  (should (fboundp 'beads-vc)))

(ert-deftest beads-vc-test-parent-menu-is-prefix ()
  "Test that beads-vc is a transient prefix."
  (should (get 'beads-vc 'transient--prefix)))

;;; Tests for Individual Transient Menus

(ert-deftest beads-vc-test-commit-transient-defined ()
  "Test that beads-vc-commit transient is defined."
  (should (fboundp 'beads-vc-commit)))

(ert-deftest beads-vc-test-merge-transient-defined ()
  "Test that beads-vc-merge transient is defined."
  (should (fboundp 'beads-vc-merge)))

(ert-deftest beads-vc-test-status-transient-defined ()
  "Test that beads-vc-status transient is defined."
  (should (fboundp 'beads-vc-status)))

(provide 'beads-command-vc-test)
;;; beads-command-vc-test.el ends here
