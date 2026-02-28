;;; beads-command-branch-test.el --- Tests for beads-command-branch -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-command-branch.el.

;;; Code:

(require 'ert)
(require 'beads-command-branch)

;;; Tests for Command Class

(ert-deftest beads-branch-test-command-class-exists ()
  "Test that beads-command-branch class is defined."
  (should (cl-find-class 'beads-command-branch)))

(ert-deftest beads-branch-test-command-subcommand ()
  "Test that subcommand returns `branch'."
  (let ((cmd (beads-command-branch)))
    (should (equal (beads-command-subcommand cmd) "branch"))))

;;; Tests for Validation

(ert-deftest beads-branch-test-validate-no-args ()
  "Test validation with no arguments (list mode)."
  (let ((cmd (beads-command-branch)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-branch-test-validate-with-name ()
  "Test validation with branch name (create mode)."
  (let ((cmd (beads-command-branch :name "feature")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for Command Line

(ert-deftest beads-branch-test-command-line-list ()
  "Test command line for listing branches."
  (let ((beads-executable "bd")
        (cmd (beads-command-branch)))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "branch" cmd-line)))))

(ert-deftest beads-branch-test-command-line-create ()
  "Test command line for creating a branch."
  (let ((beads-executable "bd")
        (cmd (beads-command-branch :name "feature")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "branch" cmd-line))
      (should (member "feature" cmd-line)))))

;;; Tests for Transient Definition

(ert-deftest beads-branch-test-transient-defined ()
  "Test that beads-branch transient is defined."
  (should (fboundp 'beads-branch)))

(ert-deftest beads-branch-test-transient-is-prefix ()
  "Test that beads-branch is a transient prefix."
  (should (get 'beads-branch 'transient--prefix)))

(provide 'beads-command-branch-test)
;;; beads-command-branch-test.el ends here
