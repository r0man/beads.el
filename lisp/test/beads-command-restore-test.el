;;; beads-command-restore-test.el --- Tests for beads-command-restore -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-command-restore.el.

;;; Code:

(require 'ert)
(require 'beads-command-restore)

;;; Tests for Command Class

(ert-deftest beads-restore-test-command-class-exists ()
  "Test that beads-command-restore class is defined."
  (should (cl-find-class 'beads-command-restore)))

(ert-deftest beads-restore-test-command-subcommand ()
  "Test that subcommand returns `restore'."
  (let ((cmd (beads-command-restore)))
    (should (equal (beads-command-subcommand cmd) "restore"))))

;;; Tests for Validation

(ert-deftest beads-restore-test-validate-missing-issue-id ()
  "Test validation when issue-id is missing."
  (let ((cmd (beads-command-restore)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-restore-test-validate-with-issue-id ()
  "Test validation with valid issue-id."
  (let ((cmd (beads-command-restore :issue-id "bd-42")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for Command Line

(ert-deftest beads-restore-test-command-line ()
  "Test command line generation."
  (let ((beads-executable "bd")
        (cmd (beads-command-restore :issue-id "bd-42")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "restore" cmd-line))
      (should (member "bd-42" cmd-line)))))

;;; Tests for Transient Definition

(ert-deftest beads-restore-test-transient-defined ()
  "Test that beads-restore transient is defined."
  (should (fboundp 'beads-restore)))

(ert-deftest beads-restore-test-transient-is-prefix ()
  "Test that beads-restore is a transient prefix."
  (should (get 'beads-restore 'transient--prefix)))

(provide 'beads-command-restore-test)
;;; beads-command-restore-test.el ends here
