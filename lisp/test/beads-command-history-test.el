;;; beads-command-history-test.el --- Tests for beads-command-history -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-command-history.el.

;;; Code:

(require 'ert)
(require 'beads-command-history)

;;; Tests for Command Class

(ert-deftest beads-history-test-command-class-exists ()
  "Test that beads-command-history class is defined."
  (should (cl-find-class 'beads-command-history)))

(ert-deftest beads-history-test-command-subcommand ()
  "Test that subcommand returns `history'."
  (let ((cmd (beads-command-history)))
    (should (equal (beads-command-subcommand cmd) "history"))))

;;; Tests for Validation

(ert-deftest beads-history-test-validate-missing-issue-id ()
  "Test validation when issue-id is missing."
  (let ((cmd (beads-command-history)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-history-test-validate-with-issue-id ()
  "Test validation with valid issue-id."
  (let ((cmd (beads-command-history :issue-id "bd-42")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-history-test-validate-empty-issue-id ()
  "Test validation with empty issue-id."
  (let ((cmd (beads-command-history :issue-id "")))
    (should (beads-command-validate cmd))))

;;; Tests for Command Line

(ert-deftest beads-history-test-command-line-basic ()
  "Test command line generation."
  (let ((beads-executable "bd")
        (cmd (beads-command-history :issue-id "bd-42")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "history" cmd-line))
      (should (member "bd-42" cmd-line)))))

(ert-deftest beads-history-test-command-line-with-limit ()
  "Test command line with --limit."
  (let ((beads-executable "bd")
        (cmd (beads-command-history :issue-id "bd-42" :limit 10)))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "--limit" cmd-line))
      (should (member "10" cmd-line)))))

;;; Tests for Transient Definition

(ert-deftest beads-history-test-transient-defined ()
  "Test that beads-history transient is defined."
  (should (fboundp 'beads-history)))

(ert-deftest beads-history-test-transient-is-prefix ()
  "Test that beads-history is a transient prefix."
  (should (get 'beads-history 'transient--prefix)))

(ert-deftest beads-history-test-class-defined ()
  "Test that command class is defined."
  (should (find-class 'beads-command-history nil)))

(provide 'beads-command-history-test)
;;; beads-command-history-test.el ends here
