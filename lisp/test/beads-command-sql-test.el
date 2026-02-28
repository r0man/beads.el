;;; beads-command-sql-test.el --- Tests for beads-command-sql -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-command-sql.el.

;;; Code:

(require 'ert)
(require 'beads-command-sql)

;;; Tests for Command Class

(ert-deftest beads-sql-test-command-class-exists ()
  "Test that beads-command-sql class is defined."
  (should (cl-find-class 'beads-command-sql)))

(ert-deftest beads-sql-test-command-subcommand ()
  "Test that subcommand returns `sql'."
  (let ((cmd (beads-command-sql)))
    (should (equal (beads-command-subcommand cmd) "sql"))))

;;; Tests for Validation

(ert-deftest beads-sql-test-validate-missing-query ()
  "Test validation when query is missing."
  (let ((cmd (beads-command-sql)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-sql-test-validate-with-query ()
  "Test validation with valid query."
  (let ((cmd (beads-command-sql :query "SELECT * FROM issues")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-sql-test-validate-empty-query ()
  "Test validation with empty query."
  (let ((cmd (beads-command-sql :query "")))
    (should (beads-command-validate cmd))))

;;; Tests for Command Line

(ert-deftest beads-sql-test-command-line-basic ()
  "Test command line generation."
  (let ((beads-executable "bd")
        (cmd (beads-command-sql :query "SELECT * FROM issues")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "sql" cmd-line))
      (should (member "SELECT * FROM issues" cmd-line)))))

(ert-deftest beads-sql-test-command-line-with-csv ()
  "Test command line with --csv flag."
  (let ((beads-executable "bd")
        (cmd (beads-command-sql :query "SELECT * FROM issues" :csv t)))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "--csv" cmd-line)))))

;;; Tests for Transient Definition

(ert-deftest beads-sql-test-transient-defined ()
  "Test that beads-sql transient is defined."
  (should (fboundp 'beads-sql)))

(ert-deftest beads-sql-test-transient-is-prefix ()
  "Test that beads-sql is a transient prefix."
  (should (get 'beads-sql 'transient--prefix)))

(provide 'beads-command-sql-test)
;;; beads-command-sql-test.el ends here
