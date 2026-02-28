;;; beads-command-diff-test.el --- Tests for beads-command-diff -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-command-diff.el.

;;; Code:

(require 'ert)
(require 'beads-command-diff)

;;; Test Utilities

(defun beads-diff-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (_program &optional _infile destination _display &rest _args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; Tests for Command Class

(ert-deftest beads-diff-test-command-class-exists ()
  "Test that beads-command-diff class is defined."
  (should (cl-find-class 'beads-command-diff)))

(ert-deftest beads-diff-test-command-subcommand ()
  "Test that subcommand returns `diff'."
  (let ((cmd (beads-command-diff)))
    (should (equal (beads-command-subcommand cmd) "diff"))))

;;; Tests for Validation

(ert-deftest beads-diff-test-validate-missing-from-ref ()
  "Test validation when from-ref is missing."
  (let ((cmd (beads-command-diff :to-ref "HEAD")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-diff-test-validate-missing-to-ref ()
  "Test validation when to-ref is missing."
  (let ((cmd (beads-command-diff :from-ref "HEAD~1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-diff-test-validate-both-refs ()
  "Test validation with both refs."
  (let ((cmd (beads-command-diff :from-ref "HEAD~1" :to-ref "HEAD")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-diff-test-validate-empty-from-ref ()
  "Test validation with empty from-ref."
  (let ((cmd (beads-command-diff :from-ref "" :to-ref "HEAD")))
    (should (beads-command-validate cmd))))

;;; Tests for Command Line

(ert-deftest beads-diff-test-command-line-basic ()
  "Test command line generation."
  (let ((beads-executable "bd")
        (cmd (beads-command-diff :from-ref "HEAD~1" :to-ref "HEAD")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "diff" cmd-line))
      (should (member "HEAD~1" cmd-line))
      (should (member "HEAD" cmd-line)))))

;;; Tests for Transient Definition

(ert-deftest beads-diff-test-transient-defined ()
  "Test that beads-diff transient is defined."
  (should (fboundp 'beads-diff)))

(ert-deftest beads-diff-test-transient-is-prefix ()
  "Test that beads-diff is a transient prefix."
  (should (get 'beads-diff 'transient--prefix)))

(ert-deftest beads-diff-test-bang-function-exists ()
  "Test that convenience function exists."
  (should (fboundp 'beads-command-diff!)))

(provide 'beads-command-diff-test)
;;; beads-command-diff-test.el ends here
