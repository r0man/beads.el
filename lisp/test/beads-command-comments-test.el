;;; beads-command-comments-test.el --- Tests for beads-command-comments -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-comments command classes.

;;; Code:

(require 'ert)
(require 'beads-command-comments)

;;; Unit Tests: beads-command-comments command-line

(ert-deftest beads-command-comments-test-command-line-basic ()
  "Unit test: comments builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-comments :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "comments" args))
    (should (member "bd-1" args))))

;; Note: beads-command-comments doesn't have a limit slot currently

(ert-deftest beads-command-comments-test-validation-missing-issue-id ()
  "Unit test: comments validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-comments)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-comments-test-validation-success ()
  "Unit test: comments validation succeeds with issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-comments :issue-id "bd-1")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-comments-add command-line

(ert-deftest beads-command-comments-add-test-command-line-basic ()
  "Unit test: comments add builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-comments-add :issue-id "bd-1" :text "Test comment"))
         (args (beads-command-line cmd)))
    (should (member "comments" args))
    (should (member "add" args))
    (should (member "bd-1" args))
    (should (member "Test comment" args))))

(ert-deftest beads-command-comments-add-test-validation-missing-issue-id ()
  "Unit test: comments add validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-comments-add :text "Test")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-comments-add-test-validation-missing-text ()
  "Unit test: comments add validation fails without text or file."
  :tags '(:unit)
  (let ((cmd (beads-command-comments-add :issue-id "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-comments-add-test-validation-success ()
  "Unit test: comments add validation succeeds with required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-comments-add :issue-id "bd-1" :text "Test")))
    (should (null (beads-command-validate cmd)))))

(provide 'beads-command-comments-test)
;;; beads-command-comments-test.el ends here
