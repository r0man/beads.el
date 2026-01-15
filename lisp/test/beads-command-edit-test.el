;;; beads-command-edit-test.el --- Tests for beads-command-edit -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-edit command classes.

;;; Code:

(require 'ert)
(require 'beads-command-edit)

;;; Unit Tests: beads-command-edit command-line

(ert-deftest beads-command-edit-test-command-line-basic ()
  "Unit test: edit builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-edit :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "edit" args))
    (should (member "bd-1" args))))

(ert-deftest beads-command-edit-test-command-line-description ()
  "Unit test: edit includes --description flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-edit :issue-id "bd-1" :description t))
         (args (beads-command-line cmd)))
    (should (member "bd-1" args))
    (should (member "--description" args))))

(ert-deftest beads-command-edit-test-command-line-title ()
  "Unit test: edit includes --title flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-edit :issue-id "bd-1" :title t))
         (args (beads-command-line cmd)))
    (should (member "bd-1" args))
    (should (member "--title" args))))

(ert-deftest beads-command-edit-test-validation-missing-issue-id ()
  "Unit test: edit validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-edit)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-edit-test-validation-success ()
  "Unit test: edit validation succeeds with issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-edit :issue-id "bd-1")))
    (should (null (beads-command-validate cmd)))))

(provide 'beads-command-edit-test)
;;; beads-command-edit-test.el ends here
