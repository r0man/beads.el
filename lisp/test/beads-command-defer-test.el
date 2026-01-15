;;; beads-command-defer-test.el --- Tests for beads-command-defer -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-defer command classes.

;;; Code:

(require 'ert)
(require 'beads-command-defer)

;;; Unit Tests: beads-command-defer command-line

(ert-deftest beads-command-defer-test-command-line-basic ()
  "Unit test: defer builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-defer :issue-ids '("bd-1")))
         (args (beads-command-line cmd)))
    (should (member "defer" args))))

(ert-deftest beads-command-defer-test-command-line-multiple-ids ()
  "Unit test: defer handles multiple issue IDs."
  :tags '(:unit)
  (let* ((cmd (beads-command-defer :issue-ids '("bd-1" "bd-2")))
         (args (beads-command-line cmd)))
    (should (member "defer" args))))

(ert-deftest beads-command-defer-test-command-line-until ()
  "Unit test: defer includes --until option."
  :tags '(:unit)
  (let* ((cmd (beads-command-defer :issue-ids '("bd-1") :until "2025-12-31"))
         (args (beads-command-line cmd)))
    (should (member "--until" args))
    (should (member "2025-12-31" args))))

(ert-deftest beads-command-defer-test-validation-missing-issue-ids ()
  "Unit test: defer validation fails without issue-ids."
  :tags '(:unit)
  (let ((cmd (beads-command-defer)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-defer-test-validation-success ()
  "Unit test: defer validation succeeds with issue-ids."
  :tags '(:unit)
  (let ((cmd (beads-command-defer :issue-ids '("bd-1"))))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-undefer command-line

(ert-deftest beads-command-undefer-test-command-line-basic ()
  "Unit test: undefer builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-undefer :issue-ids '("bd-1")))
         (args (beads-command-line cmd)))
    (should (member "undefer" args))))

(ert-deftest beads-command-undefer-test-validation-missing-issue-ids ()
  "Unit test: undefer validation fails without issue-ids."
  :tags '(:unit)
  (let ((cmd (beads-command-undefer)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-undefer-test-validation-success ()
  "Unit test: undefer validation succeeds with issue-ids."
  :tags '(:unit)
  (let ((cmd (beads-command-undefer :issue-ids '("bd-1"))))
    (should (null (beads-command-validate cmd)))))

(provide 'beads-command-defer-test)
;;; beads-command-defer-test.el ends here
