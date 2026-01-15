;;; beads-command-state-test.el --- Tests for beads-command-state -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-state command classes.

;;; Code:

(require 'ert)
(require 'beads-command-state)

;;; Unit Tests: beads-command-set-state command-line

(ert-deftest beads-command-set-state-test-command-line-basic ()
  "Unit test: set-state builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-set-state :issue-id "bd-1"
                                        :dimension-value "patrol=active"))
         (args (beads-command-line cmd)))
    (should (member "set-state" args))
    (should (member "bd-1" args))
    (should (member "patrol=active" args))))

(ert-deftest beads-command-set-state-test-command-line-reason ()
  "Unit test: set-state includes --reason option."
  :tags '(:unit)
  (let* ((cmd (beads-command-set-state :issue-id "bd-1"
                                        :dimension-value "mode=degraded"
                                        :reason "service overloaded"))
         (args (beads-command-line cmd)))
    (should (member "--reason" args))
    (should (member "service overloaded" args))))

(ert-deftest beads-command-set-state-test-validation-missing-issue-id ()
  "Unit test: set-state validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-set-state :dimension-value "patrol=active")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-set-state-test-validation-missing-dimension-value ()
  "Unit test: set-state validation fails without dimension-value."
  :tags '(:unit)
  (let ((cmd (beads-command-set-state :issue-id "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-set-state-test-validation-invalid-format ()
  "Unit test: set-state validation fails with invalid format."
  :tags '(:unit)
  (let ((cmd (beads-command-set-state :issue-id "bd-1"
                                       :dimension-value "invalid")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-set-state-test-validation-success ()
  "Unit test: set-state validation succeeds with required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-set-state :issue-id "bd-1"
                                       :dimension-value "patrol=active")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-state command-line

(ert-deftest beads-command-state-test-command-line-basic ()
  "Unit test: state builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-state :issue-id "bd-1" :dimension "patrol"))
         (args (beads-command-line cmd)))
    (should (member "state" args))
    (should (member "bd-1" args))
    (should (member "patrol" args))))

(ert-deftest beads-command-state-test-validation-missing-issue-id ()
  "Unit test: state validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-state :dimension "patrol")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-state-test-validation-missing-dimension ()
  "Unit test: state validation fails without dimension."
  :tags '(:unit)
  (let ((cmd (beads-command-state :issue-id "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-state-test-validation-success ()
  "Unit test: state validation succeeds with required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-state :issue-id "bd-1" :dimension "patrol")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-state-list command-line

(ert-deftest beads-command-state-list-test-command-line-basic ()
  "Unit test: state list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-state-list :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "state" args))
    (should (member "list" args))
    (should (member "bd-1" args))))

(ert-deftest beads-command-state-list-test-validation-missing-issue-id ()
  "Unit test: state list validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-state-list)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-state-list-test-validation-success ()
  "Unit test: state list validation succeeds with issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-state-list :issue-id "bd-1")))
    (should (null (beads-command-validate cmd)))))

(provide 'beads-command-state-test)
;;; beads-command-state-test.el ends here
