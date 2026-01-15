;;; beads-command-swarm-test.el --- Tests for beads-command-swarm -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-swarm command classes.

;;; Code:

(require 'ert)
(require 'beads-command-swarm)

;;; Unit Tests: beads-command-swarm-create command-line

(ert-deftest beads-command-swarm-create-test-command-line-basic ()
  "Unit test: swarm create builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-create :epic-id "epic-1"))
         (args (beads-command-line cmd)))
    (should (member "swarm" args))
    (should (member "create" args))
    (should (member "epic-1" args))))

(ert-deftest beads-command-swarm-create-test-validation-missing-epic-id ()
  "Unit test: swarm create validation fails without epic-id."
  :tags '(:unit)
  (let ((cmd (beads-command-swarm-create)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-swarm-create-test-validation-success ()
  "Unit test: swarm create validation succeeds with epic-id."
  :tags '(:unit)
  (let ((cmd (beads-command-swarm-create :epic-id "epic-1")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-swarm-list command-line

(ert-deftest beads-command-swarm-list-test-command-line-basic ()
  "Unit test: swarm list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-list))
         (args (beads-command-line cmd)))
    (should (member "swarm" args))
    (should (member "list" args))))

;;; Unit Tests: beads-command-swarm-status command-line

(ert-deftest beads-command-swarm-status-test-command-line-basic ()
  "Unit test: swarm status builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-status :swarm-id "swarm-1"))
         (args (beads-command-line cmd)))
    (should (member "swarm" args))
    (should (member "status" args))
    (should (member "swarm-1" args))))

;;; Unit Tests: beads-command-swarm-validate command-line

(ert-deftest beads-command-swarm-validate-test-command-line-basic ()
  "Unit test: swarm validate builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-swarm-validate :epic-id "epic-1"))
         (args (beads-command-line cmd)))
    (should (member "swarm" args))
    (should (member "validate" args))
    (should (member "epic-1" args))))

(ert-deftest beads-command-swarm-validate-test-validation-missing-epic-id ()
  "Unit test: swarm validate validation fails without epic-id."
  :tags '(:unit)
  (let ((cmd (beads-command-swarm-validate)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-swarm-validate-test-validation-success ()
  "Unit test: swarm validate validation succeeds with epic-id."
  :tags '(:unit)
  (let ((cmd (beads-command-swarm-validate :epic-id "epic-1")))
    (should (null (beads-command-validate cmd)))))

(provide 'beads-command-swarm-test)
;;; beads-command-swarm-test.el ends here
