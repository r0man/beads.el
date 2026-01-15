;;; beads-command-agent-test.el --- Tests for beads-command-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-agent command classes.

;;; Code:

(require 'ert)
(require 'beads-command-agent)

;;; Unit Tests: beads-command-agent-state command-line

(ert-deftest beads-command-agent-state-test-command-line-basic ()
  "Unit test: agent state builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-agent-state :agent-id "agent-1"))
         (args (beads-command-line cmd)))
    (should (member "agent" args))
    (should (member "state" args))
    (should (member "agent-1" args))))

(ert-deftest beads-command-agent-state-test-command-line-state ()
  "Unit test: agent state includes state positional argument."
  :tags '(:unit)
  (let* ((cmd (beads-command-agent-state :agent-id "agent-1" :state "running"))
         (args (beads-command-line cmd)))
    (should (member "running" args))))

(ert-deftest beads-command-agent-state-test-validation-missing-agent-id ()
  "Unit test: agent state validation fails without agent-id."
  :tags '(:unit)
  (let ((cmd (beads-command-agent-state :state "running")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-agent-state-test-validation-success ()
  "Unit test: agent state validation succeeds with required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-agent-state :agent-id "agent-1" :state "running")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-agent-heartbeat command-line

(ert-deftest beads-command-agent-heartbeat-test-command-line-basic ()
  "Unit test: agent heartbeat builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-agent-heartbeat :agent-id "agent-1"))
         (args (beads-command-line cmd)))
    (should (member "agent" args))
    (should (member "heartbeat" args))
    (should (member "agent-1" args))))

(ert-deftest beads-command-agent-heartbeat-test-validation-missing-agent-id ()
  "Unit test: agent heartbeat validation fails without agent-id."
  :tags '(:unit)
  (let ((cmd (beads-command-agent-heartbeat)))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-agent-show command-line

(ert-deftest beads-command-agent-show-test-command-line-basic ()
  "Unit test: agent show builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-agent-show :agent-id "agent-1"))
         (args (beads-command-line cmd)))
    (should (member "agent" args))
    (should (member "show" args))
    (should (member "agent-1" args))))

(ert-deftest beads-command-agent-show-test-validation-missing-agent-id ()
  "Unit test: agent show validation fails without agent-id."
  :tags '(:unit)
  (let ((cmd (beads-command-agent-show)))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-agent-backfill-labels command-line

(ert-deftest beads-command-agent-backfill-labels-test-command-line-basic ()
  "Unit test: agent backfill-labels builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-agent-backfill-labels))
         (args (beads-command-line cmd)))
    (should (member "agent" args))
    (should (member "backfill-labels" args))))

(ert-deftest beads-command-agent-backfill-labels-test-command-line-dry-run ()
  "Unit test: agent backfill-labels includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-agent-backfill-labels :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(provide 'beads-command-agent-test)
;;; beads-command-agent-test.el ends here
