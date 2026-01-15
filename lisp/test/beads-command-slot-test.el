;;; beads-command-slot-test.el --- Tests for beads-command-slot -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-slot command classes.

;;; Code:

(require 'ert)
(require 'beads-command-slot)

;;; Unit Tests: beads-command-slot-show command-line

(ert-deftest beads-command-slot-show-test-command-line-basic ()
  "Unit test: slot show builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-slot-show :agent-id "agent-1"))
         (args (beads-command-line cmd)))
    (should (member "slot" args))
    (should (member "show" args))
    (should (member "agent-1" args))))

(ert-deftest beads-command-slot-show-test-validation-missing-agent-id ()
  "Unit test: slot show validation fails without agent-id."
  :tags '(:unit)
  (let ((cmd (beads-command-slot-show)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-slot-show-test-validation-success ()
  "Unit test: slot show validation succeeds with agent-id."
  :tags '(:unit)
  (let ((cmd (beads-command-slot-show :agent-id "agent-1")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-slot-set command-line

(ert-deftest beads-command-slot-set-test-command-line-basic ()
  "Unit test: slot set builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-slot-set :agent-id "agent-1"
                                      :slot-name "hook"
                                      :bead-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "slot" args))
    (should (member "set" args))
    (should (member "agent-1" args))
    (should (member "hook" args))
    (should (member "bd-1" args))))

(ert-deftest beads-command-slot-set-test-validation-missing-agent-id ()
  "Unit test: slot set validation fails without agent-id."
  :tags '(:unit)
  (let ((cmd (beads-command-slot-set :slot-name "hook" :bead-id "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-slot-set-test-validation-missing-slot-name ()
  "Unit test: slot set validation fails without slot-name."
  :tags '(:unit)
  (let ((cmd (beads-command-slot-set :agent-id "agent-1" :bead-id "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-slot-set-test-validation-missing-bead-id ()
  "Unit test: slot set validation fails without bead-id."
  :tags '(:unit)
  (let ((cmd (beads-command-slot-set :agent-id "agent-1" :slot-name "hook")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-slot-set-test-validation-success ()
  "Unit test: slot set validation succeeds with all required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-slot-set :agent-id "agent-1"
                                     :slot-name "hook"
                                     :bead-id "bd-1")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-slot-clear command-line

(ert-deftest beads-command-slot-clear-test-command-line-basic ()
  "Unit test: slot clear builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-slot-clear :agent-id "agent-1" :slot-name "hook"))
         (args (beads-command-line cmd)))
    (should (member "slot" args))
    (should (member "clear" args))
    (should (member "agent-1" args))
    (should (member "hook" args))))

(ert-deftest beads-command-slot-clear-test-validation-missing-agent-id ()
  "Unit test: slot clear validation fails without agent-id."
  :tags '(:unit)
  (let ((cmd (beads-command-slot-clear :slot-name "hook")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-slot-clear-test-validation-missing-slot-name ()
  "Unit test: slot clear validation fails without slot-name."
  :tags '(:unit)
  (let ((cmd (beads-command-slot-clear :agent-id "agent-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-slot-clear-test-validation-success ()
  "Unit test: slot clear validation succeeds with required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-slot-clear :agent-id "agent-1" :slot-name "hook")))
    (should (null (beads-command-validate cmd)))))

(provide 'beads-command-slot-test)
;;; beads-command-slot-test.el ends here
