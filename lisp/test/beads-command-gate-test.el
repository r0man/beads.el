;;; beads-command-gate-test.el --- Tests for beads-command-gate -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-gate command classes.

;;; Code:

(require 'ert)
(require 'beads-command-gate)

;;; Unit Tests: beads-command-gate-list command-line

(ert-deftest beads-command-gate-list-test-command-line-basic ()
  "Unit test: gate list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gate-list))
         (args (beads-command-line cmd)))
    (should (member "gate" args))
    (should (member "list" args))))

(ert-deftest beads-command-gate-list-test-command-line-all ()
  "Unit test: gate list includes --all option."
  :tags '(:unit)
  (let* ((cmd (beads-command-gate-list :all t))
         (args (beads-command-line cmd)))
    (should (member "--all" args))))

(ert-deftest beads-command-gate-list-test-command-line-limit ()
  "Unit test: gate list includes --limit option."
  :tags '(:unit)
  (let* ((cmd (beads-command-gate-list :limit 10))
         (args (beads-command-line cmd)))
    (should (member "--limit" args))
    (should (member "10" args))))

;;; Unit Tests: beads-command-gate-check command-line

(ert-deftest beads-command-gate-check-test-command-line-basic ()
  "Unit test: gate check builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gate-check))
         (args (beads-command-line cmd)))
    (should (member "gate" args))
    (should (member "check" args))))

(ert-deftest beads-command-gate-check-test-command-line-type ()
  "Unit test: gate check includes --type option."
  :tags '(:unit)
  (let* ((cmd (beads-command-gate-check :type "timer"))
         (args (beads-command-line cmd)))
    (should (member "--type" args))
    (should (member "timer" args))))

;;; Unit Tests: beads-command-gate-resolve command-line

(ert-deftest beads-command-gate-resolve-test-command-line-basic ()
  "Unit test: gate resolve builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gate-resolve :gate-id "gate-1"))
         (args (beads-command-line cmd)))
    (should (member "gate" args))
    (should (member "resolve" args))
    (should (member "gate-1" args))))

(ert-deftest beads-command-gate-resolve-test-validation-missing-gate-id ()
  "Unit test: gate resolve validation fails without gate-id."
  :tags '(:unit)
  (let ((cmd (beads-command-gate-resolve)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-gate-resolve-test-validation-success ()
  "Unit test: gate resolve validation succeeds with gate-id."
  :tags '(:unit)
  (let ((cmd (beads-command-gate-resolve :gate-id "gate-1")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-gate-show command-line

(ert-deftest beads-command-gate-show-test-command-line-basic ()
  "Unit test: gate show builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gate-show :gate-id "gate-1"))
         (args (beads-command-line cmd)))
    (should (member "gate" args))
    (should (member "show" args))
    (should (member "gate-1" args))))

(ert-deftest beads-command-gate-show-test-validation-missing-gate-id ()
  "Unit test: gate show validation fails without gate-id."
  :tags '(:unit)
  (let ((cmd (beads-command-gate-show)))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-gate-add-waiter command-line

(ert-deftest beads-command-gate-add-waiter-test-command-line-basic ()
  "Unit test: gate add-waiter builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gate-add-waiter :gate-id "gate-1" :waiter-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "gate" args))
    (should (member "add-waiter" args))
    (should (member "gate-1" args))
    (should (member "bd-1" args))))

;;; Unit Tests: beads-command-gate-discover command-line

(ert-deftest beads-command-gate-discover-test-command-line-basic ()
  "Unit test: gate discover builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gate-discover :gate-id "gate-1"))
         (args (beads-command-line cmd)))
    (should (member "gate" args))
    (should (member "discover" args))
    (should (member "gate-1" args))))

(provide 'beads-command-gate-test)
;;; beads-command-gate-test.el ends here
