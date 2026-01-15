;;; beads-command-merge-slot-test.el --- Tests for beads-command-merge-slot -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-merge-slot command classes.

;;; Code:

(require 'ert)
(require 'beads-command-merge-slot)

;;; Unit Tests: beads-command-merge-slot-create command-line

(ert-deftest beads-command-merge-slot-create-test-command-line-basic ()
  "Unit test: merge-slot create builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-create))
         (args (beads-command-line cmd)))
    (should (member "merge-slot" args))
    (should (member "create" args))))

;;; Unit Tests: beads-command-merge-slot-check command-line

(ert-deftest beads-command-merge-slot-check-test-command-line-basic ()
  "Unit test: merge-slot check builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-check))
         (args (beads-command-line cmd)))
    (should (member "merge-slot" args))
    (should (member "check" args))))

;;; Unit Tests: beads-command-merge-slot-acquire command-line

(ert-deftest beads-command-merge-slot-acquire-test-command-line-basic ()
  "Unit test: merge-slot acquire builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-acquire))
         (args (beads-command-line cmd)))
    (should (member "merge-slot" args))
    (should (member "acquire" args))))

;;; Unit Tests: beads-command-merge-slot-release command-line

(ert-deftest beads-command-merge-slot-release-test-command-line-basic ()
  "Unit test: merge-slot release builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-release))
         (args (beads-command-line cmd)))
    (should (member "merge-slot" args))
    (should (member "release" args))))

(provide 'beads-command-merge-slot-test)
;;; beads-command-merge-slot-test.el ends here
