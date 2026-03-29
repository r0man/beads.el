;;; beads-command-merge-slot-test.el --- Tests for beads-command-merge-slot -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-merge-slot command classes.

;;; Code:

(require 'ert)
(require 'beads-command-merge-slot)

;;; Unit Tests: beads-command-merge-slot-create

(ert-deftest beads-command-merge-slot-create-test-class-exists ()
  "Unit test: beads-command-merge-slot-create class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-merge-slot-create)))

(ert-deftest beads-command-merge-slot-create-test-command-line ()
  "Unit test: merge-slot create builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-create))
         (args (beads-command-line cmd)))
    (should (member "merge-slot" args))
    (should (member "create" args))))

(ert-deftest beads-command-merge-slot-create-test-validation ()
  "Unit test: merge-slot create validation always passes."
  :tags '(:unit)
  (let ((cmd (beads-command-merge-slot-create)))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-merge-slot-check

(ert-deftest beads-command-merge-slot-check-test-class-exists ()
  "Unit test: beads-command-merge-slot-check class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-merge-slot-check)))

(ert-deftest beads-command-merge-slot-check-test-command-line ()
  "Unit test: merge-slot check builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-check))
         (args (beads-command-line cmd)))
    (should (member "merge-slot" args))
    (should (member "check" args))))

(ert-deftest beads-command-merge-slot-check-test-validation ()
  "Unit test: merge-slot check validation always passes."
  :tags '(:unit)
  (let ((cmd (beads-command-merge-slot-check)))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-merge-slot-acquire

(ert-deftest beads-command-merge-slot-acquire-test-class-exists ()
  "Unit test: beads-command-merge-slot-acquire class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-merge-slot-acquire)))

(ert-deftest beads-command-merge-slot-acquire-test-command-line-basic ()
  "Unit test: merge-slot acquire builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-acquire))
         (args (beads-command-line cmd)))
    (should (member "merge-slot" args))
    (should (member "acquire" args))))

(ert-deftest beads-command-merge-slot-acquire-test-command-line-with-holder ()
  "Unit test: merge-slot acquire includes --holder flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-acquire :holder "furiosa"))
         (args (beads-command-line cmd)))
    (should (member "merge-slot" args))
    (should (member "acquire" args))
    (should (member "--holder" args))
    (should (member "furiosa" args))))

(ert-deftest beads-command-merge-slot-acquire-test-command-line-with-wait ()
  "Unit test: merge-slot acquire includes --wait flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-acquire :wait t))
         (args (beads-command-line cmd)))
    (should (member "--wait" args))))

(ert-deftest beads-command-merge-slot-acquire-test-validation ()
  "Unit test: merge-slot acquire validation always passes."
  :tags '(:unit)
  (let ((cmd (beads-command-merge-slot-acquire)))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-merge-slot-release

(ert-deftest beads-command-merge-slot-release-test-class-exists ()
  "Unit test: beads-command-merge-slot-release class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-merge-slot-release)))

(ert-deftest beads-command-merge-slot-release-test-command-line ()
  "Unit test: merge-slot release builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-release))
         (args (beads-command-line cmd)))
    (should (member "merge-slot" args))
    (should (member "release" args))))

(ert-deftest beads-command-merge-slot-release-test-command-line-with-holder ()
  "Unit test: merge-slot release includes --holder flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge-slot-release :holder "furiosa"))
         (args (beads-command-line cmd)))
    (should (member "--holder" args))
    (should (member "furiosa" args))))

(ert-deftest beads-command-merge-slot-release-test-validation ()
  "Unit test: merge-slot release validation always passes."
  :tags '(:unit)
  (let ((cmd (beads-command-merge-slot-release)))
    (should-not (beads-command-validate cmd))))

(provide 'beads-command-merge-slot-test)
;;; beads-command-merge-slot-test.el ends here
