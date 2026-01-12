;;; beads-command-count-test.el --- Tests for beads-command-count -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;;; Commentary:

;; Unit tests for the beads-command-count module.

;;; Code:

(require 'ert)
(require 'beads-command-count)
(require 'beads-test)

;;; Class Tests

(ert-deftest beads-command-count-test-class-exists ()
  "Test that beads-command-count class is defined."
  (should (find-class 'beads-command-count)))

(ert-deftest beads-command-count-test-inherits-from-json ()
  "Test that beads-command-count inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-count 'beads-command-json)))

(ert-deftest beads-command-count-test-create-default ()
  "Test creating count command with defaults."
  (let ((cmd (beads-command-count)))
    (should (null (oref cmd assignee)))
    (should (null (oref cmd status)))
    (should (null (oref cmd by-status)))
    (should (null (oref cmd by-priority)))))

(ert-deftest beads-command-count-test-create-with-assignee ()
  "Test creating count command with assignee filter."
  (let ((cmd (beads-command-count :assignee "alice")))
    (should (equal "alice" (oref cmd assignee)))))

(ert-deftest beads-command-count-test-create-with-status ()
  "Test creating count command with status filter."
  (let ((cmd (beads-command-count :status "open")))
    (should (equal "open" (oref cmd status)))))

(ert-deftest beads-command-count-test-create-with-by-flags ()
  "Test creating count command with grouping flags."
  (let ((cmd (beads-command-count :by-status t :by-priority t)))
    (should (eq t (oref cmd by-status)))
    (should (eq t (oref cmd by-priority)))))

;;; Subcommand Tests

(ert-deftest beads-command-count-test-subcommand ()
  "Test subcommand method returns 'count'."
  (let ((cmd (beads-command-count)))
    (should (equal "count" (beads-command-subcommand cmd)))))

;;; Validation Tests

(ert-deftest beads-command-count-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-count)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-count-test-validate-with-options ()
  "Test validation passes with all options."
  (let ((cmd (beads-command-count :assignee "bob"
                                   :status "open"
                                   :by-status t)))
    (should (null (beads-command-validate cmd)))))

;;; Command Line Tests

(ert-deftest beads-command-count-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-count :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "count" args)))))

(ert-deftest beads-command-count-test-command-line-with-json ()
  "Test command line includes --json when enabled."
  (let ((cmd (beads-command-count :json t)))
    (should (member "--json" (beads-command-line cmd)))))

(ert-deftest beads-command-count-test-command-line-with-assignee ()
  "Test command line includes --assignee option."
  (let ((cmd (beads-command-count :assignee "alice")))
    (let ((args (beads-command-line cmd)))
      (should (member "--assignee" args))
      (should (member "alice" args)))))

(ert-deftest beads-command-count-test-command-line-with-by-status ()
  "Test command line includes --by-status flag."
  (let ((cmd (beads-command-count :by-status t)))
    (let ((args (beads-command-line cmd)))
      (should (member "--by-status" args)))))

(ert-deftest beads-command-count-test-command-line-all-groupings ()
  "Test command line with all grouping flags."
  (let ((cmd (beads-command-count :by-status t
                                   :by-priority t
                                   :by-type t
                                   :by-assignee t
                                   :by-label t)))
    (let ((args (beads-command-line cmd)))
      (should (member "--by-status" args))
      (should (member "--by-priority" args))
      (should (member "--by-type" args))
      (should (member "--by-assignee" args))
      (should (member "--by-label" args)))))

;;; Slot Property Tests

(ert-deftest beads-command-count-test-assignee-slot-properties ()
  "Test assignee slot has correct transient properties."
  (should (equal "a" (beads-meta-slot-property
                      'beads-command-count 'assignee :transient-key)))
  (should (equal "--assignee" (beads-meta-slot-property
                               'beads-command-count 'assignee :long-option))))

(ert-deftest beads-command-count-test-by-status-slot-properties ()
  "Test by-status slot has correct transient properties."
  (should (equal "bs" (beads-meta-slot-property
                       'beads-command-count 'by-status :transient-key)))
  (should (equal "--by-status" (beads-meta-slot-property
                                'beads-command-count 'by-status :long-option)))
  (should (eq :boolean (beads-meta-slot-property
                        'beads-command-count 'by-status :option-type))))

;;; Transient Tests

(ert-deftest beads-command-count-test-transient-defined ()
  "Test that beads-count transient prefix is defined."
  (should (fboundp 'beads-count)))

(ert-deftest beads-command-count-test-transient-is-prefix ()
  "Test that beads-count is a transient prefix."
  (should (get 'beads-count 'transient--prefix)))

(ert-deftest beads-command-count-test-execute-suffix-defined ()
  "Test that execute suffix is defined."
  (should (fboundp 'beads-count--execute)))

(ert-deftest beads-command-count-test-preview-suffix-defined ()
  "Test that preview suffix is defined."
  (should (fboundp 'beads-count--preview)))

(ert-deftest beads-command-count-test-reset-suffix-defined ()
  "Test that reset suffix is defined."
  (should (fboundp 'beads-count--reset)))

(provide 'beads-command-count-test)
;;; beads-command-count-test.el ends here
