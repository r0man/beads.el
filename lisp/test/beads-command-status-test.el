;;; beads-command-status-test.el --- Tests for beads-command-status -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;;; Commentary:

;; Unit tests for the beads-command-status module.

;;; Code:

(require 'ert)
(require 'beads-command-status)
(require 'beads-test)

;;; Class Tests

(ert-deftest beads-command-status-test-class-exists ()
  "Test that beads-command-status class is defined."
  (should (find-class 'beads-command-status)))

(ert-deftest beads-command-status-test-inherits-from-json ()
  "Test that beads-command-status inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-status 'beads-command-json)))

(ert-deftest beads-command-status-test-create-default ()
  "Test creating status command with defaults."
  (let ((cmd (beads-command-status)))
    (should (null (oref cmd all-issues)))
    (should (null (oref cmd assigned)))
    (should (null (oref cmd no-activity)))))

(ert-deftest beads-command-status-test-create-with-all ()
  "Test creating status command with --all flag."
  (let ((cmd (beads-command-status :all-issues t)))
    (should (eq t (oref cmd all-issues)))))

(ert-deftest beads-command-status-test-create-with-assigned ()
  "Test creating status command with --assigned flag."
  (let ((cmd (beads-command-status :assigned t)))
    (should (eq t (oref cmd assigned)))))

(ert-deftest beads-command-status-test-create-with-no-activity ()
  "Test creating status command with --no-activity flag."
  (let ((cmd (beads-command-status :no-activity t)))
    (should (eq t (oref cmd no-activity)))))

;;; Subcommand Tests

(ert-deftest beads-command-status-test-subcommand ()
  "Test subcommand method returns 'status'."
  (let ((cmd (beads-command-status)))
    (should (equal "status" (beads-command-subcommand cmd)))))

;;; Validation Tests

(ert-deftest beads-command-status-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-status)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-status-test-validate-with-options ()
  "Test validation passes with all options."
  (let ((cmd (beads-command-status :all-issues t
                                    :assigned t
                                    :no-activity t)))
    (should (null (beads-command-validate cmd)))))

;;; Command Line Tests

(ert-deftest beads-command-status-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-status :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "status" args)))))

(ert-deftest beads-command-status-test-command-line-with-json ()
  "Test command line includes --json when enabled."
  (let ((cmd (beads-command-status :json t)))
    (should (member "--json" (beads-command-line cmd)))))

(ert-deftest beads-command-status-test-command-line-with-all ()
  "Test command line includes --all flag."
  (let ((cmd (beads-command-status :all-issues t)))
    (should (member "--all" (beads-command-line cmd)))))

(ert-deftest beads-command-status-test-command-line-with-assigned ()
  "Test command line includes --assigned flag."
  (let ((cmd (beads-command-status :assigned t)))
    (should (member "--assigned" (beads-command-line cmd)))))

(ert-deftest beads-command-status-test-command-line-with-no-activity ()
  "Test command line includes --no-activity flag."
  (let ((cmd (beads-command-status :no-activity t)))
    (should (member "--no-activity" (beads-command-line cmd)))))

(ert-deftest beads-command-status-test-command-line-all-options ()
  "Test command line with all options."
  (let ((cmd (beads-command-status :all-issues t
                                    :assigned t
                                    :no-activity t
                                    :json t)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "status" args))
      (should (member "--json" args))
      (should (member "--all" args))
      (should (member "--assigned" args))
      (should (member "--no-activity" args)))))

;;; Slot Property Tests

(ert-deftest beads-command-status-test-all-slot-properties ()
  "Test all-issues slot has correct transient properties."
  (should (equal "a" (beads-meta-slot-property
                      'beads-command-status 'all-issues :transient-key)))
  (should (equal "--all" (beads-meta-slot-property
                          'beads-command-status 'all-issues :long-option)))
  (should (eq :boolean (beads-meta-slot-property
                        'beads-command-status 'all-issues :option-type))))

(ert-deftest beads-command-status-test-assigned-slot-properties ()
  "Test assigned slot has correct transient properties."
  (should (equal "m" (beads-meta-slot-property
                      'beads-command-status 'assigned :transient-key)))
  (should (equal "--assigned" (beads-meta-slot-property
                               'beads-command-status 'assigned :long-option))))

(ert-deftest beads-command-status-test-no-activity-slot-properties ()
  "Test no-activity slot has correct transient properties."
  (should (equal "n" (beads-meta-slot-property
                      'beads-command-status 'no-activity :transient-key)))
  (should (equal "--no-activity" (beads-meta-slot-property
                                  'beads-command-status 'no-activity :long-option))))

;;; Transient Tests

(ert-deftest beads-command-status-test-transient-defined ()
  "Test that beads-status transient prefix is defined."
  (should (fboundp 'beads-status)))

(ert-deftest beads-command-status-test-transient-is-prefix ()
  "Test that beads-status is a transient prefix."
  (should (get 'beads-status 'transient--prefix)))

(ert-deftest beads-command-status-test-execute-suffix-defined ()
  "Test that execute suffix is defined."
  (should (fboundp 'beads-status--execute)))

(ert-deftest beads-command-status-test-preview-suffix-defined ()
  "Test that preview suffix is defined."
  (should (fboundp 'beads-status--preview)))

(ert-deftest beads-command-status-test-reset-suffix-defined ()
  "Test that reset suffix is defined."
  (should (fboundp 'beads-status--reset)))

(provide 'beads-command-status-test)
;;; beads-command-status-test.el ends here
