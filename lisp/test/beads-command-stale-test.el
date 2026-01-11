;;; beads-command-stale-test.el --- Tests for beads-command-stale -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;;; Commentary:

;; Unit tests for the beads-command-stale module.

;;; Code:

(require 'ert)
(require 'beads-command-stale)
(require 'beads-test)

;;; Class Tests

(ert-deftest beads-command-stale-test-class-exists ()
  "Test that beads-command-stale class is defined."
  (should (find-class 'beads-command-stale)))

(ert-deftest beads-command-stale-test-inherits-from-json ()
  "Test that beads-command-stale inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-stale 'beads-command-json)))

(ert-deftest beads-command-stale-test-create-default ()
  "Test creating stale command with defaults."
  (let ((cmd (beads-command-stale)))
    (should (null (oref cmd days)))
    (should (null (oref cmd limit)))
    (should (null (oref cmd status)))))

(ert-deftest beads-command-stale-test-create-with-days ()
  "Test creating stale command with days option."
  (let ((cmd (beads-command-stale :days 14)))
    (should (eq 14 (oref cmd days)))))

(ert-deftest beads-command-stale-test-create-with-limit ()
  "Test creating stale command with limit option."
  (let ((cmd (beads-command-stale :limit 100)))
    (should (eq 100 (oref cmd limit)))))

(ert-deftest beads-command-stale-test-create-with-status ()
  "Test creating stale command with status option."
  (let ((cmd (beads-command-stale :status "in_progress")))
    (should (equal "in_progress" (oref cmd status)))))

;;; Subcommand Tests

(ert-deftest beads-command-stale-test-subcommand ()
  "Test subcommand method returns 'stale'."
  (let ((cmd (beads-command-stale)))
    (should (equal "stale" (beads-command-subcommand cmd)))))

;;; Validation Tests

(ert-deftest beads-command-stale-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-stale)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-stale-test-validate-with-options ()
  "Test validation passes with all options."
  (let ((cmd (beads-command-stale :days 7 :limit 20 :status "open")))
    (should (null (beads-command-validate cmd)))))

;;; Command Line Tests

(ert-deftest beads-command-stale-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-stale :json nil)))
    ;; Without --json, should just have subcommand
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "stale" args)))))

(ert-deftest beads-command-stale-test-command-line-with-json ()
  "Test command line includes --json when enabled."
  (let ((cmd (beads-command-stale :json t)))
    (should (member "--json" (beads-command-line cmd)))))

(ert-deftest beads-command-stale-test-command-line-with-days ()
  "Test command line includes --days option."
  (let ((cmd (beads-command-stale :days 14)))
    (let ((args (beads-command-line cmd)))
      (should (member "--days" args))
      (should (member "14" args)))))

(ert-deftest beads-command-stale-test-command-line-with-limit ()
  "Test command line includes --limit option."
  (let ((cmd (beads-command-stale :limit 100)))
    (let ((args (beads-command-line cmd)))
      (should (member "--limit" args))
      (should (member "100" args)))))

(ert-deftest beads-command-stale-test-command-line-with-status ()
  "Test command line includes --status option."
  (let ((cmd (beads-command-stale :status "blocked")))
    (let ((args (beads-command-line cmd)))
      (should (member "--status" args))
      (should (member "blocked" args)))))

(ert-deftest beads-command-stale-test-command-line-all-options ()
  "Test command line with all options."
  (let ((cmd (beads-command-stale :days 7 :limit 50 :status "open" :json t)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "stale" args))
      (should (member "--json" args))
      (should (member "--days" args))
      (should (member "7" args))
      (should (member "--limit" args))
      (should (member "50" args))
      (should (member "--status" args))
      (should (member "open" args)))))

;;; Slot Property Tests

(ert-deftest beads-command-stale-test-days-slot-properties ()
  "Test days slot has correct transient properties."
  (should (equal "d" (beads-meta-slot-property
                      'beads-command-stale 'days :transient-key)))
  (should (equal "--days" (beads-meta-slot-property
                           'beads-command-stale 'days :long-option)))
  (should (eq :integer (beads-meta-slot-property
                        'beads-command-stale 'days :option-type))))

(ert-deftest beads-command-stale-test-limit-slot-properties ()
  "Test limit slot has correct transient properties."
  (should (equal "n" (beads-meta-slot-property
                      'beads-command-stale 'limit :transient-key)))
  (should (equal "--limit" (beads-meta-slot-property
                            'beads-command-stale 'limit :long-option)))
  (should (eq :integer (beads-meta-slot-property
                        'beads-command-stale 'limit :option-type))))

(ert-deftest beads-command-stale-test-status-slot-properties ()
  "Test status slot has correct transient properties."
  (should (equal "s" (beads-meta-slot-property
                      'beads-command-stale 'status :transient-key)))
  (should (equal "--status" (beads-meta-slot-property
                             'beads-command-stale 'status :long-option)))
  (should (equal '("open" "in_progress" "blocked" "deferred")
                 (beads-meta-slot-property
                  'beads-command-stale 'status :transient-choices))))

;;; Transient Tests

(ert-deftest beads-command-stale-test-transient-defined ()
  "Test that beads-stale transient prefix is defined."
  (should (fboundp 'beads-stale)))

(ert-deftest beads-command-stale-test-transient-is-prefix ()
  "Test that beads-stale is a transient prefix."
  (should (get 'beads-stale 'transient--prefix)))

(ert-deftest beads-command-stale-test-execute-suffix-defined ()
  "Test that execute suffix is defined."
  (should (fboundp 'beads-stale--execute)))

(ert-deftest beads-command-stale-test-preview-suffix-defined ()
  "Test that preview suffix is defined."
  (should (fboundp 'beads-stale--preview)))

(ert-deftest beads-command-stale-test-reset-suffix-defined ()
  "Test that reset suffix is defined."
  (should (fboundp 'beads-stale--reset)))

(provide 'beads-command-stale-test)
;;; beads-command-stale-test.el ends here
