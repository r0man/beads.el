;;; beads-command-info-test.el --- Tests for beads-command-info -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;;; Commentary:

;; Unit tests for the beads-command-info module.

;;; Code:

(require 'ert)
(require 'beads-command-info)
(require 'beads-test)

;;; Class Tests

(ert-deftest beads-command-info-test-class-exists ()
  "Test that beads-command-info class is defined."
  (should (find-class 'beads-command-info)))

(ert-deftest beads-command-info-test-inherits-from-json ()
  "Test that beads-command-info inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-info 'beads-command-json)))

(ert-deftest beads-command-info-test-create-default ()
  "Test creating info command with defaults."
  (let ((cmd (beads-command-info)))
    (should (null (oref cmd schema)))
    (should (null (oref cmd whats-new)))
    (should (null (oref cmd thanks)))))

(ert-deftest beads-command-info-test-create-with-schema ()
  "Test creating info command with --schema flag."
  (let ((cmd (beads-command-info :schema t)))
    (should (eq t (oref cmd schema)))))

(ert-deftest beads-command-info-test-create-with-whats-new ()
  "Test creating info command with --whats-new flag."
  (let ((cmd (beads-command-info :whats-new t)))
    (should (eq t (oref cmd whats-new)))))

(ert-deftest beads-command-info-test-create-with-thanks ()
  "Test creating info command with --thanks flag."
  (let ((cmd (beads-command-info :thanks t)))
    (should (eq t (oref cmd thanks)))))

;;; Subcommand Tests

(ert-deftest beads-command-info-test-subcommand ()
  "Test subcommand method returns 'info'."
  (let ((cmd (beads-command-info)))
    (should (equal "info" (beads-command-subcommand cmd)))))

;;; Validation Tests

(ert-deftest beads-command-info-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-info)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-info-test-validate-with-options ()
  "Test validation passes with all options."
  (let ((cmd (beads-command-info :schema t
                                  :whats-new t
                                  :thanks t)))
    (should (null (beads-command-validate cmd)))))

;;; Command Line Tests

(ert-deftest beads-command-info-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-info :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "info" args)))))

(ert-deftest beads-command-info-test-command-line-with-json ()
  "Test command line includes --json when enabled."
  (let ((cmd (beads-command-info :json t)))
    (should (member "--json" (beads-command-line cmd)))))

(ert-deftest beads-command-info-test-command-line-with-schema ()
  "Test command line includes --schema flag."
  (let ((cmd (beads-command-info :schema t)))
    (should (member "--schema" (beads-command-line cmd)))))

(ert-deftest beads-command-info-test-command-line-with-whats-new ()
  "Test command line includes --whats-new flag."
  (let ((cmd (beads-command-info :whats-new t)))
    (should (member "--whats-new" (beads-command-line cmd)))))

(ert-deftest beads-command-info-test-command-line-with-thanks ()
  "Test command line includes --thanks flag."
  (let ((cmd (beads-command-info :thanks t)))
    (should (member "--thanks" (beads-command-line cmd)))))

(ert-deftest beads-command-info-test-command-line-all-options ()
  "Test command line with all options."
  (let ((cmd (beads-command-info :schema t
                                  :whats-new t
                                  :thanks t
                                  :json t)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "info" args))
      (should (member "--json" args))
      (should (member "--schema" args))
      (should (member "--whats-new" args))
      (should (member "--thanks" args)))))

;;; Slot Property Tests

(ert-deftest beads-command-info-test-schema-slot-properties ()
  "Test schema slot has correct transient properties."
  (should (equal "s" (beads-meta-slot-property
                      'beads-command-info 'schema :transient-key)))
  (should (equal "schema" (beads-meta-slot-property
                           'beads-command-info 'schema :long-option)))
  (should (eq :boolean (beads-meta-slot-property
                        'beads-command-info 'schema :option-type))))

(ert-deftest beads-command-info-test-whats-new-slot-properties ()
  "Test whats-new slot has correct transient properties."
  (should (equal "w" (beads-meta-slot-property
                      'beads-command-info 'whats-new :transient-key)))
  (should (equal "whats-new" (beads-meta-slot-property
                              'beads-command-info 'whats-new :long-option))))

(ert-deftest beads-command-info-test-thanks-slot-properties ()
  "Test thanks slot has correct transient properties."
  (should (equal "t" (beads-meta-slot-property
                      'beads-command-info 'thanks :transient-key)))
  (should (equal "thanks" (beads-meta-slot-property
                           'beads-command-info 'thanks :long-option))))

;;; Transient Tests

(ert-deftest beads-command-info-test-transient-defined ()
  "Test that beads-info transient prefix is defined."
  (should (fboundp 'beads-info)))

(ert-deftest beads-command-info-test-transient-is-prefix ()
  "Test that beads-info is a transient prefix."
  (should (get 'beads-info 'transient--prefix)))

(ert-deftest beads-command-info-test-execute-suffix-defined ()
  "Test that execute suffix is defined."
  (should (fboundp 'beads-info--execute)))

(ert-deftest beads-command-info-test-preview-suffix-defined ()
  "Test that preview suffix is defined."
  (should (fboundp 'beads-info--preview)))

(ert-deftest beads-command-info-test-reset-suffix-defined ()
  "Test that reset suffix is defined."
  (should (fboundp 'beads-info--reset)))

(provide 'beads-command-info-test)
;;; beads-command-info-test.el ends here
