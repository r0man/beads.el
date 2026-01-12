;;; beads-command-search-test.el --- Tests for beads-command-search -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;;; Commentary:

;; Unit tests for the beads-command-search module.

;;; Code:

(require 'ert)
(require 'beads-command-search)
(require 'beads-test)

;;; Class Tests

(ert-deftest beads-command-search-test-class-exists ()
  "Test that beads-command-search class is defined."
  (should (find-class 'beads-command-search)))

(ert-deftest beads-command-search-test-inherits-from-json ()
  "Test that beads-command-search inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-search 'beads-command-json)))

(ert-deftest beads-command-search-test-create-default ()
  "Test creating search command with defaults."
  (let ((cmd (beads-command-search)))
    (should (null (oref cmd query)))
    (should (null (oref cmd status)))
    (should (null (oref cmd assignee)))
    (should (null (oref cmd limit)))))

(ert-deftest beads-command-search-test-create-with-query ()
  "Test creating search command with query."
  (let ((cmd (beads-command-search :query "authentication")))
    (should (equal "authentication" (oref cmd query)))))

(ert-deftest beads-command-search-test-create-with-status ()
  "Test creating search command with status filter."
  (let ((cmd (beads-command-search :status "open")))
    (should (equal "open" (oref cmd status)))))

(ert-deftest beads-command-search-test-create-with-assignee ()
  "Test creating search command with assignee filter."
  (let ((cmd (beads-command-search :assignee "alice")))
    (should (equal "alice" (oref cmd assignee)))))

(ert-deftest beads-command-search-test-create-with-type ()
  "Test creating search command with type filter."
  (let ((cmd (beads-command-search :issue-type "bug")))
    (should (equal "bug" (oref cmd issue-type)))))

(ert-deftest beads-command-search-test-create-with-sort ()
  "Test creating search command with sort option."
  (let ((cmd (beads-command-search :sort "priority" :reverse t)))
    (should (equal "priority" (oref cmd sort)))
    (should (eq t (oref cmd reverse)))))

;;; Subcommand Tests

(ert-deftest beads-command-search-test-subcommand ()
  "Test subcommand method returns 'search'."
  (let ((cmd (beads-command-search)))
    (should (equal "search" (beads-command-subcommand cmd)))))

;;; Validation Tests

(ert-deftest beads-command-search-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-search)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-search-test-validate-with-options ()
  "Test validation passes with various options."
  (let ((cmd (beads-command-search :query "bug"
                                    :status "open"
                                    :assignee "bob"
                                    :limit 20)))
    (should (null (beads-command-validate cmd)))))

;;; Command Line Tests

(ert-deftest beads-command-search-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-search :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "search" args)))))

(ert-deftest beads-command-search-test-command-line-with-json ()
  "Test command line includes --json when enabled."
  (let ((cmd (beads-command-search :json t)))
    (should (member "--json" (beads-command-line cmd)))))

(ert-deftest beads-command-search-test-command-line-with-query ()
  "Test command line includes query as positional argument."
  (let ((cmd (beads-command-search :query "authentication")))
    (let ((args (beads-command-line cmd)))
      (should (member "authentication" args)))))

(ert-deftest beads-command-search-test-command-line-with-status ()
  "Test command line includes --status option."
  (let ((cmd (beads-command-search :status "open")))
    (let ((args (beads-command-line cmd)))
      (should (member "--status" args))
      (should (member "open" args)))))

(ert-deftest beads-command-search-test-command-line-with-type ()
  "Test command line includes --type option."
  (let ((cmd (beads-command-search :issue-type "bug")))
    (let ((args (beads-command-line cmd)))
      (should (member "--type" args))
      (should (member "bug" args)))))

(ert-deftest beads-command-search-test-command-line-with-sort ()
  "Test command line includes --sort and --reverse options."
  (let ((cmd (beads-command-search :sort "priority" :reverse t)))
    (let ((args (beads-command-line cmd)))
      (should (member "--sort" args))
      (should (member "priority" args))
      (should (member "--reverse" args)))))

(ert-deftest beads-command-search-test-command-line-with-limit ()
  "Test command line includes --limit option."
  (let ((cmd (beads-command-search :limit 100)))
    (let ((args (beads-command-line cmd)))
      (should (member "--limit" args))
      (should (member "100" args)))))

(ert-deftest beads-command-search-test-command-line-with-dates ()
  "Test command line includes date filter options."
  (let ((cmd (beads-command-search :created-after "2025-01-01"
                                    :updated-before "2025-12-31")))
    (let ((args (beads-command-line cmd)))
      (should (member "--created-after" args))
      (should (member "2025-01-01" args))
      (should (member "--updated-before" args))
      (should (member "2025-12-31" args)))))

;;; Slot Property Tests

(ert-deftest beads-command-search-test-query-slot-properties ()
  "Test query slot has correct transient properties."
  (should (equal "q" (beads-meta-slot-property
                      'beads-command-search 'query :transient-key)))
  (should (eq 1 (beads-meta-slot-property
                 'beads-command-search 'query :positional))))

(ert-deftest beads-command-search-test-status-slot-properties ()
  "Test status slot has correct transient properties."
  (should (equal "s" (beads-meta-slot-property
                      'beads-command-search 'status :transient-key)))
  (should (equal "--status" (beads-meta-slot-property
                             'beads-command-search 'status :long-option))))

(ert-deftest beads-command-search-test-sort-slot-properties ()
  "Test sort slot has correct transient properties."
  (should (equal "o" (beads-meta-slot-property
                      'beads-command-search 'sort :transient-key)))
  (should (equal "--sort" (beads-meta-slot-property
                           'beads-command-search 'sort :long-option)))
  (should (equal '("priority" "created" "updated" "closed" "status" "id"
                   "title" "type" "assignee")
                 (beads-meta-slot-property
                  'beads-command-search 'sort :transient-choices))))

;;; Transient Tests

(ert-deftest beads-command-search-test-transient-defined ()
  "Test that beads-search transient prefix is defined."
  (should (fboundp 'beads-search)))

(ert-deftest beads-command-search-test-transient-is-prefix ()
  "Test that beads-search is a transient prefix."
  (should (get 'beads-search 'transient--prefix)))

(ert-deftest beads-command-search-test-execute-suffix-defined ()
  "Test that execute suffix is defined."
  (should (fboundp 'beads-search--execute)))

(ert-deftest beads-command-search-test-preview-suffix-defined ()
  "Test that preview suffix is defined."
  (should (fboundp 'beads-search--preview)))

(ert-deftest beads-command-search-test-reset-suffix-defined ()
  "Test that reset suffix is defined."
  (should (fboundp 'beads-search--reset)))

(provide 'beads-command-search-test)
;;; beads-command-search-test.el ends here
