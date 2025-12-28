;;; beads-command-test.el --- Tests for beads-command.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-command.el EIEIO command classes.
;; Tests cover command line building, validation, parsing, and execution.

;;; Code:

(require 'ert)
(require 'beads-command)

;;; Helper Functions

(defmacro beads-command-test--with-executable (exe &rest body)
  "Execute BODY with `beads-executable' bound to EXE."
  (declare (indent 1))
  `(let ((beads-executable ,exe))
     ,@body))

;;; Tests for beads-command--validate-string-list

(ert-deftest beads-command-test-validate-string-list-nil ()
  "Test validation of nil value."
  (should (null (beads-command--validate-string-list nil "test"))))

(ert-deftest beads-command-test-validate-string-list-valid ()
  "Test validation of valid string list."
  (should (null (beads-command--validate-string-list '("a" "b" "c") "test"))))

(ert-deftest beads-command-test-validate-string-list-not-list ()
  "Test validation of non-list value."
  (let ((result (beads-command--validate-string-list "not-a-list" "field")))
    (should (stringp result))
    (should (string-match-p "must be a list" result))))

(ert-deftest beads-command-test-validate-string-list-non-strings ()
  "Test validation of list with non-string elements."
  (let ((result (beads-command--validate-string-list '("a" 123 "c") "labels")))
    (should (stringp result))
    (should (string-match-p "must contain only strings" result))))

;;; Tests for Base Command Class (beads-command)

(ert-deftest beads-command-test-base-global-flags-empty ()
  "Test base command with no global flags set."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init)))
      (let ((line (beads-command-line cmd)))
        ;; First element is executable
        (should (equal (car line) "bd"))
        ;; Second element is command name
        (should (equal (cadr line) "init"))))))

(ert-deftest beads-command-test-base-global-flags-actor ()
  "Test base command with --actor flag."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init :actor "alice")))
      (let ((line (beads-command-line cmd)))
        (should (member "--actor" line))
        (should (member "alice" line))))))

(ert-deftest beads-command-test-base-global-flags-db ()
  "Test base command with --db flag."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init :db "/path/to/db.sqlite")))
      (let ((line (beads-command-line cmd)))
        (should (member "--db" line))
        (should (member "/path/to/db.sqlite" line))))))

(ert-deftest beads-command-test-base-global-flags-boolean ()
  "Test base command with boolean global flags."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init
                :no-auto-flush t
                :no-auto-import t
                :no-daemon t
                :no-db t
                :sandbox t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--no-auto-flush" line))
        (should (member "--no-auto-import" line))
        (should (member "--no-daemon" line))
        (should (member "--no-db" line))
        (should (member "--sandbox" line))))))

(ert-deftest beads-command-test-base-validate-default ()
  "Test that base command validation passes by default."
  (let ((cmd (beads-command-init)))
    (should (null (beads-command-validate cmd)))))

;;; Tests for beads-command-init

(ert-deftest beads-command-test-init-basic ()
  "Test init command with no options."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init)))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "init" line))))))

(ert-deftest beads-command-test-init-with-prefix ()
  "Test init command with --prefix option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init :prefix "myproject")))
      (let ((line (beads-command-line cmd)))
        (should (member "--prefix" line))
        (should (member "myproject" line))))))

(ert-deftest beads-command-test-init-with-branch ()
  "Test init command with --branch option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init :branch "develop")))
      (let ((line (beads-command-line cmd)))
        (should (member "--branch" line))
        (should (member "develop" line))))))

(ert-deftest beads-command-test-init-with-quiet ()
  "Test init command with --quiet option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init :quiet t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--quiet" line))))))

(ert-deftest beads-command-test-init-with-contributor ()
  "Test init command with --contributor option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init :contributor t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--contributor" line))))))

(ert-deftest beads-command-test-init-with-team ()
  "Test init command with --team option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init :team t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--team" line))))))

(ert-deftest beads-command-test-init-with-skip-merge-driver ()
  "Test init command with --skip-merge-driver option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init :skip-merge-driver t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--skip-merge-driver" line))))))

(ert-deftest beads-command-test-init-validate-contributor-team-conflict ()
  "Test init command validation rejects --contributor with --team."
  (let ((cmd (beads-command-init :contributor t :team t)))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Cannot use both" error)))))

(ert-deftest beads-command-test-init-validate-valid ()
  "Test init command validation passes for valid commands."
  (let ((cmd (beads-command-init :prefix "test" :branch "main")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for beads-command-quickstart

(ert-deftest beads-command-test-quickstart-basic ()
  "Test quickstart command builds correct command line."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-quickstart)))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "quickstart" line))))))

(ert-deftest beads-command-test-quickstart-validate ()
  "Test quickstart command validation always passes."
  (let ((cmd (beads-command-quickstart)))
    (should (null (beads-command-validate cmd)))))

;;; Tests for beads-command-json

(ert-deftest beads-command-test-json-flag-default ()
  "Test that JSON commands include --json by default."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list)))
      (let ((line (beads-command-line cmd)))
        (should (member "--json" line))))))

(ert-deftest beads-command-test-json-flag-disabled ()
  "Test that --json can be disabled."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list :json nil)))
      (let ((line (beads-command-line cmd)))
        (should-not (member "--json" line))))))

;;; Tests for beads-command-export

(ert-deftest beads-command-test-export-basic ()
  "Test export command with no options."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-export)))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "export" line))))))

(ert-deftest beads-command-test-export-with-options ()
  "Test export command with options."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-export
                :force t
                :format "jsonl"
                :output "/tmp/issues.jsonl"
                :status "open")))
      (let ((line (beads-command-line cmd)))
        (should (member "--force" line))
        (should (member "-f" line))
        (should (member "jsonl" line))
        (should (member "-o" line))
        (should (member "/tmp/issues.jsonl" line))
        (should (member "-s" line))
        (should (member "open" line))))))

(ert-deftest beads-command-test-export-validate-invalid-format ()
  "Test export command validation rejects invalid format."
  (let ((cmd (beads-command-export :format "invalid")))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Invalid format" error)))))

(ert-deftest beads-command-test-export-validate-invalid-status ()
  "Test export command validation rejects invalid status."
  (let ((cmd (beads-command-export :status "invalid")))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Invalid status" error)))))

(ert-deftest beads-command-test-export-validate-valid ()
  "Test export command validation passes for valid options."
  (let ((cmd (beads-command-export :format "jsonl" :status "open")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for beads-command-import

(ert-deftest beads-command-test-import-basic ()
  "Test import command with no options."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-import)))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "import" line))))))

(ert-deftest beads-command-test-import-with-options ()
  "Test import command with options."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-import
                :clear-duplicate-external-refs t
                :dedupe-after t
                :dry-run t
                :input "/tmp/issues.jsonl"
                :orphan-handling "resurrect"
                :rename-on-import t
                :skip-existing t
                :strict t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--clear-duplicate-external-refs" line))
        (should (member "--dedupe-after" line))
        (should (member "--dry-run" line))
        (should (member "-i" line))
        (should (member "/tmp/issues.jsonl" line))
        (should (member "--orphan-handling" line))
        (should (member "resurrect" line))
        (should (member "--rename-on-import" line))
        (should (member "-s" line))
        (should (member "--strict" line))))))

(ert-deftest beads-command-test-import-validate-invalid-orphan-handling ()
  "Test import command validation rejects invalid orphan-handling."
  (let ((cmd (beads-command-import :orphan-handling "invalid")))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Invalid orphan-handling" error)))))

(ert-deftest beads-command-test-import-validate-valid ()
  "Test import command validation passes for valid options."
  (let ((cmd (beads-command-import :orphan-handling "resurrect")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for beads-command-list

(ert-deftest beads-command-test-list-basic ()
  "Test list command with no filters."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list)))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "list" line))
        (should (member "--json" line))))))

(ert-deftest beads-command-test-list-status-filter ()
  "Test list command with status filter."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list :status "open")))
      (let ((line (beads-command-line cmd)))
        (should (member "--status" line))
        (should (member "open" line))))))

(ert-deftest beads-command-test-list-priority-filter ()
  "Test list command with priority filter."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list :priority 1)))
      (let ((line (beads-command-line cmd)))
        (should (member "--priority" line))
        (should (member "1" line))))))

(ert-deftest beads-command-test-list-priority-range ()
  "Test list command with priority range."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list :priority-min 1 :priority-max 3)))
      (let ((line (beads-command-line cmd)))
        (should (member "--priority-min" line))
        (should (member "1" line))
        (should (member "--priority-max" line))
        (should (member "3" line))))))

(ert-deftest beads-command-test-list-assignee-filter ()
  "Test list command with assignee filter."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list :assignee "alice")))
      (let ((line (beads-command-line cmd)))
        (should (member "--assignee" line))
        (should (member "alice" line))))))

(ert-deftest beads-command-test-list-labels-filter ()
  "Test list command with labels filter."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list :label '("bug" "urgent"))))
      (let ((line (beads-command-line cmd)))
        ;; Should have two --label flags
        (should (= 2 (cl-count "--label" line :test #'equal)))
        (should (member "bug" line))
        (should (member "urgent" line))))))

(ert-deftest beads-command-test-list-label-any-filter ()
  "Test list command with label-any filter."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list :label-any '("bug" "feature"))))
      (let ((line (beads-command-line cmd)))
        (should (= 2 (cl-count "--label-any" line :test #'equal)))
        (should (member "bug" line))
        (should (member "feature" line))))))

(ert-deftest beads-command-test-list-date-filters ()
  "Test list command with date filters."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list
                :created-after "2025-01-01"
                :created-before "2025-12-31"
                :updated-after "2025-01-15"
                :updated-before "2025-06-30")))
      (let ((line (beads-command-line cmd)))
        (should (member "--created-after" line))
        (should (member "--created-before" line))
        (should (member "--updated-after" line))
        (should (member "--updated-before" line))))))

(ert-deftest beads-command-test-list-text-filters ()
  "Test list command with text filters."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list
                :title-contains "bug"
                :desc-contains "error"
                :notes-contains "workaround")))
      (let ((line (beads-command-line cmd)))
        (should (member "--title-contains" line))
        (should (member "bug" line))
        (should (member "--desc-contains" line))
        (should (member "error" line))
        (should (member "--notes-contains" line))
        (should (member "workaround" line))))))

(ert-deftest beads-command-test-list-boolean-filters ()
  "Test list command with boolean filters."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list
                :all t
                :empty-description t
                :long t
                :no-assignee t
                :no-labels t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--all" line))
        (should (member "--empty-description" line))
        (should (member "--long" line))
        (should (member "--no-assignee" line))
        (should (member "--no-labels" line))))))

(ert-deftest beads-command-test-list-limit ()
  "Test list command with limit."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list :limit 10)))
      (let ((line (beads-command-line cmd)))
        (should (member "--limit" line))
        (should (member "10" line))))))

(ert-deftest beads-command-test-list-type-filter ()
  "Test list command with type filter."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-list :issue-type "bug")))
      (let ((line (beads-command-line cmd)))
        (should (member "--type" line))
        (should (member "bug" line))))))

(ert-deftest beads-command-test-list-validate-priority-conflict ()
  "Test list command validation rejects priority with priority-min/max."
  (let ((cmd (beads-command-list :priority 1 :priority-min 0)))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Cannot use --priority with" error)))))

(ert-deftest beads-command-test-list-validate-assignee-conflict ()
  "Test list command validation rejects --assignee with --no-assignee."
  (let ((cmd (beads-command-list :assignee "alice" :no-assignee t)))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Cannot use both" error)))))

(ert-deftest beads-command-test-list-validate-labels-conflict ()
  "Test list command validation rejects --label with --no-labels."
  (let ((cmd (beads-command-list :label '("bug") :no-labels t)))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Cannot use" error)))))

(ert-deftest beads-command-test-list-validate-priority-range ()
  "Test list command validation rejects invalid priority."
  (let ((cmd (beads-command-list :priority 5)))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Priority must be between" error)))))

(ert-deftest beads-command-test-list-validate-valid ()
  "Test list command validation passes for valid options."
  (let ((cmd (beads-command-list :status "open" :priority 2)))
    (should (null (beads-command-validate cmd)))))

;;; Tests for beads-command-create

(ert-deftest beads-command-test-create-basic ()
  "Test create command with title."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-create :title "Test Issue")))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "create" line))
        (should (member "Test Issue" line))))))

(ert-deftest beads-command-test-create-with-options ()
  "Test create command with various options."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-create
                :title "Test Issue"
                :description "Description text"
                :assignee "alice"
                :priority 1
                :issue-type "bug")))
      (let ((line (beads-command-line cmd)))
        (should (member "Test Issue" line))
        (should (member "--description" line))
        (should (member "Description text" line))
        (should (member "--assignee" line))
        (should (member "alice" line))
        (should (member "--priority" line))
        (should (member "1" line))
        (should (member "--type" line))
        (should (member "bug" line))))))

(ert-deftest beads-command-test-create-with-labels ()
  "Test create command with labels."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-create
                :title "Test"
                :labels '("bug" "urgent"))))
      (let ((line (beads-command-line cmd)))
        (should (member "--labels" line))
        (should (member "bug,urgent" line))))))

(ert-deftest beads-command-test-create-with-deps ()
  "Test create command with dependencies."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-create
                :title "Test"
                :deps '("blocks:bd-1" "discovered-from:bd-2"))))
      (let ((line (beads-command-line cmd)))
        (should (member "--deps" line))
        (should (member "blocks:bd-1,discovered-from:bd-2" line))))))

(ert-deftest beads-command-test-create-with-file ()
  "Test create command with file option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-create :file "/tmp/issues.md")))
      (let ((line (beads-command-line cmd)))
        (should (member "--file" line))
        (should (member "/tmp/issues.md" line))))))

(ert-deftest beads-command-test-create-with-force ()
  "Test create command with force option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-create :title "Test" :force t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--force" line))))))

(ert-deftest beads-command-test-create-with-template ()
  "Test create command with template option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-create :title "Test" :from-template "bug")))
      (let ((line (beads-command-line cmd)))
        (should (member "--from-template" line))
        (should (member "bug" line))))))

(ert-deftest beads-command-test-create-with-parent ()
  "Test create command with parent option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-create :title "Test" :parent "bd-epic-1")))
      (let ((line (beads-command-line cmd)))
        (should (member "--parent" line))
        (should (member "bd-epic-1" line))))))

(ert-deftest beads-command-test-create-validate-no-title-or-file ()
  "Test create command validation requires title or file."
  (let ((cmd (beads-command-create)))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Must provide either title or --file" error)))))

(ert-deftest beads-command-test-create-validate-both-title-and-file ()
  "Test create command validation rejects both title and file."
  (let ((cmd (beads-command-create :title "Test" :file "/tmp/test.md")))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Cannot use both" error)))))

(ert-deftest beads-command-test-create-validate-empty-title ()
  "Test create command validation rejects empty title."
  (let ((cmd (beads-command-create :title "   ")))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Title cannot be empty" error)))))

(ert-deftest beads-command-test-create-validate-invalid-type ()
  "Test create command validation rejects invalid type."
  (let ((cmd (beads-command-create :title "Test" :issue-type "invalid")))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Type must be one of" error)))))

(ert-deftest beads-command-test-create-validate-invalid-priority ()
  "Test create command validation rejects invalid priority."
  (let ((cmd (beads-command-create :title "Test" :priority 10)))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Priority must be" error)))))

(ert-deftest beads-command-test-create-validate-valid ()
  "Test create command validation passes for valid options."
  (let ((cmd (beads-command-create
              :title "Valid Issue"
              :issue-type "bug"
              :priority 2)))
    (should (null (beads-command-validate cmd)))))

;;; Tests for command-line ordering

(ert-deftest beads-command-test-command-line-order ()
  "Test that command line elements are in correct order."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-init :prefix "test" :actor "alice")))
      (let ((line (beads-command-line cmd)))
        ;; First element should be executable
        (should (equal (car line) "bd"))
        ;; Second should be command name
        (should (equal (cadr line) "init"))))))

;;; Tests for parse methods

(ert-deftest beads-command-test-parse-base-returns-stdout ()
  "Test that base parse method returns stdout."
  (let ((cmd (beads-command-init)))
    (oset cmd stdout "test output")
    (oset cmd stderr "")
    (oset cmd exit-code 0)
    (should (equal (beads-command-parse cmd) "test output"))))

(ert-deftest beads-command-test-parse-json-parses-array ()
  "Test that JSON parse method parses arrays."
  (let ((cmd (beads-command-list :json t)))
    (oset cmd stdout "[{\"id\":\"bd-1\",\"title\":\"Test\"}]")
    (oset cmd stderr "")
    (oset cmd exit-code 0)
    ;; The list command converts to beads-issue instances
    (let ((result (beads-command-parse cmd)))
      (should (listp result))
      (should (= 1 (length result)))
      (should (beads-issue-p (car result))))))

(ert-deftest beads-command-test-parse-json-disabled ()
  "Test that JSON parse returns raw stdout when json is nil."
  (let ((cmd (beads-command-list :json nil)))
    (oset cmd stdout "[{\"id\":\"bd-1\"}]")
    (oset cmd stderr "")
    (oset cmd exit-code 0)
    (should (equal (beads-command-parse cmd) "[{\"id\":\"bd-1\"}]"))))

(ert-deftest beads-command-test-parse-export-uses-stderr ()
  "Test that export parse reads from stderr."
  (let ((cmd (beads-command-export :json t)))
    (oset cmd stdout "JSONL output here")
    (oset cmd stderr "{\"exported\":5,\"skipped\":0}")
    (oset cmd exit-code 0)
    (let ((result (beads-command-parse cmd)))
      (should (listp result))
      (should (= 5 (alist-get 'exported result))))))

(ert-deftest beads-command-test-parse-import-uses-stderr ()
  "Test that import parse reads from stderr."
  (let ((cmd (beads-command-import :json t)))
    (oset cmd stdout "Import output")
    (oset cmd stderr "{\"imported\":3,\"updated\":1}")
    (oset cmd exit-code 0)
    (let ((result (beads-command-parse cmd)))
      (should (listp result))
      (should (= 3 (alist-get 'imported result))))))

;;; Tests for async execution setup

(ert-deftest beads-command-test-async-validation-error ()
  "Test that async execute signals validation error immediately."
  (let ((cmd (beads-command-create)))  ; No title = invalid
    (should-error
     (beads-command-execute-async cmd)
     :type 'beads-validation-error)))

;;; Tests for command slot access

(ert-deftest beads-command-test-slots-access ()
  "Test accessing various command slots."
  (let ((cmd (beads-command-list
              :status "open"
              :priority 1
              :assignee "alice"
              :label '("bug"))))
    (should (equal (oref cmd status) "open"))
    (should (= (oref cmd priority) 1))
    (should (equal (oref cmd assignee) "alice"))
    (should (equal (oref cmd label) '("bug")))))

(ert-deftest beads-command-test-result-slots ()
  "Test result slots after mock execution."
  (let ((cmd (beads-command-init)))
    (oset cmd exit-code 0)
    (oset cmd stdout "success output")
    (oset cmd stderr "")
    (oset cmd data "parsed data")
    (should (= (oref cmd exit-code) 0))
    (should (equal (oref cmd stdout) "success output"))
    (should (equal (oref cmd stderr) ""))
    (should (equal (oref cmd data) "parsed data"))))

;;; Tests for beads-command-show

(ert-deftest beads-command-test-show-basic ()
  "Test show command with single issue ID."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-show :issue-ids '("bd-42"))))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "show" line))
        (should (member "bd-42" line))))))

(ert-deftest beads-command-test-show-multiple-ids ()
  "Test show command with multiple issue IDs."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-show :issue-ids '("bd-1" "bd-2" "bd-3"))))
      (let ((line (beads-command-line cmd)))
        (should (member "bd-1" line))
        (should (member "bd-2" line))
        (should (member "bd-3" line))))))

(ert-deftest beads-command-test-show-validate-no-ids ()
  "Test show command validation requires at least one ID."
  (let ((cmd (beads-command-show)))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Must provide at least one issue ID" error)))))

(ert-deftest beads-command-test-show-validate-valid ()
  "Test show command validation passes with issue ID."
  (let ((cmd (beads-command-show :issue-ids '("bd-42"))))
    (should (null (beads-command-validate cmd)))))

;;; Tests for beads-command-update

(ert-deftest beads-command-test-update-basic ()
  "Test update command with issue ID and status."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-update
                :issue-ids '("bd-42")
                :status "in_progress")))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "update" line))
        (should (member "bd-42" line))
        (should (member "--status" line))
        (should (member "in_progress" line))))))

(ert-deftest beads-command-test-update-multiple-fields ()
  "Test update command with multiple fields."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-update
                :issue-ids '("bd-42")
                :title "New Title"
                :description "New Description"
                :assignee "bob"
                :priority 1)))
      (let ((line (beads-command-line cmd)))
        (should (member "--title" line))
        (should (member "New Title" line))
        (should (member "--description" line))
        (should (member "--assignee" line))
        (should (member "--priority" line))))))

(ert-deftest beads-command-test-update-validate-no-ids ()
  "Test update command validation requires at least one ID."
  (let ((cmd (beads-command-update :status "open")))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Must provide at least one issue ID" error)))))

(ert-deftest beads-command-test-update-validate-no-fields ()
  "Test update command validation requires at least one field."
  (let ((cmd (beads-command-update :issue-ids '("bd-42"))))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Must provide at least one field" error)))))

(ert-deftest beads-command-test-update-validate-valid ()
  "Test update command validation passes with ID and field."
  (let ((cmd (beads-command-update :issue-ids '("bd-42") :status "open")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for beads-command-close

(ert-deftest beads-command-test-close-basic ()
  "Test close command with issue ID and reason."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-close
                :issue-ids '("bd-42")
                :reason "Completed")))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "close" line))
        (should (member "bd-42" line))
        (should (member "--reason" line))
        (should (member "Completed" line))))))

(ert-deftest beads-command-test-close-multiple-ids ()
  "Test close command with multiple issue IDs."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-close
                :issue-ids '("bd-1" "bd-2")
                :reason "Batch close")))
      (let ((line (beads-command-line cmd)))
        (should (member "bd-1" line))
        (should (member "bd-2" line))))))

(ert-deftest beads-command-test-close-validate-no-ids ()
  "Test close command validation requires at least one ID."
  (let ((cmd (beads-command-close :reason "Done")))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Must provide at least one issue ID" error)))))

(ert-deftest beads-command-test-close-validate-no-reason ()
  "Test close command validation requires reason."
  (let ((cmd (beads-command-close :issue-ids '("bd-42"))))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Must provide a reason" error)))))

;;; Tests for beads-command-reopen

(ert-deftest beads-command-test-reopen-basic ()
  "Test reopen command with issue ID."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-reopen :issue-ids '("bd-42"))))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "reopen" line))
        (should (member "bd-42" line))))))

(ert-deftest beads-command-test-reopen-with-reason ()
  "Test reopen command with reason."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-reopen
                :issue-ids '("bd-42")
                :reason "Needs more work")))
      (let ((line (beads-command-line cmd)))
        (should (member "--reason" line))
        (should (member "Needs more work" line))))))

(ert-deftest beads-command-test-reopen-validate-no-ids ()
  "Test reopen command validation requires at least one ID."
  (let ((cmd (beads-command-reopen)))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error))
      (should (string-match-p "Must provide at least one issue ID" error)))))

;;; Tests for beads-command-ready

(ert-deftest beads-command-test-ready-basic ()
  "Test ready command with no options."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-ready)))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "ready" line))))))

(ert-deftest beads-command-test-ready-with-limit ()
  "Test ready command with limit."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-ready :limit 5)))
      (let ((line (beads-command-line cmd)))
        (should (member "--limit" line))
        (should (member "5" line))))))

(ert-deftest beads-command-test-ready-validate ()
  "Test ready command validation always passes."
  (let ((cmd (beads-command-ready)))
    (should (null (beads-command-validate cmd)))))

;;; Tests for beads-command-blocked

(ert-deftest beads-command-test-blocked-basic ()
  "Test blocked command with no options."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-blocked)))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "blocked" line))))))

(ert-deftest beads-command-test-blocked-validate ()
  "Test blocked command validation always passes."
  (let ((cmd (beads-command-blocked)))
    (should (null (beads-command-validate cmd)))))

;;; Tests for beads-command-stats

(ert-deftest beads-command-test-stats-basic ()
  "Test stats command with no options."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-stats)))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "stats" line))))))

(ert-deftest beads-command-test-stats-validate ()
  "Test stats command validation always passes."
  (let ((cmd (beads-command-stats)))
    (should (null (beads-command-validate cmd)))))


;;; Tests for beads-command-dep-add

(ert-deftest beads-command-test-dep-add-basic ()
  "Test dep add command."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-dep-add
                :issue-id "bd-1"
                :dep-type "blocks"
                :depends-on-id "bd-2")))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "dep" line))
        (should (member "add" line))))))

(ert-deftest beads-command-test-dep-add-validate-missing-issue-id ()
  "Test dep add validation requires issue-id."
  (let ((cmd (beads-command-dep-add :dep-type "blocks" :depends-on-id "bd-2")))
    (let ((error (beads-command-validate cmd)))
      (should (stringp error)))))

;;; Tests for beads-command-dep-remove

(ert-deftest beads-command-test-dep-remove-basic ()
  "Test dep remove command."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-dep-remove
                :issue-id "bd-1"
                :depends-on-id "bd-2")))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "dep" line))
        (should (member "remove" line))))))

;;; Tests for beads-command-dep-tree

(ert-deftest beads-command-test-dep-tree-basic ()
  "Test dep tree command."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-dep-tree :issue-id "bd-42")))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "dep" line))
        (should (member "tree" line))
        (should (member "bd-42" line))))))

(ert-deftest beads-command-test-dep-tree-with-max-depth ()
  "Test dep tree command with max-depth option."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-dep-tree :issue-id "bd-42" :max-depth 3)))
      (let ((line (beads-command-line cmd)))
        (should (member "--max-depth" line))
        (should (member "3" line))))))

;;; Tests for beads-command-epic-close-eligible

(ert-deftest beads-command-test-epic-close-eligible-basic ()
  "Test epic close-eligible command."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-epic-close-eligible)))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "epic" line))
        (should (member "close-eligible" line))))))

(ert-deftest beads-command-test-epic-close-eligible-dry-run ()
  "Test epic close-eligible command with dry-run."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-epic-close-eligible :dry-run t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--dry-run" line))))))

;;; Tests for beads-command-epic-status

(ert-deftest beads-command-test-epic-status-basic ()
  "Test epic status command."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-epic-status)))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "epic" line))
        (should (member "status" line))))))

(ert-deftest beads-command-test-epic-status-eligible-only ()
  "Test epic status command with eligible-only."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-epic-status :eligible-only t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--eligible-only" line))))))

;;; Tests for beads-command-label-add

(ert-deftest beads-command-test-label-add-basic ()
  "Test label add command."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-label-add
                :issue-ids '("bd-42")
                :label "bug")))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "label" line))
        (should (member "add" line))))))

;;; Tests for beads-command-label-remove

(ert-deftest beads-command-test-label-remove-basic ()
  "Test label remove command."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-label-remove
                :issue-ids '("bd-42")
                :label "obsolete")))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "label" line))
        (should (member "remove" line))))))

;;; Tests for beads-command-delete

(ert-deftest beads-command-test-delete-basic ()
  "Test delete command."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-delete :issue-id "bd-42")))
      (let ((line (beads-command-line cmd)))
        (should (equal (car line) "bd"))
        (should (member "delete" line))
        (should (member "bd-42" line))))))

(ert-deftest beads-command-test-delete-force ()
  "Test delete command with force."
  (beads-command-test--with-executable "bd"
    (let ((cmd (beads-command-delete :issue-id "bd-42" :force t)))
      (let ((line (beads-command-line cmd)))
        (should (member "--force" line))))))

;;; Additional validation tests

(ert-deftest beads-command-test-update-validate-no-issue ()
  "Test update validation fails without issue."
  (beads-command-test--with-executable "bd"
    (let* ((cmd (beads-command-update :status "open"))
           (errors (beads-command-validate cmd)))
      (should (stringp errors))
      (should (string-match-p "issue" (downcase errors))))))

(ert-deftest beads-command-test-close-validate-no-issue ()
  "Test close validation fails without issue."
  (beads-command-test--with-executable "bd"
    (let* ((cmd (beads-command-close))
           (errors (beads-command-validate cmd)))
      (should (stringp errors))
      (should (string-match-p "issue" (downcase errors))))))

(ert-deftest beads-command-test-create-validate-no-title ()
  "Test create validation fails without title."
  (beads-command-test--with-executable "bd"
    (let* ((cmd (beads-command-create))
           (errors (beads-command-validate cmd)))
      (should (stringp errors))
      (should (string-match-p "title" (downcase errors))))))

(ert-deftest beads-command-test-create-validate-success ()
  "Test create validation succeeds with valid input."
  (beads-command-test--with-executable "bd"
    (let* ((cmd (beads-command-create
                 :title "Test Issue"
                 :priority 2
                 :issue-type "task"))
           (errors (beads-command-validate cmd)))
      (should (null errors)))))

;;; Priority Conversion Tests

(ert-deftest beads-command-test-priority-to-integer-nil ()
  "Test priority-to-integer with nil input."
  (should (null (beads-command--priority-to-integer nil))))

(ert-deftest beads-command-test-priority-to-integer-int ()
  "Test priority-to-integer with integer input."
  (should (= 2 (beads-command--priority-to-integer 2)))
  (should (= 0 (beads-command--priority-to-integer 0)))
  (should (= 4 (beads-command--priority-to-integer 4))))

(ert-deftest beads-command-test-priority-to-integer-string ()
  "Test priority-to-integer with string input."
  (should (= 2 (beads-command--priority-to-integer "2")))
  (should (= 0 (beads-command--priority-to-integer "0")))
  (should (= 3 (beads-command--priority-to-integer "3"))))

(ert-deftest beads-command-test-priority-to-integer-pformat ()
  "Test priority-to-integer with P-prefixed string input."
  (should (= 2 (beads-command--priority-to-integer "P2")))
  (should (= 0 (beads-command--priority-to-integer "P0")))
  (should (= 4 (beads-command--priority-to-integer "P4"))))

(ert-deftest beads-command-test-priority-to-integer-invalid ()
  "Test priority-to-integer with invalid input signals error."
  (should-error (beads-command--priority-to-integer 'symbol) :type 'error)
  (should-error (beads-command--priority-to-integer '(list)) :type 'error))

(ert-deftest beads-command-test-priority-to-string-nil ()
  "Test priority-to-string with nil input."
  (should (null (beads-command--priority-to-string nil))))

(ert-deftest beads-command-test-priority-to-string-int ()
  "Test priority-to-string with integer input."
  (should (equal "2" (beads-command--priority-to-string 2)))
  (should (equal "0" (beads-command--priority-to-string 0))))

(ert-deftest beads-command-test-priority-to-string-string ()
  "Test priority-to-string with string input."
  (should (equal "2" (beads-command--priority-to-string "2"))))

(ert-deftest beads-command-test-priority-to-string-invalid ()
  "Test priority-to-string with invalid input signals error."
  (should-error (beads-command--priority-to-string 'symbol) :type 'error))

;;; Dep Type Validation Tests

(ert-deftest beads-command-test-dep-add-validate-type ()
  "Test dep-add validates dependency type."
  (beads-command-test--with-executable "bd"
    (let* ((cmd (beads-command-dep-add
                 :issue-id "bd-1"
                 :depends-on-id "bd-2"
                 :dep-type "invalid-type"))
           (errors (beads-command-validate cmd)))
      (should (stringp errors))
      (should (string-match-p "Type" errors)))))

(ert-deftest beads-command-test-dep-add-validate-valid-types ()
  "Test dep-add accepts valid dependency types."
  (beads-command-test--with-executable "bd"
    (dolist (valid-type '("blocks" "related" "parent-child"))
      (let* ((cmd (beads-command-dep-add
                   :issue-id "bd-1"
                   :depends-on-id "bd-2"
                   :dep-type valid-type))
             (errors (beads-command-validate cmd)))
        (should (null errors))))))

;;; Label Command Tests

(ert-deftest beads-command-test-label-list-exists ()
  "Test that beads-command-label-list class exists."
  (should (fboundp 'beads-command-label-list)))

(ert-deftest beads-command-test-label-list-command-line ()
  "Test label-list command line."
  (beads-command-test--with-executable "bd"
    (let* ((cmd (beads-command-label-list :issue-id "bd-42"))
           (line (beads-command-line cmd)))
      (should (member "label" line))
      (should (member "list" line)))))

(provide 'beads-command-test)
;;; beads-command-test.el ends here
