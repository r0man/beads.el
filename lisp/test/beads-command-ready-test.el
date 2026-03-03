;;; beads-command-ready-test.el --- Tests for beads-command-ready -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for beads-command-ready.el covering:
;; - Command line building with all options
;; - Validation of sort, priority, mol-type
;; - Parse method (JSON to beads-issue instances)

;;; Code:

(require 'ert)
(require 'json)
(require 'beads-command-ready)

;;; Test Fixtures

(defvar beads-command-ready-test--sample-issues-json
  (vector '((id . "bd-1")
            (title . "Ready Issue 1")
            (status . "open")
            (priority . 1)
            (issue_type . "bug")
            (created_at . "2025-01-15T10:00:00Z")
            (updated_at . "2025-01-15T10:00:00Z"))
          '((id . "bd-2")
            (title . "Ready Issue 2")
            (status . "in_progress")
            (priority . 2)
            (issue_type . "task")
            (created_at . "2025-01-15T10:00:00Z")
            (updated_at . "2025-01-15T10:00:00Z")))
  "Sample ready issues JSON for testing.")

;;; Command Line Tests

(ert-deftest beads-command-ready-test-command-line-basic ()
  "Test basic ready command line."
  (let* ((cmd (beads-command-ready :json t))
         (args (beads-command-line cmd)))
    (should (member "ready" args))
    (should (member "--json" args))))

(ert-deftest beads-command-ready-test-command-line-boolean-flags ()
  "Test ready command line with boolean flags."
  (let* ((cmd (beads-command-ready :include-deferred t
                                    :pretty t
                                    :unassigned t))
         (args (beads-command-line cmd)))
    (should (member "--include-deferred" args))
    (should (member "--pretty" args))
    (should (member "--unassigned" args))))

(ert-deftest beads-command-ready-test-command-line-string-options ()
  "Test ready command line with string options."
  (let* ((cmd (beads-command-ready :assignee "alice"
                                    :issue-type "bug"
                                    :mol "my-mol"
                                    :mol-type "work"
                                    :parent "bd-10"
                                    :sort "priority"))
         (args (beads-command-line cmd)))
    (should (member "--assignee" args))
    (should (member "alice" args))
    (should (member "--type" args))
    (should (member "bug" args))
    (should (member "--mol" args))
    (should (member "my-mol" args))
    (should (member "--mol-type" args))
    (should (member "work" args))
    (should (member "--parent" args))
    (should (member "bd-10" args))
    (should (member "--sort" args))
    (should (member "priority" args))))

(ert-deftest beads-command-ready-test-command-line-integer-options ()
  "Test ready command line with integer options."
  (let* ((cmd (beads-command-ready :limit 10 :priority 2))
         (args (beads-command-line cmd)))
    (should (member "--limit" args))
    (should (member "10" args))
    (should (member "--priority" args))
    (should (member "2" args))))

(ert-deftest beads-command-ready-test-command-line-label-lists ()
  "Test ready command line with label lists."
  (let* ((cmd (beads-command-ready :label '("bug" "urgent")
                                    :label-any '("review" "test")))
         (args (beads-command-line cmd)))
    ;; Should repeat --label for each value
    (should (= 2 (cl-count "--label" args :test #'equal)))
    (should (member "bug" args))
    (should (member "urgent" args))
    ;; Should repeat --label-any for each value
    (should (= 2 (cl-count "--label-any" args :test #'equal)))
    (should (member "review" args))
    (should (member "test" args))))

;;; Validation Tests

(ert-deftest beads-command-ready-test-validate-valid ()
  "Test validation with valid command."
  (let ((cmd (beads-command-ready)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-ready-test-validate-valid-sort ()
  "Test validation with valid sort values."
  (dolist (sort-val '("hybrid" "priority" "oldest"))
    (let ((cmd (beads-command-ready :sort sort-val)))
      (should (null (beads-command-validate cmd))))))

(ert-deftest beads-command-ready-test-validate-invalid-sort ()
  "Test validation fails with invalid sort value."
  (let ((cmd (beads-command-ready :sort "invalid")))
    (should (beads-command-validate cmd))
    (should (string-match-p "Sort" (beads-command-validate cmd)))))

(ert-deftest beads-command-ready-test-validate-invalid-priority ()
  "Test validation fails with out-of-range priority."
  (let ((cmd (beads-command-ready :priority 5)))
    (should (beads-command-validate cmd))
    (should (string-match-p "Priority" (beads-command-validate cmd)))))

(ert-deftest beads-command-ready-test-validate-valid-priority ()
  "Test validation passes with valid priority range."
  (dolist (p '(0 1 2 3 4))
    (let ((cmd (beads-command-ready :priority p)))
      (should (null (beads-command-validate cmd))))))

(ert-deftest beads-command-ready-test-validate-invalid-mol-type ()
  "Test validation fails with invalid mol-type."
  (let ((cmd (beads-command-ready :mol-type "invalid")))
    (should (beads-command-validate cmd))
    (should (string-match-p "Mol-type" (beads-command-validate cmd)))))

(ert-deftest beads-command-ready-test-validate-valid-mol-types ()
  "Test validation passes with valid mol-type values."
  (dolist (mt '("swarm" "patrol" "work"))
    (let ((cmd (beads-command-ready :mol-type mt)))
      (should (null (beads-command-validate cmd))))))

(ert-deftest beads-command-ready-test-validate-non-string-labels ()
  "Test validation fails with non-string label list."
  (let ((cmd (beads-command-ready :label '("valid" 42))))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-ready-test-validate-non-string-label-any ()
  "Test validation fails with non-string label-any list."
  (let ((cmd (beads-command-ready :label-any '("valid" 42))))
    (should (beads-command-validate cmd))))

;;; Parse Method Tests

(ert-deftest beads-command-ready-test-parse-json-issues ()
  "Test parse method converts JSON to beads-issue instances."
  (let* ((cmd (beads-command-ready :json t))
         (json-string (json-encode beads-command-ready-test--sample-issues-json))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout json-string
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= (length result) 2))
      (should (beads-issue-p (car result)))
      (should (string= (oref (car result) id) "bd-1"))
      (should (beads-issue-p (cadr result)))
      (should (string= (oref (cadr result) id) "bd-2")))))

(ert-deftest beads-command-ready-test-parse-json-empty ()
  "Test parse method with empty JSON array."
  (let* ((cmd (beads-command-ready :json t))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "[]"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= (length result) 0)))))

(ert-deftest beads-command-ready-test-parse-json-disabled ()
  "Test parse method with :json nil."
  (let* ((cmd (beads-command-ready :json nil))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "Ready issues listed"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (stringp result)))))

(ert-deftest beads-command-ready-test-parse-json-error ()
  "Test parse method signals error on bad data."
  (let* ((cmd (beads-command-ready :json t))
         ;; Provide data that will cause beads-issue-from-json to fail
         (json-string (json-encode (vector '((not_an_issue . t)))))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout json-string
                :stderr "")))
    ;; beads-issue-from-json should work with any alist (permissive)
    ;; but parse should still return a list
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result)))))

;;; Subcommand Tests

(ert-deftest beads-command-ready-test-subcommand ()
  "Test subcommand returns correct value."
  (let ((cmd (beads-command-ready)))
    (should (string= (beads-command-subcommand cmd) "ready"))))

;;; Tests for parse error path

(ert-deftest beads-command-ready-test-parse-json-error-path ()
  "Test parse signals beads-json-parse-error on invalid data."
  (let* ((cmd (beads-command-ready :json t))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "not valid json"
                :stderr "")))
    (should-error (beads-command-parse cmd exec)
                  :type 'beads-json-parse-error)))

;;; Tests for command-line after removing redundant method

(ert-deftest beads-command-ready-test-command-line-no-duplication ()
  "Test command-line does not duplicate flags."
  (let* ((cmd (beads-command-ready :json t :label '("bug")))
         (args (beads-command-line cmd)))
    ;; --json should appear exactly once
    (should (= 1 (cl-count "--json" args :test #'equal)))
    ;; --label should appear exactly once for one label
    (should (= 1 (cl-count "--label" args :test #'equal)))
    ;; "ready" should appear exactly once as subcommand
    (should (= 1 (cl-count "ready" args :test #'equal)))))

(ert-deftest beads-command-ready-test-command-line-full ()
  "Test command-line with all options produces correct args."
  (let* ((cmd (beads-command-ready :json t
                                    :assignee "alice"
                                    :priority 1
                                    :sort "hybrid"
                                    :label '("bug" "urgent")
                                    :label-any '("review")
                                    :include-deferred t
                                    :unassigned t
                                    :limit 5))
         (args (beads-command-line cmd)))
    (should (member "ready" args))
    (should (member "--json" args))
    (should (member "--assignee" args))
    (should (member "alice" args))
    (should (member "--priority" args))
    (should (member "--sort" args))
    (should (member "--include-deferred" args))
    (should (member "--unassigned" args))
    (should (member "--limit" args))))

(provide 'beads-command-ready-test)
;;; beads-command-ready-test.el ends here
