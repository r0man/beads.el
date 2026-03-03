;;; beads-command-blocked-test.el --- Tests for beads-command-blocked -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for beads-command-blocked.el covering:
;; - Parse method (JSON to beads-blocked-issue instances)
;; - Command line building
;; - Subcommand

;;; Code:

(require 'ert)
(require 'json)
(require 'beads-command-blocked)

;;; Test Fixtures

(defvar beads-command-blocked-test--sample-json
  (vector '((id . "bd-1")
            (title . "Blocked Issue 1")
            (status . "open")
            (priority . 1)
            (issue_type . "bug")
            (blocked_by . (((id . "bd-10") (title . "Blocker"))))))
  "Sample blocked issues JSON for testing.")

;;; Parse Method Tests

(ert-deftest beads-command-blocked-test-parse-json-issues ()
  "Test parse method converts JSON to beads-blocked-issue instances."
  (let* ((cmd (beads-command-blocked :json t))
         (json-string (json-encode beads-command-blocked-test--sample-json))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout json-string
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= (length result) 1))
      (should (beads-blocked-issue-p (car result)))
      (should (string= (oref (car result) id) "bd-1")))))

(ert-deftest beads-command-blocked-test-parse-json-empty ()
  "Test parse method with empty JSON array."
  (let* ((cmd (beads-command-blocked :json t))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "[]"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (listp result))
      (should (= (length result) 0)))))

(ert-deftest beads-command-blocked-test-parse-json-disabled ()
  "Test parse method with :json nil falls back to raw stdout."
  (let* ((cmd (beads-command-blocked :json nil))
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout "Blocked issues listed"
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
      (should (stringp result)))))

(ert-deftest beads-command-blocked-test-parse-json-error ()
  "Test parse method signals error on invalid data."
  (let* ((cmd (beads-command-blocked :json t))
         ;; Provide data with non-alist elements that cause errors
         (json-string "[42]")
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout json-string
                :stderr "")))
    (should-error (beads-command-parse cmd exec)
                  :type 'beads-json-parse-error)))

;;; Command Line Tests

(ert-deftest beads-command-blocked-test-command-line-basic ()
  "Test basic blocked command line."
  (let* ((cmd (beads-command-blocked :json t))
         (args (beads-command-line cmd)))
    (should (member "blocked" args))
    (should (member "--json" args))))

(ert-deftest beads-command-blocked-test-subcommand ()
  "Test subcommand returns correct value."
  (let ((cmd (beads-command-blocked)))
    (should (string= (beads-command-subcommand cmd) "blocked"))))

(provide 'beads-command-blocked-test)
;;; beads-command-blocked-test.el ends here
