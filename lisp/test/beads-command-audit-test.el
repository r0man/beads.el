;;; beads-command-audit-test.el --- Tests for beads-command-audit -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-audit command classes.

;;; Code:

(require 'ert)
(require 'beads-command-audit)

;;; Unit Tests: beads-command-audit-record command-line

(ert-deftest beads-command-audit-record-test-command-line-basic ()
  "Unit test: audit record builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-audit-record))
         (args (beads-command-line cmd)))
    (should (member "audit" args))
    (should (member "record" args))))

(ert-deftest beads-command-audit-record-test-command-line-kind ()
  "Unit test: audit record includes --kind option."
  :tags '(:unit)
  (let* ((cmd (beads-command-audit-record :kind "llm_call"))
         (args (beads-command-line cmd)))
    (should (member "--kind" args))
    (should (member "llm_call" args))))

(ert-deftest beads-command-audit-record-test-command-line-issue-id ()
  "Unit test: audit record includes --issue-id option."
  :tags '(:unit)
  (let* ((cmd (beads-command-audit-record :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "--issue-id" args))
    (should (member "bd-1" args))))

(ert-deftest beads-command-audit-record-test-command-line-model ()
  "Unit test: audit record includes --model option."
  :tags '(:unit)
  (let* ((cmd (beads-command-audit-record :model "gpt-4"))
         (args (beads-command-line cmd)))
    (should (member "--model" args))
    (should (member "gpt-4" args))))

(ert-deftest beads-command-audit-record-test-command-line-tool-name ()
  "Unit test: audit record includes --tool-name option."
  :tags '(:unit)
  (let* ((cmd (beads-command-audit-record :tool-name "bash"))
         (args (beads-command-line cmd)))
    (should (member "--tool-name" args))
    (should (member "bash" args))))

(ert-deftest beads-command-audit-record-test-command-line-stdin ()
  "Unit test: audit record includes --stdin option."
  :tags '(:unit)
  (let* ((cmd (beads-command-audit-record :stdin t))
         (args (beads-command-line cmd)))
    (should (member "--stdin" args))))

;;; Unit Tests: beads-command-audit-label command-line

(ert-deftest beads-command-audit-label-test-command-line-basic ()
  "Unit test: audit label builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-audit-label :entry-id "entry-1" :label "good"))
         (args (beads-command-line cmd)))
    (should (member "audit" args))
    (should (member "label" args))
    (should (member "entry-1" args))
    (should (member "--label" args))
    (should (member "good" args))))

(ert-deftest beads-command-audit-label-test-command-line-reason ()
  "Unit test: audit label includes --reason option."
  :tags '(:unit)
  (let* ((cmd (beads-command-audit-label :entry-id "entry-1"
                                          :label "bad"
                                          :reason "incorrect output"))
         (args (beads-command-line cmd)))
    (should (member "--reason" args))
    (should (member "incorrect output" args))))

(ert-deftest beads-command-audit-label-test-validation-missing-entry-id ()
  "Unit test: audit label validation fails without entry-id."
  :tags '(:unit)
  (let ((cmd (beads-command-audit-label :label "good")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-audit-label-test-validation-missing-label ()
  "Unit test: audit label validation fails without label."
  :tags '(:unit)
  (let ((cmd (beads-command-audit-label :entry-id "entry-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-audit-label-test-validation-success ()
  "Unit test: audit label validation succeeds with required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-audit-label :entry-id "entry-1" :label "good")))
    (should (null (beads-command-validate cmd)))))

(provide 'beads-command-audit-test)
;;; beads-command-audit-test.el ends here
