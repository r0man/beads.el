;;; beads-command-provenance-test.el --- Tests for beads-command-provenance -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT unit tests for beads-command-provenance.el (bd 1.3.x `bd
;; provenance' group, workflow be-j2b phase 3).  Covers class
;; definitions, CLI serialization, validation, JSON parsing, and the
;; router transient.

;;; Code:

(require 'ert)
(require 'beads-command-provenance)

;;; Class existence

(ert-deftest beads-provenance-test-class-exists ()
  "Test that all three provenance classes are defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-provenance-log))
  (should (cl-find-class 'beads-command-provenance-by-ref))
  (should (cl-find-class 'beads-command-provenance-record)))

;;; Subcommand derivation

(ert-deftest beads-provenance-test-subcommands ()
  "Each class targets its `provenance' subcommand."
  :tags '(:unit)
  (should (equal (beads-command-subcommand
                  (beads-command-provenance-log :issue-id "bd-1"))
                 "provenance log"))
  (should (equal (beads-command-subcommand
                  (beads-command-provenance-by-ref :ref "abc"))
                 "provenance by-ref"))
  (should (equal (beads-command-subcommand
                  (beads-command-provenance-record
                   :issue "bd-1" :kind "cut" :source "probe"))
                 "provenance record")))

;;; Command-line serialization

(ert-deftest beads-provenance-test-log-command-line ()
  "Test `provenance log bd-1 --kind cut' serialization."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-provenance-log :issue-id "bd-1" :kind "cut"))))
    (should (member "provenance" args))
    (should (member "log" args))
    (should (member "bd-1" args))
    (should (member "--kind" args))
    (should (member "cut" args))))

(ert-deftest beads-provenance-test-by-ref-command-line ()
  "Test `provenance by-ref <ref>' serialization."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-provenance-by-ref :ref "deadbeef"))))
    (should (member "provenance" args))
    (should (member "by-ref" args))
    (should (member "deadbeef" args))))

(ert-deftest beads-provenance-test-record-command-line ()
  "Test `provenance record' full serialization."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-provenance-record
                :issue "bd-7" :kind "handoff" :source "git-hook"
                :ref "abc123" :ref-kind "git-sha" :actor "me"
                :at "2026-01-01T00:00:00Z" :payload "{\"a\":1}"))))
    (should (member "--issue" args))
    (should (member "bd-7" args))
    (should (member "--kind" args))
    (should (member "handoff" args))
    (should (member "--source" args))
    (should (member "git-hook" args))
    (should (member "--ref" args))
    (should (member "abc123" args))
    (should (member "--ref-kind" args))
    (should (member "git-sha" args))
    (should (member "--actor" args))
    (should (member "--at" args))
    (should (member "--payload" args))))

;;; Validation

(ert-deftest beads-provenance-test-log-validate-requires-issue ()
  "Log without an issue id fails validation."
  :tags '(:unit)
  (should (beads-command-validate (beads-command-provenance-log))))

(ert-deftest beads-provenance-test-record-validate-requires-core-flags ()
  "Record without --issue/--kind/--source fails validation."
  :tags '(:unit)
  (should (beads-command-validate
           (beads-command-provenance-record :issue "bd-1")))
  (should (beads-command-validate
           (beads-command-provenance-record :kind "cut")))
  (should (beads-command-validate
           (beads-command-provenance-record :source "probe"))))

(ert-deftest beads-provenance-test-record-validate-success ()
  "Record with the three required flags validates."
  :tags '(:unit)
  (should (null (beads-command-validate
                 (beads-command-provenance-record
                  :issue "bd-1" :kind "cut" :source "probe")))))

(ert-deftest beads-provenance-test-by-ref-validate-requires-ref ()
  "By-ref without a ref fails validation."
  :tags '(:unit)
  (should (beads-command-validate (beads-command-provenance-by-ref))))

;;; JSON parsing

(ert-deftest beads-provenance-test-log-parse-result ()
  "Log parses a list of provenance events."
  :tags '(:unit)
  (let* ((cmd (beads-command-provenance-log :json t :issue-id "bd-1"))
         (result (beads-command-parse
                  cmd
                  "[{\"id\":\"abc\",\"issue_id\":\"bd-1\",\"kind\":\"cut\",\"ref\":\"r1\",\"ref_kind\":\"git-sha\",\"source\":\"git-hook\",\"created_at\":\"2026-01-01T00:00:00Z\"}]")))
    (should (= (length result) 1))
    (should (beads-provenance-event-p (car result)))
    (let ((e (car result)))
      (should (equal (oref e id) "abc"))
      (should (equal (oref e issue-id) "bd-1"))
      (should (equal (oref e kind) "cut"))
      (should (equal (oref e ref) "r1"))
      (should (equal (oref e ref-kind) "git-sha"))
      (should (equal (oref e source) "git-hook"))
      (should (equal (oref e created-at) "2026-01-01T00:00:00Z")))))

(ert-deftest beads-provenance-test-record-parse-result ()
  "Record parses the idempotent-record envelope."
  :tags '(:unit)
  (let* ((cmd (beads-command-provenance-record
               :json t :issue "bd-1" :kind "cut" :source "probe"))
         (result (beads-command-parse
                  cmd
                  "{\"id\":\"xyz\",\"inserted\":true,\"issue_id\":\"bd-1\",\"kind\":\"cut\",\"schema_version\":1}")))
    (should (beads-provenance-record-result-p result))
    (should (equal (oref result id) "xyz"))
    (should (oref result inserted))
    (should (equal (oref result issue-id) "bd-1"))
    (should (equal (oref result kind) "cut"))))

;;; Transients

(ert-deftest beads-provenance-test-parent-transient-defined ()
  "The `bd provenance' router parent transient exists (policy: no class)."
  :tags '(:unit)
  (should (fboundp 'beads-provenance))
  (should (get 'beads-provenance 'transient--prefix)))

(ert-deftest beads-provenance-test-leaf-transients-defined ()
  "The three leaf command transients are defined."
  :tags '(:unit)
  (should (fboundp 'beads-provenance-log))
  (should (fboundp 'beads-provenance-by-ref))
  (should (fboundp 'beads-provenance-record)))

(provide 'beads-command-provenance-test)
;;; beads-command-provenance-test.el ends here
