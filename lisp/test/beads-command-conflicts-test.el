;;; beads-command-conflicts-test.el --- Tests for beads-command-conflicts -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT unit tests for beads-command-conflicts.el (bd 1.3.x `bd
;; conflicts' group, workflow be-j2b phase 3).  Covers class
;; definitions, CLI subcommand derivation, command-line serialization,
;; validation, JSON parsing, and transient presence.

;;; Code:

(require 'ert)
(require 'beads-command-conflicts)

;;; Class existence

(ert-deftest beads-conflicts-test-class-exists ()
  "Test that all three conflicts classes are defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-conflicts-list))
  (should (cl-find-class 'beads-command-conflicts-resolve))
  (should (cl-find-class 'beads-command-conflicts-show)))

;;; Subcommand derivation

(ert-deftest beads-conflicts-test-list-subcommand ()
  "Test that the list class targets `conflicts list'."
  :tags '(:unit)
  (should (equal (beads-command-subcommand (beads-command-conflicts-list))
                 "conflicts list")))

(ert-deftest beads-conflicts-test-resolve-subcommand ()
  "Test that the resolve class targets `conflicts resolve'."
  :tags '(:unit)
  (should (equal (beads-command-subcommand
                  (beads-command-conflicts-resolve :all t :ours t))
                 "conflicts resolve")))

(ert-deftest beads-conflicts-test-show-subcommand ()
  "Test that the show class targets `conflicts show'."
  :tags '(:unit)
  (should (equal (beads-command-subcommand (beads-command-conflicts-show))
                 "conflicts show")))

;;; Command-line serialization

(ert-deftest beads-conflicts-test-list-command-line ()
  "Test `conflicts list' command line."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-conflicts-list))))
    (should (equal args '("bd" "conflicts" "list")))))

(ert-deftest beads-conflicts-test-resolve-command-line-ours-all ()
  "Test `conflicts resolve --all --ours' serialization."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-conflicts-resolve :all t :ours t))))
    (should (member "--all" args))
    (should (member "--ours" args))))

(ert-deftest beads-conflicts-test-resolve-command-line-ids-theirs ()
  "Test `conflicts resolve' with issue IDs and --theirs."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-conflicts-resolve
                :issue-ids '("bd-1" "bd-2") :theirs t))))
    (should (member "bd-1" args))
    (should (member "bd-2" args))
    (should (member "--theirs" args))))

(ert-deftest beads-conflicts-test-resolve-command-line-strategy-table ()
  "Test `conflicts resolve --strategy ours --table issues --no-commit'."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-conflicts-resolve
                :all t :strategy "ours" :table "issues" :no-commit t))))
    (should (member "--strategy" args))
    (should (member "ours" args))
    (should (member "--table" args))
    (should (member "issues" args))
    (should (member "--no-commit" args))))

(ert-deftest beads-conflicts-test-show-command-line ()
  "Test `conflicts show' serialization with --all-fields and --table."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-conflicts-show
                :issue-id "bd-9" :all-fields t :table "issues"))))
    (should (member "conflicts" args))
    (should (member "show" args))
    (should (member "bd-9" args))
    (should (member "--all-fields" args))
    (should (member "--table" args))
    (should (member "issues" args))))

(ert-deftest beads-conflicts-test-resolve-json-nil ()
  "Conflicts resolve is :json nil (plain text even under --json)."
  :tags '(:unit)
  (should (plist-member (symbol-plist 'beads-command-conflicts-resolve)
                        'beads-json)))

;;; Validation

(ert-deftest beads-conflicts-test-resolve-validate-no-selection ()
  "Resolve without IDs or --all fails validation."
  :tags '(:unit)
  (should (beads-command-validate (beads-command-conflicts-resolve))))

(ert-deftest beads-conflicts-test-resolve-validate-both-selections ()
  "Resolve with both IDs and --all fails validation."
  :tags '(:unit)
  (should (beads-command-validate
           (beads-command-conflicts-resolve
            :issue-ids '("bd-1") :all t :ours t))))

(ert-deftest beads-conflicts-test-resolve-validate-no-strategy ()
  "Resolve without a strategy fails validation."
  :tags '(:unit)
  (should (beads-command-validate
           (beads-command-conflicts-resolve :all t))))

(ert-deftest beads-conflicts-test-resolve-validate-ours-all ()
  "Resolve with --all --ours validates."
  :tags '(:unit)
  (should (null (beads-command-validate
                 (beads-command-conflicts-resolve :all t :ours t)))))

(ert-deftest beads-conflicts-test-resolve-validate-strategy-string ()
  "Resolve with --strategy theirs validates."
  :tags '(:unit)
  (should (null (beads-command-validate
                 (beads-command-conflicts-resolve
                  :all t :strategy "theirs")))))

(ert-deftest beads-conflicts-test-resolve-validate-conclude-alone ()
  "--conclude alone validates (commit an already-resolved merge)."
  :tags '(:unit)
  (should (null (beads-command-validate
                 (beads-command-conflicts-resolve :conclude t)))))

;;; JSON parsing

(ert-deftest beads-conflicts-test-list-parse-result ()
  "List parses the bd conflicts list --json envelope."
  :tags '(:unit)
  (let* ((cmd (beads-command-conflicts-list :json t))
         (result (beads-command-parse
                  cmd
                  "{\"blockers\":{\"merging\":false},\"conflicts\":0,\"schema_version\":1,\"tables\":[]}")))
    (should (beads-conflicts-list-result-p result))
    (should (= (oref result conflicts) 0))
    (should (null (oref result merging)))))

(ert-deftest beads-conflicts-test-show-parse-result ()
  "Show parses the bd conflicts show --json envelope."
  :tags '(:unit)
  (let* ((cmd (beads-command-conflicts-show :json t))
         (result (beads-command-parse
                  cmd
                  "{\"conflicts\":2,\"rows\":[{\"table\":\"issues\"}],\"schema_version\":1}")))
    (should (beads-conflicts-show-result-p result))
    (should (= (oref result conflicts) 2))
    (should (= (length (oref result rows)) 1))))

;;; Transients

(ert-deftest beads-conflicts-test-parent-transient-defined ()
  "The `bd conflicts' router parent transient exists (policy: no class)."
  :tags '(:unit)
  (should (fboundp 'beads-conflicts))
  (should (get 'beads-conflicts 'transient--prefix)))

(ert-deftest beads-conflicts-test-leaf-transients-defined ()
  "The three leaf command transients are defined."
  :tags '(:unit)
  (should (fboundp 'beads-conflicts-list))
  (should (fboundp 'beads-conflicts-resolve))
  (should (fboundp 'beads-conflicts-show)))

(provide 'beads-command-conflicts-test)
;;; beads-command-conflicts-test.el ends here
