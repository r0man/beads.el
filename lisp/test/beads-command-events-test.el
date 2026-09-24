;;; beads-command-events-test.el --- Tests for beads-command-events -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT unit tests for beads-command-events.el (bd 1.3.x `bd events'
;; journal commands, workflow be-j2b phase 3).  Covers class
;; definitions, CLI serialization, JSON-lines parsing, and the router
;; transient.

;;; Code:

(require 'ert)
(require 'beads-command-events)

;;; Class existence

(ert-deftest beads-events-test-class-exists ()
  "Test that all three events classes are defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-events-tail))
  (should (cl-find-class 'beads-command-events-export))
  (should (cl-find-class 'beads-command-events-prune)))

;;; Subcommand derivation

(ert-deftest beads-events-test-subcommands ()
  "Each class targets its `events' subcommand."
  :tags '(:unit)
  (should (equal (beads-command-subcommand (beads-command-events-tail))
                 "events tail"))
  (should (equal (beads-command-subcommand (beads-command-events-export))
                 "events export"))
  (should (equal (beads-command-subcommand (beads-command-events-prune))
                 "events prune")))

;;; Command-line serialization

(ert-deftest beads-events-test-tail-command-line ()
  "Test `events tail --since 5 --limit 10 --follow' serialization."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-events-tail :since 5 :limit 10 :follow t))))
    (should (member "events" args))
    (should (member "tail" args))
    (should (member "--since" args))
    (should (member "5" args))
    (should (member "--limit" args))
    (should (member "10" args))
    (should (member "--follow" args))))

(ert-deftest beads-events-test-export-command-line ()
  "Test `events export' serialization."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-events-export))))
    (should (member "events" args))
    (should (member "export" args))))

(ert-deftest beads-events-test-prune-command-line ()
  "Test `events prune --before 100' serialization."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-events-prune :before 100))))
    (should (member "events" args))
    (should (member "prune" args))
    (should (member "--before" args))
    (should (member "100" args))))

;;; Validation

(ert-deftest beads-events-test-prune-validate-requires-before ()
  "Prune without --before fails validation."
  :tags '(:unit)
  (should (beads-command-validate (beads-command-events-prune))))

(ert-deftest beads-events-test-prune-validate-with-before ()
  "Prune with --before validates."
  :tags '(:unit)
  (should (null (beads-command-validate (beads-command-events-prune :before 7)))))

;;; JSON-lines parsing

(ert-deftest beads-events-test-parse-json-lines ()
  "Tail parses one-JSON-object-per-line output into event records."
  :tags '(:unit)
  (let* ((cmd (beads-command-events-tail :json t))
         (line1 "{\"seq\":1,\"ts\":\"2026-01-01T00:00:00Z\",\"op\":\"comment\",\"issue_id\":\"bd-1\",\"actor\":\"alice\",\"issue\":{\"id\":\"bd-1\",\"title\":\"t\"},\"comment\":{\"text\":\"hi\"}}")
         (line2 "{\"seq\":2,\"ts\":\"2026-01-01T00:01:00Z\",\"op\":\"update\",\"issue_id\":\"bd-1\",\"actor\":\"bob\"}")
         (result (beads-command-parse cmd (concat line1 "\n" line2 "\n"))))
    (should (= (length result) 2))
    (should (cl-every #'beads-event-record-p result))
    (let ((r1 (nth 0 result)))
      (should (= (oref r1 seq) 1))
      (should (equal (oref r1 op) "comment"))
      (should (equal (oref r1 issue-id) "bd-1"))
      (should (equal (oref r1 actor) "alice"))
      (should (beads-issue-p (oref r1 issue)))
      (should (equal (oref (oref r1 issue) title) "t")))
    (let ((r2 (nth 1 result)))
      (should (= (oref r2 seq) 2))
      (should (equal (oref r2 op) "update"))
      (should (null (oref r2 issue))))))

(ert-deftest beads-events-test-parse-empty-output ()
  "An empty journal parses to an empty list."
  :tags '(:unit)
  (should (null (beads-command-parse
                 (beads-command-events-tail :json t)
                 ""))))

(ert-deftest beads-events-test-parse-bad-line-signals-parse-error ()
  "A non-JSON line signals beads-json-parse-error."
  :tags '(:unit)
  (should-error
   (beads-command-parse (beads-command-events-tail :json t) "not json")
   :type 'beads-json-parse-error))

(ert-deftest beads-events-test-export-parse-uses-same-shape ()
  "Export parses the same JSON-lines shape as tail."
  :tags '(:unit)
  (let ((result (beads-command-parse
                 (beads-command-events-export :json t)
                 "{\"seq\":3,\"op\":\"dep_add\",\"issue_id\":\"bd-2\",\"actor\":\"x\",\"dep\":{\"kind\":\"blocks\"}}")))
    (should (= (length result) 1))
    (should (equal (oref (car result) op) "dep_add"))))

(ert-deftest beads-events-test-prune-parse-result ()
  "Prune parses the {pruned: N} envelope."
  :tags '(:unit)
  (let ((result (beads-command-parse
                 (beads-command-events-prune :json t :before 5)
                 "{\"pruned\":4,\"schema_version\":1}")))
    (should (beads-events-prune-result-p result))
    (should (= (oref result pruned) 4))))

;;; Transients

(ert-deftest beads-events-test-parent-transient-defined ()
  "The `bd events' router parent transient exists (policy: no class)."
  :tags '(:unit)
  (should (fboundp 'beads-events))
  (should (get 'beads-events 'transient--prefix)))

(ert-deftest beads-events-test-leaf-transients-defined ()
  "The three leaf command transients are defined."
  :tags '(:unit)
  (should (fboundp 'beads-events-tail))
  (should (fboundp 'beads-events-export))
  (should (fboundp 'beads-events-prune)))

(provide 'beads-command-events-test)
;;; beads-command-events-test.el ends here
