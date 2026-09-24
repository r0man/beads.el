;;; beads-command-worker-loop-test.el --- Tests for worker-lease/sync commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT unit tests for the bd 1.3.x worker-loop and sync command
;; classes implemented in workflow be-j2b phase 3:
;; - beads-command-heartbeat.el (bd heartbeat)
;; - beads-command-unclaim.el   (bd unclaim)
;; - beads-command-reclaim.el   (bd reclaim)
;; - beads-command-sync.el      (bd sync)
;; - beads-command-schema.el    (bd schema)
;; - beads-command-migrate-personal.el (bd migrate-personal)

;;; Code:

(require 'ert)
(require 'beads-command-heartbeat)
(require 'beads-command-unclaim)
(require 'beads-command-reclaim)
(require 'beads-command-sync)
(require 'beads-command-schema)
(require 'beads-command-migrate-personal)

;;; heartbeat

(ert-deftest beads-heartbeat-test-class-exists ()
  :tags '(:unit)
  (should (cl-find-class 'beads-command-heartbeat)))

(ert-deftest beads-heartbeat-test-subcommand ()
  :tags '(:unit)
  (should (equal (beads-command-subcommand
                  (beads-command-heartbeat :issue-id "bd-1"))
                 "heartbeat")))

(ert-deftest beads-heartbeat-test-command-line ()
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-heartbeat :issue-id "bd-42"))))
    (should (equal args '("bd" "heartbeat" "bd-42")))))

(ert-deftest beads-heartbeat-test-validate-requires-issue ()
  :tags '(:unit)
  (should (beads-command-validate (beads-command-heartbeat)))
  (should (null (beads-command-validate
                 (beads-command-heartbeat :issue-id "bd-1")))))

(ert-deftest beads-heartbeat-test-parse-result ()
  "Heartbeat parses the {id, owner, status} envelope."
  :tags '(:unit)
  (let ((result (beads-command-parse
                 (beads-command-heartbeat :json t :issue-id "bd-1")
                 "{\"id\":\"bd-1\",\"owner\":\"worker-7\",\"status\":\"heartbeat\",\"schema_version\":1}")))
    (should (beads-heartbeat-result-p result))
    (should (equal (oref result id) "bd-1"))
    (should (equal (oref result owner) "worker-7"))
    (should (equal (oref result status) "heartbeat"))))

(ert-deftest beads-heartbeat-test-transient-defined ()
  :tags '(:unit)
  (should (fboundp 'beads-heartbeat))
  (should (get 'beads-heartbeat 'transient--prefix)))

;;; unclaim

(ert-deftest beads-unclaim-test-class-exists ()
  :tags '(:unit)
  (should (cl-find-class 'beads-command-unclaim)))

(ert-deftest beads-unclaim-test-subcommand ()
  :tags '(:unit)
  (should (equal (beads-command-subcommand
                  (beads-command-unclaim :issue-ids '("bd-1")))
                 "unclaim")))

(ert-deftest beads-unclaim-test-command-line-basic ()
  :tags '(:unit)
  (let ((args (cdr (beads-command-line
                     (beads-command-unclaim :issue-ids '("bd-1" "bd-2"))))))
    (should (member "unclaim" args))
    (should (member "bd-1" args))
    (should (member "bd-2" args))))

(ert-deftest beads-unclaim-test-command-line-full ()
  "Test --reason, --force and --if-assignee serialization."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-unclaim :issue-ids '("bd-1")
                                      :reason "agent crashed"
                                      :if-assignee "worker-7"))))
    (should (member "--reason" args))
    (should (member "agent crashed" args))
    (should (member "--if-assignee" args))
    (should (member "worker-7" args)))
  (let ((args (beads-command-line
               (beads-command-unclaim :issue-ids '("bd-1") :force t))))
    (should (member "--force" args))))

(ert-deftest beads-unclaim-test-validate-requires-ids ()
  :tags '(:unit)
  (should (beads-command-validate (beads-command-unclaim))))

(ert-deftest beads-unclaim-test-validate-force-vs-if-assignee ()
  "--force and --if-assignee are contradictory."
  :tags '(:unit)
  (should (beads-command-validate
           (beads-command-unclaim :issue-ids '("bd-1") :force t
                                  :if-assignee "worker-7")))
  (should (null (beads-command-validate
                 (beads-command-unclaim :issue-ids '("bd-1") :force t))))
  (should (null (beads-command-validate
                 (beads-command-unclaim :issue-ids '("bd-1")
                                        :if-assignee "worker-7")))))

(ert-deftest beads-unclaim-test-parse-result ()
  "Unclaim parses a list of released issues."
  :tags '(:unit)
  (let ((result (beads-command-parse
                 (beads-command-unclaim :json t :issue-ids '("bd-1"))
                 "[{\"id\":\"bd-1\",\"title\":\"t\",\"status\":\"open\",\"priority\":2,\"issue_type\":\"task\"}]")))
    (should (= (length result) 1))
    (should (beads-issue-p (car result)))
    (should (equal (oref (car result) status) "open"))))

(ert-deftest beads-unclaim-test-transient-defined ()
  :tags '(:unit)
  (should (fboundp 'beads-unclaim))
  (should (get 'beads-unclaim 'transient--prefix)))

;;; reclaim

(ert-deftest beads-reclaim-test-class-exists ()
  :tags '(:unit)
  (should (cl-find-class 'beads-command-reclaim)))

(ert-deftest beads-reclaim-test-subcommand ()
  :tags '(:unit)
  (should (equal (beads-command-subcommand (beads-command-reclaim))
                 "reclaim")))

(ert-deftest beads-reclaim-test-command-line-default ()
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-reclaim))))
    (should (equal args '("bd" "reclaim")))))

(ert-deftest beads-reclaim-test-command-line-scoped ()
  "Test --older-than, label filters, --assignee, --id and --any-replica."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-reclaim
                :older-than "10m" :label '("lane-a")
                :label-any '("a" "b") :exclude-label '("pinned")
                :assignee '("zelda") :ids '("wy-abc" "wy-def")
                :any-replica t))))
    (should (member "--older-than" args))
    (should (member "10m" args))
    (should (member "--label" args))
    (should (member "lane-a" args))
    (should (member "--label-any" args))
    (should (member "--exclude-label" args))
    (should (member "--assignee" args))
    (should (member "zelda" args))
    (should (member "--id" args))
    (should (member "wy-abc" args))
    (should (member "wy-def" args))
    (should (member "--any-replica" args))))

(ert-deftest beads-reclaim-test-parse-result-empty ()
  "Reclaim parses the no-work envelope."
  :tags '(:unit)
  (let ((result (beads-command-parse
                 (beads-command-reclaim :json t)
                 "{\"count\":0,\"reclaimed\":null,\"schema_version\":1,\"scoped\":false}")))
    (should (beads-reclaim-result-p result))
    (should (= (oref result count) 0))
    (should (null (oref result reclaimed)))
    (should-not (oref result scoped))))

(ert-deftest beads-reclaim-test-parse-result-with-entries ()
  "Reclaim parses reclaimed entries with previous owners."
  :tags '(:unit)
  (let ((result (beads-command-parse
                 (beads-command-reclaim :json t :older-than "0s")
                 "{\"count\":1,\"reclaimed\":[{\"id\":\"wy-abc\",\"previous_owner\":\"ghost\"}],\"schema_version\":1,\"scoped\":true}")))
    (should (= (oref result count) 1))
    (should (= (length (oref result reclaimed)) 1))
    (let ((e (car (oref result reclaimed))))
      (should (beads-reclaim-entry-p e))
      (should (equal (oref e id) "wy-abc"))
      (should (equal (oref e previous-owner) "ghost")))
    (should (oref result scoped))))

(ert-deftest beads-reclaim-test-transient-defined ()
  :tags '(:unit)
  (should (fboundp 'beads-reclaim))
  (should (get 'beads-reclaim 'transient--prefix)))

;;; sync

(ert-deftest beads-sync-test-class-exists ()
  :tags '(:unit)
  (should (cl-find-class 'beads-command-sync)))

(ert-deftest beads-sync-test-subcommand ()
  :tags '(:unit)
  (should (equal (beads-command-subcommand (beads-command-sync))
                 "sync")))

(ert-deftest beads-sync-test-command-line ()
  "Test --remote, --attempts, --yes and --no-adopt serialization."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-sync :remote "mini" :attempts 5
                                   :yes t))))
    (should (member "sync" args))
    (should (member "--remote" args))
    (should (member "mini" args))
    (should (member "--attempts" args))
    (should (member "5" args))
    (should (member "--yes" args)))
  (let ((args (beads-command-line (beads-command-sync :no-adopt t))))
    (should (member "--no-adopt" args))))

(ert-deftest beads-sync-test-parse-result ()
  "Sync parses the outcome envelope."
  :tags '(:unit)
  (let ((result (beads-command-parse
                 (beads-command-sync :json t)
                 "{\"attempts\":0,\"pushed\":false,\"rows_corrected\":0,\"schema_version\":1,\"status\":\"no-remote\"}")))
    (should (beads-sync-result-p result))
    (should (equal (oref result status) "no-remote"))
    (should (= (oref result attempts) 0))
    (should-not (oref result pushed))
    (should (= (oref result rows-corrected) 0))))

(ert-deftest beads-sync-test-transient-defined ()
  :tags '(:unit)
  (should (fboundp 'beads-sync))
  (should (get 'beads-sync 'transient--prefix)))

;;; schema

(ert-deftest beads-schema-test-class-exists ()
  :tags '(:unit)
  (should (cl-find-class 'beads-command-schema)))

(ert-deftest beads-schema-test-subcommand ()
  :tags '(:unit)
  (should (equal (beads-command-subcommand (beads-command-schema))
                 "schema")))

(ert-deftest beads-schema-test-command-line ()
  :tags '(:unit)
  (should (equal (beads-command-line (beads-command-schema)) '("bd" "schema"))))

(ert-deftest beads-schema-test-parse-raw-json ()
  "With :json t and no :result, parse returns the raw JSON alist."
  :tags '(:unit)
  (let ((result (beads-command-parse
                 (beads-command-schema :json t)
                 "{\"schema_version\":1,\"types\":{\"issue\":{}}}")))
    (should (equal (alist-get 'schema_version result) 1))
    (should (assq 'types result))))

(ert-deftest beads-schema-test-transient-defined ()
  :tags '(:unit)
  (should (fboundp 'beads-schema))
  (should (get 'beads-schema 'transient--prefix)))

;;; migrate-personal

(ert-deftest beads-migrate-personal-test-class-exists ()
  :tags '(:unit)
  (should (cl-find-class 'beads-command-migrate-personal)))

(ert-deftest beads-migrate-personal-test-subcommand ()
  "The CLI path is the hyphenated `migrate-personal' (not two words)."
  :tags '(:unit)
  (should (equal (beads-command-subcommand (beads-command-migrate-personal))
                 "migrate-personal")))

(ert-deftest beads-migrate-personal-test-command-line ()
  :tags '(:unit)
  (should (equal (beads-command-line (beads-command-migrate-personal))
                 '("bd" "migrate-personal")))
  (let ((args (beads-command-line
               (beads-command-migrate-personal :yes t))))
    (should (member "--yes" args))))

(ert-deftest beads-migrate-personal-test-json-nil ()
  "migrate-personal is :json nil (its success output is plain text)."
  :tags '(:unit)
  (should (plist-member (symbol-plist 'beads-command-migrate-personal)
                        'beads-json)))

(ert-deftest beads-migrate-personal-test-transient-defined ()
  :tags '(:unit)
  (should (fboundp 'beads-migrate-personal))
  (should (get 'beads-migrate-personal 'transient--prefix)))

(provide 'beads-command-worker-loop-test)
;;; beads-command-worker-loop-test.el ends here
