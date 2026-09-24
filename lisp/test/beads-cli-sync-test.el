;;; beads-cli-sync-test.el --- Tests for CLI sync commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;;; Commentary:

;; Tests for commands added to sync with upstream bd CLI:
;; bootstrap, context, github (sync/status/repos),
;; dolt clean-databases, migrate hooks, mol seed, mol last-activity,
;; plus the bd 1.3.x CLI sync audit's category-1 command classes and
;; category-2 flag-closure matrix (workflow be-j2b phase 3).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pcase)
(require 'beads-meta)
(require 'beads-command-init)
(require 'beads-command-misc)
(require 'beads-command-integrations)
(require 'beads-command-dolt)
(require 'beads-command-migrate)
(require 'beads-command-mol)
(require 'beads-command-blocked)
(require 'beads-command-count)
(require 'beads-command-create)
(require 'beads-command-defer)
(require 'beads-command-dep)
(require 'beads-command-epic)
(require 'beads-command-formula)
(require 'beads-command-gate)
(require 'beads-command-graph)
(require 'beads-command-history)
(require 'beads-command-list)
(require 'beads-command-ready)
(require 'beads-command-restore)
(require 'beads-command-show)
(require 'beads-command-stale)
(require 'beads-command-status)
(require 'beads-command-update)
(require 'beads-command-worktree)
(require 'beads-command-conflicts)
(require 'beads-command-events)
(require 'beads-command-provenance)
(require 'beads-command-prune)
(require 'beads-command-heartbeat)
(require 'beads-command-unclaim)
(require 'beads-command-reclaim)
(require 'beads-command-sync)
(require 'beads-command-schema)
(require 'beads-command-migrate-personal)

;;; bootstrap

(ert-deftest beads-cli-sync-test-bootstrap-command-line ()
  "Test bootstrap builds correct command line."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-bootstrap))))
    (should (member "bootstrap" args))))

(ert-deftest beads-cli-sync-test-bootstrap-dry-run ()
  "Test bootstrap --dry-run flag."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-bootstrap :dry-run t))))
    (should (member "bootstrap" args))
    (should (member "--dry-run" args))))

;;; context

(ert-deftest beads-cli-sync-test-context-command-line ()
  "Test context builds correct command line."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-context))))
    (should (member "context" args))))

;;; github

(ert-deftest beads-cli-sync-test-github-sync-command-line ()
  "Test github sync builds correct command line."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-github-sync))))
    (should (member "github" args))
    (should (member "sync" args))))

(ert-deftest beads-cli-sync-test-github-sync-pull-only ()
  "Test github sync --pull-only flag (bd 1.0.3 renamed --pull)."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-github-sync :pull-only t))))
    (should (member "--pull-only" args))))

(ert-deftest beads-cli-sync-test-github-sync-push-only ()
  "Test github sync --push-only flag (bd 1.0.3 renamed --push)."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-github-sync :push-only t))))
    (should (member "--push-only" args))))

(ert-deftest beads-cli-sync-test-github-sync-dry-run ()
  "Test github sync --dry-run flag."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-github-sync :dry-run t))))
    (should (member "--dry-run" args))))

(ert-deftest beads-cli-sync-test-github-status-command-line ()
  "Test github status builds correct command line."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-github-status))))
    (should (member "github" args))
    (should (member "status" args))))

(ert-deftest beads-cli-sync-test-github-repos-command-line ()
  "Test github repos builds correct command line."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-github-repos))))
    (should (member "github" args))
    (should (member "repos" args))))

;;; dolt clean-databases

(ert-deftest beads-cli-sync-test-dolt-clean-databases-command-line ()
  "Test dolt clean-databases builds correct command line."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-dolt-clean-databases))))
    (should (member "dolt" args))
    (should (member "clean-databases" args))))

(ert-deftest beads-cli-sync-test-dolt-clean-databases-dry-run ()
  "Test dolt clean-databases --dry-run flag."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-dolt-clean-databases :dry-run t))))
    (should (member "--dry-run" args))))

;;; dolt killall

(ert-deftest beads-cli-sync-test-dolt-killall-command-line ()
  "Test dolt killall builds correct command line."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-dolt-killall))))
    (should (member "dolt" args))
    (should (member "killall" args))))

(ert-deftest beads-cli-sync-test-dolt-killall-transient-defined ()
  "Test beads-dolt-killall transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-dolt-killall)))

;;; migrate hooks

(ert-deftest beads-cli-sync-test-migrate-hooks-command-line ()
  "Test migrate hooks builds correct command line."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-migrate-hooks))))
    (should (member "migrate" args))
    (should (member "hooks" args))))

(ert-deftest beads-cli-sync-test-migrate-hooks-dry-run ()
  "Test migrate hooks --dry-run flag."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-migrate-hooks :dry-run t))))
    (should (member "--dry-run" args))))

(ert-deftest beads-cli-sync-test-migrate-hooks-apply ()
  "Test migrate hooks --apply flag."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-migrate-hooks :apply t))))
    (should (member "--apply" args))))

(ert-deftest beads-cli-sync-test-migrate-hooks-yes ()
  "Test migrate hooks --yes flag."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-migrate-hooks :yes t))))
    (should (member "--yes" args))))

;;; mol seed

(ert-deftest beads-cli-sync-test-mol-seed-command-line ()
  "Test mol seed builds correct command line."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-mol-seed))))
    (should (member "mol" args))
    (should (member "seed" args))))

(ert-deftest beads-cli-sync-test-mol-seed-var ()
  "Test mol seed --var flag (bd 1.0.3 only flag besides global)."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-mol-seed :var '("name=test")))))
    (should (member "--var" args))
    (should (member "name=test" args))))

(ert-deftest beads-cli-sync-test-mol-seed-formula-name ()
  "Test mol seed with formula name."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-mol-seed :formula-name "mol-feature"))))
    (should (member "mol-feature" args))))

;;; mol last-activity

(ert-deftest beads-cli-sync-test-mol-last-activity-command-line ()
  "Test mol last-activity builds correct command line."
  :tags '(:unit)
  (let ((args (beads-command-line
               (beads-command-mol-last-activity
                :molecule-id "hq-wisp-0laki"))))
    (should (member "mol" args))
    (should (member "last-activity" args))
    (should (member "hq-wisp-0laki" args))))

;;; Transient definitions

(ert-deftest beads-cli-sync-test-github-transient-defined ()
  "Test beads-github transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-github)))

(ert-deftest beads-cli-sync-test-bootstrap-transient-defined ()
  "Test beads-bootstrap transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-bootstrap)))

(ert-deftest beads-cli-sync-test-context-transient-defined ()
  "Test beads-context transient is defined."
  :tags '(:unit)
  (should (fboundp 'beads-context)))

;;; ============================================================
;;; bd 1.3.x sync audit: category-2 slot closure (workflow be-j2b
;;; phase 3).  Each test pins the exact flag the audit recorded as a
;;; category-2 gap, so a slot cannot silently regress.
;;; ============================================================

;;; flag-closure table: (description . body) driver

(defun beads-cli-sync-test-slot-closure-cases ()
  "Return the category-2 flag-closure matrix.
Each element is (CLASS-CONSTRUCTOR-CLASS ARGS (CLI-WORDS...)):
construct CLASS with ARGS and require every CLI word to appear in the
serialized command line."
  '((beads-command-assign (:force t) ("--force"))
      (beads-command-blocked (:label ("a") :label-any ("b")
                                     :exclude-label ("c"))
                             ("--label" "a" "--label-any" "b"
                              "--exclude-label" "c"))
      (beads-command-children (:pretty t) ("--pretty"))
      (beads-command-count (:include-infra t) ("--include-infra"))
      (beads-command-create (:status "open" :allow-empty-description t
                                    :storage-class "versioned")
                            ("--status" "open" "--allow-empty-description"
                             "--storage-class" "versioned"))
      (beads-command-defer (:issue-ids ("bd-1") :reason "why")
                           ("--reason" "why"))
      (beads-command-dep-tree (:issue-id "bd-1" :max-rows 50)
                              ("--max-rows" "50"))
      (beads-command-dolt-clean-databases (:purge-dropped t)
                                          ("--purge-dropped"))
      (beads-command-dolt-pull (:strategy "ours") ("--strategy" "ours"))
      (beads-command-dolt-push (:yes t :no-adopt t) ("--yes" "--no-adopt"))
      (beads-command-dolt-remote-add (:remote-name "r" :url "file:///x"
                                                     :allow-git-origin t)
                                     ("--allow-git-origin"))
      (beads-command-epic-close-eligible (:reason "done")
                                         ("--reason" "done"))
      (beads-command-export (:exclude-owner ("a@b"))
                            ("--exclude-owner" "a@b"))
      (beads-command-find-duplicates (:max-rows 10) ("--max-rows" "10"))
      (beads-command-gate-create (:blocks "bd-1" :title "t")
                                 ("--title" "t"))
      (beads-command-gc (:full t) ("--full"))
      (beads-command-graph (:open t :max-rows 9) ("--open" "--max-rows" "9"))
      (beads-command-history (:issue-id "bd-1" :events t) ("--events"))
      (beads-command-human-respond (:issue-id "bd-1" :response "r"
                                                   :file "f.txt" :stdin t)
                                   ("--file" "f.txt" "--stdin"))
      (beads-command-import (:allow-stale t) ("--allow-stale"))
      (beads-command-init (:init-if-missing t :server-tls t :team-server t
                                           :proxied-server-port 8443
                                           :proxied-server-idle-timeout "30s"
                                           :proxied-server-external-tls-ca-cert-path "/ca.pem"
                                           :proxied-server-external-tls-server-name "srv"
                                           :proxied-server-external-tls-skip-verify t)
                          ("--init-if-missing" "--server-tls" "--team-server"
                           "--proxied-server-port" "8443"
                           "--proxied-server-idle-timeout" "30s"
                           "--proxied-server-external-tls-ca-cert-path" "/ca.pem"
                           "--proxied-server-external-tls-server-name" "srv"
                           "--proxied-server-external-tls-skip-verify"))
      (beads-command-list (:skip-labels t :brief t :external-ref "gh-9"
                                          :external-contains "gh"
                                          :max-rows 5 :offset 2)
                          ("--skip-labels" "--brief" "--external-ref" "gh-9"
                           "--external-contains" "gh" "--max-rows" "5"
                           "--offset" "2"))
      (beads-command-migrate (:force t) ("--force"))
      (beads-command-migrate-schema (:force t) ("--force"))
      (beads-command-mol-ready (:gated t) ("--gated"))
      (beads-command-prime (:hook-json t :memories-only t :max-memories 3
                                       :max-memory-chars 100 :no-memories t)
                           ("--hook-json" "--memories-only" "--max-memories" "3"
                            "--max-memory-chars" "100" "--no-memories"))
      (beads-command-prune (:ignore-references t) ("--ignore-references"))
      (beads-command-q (:parent "bd-1") ("--parent" "bd-1"))
      (beads-command-query (:offset 4) ("--offset" "4"))
      (beads-command-ready (:brief t :label-pattern "tech-*"
                                   :label-regex "tech-(debt|legacy)"
                                   :max-rows 7 :offset 1)
                           ("--brief" "--label-pattern" "tech-*"
                            "--label-regex" "tech-(debt|legacy)"
                            "--max-rows" "7" "--offset" "1"))
      (beads-command-restore (:issue-id "bd-1" :apply t) ("--apply"))
      (beads-command-show (:issue-ids ("bd-1") :include-comments t
                                          :brief-deps t)
                          ("--include-comments" "--brief-deps"))
      (beads-command-stale (:label ("l1") :label-any ("l2")
                                   :exclude-label ("l3"))
                           ("--label" "l1" "--label-any" "l2"
                            "--exclude-label" "l3"))
      (beads-command-status (:no-blocked t) ("--no-blocked"))
      (beads-command-types (:sections t) ("--sections"))
      (beads-command-update (:issue-ids ("bd-1") :force t
                                            :if-assignee "w" :if-status "open")
                            ("--force" "--if-assignee" "w"
                             "--if-status" "open"))
      (beads-command-worktree-remove (:name "wt" :merged-into "main")
                                     ("--merged-into" "main"))))

(ert-deftest beads-cli-sync-test-category-2-flag-closure ()
  "Every category-2 flag gap from the 1.3.x audit serializes.
One test drives the whole closure matrix; a failure names the exact
slot that stopped serializing."
  :tags '(:unit)
  (pcase-dolist (`(,class ,args ,expected)
                 (beads-cli-sync-test-slot-closure-cases))
    (let* ((cmd (apply class args))
           (actual (beads-command-line cmd)))
      (should actual)
      (dolist (word expected)
        (should
         (member word actual)))))
  (should-not (cdr (assoc "list" beads-meta-parity-accepted-drift)))
  (should-not (cdr (assoc "show" beads-meta-parity-accepted-drift)))
  (should-not (cdr (assoc "ready" beads-meta-parity-accepted-drift))))

(ert-deftest beads-cli-sync-test-accepted-drift-backlog-closed ()
  "The audit's category-2 backlog was closed and removed from policy.
Only the dep.add alias entry remains, and the category-1 backlog is
empty because every planned command has a class now."
  :tags '(:unit)
  (should (equal beads-meta-parity-planned-commands nil))
  (should (equal beads-meta-parity-accepted-drift
                 '(("dep.add" "depends-on")))))

(ert-deftest beads-cli-sync-test-category-1-command-classes ()
  "Every category-1 core command now has a class."
  :tags '(:unit)
  (dolist (class '(beads-command-conflicts-list
                     beads-command-conflicts-resolve
                     beads-command-conflicts-show
                     beads-command-dolt-remote-reset-data
                     beads-command-events-export
                     beads-command-events-prune
                     beads-command-events-tail
                     beads-command-formula-schema
                     beads-command-heartbeat
                     beads-command-migrate-personal
                     beads-command-provenance-by-ref
                     beads-command-provenance-log
                     beads-command-provenance-record
                     beads-command-reclaim
                     beads-command-schema
                     beads-command-sync
                     beads-command-unclaim))
    (should (cl-find-class class))))

(ert-deftest beads-cli-sync-test-category-1-cli-paths ()
  "Category-1 classes serialize to the exact live CLI paths."
  :tags '(:unit)
  (let ((cases
         '((beads-command-conflicts-list () "conflicts.list")
            (beads-command-conflicts-resolve (:all t :ours t)
                                             "conflicts.resolve")
            (beads-command-conflicts-show () "conflicts.show")
            (beads-command-dolt-remote-reset-data (:remote-name "r")
                                                  "dolt.remote.reset-data")
            (beads-command-events-export () "events.export")
            (beads-command-events-prune (:before 1) "events.prune")
            (beads-command-events-tail () "events.tail")
            (beads-command-formula-schema () "formula.schema")
            (beads-command-heartbeat (:issue-id "bd-1") "heartbeat")
            (beads-command-migrate-personal () "migrate-personal")
            (beads-command-provenance-by-ref (:ref "r") "provenance.by-ref")
            (beads-command-provenance-log (:issue-id "bd-1")
                                          "provenance.log")
            (beads-command-provenance-record (:issue "bd-1" :kind "cut"
                                                     :source "s")
                                             "provenance.record")
            (beads-command-reclaim () "reclaim")
            (beads-command-schema () "schema")
            (beads-command-sync () "sync")
            (beads-command-unclaim (:issue-ids ("bd-1")) "unclaim"))))
    (pcase-dolist (`(,class ,args ,dotted) cases)
      (let* ((cmd (apply class args))
             (sub (beads-command-subcommand cmd)))
        (should (equal (replace-regexp-in-string " " "." sub) dotted))))))

;;; Router transients for new mid-level groups

(ert-deftest beads-cli-sync-test-conflicts-transient-defined ()
  :tags '(:unit)
  (should (fboundp 'beads-conflicts))
  (should (get 'beads-conflicts 'transient--prefix)))

(ert-deftest beads-cli-sync-test-events-transient-defined ()
  :tags '(:unit)
  (should (fboundp 'beads-events))
  (should (get 'beads-events 'transient--prefix)))

(ert-deftest beads-cli-sync-test-provenance-transient-defined ()
  :tags '(:unit)
  (should (fboundp 'beads-provenance))
  (should (get 'beads-provenance 'transient--prefix)))

(ert-deftest beads-cli-sync-test-formula-schema-command-defined ()
  "The formula schema interactive command exists."
  :tags '(:unit)
  (should (fboundp 'beads-formula-schema)))

(ert-deftest beads-cli-sync-test-dolt-remote-reset-data-defined ()
  "The dolt remote reset-data command is on the dolt remote menu."
  :tags '(:unit)
  (should (fboundp 'beads-dolt-remote-reset-data)))

(provide 'beads-cli-sync-test)

;;; beads-cli-sync-test.el ends here
