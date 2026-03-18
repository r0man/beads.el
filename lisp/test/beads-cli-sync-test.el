;;; beads-cli-sync-test.el --- Tests for CLI sync commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;;; Commentary:

;; Tests for commands added to sync with upstream bd CLI:
;; bootstrap, context, github (sync/status/repos),
;; dolt clean-databases, migrate hooks, mol seed, mol last-activity.

;;; Code:

(require 'ert)
(require 'beads-command-init)
(require 'beads-command-misc)
(require 'beads-command-integrations)
(require 'beads-command-dolt)
(require 'beads-command-migrate)
(require 'beads-command-mol)

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

(ert-deftest beads-cli-sync-test-github-sync-pull ()
  "Test github sync --pull flag."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-github-sync :pull t))))
    (should (member "--pull" args))))

(ert-deftest beads-cli-sync-test-github-sync-push ()
  "Test github sync --push flag."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-github-sync :push t))))
    (should (member "--push" args))))

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

(ert-deftest beads-cli-sync-test-mol-seed-patrol ()
  "Test mol seed --patrol flag."
  :tags '(:unit)
  (let ((args (beads-command-line (beads-command-mol-seed :patrol t))))
    (should (member "--patrol" args))))

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

(provide 'beads-cli-sync-test)

;;; beads-cli-sync-test.el ends here
