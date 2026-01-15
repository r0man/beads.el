;;; beads-command-migrate-test.el --- Tests for beads-command-migrate -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-migrate command classes.

;;; Code:

(require 'ert)
(require 'beads-command-migrate)

;;; Unit Tests: beads-command-migrate command-line

(ert-deftest beads-command-migrate-test-command-line-basic ()
  "Unit test: migrate builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate))
         (args (beads-command-line cmd)))
    (should (member "migrate" args))))

(ert-deftest beads-command-migrate-test-command-line-dry-run ()
  "Unit test: migrate includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-migrate-test-command-line-cleanup ()
  "Unit test: migrate includes --cleanup option."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate :cleanup t))
         (args (beads-command-line cmd)))
    (should (member "--cleanup" args))))

(ert-deftest beads-command-migrate-test-command-line-yes ()
  "Unit test: migrate includes --yes option."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate :yes t))
         (args (beads-command-line cmd)))
    (should (member "--yes" args))))

(ert-deftest beads-command-migrate-test-command-line-inspect ()
  "Unit test: migrate includes --inspect option."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate :inspect t))
         (args (beads-command-line cmd)))
    (should (member "--inspect" args))))

;;; Unit Tests: beads-command-migrate-hash-ids command-line

(ert-deftest beads-command-migrate-hash-ids-test-command-line-basic ()
  "Unit test: migrate hash-ids builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate-hash-ids))
         (args (beads-command-line cmd)))
    (should (member "migrate" args))
    (should (member "hash-ids" args))))

(ert-deftest beads-command-migrate-hash-ids-test-command-line-dry-run ()
  "Unit test: migrate hash-ids includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate-hash-ids :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

;;; Unit Tests: beads-command-migrate-issues command-line

(ert-deftest beads-command-migrate-issues-test-command-line-basic ()
  "Unit test: migrate issues builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate-issues :source "/src" :target "/dst"))
         (args (beads-command-line cmd)))
    (should (member "migrate" args))
    (should (member "issues" args))
    (should (member "/src" args))
    (should (member "/dst" args))))

;;; Unit Tests: beads-command-migrate-sync command-line

(ert-deftest beads-command-migrate-sync-test-command-line-basic ()
  "Unit test: migrate sync builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate-sync))
         (args (beads-command-line cmd)))
    (should (member "migrate" args))
    (should (member "sync" args))))

;;; Unit Tests: beads-command-migrate-tombstones command-line

(ert-deftest beads-command-migrate-tombstones-test-command-line-basic ()
  "Unit test: migrate tombstones builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate-tombstones))
         (args (beads-command-line cmd)))
    (should (member "migrate" args))
    (should (member "tombstones" args))))

(ert-deftest beads-command-migrate-tombstones-test-command-line-dry-run ()
  "Unit test: migrate tombstones includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate-tombstones :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(provide 'beads-command-migrate-test)
;;; beads-command-migrate-test.el ends here
