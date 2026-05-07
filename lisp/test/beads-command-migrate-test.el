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

(ert-deftest beads-command-migrate-test-command-line-update-repo-id ()
  "Unit test: migrate includes --update-repo-id option."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate :update-repo-id t))
         (args (beads-command-line cmd)))
    (should (member "--update-repo-id" args))))

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

;;; Unit Tests: beads-command-migrate-issues command-line

(ert-deftest beads-command-migrate-issues-test-command-line-basic ()
  "Unit test: migrate issues builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate-issues :from "/src" :to "/dst"))
         (args (beads-command-line cmd)))
    (should (member "migrate" args))
    (should (member "issues" args))
    (should (member "--from" args))
    (should (member "/src" args))
    (should (member "--to" args))
    (should (member "/dst" args))))

;;; Unit Tests: beads-command-migrate-sync command-line

(ert-deftest beads-command-migrate-sync-test-command-line-basic ()
  "Unit test: migrate sync builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-migrate-sync))
         (args (beads-command-line cmd)))
    (should (member "migrate" args))
    (should (member "sync" args))))

(provide 'beads-command-migrate-test)
;;; beads-command-migrate-test.el ends here
