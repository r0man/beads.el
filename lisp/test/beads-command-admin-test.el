;;; beads-command-admin-test.el --- Tests for beads-command-admin -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-admin command classes.

;;; Code:

(require 'ert)
(require 'beads-command-admin)

;;; Unit Tests: beads-command-admin-cleanup command-line

(ert-deftest beads-command-admin-cleanup-test-command-line-basic ()
  "Unit test: admin cleanup builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-admin-cleanup))
         (args (beads-command-line cmd)))
    (should (member "admin" args))
    (should (member "cleanup" args))))

(ert-deftest beads-command-admin-cleanup-test-command-line-dry-run ()
  "Unit test: admin cleanup includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-admin-cleanup :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-admin-cleanup-test-command-line-force ()
  "Unit test: admin cleanup includes --force option."
  :tags '(:unit)
  (let* ((cmd (beads-command-admin-cleanup :force t))
         (args (beads-command-line cmd)))
    (should (member "--force" args))))

(ert-deftest beads-command-admin-cleanup-test-validation-missing-force ()
  "Unit test: admin cleanup validation requires --force or --dry-run.
Mirrors bd's runtime safety check; without either flag bd refuses
to act, so we fail fast in Emacs instead of round-tripping."
  :tags '(:unit)
  (let ((cmd (beads-command-admin-cleanup)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-admin-cleanup-test-validation-with-force ()
  "Unit test: admin cleanup validation succeeds with --force."
  :tags '(:unit)
  (let ((cmd (beads-command-admin-cleanup :force t)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-admin-cleanup-test-validation-with-dry-run ()
  "Unit test: admin cleanup validation succeeds with --dry-run."
  :tags '(:unit)
  (let ((cmd (beads-command-admin-cleanup :dry-run t)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-admin-cleanup-test-validation-filter-only-fails ()
  "Unit test: admin cleanup validation fails when only filters are set.
Filters narrow what gets deleted but do not satisfy the safety
gate -- bd still requires --force or --dry-run."
  :tags '(:unit)
  (should (beads-command-validate
           (beads-command-admin-cleanup :older-than "30")))
  (should (beads-command-validate
           (beads-command-admin-cleanup :ephemeral t))))

;;; Unit Tests: beads-command-admin-compact command-line

(ert-deftest beads-command-admin-compact-test-command-line-basic ()
  "Unit test: admin compact builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-admin-compact))
         (args (beads-command-line cmd)))
    (should (member "admin" args))
    (should (member "compact" args))))

(ert-deftest beads-command-admin-compact-test-command-line-dry-run ()
  "Unit test: admin compact includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-admin-compact :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

;;; Unit Tests: beads-command-admin-reset command-line

(ert-deftest beads-command-admin-reset-test-command-line-basic ()
  "Unit test: admin reset builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-admin-reset))
         (args (beads-command-line cmd)))
    (should (member "admin" args))
    (should (member "reset" args))))

(ert-deftest beads-command-admin-reset-test-command-line-force ()
  "Unit test: admin reset includes --force option."
  :tags '(:unit)
  (let* ((cmd (beads-command-admin-reset :force t))
         (args (beads-command-line cmd)))
    (should (member "--force" args))))

(provide 'beads-command-admin-test)
;;; beads-command-admin-test.el ends here
