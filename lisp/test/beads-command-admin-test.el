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
