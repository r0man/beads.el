;;; beads-command-sync-test.el --- Tests for beads-command-sync -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-sync command classes.

;;; Code:

(require 'ert)
(require 'beads-command-sync)

;;; Unit Tests: beads-command-sync command-line

(ert-deftest beads-command-sync-test-command-line-basic ()
  "Unit test: sync builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync))
         (args (beads-command-line cmd)))
    (should (member "sync" args))))

(ert-deftest beads-command-sync-test-command-line-dry-run ()
  "Unit test: sync includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-sync-test-command-line-no-pull ()
  "Unit test: sync includes --no-pull option."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync :no-pull t))
         (args (beads-command-line cmd)))
    (should (member "--no-pull" args))))

(ert-deftest beads-command-sync-test-command-line-no-push ()
  "Unit test: sync includes --no-push option."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync :no-push t))
         (args (beads-command-line cmd)))
    (should (member "--no-push" args))))

(ert-deftest beads-command-sync-test-command-line-flush-only ()
  "Unit test: sync includes --flush-only option."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync :flush-only t))
         (args (beads-command-line cmd)))
    (should (member "--flush-only" args))))

(ert-deftest beads-command-sync-test-command-line-import-only ()
  "Unit test: sync includes --import-only option."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync :import-only t))
         (args (beads-command-line cmd)))
    (should (member "--import-only" args))))

(ert-deftest beads-command-sync-test-command-line-squash ()
  "Unit test: sync includes --squash option."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync :squash t))
         (args (beads-command-line cmd)))
    (should (member "--squash" args))))

(ert-deftest beads-command-sync-test-command-line-message ()
  "Unit test: sync includes --message option."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync :message "Test commit"))
         (args (beads-command-line cmd)))
    (should (member "--message" args))
    (should (member "Test commit" args))))

(ert-deftest beads-command-sync-test-command-line-status ()
  "Unit test: sync includes --status option."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync :status-flag t))
         (args (beads-command-line cmd)))
    (should (member "--status" args))))

(ert-deftest beads-command-sync-test-command-line-merge ()
  "Unit test: sync includes --merge option."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync :merge-flag t))
         (args (beads-command-line cmd)))
    (should (member "--merge" args))))

(ert-deftest beads-command-sync-test-command-line-check ()
  "Unit test: sync includes --check option."
  :tags '(:unit)
  (let* ((cmd (beads-command-sync :check t))
         (args (beads-command-line cmd)))
    (should (member "--check" args))))

(provide 'beads-command-sync-test)
;;; beads-command-sync-test.el ends here
