;;; beads-command-integrations-test.el --- Tests for beads-command-integrations -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-integrations command classes.

;;; Code:

(require 'ert)
(require 'beads-command-integrations)

;;; Unit Tests: beads-command-jira-sync command-line

(ert-deftest beads-command-jira-sync-test-command-line-basic ()
  "Unit test: jira sync builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-jira-sync))
         (args (beads-command-line cmd)))
    (should (member "jira" args))
    (should (member "sync" args))))

(ert-deftest beads-command-jira-sync-test-command-line-pull ()
  "Unit test: jira sync includes --pull option."
  :tags '(:unit)
  (let* ((cmd (beads-command-jira-sync :pull t))
         (args (beads-command-line cmd)))
    (should (member "--pull" args))))

(ert-deftest beads-command-jira-sync-test-command-line-push ()
  "Unit test: jira sync includes --push option."
  :tags '(:unit)
  (let* ((cmd (beads-command-jira-sync :push t))
         (args (beads-command-line cmd)))
    (should (member "--push" args))))

(ert-deftest beads-command-jira-sync-test-command-line-dry-run ()
  "Unit test: jira sync includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-jira-sync :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

;;; Unit Tests: beads-command-jira-status command-line

(ert-deftest beads-command-jira-status-test-command-line-basic ()
  "Unit test: jira status builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-jira-status))
         (args (beads-command-line cmd)))
    (should (member "jira" args))
    (should (member "status" args))))

;;; Unit Tests: beads-command-linear-sync command-line

(ert-deftest beads-command-linear-sync-test-command-line-basic ()
  "Unit test: linear sync builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-linear-sync))
         (args (beads-command-line cmd)))
    (should (member "linear" args))
    (should (member "sync" args))))

(ert-deftest beads-command-linear-sync-test-command-line-pull ()
  "Unit test: linear sync includes --pull option."
  :tags '(:unit)
  (let* ((cmd (beads-command-linear-sync :pull t))
         (args (beads-command-line cmd)))
    (should (member "--pull" args))))

(ert-deftest beads-command-linear-sync-test-command-line-push ()
  "Unit test: linear sync includes --push option."
  :tags '(:unit)
  (let* ((cmd (beads-command-linear-sync :push t))
         (args (beads-command-line cmd)))
    (should (member "--push" args))))

(ert-deftest beads-command-linear-sync-test-command-line-dry-run ()
  "Unit test: linear sync includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-linear-sync :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

;;; Unit Tests: beads-command-linear-status command-line

(ert-deftest beads-command-linear-status-test-command-line-basic ()
  "Unit test: linear status builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-linear-status))
         (args (beads-command-line cmd)))
    (should (member "linear" args))
    (should (member "status" args))))

;;; Unit Tests: beads-command-linear-teams command-line

(ert-deftest beads-command-linear-teams-test-command-line-basic ()
  "Unit test: linear teams builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-linear-teams))
         (args (beads-command-line cmd)))
    (should (member "linear" args))
    (should (member "teams" args))))

;;; Unit Tests: beads-command-repo-add command-line

(ert-deftest beads-command-repo-add-test-command-line-basic ()
  "Unit test: repo add builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-repo-add :repo-path "/path/to/repo"))
         (args (beads-command-line cmd)))
    (should (member "repo" args))
    (should (member "add" args))
    (should (member "/path/to/repo" args))))

(ert-deftest beads-command-repo-add-test-validation-missing-repo-path ()
  "Unit test: repo add validation fails without repo-path."
  :tags '(:unit)
  (let ((cmd (beads-command-repo-add)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-repo-add-test-validation-success ()
  "Unit test: repo add validation succeeds with repo-path."
  :tags '(:unit)
  (let ((cmd (beads-command-repo-add :repo-path "/path/to/repo")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-repo-list command-line

(ert-deftest beads-command-repo-list-test-command-line-basic ()
  "Unit test: repo list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-repo-list))
         (args (beads-command-line cmd)))
    (should (member "repo" args))
    (should (member "list" args))))

;;; Unit Tests: beads-command-repo-remove command-line

(ert-deftest beads-command-repo-remove-test-command-line-basic ()
  "Unit test: repo remove builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-repo-remove :repo-path "/path/to/repo"))
         (args (beads-command-line cmd)))
    (should (member "repo" args))
    (should (member "remove" args))
    (should (member "/path/to/repo" args))))

(ert-deftest beads-command-repo-remove-test-validation-missing-repo-path ()
  "Unit test: repo remove validation fails without repo-path."
  :tags '(:unit)
  (let ((cmd (beads-command-repo-remove)))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-repo-sync command-line

(ert-deftest beads-command-repo-sync-test-command-line-basic ()
  "Unit test: repo sync builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-repo-sync))
         (args (beads-command-line cmd)))
    (should (member "repo" args))
    (should (member "sync" args))))

(provide 'beads-command-integrations-test)
;;; beads-command-integrations-test.el ends here
