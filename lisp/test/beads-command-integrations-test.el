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

;;; Unit Tests: beads-command-gitlab-sync command-line

(ert-deftest beads-command-gitlab-sync-test-command-line-basic ()
  "Unit test: gitlab sync builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gitlab-sync))
         (args (beads-command-line cmd)))
    (should (member "gitlab" args))
    (should (member "sync" args))))

(ert-deftest beads-command-gitlab-sync-test-command-line-pull-only ()
  "Unit test: gitlab sync includes --pull-only option."
  :tags '(:unit)
  (let* ((cmd (beads-command-gitlab-sync :pull-only t))
         (args (beads-command-line cmd)))
    (should (member "--pull-only" args))))

(ert-deftest beads-command-gitlab-sync-test-command-line-push-only ()
  "Unit test: gitlab sync includes --push-only option."
  :tags '(:unit)
  (let* ((cmd (beads-command-gitlab-sync :push-only t))
         (args (beads-command-line cmd)))
    (should (member "--push-only" args))))

(ert-deftest beads-command-gitlab-sync-test-command-line-dry-run ()
  "Unit test: gitlab sync includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-gitlab-sync :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

;;; Unit Tests: beads-command-gitlab-status command-line

(ert-deftest beads-command-gitlab-status-test-command-line-basic ()
  "Unit test: gitlab status builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gitlab-status))
         (args (beads-command-line cmd)))
    (should (member "gitlab" args))
    (should (member "status" args))))

;;; Unit Tests: beads-command-gitlab-projects command-line

(ert-deftest beads-command-gitlab-projects-test-command-line-basic ()
  "Unit test: gitlab projects builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gitlab-projects))
         (args (beads-command-line cmd)))
    (should (member "gitlab" args))
    (should (member "projects" args))))

;;; Unit Tests: beads-command-ado-sync command-line

(ert-deftest beads-command-ado-sync-test-command-line-basic ()
  "Unit test: ado sync builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-ado-sync))
         (args (beads-command-line cmd)))
    (should (member "ado" args))
    (should (member "sync" args))))

(ert-deftest beads-command-ado-sync-test-command-line-dry-run ()
  "Unit test: ado sync includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-ado-sync :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-ado-sync-test-command-line-pull-only ()
  "Unit test: ado sync includes --pull-only option."
  :tags '(:unit)
  (let* ((cmd (beads-command-ado-sync :pull-only t))
         (args (beads-command-line cmd)))
    (should (member "--pull-only" args))))

(ert-deftest beads-command-ado-sync-test-command-line-push-only ()
  "Unit test: ado sync includes --push-only option."
  :tags '(:unit)
  (let* ((cmd (beads-command-ado-sync :push-only t))
         (args (beads-command-line cmd)))
    (should (member "--push-only" args))))

;;; Unit Tests: beads-command-ado-status command-line

(ert-deftest beads-command-ado-status-test-command-line-basic ()
  "Unit test: ado status builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-ado-status))
         (args (beads-command-line cmd)))
    (should (member "ado" args))
    (should (member "status" args))))

;;; Unit Tests: beads-command-ado-projects command-line

(ert-deftest beads-command-ado-projects-test-command-line-basic ()
  "Unit test: ado projects builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-ado-projects))
         (args (beads-command-line cmd)))
    (should (member "ado" args))
    (should (member "projects" args))))

;;; Unit Tests: beads-command-notion-connect

(ert-deftest beads-command-notion-connect-test-class-exists ()
  "Unit test: beads-command-notion-connect class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-notion-connect)))

(ert-deftest beads-command-notion-connect-test-command-line-basic ()
  "Unit test: notion connect builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-notion-connect))
         (args (beads-command-line cmd)))
    (should (member "notion" args))
    (should (member "connect" args))))

(ert-deftest beads-command-notion-connect-test-command-line-with-url ()
  "Unit test: notion connect includes --url flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-notion-connect :url "https://notion.so/db"))
         (args (beads-command-line cmd)))
    (should (member "--url" args))
    (should (member "https://notion.so/db" args))))

;;; Unit Tests: beads-command-notion-init

(ert-deftest beads-command-notion-init-test-class-exists ()
  "Unit test: beads-command-notion-init class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-notion-init)))

(ert-deftest beads-command-notion-init-test-command-line-basic ()
  "Unit test: notion init builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-notion-init))
         (args (beads-command-line cmd)))
    (should (member "notion" args))
    (should (member "init" args))))

;;; Unit Tests: beads-command-notion-status

(ert-deftest beads-command-notion-status-test-class-exists ()
  "Unit test: beads-command-notion-status class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-notion-status)))

(ert-deftest beads-command-notion-status-test-command-line-basic ()
  "Unit test: notion status builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-notion-status))
         (args (beads-command-line cmd)))
    (should (member "notion" args))
    (should (member "status" args))))

;;; Unit Tests: beads-command-notion-sync

(ert-deftest beads-command-notion-sync-test-class-exists ()
  "Unit test: beads-command-notion-sync class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-notion-sync)))

(ert-deftest beads-command-notion-sync-test-command-line-basic ()
  "Unit test: notion sync builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-notion-sync))
         (args (beads-command-line cmd)))
    (should (member "notion" args))
    (should (member "sync" args))))

(ert-deftest beads-command-notion-sync-test-command-line-pull ()
  "Unit test: notion sync includes --pull flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-notion-sync :pull t))
         (args (beads-command-line cmd)))
    (should (member "--pull" args))))

(ert-deftest beads-command-notion-sync-test-command-line-dry-run ()
  "Unit test: notion sync includes --dry-run flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-notion-sync :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

;;; ============================================================
;;; Unit Tests: Per-provider pull/push commands (bde-s84l.13)
;;; ============================================================

;; Each provider exposes directional `<provider>-pull` and `<provider>-push`
;; subcommands as alternatives to bidirectional `sync`.  These smoke tests
;; verify that each new class is defined, builds the right two-word
;; subcommand, and serializes its --dry-run flag.

(ert-deftest beads-command-jira-pull-test-class-exists ()
  "Unit test: beads-command-jira-pull class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-jira-pull)))

(ert-deftest beads-command-jira-pull-test-command-line-basic ()
  "Unit test: jira pull builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-jira-pull))
         (args (beads-command-line cmd)))
    (should (member "jira" args))
    (should (member "pull" args))))

(ert-deftest beads-command-jira-pull-test-command-line-dry-run ()
  "Unit test: jira pull includes --dry-run flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-jira-pull :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-jira-push-test-class-exists ()
  "Unit test: beads-command-jira-push class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-jira-push)))

(ert-deftest beads-command-jira-push-test-command-line-basic ()
  "Unit test: jira push builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-jira-push))
         (args (beads-command-line cmd)))
    (should (member "jira" args))
    (should (member "push" args))))

(ert-deftest beads-command-linear-pull-test-class-exists ()
  "Unit test: beads-command-linear-pull class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-linear-pull)))

(ert-deftest beads-command-linear-pull-test-command-line-basic ()
  "Unit test: linear pull builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-linear-pull))
         (args (beads-command-line cmd)))
    (should (member "linear" args))
    (should (member "pull" args))))

(ert-deftest beads-command-linear-pull-test-command-line-relations ()
  "Unit test: linear pull includes --relations flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-linear-pull :relations t))
         (args (beads-command-line cmd)))
    (should (member "--relations" args))))

(ert-deftest beads-command-linear-push-test-class-exists ()
  "Unit test: beads-command-linear-push class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-linear-push)))

(ert-deftest beads-command-linear-push-test-command-line-basic ()
  "Unit test: linear push builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-linear-push))
         (args (beads-command-line cmd)))
    (should (member "linear" args))
    (should (member "push" args))))

(ert-deftest beads-command-gitlab-pull-test-class-exists ()
  "Unit test: beads-command-gitlab-pull class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-gitlab-pull)))

(ert-deftest beads-command-gitlab-pull-test-command-line-basic ()
  "Unit test: gitlab pull builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gitlab-pull))
         (args (beads-command-line cmd)))
    (should (member "gitlab" args))
    (should (member "pull" args))))

(ert-deftest beads-command-gitlab-push-test-class-exists ()
  "Unit test: beads-command-gitlab-push class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-gitlab-push)))

(ert-deftest beads-command-gitlab-push-test-command-line-basic ()
  "Unit test: gitlab push builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-gitlab-push))
         (args (beads-command-line cmd)))
    (should (member "gitlab" args))
    (should (member "push" args))))

(ert-deftest beads-command-github-pull-test-class-exists ()
  "Unit test: beads-command-github-pull class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-github-pull)))

(ert-deftest beads-command-github-pull-test-command-line-basic ()
  "Unit test: github pull builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-github-pull))
         (args (beads-command-line cmd)))
    (should (member "github" args))
    (should (member "pull" args))))

(ert-deftest beads-command-github-push-test-class-exists ()
  "Unit test: beads-command-github-push class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-github-push)))

(ert-deftest beads-command-github-push-test-command-line-basic ()
  "Unit test: github push builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-github-push))
         (args (beads-command-line cmd)))
    (should (member "github" args))
    (should (member "push" args))))

(ert-deftest beads-command-ado-pull-test-class-exists ()
  "Unit test: beads-command-ado-pull class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-ado-pull)))

(ert-deftest beads-command-ado-pull-test-command-line-basic ()
  "Unit test: ado pull builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-ado-pull))
         (args (beads-command-line cmd)))
    (should (member "ado" args))
    (should (member "pull" args))))

(ert-deftest beads-command-ado-push-test-class-exists ()
  "Unit test: beads-command-ado-push class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-ado-push)))

(ert-deftest beads-command-ado-push-test-command-line-basic ()
  "Unit test: ado push builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-ado-push))
         (args (beads-command-line cmd)))
    (should (member "ado" args))
    (should (member "push" args))))

(ert-deftest beads-command-notion-pull-test-class-exists ()
  "Unit test: beads-command-notion-pull class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-notion-pull)))

(ert-deftest beads-command-notion-pull-test-command-line-basic ()
  "Unit test: notion pull builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-notion-pull))
         (args (beads-command-line cmd)))
    (should (member "notion" args))
    (should (member "pull" args))))

(ert-deftest beads-command-notion-push-test-class-exists ()
  "Unit test: beads-command-notion-push class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-notion-push)))

(ert-deftest beads-command-notion-push-test-command-line-basic ()
  "Unit test: notion push builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-notion-push))
         (args (beads-command-line cmd)))
    (should (member "notion" args))
    (should (member "push" args))))

(provide 'beads-command-integrations-test)
;;; beads-command-integrations-test.el ends here
