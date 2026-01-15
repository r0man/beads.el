;;; beads-command-misc-test.el --- Tests for beads-command-misc -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-misc command classes.

;;; Code:

(require 'ert)
(require 'beads-command-misc)

;;; Unit Tests: beads-command-duplicate command-line

(ert-deftest beads-command-duplicate-test-command-line-basic ()
  "Unit test: duplicate builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-duplicate :issue-id "bd-1" :of "bd-2"))
         (args (beads-command-line cmd)))
    (should (member "duplicate" args))
    (should (member "bd-1" args))
    (should (member "--of" args))
    (should (member "bd-2" args))))

(ert-deftest beads-command-duplicate-test-validation-missing-issue-id ()
  "Unit test: duplicate validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-duplicate :of "bd-2")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-duplicate-test-validation-missing-of ()
  "Unit test: duplicate validation fails without --of."
  :tags '(:unit)
  (let ((cmd (beads-command-duplicate :issue-id "bd-1")))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-duplicates command-line

(ert-deftest beads-command-duplicates-test-command-line-basic ()
  "Unit test: duplicates builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-duplicates))
         (args (beads-command-line cmd)))
    (should (member "duplicates" args))))

(ert-deftest beads-command-duplicates-test-command-line-merge ()
  "Unit test: duplicates includes --merge option."
  :tags '(:unit)
  (let* ((cmd (beads-command-duplicates :merge t))
         (args (beads-command-line cmd)))
    (should (member "--merge" args))))

;;; Unit Tests: beads-command-supersede command-line

(ert-deftest beads-command-supersede-test-command-line-basic ()
  "Unit test: supersede builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-supersede :issue-id "bd-1" :with-id "bd-2"))
         (args (beads-command-line cmd)))
    (should (member "supersede" args))
    (should (member "bd-1" args))
    (should (member "--with" args))
    (should (member "bd-2" args))))

(ert-deftest beads-command-supersede-test-validation-missing-issue-id ()
  "Unit test: supersede validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-supersede :with-id "bd-2")))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-orphans command-line

(ert-deftest beads-command-orphans-test-command-line-basic ()
  "Unit test: orphans builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-orphans))
         (args (beads-command-line cmd)))
    (should (member "orphans" args))))

(ert-deftest beads-command-orphans-test-command-line-details ()
  "Unit test: orphans includes --details option."
  :tags '(:unit)
  (let* ((cmd (beads-command-orphans :details t))
         (args (beads-command-line cmd)))
    (should (member "--details" args))))

(ert-deftest beads-command-orphans-test-command-line-fix ()
  "Unit test: orphans includes --fix option."
  :tags '(:unit)
  (let* ((cmd (beads-command-orphans :fix t))
         (args (beads-command-line cmd)))
    (should (member "--fix" args))))

;;; Unit Tests: beads-command-lint command-line

(ert-deftest beads-command-lint-test-command-line-basic ()
  "Unit test: lint builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-lint))
         (args (beads-command-line cmd)))
    (should (member "lint" args))))

(ert-deftest beads-command-lint-test-command-line-status ()
  "Unit test: lint includes --status option."
  :tags '(:unit)
  (let* ((cmd (beads-command-lint :status "all"))
         (args (beads-command-line cmd)))
    (should (member "--status" args))
    (should (member "all" args))))

(ert-deftest beads-command-lint-test-command-line-type ()
  "Unit test: lint includes --type option."
  :tags '(:unit)
  (let* ((cmd (beads-command-lint :issue-type "bug"))
         (args (beads-command-line cmd)))
    (should (member "--type" args))
    (should (member "bug" args))))

;;; Unit Tests: beads-command-move command-line

(ert-deftest beads-command-move-test-command-line-basic ()
  "Unit test: move builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-move :issue-id "bd-1" :to "other"))
         (args (beads-command-line cmd)))
    (should (member "move" args))
    (should (member "bd-1" args))
    (should (member "--to" args))
    (should (member "other" args))))

(ert-deftest beads-command-move-test-validation-missing-issue-id ()
  "Unit test: move validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-move :to "other")))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-refile command-line

(ert-deftest beads-command-refile-test-command-line-basic ()
  "Unit test: refile builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-refile :source-id "bd-1" :target-rig "other"))
         (args (beads-command-line cmd)))
    (should (member "refile" args))
    (should (member "bd-1" args))
    (should (member "other" args))))

(ert-deftest beads-command-refile-test-validation-missing-source-id ()
  "Unit test: refile validation fails without source-id."
  :tags '(:unit)
  (let ((cmd (beads-command-refile :target-rig "other")))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-q command-line

(ert-deftest beads-command-q-test-command-line-basic ()
  "Unit test: q builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-q :title "Quick issue"))
         (args (beads-command-line cmd)))
    (should (member "q" args))
    (should (member "Quick issue" args))))

(ert-deftest beads-command-q-test-command-line-type ()
  "Unit test: q includes --type option."
  :tags '(:unit)
  (let* ((cmd (beads-command-q :title "Quick bug" :issue-type "bug"))
         (args (beads-command-line cmd)))
    (should (member "--type" args))
    (should (member "bug" args))))

(ert-deftest beads-command-q-test-validation-missing-title ()
  "Unit test: q validation fails without title."
  :tags '(:unit)
  (let ((cmd (beads-command-q)))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-version command-line

(ert-deftest beads-command-version-test-command-line-basic ()
  "Unit test: version builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-version))
         (args (beads-command-line cmd)))
    (should (member "version" args))))

(ert-deftest beads-command-version-test-command-line-daemon ()
  "Unit test: version includes --daemon option."
  :tags '(:unit)
  (let* ((cmd (beads-command-version :daemon t))
         (args (beads-command-line cmd)))
    (should (member "--daemon" args))))

;;; Unit Tests: beads-command-where command-line

(ert-deftest beads-command-where-test-command-line-basic ()
  "Unit test: where builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-where))
         (args (beads-command-line cmd)))
    (should (member "where" args))))

;;; Unit Tests: beads-command-human command-line

(ert-deftest beads-command-human-test-command-line-basic ()
  "Unit test: human builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-human))
         (args (beads-command-line cmd)))
    (should (member "human" args))))

;;; Unit Tests: beads-command-onboard command-line

(ert-deftest beads-command-onboard-test-command-line-basic ()
  "Unit test: onboard builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-onboard))
         (args (beads-command-line cmd)))
    (should (member "onboard" args))))

;;; Unit Tests: beads-command-prime command-line

(ert-deftest beads-command-prime-test-command-line-basic ()
  "Unit test: prime builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-prime))
         (args (beads-command-line cmd)))
    (should (member "prime" args))))

;;; Unit Tests: beads-command-preflight command-line

(ert-deftest beads-command-preflight-test-command-line-basic ()
  "Unit test: preflight builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-preflight))
         (args (beads-command-line cmd)))
    (should (member "preflight" args))))

;;; Unit Tests: beads-command-upgrade command-line

(ert-deftest beads-command-upgrade-test-command-line-basic ()
  "Unit test: upgrade builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-upgrade))
         (args (beads-command-line cmd)))
    (should (member "upgrade" args))))

;;; Unit Tests: beads-command-rename-prefix command-line

(ert-deftest beads-command-rename-prefix-test-command-line-basic ()
  "Unit test: rename-prefix builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-rename-prefix :old-prefix "old" :new-prefix "new"))
         (args (beads-command-line cmd)))
    (should (member "rename-prefix" args))
    (should (member "old" args))
    (should (member "new" args))))

(ert-deftest beads-command-rename-prefix-test-validation-missing-old ()
  "Unit test: rename-prefix validation fails without old-prefix."
  :tags '(:unit)
  (let ((cmd (beads-command-rename-prefix :new-prefix "new")))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-repair command-line

(ert-deftest beads-command-repair-test-command-line-basic ()
  "Unit test: repair builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-repair))
         (args (beads-command-line cmd)))
    (should (member "repair" args))))

;;; Unit Tests: beads-command-resolve-conflicts command-line

(ert-deftest beads-command-resolve-conflicts-test-command-line-basic ()
  "Unit test: resolve-conflicts builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-resolve-conflicts))
         (args (beads-command-line cmd)))
    (should (member "resolve-conflicts" args))))

;;; Unit Tests: beads-command-restore command-line

(ert-deftest beads-command-restore-test-command-line-basic ()
  "Unit test: restore builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-restore :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "restore" args))
    (should (member "bd-1" args))))

(ert-deftest beads-command-restore-test-validation-missing-issue-id ()
  "Unit test: restore validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-restore)))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-merge command-line

(ert-deftest beads-command-merge-test-command-line-basic ()
  "Unit test: merge builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-merge :ancestor "/a" :current "/b" :other "/c"))
         (args (beads-command-line cmd)))
    (should (member "merge" args))
    (should (member "/a" args))
    (should (member "/b" args))
    (should (member "/c" args))))

;;; Unit Tests: beads-command-setup command-line

(ert-deftest beads-command-setup-test-command-line-basic ()
  "Unit test: setup builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-setup :editor "cursor"))
         (args (beads-command-line cmd)))
    (should (member "setup" args))
    (should (member "cursor" args))))

;;; Unit Tests: beads-command-ship command-line

(ert-deftest beads-command-ship-test-command-line-basic ()
  "Unit test: ship builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-ship :capability "auth"))
         (args (beads-command-line cmd)))
    (should (member "ship" args))
    (should (member "auth" args))))

;;; Unit Tests: beads-command-cook command-line

(ert-deftest beads-command-cook-test-command-line-basic ()
  "Unit test: cook builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-cook :formula-id "formula-1"))
         (args (beads-command-line cmd)))
    (should (member "cook" args))
    (should (member "formula-1" args))))

;;; Unit Tests: beads-command-mail command-line

(ert-deftest beads-command-mail-test-command-line-basic ()
  "Unit test: mail builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-mail))
         (args (beads-command-line cmd)))
    (should (member "mail" args))))

(provide 'beads-command-misc-test)
;;; beads-command-misc-test.el ends here
