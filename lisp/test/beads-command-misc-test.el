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

;;; Unit Tests: beads-command-upgrade-status command-line

(ert-deftest beads-command-upgrade-status-test-class-exists ()
  "Unit test: beads-command-upgrade-status class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-upgrade-status)))

(ert-deftest beads-command-upgrade-status-test-subcommand ()
  "Unit test: upgrade status subcommand is 'upgrade status'."
  :tags '(:unit)
  (let ((cmd (beads-command-upgrade-status)))
    (should (equal (beads-command-subcommand cmd) "upgrade status"))))

(ert-deftest beads-command-upgrade-status-test-command-line ()
  "Unit test: upgrade status builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-upgrade-status))
         (args (beads-command-line cmd)))
    (should (member "upgrade" args))
    (should (member "status" args))))

;;; Unit Tests: beads-command-upgrade-review command-line

(ert-deftest beads-command-upgrade-review-test-class-exists ()
  "Unit test: beads-command-upgrade-review class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-upgrade-review)))

(ert-deftest beads-command-upgrade-review-test-subcommand ()
  "Unit test: upgrade review subcommand is 'upgrade review'."
  :tags '(:unit)
  (let ((cmd (beads-command-upgrade-review)))
    (should (equal (beads-command-subcommand cmd) "upgrade review"))))

(ert-deftest beads-command-upgrade-review-test-command-line ()
  "Unit test: upgrade review builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-upgrade-review))
         (args (beads-command-line cmd)))
    (should (member "upgrade" args))
    (should (member "review" args))))

;;; Unit Tests: beads-command-upgrade-ack command-line

(ert-deftest beads-command-upgrade-ack-test-class-exists ()
  "Unit test: beads-command-upgrade-ack class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-upgrade-ack)))

(ert-deftest beads-command-upgrade-ack-test-subcommand ()
  "Unit test: upgrade ack subcommand is 'upgrade ack'."
  :tags '(:unit)
  (let ((cmd (beads-command-upgrade-ack)))
    (should (equal (beads-command-subcommand cmd) "upgrade ack"))))

(ert-deftest beads-command-upgrade-ack-test-command-line ()
  "Unit test: upgrade ack builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-upgrade-ack))
         (args (beads-command-line cmd)))
    (should (member "upgrade" args))
    (should (member "ack" args))))

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

;;; Unit Tests: beads-command-rename command-line

(ert-deftest beads-command-rename-test-command-line-basic ()
  "Unit test: rename builds correct command line with both IDs."
  :tags '(:unit)
  (let* ((cmd (beads-command-rename :old-id "bd-w382l" :new-id "bd-auth"))
         (args (beads-command-line cmd)))
    (should (member "rename" args))
    (should (member "bd-w382l" args))
    (should (member "bd-auth" args))))

(ert-deftest beads-command-rename-test-validation-missing-old-id ()
  "Unit test: rename validation fails without old-id."
  :tags '(:unit)
  (let ((cmd (beads-command-rename :new-id "bd-auth")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-rename-test-validation-missing-new-id ()
  "Unit test: rename validation fails without new-id."
  :tags '(:unit)
  (let ((cmd (beads-command-rename :old-id "bd-w382l")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-rename-test-validation-valid ()
  "Unit test: rename validation passes with both IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-rename :old-id "bd-w382l" :new-id "bd-auth")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-note command-line

(ert-deftest beads-command-note-test-command-line-basic ()
  "Unit test: note builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-note :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "note" args))
    (should (member "bd-1" args))))

(ert-deftest beads-command-note-test-command-line-stdin ()
  "Unit test: note includes --stdin option."
  :tags '(:unit)
  (let* ((cmd (beads-command-note :issue-id "bd-1" :stdin t))
         (args (beads-command-line cmd)))
    (should (member "--stdin" args))))

(ert-deftest beads-command-note-test-command-line-file ()
  "Unit test: note includes --file option."
  :tags '(:unit)
  (let* ((cmd (beads-command-note :issue-id "bd-1" :file "notes.txt"))
         (args (beads-command-line cmd)))
    (should (member "--file" args))
    (should (member "notes.txt" args))))

(ert-deftest beads-command-note-test-validation-missing-issue-id ()
  "Unit test: note validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-note)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-note-test-validation-valid ()
  "Unit test: note validation passes with issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-note :issue-id "bd-1")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-import command-line

(ert-deftest beads-command-import-test-command-line-basic ()
  "Unit test: import builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-import))
         (args (beads-command-line cmd)))
    (should (member "import" args))))

(ert-deftest beads-command-import-test-command-line-file ()
  "Unit test: import includes file argument."
  :tags '(:unit)
  (let* ((cmd (beads-command-import :file "issues.jsonl"))
         (args (beads-command-line cmd)))
    (should (member "issues.jsonl" args))))

(ert-deftest beads-command-import-test-command-line-dry-run ()
  "Unit test: import includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-import :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

;;; Unit Tests: beads-command-todo-add command-line

(ert-deftest beads-command-todo-add-test-command-line-basic ()
  "Unit test: todo-add builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-todo-add :title "Buy milk"))
         (args (beads-command-line cmd)))
    (should (member "todo" args))
    (should (member "add" args))
    (should (member "Buy milk" args))))

(ert-deftest beads-command-todo-add-test-command-line-priority ()
  "Unit test: todo-add includes --priority option."
  :tags '(:unit)
  (let* ((cmd (beads-command-todo-add :title "Fix bug" :priority "1"))
         (args (beads-command-line cmd)))
    (should (member "--priority" args))
    (should (member "1" args))))

(ert-deftest beads-command-todo-add-test-command-line-description ()
  "Unit test: todo-add includes --description option."
  :tags '(:unit)
  (let* ((cmd (beads-command-todo-add
               :title "Fix bug" :description "Details here"))
         (args (beads-command-line cmd)))
    (should (member "--description" args))
    (should (member "Details here" args))))

(ert-deftest beads-command-todo-add-test-validation-missing-title ()
  "Unit test: todo-add validation fails without title."
  :tags '(:unit)
  (let ((cmd (beads-command-todo-add)))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-todo-list command-line

(ert-deftest beads-command-todo-list-test-command-line-basic ()
  "Unit test: todo-list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-todo-list))
         (args (beads-command-line cmd)))
    (should (member "todo" args))
    (should (member "list" args))))

(ert-deftest beads-command-todo-list-test-command-line-all ()
  "Unit test: todo-list includes --all option."
  :tags '(:unit)
  (let* ((cmd (beads-command-todo-list :all t))
         (args (beads-command-line cmd)))
    (should (member "--all" args))))

;;; Unit Tests: beads-command-todo-done command-line

(ert-deftest beads-command-todo-done-test-command-line-basic ()
  "Unit test: todo-done builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-todo-done :issue-ids '("bd-1")))
         (args (beads-command-line cmd)))
    (should (member "todo" args))
    (should (member "done" args))
    (should (member "bd-1" args))))

(ert-deftest beads-command-todo-done-test-command-line-multiple-ids ()
  "Unit test: todo-done includes multiple issue IDs."
  :tags '(:unit)
  (let* ((cmd (beads-command-todo-done :issue-ids '("bd-1" "bd-2")))
         (args (beads-command-line cmd)))
    (should (member "bd-1" args))
    (should (member "bd-2" args))))

(ert-deftest beads-command-todo-done-test-command-line-reason ()
  "Unit test: todo-done includes --reason option."
  :tags '(:unit)
  (let* ((cmd (beads-command-todo-done
               :issue-ids '("bd-1") :reason "All done"))
         (args (beads-command-line cmd)))
    (should (member "--reason" args))
    (should (member "All done" args))))

(ert-deftest beads-command-todo-done-test-validation-missing-ids ()
  "Unit test: todo-done validation fails without issue IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-todo-done)))
    (should (beads-command-validate cmd))))


;;; Unit Tests: beads-command-backup-init command-line

(ert-deftest beads-command-backup-init-test-class-exists ()
  "Unit test: beads-command-backup-init class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-backup-init)))

(ert-deftest beads-command-backup-init-test-subcommand ()
  "Unit test: backup init subcommand is 'backup init'."
  :tags '(:unit)
  (let ((cmd (beads-command-backup-init)))
    (should (equal (beads-command-subcommand cmd) "backup init"))))

(ert-deftest beads-command-backup-init-test-command-line-with-path ()
  "Unit test: backup init builds correct command line with path."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-init :path "/mnt/usb/backup"))
         (args (beads-command-line cmd)))
    (should (member "backup" args))
    (should (member "init" args))
    (should (member "/mnt/usb/backup" args))))

(ert-deftest beads-command-backup-init-test-validation-missing-path ()
  "Unit test: backup init validation fails without path."
  :tags '(:unit)
  (let ((cmd (beads-command-backup-init)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-backup-init-test-validation-with-path ()
  "Unit test: backup init validation passes with path."
  :tags '(:unit)
  (let ((cmd (beads-command-backup-init :path "/mnt/usb/backup")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-backup-status command-line

(ert-deftest beads-command-backup-status-test-class-exists ()
  "Unit test: beads-command-backup-status class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-backup-status)))

(ert-deftest beads-command-backup-status-test-subcommand ()
  "Unit test: backup status subcommand is 'backup status'."
  :tags '(:unit)
  (let ((cmd (beads-command-backup-status)))
    (should (equal (beads-command-subcommand cmd) "backup status"))))

(ert-deftest beads-command-backup-status-test-command-line ()
  "Unit test: backup status builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-status))
         (args (beads-command-line cmd)))
    (should (member "backup" args))
    (should (member "status" args))))

;;; Unit Tests: beads-command-backup-sync command-line

(ert-deftest beads-command-backup-sync-test-class-exists ()
  "Unit test: beads-command-backup-sync class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-backup-sync)))

(ert-deftest beads-command-backup-sync-test-subcommand ()
  "Unit test: backup sync subcommand is 'backup sync'."
  :tags '(:unit)
  (let ((cmd (beads-command-backup-sync)))
    (should (equal (beads-command-subcommand cmd) "backup sync"))))

(ert-deftest beads-command-backup-sync-test-command-line ()
  "Unit test: backup sync builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-sync))
         (args (beads-command-line cmd)))
    (should (member "backup" args))
    (should (member "sync" args))))

;;; Unit Tests: beads-command-backup-restore command-line

(ert-deftest beads-command-backup-restore-test-class-exists ()
  "Unit test: beads-command-backup-restore class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-backup-restore)))

(ert-deftest beads-command-backup-restore-test-subcommand ()
  "Unit test: backup restore subcommand is 'backup restore'."
  :tags '(:unit)
  (let ((cmd (beads-command-backup-restore)))
    (should (equal (beads-command-subcommand cmd) "backup restore"))))

(ert-deftest beads-command-backup-restore-test-command-line-basic ()
  "Unit test: backup restore builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-restore))
         (args (beads-command-line cmd)))
    (should (member "backup" args))
    (should (member "restore" args))))

(ert-deftest beads-command-backup-restore-test-command-line-path ()
  "Unit test: backup restore includes optional path argument."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-restore :path "/tmp/backup"))
         (args (beads-command-line cmd)))
    (should (member "/tmp/backup" args))))

(ert-deftest beads-command-backup-restore-test-command-line-dry-run ()
  "Unit test: backup restore includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-restore :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

;;; Unit Tests: beads-command-backup-export-git command-line

(ert-deftest beads-command-backup-export-git-test-class-exists ()
  "Unit test: beads-command-backup-export-git class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-backup-export-git)))

(ert-deftest beads-command-backup-export-git-test-subcommand ()
  "Unit test: backup export-git subcommand is 'backup export-git'."
  :tags '(:unit)
  (let ((cmd (beads-command-backup-export-git)))
    (should (equal (beads-command-subcommand cmd) "backup export-git"))))

(ert-deftest beads-command-backup-export-git-test-command-line-basic ()
  "Unit test: backup export-git builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-export-git))
         (args (beads-command-line cmd)))
    (should (member "backup" args))
    (should (member "export-git" args))))

(ert-deftest beads-command-backup-export-git-test-command-line-branch ()
  "Unit test: backup export-git includes --branch option."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-export-git :branch "my-backup"))
         (args (beads-command-line cmd)))
    (should (member "--branch" args))
    (should (member "my-backup" args))))

(ert-deftest beads-command-backup-export-git-test-command-line-dry-run ()
  "Unit test: backup export-git includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-export-git :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-backup-export-git-test-command-line-force ()
  "Unit test: backup export-git includes --force option."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-export-git :force t))
         (args (beads-command-line cmd)))
    (should (member "--force" args))))

;;; Unit Tests: beads-command-backup-fetch-git command-line

(ert-deftest beads-command-backup-fetch-git-test-class-exists ()
  "Unit test: beads-command-backup-fetch-git class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-backup-fetch-git)))

(ert-deftest beads-command-backup-fetch-git-test-subcommand ()
  "Unit test: backup fetch-git subcommand is 'backup fetch-git'."
  :tags '(:unit)
  (let ((cmd (beads-command-backup-fetch-git)))
    (should (equal (beads-command-subcommand cmd) "backup fetch-git"))))

(ert-deftest beads-command-backup-fetch-git-test-command-line-basic ()
  "Unit test: backup fetch-git builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-fetch-git))
         (args (beads-command-line cmd)))
    (should (member "backup" args))
    (should (member "fetch-git" args))))

(ert-deftest beads-command-backup-fetch-git-test-command-line-branch ()
  "Unit test: backup fetch-git includes --branch option."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-fetch-git :branch "my-backup"))
         (args (beads-command-line cmd)))
    (should (member "--branch" args))
    (should (member "my-backup" args))))

(ert-deftest beads-command-backup-fetch-git-test-command-line-remote ()
  "Unit test: backup fetch-git includes --remote option."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-fetch-git :remote "upstream"))
         (args (beads-command-line cmd)))
    (should (member "--remote" args))
    (should (member "upstream" args))))

(ert-deftest beads-command-backup-fetch-git-test-command-line-dry-run ()
  "Unit test: backup fetch-git includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-fetch-git :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

;;; Unit Tests: beads-command-kv-get

(ert-deftest beads-command-kv-get-test-class-exists ()
  "Unit test: beads-command-kv-get class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-kv-get)))

(ert-deftest beads-command-kv-get-test-subcommand ()
  "Unit test: kv get subcommand is 'kv get'."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-get)))
    (should (equal (beads-command-subcommand cmd) "kv get"))))

(ert-deftest beads-command-kv-get-test-command-line-with-key ()
  "Unit test: kv get builds correct command line with key."
  :tags '(:unit)
  (let* ((cmd (beads-command-kv-get :kv-key "mykey"))
         (args (beads-command-line cmd)))
    (should (member "kv" args))
    (should (member "get" args))
    (should (member "mykey" args))))

(ert-deftest beads-command-kv-get-test-validation-missing-key ()
  "Unit test: kv get validation fails without key."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-get)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-kv-get-test-validation-with-key ()
  "Unit test: kv get validation passes with key."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-get :kv-key "mykey")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-kv-set

(ert-deftest beads-command-kv-set-test-class-exists ()
  "Unit test: beads-command-kv-set class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-kv-set)))

(ert-deftest beads-command-kv-set-test-subcommand ()
  "Unit test: kv set subcommand is 'kv set'."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-set)))
    (should (equal (beads-command-subcommand cmd) "kv set"))))

(ert-deftest beads-command-kv-set-test-command-line-with-key-and-value ()
  "Unit test: kv set builds correct command line with key and value."
  :tags '(:unit)
  (let* ((cmd (beads-command-kv-set :kv-key "mykey" :kv-value "myvalue"))
         (args (beads-command-line cmd)))
    (should (member "kv" args))
    (should (member "set" args))
    (should (member "mykey" args))
    (should (member "myvalue" args))))

(ert-deftest beads-command-kv-set-test-validation-missing-key ()
  "Unit test: kv set validation fails without key."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-set :kv-value "myvalue")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-kv-set-test-validation-missing-value ()
  "Unit test: kv set validation fails without value."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-set :kv-key "mykey")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-kv-set-test-validation-with-key-and-value ()
  "Unit test: kv set validation passes with key and value."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-set :kv-key "mykey" :kv-value "myvalue")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-kv-clear

(ert-deftest beads-command-kv-clear-test-class-exists ()
  "Unit test: beads-command-kv-clear class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-kv-clear)))

(ert-deftest beads-command-kv-clear-test-subcommand ()
  "Unit test: kv clear subcommand is 'kv clear'."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-clear)))
    (should (equal (beads-command-subcommand cmd) "kv clear"))))

(ert-deftest beads-command-kv-clear-test-command-line-with-key ()
  "Unit test: kv clear builds correct command line with key."
  :tags '(:unit)
  (let* ((cmd (beads-command-kv-clear :kv-key "mykey"))
         (args (beads-command-line cmd)))
    (should (member "kv" args))
    (should (member "clear" args))
    (should (member "mykey" args))))

(ert-deftest beads-command-kv-clear-test-validation-missing-key ()
  "Unit test: kv clear validation fails without key."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-clear)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-kv-clear-test-validation-with-key ()
  "Unit test: kv clear validation passes with key."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-clear :kv-key "mykey")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-kv-list

(ert-deftest beads-command-kv-list-test-class-exists ()
  "Unit test: beads-command-kv-list class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-kv-list)))

(ert-deftest beads-command-kv-list-test-subcommand ()
  "Unit test: kv list subcommand is 'kv list'."
  :tags '(:unit)
  (let ((cmd (beads-command-kv-list)))
    (should (equal (beads-command-subcommand cmd) "kv list"))))

(ert-deftest beads-command-kv-list-test-command-line ()
  "Unit test: kv list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-kv-list))
         (args (beads-command-line cmd)))
    (should (member "kv" args))
    (should (member "list" args))))

(provide 'beads-command-misc-test)
;;; beads-command-misc-test.el ends here
