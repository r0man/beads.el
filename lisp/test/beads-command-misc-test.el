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

(ert-deftest beads-command-duplicates-test-command-line-auto-merge ()
  "Unit test: duplicates includes --auto-merge option."
  :tags '(:unit)
  (let* ((cmd (beads-command-duplicates :auto-merge t))
         (args (beads-command-line cmd)))
    (should (member "--auto-merge" args))))

(ert-deftest beads-command-duplicates-test-command-line-dry-run ()
  "Unit test: duplicates includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-duplicates :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

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
  (let* ((cmd (beads-command-rename-prefix :new-prefix "new"))
         (args (beads-command-line cmd)))
    (should (member "rename-prefix" args))
    (should (member "new" args))))

(ert-deftest beads-command-rename-prefix-test-command-line-dry-run ()
  "Unit test: rename-prefix includes --dry-run."
  :tags '(:unit)
  (let* ((cmd (beads-command-rename-prefix :new-prefix "new" :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-rename-prefix-test-command-line-repair ()
  "Unit test: rename-prefix includes --repair."
  :tags '(:unit)
  (let* ((cmd (beads-command-rename-prefix :new-prefix "new" :repair t))
         (args (beads-command-line cmd)))
    (should (member "--repair" args))))

(ert-deftest beads-command-rename-prefix-test-validation-missing-new ()
  "Unit test: rename-prefix validation fails without new-prefix."
  :tags '(:unit)
  (let ((cmd (beads-command-rename-prefix)))
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

(ert-deftest beads-command-rename-test-validation-self-rename-rejected ()
  "Unit test: rename validation fails when old-id equals new-id."
  :tags '(:unit)
  (let ((cmd (beads-command-rename :old-id "bd-auth" :new-id "bd-auth")))
    (should (beads-command-validate cmd))))

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

;;; Unit Tests: beads-command-human-list

(ert-deftest beads-command-human-list-test-class-exists ()
  "Unit test: beads-command-human-list class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-human-list)))

(ert-deftest beads-command-human-list-test-subcommand ()
  "Unit test: human list subcommand is 'human list'."
  :tags '(:unit)
  (let ((cmd (beads-command-human-list)))
    (should (equal (beads-command-subcommand cmd) "human list"))))

(ert-deftest beads-command-human-list-test-command-line ()
  "Unit test: human list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-human-list))
         (args (beads-command-line cmd)))
    (should (member "human" args))
    (should (member "list" args))))

(ert-deftest beads-command-human-list-test-command-line-with-status ()
  "Unit test: human list builds correct command line with status filter."
  :tags '(:unit)
  (let* ((cmd (beads-command-human-list :status "open"))
         (args (beads-command-line cmd)))
    (should (member "human" args))
    (should (member "list" args))
    (should (member "--status" args))
    (should (member "open" args))))

;;; Unit Tests: beads-command-human-respond

(ert-deftest beads-command-human-respond-test-class-exists ()
  "Unit test: beads-command-human-respond class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-human-respond)))

(ert-deftest beads-command-human-respond-test-subcommand ()
  "Unit test: human respond subcommand is 'human respond'."
  :tags '(:unit)
  (let ((cmd (beads-command-human-respond)))
    (should (equal (beads-command-subcommand cmd) "human respond"))))

(ert-deftest beads-command-human-respond-test-command-line ()
  "Unit test: human respond builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-human-respond
               :issue-id "bd-123"
               :response "Use OAuth2"))
         (args (beads-command-line cmd)))
    (should (member "human" args))
    (should (member "respond" args))))

(ert-deftest beads-command-human-respond-test-validation-missing-issue-id ()
  "Unit test: human respond validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-human-respond :response "Some response")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-human-respond-test-validation-missing-response ()
  "Unit test: human respond validation fails without response."
  :tags '(:unit)
  (let ((cmd (beads-command-human-respond :issue-id "bd-123")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-human-respond-test-validation-with-all-required ()
  "Unit test: human respond validation passes with issue-id and response."
  :tags '(:unit)
  (let ((cmd (beads-command-human-respond
              :issue-id "bd-123"
              :response "Use OAuth2")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-human-dismiss

(ert-deftest beads-command-human-dismiss-test-class-exists ()
  "Unit test: beads-command-human-dismiss class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-human-dismiss)))

(ert-deftest beads-command-human-dismiss-test-subcommand ()
  "Unit test: human dismiss subcommand is 'human dismiss'."
  :tags '(:unit)
  (let ((cmd (beads-command-human-dismiss)))
    (should (equal (beads-command-subcommand cmd) "human dismiss"))))

(ert-deftest beads-command-human-dismiss-test-command-line ()
  "Unit test: human dismiss builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-human-dismiss :issue-id "bd-123"))
         (args (beads-command-line cmd)))
    (should (member "human" args))
    (should (member "dismiss" args))))

(ert-deftest beads-command-human-dismiss-test-validation-missing-issue-id ()
  "Unit test: human dismiss validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-human-dismiss)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-human-dismiss-test-validation-with-issue-id ()
  "Unit test: human dismiss validation passes with issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-human-dismiss :issue-id "bd-123")))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-command-human-dismiss-test-validation-with-reason ()
  "Unit test: human dismiss validation passes with issue-id and reason."
  :tags '(:unit)
  (let ((cmd (beads-command-human-dismiss
              :issue-id "bd-123"
              :reason "No longer applicable")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-human-stats

(ert-deftest beads-command-human-stats-test-class-exists ()
  "Unit test: beads-command-human-stats class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-human-stats)))

(ert-deftest beads-command-human-stats-test-subcommand ()
  "Unit test: human stats subcommand is 'human stats'."
  :tags '(:unit)
  (let ((cmd (beads-command-human-stats)))
    (should (equal (beads-command-subcommand cmd) "human stats"))))

(ert-deftest beads-command-human-stats-test-command-line ()
  "Unit test: human stats builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-human-stats))
         (args (beads-command-line cmd)))
    (should (member "human" args))
    (should (member "stats" args))))

;;; Unit Tests: beads-command-assign

(ert-deftest beads-command-assign-test-class-exists ()
  "Unit test: beads-command-assign class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-assign)))

(ert-deftest beads-command-assign-test-command-line ()
  "Unit test: assign builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-assign :issue-id "bd-123" :assignee "alice"))
         (args (beads-command-line cmd)))
    (should (member "assign" args))
    (should (member "bd-123" args))
    (should (member "alice" args))))

(ert-deftest beads-command-assign-test-validation-missing-id ()
  "Unit test: assign validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-assign :assignee "alice")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-assign-test-validation-success ()
  "Unit test: assign validation succeeds with issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-assign :issue-id "bd-123" :assignee "alice")))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-command-assign-test-empty-assignee-unassigns ()
  "Unit test: assign with empty string assignee passes validation (unassign)."
  :tags '(:unit)
  (let ((cmd (beads-command-assign :issue-id "bd-123" :assignee "")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-comment

(ert-deftest beads-command-comment-test-class-exists ()
  "Unit test: beads-command-comment class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-comment)))

(ert-deftest beads-command-comment-test-command-line ()
  "Unit test: comment builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-comment :issue-id "bd-123" :text "hello"))
         (args (beads-command-line cmd)))
    (should (member "comment" args))
    (should (member "bd-123" args))
    (should (member "hello" args))))

(ert-deftest beads-command-comment-test-validation-missing-id ()
  "Unit test: comment validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-comment :text "hello")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-comment-test-validation-success ()
  "Unit test: comment validation succeeds with issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-comment :issue-id "bd-123")))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-command-comment-test-command-line-stdin ()
  "Unit test: comment builds correct command line with --stdin."
  :tags '(:unit)
  (let* ((cmd (beads-command-comment :issue-id "bd-123" :stdin t))
         (args (beads-command-line cmd)))
    (should (member "comment" args))
    (should (member "bd-123" args))
    (should (member "--stdin" args))))

(ert-deftest beads-command-comment-test-command-line-file ()
  "Unit test: comment builds correct command line with --file."
  :tags '(:unit)
  (let* ((cmd (beads-command-comment :issue-id "bd-123" :file "note.md"))
         (args (beads-command-line cmd)))
    (should (member "comment" args))
    (should (member "bd-123" args))
    (should (member "--file" args))
    (should (member "note.md" args))))

;;; Unit Tests: beads-command-link

(ert-deftest beads-command-link-test-class-exists ()
  "Unit test: beads-command-link class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-link)))

(ert-deftest beads-command-link-test-command-line ()
  "Unit test: link builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-link :id1 "bd-1" :id2 "bd-2"))
         (args (beads-command-line cmd)))
    (should (member "link" args))
    (should (member "bd-1" args))
    (should (member "bd-2" args))))

(ert-deftest beads-command-link-test-validation-missing-id1 ()
  "Unit test: link validation fails without id1."
  :tags '(:unit)
  (let ((cmd (beads-command-link :id2 "bd-2")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-link-test-validation-missing-id2 ()
  "Unit test: link validation fails without id2."
  :tags '(:unit)
  (let ((cmd (beads-command-link :id1 "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-link-test-validation-success ()
  "Unit test: link validation succeeds with both ids."
  :tags '(:unit)
  (let ((cmd (beads-command-link :id1 "bd-1" :id2 "bd-2")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-priority

(ert-deftest beads-command-priority-test-class-exists ()
  "Unit test: beads-command-priority class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-priority)))

(ert-deftest beads-command-priority-test-command-line ()
  "Unit test: priority builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-priority :issue-id "bd-123" :level 1))
         (args (beads-command-line cmd)))
    (should (member "priority" args))
    (should (member "bd-123" args))
    ;; Integer positional args are serialized to strings in command line
    (should (member "1" args))))

(ert-deftest beads-command-priority-test-validation-missing-id ()
  "Unit test: priority validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-priority :level 2)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-priority-test-validation-missing-level ()
  "Unit test: priority validation fails without level."
  :tags '(:unit)
  (let ((cmd (beads-command-priority :issue-id "bd-123")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-priority-test-validation-success ()
  "Unit test: priority validation succeeds with id and level."
  :tags '(:unit)
  (let ((cmd (beads-command-priority :issue-id "bd-123" :level 2)))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-command-priority-test-validation-level-zero ()
  "Unit test: priority validation succeeds for level 0 (critical)."
  :tags '(:unit)
  (let ((cmd (beads-command-priority :issue-id "bd-123" :level 0)))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-command-priority-test-validation-level-out-of-range ()
  "Unit test: priority validation fails for level out of 0-4 range."
  :tags '(:unit)
  (let ((cmd (beads-command-priority :issue-id "bd-123" :level 5)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-priority-test-string-level-accepted ()
  "Unit test: priority accepts string level from transient reader."
  :tags '(:unit)
  (let ((cmd (beads-command-priority :issue-id "bd-123" :level "2")))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-command-priority-test-string-level-zero-accepted ()
  "Unit test: priority accepts string level \"0\" (critical)."
  :tags '(:unit)
  (let ((cmd (beads-command-priority :issue-id "bd-123" :level "0")))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-command-priority-test-string-level-command-line ()
  "Unit test: priority builds correct command line with string level."
  :tags '(:unit)
  (let* ((cmd (beads-command-priority :issue-id "bd-123" :level "1"))
         (args (beads-command-line cmd)))
    (should (member "bd-123" args))
    (should (member "1" args))))

(ert-deftest beads-command-priority-test-string-level-empty-rejected ()
  "Unit test: priority rejects empty string level."
  :tags '(:unit)
  (let ((cmd (beads-command-priority :issue-id "bd-123" :level "")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-priority-test-string-level-non-numeric-rejected ()
  "Unit test: priority rejects non-numeric string level like \"high\"."
  :tags '(:unit)
  (let ((cmd (beads-command-priority :issue-id "bd-123" :level "high")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-priority-test-string-level-float-rejected ()
  "Unit test: priority rejects float-like string level like \"1.5\"."
  :tags '(:unit)
  (let ((cmd (beads-command-priority :issue-id "bd-123" :level "1.5")))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-link self-link

(ert-deftest beads-command-link-test-validation-self-link-rejected ()
  "Unit test: link validation fails when id1 equals id2."
  :tags '(:unit)
  (let ((cmd (beads-command-link :id1 "bd-1" :id2 "bd-1")))
    (should (beads-command-validate cmd))))

;;; Unit Tests: beads-command-tag

(ert-deftest beads-command-tag-test-class-exists ()
  "Unit test: beads-command-tag class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-tag)))

(ert-deftest beads-command-tag-test-command-line ()
  "Unit test: tag builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-tag :issue-id "bd-123" :label "bug"))
         (args (beads-command-line cmd)))
    (should (member "tag" args))
    (should (member "bd-123" args))
    (should (member "bug" args))))

(ert-deftest beads-command-tag-test-validation-missing-id ()
  "Unit test: tag validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-tag :label "bug")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-tag-test-validation-missing-label ()
  "Unit test: tag validation fails without label."
  :tags '(:unit)
  (let ((cmd (beads-command-tag :issue-id "bd-123")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-tag-test-validation-success ()
  "Unit test: tag validation succeeds with id and label."
  :tags '(:unit)
  (let ((cmd (beads-command-tag :issue-id "bd-123" :label "bug")))
    (should-not (beads-command-validate cmd))))

;;; Unit Tests: beads-command-statuses

(ert-deftest beads-command-statuses-test-class-exists ()
  "Unit test: beads-command-statuses class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-statuses)))

(ert-deftest beads-command-statuses-test-command-line ()
  "Unit test: statuses builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-statuses))
         (args (beads-command-line cmd)))
    (should (member "statuses" args))))

(ert-deftest beads-command-statuses-test-validation ()
  "Unit test: statuses validation always passes."
  :tags '(:unit)
  (let ((cmd (beads-command-statuses)))
    (should-not (beads-command-validate cmd))))

;;; Transient reader tests — Category A (readers added to existing slots)

(ert-deftest beads-command-duplicate-of-has-reader ()
  "Unit test: duplicate 'of' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads-reader-issue-id
              (beads-meta-slot-property
               'beads-command-duplicate 'of :transient-reader))))

(ert-deftest beads-command-supersede-with-id-has-reader ()
  "Unit test: supersede 'with-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads-reader-issue-id
              (beads-meta-slot-property
               'beads-command-supersede 'with-id :transient-reader))))

(ert-deftest beads-command-lint-status-has-choices ()
  "Unit test: lint 'status' slot has status choices."
  :tags '(:unit)
  (should (equal '("open" "in_progress" "blocked" "closed" "all")
                 (beads-meta-slot-property
                  'beads-command-lint 'status :transient-choices))))

(ert-deftest beads-command-human-list-status-has-choices ()
  "Unit test: human-list 'status' slot has status choices."
  :tags '(:unit)
  (should (equal '("open" "in_progress" "blocked" "closed" "all")
                 (beads-meta-slot-property
                  'beads-command-human-list 'status :transient-choices))))

(ert-deftest beads-command-human-respond-issue-id-has-reader ()
  "Unit test: human-respond 'issue-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads-reader-issue-id
              (beads-meta-slot-property
               'beads-command-human-respond 'issue-id :transient-reader))))

(ert-deftest beads-command-human-dismiss-issue-id-has-reader ()
  "Unit test: human-dismiss 'issue-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads-reader-issue-id
              (beads-meta-slot-property
               'beads-command-human-dismiss 'issue-id :transient-reader))))

(ert-deftest beads-command-children-issue-id-has-reader ()
  "Unit test: children 'issue-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads--read-issue-at-point-or-prompt
              (beads-meta-slot-property
               'beads-command-children 'issue-id :transient-reader))))

(ert-deftest beads-command-promote-issue-id-has-reader ()
  "Unit test: promote 'issue-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads-reader-issue-id
              (beads-meta-slot-property
               'beads-command-promote 'issue-id :transient-reader))))

(ert-deftest beads-command-rename-old-id-has-reader ()
  "Unit test: rename 'old-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads--read-issue-at-point-or-prompt
              (beads-meta-slot-property
               'beads-command-rename 'old-id :transient-reader))))

;;; Transient UI tests — Category B (UI slots added to positional-only slots)

(ert-deftest beads-command-assign-issue-id-has-transient-key ()
  "Unit test: assign 'issue-id' slot has transient key."
  :tags '(:unit)
  (should (equal "i"
                 (beads-meta-slot-property
                  'beads-command-assign 'issue-id :transient-key))))

(ert-deftest beads-command-assign-issue-id-has-reader ()
  "Unit test: assign 'issue-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads--read-issue-at-point-or-prompt
              (beads-meta-slot-property
               'beads-command-assign 'issue-id :transient-reader))))

(ert-deftest beads-command-assign-assignee-has-transient-key ()
  "Unit test: assign 'assignee' slot has transient key."
  :tags '(:unit)
  (should (equal "a"
                 (beads-meta-slot-property
                  'beads-command-assign 'assignee :transient-key))))

(ert-deftest beads-command-comment-issue-id-has-transient-key ()
  "Unit test: comment 'issue-id' slot has transient key."
  :tags '(:unit)
  (should (equal "i"
                 (beads-meta-slot-property
                  'beads-command-comment 'issue-id :transient-key))))

(ert-deftest beads-command-comment-issue-id-has-reader ()
  "Unit test: comment 'issue-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads--read-issue-at-point-or-prompt
              (beads-meta-slot-property
               'beads-command-comment 'issue-id :transient-reader))))

(ert-deftest beads-command-comment-text-has-transient-key ()
  "Unit test: comment 'text' slot has transient key."
  :tags '(:unit)
  (should (equal "t"
                 (beads-meta-slot-property
                  'beads-command-comment 'text :transient-key))))

(ert-deftest beads-command-note-issue-id-has-transient-key ()
  "Unit test: note 'issue-id' slot has transient key."
  :tags '(:unit)
  (should (equal "i"
                 (beads-meta-slot-property
                  'beads-command-note 'issue-id :transient-key))))

(ert-deftest beads-command-note-issue-id-has-reader ()
  "Unit test: note 'issue-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads--read-issue-at-point-or-prompt
              (beads-meta-slot-property
               'beads-command-note 'issue-id :transient-reader))))

(ert-deftest beads-command-link-id1-has-transient-key ()
  "Unit test: link 'id1' slot has transient key."
  :tags '(:unit)
  (should (equal "1"
                 (beads-meta-slot-property
                  'beads-command-link 'id1 :transient-key))))

(ert-deftest beads-command-link-id1-has-reader ()
  "Unit test: link 'id1' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads--read-issue-at-point-or-prompt
              (beads-meta-slot-property
               'beads-command-link 'id1 :transient-reader))))

(ert-deftest beads-command-link-id2-has-transient-key ()
  "Unit test: link 'id2' slot has transient key."
  :tags '(:unit)
  (should (equal "2"
                 (beads-meta-slot-property
                  'beads-command-link 'id2 :transient-key))))

(ert-deftest beads-command-link-id2-has-reader ()
  "Unit test: link 'id2' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads-reader-issue-id
              (beads-meta-slot-property
               'beads-command-link 'id2 :transient-reader))))

(ert-deftest beads-command-priority-issue-id-has-transient-key ()
  "Unit test: priority 'issue-id' slot has transient key."
  :tags '(:unit)
  (should (equal "i"
                 (beads-meta-slot-property
                  'beads-command-priority 'issue-id :transient-key))))

(ert-deftest beads-command-priority-issue-id-has-reader ()
  "Unit test: priority 'issue-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads--read-issue-at-point-or-prompt
              (beads-meta-slot-property
               'beads-command-priority 'issue-id :transient-reader))))

(ert-deftest beads-command-priority-level-has-transient-key ()
  "Unit test: priority 'level' slot has transient key."
  :tags '(:unit)
  (should (equal "p"
                 (beads-meta-slot-property
                  'beads-command-priority 'level :transient-key))))

(ert-deftest beads-command-priority-level-has-reader ()
  "Unit test: priority 'level' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads-reader-priority-level
              (beads-meta-slot-property
               'beads-command-priority 'level :transient-reader))))

(ert-deftest beads-command-tag-issue-id-has-transient-key ()
  "Unit test: tag 'issue-id' slot has transient key."
  :tags '(:unit)
  (should (equal "i"
                 (beads-meta-slot-property
                  'beads-command-tag 'issue-id :transient-key))))

(ert-deftest beads-command-tag-issue-id-has-reader ()
  "Unit test: tag 'issue-id' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads--read-issue-at-point-or-prompt
              (beads-meta-slot-property
               'beads-command-tag 'issue-id :transient-reader))))

(ert-deftest beads-command-tag-label-has-transient-key ()
  "Unit test: tag 'label' slot has transient key."
  :tags '(:unit)
  (should (equal "l"
                 (beads-meta-slot-property
                  'beads-command-tag 'label :transient-key))))

(ert-deftest beads-command-tag-label-has-reader ()
  "Unit test: tag 'label' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads-reader-label-name
              (beads-meta-slot-property
               'beads-command-tag 'label :transient-reader))))

;;; Transient UI tests — remaining :choices and :transient-reader parity

(ert-deftest beads-command-lint-issue-type-has-choices ()
  "Unit test: lint 'issue-type' slot has type choices."
  :tags '(:unit)
  (should (equal '("bug" "task" "feature" "epic" "chore")
                 (beads-meta-slot-property
                  'beads-command-lint 'issue-type :transient-choices))))


(ert-deftest beads-command-q-issue-type-has-choices ()
  "Unit test: q 'issue-type' slot has type choices."
  :tags '(:unit)
  (should (equal '("task" "bug" "feature" "epic" "chore")
                 (beads-meta-slot-property
                  'beads-command-q 'issue-type :transient-choices))))

(ert-deftest beads-command-todo-add-priority-has-reader ()
  "Unit test: todo-add 'priority' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads-reader-issue-priority
              (beads-meta-slot-property
               'beads-command-todo-add 'priority :transient-reader))))

(ert-deftest beads-command-todo-done-issue-ids-has-reader ()
  "Unit test: todo-done 'issue-ids' slot has transient-reader."
  :tags '(:unit)
  (should (eq 'beads-reader-issue-id
              (beads-meta-slot-property
               'beads-command-todo-done 'issue-ids :transient-reader))))

(ert-deftest beads-command-link-link-type-has-choices ()
  "Unit test: link 'link-type' slot has dependency type choices."
  :tags '(:unit)
  (should (equal '("blocks" "tracks" "related" "parent-child" "discovered-from")
                 (beads-meta-slot-property
                  'beads-command-link 'link-type :transient-choices))))

;;; Unit Tests: beads-command-rules-audit command-line

(ert-deftest beads-command-rules-audit-test-class-exists ()
  "Unit test: beads-command-rules-audit class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-rules-audit)))

(ert-deftest beads-command-rules-audit-test-subcommand ()
  "Unit test: rules audit subcommand is 'rules audit'."
  :tags '(:unit)
  (let ((cmd (beads-command-rules-audit)))
    (should (equal (beads-command-subcommand cmd) "rules audit"))))

(ert-deftest beads-command-rules-audit-test-command-line-basic ()
  "Unit test: rules audit builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-rules-audit))
         (args (beads-command-line cmd)))
    (should (member "rules" args))
    (should (member "audit" args))))

(ert-deftest beads-command-rules-audit-test-command-line-path ()
  "Unit test: rules audit includes --path option."
  :tags '(:unit)
  (let* ((cmd (beads-command-rules-audit :path "/tmp/rules"))
         (args (beads-command-line cmd)))
    (should (member "--path" args))
    (should (member "/tmp/rules" args))))

;;; Unit Tests: beads-command-rules-compact command-line

(ert-deftest beads-command-rules-compact-test-class-exists ()
  "Unit test: beads-command-rules-compact class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-rules-compact)))

(ert-deftest beads-command-rules-compact-test-subcommand ()
  "Unit test: rules compact subcommand is 'rules compact'."
  :tags '(:unit)
  (let ((cmd (beads-command-rules-compact)))
    (should (equal (beads-command-subcommand cmd) "rules compact"))))

(ert-deftest beads-command-rules-compact-test-command-line-dry-run ()
  "Unit test: rules compact includes --dry-run option."
  :tags '(:unit)
  (let* ((cmd (beads-command-rules-compact :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--dry-run" args))))

(ert-deftest beads-command-rules-compact-test-command-line-auto ()
  "Unit test: rules compact includes --auto option."
  :tags '(:unit)
  (let* ((cmd (beads-command-rules-compact :auto t))
         (args (beads-command-line cmd)))
    (should (member "--auto" args))))

;;; Unit Tests: beads-command-backup-remove command-line

(ert-deftest beads-command-backup-remove-test-class-exists ()
  "Unit test: beads-command-backup-remove class is defined."
  :tags '(:unit)
  (should (cl-find-class 'beads-command-backup-remove)))

(ert-deftest beads-command-backup-remove-test-subcommand ()
  "Unit test: backup remove subcommand is 'backup remove'."
  :tags '(:unit)
  (let ((cmd (beads-command-backup-remove)))
    (should (equal (beads-command-subcommand cmd) "backup remove"))))

(ert-deftest beads-command-backup-remove-test-command-line-basic ()
  "Unit test: backup remove builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-backup-remove))
         (args (beads-command-line cmd)))
    (should (member "backup" args))
    (should (member "remove" args))))

(provide 'beads-command-misc-test)
;;; beads-command-misc-test.el ends here
