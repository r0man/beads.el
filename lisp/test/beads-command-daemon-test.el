;;; beads-command-daemon-test.el --- Tests for beads-command-daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;;; Commentary:

;; Unit tests for the beads-command-daemon module.
;; Tests all 8 daemon subcommand classes and the parent transient menu.

;;; Code:

(require 'ert)
(require 'beads-command-daemon)
(require 'beads-test)

;;; ============================================================
;;; Daemon List Tests
;;; ============================================================

(ert-deftest beads-command-daemon-list-test-class-exists ()
  "Test that beads-command-daemon-list class is defined."
  (should (find-class 'beads-command-daemon-list)))

(ert-deftest beads-command-daemon-list-test-inherits-from-json ()
  "Test that beads-command-daemon-list inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-daemon-list 'beads-command-json)))

(ert-deftest beads-command-daemon-list-test-create-default ()
  "Test creating daemon list command with defaults."
  (let ((cmd (beads-command-daemon-list)))
    (should (null (oref cmd no-cleanup)))
    (should (null (oref cmd search)))))

(ert-deftest beads-command-daemon-list-test-create-with-no-cleanup ()
  "Test creating daemon list command with --no-cleanup."
  (let ((cmd (beads-command-daemon-list :no-cleanup t)))
    (should (eq t (oref cmd no-cleanup)))))

(ert-deftest beads-command-daemon-list-test-subcommand ()
  "Test subcommand method returns 'daemon list'."
  (let ((cmd (beads-command-daemon-list)))
    (should (equal "daemon list" (beads-command-subcommand cmd)))))

(ert-deftest beads-command-daemon-list-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-daemon-list)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-daemon-list-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-daemon-list :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "daemon" args))
      (should (member "list" args)))))

(ert-deftest beads-command-daemon-list-test-command-line-with-no-cleanup ()
  "Test command line includes --no-cleanup flag."
  (let ((cmd (beads-command-daemon-list :no-cleanup t)))
    (should (member "--no-cleanup" (beads-command-line cmd)))))

(ert-deftest beads-command-daemon-list-test-transient-defined ()
  "Test that beads-daemon-list transient prefix is defined."
  (should (fboundp 'beads-daemon-list)))

(ert-deftest beads-command-daemon-list-test-transient-is-prefix ()
  "Test that beads-daemon-list is a transient prefix."
  (should (get 'beads-daemon-list 'transient--prefix)))

;;; ============================================================
;;; Daemon Start Tests
;;; ============================================================

(ert-deftest beads-command-daemon-start-test-class-exists ()
  "Test that beads-command-daemon-start class is defined."
  (should (find-class 'beads-command-daemon-start)))

(ert-deftest beads-command-daemon-start-test-inherits-from-json ()
  "Test that beads-command-daemon-start inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-daemon-start 'beads-command-json)))

(ert-deftest beads-command-daemon-start-test-create-default ()
  "Test creating daemon start command with defaults."
  (let ((cmd (beads-command-daemon-start)))
    (should (null (oref cmd auto-commit)))
    (should (null (oref cmd auto-push)))
    (should (null (oref cmd auto-pull)))
    (should (null (oref cmd foreground)))
    (should (null (oref cmd local)))
    (should (null (oref cmd interval)))
    (should (null (oref cmd log-file)))
    (should (null (oref cmd log-level)))
    (should (null (oref cmd log-json)))))

(ert-deftest beads-command-daemon-start-test-create-with-auto-commit ()
  "Test creating daemon start command with --auto-commit."
  (let ((cmd (beads-command-daemon-start :auto-commit t)))
    (should (eq t (oref cmd auto-commit)))))

(ert-deftest beads-command-daemon-start-test-create-with-auto-push ()
  "Test creating daemon start command with --auto-push."
  (let ((cmd (beads-command-daemon-start :auto-push t)))
    (should (eq t (oref cmd auto-push)))))

(ert-deftest beads-command-daemon-start-test-create-with-foreground ()
  "Test creating daemon start command with --foreground."
  (let ((cmd (beads-command-daemon-start :foreground t)))
    (should (eq t (oref cmd foreground)))))

(ert-deftest beads-command-daemon-start-test-create-with-local ()
  "Test creating daemon start command with --local."
  (let ((cmd (beads-command-daemon-start :local t)))
    (should (eq t (oref cmd local)))))

(ert-deftest beads-command-daemon-start-test-create-with-interval ()
  "Test creating daemon start command with --interval."
  (let ((cmd (beads-command-daemon-start :interval "10s")))
    (should (equal "10s" (oref cmd interval)))))

(ert-deftest beads-command-daemon-start-test-create-with-log-level ()
  "Test creating daemon start command with --log-level."
  (let ((cmd (beads-command-daemon-start :log-level "debug")))
    (should (equal "debug" (oref cmd log-level)))))

(ert-deftest beads-command-daemon-start-test-subcommand ()
  "Test subcommand method returns 'daemon start'."
  (let ((cmd (beads-command-daemon-start)))
    (should (equal "daemon start" (beads-command-subcommand cmd)))))

(ert-deftest beads-command-daemon-start-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-daemon-start)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-daemon-start-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-daemon-start :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "daemon" args))
      (should (member "start" args)))))

(ert-deftest beads-command-daemon-start-test-command-line-with-auto-commit ()
  "Test command line includes --auto-commit flag."
  (let ((cmd (beads-command-daemon-start :auto-commit t)))
    (should (member "--auto-commit" (beads-command-line cmd)))))

(ert-deftest beads-command-daemon-start-test-command-line-with-auto-push ()
  "Test command line includes --auto-push flag."
  (let ((cmd (beads-command-daemon-start :auto-push t)))
    (should (member "--auto-push" (beads-command-line cmd)))))

(ert-deftest beads-command-daemon-start-test-command-line-with-foreground ()
  "Test command line includes --foreground flag."
  (let ((cmd (beads-command-daemon-start :foreground t)))
    (should (member "--foreground" (beads-command-line cmd)))))

(ert-deftest beads-command-daemon-start-test-command-line-with-interval ()
  "Test command line includes --interval option."
  (let ((cmd (beads-command-daemon-start :interval "10s")))
    (should (member "--interval" (beads-command-line cmd)))
    (should (member "10s" (beads-command-line cmd)))))

(ert-deftest beads-command-daemon-start-test-transient-defined ()
  "Test that beads-daemon-start transient prefix is defined."
  (should (fboundp 'beads-daemon-start)))

(ert-deftest beads-command-daemon-start-test-transient-is-prefix ()
  "Test that beads-daemon-start is a transient prefix."
  (should (get 'beads-daemon-start 'transient--prefix)))

;;; ============================================================
;;; Daemon Stop Tests
;;; ============================================================

(ert-deftest beads-command-daemon-stop-test-class-exists ()
  "Test that beads-command-daemon-stop class is defined."
  (should (find-class 'beads-command-daemon-stop)))

(ert-deftest beads-command-daemon-stop-test-inherits-from-json ()
  "Test that beads-command-daemon-stop inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-daemon-stop 'beads-command-json)))

(ert-deftest beads-command-daemon-stop-test-create-default ()
  "Test creating daemon stop command with defaults."
  (let ((cmd (beads-command-daemon-stop)))
    (should (null (oref cmd target)))))

(ert-deftest beads-command-daemon-stop-test-create-with-target ()
  "Test creating daemon stop command with target."
  (let ((cmd (beads-command-daemon-stop :target "/path/to/workspace")))
    (should (equal "/path/to/workspace" (oref cmd target)))))

(ert-deftest beads-command-daemon-stop-test-subcommand ()
  "Test subcommand method returns 'daemon stop'."
  (let ((cmd (beads-command-daemon-stop)))
    (should (equal "daemon stop" (beads-command-subcommand cmd)))))

(ert-deftest beads-command-daemon-stop-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-daemon-stop)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-daemon-stop-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-daemon-stop :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "daemon" args))
      (should (member "stop" args)))))

(ert-deftest beads-command-daemon-stop-test-transient-defined ()
  "Test that beads-daemon-stop transient prefix is defined."
  (should (fboundp 'beads-daemon-stop)))

;;; ============================================================
;;; Daemon Status Tests
;;; ============================================================

(ert-deftest beads-command-daemon-status-test-class-exists ()
  "Test that beads-command-daemon-status class is defined."
  (should (find-class 'beads-command-daemon-status)))

(ert-deftest beads-command-daemon-status-test-inherits-from-json ()
  "Test that beads-command-daemon-status inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-daemon-status 'beads-command-json)))

(ert-deftest beads-command-daemon-status-test-create-default ()
  "Test creating daemon status command with defaults."
  (let ((cmd (beads-command-daemon-status)))
    (should (null (oref cmd all)))
    (should (null (oref cmd search)))))

(ert-deftest beads-command-daemon-status-test-create-with-all ()
  "Test creating daemon status command with --all."
  (let ((cmd (beads-command-daemon-status :all t)))
    (should (eq t (oref cmd all)))))

(ert-deftest beads-command-daemon-status-test-subcommand ()
  "Test subcommand method returns 'daemon status'."
  (let ((cmd (beads-command-daemon-status)))
    (should (equal "daemon status" (beads-command-subcommand cmd)))))

(ert-deftest beads-command-daemon-status-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-daemon-status)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-daemon-status-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-daemon-status :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "daemon" args))
      (should (member "status" args)))))

(ert-deftest beads-command-daemon-status-test-command-line-with-all ()
  "Test command line includes --all flag."
  (let ((cmd (beads-command-daemon-status :all t)))
    (should (member "--all" (beads-command-line cmd)))))

(ert-deftest beads-command-daemon-status-test-transient-defined ()
  "Test that beads-daemon-status transient prefix is defined."
  (should (fboundp 'beads-daemon-status)))

;;; ============================================================
;;; Daemon Health Tests
;;; ============================================================

(ert-deftest beads-command-daemon-health-test-class-exists ()
  "Test that beads-command-daemon-health class is defined."
  (should (find-class 'beads-command-daemon-health)))

(ert-deftest beads-command-daemon-health-test-inherits-from-json ()
  "Test that beads-command-daemon-health inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-daemon-health 'beads-command-json)))

(ert-deftest beads-command-daemon-health-test-create-default ()
  "Test creating daemon health command with defaults."
  (let ((cmd (beads-command-daemon-health)))
    (should (null (oref cmd search)))))

(ert-deftest beads-command-daemon-health-test-subcommand ()
  "Test subcommand method returns 'daemon health'."
  (let ((cmd (beads-command-daemon-health)))
    (should (equal "daemon health" (beads-command-subcommand cmd)))))

(ert-deftest beads-command-daemon-health-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-daemon-health)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-daemon-health-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-daemon-health :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "daemon" args))
      (should (member "health" args)))))

(ert-deftest beads-command-daemon-health-test-transient-defined ()
  "Test that beads-daemon-health transient prefix is defined."
  (should (fboundp 'beads-daemon-health)))

;;; ============================================================
;;; Daemon Killall Tests
;;; ============================================================

(ert-deftest beads-command-daemon-killall-test-class-exists ()
  "Test that beads-command-daemon-killall class is defined."
  (should (find-class 'beads-command-daemon-killall)))

(ert-deftest beads-command-daemon-killall-test-inherits-from-json ()
  "Test that beads-command-daemon-killall inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-daemon-killall 'beads-command-json)))

(ert-deftest beads-command-daemon-killall-test-create-default ()
  "Test creating daemon killall command with defaults."
  (let ((cmd (beads-command-daemon-killall)))
    (should (null (oref cmd force)))
    (should (null (oref cmd search)))))

(ert-deftest beads-command-daemon-killall-test-create-with-force ()
  "Test creating daemon killall command with --force."
  (let ((cmd (beads-command-daemon-killall :force t)))
    (should (eq t (oref cmd force)))))

(ert-deftest beads-command-daemon-killall-test-subcommand ()
  "Test subcommand method returns 'daemon killall'."
  (let ((cmd (beads-command-daemon-killall)))
    (should (equal "daemon killall" (beads-command-subcommand cmd)))))

(ert-deftest beads-command-daemon-killall-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-daemon-killall)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-daemon-killall-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-daemon-killall :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "daemon" args))
      (should (member "killall" args)))))

(ert-deftest beads-command-daemon-killall-test-command-line-with-force ()
  "Test command line includes --force flag."
  (let ((cmd (beads-command-daemon-killall :force t)))
    (should (member "--force" (beads-command-line cmd)))))

(ert-deftest beads-command-daemon-killall-test-transient-defined ()
  "Test that beads-daemon-killall transient prefix is defined."
  (should (fboundp 'beads-daemon-killall)))

;;; ============================================================
;;; Daemon Logs Tests
;;; ============================================================

(ert-deftest beads-command-daemon-logs-test-class-exists ()
  "Test that beads-command-daemon-logs class is defined."
  (should (find-class 'beads-command-daemon-logs)))

(ert-deftest beads-command-daemon-logs-test-inherits-from-json ()
  "Test that beads-command-daemon-logs inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-daemon-logs 'beads-command-json)))

(ert-deftest beads-command-daemon-logs-test-create-default ()
  "Test creating daemon logs command with defaults."
  (let ((cmd (beads-command-daemon-logs)))
    (should (null (oref cmd target)))
    (should (null (oref cmd follow)))
    (should (null (oref cmd lines)))))

(ert-deftest beads-command-daemon-logs-test-create-with-follow ()
  "Test creating daemon logs command with --follow."
  (let ((cmd (beads-command-daemon-logs :follow t)))
    (should (eq t (oref cmd follow)))))

(ert-deftest beads-command-daemon-logs-test-create-with-lines ()
  "Test creating daemon logs command with --lines."
  (let ((cmd (beads-command-daemon-logs :lines 100)))
    (should (equal 100 (oref cmd lines)))))

(ert-deftest beads-command-daemon-logs-test-subcommand ()
  "Test subcommand method returns 'daemon logs'."
  (let ((cmd (beads-command-daemon-logs)))
    (should (equal "daemon logs" (beads-command-subcommand cmd)))))

(ert-deftest beads-command-daemon-logs-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-daemon-logs)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-daemon-logs-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-daemon-logs :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "daemon" args))
      (should (member "logs" args)))))

(ert-deftest beads-command-daemon-logs-test-command-line-with-follow ()
  "Test command line includes --follow flag."
  (let ((cmd (beads-command-daemon-logs :follow t)))
    (should (member "--follow" (beads-command-line cmd)))))

(ert-deftest beads-command-daemon-logs-test-command-line-with-lines ()
  "Test command line includes --lines option."
  (let ((cmd (beads-command-daemon-logs :lines 100)))
    (should (member "--lines" (beads-command-line cmd)))
    (should (member "100" (beads-command-line cmd)))))

(ert-deftest beads-command-daemon-logs-test-transient-defined ()
  "Test that beads-daemon-logs transient prefix is defined."
  (should (fboundp 'beads-daemon-logs)))

;;; ============================================================
;;; Daemon Restart Tests
;;; ============================================================

(ert-deftest beads-command-daemon-restart-test-class-exists ()
  "Test that beads-command-daemon-restart class is defined."
  (should (find-class 'beads-command-daemon-restart)))

(ert-deftest beads-command-daemon-restart-test-inherits-from-json ()
  "Test that beads-command-daemon-restart inherits from beads-command-json."
  (should (child-of-class-p 'beads-command-daemon-restart 'beads-command-json)))

(ert-deftest beads-command-daemon-restart-test-create-default ()
  "Test creating daemon restart command with defaults."
  (let ((cmd (beads-command-daemon-restart)))
    (should (null (oref cmd target)))
    (should (null (oref cmd search)))))

(ert-deftest beads-command-daemon-restart-test-create-with-target ()
  "Test creating daemon restart command with target."
  (let ((cmd (beads-command-daemon-restart :target "/path/to/workspace")))
    (should (equal "/path/to/workspace" (oref cmd target)))))

(ert-deftest beads-command-daemon-restart-test-subcommand ()
  "Test subcommand method returns 'daemon restart'."
  (let ((cmd (beads-command-daemon-restart)))
    (should (equal "daemon restart" (beads-command-subcommand cmd)))))

(ert-deftest beads-command-daemon-restart-test-validate-default ()
  "Test validation passes with defaults."
  (let ((cmd (beads-command-daemon-restart)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-daemon-restart-test-command-line-default ()
  "Test command line with default options."
  (let ((cmd (beads-command-daemon-restart :json nil)))
    (let ((args (beads-command-line cmd)))
      (should (equal "bd" (car args)))
      (should (member "daemon" args))
      (should (member "restart" args)))))

(ert-deftest beads-command-daemon-restart-test-transient-defined ()
  "Test that beads-daemon-restart transient prefix is defined."
  (should (fboundp 'beads-daemon-restart)))

;;; ============================================================
;;; Parent Transient Menu Tests
;;; ============================================================

(ert-deftest beads-command-daemon-test-parent-transient-defined ()
  "Test that beads-daemon parent transient prefix is defined."
  (should (fboundp 'beads-daemon)))

(ert-deftest beads-command-daemon-test-parent-transient-is-prefix ()
  "Test that beads-daemon is a transient prefix."
  (should (get 'beads-daemon 'transient--prefix)))

(provide 'beads-command-daemon-test)
;;; beads-command-daemon-test.el ends here
