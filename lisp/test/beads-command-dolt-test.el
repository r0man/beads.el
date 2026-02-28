;;; beads-command-dolt-test.el --- Tests for beads-command-dolt -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-command-dolt.el.

;;; Code:

(require 'ert)
(require 'beads-command-dolt)

;;; Tests for Dolt Show

(ert-deftest beads-dolt-test-show-class-exists ()
  "Test that beads-command-dolt-show class is defined."
  (should (cl-find-class 'beads-command-dolt-show)))

(ert-deftest beads-dolt-test-show-subcommand ()
  "Test that subcommand returns `dolt show'."
  (let ((cmd (beads-command-dolt-show)))
    (should (equal (beads-command-subcommand cmd) "dolt show"))))

;;; Tests for Dolt Set

(ert-deftest beads-dolt-test-set-class-exists ()
  "Test that beads-command-dolt-set class is defined."
  (should (cl-find-class 'beads-command-dolt-set)))

(ert-deftest beads-dolt-test-set-subcommand ()
  "Test that subcommand returns `dolt set'."
  (let ((cmd (beads-command-dolt-set)))
    (should (equal (beads-command-subcommand cmd) "dolt set"))))

(ert-deftest beads-dolt-test-set-validate-missing-key ()
  "Test validation when config-key is missing."
  (let ((cmd (beads-command-dolt-set :config-value "val")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-dolt-test-set-validate-missing-value ()
  "Test validation when config-value is missing."
  (let ((cmd (beads-command-dolt-set :config-key "host")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-dolt-test-set-validate-both ()
  "Test validation with both key and value."
  (let ((cmd (beads-command-dolt-set :config-key "host"
                                     :config-value "localhost")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-dolt-test-set-command-line ()
  "Test set command line generation."
  (let ((beads-executable "bd")
        (cmd (beads-command-dolt-set :config-key "host"
                                     :config-value "localhost")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "dolt" cmd-line))
      (should (member "set" cmd-line))
      (should (member "host" cmd-line))
      (should (member "localhost" cmd-line)))))

;;; Tests for Dolt Test

(ert-deftest beads-dolt-test-test-class-exists ()
  "Test that beads-command-dolt-test class is defined."
  (should (cl-find-class 'beads-command-dolt-test)))

(ert-deftest beads-dolt-test-test-subcommand ()
  "Test that subcommand returns `dolt test'."
  (let ((cmd (beads-command-dolt-test)))
    (should (equal (beads-command-subcommand cmd) "dolt test"))))

;;; Tests for Dolt Commit

(ert-deftest beads-dolt-test-commit-class-exists ()
  "Test that beads-command-dolt-commit class is defined."
  (should (cl-find-class 'beads-command-dolt-commit)))

(ert-deftest beads-dolt-test-commit-subcommand ()
  "Test that subcommand returns `dolt commit'."
  (let ((cmd (beads-command-dolt-commit)))
    (should (equal (beads-command-subcommand cmd) "dolt commit"))))

(ert-deftest beads-dolt-test-commit-command-line-with-message ()
  "Test commit command line with message."
  (let ((beads-executable "bd")
        (cmd (beads-command-dolt-commit :message "test")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "--message" cmd-line))
      (should (member "test" cmd-line)))))

;;; Tests for Dolt Push

(ert-deftest beads-dolt-test-push-class-exists ()
  "Test that beads-command-dolt-push class is defined."
  (should (cl-find-class 'beads-command-dolt-push)))

(ert-deftest beads-dolt-test-push-subcommand ()
  "Test that subcommand returns `dolt push'."
  (let ((cmd (beads-command-dolt-push)))
    (should (equal (beads-command-subcommand cmd) "dolt push"))))

(ert-deftest beads-dolt-test-push-command-line-with-force ()
  "Test push command line with --force."
  (let ((beads-executable "bd")
        (cmd (beads-command-dolt-push :force t)))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "--force" cmd-line)))))

;;; Tests for Dolt Pull

(ert-deftest beads-dolt-test-pull-class-exists ()
  "Test that beads-command-dolt-pull class is defined."
  (should (cl-find-class 'beads-command-dolt-pull)))

(ert-deftest beads-dolt-test-pull-subcommand ()
  "Test that subcommand returns `dolt pull'."
  (let ((cmd (beads-command-dolt-pull)))
    (should (equal (beads-command-subcommand cmd) "dolt pull"))))

;;; Tests for Dolt Start/Stop/Status

(ert-deftest beads-dolt-test-start-subcommand ()
  "Test that subcommand returns `dolt start'."
  (let ((cmd (beads-command-dolt-start)))
    (should (equal (beads-command-subcommand cmd) "dolt start"))))

(ert-deftest beads-dolt-test-stop-subcommand ()
  "Test that subcommand returns `dolt stop'."
  (let ((cmd (beads-command-dolt-stop)))
    (should (equal (beads-command-subcommand cmd) "dolt stop"))))

(ert-deftest beads-dolt-test-status-subcommand ()
  "Test that subcommand returns `dolt status'."
  (let ((cmd (beads-command-dolt-status)))
    (should (equal (beads-command-subcommand cmd) "dolt status"))))

;;; Tests for Dolt Remote

(ert-deftest beads-dolt-test-remote-add-subcommand ()
  "Test that subcommand returns `dolt remote add'."
  (let ((cmd (beads-command-dolt-remote-add)))
    (should (equal (beads-command-subcommand cmd) "dolt remote add"))))

(ert-deftest beads-dolt-test-remote-add-validate-missing-name ()
  "Test validation when remote name is missing."
  (let ((cmd (beads-command-dolt-remote-add :url "dolthub://org/repo")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-dolt-test-remote-add-validate-missing-url ()
  "Test validation when URL is missing."
  (let ((cmd (beads-command-dolt-remote-add :remote-name "origin")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-dolt-test-remote-add-validate-both ()
  "Test validation with both name and URL."
  (let ((cmd (beads-command-dolt-remote-add :remote-name "origin"
                                            :url "dolthub://org/repo")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-dolt-test-remote-list-subcommand ()
  "Test that subcommand returns `dolt remote list'."
  (let ((cmd (beads-command-dolt-remote-list)))
    (should (equal (beads-command-subcommand cmd) "dolt remote list"))))

(ert-deftest beads-dolt-test-remote-remove-subcommand ()
  "Test that subcommand returns `dolt remote remove'."
  (let ((cmd (beads-command-dolt-remote-remove)))
    (should (equal (beads-command-subcommand cmd) "dolt remote remove"))))

(ert-deftest beads-dolt-test-remote-remove-validate ()
  "Test remote remove validation."
  (let ((cmd (beads-command-dolt-remote-remove)))
    (should (beads-command-validate cmd)))
  (let ((cmd (beads-command-dolt-remote-remove :remote-name "origin")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for Transient Menus

(ert-deftest beads-dolt-test-parent-menu-defined ()
  "Test that beads-dolt parent menu is defined."
  (should (fboundp 'beads-dolt)))

(ert-deftest beads-dolt-test-parent-menu-is-prefix ()
  "Test that beads-dolt is a transient prefix."
  (should (get 'beads-dolt 'transient--prefix)))

(ert-deftest beads-dolt-test-remote-menu-defined ()
  "Test that beads-dolt-remote menu is defined."
  (should (fboundp 'beads-dolt-remote)))

(provide 'beads-command-dolt-test)
;;; beads-command-dolt-test.el ends here
