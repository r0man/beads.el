;;; beads-command-federation-test.el --- Tests for beads-command-federation -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-command-federation.el.

;;; Code:

(require 'ert)
(require 'beads-command-federation)

;;; Tests for Federation Add-Peer

(ert-deftest beads-federation-test-add-peer-class-exists ()
  "Test that beads-command-federation-add-peer class is defined."
  (should (cl-find-class 'beads-command-federation-add-peer)))

(ert-deftest beads-federation-test-add-peer-subcommand ()
  "Test that subcommand returns `federation add-peer'."
  (let ((cmd (beads-command-federation-add-peer)))
    (should (equal (beads-command-subcommand cmd) "federation add-peer"))))

(ert-deftest beads-federation-test-add-peer-validate-missing-name ()
  "Test validation when peer name is missing."
  (let ((cmd (beads-command-federation-add-peer :url "dolthub://org/repo")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-federation-test-add-peer-validate-missing-url ()
  "Test validation when URL is missing."
  (let ((cmd (beads-command-federation-add-peer :peer-name "peer1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-federation-test-add-peer-validate-both ()
  "Test validation with both name and URL."
  (let ((cmd (beads-command-federation-add-peer :peer-name "peer1"
                                                :url "dolthub://org/repo")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-federation-test-add-peer-command-line ()
  "Test add-peer command line generation."
  (let ((beads-executable "bd")
        (cmd (beads-command-federation-add-peer :peer-name "peer1"
                                                :url "dolthub://org/repo")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "federation" cmd-line))
      (should (member "add-peer" cmd-line))
      (should (member "peer1" cmd-line))
      (should (member "dolthub://org/repo" cmd-line)))))

;;; Tests for Federation Remove-Peer

(ert-deftest beads-federation-test-remove-peer-class-exists ()
  "Test that beads-command-federation-remove-peer class is defined."
  (should (cl-find-class 'beads-command-federation-remove-peer)))

(ert-deftest beads-federation-test-remove-peer-subcommand ()
  "Test that subcommand returns `federation remove-peer'."
  (let ((cmd (beads-command-federation-remove-peer)))
    (should (equal (beads-command-subcommand cmd) "federation remove-peer"))))

(ert-deftest beads-federation-test-remove-peer-validate ()
  "Test remove-peer validation."
  (let ((cmd (beads-command-federation-remove-peer)))
    (should (beads-command-validate cmd)))
  (let ((cmd (beads-command-federation-remove-peer :peer-name "peer1")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for Federation List-Peers

(ert-deftest beads-federation-test-list-peers-class-exists ()
  "Test that beads-command-federation-list-peers class is defined."
  (should (cl-find-class 'beads-command-federation-list-peers)))

(ert-deftest beads-federation-test-list-peers-subcommand ()
  "Test that subcommand returns `federation list-peers'."
  (let ((cmd (beads-command-federation-list-peers)))
    (should (equal (beads-command-subcommand cmd) "federation list-peers"))))

;;; Tests for Federation Sync

(ert-deftest beads-federation-test-sync-class-exists ()
  "Test that beads-command-federation-sync class is defined."
  (should (cl-find-class 'beads-command-federation-sync)))

(ert-deftest beads-federation-test-sync-subcommand ()
  "Test that subcommand returns `federation sync'."
  (let ((cmd (beads-command-federation-sync)))
    (should (equal (beads-command-subcommand cmd) "federation sync"))))

(ert-deftest beads-federation-test-sync-command-line-with-peer ()
  "Test sync command line with --peer."
  (let ((beads-executable "bd")
        (cmd (beads-command-federation-sync :peer "peer1")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "--peer" cmd-line))
      (should (member "peer1" cmd-line)))))

;;; Tests for Federation Status

(ert-deftest beads-federation-test-status-class-exists ()
  "Test that beads-command-federation-status class is defined."
  (should (cl-find-class 'beads-command-federation-status)))

(ert-deftest beads-federation-test-status-subcommand ()
  "Test that subcommand returns `federation status'."
  (let ((cmd (beads-command-federation-status)))
    (should (equal (beads-command-subcommand cmd) "federation status"))))

;;; Tests for Transient Menus

(ert-deftest beads-federation-test-parent-menu-defined ()
  "Test that beads-federation parent menu is defined."
  (should (fboundp 'beads-federation)))

(ert-deftest beads-federation-test-parent-menu-is-prefix ()
  "Test that beads-federation is a transient prefix."
  (should (get 'beads-federation 'transient--prefix)))

(provide 'beads-command-federation-test)
;;; beads-command-federation-test.el ends here
