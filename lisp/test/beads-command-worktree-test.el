;;; beads-command-worktree-test.el --- Tests for beads-command-worktree.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-command-worktree.el including:
;; - Domain type creation and slot access (beads-worktree, beads-worktree-info)
;; - JSON conversion functions
;; - Command line building
;; - Command validation
;; - Integration tests with CLI (tagged :integration)

;;; Code:

(require 'ert)
(require 'beads-command-worktree)
(require 'beads-test)
(require 'beads-integration-test)

;;; ============================================================
;;; Test Fixtures
;;; ============================================================

(defvar beads-command-worktree-test--sample-worktree-json
  '((name . "feature-auth")
    (path . "/home/user/project/.git/worktrees/feature-auth")
    (branch . "feature-auth")
    (is_main . :json-false)
    (beads_state . "redirect"))
  "Sample worktree JSON for testing.")

(defvar beads-command-worktree-test--sample-main-worktree-json
  '((name . "myproject")
    (path . "/home/user/project")
    (branch . "main")
    (is_main . t)
    (beads_state . "shared"))
  "Sample main worktree JSON for testing.")

(defvar beads-command-worktree-test--sample-worktree-list-json
  (vector
   '((name . "myproject")
     (path . "/home/user/project")
     (branch . "main")
     (is_main . t)
     (beads_state . "shared"))
   '((name . "feature-auth")
     (path . "/home/user/project/.git/worktrees/feature-auth")
     (branch . "feature-auth")
     (is_main . :json-false)
     (beads_state . "redirect")))
  "Sample worktree list JSON for testing.")

(defvar beads-command-worktree-test--sample-info-not-worktree-json
  '((is_worktree . :json-false))
  "Sample worktree info JSON when not in a worktree.")

(defvar beads-command-worktree-test--sample-info-worktree-json
  '((is_worktree . t)
    (name . "feature-auth")
    (path . "/home/user/project/.git/worktrees/feature-auth")
    (branch . "feature-auth")
    (main_path . "/home/user/project")
    (beads_state . "redirect"))
  "Sample worktree info JSON when in a linked worktree.")

;;; ============================================================
;;; beads-worktree Type Tests
;;; ============================================================

(ert-deftest beads-command-worktree-test-worktree-from-json ()
  "Test beads-worktree-from-json creates instance correctly."
  (let ((wt (beads-worktree-from-json
             beads-command-worktree-test--sample-worktree-json)))
    (should (beads-worktree-p wt))
    (should (string= (oref wt name) "feature-auth"))
    (should (string= (oref wt path)
                     "/home/user/project/.git/worktrees/feature-auth"))
    (should (string= (oref wt branch) "feature-auth"))
    (should-not (oref wt is-main))
    (should (string= (oref wt beads-state) "redirect"))))

(ert-deftest beads-command-worktree-test-main-worktree-from-json ()
  "Test beads-worktree-from-json for main worktree."
  (let ((wt (beads-worktree-from-json
             beads-command-worktree-test--sample-main-worktree-json)))
    (should (beads-worktree-p wt))
    (should (string= (oref wt name) "myproject"))
    (should (string= (oref wt path) "/home/user/project"))
    (should (string= (oref wt branch) "main"))
    (should (oref wt is-main))
    (should (string= (oref wt beads-state) "shared"))))

(ert-deftest beads-command-worktree-test-worktree-manual-creation ()
  "Test creating beads-worktree manually."
  (let ((wt (beads-worktree :name "test"
                            :path "/tmp/test"
                            :branch "feature"
                            :is-main nil
                            :beads-state "local")))
    (should (beads-worktree-p wt))
    (should (string= (oref wt name) "test"))
    (should (string= (oref wt path) "/tmp/test"))
    (should (string= (oref wt branch) "feature"))
    (should-not (oref wt is-main))
    (should (string= (oref wt beads-state) "local"))))

;;; ============================================================
;;; beads-worktree-info Type Tests
;;; ============================================================

(ert-deftest beads-command-worktree-test-info-not-worktree-from-json ()
  "Test beads-worktree-info-from-json when not in worktree."
  (let ((info (beads-worktree-info-from-json
               beads-command-worktree-test--sample-info-not-worktree-json)))
    (should (beads-worktree-info-p info))
    (should-not (oref info is-worktree))
    (should-not (oref info name))
    (should-not (oref info path))))

(ert-deftest beads-command-worktree-test-info-worktree-from-json ()
  "Test beads-worktree-info-from-json when in a linked worktree."
  (let ((info (beads-worktree-info-from-json
               beads-command-worktree-test--sample-info-worktree-json)))
    (should (beads-worktree-info-p info))
    (should (oref info is-worktree))
    (should (string= (oref info name) "feature-auth"))
    (should (string= (oref info path)
                     "/home/user/project/.git/worktrees/feature-auth"))
    (should (string= (oref info branch) "feature-auth"))
    (should (string= (oref info main-path) "/home/user/project"))
    (should (string= (oref info beads-state) "redirect"))))

;;; ============================================================
;;; beads-command-worktree-create Tests
;;; ============================================================

(ert-deftest beads-command-worktree-test-create-command-line-basic ()
  "Test beads-command-worktree-create builds correct command line."
  (let* ((cmd (beads-command-worktree-create :name "feature-x"))
         (args (beads-command-line cmd)))
    ;; Should start with executable
    (should (string= (car args) "bd"))
    ;; Should have worktree create subcommand
    (should (member "worktree" args))
    (should (member "create" args))
    ;; Should have --json by default
    (should (member "--json" args))
    ;; Should have the name
    (should (member "feature-x" args))))

(ert-deftest beads-command-worktree-test-create-command-line-with-branch ()
  "Test beads-command-worktree-create with --branch flag."
  (let* ((cmd (beads-command-worktree-create :name "wt-1"
                                              :branch "feature/auth"))
         (args (beads-command-line cmd)))
    (should (member "wt-1" args))
    (should (member "--branch" args))
    (should (member "feature/auth" args))))

(ert-deftest beads-command-worktree-test-create-command-line-no-json ()
  "Test beads-command-worktree-create with :json nil."
  (let* ((cmd (beads-command-worktree-create :name "test" :json nil))
         (args (beads-command-line cmd)))
    (should-not (member "--json" args))
    (should (member "test" args))))

(ert-deftest beads-command-worktree-test-create-command-line-with-global-flags ()
  "Test beads-command-worktree-create with global flags."
  (let* ((cmd (beads-command-worktree-create :name "test"
                                              :actor "alice"
                                              :no-daemon t))
         (args (beads-command-line cmd)))
    (should (member "--actor" args))
    (should (member "alice" args))
    (should (member "--no-daemon" args))))

(ert-deftest beads-command-worktree-test-create-validate-missing-name ()
  "Test validation fails when name is missing."
  (let* ((cmd (beads-command-worktree-create))
         (error (beads-command-validate cmd)))
    (should error)
    (should (string-match-p "name" error))))

(ert-deftest beads-command-worktree-test-create-validate-empty-name ()
  "Test validation fails when name is empty."
  (let* ((cmd (beads-command-worktree-create :name ""))
         (error (beads-command-validate cmd)))
    (should error)
    (should (string-match-p "empty" error))))

(ert-deftest beads-command-worktree-test-create-validate-success ()
  "Test validation succeeds with valid name."
  (let* ((cmd (beads-command-worktree-create :name "valid-name"))
         (error (beads-command-validate cmd)))
    (should-not error)))

(ert-deftest beads-command-worktree-test-create-subcommand ()
  "Test beads-command-subcommand returns correct value."
  (let ((cmd (beads-command-worktree-create)))
    (should (string= (beads-command-subcommand cmd) "worktree create"))))

;;; ============================================================
;;; beads-command-worktree-list Tests
;;; ============================================================

(ert-deftest beads-command-worktree-test-list-command-line-basic ()
  "Test beads-command-worktree-list builds correct command line."
  (let* ((cmd (beads-command-worktree-list))
         (args (beads-command-line cmd)))
    (should (string= (car args) "bd"))
    (should (member "worktree" args))
    (should (member "list" args))
    (should (member "--json" args))))

(ert-deftest beads-command-worktree-test-list-command-line-no-json ()
  "Test beads-command-worktree-list with :json nil."
  (let* ((cmd (beads-command-worktree-list :json nil))
         (args (beads-command-line cmd)))
    (should-not (member "--json" args))))

(ert-deftest beads-command-worktree-test-list-subcommand ()
  "Test beads-command-subcommand returns correct value."
  (let ((cmd (beads-command-worktree-list)))
    (should (string= (beads-command-subcommand cmd) "worktree list"))))

;;; ============================================================
;;; beads-command-worktree-remove Tests
;;; ============================================================

(ert-deftest beads-command-worktree-test-remove-command-line-basic ()
  "Test beads-command-worktree-remove builds correct command line."
  (let* ((cmd (beads-command-worktree-remove :name "old-feature"))
         (args (beads-command-line cmd)))
    (should (string= (car args) "bd"))
    (should (member "worktree" args))
    (should (member "remove" args))
    (should (member "--json" args))
    (should (member "old-feature" args))))

(ert-deftest beads-command-worktree-test-remove-command-line-with-force ()
  "Test beads-command-worktree-remove with --force flag."
  (let* ((cmd (beads-command-worktree-remove :name "stale" :force t))
         (args (beads-command-line cmd)))
    (should (member "stale" args))
    (should (member "--force" args))))

(ert-deftest beads-command-worktree-test-remove-validate-missing-name ()
  "Test validation fails when name is missing."
  (let* ((cmd (beads-command-worktree-remove))
         (error (beads-command-validate cmd)))
    (should error)
    (should (string-match-p "name" error))))

(ert-deftest beads-command-worktree-test-remove-validate-success ()
  "Test validation succeeds with valid name."
  (let* ((cmd (beads-command-worktree-remove :name "to-remove"))
         (error (beads-command-validate cmd)))
    (should-not error)))

(ert-deftest beads-command-worktree-test-remove-subcommand ()
  "Test beads-command-subcommand returns correct value."
  (let ((cmd (beads-command-worktree-remove)))
    (should (string= (beads-command-subcommand cmd) "worktree remove"))))

;;; ============================================================
;;; beads-command-worktree-info Tests
;;; ============================================================

(ert-deftest beads-command-worktree-test-info-command-line-basic ()
  "Test beads-command-worktree-info builds correct command line."
  (let* ((cmd (beads-command-worktree-info))
         (args (beads-command-line cmd)))
    (should (string= (car args) "bd"))
    (should (member "worktree" args))
    (should (member "info" args))
    (should (member "--json" args))))

(ert-deftest beads-command-worktree-test-info-command-line-no-json ()
  "Test beads-command-worktree-info with :json nil."
  (let* ((cmd (beads-command-worktree-info :json nil))
         (args (beads-command-line cmd)))
    (should-not (member "--json" args))))

(ert-deftest beads-command-worktree-test-info-subcommand ()
  "Test beads-command-subcommand returns correct value."
  (let ((cmd (beads-command-worktree-info)))
    (should (string= (beads-command-subcommand cmd) "worktree info"))))

;;; ============================================================
;;; Global Flags Tests
;;; ============================================================

(ert-deftest beads-command-worktree-test-global-flags-in-command-line ()
  "Test that global flags are included in command line."
  (let* ((cmd (beads-command-worktree-list :actor "bob"
                                            :no-daemon t
                                            :sandbox t))
         (args (beads-command-line cmd)))
    (should (member "--actor" args))
    (should (member "bob" args))
    (should (member "--no-daemon" args))
    (should (member "--sandbox" args))))

;;; ============================================================
;;; Parse Method Tests (with mocked data)
;;; ============================================================

(ert-deftest beads-command-worktree-test-create-parse ()
  "Test beads-command-worktree-create parse method."
  (let ((cmd (beads-command-worktree-create :name "test")))
    ;; Simulate execution results
    (oset cmd exit-code 0)
    (oset cmd stdout (json-encode
                      beads-command-worktree-test--sample-worktree-json))
    (oset cmd stderr "")
    ;; Parse the result
    (let ((result (beads-command-parse cmd)))
      (should (beads-worktree-p result))
      (should (string= (oref result name) "feature-auth")))))

(ert-deftest beads-command-worktree-test-list-parse ()
  "Test beads-command-worktree-list parse method."
  (let ((cmd (beads-command-worktree-list)))
    ;; Simulate execution results
    (oset cmd exit-code 0)
    (oset cmd stdout (json-encode
                      beads-command-worktree-test--sample-worktree-list-json))
    (oset cmd stderr "")
    ;; Parse the result
    (let ((result (beads-command-parse cmd)))
      (should (listp result))
      (should (= (length result) 2))
      (should (beads-worktree-p (car result)))
      (should (beads-worktree-p (cadr result)))
      ;; First should be main
      (should (oref (car result) is-main))
      ;; Second should not be main
      (should-not (oref (cadr result) is-main)))))

(ert-deftest beads-command-worktree-test-info-parse ()
  "Test beads-command-worktree-info parse method."
  (let ((cmd (beads-command-worktree-info)))
    ;; Simulate execution results
    (oset cmd exit-code 0)
    (oset cmd stdout (json-encode
                      beads-command-worktree-test--sample-info-worktree-json))
    (oset cmd stderr "")
    ;; Parse the result
    (let ((result (beads-command-parse cmd)))
      (should (beads-worktree-info-p result))
      (should (oref result is-worktree))
      (should (string= (oref result name) "feature-auth")))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-command-worktree-test-list-integration ()
  "Integration test for beads-command-worktree-list.
Requires bd to be installed and a git repo."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  (skip-unless (executable-find "git"))
  (beads-test-with-temp-repo (:init-beads t)
    ;; List worktrees - should at least have the main worktree
    (let ((worktrees (beads-command-worktree-list!)))
      (should (listp worktrees))
      (should (>= (length worktrees) 1))
      ;; First/main worktree should exist
      (let ((main (seq-find (lambda (wt) (oref wt is-main)) worktrees)))
        (should main)
        (should (oref main path))))))

(ert-deftest beads-command-worktree-test-info-integration ()
  "Integration test for beads-command-worktree-info.
Requires bd to be installed and a git repo."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  (skip-unless (executable-find "git"))
  (beads-test-with-temp-repo (:init-beads t)
    ;; Get info about current directory
    (let ((info (beads-command-worktree-info!)))
      (should (beads-worktree-info-p info))
      ;; In a fresh git repo, we should not be in a linked worktree
      ;; but is_worktree might be false or we might be in main
      (should info))))

(ert-deftest beads-command-worktree-test-create-remove-integration ()
  "Integration test for create and remove worktree.
Requires bd to be installed and a git repo."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  (skip-unless (executable-find "git"))
  (beads-test-with-temp-repo (:init-beads t)
    ;; Create a worktree
    (let ((wt (beads-command-worktree-create! :name "test-worktree")))
      (should (beads-worktree-p wt))
      (should (string= (oref wt name) "test-worktree"))
      ;; Verify it appears in the list
      (let ((worktrees (beads-command-worktree-list!)))
        (should (seq-find (lambda (w)
                            (string= (oref w name) "test-worktree"))
                          worktrees)))
      ;; Remove it
      (beads-command-worktree-remove! :name "test-worktree" :force t)
      ;; Verify it's gone
      (let ((worktrees (beads-command-worktree-list!)))
        (should-not (seq-find (lambda (w)
                                (string= (oref w name) "test-worktree"))
                              worktrees))))))

(provide 'beads-command-worktree-test)
;;; beads-command-worktree-test.el ends here
