;;; beads-worktree-test.el --- Tests for beads-worktree -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-worktree.el transient menu.
;; Tests cover:
;; - State management and reset functions
;; - Validation functions
;; - Suffix commands (create, list, remove, info)
;; - List mode and entry formatting
;; - Helper functions

;;; Code:

(require 'ert)
(require 'beads-command-worktree)
(require 'beads-command-worktree)

;;; Test Fixtures

(defvar beads-worktree-test--mock-worktrees nil
  "Mock worktree data for tests.")

(defun beads-worktree-test--make-mock-worktrees ()
  "Create mock worktree objects for testing."
  (list
   (beads-worktree
    :name "main"
    :path "/path/to/repo"
    :branch "main"
    :is-main t
    :beads-state "shared")
   (beads-worktree
    :name "feature-auth"
    :path "/path/to/repo-feature-auth"
    :branch "feature/auth"
    :is-main nil
    :beads-state "redirect")
   (beads-worktree
    :name "bugfix"
    :path "/path/to/repo-bugfix"
    :branch "bugfix/login"
    :is-main nil
    :beads-state "none")))

;;; ============================================================
;;; Tests for State Variables
;;; ============================================================

(ert-deftest beads-worktree-test-state-variables-exist ()
  "Test that all state variables are defined."
  (should (boundp 'beads-worktree--name))
  (should (boundp 'beads-worktree--branch))
  (should (boundp 'beads-worktree--force))
  (should (boundp 'beads-worktree--target)))

;;; ============================================================
;;; Tests for Reset Functions
;;; ============================================================

(ert-deftest beads-worktree-test-reset-create-state ()
  "Test resetting create state variables."
  (let ((beads-worktree--name "test-name")
        (beads-worktree--branch "test-branch"))
    (beads-worktree--reset-create-state)
    (should (null beads-worktree--name))
    (should (null beads-worktree--branch))))

(ert-deftest beads-worktree-test-reset-remove-state ()
  "Test resetting remove state variables."
  (let ((beads-worktree--force t)
        (beads-worktree--target "test-target"))
    (beads-worktree--reset-remove-state)
    (should (null beads-worktree--force))
    (should (null beads-worktree--target))))

(ert-deftest beads-worktree-test-reset-all-state ()
  "Test resetting all state variables."
  (let ((beads-worktree--name "test-name")
        (beads-worktree--branch "test-branch")
        (beads-worktree--force t)
        (beads-worktree--target "test-target"))
    (beads-worktree--reset-all-state)
    (should (null beads-worktree--name))
    (should (null beads-worktree--branch))
    (should (null beads-worktree--force))
    (should (null beads-worktree--target))))

;;; ============================================================
;;; Tests for Validation Functions
;;; ============================================================

(ert-deftest beads-worktree-test-validate-create-no-name ()
  "Test validation fails when name is nil."
  (let ((beads-worktree--name nil))
    (should (beads-worktree--validate-create))))

(ert-deftest beads-worktree-test-validate-create-empty-name ()
  "Test validation fails when name is empty string."
  (let ((beads-worktree--name ""))
    (should (beads-worktree--validate-create))))

(ert-deftest beads-worktree-test-validate-create-valid ()
  "Test validation succeeds with valid name."
  (let ((beads-worktree--name "feature-auth"))
    (should-not (beads-worktree--validate-create))))

(ert-deftest beads-worktree-test-validate-remove-no-target ()
  "Test validation fails when target is nil."
  (let ((beads-worktree--target nil))
    (should (beads-worktree--validate-remove))))

(ert-deftest beads-worktree-test-validate-remove-empty-target ()
  "Test validation fails when target is empty string."
  (let ((beads-worktree--target ""))
    (should (beads-worktree--validate-remove))))

(ert-deftest beads-worktree-test-validate-remove-valid ()
  "Test validation succeeds with valid target."
  (let ((beads-worktree--target "feature-auth"))
    (should-not (beads-worktree--validate-remove))))

;;; ============================================================
;;; Tests for Helper Functions
;;; ============================================================

(ert-deftest beads-worktree-test-find-by-name-exists ()
  "Test that beads-worktree-find-by-name is defined."
  (should (fboundp 'beads-worktree-find-by-name)))

(ert-deftest beads-worktree-test-find-by-name-found ()
  "Test finding worktree by name when it exists."
  (cl-letf (((symbol-function 'beads-command-worktree-list!)
             #'beads-worktree-test--make-mock-worktrees))
    (let ((result (beads-worktree-find-by-name "feature-auth")))
      (should result)
      (should (equal (oref result name) "feature-auth"))
      (should (equal (oref result branch) "feature/auth")))))

(ert-deftest beads-worktree-test-find-by-name-not-found ()
  "Test finding worktree by name when it doesn't exist."
  (cl-letf (((symbol-function 'beads-command-worktree-list!)
             #'beads-worktree-test--make-mock-worktrees))
    (let ((result (beads-worktree-find-by-name "nonexistent")))
      (should-not result))))

(ert-deftest beads-worktree-test-find-by-name-handles-error ()
  "Test that find-by-name handles errors gracefully."
  (cl-letf (((symbol-function 'beads-command-worktree-list!)
             (lambda () (error "bd command failed"))))
    (let ((result (beads-worktree-find-by-name "any")))
      (should-not result))))

;;; ============================================================
;;; Tests for Entry Formatting
;;; ============================================================

(ert-deftest beads-worktree-test-format-entry-basic ()
  "Test basic worktree entry formatting."
  (let* ((worktree (beads-worktree
                    :name "feature-auth"
                    :path "/path/to/repo-feature-auth"
                    :branch "feature/auth"
                    :is-main nil
                    :beads-state "redirect"))
         (entry (beads-worktree--format-entry worktree)))
    (should (equal (car entry) "feature-auth"))
    (should (vectorp (cadr entry)))
    (should (= (length (cadr entry)) 5))))

(ert-deftest beads-worktree-test-format-entry-main-worktree ()
  "Test formatting entry for main worktree."
  (let* ((worktree (beads-worktree
                    :name "main"
                    :path "/path/to/repo"
                    :branch "main"
                    :is-main t
                    :beads-state "shared"))
         (entry (beads-worktree--format-entry worktree))
         (vec (cadr entry)))
    ;; Main column should show "Yes"
    (should (string-match-p "Yes" (aref vec 3)))))

(ert-deftest beads-worktree-test-format-entry-non-main ()
  "Test formatting entry for non-main worktree."
  (let* ((worktree (beads-worktree
                    :name "feature"
                    :path "/path/to/feature"
                    :branch "feature-branch"
                    :is-main nil
                    :beads-state "redirect"))
         (entry (beads-worktree--format-entry worktree))
         (vec (cadr entry)))
    ;; Main column should be empty
    (should (string-empty-p (aref vec 3)))))

;;; ============================================================
;;; Tests for Transient Definition
;;; ============================================================

(ert-deftest beads-worktree-test-transient-defined ()
  "Test that beads-worktree-menu transient prefix is defined."
  (should (fboundp 'beads-worktree-menu)))

(ert-deftest beads-worktree-test-transient-is-prefix ()
  "Test that beads-worktree-menu is a transient prefix."
  (should (get 'beads-worktree-menu 'transient--prefix)))

;;; ============================================================
;;; Tests for Suffix Commands
;;; ============================================================

(ert-deftest beads-worktree-test-suffix-create-defined ()
  "Test that create suffix is defined."
  (should (fboundp 'beads-worktree--create)))

(ert-deftest beads-worktree-test-suffix-list-defined ()
  "Test that list suffix is defined."
  (should (fboundp 'beads-worktree--list)))

(ert-deftest beads-worktree-test-suffix-remove-defined ()
  "Test that remove suffix is defined."
  (should (fboundp 'beads-worktree--remove)))

(ert-deftest beads-worktree-test-suffix-info-defined ()
  "Test that info suffix is defined."
  (should (fboundp 'beads-worktree--info)))

(ert-deftest beads-worktree-test-suffix-reset-defined ()
  "Test that reset suffix is defined."
  (should (fboundp 'beads-worktree--reset)))

;;; ============================================================
;;; Tests for List Mode
;;; ============================================================

(ert-deftest beads-worktree-test-list-mode-defined ()
  "Test that beads-worktree-list-mode is defined."
  (should (fboundp 'beads-worktree-list-mode)))

(ert-deftest beads-worktree-test-list-mode-keymap ()
  "Test that list mode has expected keybindings."
  (should (keymapp beads-worktree-list-mode-map))
  (should (lookup-key beads-worktree-list-mode-map (kbd "RET")))
  (should (lookup-key beads-worktree-list-mode-map (kbd "d")))
  (should (lookup-key beads-worktree-list-mode-map (kbd "g")))
  (should (lookup-key beads-worktree-list-mode-map (kbd "c")))
  (should (lookup-key beads-worktree-list-mode-map (kbd "q"))))

(ert-deftest beads-worktree-test-list-commands-defined ()
  "Test that list mode commands are defined."
  (should (fboundp 'beads-worktree-list-info))
  (should (fboundp 'beads-worktree-list-remove))
  (should (fboundp 'beads-worktree-list-refresh))
  (should (fboundp 'beads-worktree-list-create)))

;;; ============================================================
;;; Tests for Infix Commands
;;; ============================================================

(ert-deftest beads-worktree-test-infix-name-defined ()
  "Test that name infix is defined."
  (should (fboundp 'beads-worktree--infix-name)))

(ert-deftest beads-worktree-test-infix-branch-defined ()
  "Test that branch infix is defined."
  (should (fboundp 'beads-worktree--infix-branch)))

(ert-deftest beads-worktree-test-infix-force-defined ()
  "Test that force infix is defined."
  (should (fboundp 'beads-worktree--infix-force)))

(ert-deftest beads-worktree-test-infix-target-defined ()
  "Test that target infix is defined."
  (should (fboundp 'beads-worktree--infix-target)))

;;; ============================================================
;;; Tests for Create Workflow
;;; ============================================================

(ert-deftest beads-worktree-test-create-validation-error ()
  "Test create suffix handles validation error."
  (let ((beads-worktree--name nil))
    (should-error (beads-worktree--create) :type 'user-error)))

(ert-deftest beads-worktree-test-create-success ()
  "Test create suffix succeeds with valid input."
  (let ((beads-worktree--name "test-worktree")
        (beads-worktree--branch "test-branch")
        (created-worktree (beads-worktree
                          :name "test-worktree"
                          :path "/path/to/test-worktree"
                          :branch "test-branch"
                          :is-main nil
                          :beads-state "redirect")))
    (cl-letf (((symbol-function 'beads-command-worktree-create!)
               (lambda (&rest _args) created-worktree))
              ((symbol-function 'beads-completion-invalidate-worktree-cache)
               #'ignore))
      (beads-worktree--create)
      ;; State should be reset after success
      (should (null beads-worktree--name))
      (should (null beads-worktree--branch)))))

;;; ============================================================
;;; Tests for List Workflow
;;; ============================================================

(ert-deftest beads-worktree-test-list-empty ()
  "Test list suffix handles empty worktree list."
  (cl-letf (((symbol-function 'beads-command-worktree-list!)
             (lambda () nil)))
    ;; Should not error, just message
    (beads-worktree--list)))

(ert-deftest beads-worktree-test-list-with-worktrees ()
  "Test list suffix displays worktrees."
  (let ((displayed nil))
    (cl-letf (((symbol-function 'beads-command-worktree-list!)
               #'beads-worktree-test--make-mock-worktrees)
              ((symbol-function 'beads-worktree--display-list)
               (lambda (wts) (setq displayed wts))))
      (beads-worktree--list)
      (should displayed)
      (should (= 3 (length displayed))))))

;;; ============================================================
;;; Tests for Info Workflow
;;; ============================================================

(ert-deftest beads-worktree-test-info-not-worktree ()
  "Test info suffix when not in a worktree."
  (let ((info (beads-worktree-info
              :is-worktree nil
              :name nil
              :path "/path/to/repo"
              :branch "main"
              :main-path nil
              :beads-state "shared")))
    (cl-letf (((symbol-function 'beads-command-worktree-info!)
               (lambda () info)))
      ;; Should not error
      (beads-worktree--info))))

(ert-deftest beads-worktree-test-info-in-worktree ()
  "Test info suffix when in a worktree."
  (let ((info (beads-worktree-info
              :is-worktree t
              :name "feature-auth"
              :path "/path/to/feature-auth"
              :branch "feature/auth"
              :main-path "/path/to/repo"
              :beads-state "redirect")))
    (cl-letf (((symbol-function 'beads-command-worktree-info!)
               (lambda () info)))
      ;; Should not error
      (beads-worktree--info))))

(provide 'beads-worktree-test)
;;; beads-worktree-test.el ends here
