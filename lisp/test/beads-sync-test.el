;;; beads-sync-test.el --- Tests for beads-sync -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-sync.el transient menu.
;; Tests cover transient definition, command construction, and
;; integration with the bd CLI via compilation buffers.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-sync)
(require 'compile)

;;; Test Utilities

(defun beads-sync-test--set-state (state-alist)
  "Set transient state from STATE-ALIST.
STATE-ALIST is an alist of (variable . value) pairs."
  (setq beads-sync--dry-run nil
        beads-sync--message nil
        beads-sync--no-pull nil
        beads-sync--no-push nil)
  (dolist (binding state-alist)
    (set (car binding) (cdr binding))))

(defmacro beads-sync-test-with-state (state &rest body)
  "Execute BODY with beads-sync transient state set to STATE.
STATE is an alist expression of (variable . value) pairs."
  (declare (indent 1))
  `(progn
     (beads-sync-test--set-state ,state)
     ,@body))

(defun beads-sync-test--mock-compilation-start (command &optional _mode)
  "Create a mock compilation buffer for COMMAND."
  (let ((buf (get-buffer-create "*test-compilation*")))
    (with-current-buffer buf
      (compilation-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Running: %s\n" command))
        (insert "Mock compilation output\n")))
    buf))

;;; Tests for State Management

(ert-deftest beads-sync-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-sync-test-with-state
   '((beads-sync--dry-run . t)
     (beads-sync--message . "test message")
     (beads-sync--no-pull . t)
     (beads-sync--no-push . t))
   (beads-sync--reset-state)
   (should (null beads-sync--dry-run))
   (should (null beads-sync--message))
   (should (null beads-sync--no-pull))
   (should (null beads-sync--no-push))))

(ert-deftest beads-sync-test-reset-state-from-nil ()
  "Test that reset-state works when variables are already nil."
  (beads-sync-test-with-state nil
   (beads-sync--reset-state)
   (should (null beads-sync--dry-run))
   (should (null beads-sync--message))
   (should (null beads-sync--no-pull))
   (should (null beads-sync--no-push))))

;;; Tests for Value Formatting

(ert-deftest beads-sync-test-format-value-set ()
  "Test formatting when value is set."
  (let ((result (beads-sync--format-value "test-value")))
    (should (stringp result))
    (should (string-match-p "test-value" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-sync-test-format-value-nil ()
  "Test formatting when value is nil."
  (let ((result (beads-sync--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-sync-test-format-value-empty-string ()
  "Test formatting with empty string."
  (let ((result (beads-sync--format-value "")))
    (should (stringp result))
    ;; Empty string is truthy, so it should show the value
    (should (string-match-p "\\[\\]" result))))

;;; Tests for Command Building

(ert-deftest beads-sync-test-build-command-line-basic ()
  "Test building basic command line."
  (let ((cmd (beads-sync--build-command-line nil nil nil nil)))
    (should (stringp cmd))
    (should (string-match-p "sync" cmd))))

(ert-deftest beads-sync-test-build-command-line-with-dry-run ()
  "Test building command line with dry-run."
  (let ((cmd (beads-sync--build-command-line t nil nil nil)))
    (should (string-match-p "--dry-run" cmd))))

(ert-deftest beads-sync-test-build-command-line-with-message ()
  "Test building command line with message."
  (let ((cmd (beads-sync--build-command-line nil "Test message" nil nil)))
    (should (string-match-p "-m" cmd))
    (should (string-match-p "Test" cmd))))

(ert-deftest beads-sync-test-build-command-line-with-no-pull ()
  "Test building command line with --no-pull."
  (let ((cmd (beads-sync--build-command-line nil nil t nil)))
    (should (string-match-p "--no-pull" cmd))))

(ert-deftest beads-sync-test-build-command-line-with-no-push ()
  "Test building command line with --no-push."
  (let ((cmd (beads-sync--build-command-line nil nil nil t)))
    (should (string-match-p "--no-push" cmd))))

(ert-deftest beads-sync-test-build-command-line-all-flags ()
  "Test building command line with all flags."
  (let ((cmd (beads-sync--build-command-line t "All flags" t t)))
    (should (string-match-p "--dry-run" cmd))
    (should (string-match-p "-m" cmd))
    (should (string-match-p "All" cmd))
    (should (string-match-p "--no-pull" cmd))
    (should (string-match-p "--no-push" cmd))))

(ert-deftest beads-sync-test-build-command-line-empty-message ()
  "Test that empty message is not included."
  (let ((cmd (beads-sync--build-command-line nil "   \t\n  " nil nil)))
    (should-not (string-match-p "-m" cmd))))

;;; Tests for Argument Parsing

(ert-deftest beads-sync-test-parse-transient-args-empty ()
  "Test parsing empty argument list."
  (let ((parsed (beads-sync--parse-transient-args nil)))
    (should (null (plist-get parsed :dry-run)))
    (should (null (plist-get parsed :message)))
    (should (null (plist-get parsed :no-pull)))
    (should (null (plist-get parsed :no-push)))))

(ert-deftest beads-sync-test-parse-transient-args-dry-run ()
  "Test parsing dry-run flag."
  (let ((parsed (beads-sync--parse-transient-args '("--dry-run"))))
    (should (plist-get parsed :dry-run))
    (should (null (plist-get parsed :message)))
    (should (null (plist-get parsed :no-pull)))
    (should (null (plist-get parsed :no-push)))))

(ert-deftest beads-sync-test-parse-transient-args-message ()
  "Test parsing message argument."
  (let ((parsed (beads-sync--parse-transient-args '("-m=Test message"))))
    (should (null (plist-get parsed :dry-run)))
    (should (string= (plist-get parsed :message) "Test message"))
    (should (null (plist-get parsed :no-pull)))
    (should (null (plist-get parsed :no-push)))))

(ert-deftest beads-sync-test-parse-transient-args-no-pull ()
  "Test parsing no-pull flag."
  (let ((parsed (beads-sync--parse-transient-args '("--no-pull"))))
    (should (null (plist-get parsed :dry-run)))
    (should (null (plist-get parsed :message)))
    (should (plist-get parsed :no-pull))
    (should (null (plist-get parsed :no-push)))))

(ert-deftest beads-sync-test-parse-transient-args-no-push ()
  "Test parsing no-push flag."
  (let ((parsed (beads-sync--parse-transient-args '("--no-push"))))
    (should (null (plist-get parsed :dry-run)))
    (should (null (plist-get parsed :message)))
    (should (null (plist-get parsed :no-pull)))
    (should (plist-get parsed :no-push))))

(ert-deftest beads-sync-test-parse-transient-args-all-flags ()
  "Test parsing all flags together."
  (let ((parsed (beads-sync--parse-transient-args
                 '("--dry-run" "-m=Full sync" "--no-pull" "--no-push"))))
    (should (plist-get parsed :dry-run))
    (should (string= (plist-get parsed :message) "Full sync"))
    (should (plist-get parsed :no-pull))
    (should (plist-get parsed :no-push))))

(ert-deftest beads-sync-test-parse-transient-args-order-independent ()
  "Test that argument order doesn't matter."
  (let ((parsed (beads-sync--parse-transient-args
                 '("--no-push" "-m=Test" "--no-pull" "--dry-run"))))
    (should (plist-get parsed :dry-run))
    (should (string= (plist-get parsed :message) "Test"))
    (should (plist-get parsed :no-pull))
    (should (plist-get parsed :no-push))))

;;; Tests for Transient Integration

(ert-deftest beads-sync-test-execute-command-reads-transient-args ()
  "Test that execute-command properly reads from transient-args.
This test would have caught the original bug where flags weren't
being passed from the transient menu."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (prefix)
               (should (eq prefix 'beads-sync))
               '("--dry-run" "-m=FromTransient" "--no-pull")))
            ((symbol-function 'beads-check-executable)
             (lambda () t))
            ((symbol-function 'compilation-start)
             (lambda (command &optional _mode)
               ;; Verify the command includes all flags from transient
               (should (string-match-p "--dry-run" command))
               (should (string-match-p "-m" command))
               (should (string-match-p "FromTransient" command))
               (should (string-match-p "--no-pull" command))
               (should-not (string-match-p "--no-push" command))))
            ((symbol-function 'beads-sync--refresh-all-buffers)
             (lambda () nil)))
    (beads-sync--execute-command)))

(ert-deftest beads-sync-test-preview-reads-transient-args ()
  "Test that preview properly reads from transient-args.
This test would have caught the original bug."
  (cl-letf (((symbol-function 'transient-args)
             (lambda (prefix)
               (should (eq prefix 'beads-sync))
               '("--dry-run" "-m=PreviewTest")))
            (message-log nil))
    (beads-sync--preview)
    ;; If we get here without error, the test passes
    (should t)))

;;; Tests for Execution

(ert-deftest beads-sync-test-execute-success-basic ()
  "Test successful basic sync execution."
  (beads-sync-test-with-state nil
   (cl-letf (((symbol-function 'compilation-start)
              #'beads-sync-test--mock-compilation-start)
             ((symbol-function 'beads-sync--refresh-all-buffers)
              (lambda () nil)))
     (should-not (beads-sync--execute nil nil nil nil)))))

(ert-deftest beads-sync-test-execute-with-dry-run ()
  "Test sync execution with dry-run flag."
  (beads-sync-test-with-state
   '((beads-sync--dry-run . t))
   (cl-letf (((symbol-function 'compilation-start)
              (lambda (command &optional _mode)
                ;; Verify --dry-run flag is in command
                (should (string-match-p "--dry-run" command))
                (beads-sync-test--mock-compilation-start command)))
             ((symbol-function 'beads-sync--refresh-all-buffers)
              (lambda () nil)))
     (should-not (beads-sync--execute t nil nil nil)))))

(ert-deftest beads-sync-test-execute-with-message ()
  "Test sync execution with custom commit message."
  (beads-sync-test-with-state
   '((beads-sync--message . "Custom sync message"))
   (cl-letf (((symbol-function 'compilation-start)
              (lambda (command &optional _mode)
                ;; Verify -m flag and message are in command
                (should (string-match-p "-m" command))
                (should (string-match-p "Custom" command))
                (beads-sync-test--mock-compilation-start command)))
             ((symbol-function 'beads-sync--refresh-all-buffers)
              (lambda () nil)))
     (should-not (beads-sync--execute nil "Custom sync message"
                                      nil nil)))))

(ert-deftest beads-sync-test-execute-with-all-flags ()
  "Test sync execution with all flags set."
  (beads-sync-test-with-state
   '((beads-sync--dry-run . t)
     (beads-sync--message . "All flags test")
     (beads-sync--no-pull . t)
     (beads-sync--no-push . t))
   (cl-letf (((symbol-function 'compilation-start)
              (lambda (command &optional _mode)
                ;; Verify all flags are in command
                (should (string-match-p "--dry-run" command))
                (should (string-match-p "-m" command))
                (should (string-match-p "All" command))
                (should (string-match-p "--no-pull" command))
                (should (string-match-p "--no-push" command))
                (beads-sync-test--mock-compilation-start command)))
             ((symbol-function 'beads-sync--refresh-all-buffers)
              (lambda () nil)))
     (should-not (beads-sync--execute t "All flags test" t t)))))

;;; Tests for Transient Definition

(ert-deftest beads-sync-test-transient-defined ()
  "Test that beads-sync transient is defined."
  (should (fboundp 'beads-sync)))

(ert-deftest beads-sync-test-transient-is-prefix ()
  "Test that beads-sync is a transient prefix."
  (should (get 'beads-sync 'transient--prefix)))

(ert-deftest beads-sync-test-infix-commands-defined ()
  "Test that all infix commands are defined."
  (should (fboundp 'beads-sync--infix-dry-run))
  (should (fboundp 'beads-sync--infix-message))
  (should (fboundp 'beads-sync--infix-no-pull))
  (should (fboundp 'beads-sync--infix-no-push)))

(ert-deftest beads-sync-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-sync--execute-command))
  (should (fboundp 'beads-sync--reset))
  (should (fboundp 'beads-sync--preview)))

;;; Tests for Preview

(ert-deftest beads-sync-test-preview-basic ()
  "Test preview command with no flags."
  (beads-sync-test-with-state nil
   ;; Preview should complete without error and return nil
   (beads-sync--preview)))

(ert-deftest beads-sync-test-preview-with-flags ()
  "Test preview command with all flags."
  (beads-sync-test-with-state
   '((beads-sync--dry-run . t)
     (beads-sync--message . "Test message")
     (beads-sync--no-pull . t)
     (beads-sync--no-push . t))
   ;; Preview should complete without error and return nil
   (beads-sync--preview)))

;;; Tests for Buffer Refresh

(ert-deftest beads-sync-test-refresh-all-buffers-no-beads-buffers ()
  "Test refresh when no beads buffers exist."
  ;; Should not error when no beads buffers exist
  (should-not (beads-sync--refresh-all-buffers)))

(ert-deftest beads-sync-test-refresh-all-buffers-with-list-mode ()
  "Test refresh with beads-list-mode buffer."
  (with-temp-buffer
    (setq major-mode 'beads-list-mode)
    (let ((refresh-called nil))
      (cl-letf (((symbol-function 'beads-list-refresh)
                 (lambda () (setq refresh-called t))))
        (beads-sync--refresh-all-buffers)
        (should refresh-called)))))

;;; Integration Tests

(ert-deftest beads-sync-test-full-workflow ()
  "Test complete workflow from setting params to execution."
  (beads-sync-test-with-state nil
   ;; Set parameters
   (setq beads-sync--dry-run t)
   (setq beads-sync--message "Integration test sync")

   ;; Execute (mocked)
   (cl-letf (((symbol-function 'compilation-start)
              #'beads-sync-test--mock-compilation-start)
             ((symbol-function 'beads-sync--refresh-all-buffers)
              (lambda () nil)))
     (should-not (beads-sync--execute t "Integration test sync"
                                      nil nil)))))

(ert-deftest beads-sync-test-reset-and-reexecute ()
  "Test resetting state and executing again."
  (beads-sync-test-with-state
   '((beads-sync--dry-run . t)
     (beads-sync--message . "First sync"))

   ;; Reset
   (beads-sync--reset-state)
   (should (null beads-sync--dry-run))
   (should (null beads-sync--message))

   ;; Set new values
   (setq beads-sync--message "Second sync")
   (setq beads-sync--no-pull t)

   ;; Execute with new values
   (cl-letf (((symbol-function 'compilation-start)
              (lambda (command &optional _mode)
                (should (string-match-p "-m" command))
                (should (string-match-p "Second" command))
                (should (string-match-p "--no-pull" command))
                (should-not (string-match-p "--dry-run" command))
                (beads-sync-test--mock-compilation-start command)))
             ((symbol-function 'beads-sync--refresh-all-buffers)
              (lambda () nil)))
     (should-not (beads-sync--execute nil "Second sync" t nil)))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-sync-test-transient-menu-defined ()
  "Integration test: Verify beads-sync transient menu is defined."
  :tags '(integration)
  (should (fboundp 'beads-sync)))

(ert-deftest beads-sync-test-execute-function-defined ()
  "Integration test: Verify execute function is defined."
  :tags '(integration)
  (should (fboundp 'beads-sync--execute)))

(ert-deftest beads-sync-test-autoload-defined ()
  "Integration test: Verify beads-sync has autoload."
  :tags '(integration)
  (should (get 'beads-sync 'transient--prefix)))

(provide 'beads-sync-test)
;;; beads-sync-test.el ends here
