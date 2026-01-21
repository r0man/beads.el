;;; beads-command-comments-test.el --- Tests for beads-command-comments -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-command-comments.el.
;; Tests cover command class behavior, parsing, validation, and execution.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads-command-comments)

;;; Test Utilities

(defun beads-comments-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; ============================================================
;;; Tests for beads-command-comments (list)
;;; ============================================================

(ert-deftest beads-comments-test-class-exists ()
  "Test that beads-command-comments class is defined."
  (should (cl-find-class 'beads-command-comments)))

(ert-deftest beads-comments-test-subcommand ()
  "Test that subcommand returns 'comments'."
  (let ((cmd (beads-command-comments)))
    (should (equal (beads-command-subcommand cmd) "comments"))))

(ert-deftest beads-comments-test-command-line-basic ()
  "Test comments builds correct command line."
  (let* ((cmd (beads-command-comments :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "comments" args))
    (should (member "bd-1" args))))

(ert-deftest beads-comments-test-validation-missing-issue-id ()
  "Test comments validation fails without issue-id."
  (let ((cmd (beads-command-comments)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-comments-test-validation-empty-issue-id ()
  "Test comments validation fails with empty issue-id."
  (let ((cmd (beads-command-comments :issue-id "")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-comments-test-validation-success ()
  "Test comments validation succeeds with issue-id."
  (let ((cmd (beads-command-comments :issue-id "bd-1")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for Argument Parsing

(ert-deftest beads-comments-test-parse-args-empty ()
  "Test parsing empty arguments."
  (let ((cmd (beads-comments--parse-transient-args '())))
    (should (beads-command-comments-p cmd))
    (should (null (oref cmd issue-id)))))

(ert-deftest beads-comments-test-parse-args-issue-id ()
  "Test parsing issue ID argument."
  (let ((cmd (beads-comments--parse-transient-args '("--issue-id=bd-42"))))
    (should (beads-command-comments-p cmd))
    (should (equal (oref cmd issue-id) "bd-42"))))

;;; Tests for Transient Definition

(ert-deftest beads-comments-test-transient-defined ()
  "Test that beads-comments transient is defined."
  (should (fboundp 'beads-comments)))

(ert-deftest beads-comments-test-transient-is-prefix ()
  "Test that beads-comments is a transient prefix."
  (should (get 'beads-comments 'transient--prefix)))

(ert-deftest beads-comments-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-comments--execute))
  (should (fboundp 'beads-comments--reset))
  (should (fboundp 'beads-comments--preview)))

;;; ============================================================
;;; Tests for beads-command-comments-add
;;; ============================================================

(ert-deftest beads-comments-add-test-class-exists ()
  "Test that beads-command-comments-add class is defined."
  (should (cl-find-class 'beads-command-comments-add)))

(ert-deftest beads-comments-add-test-subcommand ()
  "Test that subcommand returns 'comments add'."
  (let ((cmd (beads-command-comments-add)))
    (should (equal (beads-command-subcommand cmd) "comments add"))))

(ert-deftest beads-comments-add-test-command-line-basic ()
  "Test comments add builds correct command line."
  (let* ((cmd (beads-command-comments-add :issue-id "bd-1" :text "Test comment"))
         (args (beads-command-line cmd)))
    (should (member "comments" args))
    (should (member "add" args))
    (should (member "bd-1" args))
    (should (member "Test comment" args))))

(ert-deftest beads-comments-add-test-command-line-with-file ()
  "Test comments add with file option."
  (let* ((cmd (beads-command-comments-add :issue-id "bd-1" :file "/tmp/comment.txt"))
         (args (beads-command-line cmd)))
    (should (member "comments" args))
    (should (member "add" args))
    (should (member "--file" args))
    (should (member "/tmp/comment.txt" args))))

(ert-deftest beads-comments-add-test-command-line-with-author ()
  "Test comments add with author option."
  (let* ((cmd (beads-command-comments-add :issue-id "bd-1"
                                           :text "Comment"
                                           :author "tester"))
         (args (beads-command-line cmd)))
    (should (member "--author" args))
    (should (member "tester" args))))

(ert-deftest beads-comments-add-test-validation-missing-issue-id ()
  "Test comments add validation fails without issue-id."
  (let ((cmd (beads-command-comments-add :text "Test")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-comments-add-test-validation-missing-text-and-file ()
  "Test comments add validation fails without text or file."
  (let ((cmd (beads-command-comments-add :issue-id "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-comments-add-test-validation-empty-issue-id ()
  "Test comments add validation fails with empty issue-id."
  (let ((cmd (beads-command-comments-add :issue-id "" :text "Test")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-comments-add-test-validation-empty-text ()
  "Test comments add validation fails with empty text and no file."
  (let ((cmd (beads-command-comments-add :issue-id "bd-1" :text "")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-comments-add-test-validation-success-with-text ()
  "Test comments add validation succeeds with text."
  (let ((cmd (beads-command-comments-add :issue-id "bd-1" :text "Test")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-comments-add-test-validation-success-with-file ()
  "Test comments add validation succeeds with file."
  (let ((cmd (beads-command-comments-add :issue-id "bd-1"
                                          :file "/tmp/comment.txt")))
    (should (null (beads-command-validate cmd)))))

;;; Tests for Argument Parsing

(ert-deftest beads-comments-add-test-parse-args-empty ()
  "Test parsing empty arguments."
  (let ((cmd (beads-comments-add--parse-transient-args '())))
    (should (beads-command-comments-add-p cmd))
    (should (null (oref cmd issue-id)))
    (should (null (oref cmd text)))))

(ert-deftest beads-comments-add-test-parse-args-all-fields ()
  "Test parsing all fields."
  (let ((cmd (beads-comments-add--parse-transient-args
              '("--issue-id=bd-42"
                "--text=Test comment"
                "--file=/tmp/file.txt"
                "--author=tester"))))
    (should (beads-command-comments-add-p cmd))
    (should (equal (oref cmd issue-id) "bd-42"))
    (should (equal (oref cmd text) "Test comment"))
    (should (equal (oref cmd file) "/tmp/file.txt"))
    (should (equal (oref cmd author) "tester"))))

;;; Tests for Transient Definition

(ert-deftest beads-comments-add-test-transient-defined ()
  "Test that beads-comments-add transient is defined."
  (should (fboundp 'beads-comments-add)))

(ert-deftest beads-comments-add-test-transient-is-prefix ()
  "Test that beads-comments-add is a transient prefix."
  (should (get 'beads-comments-add 'transient--prefix)))

(ert-deftest beads-comments-add-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-comments-add--execute))
  (should (fboundp 'beads-comments-add--reset))
  (should (fboundp 'beads-comments-add--preview)))

;;; ============================================================
;;; Tests for Parent Menu
;;; ============================================================

(ert-deftest beads-comments-test-menu-defined ()
  "Test that beads-comments-menu is defined."
  (should (fboundp 'beads-comments-menu)))

(ert-deftest beads-comments-test-menu-is-prefix ()
  "Test that beads-comments-menu is a transient prefix."
  (should (get 'beads-comments-menu 'transient--prefix)))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-comments-test-full-workflow ()
  "Integration test: Full workflow for listing comments."
  :tags '(integration)
  (let ((cmd (beads-comments--parse-transient-args '("--issue-id=bd-42"))))
    ;; Validate
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-comments-add-test-full-workflow ()
  "Integration test: Full workflow for adding comment."
  :tags '(integration)
  (let ((cmd (beads-comments-add--parse-transient-args
              '("--issue-id=bd-42" "--text=Test comment"))))
    ;; Validate
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-comments-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (let ((cmd (beads-command-comments :issue-id "bd-42"))
        (start-time (current-time)))
    (dotimes (_ 1000)
      (beads-command-validate cmd))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should validate 1000 times in under 0.5 seconds
      (should (< elapsed 0.5)))))

;;; ============================================================
;;; Convenience Function Tests
;;; ============================================================

(ert-deftest beads-comments-test-bang-function-exists ()
  "Test that convenience functions are defined."
  (should (fboundp 'beads-command-comments!))
  (should (fboundp 'beads-command-comments-add!)))

(provide 'beads-command-comments-test)
;;; beads-command-comments-test.el ends here
