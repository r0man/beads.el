;;; beads-misc-test.el --- Tests for beads-misc -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-misc.el transient menus.
;; Tests cover: import and init commands.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-misc)

;;; Test Fixtures

(defvar beads-misc-test--sample-issue
  '((id . "bd-42")
    (title . "Test Issue")
    (status . "open")
    (priority . 1))
  "Sample issue for testing.")

(defvar beads-misc-test--sample-closed-issue
  '((id . "bd-42")
    (title . "Test Issue")
    (status . "closed")
    (closed_at . "2025-01-15T10:30:00Z"))
  "Sample closed issue for testing.")

(defvar beads-misc-test--sample-dependency
  '((from_issue . "bd-1")
    (to_issue . "bd-2")
    (dependency_type . "blocks"))
  "Sample dependency for testing.")

;;; Test Utilities

(defun beads-misc-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; ============================================================
;;; bd init tests
;;; ============================================================

(ert-deftest beads-misc-test-init-format-value-set ()
  "Test init formatting when value is set."
  (let ((result (beads-init--format-value "bd")))
    (should (stringp result))
    (should (string-match-p "bd" result))))

(ert-deftest beads-misc-test-init-format-value-nil ()
  "Test init formatting when value is nil."
  (let ((result (beads-init--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

(ert-deftest beads-misc-test-init-execute-success ()
  "Test successful init execution."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 0 "")))
    (should-not (beads-init--execute nil nil))))

(ert-deftest beads-misc-test-init-execute-with-prefix ()
  "Test init with prefix."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-init--execute "test" nil)
      (should (member "--prefix" captured-args))
      (should (member "test" captured-args)))))

(ert-deftest beads-misc-test-init-execute-with-db-path ()
  "Test init with database path."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-init--execute nil "/tmp/test.db")
      (should (member "--db" captured-args))
      (should (member "/tmp/test.db" captured-args)))))

(ert-deftest beads-misc-test-init-execute-with-both ()
  "Test init with both prefix and db path."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-init--execute "myproj" "/tmp/test.db")
      (should (member "--prefix" captured-args))
      (should (member "myproj" captured-args))
      (should (member "--db" captured-args))
      (should (member "/tmp/test.db" captured-args)))))

(ert-deftest beads-misc-test-init-execute-failure ()
  "Test init execution failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 1 "Error")))
    (should-error (beads-init--execute nil nil))))

(ert-deftest beads-misc-test-init-execute-empty-prefix ()
  "Test init with empty prefix is ignored."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-init--execute "" nil)
      (should-not (member "--prefix" captured-args)))))

(ert-deftest beads-misc-test-init-execute-whitespace-prefix ()
  "Test init with whitespace prefix is ignored."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-init--execute "   " nil)
      (should-not (member "--prefix" captured-args)))))

(ert-deftest beads-misc-test-init-transient-defined ()
  "Test that beads-init transient is defined."
  (should (fboundp 'beads-init)))

(ert-deftest beads-misc-test-init-transient-is-prefix ()
  "Test that beads-init--menu is a transient prefix."
  (should (get 'beads-init--menu 'transient--prefix)))

(ert-deftest beads-misc-test-init-infix-commands-defined ()
  "Test that init infix commands are defined."
  (should (fboundp 'beads-option-init-prefix))
  (should (fboundp 'beads-option-init-db)))

(ert-deftest beads-misc-test-init-suffix-commands-defined ()
  "Test that init suffix commands are defined."
  (should (fboundp 'beads-init--execute-command))
  (should (fboundp 'beads-init--reset)))

;;; Edge Cases and Integration Tests

(provide 'beads-misc-test)
;;; beads-misc-test.el ends here
