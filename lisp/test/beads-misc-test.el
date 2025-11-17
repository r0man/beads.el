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
;;; bd import tests
;;; ============================================================

(ert-deftest beads-misc-test-import-format-value-set ()
  "Test import formatting when value is set."
  (let ((result (beads-import--format-value "/tmp/test.jsonl")))
    (should (stringp result))
    (should (string-match-p "test.jsonl" result))))

(ert-deftest beads-misc-test-import-format-value-nil ()
  "Test import formatting when value is nil."
  (let ((result (beads-import--format-value nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

(ert-deftest beads-misc-test-import-validate-input-nil ()
  "Test input validation when nil."
  (should (beads-import--validate-input nil)))

(ert-deftest beads-misc-test-import-validate-input-empty ()
  "Test input validation when empty."
  (should (beads-import--validate-input "")))

(ert-deftest beads-misc-test-import-validate-input-valid ()
  "Test input validation when valid."
  (should (null (beads-import--validate-input "/tmp/test.jsonl"))))

(ert-deftest beads-misc-test-import-validate-all-success ()
  "Test validate-all with valid input."
  (should (null (beads-import--validate-all "/tmp/test.jsonl"))))

(ert-deftest beads-misc-test-import-validate-all-failure ()
  "Test validate-all with missing input."
  (let ((errors (beads-import--validate-all nil)))
    (should errors)
    (should (>= (length errors) 1))))

(ert-deftest beads-misc-test-import-execute-success ()
  "Test successful import execution."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 0 "Imported 5 issues")))
    (should-not (beads-import--execute "/tmp/test.jsonl" nil nil))))

(ert-deftest beads-misc-test-import-execute-with-dry-run ()
  "Test import with dry-run flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 (with-current-buffer (current-buffer)
                   (insert "Dry run output"))
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-import--execute "/tmp/test.jsonl" t nil)
      (should (member "--dry-run" captured-args)))))

(ert-deftest beads-misc-test-import-execute-with-resolve-collisions ()
  "Test import with resolve-collisions flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-import--execute "/tmp/test.jsonl" nil t)
      (should (member "--resolve-collisions" captured-args)))))

(ert-deftest beads-misc-test-import-execute-with-collisions ()
  "Test import shows collision report in buffer."
  (let ((collision-output "collision detected in bd-1"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 collision-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-import--execute "/tmp/test.jsonl" nil nil)
      (should (get-buffer "*beads-import*")))))

(ert-deftest beads-misc-test-import-execute-failure ()
  "Test import execution failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-misc-test--mock-call-process 1 "Error")))
    (should-error (beads-import--execute "/tmp/test.jsonl" nil nil))))

(ert-deftest beads-misc-test-import-execute-command-args ()
  "Test import command includes correct arguments."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0)))
      (beads-import--execute "/tmp/custom.jsonl" nil nil)
      (should (member "import" captured-args))
      (should (member "-i" captured-args))
      (should (member "/tmp/custom.jsonl" captured-args)))))

(ert-deftest beads-misc-test-import-transient-defined ()
  "Test that beads-import transient is defined."
  (should (fboundp 'beads-import)))

(ert-deftest beads-misc-test-import-transient-is-prefix ()
  "Test that beads-import--menu is a transient prefix."
  (should (get 'beads-import--menu 'transient--prefix)))

(ert-deftest beads-misc-test-import-infix-commands-defined ()
  "Test that import infix commands are defined."
  (should (fboundp 'beads-option-import-input))
  (should (fboundp 'beads-option-import-dry-run))
  (should (fboundp 'beads-option-import-resolve-collisions)))

(ert-deftest beads-misc-test-import-suffix-commands-defined ()
  "Test that import suffix commands are defined."
  (should (fboundp 'beads-import--execute-command))
  (should (fboundp 'beads-import--reset)))

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

(ert-deftest beads-misc-test-import-dry-run-shows-buffer ()
  "Test that dry-run mode displays buffer."
  (let ((dry-run-output "Preview: Would import 5 issues"))
    (cl-letf (((symbol-function 'call-process)
               (beads-misc-test--mock-call-process 0 dry-run-output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-import--execute "/tmp/test.jsonl" t nil)
      (should (get-buffer "*beads-import*"))
      (with-current-buffer "*beads-import*"
        (should (string-match-p "Preview" (buffer-string)))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-misc-test-import-command-exists ()
  "Integration test: Verify beads-import command exists."
  :tags '(integration)
  (should (fboundp 'beads-import)))

(provide 'beads-misc-test)
;;; beads-misc-test.el ends here
