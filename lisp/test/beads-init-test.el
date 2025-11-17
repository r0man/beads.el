;;; beads-init-test.el --- Tests for beads-init -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-init.el transient menu.
;; Tests cover transient definition, command construction, validation,
;; execution, and integration with the bd CLI.
;;
;; This test file uses the transient-args pattern where tests mock
;; (transient-args 'beads-init) to return argument lists like
;; '("--prefix=myproj" "--branch=main").

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-init)

;;; Test Utilities

(defun beads-init-test--mock-transient-args (args)
  "Create a mock for `transient-args' returning ARGS.
ARGS should be a list of strings like (\"--prefix=myproj\" \"--branch=main\")."
  (lambda (prefix)
    (when (eq prefix 'beads-init)
      args)))

(defun beads-init-test--mock-process-file (exit-code output)
  "Create a mock for `process-file' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile buffer display &rest args)
    (when buffer
      (let ((buf (if (listp buffer)
                     (car buffer)
                   buffer)))
        (when buf
          (with-current-buffer (if (bufferp buf) buf (current-buffer))
            (insert output)))))
    exit-code))

;;; Tests for Argument Parsing

(ert-deftest beads-init-test-parse-args-empty ()
  "Test parsing empty argument list."
  (let ((cmd (beads-init--parse-transient-args nil)))
    (should (beads-command-init-p cmd))
    (should (null (oref cmd prefix)))
    (should (null (oref cmd branch)))
    (should (null (oref cmd db)))
    (should (null (oref cmd contributor)))
    (should (null (oref cmd quiet)))
    (should (null (oref cmd skip-merge-driver)))
    (should (null (oref cmd team)))))

(ert-deftest beads-init-test-parse-args-prefix-only ()
  "Test parsing with only prefix."
  (let ((cmd (beads-init--parse-transient-args
              '("--prefix=myproject"))))
    (should (beads-command-init-p cmd))
    (should (equal (oref cmd prefix) "myproject"))
    (should (null (oref cmd branch)))))

(ert-deftest beads-init-test-parse-args-all-options ()
  "Test parsing with all options."
  (let ((cmd (beads-init--parse-transient-args
              '("--prefix=myproj"
                "--branch=develop"
                "--db=/path/to/db"
                "--contributor"
                "--quiet"
                "--skip-merge-driver"))))
    (should (beads-command-init-p cmd))
    (should (equal (oref cmd prefix) "myproj"))
    (should (equal (oref cmd branch) "develop"))
    (should (equal (oref cmd db) "/path/to/db"))
    (should (equal (oref cmd contributor) t))
    (should (equal (oref cmd quiet) t))
    (should (equal (oref cmd skip-merge-driver) t))
    (should (null (oref cmd team)))))

(ert-deftest beads-init-test-parse-args-team-wizard ()
  "Test parsing with team wizard option."
  (let ((cmd (beads-init--parse-transient-args
              '("--team"))))
    (should (beads-command-init-p cmd))
    (should (equal (oref cmd team) t))
    (should (null (oref cmd contributor)))))

(ert-deftest beads-init-test-parse-args-branch-and-prefix ()
  "Test parsing with branch and prefix."
  (let ((cmd (beads-init--parse-transient-args
              '("--prefix=proj"
                "--branch=main"))))
    (should (beads-command-init-p cmd))
    (should (equal (oref cmd prefix) "proj"))
    (should (equal (oref cmd branch) "main"))))

;;; Tests for Validation

(ert-deftest beads-init-test-validate-all-valid ()
  "Test validation with valid options."
  (let ((cmd (beads-command-init :prefix "myproj")))
    (should (null (beads-init--validate-all cmd)))))

(ert-deftest beads-init-test-validate-contributor-and-team-conflict ()
  "Test validation rejects both --contributor and --team."
  (let ((cmd (beads-command-init
              :contributor t
              :team t)))
    (should (beads-init--validate-all cmd))
    (should (stringp (car (beads-init--validate-all cmd))))
    (should (string-match-p "contributor\\|team"
                           (downcase (car (beads-init--validate-all cmd)))))))

(ert-deftest beads-init-test-validate-contributor-only ()
  "Test validation allows --contributor alone."
  (let ((cmd (beads-command-init :contributor t)))
    (should (null (beads-init--validate-all cmd)))))

(ert-deftest beads-init-test-validate-team-only ()
  "Test validation allows --team alone."
  (let ((cmd (beads-command-init :team t)))
    (should (null (beads-init--validate-all cmd)))))

(ert-deftest beads-init-test-validate-all-other-options ()
  "Test validation allows all other combinations."
  (let ((cmd (beads-command-init
              :prefix "myproj"
              :branch "develop"
              :quiet t
              :skip-merge-driver t)))
    (should (null (beads-init--validate-all cmd)))))

;;; Tests for Execution (with mocking)

(ert-deftest beads-init-test-execute-basic ()
  "Test basic init execution."
  :tags '(:unit :mock)
  (let* ((args '("--prefix=testproj"))
         (cmd (beads-init--parse-transient-args args))
         (executed-args nil))
    ;; Mock beads-command-execute to capture args
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (command)
                 (setq executed-args (beads-command-line command))
                 '(0 "Initialized" ""))))
      (beads-command-execute cmd)
      (should (member "init" executed-args))
      (should (member "--prefix" executed-args))
      (should (member "testproj" executed-args)))))

(ert-deftest beads-init-test-execute-with-all-options ()
  "Test init execution with all options."
  :tags '(:unit :mock)
  (let* ((args '("--prefix=proj"
                "--branch=main"
                "--quiet"
                "--skip-merge-driver"))
         (cmd (beads-init--parse-transient-args args))
         (executed-args nil))
    ;; Mock beads-command-execute to capture args
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (command)
                 (setq executed-args (beads-command-line command))
                 '(0 "Initialized" ""))))
      (beads-command-execute cmd)
      (should (member "init" executed-args))
      (should (member "--prefix" executed-args))
      (should (member "proj" executed-args))
      (should (member "--branch" executed-args))
      (should (member "main" executed-args))
      (should (member "--quiet" executed-args))
      (should (member "--skip-merge-driver" executed-args)))))

(ert-deftest beads-init-test-execute-with-team ()
  "Test init execution with team wizard."
  :tags '(:unit :mock)
  (let* ((args '("--team"))
         (cmd (beads-init--parse-transient-args args))
         (executed-args nil))
    ;; Mock beads-command-execute to capture args
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (command)
                 (setq executed-args (beads-command-line command))
                 '(0 "Initialized" ""))))
      (beads-command-execute cmd)
      (should (member "init" executed-args))
      (should (member "--team" executed-args)))))

(ert-deftest beads-init-test-execute-with-contributor ()
  "Test init execution with contributor wizard."
  :tags '(:unit :mock)
  (let* ((args '("--contributor"))
         (cmd (beads-init--parse-transient-args args))
         (executed-args nil))
    ;; Mock beads-command-execute to capture args
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (command)
                 (setq executed-args (beads-command-line command))
                 '(0 "Initialized" ""))))
      (beads-command-execute cmd)
      (should (member "init" executed-args))
      (should (member "--contributor" executed-args)))))

;;; Tests for Transient Integration

(ert-deftest beads-init-test-transient-defined ()
  "Test that beads-init transient is defined."
  (should (commandp 'beads-init))
  (should (get 'beads-init 'transient--prefix)))

(ert-deftest beads-init-test-suffix-execute-defined ()
  "Test that execute suffix is defined."
  (should (commandp 'beads-init--execute))
  (should (get 'beads-init--execute 'transient--suffix)))

(ert-deftest beads-init-test-suffix-preview-defined ()
  "Test that preview suffix is defined."
  (should (commandp 'beads-init--preview))
  (should (get 'beads-init--preview 'transient--suffix)))

(ert-deftest beads-init-test-suffix-reset-defined ()
  "Test that reset suffix is defined."
  (should (commandp 'beads-init--reset))
  (should (get 'beads-init--reset 'transient--suffix)))

;;; Footer

(provide 'beads-init-test)
;;; beads-init-test.el ends here
