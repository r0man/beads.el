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

;;; Tests for Edge Cases

(ert-deftest beads-init-test-parse-args-db-path ()
  "Test parsing with database path."
  (let ((cmd (beads-init--parse-transient-args
              '("--db=/path/to/custom.db"))))
    (should (beads-command-init-p cmd))
    (should (equal (oref cmd db) "/path/to/custom.db"))))

(ert-deftest beads-init-test-parse-args-all-booleans ()
  "Test parsing with all boolean flags."
  (let ((cmd (beads-init--parse-transient-args
              '("--quiet" "--skip-merge-driver" "--contributor"))))
    (should (beads-command-init-p cmd))
    (should (eq (oref cmd quiet) t))
    (should (eq (oref cmd skip-merge-driver) t))
    (should (eq (oref cmd contributor) t))))

(ert-deftest beads-init-test-parse-args-unknown-arg ()
  "Test parsing ignores unknown arguments."
  (let ((cmd (beads-init--parse-transient-args
              '("--unknown=value" "--prefix=myproj"))))
    (should (beads-command-init-p cmd))
    (should (equal (oref cmd prefix) "myproj"))))

(ert-deftest beads-init-test-parse-args-empty-prefix ()
  "Test parsing with empty prefix value."
  (let ((cmd (beads-init--parse-transient-args
              '("--prefix="))))
    (should (beads-command-init-p cmd))
    ;; Empty prefix after = is parsed as nil or empty string depending on implementation
    (should (or (null (oref cmd prefix))
                (equal (oref cmd prefix) "")))))

;;; Tests for Command Construction

(ert-deftest beads-init-test-command-line-basic ()
  "Test command line construction for basic init."
  (let* ((cmd (beads-command-init :prefix "test"))
         (args (beads-command-line cmd)))
    (should (member "init" args))
    (should (member "--prefix" args))
    (should (member "test" args))))

(ert-deftest beads-init-test-command-line-with-branch ()
  "Test command line construction with branch."
  (let* ((cmd (beads-command-init :prefix "proj" :branch "develop"))
         (args (beads-command-line cmd)))
    (should (member "init" args))
    (should (member "--branch" args))
    (should (member "develop" args))))

(ert-deftest beads-init-test-command-line-with-db ()
  "Test command line construction with custom db path."
  (let* ((cmd (beads-command-init :db "/path/to/db"))
         (args (beads-command-line cmd)))
    (should (member "init" args))
    (should (member "--db" args))
    (should (member "/path/to/db" args))))

(ert-deftest beads-init-test-command-line-with-booleans ()
  "Test command line construction with boolean flags."
  (let* ((cmd (beads-command-init :quiet t :skip-merge-driver t))
         (args (beads-command-line cmd)))
    (should (member "init" args))
    (should (member "--quiet" args))
    (should (member "--skip-merge-driver" args))))

(ert-deftest beads-init-test-command-line-contributor ()
  "Test command line construction with contributor flag."
  (let* ((cmd (beads-command-init :contributor t))
         (args (beads-command-line cmd)))
    (should (member "init" args))
    (should (member "--contributor" args))))

(ert-deftest beads-init-test-command-line-team ()
  "Test command line construction with team flag."
  (let* ((cmd (beads-command-init :team t))
         (args (beads-command-line cmd)))
    (should (member "init" args))
    (should (member "--team" args))))

;;; Tests for Validation Edge Cases

(ert-deftest beads-init-test-validate-db-with-contributor ()
  "Test validation allows db with contributor."
  (let ((cmd (beads-command-init :db "/path/to/db" :contributor t)))
    (should (null (beads-init--validate-all cmd)))))

(ert-deftest beads-init-test-validate-db-with-team ()
  "Test validation allows db with team."
  (let ((cmd (beads-command-init :db "/path/to/db" :team t)))
    (should (null (beads-init--validate-all cmd)))))

(ert-deftest beads-init-test-validate-branch-with-prefix ()
  "Test validation allows branch with prefix."
  (let ((cmd (beads-command-init :prefix "proj" :branch "main")))
    (should (null (beads-init--validate-all cmd)))))

;;; Transient Suffix Command Tests
;;
;; These tests exercise the transient suffix commands by mocking
;; interactive functions to test the execution paths.

(ert-deftest beads-init-test-execute-suffix-flow ()
  "Test execute suffix command flow."
  :tags '(:unit)
  (let ((executed nil)
        (message-output nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-args)
               (lambda (_prefix) '("--prefix=testproj")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) (setq executed t)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-init--execute)
      (should executed)
      (should message-output)
      (should (string-match-p "initialized" message-output)))))

(ert-deftest beads-init-test-execute-suffix-with-prefix-message ()
  "Test execute suffix shows prefix in success message."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-args)
               (lambda (_prefix) '("--prefix=myproj")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-init--execute)
      (should message-output)
      (should (string-match-p "myproj" message-output)))))

(ert-deftest beads-init-test-execute-suffix-user-declines ()
  "Test execute suffix does nothing when user declines."
  :tags '(:unit)
  (let ((executed nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) (setq executed t))))
      (beads-init--execute)
      (should-not executed))))

(ert-deftest beads-init-test-execute-suffix-validation-error ()
  "Test execute suffix handles validation errors."
  :tags '(:unit)
  (let ((error-caught nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-args)
               (lambda (_prefix) '("--contributor" "--team"))))
      (condition-case nil
          (beads-init--execute)
        (user-error (setq error-caught t)))
      (should error-caught))))

(ert-deftest beads-init-test-execute-suffix-command-error ()
  "Test execute suffix handles command execution errors."
  :tags '(:unit)
  (let ((error-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-args)
               (lambda (_prefix) '("--prefix=test")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) (error "Command failed")))
              ((symbol-function 'beads--error)
               (lambda (_fmt &rest _args) (setq error-called t))))
      (beads-init--execute)
      (should error-called))))

(ert-deftest beads-init-test-preview-suffix-shows-command ()
  "Test preview suffix displays command."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--prefix=proj" "--branch=main")))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-init--preview)
      (should message-output)
      (should (string-match-p "init" message-output))
      (should (string-match-p "--prefix" message-output)))))

(ert-deftest beads-init-test-reset-suffix-confirmed ()
  "Test reset suffix when user confirms."
  :tags '(:unit)
  (let ((reset-called nil)
        (redisplay-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t)))
              ((symbol-function 'transient--redisplay)
               (lambda () (setq redisplay-called t)))
              ((symbol-function 'message)
               (lambda (_fmt &rest _args) nil)))
      (beads-init--reset)
      (should reset-called)
      (should redisplay-called))))

(ert-deftest beads-init-test-reset-suffix-declined ()
  "Test reset suffix when user declines."
  :tags '(:unit)
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t))))
      (beads-init--reset)
      (should-not reset-called))))

;;; Footer

(provide 'beads-init-test)
;;; beads-init-test.el ends here
