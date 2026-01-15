;;; beads-command-config-test.el --- Tests for beads-command-config -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for beads-command-config command classes.

;;; Code:

(require 'ert)
(require 'beads-command-config)

;;; Unit Tests: beads-command-config-get command-line

(ert-deftest beads-command-config-get-test-command-line-basic ()
  "Unit test: config get builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-get :key "sync.branch"))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "get" args))
    (should (member "sync.branch" args))))

(ert-deftest beads-command-config-get-test-validation-missing-key ()
  "Unit test: config get validation fails without key."
  :tags '(:unit)
  (let ((cmd (beads-command-config-get)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-config-get-test-validation-success ()
  "Unit test: config get validation succeeds with key."
  :tags '(:unit)
  (let ((cmd (beads-command-config-get :key "test.key")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-config-set command-line

(ert-deftest beads-command-config-set-test-command-line-basic ()
  "Unit test: config set builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-set :key "sync.branch" :value "main"))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "set" args))
    (should (member "sync.branch" args))
    (should (member "main" args))))

(ert-deftest beads-command-config-set-test-validation-missing-key ()
  "Unit test: config set validation fails without key."
  :tags '(:unit)
  (let ((cmd (beads-command-config-set :value "test")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-config-set-test-validation-missing-value ()
  "Unit test: config set validation fails without value."
  :tags '(:unit)
  (let ((cmd (beads-command-config-set :key "test.key")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-config-set-test-validation-success ()
  "Unit test: config set validation succeeds with key and value."
  :tags '(:unit)
  (let ((cmd (beads-command-config-set :key "test.key" :value "test")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-config-list command-line

(ert-deftest beads-command-config-list-test-command-line-basic ()
  "Unit test: config list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-list))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "list" args))))

;;; Unit Tests: beads-command-config-unset command-line

(ert-deftest beads-command-config-unset-test-command-line-basic ()
  "Unit test: config unset builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-config-unset :key "test.key"))
         (args (beads-command-line cmd)))
    (should (member "config" args))
    (should (member "unset" args))
    (should (member "test.key" args))))

(ert-deftest beads-command-config-unset-test-validation-missing-key ()
  "Unit test: config unset validation fails without key."
  :tags '(:unit)
  (let ((cmd (beads-command-config-unset)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-config-unset-test-validation-success ()
  "Unit test: config unset validation succeeds with key."
  :tags '(:unit)
  (let ((cmd (beads-command-config-unset :key "test.key")))
    (should (null (beads-command-validate cmd)))))

(provide 'beads-command-config-test)
;;; beads-command-config-test.el ends here
