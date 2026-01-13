;;; beads-command-formula-test.el --- Tests for beads-command-formula -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-command-formula.el including:
;; - Formula type definitions (beads-formula-summary, beads-formula)
;; - Command class tests (beads-command-formula-list, beads-command-formula-show)
;; - JSON parsing tests
;; - Command line building tests
;; - UI utility functions tests (formatting, faces)

;;; Code:

(require 'ert)
(require 'beads-types)
(require 'beads-command-formula)

;;; ========================================
;;; Test Fixtures
;;; ========================================

(defvar beads-command-formula-test--sample-list-json
  '(((name . "emacs-lisp-dev")
     (type . "workflow")
     (description . "Emacs Lisp Feature Development...")
     (source . "/path/to/emacs-lisp-dev.formula.toml")
     (steps . 20)
     (vars . 5))
    ((name . "security-audit")
     (type . "aspect")
     (description . "Security audit aspect...")
     (source . "/path/to/security-audit.formula.toml")
     (steps . 5)
     (vars . 2)))
  "Sample formula list JSON for testing.")

(defvar beads-command-formula-test--sample-show-json
  '((formula . "emacs-lisp-dev")
    (description . "Emacs Lisp Feature Development\n\nFull description here.")
    (version . 1)
    (type . "workflow")
    (vars . ((feature . ((description . "Feature description")
                         (required . t)))
             (module . ((description . "Module name")
                        (required . t)))
             (use_guix . ((description . "Use guix")
                          (default . "true")))))
    (steps . (((id . "step-1")
               (title . "First step")
               (description . "Do something first")
               (needs . nil))
              ((id . "step-2")
               (title . "Second step")
               (description . "Do something second")
               (needs . ("step-1")))))
    (source . "/path/to/emacs-lisp-dev.formula.toml"))
  "Sample formula show JSON for testing.")

;;; ========================================
;;; beads-formula-summary Tests
;;; ========================================

(ert-deftest beads-command-formula-test-summary-from-json ()
  "Test creating beads-formula-summary from JSON."
  (let* ((json (car beads-command-formula-test--sample-list-json))
         (formula (beads-formula-summary-from-json json)))
    (should (beads-formula-summary-p formula))
    (should (string= (oref formula name) "emacs-lisp-dev"))
    (should (string= (oref formula formula-type) "workflow"))
    (should (string= (oref formula description) "Emacs Lisp Feature Development..."))
    (should (string= (oref formula source)
                     "/path/to/emacs-lisp-dev.formula.toml"))
    (should (= (oref formula steps) 20))
    (should (= (oref formula vars) 5))))

(ert-deftest beads-command-formula-test-summary-from-json-minimal ()
  "Test beads-formula-summary handles minimal JSON."
  (let* ((json '((name . "minimal")))
         (formula (beads-formula-summary-from-json json)))
    (should (beads-formula-summary-p formula))
    (should (string= (oref formula name) "minimal"))
    (should (null (oref formula formula-type)))
    (should (null (oref formula description)))
    (should (null (oref formula steps)))))

;;; ========================================
;;; beads-formula Tests
;;; ========================================

(ert-deftest beads-command-formula-test-formula-from-json ()
  "Test creating beads-formula from JSON."
  (let* ((json beads-command-formula-test--sample-show-json)
         (formula (beads-formula-from-json json)))
    (should (beads-formula-p formula))
    (should (string= (oref formula name) "emacs-lisp-dev"))
    (should (string-match-p "Emacs Lisp" (oref formula description)))
    (should (= (oref formula version) 1))
    (should (string= (oref formula formula-type) "workflow"))
    (should (= (length (oref formula steps)) 2))
    (should (oref formula vars))
    (should (string= (oref formula source)
                     "/path/to/emacs-lisp-dev.formula.toml"))))

(ert-deftest beads-command-formula-test-formula-vars ()
  "Test formula variables parsing."
  (let* ((json beads-command-formula-test--sample-show-json)
         (formula (beads-formula-from-json json))
         (vars (oref formula vars)))
    (should vars)
    ;; Check feature var
    (let ((feature-var (alist-get 'feature vars)))
      (should feature-var)
      (should (string= (alist-get 'description feature-var)
                       "Feature description"))
      (should (eq (alist-get 'required feature-var) t)))
    ;; Check use_guix var
    (let ((guix-var (alist-get 'use_guix vars)))
      (should guix-var)
      (should (string= (alist-get 'default guix-var) "true")))))

(ert-deftest beads-command-formula-test-formula-steps ()
  "Test formula steps parsing."
  (let* ((json beads-command-formula-test--sample-show-json)
         (formula (beads-formula-from-json json))
         (steps (oref formula steps)))
    (should (= (length steps) 2))
    ;; Check first step
    (let ((step1 (car steps)))
      (should (string= (alist-get 'id step1) "step-1"))
      (should (string= (alist-get 'title step1) "First step"))
      (should (null (alist-get 'needs step1))))
    ;; Check second step
    (let ((step2 (cadr steps)))
      (should (string= (alist-get 'id step2) "step-2"))
      (should (equal (alist-get 'needs step2) '("step-1"))))))

;;; ========================================
;;; beads-command-formula-list Tests
;;; ========================================

(ert-deftest beads-command-formula-test-list-subcommand ()
  "Test formula list subcommand name."
  (let ((cmd (beads-command-formula-list)))
    (should (string= (beads-command-subcommand cmd) "formula list"))))

(ert-deftest beads-command-formula-test-list-command-line-basic ()
  "Test formula list basic command line."
  (let* ((cmd (beads-command-formula-list :json t))
         (args (beads-command-line cmd)))
    (should (member "formula" args))
    (should (member "list" args))
    (should (member "--json" args))))

(ert-deftest beads-command-formula-test-list-command-line-with-type ()
  "Test formula list command line with type filter."
  (let* ((cmd (beads-command-formula-list :json t :formula-type "workflow"))
         (args (beads-command-line cmd)))
    (should (member "formula" args))
    (should (member "list" args))
    (should (member "--type" args))
    (should (member "workflow" args))))

(ert-deftest beads-command-formula-test-list-validate ()
  "Test formula list validation (should pass with no args)."
  (let ((cmd (beads-command-formula-list)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-formula-test-list-parse ()
  "Test formula list JSON parsing."
  (let* ((cmd (beads-command-formula-list :json t))
         (json-string (json-encode beads-command-formula-test--sample-list-json)))
    (oset cmd stdout json-string)
    (let ((result (beads-command-parse cmd)))
      (should (= (length result) 2))
      (should (beads-formula-summary-p (car result)))
      (should (string= (oref (car result) name) "emacs-lisp-dev"))
      (should (string= (oref (cadr result) name) "security-audit")))))

;;; ========================================
;;; beads-command-formula-show Tests
;;; ========================================

(ert-deftest beads-command-formula-test-show-subcommand ()
  "Test formula show subcommand name."
  (let ((cmd (beads-command-formula-show)))
    (should (string= (beads-command-subcommand cmd) "formula show"))))

(ert-deftest beads-command-formula-test-show-command-line ()
  "Test formula show command line."
  (let* ((cmd (beads-command-formula-show :json t :formula-name "test-formula"))
         (args (beads-command-line cmd)))
    (should (member "formula" args))
    (should (member "show" args))
    (should (member "--json" args))
    (should (member "test-formula" args))))

(ert-deftest beads-command-formula-test-show-validate-required ()
  "Test formula show requires formula name."
  (let ((cmd (beads-command-formula-show)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-formula-test-show-validate-with-name ()
  "Test formula show validates with name."
  (let ((cmd (beads-command-formula-show :formula-name "test")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-formula-test-show-parse ()
  "Test formula show JSON parsing."
  (let* ((cmd (beads-command-formula-show :json t :formula-name "test"))
         (json-string (json-encode beads-command-formula-test--sample-show-json)))
    (oset cmd stdout json-string)
    (let ((result (beads-command-parse cmd)))
      (should (beads-formula-p result))
      (should (string= (oref result name) "emacs-lisp-dev"))
      (should (= (oref result version) 1)))))

;;; ========================================
;;; UI Utility Tests
;;; ========================================

(ert-deftest beads-command-formula-test-type-face ()
  "Test formula type face mapping."
  (should (eq (beads-formula-list--type-face "workflow")
              'beads-formula-type-workflow))
  (should (eq (beads-formula-list--type-face "expansion")
              'beads-formula-type-expansion))
  (should (eq (beads-formula-list--type-face "aspect")
              'beads-formula-type-aspect))
  (should (eq (beads-formula-list--type-face "unknown")
              'default)))

(ert-deftest beads-command-formula-test-format-type ()
  "Test formula type formatting."
  (let ((formatted (beads-formula-list--format-type "workflow")))
    (should (stringp formatted))
    (should (string= formatted "workflow"))
    (should (eq (get-text-property 0 'face formatted)
                'beads-formula-type-workflow))))

(ert-deftest beads-command-formula-test-formula-to-entry ()
  "Test formula to tabulated-list entry conversion."
  (let* ((json (car beads-command-formula-test--sample-list-json))
         (formula (beads-formula-summary-from-json json))
         (entry (beads-formula-list--formula-to-entry formula)))
    (should (consp entry))
    (should (string= (car entry) "emacs-lisp-dev"))
    (should (vectorp (cadr entry)))
    (let ((vec (cadr entry)))
      (should (string= (aref vec 0) "emacs-lisp-dev"))
      (should (string= (aref vec 2) "20"))
      (should (string= (aref vec 3) "5")))))

;;; ========================================
;;; Integration Tests
;;; ========================================

(ert-deftest beads-command-formula-test-list-integration ()
  "Integration test for formula list command."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  ;; This test will actually run bd formula list --json
  (let* ((cmd (beads-command-formula-list :json t))
         (result (beads-command-execute cmd)))
    ;; Should return command object
    (should (cl-typep result 'beads-command))
    ;; Exit code should be 0
    (should (= (oref result exit-code) 0))
    ;; Data should be a list (possibly empty)
    (should (listp (oref result data)))))

(provide 'beads-command-formula-test)
;;; beads-command-formula-test.el ends here
