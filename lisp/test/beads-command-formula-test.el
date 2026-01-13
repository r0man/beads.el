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
;;; Buffer Population Tests
;;; ========================================

(ert-deftest beads-command-formula-test-populate-buffer ()
  "Test buffer population from formula list."
  (let* ((json beads-command-formula-test--sample-list-json)
         (formulas (mapcar #'beads-formula-summary-from-json json)))
    (with-temp-buffer
      (beads-formula-list-mode)
      (beads-formula-list--populate-buffer formulas nil)
      (should (= (length tabulated-list-entries) 2))
      (should beads-formula-list--formulas)
      (should (= (length beads-formula-list--formulas) 2)))))

(ert-deftest beads-command-formula-test-populate-buffer-empty ()
  "Test buffer population with empty formula list."
  (with-temp-buffer
    (beads-formula-list-mode)
    (beads-formula-list--populate-buffer nil nil)
    (should (null tabulated-list-entries))
    (should (null beads-formula-list--formulas))))

;;; ========================================
;;; Render Tests
;;; ========================================

(ert-deftest beads-command-formula-test-render-basic ()
  "Test formula show rendering."
  (let ((formula (beads-formula :name "test-formula"
                                :formula-type "workflow"
                                :version 1
                                :description "Test description"
                                :vars nil
                                :steps nil
                                :source "/path/to/test.toml")))
    (with-temp-buffer
      (beads-formula-show-mode)
      (beads-formula-show--render formula)
      (should (string-match-p "Formula: test-formula" (buffer-string)))
      (should (string-match-p "Type:" (buffer-string)))
      (should (string-match-p "workflow" (buffer-string)))
      (should (string-match-p "Description" (buffer-string)))
      (should (string-match-p "Test description" (buffer-string))))))

(ert-deftest beads-command-formula-test-render-nil-vars ()
  "Test formula show render handles nil vars."
  (let ((formula (beads-formula :name "test"
                                :formula-type "workflow"
                                :vars nil
                                :steps nil)))
    (with-temp-buffer
      (beads-formula-show-mode)
      (beads-formula-show--render formula)
      (should (string-match-p "Formula: test" (buffer-string)))
      ;; Should not have Variables section
      (should-not (string-match-p "Variables" (buffer-string))))))

(ert-deftest beads-command-formula-test-render-with-vars ()
  "Test formula show render with variables."
  (let ((formula (beads-formula :name "test"
                                :formula-type "workflow"
                                :vars '((feature . ((description . "Feature desc")
                                                    (required . t)))))))
    (with-temp-buffer
      (beads-formula-show-mode)
      (beads-formula-show--render formula)
      (should (string-match-p "Variables" (buffer-string)))
      (should (string-match-p "feature" (buffer-string)))
      (should (string-match-p "(required)" (buffer-string))))))

(ert-deftest beads-command-formula-test-render-with-steps ()
  "Test formula show render with steps."
  (let ((formula (beads-formula :name "test"
                                :formula-type "workflow"
                                :steps '(((id . "step-1")
                                          (title . "First step")
                                          (description . "Do something"))))))
    (with-temp-buffer
      (beads-formula-show-mode)
      (beads-formula-show--render formula)
      (should (string-match-p "Steps (1)" (buffer-string)))
      (should (string-match-p "First step" (buffer-string))))))

(ert-deftest beads-command-formula-test-render-trims-description ()
  "Test formula show render trims trailing whitespace from description."
  (let ((formula (beads-formula :name "test"
                                :formula-type "workflow"
                                :description "Test description\n\n\n")))
    (with-temp-buffer
      (beads-formula-show-mode)
      (beads-formula-show--render formula)
      ;; Should have content but trailing newlines should be trimmed
      (should (string-match-p "Test description" (buffer-string)))
      ;; The description should be followed by a single newline, not multiple
      (should-not (string-match-p "Test description\n\n\n\n" (buffer-string))))))

;;; ========================================
;;; Buffer Management Tests
;;; ========================================

(ert-deftest beads-command-formula-test-normalize-directory ()
  "Test directory normalization."
  (should (string= (beads-formula-list--normalize-directory "/tmp/foo/")
                   "/tmp/foo"))
  (should (string= (beads-formula-list--normalize-directory "/tmp/foo")
                   "/tmp/foo")))

(ert-deftest beads-command-formula-test-find-buffer-for-project ()
  "Test finding buffer for project."
  (let ((buf nil))
    (unwind-protect
        (progn
          ;; Create a test buffer
          (setq buf (get-buffer-create "*beads-formula-list-test*"))
          (with-current-buffer buf
            (beads-formula-list-mode)
            (setq beads-formula-list--project-dir "/tmp/test-project"))
          ;; Should find buffer
          (should (eq (beads-formula-list--find-buffer-for-project
                       "/tmp/test-project/")
                      buf))
          ;; Should not find buffer for different project
          (should (null (beads-formula-list--find-buffer-for-project
                         "/tmp/other-project"))))
      ;; Cleanup
      (when buf (kill-buffer buf)))))

;;; ========================================
;;; Navigation Tests
;;; ========================================

(ert-deftest beads-command-formula-test-get-formula-by-name ()
  "Test getting formula by name."
  (let* ((json beads-command-formula-test--sample-list-json)
         (formulas (mapcar #'beads-formula-summary-from-json json)))
    (with-temp-buffer
      (beads-formula-list-mode)
      (beads-formula-list--populate-buffer formulas nil)
      ;; Should find existing formula
      (let ((found (beads-formula-list--get-formula-by-name "emacs-lisp-dev")))
        (should found)
        (should (string= (oref found name) "emacs-lisp-dev")))
      ;; Should return nil for non-existent
      (should (null (beads-formula-list--get-formula-by-name "nonexistent"))))))

;;; ========================================
;;; Integration Tests
;;; ========================================

;; Run integration tests with: eldev test --selector '(tag :integration)'
(ert-deftest beads-command-formula-test-list-integration ()
  "Integration test for formula list command."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  ;; Check for .beads directory with actual database file
  (let ((beads-dir (locate-dominating-file default-directory ".beads")))
    (skip-unless beads-dir)
    (skip-unless (directory-files (expand-file-name ".beads" beads-dir)
                                  nil "\\.db\\'" t)))
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
