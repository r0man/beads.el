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
    (should (= (length vars) 3))
    ;; All items should be beads-formula-var objects
    (should (cl-every #'beads-formula-var-p vars))
    ;; Check feature var
    (let ((feature-var (cl-find "feature" vars
                                :key (lambda (v) (oref v name))
                                :test #'string=)))
      (should feature-var)
      (should (string= (oref feature-var description) "Feature description"))
      (should (eq (oref feature-var required) t)))
    ;; Check use_guix var
    (let ((guix-var (cl-find "use_guix" vars
                             :key (lambda (v) (oref v name))
                             :test #'string=)))
      (should guix-var)
      (should (string= (oref guix-var default) "true")))))

(ert-deftest beads-command-formula-test-formula-steps ()
  "Test formula steps parsing."
  (let* ((json beads-command-formula-test--sample-show-json)
         (formula (beads-formula-from-json json))
         (steps (oref formula steps)))
    (should (= (length steps) 2))
    ;; All items should be beads-formula-step objects
    (should (cl-every #'beads-formula-step-p steps))
    ;; Check first step
    (let ((step1 (car steps)))
      (should (string= (oref step1 id) "step-1"))
      (should (string= (oref step1 title) "First step"))
      (should (null (oref step1 needs))))
    ;; Check second step
    (let ((step2 (cadr steps)))
      (should (string= (oref step2 id) "step-2"))
      (should (equal (oref step2 needs) '("step-1"))))))

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
         (json-string (json-encode beads-command-formula-test--sample-list-json))
         ;; Create execution object with simulated results
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout json-string
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
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
         (json-string (json-encode beads-command-formula-test--sample-show-json))
         ;; Create execution object with simulated results
         (exec (beads-command-execution
                :command cmd
                :exit-code 0
                :stdout json-string
                :stderr "")))
    (let ((result (beads-command-parse cmd exec)))
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
                                :vars (list (beads-formula-var
                                             :name "feature"
                                             :description "Feature desc"
                                             :required t)))))
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
                                :steps (list (beads-formula-step
                                              :id "step-1"
                                              :title "First step"
                                              :description "Do something")))))
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
  (skip-unless (executable-find beads-executable))
  ;; Create a temporary beads directory for testing
  (let* ((temp-dir (make-temp-file "beads-formula-test-" t))
         (default-directory temp-dir)
         (prefix (format "bt%06d" (random 999999))))
    (unwind-protect
        (progn
          ;; Initialize git first - required for bd init
          (call-process "git" nil nil nil "init" "-q")
          (call-process "git" nil nil nil "config" "user.email" "test@beads-test.local")
          (call-process "git" nil nil nil "config" "user.name" "Beads Test")
          ;; Initialize beads in the temp directory
          (let* ((init-cmd (beads-command-init :prefix prefix))
                 (init-exec (beads-command-execute init-cmd)))
            (should (= (oref init-exec exit-code) 0)))
          ;; This test will actually run bd formula list --json
          (let* ((cmd (beads-command-formula-list :json t))
                 (exec (beads-command-execute cmd)))
            ;; Should return execution object
            (should (cl-typep exec 'beads-command-execution))
            ;; Exit code should be 0
            (should (= (oref exec exit-code) 0))
            ;; Result should be a list (possibly empty)
            (should (listp (oref exec result)))))
      ;; Drop the Dolt database to prevent orphan accumulation
      (ignore-errors
        (call-process "bd" nil nil nil
                      "sql" (format "DROP DATABASE IF EXISTS `%s`" prefix)))
      ;; Cleanup: remove temp directory
      (delete-directory temp-dir t))))

;;; ========================================
;;; Formula Show Render Tests
;;; ========================================

(ert-deftest beads-command-formula-test-show-render-header ()
  "Test render-header produces label-value pair."
  (with-temp-buffer
    (beads-formula-show--render-header "Type" "workflow")
    (let ((text (buffer-string)))
      (should (string-match-p "Type:" text))
      (should (string-match-p "workflow" text)))))

(ert-deftest beads-command-formula-test-show-render-header-nil ()
  "Test render-header handles nil value."
  (with-temp-buffer
    (beads-formula-show--render-header "Source" nil)
    (let ((text (buffer-string)))
      (should (string-match-p "Source:" text)))))

(ert-deftest beads-command-formula-test-show-render-section ()
  "Test render-section produces section header."
  (with-temp-buffer
    (beads-formula-show--render-section "Variables")
    (let ((text (buffer-string)))
      (should (string-match-p "Variables" text))
      (should (string-match-p "=========" text)))))

(ert-deftest beads-command-formula-test-show-render-var ()
  "Test render-var produces variable description."
  (with-temp-buffer
    (beads-formula-show--render-var
     "project-name"
     (beads-formula-var
      :name "project-name"
      :description "The project name"
      :default "my-project"
      :required t))
    (let ((text (buffer-string)))
      (should (string-match-p "project-name" text))
      (should (string-match-p "required" text))
      (should (string-match-p "The project name" text))
      (should (string-match-p "my-project" text)))))

(ert-deftest beads-command-formula-test-show-render-var-minimal ()
  "Test render-var with minimal definition."
  (with-temp-buffer
    (beads-formula-show--render-var
     "simple"
     (beads-formula-var :name "simple"))
    (let ((text (buffer-string)))
      (should (string-match-p "simple" text)))))

(ert-deftest beads-command-formula-test-show-render-step ()
  "Test render-step produces step info."
  (with-temp-buffer
    (beads-formula-show--render-step
     (beads-formula-step
      :id "step-1"
      :title "Setup Environment"
      :description "Configure the development environment"
      :needs '("step-0"))
     0)
    (let ((text (buffer-string)))
      (should (string-match-p "1\\. Setup Environment" text))
      (should (string-match-p "ID: step-1" text))
      (should (string-match-p "Needs: step-0" text))
      (should (string-match-p "Configure" text)))))

(ert-deftest beads-command-formula-test-show-render-step-no-needs ()
  "Test render-step without dependencies."
  (with-temp-buffer
    (beads-formula-show--render-step
     (beads-formula-step
      :id "step-2"
      :title "Simple Step")
     1)
    (let ((text (buffer-string)))
      (should (string-match-p "2\\. Simple Step" text))
      (should-not (string-match-p "Needs:" text)))))

;;; Formula List Navigation Tests

(ert-deftest beads-command-formula-test-list-next ()
  "Test formula list next navigation."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (insert "header\nformula-1\nformula-2\n")
      (goto-char (point-min))
      (forward-line 1)
      (beads-formula-list-next)
      ;; Should have moved forward
      (should (= (line-number-at-pos) 3)))))

(ert-deftest beads-command-formula-test-list-previous ()
  "Test formula list previous navigation."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (insert "header\nformula-1\nformula-2\n")
      (goto-char (point-max))
      (forward-line -1)
      (beads-formula-list-previous)
      ;; Should have moved backward
      (should (<= (line-number-at-pos) 2)))))

(ert-deftest beads-command-formula-test-list-quit ()
  "Test formula list quit is callable."
  (should (fboundp 'beads-formula-list-quit)))

(ert-deftest beads-command-formula-test-show-quit ()
  "Test formula show quit is callable."
  (should (fboundp 'beads-formula-show-quit)))

(ert-deftest beads-command-formula-test-show-mode-defined ()
  "Test formula show mode is defined."
  (should (fboundp 'beads-formula-show-mode)))

(ert-deftest beads-command-formula-test-show-mode-keymap ()
  "Test formula show mode has keybindings."
  (should (keymapp beads-formula-show-mode-map))
  (should (lookup-key beads-formula-show-mode-map "g"))
  (should (lookup-key beads-formula-show-mode-map "q"))
  (should (lookup-key beads-formula-show-mode-map "o")))

;;; Formula List Error Path Tests

(ert-deftest beads-command-formula-test-list-show-no-formula ()
  "Test list-show signals error when no formula at point."
  :tags '(:unit)
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (should-error (beads-formula-list-show) :type 'user-error))))

(ert-deftest beads-command-formula-test-list-open-source-no-formula ()
  "Test list-open-source signals error when no formula at point."
  :tags '(:unit)
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (should-error (beads-formula-list-open-source) :type 'user-error))))

(ert-deftest beads-command-formula-test-list-next-at-header ()
  "Test list-next skips header line."
  :tags '(:unit)
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (insert "Name  Type\nformula-1  workflow\nformula-2  expansion\n")
      (goto-char (point-min))
      (beads-formula-list-next)
      ;; Should move forward from header
      (should (> (line-number-at-pos) 1)))))

(ert-deftest beads-command-formula-test-list-previous-at-start ()
  "Test list-previous stops at beginning."
  :tags '(:unit)
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (insert "Name  Type\nformula-1  workflow\n")
      (goto-char (point-min))
      ;; Already at beginning, should not error
      (beads-formula-list-previous)
      (should (= (line-number-at-pos) 1)))))

(ert-deftest beads-command-formula-test-list-next-at-eob ()
  "Test list-next at end of buffer does nothing."
  :tags '(:unit)
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (beads-formula-list-mode)
      (insert "Name  Type\nformula-1  workflow\n")
      (goto-char (point-max))
      (beads-formula-list-next)
      (should (eobp)))))

(ert-deftest beads-command-formula-test-formula-to-entry-with-type ()
  "Test formula-to-entry produces formatted type."
  :tags '(:unit)
  (let* ((formula (beads-formula-summary
                   :name "test-formula"
                   :formula-type "workflow"
                   :description "A test formula"
                   :steps 3
                   :vars 2))
         (entry (beads-formula-list--formula-to-entry formula)))
    (should (equal (car entry) "test-formula"))
    (let ((vec (cadr entry)))
      (should (equal (aref vec 0) "test-formula"))
      ;; Type should be propertized
      (should (stringp (aref vec 1)))
      (should (equal (aref vec 2) "3"))
      (should (equal (aref vec 3) "2"))
      (should (equal (aref vec 4) "A test formula")))))

(ert-deftest beads-command-formula-test-formula-to-entry-nil-fields ()
  "Test formula-to-entry handles nil fields."
  :tags '(:unit)
  (let* ((formula (beads-formula-summary :name nil :formula-type nil))
         (entry (beads-formula-list--formula-to-entry formula)))
    (should entry)
    (let ((vec (cadr entry)))
      (should (equal (aref vec 0) ""))
      (should (equal (aref vec 2) "0"))
      (should (equal (aref vec 3) "0")))))

(ert-deftest beads-command-formula-test-format-type-workflow ()
  "Test format-type applies workflow face."
  :tags '(:unit)
  (let ((result (beads-formula-list--format-type "workflow")))
    (should (equal (get-text-property 0 'face result)
                   'beads-formula-type-workflow))))

(ert-deftest beads-command-formula-test-format-type-unknown ()
  "Test format-type applies default face for unknown type."
  :tags '(:unit)
  (let ((result (beads-formula-list--format-type "bogus")))
    (should (equal (get-text-property 0 'face result)
                   'default))))

(ert-deftest beads-command-formula-test-format-type-nil ()
  "Test format-type handles nil type."
  :tags '(:unit)
  (let ((result (beads-formula-list--format-type nil)))
    (should (stringp result))
    (should (equal result ""))))

;;; ========================================
;;; beads-command-formula-convert Tests
;;; ========================================

(ert-deftest beads-command-formula-test-convert-subcommand ()
  "Test formula convert subcommand name."
  (let ((cmd (beads-command-formula-convert)))
    (should (string= (beads-command-subcommand cmd) "formula convert"))))

(ert-deftest beads-command-formula-test-convert-command-line-basic ()
  "Test formula convert basic command line with name."
  (let* ((cmd (beads-command-formula-convert
               :formula-name "shiny"))
         (args (beads-command-line cmd)))
    (should (member "formula" args))
    (should (member "convert" args))
    (should (member "shiny" args))))

(ert-deftest beads-command-formula-test-convert-command-line-all ()
  "Test formula convert with --all flag."
  (let* ((cmd (beads-command-formula-convert :all t))
         (args (beads-command-line cmd)))
    (should (member "formula" args))
    (should (member "convert" args))
    (should (member "--all" args))))

(ert-deftest beads-command-formula-test-convert-command-line-delete ()
  "Test formula convert with --delete flag."
  (let* ((cmd (beads-command-formula-convert
               :formula-name "shiny" :delete t))
         (args (beads-command-line cmd)))
    (should (member "--delete" args))
    (should (member "shiny" args))))

(ert-deftest beads-command-formula-test-convert-command-line-stdout ()
  "Test formula convert with --stdout flag."
  (let* ((cmd (beads-command-formula-convert
               :formula-name "shiny" :stdout t))
         (args (beads-command-line cmd)))
    (should (member "--stdout" args))
    (should (member "shiny" args))))

(ert-deftest beads-command-formula-test-convert-validate-requires-name-or-all ()
  "Test formula convert requires formula name or --all."
  (let ((cmd (beads-command-formula-convert)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-formula-test-convert-validate-with-name ()
  "Test formula convert validates with name."
  (let ((cmd (beads-command-formula-convert :formula-name "shiny")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-formula-test-convert-validate-with-all ()
  "Test formula convert validates with --all."
  (let ((cmd (beads-command-formula-convert :all t)))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-formula-test-convert-in-menu ()
  "Test formula convert is accessible from formula-menu."
  (require 'beads-command-formula)
  (should (fboundp 'beads-formula-convert)))

(provide 'beads-command-formula-test)
;;; beads-command-formula-test.el ends here
