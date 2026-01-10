;;; beads-import-test.el --- Tests for beads-import -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; Integration tests for beads-import function and transient menu.
;;
;; These tests verify that the beads-import transient menu and
;; execution functions work correctly.

;;; Code:

(require 'ert)
(require 'beads-import)
(require 'beads-command)
(require 'beads-test)

;;; Unit Tests - Validation

(ert-deftest beads-import-test-validate-input-missing ()
  "Test validation fails when input is missing."
  (let ((result (beads-import--validate-input nil)))
    (should (stringp result))
    (should (string-match-p "required" result))))

(ert-deftest beads-import-test-validate-input-empty ()
  "Test validation fails when input is empty."
  (let ((result (beads-import--validate-input "")))
    (should (stringp result))
    (should (string-match-p "required" result))))

(ert-deftest beads-import-test-validate-input-valid ()
  "Test validation succeeds when input is valid."
  (let ((result (beads-import--validate-input "/tmp/import.jsonl")))
    (should (null result))))

(ert-deftest beads-import-test-get-default-input ()
  "Test getting default input path."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((default-input (beads-import--get-default-input)))
      (should (stringp default-input))
      (should (string-match-p "issues\\.jsonl$" default-input)))))

;;; Unit Tests - Parse Transient Args

(ert-deftest beads-import-test-parse-transient-args-basic ()
  "Test parsing basic transient args."
  (let* ((args '("--input=/tmp/test.jsonl"))
         (cmd (beads-import--parse-transient-args args)))
    (should (beads-command-import-p cmd))
    (should (string= (oref cmd input) "/tmp/test.jsonl"))
    (should (null (oref cmd dry-run)))
    (should (null (oref cmd skip-existing)))
    (should (null (oref cmd orphan-handling)))))

(ert-deftest beads-import-test-parse-transient-args-with-flags ()
  "Test parsing transient args with all flags."
  (let* ((args '("--input=/tmp/test.jsonl"
                 "--dry-run"
                 "--skip-existing"
                 "--clear-duplicate-external-refs"
                 "--dedupe-after"
                 "--rename-on-import"
                 "--strict"
                 "--orphan-handling=strict"))
         (cmd (beads-import--parse-transient-args args)))
    (should (beads-command-import-p cmd))
    (should (string= (oref cmd input) "/tmp/test.jsonl"))
    (should (eq (oref cmd dry-run) t))
    (should (eq (oref cmd skip-existing) t))
    (should (eq (oref cmd clear-duplicate-external-refs) t))
    (should (eq (oref cmd dedupe-after) t))
    (should (eq (oref cmd rename-on-import) t))
    (should (eq (oref cmd strict) t))
    (should (string= (oref cmd orphan-handling) "strict"))))

;;; Integration Tests - Import Execution

(ert-deftest beads-import-test-execute-basic ()
  "Test basic import execution.
Integration test that runs real bd import command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create and export an issue first
    (beads-command-create! :title "Import test issue")
    (let ((temp-file (make-temp-file "beads-import-test-" nil ".jsonl")))
      (unwind-protect
          (progn
            ;; Export to temp file
            (beads-command-export! :output temp-file)
            ;; Create a new project to import into
            (beads-test-with-project ()
              ;; Import from temp file (use rename-on-import for prefix mismatch)
              (let* ((cmd (beads-command-import :input temp-file
                                                :rename-on-import t))
                     (result (beads-import--execute cmd)))
                ;; Should return result
                (should result)
                ;; Verify issue was imported
                (let ((issues (beads-command-list!)))
                  (should (> (length issues) 0))
                  (should (cl-some
                           (lambda (issue)
                             (string= (oref issue title)
                                      "Import test issue"))
                           issues))))))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-import-test-execute-with-dry-run ()
  "Test import with dry-run flag.
Integration test that verifies dry-run doesn't modify database."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create and export an issue
    (beads-command-create! :title "Dry run test issue")
    (let ((temp-file (make-temp-file "beads-import-test-" nil ".jsonl")))
      (unwind-protect
          (progn
            ;; Export to temp file
            (beads-command-export! :output temp-file)
            ;; Create new project and import with dry-run
            (beads-test-with-project ()
              (let* ((cmd (beads-command-import :input temp-file
                                                :dry-run t
                                                :rename-on-import t))
                     (result (beads-import--execute cmd)))
                ;; Should return result
                (should result)
                ;; Database should still be empty (dry-run)
                (let ((issues (beads-command-list!)))
                  (should (= (length issues) 0))))))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-import-test-execute-with-skip-existing ()
  "Test import with skip-existing flag.
Integration test that verifies skip-existing import succeeds."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create initial issue
    (let ((issue (beads-command-create! :title "Original title")))
      (let ((temp-file (make-temp-file "beads-import-test-" nil ".jsonl")))
        (unwind-protect
            (progn
              ;; Export to temp file
              (beads-command-export! :output temp-file)
              ;; Modify issue title in database
              (beads-command-update! :issue-ids (list (oref issue id))
                                     :title "Modified title")
              ;; Import with skip-existing (should not update)
              (let* ((cmd (beads-command-import :input temp-file
                                                :skip-existing t))
                     (result (beads-import--execute cmd)))
                ;; Should return result (data slot value - typically a string)
                (should result)
                ;; Result should be a string (raw stderr output)
                (should (stringp result))))
          ;; Clean up temp file
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))))

(ert-deftest beads-import-test-transient-menu-exists ()
  "Test that beads-import transient menu is defined."
  (should (commandp 'beads-import))
  (should (get 'beads-import 'transient--prefix)))

(ert-deftest beads-import-test-suffix-commands-exist ()
  "Test that import suffix commands are defined."
  (should (commandp 'beads-import--execute-command))
  (should (commandp 'beads-import--preview))
  (should (commandp 'beads-import--reset)))

(ert-deftest beads-import-test-validate-all-missing-input ()
  "Test validate-all with missing input."
  (let* ((cmd (beads-command-import :input nil))
         (errors (beads-import--validate-all cmd)))
    (should errors)
    (should (= (length errors) 1))
    (should (string-match-p "required" (car errors)))))

(ert-deftest beads-import-test-validate-all-invalid-orphan-handling ()
  "Test validate-all with invalid orphan-handling."
  (let* ((cmd (beads-command-import
               :input "/tmp/test.jsonl"
               :orphan-handling "invalid"))
         (errors (beads-import--validate-all cmd)))
    (should errors)
    (should (string-match-p "orphan-handling" (car errors)))))

(ert-deftest beads-import-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (let* ((cmd (beads-command-import
               :input "/tmp/test.jsonl"
               :orphan-handling "strict"
               :dry-run t))
         (errors (beads-import--validate-all cmd)))
    (should (null errors))))

;;; Additional Validation Tests

(ert-deftest beads-import-test-validate-orphan-handling-resurrect ()
  "Test validation accepts resurrect orphan-handling."
  (let* ((cmd (beads-command-import
               :input "/tmp/test.jsonl"
               :orphan-handling "resurrect"))
         (errors (beads-import--validate-all cmd)))
    (should (null errors))))

(ert-deftest beads-import-test-validate-orphan-handling-allow ()
  "Test validation accepts allow orphan-handling."
  (let* ((cmd (beads-command-import
               :input "/tmp/test.jsonl"
               :orphan-handling "allow"))
         (errors (beads-import--validate-all cmd)))
    (should (null errors))))

(ert-deftest beads-import-test-validate-orphan-handling-skip ()
  "Test validation accepts skip orphan-handling."
  (let* ((cmd (beads-command-import
               :input "/tmp/test.jsonl"
               :orphan-handling "skip"))
         (errors (beads-import--validate-all cmd)))
    (should (null errors))))

;;; Command Line Generation Tests

(ert-deftest beads-import-test-command-line-basic ()
  "Test command line generation for basic import."
  (let* ((cmd (beads-command-import :input "/tmp/test.jsonl"))
         (args (beads-command-line cmd)))
    (should (member "import" args))
    ;; Uses --input for input flag (slot metadata uses long form)
    (should (member "--input" args))
    (should (member "/tmp/test.jsonl" args))))

(ert-deftest beads-import-test-command-line-with-dry-run ()
  "Test command line generation with dry-run flag."
  (let* ((cmd (beads-command-import :input "/tmp/test.jsonl"
                                    :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "import" args))
    (should (or (member "-n" args)
                (member "--dry-run" args)))))

(ert-deftest beads-import-test-command-line-with-skip-existing ()
  "Test command line generation with skip-existing flag."
  (let* ((cmd (beads-command-import :input "/tmp/test.jsonl"
                                    :skip-existing t))
         (args (beads-command-line cmd)))
    (should (member "import" args))
    (should (or (member "-s" args)
                (member "--skip-existing" args)))))

(ert-deftest beads-import-test-command-line-with-orphan-handling ()
  "Test command line generation with orphan-handling."
  (let* ((cmd (beads-command-import :input "/tmp/test.jsonl"
                                    :orphan-handling "strict"))
         (args (beads-command-line cmd)))
    (should (member "import" args))
    ;; orphan-handling should be in args somehow
    (should (cl-some (lambda (arg) (string-match-p "strict" arg)) args))))

(ert-deftest beads-import-test-command-line-with-clear-duplicate-external-refs ()
  "Test command line generation with clear-duplicate-external-refs."
  (let* ((cmd (beads-command-import :input "/tmp/test.jsonl"
                                    :clear-duplicate-external-refs t))
         (args (beads-command-line cmd)))
    (should (member "import" args))
    (should (or (member "-c" args)
                (member "--clear-duplicate-external-refs" args)))))

(ert-deftest beads-import-test-command-line-with-dedupe-after ()
  "Test command line generation with dedupe-after."
  (let* ((cmd (beads-command-import :input "/tmp/test.jsonl"
                                    :dedupe-after t))
         (args (beads-command-line cmd)))
    (should (member "import" args))
    (should (or (member "-d" args)
                (member "--dedupe-after" args)))))

(ert-deftest beads-import-test-command-line-with-rename-on-import ()
  "Test command line generation with rename-on-import."
  (let* ((cmd (beads-command-import :input "/tmp/test.jsonl"
                                    :rename-on-import t))
         (args (beads-command-line cmd)))
    (should (member "import" args))
    (should (or (member "-r" args)
                (member "--rename-on-import" args)))))

(ert-deftest beads-import-test-command-line-with-strict ()
  "Test command line generation with strict."
  (let* ((cmd (beads-command-import :input "/tmp/test.jsonl"
                                    :strict t))
         (args (beads-command-line cmd)))
    (should (member "import" args))
    (should (or (member "-t" args)
                (member "--strict" args)))))

;;; Tests for Reset Function

(ert-deftest beads-import-test-reset-confirmed ()
  "Test reset when user confirms."
  (let ((reset-called nil)
        (message-output nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t)))
              ((symbol-function 'transient--redisplay)
               (lambda ()))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-import--reset)
      (should reset-called)
      (should (string-match-p "reset" message-output)))))

(ert-deftest beads-import-test-reset-declined ()
  "Test reset when user declines."
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t))))
      (beads-import--reset)
      (should-not reset-called))))

;;; Tests for Execute Function

(ert-deftest beads-import-test-execute-command-with-default-input ()
  "Test execute-command uses default input when not specified."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((executed nil)
          (input-used nil))
      (cl-letf (((symbol-function 'transient-args)
                 (lambda (_prefix)
                   '()))  ; No input specified
                ((symbol-function 'beads-import--execute)
                 (lambda (cmd)
                   (setq executed t)
                   (setq input-used (oref cmd input)))))
        (beads-import--execute-command)
        (should executed)
        (should (stringp input-used))
        (should (string-match-p "issues\\.jsonl$" input-used))))))

;;; Tests for Preview Function

(ert-deftest beads-import-test-preview-displays-command ()
  "Test preview displays command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((message-output nil))
      (cl-letf (((symbol-function 'transient-args)
                 (lambda (_prefix)
                   '("--input=/tmp/test.jsonl")))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-output (apply #'format fmt args)))))
        (beads-import--preview)
        (should message-output)
        (should (string-match-p "import" message-output))))))

;;; Preview with Default Input

(ert-deftest beads-import-test-preview-uses-default-input ()
  "Test preview uses default input when not specified."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '()))
              ((symbol-function 'beads-import--get-default-input)
               (lambda () "/tmp/default.jsonl"))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-import--preview)
      (should message-output)
      (should (string-match-p "Command:" message-output)))))

;;; Execute with Validation Error

(ert-deftest beads-import-test-execute-command-validation-error ()
  "Test execute-command handles validation errors."
  :tags '(:unit)
  (let ((error-caught nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--input=")))  ; Empty input (invalid)
              ((symbol-function 'beads-import--get-default-input)
               (lambda () "")))  ; Also empty
      (condition-case nil
          (beads-import--execute-command)
        (user-error (setq error-caught t)))
      (should error-caught))))

;;; Execute with Error

(ert-deftest beads-import-test-execute-error-handling ()
  "Test execute handles errors gracefully."
  :tags '(:unit)
  (let ((cmd (beads-command-import :input "/tmp/test.jsonl"))
        (error-signaled nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) (error "Import failed")))
              ((symbol-function 'beads--error)
               (lambda (&rest _args) (setq error-signaled t))))
      (beads-import--execute cmd)
      (should error-signaled))))

(provide 'beads-import-test)
;;; beads-import-test.el ends here
