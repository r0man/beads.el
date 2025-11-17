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
(require 'beads-test-helper)

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
Integration test that verifies skip-existing doesn't update issues."
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
                ;; Should return result
                (should result)
                ;; Issue should still have modified title
                (let ((updated-issue (beads-command-show!
                                      :issue-ids (list (oref issue id)))))
                  (should (string= (oref updated-issue title)
                                   "Modified title")))))
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

(provide 'beads-import-test)
;;; beads-import-test.el ends here
