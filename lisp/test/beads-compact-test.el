;;; beads-compact-test.el --- Tests for beads-compact -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-compact.el.
;; Tests cover compact command execution, validation, flag handling,
;; and transient menu behavior.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-compact)

;;; Test Utilities

(defun beads-compact-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

(defun beads-compact-test--reset-state ()
  "Reset compact state for testing."
  (beads-compact--reset-state))

;;; Tests for State Management

(ert-deftest beads-compact-test-reset-state ()
  "Test that reset-state clears all variables."
  (setq beads-compact--all t
        beads-compact--dry-run t
        beads-compact--stats t
        beads-compact--issue-id "bd-1"
        beads-compact--force t
        beads-compact--tier 2
        beads-compact--batch-size 20
        beads-compact--workers 10)
  (beads-compact--reset-state)
  (should (null beads-compact--all))
  (should (null beads-compact--dry-run))
  (should (null beads-compact--stats))
  (should (null beads-compact--issue-id))
  (should (null beads-compact--force))
  (should (null beads-compact--tier))
  (should (null beads-compact--batch-size))
  (should (null beads-compact--workers)))

;;; Tests for Validation

(ert-deftest beads-compact-test-validate-tier-valid ()
  "Test tier validation with valid values."
  (setq beads-compact--tier 1)
  (should (null (beads-compact--validate-tier)))
  (setq beads-compact--tier 2)
  (should (null (beads-compact--validate-tier)))
  (setq beads-compact--tier nil)
  (should (null (beads-compact--validate-tier))))

(ert-deftest beads-compact-test-validate-tier-invalid ()
  "Test tier validation with invalid values."
  (setq beads-compact--tier 0)
  (should (stringp (beads-compact--validate-tier)))
  (setq beads-compact--tier 3)
  (should (stringp (beads-compact--validate-tier)))
  (setq beads-compact--tier -1)
  (should (stringp (beads-compact--validate-tier))))

(ert-deftest beads-compact-test-validate-batch-size-valid ()
  "Test batch-size validation with valid values."
  (setq beads-compact--batch-size 1)
  (should (null (beads-compact--validate-batch-size)))
  (setq beads-compact--batch-size 100)
  (should (null (beads-compact--validate-batch-size)))
  (setq beads-compact--batch-size nil)
  (should (null (beads-compact--validate-batch-size))))

(ert-deftest beads-compact-test-validate-batch-size-invalid ()
  "Test batch-size validation with invalid values."
  (setq beads-compact--batch-size 0)
  (should (stringp (beads-compact--validate-batch-size)))
  (setq beads-compact--batch-size -1)
  (should (stringp (beads-compact--validate-batch-size)))
  (setq beads-compact--batch-size "not-a-number")
  (should (stringp (beads-compact--validate-batch-size))))

(ert-deftest beads-compact-test-validate-workers-valid ()
  "Test workers validation with valid values."
  (setq beads-compact--workers 1)
  (should (null (beads-compact--validate-workers)))
  (setq beads-compact--workers 10)
  (should (null (beads-compact--validate-workers)))
  (setq beads-compact--workers nil)
  (should (null (beads-compact--validate-workers))))

(ert-deftest beads-compact-test-validate-workers-invalid ()
  "Test workers validation with invalid values."
  (setq beads-compact--workers 0)
  (should (stringp (beads-compact--validate-workers)))
  (setq beads-compact--workers -5)
  (should (stringp (beads-compact--validate-workers)))
  (setq beads-compact--workers "five")
  (should (stringp (beads-compact--validate-workers))))

(ert-deftest beads-compact-test-validate-force-requires-id ()
  "Test that --force requires --id to be set."
  (setq beads-compact--force t
        beads-compact--issue-id nil)
  (should (stringp (beads-compact--validate-force-requires-id)))

  (setq beads-compact--issue-id "")
  (should (stringp (beads-compact--validate-force-requires-id)))

  (setq beads-compact--issue-id "bd-1")
  (should (null (beads-compact--validate-force-requires-id)))

  (setq beads-compact--force nil)
  (should (null (beads-compact--validate-force-requires-id))))

(ert-deftest beads-compact-test-validate-all ()
  "Test validate-all with multiple errors."
  (beads-compact-test--reset-state)
  (should (null (beads-compact--validate-all)))

  (setq beads-compact--tier 3)
  (let ((errors (beads-compact--validate-all)))
    (should (consp errors))
    (should (= 1 (length errors))))

  (setq beads-compact--batch-size 0)
  (let ((errors (beads-compact--validate-all)))
    (should (consp errors))
    (should (= 2 (length errors))))

  (beads-compact-test--reset-state))

;;; Tests for Argument Building

(ert-deftest beads-compact-test-build-args-empty ()
  "Test building args with no flags set."
  (beads-compact-test--reset-state)
  (let ((args (beads-compact--build-args)))
    (should (null args))))

(ert-deftest beads-compact-test-build-args-all ()
  "Test building args with --all flag."
  (beads-compact-test--reset-state)
  (setq beads-compact--all t)
  (let ((args (beads-compact--build-args)))
    (should (member "--all" args))))

(ert-deftest beads-compact-test-build-args-dry-run ()
  "Test building args with --dry-run flag."
  (beads-compact-test--reset-state)
  (setq beads-compact--dry-run t)
  (let ((args (beads-compact--build-args)))
    (should (member "--dry-run" args))))

(ert-deftest beads-compact-test-build-args-stats ()
  "Test building args with --stats flag."
  (beads-compact-test--reset-state)
  (setq beads-compact--stats t)
  (let ((args (beads-compact--build-args)))
    (should (member "--stats" args))))

(ert-deftest beads-compact-test-build-args-force ()
  "Test building args with --force flag."
  (beads-compact-test--reset-state)
  (setq beads-compact--force t)
  (let ((args (beads-compact--build-args)))
    (should (member "--force" args))))

(ert-deftest beads-compact-test-build-args-id ()
  "Test building args with --id option."
  (beads-compact-test--reset-state)
  (setq beads-compact--issue-id "bd-42")
  (let ((args (beads-compact--build-args)))
    (should (member "--id" args))
    (should (member "bd-42" args))))

(ert-deftest beads-compact-test-build-args-tier ()
  "Test building args with --tier option."
  (beads-compact-test--reset-state)
  (setq beads-compact--tier 2)
  (let ((args (beads-compact--build-args)))
    (should (member "--tier" args))
    (should (member "2" args))))

(ert-deftest beads-compact-test-build-args-batch-size ()
  "Test building args with --batch-size option."
  (beads-compact-test--reset-state)
  (setq beads-compact--batch-size 20)
  (let ((args (beads-compact--build-args)))
    (should (member "--batch-size" args))
    (should (member "20" args))))

(ert-deftest beads-compact-test-build-args-workers ()
  "Test building args with --workers option."
  (beads-compact-test--reset-state)
  (setq beads-compact--workers 10)
  (let ((args (beads-compact--build-args)))
    (should (member "--workers" args))
    (should (member "10" args))))

(ert-deftest beads-compact-test-build-args-multiple ()
  "Test building args with multiple flags set."
  (beads-compact-test--reset-state)
  (setq beads-compact--all t
        beads-compact--dry-run t
        beads-compact--tier 2
        beads-compact--batch-size 15
        beads-compact--workers 8)
  (let ((args (beads-compact--build-args)))
    (should (member "--all" args))
    (should (member "--dry-run" args))
    (should (member "--tier" args))
    (should (member "2" args))
    (should (member "--batch-size" args))
    (should (member "15" args))
    (should (member "--workers" args))
    (should (member "8" args))))

;;; Tests for Execution

(ert-deftest beads-compact-test-execute-success ()
  "Test successful compact execution."
  (beads-compact-test--reset-state)
  (setq beads-compact--dry-run t)
  (let ((output "Compact preview:\nCandidate issues: bd-1, bd-2\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-compact-test--mock-call-process 0 output)))
      (beads-compact--execute-internal (beads-compact--build-args))
      (should (get-buffer "*beads-compact*"))
      (with-current-buffer "*beads-compact*"
        (should (string-match-p "Compact preview" (buffer-string))))
      (kill-buffer "*beads-compact*"))))

(ert-deftest beads-compact-test-execute-failure ()
  "Test compact execution with error."
  (beads-compact-test--reset-state)
  (setq beads-compact--all t)
  (let ((output "Error: no candidates found\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-compact-test--mock-call-process 1 output)))
      (should-error (beads-compact--execute-internal
                     (beads-compact--build-args))))))

(ert-deftest beads-compact-test-execute-displays-buffer ()
  "Test that execution displays results buffer."
  (beads-compact-test--reset-state)
  (setq beads-compact--stats t)
  (let ((output "Statistics:\nCompacted: 5 issues\nSaved: 1.2 MB\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-compact-test--mock-call-process 0 output)))
      (beads-compact--execute-internal (beads-compact--build-args))
      (should (get-buffer "*beads-compact*"))
      (with-current-buffer "*beads-compact*"
        (should (string-match-p "Statistics" (buffer-string)))
        (should (string-match-p "Compacted: 5 issues" (buffer-string))))
      (kill-buffer "*beads-compact*"))))

;;; Tests for Context Detection

(ert-deftest beads-compact-test-detect-issue-id-from-show-buffer ()
  "Test detecting issue ID from beads-show buffer name."
  (with-temp-buffer
    (rename-buffer "*beads-show: bd-123*")
    (let ((id (beads-compact--detect-issue-id)))
      (should (string= id "bd-123")))))

(ert-deftest beads-compact-test-detect-issue-id-no-context ()
  "Test detecting issue ID with no context."
  (with-temp-buffer
    (let ((id (beads-compact--detect-issue-id)))
      (should (null id)))))

;;; Tests for Formatting

(ert-deftest beads-compact-test-format-value ()
  "Test formatting values for display."
  (should (string-match-p "\\[test\\]"
                         (beads-compact--format-value "test")))
  (should (string-match-p "unset"
                         (beads-compact--format-value nil))))

(ert-deftest beads-compact-test-format-bool ()
  "Test formatting boolean values for display."
  (should (string-match-p "enabled"
                         (beads-compact--format-bool t)))
  (should (string-match-p "disabled"
                         (beads-compact--format-bool nil))))

;;; Integration Tests

(ert-deftest beads-compact-test-integration-dry-run ()
  "Test complete dry-run workflow."
  (beads-compact-test--reset-state)
  (setq beads-compact--all t
        beads-compact--dry-run t)
  (let ((output "Dry run - would compact:\nbd-1 (tier 1, 30 days old)\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-compact-test--mock-call-process 0 output)))
      (beads-compact--execute-internal (beads-compact--build-args))
      (should (get-buffer "*beads-compact*"))
      (with-current-buffer "*beads-compact*"
        (should (string-match-p "Dry run" (buffer-string))))
      (kill-buffer "*beads-compact*"))))

(ert-deftest beads-compact-test-integration-specific-issue ()
  "Test compacting specific issue."
  (beads-compact-test--reset-state)
  (setq beads-compact--issue-id "bd-42"
        beads-compact--force t
        beads-compact--tier 2)
  (let ((output "Compacted bd-42 (tier 2)\nOriginal: 10 KB\nCompacted: 0.5 KB\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-compact-test--mock-call-process 0 output)))
      (beads-compact--execute-internal (beads-compact--build-args))
      (should (get-buffer "*beads-compact*"))
      (with-current-buffer "*beads-compact*"
        (should (string-match-p "bd-42" (buffer-string)))
        (should (string-match-p "tier 2" (buffer-string))))
      (kill-buffer "*beads-compact*"))))

(ert-deftest beads-compact-test-integration-stats ()
  "Test displaying statistics."
  (beads-compact-test--reset-state)
  (setq beads-compact--stats t)
  (let ((output "Compaction Statistics:\nTier 1 candidates: 10\nTier 2 candidates: 5\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-compact-test--mock-call-process 0 output)))
      (beads-compact--execute-internal (beads-compact--build-args))
      (should (get-buffer "*beads-compact*"))
      (with-current-buffer "*beads-compact*"
        (should (string-match-p "Statistics" (buffer-string))))
      (kill-buffer "*beads-compact*"))))

;;; Edge Cases

(ert-deftest beads-compact-test-edge-case-empty-id ()
  "Test with empty issue ID string."
  (beads-compact-test--reset-state)
  (setq beads-compact--issue-id "   ")
  (let ((args (beads-compact--build-args)))
    (should-not (member "--id" args))))

(ert-deftest beads-compact-test-edge-case-all-flags-enabled ()
  "Test with all boolean flags enabled."
  (beads-compact-test--reset-state)
  (setq beads-compact--all t
        beads-compact--dry-run t
        beads-compact--stats t
        beads-compact--force t
        beads-compact--issue-id "bd-1")
  (let ((args (beads-compact--build-args)))
    (should (member "--all" args))
    (should (member "--dry-run" args))
    (should (member "--stats" args))
    (should (member "--force" args))
    (should (member "--id" args))))

(ert-deftest beads-compact-test-edge-case-tier-1 ()
  "Test tier 1 compaction."
  (beads-compact-test--reset-state)
  (setq beads-compact--all t
        beads-compact--tier 1)
  (let ((output "Tier 1 compaction completed\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-compact-test--mock-call-process 0 output)))
      (beads-compact--execute-internal (beads-compact--build-args))
      (should (get-buffer "*beads-compact*"))
      (kill-buffer "*beads-compact*"))))

(ert-deftest beads-compact-test-edge-case-tier-2 ()
  "Test tier 2 compaction."
  (beads-compact-test--reset-state)
  (setq beads-compact--all t
        beads-compact--tier 2)
  (let ((output "Tier 2 compaction completed\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-compact-test--mock-call-process 0 output)))
      (beads-compact--execute-internal (beads-compact--build-args))
      (should (get-buffer "*beads-compact*"))
      (kill-buffer "*beads-compact*"))))

(ert-deftest beads-compact-test-edge-case-large-batch ()
  "Test with large batch size."
  (beads-compact-test--reset-state)
  (setq beads-compact--all t
        beads-compact--batch-size 1000)
  (let ((args (beads-compact--build-args)))
    (should (member "--batch-size" args))
    (should (member "1000" args))))

(ert-deftest beads-compact-test-edge-case-many-workers ()
  "Test with many workers."
  (beads-compact-test--reset-state)
  (setq beads-compact--all t
        beads-compact--workers 50)
  (let ((args (beads-compact--build-args)))
    (should (member "--workers" args))
    (should (member "50" args))))

;;; Performance Tests

(ert-deftest beads-compact-test-performance-build-args ()
  "Test argument building performance."
  :tags '(:performance)
  (beads-compact-test--reset-state)
  (setq beads-compact--all t
        beads-compact--dry-run t
        beads-compact--tier 2
        beads-compact--batch-size 20
        beads-compact--workers 10)
  (let ((start-time (current-time)))
    (dotimes (_ 1000)
      (beads-compact--build-args))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should build args 1000 times in under 0.5 seconds
      (should (< elapsed 0.5)))))

(ert-deftest beads-compact-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (beads-compact-test--reset-state)
  (setq beads-compact--tier 1
        beads-compact--batch-size 10
        beads-compact--workers 5)
  (let ((start-time (current-time)))
    (dotimes (_ 1000)
      (beads-compact--validate-all))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should validate 1000 times in under 0.5 seconds
      (should (< elapsed 0.5)))))

;;; Cleanup

(ert-deftest beads-compact-test-cleanup ()
  "Clean up test state."
  (beads-compact-test--reset-state)
  (when (get-buffer "*beads-compact*")
    (kill-buffer "*beads-compact*")))

(provide 'beads-compact-test)
;;; beads-compact-test.el ends here
