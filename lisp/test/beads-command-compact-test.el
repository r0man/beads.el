;;; beads-command-compact-test.el --- Tests for beads-command-compact -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Unit tests for beads-command-compact command classes.
;; Tests cover all compact modes: stats, prune, purge, analyze, apply, auto.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads-command-compact)

;;; Test Utilities

(defun beads-compact-test--mock-process-file (exit-code output)
  "Create a mock for `process-file' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile buffer display &rest args)
    (when buffer
      (let ((buf (if (listp buffer) (car buffer) buffer)))
        (when buf
          (with-current-buffer (if (bufferp buf) buf (current-buffer))
            (insert output)))))
    exit-code))

;;; Tests for Stats Command

(ert-deftest beads-compact-test-stats-class-exists ()
  "Test that beads-command-compact-stats class is defined."
  (should (cl-find-class 'beads-command-compact-stats)))

(ert-deftest beads-compact-test-stats-subcommand ()
  "Test that subcommand returns 'compact'."
  (let ((cmd (beads-command-compact-stats)))
    (should (equal (beads-command-subcommand cmd) "compact"))))

(ert-deftest beads-compact-test-stats-command-line ()
  "Test that stats command line includes --stats."
  (let* ((cmd (beads-command-compact-stats))
         (args (beads-command-line cmd)))
    (should (member "compact" args))
    (should (member "--stats" args))))

;;; Tests for Prune Command

(ert-deftest beads-compact-test-prune-class-exists ()
  "Test that beads-command-compact-prune class is defined."
  (should (cl-find-class 'beads-command-compact-prune)))

(ert-deftest beads-compact-test-prune-subcommand ()
  "Test that subcommand returns 'compact'."
  (let ((cmd (beads-command-compact-prune)))
    (should (equal (beads-command-subcommand cmd) "compact"))))

(ert-deftest beads-compact-test-prune-command-line-basic ()
  "Test that prune command line includes --prune."
  (let* ((cmd (beads-command-compact-prune))
         (args (beads-command-line cmd)))
    (should (member "compact" args))
    (should (member "--prune" args))))

(ert-deftest beads-compact-test-prune-command-line-older-than ()
  "Test prune command line with --older-than option."
  (let* ((cmd (beads-command-compact-prune :older-than "7"))
         (args (beads-command-line cmd)))
    (should (member "--prune" args))
    (should (member "--older-than" args))
    (should (member "7" args))))

(ert-deftest beads-compact-test-prune-command-line-dry-run ()
  "Test prune command line with --dry-run option."
  (let* ((cmd (beads-command-compact-prune :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--prune" args))
    (should (member "--dry-run" args))))

;;; Tests for Purge Command

(ert-deftest beads-compact-test-purge-class-exists ()
  "Test that beads-command-compact-purge class is defined."
  (should (cl-find-class 'beads-command-compact-purge)))

(ert-deftest beads-compact-test-purge-command-line ()
  "Test that purge command line includes --purge-tombstones."
  (let* ((cmd (beads-command-compact-purge))
         (args (beads-command-line cmd)))
    (should (member "compact" args))
    (should (member "--purge-tombstones" args))))

(ert-deftest beads-compact-test-purge-command-line-dry-run ()
  "Test purge command line with --dry-run option."
  (let* ((cmd (beads-command-compact-purge :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--purge-tombstones" args))
    (should (member "--dry-run" args))))

;;; Tests for Analyze Command

(ert-deftest beads-compact-test-analyze-class-exists ()
  "Test that beads-command-compact-analyze class is defined."
  (should (cl-find-class 'beads-command-compact-analyze)))

(ert-deftest beads-compact-test-analyze-command-line-basic ()
  "Test that analyze command line includes --analyze."
  (let* ((cmd (beads-command-compact-analyze))
         (args (beads-command-line cmd)))
    (should (member "compact" args))
    (should (member "--analyze" args))))

(ert-deftest beads-compact-test-analyze-command-line-tier ()
  "Test analyze command line with --tier option."
  (let* ((cmd (beads-command-compact-analyze :tier "2"))
         (args (beads-command-line cmd)))
    (should (member "--analyze" args))
    (should (member "--tier" args))
    (should (member "2" args))))

(ert-deftest beads-compact-test-analyze-command-line-limit ()
  "Test analyze command line with --limit option."
  (let* ((cmd (beads-command-compact-analyze :limit "10"))
         (args (beads-command-line cmd)))
    (should (member "--analyze" args))
    (should (member "--limit" args))
    (should (member "10" args))))

;;; Tests for Apply Command

(ert-deftest beads-compact-test-apply-class-exists ()
  "Test that beads-command-compact-apply class is defined."
  (should (cl-find-class 'beads-command-compact-apply)))

(ert-deftest beads-compact-test-apply-command-line-basic ()
  "Test that apply command line includes --apply."
  (let* ((cmd (beads-command-compact-apply :issue-id "bd-1" :summary "sum.txt"))
         (args (beads-command-line cmd)))
    (should (member "compact" args))
    (should (member "--apply" args))
    (should (member "--id" args))
    (should (member "bd-1" args))
    (should (member "--summary" args))
    (should (member "sum.txt" args))))

(ert-deftest beads-compact-test-apply-validation-missing-id ()
  "Test apply validation fails without issue-id."
  (let ((cmd (beads-command-compact-apply :summary "sum.txt")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-compact-test-apply-validation-missing-summary ()
  "Test apply validation fails without summary."
  (let ((cmd (beads-command-compact-apply :issue-id "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-compact-test-apply-validation-success ()
  "Test apply validation succeeds with required fields."
  (let ((cmd (beads-command-compact-apply :issue-id "bd-1" :summary "sum.txt")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-compact-test-apply-validation-empty-id ()
  "Test apply validation fails with empty issue-id."
  (let ((cmd (beads-command-compact-apply :issue-id "" :summary "sum.txt")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-compact-test-apply-validation-empty-summary ()
  "Test apply validation fails with empty summary."
  (let ((cmd (beads-command-compact-apply :issue-id "bd-1" :summary "")))
    (should (beads-command-validate cmd))))

;;; Tests for Auto Command

(ert-deftest beads-compact-test-auto-class-exists ()
  "Test that beads-command-compact-auto class is defined."
  (should (cl-find-class 'beads-command-compact-auto)))

(ert-deftest beads-compact-test-auto-command-line-basic ()
  "Test that auto command line includes --auto."
  (let* ((cmd (beads-command-compact-auto :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "compact" args))
    (should (member "--auto" args))
    (should (member "--id" args))
    (should (member "bd-1" args))))

(ert-deftest beads-compact-test-auto-command-line-all ()
  "Test auto command line with --all option."
  (let* ((cmd (beads-command-compact-auto :all t))
         (args (beads-command-line cmd)))
    (should (member "--auto" args))
    (should (member "--all" args))))

(ert-deftest beads-compact-test-auto-command-line-dry-run ()
  "Test auto command line with --dry-run option."
  (let* ((cmd (beads-command-compact-auto :issue-id "bd-1" :dry-run t))
         (args (beads-command-line cmd)))
    (should (member "--auto" args))
    (should (member "--dry-run" args))))

(ert-deftest beads-compact-test-auto-validation-missing-target ()
  "Test auto validation fails without --id or --all."
  (let ((cmd (beads-command-compact-auto)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-compact-test-auto-validation-force-without-id ()
  "Test auto validation fails with --force but no --id."
  (let ((cmd (beads-command-compact-auto :force t :all t)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-compact-test-auto-validation-success-with-id ()
  "Test auto validation succeeds with --id."
  (let ((cmd (beads-command-compact-auto :issue-id "bd-1")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-compact-test-auto-validation-success-with-all ()
  "Test auto validation succeeds with --all."
  (let ((cmd (beads-command-compact-auto :all t)))
    (should (null (beads-command-validate cmd)))))

;;; Tests for Transient Definitions

(ert-deftest beads-compact-test-transient-stats-defined ()
  "Test that beads-compact-stats transient is defined."
  (should (fboundp 'beads-compact-stats)))

(ert-deftest beads-compact-test-transient-prune-defined ()
  "Test that beads-compact-prune transient is defined."
  (should (fboundp 'beads-compact-prune)))

(ert-deftest beads-compact-test-transient-purge-defined ()
  "Test that beads-compact-purge transient is defined."
  (should (fboundp 'beads-compact-purge)))

(ert-deftest beads-compact-test-transient-analyze-defined ()
  "Test that beads-compact-analyze transient is defined."
  (should (fboundp 'beads-compact-analyze)))

(ert-deftest beads-compact-test-transient-apply-defined ()
  "Test that beads-compact-apply transient is defined."
  (should (fboundp 'beads-compact-apply)))

(ert-deftest beads-compact-test-transient-auto-defined ()
  "Test that beads-compact-auto transient is defined."
  (should (fboundp 'beads-compact-auto)))

(ert-deftest beads-compact-test-menu-defined ()
  "Test that beads-compact parent menu is defined."
  (should (fboundp 'beads-compact)))

;;; Tests for Convenience Functions

(ert-deftest beads-compact-test-bang-functions-exist ()
  "Test that convenience functions are defined."
  (should (fboundp 'beads-command-compact-stats!))
  (should (fboundp 'beads-command-compact-prune!))
  (should (fboundp 'beads-command-compact-purge!))
  (should (fboundp 'beads-command-compact-analyze!))
  (should (fboundp 'beads-command-compact-apply!))
  (should (fboundp 'beads-command-compact-auto!)))

;;; Integration Tests

(ert-deftest beads-compact-test-parse-transient-args-stats ()
  "Test parsing transient args for stats command."
  (let ((cmd (beads-compact-stats--parse-transient-args nil)))
    (should (beads-command-compact-stats-p cmd))))

(ert-deftest beads-compact-test-parse-transient-args-prune ()
  "Test parsing transient args for prune command."
  (let ((cmd (beads-compact-prune--parse-transient-args
              '("--older-than=14" "--dry-run"))))
    (should (beads-command-compact-prune-p cmd))
    ;; Transient returns string values for options
    (should (equal (oref cmd older-than) "14"))
    (should (oref cmd dry-run))))

(ert-deftest beads-compact-test-parse-transient-args-analyze ()
  "Test parsing transient args for analyze command."
  (let ((cmd (beads-compact-analyze--parse-transient-args
              '("--tier=2" "--limit=5"))))
    (should (beads-command-compact-analyze-p cmd))
    ;; Transient returns string values for options
    (should (equal (oref cmd tier) "2"))
    (should (equal (oref cmd limit) "5"))))

(ert-deftest beads-compact-test-parse-transient-args-apply ()
  "Test parsing transient args for apply command."
  (let ((cmd (beads-compact-apply--parse-transient-args
              '("--id=bd-42" "--summary=summary.txt"))))
    (should (beads-command-compact-apply-p cmd))
    (should (equal (oref cmd issue-id) "bd-42"))
    (should (equal (oref cmd summary) "summary.txt"))))

(ert-deftest beads-compact-test-parse-transient-args-auto ()
  "Test parsing transient args for auto command."
  (let ((cmd (beads-compact-auto--parse-transient-args
              '("--id=bd-42" "--dry-run" "--tier=1"))))
    (should (beads-command-compact-auto-p cmd))
    (should (equal (oref cmd issue-id) "bd-42"))
    (should (oref cmd dry-run))
    ;; Transient returns string values for options
    (should (equal (oref cmd tier) "1"))))

(provide 'beads-command-compact-test)
;;; beads-command-compact-test.el ends here
