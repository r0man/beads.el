;;; beads-export-test.el --- Tests for beads-export -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; Integration tests for beads-export function and transient menu.
;;
;; These tests verify that the beads-export transient menu and
;; execution functions work correctly.

;;; Code:

(require 'ert)
(require 'beads-command-export)
(require 'beads-command)
(require 'beads-test)

;;; Unit Tests - Validation

(ert-deftest beads-export-test-validate-output-missing ()
  "Test validation fails when output is missing."
  (let ((result (beads-export--validate-output nil)))
    (should (stringp result))
    (should (string-match-p "required" result))))

(ert-deftest beads-export-test-validate-output-empty ()
  "Test validation fails when output is empty."
  (let ((result (beads-export--validate-output "")))
    (should (stringp result))
    (should (string-match-p "required" result))))

(ert-deftest beads-export-test-validate-output-valid ()
  "Test validation succeeds when output is valid."
  (let ((result (beads-export--validate-output "/tmp/export.jsonl")))
    (should (null result))))

(ert-deftest beads-export-test-get-default-output ()
  "Test getting default output path."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((default-output (beads-export--get-default-output)))
      (should (stringp default-output))
      (should (string-match-p "issues\\.jsonl$" default-output)))))

;;; Unit Tests - Parse Transient Args

(ert-deftest beads-export-test-parse-transient-args-basic ()
  "Test parsing basic transient args."
  (let* ((args '("--output=/tmp/test.jsonl"))
         (cmd (beads-export--parse-transient-args args)))
    (should (beads-command-export-p cmd))
    (should (string= (oref cmd output) "/tmp/test.jsonl"))
    (should (null (oref cmd format)))
    (should (null (oref cmd status)))
    (should (null (oref cmd force)))))

(ert-deftest beads-export-test-parse-transient-args-with-flags ()
  "Test parsing transient args with all flags."
  (let* ((args '("--output=/tmp/test.jsonl"
                 "--format=jsonl"
                 "--status=open"
                 "--force"))
         (cmd (beads-export--parse-transient-args args)))
    (should (beads-command-export-p cmd))
    (should (string= (oref cmd output) "/tmp/test.jsonl"))
    (should (string= (oref cmd format) "jsonl"))
    (should (string= (oref cmd status) "open"))
    (should (eq (oref cmd force) t))))

;;; Integration Tests - Export Execution

(ert-deftest beads-export-test-execute-basic ()
  "Test basic export execution.
Integration test that runs real bd export command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a test issue
    (beads-command-create! :title "Export test issue")
    ;; Export to temp file
    (let* ((temp-file (make-temp-file "beads-export-test-" nil ".jsonl"))
           (cmd (beads-command-export :output temp-file)))
      (unwind-protect
          (progn
            ;; Execute export
            (beads-export--execute cmd)
            ;; File should exist
            (should (file-exists-p temp-file))
            ;; File should contain data
            (should (> (nth 7 (file-attributes temp-file)) 0)))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-export-test-execute-with-status-filter ()
  "Test export with status filter.
Integration test that verifies status filtering."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create issues with different statuses
    (beads-command-create! :title "Open issue")
    (let ((closed-issue (beads-command-create! :title "Closed issue")))
      (beads-command-close! :issue-ids (list (oref closed-issue id))
                            :reason "Test"))
    ;; Export only open issues
    (let* ((temp-file (make-temp-file "beads-export-test-" nil ".jsonl"))
           (cmd (beads-command-export :output temp-file
                                      :status "open")))
      (unwind-protect
          (progn
            ;; Execute export
            (beads-export--execute cmd)
            ;; File should exist
            (should (file-exists-p temp-file))
            ;; Verify file contains only open issues
            (with-temp-buffer
              (insert-file-contents temp-file)
              (let ((content (buffer-string)))
                (should (string-match-p "\"status\":\"open\"" content))
                ;; Should not contain closed status
                (should-not (string-match-p "\"status\":\"closed\"" content)))))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-export-test-execute-with-force ()
  "Test export with force flag.
Integration test that verifies force flag works."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Don't create any issues - database is empty
    (let* ((temp-file (make-temp-file "beads-export-test-" nil ".jsonl"))
           (cmd (beads-command-export :output temp-file
                                      :force t)))
      (unwind-protect
          (progn
            ;; Execute export with force
            (beads-export--execute cmd)
            ;; File should exist even if db is empty
            (should (file-exists-p temp-file)))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-export-test-transient-menu-exists ()
  "Test that beads-export transient menu is defined."
  (should (commandp 'beads-export))
  (should (get 'beads-export 'transient--prefix)))

(ert-deftest beads-export-test-suffix-commands-exist ()
  "Test that export suffix commands are defined."
  (should (commandp 'beads-export--execute-command))
  (should (commandp 'beads-export--preview))
  (should (commandp 'beads-export--reset)))

(ert-deftest beads-export-test-validate-all-missing-output ()
  "Test validate-all with missing output."
  (let* ((cmd (beads-command-export :output nil))
         (errors (beads-export--validate-all cmd)))
    (should errors)
    (should (= (length errors) 1))
    (should (string-match-p "required" (car errors)))))

(ert-deftest beads-export-test-validate-all-invalid-format ()
  "Test validate-all with invalid format."
  (let* ((cmd (beads-command-export
               :output "/tmp/test.jsonl"
               :format "invalid"))
         (errors (beads-export--validate-all cmd)))
    (should errors)
    (should (string-match-p "format" (car errors)))))

(ert-deftest beads-export-test-validate-all-invalid-status ()
  "Test validate-all with invalid status."
  (let* ((cmd (beads-command-export
               :output "/tmp/test.jsonl"
               :status "invalid"))
         (errors (beads-export--validate-all cmd)))
    (should errors)
    (should (string-match-p "status" (car errors)))))

(ert-deftest beads-export-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (let* ((cmd (beads-command-export
               :output "/tmp/test.jsonl"
               :format "jsonl"
               :status "open"))
         (errors (beads-export--validate-all cmd)))
    (should (null errors))))

;;; Transient Suffix Tests
;;
;; These tests exercise the transient suffix commands by mocking
;; transient-args to bypass the interactive UI.

(ert-deftest beads-export-test-execute-command-flow ()
  "Test execute-command suffix flow with mocked transient-args."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create test issue
    (beads-command-create! :title "Export flow test")
    (let ((temp-file (make-temp-file "beads-export-test-" nil ".jsonl")))
      (unwind-protect
          (cl-letf (((symbol-function 'transient-args)
                     (lambda (_prefix)
                       (list (format "--output=%s" temp-file)))))
            (beads-export--execute-command)
            (should (file-exists-p temp-file)))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-export-test-preview-command ()
  "Test preview command shows correct command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((message-output nil))
      (cl-letf (((symbol-function 'transient-args)
                 (lambda (_prefix)
                   '("--output=/tmp/test.jsonl" "--status=open")))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq message-output (apply #'format fmt args)))))
        (beads-export--preview)
        (should message-output)
        (should (string-match-p "export" message-output))
        (should (string-match-p "-o" message-output))))))

(ert-deftest beads-export-test-execute-error-handling ()
  "Test execute handles errors gracefully."
  :tags '(:unit)
  (let* ((cmd (beads-command-export :output "/tmp/test.jsonl"))
         (error-called nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) (error "Test error")))
              ((symbol-function 'beads--error)
               (lambda (_fmt &rest _args) (setq error-called t))))
      (beads-export--execute cmd)
      (should error-called))))

(ert-deftest beads-export-test-execute-command-with-default-output ()
  "Test execute-command uses default output when not specified."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create test issue
    (beads-command-create! :title "Default output test")
    (let ((executed nil)
          (output-used nil))
      (cl-letf (((symbol-function 'transient-args)
                 (lambda (_prefix)
                   '()))  ; No output specified
                ((symbol-function 'beads-export--execute)
                 (lambda (cmd)
                   (setq executed t)
                   (setq output-used (oref cmd output)))))
        (beads-export--execute-command)
        (should executed)
        (should (stringp output-used))
        (should (string-match-p "issues\\.jsonl$" output-used))))))

(ert-deftest beads-export-test-execute-command-validation-error ()
  "Test execute-command handles validation errors."
  :tags '(:unit)
  (let ((error-caught nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix)
                 '("--format=invalid")))  ; Invalid format
              ((symbol-function 'beads-export--get-default-output)
               (lambda () "/tmp/test.jsonl")))
      (condition-case nil
          (beads-export--execute-command)
        (user-error (setq error-caught t)))
      (should error-caught))))

;;; Reset Tests

(ert-deftest beads-export-test-reset-confirmed ()
  "Test reset when user confirms."
  :tags '(:unit)
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
      (beads-export--reset)
      (should reset-called)
      (should (string-match-p "reset" message-output)))))

(ert-deftest beads-export-test-reset-declined ()
  "Test reset when user declines."
  :tags '(:unit)
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t))))
      (beads-export--reset)
      (should-not reset-called))))

;;; Preview Tests

(ert-deftest beads-export-test-preview-with-default-output ()
  "Test preview uses default output when not specified."
  :tags '(:unit)
  (let ((message-output nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '()))
              ((symbol-function 'beads-export--get-default-output)
               (lambda () "/tmp/default.jsonl"))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-export--preview)
      (should message-output)
      (should (string-match-p "Command:" message-output))
      (should (string-match-p "export" message-output)))))

(provide 'beads-export-test)
;;; beads-export-test.el ends here
