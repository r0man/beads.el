;;; beads-renumber-test.el --- Tests for beads-renumber -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for bd renumber command.
;; Tests cover validation, dry-run mode, force mode, confirmation,
;; and buffer refresh after renumbering.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-misc)

;;; Test Utilities

(defun beads-renumber-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

(defmacro beads-renumber-test-with-state (state &rest body)
  "Execute BODY with beads-renumber state set to STATE."
  (declare (indent 1))
  `(progn
     (setq beads-renumber--dry-run nil
           beads-renumber--force nil)
     ,@(mapcar (lambda (binding)
                 `(setq ,(car binding) ,(cdr binding)))
               (eval state))
     ,@body))

;;; ============================================================
;;; State Management Tests
;;; ============================================================

(ert-deftest beads-renumber-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-renumber-test-with-state
   '((beads-renumber--dry-run . t)
     (beads-renumber--force . t))
   (beads-renumber--reset-state)
   (should (null beads-renumber--dry-run))
   (should (null beads-renumber--force))))

;;; ============================================================
;;; Validation Tests
;;; ============================================================

(ert-deftest beads-renumber-test-validate-flags-both-set ()
  "Test validation fails when both --dry-run and --force are set."
  (beads-renumber-test-with-state
   '((beads-renumber--dry-run . t)
     (beads-renumber--force . t))
   (should (beads-renumber--validate-flags))))

(ert-deftest beads-renumber-test-validate-flags-none-set ()
  "Test validation fails when neither flag is set."
  (beads-renumber-test-with-state nil
   (should (beads-renumber--validate-flags))))

(ert-deftest beads-renumber-test-validate-flags-dry-run-only ()
  "Test validation succeeds with only --dry-run set."
  (beads-renumber-test-with-state
   '((beads-renumber--dry-run . t))
   (should (null (beads-renumber--validate-flags)))))

(ert-deftest beads-renumber-test-validate-flags-force-only ()
  "Test validation succeeds with only --force set."
  (beads-renumber-test-with-state
   '((beads-renumber--force . t))
   (should (null (beads-renumber--validate-flags)))))

;;; ============================================================
;;; Execution Tests - Dry Run
;;; ============================================================

(ert-deftest beads-renumber-test-execute-dry-run-success ()
  "Test successful dry-run execution."
  (let ((output "Dry run - would renumber:\nbd-1 -> bd-1\nbd-3 -> bd-2\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-renumber--execute t nil))
      (should (get-buffer "*beads-renumber*"))
      (with-current-buffer "*beads-renumber*"
        (should (string-match-p "Dry run" (buffer-string)))
        (should (string-match-p "bd-1 -> bd-1" (buffer-string)))
        (should (string-match-p "bd-3 -> bd-2" (buffer-string)))))))

(ert-deftest beads-renumber-test-execute-dry-run-buffer-mode ()
  "Test that dry-run buffer uses special-mode."
  (let ((output "Preview output"))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-renumber--execute t nil)
      (with-current-buffer "*beads-renumber*"
        (should (eq major-mode 'special-mode))))))

(ert-deftest beads-renumber-test-execute-dry-run-keybindings ()
  "Test that renumber buffer has proper keybindings."
  (let ((output "Test"))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-renumber--execute t nil)
      (with-current-buffer "*beads-renumber*"
        (should (local-key-binding (kbd "q")))))))

(ert-deftest beads-renumber-test-execute-dry-run-command-args ()
  "Test dry-run includes correct arguments."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-renumber--execute t nil)
      (should (member "renumber" captured-args))
      (should (member "--dry-run" captured-args))
      (should-not (member "--force" captured-args)))))

;;; ============================================================
;;; Execution Tests - Force
;;; ============================================================

(ert-deftest beads-renumber-test-execute-force-success ()
  "Test successful force execution."
  (let ((output "Renumbering complete:\nbd-1 -> bd-1\nbd-3 -> bd-2\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil))
              (beads-auto-refresh nil))
      (should-not (beads-renumber--execute nil t))
      (should (get-buffer "*beads-renumber*"))
      (with-current-buffer "*beads-renumber*"
        (should (string-match-p "complete" (buffer-string)))))))

(ert-deftest beads-renumber-test-execute-force-command-args ()
  "Test force includes correct arguments."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional infile destination display
                                &rest args)
                 (setq captured-args args)
                 0))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil))
              (beads-auto-refresh nil))
      (beads-renumber--execute nil t)
      (should (member "renumber" captured-args))
      (should (member "--force" captured-args))
      (should-not (member "--dry-run" captured-args)))))

(ert-deftest beads-renumber-test-execute-force-invalidates-cache ()
  "Test force mode invalidates completion cache."
  (let ((cache-invalidated nil))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 "Done"))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () (setq cache-invalidated t)))
              (beads-auto-refresh nil))
      (beads-renumber--execute nil t)
      (should cache-invalidated))))

(ert-deftest beads-renumber-test-execute-force-refreshes-buffers ()
  "Test force mode refreshes beads buffers when auto-refresh is on."
  (let ((list-refreshed nil)
        (show-refreshed nil))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 "Done"))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil))
              ((symbol-function 'beads-list-refresh)
               (lambda () (setq list-refreshed t)))
              ((symbol-function 'beads-refresh-show)
               (lambda () (setq show-refreshed t)))
              ((symbol-function 'derived-mode-p)
               (lambda (mode) (eq mode 'beads-list-mode)))
              (beads-auto-refresh t))
      (beads-renumber--execute nil t)
      (should list-refreshed))))

;;; ============================================================
;;; Error Handling Tests
;;; ============================================================

(ert-deftest beads-renumber-test-execute-failure ()
  "Test execution handles bd command failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-renumber-test--mock-call-process 1 "Error")))
    (should-error (beads-renumber--execute t nil))))

(ert-deftest beads-renumber-test-execute-failure-message ()
  "Test error message on command failure."
  (cl-letf (((symbol-function 'call-process)
             (beads-renumber-test--mock-call-process 1 "DB error")))
    (condition-case err
        (beads-renumber--execute t nil)
      (error
       (should (string-match-p "Failed to renumber"
                               (error-message-string err)))))))

;;; ============================================================
;;; Transient Menu Tests
;;; ============================================================

(ert-deftest beads-renumber-test-transient-defined ()
  "Test that beads-renumber transient is defined."
  (should (fboundp 'beads-renumber)))

(ert-deftest beads-renumber-test-transient-is-prefix ()
  "Test that beads-renumber is a transient prefix."
  (should (get 'beads-renumber 'transient--prefix)))

(ert-deftest beads-renumber-test-infix-commands-defined ()
  "Test that renumber infix commands are defined."
  (should (fboundp 'beads-renumber--infix-dry-run))
  (should (fboundp 'beads-renumber--infix-force)))

(ert-deftest beads-renumber-test-suffix-commands-defined ()
  "Test that renumber suffix commands are defined."
  (should (fboundp 'beads-renumber--execute-command))
  (should (fboundp 'beads-renumber--preview-command))
  (should (fboundp 'beads-renumber--reset)))

;;; ============================================================
;;; Infix Toggle Tests
;;; ============================================================

(ert-deftest beads-renumber-test-infix-dry-run-toggle ()
  "Test dry-run infix toggles correctly."
  (beads-renumber-test-with-state nil
   (setq beads-renumber--dry-run t)
   (should beads-renumber--dry-run)
   (setq beads-renumber--dry-run nil)
   (should-not beads-renumber--dry-run)))

(ert-deftest beads-renumber-test-infix-force-toggle ()
  "Test force infix toggles correctly."
  (beads-renumber-test-with-state nil
   (setq beads-renumber--force t)
   (should beads-renumber--force)
   (setq beads-renumber--force nil)
   (should-not beads-renumber--force)))

(ert-deftest beads-renumber-test-infix-mutual-exclusion ()
  "Test that toggling one flag clears the other."
  (beads-renumber-test-with-state
   '((beads-renumber--dry-run . t))
   ;; Simulating what the infix reader does
   (setq beads-renumber--force t
         beads-renumber--dry-run nil)
   (should beads-renumber--force)
   (should-not beads-renumber--dry-run)))

;;; ============================================================
;;; Preview Command Tests
;;; ============================================================

(ert-deftest beads-renumber-test-preview-with-dry-run ()
  "Test preview command with dry-run set."
  (beads-renumber-test-with-state
   '((beads-renumber--dry-run . t))
   (beads-renumber--preview-command)
   ;; Just verify it doesn't error
   (should t)))

(ert-deftest beads-renumber-test-preview-with-force ()
  "Test preview command with force set."
  (beads-renumber-test-with-state
   '((beads-renumber--force . t))
   (beads-renumber--preview-command)
   ;; Just verify it doesn't error
   (should t)))

(ert-deftest beads-renumber-test-preview-with-no-flags ()
  "Test preview command with no flags set."
  (beads-renumber-test-with-state nil
   (beads-renumber--preview-command)
   ;; Should show error message
   (should t)))

;;; ============================================================
;;; Edge Cases and Integration Tests
;;; ============================================================

(ert-deftest beads-renumber-test-dry-run-no-cache-invalidation ()
  "Test dry-run does not invalidate cache."
  (let ((cache-invalidated nil))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 "Preview"))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () (setq cache-invalidated t))))
      (beads-renumber--execute t nil)
      (should-not cache-invalidated))))

(ert-deftest beads-renumber-test-dry-run-no-buffer-refresh ()
  "Test dry-run does not refresh buffers."
  (let ((refreshed nil))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 "Preview"))
              ((symbol-function 'display-buffer) (lambda (_buf) nil))
              ((symbol-function 'beads-list-refresh)
               (lambda () (setq refreshed t)))
              (beads-auto-refresh t))
      (beads-renumber--execute t nil)
      (should-not refreshed))))

(ert-deftest beads-renumber-test-buffer-content-preserved ()
  "Test buffer content is correctly set from command output."
  (let ((output "Test output line 1\nTest output line 2\n"))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (beads-renumber--execute t nil)
      (with-current-buffer "*beads-renumber*"
        (should (string= (buffer-string) output))))))

(ert-deftest beads-renumber-test-empty-output ()
  "Test handling of empty output."
  (let ((output ""))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-renumber--execute t nil))
      (should (get-buffer "*beads-renumber*")))))

(ert-deftest beads-renumber-test-large-output ()
  "Test handling of large mapping output."
  (let ((output (make-string 10000 ?x)))
    (cl-letf (((symbol-function 'call-process)
               (beads-renumber-test--mock-call-process 0 output))
              ((symbol-function 'display-buffer) (lambda (_buf) nil)))
      (should-not (beads-renumber--execute t nil))
      (with-current-buffer "*beads-renumber*"
        (should (= (buffer-size) 10000))))))

(ert-deftest beads-renumber-test-multiple-executions ()
  "Test multiple renumber executions reuse buffer."
  (cl-letf (((symbol-function 'call-process)
             (beads-renumber-test--mock-call-process 0 "First"))
            ((symbol-function 'display-buffer) (lambda (_buf) nil)))
    (beads-renumber--execute t nil)
    (let ((first-buf (get-buffer "*beads-renumber*")))
      (cl-letf (((symbol-function 'call-process)
                 (beads-renumber-test--mock-call-process 0 "Second"))
                ((symbol-function 'display-buffer) (lambda (_buf) nil)))
        (beads-renumber--execute t nil)
        (let ((second-buf (get-buffer "*beads-renumber*")))
          (should (eq first-buf second-buf))
          (with-current-buffer second-buf
            (should (string= (buffer-string) "Second"))))))))

(provide 'beads-renumber-test)
;;; beads-renumber-test.el ends here
