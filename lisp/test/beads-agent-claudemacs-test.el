;;; beads-agent-claudemacs-test.el --- Tests for claudemacs backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-agent-claudemacs.el - the claudemacs backend.
;; These tests verify the buffer finding logic and other backend methods
;; without requiring the actual claudemacs package.
;;
;; claudemacs buffer naming convention:
;; - Main buffer: *claudemacs:TOOL:SESSION-ID*
;; - Instance buffer: *claudemacs:TOOL-N:SESSION-ID*
;; Where TOOL is claude/codex/gemini, N is instance number, and
;; SESSION-ID is derived from workspace/project name.
;;
;; Unlike claude-code.el which uses buffer name patterns, claudemacs
;; uses a buffer-local variable `claudemacs--cwd' to track the working
;; directory for each session.

;;; Code:

(require 'ert)
(require 'beads-agent-backend)
(require 'beads-agent-claudemacs)

;;; Buffer Finding Tests
;;
;; These tests verify that `beads-agent-claudemacs--find-buffers' correctly
;; identifies claudemacs buffers based on the `claudemacs--cwd' variable.

(ert-deftest beads-agent-claudemacs-test-find-buffers-by-cwd ()
  "Test finding claudemacs buffer by its `claudemacs--cwd' variable."
  (let ((test-buf (generate-new-buffer "*claudemacs:claude:test*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (setq-local claudemacs--cwd "/home/roman/workspace/test/"))
          (let ((found (beads-agent-claudemacs--find-buffers
                        "/home/roman/workspace/test/")))
            (should (= 1 (length found)))
            (should (eq (car found) test-buf))))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claudemacs-test-find-buffers-without-trailing-slash ()
  "Test that input without trailing slash still finds buffers."
  (let ((test-buf (generate-new-buffer "*claudemacs:claude:test*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (setq-local claudemacs--cwd "/home/roman/workspace/test/"))
          (let ((found (beads-agent-claudemacs--find-buffers
                        "/home/roman/workspace/test")))
            (should (= 1 (length found)))
            (should (eq (car found) test-buf))))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claudemacs-test-find-buffers-multiple ()
  "Test finding multiple claudemacs buffers for same directory."
  (let ((buf1 (generate-new-buffer "*claudemacs:claude:test*"))
        (buf2 (generate-new-buffer "*claudemacs:claude-2:test*"))
        (buf3 (generate-new-buffer "*claudemacs:codex:test*")))
    (unwind-protect
        (progn
          ;; Set all buffers to same working directory
          (dolist (buf (list buf1 buf2 buf3))
            (with-current-buffer buf
              (setq-local claudemacs--cwd "/home/roman/workspace/test/")))
          (let ((found (beads-agent-claudemacs--find-buffers
                        "/home/roman/workspace/test/")))
            (should (= 3 (length found)))
            (should (memq buf1 found))
            (should (memq buf2 found))
            (should (memq buf3 found))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

(ert-deftest beads-agent-claudemacs-test-find-buffers-different-projects ()
  "Test that we don't find buffers from other projects."
  (let ((our-buf (generate-new-buffer "*claudemacs:claude:test*"))
        (other-buf (generate-new-buffer "*claudemacs:claude:other*")))
    (unwind-protect
        (progn
          (with-current-buffer our-buf
            (setq-local claudemacs--cwd "/home/roman/workspace/test/"))
          (with-current-buffer other-buf
            (setq-local claudemacs--cwd "/home/roman/workspace/other/"))
          (let ((found (beads-agent-claudemacs--find-buffers
                        "/home/roman/workspace/test/")))
            (should (= 1 (length found)))
            (should (eq (car found) our-buf))))
      (kill-buffer our-buf)
      (kill-buffer other-buf))))

(ert-deftest beads-agent-claudemacs-test-find-buffers-no-match ()
  "Test that we return nil when no matching buffers exist."
  (let ((other-buf (generate-new-buffer "*claudemacs:claude:other*")))
    (unwind-protect
        (progn
          (with-current-buffer other-buf
            (setq-local claudemacs--cwd "/home/roman/workspace/other/"))
          (let ((found (beads-agent-claudemacs--find-buffers
                        "/home/roman/workspace/test/")))
            (should (null found))))
      (kill-buffer other-buf))))

(ert-deftest beads-agent-claudemacs-test-find-buffers-no-cwd-set ()
  "Test that buffers without `claudemacs--cwd' are not matched."
  (let ((test-buf (generate-new-buffer "*claudemacs:claude:test*")))
    (unwind-protect
        ;; Buffer exists but has no claudemacs--cwd set
        (let ((found (beads-agent-claudemacs--find-buffers
                      "/home/roman/workspace/test/")))
          (should (null found)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claudemacs-test-find-buffers-symlink-resolution ()
  "Test that symlinks are resolved when matching directories."
  ;; This test verifies that file-truename is used for comparison
  (let ((test-buf (generate-new-buffer "*claudemacs:claude:test*")))
    (unwind-protect
        (progn
          ;; Set cwd with expanded path
          (with-current-buffer test-buf
            (setq-local claudemacs--cwd (expand-file-name "~/workspace/test/")))
          ;; Search with ~ unexpanded - should still find due to file-truename
          (let ((found (beads-agent-claudemacs--find-buffers "~/workspace/test/")))
            (should (= 1 (length found)))
            (should (eq (car found) test-buf))))
      (kill-buffer test-buf))))

;;; Buffer Process Tests

(ert-deftest beads-agent-claudemacs-test-buffer-has-process-live ()
  "Test that buffer with process is detected."
  (let ((test-buf (generate-new-buffer "*test-process-buf*")))
    (unwind-protect
        (progn
          ;; Start a simple process
          (let ((proc (start-process "test" test-buf "sleep" "10")))
            (should (beads-agent-claudemacs--buffer-has-process-p test-buf))
            ;; Clean up
            (delete-process proc)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claudemacs-test-buffer-has-process-dead ()
  "Test that buffer without process returns nil."
  (let ((test-buf (generate-new-buffer "*test-no-process*")))
    (unwind-protect
        (should (not (beads-agent-claudemacs--buffer-has-process-p test-buf)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claudemacs-test-buffer-has-process-killed-buffer ()
  "Test that killed buffer returns nil."
  (let ((test-buf (generate-new-buffer "*test-killed*")))
    (kill-buffer test-buf)
    (should (not (beads-agent-claudemacs--buffer-has-process-p test-buf)))))

;;; Backend Registration Test

(ert-deftest beads-agent-claudemacs-test-backend-registered ()
  "Test that claudemacs backend is registered."
  ;; Backends are stored as a list of EIEIO objects
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "claudemacs"))
                  beads-agent--backends)))
    (should backend)
    (should (beads-agent-backend-claudemacs-p backend))))

(ert-deftest beads-agent-claudemacs-test-backend-priority ()
  "Test that claudemacs backend has correct priority."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "claudemacs"))
                  beads-agent--backends)))
    (should backend)
    ;; Priority 35 - between claude-code-ide (20) and claude-code (40)
    (should (= 35 (oref backend priority)))))

;;; Backend Availability Test

(ert-deftest beads-agent-claudemacs-test-backend-available-checks-package ()
  "Test that backend availability checks for claudemacs package."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "claudemacs"))
                  beads-agent--backends)))
    (should backend)
    ;; The backend should report unavailable if claudemacs isn't loaded
    ;; and can't be required (which is the case in our test environment)
    (unless (featurep 'claudemacs)
      ;; Without the claudemacs package, availability should be nil
      (should (not (beads-agent-backend-available-p backend))))))

;;; Customization Tests

(ert-deftest beads-agent-claudemacs-test-tool-customization ()
  "Test that tool customization variable exists and has default."
  (should (boundp 'beads-agent-claudemacs-tool))
  (should (eq beads-agent-claudemacs-tool 'claude)))

(provide 'beads-agent-claudemacs-test)
;;; beads-agent-claudemacs-test.el ends here
