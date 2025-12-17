;;; beads-agent-agent-shell-test.el --- Tests for agent-shell backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-agent-agent-shell.el - the agent-shell backend.
;; These tests verify the buffer finding logic and other backend methods
;; without requiring the actual agent-shell package.
;;
;; agent-shell buffers are identified by:
;; - Being in `agent-shell-mode'
;; - Having a matching `default-directory'

;;; Code:

(require 'ert)
(require 'beads-agent-backend)

;; Prevent the real agent-shell from being loaded.
;; The real agent-shell-mode has a bug that causes (void-function keymap)
;; in batch Emacs mode. We need to completely block it from loading.
;; See: https://github.com/xenodium/shell-maker/issues/XXX

;; First, remove any agent-shell autoloads that might trigger loading
(dolist (sym '(agent-shell-mode agent-shell agent-shell-start))
  (when (fboundp sym)
    (fmakunbound sym)))

;; Pretend agent-shell is already loaded so require won't load it
(provide 'agent-shell)

;; Define mock functions that the backend checks for
(defun agent-shell-start (&rest _args)
  "Mock agent-shell-start for testing."
  nil)

(defun agent-shell-insert (&rest _args)
  "Mock agent-shell-insert for testing."
  nil)

;; Define mock variable that the backend checks
(defvar agent-shell-agent-configs '((mock-config))
  "Mock agent configs for testing.")

;; Define a mock agent-shell-mode for testing.
;; This must happen AFTER we block the real package.
(define-derived-mode agent-shell-mode special-mode "AgentShell"
  "Mock agent-shell-mode for testing.")

;; Now load the backend - it will see our mock mode
(require 'beads-agent-agent-shell)

;;; Buffer Finding Tests
;;
;; These tests verify that `beads-agent-agent-shell--find-buffers' correctly
;; identifies agent-shell buffers based on major mode and default-directory.

(ert-deftest beads-agent-agent-shell-test-find-buffers-by-directory ()
  "Test finding agent-shell buffer by its default-directory."
  (let ((test-buf (generate-new-buffer "*agent-shell-test*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (agent-shell-mode)
            (setq default-directory "/home/roman/workspace/test/"))
          (let ((found (beads-agent-agent-shell--find-buffers
                        "/home/roman/workspace/test/")))
            (should (= 1 (length found)))
            (should (eq (car found) test-buf))))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-agent-shell-test-find-buffers-without-trailing-slash ()
  "Test that input without trailing slash still finds buffers."
  (let ((test-buf (generate-new-buffer "*agent-shell-test*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (agent-shell-mode)
            (setq default-directory "/home/roman/workspace/test/"))
          (let ((found (beads-agent-agent-shell--find-buffers
                        "/home/roman/workspace/test")))
            (should (= 1 (length found)))
            (should (eq (car found) test-buf))))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-agent-shell-test-find-buffers-multiple ()
  "Test finding multiple agent-shell buffers for same directory."
  (let ((buf1 (generate-new-buffer "*agent-shell-1*"))
        (buf2 (generate-new-buffer "*agent-shell-2*"))
        (buf3 (generate-new-buffer "*agent-shell-3*")))
    (unwind-protect
        (progn
          ;; Set all buffers to same working directory
          (dolist (buf (list buf1 buf2 buf3))
            (with-current-buffer buf
              (agent-shell-mode)
              (setq default-directory "/home/roman/workspace/test/")))
          (let ((found (beads-agent-agent-shell--find-buffers
                        "/home/roman/workspace/test/")))
            (should (= 3 (length found)))
            (should (memq buf1 found))
            (should (memq buf2 found))
            (should (memq buf3 found))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

(ert-deftest beads-agent-agent-shell-test-find-buffers-different-projects ()
  "Test that we don't find buffers from other projects."
  (let ((our-buf (generate-new-buffer "*agent-shell-ours*"))
        (other-buf (generate-new-buffer "*agent-shell-other*")))
    (unwind-protect
        (progn
          (with-current-buffer our-buf
            (agent-shell-mode)
            (setq default-directory "/home/roman/workspace/test/"))
          (with-current-buffer other-buf
            (agent-shell-mode)
            (setq default-directory "/home/roman/workspace/other/"))
          (let ((found (beads-agent-agent-shell--find-buffers
                        "/home/roman/workspace/test/")))
            (should (= 1 (length found)))
            (should (eq (car found) our-buf))))
      (kill-buffer our-buf)
      (kill-buffer other-buf))))

(ert-deftest beads-agent-agent-shell-test-find-buffers-no-match ()
  "Test that we return nil when no matching buffers exist."
  (let ((other-buf (generate-new-buffer "*agent-shell-other*")))
    (unwind-protect
        (progn
          (with-current-buffer other-buf
            (agent-shell-mode)
            (setq default-directory "/home/roman/workspace/other/"))
          (let ((found (beads-agent-agent-shell--find-buffers
                        "/home/roman/workspace/test/")))
            (should (null found))))
      (kill-buffer other-buf))))

(ert-deftest beads-agent-agent-shell-test-find-buffers-wrong-mode ()
  "Test that buffers not in agent-shell-mode are not matched."
  (let ((test-buf (generate-new-buffer "*not-agent-shell*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            ;; Buffer has right directory but wrong mode
            (setq default-directory "/home/roman/workspace/test/"))
          (let ((found (beads-agent-agent-shell--find-buffers
                        "/home/roman/workspace/test/")))
            (should (null found))))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-agent-shell-test-find-buffers-symlink-resolution ()
  "Test that symlinks are resolved when matching directories."
  ;; This test verifies that file-truename is used for comparison
  (let ((test-buf (generate-new-buffer "*agent-shell-symlink*")))
    (unwind-protect
        (progn
          ;; Set directory with expanded path
          (with-current-buffer test-buf
            (agent-shell-mode)
            (setq default-directory (expand-file-name "~/workspace/test/")))
          ;; Search with ~ unexpanded - should still find due to file-truename
          (let ((found (beads-agent-agent-shell--find-buffers
                        "~/workspace/test/")))
            (should (= 1 (length found)))
            (should (eq (car found) test-buf))))
      (kill-buffer test-buf))))

;;; Buffer Active Tests

(ert-deftest beads-agent-agent-shell-test-buffer-active-with-process ()
  "Test that buffer with process is detected as active."
  (let ((test-buf (generate-new-buffer "*test-process-buf*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (agent-shell-mode))
          ;; Start a simple process
          (let ((proc (start-process "test" test-buf "sleep" "10")))
            (should (beads-agent-agent-shell--buffer-active-p test-buf))
            ;; Clean up
            (delete-process proc)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-agent-shell-test-buffer-active-no-process ()
  "Test that buffer without process returns nil."
  (let ((test-buf (generate-new-buffer "*test-no-process*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (agent-shell-mode))
          (should (not (beads-agent-agent-shell--buffer-active-p test-buf))))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-agent-shell-test-buffer-active-killed-buffer ()
  "Test that killed buffer returns nil."
  (let ((test-buf (generate-new-buffer "*test-killed*")))
    (kill-buffer test-buf)
    (should (not (beads-agent-agent-shell--buffer-active-p test-buf)))))

(ert-deftest beads-agent-agent-shell-test-buffer-active-wrong-mode ()
  "Test that buffer with process but wrong mode returns nil."
  (let ((test-buf (generate-new-buffer "*test-wrong-mode*")))
    (unwind-protect
        (progn
          ;; Buffer has process but is not in agent-shell-mode
          (let ((proc (start-process "test" test-buf "sleep" "10")))
            (should (not (beads-agent-agent-shell--buffer-active-p test-buf)))
            (delete-process proc)))
      (kill-buffer test-buf))))

;;; Backend Registration Tests

(ert-deftest beads-agent-agent-shell-test-backend-registered ()
  "Test that agent-shell backend is registered."
  ;; Backends are stored as a list of EIEIO objects
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "agent-shell"))
                  beads-agent--backends)))
    (should backend)
    (should (beads-agent-backend-agent-shell-p backend))))

(ert-deftest beads-agent-agent-shell-test-backend-priority ()
  "Test that agent-shell backend has correct priority."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "agent-shell"))
                  beads-agent--backends)))
    (should backend)
    ;; Priority 15 - high priority (lower than claude-code-ide at 20)
    (should (= 15 (oref backend priority)))))

;;; Backend Availability Tests

(ert-deftest beads-agent-agent-shell-test-backend-available-checks-package ()
  "Test that backend availability check works correctly."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "agent-shell"))
                  beads-agent--backends)))
    (should backend)
    ;; Test availability based on whether agent-shell is installed
    (if (featurep 'agent-shell)
        ;; When agent-shell is available, backend should report available
        ;; (assuming agent-shell-agent-configs is defined and non-nil)
        (should (beads-agent-backend-available-p backend))
      ;; Without the agent-shell package, availability should be nil
      (should (not (beads-agent-backend-available-p backend))))))

;;; Customization Tests

(ert-deftest beads-agent-agent-shell-test-config-customization ()
  "Test that config customization variable exists and has default."
  (should (boundp 'beads-agent-agent-shell-config))
  (should (null beads-agent-agent-shell-config)))

;;; Session Detection Tests

(ert-deftest beads-agent-agent-shell-test-session-active-with-matching-buffer ()
  "Test session-active-p returns t when matching active buffer exists."
  (let ((test-buf (generate-new-buffer "*agent-shell-active*"))
        (backend (seq-find
                  (lambda (b) (equal (oref b name) "agent-shell"))
                  beads-agent--backends))
        (session (beads-agent-session
                  :id "test-session"
                  :issue-id "test-1"
                  :backend-name "agent-shell"
                  :project-dir "/home/roman/workspace/test/"
                  :started-at "2025-01-01T00:00:00+0000")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (agent-shell-mode)
            (setq default-directory "/home/roman/workspace/test/"))
          (let ((proc (start-process "test" test-buf "sleep" "10")))
            (should (beads-agent-backend-session-active-p backend session))
            (delete-process proc)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-agent-shell-test-session-not-active-without-buffer ()
  "Test session-active-p returns nil when no matching buffer exists."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "agent-shell"))
                  beads-agent--backends))
        (session (beads-agent-session
                  :id "test-session"
                  :issue-id "test-1"
                  :backend-name "agent-shell"
                  :project-dir "/nonexistent/path/"
                  :started-at "2025-01-01T00:00:00+0000")))
    (should (not (beads-agent-backend-session-active-p backend session)))))

(ert-deftest beads-agent-agent-shell-test-session-not-active-without-process ()
  "Test session-active-p returns nil when buffer exists but has no process."
  (let ((test-buf (generate-new-buffer "*agent-shell-no-proc*"))
        (backend (seq-find
                  (lambda (b) (equal (oref b name) "agent-shell"))
                  beads-agent--backends))
        (session (beads-agent-session
                  :id "test-session"
                  :issue-id "test-1"
                  :backend-name "agent-shell"
                  :project-dir "/home/roman/workspace/test/"
                  :started-at "2025-01-01T00:00:00+0000")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (agent-shell-mode)
            (setq default-directory "/home/roman/workspace/test/"))
          ;; Buffer exists but no process
          (should (not (beads-agent-backend-session-active-p backend session))))
      (kill-buffer test-buf))))

;;; Get Buffer Tests

(ert-deftest beads-agent-agent-shell-test-get-buffer-returns-active ()
  "Test get-buffer returns the active buffer."
  (let ((test-buf (generate-new-buffer "*agent-shell-get*"))
        (backend (seq-find
                  (lambda (b) (equal (oref b name) "agent-shell"))
                  beads-agent--backends))
        (session (beads-agent-session
                  :id "test-session"
                  :issue-id "test-1"
                  :backend-name "agent-shell"
                  :project-dir "/home/roman/workspace/test/"
                  :started-at "2025-01-01T00:00:00+0000")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (agent-shell-mode)
            (setq default-directory "/home/roman/workspace/test/"))
          (let ((proc (start-process "test" test-buf "sleep" "10")))
            (should (eq test-buf (beads-agent-backend-get-buffer
                                  backend session)))
            (delete-process proc)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-agent-shell-test-get-buffer-returns-nil-no-match ()
  "Test get-buffer returns nil when no matching buffer exists."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "agent-shell"))
                  beads-agent--backends))
        (session (beads-agent-session
                  :id "test-session"
                  :issue-id "test-1"
                  :backend-name "agent-shell"
                  :project-dir "/nonexistent/path/"
                  :started-at "2025-01-01T00:00:00+0000")))
    (should (null (beads-agent-backend-get-buffer backend session)))))

;;; Worktree Directory Tests

(ert-deftest beads-agent-agent-shell-test-uses-worktree-dir-when-set ()
  "Test that session uses worktree-dir when set."
  (let ((test-buf (generate-new-buffer "*agent-shell-worktree*"))
        (backend (seq-find
                  (lambda (b) (equal (oref b name) "agent-shell"))
                  beads-agent--backends))
        (session (beads-agent-session
                  :id "test-session"
                  :issue-id "test-1"
                  :backend-name "agent-shell"
                  :project-dir "/home/roman/workspace/main/"
                  :worktree-dir "/home/roman/workspace/worktree/"
                  :started-at "2025-01-01T00:00:00+0000")))
    (unwind-protect
        (progn
          ;; Buffer is in worktree-dir, not project-dir
          (with-current-buffer test-buf
            (agent-shell-mode)
            (setq default-directory "/home/roman/workspace/worktree/"))
          (let ((proc (start-process "test" test-buf "sleep" "10")))
            ;; Should find buffer in worktree-dir
            (should (beads-agent-backend-session-active-p backend session))
            (should (eq test-buf (beads-agent-backend-get-buffer
                                  backend session)))
            (delete-process proc)))
      (kill-buffer test-buf))))

(provide 'beads-agent-agent-shell-test)
;;; beads-agent-agent-shell-test.el ends here
