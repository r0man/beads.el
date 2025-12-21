;;; beads-agent-claude-code-ide-test.el --- Tests for claude-code-ide backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-agent-claude-code-ide.el - the claude-code-ide backend.
;; These tests verify the buffer finding logic, stop behavior, and other
;; backend methods without requiring the actual claude-code-ide package.
;;
;; claude-code-ide buffer naming convention:
;; - Buffer: *Claude Code: DIRECTORY_NAME*
;; Where DIRECTORY_NAME is the basename of the project directory.

;;; Code:

(require 'ert)
(require 'beads-agent-backend)
(require 'beads-agent-claude-code-ide)

;;; Buffer Finding Tests
;;
;; These tests verify that `beads-agent-claude-code-ide--find-buffers' correctly
;; identifies Claude Code IDE buffers based on the naming convention.

(ert-deftest beads-agent-claude-code-ide-test-find-buffers-main-buffer ()
  "Test finding main Claude Code IDE buffer."
  (let ((test-buf (generate-new-buffer "*Claude Code: test*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code-ide--find-buffers
                      "/home/roman/workspace/test/")))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) "*Claude Code: test*")))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-ide-test-find-buffers-without-trailing-slash ()
  "Test that input without trailing slash still finds buffers."
  (let ((test-buf (generate-new-buffer "*Claude Code: test*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code-ide--find-buffers
                      "/home/roman/workspace/test")))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) "*Claude Code: test*")))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-ide-test-find-buffers-different-projects ()
  "Test that we don't find buffers from other projects."
  (let ((our-buf (generate-new-buffer "*Claude Code: test*"))
        (other-buf (generate-new-buffer "*Claude Code: other*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code-ide--find-buffers
                      "/home/roman/workspace/test/")))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) "*Claude Code: test*")))
      (kill-buffer our-buf)
      (kill-buffer other-buf))))

(ert-deftest beads-agent-claude-code-ide-test-find-buffers-no-match ()
  "Test that we return nil when no matching buffers exist."
  (let ((other-buf (generate-new-buffer "*Claude Code: other*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code-ide--find-buffers
                      "/home/roman/workspace/test/")))
          (should (null found)))
      (kill-buffer other-buf))))

(ert-deftest beads-agent-claude-code-ide-test-find-buffers-similar-name ()
  "Test that similar directory names don't cause false matches."
  ;; Buffer for /workspace/test-extended should NOT match /workspace/test
  (let ((extended-buf (generate-new-buffer "*Claude Code: test-extended*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code-ide--find-buffers
                      "/home/roman/workspace/test/")))
          (should (null found)))
      (kill-buffer extended-buf))))

;;; Backend Registration Test

(ert-deftest beads-agent-claude-code-ide-test-backend-registered ()
  "Test that claude-code-ide backend is registered."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "claude-code-ide"))
                  beads-agent--backends)))
    (should backend)
    (should (beads-agent-backend-claude-code-ide-p backend))))

(ert-deftest beads-agent-claude-code-ide-test-backend-priority ()
  "Test that claude-code-ide has highest priority (lowest number)."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "claude-code-ide"))
                  beads-agent--backends)))
    (should backend)
    ;; Priority 10 - highest priority (lowest number)
    (should (= 10 (oref backend priority)))))

;;; Backend Stop Tests
;;
;; These tests verify that `beads-agent-backend-stop' properly kills the
;; underlying process before killing the buffer, preventing zombie processes.

(ert-deftest beads-agent-claude-code-ide-test-stop-kills-process-before-buffer ()
  "Test that stop kills the process before killing the buffer."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-ide-stop*"))
         (backend (seq-find
                   (lambda (b) (equal (oref b name) "claude-code-ide"))
                   beads-agent--backends))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code-ide"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf
                   :backend-session nil))
         (delete-process-called nil)
         (process-was-alive-when-deleted nil)
         (buffer-was-alive-when-deleted nil))
    (unwind-protect
        (progn
          ;; Start a process in the buffer
          (let ((proc (start-process "test" test-buf "sleep" "60")))
            ;; Verify process is running
            (should (process-live-p proc))
            ;; Track when delete-process is called and what state things are in
            (cl-letf (((symbol-function 'delete-process)
                       (let ((orig (symbol-function 'delete-process)))
                         (lambda (p)
                           (setq delete-process-called t)
                           (setq process-was-alive-when-deleted (process-live-p p))
                           (setq buffer-was-alive-when-deleted (buffer-live-p test-buf))
                           (funcall orig p)))))
              ;; Call stop
              (beads-agent-backend-stop backend session))
            ;; Verify delete-process was called
            (should delete-process-called)
            ;; Verify process was alive when delete-process was called
            (should process-was-alive-when-deleted)
            ;; Verify buffer was alive when delete-process was called
            ;; (meaning process killed before buffer)
            (should buffer-was-alive-when-deleted)
            ;; Verify buffer is now dead
            (should-not (buffer-live-p test-buf))))
      ;; Cleanup if test fails
      (when (buffer-live-p test-buf)
        (let ((proc (get-buffer-process test-buf)))
          (when proc (delete-process proc)))
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claude-code-ide-test-stop-no-process ()
  "Test that stop works when buffer has no process."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-ide-no-proc*"))
         (backend (seq-find
                   (lambda (b) (equal (oref b name) "claude-code-ide"))
                   beads-agent--backends))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code-ide"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf
                   :backend-session nil)))
    (unwind-protect
        (progn
          ;; Verify no process
          (should-not (get-buffer-process test-buf))
          ;; Stop should not error
          (beads-agent-backend-stop backend session)
          ;; Buffer should be killed
          (should-not (buffer-live-p test-buf)))
      ;; Cleanup
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claude-code-ide-test-stop-dead-buffer ()
  "Test that stop handles already-killed buffer gracefully."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-ide-dead*"))
         (backend (seq-find
                   (lambda (b) (equal (oref b name) "claude-code-ide"))
                   beads-agent--backends))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code-ide"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf
                   :backend-session nil)))
    ;; Kill buffer first
    (kill-buffer test-buf)
    ;; Stop should not error on dead buffer
    (should-not (condition-case nil
                    (progn (beads-agent-backend-stop backend session) nil)
                  (error t)))))

(ert-deftest beads-agent-claude-code-ide-test-stop-nil-buffer ()
  "Test that stop handles nil buffer gracefully."
  (let* ((backend (seq-find
                   (lambda (b) (equal (oref b name) "claude-code-ide"))
                   beads-agent--backends))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code-ide"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer nil
                   :backend-session nil)))
    ;; Stop should not error on nil buffer
    (should-not (condition-case nil
                    (progn (beads-agent-backend-stop backend session) nil)
                  (error t)))))

;;; Backend Availability Test

(ert-deftest beads-agent-claude-code-ide-test-backend-available-checks-package ()
  "Test that backend availability checks for claude-code-ide package."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "claude-code-ide"))
                  beads-agent--backends)))
    (should backend)
    ;; When web-server, claude-code-ide package, and claude executable are
    ;; all available, the backend should report available
    (if (and (or (featurep 'web-server)
                 (require 'web-server nil t))
             (or (featurep 'claude-code-ide)
                 (require 'claude-code-ide nil t))
             (executable-find "claude"))
        (should (beads-agent-backend-available-p backend))
      ;; Otherwise it should be unavailable
      (should (not (beads-agent-backend-available-p backend))))))

(provide 'beads-agent-claude-code-ide-test)
;;; beads-agent-claude-code-ide-test.el ends here
