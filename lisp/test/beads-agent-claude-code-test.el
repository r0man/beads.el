;;; beads-agent-claude-code-test.el --- Tests for claude-code backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for beads-agent-claude-code.el - the claude-code.el backend.
;; These tests verify the buffer finding logic and other backend methods
;; without requiring the actual claude-code package.

;;; Code:

(require 'ert)
(require 'beads-agent-backend)
(require 'beads-agent-claude-code)

;;; Buffer Finding Tests
;;
;; These tests verify that `beads-agent-claude-code--find-buffers' correctly
;; identifies Claude buffers based on the naming convention used by claude-code.el.
;;
;; claude-code buffer naming convention:
;; - Main buffer: *claude:DIRECTORY/*
;; - Named instance: *claude:DIRECTORY/:INSTANCE*
;; Where DIRECTORY is the abbreviated truename with trailing slash.

(ert-deftest beads-agent-claude-code-test-find-buffers-main-buffer ()
  "Test finding main Claude buffer (no instance name)."
  ;; Use expanded path to work on any machine, not just developer's
  (let* ((test-dir (file-name-as-directory
                    (expand-file-name "workspace/test/" (getenv "HOME"))))
         (abbreviated (abbreviate-file-name test-dir))
         (buf-name (format "*claude:%s*" abbreviated))
         (test-buf (generate-new-buffer buf-name)))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers test-dir)))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) buf-name)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-test-find-buffers-without-trailing-slash ()
  "Test that input without trailing slash still finds buffers."
  (let* ((test-dir (file-name-as-directory
                    (expand-file-name "workspace/test/" (getenv "HOME"))))
         (test-dir-no-slash (directory-file-name test-dir))
         (abbreviated (abbreviate-file-name test-dir))
         (buf-name (format "*claude:%s*" abbreviated))
         (test-buf (generate-new-buffer buf-name)))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers test-dir-no-slash)))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) buf-name)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-test-find-buffers-instance-buffer ()
  "Test finding Claude buffer with instance name."
  (let* ((test-dir (file-name-as-directory
                    (expand-file-name "workspace/test/" (getenv "HOME"))))
         (abbreviated (abbreviate-file-name test-dir))
         (buf-name (format "*claude:%s:my-instance*" abbreviated))
         (test-buf (generate-new-buffer buf-name)))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers test-dir)))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) buf-name)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-test-find-buffers-multiple ()
  "Test finding multiple Claude buffers for same directory."
  (let* ((test-dir (file-name-as-directory
                    (expand-file-name "workspace/test/" (getenv "HOME"))))
         (abbreviated (abbreviate-file-name test-dir))
         (main-buf-name (format "*claude:%s*" abbreviated))
         (tests-buf-name (format "*claude:%s:tests*" abbreviated))
         (dev-buf-name (format "*claude:%s:dev*" abbreviated))
         (buf1 (generate-new-buffer main-buf-name))
         (buf2 (generate-new-buffer tests-buf-name))
         (buf3 (generate-new-buffer dev-buf-name)))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers test-dir)))
          (should (= 3 (length found)))
          (should (member main-buf-name (mapcar #'buffer-name found)))
          (should (member tests-buf-name (mapcar #'buffer-name found)))
          (should (member dev-buf-name (mapcar #'buffer-name found))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

(ert-deftest beads-agent-claude-code-test-find-buffers-different-projects ()
  "Test that we don't find buffers from other projects."
  (let* ((test-dir (file-name-as-directory
                    (expand-file-name "workspace/test/" (getenv "HOME"))))
         (other-dir (file-name-as-directory
                     (expand-file-name "workspace/other/" (getenv "HOME"))))
         (test-abbreviated (abbreviate-file-name test-dir))
         (other-abbreviated (abbreviate-file-name other-dir))
         (our-buf-name (format "*claude:%s*" test-abbreviated))
         (other-buf-name (format "*claude:%s*" other-abbreviated))
         (our-buf (generate-new-buffer our-buf-name))
         (other-buf (generate-new-buffer other-buf-name)))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers test-dir)))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) our-buf-name)))
      (kill-buffer our-buf)
      (kill-buffer other-buf))))

(ert-deftest beads-agent-claude-code-test-find-buffers-no-match ()
  "Test that we return nil when no matching buffers exist."
  (let* ((test-dir (file-name-as-directory
                    (expand-file-name "workspace/test/" (getenv "HOME"))))
         (other-dir (file-name-as-directory
                     (expand-file-name "workspace/other/" (getenv "HOME"))))
         (other-abbreviated (abbreviate-file-name other-dir))
         (other-buf-name (format "*claude:%s*" other-abbreviated))
         (other-buf (generate-new-buffer other-buf-name)))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers test-dir)))
          (should (null found)))
      (kill-buffer other-buf))))

(ert-deftest beads-agent-claude-code-test-find-buffers-similar-prefix ()
  "Test that similar directory prefixes don't cause false matches."
  ;; Buffer for /workspace/test-extended should NOT match /workspace/test
  (let* ((test-dir (file-name-as-directory
                    (expand-file-name "workspace/test/" (getenv "HOME"))))
         (extended-dir (file-name-as-directory
                        (expand-file-name "workspace/test-extended/" (getenv "HOME"))))
         (extended-abbreviated (abbreviate-file-name extended-dir))
         (extended-buf-name (format "*claude:%s*" extended-abbreviated))
         (extended-buf (generate-new-buffer extended-buf-name)))
    (unwind-protect
        (let ((found (beads-agent-claude-code--find-buffers test-dir)))
          (should (null found)))
      (kill-buffer extended-buf))))

;;; Backend Registration Test

(ert-deftest beads-agent-claude-code-test-backend-registered ()
  "Test that claude-code backend is registered."
  ;; Backends are stored as a list of EIEIO objects
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "claude-code"))
                  beads-agent--backends)))
    (should backend)
    (should (beads-agent-backend-claude-code-p backend))))

;;; Backend Stop Tests
;;
;; These tests verify that `beads-agent-backend-stop' properly kills the
;; underlying process before killing the buffer, preventing zombie processes.

(ert-deftest beads-agent-claude-code-test-stop-kills-process-before-buffer ()
  "Test that stop kills the process before killing the buffer."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-stop*"))
         (backend (seq-find
                   (lambda (b) (equal (oref b name) "claude-code"))
                   beads-agent--backends))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
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

(ert-deftest beads-agent-claude-code-test-stop-no-process ()
  "Test that stop works when buffer has no process."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-no-proc*"))
         (backend (seq-find
                   (lambda (b) (equal (oref b name) "claude-code"))
                   beads-agent--backends))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
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

(ert-deftest beads-agent-claude-code-test-stop-dead-buffer ()
  "Test that stop handles already-killed buffer gracefully."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-dead*"))
         (backend (seq-find
                   (lambda (b) (equal (oref b name) "claude-code"))
                   beads-agent--backends))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
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

(ert-deftest beads-agent-claude-code-test-stop-nil-buffer ()
  "Test that stop handles nil buffer gracefully."
  (let* ((backend (seq-find
                   (lambda (b) (equal (oref b name) "claude-code"))
                   beads-agent--backends))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer nil
                   :backend-session nil)))
    ;; Stop should not error on nil buffer
    (should-not (condition-case nil
                    (progn (beads-agent-backend-stop backend session) nil)
                  (error t)))))

;;; Backend Availability Test

(ert-deftest beads-agent-claude-code-test-backend-available-checks-package ()
  "Test that backend availability checks for claude-code package."
  (let ((backend (seq-find
                  (lambda (b) (equal (oref b name) "claude-code"))
                  beads-agent--backends)))
    (should backend)
    ;; When claude-code package and claude executable are both available,
    ;; the backend should report available
    (if (and (or (featurep 'claude-code)
                 (require 'claude-code nil t))
             (executable-find "claude"))
        (should (beads-agent-backend-available-p backend))
      ;; Otherwise it should be unavailable
      (should (not (beads-agent-backend-available-p backend))))))

(provide 'beads-agent-claude-code-test)
;;; beads-agent-claude-code-test.el ends here
