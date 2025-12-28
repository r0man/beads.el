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
;; - Buffer: *claude-code[DIRECTORY_NAME]*
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
  (let ((test-buf (generate-new-buffer "*claude-code[test]*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code-ide--find-buffers
                      "/home/roman/workspace/test/")))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) "*claude-code[test]*")))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-ide-test-find-buffers-without-trailing-slash ()
  "Test that input without trailing slash still finds buffers."
  (let ((test-buf (generate-new-buffer "*claude-code[test]*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code-ide--find-buffers
                      "/home/roman/workspace/test")))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) "*claude-code[test]*")))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-ide-test-find-buffers-different-projects ()
  "Test that we don't find buffers from other projects."
  (let ((our-buf (generate-new-buffer "*claude-code[test]*"))
        (other-buf (generate-new-buffer "*claude-code[other]*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code-ide--find-buffers
                      "/home/roman/workspace/test/")))
          (should (= 1 (length found)))
          (should (equal (buffer-name (car found)) "*claude-code[test]*")))
      (kill-buffer our-buf)
      (kill-buffer other-buf))))

(ert-deftest beads-agent-claude-code-ide-test-find-buffers-no-match ()
  "Test that we return nil when no matching buffers exist."
  (let ((other-buf (generate-new-buffer "*claude-code[other]*")))
    (unwind-protect
        (let ((found (beads-agent-claude-code-ide--find-buffers
                      "/home/roman/workspace/test/")))
          (should (null found)))
      (kill-buffer other-buf))))

(ert-deftest beads-agent-claude-code-ide-test-find-buffers-similar-name ()
  "Test that similar directory names don't cause false matches."
  ;; Buffer for /workspace/test-extended should NOT match /workspace/test
  (let ((extended-buf (generate-new-buffer "*claude-code[test-extended]*")))
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

;;; Mocked Availability Tests

(ert-deftest beads-agent-claude-code-ide-test-available-all-deps ()
  "Test availability when all dependencies are present."
  (let ((backend (beads-agent-backend-claude-code-ide)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (memq f '(web-server claude-code-ide))))
              ((symbol-function 'fboundp)
               (lambda (f) (memq f '(claude-code-ide
                                     claude-code-ide-stop
                                     claude-code-ide-send-prompt
                                     claude-code-ide-switch-to-buffer))))
              ((symbol-function 'executable-find)
               (lambda (name) (when (equal name "claude") "/usr/bin/claude"))))
      (should (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-claude-code-ide-test-not-available-no-web-server ()
  "Test unavailability when web-server is missing."
  (let ((backend (beads-agent-backend-claude-code-ide)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'claude-code-ide)))
              ((symbol-function 'require)
               (lambda (f &rest _) (not (eq f 'web-server))))
              ((symbol-function 'fboundp) (lambda (_) t))
              ((symbol-function 'executable-find) (lambda (_) "/usr/bin/claude")))
      (should-not (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-claude-code-ide-test-not-available-no-claude ()
  "Test unavailability when claude executable is missing."
  (let ((backend (beads-agent-backend-claude-code-ide)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'fboundp) (lambda (_) t))
              ((symbol-function 'executable-find) (lambda (_) nil)))
      (should-not (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-claude-code-ide-test-not-available-no-package ()
  "Test unavailability when claude-code-ide package is missing."
  (let ((backend (beads-agent-backend-claude-code-ide)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'web-server)))
              ((symbol-function 'require)
               (lambda (f &rest _) (not (eq f 'claude-code-ide))))
              ((symbol-function 'fboundp) (lambda (_) nil))
              ((symbol-function 'executable-find) (lambda (_) "/usr/bin/claude")))
      (should-not (beads-agent-backend-available-p backend)))))

;;; Start Session Tests (Mocked)

(ert-deftest beads-agent-claude-code-ide-test-start-error-no-web-server ()
  "Test that start errors when web-server is missing."
  (let ((backend (beads-agent-backend-claude-code-ide)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) nil))
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

(ert-deftest beads-agent-claude-code-ide-test-start-error-no-package ()
  "Test that start errors when claude-code-ide is missing."
  (let ((backend (beads-agent-backend-claude-code-ide)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'web-server)))
              ((symbol-function 'require)
               (lambda (f &rest _) (eq f 'web-server))))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

(ert-deftest beads-agent-claude-code-ide-test-start-error-no-claude ()
  "Test that start errors when claude executable is missing."
  (let ((backend (beads-agent-backend-claude-code-ide)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'executable-find) (lambda (_) nil)))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

;;; Session Active Tests

(ert-deftest beads-agent-claude-code-ide-test-session-active-with-backend ()
  "Test session-active-p when MCP session exists."
  (let ((backend (beads-agent-backend-claude-code-ide))
        (session (beads-agent-session
                  :id "test#1"
                  :issue-id "test"
                  :backend-name "claude-code-ide"
                  :project-dir "/tmp/test"
                  :started-at "2025-01-01T00:00:00Z"
                  :backend-session 'mock-mcp-session)))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'claude-code-ide-mcp--get-session-for-project)
               (lambda (_dir) 'mock-mcp-session)))
      (should (beads-agent-backend-session-active-p backend session)))))

(ert-deftest beads-agent-claude-code-ide-test-session-not-active-no-backend ()
  "Test session-active-p when backend-session is nil."
  (let ((backend (beads-agent-backend-claude-code-ide))
        (session (beads-agent-session
                  :id "test#1"
                  :issue-id "test"
                  :backend-name "claude-code-ide"
                  :project-dir "/tmp/test"
                  :started-at "2025-01-01T00:00:00Z"
                  :backend-session nil)))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t)))
      (should-not (beads-agent-backend-session-active-p backend session)))))

(ert-deftest beads-agent-claude-code-ide-test-session-not-active-mcp-gone ()
  "Test session-active-p when MCP session is no longer available."
  (let ((backend (beads-agent-backend-claude-code-ide))
        (session (beads-agent-session
                  :id "test#1"
                  :issue-id "test"
                  :backend-name "claude-code-ide"
                  :project-dir "/tmp/test"
                  :started-at "2025-01-01T00:00:00Z"
                  :backend-session 'mock-mcp-session)))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'claude-code-ide-mcp--get-session-for-project)
               (lambda (_dir) nil)))
      (should-not (beads-agent-backend-session-active-p backend session)))))

;;; Switch to Buffer Tests

(ert-deftest beads-agent-claude-code-ide-test-switch-to-buffer-success ()
  "Test switching to an existing live buffer."
  (let* ((test-buf (generate-new-buffer "*test-claude-switch*"))
         (backend (beads-agent-backend-claude-code-ide))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code-ide"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf))
         (switched-to nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--pop-to-buffer-other-window)
                   (lambda (buf) (setq switched-to buf))))
          (beads-agent-backend-switch-to-buffer backend session)
          (should (equal switched-to test-buf)))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-ide-test-switch-to-buffer-killed ()
  "Test switching to a killed buffer errors appropriately."
  (let* ((test-buf (generate-new-buffer "*test-claude-switch-killed*"))
         (backend (beads-agent-backend-claude-code-ide))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code-ide"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (kill-buffer test-buf)
    (should-error (beads-agent-backend-switch-to-buffer backend session)
                  :type 'user-error)))

(ert-deftest beads-agent-claude-code-ide-test-switch-to-buffer-nil ()
  "Test switching when no buffer is set errors appropriately."
  (let ((backend (beads-agent-backend-claude-code-ide))
        (session (beads-agent-session
                  :id "test#1"
                  :issue-id "test"
                  :backend-name "claude-code-ide"
                  :project-dir "/tmp/test"
                  :started-at "2025-01-01T00:00:00Z"
                  :buffer nil)))
    (should-error (beads-agent-backend-switch-to-buffer backend session)
                  :type 'user-error)))

;;; Send Prompt Tests

(ert-deftest beads-agent-claude-code-ide-test-send-prompt ()
  "Test sending a prompt to the session."
  (let ((backend (beads-agent-backend-claude-code-ide))
        (session (beads-agent-session
                  :id "test#1"
                  :issue-id "test"
                  :backend-name "claude-code-ide"
                  :project-dir "/tmp/test"
                  :started-at "2025-01-01T00:00:00Z"))
        (sent-prompt nil)
        (sent-in-dir nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'claude-code-ide-send-prompt)
               (lambda (prompt)
                 (setq sent-prompt prompt)
                 (setq sent-in-dir default-directory))))
      (beads-agent-backend-send-prompt backend session "Test prompt")
      (should (equal sent-prompt "Test prompt"))
      (should (equal sent-in-dir "/tmp/test")))))

;;; Backend Class Tests

(ert-deftest beads-agent-claude-code-ide-test-backend-name ()
  "Test backend has correct name."
  (let ((backend (beads-agent-backend-claude-code-ide)))
    (should (equal (oref backend name) "claude-code-ide"))))

(ert-deftest beads-agent-claude-code-ide-test-backend-description ()
  "Test backend has description."
  (let ((backend (beads-agent-backend-claude-code-ide)))
    (should (stringp (oref backend description)))
    (should (> (length (oref backend description)) 0))))

(provide 'beads-agent-claude-code-ide-test)
;;; beads-agent-claude-code-ide-test.el ends here
