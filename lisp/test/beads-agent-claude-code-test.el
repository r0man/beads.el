;;; beads-agent-claude-code-test.el --- Tests for claude-code backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Tests for beads-agent-claude-code.el - the claude-code.el backend.
;; All tests mock the claude-code package since it may not be installed.

;;; Code:

(require 'ert)
(require 'beads-agent-backend)
(require 'beads-agent-claude-code)

;;; Backend Registration Tests

(ert-deftest beads-agent-claude-code-test-backend-registered ()
  "Test that claude-code backend is registered."
  (let ((backends (beads-agent--get-all-backends)))
    (should (cl-some (lambda (b) (equal (oref b name) "claude-code")) backends))))

(ert-deftest beads-agent-claude-code-test-backend-priority ()
  "Test claude-code backend has correct priority."
  (let ((backend (beads-agent-backend-claude-code)))
    (should (= (oref backend priority) 40))))

(ert-deftest beads-agent-claude-code-test-backend-name ()
  "Test claude-code backend has correct name."
  (let ((backend (beads-agent-backend-claude-code)))
    (should (equal (oref backend name) "claude-code"))))

(ert-deftest beads-agent-claude-code-test-backend-description ()
  "Test claude-code backend has a description."
  (let ((backend (beads-agent-backend-claude-code)))
    (should (stringp (oref backend description)))
    (should (> (length (oref backend description)) 0))))

;;; Availability Tests

(ert-deftest beads-agent-claude-code-test-available-all-deps ()
  "Test availability when all dependencies are present."
  (let ((backend (beads-agent-backend-claude-code)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'claude-code)))
              ((symbol-function 'fboundp)
               (lambda (f) (memq f '(claude-code
                                     claude-code-kill
                                     claude-code-send-command
                                     claude-code-switch-to-buffer))))
              ((symbol-function 'executable-find)
               (lambda (name) (when (equal name "claude") "/usr/bin/claude"))))
      (should (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-claude-code-test-not-available-no-package ()
  "Test unavailability when claude-code package is missing."
  (let ((backend (beads-agent-backend-claude-code)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) nil))
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-not (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-claude-code-test-not-available-no-claude ()
  "Test unavailability when claude executable is missing."
  (let ((backend (beads-agent-backend-claude-code)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'fboundp) (lambda (_) t))
              ((symbol-function 'executable-find) (lambda (_) nil)))
      (should-not (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-claude-code-test-not-available-missing-functions ()
  "Test unavailability when required functions are missing."
  (let ((backend (beads-agent-backend-claude-code)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'fboundp)
               (lambda (f) (eq f 'claude-code)))
              ((symbol-function 'executable-find)
               (lambda (_) "/usr/bin/claude")))
      (should-not (beads-agent-backend-available-p backend)))))

;;; Start Session Tests

(ert-deftest beads-agent-claude-code-test-start-error-no-package ()
  "Test that start errors when claude-code is missing."
  (let ((backend (beads-agent-backend-claude-code)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) nil))
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

(ert-deftest beads-agent-claude-code-test-start-error-no-claude ()
  "Test that start errors when claude executable is missing."
  (let ((backend (beads-agent-backend-claude-code)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'executable-find) (lambda (_) nil)))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

;;; Buffer Finding Tests

(ert-deftest beads-agent-claude-code-test-find-buffers-main ()
  "Test finding main Claude buffer."
  (let ((dir (file-name-as-directory
              (abbreviate-file-name (file-truename "/tmp/test"))))
        (test-buf nil))
    (unwind-protect
        (progn
          (setq test-buf (generate-new-buffer (format "*claude:%s*" dir)))
          (let ((found (beads-agent-claude-code--find-buffers "/tmp/test")))
            (should (= (length found) 1))
            (should (equal (car found) test-buf))))
      (when (and test-buf (buffer-live-p test-buf))
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claude-code-test-find-buffers-instance ()
  "Test finding Claude instance buffer."
  (let ((dir (file-name-as-directory
              (abbreviate-file-name (file-truename "/tmp/test"))))
        (test-buf nil))
    (unwind-protect
        (progn
          (setq test-buf (generate-new-buffer (format "*claude:%s:instance*" dir)))
          (let ((found (beads-agent-claude-code--find-buffers "/tmp/test")))
            (should (= (length found) 1))
            (should (equal (car found) test-buf))))
      (when (and test-buf (buffer-live-p test-buf))
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claude-code-test-find-buffers-no-match ()
  "Test finding buffers when none match."
  (let ((dir (file-name-as-directory
              (abbreviate-file-name (file-truename "/tmp/other"))))
        (test-buf nil))
    (unwind-protect
        (progn
          (setq test-buf (generate-new-buffer (format "*claude:%s*" dir)))
          (let ((found (beads-agent-claude-code--find-buffers "/tmp/test")))
            (should (null found))))
      (when (and test-buf (buffer-live-p test-buf))
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claude-code-test-find-buffers-similar-prefix ()
  "Test that similar directory prefixes don't match."
  (let ((dir (file-name-as-directory
              (abbreviate-file-name (file-truename "/tmp/test-extended"))))
        (test-buf nil))
    (unwind-protect
        (progn
          (setq test-buf (generate-new-buffer (format "*claude:%s*" dir)))
          (let ((found (beads-agent-claude-code--find-buffers "/tmp/test")))
            (should (null found))))
      (when (and test-buf (buffer-live-p test-buf))
        (kill-buffer test-buf)))))

;;; Stop Session Tests

(ert-deftest beads-agent-claude-code-test-stop-kills-process-and-buffer ()
  "Test that stop kills process and buffer."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-stop*"))
         (backend (beads-agent-backend-claude-code))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (unwind-protect
        (progn
          (let ((proc (start-process "test" test-buf "sleep" "60")))
            (beads-agent-backend-stop backend session)
            (should-not (buffer-live-p test-buf))))
      (when (buffer-live-p test-buf)
        (let ((proc (get-buffer-process test-buf)))
          (when proc (delete-process proc)))
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claude-code-test-stop-no-process ()
  "Test that stop works when buffer has no process."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-no-proc*"))
         (backend (beads-agent-backend-claude-code))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (unwind-protect
        (progn
          (beads-agent-backend-stop backend session)
          (should-not (buffer-live-p test-buf)))
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claude-code-test-stop-nil-buffer ()
  "Test that stop handles nil buffer gracefully."
  (let* ((backend (beads-agent-backend-claude-code))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer nil)))
    (should-not (condition-case nil
                    (progn (beads-agent-backend-stop backend session) nil)
                  (error t)))))

(ert-deftest beads-agent-claude-code-test-stop-dead-buffer ()
  "Test that stop handles dead buffer gracefully."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-dead*"))
         (backend (beads-agent-backend-claude-code))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (kill-buffer test-buf)
    (should-not (condition-case nil
                    (progn (beads-agent-backend-stop backend session) nil)
                  (error t)))))

;;; Session Active Tests

(ert-deftest beads-agent-claude-code-test-session-active-with-process ()
  "Test session-active-p when buffer has process."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-active*"))
         (backend (beads-agent-backend-claude-code))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (unwind-protect
        (let ((proc (start-process "test" test-buf "sleep" "60")))
          (should (beads-agent-backend-session-active-p backend session))
          (delete-process proc))
      (when (buffer-live-p test-buf)
        (let ((proc (get-buffer-process test-buf)))
          (when proc (delete-process proc)))
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-claude-code-test-session-not-active-no-process ()
  "Test session-active-p when buffer has no process."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-inactive*"))
         (backend (beads-agent-backend-claude-code))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (unwind-protect
        (should-not (beads-agent-backend-session-active-p backend session))
      (kill-buffer test-buf))))

(ert-deftest beads-agent-claude-code-test-session-not-active-nil-buffer ()
  "Test session-active-p when buffer is nil."
  (let* ((backend (beads-agent-backend-claude-code))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer nil)))
    (should-not (beads-agent-backend-session-active-p backend session))))

;;; Switch to Buffer Tests

(ert-deftest beads-agent-claude-code-test-switch-to-buffer-success ()
  "Test switching to an existing live buffer."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-switch*"))
         (backend (beads-agent-backend-claude-code))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
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

(ert-deftest beads-agent-claude-code-test-switch-to-buffer-killed ()
  "Test switching to a killed buffer errors appropriately."
  (let* ((test-buf (generate-new-buffer "*test-claude-code-switch-killed*"))
         (backend (beads-agent-backend-claude-code))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "claude-code"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (kill-buffer test-buf)
    (should-error (beads-agent-backend-switch-to-buffer backend session)
                  :type 'user-error)))

(ert-deftest beads-agent-claude-code-test-switch-to-buffer-nil ()
  "Test switching when no buffer is set errors appropriately."
  (let ((backend (beads-agent-backend-claude-code))
        (session (beads-agent-session
                  :id "test#1"
                  :issue-id "test"
                  :backend-name "claude-code"
                  :project-dir "/tmp/test"
                  :started-at "2025-01-01T00:00:00Z"
                  :buffer nil)))
    (should-error (beads-agent-backend-switch-to-buffer backend session)
                  :type 'user-error)))

;;; Send Prompt Tests

(ert-deftest beads-agent-claude-code-test-send-prompt ()
  "Test sending a prompt to the session."
  (let ((backend (beads-agent-backend-claude-code))
        (session (beads-agent-session
                  :id "test#1"
                  :issue-id "test"
                  :backend-name "claude-code"
                  :project-dir "/tmp/test"
                  :started-at "2025-01-01T00:00:00Z"))
        (sent-prompt nil)
        (sent-in-dir nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'claude-code-send-command)
               (lambda (prompt)
                 (setq sent-prompt prompt)
                 (setq sent-in-dir default-directory))))
      (beads-agent-backend-send-prompt backend session "Test prompt")
      (should (equal sent-prompt "Test prompt"))
      (should (equal sent-in-dir "/tmp/test")))))

(provide 'beads-agent-claude-code-test)
;;; beads-agent-claude-code-test.el ends here
