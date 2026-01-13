;;; beads-agent-eca-test.el --- Tests for ECA backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Tests for beads-agent-eca.el - the ECA (Editor Code Assistant) backend.
;; All tests mock eca-emacs since it may not be installed.

;;; Code:

(require 'ert)
(require 'beads-agent-backend)
(require 'beads-agent-eca)

;;; Mock Environment Macro

(defmacro with-mocked-eca-environment (&rest body)
  "Execute BODY with ECA-related functions mocked.
This provides a controlled environment for testing without
requiring the actual eca-emacs package."
  `(cl-letf (;; Mock feature checks
             ((symbol-function 'featurep)
              (lambda (f) (memq f '(eca eca-chat eca-process))))
             ((symbol-function 'require)
              (lambda (f &optional _ _) (memq f '(eca eca-chat eca-process))))
             ((symbol-function 'fboundp)
              (lambda (f)
                (memq f '(eca eca-stop eca-restart eca-session eca-create-session
                              eca-process-running-p eca-process-start eca-process-stop
                              eca-chat-send-prompt eca-chat-open eca-chat-exit
                              eca-install-server))))
             ((symbol-function 'executable-find)
              (lambda (name) (when (equal name "eca") "/usr/bin/eca"))))
     ,@body))

;;; Backend Registration Tests

(ert-deftest beads-agent-eca-test-backend-registered ()
  "Test that ECA backend is registered."
  (let ((backends (beads-agent--get-all-backends)))
    (should (cl-some (lambda (b) (equal (oref b name) "eca")) backends))))

(ert-deftest beads-agent-eca-test-backend-priority ()
  "Test ECA backend has correct priority."
  (let ((backend (beads-agent-backend-eca)))
    (should (= (oref backend priority) 20))))

(ert-deftest beads-agent-eca-test-backend-name ()
  "Test ECA backend has correct name."
  (let ((backend (beads-agent-backend-eca)))
    (should (equal (oref backend name) "eca"))))

(ert-deftest beads-agent-eca-test-backend-description ()
  "Test ECA backend has a description."
  (let ((backend (beads-agent-backend-eca)))
    (should (stringp (oref backend description)))
    (should (> (length (oref backend description)) 0))))

;;; Availability Tests

(ert-deftest beads-agent-eca-test-available-all-deps ()
  "Test availability when all dependencies are present."
  (let ((backend (beads-agent-backend-eca)))
    (with-mocked-eca-environment
     (should (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-eca-test-not-available-no-package ()
  "Test unavailability when eca package is missing."
  (let ((backend (beads-agent-backend-eca)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) nil))
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-not (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-eca-test-not-available-no-eca-command ()
  "Test unavailability when eca executable is missing and no auto-installer."
  (let ((backend (beads-agent-backend-eca)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (memq f '(eca eca-chat eca-process))))
              ((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'fboundp)
               ;; All functions EXCEPT eca-install-server
               (lambda (f)
                 (memq f '(eca eca-stop eca-chat-send-prompt
                               eca-process-running-p))))
              ((symbol-function 'executable-find)
               (lambda (_) nil)))
      (should-not (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-eca-test-available-with-installer ()
  "Test availability when eca-install-server is available."
  (let ((backend (beads-agent-backend-eca)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (memq f '(eca eca-chat eca-process))))
              ((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'fboundp)
               (lambda (f)
                 (memq f '(eca eca-stop eca-chat-send-prompt
                               eca-process-running-p eca-install-server))))
              ((symbol-function 'executable-find)
               (lambda (_) nil)))
      ;; Should be available because eca-install-server exists
      (should (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-eca-test-not-available-missing-chat ()
  "Test unavailability when eca-chat module is missing."
  (let ((backend (beads-agent-backend-eca)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'eca)))  ; Only eca, not eca-chat
              ((symbol-function 'require)
               (lambda (f &rest _)
                 (eq f 'eca)))  ; Can load eca but not eca-chat
              ((symbol-function 'fboundp)
               (lambda (f)
                 (memq f '(eca eca-stop eca-process-running-p))))
              ((symbol-function 'executable-find)
               (lambda (name) (when (equal name "eca") "/usr/bin/eca"))))
      (should-not (beads-agent-backend-available-p backend)))))

;;; Start Session Tests

(ert-deftest beads-agent-eca-test-start-error-no-package ()
  "Test that start errors when eca package is missing."
  (let ((backend (beads-agent-backend-eca)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) nil))
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

(ert-deftest beads-agent-eca-test-start-error-no-chat ()
  "Test that start errors when eca-chat module is missing."
  (let ((backend (beads-agent-backend-eca)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'eca)))
              ((symbol-function 'require)
               (lambda (f &rest _)
                 (if (eq f 'eca) t nil))))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

(ert-deftest beads-agent-eca-test-start-creates-session ()
  "Test that start creates session and returns buffer."
  (let* ((backend (beads-agent-backend-eca))
         (mock-session (list :id "mock-session"))
         (mock-buffer (generate-new-buffer "*test-eca-chat*"))
         (eca-called nil)
         (prompt-sent nil))
    (unwind-protect
        (cl-letf (((symbol-function 'featurep)
                   (lambda (f) (memq f '(eca eca-chat eca-process))))
                  ((symbol-function 'require)
                   (lambda (&rest _) t))
                  ((symbol-function 'eca)
                   (lambda () (setq eca-called t)))
                  ((symbol-function 'eca-session)
                   (lambda () mock-session))
                  ((symbol-function 'eca-chat-send-prompt)
                   (lambda (prompt) (setq prompt-sent prompt)))
                  ((symbol-function 'beads-agent-eca--find-chat-buffer)
                   (lambda (_) mock-buffer))
                  ((symbol-function 'beads-agent-eca--wait-for-session)
                   (lambda (&optional _) mock-session)))
          (let ((result (beads-agent-backend-start backend nil "Test prompt")))
            (should eca-called)
            (should (equal prompt-sent "Test prompt"))
            (should (consp result))
            (should (equal (car result) mock-session))
            (should (equal (cdr result) mock-buffer))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer)))))

;;; Stop Session Tests

(ert-deftest beads-agent-eca-test-stop-kills-process-and-buffer ()
  "Test that stop kills process and buffer."
  (let* ((test-buf (generate-new-buffer "*test-eca-stop*"))
         (backend (beads-agent-backend-eca))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "eca"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf
                   :backend-session nil)))
    (unwind-protect
        (progn
          (let ((proc (start-process "test" test-buf "sleep" "60")))
            (cl-letf (((symbol-function 'featurep) (lambda (_) t))
                      ((symbol-function 'fboundp) (lambda (_) nil)))
              (beads-agent-backend-stop backend session))
            (should-not (buffer-live-p test-buf))))
      (when (buffer-live-p test-buf)
        (let ((proc (get-buffer-process test-buf)))
          (when proc (delete-process proc)))
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-eca-test-stop-no-process ()
  "Test that stop works when buffer has no process."
  (let* ((test-buf (generate-new-buffer "*test-eca-no-proc*"))
         (backend (beads-agent-backend-eca))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "eca"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf
                   :backend-session nil)))
    (unwind-protect
        (progn
          (beads-agent-backend-stop backend session)
          (should-not (buffer-live-p test-buf)))
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-eca-test-stop-nil-buffer ()
  "Test that stop handles nil buffer gracefully."
  (let* ((backend (beads-agent-backend-eca))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "eca"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer nil
                   :backend-session nil)))
    (should-not (condition-case nil
                    (progn (beads-agent-backend-stop backend session) nil)
                  (error t)))))

(ert-deftest beads-agent-eca-test-stop-dead-buffer ()
  "Test that stop handles dead buffer gracefully."
  (let* ((test-buf (generate-new-buffer "*test-eca-dead*"))
         (backend (beads-agent-backend-eca))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "eca"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf
                   :backend-session nil)))
    (kill-buffer test-buf)
    (should-not (condition-case nil
                    (progn (beads-agent-backend-stop backend session) nil)
                  (error t)))))

;;; Session Active Tests

(ert-deftest beads-agent-eca-test-session-active-with-process ()
  "Test session-active-p when ECA process is running."
  (let* ((backend (beads-agent-backend-eca))
         (mock-eca-session (list :id "mock-session"))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "eca"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :backend-session mock-eca-session)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (memq f '(eca-process))))
              ((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'fboundp)
               (lambda (f) (eq f 'eca-process-running-p)))
              ((symbol-function 'eca-process-running-p)
               (lambda (sess) (eq sess mock-eca-session))))
      (should (beads-agent-backend-session-active-p backend session)))))

(ert-deftest beads-agent-eca-test-session-not-active-process-stopped ()
  "Test session-active-p when ECA process is not running."
  (let* ((backend (beads-agent-backend-eca))
         (mock-eca-session (list :id "mock-session"))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "eca"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :backend-session mock-eca-session)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (memq f '(eca-process))))
              ((symbol-function 'require)
               (lambda (&rest _) t))
              ((symbol-function 'fboundp)
               (lambda (f) (eq f 'eca-process-running-p)))
              ((symbol-function 'eca-process-running-p)
               (lambda (_) nil)))
      (should-not (beads-agent-backend-session-active-p backend session)))))

(ert-deftest beads-agent-eca-test-session-not-active-nil-backend-session ()
  "Test session-active-p when backend-session is nil."
  (let* ((backend (beads-agent-backend-eca))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "eca"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :backend-session nil)))
    (should-not (beads-agent-backend-session-active-p backend session))))

;;; Switch to Buffer Tests

(ert-deftest beads-agent-eca-test-switch-to-buffer-success ()
  "Test switching to an existing live buffer."
  (let* ((test-buf (generate-new-buffer "*test-eca-switch*"))
         (backend (beads-agent-backend-eca))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "eca"
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

(ert-deftest beads-agent-eca-test-switch-to-buffer-killed ()
  "Test switching to a killed buffer errors appropriately."
  (let* ((test-buf (generate-new-buffer "*test-eca-switch-killed*"))
         (backend (beads-agent-backend-eca))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "eca"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer test-buf)))
    (kill-buffer test-buf)
    (should-error (beads-agent-backend-switch-to-buffer backend session)
                  :type 'user-error)))

(ert-deftest beads-agent-eca-test-switch-to-buffer-fallback-eca-chat ()
  "Test switching falls back to eca-chat-open when no stored buffer."
  (let* ((backend (beads-agent-backend-eca))
         (mock-eca-session (list :id "mock-session"))
         (session (beads-agent-session
                   :id "test#1"
                   :issue-id "test"
                   :backend-name "eca"
                   :project-dir "/tmp/test"
                   :started-at "2025-01-01T00:00:00Z"
                   :buffer nil
                   :backend-session mock-eca-session))
         (eca-chat-open-called nil))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'eca-chat)))
              ((symbol-function 'fboundp)
               (lambda (f) (eq f 'eca-chat-open)))
              ((symbol-function 'eca-chat-open)
               (lambda (sess)
                 (when (eq sess mock-eca-session)
                   (setq eca-chat-open-called t)))))
      (beads-agent-backend-switch-to-buffer backend session)
      (should eca-chat-open-called))))

(ert-deftest beads-agent-eca-test-switch-to-buffer-nil-no-fallback ()
  "Test switching errors when no buffer and no backend session."
  (let ((backend (beads-agent-backend-eca))
        (session (beads-agent-session
                  :id "test#1"
                  :issue-id "test"
                  :backend-name "eca"
                  :project-dir "/tmp/test"
                  :started-at "2025-01-01T00:00:00Z"
                  :buffer nil
                  :backend-session nil)))
    (should-error (beads-agent-backend-switch-to-buffer backend session)
                  :type 'user-error)))

;;; Send Prompt Tests

(ert-deftest beads-agent-eca-test-send-prompt ()
  "Test sending a prompt to the session."
  (let ((backend (beads-agent-backend-eca))
        (session (beads-agent-session
                  :id "test#1"
                  :issue-id "test"
                  :backend-name "eca"
                  :project-dir "/tmp/test"
                  :started-at "2025-01-01T00:00:00Z"))
        (sent-prompt nil))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'eca-chat)))
              ((symbol-function 'fboundp)
               (lambda (f) (eq f 'eca-chat-send-prompt)))
              ((symbol-function 'eca-chat-send-prompt)
               (lambda (prompt) (setq sent-prompt prompt))))
      (beads-agent-backend-send-prompt backend session "Test prompt")
      (should (equal sent-prompt "Test prompt")))))

;;; Helper Function Tests

(ert-deftest beads-agent-eca-test-find-chat-buffer-found ()
  "Test finding ECA chat buffer by session.
ECA uses angle brackets for buffer names: <eca-chat:N:M>."
  (let* ((mock-session (list :id "test-session"))
         (test-buf (generate-new-buffer "<eca-chat:test-session>")))
    (unwind-protect
        (cl-letf (((symbol-function 'fboundp)
                   (lambda (f) (eq f 'eca--session-id)))
                  ((symbol-function 'eca--session-id)
                   (lambda (_) "test-session")))
          (let ((found (beads-agent-eca--find-chat-buffer mock-session)))
            (should (equal found test-buf))))
      (when (buffer-live-p test-buf)
        (kill-buffer test-buf)))))

(ert-deftest beads-agent-eca-test-find-chat-buffer-not-found ()
  "Test finding ECA chat buffer when not present."
  (let ((mock-session (list :id "test-session")))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (f) (eq f 'eca--session-id)))
              ((symbol-function 'eca--session-id)
               (lambda (_) "test-session")))
      (let ((found (beads-agent-eca--find-chat-buffer mock-session)))
        (should-not found)))))

(ert-deftest beads-agent-eca-test-find-chat-buffer-nil-session ()
  "Test finding ECA chat buffer with nil session."
  (should-not (beads-agent-eca--find-chat-buffer nil)))

(ert-deftest beads-agent-eca-test-get-session-for-dir ()
  "Test getting ECA session for directory."
  (let ((mock-session (list :id "test-session")))
    (cl-letf (((symbol-function 'featurep)
               (lambda (f) (eq f 'eca)))
              ((symbol-function 'fboundp)
               (lambda (f) (eq f 'eca-session)))
              ((symbol-function 'eca-session)
               (lambda () mock-session)))
      (let ((session (beads-agent-eca--get-session-for-dir "/tmp/test")))
        (should (equal session mock-session))))))

(ert-deftest beads-agent-eca-test-get-session-for-dir-no-eca ()
  "Test getting ECA session when eca not loaded."
  (cl-letf (((symbol-function 'featurep)
             (lambda (_) nil)))
    (let ((session (beads-agent-eca--get-session-for-dir "/tmp/test")))
      (should-not session))))

(ert-deftest beads-agent-eca-test-wait-for-session-found ()
  "Test waiting for session when found immediately."
  (let ((mock-session (list :id "test-session")))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (f) (eq f 'eca-session)))
              ((symbol-function 'eca-session)
               (lambda () mock-session)))
      (let ((session (beads-agent-eca--wait-for-session 1.0)))
        (should (equal session mock-session))))))

(ert-deftest beads-agent-eca-test-wait-for-session-timeout ()
  "Test waiting for session times out when not available."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (f) (eq f 'eca-session)))
            ((symbol-function 'eca-session)
             (lambda () nil)))
    (let ((session (beads-agent-eca--wait-for-session 0.2)))
      (should-not session))))

;;; Integration Tests

(ert-deftest beads-agent-eca-test-full-workflow ()
  "Test complete workflow: start → send-prompt → stop.
This integration test ensures state management works correctly
across the full lifecycle."
  (let* ((backend (beads-agent-backend-eca))
         (mock-eca-session (list :id "integration-test"))
         (mock-buffer (generate-new-buffer "*test-eca-integration*"))
         (prompts-sent nil)
         (eca-stopped nil))
    (unwind-protect
        (cl-letf (((symbol-function 'featurep)
                   (lambda (f) (memq f '(eca eca-chat eca-process))))
                  ((symbol-function 'require)
                   (lambda (&rest _) t))
                  ((symbol-function 'eca)
                   (lambda () t))
                  ((symbol-function 'eca-session)
                   (lambda () mock-eca-session))
                  ((symbol-function 'eca-stop)
                   (lambda () (setq eca-stopped t)))
                  ((symbol-function 'eca-chat-send-prompt)
                   (lambda (prompt) (push prompt prompts-sent)))
                  ((symbol-function 'eca-process-running-p)
                   (lambda (_) (not eca-stopped)))
                  ((symbol-function 'beads-agent-eca--find-chat-buffer)
                   (lambda (_) mock-buffer))
                  ((symbol-function 'beads-agent-eca--wait-for-session)
                   (lambda (&optional _) mock-eca-session))
                  ;; Mock the helper function
                  ((symbol-function 'beads-agent-eca--has-feature-p)
                   (lambda (feature _func)
                     (memq feature '(eca eca-chat eca-process))))
                  ((symbol-function 'fboundp)
                   (lambda (f)
                     (memq f '(eca-session eca-stop
                                           eca-process-running-p)))))
          ;; Step 1: Start session
          (let* ((result (beads-agent-backend-start
                         backend nil "Initial prompt"))
                 (session (beads-agent-session
                          :id "test#integration"
                          :issue-id "test"
                          :backend-name "eca"
                          :project-dir "/tmp/test"
                          :started-at "2025-01-01T00:00:00Z"
                          :buffer (cdr result)
                          :backend-session (car result))))
            ;; Verify start worked
            (should (equal (car prompts-sent) "Initial prompt"))
            (should (beads-agent-backend-session-active-p backend session))

            ;; Step 2: Send additional prompt
            (beads-agent-backend-send-prompt backend session "Follow-up")
            (should (equal (car prompts-sent) "Follow-up"))

            ;; Step 3: Stop session
            (beads-agent-backend-stop backend session)
            (should eca-stopped)
            (should-not (beads-agent-backend-session-active-p
                        backend session))))
      (when (buffer-live-p mock-buffer)
        (kill-buffer mock-buffer)))))

(ert-deftest beads-agent-eca-test-helper-has-feature-p ()
  "Test the helper function for feature checking."
  ;; When both feature and function are available
  (cl-letf (((symbol-function 'featurep)
             (lambda (f) (eq f 'test-feature)))
            ((symbol-function 'require)
             (lambda (&rest _) t))
            ((symbol-function 'fboundp)
             (lambda (f) (eq f 'test-func))))
    (should (beads-agent-eca--has-feature-p 'test-feature 'test-func)))

  ;; When feature is missing
  (cl-letf (((symbol-function 'featurep)
             (lambda (_) nil))
            ((symbol-function 'require)
             (lambda (&rest _) nil))
            ((symbol-function 'fboundp)
             (lambda (f) (eq f 'test-func))))
    (should-not (beads-agent-eca--has-feature-p
                'test-feature 'test-func)))

  ;; When function is missing
  (cl-letf (((symbol-function 'featurep)
             (lambda (f) (eq f 'test-feature)))
            ((symbol-function 'require)
             (lambda (&rest _) t))
            ((symbol-function 'fboundp)
             (lambda (_) nil)))
    (should-not (beads-agent-eca--has-feature-p
                'test-feature 'test-func))))

(provide 'beads-agent-eca-test)
;;; beads-agent-eca-test.el ends here
