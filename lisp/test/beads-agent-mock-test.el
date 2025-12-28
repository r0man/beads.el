;;; beads-agent-mock-test.el --- Tests for beads-agent-mock -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Unit tests for beads-agent-mock.el - the mock backend for testing.
;; These tests verify the mock backend functionality itself works correctly.

;;; Code:

(require 'ert)
(require 'beads-agent)
(require 'beads-agent-mock)

;;; Test Fixtures

(defun beads-agent-mock-test--with-clean-state (body-fn)
  "Run BODY-FN with clean mock state."
  (let ((saved-available beads-agent-mock-available)
        (saved-start-error beads-agent-mock-start-should-error)
        (saved-stop-error beads-agent-mock-stop-should-error))
    (unwind-protect
        (progn
          (beads-agent-mock-reset)
          (funcall body-fn))
      ;; Restore saved state
      (beads-agent-mock-reset)
      (setq beads-agent-mock-available saved-available
            beads-agent-mock-start-should-error saved-start-error
            beads-agent-mock-stop-should-error saved-stop-error))))

;;; Configuration Variable Tests

(ert-deftest beads-agent-mock-test-available-default ()
  "Test that mock available defaults to t."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (should beads-agent-mock-available))))

(ert-deftest beads-agent-mock-test-available-can-be-disabled ()
  "Test that mock availability can be disabled."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (setq beads-agent-mock-available nil)
     (let ((backend (beads-agent-mock-get-instance)))
       (should-not (beads-agent-backend-available-p backend))))))

(ert-deftest beads-agent-mock-test-start-error-default ()
  "Test that start error defaults to nil."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (should-not beads-agent-mock-start-should-error))))

(ert-deftest beads-agent-mock-test-stop-error-default ()
  "Test that stop error defaults to nil."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (should-not beads-agent-mock-stop-should-error))))

;;; Mock Backend Class Tests

(ert-deftest beads-agent-mock-test-class-exists ()
  "Test that mock backend class is defined."
  (should (class-p 'beads-agent-backend-mock)))

(ert-deftest beads-agent-mock-test-class-inherits ()
  "Test that mock backend inherits from beads-agent-backend."
  (should (child-of-class-p 'beads-agent-backend-mock 'beads-agent-backend)))

(ert-deftest beads-agent-mock-test-instance-creation ()
  "Test creating mock backend instance."
  (let ((backend (beads-agent-backend-mock)))
    (should (beads-agent-backend-mock-p backend))
    (should (equal (oref backend name) "mock"))
    (should (equal (oref backend priority) 100))))

;;; Singleton Instance Tests

(ert-deftest beads-agent-mock-test-singleton-get ()
  "Test getting mock backend singleton."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let ((instance1 (beads-agent-mock-get-instance))
           (instance2 (beads-agent-mock-get-instance)))
       (should (beads-agent-backend-mock-p instance1))
       (should (eq instance1 instance2))))))

;;; Registration Tests

(ert-deftest beads-agent-mock-test-register ()
  "Test registering mock backend."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     ;; First unregister if already registered
     (beads-agent-mock-unregister)
     ;; Now register
     (let ((backend (beads-agent-mock-register)))
       (should (beads-agent-backend-mock-p backend))
       (should (member backend beads-agent--backends))))))

(ert-deftest beads-agent-mock-test-unregister ()
  "Test unregistering mock backend."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     ;; Ensure registered first
     (beads-agent-mock-register)
     ;; Now unregister
     (beads-agent-mock-unregister)
     (should-not (cl-find-if
                  (lambda (b) (equal (oref b name) "mock"))
                  beads-agent--backends)))))

;;; Start Session Tests

(ert-deftest beads-agent-mock-test-start-returns-cons ()
  "Test that start returns (handle . buffer) cons."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test"))
            (result (beads-agent-backend-start backend issue "test prompt")))
       (should (consp result))
       (should (beads-agent-mock-session-handle-p (car result)))
       (should (bufferp (cdr result)))
       ;; Clean up
       (when (buffer-live-p (cdr result))
         (kill-buffer (cdr result)))))))

(ert-deftest beads-agent-mock-test-start-tracks-session ()
  "Test that start tracks session in internal state."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test"))
            (result (beads-agent-backend-start backend issue "prompt")))
       (should (= 1 (length (beads-agent-mock-sessions))))
       ;; Clean up
       (when (buffer-live-p (cdr result))
         (kill-buffer (cdr result)))))))

(ert-deftest beads-agent-mock-test-start-records-call ()
  "Test that start call is recorded."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test"))
            (result (beads-agent-backend-start backend issue "test prompt")))
       (should (= 1 (length (beads-agent-mock-start-calls))))
       (let ((call (car (beads-agent-mock-start-calls))))
         (should (equal (car call) issue))
         (should (equal (cadr call) "test prompt")))
       ;; Clean up
       (when (buffer-live-p (cdr result))
         (kill-buffer (cdr result)))))))

(ert-deftest beads-agent-mock-test-start-error-signals ()
  "Test that start signals error when configured."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (setq beads-agent-mock-start-should-error t)
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test")))
       (should-error (beads-agent-backend-start backend issue "prompt")
                     :type 'error)))))

(ert-deftest beads-agent-mock-test-start-error-custom-message ()
  "Test that start error uses custom message."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (setq beads-agent-mock-start-should-error "Custom error")
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test")))
       (condition-case err
           (beads-agent-backend-start backend issue "prompt")
         (error
          (should (string= (cadr err) "Custom error"))))))))

;;; Session Handle Tests

(ert-deftest beads-agent-mock-test-handle-class-exists ()
  "Test that mock session handle class is defined."
  (should (class-p 'beads-agent-mock-session-handle)))

(ert-deftest beads-agent-mock-test-handle-active-by-default ()
  "Test that new session handle is active."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test"))
            (result (beads-agent-backend-start backend issue "prompt"))
            (handle (car result)))
       (should (oref handle active))
       ;; Clean up
       (when (buffer-live-p (cdr result))
         (kill-buffer (cdr result)))))))

;;; Reset Tests

(ert-deftest beads-agent-mock-test-reset-clears-sessions ()
  "Test that reset clears all sessions."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test")))
       ;; Start a session
       (beads-agent-backend-start backend issue "prompt")
       (should (= 1 (length (beads-agent-mock-sessions))))
       ;; Reset
       (beads-agent-mock-reset)
       (should (= 0 (length (beads-agent-mock-sessions))))))))

(ert-deftest beads-agent-mock-test-reset-clears-call-logs ()
  "Test that reset clears call logs."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test")))
       ;; Start a session
       (beads-agent-backend-start backend issue "prompt")
       (should (= 1 (length (beads-agent-mock-start-calls))))
       ;; Reset
       (beads-agent-mock-reset)
       (should (= 0 (length (beads-agent-mock-start-calls))))
       (should (= 0 (length (beads-agent-mock-stop-calls))))
       (should (= 0 (length (beads-agent-mock-sent-prompts))))))))

(ert-deftest beads-agent-mock-test-reset-restores-defaults ()
  "Test that reset restores default configuration."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (setq beads-agent-mock-available nil
           beads-agent-mock-start-should-error t
           beads-agent-mock-stop-should-error t)
     (beads-agent-mock-reset)
     (should beads-agent-mock-available)
     (should-not beads-agent-mock-start-should-error)
     (should-not beads-agent-mock-stop-should-error))))

;;; Active Sessions Tests

(ert-deftest beads-agent-mock-test-active-sessions-returns-active ()
  "Test that active-sessions returns only active sessions."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test")))
       (beads-agent-backend-start backend issue "prompt")
       (should (= 1 (length (beads-agent-mock-active-sessions))))
       ;; All sessions are active after start
       (should (= 1 (length (beads-agent-mock-sessions))))))))

;;; Assertion Helper Tests

(ert-deftest beads-agent-mock-test-assert-start-called-succeeds ()
  "Test that assert-start-called succeeds when called."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test"))
            (result (beads-agent-backend-start backend issue "prompt")))
       (should-not (beads-agent-mock-assert-start-called 1))
       ;; Clean up
       (when (buffer-live-p (cdr result))
         (kill-buffer (cdr result)))))))

(ert-deftest beads-agent-mock-test-assert-start-called-fails ()
  "Test that assert-start-called fails when not called."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (should-error (beads-agent-mock-assert-start-called 1)))))

(ert-deftest beads-agent-mock-test-assert-no-sessions-succeeds ()
  "Test that assert-no-sessions succeeds with no sessions."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (should-not (beads-agent-mock-assert-no-sessions)))))

(ert-deftest beads-agent-mock-test-assert-no-sessions-fails ()
  "Test that assert-no-sessions fails with sessions."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test"))
            (result (beads-agent-backend-start backend issue "prompt")))
       (should-error (beads-agent-mock-assert-no-sessions))
       ;; Clean up
       (when (buffer-live-p (cdr result))
         (kill-buffer (cdr result)))))))

(ert-deftest beads-agent-mock-test-assert-session-count-succeeds ()
  "Test that assert-session-count succeeds with correct count."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (issue (beads-issue :id "test-1" :title "Test"))
            (result (beads-agent-backend-start backend issue "prompt")))
       (should-not (beads-agent-mock-assert-session-count 1))
       ;; Clean up
       (when (buffer-live-p (cdr result))
         (kill-buffer (cdr result)))))))

(ert-deftest beads-agent-mock-test-assert-session-count-fails ()
  "Test that assert-session-count fails with wrong count."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (should-error (beads-agent-mock-assert-session-count 1)))))

;;; Send Prompt Tests

(ert-deftest beads-agent-mock-test-send-prompt-records-call ()
  "Test that send-prompt records the call."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (session (beads-agent-session
                      :id "test-session"
                      :issue-id "test-1"
                      :backend-name "mock"
                      :project-dir "/tmp"
                      :started-at "2025-01-01T00:00:00Z")))
       (beads-agent-backend-send-prompt backend session "follow-up prompt")
       (should (= 1 (length (beads-agent-mock-sent-prompts))))
       (let ((call (car (beads-agent-mock-sent-prompts))))
         (should (equal (car call) session))
         (should (equal (cadr call) "follow-up prompt")))))))

;;; Stop Error Tests

(ert-deftest beads-agent-mock-test-stop-with-error-flag ()
  "Test that stop raises error when error flag is set."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (beads-agent-mock-stop-should-error t)
            (session (beads-agent-session
                      :id "test-session"
                      :issue-id "test-1"
                      :backend-name "mock"
                      :project-dir "/tmp"
                      :started-at "2025-01-01T00:00:00Z")))
       (should-error (beads-agent-backend-stop backend session))))))

(ert-deftest beads-agent-mock-test-stop-with-error-message ()
  "Test that stop raises custom error message."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (beads-agent-mock-stop-should-error "Custom error message")
            (session (beads-agent-session
                      :id "test-session"
                      :issue-id "test-1"
                      :backend-name "mock"
                      :project-dir "/tmp"
                      :started-at "2025-01-01T00:00:00Z")))
       (should-error (beads-agent-backend-stop backend session)
                     :type 'error)))))

;;; Session Active Tests

(ert-deftest beads-agent-mock-test-session-active-p-nil-for-unknown ()
  "Test session-active-p returns nil for unknown session."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (session (beads-agent-session
                      :id "unknown-session"
                      :issue-id "test-1"
                      :backend-name "mock"
                      :project-dir "/tmp"
                      :started-at "2025-01-01T00:00:00Z"
                      :backend-session (beads-agent-mock-session-handle
                                       :id "unknown-handle"))))
       (should-not (beads-agent-backend-session-active-p backend session))))))

;;; Switch To Buffer Tests

(ert-deftest beads-agent-mock-test-switch-to-buffer-no-buffer ()
  "Test switch-to-buffer errors when no buffer."
  (beads-agent-mock-test--with-clean-state
   (lambda ()
     (let* ((backend (beads-agent-mock-get-instance))
            (session (beads-agent-session
                      :id "test-session"
                      :issue-id "test-1"
                      :backend-name "mock"
                      :project-dir "/tmp"
                      :started-at "2025-01-01T00:00:00Z")))
       (should-error (beads-agent-backend-switch-to-buffer backend session)
                     :type 'user-error)))))

(provide 'beads-agent-mock-test)
;;; beads-agent-mock-test.el ends here
