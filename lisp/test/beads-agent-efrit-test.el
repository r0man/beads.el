;;; beads-agent-efrit-test.el --- Tests for beads-agent-efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Tests for the efrit backend for beads-agent.
;; All tests mock the efrit package since it may not be installed.

;;; Code:

(require 'ert)
(require 'beads-agent-backend)
(require 'beads-agent-efrit)

;;; Test Fixtures

(defvar beads-agent-efrit-test--mock-session nil
  "Mock efrit session for testing.")

(defun beads-agent-efrit-test--make-mock-efrit-session ()
  "Create a mock efrit session structure."
  (list 'efrit-session
        :id "test-session-123"
        :buffer (generate-new-buffer " *efrit-test*")
        :status 'active))

(defun beads-agent-efrit-test--session-id (session)
  "Get mock SESSION id."
  (plist-get (cdr session) :id))

(defun beads-agent-efrit-test--session-buffer (session)
  "Get mock SESSION buffer."
  (plist-get (cdr session) :buffer))

(defun beads-agent-efrit-test--session-status (session)
  "Get mock SESSION status."
  (plist-get (cdr session) :status))

;;; Backend Registration Tests

(ert-deftest beads-agent-efrit-test-backend-registered ()
  "Test that efrit backend is registered."
  (let ((backends (beads-agent--get-all-backends)))
    (should (cl-some (lambda (b) (equal (oref b name) "efrit")) backends))))

(ert-deftest beads-agent-efrit-test-backend-priority ()
  "Test efrit backend has correct priority."
  (let ((backend (beads-agent-backend-efrit)))
    (should (= (oref backend priority) 20))))

(ert-deftest beads-agent-efrit-test-backend-name ()
  "Test efrit backend has correct name."
  (let ((backend (beads-agent-backend-efrit)))
    (should (equal (oref backend name) "efrit"))))

(ert-deftest beads-agent-efrit-test-backend-description ()
  "Test efrit backend has a description."
  (let ((backend (beads-agent-backend-efrit)))
    (should (stringp (oref backend description)))
    (should (> (length (oref backend description)) 0))))

;;; Availability Tests

(ert-deftest beads-agent-efrit-test-available-when-feature-loaded ()
  "Test availability when efrit feature is already loaded."
  (let ((backend (beads-agent-backend-efrit)))
    ;; Mock featurep to return t for efrit
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature)
                 (eq feature 'efrit))))
      (should (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-efrit-test-available-when-require-succeeds ()
  "Test availability when efrit can be required."
  (let ((backend (beads-agent-backend-efrit)))
    ;; Mock featurep to return nil, but require succeeds
    (cl-letf (((symbol-function 'featurep)
               (lambda (_feature) nil))
              ((symbol-function 'require)
               (lambda (feature &optional _filename _noerror)
                 (eq feature 'efrit))))
      (should (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-efrit-test-not-available-when-missing ()
  "Test unavailability when efrit is not installed."
  (let ((backend (beads-agent-backend-efrit)))
    ;; Mock both featurep and require to fail
    (cl-letf (((symbol-function 'featurep)
               (lambda (_feature) nil))
              ((symbol-function 'require)
               (lambda (_feature &optional _filename _noerror) nil)))
      (should-not (beads-agent-backend-available-p backend)))))

;;; Start Session Tests

(ert-deftest beads-agent-efrit-test-start-creates-session ()
  "Test that start creates an efrit session."
  (let ((backend (beads-agent-backend-efrit))
        (mock-session (beads-agent-efrit-test--make-mock-efrit-session)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-do)
               (lambda (_prompt) mock-session))
              ((symbol-function 'efrit-session-id)
               #'beads-agent-efrit-test--session-id)
              ((symbol-function 'efrit-progress-get-buffer)
               (lambda (_id)
                 (beads-agent-efrit-test--session-buffer mock-session))))
      (let ((result (beads-agent-backend-start backend nil "Test prompt")))
        (should (consp result))
        (should (equal (car result) mock-session))
        (should (bufferp (cdr result)))))))

(ert-deftest beads-agent-efrit-test-start-sets-bd-no-daemon ()
  "Test that start sets BD_NO_DAEMON environment variable."
  (let ((backend (beads-agent-backend-efrit))
        (mock-session (beads-agent-efrit-test--make-mock-efrit-session))
        (captured-env nil))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-do)
               (lambda (_prompt)
                 (setq captured-env process-environment)
                 mock-session))
              ((symbol-function 'efrit-session-id)
               #'beads-agent-efrit-test--session-id)
              ((symbol-function 'efrit-progress-get-buffer)
               (lambda (_id) nil)))
      (beads-agent-backend-start backend nil "Test prompt")
      (should (member "BD_NO_DAEMON=1" captured-env)))))

(ert-deftest beads-agent-efrit-test-start-error-when-efrit-missing ()
  "Test that start signals error when efrit is not available."
  (let ((backend (beads-agent-backend-efrit)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) nil))
              ((symbol-function 'require) (lambda (&rest _) nil)))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

(ert-deftest beads-agent-efrit-test-start-propagates-efrit-errors ()
  "Test that start propagates errors from efrit-do."
  (let ((backend (beads-agent-backend-efrit)))
    (cl-letf (((symbol-function 'featurep) (lambda (_) t))
              ((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-do)
               (lambda (_prompt)
                 (error "Efrit initialization failed"))))
      (should-error (beads-agent-backend-start backend nil "Test prompt")
                    :type 'error))))

;;; Stop Session Tests

(ert-deftest beads-agent-efrit-test-stop-cancels-session ()
  "Test that stop cancels the efrit session."
  (let ((backend (beads-agent-backend-efrit))
        (mock-session (beads-agent-efrit-test--make-mock-efrit-session))
        (cancel-called nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-session-cancel)
               (lambda (session)
                 (setq cancel-called session))))
      (let ((beads-session (beads-agent-session
                            :id "test"
                            :backend-session mock-session)))
        (beads-agent-backend-stop backend beads-session)
        (should (equal cancel-called mock-session))))))

(ert-deftest beads-agent-efrit-test-stop-handles-nil-session ()
  "Test that stop handles nil backend session gracefully."
  (let ((backend (beads-agent-backend-efrit))
        (cancel-called nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-session-cancel)
               (lambda (_) (setq cancel-called t))))
      (let ((beads-session (beads-agent-session
                            :id "test"
                            :backend-session nil)))
        (beads-agent-backend-stop backend beads-session)
        (should-not cancel-called)))))

;;; Session Active Tests

(ert-deftest beads-agent-efrit-test-session-active-when-running ()
  "Test session-active-p returns t when session is running."
  (let ((backend (beads-agent-backend-efrit))
        (mock-session (beads-agent-efrit-test--make-mock-efrit-session)))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-session-active)
               (lambda () mock-session))
              ((symbol-function 'efrit-session-id)
               #'beads-agent-efrit-test--session-id)
              ((symbol-function 'efrit-session-status)
               (lambda (_) 'active)))
      (let ((beads-session (beads-agent-session
                            :id "test"
                            :backend-session mock-session)))
        (should (beads-agent-backend-session-active-p backend beads-session))))))

(ert-deftest beads-agent-efrit-test-session-not-active-when-completed ()
  "Test session-active-p returns nil when session is completed."
  (let ((backend (beads-agent-backend-efrit))
        (mock-session (beads-agent-efrit-test--make-mock-efrit-session)))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-session-active)
               (lambda () mock-session))
              ((symbol-function 'efrit-session-id)
               #'beads-agent-efrit-test--session-id)
              ((symbol-function 'efrit-session-status)
               (lambda (_) 'completed)))
      (let ((beads-session (beads-agent-session
                            :id "test"
                            :backend-session mock-session)))
        (should-not (beads-agent-backend-session-active-p backend beads-session))))))

(ert-deftest beads-agent-efrit-test-session-not-active-when-different ()
  "Test session-active-p returns nil when different session is active."
  (let ((backend (beads-agent-backend-efrit))
        (mock-session (beads-agent-efrit-test--make-mock-efrit-session))
        (other-session (list 'efrit-session :id "other-id" :status 'active)))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-session-active)
               (lambda () other-session))
              ((symbol-function 'efrit-session-id)
               #'beads-agent-efrit-test--session-id)
              ((symbol-function 'efrit-session-status)
               (lambda (_) 'active)))
      (let ((beads-session (beads-agent-session
                            :id "test"
                            :backend-session mock-session)))
        (should-not (beads-agent-backend-session-active-p backend beads-session))))))

(ert-deftest beads-agent-efrit-test-session-not-active-when-nil ()
  "Test session-active-p returns nil when no active session."
  (let ((backend (beads-agent-backend-efrit))
        (mock-session (beads-agent-efrit-test--make-mock-efrit-session)))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-session-active)
               (lambda () nil)))
      (let ((beads-session (beads-agent-session
                            :id "test"
                            :backend-session mock-session)))
        (should-not (beads-agent-backend-session-active-p backend beads-session))))))

;;; Switch to Buffer Tests

(ert-deftest beads-agent-efrit-test-switch-to-buffer-success ()
  "Test switch-to-buffer switches to session buffer."
  (let ((backend (beads-agent-backend-efrit))
        (test-buffer (generate-new-buffer " *efrit-switch-test*"))
        (switched-to nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--pop-to-buffer-other-window)
                   (lambda (buf) (setq switched-to buf))))
          (let ((beads-session (beads-agent-session :id "test")))
            (beads-agent-session-set-buffer beads-session test-buffer)
            (beads-agent-backend-switch-to-buffer backend beads-session)
            (should (equal switched-to test-buffer))))
      (kill-buffer test-buffer))))

(ert-deftest beads-agent-efrit-test-switch-to-buffer-killed-error ()
  "Test switch-to-buffer errors when buffer is killed."
  (let ((backend (beads-agent-backend-efrit))
        (test-buffer (generate-new-buffer " *efrit-killed-test*")))
    (kill-buffer test-buffer)
    (let ((beads-session (beads-agent-session :id "test")))
      (beads-agent-session-set-buffer beads-session test-buffer)
      (should-error (beads-agent-backend-switch-to-buffer backend beads-session)
                    :type 'user-error))))

;;; Send Prompt Tests

(ert-deftest beads-agent-efrit-test-send-prompt-injects ()
  "Test send-prompt injects guidance into session."
  (let ((backend (beads-agent-backend-efrit))
        (mock-session (beads-agent-efrit-test--make-mock-efrit-session))
        (injected-prompt nil)
        (injected-type nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-session-id)
               #'beads-agent-efrit-test--session-id)
              ((symbol-function 'efrit-progress-inject)
               (lambda (_id type prompt)
                 (setq injected-type type)
                 (setq injected-prompt prompt))))
      (let ((beads-session (beads-agent-session
                            :id "test"
                            :backend-session mock-session)))
        (beads-agent-backend-send-prompt backend beads-session "Follow-up guidance")
        (should (equal injected-type 'guidance))
        (should (equal injected-prompt "Follow-up guidance"))))))

(ert-deftest beads-agent-efrit-test-send-prompt-starts-new-when-no-session ()
  "Test send-prompt starts new efrit-do when no backend session."
  (let ((backend (beads-agent-backend-efrit))
        (started-prompt nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'efrit-do)
               (lambda (prompt)
                 (setq started-prompt prompt)
                 nil)))
      (let ((beads-session (beads-agent-session
                            :id "test"
                            :backend-session nil)))
        (beads-agent-backend-send-prompt backend beads-session "New task")
        (should (equal started-prompt "New task"))))))

;;; Customization Tests

(ert-deftest beads-agent-efrit-test-interface-customization-exists ()
  "Test that interface customization variable exists."
  (should (boundp 'beads-agent-efrit-interface)))

(ert-deftest beads-agent-efrit-test-interface-default-value ()
  "Test that interface defaults to efrit-do."
  (should (eq (default-value 'beads-agent-efrit-interface) 'efrit-do)))

(ert-deftest beads-agent-efrit-test-customization-group-exists ()
  "Test that customization group exists."
  (should (get 'beads-agent-efrit 'group-documentation)))

(provide 'beads-agent-efrit-test)
;;; beads-agent-efrit-test.el ends here
