;;; beads-agent-ralph-backend-test.el --- Tests for Ralph backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Tests for `beads-agent-backend-ralph', the `beads-agent-backend'
;; subclass that exposes the Ralph controller to the rest of beads.el.
;; Mirrors the registration/availability/protocol coverage other
;; backend tests already provide so the registry stays consistent.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-backend)
(require 'beads-agent-ralph-backend)

;;; Backend Registration

(ert-deftest beads-agent-ralph-backend-test-backend-registered ()
  "Ralph backend is present in the global backend registry on load."
  (let ((backends (beads-agent--get-all-backends)))
    (should (cl-some (lambda (b) (equal (oref b name) "ralph")) backends))))

(ert-deftest beads-agent-ralph-backend-test-backend-name ()
  "Ralph backend reports its conventional name."
  (let ((backend (beads-agent-backend-ralph)))
    (should (equal (oref backend name) "ralph"))))

(ert-deftest beads-agent-ralph-backend-test-backend-priority ()
  "Ralph backend reports its declared priority."
  (let ((backend (beads-agent-backend-ralph)))
    (should (= (oref backend priority) 60))))

(ert-deftest beads-agent-ralph-backend-test-backend-description ()
  "Ralph backend has a non-empty description string."
  (let ((backend (beads-agent-backend-ralph)))
    (should (stringp (oref backend description)))
    (should (> (length (oref backend description)) 0))))

;;; Availability

(ert-deftest beads-agent-ralph-backend-test-available-when-claude-on-path ()
  "Backend is available iff `claude' is on `exec-path'."
  (let ((backend (beads-agent-backend-ralph)))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (when (equal name "claude") "/usr/bin/claude"))))
      (should (beads-agent-backend-available-p backend)))))

(ert-deftest beads-agent-ralph-backend-test-not-available-without-claude ()
  "Backend reports unavailable when `claude' cannot be found."
  (let ((backend (beads-agent-backend-ralph)))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-not (beads-agent-backend-available-p backend)))))

;;; Protocol

(ert-deftest beads-agent-ralph-backend-test-send-prompt-rejected ()
  "Ad-hoc send-prompt signals user-error: Ralph drives its own loop."
  (let ((backend (beads-agent-backend-ralph)))
    (should-error
     (beads-agent-backend-send-prompt backend nil "hi")
     :type 'user-error)))

(defun beads-agent-ralph-backend-test--make-session (controller)
  "Build a `beads-agent-session' whose `backend-session' is CONTROLLER."
  (beads-agent-session
   :id "ralph#1"
   :project-dir "/tmp/ralph-test"
   :backend-name "ralph"
   :started-at "2026-05-12T00:00:00Z"
   :backend-session controller))

(ert-deftest beads-agent-ralph-backend-test-session-active-while-running ()
  "Session is active when the controller is running or cooling down."
  (let* ((backend (beads-agent-backend-ralph))
         (controller (beads-agent-ralph--controller
                      :root-id "bde-x" :status 'running))
         (session (beads-agent-ralph-backend-test--make-session controller)))
    (should (beads-agent-backend-session-active-p backend session))
    (oset controller status 'cooling-down)
    (should (beads-agent-backend-session-active-p backend session))))

(ert-deftest beads-agent-ralph-backend-test-session-inactive-on-auto-paused ()
  "Auto-paused is treated as inactive so sesman can sweep cleanly."
  (let* ((backend (beads-agent-backend-ralph))
         (controller (beads-agent-ralph--controller
                      :root-id "bde-x" :status 'auto-paused))
         (session (beads-agent-ralph-backend-test--make-session controller)))
    (should-not (beads-agent-backend-session-active-p backend session))))

(ert-deftest beads-agent-ralph-backend-test-session-inactive-on-terminal ()
  "Terminal states (`done', `stopped', `failed') are inactive."
  (let ((backend (beads-agent-backend-ralph)))
    (dolist (state '(done stopped failed idle))
      (let* ((controller (beads-agent-ralph--controller
                          :root-id "bde-x" :status state))
             (session (beads-agent-ralph-backend-test--make-session controller)))
        (should-not (beads-agent-backend-session-active-p backend session))))))

(ert-deftest beads-agent-ralph-backend-test-start-delegates-to-entry-point ()
  "`start' forwards :issue and :prompt to `beads-agent-ralph-start'."
  (let* ((backend (beads-agent-backend-ralph))
         (calls nil))
    (cl-letf (((symbol-function 'beads-agent-ralph-start)
               (lambda (&rest args) (push args calls) 'sentinel)))
      (should (eq (beads-agent-backend-start backend "bde-r" "P") 'sentinel)))
    (let ((args (car calls)))
      (should (equal (plist-get args :issue) "bde-r"))
      (should (equal (plist-get args :prompt) "P")))))

;;; Autoloads

(ert-deftest beads-agent-ralph-backend-test-entry-points-autoloaded ()
  "Entry-point commands carry `;;;###autoload' cookies and are bound."
  (should (fboundp 'beads-agent-ralph-start))
  (should (fboundp 'beads-agent-ralph-stop))
  (should (fboundp 'beads-agent-ralph-show-history))
  (should (commandp 'beads-agent-ralph-show-history)))

(provide 'beads-agent-ralph-backend-test)

;;; beads-agent-ralph-backend-test.el ends here
