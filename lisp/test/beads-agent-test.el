;;; beads-agent-test.el --- Tests for beads-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-agent.el AI agent integration.
;; Tests cover backend registration, session management, and
;; the core abstraction layer.
;;
;; This test file uses a mock backend to test without requiring
;; real AI agent packages.

;;; Code:

(require 'ert)
(require 'beads-agent)
(require 'beads-sesman)
(require 'beads-agent-mock)

;;; Mock Sesman Storage for Testing
;;
;; Since sessions are now stored in sesman, we need to mock sesman
;; functions to keep tests isolated.  The mock storage simulates
;; sesman's session registry.

(defvar beads-agent-test--sesman-sessions nil
  "Mock sesman session storage for tests.
List of sesman session lists: ((name handle beads-session) ...).")

;;; Test-Only Mock Backend
;;
;; This is a lightweight mock for internal tests that don't need buffer
;; creation.  For tests that need real buffer functionality, use the
;; production mock from beads-agent-mock.el.

(defclass beads-agent-backend-test-mock (beads-agent-backend)
  ((name :initform "mock")
   (priority :initform 100)
   (start-called :initform nil :type boolean)
   (stop-called :initform nil :type boolean)
   (active-sessions :initform nil :type list))
  :documentation "Lightweight mock backend for testing.
Does not create real buffers - use production mock for that.")

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-test-mock))
  "Mock backend is always available."
  t)

(cl-defmethod beads-agent-backend-start
    ((backend beads-agent-backend-test-mock) _issue _prompt)
  "Mock starting a session."
  (oset backend start-called t)
  ;; Return a mock session handle
  'mock-session-handle)

(cl-defmethod beads-agent-backend-stop
    ((backend beads-agent-backend-test-mock) session)
  "Mock stopping a session."
  (oset backend stop-called t)
  (oset backend active-sessions
        (delete (oref session id) (oref backend active-sessions))))

(cl-defmethod beads-agent-backend-stop-async
    ((backend beads-agent-backend-test-mock) session callback)
  "Mock stopping a session asynchronously.
Calls sync stop immediately for testing purposes."
  (beads-agent-backend-stop backend session)
  (when callback
    (funcall callback)))

(cl-defmethod beads-agent-backend-session-active-p
    ((backend beads-agent-backend-test-mock) session)
  "Mock session active check."
  (member (oref session id) (oref backend active-sessions)))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((_backend beads-agent-backend-test-mock) _session)
  "Mock buffer switch."
  t)

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-test-mock) _session _prompt)
  "Mock sending prompt."
  t)

;;; Test Fixtures

(defvar beads-agent-test--mock-backend nil
  "Mock backend instance for tests.")

(defvar beads-agent-test--saved-hook-handlers nil
  "Saved hook handlers to restore after tests.")

(defun beads-agent-test--mock-sesman-sessions (_system)
  "Mock implementation of `sesman-sessions' for tests."
  beads-agent-test--sesman-sessions)

(defun beads-agent-test--mock-state-change-handler (action session)
  "Mock state change handler that updates mock sesman storage.
ACTION is `started', `stopped', or `failed'.
SESSION is the beads-agent-session object."
  (pcase action
    ('started
     ;; Add to mock sesman storage
     (let* ((name (beads-sesman--session-name session))
            (sesman-session (list name (oref session backend-session) session)))
       (push sesman-session beads-agent-test--sesman-sessions)))
    ('stopped
     ;; Remove from mock sesman storage
     (setq beads-agent-test--sesman-sessions
           (cl-remove-if (lambda (s)
                           (eq (nth 2 s) session))
                         beads-agent-test--sesman-sessions)))))

(defun beads-agent-test--setup ()
  "Setup test fixtures."
  ;; Clear mock sesman storage
  (setq beads-agent-test--sesman-sessions nil)
  ;; Clear backends
  (setq beads-agent--backends nil)
  ;; Save and replace hook handlers
  (setq beads-agent-test--saved-hook-handlers beads-agent-state-change-hook)
  (setq beads-agent-state-change-hook
        (list #'beads-agent-test--mock-state-change-handler))
  ;; Create and register test mock backend (lightweight, no buffers)
  (setq beads-agent-test--mock-backend (beads-agent-backend-test-mock))
  (beads-agent--register-backend beads-agent-test--mock-backend))

(defun beads-agent-test--teardown ()
  "Teardown test fixtures."
  ;; Clear mock sesman storage
  (setq beads-agent-test--sesman-sessions nil)
  ;; Clear backends
  (setq beads-agent--backends nil)
  ;; Restore original hook handlers
  (setq beads-agent-state-change-hook beads-agent-test--saved-hook-handlers)
  (setq beads-agent-test--saved-hook-handlers nil)
  (setq beads-agent-test--mock-backend nil))

;;; Tests for Backend Registration

(ert-deftest beads-agent-test-register-backend ()
  "Test backend registration."
  (beads-agent-test--setup)
  (unwind-protect
      (progn
        (should (= (length beads-agent--backends) 1))
        (should (equal (oref (car beads-agent--backends) name) "mock")))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-register-backend-replaces-existing ()
  "Test that registering backend with same name replaces existing."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((new-backend (beads-agent-backend-mock)))
        (oset new-backend priority 50)
        (beads-agent--register-backend new-backend)
        ;; Should still have only one backend
        (should (= (length beads-agent--backends) 1))
        ;; Should have new priority
        (should (= (oref (car beads-agent--backends) priority) 50)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-get-backend ()
  "Test getting backend by name."
  (beads-agent-test--setup)
  (unwind-protect
      (progn
        (should (beads-agent--get-backend "mock"))
        (should (null (beads-agent--get-backend "nonexistent"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-get-available-backends ()
  "Test getting available backends."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((available (beads-agent--get-available-backends)))
        (should (= (length available) 1))
        (should (equal (oref (car available) name) "mock")))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-backend-names ()
  "Test getting backend names."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((names (beads-agent--backend-names)))
        (should (equal names '("mock"))))
    (beads-agent-test--teardown)))

;;; Tests for Session Management

;;; Tests for Session Numbering

(ert-deftest beads-agent-test-session-number-from-id ()
  "Test extracting session number from ID."
  ;; Valid formats
  (should (= (beads-agent--session-number-from-id "bd-123#1") 1))
  (should (= (beads-agent--session-number-from-id "bd-123#42") 42))
  (should (= (beads-agent--session-number-from-id "my-project#100") 100))
  ;; Edge cases with special characters in issue ID
  (should (= (beads-agent--session-number-from-id "beads.el-abc#5") 5))
  ;; Invalid formats
  (should (null (beads-agent--session-number-from-id "bd-123")))
  (should (null (beads-agent--session-number-from-id "session-20251217-abcd")))
  (should (null (beads-agent--session-number-from-id nil)))
  (should (null (beads-agent--session-number-from-id "")))
  ;; Number must be at the end
  (should (null (beads-agent--session-number-from-id "bd-123#1extra"))))

(ert-deftest beads-agent-test-next-session-number-empty ()
  "Test next session number when no sessions exist."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; No sessions exist
        (should (= (beads-agent--next-session-number "bd-123") 1)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-next-session-number-existing ()
  "Test next session number with existing sessions."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Create some sessions
        (beads-agent--create-session "bd-123" "mock" "/tmp" 'h1)
        (beads-agent--create-session "bd-123" "mock" "/tmp" 'h2)
        ;; Next should be 3
        (should (= (beads-agent--next-session-number "bd-123") 3)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-next-session-number-per-issue ()
  "Test that session numbers are independent per issue."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Create sessions for different issues
        (beads-agent--create-session "bd-123" "mock" "/tmp" 'h1)
        (beads-agent--create-session "bd-123" "mock" "/tmp" 'h2)
        (beads-agent--create-session "bd-456" "mock" "/tmp" 'h3)
        ;; Each issue has independent numbering
        (should (= (beads-agent--next-session-number "bd-123") 3))
        (should (= (beads-agent--next-session-number "bd-456") 2))
        (should (= (beads-agent--next-session-number "bd-789") 1)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-next-session-number-gaps ()
  "Test that gaps in numbering don't affect next number."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Create sessions
        (let ((s1 (beads-agent--create-session "bd-123" "mock" "/tmp" 'h1))
              (s2 (beads-agent--create-session "bd-123" "mock" "/tmp" 'h2)))
          ;; Destroy session 1, leaving a gap
          (beads-agent--destroy-session (oref s1 id))
          ;; Should still return max+1, not reuse the gap
          (should (= (beads-agent--next-session-number "bd-123") 3))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-generate-session-id ()
  "Test session ID generation with numbered format."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; First session for an issue should be #1
        (let ((id1 (beads-agent--generate-session-id "bd-123")))
          (should (equal id1 "bd-123#1")))
        ;; After creating a session, next should be #2
        (beads-agent--create-session "bd-123" "mock" "/tmp" 'h1)
        (let ((id2 (beads-agent--generate-session-id "bd-123")))
          (should (equal id2 "bd-123#2")))
        ;; Different issue starts at #1
        (let ((id3 (beads-agent--generate-session-id "bd-456")))
          (should (equal id3 "bd-456#1"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-create-session ()
  "Test session creation."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-123" "mock" "/tmp/project" 'handle)))
          (should (beads-agent-session-p session))
          (should (equal (oref session issue-id) "bd-123"))
          (should (equal (oref session backend-name) "mock"))
          (should (equal (oref session project-dir) "/tmp/project"))
          (should (equal (oref session backend-session) 'handle))
          ;; Should be registered in mock sesman storage
          (should (= (length beads-agent-test--sesman-sessions) 1))
          ;; Should be retrievable via get-session
          (should (beads-agent--get-session (oref session id)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-destroy-session ()
  "Test session destruction."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let* ((session (beads-agent--create-session
                         "bd-123" "mock" "/tmp/project" 'handle))
               (session-id (oref session id)))
          ;; Verify exists
          (should (beads-agent--get-session session-id))
          ;; Destroy
          (beads-agent--destroy-session session-id)
          ;; Should be gone
          (should (null (beads-agent--get-session session-id)))
          (should (null (beads-agent--get-sessions-for-issue "bd-123")))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-get-session ()
  "Test getting session by ID."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let* ((session (beads-agent--create-session
                         "bd-123" "mock" "/tmp/project" 'handle))
               (session-id (oref session id)))
          (should (equal (beads-agent--get-session session-id) session))
          (should (null (beads-agent--get-session "nonexistent")))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-get-sessions-for-issue ()
  "Test getting sessions for an issue."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (beads-agent--create-session "bd-123" "mock" "/tmp/project" 'h1)
        (beads-agent--create-session "bd-123" "mock" "/tmp/project" 'h2)
        (beads-agent--create-session "bd-456" "mock" "/tmp/project" 'h3)
        (let ((sessions (beads-agent--get-sessions-for-issue "bd-123")))
          (should (= (length sessions) 2))
          (should (cl-every (lambda (s)
                              (equal (oref s issue-id) "bd-123"))
                            sessions))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-get-all-sessions ()
  "Test getting all sessions."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (beads-agent--create-session "bd-1" "mock" "/tmp" 'h1)
        (beads-agent--create-session "bd-2" "mock" "/tmp" 'h2)
        (beads-agent--create-session "bd-3" "mock" "/tmp" 'h3)
        (let ((all (beads-agent--get-all-sessions)))
          (should (= (length all) 3))))
    (beads-agent-test--teardown)))

;;; Tests for Issue Outcomes

(ert-deftest beads-agent-test-get-issue-outcome-none ()
  "Test getting outcome when no outcome recorded."
  (beads-agent-test--setup)
  (unwind-protect
      (progn
        ;; Clear any existing outcomes
        (clrhash beads-agent--issue-outcomes)
        (should (null (beads-agent--get-issue-outcome "bd-123"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-get-issue-outcome-finished ()
  "Test getting finished outcome."
  (beads-agent-test--setup)
  (unwind-protect
      (progn
        (clrhash beads-agent--issue-outcomes)
        (puthash "bd-123" 'finished beads-agent--issue-outcomes)
        (should (eq (beads-agent--get-issue-outcome "bd-123") 'finished)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-get-issue-outcome-failed ()
  "Test getting failed outcome."
  (beads-agent-test--setup)
  (unwind-protect
      (progn
        (clrhash beads-agent--issue-outcomes)
        (puthash "bd-123" 'failed beads-agent--issue-outcomes)
        (should (eq (beads-agent--get-issue-outcome "bd-123") 'failed)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-outcome-cleared-on-start ()
  "Test that outcome is cleared when a new session starts."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (clrhash beads-agent--issue-outcomes)
        ;; Set a previous outcome
        (puthash "bd-123" 'finished beads-agent--issue-outcomes)
        (should (eq (beads-agent--get-issue-outcome "bd-123") 'finished))
        ;; Start a new session
        (beads-agent--create-session "bd-123" "mock" "/tmp" 'h1)
        ;; Outcome should be cleared
        (should (null (beads-agent--get-issue-outcome "bd-123"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-outcome-cleared-on-stop ()
  "Test that outcome is cleared when session stops.
Stopped agents should not show indicators - only active or failed agents
should have indicators displayed."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (clrhash beads-agent--issue-outcomes)
        ;; Create and then destroy a session with agent type
        (let* ((session (beads-agent--create-session
                         "bd-123" "mock" "/tmp" 'h1 nil "Task"))
               (session-id (oref session id)))
          ;; Outcome should be nil while running (cleared on start)
          (should (null (beads-agent--get-issue-outcome "bd-123")))
          ;; Destroy the session
          (beads-agent--destroy-session session-id)
          ;; Outcome should be cleared (nil) - stopped agents don't show indicators
          (should (null (beads-agent--get-issue-outcome "bd-123")))))
    (beads-agent-test--teardown)))

;;; Tests for Context Building

(ert-deftest beads-agent-test-build-prompt ()
  "Test building prompt from issue."
  ;; Create a mock issue object
  (let ((issue (beads-issue :id "bd-123"
                            :title "Test Issue"
                            :description "This is a test"
                            :acceptance-criteria "It works")))
    (let ((prompt (beads-agent--build-prompt issue)))
      (should (stringp prompt))
      (should (string-match-p "bd-123" prompt))
      (should (string-match-p "Test Issue" prompt))
      (should (string-match-p "This is a test" prompt))
      (should (string-match-p "It works" prompt)))))

(ert-deftest beads-agent-test-build-prompt-minimal ()
  "Test building prompt from issue with minimal fields."
  (let ((issue (beads-issue :id "bd-456" :title "Minimal")))
    (let ((prompt (beads-agent--build-prompt issue)))
      (should (stringp prompt))
      (should (string-match-p "bd-456" prompt))
      (should (string-match-p "Minimal" prompt)))))

;;; Tests for Backend Protocol

(ert-deftest beads-agent-test-backend-available-p ()
  "Test backend availability check."
  (beads-agent-test--setup)
  (unwind-protect
      (should (beads-agent-backend-available-p beads-agent-test--mock-backend))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-backend-start ()
  "Test backend start method."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((issue (beads-issue :id "bd-123" :title "Test")))
        (beads-agent-backend-start beads-agent-test--mock-backend issue "prompt")
        (should (oref beads-agent-test--mock-backend start-called)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-backend-stop ()
  "Test backend stop method."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((session (beads-agent--create-session
                      "bd-123" "mock" "/tmp" 'handle)))
        (beads-agent-backend-stop beads-agent-test--mock-backend session)
        (should (oref beads-agent-test--mock-backend stop-called)))
    (beads-agent-test--teardown)))

;;; Tests for Transient Menu Header

(ert-deftest beads-agent-test-format-header-no-sessions ()
  "Test header format with no sessions."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((header (beads-agent--format-header)))
          (should (stringp header))
          (should (string-match-p "0 session" header))
          (should (string-match-p "1 backend" header))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-format-header-with-sessions ()
  "Test header format with sessions."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (beads-agent--create-session "bd-1" "mock" "/tmp" 'h1)
        (beads-agent--create-session "bd-2" "mock" "/tmp" 'h2)
        (let ((header (beads-agent--format-header)))
          (should (string-match-p "2 sessions" header))))
    (beads-agent-test--teardown)))

;;; Tests for Customization

(ert-deftest beads-agent-test-custom-auto-set-in-progress ()
  "Test auto-set-in-progress customization default."
  (should (eq beads-agent-auto-set-in-progress t)))

(ert-deftest beads-agent-test-custom-default-backend ()
  "Test default-backend customization default."
  (should (null beads-agent-default-backend)))

(ert-deftest beads-agent-test-custom-use-worktrees ()
  "Test use-worktrees customization default."
  (should (eq beads-agent-use-worktrees t)))

(ert-deftest beads-agent-test-should-use-worktree-p-true ()
  "Test should-use-worktree-p when setting is t."
  (let ((beads-agent-use-worktrees t))
    (should (eq (beads-agent--should-use-worktree-p "bd-123") t))))

(ert-deftest beads-agent-test-should-use-worktree-p-nil ()
  "Test should-use-worktree-p when setting is nil."
  (let ((beads-agent-use-worktrees nil))
    (should (eq (beads-agent--should-use-worktree-p "bd-123") nil))))

(ert-deftest beads-agent-test-should-use-worktree-p-ask-yes ()
  "Test should-use-worktree-p when setting is ask and user says yes."
  (let ((beads-agent-use-worktrees 'ask))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (should (eq (beads-agent--should-use-worktree-p "bd-123") t)))))

(ert-deftest beads-agent-test-should-use-worktree-p-ask-no ()
  "Test should-use-worktree-p when setting is ask and user says no."
  (let ((beads-agent-use-worktrees 'ask))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (should (eq (beads-agent--should-use-worktree-p "bd-123") nil)))))

(ert-deftest beads-agent-test-custom-worktree-parent ()
  "Test worktree-parent customization default."
  (should (null beads-agent-worktree-parent)))

;;; Tests for Session Worktree Tracking

(ert-deftest beads-agent-test-session-worktree-accessors ()
  "Test session worktree accessors."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-123" "mock" "/main/repo" 'handle "/worktree/bd-123")))
          ;; Test worktree-dir accessor
          (should (equal (beads-agent-session-worktree-dir session)
                         "/worktree/bd-123"))
          ;; Test working-dir accessor (prefers worktree)
          (should (equal (beads-agent-session-working-dir session)
                         "/worktree/bd-123"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-session-no-worktree ()
  "Test session without worktree."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-456" "mock" "/main/repo" 'handle)))
          ;; No worktree
          (should (null (beads-agent-session-worktree-dir session)))
          ;; Working dir falls back to project-dir
          (should (equal (beads-agent-session-working-dir session)
                         "/main/repo"))))
    (beads-agent-test--teardown)))

;;; Tests for Directory-Bound Session Accessors

(ert-deftest beads-agent-test-session-project-dir-accessor ()
  "Test project-dir accessor returns the directory identity."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-123" "mock" "/home/user/projects/myrepo" 'handle)))
          (should (equal (beads-agent-session-project-dir session)
                         "/home/user/projects/myrepo"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-session-current-issue-accessor ()
  "Test current-issue accessor and setter."
  (let ((session (beads-agent-session
                  :id "test#1"
                  :project-dir "/test/project"
                  :backend-name "mock"
                  :started-at "2025-01-01T00:00:00Z")))
    ;; Initially nil
    (should (null (beads-agent-session-current-issue session)))
    ;; Set current issue
    (beads-agent-session-set-current-issue session "bd-001")
    (should (equal (beads-agent-session-current-issue session) "bd-001"))
    ;; Should also add to touched-issues
    (should (member "bd-001" (beads-agent-session-touched-issues session)))))

(ert-deftest beads-agent-test-session-touched-issues-tracking ()
  "Test touched-issues accumulates issues correctly."
  (let ((session (beads-agent-session
                  :id "test#1"
                  :project-dir "/test/project"
                  :backend-name "mock"
                  :started-at "2025-01-01T00:00:00Z")))
    ;; Initially empty
    (should (null (beads-agent-session-touched-issues session)))
    ;; Add first issue
    (should (beads-agent-session-add-touched-issue session "bd-001"))
    (should (equal (beads-agent-session-touched-issues session) '("bd-001")))
    ;; Add second issue
    (should (beads-agent-session-add-touched-issue session "bd-002"))
    (should (member "bd-001" (beads-agent-session-touched-issues session)))
    (should (member "bd-002" (beads-agent-session-touched-issues session)))
    ;; Adding duplicate returns nil
    (should-not (beads-agent-session-add-touched-issue session "bd-001"))))

(ert-deftest beads-agent-test-session-proj-name-accessor ()
  "Test proj-name accessor for project display name."
  (let ((session (beads-agent-session
                  :id "myproject#1"
                  :project-dir "/home/user/myproject"
                  :proj-name "myproject"
                  :backend-name "mock"
                  :started-at "2025-01-01T00:00:00Z")))
    (should (equal (beads-agent-session-project-name session) "myproject"))))

(ert-deftest beads-agent-test-session-instance-number-accessor ()
  "Test instance-number accessor."
  (let ((session (beads-agent-session
                  :id "myproject#3"
                  :project-dir "/test/project"
                  :instance-number 3
                  :backend-name "mock"
                  :started-at "2025-01-01T00:00:00Z")))
    (should (= (beads-agent-session-instance-number session) 3))))

(ert-deftest beads-agent-test-session-issue-id-backward-compat ()
  "Test issue-id accessor for backward compatibility."
  (let ((session (beads-agent-session
                  :id "test#1"
                  :project-dir "/test/project"
                  :issue-id "legacy-issue"
                  :backend-name "mock"
                  :started-at "2025-01-01T00:00:00Z")))
    ;; When current-issue is nil, falls back to issue-id
    (should (equal (beads-agent-session-issue-id session) "legacy-issue"))
    ;; When current-issue is set, returns current-issue
    (beads-agent-session-set-current-issue session "new-issue")
    (should (equal (beads-agent-session-issue-id session) "new-issue"))))

(ert-deftest beads-agent-test-derive-project-name ()
  "Test project name derivation from directory."
  (should (equal (beads-agent--derive-project-name "/home/user/projects/beads.el")
                 "beads.el"))
  (should (equal (beads-agent--derive-project-name "/home/user/my-project/")
                 "my-project"))
  (should (equal (beads-agent--derive-project-name "~/workspace/foo")
                 "foo")))

(ert-deftest beads-agent-test-create-project-session ()
  "Test directory-bound project session creation."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-project-session
                        "/home/user/myproject" "mock" 'handle "bd-001" "Task")))
          ;; Session ID uses project name, not issue
          (should (string-match-p "^myproject#[0-9]+$" (oref session id)))
          ;; Project name is set
          (should (equal (beads-agent-session-project-name session) "myproject"))
          ;; Instance number is set
          (should (= (beads-agent-session-instance-number session) 1))
          ;; current-issue is set from initial-issue
          (should (equal (beads-agent-session-current-issue session) "bd-001"))
          ;; touched-issues contains initial-issue
          (should (member "bd-001" (beads-agent-session-touched-issues session)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-create-project-session-no-initial-issue ()
  "Test project session creation without initial issue."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-project-session
                        "/home/user/myproject" "mock" 'handle)))
          ;; Session ID uses project name
          (should (string-match-p "^myproject#[0-9]+$" (oref session id)))
          ;; No current-issue
          (should (null (beads-agent-session-current-issue session)))
          ;; No touched-issues
          (should (null (beads-agent-session-touched-issues session)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-generate-project-session-id ()
  "Test project session ID generation."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; First session for project
        (let ((id (beads-agent--generate-project-session-id "/home/user/beads.el")))
          (should (equal id "beads.el#1"))))
    (beads-agent-test--teardown)))

;;; Tests for Directory-Bound Buffer Naming

(ert-deftest beads-agent-test-generate-project-buffer-name ()
  "Test directory-bound buffer name generation with type."
  (should (equal (beads-agent--generate-project-buffer-name "beads.el" "Task" 1)
                 "*beads-agent[beads.el][Task#1]*"))
  (should (equal (beads-agent--generate-project-buffer-name "my-project" "Plan" 3)
                 "*beads-agent[my-project][Plan#3]*"))
  (should (equal (beads-agent--generate-project-buffer-name "test" "Review" 2)
                 "*beads-agent[test][Review#2]*")))

(ert-deftest beads-agent-test-generate-buffer-name-for-project-session ()
  "Test buffer name generation from session object."
  (beads-agent--reset-typed-instance-counters)
  (unwind-protect
      (progn
        ;; Session with explicit type
        (let ((session (beads-agent-session
                        :id "beads.el#1"
                        :project-dir "/home/user/beads.el"
                        :proj-name "beads.el"
                        :instance-number 1
                        :agent-type-name "Task"
                        :backend-name "mock"
                        :started-at "2025-01-01T00:00:00Z")))
          (should (equal (beads-agent--generate-buffer-name-for-project-session session)
                         "*beads-agent[beads.el][Task#1]*")))
        ;; Session without type defaults to "Agent" - separate counter from Task
        (let ((session (beads-agent-session
                        :id "beads.el#2"
                        :project-dir "/home/user/beads.el"
                        :proj-name "beads.el"
                        :instance-number 2
                        :backend-name "mock"
                        :started-at "2025-01-01T00:00:00Z")))
          ;; Agent#1 because (beads.el, Agent) counter starts fresh
          (should (equal (beads-agent--generate-buffer-name-for-project-session session)
                         "*beads-agent[beads.el][Agent#1]*"))))
    (beads-agent--reset-typed-instance-counters)))

(ert-deftest beads-agent-test-generate-buffer-name-for-project-session-idempotent ()
  "Test that buffer name generation is idempotent when session has buffer."
  (beads-agent--reset-typed-instance-counters)
  (unwind-protect
      (let* ((buf (generate-new-buffer "*test-idempotent*"))
             (session (beads-agent-session
                       :id "beads.el#1"
                       :project-dir "/home/user/beads.el"
                       :proj-name "beads.el"
                       :instance-number 1
                       :agent-type-name "Task"
                       :backend-name "mock"
                       :started-at "2025-01-01T00:00:00Z")))
        (unwind-protect
            (progn
              ;; First call generates name and increments counter
              (let ((first-name (beads-agent--generate-buffer-name-for-project-session session)))
                (should (equal first-name "*beads-agent[beads.el][Task#1]*"))
                ;; Store buffer in session
                (with-current-buffer buf
                  (rename-buffer first-name t))
                (oset session buffer buf)
                ;; Second call returns existing buffer name (idempotent)
                (let ((second-name (beads-agent--generate-buffer-name-for-project-session session)))
                  (should (equal second-name first-name))
                  ;; Counter should NOT have incremented
                  (should (= (beads-agent--peek-typed-instance-number "beads.el" "Task") 2)))))
          (when (buffer-live-p buf)
            (kill-buffer buf))))
    (beads-agent--reset-typed-instance-counters)))

(ert-deftest beads-agent-test-rename-and-store-buffer-idempotent ()
  "Test that rename-and-store-buffer is idempotent."
  (beads-agent--reset-typed-instance-counters)
  (unwind-protect
      (let* ((buf (generate-new-buffer "*test-rename-idempotent*"))
             (session (beads-agent-session
                       :id "beads.el#1"
                       :project-dir "/tmp/test-project"
                       :proj-name "beads.el"
                       :instance-number 1
                       :agent-type-name "Review"
                       :backend-name "mock"
                       :started-at "2025-01-01T00:00:00Z")))
        (unwind-protect
            (progn
              ;; First call renames and stores
              (beads-agent--rename-and-store-buffer session buf)
              (should (equal (buffer-name buf) "*beads-agent[beads.el][Review#1]*"))
              (should (eq (oref session buffer) buf))
              ;; Second call with different buffer does nothing (idempotent)
              (let ((buf2 (generate-new-buffer "*another-buffer*")))
                (unwind-protect
                    (progn
                      (beads-agent--rename-and-store-buffer session buf2)
                      ;; Session still points to original buffer
                      (should (eq (oref session buffer) buf))
                      ;; Counter should NOT have incremented
                      (should (= (beads-agent--peek-typed-instance-number "beads.el" "Review") 2)))
                  (when (buffer-live-p buf2)
                    (kill-buffer buf2)))))
          (when (buffer-live-p buf)
            (kill-buffer buf))))
    (beads-agent--reset-typed-instance-counters)))

(ert-deftest beads-agent-test-parse-project-buffer-name ()
  "Test parsing directory-bound buffer names."
  (let ((parsed (beads-agent--parse-project-buffer-name
                 "*beads-agent[beads.el][Task#1]*")))
    (should (equal (plist-get parsed :project-name) "beads.el"))
    (should (equal (plist-get parsed :type-name) "Task"))
    (should (= (plist-get parsed :instance-n) 1)))
  ;; Another valid format
  (let ((parsed (beads-agent--parse-project-buffer-name
                 "*beads-agent[my-project][Plan#3]*")))
    (should (equal (plist-get parsed :project-name) "my-project"))
    (should (equal (plist-get parsed :type-name) "Plan"))
    (should (= (plist-get parsed :instance-n) 3)))
  ;; Invalid format returns nil
  (should (null (beads-agent--parse-project-buffer-name "invalid")))
  (should (null (beads-agent--parse-project-buffer-name "*scratch*"))))

(ert-deftest beads-agent-test-project-buffer-name-p ()
  "Test predicate for directory-bound buffer names."
  ;; Directory-bound format matches (now includes type)
  (should (beads-agent--project-buffer-name-p "*beads-agent[beads.el][Task#1]*"))
  (should (beads-agent--project-buffer-name-p "*beads-agent[my-project][Plan#2]*"))
  ;; Legacy format also matches (same syntax: [NAME][TYPE#N])
  (should (beads-agent--project-buffer-name-p "*beads-agent[bd-42][Task#1]*"))
  ;; Invalid names don't match
  (should-not (beads-agent--project-buffer-name-p nil))
  (should-not (beads-agent--project-buffer-name-p "*scratch*")))

(ert-deftest beads-agent-test-buffer-name-p-both-formats ()
  "Test that buffer-name-p matches both directory-bound and legacy formats.
Both formats are now syntactically identical: [NAME][TYPE#N]."
  ;; Directory-bound format matches (with type)
  (should (beads-agent--buffer-name-p "*beads-agent[beads.el][Task#1]*"))
  (should (beads-agent--buffer-name-p "*beads-agent[my-project][Plan#2]*"))
  ;; Legacy format matches (same syntax)
  (should (beads-agent--buffer-name-p "*beads-agent[bd-42][Task#1]*"))
  (should (beads-agent--buffer-name-p "*beads-agent[beads.el-xrrt][Review#3]*"))
  ;; Invalid names don't match
  (should-not (beads-agent--buffer-name-p nil))
  (should-not (beads-agent--buffer-name-p "*scratch*"))
  (should-not (beads-agent--buffer-name-p "*beads-agent[test][]*")))

;;; Tests for Focus-Issue Commands

(ert-deftest beads-agent-test-focus-issue-updates-session ()
  "Test that focus-issue command updates session correctly."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Create a project session
        (let ((session (beads-agent--create-project-session
                        "/home/user/myproject" "mock" 'handle)))
          ;; Add to mock storage
          (push (list (oref session id) 'handle session)
                beads-agent-test--sesman-sessions)
          ;; Focus on an issue
          (cl-letf (((symbol-function 'beads-git-find-project-root)
                     (lambda () "/home/user/myproject")))
            (beads-agent-focus-issue "bd-001")
            ;; Verify current-issue is set
            (should (equal (beads-agent-session-current-issue session) "bd-001"))
            ;; Verify touched-issues contains the issue
            (should (member "bd-001" (beads-agent-session-touched-issues session))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-clear-focus-clears-current-issue ()
  "Test that clear-focus command clears current-issue."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Create a project session with initial issue
        (let ((session (beads-agent--create-project-session
                        "/home/user/myproject" "mock" 'handle "bd-001")))
          ;; Add to mock storage
          (push (list (oref session id) 'handle session)
                beads-agent-test--sesman-sessions)
          ;; Clear focus
          (cl-letf (((symbol-function 'beads-git-find-project-root)
                     (lambda () "/home/user/myproject")))
            (beads-agent-clear-focus)
            ;; Verify current-issue is nil
            (should (null (beads-agent-session-current-issue session)))
            ;; Verify touched-issues still contains the original issue
            (should (member "bd-001" (beads-agent-session-touched-issues session))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-show-touched-reports-issues ()
  "Test that show-touched command reports touched issues."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Create a project session with touched issues
        (let ((session (beads-agent--create-project-session
                        "/home/user/myproject" "mock" 'handle "bd-001")))
          ;; Add more touched issues
          (beads-agent-session-add-touched-issue session "bd-002")
          (beads-agent-session-add-touched-issue session "bd-003")
          ;; Add to mock storage
          (push (list (oref session id) 'handle session)
                beads-agent-test--sesman-sessions)
          ;; Show touched - verify it doesn't error
          (cl-letf (((symbol-function 'beads-git-find-project-root)
                     (lambda () "/home/user/myproject")))
            ;; Capture the message output
            (let ((messages nil))
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (setq messages (apply #'format format-string args)))))
                (beads-agent-show-touched)
                ;; Verify message contains touched issues
                (should (string-match-p "bd-001" messages))
                (should (string-match-p "bd-002" messages)))))))
    (beads-agent-test--teardown)))

;;; Integration Tests for Directory-Bound Agents

(ert-deftest beads-agent-test-session-keyed-by-project ()
  "Session identity is project directory, not issue."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-project-session
                        "/home/user/beads.el" "mock" 'handle "bd-42")))
          ;; Session ID is project-name#N
          (should (string-match-p "^beads.el#" (oref session id)))
          ;; Project directory is set correctly
          (should (string-suffix-p "beads.el" (oref session project-dir)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-multiple-instances-same-project ()
  "Multiple starts in same project increment instance number."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Start first session
        (let ((session1 (beads-agent--create-project-session
                         "/home/user/beads.el" "mock" 'handle "bd-42")))
          ;; Add to mock storage so second session sees it
          (push (list (oref session1 id) 'handle session1)
                beads-agent-test--sesman-sessions)
          ;; Start second session in same project
          (let ((session2 (beads-agent--create-project-session
                           "/home/user/beads.el" "mock" 'handle "bd-43")))
            ;; Instance numbers should be different
            (should (= 1 (oref session1 instance-number)))
            (should (= 2 (oref session2 instance-number)))
            ;; Session IDs should reflect this
            (should (equal "beads.el#1" (oref session1 id)))
            (should (equal "beads.el#2" (oref session2 id))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-focus-switch-tracks-all-issues ()
  "Agent tracks all issues it has focused on."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Start with focus on bd-42
        (let ((session (beads-agent--create-project-session
                        "/home/user/beads.el" "mock" 'handle "bd-42")))
          ;; Initial state
          (should (equal "bd-42" (beads-agent-session-current-issue session)))
          (should (member "bd-42" (beads-agent-session-touched-issues session)))
          ;; Switch to bd-43
          (beads-agent-session-set-current-issue session "bd-43")
          ;; Current changed
          (should (equal "bd-43" (beads-agent-session-current-issue session)))
          ;; Both tracked
          (should (member "bd-42" (beads-agent-session-touched-issues session)))
          (should (member "bd-43" (beads-agent-session-touched-issues session)))
          ;; Switch to bd-44
          (beads-agent-session-set-current-issue session "bd-44")
          ;; All three tracked
          (should (= 3 (length (beads-agent-session-touched-issues session))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-project-sessions-isolated ()
  "Different projects have independent sessions."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Session in project A
        (let ((session-a (beads-agent--create-project-session
                          "/home/user/project-a" "mock" 'handle "bd-1")))
          (push (list (oref session-a id) 'handle session-a)
                beads-agent-test--sesman-sessions)
          ;; Session in project B
          (let ((session-b (beads-agent--create-project-session
                            "/home/user/project-b" "mock" 'handle "bd-2")))
            ;; Both should have instance #1 (independent)
            (should (= 1 (oref session-a instance-number)))
            (should (= 1 (oref session-b instance-number)))
            ;; Different project names
            (should (equal "project-a" (oref session-a proj-name)))
            (should (equal "project-b" (oref session-b proj-name))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-buffer-name-uses-project ()
  "Buffer names use project name and agent type, not issue ID."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Test with explicit agent type
        (let ((session (beads-agent--create-project-session
                        "/home/user/beads.el" "mock" 'handle "bd-42" "Task")))
          (let ((buffer-name (beads-agent--generate-buffer-name-for-project-session session)))
            ;; Buffer name contains project name
            (should (string-match-p "beads.el" buffer-name))
            ;; Buffer name contains agent type
            (should (string-match-p "Task" buffer-name))
            ;; Buffer name does NOT contain issue ID
            (should-not (string-match-p "bd-42" buffer-name))
            ;; Format is correct: *beads-agent[PROJECT][TYPE#N]*
            (should (equal "*beads-agent[beads.el][Task#1]*" buffer-name))))
        ;; Test without agent type (defaults to "Agent")
        (let ((session (beads-agent--create-project-session
                        "/home/user/another" "mock" 'handle "bd-99")))
          (let ((buffer-name (beads-agent--generate-buffer-name-for-project-session session)))
            (should (equal "*beads-agent[another][Agent#1]*" buffer-name)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-buffer-name-uses-worktree-when-available ()
  "Buffer names use worktree directory name when worktree-dir is set."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Create session with worktree-dir set
        (let ((session (beads-agent-session
                        :id "beads.el#1"
                        :project-dir "/home/user/beads.el"
                        :proj-name "beads.el"
                        :worktree-dir "/home/user/beads.el-42"
                        :instance-number 1
                        :agent-type-name "Task"
                        :backend-name "mock"
                        :started-at "2025-01-01T00:00:00Z")))
          (let ((buffer-name (beads-agent--generate-buffer-name-for-project-session session)))
            ;; Buffer name should contain worktree name, not project name
            (should (string-match-p "beads.el-42" buffer-name))
            ;; Format is correct: *beads-agent[WORKTREE][TYPE#N]*
            (should (equal "*beads-agent[beads.el-42][Task#1]*" buffer-name))))
        ;; Session without worktree-dir still uses project name
        (let ((session (beads-agent-session
                        :id "beads.el#2"
                        :project-dir "/home/user/beads.el"
                        :proj-name "beads.el"
                        :instance-number 2
                        :agent-type-name "Review"
                        :backend-name "mock"
                        :started-at "2025-01-01T00:00:00Z")))
          (let ((buffer-name (beads-agent--generate-buffer-name-for-project-session session)))
            (should (equal "*beads-agent[beads.el][Review#1]*" buffer-name))))
        ;; Session with empty string worktree-dir falls back to project name
        (let ((session (beads-agent-session
                        :id "beads.el#3"
                        :project-dir "/home/user/beads.el"
                        :proj-name "beads.el"
                        :worktree-dir ""
                        :instance-number 3
                        :agent-type-name "Plan"
                        :backend-name "mock"
                        :started-at "2025-01-01T00:00:00Z")))
          (let ((buffer-name (beads-agent--generate-buffer-name-for-project-session session)))
            (should (equal "*beads-agent[beads.el][Plan#1]*" buffer-name)))))
    (beads-agent-test--teardown)))

;;; Tests for Worktree Path Calculation

(ert-deftest beads-agent-test-worktree-path-for-issue ()
  "Test worktree path calculation."
  (let ((beads-agent-worktree-parent nil))
    ;; Mock main repo root
    (cl-letf (((symbol-function 'beads-git-main-repo-root)
               (lambda () "/home/user/projects/myrepo")))
      ;; Worktree should be sibling to repo
      (should (equal (beads-git-worktree-path-for-issue "bd-123")
                     "/home/user/projects/bd-123")))))

(ert-deftest beads-agent-test-worktree-path-custom-parent ()
  "Test worktree path calculation with custom parent."
  (let ((beads-agent-worktree-parent "/tmp/worktrees"))
    ;; Mock main repo root
    (cl-letf (((symbol-function 'beads-git-main-repo-root)
               (lambda () "/home/user/projects/myrepo")))
      ;; Worktree should be under custom parent
      (should (equal (beads-git-worktree-path-for-issue "bd-456")
                     "/tmp/worktrees/bd-456")))))

;;; =========================================================================
;;; Agent Lifecycle Integration Tests
;;; =========================================================================

;; These tests verify the complete agent lifecycle as described in issue
;; beads.el-84gn:
;; 1. Start agents on multiple issues
;; 2. Verify sessions exist for each
;; 3. Re-invoke start (should recognize existing session)
;; 4. Stop all sessions
;; 5. Verify sessions are gone
;; 6. Restart agents
;; 7. Verify sessions exist again

(ert-deftest beads-agent-lifecycle-start-multiple-agents ()
  "Test starting agents on multiple issues."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((issues '("bd-001" "bd-002" "bd-003")))
          ;; Start agents on all three issues
          (dolist (issue-id issues)
            (let ((session (beads-agent--create-session
                            issue-id "mock" "/tmp/project" 'mock-handle)))
              ;; Mark as active in mock backend
              (push (oref session id)
                    (oref beads-agent-test--mock-backend active-sessions))))
          ;; Verify all three sessions exist
          (should (= (length (beads-agent--get-all-sessions)) 3))
          ;; Verify each issue has a session
          (dolist (issue-id issues)
            (should (beads-agent--get-sessions-for-issue issue-id)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-lifecycle-detect-existing-session ()
  "Test that existing sessions are detected before starting new ones.
When an agent session already exists for an issue, the system should
detect it rather than blindly starting a new session."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((issue-id "bd-123"))
          ;; Create initial session
          (let ((session (beads-agent--create-session
                          issue-id "mock" "/tmp/project" 'mock-handle)))
            (push (oref session id)
                  (oref beads-agent-test--mock-backend active-sessions)))
          ;; Verify session exists
          (let ((existing (beads-agent--get-sessions-for-issue issue-id)))
            (should existing)
            (should (= (length existing) 1)))
          ;; Try to get sessions again (simulating what beads-agent-start-at-point does)
          (let ((sessions-again (beads-agent--get-sessions-for-issue issue-id)))
            ;; Should still find the existing session
            (should sessions-again)
            (should (= (length sessions-again) 1)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-lifecycle-stop-all-sessions ()
  "Test stopping all agent sessions."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((issues '("bd-001" "bd-002" "bd-003")))
          ;; Start agents on all three issues
          (dolist (issue-id issues)
            (let ((session (beads-agent--create-session
                            issue-id "mock" "/tmp/project" 'mock-handle)))
              (push (oref session id)
                    (oref beads-agent-test--mock-backend active-sessions))))
          ;; Verify all three sessions exist
          (should (= (length (beads-agent--get-all-sessions)) 3))
          ;; Stop all sessions
          (let ((all-sessions (beads-agent--get-all-sessions)))
            (dolist (session all-sessions)
              (beads-agent-backend-stop beads-agent-test--mock-backend session)
              (beads-agent--destroy-session (oref session id))))
          ;; Verify all sessions are gone
          (should (= (length (beads-agent--get-all-sessions)) 0))
          ;; Verify no sessions for any issue
          (dolist (issue-id issues)
            (should (null (beads-agent--get-sessions-for-issue issue-id))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-lifecycle-restart-after-stop ()
  "Test that agents can be restarted after being stopped."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((issues '("bd-001" "bd-002" "bd-003")))
          ;; Phase 1: Start agents
          (dolist (issue-id issues)
            (let ((session (beads-agent--create-session
                            issue-id "mock" "/tmp/project" 'mock-handle)))
              (push (oref session id)
                    (oref beads-agent-test--mock-backend active-sessions))))
          (should (= (length (beads-agent--get-all-sessions)) 3))

          ;; Phase 2: Stop all agents
          (let ((all-sessions (beads-agent--get-all-sessions)))
            (dolist (session all-sessions)
              (beads-agent-backend-stop beads-agent-test--mock-backend session)
              (beads-agent--destroy-session (oref session id))))
          (should (= (length (beads-agent--get-all-sessions)) 0))

          ;; Phase 3: Restart agents on all issues
          (dolist (issue-id issues)
            (let ((session (beads-agent--create-session
                            issue-id "mock" "/tmp/project" 'mock-handle-2)))
              (push (oref session id)
                    (oref beads-agent-test--mock-backend active-sessions))))
          ;; Verify all three sessions exist again
          (should (= (length (beads-agent--get-all-sessions)) 3))
          ;; Verify each issue has exactly one session
          (dolist (issue-id issues)
            (let ((sessions (beads-agent--get-sessions-for-issue issue-id)))
              (should sessions)
              (should (= (length sessions) 1))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-lifecycle-full-cycle ()
  "Full lifecycle test: create issues, start/stop/restart agents.
This test exercises the complete agent lifecycle workflow:
1. Create sessions for 3 issues
2. Verify sessions are tracked properly
3. Simulate 'jump to existing' by checking existing sessions
4. Stop all sessions
5. Verify cleanup
6. Restart and verify again."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((issues '("test-001" "test-002" "test-003"))
              (session-ids nil))

          ;; === Step 1: Start agents on all issues ===
          (dolist (issue-id issues)
            (let ((session (beads-agent--create-session
                            issue-id "mock" "/tmp/project" 'handle)))
              (push (oref session id) session-ids)
              (push (oref session id)
                    (oref beads-agent-test--mock-backend active-sessions))))
          (setq session-ids (nreverse session-ids))

          ;; Verify: 3 sessions total
          (should (= (length (beads-agent--get-all-sessions)) 3))

          ;; === Step 2: Verify each issue has exactly one session ===
          (dolist (issue-id issues)
            (let ((sessions (beads-agent--get-sessions-for-issue issue-id)))
              (should (= (length sessions) 1))))

          ;; === Step 3: Simulate 'A' key press on issue with existing session ===
          ;; The behavior of beads-agent-start-at-point is to check for
          ;; existing sessions first and jump to them
          (let* ((first-issue (car issues))
                 (existing (beads-agent--get-sessions-for-issue first-issue)))
            ;; Should find existing session
            (should existing)
            ;; In real code, this would trigger beads-agent-jump instead
            ;; of starting a new session. Verify the detection works.
            (should (= (length existing) 1)))

          ;; === Step 4: Stop all sessions ===
          (let ((all-sessions (beads-agent--get-all-sessions)))
            (dolist (session all-sessions)
              (beads-agent-backend-stop beads-agent-test--mock-backend session)
              (beads-agent--destroy-session (oref session id))))

          ;; === Step 5: Verify all sessions are gone ===
          (should (= (length (beads-agent--get-all-sessions)) 0))
          (dolist (issue-id issues)
            (should (null (beads-agent--get-sessions-for-issue issue-id))))

          ;; === Step 6: Restart agents ===
          (setq session-ids nil)
          (dolist (issue-id issues)
            (let ((session (beads-agent--create-session
                            issue-id "mock" "/tmp/project" 'new-handle)))
              (push (oref session id) session-ids)
              (push (oref session id)
                    (oref beads-agent-test--mock-backend active-sessions))))

          ;; === Step 7: Verify sessions are back ===
          (should (= (length (beads-agent--get-all-sessions)) 3))
          (dolist (issue-id issues)
            (let ((sessions (beads-agent--get-sessions-for-issue issue-id)))
              (should sessions)
              (should (= (length sessions) 1))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-lifecycle-start-at-point-behavior ()
  "Test beads-agent-start-at-point session detection logic.
When invoked on an issue:
- If no session exists: should allow starting one
- If session exists: should detect it (real code would jump instead)"
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((issue-id "bd-test-123"))
          ;; Initially no session exists
          (should (null (beads-agent--get-sessions-for-issue issue-id)))

          ;; Start a session
          (let ((session (beads-agent--create-session
                          issue-id "mock" "/tmp/project" 'handle)))
            (push (oref session id)
                  (oref beads-agent-test--mock-backend active-sessions)))

          ;; Now session should be detected
          (let ((sessions (beads-agent--get-sessions-for-issue issue-id)))
            (should sessions)
            ;; This is the check that beads-agent-start-at-point does
            ;; to decide whether to jump vs start
            (should (= (length sessions) 1)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-lifecycle-multiple-sessions-same-issue ()
  "Test handling of multiple sessions for the same issue.
While normally not desired, the system should handle this gracefully."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((issue-id "bd-multi-session"))
          ;; Create two sessions for the same issue
          (let ((s1 (beads-agent--create-session
                     issue-id "mock" "/tmp/project" 'handle1))
                (s2 (beads-agent--create-session
                     issue-id "mock" "/tmp/project" 'handle2)))
            (push (oref s1 id)
                  (oref beads-agent-test--mock-backend active-sessions))
            (push (oref s2 id)
                  (oref beads-agent-test--mock-backend active-sessions)))

          ;; Should have 2 total sessions
          (should (= (length (beads-agent--get-all-sessions)) 2))

          ;; Both should be associated with the same issue
          (let ((sessions (beads-agent--get-sessions-for-issue issue-id)))
            (should (= (length sessions) 2)))))
    (beads-agent-test--teardown)))

;;; =========================================================================
;;; Async Stop Tests
;;; =========================================================================

(ert-deftest beads-agent-test-backend-stop-async ()
  "Test async stop via mock backend."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-async" "mock" "/tmp" 'handle))
              (callback-called nil))
          (beads-agent-backend-stop-async
           beads-agent-test--mock-backend
           session
           (lambda () (setq callback-called t)))
          ;; Callback should have been called (mock is sync)
          (should callback-called)
          ;; Stop should have been called
          (should (oref beads-agent-test--mock-backend stop-called))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-stop-async-single ()
  "Test beads-agent-stop-async for single session."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let* ((session (beads-agent--create-session
                         "bd-single" "mock" "/tmp" 'handle))
               (session-id (oref session id))
               (callback-called nil))
          (push session-id
                (oref beads-agent-test--mock-backend active-sessions))
          ;; Should have 1 session
          (should (= (length (beads-agent--get-all-sessions)) 1))
          ;; Stop async
          (beads-agent-stop-async
           session-id
           (lambda () (setq callback-called t)))
          ;; Callback should have been called
          (should callback-called)
          ;; Session should be gone
          (should (= (length (beads-agent--get-all-sessions)) 0))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-stop-async-nonexistent ()
  "Test beads-agent-stop-async with nonexistent session."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((callback-called nil))
          ;; Stop nonexistent session
          (beads-agent-stop-async
           "nonexistent-session-id"
           (lambda () (setq callback-called t)))
          ;; Callback should still be called
          (should callback-called)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-stop-all-async ()
  "Test beads-agent-stop-all-async stops multiple sessions."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((issues '("bd-a" "bd-b" "bd-c"))
              (callback-called nil))
          ;; Create sessions
          (dolist (issue-id issues)
            (let ((session (beads-agent--create-session
                            issue-id "mock" "/tmp" 'handle)))
              (push (oref session id)
                    (oref beads-agent-test--mock-backend active-sessions))))
          ;; Should have 3 sessions
          (should (= (length (beads-agent--get-all-sessions)) 3))
          ;; Stop all async
          (beads-agent-stop-all-async
           (lambda () (setq callback-called t)))
          ;; Callback should have been called
          (should callback-called)
          ;; All sessions should be gone
          (should (= (length (beads-agent--get-all-sessions)) 0))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-stop-all-async-empty ()
  "Test beads-agent-stop-all-async with no sessions."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((callback-called nil))
          ;; No sessions
          (should (= (length (beads-agent--get-all-sessions)) 0))
          ;; Stop all async (should work with empty list)
          (beads-agent-stop-all-async
           (lambda () (setq callback-called t)))
          ;; Callback should have been called
          (should callback-called)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-stop-all-async-lifecycle ()
  "Test full lifecycle with async stop."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((issues '("test-a" "test-b" "test-c"))
              (phase1-done nil)
              (phase2-done nil))
          ;; Phase 1: Start sessions
          (dolist (issue-id issues)
            (let ((session (beads-agent--create-session
                            issue-id "mock" "/tmp" 'handle)))
              (push (oref session id)
                    (oref beads-agent-test--mock-backend active-sessions))))
          (should (= (length (beads-agent--get-all-sessions)) 3))
          (setq phase1-done t)

          ;; Phase 2: Stop all async
          (beads-agent-stop-all-async
           (lambda () (setq phase2-done t)))
          (should phase2-done)
          (should (= (length (beads-agent--get-all-sessions)) 0))

          ;; Phase 3: Restart sessions
          (dolist (issue-id issues)
            (let ((session (beads-agent--create-session
                            issue-id "mock" "/tmp" 'new-handle)))
              (push (oref session id)
                    (oref beads-agent-test--mock-backend active-sessions))))
          (should (= (length (beads-agent--get-all-sessions)) 3))))
    (beads-agent-test--teardown)))

;;; =========================================================================
;;; Backend Selection Tests
;;; =========================================================================

(ert-deftest beads-agent-test-select-backend-uses-default ()
  "Test that select-backend uses default when set and available."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend "mock")
            (prompt-called nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (setq prompt-called t)
                     "mock")))
          (let ((backend (beads-agent--select-backend)))
            ;; Should return mock backend
            (should (equal (oref backend name) "mock"))
            ;; Should NOT have prompted - uses default
            (should (null prompt-called)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-select-backend-prompts-without-default ()
  "Test that select-backend prompts when no default set."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend nil)
            (prompt-called nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (setq prompt-called t)
                     "mock")))
          (let ((backend (beads-agent--select-backend)))
            ;; Should return mock backend
            (should (equal (oref backend name) "mock"))
            ;; Should have prompted - no default set
            (should prompt-called))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-select-backend-prompts-for-single ()
  "Test that select-backend prompts even with single backend available.
Previously it auto-selected when only one was available, but the new
behavior is to always prompt unless a default is configured."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend nil)
            (prompt-called nil))
        ;; Only one backend is registered (mock)
        (should (= (length (beads-agent--get-available-backends)) 1))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (setq prompt-called t)
                     "mock")))
          (beads-agent--select-backend)
          ;; Should still prompt even though only one backend
          (should prompt-called)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-select-backend-unavailable-default ()
  "Test that select-backend prompts if default is unavailable."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend "nonexistent")
            (prompt-called nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (setq prompt-called t)
                     "mock")))
          (let ((backend (beads-agent--select-backend)))
            ;; Should return mock backend (from prompt)
            (should (equal (oref backend name) "mock"))
            ;; Should have prompted because default doesn't exist
            (should prompt-called))))
    (beads-agent-test--teardown)))

;;; =========================================================================
;;; Switch Backend Tests
;;; =========================================================================

(ert-deftest beads-agent-test-switch-backend-function-exists ()
  "Test that beads-agent-switch-backend is defined."
  (should (fboundp 'beads-agent-switch-backend)))

(ert-deftest beads-agent-test-switch-backend-sets-default ()
  "Test that switch-backend sets beads-agent-default-backend."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "mock")))
          (beads-agent-switch-backend)
          ;; Should have set the default
          (should (equal beads-agent-default-backend "mock"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-switch-backend-shows-current ()
  "Test that switch-backend shows current backend in prompt."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend "existing")
            (prompt-seen nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt &rest _)
                     (setq prompt-seen prompt)
                     "mock")))
          (beads-agent-switch-backend)
          ;; Prompt should show current backend
          (should (string-match-p "current: existing" prompt-seen))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-switch-backend-save-option ()
  "Test that switch-backend can save to customize."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend nil)
            (customize-called nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) "mock"))
                  ((symbol-function 'customize-save-variable)
                   (lambda (var val)
                     (setq customize-called (list var val)))))
          ;; Call with save=t
          (beads-agent-switch-backend t)
          ;; Should have called customize-save-variable
          (should customize-called)
          (should (eq (car customize-called) 'beads-agent-default-backend))
          (should (equal (cadr customize-called) "mock"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-switch-backend-no-backends ()
  "Test that switch-backend errors with no backends available."
  (beads-agent-test--setup)
  (unwind-protect
      (progn
        ;; Clear backends
        (setq beads-agent--backends nil)
        (should-error (beads-agent-switch-backend) :type 'user-error))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-switch-backend-suffix-exists ()
  "Test that beads-agent--switch-backend-suffix is defined."
  (should (fboundp 'beads-agent--switch-backend-suffix))
  (should (get 'beads-agent--switch-backend-suffix 'transient--suffix)))

;;; =========================================================================
;;; Transient Menu Tests
;;; =========================================================================

(ert-deftest beads-agent-test-transient-prefix-defined ()
  "Test that beads-agent transient prefix is defined."
  (should (fboundp 'beads-agent))
  (should (get 'beads-agent 'transient--prefix)))

(ert-deftest beads-agent-test-start-menu-prefix-defined ()
  "Test that beads-agent-start-menu transient prefix is defined."
  (should (fboundp 'beads-agent-start-menu))
  (should (get 'beads-agent-start-menu 'transient--prefix)))

(ert-deftest beads-agent-test-start-menu-infixes-defined ()
  "Test that beads-agent-start-menu infixes are defined."
  ;; Issue ID infix
  (should (fboundp 'beads-agent-start--infix-issue-id))
  (should (get 'beads-agent-start--infix-issue-id 'transient--suffix))
  ;; Backend infix
  (should (fboundp 'beads-agent-start--infix-backend))
  (should (get 'beads-agent-start--infix-backend 'transient--suffix)))

(ert-deftest beads-agent-test-start-menu-suffixes-defined ()
  "Test that beads-agent-start-menu suffixes are defined."
  ;; Execute suffix
  (should (fboundp 'beads-agent-start--execute))
  (should (get 'beads-agent-start--execute 'transient--suffix))
  ;; Preview suffix
  (should (fboundp 'beads-agent-start--preview))
  (should (get 'beads-agent-start--preview 'transient--suffix))
  ;; Reset suffix
  (should (fboundp 'beads-agent-start--reset))
  (should (get 'beads-agent-start--reset 'transient--suffix)))

(ert-deftest beads-agent-test-start-format-header ()
  "Test format header for start menu."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-agent--detect-issue-id)
                 (lambda () nil)))
        (let ((header (beads-agent-start--format-header)))
          (should (stringp header))
          (should (string-match-p "Start AI Agent" header))
          (should (string-match-p "1 backend" header))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-start-format-header-with-context ()
  "Test format header for start menu with detected context."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-agent--detect-issue-id)
                 (lambda () "bd-42")))
        (let ((header (beads-agent-start--format-header)))
          (should (stringp header))
          (should (string-match-p "context: bd-42" header))))
    (beads-agent-test--teardown)))

;;; =========================================================================
;;; Per-Issue Agent Menu Tests
;;; =========================================================================

(ert-deftest beads-agent-test-issue-menu-prefix-defined ()
  "Test that beads-agent-issue transient prefix is defined."
  (should (fboundp 'beads-agent-issue))
  (should (get 'beads-agent-issue 'transient--prefix)))

(ert-deftest beads-agent-test-session-display-name-format ()
  "Test session display name format."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-123" "mock" "/tmp" 'handle)))
          (let ((display-name (beads-agent--session-display-name session)))
            ;; Should match "Type#N (backend)" format
            ;; Default type is "Agent" when not specified
            (should (stringp display-name))
            (should (string-match-p "^Agent#[0-9]+ (mock)$" display-name)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-issue-format-header ()
  "Test per-issue menu header format."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Set up the context
        (setq beads-agent-issue--current-issue-id "bd-test")
        ;; Create some sessions for the issue
        (beads-agent--create-session "bd-test" "mock" "/tmp" 'h1)
        (beads-agent--create-session "bd-test" "mock" "/tmp" 'h2)
        (let ((header (beads-agent-issue--format-header)))
          (should (stringp header))
          (should (string-match-p "bd-test" header))
          (should (string-match-p "2 active" header))))
    (setq beads-agent-issue--current-issue-id nil)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-issue-get-sessions-sorted ()
  "Test that per-issue sessions are returned sorted by number."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (setq beads-agent-issue--current-issue-id "bd-sorted")
        ;; Create sessions (they get sequential numbers)
        (beads-agent--create-session "bd-sorted" "mock" "/tmp" 'h1)
        (beads-agent--create-session "bd-sorted" "mock" "/tmp" 'h2)
        (beads-agent--create-session "bd-sorted" "mock" "/tmp" 'h3)
        (let ((sessions (beads-agent-issue--get-sessions)))
          (should (= (length sessions) 3))
          ;; Verify sorted by session number
          (let ((nums (mapcar (lambda (s)
                                (beads-agent--session-number-from-id (oref s id)))
                              sessions)))
            (should (equal nums '(1 2 3))))))
    (setq beads-agent-issue--current-issue-id nil)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-issue-setup-agents-empty ()
  "Test setup-agents returns placeholder when no agents."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (setq beads-agent-issue--current-issue-id "bd-empty")
        ;; No sessions for this issue
        (let ((suffixes (beads-agent-issue--setup-agents)))
          ;; Should return a vector with placeholder
          (should (vectorp suffixes))
          (should (= (length suffixes) 1))))
    (setq beads-agent-issue--current-issue-id nil)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-issue-setup-agents-with-sessions ()
  "Test setup-agents returns jump suffixes for each agent."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (setq beads-agent-issue--current-issue-id "bd-agents")
        ;; Create 2 sessions
        (beads-agent--create-session "bd-agents" "mock" "/tmp" 'h1)
        (beads-agent--create-session "bd-agents" "mock" "/tmp" 'h2)
        (let ((suffixes (beads-agent-issue--setup-agents)))
          ;; Should return a vector with 2 jump suffixes
          (should (vectorp suffixes))
          (should (= (length suffixes) 2))))
    (setq beads-agent-issue--current-issue-id nil)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-issue-make-jump-suffix ()
  "Test that jump suffix is created with correct key and description."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-jump" "mock" "/tmp" 'handle)))
          (let ((suffix (beads-agent-issue--make-jump-suffix session 1)))
            ;; Suffix should be (key desc lambda)
            (should (listp suffix))
            (should (equal (car suffix) "j1"))  ; j1 for index 1
            (should (stringp (cadr suffix)))    ; description
            (should (functionp (caddr suffix))))))
    (beads-agent-test--teardown)))

;;; =========================================================================
;;; Context-Sensitive Start Tests (beads-agent-start-at-point)
;;; =========================================================================

(ert-deftest beads-agent-test-start-at-point-no-sessions-starts-directly ()
  "Test that start-at-point starts agent directly when no sessions exist."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions)
                ((symbol-function 'beads-agent--detect-issue-id)
                 (lambda () "bd-direct"))
                ((symbol-function 'beads-agent-start)
                 (lambda (id &rest _args)
                   ;; Track that this was called with correct ID
                   (should (equal id "bd-direct"))
                   'start-called))
                ((symbol-function 'beads-agent-issue)
                 (lambda (&rest _)
                   (error "Should not show menu when no sessions"))))
        ;; No sessions exist for this issue
        (should (null (beads-agent--get-sessions-for-issue "bd-direct")))
        ;; Call start-at-point - should call start function, not menu
        (beads-agent-start-at-point))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-start-at-point-with-sessions-shows-menu ()
  "Test that start-at-point shows management menu when sessions exist."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions)
                ((symbol-function 'beads-agent--detect-issue-id)
                 (lambda () "bd-menu"))
                ((symbol-function 'beads-agent-start)
                 (lambda (&rest _)
                   (error "Should not start when sessions exist")))
                ((symbol-function 'beads-agent-issue)
                 (lambda (id)
                   ;; Track that menu was called with correct ID
                   (should (equal id "bd-menu"))
                   'menu-called)))
        ;; Create a session for this issue
        (beads-agent--create-session "bd-menu" "mock" "/tmp" 'handle)
        (should (beads-agent--get-sessions-for-issue "bd-menu"))
        ;; Call start-at-point - should call menu, not start
        (beads-agent-start-at-point))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-start-at-point-no-context-prompts ()
  "Test that start-at-point prompts when no issue ID detected."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((start-called nil))
        (cl-letf (((symbol-function 'sesman-sessions)
                   #'beads-agent-test--mock-sesman-sessions)
                  ((symbol-function 'beads-agent--detect-issue-id)
                   (lambda () nil))
                  ;; Need to mock as interactive command for call-interactively
                  ((symbol-function 'beads-agent-start)
                   (lambda (&optional _issue-id _backend _prompt)
                     (interactive)
                     (setq start-called t))))
          ;; Call start-at-point with no detected ID
          ;; Should fall through to interactive beads-agent-start
          (beads-agent-start-at-point)
          (should start-called)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-start-at-point-transition-after-stop ()
  "Test transition: sessions exist -> stop all -> direct start again."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((menu-called 0)
            (start-called 0))
        (cl-letf (((symbol-function 'sesman-sessions)
                   #'beads-agent-test--mock-sesman-sessions)
                  ((symbol-function 'beads-agent--detect-issue-id)
                   (lambda () "bd-transition"))
                  ((symbol-function 'beads-agent-start)
                   (lambda (id &rest _args)
                     (should (equal id "bd-transition"))
                     (cl-incf start-called)))
                  ((symbol-function 'beads-agent-issue)
                   (lambda (id)
                     (should (equal id "bd-transition"))
                     (cl-incf menu-called))))
          ;; Phase 1: No sessions - should start directly
          (beads-agent-start-at-point)
          (should (= start-called 1))
          (should (= menu-called 0))

          ;; Phase 2: Create session - should show menu
          (let ((session (beads-agent--create-session
                          "bd-transition" "mock" "/tmp" 'handle)))
            (beads-agent-start-at-point)
            (should (= start-called 1))  ; unchanged
            (should (= menu-called 1))

            ;; Phase 3: Stop session - should go back to direct start
            (beads-agent--destroy-session (oref session id)))
          (beads-agent-start-at-point)
          (should (= start-called 2))
          (should (= menu-called 1))))  ; unchanged
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-start-at-point-cross-issue-independence ()
  "Test that sessions for other issues don't affect current issue."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((detected-issue "bd-current")
            (start-called nil)
            (menu-called nil))
        (cl-letf (((symbol-function 'sesman-sessions)
                   #'beads-agent-test--mock-sesman-sessions)
                  ((symbol-function 'beads-agent--detect-issue-id)
                   (lambda () detected-issue))
                  ((symbol-function 'beads-agent-start)
                   (lambda (&rest _) (setq start-called t)))
                  ((symbol-function 'beads-agent-issue)
                   (lambda (_) (setq menu-called t))))
          ;; Create sessions for OTHER issues, not the current one
          (beads-agent--create-session "bd-other-1" "mock" "/tmp" 'h1)
          (beads-agent--create-session "bd-other-2" "mock" "/tmp" 'h2)
          ;; Current issue has no sessions
          (should (null (beads-agent--get-sessions-for-issue "bd-current")))
          ;; Should start directly, not show menu
          (beads-agent-start-at-point)
          (should start-called)
          (should (null menu-called))))
    (beads-agent-test--teardown)))

;;; =========================================================================
;;; Transient Suffix Behavior Tests (Issue beads.el-r5bd)
;;; =========================================================================

;; These tests verify that transient suffixes have correct :transient
;; properties:
;; - Actions that complete work (start, stop, jump) should close transient
;; - Actions for preview/settings should keep transient open

(defun beads-agent-test--suffix-transient-p (suffix-symbol)
  "Return the :transient property of SUFFIX-SYMBOL.
Returns t if suffix stays open, nil if it closes (unbound or nil)."
  (when-let ((suffix (get suffix-symbol 'transient--suffix)))
    (and (slot-boundp suffix 'transient)
         (slot-value suffix 'transient))))

(ert-deftest beads-agent-test-issue-start-new-closes-transient ()
  "Test that starting a new agent closes the per-issue menu.
After starting an agent, user should return to their previous context."
  (should (null (beads-agent-test--suffix-transient-p
                 'beads-agent-issue--start-new))))

(ert-deftest beads-agent-test-issue-stop-one-closes-transient ()
  "Test that stopping one agent closes the per-issue menu.
After stopping an agent, user should return to their previous context."
  (should (null (beads-agent-test--suffix-transient-p
                 'beads-agent-issue--stop-one))))

(ert-deftest beads-agent-test-issue-stop-all-closes-transient ()
  "Test that stopping all agents closes the per-issue menu.
After stopping all agents, user should return to their previous context."
  (should (null (beads-agent-test--suffix-transient-p
                 'beads-agent-issue--stop-all))))

(ert-deftest beads-agent-test-issue-refresh-stays-open ()
  "Test that refresh keeps the per-issue menu open.
Refreshing is a preview action that should not exit the transient."
  (should (eq t (beads-agent-test--suffix-transient-p
                 'beads-agent-issue--refresh))))

(ert-deftest beads-agent-test-main-menu-cleanup-stays-open ()
  "Test that cleanup suffix keeps the main menu open.
Cleanup shows feedback and user may want to continue."
  (should (eq t (beads-agent-test--suffix-transient-p
                 'beads-agent--cleanup-suffix))))

(ert-deftest beads-agent-test-main-menu-refresh-stays-open ()
  "Test that refresh suffix keeps the main menu open."
  (should (eq t (beads-agent-test--suffix-transient-p
                 'beads-agent--refresh-suffix))))

(ert-deftest beads-agent-test-main-menu-switch-backend-stays-open ()
  "Test that switch-backend suffix keeps the main menu open.
Settings changes should allow continued configuration."
  (should (eq t (beads-agent-test--suffix-transient-p
                 'beads-agent--switch-backend-suffix))))

(ert-deftest beads-agent-test-start-menu-preview-stays-open ()
  "Test that preview suffix in start menu stays open."
  (should (eq t (beads-agent-test--suffix-transient-p
                 'beads-agent-start--preview))))

(ert-deftest beads-agent-test-start-menu-reset-stays-open ()
  "Test that reset suffix in start menu stays open."
  (should (eq t (beads-agent-test--suffix-transient-p
                 'beads-agent-start--reset))))

(ert-deftest beads-agent-test-start-menu-execute-closes ()
  "Test that execute suffix in start menu closes."
  (should (null (beads-agent-test--suffix-transient-p
                 'beads-agent-start--execute))))

;;; Tests for Agent Type Name Slot

(ert-deftest beads-agent-test-session-type-name-slot ()
  "Test that session has agent-type-name slot."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-123" "mock" "/tmp" 'handle nil "Task")))
          (should (equal (oref session agent-type-name) "Task"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-session-type-name-accessor ()
  "Test beads-agent-session-type-name accessor function."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-456" "mock" "/tmp" 'handle nil "Review")))
          (should (equal (beads-agent-session-type-name session) "Review"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-session-type-name-nil-when-not-set ()
  "Test that agent-type-name is nil when not provided."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Create session without type name (backwards compatibility)
        (let ((session (beads-agent--create-session
                        "bd-789" "mock" "/tmp" 'handle)))
          (should (null (beads-agent-session-type-name session)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-session-type-name-with-worktree ()
  "Test session creation with both worktree and agent type name."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-101" "mock" "/main/repo" 'handle
                        "/worktrees/bd-101" "QA")))
          (should (equal (oref session worktree-dir) "/worktrees/bd-101"))
          (should (equal (beads-agent-session-type-name session) "QA"))))
    (beads-agent-test--teardown)))

;;; Tests for Buffer Naming

(ert-deftest beads-agent-test-generate-buffer-name ()
  "Test basic buffer name generation."
  (should (equal (beads-agent--generate-buffer-name "beads.el-xrrt" "Task" 1)
                 "*beads-agent[beads.el-xrrt][Task#1]*"))
  (should (equal (beads-agent--generate-buffer-name "beads.el-xrrt" "Plan" 2)
                 "*beads-agent[beads.el-xrrt][Plan#2]*"))
  (should (equal (beads-agent--generate-buffer-name "bd-123" "Review" 5)
                 "*beads-agent[bd-123][Review#5]*")))

(ert-deftest beads-agent-test-generate-buffer-name-special-chars ()
  "Test buffer name generation with special characters in issue ID."
  (should (equal (beads-agent--generate-buffer-name "my-project.el-abc" "QA" 1)
                 "*beads-agent[my-project.el-abc][QA#1]*"))
  (should (equal (beads-agent--generate-buffer-name "proj_v2-xyz" "Custom" 3)
                 "*beads-agent[proj_v2-xyz][Custom#3]*")))

(ert-deftest beads-agent-test-parse-buffer-name ()
  "Test parsing buffer names back to components."
  (let ((parsed (beads-agent--parse-buffer-name
                 "*beads-agent[beads.el-xrrt][Task#1]*")))
    (should parsed)
    (should (equal (plist-get parsed :issue-id) "beads.el-xrrt"))
    (should (equal (plist-get parsed :type-name) "Task"))
    (should (= (plist-get parsed :instance-n) 1))))

(ert-deftest beads-agent-test-parse-buffer-name-various ()
  "Test parsing various buffer name formats."
  ;; Multi-digit instance number
  (let ((parsed (beads-agent--parse-buffer-name
                 "*beads-agent[bd-42][Plan#12]*")))
    (should parsed)
    (should (= (plist-get parsed :instance-n) 12)))
  ;; Complex issue ID
  (let ((parsed (beads-agent--parse-buffer-name
                 "*beads-agent[my-project.el-abc][Review#7]*")))
    (should parsed)
    (should (equal (plist-get parsed :issue-id) "my-project.el-abc"))
    (should (equal (plist-get parsed :type-name) "Review"))))

(ert-deftest beads-agent-test-parse-buffer-name-invalid ()
  "Test that invalid buffer names return nil."
  (should (null (beads-agent--parse-buffer-name "*scratch*")))
  (should (null (beads-agent--parse-buffer-name "*Messages*")))
  (should (null (beads-agent--parse-buffer-name "regular-buffer")))
  ;; Missing parts
  (should (null (beads-agent--parse-buffer-name "*beads-agent[issue]*")))
  (should (null (beads-agent--parse-buffer-name "*beads-agent[issue][Type]*"))))

(ert-deftest beads-agent-test-buffer-name-p ()
  "Test buffer name predicate."
  (should (beads-agent--buffer-name-p "*beads-agent[bd-1][Task#1]*"))
  (should (beads-agent--buffer-name-p "*beads-agent[beads.el-xyz][Plan#99]*"))
  (should-not (beads-agent--buffer-name-p "*scratch*"))
  (should-not (beads-agent--buffer-name-p nil))
  (should-not (beads-agent--buffer-name-p "")))

(ert-deftest beads-agent-test-roundtrip-buffer-name ()
  "Test that generate and parse are inverses."
  (let* ((issue-id "beads.el-test")
         (type-name "Custom")
         (instance-n 42)
         (buf-name (beads-agent--generate-buffer-name
                    issue-id type-name instance-n))
         (parsed (beads-agent--parse-buffer-name buf-name)))
    (should parsed)
    (should (equal (plist-get parsed :issue-id) issue-id))
    (should (equal (plist-get parsed :type-name) type-name))
    (should (= (plist-get parsed :instance-n) instance-n))))

(ert-deftest beads-agent-test-typed-instance-counters-basic ()
  "Test typed instance counter basics."
  (beads-agent--reset-typed-instance-counters)
  (unwind-protect
      (progn
        ;; First instance should be 1
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Task") 1))
        ;; Second instance should be 2
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Task") 2))
        ;; Third instance should be 3
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Task") 3)))
    (beads-agent--reset-typed-instance-counters)))

(ert-deftest beads-agent-test-typed-instance-counters-independent ()
  "Test that counters are independent per (issue, type)."
  (beads-agent--reset-typed-instance-counters)
  (unwind-protect
      (progn
        ;; Different types for same issue have independent counters
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Task") 1))
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Plan") 1))
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Review") 1))
        ;; Second Task instance
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Task") 2))
        ;; Plan still at 1 (next would be 2)
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Plan") 2)))
    (beads-agent--reset-typed-instance-counters)))

(ert-deftest beads-agent-test-typed-instance-counters-different-issues ()
  "Test that different issues have independent counters."
  (beads-agent--reset-typed-instance-counters)
  (unwind-protect
      (progn
        ;; Same type for different issues
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Task") 1))
        (should (= (beads-agent--next-typed-instance-number "bd-2" "Task") 1))
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Task") 2))
        (should (= (beads-agent--next-typed-instance-number "bd-2" "Task") 2)))
    (beads-agent--reset-typed-instance-counters)))

(ert-deftest beads-agent-test-peek-typed-instance-number ()
  "Test peeking at next instance number without incrementing."
  (beads-agent--reset-typed-instance-counters)
  (unwind-protect
      (progn
        ;; Peek should return 1 for new combination
        (should (= (beads-agent--peek-typed-instance-number "bd-1" "Task") 1))
        ;; Peek again should still return 1 (not incremented)
        (should (= (beads-agent--peek-typed-instance-number "bd-1" "Task") 1))
        ;; Actually get the instance
        (should (= (beads-agent--next-typed-instance-number "bd-1" "Task") 1))
        ;; Now peek should return 2
        (should (= (beads-agent--peek-typed-instance-number "bd-1" "Task") 2)))
    (beads-agent--reset-typed-instance-counters)))

(ert-deftest beads-agent-test-generate-buffer-name-for-session ()
  "Test generating buffer name from session object."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-42" "mock" "/tmp" 'handle nil "Task")))
          (should (string-match-p
                   "\\*beads-agent\\[bd-42\\]\\[Task#[0-9]+\\]\\*"
                   (beads-agent--generate-buffer-name-for-session session)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-generate-buffer-name-for-session-no-type ()
  "Test buffer name generation when session has no agent type."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-99" "mock" "/tmp" 'handle)))
          ;; Should use "Agent" as default type
          (should (string-match-p
                   "\\*beads-agent\\[bd-99\\]\\[Agent#[0-9]+\\]\\*"
                   (beads-agent--generate-buffer-name-for-session session)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-session-instance-number ()
  "Test extracting instance number from session."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        ;; Create first session - should be #1
        (let ((session1 (beads-agent--create-session
                         "bd-50" "mock" "/tmp" 'handle1 nil "Task")))
          (should (= (beads-agent--session-instance-number session1) 1)))
        ;; Create second session - should be #2
        (let ((session2 (beads-agent--create-session
                         "bd-50" "mock" "/tmp" 'handle2 nil "Task")))
          (should (= (beads-agent--session-instance-number session2) 2))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-find-buffers-by-issue ()
  "Test finding buffers by issue ID."
  (let ((buf1 nil) (buf2 nil) (buf3 nil))
    (unwind-protect
        (progn
          ;; Create test buffers with beads naming convention
          (setq buf1 (get-buffer-create "*beads-agent[bd-1][Task#1]*"))
          (setq buf2 (get-buffer-create "*beads-agent[bd-1][Plan#1]*"))
          (setq buf3 (get-buffer-create "*beads-agent[bd-2][Task#1]*"))
          ;; Find buffers for bd-1
          (let ((found (beads-agent--find-buffers-by-issue "bd-1")))
            (should (= (length found) 2))
            (should (memq buf1 found))
            (should (memq buf2 found))
            (should-not (memq buf3 found)))
          ;; Find buffers for bd-2
          (let ((found (beads-agent--find-buffers-by-issue "bd-2")))
            (should (= (length found) 1))
            (should (memq buf3 found))))
      ;; Cleanup
      (when buf1 (kill-buffer buf1))
      (when buf2 (kill-buffer buf2))
      (when buf3 (kill-buffer buf3)))))

(ert-deftest beads-agent-test-find-buffers-by-type ()
  "Test finding buffers by type name."
  (let ((buf1 nil) (buf2 nil) (buf3 nil))
    (unwind-protect
        (progn
          ;; Create test buffers
          (setq buf1 (get-buffer-create "*beads-agent[bd-1][Task#1]*"))
          (setq buf2 (get-buffer-create "*beads-agent[bd-2][Task#1]*"))
          (setq buf3 (get-buffer-create "*beads-agent[bd-1][Plan#1]*"))
          ;; Find all Task buffers
          (let ((found (beads-agent--find-buffers-by-type "Task")))
            (should (= (length found) 2))
            (should (memq buf1 found))
            (should (memq buf2 found))
            (should-not (memq buf3 found)))
          ;; Find Plan buffers
          (let ((found (beads-agent--find-buffers-by-type "Plan")))
            (should (= (length found) 1))
            (should (memq buf3 found))))
      ;; Cleanup
      (when buf1 (kill-buffer buf1))
      (when buf2 (kill-buffer buf2))
      (when buf3 (kill-buffer buf3)))))

(ert-deftest beads-agent-test-find-buffers-none-found ()
  "Test that find functions return empty list when no matches."
  (should (null (beads-agent--find-buffers-by-issue "nonexistent-issue")))
  (should (null (beads-agent--find-buffers-by-type "NonexistentType"))))

;;; Tests for Session Buffer Slot

(ert-deftest beads-agent-test-session-buffer-slot ()
  "Test that session has buffer slot."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-buf" "mock" "/tmp" 'handle)))
          ;; Initially nil
          (should (null (beads-agent-session-buffer session)))
          ;; Can be set
          (let ((buf (get-buffer-create "*test-buffer*")))
            (unwind-protect
                (progn
                  (beads-agent-session-set-buffer session buf)
                  (should (eq (beads-agent-session-buffer session) buf)))
              (kill-buffer buf)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-session-buffer-nil-when-not-set ()
  "Test that buffer accessor returns nil when not set."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-nobuf" "mock" "/tmp" 'handle)))
          (should (null (beads-agent-session-buffer session)))))
    (beads-agent-test--teardown)))

;;; Tests for Mock Backend Buffer Creation

(ert-deftest beads-agent-test-mock-backend-creates-buffer ()
  "Test that mock backend creates a buffer on start."
  (beads-agent-mock-reset)
  (unwind-protect
      (let* ((mock-backend (beads-agent-mock-get-instance))
             (issue (beads-issue :id "bd-mockbuf" :title "Test")))
        ;; backend-start now returns (backend-session . buffer)
        (let* ((start-result (beads-agent-backend-start mock-backend issue "test prompt"))
               (handle (car start-result))
               (buffer (cdr start-result)))
          (should handle)
          (should buffer)
          ;; Handle should have a buffer slot
          (should (oref handle buffer))
          (should (buffer-live-p (oref handle buffer)))
          ;; The returned buffer should match the handle's buffer
          (should (eq buffer (oref handle buffer)))
          ;; Buffer should have mock prefix
          (should (string-prefix-p "*mock-agent-"
                                   (buffer-name buffer)))))
    (beads-agent-mock-reset)))

(ert-deftest beads-agent-test-mock-backend-get-buffer ()
  "Test that mock backend get-buffer returns the session's stored buffer."
  (beads-agent-mock-reset)
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let* ((mock-backend (beads-agent-mock-get-instance))
               (issue (beads-issue :id "bd-getbuf" :title "Test"))
               ;; backend-start now returns (backend-session . buffer)
               (start-result (beads-agent-backend-start mock-backend issue "test prompt"))
               (handle (car start-result))
               (buffer (cdr start-result))
               (session (beads-agent--create-session
                         "bd-getbuf" "mock" "/tmp" handle)))
          ;; Store the buffer in session (as caller would)
          (beads-agent-session-set-buffer session buffer)
          ;; get-buffer returns the session's stored buffer
          (let ((buf (beads-agent-backend-get-buffer mock-backend session)))
            (should buf)
            (should (buffer-live-p buf))
            (should (eq buf buffer)))
          ;; Set a different buffer as stored
          (let ((stored-buf (get-buffer-create "*stored-test*")))
            (unwind-protect
                (progn
                  (beads-agent-session-set-buffer session stored-buf)
                  ;; Now should return the stored buffer
                  (should (eq (beads-agent-backend-get-buffer mock-backend session)
                              stored-buf)))
              (kill-buffer stored-buf)))))
    (beads-agent-mock-reset)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mock-backend-stop-kills-buffer ()
  "Test that mock backend stop kills the buffer."
  (beads-agent-mock-reset)
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let* ((mock-backend (beads-agent-mock-get-instance))
               (issue (beads-issue :id "bd-stopbuf" :title "Test"))
               ;; backend-start now returns (backend-session . buffer)
               (start-result (beads-agent-backend-start mock-backend issue "test prompt"))
               (handle (car start-result))
               (buffer (cdr start-result))
               (session (beads-agent--create-session
                         "bd-stopbuf" "mock" "/tmp" handle)))
          ;; Buffer should exist
          (should (buffer-live-p buffer))
          ;; Stop the session
          (beads-agent-backend-stop mock-backend session)
          ;; Buffer should be killed
          (should-not (buffer-live-p buffer))))
    (beads-agent-mock-reset)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mock-reset-kills-buffers ()
  "Test that mock reset kills all session buffers."
  (beads-agent-mock-reset)
  (unwind-protect
      (let ((mock-backend (beads-agent-mock-get-instance))
            (buffers nil))
        ;; Create several sessions
        (dotimes (_ 3)
          (let* ((issue (beads-issue :id "bd-reset" :title "Test"))
                 ;; backend-start now returns (backend-session . buffer)
                 (start-result (beads-agent-backend-start mock-backend issue "test"))
                 (buffer (cdr start-result)))
            (push buffer buffers)))
        ;; All buffers should exist
        (should (cl-every #'buffer-live-p buffers))
        ;; Reset
        (beads-agent-mock-reset)
        ;; All buffers should be killed
        (should (cl-notany #'buffer-live-p buffers)))
    (beads-agent-mock-reset)))

;;; Tests for Buffer Renaming Integration

(ert-deftest beads-agent-test-rename-and-store-buffer ()
  "Test that beads-agent--rename-and-store-buffer works correctly.
Buffer names now use project name (from project-dir), not issue ID."
  (beads-agent-mock-reset)
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (beads-agent--reset-typed-instance-counters)
        (let* ((mock-backend (beads-agent-mock-get-instance))
               (issue (beads-issue :id "bd-rename" :title "Test"))
               ;; backend-start now returns (backend-session . buffer)
               (start-result (beads-agent-backend-start mock-backend issue "test prompt"))
               (handle (car start-result))
               (original-buffer (cdr start-result))
               ;; Use /home/user/myproject as project-dir for clearer test
               (session (beads-agent--create-session
                         "bd-rename" "mock" "/home/user/myproject" handle nil "Task")))
          ;; Rename and store (new signature: session buffer)
          (beads-agent--rename-and-store-buffer session original-buffer)
          ;; Buffer should be renamed to beads format
          (let ((stored-buffer (beads-agent-session-buffer session)))
            (should stored-buffer)
            (should (buffer-live-p stored-buffer))
            (should (beads-agent--buffer-name-p (buffer-name stored-buffer)))
            ;; Should match expected format using project name, not issue ID
            ;; Format: *beads-agent[PROJECT][TYPE#N]*
            (should (string-match-p "\\*beads-agent\\[myproject\\]\\[Task#1\\]\\*"
                                    (buffer-name stored-buffer))))))
    (beads-agent--reset-typed-instance-counters)
    (beads-agent-mock-reset)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-rename-increments-counter ()
  "Test that renaming buffers increments the typed instance counter."
  (beads-agent-mock-reset)
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (beads-agent--reset-typed-instance-counters)
        (let ((mock-backend (beads-agent-mock-get-instance))
              sessions)
          ;; Create three sessions with same type
          (dotimes (_i 3)
            (let* ((issue (beads-issue :id "bd-inc" :title "Test"))
                   ;; backend-start now returns (backend-session . buffer)
                   (start-result (beads-agent-backend-start mock-backend issue "test"))
                   (handle (car start-result))
                   (buffer (cdr start-result))
                   (session (beads-agent--create-session
                             "bd-inc" "mock" "/tmp" handle nil "Task")))
              ;; Rename and store (new signature: session buffer)
              (beads-agent--rename-and-store-buffer session buffer)
              (push session sessions)))
          ;; Check buffer names have incrementing counters
          (let ((names (mapcar (lambda (s)
                                 (buffer-name (beads-agent-session-buffer s)))
                               (nreverse sessions))))
            (should (string-match-p "Task#1" (nth 0 names)))
            (should (string-match-p "Task#2" (nth 1 names)))
            (should (string-match-p "Task#3" (nth 2 names))))))
    (beads-agent--reset-typed-instance-counters)
    (beads-agent-mock-reset)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-rename-different-types-independent ()
  "Test that different types have independent counters.
Buffer names use project name (from project-dir), not issue ID.
Each type maintains its own instance counter per project."
  (beads-agent-mock-reset)
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (beads-agent--reset-typed-instance-counters)
        (let ((mock-backend (beads-agent-mock-get-instance))
              (types '("Task" "Plan" "Review"))
              (sessions nil))
          ;; Create one session for each type
          (dolist (type-name types)
            (let* ((issue (beads-issue :id "bd-types" :title "Test"))
                   ;; backend-start now returns (backend-session . buffer)
                   (start-result (beads-agent-backend-start mock-backend issue "test"))
                   (handle (car start-result))
                   (buffer (cdr start-result))
                   ;; Use /home/user/myproject for clearer test
                   (session (beads-agent--create-session
                             "bd-types" "mock" "/home/user/myproject" handle nil type-name)))
              ;; Rename and store (new signature: session buffer)
              (beads-agent--rename-and-store-buffer session buffer)
              (push (cons type-name session) sessions)))
          ;; Each type should be #1 (within same project)
          ;; Buffer format: *beads-agent[myproject][TYPE#N]*
          (dolist (pair sessions)
            (let ((type-name (car pair))
                  (session (cdr pair)))
              (should (string-match-p (format "myproject\\]\\[%s#1" type-name)
                                      (buffer-name
                                       (beads-agent-session-buffer session))))))))
    (beads-agent--reset-typed-instance-counters)
    (beads-agent-mock-reset)
    (beads-agent-test--teardown)))

;;; Tests for Session Selection with completing-read

(ert-deftest beads-agent-test-select-session-completing-read-nil-sessions ()
  "Test that select-session-completing-read returns nil for empty sessions."
  (should (null (beads-agent--select-session-completing-read nil "Select: "))))

(ert-deftest beads-agent-test-select-session-completing-read-formats-label ()
  "Test that session labels are formatted as Type#N (backend)."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-123" "mock" "/tmp" 'handle nil "Task")))
          ;; Mock completing-read to capture choices
          (let ((captured-choices nil))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _)
                         (setq captured-choices choices)
                         ;; Return first choice
                         (caar choices))))
              (beads-agent--select-session-completing-read
               (list session) "Select: ")
              ;; Check the format: Type#N (backend)
              (should (= (length captured-choices) 1))
              (should (string-match-p "Task#1 (mock)" (caar captured-choices)))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-select-session-completing-read-returns-session ()
  "Test that select-session-completing-read returns the selected session."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let* ((session1 (beads-agent--create-session
                          "bd-123" "mock" "/tmp" 'h1 nil "Task"))
               (session2 (beads-agent--create-session
                          "bd-123" "mock" "/tmp" 'h2 nil "Review")))
          ;; Mock completing-read to return the second choice
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _)
                       (car (nth 1 choices)))))
            (let ((selected (beads-agent--select-session-completing-read
                             (list session1 session2) "Select: ")))
              ;; Should return the second session
              (should (eq selected session2))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-select-session-uses-agent-as-default-type ()
  "Test that sessions without type-name use Agent as label."
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions))
        (let ((session (beads-agent--create-session
                        "bd-123" "mock" "/tmp" 'handle nil nil)))
          (let ((captured-choices nil))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _)
                         (setq captured-choices choices)
                         (caar choices))))
              (beads-agent--select-session-completing-read
               (list session) "Select: ")
              ;; Should use "Agent" when type-name is nil
              (should (string-match-p "Agent#1 (mock)" (caar captured-choices)))))))
    (beads-agent-test--teardown)))

;;; Tests for jump-at-point with Multiple Sessions

(ert-deftest beads-agent-test-jump-at-point-single-session-no-prompt ()
  "Test that jump-at-point with single session doesn't prompt."
  (beads-agent-mock-reset)
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions)
                ((symbol-function 'beads-agent--detect-issue-id)
                 (lambda () "bd-123")))
        ;; Create one session
        (let ((session (beads-agent--create-session
                        "bd-123" "mock" "/tmp" 'handle nil "Task")))
          ;; Track if completing-read was called
          (let ((completing-read-called nil))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (setq completing-read-called t)
                         nil))
                      ((symbol-function 'beads-agent-jump)
                       (lambda (id) id)))
              (beads-agent-jump-at-point)
              ;; Should NOT have prompted
              (should-not completing-read-called)))))
    (beads-agent-mock-reset)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-jump-at-point-multiple-sessions-prompts ()
  "Test that jump-at-point with multiple sessions prompts for selection."
  (beads-agent-mock-reset)
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions)
                ((symbol-function 'beads-agent--detect-issue-id)
                 (lambda () "bd-123")))
        ;; Create multiple sessions
        (let* ((session1 (beads-agent--create-session
                          "bd-123" "mock" "/tmp" 'h1 nil "Task"))
               (session2 (beads-agent--create-session
                          "bd-123" "mock" "/tmp" 'h2 nil "Review")))
          ;; Track if completing-read was called
          (let ((completing-read-called nil)
                (jumped-to-id nil))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (prompt choices &rest _)
                         (setq completing-read-called t)
                         ;; Return first choice
                         (caar choices)))
                      ((symbol-function 'beads-agent-jump)
                       (lambda (id)
                         (setq jumped-to-id id))))
              (beads-agent-jump-at-point)
              ;; Should have prompted
              (should completing-read-called)
              ;; Should have jumped to one of the sessions (order not guaranteed)
              (should (member jumped-to-id
                              (list (oref session1 id) (oref session2 id))))))))
    (beads-agent-mock-reset)
    (beads-agent-test--teardown)))

;;; Tests for start-typed with Multiple Sessions

(ert-deftest beads-agent-test-start-typed-single-session-no-prompt ()
  "Test that start-typed with single matching session doesn't prompt."
  (beads-agent-mock-reset)
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions)
                ((symbol-function 'beads-agent--detect-issue-id)
                 (lambda () "bd-123")))
        ;; Create one Task session
        (let ((session (beads-agent--create-session
                        "bd-123" "mock" "/tmp" 'handle nil "Task")))
          (let ((completing-read-called nil))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (setq completing-read-called t)
                         nil))
                      ((symbol-function 'beads-agent-jump)
                       (lambda (id) id)))
              (beads-agent--start-typed "Task")
              ;; Should NOT have prompted
              (should-not completing-read-called)))))
    (beads-agent-mock-reset)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-start-typed-multiple-sessions-prompts ()
  "Test that start-typed with multiple matching sessions prompts."
  (beads-agent-mock-reset)
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions)
                ((symbol-function 'beads-agent--detect-issue-id)
                 (lambda () "bd-123")))
        ;; Create multiple Task sessions
        (let* ((session1 (beads-agent--create-session
                          "bd-123" "mock" "/tmp" 'h1 nil "Task"))
               (session2 (beads-agent--create-session
                          "bd-123" "mock" "/tmp" 'h2 nil "Task")))
          (let ((completing-read-called nil)
                (jumped-to-id nil))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (prompt choices &rest _)
                         (setq completing-read-called t)
                         ;; Check prompt includes type
                         (should (string-match-p "Task" prompt))
                         ;; Return first choice
                         (caar choices)))
                      ((symbol-function 'beads-agent-jump)
                       (lambda (id)
                         (setq jumped-to-id id))))
              (beads-agent--start-typed "Task")
              ;; Should have prompted
              (should completing-read-called)
              ;; Should have jumped to one of the Task sessions (order not guaranteed)
              (should (member jumped-to-id
                              (list (oref session1 id) (oref session2 id))))))))
    (beads-agent-mock-reset)
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-start-typed-only-matches-type ()
  "Test that start-typed only considers sessions of matching type."
  (beads-agent-mock-reset)
  (beads-agent-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-sessions)
                 #'beads-agent-test--mock-sesman-sessions)
                ((symbol-function 'beads-agent--detect-issue-id)
                 (lambda () "bd-123")))
        ;; Create one Task and one Review session
        (beads-agent--create-session "bd-123" "mock" "/tmp" 'h1 nil "Task")
        (beads-agent--create-session "bd-123" "mock" "/tmp" 'h2 nil "Review")
        ;; Starting a Plan agent should NOT match either
        (let ((completing-read-called nil)
              (started-agent nil))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (setq completing-read-called t)
                       nil))
                    ((symbol-function 'beads-agent-start)
                     (lambda (&rest args)
                       (setq started-agent t))))
            (beads-agent--start-typed "Plan")
            ;; Should NOT have prompted (no Plan sessions)
            (should-not completing-read-called)
            ;; Should have started a new agent
            (should started-agent))))
    (beads-agent-mock-reset)
    (beads-agent-test--teardown)))

;;; Buffer Acquisition Tests

(ert-deftest beads-agent-test-wait-for-buffer-immediate ()
  "Test wait-for-buffer returns immediately when buffer exists."
  (let ((test-buffer (generate-new-buffer "*test-wait*")))
    (unwind-protect
        (let ((result (beads-agent--wait-for-buffer
                       (lambda () test-buffer)
                       1.0)))
          (should (eq result test-buffer)))
      (kill-buffer test-buffer))))

(ert-deftest beads-agent-test-wait-for-buffer-delayed ()
  "Test wait-for-buffer retries and finds buffer after delay."
  (let ((test-buffer nil)
        (call-count 0))
    (unwind-protect
        (progn
          ;; Schedule buffer creation after short delay
          (run-at-time 0.2 nil (lambda ()
                                 (setq test-buffer
                                       (generate-new-buffer "*test-delayed*"))))
          (let ((result (beads-agent--wait-for-buffer
                         (lambda ()
                           (cl-incf call-count)
                           test-buffer)
                         2.0
                         0.05)))
            ;; Should have retried multiple times
            (should (> call-count 1))
            ;; Should have found the buffer
            (should (eq result test-buffer))))
      (when test-buffer
        (kill-buffer test-buffer)))))

(ert-deftest beads-agent-test-wait-for-buffer-timeout ()
  "Test wait-for-buffer returns nil on timeout."
  (let ((result (beads-agent--wait-for-buffer
                 (lambda () nil)  ; Never returns a buffer
                 0.3              ; Short timeout
                 0.05)))          ; Short interval
    (should (null result))))

;;; Async Fetch Tests

(ert-deftest beads-agent-test-fetch-issue-async-success ()
  "Test that fetch-issue-async calls callback with parsed issue on success."
  (let* ((callback-result nil)
         ;; Create a mock issue to return
         (mock-issue (beads-issue :id "bd-1"
                                  :title "Test Issue"
                                  :status "open"
                                  :priority 2)))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (cmd callback)
                 ;; Create mock result with exit-code 0 and parsed data
                 (oset cmd exit-code 0)
                 (oset cmd data (vector mock-issue))
                 (funcall callback cmd))))
      (beads-agent--fetch-issue-async
       "bd-1"
       (lambda (issue) (setq callback-result issue)))
      ;; Callback should have been invoked with a beads-issue
      (should callback-result)
      (should (cl-typep callback-result 'beads-issue))
      (should (equal (oref callback-result id) "bd-1")))))

(ert-deftest beads-agent-test-fetch-issue-async-exit-error ()
  "Test that fetch-issue-async calls callback with nil on non-zero exit."
  (let* ((callback-called nil)
         (callback-result 'not-set))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (cmd callback)
                 ;; Create mock result with non-zero exit code
                 (oset cmd exit-code 1)
                 (oset cmd stderr "Error: issue not found")
                 (oset cmd data nil)
                 (funcall callback cmd))))
      (beads-agent--fetch-issue-async
       "bd-nonexistent"
       (lambda (issue)
         (setq callback-called t)
         (setq callback-result issue)))
      ;; Callback should have been invoked with nil
      (should callback-called)
      (should (null callback-result)))))

(ert-deftest beads-agent-test-fetch-issue-async-parse-error ()
  "Test that fetch-issue-async calls callback with nil on data access error."
  (let* ((callback-called nil)
         (callback-result 'not-set))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (cmd callback)
                 ;; Create mock result with exit-code 0 but data that causes
                 ;; an error when accessed (empty vector with aref attempt)
                 (oset cmd exit-code 0)
                 (oset cmd data [])  ; Empty vector causes (aref [] 0) to error
                 (funcall callback cmd))))
      (beads-agent--fetch-issue-async
       "bd-1"
       (lambda (issue)
         (setq callback-called t)
         (setq callback-result issue)))
      ;; Callback should have been invoked with nil due to array bounds error
      (should callback-called)
      (should (null callback-result)))))

(ert-deftest beads-agent-test-fetch-issue-async-nil-data ()
  "Test that fetch-issue-async handles nil data gracefully."
  (let* ((callback-called nil)
         (callback-result 'not-set))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (cmd callback)
                 ;; Create mock result with exit-code 0 but nil data
                 ;; (happens when parse returns nil due to empty result)
                 (oset cmd exit-code 0)
                 (oset cmd data nil)
                 (funcall callback cmd))))
      (beads-agent--fetch-issue-async
       "bd-1"
       (lambda (issue)
         (setq callback-called t)
         (setq callback-result issue)))
      ;; Callback should have been invoked with nil due to nil data
      (should callback-called)
      (should (null callback-result)))))

(ert-deftest beads-agent-test-start-async-handles-nil-issue ()
  "Test that start-async shows error message when issue fetch fails."
  (let ((message-shown nil))
    (cl-letf (((symbol-function 'beads-agent--fetch-issue-async)
               (lambda (_issue-id callback)
                 ;; Simulate fetch failure
                 (funcall callback nil)))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when (string-match-p "Cannot start agent" fmt)
                   (setq message-shown (apply #'format fmt args))))))
      ;; Create mock backend
      (let ((backend (beads-agent-backend-mock)))
        (beads-agent--start-async "bd-test" backend "/tmp" nil nil))
      ;; Should have shown error message
      (should message-shown)
      (should (string-match-p "bd-test" message-shown)))))

;;; Tests for Backend Selection with Type Preferences

(ert-deftest beads-agent-test-backend-available-and-get-exists ()
  "Test backend-available-and-get returns backend when available."
  (beads-agent-test--setup)
  (unwind-protect
      ;; Mock backend should be available
      (let ((result (beads-agent--backend-available-and-get "mock")))
        (should result)
        (should (object-of-class-p result 'beads-agent-backend)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-backend-available-and-get-not-found ()
  "Test backend-available-and-get returns nil for non-existent backend."
  (beads-agent-test--setup)
  (unwind-protect
      (should (null (beads-agent--backend-available-and-get "non-existent")))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-select-backend-type-preference ()
  "Test select-backend uses type-specific preference when set."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend nil)
            (beads-agent-task-backend "mock"))
        ;; Get the Task type
        (let ((agent-type (beads-agent-type-get "task")))
          (should agent-type)
          ;; Select backend should use type preference
          (let ((result (beads-agent--select-backend agent-type)))
            (should result)
            (should (equal (oref result name) "mock")))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-select-backend-global-default ()
  "Test select-backend uses global default when type preference nil."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend "mock")
            (beads-agent-task-backend nil))
        ;; Get the Task type
        (let ((agent-type (beads-agent-type-get "task")))
          (should agent-type)
          ;; Select backend should fall back to global default
          (let ((result (beads-agent--select-backend agent-type)))
            (should result)
            (should (equal (oref result name) "mock")))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-select-backend-type-over-global ()
  "Test type-specific backend takes precedence over global default."
  (beads-agent-test--setup)
  (unwind-protect
      ;; Register a second backend for this test
      (let ((secondary-backend (beads-agent-backend-test-mock :name "secondary")))
        (beads-agent--register-backend secondary-backend)
        (unwind-protect
            (let ((beads-agent-default-backend "secondary")
                  (beads-agent-task-backend "mock"))
              (let* ((agent-type (beads-agent-type-get "task"))
                     (result (beads-agent--select-backend agent-type)))
                ;; Should use type-specific "mock", not global "secondary"
                (should result)
                (should (equal (oref result name) "mock"))))
          ;; Cleanup secondary backend
          (setq beads-agent--backends
                (cl-remove-if (lambda (b)
                                (equal (oref b name) "secondary"))
                              beads-agent--backends))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-select-backend-nil-type ()
  "Test select-backend works with nil agent-type argument."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend "mock"))
        ;; Select backend with nil type should use global default
        (let ((result (beads-agent--select-backend nil)))
          (should result)
          (should (equal (oref result name) "mock"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-select-backend-unavailable-type-preference ()
  "Test select-backend falls through when type preference unavailable."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-default-backend "mock")
            (beads-agent-task-backend "non-existent-backend"))
        ;; Get the Task type
        (let ((agent-type (beads-agent-type-get "task")))
          ;; Type preference doesn't exist, should fall back to global default
          (let ((result (beads-agent--select-backend agent-type)))
            (should result)
            (should (equal (oref result name) "mock")))))
    (beads-agent-test--teardown)))

;;; Tests for Mode-Line Indicator

(ert-deftest beads-agent-test-mode-line-context-no-project ()
  "Test mode-line context when not in a project."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((default-directory "/tmp")
            (beads-agent--mode-line-cache nil))  ; Clear cache for test
        (cl-letf (((symbol-function 'beads-git-get-project-name)
                   (lambda () nil))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () nil))
                  ((symbol-function 'beads-git-in-worktree-p)
                   (lambda () nil))
                  ((symbol-function 'beads-agent--get-current-project-session)
                   (lambda () nil)))
          (let ((ctx (beads-agent--mode-line-context)))
            (should (null (plist-get ctx :project-name)))
            (should (null (plist-get ctx :branch)))
            (should (null (plist-get ctx :in-worktree)))
            (should (null (plist-get ctx :agent-session))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-context-in-project ()
  "Test mode-line context when in a project."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent--mode-line-cache nil))  ; Clear cache for test
        (cl-letf (((symbol-function 'beads-git-get-project-name)
                   (lambda () "beads.el"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main"))
                  ((symbol-function 'beads-git-in-worktree-p)
                   (lambda () nil))
                  ((symbol-function 'beads-agent--get-current-project-session)
                   (lambda () nil)))
          (let ((ctx (beads-agent--mode-line-context)))
            (should (equal (plist-get ctx :project-name) "beads.el"))
            (should (equal (plist-get ctx :branch) "main"))
            (should (null (plist-get ctx :in-worktree)))
            (should (null (plist-get ctx :agent-session))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-context-in-worktree ()
  "Test mode-line context when in a git worktree."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent--mode-line-cache nil))  ; Clear cache for test
        (cl-letf (((symbol-function 'beads-git-get-project-name)
                   (lambda () "beads.el"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "feature-branch"))
                  ((symbol-function 'beads-git-in-worktree-p)
                   (lambda () t))
                  ((symbol-function 'beads-agent--get-current-project-session)
                   (lambda () nil)))
          (let ((ctx (beads-agent--mode-line-context)))
            (should (equal (plist-get ctx :project-name) "beads.el"))
            (should (plist-get ctx :in-worktree)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-context-with-agent ()
  "Test mode-line context when an agent session is active."
  (beads-agent-test--setup)
  (unwind-protect
      (let* ((beads-agent--mode-line-cache nil)  ; Clear cache for test
             (mock-session (beads-agent-session
                            :id "beads.el#1"
                            :project-dir "/home/user/beads.el"
                            :proj-name "beads.el"
                            :instance-number 1
                            :backend-name "mock"
                            :agent-type-name "Task"
                            :started-at (format-time-string "%Y-%m-%dT%H:%M:%S"))))
        (cl-letf (((symbol-function 'beads-git-get-project-name)
                   (lambda () "beads.el"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main"))
                  ((symbol-function 'beads-git-in-worktree-p)
                   (lambda () nil))
                  ((symbol-function 'beads-agent--get-current-project-session)
                   (lambda () mock-session)))
          (let ((ctx (beads-agent--mode-line-context)))
            (should (equal (plist-get ctx :project-name) "beads.el"))
            (should (eq (plist-get ctx :agent-session) mock-session))
            (should (equal (plist-get ctx :agent-type) "Task"))
            (should (equal (plist-get ctx :agent-instance) 1)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-format-default-no-agent ()
  "Test default format without active agent."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-mode-line-faces nil)
            (ctx (list :project-name "beads.el"
                       :branch "main"
                       :in-worktree nil
                       :agent-session nil
                       :agent-type nil
                       :agent-instance nil)))
        (let ((result (beads-agent--mode-line-format-default ctx)))
          (should (stringp result))
          (should (string= result "[beads.el:main]"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-format-default-with-agent ()
  "Test default format with active agent."
  (beads-agent-test--setup)
  (unwind-protect
      (let* ((beads-agent-mode-line-faces nil)
             (mock-session (beads-agent-session
                            :id "beads.el#1"
                            :project-dir "/home/user/beads.el"
                            :backend-name "mock"
                            :agent-type-name "Task"
                            :instance-number 1
                            :started-at (format-time-string "%Y-%m-%dT%H:%M:%S")))
             (ctx (list :project-name "beads.el"
                        :branch "main"
                        :in-worktree nil
                        :agent-session mock-session
                        :agent-type "Task"
                        :agent-instance 1)))
        (let ((result (beads-agent--mode-line-format-default ctx)))
          (should (stringp result))
          (should (string= result "[beads.el:Task#1@main]"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-format-default-in-worktree ()
  "Test default format shows worktree indicator."
  (beads-agent-test--setup)
  (unwind-protect
      (let* ((beads-agent-mode-line-faces nil)
             (mock-session (beads-agent-session
                            :id "beads.el#1"
                            :project-dir "/home/user/beads.el-wt"
                            :backend-name "mock"
                            :agent-type-name "Review"
                            :instance-number 2
                            :started-at (format-time-string "%Y-%m-%dT%H:%M:%S")))
             (ctx (list :project-name "beads.el"
                        :branch "feature"
                        :in-worktree t
                        :agent-session mock-session
                        :agent-type "Review"
                        :agent-instance 2)))
        (let ((result (beads-agent--mode-line-format-default ctx)))
          (should (stringp result))
          (should (string= result "[beads.el:Review#2@wt]"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-format-compact ()
  "Test compact format."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((ctx (list :project-name "beads.el"
                       :branch "main"
                       :in-worktree nil
                       :agent-session nil
                       :agent-type "Task"
                       :agent-instance 1)))
        (let ((result (beads-agent--mode-line-format-compact ctx)))
          (should (stringp result))
          (should (string= result "[b:T#1]"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-format-compact-in-worktree ()
  "Test compact format shows worktree asterisk."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((ctx (list :project-name "my-project"
                       :branch "feature"
                       :in-worktree t
                       :agent-session nil
                       :agent-type nil
                       :agent-instance nil)))
        (let ((result (beads-agent--mode-line-format-compact ctx)))
          (should (stringp result))
          (should (string= result "[m*]"))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-format-full ()
  "Test full format with all details."
  (beads-agent-test--setup)
  (unwind-protect
      (let* ((beads-agent-mode-line-faces nil)
             (mock-session (beads-agent-session
                            :id "beads.el#1"
                            :project-dir "/home/user/beads.el"
                            :backend-name "mock"
                            :agent-type-name "Plan"
                            :instance-number 1
                            :current-issue "bd-42"
                            :started-at (format-time-string "%Y-%m-%dT%H:%M:%S")))
             (ctx (list :project-name "beads.el"
                        :branch "main"
                        :in-worktree nil
                        :agent-session mock-session
                        :agent-type "Plan"
                        :agent-instance 1)))
        (let ((result (beads-agent--mode-line-format-full ctx)))
          (should (stringp result))
          (should (string-match-p "beads.el" result))
          (should (string-match-p "main" result))
          (should (string-match-p "Plan#1" result))
          (should (string-match-p "bd-42" result))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-format-full-in-worktree ()
  "Test full format with worktree indicator."
  (beads-agent-test--setup)
  (unwind-protect
      (let* ((beads-agent-mode-line-faces nil)
             (ctx (list :project-name "beads.el"
                        :branch "feature"
                        :in-worktree t
                        :agent-session nil
                        :agent-type nil
                        :agent-instance nil)))
        (let ((result (beads-agent--mode-line-format-full ctx)))
          (should (stringp result))
          (should (string-match-p "beads.el" result))
          (should (string-match-p "feature" result))
          (should (string-match-p "\\[worktree\\]" result))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-indicator-nil-when-no-project ()
  "Test indicator returns nil when not in a project."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent--mode-line-cache nil))  ; Clear cache for test
        (cl-letf (((symbol-function 'beads-git-get-project-name)
                   (lambda () nil))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () nil))
                  ((symbol-function 'beads-git-in-worktree-p)
                   (lambda () nil))
                  ((symbol-function 'beads-agent--get-current-project-session)
                   (lambda () nil)))
          (let ((result (beads-agent--mode-line-indicator)))
            (should (null result)))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-indicator-uses-format-setting ()
  "Test indicator respects beads-agent-mode-line-format setting."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent--mode-line-cache nil))  ; Clear cache for test
        (cl-letf (((symbol-function 'beads-git-get-project-name)
                   (lambda () "test-proj"))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "dev"))
                  ((symbol-function 'beads-git-in-worktree-p)
                   (lambda () nil))
                  ((symbol-function 'beads-agent--get-current-project-session)
                   (lambda () nil)))
          (let ((beads-agent-mode-line-faces nil))
            ;; Test default format
            (let ((beads-agent-mode-line-format 'default))
              (should (string= (beads-agent--mode-line-indicator)
                               "[test-proj:dev]")))
            ;; Test compact format
            (let ((beads-agent-mode-line-format 'compact))
              (should (string= (beads-agent--mode-line-indicator)
                               "[t]"))))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-indicator-custom-function ()
  "Test indicator respects custom function format."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent-mode-line-format (lambda () "[CUSTOM]")))
        (should (string= (beads-agent--mode-line-indicator) "[CUSTOM]")))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-mode-adds-to-misc-info ()
  "Test enabling mode-line-mode adds indicator to mode-line-misc-info."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((original-misc-info mode-line-misc-info))
        (unwind-protect
            (progn
              ;; Enable mode
              (beads-agent-mode-line-mode 1)
              (should (member beads-agent--mode-line-misc-info-entry
                              mode-line-misc-info))
              ;; Disable mode
              (beads-agent-mode-line-mode -1)
              (should-not (member beads-agent--mode-line-misc-info-entry
                                  mode-line-misc-info)))
          ;; Restore original
          (setq mode-line-misc-info original-misc-info)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-mode-does-not-remove-other-spaces ()
  "Test disabling mode-line-mode does not remove unrelated space strings."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((original-misc-info mode-line-misc-info))
        (unwind-protect
            (let ((test-entry '(" " "test")))
              ;; Add a test entry with space
              (setq mode-line-misc-info (list test-entry))
              ;; Enable and disable mode
              (beads-agent-mode-line-mode 1)
              (beads-agent-mode-line-mode -1)
              ;; Our entry should be gone but test entry should remain
              (should-not (member beads-agent--mode-line-misc-info-entry
                                  mode-line-misc-info))
              (should (member test-entry mode-line-misc-info)))
          ;; Restore original
          (setq mode-line-misc-info original-misc-info)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-cache-valid-p ()
  "Test cache validity check."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent--mode-line-cache nil))
        ;; Empty cache is invalid
        (should-not (beads-agent--mode-line-cache-valid-p))
        ;; Set cache for current directory
        (setq beads-agent--mode-line-cache
              (list default-directory "main" nil (float-time)))
        (should (beads-agent--mode-line-cache-valid-p))
        ;; Different directory invalidates cache
        (setq beads-agent--mode-line-cache
              (list "/some/other/dir" "main" nil (float-time)))
        (should-not (beads-agent--mode-line-cache-valid-p))
        ;; Expired cache is invalid
        (setq beads-agent--mode-line-cache
              (list default-directory "main" nil
                    (- (float-time) 10.0)))  ; 10 seconds ago
        (should-not (beads-agent--mode-line-cache-valid-p)))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-cached-git-info ()
  "Test cached git info fetching."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((beads-agent--mode-line-cache nil)
            (call-count 0))
        (cl-letf (((symbol-function 'beads-git-get-branch)
                   (lambda ()
                     (cl-incf call-count)
                     "test-branch"))
                  ((symbol-function 'beads-git-in-worktree-p)
                   (lambda () t)))
          ;; First call should fetch
          (let ((result (beads-agent--mode-line-cached-git-info)))
            (should (equal (car result) "test-branch"))
            (should (cdr result))
            (should (= call-count 1)))
          ;; Second call should use cache
          (let ((result (beads-agent--mode-line-cached-git-info)))
            (should (equal (car result) "test-branch"))
            (should (= call-count 1)))))  ; Still 1, not 2
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-format-compact-empty-project ()
  "Test compact format handles empty project name gracefully."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((ctx (list :project-name ""
                       :branch "main"
                       :in-worktree nil
                       :agent-session nil
                       :agent-type nil
                       :agent-instance nil)))
        ;; Should return nil for empty project name, not crash
        (should (null (beads-agent--mode-line-format-compact ctx))))
    (beads-agent-test--teardown)))

(ert-deftest beads-agent-test-mode-line-format-compact-empty-agent-type ()
  "Test compact format handles empty agent type gracefully."
  (beads-agent-test--setup)
  (unwind-protect
      (let ((ctx (list :project-name "test"
                       :branch "main"
                       :in-worktree nil
                       :agent-session t  ; non-nil session
                       :agent-type ""    ; empty type
                       :agent-instance 1)))
        ;; Should work without agent part
        (let ((result (beads-agent--mode-line-format-compact ctx)))
          (should (stringp result))
          (should (string= result "[t]"))))
    (beads-agent-test--teardown)))

;;; Footer

(provide 'beads-agent-test)
;;; beads-agent-test.el ends here
