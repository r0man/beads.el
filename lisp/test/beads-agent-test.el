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

;;; Tests for Worktree Environment

(ert-deftest beads-agent-test-setup-worktree-environment ()
  "Test worktree environment setup."
  (let* ((original-env process-environment)
         (new-env (beads-agent--setup-worktree-environment)))
    ;; Should have BD_NO_DAEMON=1 at the front
    (should (string-prefix-p "BD_NO_DAEMON=1" (car new-env)))
    ;; Should not modify original
    (should (eq process-environment original-env))))

;;; Tests for Worktree Path Calculation

(ert-deftest beads-agent-test-worktree-path-for-issue ()
  "Test worktree path calculation."
  (let ((beads-agent-worktree-parent nil))
    ;; Mock main repo root
    (cl-letf (((symbol-function 'beads-agent--main-repo-root)
               (lambda () "/home/user/projects/myrepo")))
      ;; Worktree should be sibling to repo
      (should (equal (beads-agent--worktree-path-for-issue "bd-123")
                     "/home/user/projects/bd-123")))))

(ert-deftest beads-agent-test-worktree-path-custom-parent ()
  "Test worktree path calculation with custom parent."
  (let ((beads-agent-worktree-parent "/tmp/worktrees"))
    ;; Mock main repo root
    (cl-letf (((symbol-function 'beads-agent--main-repo-root)
               (lambda () "/home/user/projects/myrepo")))
      ;; Worktree should be under custom parent
      (should (equal (beads-agent--worktree-path-for-issue "bd-456")
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
  "Test that beads-agent--rename-and-store-buffer works correctly."
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
               (session (beads-agent--create-session
                         "bd-rename" "mock" "/tmp" handle nil "Task")))
          ;; Rename and store (new signature: session buffer)
          (beads-agent--rename-and-store-buffer session original-buffer)
          ;; Buffer should be renamed to beads format
          (let ((stored-buffer (beads-agent-session-buffer session)))
            (should stored-buffer)
            (should (buffer-live-p stored-buffer))
            (should (beads-agent--buffer-name-p (buffer-name stored-buffer)))
            ;; Should match expected format
            (should (string-match-p "\\*beads-agent\\[bd-rename\\]\\[Task#1\\]\\*"
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
  "Test that different types have independent counters."
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
                   (session (beads-agent--create-session
                             "bd-types" "mock" "/tmp" handle nil type-name)))
              ;; Rename and store (new signature: session buffer)
              (beads-agent--rename-and-store-buffer session buffer)
              (push (cons type-name session) sessions)))
          ;; Each type should be #1
          (dolist (pair sessions)
            (let ((type-name (car pair))
                  (session (cdr pair)))
              (should (string-match-p (format "%s#1" type-name)
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
         (sentinel-fn nil)
         (output-buffer nil)
         (valid-json "[{\"id\":\"bd-1\",\"title\":\"Test Issue\",\"status\":\"open\",\"priority\":2}]"))
    (cl-letf (((symbol-function 'start-process)
               (lambda (name buffer &rest _program)
                 (setq output-buffer buffer)
                 ;; Insert valid JSON into output buffer
                 (with-current-buffer buffer
                   (insert valid-json))
                 (let ((proc (make-process :name name
                                          :buffer nil
                                          :command '("true")
                                          :noquery t)))
                   proc)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc fn)
                 (setq sentinel-fn fn)))
              ((symbol-function 'process-status)
               (lambda (_proc) 'exit))
              ((symbol-function 'process-exit-status)
               (lambda (_proc) 0))
              ((symbol-function 'beads-command-line)
               (lambda (_cmd) '("bd" "show"))))
      (beads-agent--fetch-issue-async
       "bd-1"
       (lambda (issue) (setq callback-result issue)))
      ;; Simulate process exit
      (when sentinel-fn
        (funcall sentinel-fn 'mock-proc "finished\n"))
      ;; Callback should have been invoked with a beads-issue
      (should callback-result)
      (should (cl-typep callback-result 'beads-issue))
      (should (equal (oref callback-result id) "bd-1")))))

(ert-deftest beads-agent-test-fetch-issue-async-exit-error ()
  "Test that fetch-issue-async calls callback with nil on non-zero exit."
  (let* ((callback-called nil)
         (callback-result 'not-set)
         (sentinel-fn nil)
         (output-buffer nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (name buffer &rest _program)
                 (setq output-buffer buffer)
                 (with-current-buffer buffer
                   (insert "Error: issue not found"))
                 (let ((proc (make-process :name name
                                          :buffer nil
                                          :command '("true")
                                          :noquery t)))
                   proc)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc fn)
                 (setq sentinel-fn fn)))
              ((symbol-function 'process-status)
               (lambda (_proc) 'exit))
              ((symbol-function 'process-exit-status)
               (lambda (_proc) 1))
              ((symbol-function 'beads-command-line)
               (lambda (_cmd) '("bd" "show"))))
      (beads-agent--fetch-issue-async
       "bd-nonexistent"
       (lambda (issue)
         (setq callback-called t)
         (setq callback-result issue)))
      ;; Simulate process exit
      (when sentinel-fn
        (funcall sentinel-fn 'mock-proc "finished\n"))
      ;; Callback should have been invoked with nil
      (should callback-called)
      (should (null callback-result)))))

(ert-deftest beads-agent-test-fetch-issue-async-parse-error ()
  "Test that fetch-issue-async calls callback with nil on JSON parse error."
  (let* ((callback-called nil)
         (callback-result 'not-set)
         (sentinel-fn nil)
         (output-buffer nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (name buffer &rest _program)
                 (setq output-buffer buffer)
                 (with-current-buffer buffer
                   (insert "invalid json {{{"))
                 (let ((proc (make-process :name name
                                          :buffer nil
                                          :command '("true")
                                          :noquery t)))
                   proc)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_proc fn)
                 (setq sentinel-fn fn)))
              ((symbol-function 'process-status)
               (lambda (_proc) 'exit))
              ((symbol-function 'process-exit-status)
               (lambda (_proc) 0))
              ((symbol-function 'beads-command-line)
               (lambda (_cmd) '("bd" "show"))))
      (beads-agent--fetch-issue-async
       "bd-1"
       (lambda (issue)
         (setq callback-called t)
         (setq callback-result issue)))
      ;; Simulate process exit
      (when sentinel-fn
        (funcall sentinel-fn 'mock-proc "finished\n"))
      ;; Callback should have been invoked with nil due to parse error
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

;;; Footer

(provide 'beads-agent-test)
;;; beads-agent-test.el ends here
