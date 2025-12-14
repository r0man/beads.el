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

;;; Mock Sesman Storage for Testing
;;
;; Since sessions are now stored in sesman, we need to mock sesman
;; functions to keep tests isolated.  The mock storage simulates
;; sesman's session registry.

(defvar beads-agent-test--sesman-sessions nil
  "Mock sesman session storage for tests.
List of sesman session lists: ((name handle beads-session) ...).")

;;; Mock Backend for Testing

(defclass beads-agent-backend-mock (beads-agent-backend)
  ((name :initform "mock")
   (priority :initform 100)
   (start-called :initform nil :type boolean)
   (stop-called :initform nil :type boolean)
   (active-sessions :initform nil :type list))
  :documentation "Mock backend for testing.")

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-mock))
  "Mock backend is always available."
  t)

(cl-defmethod beads-agent-backend-start
    ((backend beads-agent-backend-mock) _issue _prompt)
  "Mock starting a session."
  (oset backend start-called t)
  ;; Return a mock session handle
  'mock-session-handle)

(cl-defmethod beads-agent-backend-stop
    ((backend beads-agent-backend-mock) session)
  "Mock stopping a session."
  (oset backend stop-called t)
  (oset backend active-sessions
        (delete (oref session id) (oref backend active-sessions))))

(cl-defmethod beads-agent-backend-stop-async
    ((backend beads-agent-backend-mock) session callback)
  "Mock stopping a session asynchronously.
Calls sync stop immediately for testing purposes."
  (beads-agent-backend-stop backend session)
  (when callback
    (funcall callback)))

(cl-defmethod beads-agent-backend-session-active-p
    ((backend beads-agent-backend-mock) session)
  "Mock session active check."
  (member (oref session id) (oref backend active-sessions)))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((_backend beads-agent-backend-mock) _session)
  "Mock buffer switch."
  t)

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-mock) _session _prompt)
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
  ;; Create and register mock backend
  (setq beads-agent-test--mock-backend (beads-agent-backend-mock))
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

(ert-deftest beads-agent-test-generate-session-id ()
  "Test session ID generation."
  (let ((id1 (beads-agent--generate-session-id))
        (id2 (beads-agent--generate-session-id)))
    (should (stringp id1))
    (should (stringp id2))
    (should (string-prefix-p "session-" id1))
    ;; IDs should be unique (with high probability)
    (should (not (equal id1 id2)))))

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

;;; Footer

(provide 'beads-agent-test)
;;; beads-agent-test.el ends here
