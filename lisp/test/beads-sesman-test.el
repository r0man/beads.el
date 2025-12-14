;;; beads-sesman-test.el --- Tests for beads-sesman -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-sesman.el sesman integration.
;; Tests cover session naming, registration, context linking,
;; and the sesman generic method implementations.
;;
;; This test file mocks sesman functions to test the integration
;; layer without side effects on global sesman state.

;;; Code:

(require 'ert)
(require 'beads-sesman)
(require 'beads-agent-backend)

;;; Test Fixtures

;; Concrete test backend class for testing generic methods
(defclass beads-sesman-test--backend (beads-agent-backend)
  ((name :initform "test-backend"))
  :documentation "Test backend for beads-sesman tests.")

(defvar beads-sesman-test--mock-session nil
  "Mock beads-agent-session for tests.")

(defvar beads-sesman-test--registered-sessions nil
  "List of sessions registered with mock sesman.")

(defvar beads-sesman-test--session-links nil
  "List of (session context-type context-value) links.")

(defun beads-sesman-test--make-mock-session (&optional issue-id project-dir worktree-dir)
  "Create a mock beads-agent-session.
ISSUE-ID defaults to \"test-123\".
PROJECT-DIR defaults to \"/tmp/project\".
WORKTREE-DIR is optional worktree directory."
  (beads-agent-session
   :id "session-123"
   :issue-id (or issue-id "test-123")
   :backend-name "mock"
   :project-dir (or project-dir "/tmp/project")
   :worktree-dir worktree-dir
   :started-at "2025-01-01T12:00:00+0000"
   :backend-session 'mock-handle))

(defun beads-sesman-test--setup ()
  "Set up test fixtures."
  (setq beads-sesman-test--mock-session
        (beads-sesman-test--make-mock-session))
  (setq beads-sesman-test--registered-sessions nil)
  (setq beads-sesman-test--session-links nil))

(defun beads-sesman-test--teardown ()
  "Tear down test fixtures."
  (setq beads-sesman-test--mock-session nil)
  (setq beads-sesman-test--registered-sessions nil)
  (setq beads-sesman-test--session-links nil))

;;; Tests for Session Naming

(ert-deftest beads-sesman-test-session-name-basic ()
  "Test session name generation with project directory."
  (beads-sesman-test--setup)
  (unwind-protect
      (let ((name (beads-sesman--session-name beads-sesman-test--mock-session)))
        (should (stringp name))
        (should (string-match-p "^test-123@" name))
        (should (string-match-p "/tmp/project" name)))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-session-name-with-worktree ()
  "Test session name generation with worktree directory."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((session (beads-sesman-test--make-mock-session
                       "issue-456" "/tmp/main" "/tmp/worktree/issue-456"))
             (name (beads-sesman--session-name session)))
        (should (stringp name))
        (should (string-match-p "^issue-456@" name))
        ;; Should use worktree, not project
        (should (string-match-p "worktree" name)))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-session-name-fallback ()
  "Test session name uses fallback when backend not found."
  (beads-sesman-test--setup)
  (unwind-protect
      ;; Mock session has backend-name "mock" which is not registered
      (cl-letf (((symbol-function 'beads-agent--get-backend)
                 (lambda (_name) nil)))
        (let ((name (beads-sesman--session-name beads-sesman-test--mock-session)))
          (should (stringp name))
          (should (string-match-p "^test-123@" name))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-session-name-uses-generic ()
  "Test session name delegates to backend generic when available."
  (beads-sesman-test--setup)
  (unwind-protect
      (let ((generic-called nil)
            (mock-backend 'fake-backend))
        (cl-letf (((symbol-function 'beads-agent--get-backend)
                   (lambda (name)
                     (when (equal name "mock")
                       mock-backend)))
                  ((symbol-function 'beads-agent-backend-session-name)
                   (lambda (backend session)
                     (setq generic-called (list backend session))
                     "custom-session-name")))
          (let ((name (beads-sesman--session-name beads-sesman-test--mock-session)))
            ;; Should have called the generic
            (should generic-called)
            (should (eq (car generic-called) mock-backend))
            (should (eq (cadr generic-called) beads-sesman-test--mock-session))
            ;; Should return the custom name
            (should (equal name "custom-session-name")))))
    (beads-sesman-test--teardown)))

;;; Tests for Backend Session Name Generic

(ert-deftest beads-sesman-test-backend-session-name-default ()
  "Test default implementation of backend session name generic."
  (beads-sesman-test--setup)
  (unwind-protect
      ;; Use the default generic implementation with concrete test backend
      (let* ((mock-backend (beads-sesman-test--backend))
             (name (beads-agent-backend-session-name
                    mock-backend beads-sesman-test--mock-session)))
        (should (stringp name))
        (should (string-match-p "^test-123@" name))
        (should (string-match-p "/tmp/project" name)))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-backend-session-name-with-worktree ()
  "Test backend session name default uses worktree when available."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((session (beads-sesman-test--make-mock-session
                       "bd-42" "/tmp/main" "/tmp/worktrees/bd-42"))
             (mock-backend (beads-sesman-test--backend))
             (name (beads-agent-backend-session-name mock-backend session)))
        (should (stringp name))
        (should (string-match-p "^bd-42@" name))
        (should (string-match-p "worktrees" name)))
    (beads-sesman-test--teardown)))

;; Custom backend class for testing method override
(defclass beads-sesman-test--custom-naming-backend (beads-agent-backend)
  ((name :initform "custom-naming-backend"))
  :documentation "Test backend with custom session naming.")

(cl-defmethod beads-agent-backend-session-name
    ((_backend beads-sesman-test--custom-naming-backend) session)
  "Return custom session name format."
  (format "CUSTOM:%s" (oref session issue-id)))

(ert-deftest beads-sesman-test-backend-session-name-custom-override ()
  "Test that backends can override session naming via cl-defmethod."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((custom-backend (beads-sesman-test--custom-naming-backend))
             (name (beads-agent-backend-session-name
                    custom-backend beads-sesman-test--mock-session)))
        ;; Should use the custom format, not the default
        (should (equal name "CUSTOM:test-123")))
    (beads-sesman-test--teardown)))

;;; Tests for Sesman Session Creation

(ert-deftest beads-sesman-test-make-sesman-session ()
  "Test creating sesman session list from beads session."
  (beads-sesman-test--setup)
  (unwind-protect
      (let ((sesman-session (beads-sesman--make-sesman-session
                             beads-sesman-test--mock-session)))
        (should (listp sesman-session))
        (should (= (length sesman-session) 3))
        ;; First element is name
        (should (stringp (nth 0 sesman-session)))
        ;; Second element is backend handle
        (should (eq (nth 1 sesman-session) 'mock-handle))
        ;; Third element is the beads session
        (should (eq (nth 2 sesman-session) beads-sesman-test--mock-session)))
    (beads-sesman-test--teardown)))

;;; Tests for Session Registration

(ert-deftest beads-sesman-test-register-session ()
  "Test registering session with sesman."
  (beads-sesman-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-register)
                 (lambda (_system session)
                   (push session beads-sesman-test--registered-sessions)))
                ((symbol-function 'sesman-link-session)
                 (lambda (_system session type val)
                   (push (list session type val)
                         beads-sesman-test--session-links))))
        (beads-sesman--register-session beads-sesman-test--mock-session)
        ;; Should have registered one session
        (should (= (length beads-sesman-test--registered-sessions) 1))
        ;; Should have one link (project, no worktree)
        (should (= (length beads-sesman-test--session-links) 1))
        ;; Link should be to project
        (should (eq (nth 1 (car beads-sesman-test--session-links)) 'project)))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-register-session-with-worktree ()
  "Test registering session with worktree creates dual links."
  (beads-sesman-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'sesman-register)
                 (lambda (_system session)
                   (push session beads-sesman-test--registered-sessions)))
                ((symbol-function 'sesman-link-session)
                 (lambda (_system session type val)
                   (push (list session type val)
                         beads-sesman-test--session-links))))
        (let ((session (beads-sesman-test--make-mock-session
                        "bd-99" "/tmp/main" "/tmp/worktree/bd-99")))
          (beads-sesman--register-session session)
          ;; Should have registered one session
          (should (= (length beads-sesman-test--registered-sessions) 1))
          ;; Should have TWO links (directory + project)
          (should (= (length beads-sesman-test--session-links) 2))
          ;; Check link types
          (let ((types (mapcar (lambda (l) (nth 1 l))
                               beads-sesman-test--session-links)))
            (should (member 'directory types))
            (should (member 'project types)))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-unregister-session ()
  "Test unregistering session from sesman."
  (beads-sesman-test--setup)
  (unwind-protect
      (let ((unregistered nil))
        (cl-letf (((symbol-function 'sesman-unregister)
                   (lambda (_system session)
                     (setq unregistered session)))
                  ((symbol-function 'sesman-session)
                   (lambda (_system _name)
                     'found-session)))
          (beads-sesman--unregister-session beads-sesman-test--mock-session)
          (should (eq unregistered 'found-session))))
    (beads-sesman-test--teardown)))

;;; Tests for State Change Hook

(ert-deftest beads-sesman-test-state-change-started ()
  "Test state change handler registers session on start."
  (beads-sesman-test--setup)
  (unwind-protect
      (let ((registered nil))
        (cl-letf (((symbol-function 'beads-sesman--register-session)
                   (lambda (s) (setq registered s))))
          (beads-sesman--state-change-handler 'started
                                               beads-sesman-test--mock-session)
          (should (eq registered beads-sesman-test--mock-session))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-state-change-stopped ()
  "Test state change handler unregisters session on stop."
  (beads-sesman-test--setup)
  (unwind-protect
      (let ((unregistered nil))
        (cl-letf (((symbol-function 'beads-sesman--unregister-session)
                   (lambda (s) (setq unregistered s))))
          (beads-sesman--state-change-handler 'stopped
                                               beads-sesman-test--mock-session)
          (should (eq unregistered beads-sesman-test--mock-session))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-state-change-failed ()
  "Test state change handler ignores failed action."
  (beads-sesman-test--setup)
  (unwind-protect
      (let ((called nil))
        (cl-letf (((symbol-function 'beads-sesman--register-session)
                   (lambda (_s) (setq called 'register)))
                  ((symbol-function 'beads-sesman--unregister-session)
                   (lambda (_s) (setq called 'unregister))))
          (beads-sesman--state-change-handler 'failed
                                               beads-sesman-test--mock-session)
          (should-not called)))
    (beads-sesman-test--teardown)))

;;; Tests for Sesman Generic Methods

(ert-deftest beads-sesman-test-context-types ()
  "Test sesman-context-types returns expected types."
  (let ((types (sesman-context-types 'beads)))
    (should (listp types))
    (should (member 'buffer types))
    (should (member 'directory types))
    (should (member 'project types))))

(ert-deftest beads-sesman-test-more-relevant-p ()
  "Test sesman-more-relevant-p compares by recency."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((older (beads-sesman-test--make-mock-session))
             (newer (beads-agent-session
                     :id "session-456"
                     :issue-id "test-456"
                     :backend-name "mock"
                     :project-dir "/tmp/project"
                     :started-at "2025-01-02T12:00:00+0000"
                     :backend-session 'mock-handle-2))
             (older-sesman (beads-sesman--make-sesman-session older))
             (newer-sesman (beads-sesman--make-sesman-session newer)))
        ;; Newer should be more relevant than older
        (should (sesman-more-relevant-p 'beads newer-sesman older-sesman))
        ;; Older should NOT be more relevant than newer
        (should-not (sesman-more-relevant-p 'beads older-sesman newer-sesman)))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-more-relevant-p-nil-sessions ()
  "Test sesman-more-relevant-p handles nil beads sessions."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((valid-session (beads-sesman-test--make-mock-session))
             (valid-sesman (beads-sesman--make-sesman-session valid-session))
             (nil-sesman '("name" handle nil)))
        ;; Both combinations with nil should return nil
        (should-not (sesman-more-relevant-p 'beads nil-sesman valid-sesman))
        (should-not (sesman-more-relevant-p 'beads valid-sesman nil-sesman))
        ;; Both nil should return nil
        (should-not (sesman-more-relevant-p 'beads nil-sesman nil-sesman)))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-more-relevant-p-equal-timestamps ()
  "Test sesman-more-relevant-p with equal timestamps."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((session1 (beads-sesman-test--make-mock-session))
             (session2 (beads-sesman-test--make-mock-session "other-id"))
             (sesman1 (beads-sesman--make-sesman-session session1))
             (sesman2 (beads-sesman--make-sesman-session session2)))
        ;; Same timestamp - neither should be more relevant
        (should-not (sesman-more-relevant-p 'beads sesman1 sesman2)))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-session-info ()
  "Test sesman-session-info returns display info."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((sesman-session (beads-sesman--make-sesman-session
                              beads-sesman-test--mock-session))
             (info (sesman-session-info 'beads sesman-session)))
        (should (plist-get info :objects))
        (should (plist-get info :strings))
        ;; Check strings contain expected info
        (let ((strings (plist-get info :strings)))
          (should (cl-some (lambda (s) (and s (string-match-p "Issue:" s)))
                           strings))
          (should (cl-some (lambda (s) (and s (string-match-p "Backend:" s)))
                           strings))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-session-info-nil-beads-session ()
  "Test sesman-session-info handles nil beads session."
  (let ((info (sesman-session-info 'beads '("name" handle nil))))
    (should (null info))))

(ert-deftest beads-sesman-test-session-info-with-worktree ()
  "Test sesman-session-info includes worktree when present."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((session (beads-sesman-test--make-mock-session
                       "test-id" "/tmp/main" "/tmp/worktree/test-id"))
             (sesman-session (beads-sesman--make-sesman-session session))
             (info (sesman-session-info 'beads sesman-session)))
        (should (plist-get info :strings))
        ;; Check worktree is included
        (let ((strings (plist-get info :strings)))
          (should (cl-some (lambda (s) (and s (string-match-p "Worktree:" s)))
                           strings))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-session-info-objects-contains-handle ()
  "Test sesman-session-info :objects contains backend handle."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((sesman-session (beads-sesman--make-sesman-session
                              beads-sesman-test--mock-session))
             (info (sesman-session-info 'beads sesman-session))
             (objects (plist-get info :objects)))
        (should (member 'mock-handle objects)))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-start-session ()
  "Test sesman-start-session prompts for issue and starts agent."
  (let ((read-issue-called nil)
        (agent-start-called nil))
    (cl-letf (((symbol-function 'beads-agent--read-issue-id)
               (lambda ()
                 (setq read-issue-called t)
                 "test-issue-42"))
              ((symbol-function 'beads-agent-start)
               (lambda (issue-id)
                 (setq agent-start-called issue-id))))
      (let ((result (sesman-start-session 'beads)))
        ;; Should return nil (async registration via hook)
        (should (null result))
        ;; Should have prompted for issue
        (should read-issue-called)
        ;; Should have started agent with the issue ID
        (should (equal agent-start-called "test-issue-42"))))))

(ert-deftest beads-sesman-test-quit-session ()
  "Test sesman-quit-session stops the agent."
  (beads-sesman-test--setup)
  (unwind-protect
      (let ((stop-called nil))
        (cl-letf (((symbol-function 'beads-agent-stop)
                   (lambda (session-id)
                     (setq stop-called session-id))))
          (let ((sesman-session (beads-sesman--make-sesman-session
                                  beads-sesman-test--mock-session)))
            (sesman-quit-session 'beads sesman-session)
            ;; Should have called stop with the session ID
            (should (equal stop-called "session-123")))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-quit-session-nil-beads-session ()
  "Test sesman-quit-session handles nil beads session gracefully."
  (let ((stop-called nil))
    (cl-letf (((symbol-function 'beads-agent-stop)
               (lambda (_) (setq stop-called t))))
      ;; Session with nil at position 2
      (sesman-quit-session 'beads '("name" handle nil))
      ;; Should not have called stop
      (should-not stop-called))))

(ert-deftest beads-sesman-test-restart-session ()
  "Test sesman-restart-session quits and restarts with same issue."
  (beads-sesman-test--setup)
  (unwind-protect
      (let ((quit-called nil)
            (timer-fn nil)
            (timer-delay nil))
        (cl-letf (((symbol-function 'sesman-quit-session)
                   (lambda (system session)
                     (setq quit-called (list system session))))
                  ((symbol-function 'run-at-time)
                   (lambda (delay _repeat fn)
                     (setq timer-delay delay)
                     (setq timer-fn fn)
                     'mock-timer)))
          (let ((sesman-session (beads-sesman--make-sesman-session
                                  beads-sesman-test--mock-session)))
            (sesman-restart-session 'beads sesman-session)
            ;; Should have called quit
            (should quit-called)
            (should (eq (car quit-called) 'beads))
            ;; Should have scheduled restart
            (should timer-fn)
            (should (= timer-delay 0.5))
            ;; Simulate timer firing
            (let ((agent-start-called nil))
              (cl-letf (((symbol-function 'beads-agent-start)
                         (lambda (issue-id)
                           (setq agent-start-called issue-id))))
                (funcall timer-fn)
                ;; Should restart with same issue ID
                (should (equal agent-start-called "test-123")))))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-restart-session-nil-beads-session ()
  "Test sesman-restart-session handles nil beads session gracefully."
  (let ((quit-called nil))
    (cl-letf (((symbol-function 'sesman-quit-session)
               (lambda (_system _session) (setq quit-called t))))
      ;; Session with nil at position 2
      (sesman-restart-session 'beads '("name" handle nil))
      ;; Should not have called quit
      (should-not quit-called))))

(ert-deftest beads-sesman-test-project ()
  "Test sesman-project returns project root."
  (cl-letf (((symbol-function 'beads--find-project-root)
             (lambda () "/path/to/project")))
    (should (equal (sesman-project 'beads) "/path/to/project"))))

(ert-deftest beads-sesman-test-project-nil ()
  "Test sesman-project handles nil project root."
  (cl-letf (((symbol-function 'beads--find-project-root)
             (lambda () nil)))
    (should (null (sesman-project 'beads)))))

;;; Tests for User Commands

(ert-deftest beads-sesman-test-quit-no-session ()
  "Test beads-sesman-quit errors when no session."
  (cl-letf (((symbol-function 'sesman-current-session)
             (lambda (_system) nil)))
    (should-error (beads-sesman-quit) :type 'user-error)))

(ert-deftest beads-sesman-test-restart-no-session ()
  "Test beads-sesman-restart errors when no session."
  (cl-letf (((symbol-function 'sesman-current-session)
             (lambda (_system) nil)))
    (should-error (beads-sesman-restart) :type 'user-error)))

;;; Tests for Keymap

(ert-deftest beads-sesman-test-keymap-defined ()
  "Test beads-sesman-map is defined and populated."
  (should (keymapp beads-sesman-map))
  ;; Check key bindings exist
  (should (lookup-key beads-sesman-map "s"))
  (should (lookup-key beads-sesman-map "q"))
  (should (lookup-key beads-sesman-map "r"))
  (should (lookup-key beads-sesman-map "b"))
  (should (lookup-key beads-sesman-map "i"))
  (should (lookup-key beads-sesman-map "l")))

(ert-deftest beads-sesman-test-keymap-bindings ()
  "Test beads-sesman-map has correct bindings."
  (should (eq (lookup-key beads-sesman-map "s") #'beads-sesman-start))
  (should (eq (lookup-key beads-sesman-map "q") #'beads-sesman-quit))
  (should (eq (lookup-key beads-sesman-map "r") #'beads-sesman-restart))
  (should (eq (lookup-key beads-sesman-map "b") #'beads-sesman-browser))
  (should (eq (lookup-key beads-sesman-map "i") #'beads-sesman-info))
  (should (eq (lookup-key beads-sesman-map "l") #'beads-sesman-link)))

;;; Tests for Hook Registration

(ert-deftest beads-sesman-test-hook-registered ()
  "Test state change hook is registered."
  (should (member #'beads-sesman--state-change-handler
                  beads-agent-state-change-hook)))

;;; Integration Tests
;;
;; These tests exercise the full session lifecycle using the real
;; sesman integration (not mocked), verifying that sessions flow
;; correctly through create → register → query → destroy.

(ert-deftest beads-sesman-integration-full-lifecycle ()
  "Integration test: full session lifecycle with real sesman.
Tests that beads-agent--create-session correctly registers with sesman
via the hook, and that query/destroy functions work correctly."
  ;; Save original sesman sessions to restore later
  (let ((original-sessions (copy-sequence (sesman-sessions 'beads))))
    (unwind-protect
        (let* ((test-issue-id "integration-test-issue")
               (test-project "/tmp/integration-test")
               ;; Create session - this runs hook which registers with sesman
               (session (beads-agent--create-session
                         test-issue-id
                         "mock-backend"
                         test-project
                         'mock-handle)))
          ;; Verify session was created
          (should (beads-agent-session-p session))
          (should (equal (oref session issue-id) test-issue-id))

          ;; Verify session is queryable via sesman
          (let ((found (beads-agent--get-session (oref session id))))
            (should found)
            (should (eq found session)))

          ;; Verify get-sessions-for-issue works
          (let ((issue-sessions (beads-agent--get-sessions-for-issue test-issue-id)))
            (should (= (length issue-sessions) 1))
            (should (eq (car issue-sessions) session)))

          ;; Verify get-all-sessions includes our session
          (let ((all-sessions (beads-agent--get-all-sessions)))
            (should (member session all-sessions)))

          ;; Destroy session - this runs hook which unregisters from sesman
          (beads-agent--destroy-session (oref session id))

          ;; Verify session is gone
          (should (null (beads-agent--get-session (oref session id))))
          (should (null (beads-agent--get-sessions-for-issue test-issue-id))))

      ;; Cleanup: remove any test sessions that might have leaked
      (dolist (sesman-session (sesman-sessions 'beads))
        (unless (member sesman-session original-sessions)
          (sesman-unregister 'beads sesman-session))))))

(ert-deftest beads-sesman-integration-multiple-sessions ()
  "Integration test: multiple sessions for different issues."
  (let ((original-sessions (copy-sequence (sesman-sessions 'beads))))
    (unwind-protect
        (let* ((s1 (beads-agent--create-session
                    "issue-1" "mock" "/tmp/p1" 'h1))
               (s2 (beads-agent--create-session
                    "issue-2" "mock" "/tmp/p2" 'h2))
               (s3 (beads-agent--create-session
                    "issue-1" "mock" "/tmp/p3" 'h3)))  ; Same issue as s1

          ;; Verify all sessions queryable
          (should (= (length (beads-agent--get-all-sessions))
                     (+ (length original-sessions) 3)))

          ;; Verify issue-1 has two sessions
          (let ((issue-1-sessions (beads-agent--get-sessions-for-issue "issue-1")))
            (should (= (length issue-1-sessions) 2))
            (should (member s1 issue-1-sessions))
            (should (member s3 issue-1-sessions)))

          ;; Verify issue-2 has one session
          (let ((issue-2-sessions (beads-agent--get-sessions-for-issue "issue-2")))
            (should (= (length issue-2-sessions) 1))
            (should (eq (car issue-2-sessions) s2)))

          ;; Destroy one session from issue-1
          (beads-agent--destroy-session (oref s1 id))
          (should (= (length (beads-agent--get-sessions-for-issue "issue-1")) 1))

          ;; Destroy remaining sessions
          (beads-agent--destroy-session (oref s2 id))
          (beads-agent--destroy-session (oref s3 id))

          ;; Verify all test sessions gone
          (should (null (beads-agent--get-sessions-for-issue "issue-1")))
          (should (null (beads-agent--get-sessions-for-issue "issue-2"))))

      ;; Cleanup
      (dolist (sesman-session (sesman-sessions 'beads))
        (unless (member sesman-session original-sessions)
          (sesman-unregister 'beads sesman-session))))))

(provide 'beads-sesman-test)
;;; beads-sesman-test.el ends here
