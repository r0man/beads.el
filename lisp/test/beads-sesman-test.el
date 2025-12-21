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
(require 'beads-agent)

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

(defvar beads-sesman-test--session-counter 0
  "Counter for generating unique session numbers in tests.")

(defun beads-sesman-test--make-mock-session (&optional issue-id project-dir worktree-dir session-num)
  "Create a mock beads-agent-session.
ISSUE-ID defaults to \"test-123\".
PROJECT-DIR defaults to \"/tmp/project\".
WORKTREE-DIR is optional worktree directory.
SESSION-NUM is the session number (defaults to auto-incrementing counter)."
  (let* ((issue (or issue-id "test-123"))
         (num (or session-num (cl-incf beads-sesman-test--session-counter)))
         (session-id (format "%s#%d" issue num)))
    (beads-agent-session
     :id session-id
     :issue-id issue
     :backend-name "mock"
     :project-dir (or project-dir "/tmp/project")
     :worktree-dir worktree-dir
     :started-at "2025-01-01T12:00:00+0000"
     :backend-session 'mock-handle)))

(defun beads-sesman-test--setup ()
  "Set up test fixtures."
  (setq beads-sesman-test--session-counter 0)
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
        ;; Format is now session-id@path where session-id is issue-id#N
        (should (string-match-p "^test-123#1@" name))
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
        ;; Format is now session-id@path where session-id is issue-id#N
        (should (string-match-p "^issue-456#" name))
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
          ;; Fallback uses session-id (issue-id#N format)
          (should (string-match-p "^test-123#1@" name))))
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
        ;; Default uses session-id (issue-id#N format)
        (should (string-match-p "^test-123#1@" name))
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
        ;; Uses session-id (issue-id#N format)
        (should (string-match-p "^bd-42#" name))
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

(ert-deftest beads-sesman-test-session-name-numbered-format ()
  "Test that session names use numbered format for multiple sessions."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((session1 (beads-sesman-test--make-mock-session "bd-1" "/tmp" nil 1))
             (session2 (beads-sesman-test--make-mock-session "bd-1" "/tmp" nil 2))
             (session3 (beads-sesman-test--make-mock-session "bd-2" "/tmp" nil 1))
             (mock-backend (beads-sesman-test--backend))
             (name1 (beads-agent-backend-session-name mock-backend session1))
             (name2 (beads-agent-backend-session-name mock-backend session2))
             (name3 (beads-agent-backend-session-name mock-backend session3)))
        ;; Same issue, different session numbers
        (should (string-match-p "^bd-1#1@" name1))
        (should (string-match-p "^bd-1#2@" name2))
        ;; Different issue, session #1
        (should (string-match-p "^bd-2#1@" name3))
        ;; Names for same issue should be distinguishable
        (should-not (equal name1 name2)))
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
  (let ((types (sesman-context-types beads-sesman-system)))
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
        (should (sesman-more-relevant-p beads-sesman-system newer-sesman older-sesman))
        ;; Older should NOT be more relevant than newer
        (should-not (sesman-more-relevant-p beads-sesman-system older-sesman newer-sesman)))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-more-relevant-p-nil-sessions ()
  "Test sesman-more-relevant-p handles nil beads sessions."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((valid-session (beads-sesman-test--make-mock-session))
             (valid-sesman (beads-sesman--make-sesman-session valid-session))
             (nil-sesman '("name" handle nil)))
        ;; Both combinations with nil should return nil
        (should-not (sesman-more-relevant-p beads-sesman-system nil-sesman valid-sesman))
        (should-not (sesman-more-relevant-p beads-sesman-system valid-sesman nil-sesman))
        ;; Both nil should return nil
        (should-not (sesman-more-relevant-p beads-sesman-system nil-sesman nil-sesman)))
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
        (should-not (sesman-more-relevant-p beads-sesman-system sesman1 sesman2)))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-session-info ()
  "Test sesman-session-info returns display info."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((sesman-session (beads-sesman--make-sesman-session
                              beads-sesman-test--mock-session))
             (info (sesman-session-info beads-sesman-system sesman-session)))
        (should (plist-get info :objects))
        (should (plist-get info :strings))
        ;; Check strings contain expected info
        (let ((strings (plist-get info :strings)))
          (should (cl-some (lambda (s) (and s (string-match-p "Session:" s)))
                           strings))
          (should (cl-some (lambda (s) (and s (string-match-p "Issue:" s)))
                           strings))
          (should (cl-some (lambda (s) (and s (string-match-p "Started:" s)))
                           strings))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-session-info-with-type ()
  "Test sesman-session-info includes Type when agent-type-name is set."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((session (beads-agent-session
                       :id "test-123#1"
                       :issue-id "test-123"
                       :backend-name "mock"
                       :agent-type-name "Task"
                       :project-dir "/tmp/project"
                       :started-at "2025-01-01T12:00:00+0000"
                       :backend-session 'mock-handle))
             (sesman-session (beads-sesman--make-sesman-session session))
             (info (sesman-session-info beads-sesman-system sesman-session)))
        (should (plist-get info :strings))
        ;; Check Type is included when agent-type-name is set
        (let ((strings (plist-get info :strings)))
          (should (cl-some (lambda (s) (and s (string-match-p "Type: Task" s)))
                           strings))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-session-info-nil-beads-session ()
  "Test sesman-session-info handles nil beads session."
  (let ((info (sesman-session-info beads-sesman-system '("name" handle nil))))
    (should (null info))))

(ert-deftest beads-sesman-test-session-info-with-worktree ()
  "Test sesman-session-info includes worktree when present."
  (beads-sesman-test--setup)
  (unwind-protect
      (let* ((session (beads-sesman-test--make-mock-session
                       "test-id" "/tmp/main" "/tmp/worktree/test-id"))
             (sesman-session (beads-sesman--make-sesman-session session))
             (info (sesman-session-info beads-sesman-system sesman-session)))
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
      (let ((result (sesman-start-session beads-sesman-system)))
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
            (sesman-quit-session beads-sesman-system sesman-session)
            ;; Should have called stop with the session ID (now in issue-id#N format)
            (should (equal stop-called "test-123#1")))))
    (beads-sesman-test--teardown)))

(ert-deftest beads-sesman-test-quit-session-nil-beads-session ()
  "Test sesman-quit-session handles nil beads session gracefully."
  (let ((stop-called nil))
    (cl-letf (((symbol-function 'beads-agent-stop)
               (lambda (_) (setq stop-called t))))
      ;; Session with nil at position 2
      (sesman-quit-session beads-sesman-system '("name" handle nil))
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
            (sesman-restart-session beads-sesman-system sesman-session)
            ;; Should have called quit
            (should quit-called)
            (should (eq (car quit-called) beads-sesman-system))
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
      (sesman-restart-session beads-sesman-system '("name" handle nil))
      ;; Should not have called quit
      (should-not quit-called))))

(ert-deftest beads-sesman-test-project ()
  "Test sesman-project returns project root."
  (cl-letf (((symbol-function 'beads--find-project-root)
             (lambda () "/path/to/project")))
    (should (equal (sesman-project beads-sesman-system) "/path/to/project"))))

(ert-deftest beads-sesman-test-project-nil ()
  "Test sesman-project handles nil project root."
  (cl-letf (((symbol-function 'beads--find-project-root)
             (lambda () nil)))
    (should (null (sesman-project beads-sesman-system)))))

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
  (should (lookup-key beads-sesman-map "l")))

(ert-deftest beads-sesman-test-keymap-bindings ()
  "Test beads-sesman-map has correct bindings."
  (should (eq (lookup-key beads-sesman-map "s") #'beads-sesman-start))
  (should (eq (lookup-key beads-sesman-map "q") #'beads-sesman-quit))
  (should (eq (lookup-key beads-sesman-map "r") #'beads-sesman-restart))
  (should (eq (lookup-key beads-sesman-map "b") #'beads-sesman-browser))
  (should (eq (lookup-key beads-sesman-map "l") #'beads-sesman-link)))

;;; Tests for Hook Registration

(ert-deftest beads-sesman-test-hook-always-registered ()
  "Test sesman hook is registered when beads-sesman is loaded."
  ;; The hook should always be registered (added at load time)
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
  ;; Save original sessions to restore later
  (let ((original-sessions (copy-sequence (sesman-sessions beads-sesman-system))))
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
      (dolist (sesman-session (sesman-sessions beads-sesman-system))
        (unless (member sesman-session original-sessions)
          (sesman-unregister beads-sesman-system sesman-session))))))

(ert-deftest beads-sesman-integration-multiple-sessions ()
  "Integration test: multiple sessions for different issues."
  (let ((original-sessions (copy-sequence (sesman-sessions beads-sesman-system))))
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
      (dolist (sesman-session (sesman-sessions beads-sesman-system))
        (unless (member sesman-session original-sessions)
          (sesman-unregister beads-sesman-system sesman-session))))))

;;; =========================================================================
;;; Buffer Kill Cleanup Tests (Issue beads.el-xtqv)
;;; =========================================================================

;; These tests verify that agent sessions are properly cleaned up when
;; their buffers are killed manually (e.g., via C-x k or kill-buffer).

(ert-deftest beads-sesman-test-buffer-session-id-set-on-register ()
  "Test that buffer-local session ID is set when registering with buffer."
  (let ((original-sessions (copy-sequence (sesman-sessions beads-sesman-system)))
        (test-buffer (generate-new-buffer "*test-agent-buffer*")))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--get-backend)
                   (lambda (_name) 'mock-backend))
                  ((symbol-function 'beads-agent-backend-get-buffer)
                   (lambda (_backend _session) test-buffer)))
          ;; Create session - this will register and link buffer
          (let* ((session (beads-agent--create-session
                           "test-issue" "mock" "/tmp/project" 'handle)))
            ;; Verify buffer-local session ID is set
            (with-current-buffer test-buffer
              (should beads-sesman--buffer-session-id)
              (should (equal beads-sesman--buffer-session-id (oref session id))))
            ;; Cleanup
            (beads-agent--destroy-session (oref session id))))
      ;; Ensure buffer is killed and cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (dolist (sesman-session (sesman-sessions beads-sesman-system))
        (unless (member sesman-session original-sessions)
          (sesman-unregister beads-sesman-system sesman-session))))))

(ert-deftest beads-sesman-test-buffer-session-id-cleared-on-unregister ()
  "Test that buffer-local session ID is cleared when unregistering."
  (let ((original-sessions (copy-sequence (sesman-sessions beads-sesman-system)))
        (test-buffer (generate-new-buffer "*test-agent-buffer*")))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--get-backend)
                   (lambda (_name) 'mock-backend))
                  ((symbol-function 'beads-agent-backend-get-buffer)
                   (lambda (_backend _session) test-buffer)))
          (let* ((session (beads-agent--create-session
                           "test-issue" "mock" "/tmp/project" 'handle)))
            ;; Session ID should be set
            (with-current-buffer test-buffer
              (should beads-sesman--buffer-session-id))
            ;; Destroy session - this should clear buffer session ID
            (beads-agent--destroy-session (oref session id))
            ;; Session ID should be cleared
            (with-current-buffer test-buffer
              (should (null beads-sesman--buffer-session-id)))))
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (dolist (sesman-session (sesman-sessions beads-sesman-system))
        (unless (member sesman-session original-sessions)
          (sesman-unregister beads-sesman-system sesman-session))))))

(ert-deftest beads-sesman-test-kill-buffer-hook-installed ()
  "Test that kill-buffer-hook is installed for agent buffer."
  (let ((original-sessions (copy-sequence (sesman-sessions beads-sesman-system)))
        (test-buffer (generate-new-buffer "*test-agent-buffer*")))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--get-backend)
                   (lambda (_name) 'mock-backend))
                  ((symbol-function 'beads-agent-backend-get-buffer)
                   (lambda (_backend _session) test-buffer)))
          (let* ((session (beads-agent--create-session
                           "test-issue" "mock" "/tmp/project" 'handle)))
            ;; Verify kill-buffer-hook is installed
            (with-current-buffer test-buffer
              (should (memq #'beads-sesman--buffer-kill-handler
                            kill-buffer-hook)))
            ;; Cleanup normally
            (beads-agent--destroy-session (oref session id))))
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (dolist (sesman-session (sesman-sessions beads-sesman-system))
        (unless (member sesman-session original-sessions)
          (sesman-unregister beads-sesman-system sesman-session))))))

(ert-deftest beads-sesman-test-kill-buffer-hook-removed-on-unregister ()
  "Test that kill-buffer-hook is removed when unregistering session."
  (let ((original-sessions (copy-sequence (sesman-sessions beads-sesman-system)))
        (test-buffer (generate-new-buffer "*test-agent-buffer*")))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--get-backend)
                   (lambda (_name) 'mock-backend))
                  ((symbol-function 'beads-agent-backend-get-buffer)
                   (lambda (_backend _session) test-buffer)))
          (let* ((session (beads-agent--create-session
                           "test-issue" "mock" "/tmp/project" 'handle)))
            ;; Verify kill-buffer-hook is installed
            (with-current-buffer test-buffer
              (should (memq #'beads-sesman--buffer-kill-handler
                            kill-buffer-hook)))
            ;; Unregister session
            (beads-agent--destroy-session (oref session id))
            ;; Verify kill-buffer-hook is removed
            (with-current-buffer test-buffer
              (should-not (memq #'beads-sesman--buffer-kill-handler
                                kill-buffer-hook)))))
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (dolist (sesman-session (sesman-sessions beads-sesman-system))
        (unless (member sesman-session original-sessions)
          (sesman-unregister beads-sesman-system sesman-session))))))

(ert-deftest beads-sesman-test-buffer-kill-cleans-up-session ()
  "Test that killing agent buffer triggers session cleanup."
  (let ((original-sessions (copy-sequence (sesman-sessions beads-sesman-system)))
        (test-buffer (generate-new-buffer "*test-agent-buffer*"))
        (stop-called nil)
        (stopped-session-id nil))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--get-backend)
                   (lambda (_name) 'mock-backend))
                  ((symbol-function 'beads-agent-backend-get-buffer)
                   (lambda (_backend _session) test-buffer))
                  ((symbol-function 'beads-agent-stop)
                   (lambda (session-id)
                     (setq stop-called t)
                     (setq stopped-session-id session-id))))
          (let* ((session (beads-agent--create-session
                           "test-issue" "mock" "/tmp/project" 'handle))
                 (session-id (oref session id)))
            ;; Session should be registered
            (should (beads-agent--get-session session-id))
            ;; Kill the buffer - this should trigger cleanup
            (kill-buffer test-buffer)
            ;; Verify stop was called with correct session ID
            (should stop-called)
            (should (equal stopped-session-id session-id))))
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (dolist (sesman-session (sesman-sessions beads-sesman-system))
        (unless (member sesman-session original-sessions)
          (sesman-unregister beads-sesman-system sesman-session))))))

(ert-deftest beads-sesman-test-buffer-kill-after-stop-no-double-cleanup ()
  "Test that killing buffer after stop doesn't trigger double cleanup."
  (let ((original-sessions (copy-sequence (sesman-sessions beads-sesman-system)))
        (test-buffer (generate-new-buffer "*test-agent-buffer*"))
        (stop-call-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--get-backend)
                   (lambda (_name) 'mock-backend))
                  ((symbol-function 'beads-agent-backend-get-buffer)
                   (lambda (_backend _session) test-buffer))
                  ((symbol-function 'beads-agent-stop)
                   (lambda (_session-id)
                     (cl-incf stop-call-count))))
          (let* ((session (beads-agent--create-session
                           "test-issue" "mock" "/tmp/project" 'handle)))
            ;; Stop session normally - this clears buffer session ID
            (beads-agent--destroy-session (oref session id))
            ;; Now kill buffer - should NOT call stop again
            (kill-buffer test-buffer)
            ;; Stop should not have been called at all since we used
            ;; destroy-session directly (which doesn't call stop)
            ;; The important thing is that session ID was cleared
            ;; so the hook doesn't try to call stop
            (should (= stop-call-count 0))))
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (dolist (sesman-session (sesman-sessions beads-sesman-system))
        (unless (member sesman-session original-sessions)
          (sesman-unregister beads-sesman-system sesman-session))))))

(ert-deftest beads-sesman-test-buffer-kill-integration ()
  "Integration test: killing buffer with session ID triggers full cleanup.
This test verifies the complete flow without mocking beads-agent-stop,
ensuring the real cleanup path is exercised."
  (let ((original-sessions (copy-sequence (sesman-sessions beads-sesman-system)))
        (original-agent-sessions (copy-sequence (beads-agent--get-all-sessions)))
        (test-buffer (generate-new-buffer "*test-agent-integration*")))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--get-backend)
                   (lambda (_name) 'mock-backend))
                  ((symbol-function 'beads-agent-backend-get-buffer)
                   (lambda (_backend _session) test-buffer))
                  ;; Mock stop to track calls but still perform cleanup
                  ((symbol-function 'beads-agent-backend-stop)
                   (lambda (_backend _session) nil)))
          (let* ((session (beads-agent--create-session
                           "integration-test" "mock" "/tmp/project" 'handle))
                 (session-id (oref session id)))
            ;; Verify initial state
            (should (= 1 (length (beads-agent--get-all-sessions))))
            (should (beads-agent--get-session session-id))
            (with-current-buffer test-buffer
              (should beads-sesman--buffer-session-id)
              (should (equal beads-sesman--buffer-session-id session-id))
              (should (memq #'beads-sesman--buffer-kill-handler kill-buffer-hook)))
            ;; Kill the buffer - this should trigger the full cleanup chain:
            ;; kill-buffer-hook -> beads-sesman--buffer-kill-handler
            ;;   -> beads-agent-stop -> beads-agent--destroy-session
            ;;   -> beads-sesman--state-change-handler ('stopped)
            ;;   -> beads-sesman--unregister-session
            (kill-buffer test-buffer)
            ;; Verify session was cleaned up
            (should (= 0 (length (beads-agent--get-all-sessions))))
            (should (null (beads-agent--get-session session-id)))))
      ;; Cleanup - restore original state
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      ;; Cleanup any remaining sessions
      (dolist (session (beads-agent--get-all-sessions))
        (unless (member session original-agent-sessions)
          (beads-agent--destroy-session (oref session id))))
      (dolist (sesman-session (sesman-sessions beads-sesman-system))
        (unless (member sesman-session original-sessions)
          (sesman-unregister beads-sesman-system sesman-session))))))

(ert-deftest beads-sesman-test-register-handles-buffer-killed-during-registration ()
  "Test that registration handles buffer killed during the process.
This tests the race condition where buffer is killed between checks."
  (let* ((original-sessions (copy-sequence (sesman-sessions beads-sesman-system)))
         (original-agent-sessions (copy-sequence (beads-agent--get-all-sessions)))
         (test-buffer (generate-new-buffer "*test-race*"))
         (buffer-live-check-count 0)
         (original-buffer-live-p (symbol-function 'buffer-live-p)))
    (unwind-protect
        (cl-letf (((symbol-function 'beads-agent--get-backend)
                   (lambda (_name) 'mock-backend))
                  ((symbol-function 'beads-agent-backend-get-buffer)
                   (lambda (_backend _session) test-buffer))
                  ;; Mock buffer-live-p to kill buffer on second call
                  ;; (simulating race condition)
                  ((symbol-function 'buffer-live-p)
                   (lambda (buf)
                     (when (eq buf test-buffer)
                       (cl-incf buffer-live-check-count)
                       (when (= buffer-live-check-count 2)
                         ;; Kill buffer on second check to simulate race
                         (with-current-buffer buf
                           (let ((kill-buffer-hook nil)) ; Prevent hook
                             (set-buffer-modified-p nil)))
                         (kill-buffer buf)))
                     (funcall original-buffer-live-p buf))))
          ;; Create session - should not error even though buffer dies
          (let ((session (beads-agent--create-session
                          "race-test" "mock" "/tmp/project" 'handle)))
            ;; Session should still be created
            (should session)
            (should (beads-agent--get-session (oref session id)))
            ;; But buffer should be dead
            (should-not (buffer-live-p test-buffer))
            ;; Second check should have been made (our mock was called)
            (should (>= buffer-live-check-count 2))))
      ;; Cleanup
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (dolist (session (beads-agent--get-all-sessions))
        (unless (member session original-agent-sessions)
          (beads-agent--destroy-session (oref session id))))
      (dolist (sesman-session (sesman-sessions beads-sesman-system))
        (unless (member sesman-session original-sessions)
          (sesman-unregister beads-sesman-system sesman-session))))))

(ert-deftest beads-sesman-test-unregister-handles-missing-session ()
  "Test that unregister handles case where session is not found by name.
This can happen if session name changed between registration and
unregistration (e.g., path normalization differences)."
  (beads-sesman-test--setup)
  (unwind-protect
      (let ((warning-logged nil))
        (cl-letf (((symbol-function 'sesman-session)
                   (lambda (_system _name)
                     ;; Simulate session not found
                     nil))
                  ((symbol-function 'sesman-unregister)
                   (lambda (_system _session)
                     ;; Should not be called with nil
                     (error "sesman-unregister called with nil")))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when (string-match-p "session not found" fmt)
                       (setq warning-logged t)))))
          ;; Should not error when session not found
          (beads-sesman--unregister-session beads-sesman-test--mock-session)
          ;; Should have logged a warning
          (should warning-logged)))
    (beads-sesman-test--teardown)))

;;; =========================================================================
;;; Worktree Session Tests (beads.el-f25t)
;;; =========================================================================
;;
;; These tests verify the beads-worktree-session EIEIO class which groups
;; beads buffers by project directory.  Key principle: Directory is identity,
;; branch is metadata.

(defun beads-sesman-test--worktree-setup ()
  "Set up worktree session tests."
  (setq beads-sesman--worktree-sessions nil))

(defun beads-sesman-test--worktree-teardown ()
  "Tear down worktree session tests."
  (setq beads-sesman--worktree-sessions nil))

;;; Tests for beads-worktree-session class

(ert-deftest beads-sesman-worktree-session-class-instantiation ()
  "Test beads-worktree-session class can be instantiated."
  (let ((session (beads-worktree-session
                  :id "~/code/beads.el"
                  :project-dir "/home/user/code/beads.el"
                  :proj-name "beads.el"
                  :branch "main")))
    (should (beads-worktree-session-p session))
    (should (equal (oref session id) "~/code/beads.el"))
    (should (equal (oref session project-dir) "/home/user/code/beads.el"))
    (should (equal (oref session proj-name) "beads.el"))
    (should (equal (oref session branch) "main"))
    (should (null (oref session buffers)))
    (should (null (oref session agent-sessions)))))

(ert-deftest beads-sesman-worktree-session-optional-branch ()
  "Test beads-worktree-session can be created without branch."
  (let ((session (beads-worktree-session
                  :id "~/code/project"
                  :project-dir "/home/user/code/project"
                  :proj-name "project")))
    (should (beads-worktree-session-p session))
    (should (null (oref session branch)))))

;;; Tests for beads-worktree-session-empty-p

(ert-deftest beads-sesman-worktree-session-empty-p-new-session ()
  "Test empty-p returns t for new session with no buffers or agents."
  (let ((session (beads-worktree-session
                  :id "test"
                  :project-dir "/tmp/test"
                  :proj-name "test")))
    (should (beads-worktree-session-empty-p session))))

(ert-deftest beads-sesman-worktree-session-empty-p-with-buffers ()
  "Test empty-p returns nil when session has buffers."
  (let ((session (beads-worktree-session
                  :id "test"
                  :project-dir "/tmp/test"
                  :proj-name "test"
                  :buffers (list (current-buffer)))))
    (should-not (beads-worktree-session-empty-p session))))

(ert-deftest beads-sesman-worktree-session-empty-p-with-agents ()
  "Test empty-p returns nil when session has agent sessions."
  (let ((session (beads-worktree-session
                  :id "test"
                  :project-dir "/tmp/test"
                  :proj-name "test"
                  :agent-sessions (list 'mock-agent))))
    (should-not (beads-worktree-session-empty-p session))))

;;; Tests for beads-worktree-session buffer management

(ert-deftest beads-sesman-worktree-session-add-buffer ()
  "Test adding buffer to session."
  (let* ((test-buffer (generate-new-buffer "*test*"))
         (session (beads-worktree-session
                   :id "test"
                   :project-dir "/tmp/test"
                   :proj-name "test")))
    (unwind-protect
        (progn
          ;; First add should return t
          (should (beads-worktree-session-add-buffer session test-buffer))
          (should (memq test-buffer (oref session buffers)))
          ;; Second add should return nil (already present)
          (should-not (beads-worktree-session-add-buffer session test-buffer))
          (should (= 1 (length (oref session buffers)))))
      (kill-buffer test-buffer))))

(ert-deftest beads-sesman-worktree-session-remove-buffer ()
  "Test removing buffer from session."
  (let* ((test-buffer (generate-new-buffer "*test*"))
         (session (beads-worktree-session
                   :id "test"
                   :project-dir "/tmp/test"
                   :proj-name "test"
                   :buffers (list test-buffer))))
    (unwind-protect
        (progn
          ;; Remove should return t
          (should (beads-worktree-session-remove-buffer session test-buffer))
          (should-not (memq test-buffer (oref session buffers)))
          ;; Second remove should return nil (not present)
          (should-not (beads-worktree-session-remove-buffer session test-buffer)))
      (kill-buffer test-buffer))))

;;; Tests for beads-worktree-session-refresh-branch

(ert-deftest beads-sesman-worktree-session-refresh-branch ()
  "Test refreshing branch metadata."
  (let ((session (beads-worktree-session
                  :id "test"
                  :project-dir "/tmp/test"
                  :proj-name "test"
                  :branch "old-branch")))
    (cl-letf (((symbol-function 'beads--get-git-branch)
               (lambda () "new-branch")))
      (beads-worktree-session-refresh-branch session)
      (should (equal (oref session branch) "new-branch")))))

(ert-deftest beads-sesman-worktree-session-refresh-branch-nil ()
  "Test refresh-branch handles non-git directory."
  (let ((session (beads-worktree-session
                  :id "test"
                  :project-dir "/tmp/test"
                  :proj-name "test"
                  :branch "old-branch")))
    (cl-letf (((symbol-function 'beads--get-git-branch)
               (lambda () nil)))
      (beads-worktree-session-refresh-branch session)
      (should (null (oref session branch))))))

;;; Tests for session management functions

(ert-deftest beads-sesman-normalize-directory ()
  "Test directory normalization."
  ;; Basic normalization
  (let ((normalized (beads-sesman--normalize-directory "~/code/project")))
    (should (stringp normalized))
    (should (file-name-absolute-p normalized))
    ;; Should not contain ~
    (should-not (string-prefix-p "~" normalized))))

(ert-deftest beads-sesman-session-for-directory-not-found ()
  "Test finding session when none exists."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (should (null (beads-sesman--session-for-directory "/nonexistent")))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-session-for-directory-found ()
  "Test finding existing session by directory."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (let* ((dir "/tmp/test-project")
             (session (beads-worktree-session
                       :id (abbreviate-file-name dir)
                       :project-dir (beads-sesman--normalize-directory dir)
                       :proj-name "test-project")))
        (push session beads-sesman--worktree-sessions)
        (let ((found (beads-sesman--session-for-directory dir)))
          (should (eq found session))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-create-worktree-session ()
  "Test creating new worktree session."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--get-git-branch)
                 (lambda () "main")))
        (let* ((dir "/tmp/new-project")
               (session (beads-sesman--create-worktree-session dir)))
          ;; Session should be created
          (should (beads-worktree-session-p session))
          (should (equal (oref session proj-name) "new-project"))
          (should (equal (oref session branch) "main"))
          ;; Session should be in global list
          (should (member session beads-sesman--worktree-sessions))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-ensure-worktree-session-creates-new ()
  "Test ensure-worktree-session creates new session when none exists."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda () "/tmp/project"))
                ((symbol-function 'beads--get-git-branch)
                 (lambda () "feature")))
        (let ((session (beads-sesman--ensure-worktree-session)))
          (should (beads-worktree-session-p session))
          (should (equal (oref session proj-name) "project"))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-ensure-worktree-session-reuses-existing ()
  "Test ensure-worktree-session reuses existing session."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda () "/tmp/project"))
                ((symbol-function 'beads--get-git-branch)
                 (lambda () "main")))
        ;; Create first session
        (let ((session1 (beads-sesman--ensure-worktree-session)))
          (should (beads-worktree-session-p session1))
          ;; Ensure again should return same session
          (let ((session2 (beads-sesman--ensure-worktree-session)))
            (should (eq session1 session2))
            ;; Should only have one session in list
            (should (= 1 (length beads-sesman--worktree-sessions))))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-ensure-worktree-session-different-dirs ()
  "Test ensure-worktree-session creates different sessions for different dirs."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--get-git-branch)
                 (lambda () "main")))
        ;; Create session for dir1
        (cl-letf (((symbol-function 'beads--find-project-root)
                   (lambda () "/tmp/project1")))
          (beads-sesman--ensure-worktree-session))
        ;; Create session for dir2
        (cl-letf (((symbol-function 'beads--find-project-root)
                   (lambda () "/tmp/project2")))
          (beads-sesman--ensure-worktree-session))
        ;; Should have two sessions
        (should (= 2 (length beads-sesman--worktree-sessions))))
    (beads-sesman-test--worktree-teardown)))

;;; Tests for session cleanup

(ert-deftest beads-sesman-maybe-cleanup-worktree-session-empty ()
  "Test cleanup removes empty session."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (let ((session (beads-worktree-session
                      :id "test"
                      :project-dir "/tmp/test"
                      :proj-name "test")))
        (push session beads-sesman--worktree-sessions)
        (should (= 1 (length beads-sesman--worktree-sessions)))
        (beads-sesman--maybe-cleanup-worktree-session session)
        (should (= 0 (length beads-sesman--worktree-sessions))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-maybe-cleanup-worktree-session-not-empty ()
  "Test cleanup preserves non-empty session."
  (beads-sesman-test--worktree-setup)
  (let ((test-buffer (generate-new-buffer "*test*")))
    (unwind-protect
        (let ((session (beads-worktree-session
                        :id "test"
                        :project-dir "/tmp/test"
                        :proj-name "test"
                        :buffers (list test-buffer))))
          (push session beads-sesman--worktree-sessions)
          (should (= 1 (length beads-sesman--worktree-sessions)))
          (beads-sesman--maybe-cleanup-worktree-session session)
          ;; Session should still exist
          (should (= 1 (length beads-sesman--worktree-sessions))))
      (kill-buffer test-buffer)
      (beads-sesman-test--worktree-teardown))))

;;; Tests for finding session by buffer

(ert-deftest beads-sesman-buffer-worktree-session-not-found ()
  "Test finding session by buffer when not registered."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (should (null (beads-sesman--buffer-worktree-session (current-buffer))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-buffer-worktree-session-found ()
  "Test finding session by buffer when registered."
  (beads-sesman-test--worktree-setup)
  (let ((test-buffer (generate-new-buffer "*test*")))
    (unwind-protect
        (let ((session (beads-worktree-session
                        :id "test"
                        :project-dir "/tmp/test"
                        :proj-name "test"
                        :buffers (list test-buffer))))
          (push session beads-sesman--worktree-sessions)
          (let ((found (beads-sesman--buffer-worktree-session test-buffer)))
            (should (eq found session))))
      (kill-buffer test-buffer)
      (beads-sesman-test--worktree-teardown))))

;;; Directory-as-identity tests (critical behavioral tests)

(ert-deftest beads-sesman-same-dir-different-branch-same-session ()
  "Test that same directory with different branch uses same session.
This is the CRITICAL behavioral test for the directory-as-identity model."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (let ((dir "/tmp/project"))
        ;; Create session on 'main' branch
        (cl-letf (((symbol-function 'beads--find-project-root)
                   (lambda () dir))
                  ((symbol-function 'beads--get-git-branch)
                   (lambda () "main")))
          (let ((session1 (beads-sesman--ensure-worktree-session)))
            (should (equal (oref session1 branch) "main"))
            ;; Simulate branch switch - branch changes but directory doesn't
            (cl-letf (((symbol-function 'beads--get-git-branch)
                       (lambda () "feature")))
              ;; Ensure session should return SAME session (same directory)
              (let ((session2 (beads-sesman--ensure-worktree-session)))
                ;; CRITICAL: Same session object
                (should (eq session1 session2))
                ;; Branch metadata is NOT updated automatically by ensure
                ;; (would need explicit refresh)
                )))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-different-dir-same-branch-different-session ()
  "Test that different directories create different sessions.
Even if they have the same branch name."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--get-git-branch)
                 (lambda () "main")))
        ;; Create session for dir1
        (cl-letf (((symbol-function 'beads--find-project-root)
                   (lambda () "/tmp/project1")))
          (let ((session1 (beads-sesman--ensure-worktree-session)))
            ;; Create session for dir2
            (cl-letf (((symbol-function 'beads--find-project-root)
                       (lambda () "/tmp/project2")))
              (let ((session2 (beads-sesman--ensure-worktree-session)))
                ;; CRITICAL: Different session objects
                (should-not (eq session1 session2))
                ;; Both have same branch name
                (should (equal (oref session1 branch) "main"))
                (should (equal (oref session2 branch) "main")))))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-worktree-separate-from-main ()
  "Test that worktrees (different directories) get separate sessions."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--get-git-branch)
                 (lambda ()
                   ;; Return branch based on directory
                   (if (string-match-p "worktree" default-directory)
                       "feature"
                     "main"))))
        ;; Create session for main repo
        (cl-letf (((symbol-function 'beads--find-project-root)
                   (lambda () "/tmp/project")))
          (let ((main-session (beads-sesman--ensure-worktree-session)))
            (should (equal (oref main-session proj-name) "project"))
            ;; Create session for worktree (different directory!)
            (cl-letf (((symbol-function 'beads--find-project-root)
                       (lambda () "/tmp/project-worktree/feature")))
              (let ((worktree-session (beads-sesman--ensure-worktree-session)))
                ;; Different sessions (different directories)
                (should-not (eq main-session worktree-session))
                (should (equal (oref worktree-session proj-name) "feature"))
                ;; Both sessions exist
                (should (= 2 (length beads-sesman--worktree-sessions))))))))
    (beads-sesman-test--worktree-teardown)))

;;; =========================================================================
;;; Agent Session Integration with Worktree Sessions (beads.el-lfgz)
;;; =========================================================================
;;
;; These tests verify agent session integration with worktree sessions.
;; Agent sessions are tracked within their parent project session.

(ert-deftest beads-sesman-add-agent-to-worktree ()
  "Test that adding agent session creates/uses worktree session."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda () "/tmp/test-project"))
                ((symbol-function 'beads--get-git-branch)
                 (lambda () "main"))
                ((symbol-function 'beads--get-project-name)
                 (lambda () "test-project")))
        (let ((agent-session (beads-agent-session
                              :id "test#1"
                              :project-dir "/tmp/test-project"
                              :proj-name "test-project"
                              :backend-name "test"
                              :started-at "2025-01-01T00:00:00Z")))
          (beads-sesman--add-agent-to-worktree agent-session)
          ;; Worktree session should exist
          (should (= 1 (length beads-sesman--worktree-sessions)))
          ;; Agent should be in worktree session
          (let ((wt-session (car beads-sesman--worktree-sessions)))
            (should (memq agent-session (oref wt-session agent-sessions))))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-remove-agent-from-worktree ()
  "Test that removing agent session cleans up properly."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda () "/tmp/test-project"))
                ((symbol-function 'beads--get-git-branch)
                 (lambda () "main"))
                ((symbol-function 'beads--get-project-name)
                 (lambda () "test-project")))
        (let ((agent-session (beads-agent-session
                              :id "test#1"
                              :project-dir "/tmp/test-project"
                              :proj-name "test-project"
                              :backend-name "test"
                              :started-at "2025-01-01T00:00:00Z")))
          (beads-sesman--add-agent-to-worktree agent-session)
          ;; Verify agent is in session
          (let ((wt-session (car beads-sesman--worktree-sessions)))
            (should (memq agent-session (oref wt-session agent-sessions))))
          ;; Remove agent
          (beads-sesman--remove-agent-from-worktree agent-session)
          ;; Worktree session should be cleaned up (empty)
          (should (null beads-sesman--worktree-sessions))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-multiple-agents-same-project ()
  "Test that multiple agents can be in the same worktree session."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda () "/tmp/test-project"))
                ((symbol-function 'beads--get-git-branch)
                 (lambda () "main"))
                ((symbol-function 'beads--get-project-name)
                 (lambda () "test-project")))
        (let ((agent1 (beads-agent-session
                       :id "test#1"
                       :project-dir "/tmp/test-project"
                       :proj-name "test-project"
                       :backend-name "test"
                       :started-at "2025-01-01T00:00:00Z"))
              (agent2 (beads-agent-session
                       :id "test#2"
                       :project-dir "/tmp/test-project"
                       :proj-name "test-project"
                       :backend-name "test"
                       :started-at "2025-01-01T00:01:00Z")))
          (beads-sesman--add-agent-to-worktree agent1)
          (beads-sesman--add-agent-to-worktree agent2)
          ;; Still only one worktree session
          (should (= 1 (length beads-sesman--worktree-sessions)))
          ;; Both agents in session
          (let ((wt-session (car beads-sesman--worktree-sessions)))
            (should (memq agent1 (oref wt-session agent-sessions)))
            (should (memq agent2 (oref wt-session agent-sessions)))
            (should (= 2 (length (oref wt-session agent-sessions)))))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-get-agents-for-project ()
  "Test getting agent sessions for a project directory."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda () "/tmp/test-project"))
                ((symbol-function 'beads--get-git-branch)
                 (lambda () "main"))
                ((symbol-function 'beads--get-project-name)
                 (lambda () "test-project")))
        (let ((agent (beads-agent-session
                      :id "test#1"
                      :project-dir "/tmp/test-project"
                      :proj-name "test-project"
                      :backend-name "test"
                      :started-at "2025-01-01T00:00:00Z")))
          (beads-sesman--add-agent-to-worktree agent)
          ;; Should find the agent
          (let ((agents (beads-sesman--get-agents-for-project "/tmp/test-project")))
            (should (= 1 (length agents)))
            (should (memq agent agents)))
          ;; Should not find for different project
          (should (null (beads-sesman--get-agents-for-project "/tmp/other")))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-agent-worktree-session-found ()
  "Test finding worktree session by agent."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda () "/tmp/test-project"))
                ((symbol-function 'beads--get-git-branch)
                 (lambda () "main"))
                ((symbol-function 'beads--get-project-name)
                 (lambda () "test-project")))
        (let ((agent (beads-agent-session
                      :id "test#1"
                      :project-dir "/tmp/test-project"
                      :proj-name "test-project"
                      :backend-name "test"
                      :started-at "2025-01-01T00:00:00Z")))
          (beads-sesman--add-agent-to-worktree agent)
          (let ((found (beads-sesman--agent-worktree-session agent)))
            (should found)
            (should (memq agent (oref found agent-sessions))))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-remove-one-agent-keeps-others ()
  "Test that removing one agent doesn't affect others in session."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda () "/tmp/test-project"))
                ((symbol-function 'beads--get-git-branch)
                 (lambda () "main"))
                ((symbol-function 'beads--get-project-name)
                 (lambda () "test-project")))
        (let ((agent1 (beads-agent-session
                       :id "test#1"
                       :project-dir "/tmp/test-project"
                       :proj-name "test-project"
                       :backend-name "test"
                       :started-at "2025-01-01T00:00:00Z"))
              (agent2 (beads-agent-session
                       :id "test#2"
                       :project-dir "/tmp/test-project"
                       :proj-name "test-project"
                       :backend-name "test"
                       :started-at "2025-01-01T00:01:00Z")))
          (beads-sesman--add-agent-to-worktree agent1)
          (beads-sesman--add-agent-to-worktree agent2)
          ;; Remove first agent
          (beads-sesman--remove-agent-from-worktree agent1)
          ;; Session should still exist with agent2
          (should (= 1 (length beads-sesman--worktree-sessions)))
          (let ((wt-session (car beads-sesman--worktree-sessions)))
            (should (memq agent2 (oref wt-session agent-sessions)))
            (should-not (memq agent1 (oref wt-session agent-sessions))))))
    (beads-sesman-test--worktree-teardown)))

(ert-deftest beads-sesman-add-agent-no-duplicate ()
  "Test that adding same agent twice doesn't duplicate in session."
  (beads-sesman-test--worktree-setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads--find-project-root)
                 (lambda () "/tmp/test-project"))
                ((symbol-function 'beads--get-git-branch)
                 (lambda () "main"))
                ((symbol-function 'beads--get-project-name)
                 (lambda () "test-project")))
        (let ((agent (beads-agent-session
                      :id "test#1"
                      :project-dir "/tmp/test-project"
                      :proj-name "test-project"
                      :backend-name "test"
                      :started-at "2025-01-01T00:00:00Z")))
          ;; Add twice
          (beads-sesman--add-agent-to-worktree agent)
          (beads-sesman--add-agent-to-worktree agent)
          ;; Should only appear once
          (let ((wt-session (car beads-sesman--worktree-sessions)))
            (should (= 1 (length (oref wt-session agent-sessions)))))))
    (beads-sesman-test--worktree-teardown)))

(provide 'beads-sesman-test)
;;; beads-sesman-test.el ends here
