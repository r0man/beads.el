;;; beads-worktree-session-test.el --- Tests for worktree sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Tests for the in-house agent session registry
;; (`beads-agent-backend.el'), the per-directory worktree-buffer grouping
;; (`beads-worktree-session.el'), and the context lookup used by the
;; session commands (`beads-agent-session.el').
;;
;; Key principle for worktree sessions: directory is identity, branch is
;; metadata.

;;; Code:

(require 'ert)
(require 'beads-agent)
(require 'beads-agent-session)
(require 'beads-worktree-session)
(require 'beads-agent-backend)

;;; =========================================================================
;;; Agent session registry (beads-agent-backend.el)
;;; =========================================================================

(defun beads-worktree-session-test--make-session (&rest args)
  "Make a beads-agent-session with ARGS, filling in defaults."
  (apply #'beads-agent-session
         (append
          args
          (unless (plist-member args :project-dir)
            (list :project-dir "/tmp/project"))
          (unless (plist-member args :proj-name)
            (list :proj-name "project"))
          (unless (plist-member args :backend-name)
            (list :backend-name "test"))
          (unless (plist-member args :started-at)
            (list :started-at "2025-01-01T00:00:00Z")))))

(ert-deftest beads-worktree-session-registry-register ()
  "Registering sessions makes them visible to `beads-agent--all-sessions'."
  (beads-agent--clear-sessions)
  (unwind-protect
      (let ((session (beads-worktree-session-test--make-session)))
        (beads-agent--register-session session)
        (should (= 1 (length (beads-agent--all-sessions))))
        ;; Registering twice does not duplicate
        (beads-agent--register-session session)
        (should (= 1 (length (beads-agent--all-sessions))))
        (should (eq session (car (beads-agent--all-sessions)))))
    (beads-agent--clear-sessions)))

(ert-deftest beads-worktree-session-registry-unregister ()
  "Unregistering removes the session and reports presence."
  (beads-agent--clear-sessions)
  (unwind-protect
      (let ((session (beads-worktree-session-test--make-session)))
        (beads-agent--register-session session)
        (should (beads-agent--unregister-session session))
        (should (null (beads-agent--all-sessions)))
        ;; Removing again returns nil
        (should-not (beads-agent--unregister-session session)))
    (beads-agent--clear-sessions)))

(ert-deftest beads-worktree-session-registry-clear ()
  "Clearing empties the registry."
  (beads-agent--register-session (beads-worktree-session-test--make-session))
  (beads-agent--clear-sessions)
  (should (null (beads-agent--all-sessions))))

(ert-deftest beads-worktree-session-registry-get-session ()
  "`beads-agent--get-session' finds by id."
  (beads-agent--clear-sessions)
  (unwind-protect
      (let ((a (beads-worktree-session-test--make-session :id "a#1"))
            (b (beads-worktree-session-test--make-session :id "b#1")))
        (beads-agent--register-session a)
        (beads-agent--register-session b)
        (should (eq a (beads-agent--get-session "a#1")))
        (should (eq b (beads-agent--get-session "b#1")))
        (should (null (beads-agent--get-session "missing#1"))))
    (beads-agent--clear-sessions)))

(ert-deftest beads-worktree-session-registry-get-sessions-for-issue ()
  "`beads-agent--get-sessions-for-issue' filters by issue-id."
  (beads-agent--clear-sessions)
  (unwind-protect
      (let ((a (beads-worktree-session-test--make-session
                :id "a#1" :issue-id "bd-1"))
            (b (beads-worktree-session-test--make-session
                :id "b#1" :issue-id "bd-2"))
            (c (beads-worktree-session-test--make-session
                :id "c#1" :issue-id "bd-1")))
        (mapc #'beads-agent--register-session (list a b c))
        (let ((found (beads-agent--get-sessions-for-issue "bd-1")))
          (should (= 2 (length found)))
          (should (memq a found))
          (should (memq c found))
          (should-not (memq b found))))
    (beads-agent--clear-sessions)))

(ert-deftest beads-worktree-session-registry-get-sessions-for-project ()
  "`beads-agent--get-sessions-for-project' normalizes the directory."
  (beads-agent--clear-sessions)
  (unwind-protect
      (let ((a (beads-worktree-session-test--make-session
                :id "a#1" :project-dir "/tmp/project-a"))
            (b (beads-worktree-session-test--make-session
                :id "b#1" :project-dir "/tmp/project-b")))
        (mapc #'beads-agent--register-session (list a b))
        (should (eq a (car (beads-agent--get-sessions-for-project
                            "/tmp/project-a"))))
        (should (null (beads-agent--get-sessions-for-project "/tmp/project-b/c"))))
    (beads-agent--clear-sessions)))

(ert-deftest beads-worktree-session-registry-touching-issue ()
  "`beads-agent--get-sessions-touching-issue' uses touched-issues."
  (beads-agent--clear-sessions)
  (unwind-protect
      (let ((a (beads-worktree-session-test--make-session
                :id "a#1" :touched-issues '("bd-1" "bd-2")))
            (b (beads-worktree-session-test--make-session
                :id "b#1" :touched-issues '("bd-3"))))
        (mapc #'beads-agent--register-session (list a b))
        (let ((found (beads-agent--get-sessions-touching-issue "bd-2")))
          (should (= 1 (length found)))
          (should (eq a (car found)))))
    (beads-agent--clear-sessions)))

(ert-deftest beads-worktree-session-registry-focused-on-issue ()
  "`beads-agent--get-sessions-focused-on-issue' uses current-issue."
  (beads-agent--clear-sessions)
  (unwind-protect
      (let ((a (beads-worktree-session-test--make-session
                :id "a#1" :current-issue "bd-9"))
            (b (beads-worktree-session-test--make-session
                :id "b#1" :current-issue "bd-10")))
        (mapc #'beads-agent--register-session (list a b))
        (should (eq a (car (beads-agent--get-sessions-focused-on-issue "bd-9"))))
        (should (null (beads-agent--get-sessions-focused-on-issue "bd-11"))))
    (beads-agent--clear-sessions)))

(ert-deftest beads-worktree-session-registry-next-instance-number ()
  "Instance numbering scans the registry."
  (beads-agent--clear-sessions)
  (unwind-protect
      (progn
        (should (= 1 (beads-agent--next-project-instance-number "/tmp/project")))
        (beads-agent--register-session
         (beads-worktree-session-test--make-session
          :id "project#1" :project-dir "/tmp/project" :instance-number 1))
        (should (= 2 (beads-agent--next-project-instance-number "/tmp/project"))))
    (beads-agent--clear-sessions)))

(ert-deftest beads-worktree-session-registry-destroy-unregisters ()
  "`beads-agent--destroy-session' unregisters and runs the hook."
  (beads-agent--clear-sessions)
  (unwind-protect
      (let ((session (beads-worktree-session-test--make-session :id "gone#1"))
            (actions nil))
        (beads-agent--register-session session)
        (let ((beads-agent-state-change-hook
               (list (lambda (action _s) (push action actions)))))
          (beads-agent--destroy-session "gone#1"))
        (should (null (beads-agent--get-session "gone#1")))
        (should (equal actions '(stopped))))
    (beads-agent--clear-sessions)))

;;; =========================================================================
;;; Agent buffer cleanup (beads-agent-backend.el)
;;; =========================================================================

(ert-deftest beads-worktree-session-setup-session-buffer ()
  "`beads-agent--setup-session-buffer' labels the buffer and installs the hook."
  (beads-agent--clear-sessions)
  (let ((buffer (generate-new-buffer "*agent-test*")))
    (unwind-protect
        (let ((session (beads-worktree-session-test--make-session
                        :id "buf#1")))
          (beads-agent--setup-session-buffer session buffer)
          (with-current-buffer buffer
            (should (equal beads-agent--buffer-session-id "buf#1"))
            (should (memq #'beads-agent--buffer-kill-handler kill-buffer-hook))))
      (kill-buffer buffer))))

(ert-deftest beads-worktree-session-buffer-kill-stops-session ()
  "Killing a linked agent buffer stops its session."
  (beads-agent--clear-sessions)
  (let ((buffer (generate-new-buffer "*agent-test*"))
        (stopped nil))
    (unwind-protect
        (let ((session (beads-worktree-session-test--make-session
                        :id "kill#1")))
          (beads-agent--register-session session)
          (beads-agent--setup-session-buffer session buffer)
          (cl-letf (((symbol-function 'beads-agent-stop)
                     (lambda (id) (setq stopped id))))
            (kill-buffer buffer))
          (should (equal stopped "kill#1")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (beads-agent--clear-sessions))))

;;; =========================================================================
;;; Session context lookup (beads-agent-session.el)
;;; =========================================================================

(ert-deftest beads-agent-session-for-buffer ()
  "Context lookup finds the session owning a buffer."
  (beads-agent--clear-sessions)
  (let ((buffer (generate-new-buffer "*agent-ctx*")))
    (unwind-protect
        (let ((session (beads-worktree-session-test--make-session
                        :id "ctx#1")))
          (beads-agent-session-set-buffer session buffer)
          (beads-agent--register-session session)
          (should (eq session (beads-agent--session-for-buffer buffer))))
      (kill-buffer buffer)
      (beads-agent--clear-sessions))))

(ert-deftest beads-agent-session-for-directory ()
  "Context lookup matches the session working directory."
  (beads-agent--clear-sessions)
  (unwind-protect
      (let ((session (beads-worktree-session-test--make-session
                      :id "dir#1" :project-dir "/tmp/ctx-project")))
        (beads-agent--register-session session)
        (should (eq session (beads-agent--session-for-directory
                             "/tmp/ctx-project")))
        (should (null (beads-agent--session-for-directory "/tmp/other"))))
    (beads-agent--clear-sessions)))

;;; =========================================================================
;;; Worktree Session Tests
;;; =========================================================================

(defun beads-worktree-session-test--setup ()
  "Set up worktree session tests."
  (setq beads-worktree-session--sessions nil))

(defun beads-worktree-session-test--teardown ()
  "Tear down worktree session tests."
  (setq beads-worktree-session--sessions nil))

;;; Tests for beads-worktree-session class

(ert-deftest beads-worktree-session-class-instantiation ()
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

(ert-deftest beads-worktree-session-optional-branch ()
  "Test beads-worktree-session can be created without branch."
  (let ((session (beads-worktree-session
                  :id "~/code/project"
                  :project-dir "/home/user/code/project"
                  :proj-name "project")))
    (should (beads-worktree-session-p session))
    (should (null (oref session branch)))))

;;; Tests for beads-worktree-session-empty-p

(ert-deftest beads-worktree-session-empty-p-new-session ()
  "Test empty-p returns t for new session with no buffers or agents."
  (let ((session (beads-worktree-session
                  :id "test"
                  :project-dir "/tmp/test"
                  :proj-name "test")))
    (should (beads-worktree-session-empty-p session))))

(ert-deftest beads-worktree-session-empty-p-with-buffers ()
  "Test empty-p returns nil when session has buffers."
  (let ((session (beads-worktree-session
                  :id "test"
                  :project-dir "/tmp/test"
                  :proj-name "test"
                  :buffers (list (current-buffer)))))
    (should-not (beads-worktree-session-empty-p session))))

(ert-deftest beads-worktree-session-empty-p-with-agents ()
  "Test empty-p returns nil when session has agent sessions."
  (let ((session (beads-worktree-session
                  :id "test"
                  :project-dir "/tmp/test"
                  :proj-name "test"
                  :agent-sessions (list 'mock-agent))))
    (should-not (beads-worktree-session-empty-p session))))

;;; Tests for beads-worktree-session buffer management

(ert-deftest beads-worktree-session-add-buffer ()
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

(ert-deftest beads-worktree-session-remove-buffer ()
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

(ert-deftest beads-worktree-session-refresh-branch ()
  "Test refreshing branch metadata."
  (let ((session (beads-worktree-session
                  :id "test"
                  :project-dir "/tmp/test"
                  :proj-name "test"
                  :branch "old-branch")))
    (cl-letf (((symbol-function 'beads-git-get-branch)
               (lambda () "new-branch")))
      (beads-worktree-session-refresh-branch session)
      (should (equal (oref session branch) "new-branch")))))

(ert-deftest beads-worktree-session-refresh-branch-nil ()
  "Test refresh-branch handles non-git directory."
  (let ((session (beads-worktree-session
                  :id "test"
                  :project-dir "/tmp/test"
                  :proj-name "test"
                  :branch "old-branch")))
    (cl-letf (((symbol-function 'beads-git-get-branch)
               (lambda () nil)))
      (beads-worktree-session-refresh-branch session)
      (should (null (oref session branch))))))

;;; Tests for session management functions

(ert-deftest beads-worktree-session-normalize-directory ()
  "Test directory normalization."
  (let ((normalized (beads-worktree-session--normalize-directory
                     "~/code/project")))
    (should (stringp normalized))
    (should (file-name-absolute-p normalized))
    (should-not (string-prefix-p "~" normalized))))

(ert-deftest beads-worktree-session-for-directory-not-found ()
  "Test finding session when none exists."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (should (null (beads-worktree-session--for-directory "/nonexistent")))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-for-directory-found ()
  "Test finding existing session by directory."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (let* ((dir "/tmp/test-project")
             (session (beads-worktree-session
                       :id (abbreviate-file-name dir)
                       :project-dir (beads-worktree-session--normalize-directory dir)
                       :proj-name "test-project")))
        (push session beads-worktree-session--sessions)
        (let ((found (beads-worktree-session--for-directory dir)))
          (should (eq found session))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-create ()
  "Test creating new worktree session."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
        (let* ((dir "/tmp/new-project")
               (session (beads-worktree-session--create dir)))
          (should (beads-worktree-session-p session))
          (should (equal (oref session proj-name) "new-project"))
          (should (equal (oref session branch) "main"))
          (should (member session beads-worktree-session--sessions))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-ensure-creates-new ()
  "Test ensure creates new session when none exists."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-find-project-root)
                 (lambda () "/tmp/project"))
                ((symbol-function 'beads-git-get-branch)
                 (lambda () "feature")))
        (let ((session (beads-worktree-session-ensure)))
          (should (beads-worktree-session-p session))
          (should (equal (oref session proj-name) "project"))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-ensure-reuses-existing ()
  "Test ensure reuses existing session."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-find-project-root)
                 (lambda () "/tmp/project"))
                ((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
        (let ((session1 (beads-worktree-session-ensure)))
          (should (beads-worktree-session-p session1))
          (let ((session2 (beads-worktree-session-ensure)))
            (should (eq session1 session2))
            (should (= 1 (length beads-worktree-session--sessions))))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-ensure-different-dirs ()
  "Test ensure creates different sessions for different dirs."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/project1")))
          (beads-worktree-session-ensure))
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/project2")))
          (beads-worktree-session-ensure))
        (should (= 2 (length beads-worktree-session--sessions))))
    (beads-worktree-session-test--teardown)))

;;; Tests for session cleanup

(ert-deftest beads-worktree-session-cleanup-empty ()
  "Test cleanup removes empty session."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (let ((session (beads-worktree-session
                      :id "test"
                      :project-dir "/tmp/test"
                      :proj-name "test")))
        (push session beads-worktree-session--sessions)
        (should (= 1 (length beads-worktree-session--sessions)))
        (beads-worktree-session-cleanup session)
        (should (= 0 (length beads-worktree-session--sessions))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-cleanup-not-empty ()
  "Test cleanup preserves non-empty session."
  (beads-worktree-session-test--setup)
  (let ((test-buffer (generate-new-buffer "*test*")))
    (unwind-protect
        (let ((session (beads-worktree-session
                        :id "test"
                        :project-dir "/tmp/test"
                        :proj-name "test"
                        :buffers (list test-buffer))))
          (push session beads-worktree-session--sessions)
          (should (= 1 (length beads-worktree-session--sessions)))
          (beads-worktree-session-cleanup session)
          (should (= 1 (length beads-worktree-session--sessions))))
      (kill-buffer test-buffer)
      (beads-worktree-session-test--teardown))))

;;; Tests for finding session by buffer

(ert-deftest beads-worktree-session-for-buffer-not-found ()
  "Test finding session by buffer when not registered."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (should (null (beads-worktree-session-for-buffer (current-buffer))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-for-buffer-found ()
  "Test finding session by buffer when registered."
  (beads-worktree-session-test--setup)
  (let ((test-buffer (generate-new-buffer "*test*")))
    (unwind-protect
        (let ((session (beads-worktree-session
                        :id "test"
                        :project-dir "/tmp/test"
                        :proj-name "test"
                        :buffers (list test-buffer))))
          (push session beads-worktree-session--sessions)
          (let ((found (beads-worktree-session-for-buffer test-buffer)))
            (should (eq found session))))
      (kill-buffer test-buffer)
      (beads-worktree-session-test--teardown))))

;;; Directory-as-identity tests (critical behavioral tests)

(ert-deftest beads-worktree-session-same-dir-different-branch-same-session ()
  "Test that same directory with different branch uses same session."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (let ((dir "/tmp/project"))
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () dir))
                  ((symbol-function 'beads-git-get-branch)
                   (lambda () "main")))
          (let ((session1 (beads-worktree-session-ensure)))
            (should (equal (oref session1 branch) "main"))
            (cl-letf (((symbol-function 'beads-git-get-branch)
                       (lambda () "feature")))
              (let ((session2 (beads-worktree-session-ensure)))
                (should (eq session1 session2)))))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-different-dir-same-branch-different-session ()
  "Test that different directories create different sessions."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/project1")))
          (let ((session1 (beads-worktree-session-ensure)))
            (cl-letf (((symbol-function 'beads-git-find-project-root)
                       (lambda () "/tmp/project2")))
              (let ((session2 (beads-worktree-session-ensure)))
                (should-not (eq session1 session2))
                (should (equal (oref session1 branch) "main"))
                (should (equal (oref session2 branch) "main")))))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-separate-from-main ()
  "Test that worktrees (different directories) get separate sessions."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda ()
                   (if (string-match-p "worktree" default-directory)
                       "feature"
                     "main"))))
        (cl-letf (((symbol-function 'beads-git-find-project-root)
                   (lambda () "/tmp/project")))
          (let ((main-session (beads-worktree-session-ensure)))
            (should (equal (oref main-session proj-name) "project"))
            (cl-letf (((symbol-function 'beads-git-find-project-root)
                       (lambda () "/tmp/project-worktree/feature")))
              (let ((worktree-session (beads-worktree-session-ensure)))
                (should-not (eq main-session worktree-session))
                (should (equal (oref worktree-session proj-name) "feature"))
                (should (= 2 (length beads-worktree-session--sessions))))))))
    (beads-worktree-session-test--teardown)))

;;; =========================================================================
;;; Agent Session Integration with Worktree Sessions
;;; =========================================================================

(ert-deftest beads-worktree-session-add-agent ()
  "Test that adding agent session creates/uses worktree session."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
        (let ((agent-session (beads-agent-session
                              :id "test#1"
                              :project-dir "/tmp/test-project"
                              :proj-name "test-project"
                              :backend-name "test"
                              :started-at "2025-01-01T00:00:00Z")))
          (beads-worktree-session--add-agent agent-session)
          (should (= 1 (length beads-worktree-session--sessions)))
          (let ((wt-session (car beads-worktree-session--sessions)))
            (should (memq agent-session (oref wt-session agent-sessions))))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-remove-agent ()
  "Test that removing agent session cleans up properly."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
        (let ((agent-session (beads-agent-session
                              :id "test#1"
                              :project-dir "/tmp/test-project"
                              :proj-name "test-project"
                              :backend-name "test"
                              :started-at "2025-01-01T00:00:00Z")))
          (beads-worktree-session--add-agent agent-session)
          (let ((wt-session (car beads-worktree-session--sessions)))
            (should (memq agent-session (oref wt-session agent-sessions))))
          (beads-worktree-session--remove-agent agent-session)
          (should (null beads-worktree-session--sessions))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-multiple-agents-same-project ()
  "Test that multiple agents can be in the same worktree session."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
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
          (beads-worktree-session--add-agent agent1)
          (beads-worktree-session--add-agent agent2)
          (should (= 1 (length beads-worktree-session--sessions)))
          (let ((wt-session (car beads-worktree-session--sessions)))
            (should (memq agent1 (oref wt-session agent-sessions)))
            (should (memq agent2 (oref wt-session agent-sessions)))
            (should (= 2 (length (oref wt-session agent-sessions)))))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-agents-for-project ()
  "Test getting agent sessions for a project directory."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
        (let ((agent (beads-agent-session
                      :id "test#1"
                      :project-dir "/tmp/test-project"
                      :proj-name "test-project"
                      :backend-name "test"
                      :started-at "2025-01-01T00:00:00Z")))
          (beads-worktree-session--add-agent agent)
          (let ((agents (beads-worktree-session--agents-for-project
                         "/tmp/test-project")))
            (should (= 1 (length agents)))
            (should (memq agent agents)))
          (should (null (beads-worktree-session--agents-for-project "/tmp/other")))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-for-agent-found ()
  "Test finding worktree session by agent."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
        (let ((agent (beads-agent-session
                      :id "test#1"
                      :project-dir "/tmp/test-project"
                      :proj-name "test-project"
                      :backend-name "test"
                      :started-at "2025-01-01T00:00:00Z")))
          (beads-worktree-session--add-agent agent)
          (let ((found (beads-worktree-session--for-agent agent)))
            (should found)
            (should (memq agent (oref found agent-sessions))))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-remove-one-agent-keeps-others ()
  "Test that removing one agent doesn't affect others in session."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
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
          (beads-worktree-session--add-agent agent1)
          (beads-worktree-session--add-agent agent2)
          (beads-worktree-session--remove-agent agent1)
          (should (= 1 (length beads-worktree-session--sessions)))
          (let ((wt-session (car beads-worktree-session--sessions)))
            (should (memq agent2 (oref wt-session agent-sessions)))
            (should-not (memq agent1 (oref wt-session agent-sessions))))))
    (beads-worktree-session-test--teardown)))

(ert-deftest beads-worktree-session-add-agent-no-duplicate ()
  "Test that adding same agent twice doesn't duplicate in session."
  (beads-worktree-session-test--setup)
  (unwind-protect
      (cl-letf (((symbol-function 'beads-git-get-branch)
                 (lambda () "main")))
        (let ((agent (beads-agent-session
                      :id "test#1"
                      :project-dir "/tmp/test-project"
                      :proj-name "test-project"
                      :backend-name "test"
                      :started-at "2025-01-01T00:00:00Z")))
          (beads-worktree-session--add-agent agent)
          (beads-worktree-session--add-agent agent)
          (let ((wt-session (car beads-worktree-session--sessions)))
            (should (= 1 (length (oref wt-session agent-sessions)))))))
    (beads-worktree-session-test--teardown)))

(provide 'beads-worktree-session-test)
;;; beads-worktree-session-test.el ends here
