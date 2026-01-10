;;; beads-agent-backend-test.el --- Tests for beads-agent-backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-agent-backend.el infrastructure module.
;; Tests cover:
;; - Session EIEIO class and accessors
;; - Backend registry functions
;; - Session management functions
;; - Buffer naming functions
;; - Window management helpers

;;; Code:

(require 'ert)
(require 'beads-agent-backend)
(require 'beads-test)

;;; Test Fixtures

(defun beads-agent-backend-test--make-session (&rest args)
  "Create a test session with ARGS.
Provides sensible defaults for required fields."
  (apply #'beads-agent-session
         (append args
                 (list :id (or (plist-get args :id) "test-proj#1")
                       :project-dir (or (plist-get args :project-dir) "/tmp/test-project")
                       :backend-name (or (plist-get args :backend-name) "mock-backend")
                       :started-at (or (plist-get args :started-at) "2025-01-15T10:00:00Z")))))

;;; ========================================
;;; Session Accessor Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-session-backend-name ()
  "Test beads-agent-session-backend-name accessor."
  (let ((session (beads-agent-backend-test--make-session
                  :backend-name "claude-code")))
    (should (equal (beads-agent-session-backend-name session) "claude-code"))))

(ert-deftest beads-agent-backend-test-session-type-name ()
  "Test beads-agent-session-type-name accessor."
  (let ((session (beads-agent-backend-test--make-session
                  :agent-type-name "Task")))
    (should (equal (beads-agent-session-type-name session) "Task"))))

(ert-deftest beads-agent-backend-test-session-type-name-nil ()
  "Test beads-agent-session-type-name returns nil when not set."
  (let ((session (beads-agent-backend-test--make-session)))
    (should (null (beads-agent-session-type-name session)))))

(ert-deftest beads-agent-backend-test-session-started-at ()
  "Test beads-agent-session-started-at accessor."
  (let ((session (beads-agent-backend-test--make-session
                  :started-at "2025-06-15T14:30:00Z")))
    (should (equal (beads-agent-session-started-at session)
                   "2025-06-15T14:30:00Z"))))

(ert-deftest beads-agent-backend-test-session-worktree-dir ()
  "Test beads-agent-session-worktree-dir accessor."
  (let ((session (beads-agent-backend-test--make-session
                  :worktree-dir "/tmp/worktree")))
    (should (equal (beads-agent-session-worktree-dir session) "/tmp/worktree"))))

(ert-deftest beads-agent-backend-test-session-worktree-dir-nil ()
  "Test beads-agent-session-worktree-dir returns nil when not set."
  (let ((session (beads-agent-backend-test--make-session)))
    (should (null (beads-agent-session-worktree-dir session)))))

(ert-deftest beads-agent-backend-test-session-working-dir-with-worktree ()
  "Test beads-agent-session-working-dir returns worktree when set."
  (let ((session (beads-agent-backend-test--make-session
                  :project-dir "/tmp/main"
                  :worktree-dir "/tmp/worktree")))
    (should (equal (beads-agent-session-working-dir session) "/tmp/worktree"))))

(ert-deftest beads-agent-backend-test-session-working-dir-without-worktree ()
  "Test beads-agent-session-working-dir returns project-dir when no worktree."
  (let ((session (beads-agent-backend-test--make-session
                  :project-dir "/tmp/main")))
    (should (equal (beads-agent-session-working-dir session) "/tmp/main"))))

(ert-deftest beads-agent-backend-test-session-buffer-accessor ()
  "Test beads-agent-session-buffer accessor."
  (let ((session (beads-agent-backend-test--make-session)))
    (should (null (beads-agent-session-buffer session)))))

(ert-deftest beads-agent-backend-test-session-set-buffer ()
  "Test beads-agent-session-set-buffer mutator."
  (let ((session (beads-agent-backend-test--make-session))
        (buf (get-buffer-create "*test-agent-buffer*")))
    (unwind-protect
        (progn
          (beads-agent-session-set-buffer session buf)
          (should (eq (beads-agent-session-buffer session) buf)))
      (kill-buffer buf))))

;;; ========================================
;;; Directory-Bound Session Accessor Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-session-project-dir ()
  "Test beads-agent-session-project-dir accessor."
  (let ((session (beads-agent-backend-test--make-session
                  :project-dir "/home/user/projects/myapp")))
    (should (equal (beads-agent-session-project-dir session)
                   "/home/user/projects/myapp"))))

(ert-deftest beads-agent-backend-test-session-project-name ()
  "Test beads-agent-session-project-name accessor."
  (let ((session (beads-agent-backend-test--make-session
                  :proj-name "myapp")))
    (should (equal (beads-agent-session-project-name session) "myapp"))))

(ert-deftest beads-agent-backend-test-session-project-name-nil ()
  "Test beads-agent-session-project-name returns nil when not set."
  (let ((session (beads-agent-backend-test--make-session)))
    (should (null (beads-agent-session-project-name session)))))

(ert-deftest beads-agent-backend-test-session-instance-number ()
  "Test beads-agent-session-instance-number accessor."
  (let ((session (beads-agent-backend-test--make-session
                  :instance-number 3)))
    (should (= (beads-agent-session-instance-number session) 3))))

(ert-deftest beads-agent-backend-test-session-instance-number-default ()
  "Test beads-agent-session-instance-number default value."
  (let ((session (beads-agent-backend-test--make-session)))
    (should (= (beads-agent-session-instance-number session) 1))))

(ert-deftest beads-agent-backend-test-session-current-issue ()
  "Test beads-agent-session-current-issue accessor."
  (let ((session (beads-agent-backend-test--make-session
                  :current-issue "bd-42")))
    (should (equal (beads-agent-session-current-issue session) "bd-42"))))

(ert-deftest beads-agent-backend-test-session-current-issue-nil ()
  "Test beads-agent-session-current-issue returns nil when not set."
  (let ((session (beads-agent-backend-test--make-session)))
    (should (null (beads-agent-session-current-issue session)))))

(ert-deftest beads-agent-backend-test-session-set-current-issue ()
  "Test beads-agent-session-set-current-issue mutator."
  (let ((session (beads-agent-backend-test--make-session)))
    (beads-agent-session-set-current-issue session "bd-99")
    (should (equal (beads-agent-session-current-issue session) "bd-99"))
    ;; Should also add to touched-issues
    (should (member "bd-99" (beads-agent-session-touched-issues session)))))

(ert-deftest beads-agent-backend-test-session-set-current-issue-nil ()
  "Test beads-agent-session-set-current-issue with nil."
  (let ((session (beads-agent-backend-test--make-session
                  :current-issue "bd-42")))
    (beads-agent-session-set-current-issue session nil)
    (should (null (beads-agent-session-current-issue session)))))

(ert-deftest beads-agent-backend-test-session-touched-issues ()
  "Test beads-agent-session-touched-issues accessor."
  (let ((session (beads-agent-backend-test--make-session
                  :touched-issues '("bd-1" "bd-2" "bd-3"))))
    (should (equal (beads-agent-session-touched-issues session)
                   '("bd-1" "bd-2" "bd-3")))))

(ert-deftest beads-agent-backend-test-session-add-touched-issue ()
  "Test beads-agent-session-add-touched-issue adds new issue."
  (let ((session (beads-agent-backend-test--make-session
                  :touched-issues '("bd-1"))))
    (should (beads-agent-session-add-touched-issue session "bd-2"))
    (should (member "bd-2" (beads-agent-session-touched-issues session)))
    (should (member "bd-1" (beads-agent-session-touched-issues session)))))

(ert-deftest beads-agent-backend-test-session-add-touched-issue-duplicate ()
  "Test beads-agent-session-add-touched-issue returns nil for duplicate."
  (let ((session (beads-agent-backend-test--make-session
                  :touched-issues '("bd-1" "bd-2"))))
    (should-not (beads-agent-session-add-touched-issue session "bd-1"))
    ;; List should be unchanged
    (should (= (length (beads-agent-session-touched-issues session)) 2))))

(ert-deftest beads-agent-backend-test-session-issue-id-backward-compat ()
  "Test beads-agent-session-issue-id backward compatibility."
  ;; With current-issue set
  (let ((session (beads-agent-backend-test--make-session
                  :current-issue "bd-new"
                  :issue-id "bd-old")))
    (should (equal (beads-agent-session-issue-id session) "bd-new")))
  ;; Without current-issue, falls back to issue-id
  (let ((session (beads-agent-backend-test--make-session
                  :issue-id "bd-old")))
    (should (equal (beads-agent-session-issue-id session) "bd-old"))))

;;; ========================================
;;; Backend Registry Tests
;;; ========================================

;; Define a concrete test backend
(defclass beads-agent-backend-test--mock-backend (beads-agent-backend)
  ((available :initarg :available :initform t))
  :documentation "Mock backend for testing.")

(cl-defmethod beads-agent-backend-available-p ((backend beads-agent-backend-test--mock-backend))
  "Return availability status for mock BACKEND."
  (oref backend available))

(ert-deftest beads-agent-backend-test-register-backend ()
  "Test beads-agent--register-backend adds backend to registry."
  (let ((beads-agent--backends nil)
        (backend (beads-agent-backend-test--mock-backend
                  :name "test-backend"
                  :priority 50)))
    (beads-agent--register-backend backend)
    (should (= (length beads-agent--backends) 1))
    (should (equal (oref (car beads-agent--backends) name) "test-backend"))))

(ert-deftest beads-agent-backend-test-register-backend-replaces-same-name ()
  "Test beads-agent--register-backend replaces existing backend with same name."
  (let ((beads-agent--backends nil)
        (backend1 (beads-agent-backend-test--mock-backend
                   :name "test-backend"
                   :priority 50
                   :description "First"))
        (backend2 (beads-agent-backend-test--mock-backend
                   :name "test-backend"
                   :priority 30
                   :description "Second")))
    (beads-agent--register-backend backend1)
    (beads-agent--register-backend backend2)
    (should (= (length beads-agent--backends) 1))
    (should (equal (oref (car beads-agent--backends) description) "Second"))))

(ert-deftest beads-agent-backend-test-register-backend-sorts-by-priority ()
  "Test beads-agent--register-backend sorts by priority."
  (let ((beads-agent--backends nil)
        (backend1 (beads-agent-backend-test--mock-backend
                   :name "low-priority" :priority 100))
        (backend2 (beads-agent-backend-test--mock-backend
                   :name "high-priority" :priority 10))
        (backend3 (beads-agent-backend-test--mock-backend
                   :name "medium-priority" :priority 50)))
    (beads-agent--register-backend backend1)
    (beads-agent--register-backend backend2)
    (beads-agent--register-backend backend3)
    (should (equal (mapcar (lambda (b) (oref b name)) beads-agent--backends)
                   '("high-priority" "medium-priority" "low-priority")))))

(ert-deftest beads-agent-backend-test-register-backend-rejects-non-backend ()
  "Test beads-agent--register-backend rejects non-backend objects."
  (let ((beads-agent--backends nil))
    (should-error (beads-agent--register-backend "not-a-backend"))))

(ert-deftest beads-agent-backend-test-get-backend ()
  "Test beads-agent--get-backend finds backend by name."
  (let ((beads-agent--backends nil)
        (backend (beads-agent-backend-test--mock-backend
                  :name "find-me" :priority 50)))
    (beads-agent--register-backend backend)
    (should (eq (beads-agent--get-backend "find-me") backend))
    (should (null (beads-agent--get-backend "not-found")))))

(ert-deftest beads-agent-backend-test-get-available-backends ()
  "Test beads-agent--get-available-backends filters by availability."
  (let ((beads-agent--backends nil)
        (available (beads-agent-backend-test--mock-backend
                    :name "available" :priority 50 :available t))
        (unavailable (beads-agent-backend-test--mock-backend
                      :name "unavailable" :priority 10 :available nil)))
    (beads-agent--register-backend available)
    (beads-agent--register-backend unavailable)
    (let ((result (beads-agent--get-available-backends)))
      (should (= (length result) 1))
      (should (equal (oref (car result) name) "available")))))

(ert-deftest beads-agent-backend-test-get-all-backends ()
  "Test beads-agent--get-all-backends returns all backends."
  (let ((beads-agent--backends nil)
        (backend1 (beads-agent-backend-test--mock-backend
                   :name "one" :priority 50 :available t))
        (backend2 (beads-agent-backend-test--mock-backend
                   :name "two" :priority 10 :available nil)))
    (beads-agent--register-backend backend1)
    (beads-agent--register-backend backend2)
    (should (= (length (beads-agent--get-all-backends)) 2))))

(ert-deftest beads-agent-backend-test-backend-names ()
  "Test beads-agent--backend-names returns available backend names."
  (let ((beads-agent--backends nil)
        (available (beads-agent-backend-test--mock-backend
                    :name "avail" :priority 50 :available t))
        (unavailable (beads-agent-backend-test--mock-backend
                      :name "unavail" :priority 10 :available nil)))
    (beads-agent--register-backend available)
    (beads-agent--register-backend unavailable)
    (should (equal (beads-agent--backend-names) '("avail")))))

;;; ========================================
;;; Issue Outcome Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-get-issue-outcome ()
  "Test beads-agent--get-issue-outcome returns stored outcome."
  (let ((beads-agent--issue-outcomes (make-hash-table :test #'equal)))
    (puthash "bd-1" 'finished beads-agent--issue-outcomes)
    (puthash "bd-2" 'failed beads-agent--issue-outcomes)
    (should (eq (beads-agent--get-issue-outcome "bd-1") 'finished))
    (should (eq (beads-agent--get-issue-outcome "bd-2") 'failed))
    (should (null (beads-agent--get-issue-outcome "bd-3")))))

;;; ========================================
;;; Session Number Extraction Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-session-number-from-id ()
  "Test beads-agent--session-number-from-id extracts number."
  (should (= (beads-agent--session-number-from-id "bd-42#1") 1))
  (should (= (beads-agent--session-number-from-id "beads.el#3") 3))
  (should (= (beads-agent--session-number-from-id "project#123") 123)))

(ert-deftest beads-agent-backend-test-session-number-from-id-invalid ()
  "Test beads-agent--session-number-from-id returns nil for invalid format."
  (should (null (beads-agent--session-number-from-id "no-number")))
  (should (null (beads-agent--session-number-from-id "bd-42")))
  (should (null (beads-agent--session-number-from-id nil))))

;;; ========================================
;;; Derive Project Name Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-derive-project-name ()
  "Test beads-agent--derive-project-name extracts basename."
  (should (equal (beads-agent--derive-project-name "/home/user/code/beads.el")
                 "beads.el"))
  (should (equal (beads-agent--derive-project-name "/home/user/code/beads.el/")
                 "beads.el"))
  (should (equal (beads-agent--derive-project-name "/tmp/my-project")
                 "my-project")))

;;; ========================================
;;; Buffer Naming Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-generate-buffer-name ()
  "Test beads-agent--generate-buffer-name creates correct format."
  ;; Basic format: PROJECT/TYPE#N
  (should (equal (beads-agent--generate-buffer-name
                  "myproject" "Task" 1)
                 "*beads-agent: myproject/Task#1*"))
  (should (equal (beads-agent--generate-buffer-name
                  "beads.el" "Review" 2)
                 "*beads-agent: beads.el/Review#2*"))
  ;; With issue context
  (should (equal (beads-agent--generate-buffer-name
                  "proj" "Task" 1 "bd-42" "Fix bug")
                 "*beads-agent: proj/Task#1 bd-42 Fix bug*")))

(ert-deftest beads-agent-backend-test-generate-project-buffer-name ()
  "Test beads-agent--generate-project-buffer-name creates correct format."
  ;; Basic format: PROJECT/TYPE#N
  (should (equal (beads-agent--generate-project-buffer-name
                  "beads.el" "Task" 1)
                 "*beads-agent: beads.el/Task#1*"))
  (should (equal (beads-agent--generate-project-buffer-name
                  "myapp" "Plan" 3)
                 "*beads-agent: myapp/Plan#3*"))
  ;; With issue context
  (should (equal (beads-agent--generate-project-buffer-name
                  "proj" "Task" 1 "bd-42" "Fix bug")
                 "*beads-agent: proj/Task#1 bd-42 Fix bug*"))
  ;; With worktree context
  (should (equal (beads-agent--generate-project-buffer-name
                  "proj" "Review" 2 nil nil "wt" "feature")
                 "*beads-agent: proj/wt@feature/Review#2*")))

(ert-deftest beads-agent-backend-test-parse-buffer-name ()
  "Test beads-agent--parse-buffer-name parses correctly."
  (let ((parsed (beads-agent--parse-buffer-name
                 "*beads-agent: myproject/Task#1*")))
    (should (equal (plist-get parsed :project) "myproject"))
    (should (equal (plist-get parsed :type) "Task"))
    (should (= (plist-get parsed :instance) 1))))

(ert-deftest beads-agent-backend-test-parse-buffer-name-complex ()
  "Test beads-agent--parse-buffer-name with complex names."
  (let ((parsed (beads-agent--parse-buffer-name
                 "*beads-agent: beads.el/wt@feat/Review#15*")))
    (should (equal (plist-get parsed :project) "beads.el"))
    (should (equal (plist-get parsed :worktree) "wt"))
    (should (equal (plist-get parsed :branch) "feat"))
    (should (equal (plist-get parsed :type) "Review"))
    (should (= (plist-get parsed :instance) 15))))

(ert-deftest beads-agent-backend-test-parse-buffer-name-invalid ()
  "Test beads-agent--parse-buffer-name returns nil for invalid format."
  (should (null (beads-agent--parse-buffer-name "*scratch*")))
  (should (null (beads-agent--parse-buffer-name "not-a-buffer-name")))
  (should (null (beads-agent--parse-buffer-name "*beads-agent: incomplete*"))))

(ert-deftest beads-agent-backend-test-parse-project-buffer-name ()
  "Test beads-agent--parse-project-buffer-name parses correctly.
The function returns legacy keys (:project-name, :type-name, etc.)."
  ;; Basic format
  (let ((parsed (beads-agent--parse-project-buffer-name
                 "*beads-agent: beads.el/Task#2*")))
    (should (equal (plist-get parsed :project-name) "beads.el"))
    (should (equal (plist-get parsed :type-name) "Task"))
    (should (= (plist-get parsed :instance-n) 2)))
  ;; With worktree
  (let ((parsed (beads-agent--parse-project-buffer-name
                 "*beads-agent: proj/wt@feat/Plan#3*")))
    (should (equal (plist-get parsed :project-name) "proj"))
    (should (equal (plist-get parsed :worktree) "wt"))
    (should (equal (plist-get parsed :branch) "feat"))
    (should (equal (plist-get parsed :type-name) "Plan"))
    (should (= (plist-get parsed :instance-n) 3)))
  ;; With issue context
  (let ((parsed (beads-agent--parse-project-buffer-name
                 "*beads-agent: proj/Task#1 bd-42 Fix bug*")))
    (should (equal (plist-get parsed :project-name) "proj"))
    (should (equal (plist-get parsed :type-name) "Task"))
    (should (= (plist-get parsed :instance-n) 1))
    (should (equal (plist-get parsed :issue-id) "bd-42"))
    (should (equal (plist-get parsed :title) "Fix bug"))))

(ert-deftest beads-agent-backend-test-buffer-name-p ()
  "Test beads-agent--buffer-name-p validates buffer names."
  ;; Centralized format
  (should (beads-agent--buffer-name-p "*beads-agent: beads.el/Task#1*"))
  (should (beads-agent--buffer-name-p "*beads-agent: proj/wt@feat/Plan#2*"))
  (should (beads-agent--buffer-name-p "*beads-agent: proj/Task#1 bd-42 Fix bug*"))
  ;; Invalid formats
  (should-not (beads-agent--buffer-name-p "*scratch*"))
  (should-not (beads-agent--buffer-name-p nil))
  (should-not (beads-agent--buffer-name-p "*beads-agent: incomplete*"))
  ;; Legacy format no longer accepted
  (should-not (beads-agent--buffer-name-p "*beads-agent[bd-42][Task#1]*")))

(ert-deftest beads-agent-backend-test-project-buffer-name-p ()
  "Test beads-agent--project-buffer-name-p validates buffer names."
  ;; Centralized format
  (should (beads-agent--project-buffer-name-p "*beads-agent: beads.el/Task#1*"))
  (should (beads-agent--project-buffer-name-p "*beads-agent: proj/wt@feat/Plan#2*"))
  (should (beads-agent--project-buffer-name-p "*beads-agent: proj/Task#1 bd-42 Fix bug*"))
  ;; Invalid formats
  (should-not (beads-agent--project-buffer-name-p "*beads-agent[beads.el][Task#1]*"))
  (should-not (beads-agent--project-buffer-name-p "*scratch*")))

;;; ========================================
;;; Typed Instance Counter Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-next-typed-instance-number ()
  "Test beads-agent--next-typed-instance-number increments."
  (let ((beads-agent--typed-instance-counters (make-hash-table :test #'equal)))
    (should (= (beads-agent--next-typed-instance-number "proj" "Task") 1))
    (should (= (beads-agent--next-typed-instance-number "proj" "Task") 2))
    (should (= (beads-agent--next-typed-instance-number "proj" "Task") 3))))

(ert-deftest beads-agent-backend-test-next-typed-instance-number-per-type ()
  "Test beads-agent--next-typed-instance-number tracks per type."
  (let ((beads-agent--typed-instance-counters (make-hash-table :test #'equal)))
    (should (= (beads-agent--next-typed-instance-number "proj" "Task") 1))
    (should (= (beads-agent--next-typed-instance-number "proj" "Plan") 1))
    (should (= (beads-agent--next-typed-instance-number "proj" "Task") 2))
    (should (= (beads-agent--next-typed-instance-number "proj" "Plan") 2))))

(ert-deftest beads-agent-backend-test-next-typed-instance-number-per-project ()
  "Test beads-agent--next-typed-instance-number tracks per project."
  (let ((beads-agent--typed-instance-counters (make-hash-table :test #'equal)))
    (should (= (beads-agent--next-typed-instance-number "proj1" "Task") 1))
    (should (= (beads-agent--next-typed-instance-number "proj2" "Task") 1))
    (should (= (beads-agent--next-typed-instance-number "proj1" "Task") 2))))

(ert-deftest beads-agent-backend-test-peek-typed-instance-number ()
  "Test beads-agent--peek-typed-instance-number doesn't increment."
  (let ((beads-agent--typed-instance-counters (make-hash-table :test #'equal)))
    (should (= (beads-agent--peek-typed-instance-number "proj" "Task") 1))
    (should (= (beads-agent--peek-typed-instance-number "proj" "Task") 1))
    ;; After actual increment
    (beads-agent--next-typed-instance-number "proj" "Task")
    (should (= (beads-agent--peek-typed-instance-number "proj" "Task") 2))))

(ert-deftest beads-agent-backend-test-reset-typed-instance-counters ()
  "Test beads-agent--reset-typed-instance-counters clears all."
  (let ((beads-agent--typed-instance-counters (make-hash-table :test #'equal)))
    (beads-agent--next-typed-instance-number "proj" "Task")
    (beads-agent--next-typed-instance-number "proj" "Task")
    (beads-agent--reset-typed-instance-counters)
    (should (= (beads-agent--next-typed-instance-number "proj" "Task") 1))))

;;; ========================================
;;; Generate Buffer Name for Session Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-generate-buffer-name-for-session ()
  "Test beads-agent--generate-buffer-name-for-session."
  (let ((beads-agent--typed-instance-counters (make-hash-table :test #'equal)))
    (let ((session (beads-agent-backend-test--make-session
                    :id "beads.el#1"
                    :proj-name "beads.el"
                    :project-dir "/home/user/beads.el"
                    :agent-type-name "Task"
                    :backend-name "claude-code"
                    :current-issue "bd-42")))
      (should (equal (beads-agent--generate-buffer-name-for-session session)
                     "*beads-agent: beads.el/Task#1 bd-42*")))))

(ert-deftest beads-agent-backend-test-generate-buffer-name-for-session-default-type ()
  "Test beads-agent--generate-buffer-name-for-session with no type name."
  (let ((beads-agent--typed-instance-counters (make-hash-table :test #'equal)))
    (let ((session (beads-agent-backend-test--make-session
                    :id "beads.el#1"
                    :proj-name "beads.el"
                    :project-dir "/home/user/beads.el"
                    :backend-name "claude-code")))
      ;; Should use "Agent" as default
      (should (string-match-p "/Agent#"
                              (beads-agent--generate-buffer-name-for-session session))))))

(ert-deftest beads-agent-backend-test-generate-buffer-name-for-project-session ()
  "Test beads-agent--generate-buffer-name-for-project-session."
  (let ((beads-agent--typed-instance-counters (make-hash-table :test #'equal)))
    (let ((session (beads-agent-backend-test--make-session
                    :id "beads.el#1"
                    :proj-name "beads.el"
                    :project-dir "/home/user/beads.el"
                    :agent-type-name "Task"
                    :backend-name "mock")))
      (should (equal (beads-agent--generate-buffer-name-for-project-session session)
                     "*beads-agent: beads.el/Task#1*")))))

(ert-deftest beads-agent-backend-test-generate-buffer-name-for-project-session-idempotent ()
  "Test beads-agent--generate-buffer-name-for-project-session is idempotent."
  (let ((beads-agent--typed-instance-counters (make-hash-table :test #'equal))
        (buf (get-buffer-create "*beads-agent: beads.el/Task#1*")))
    (unwind-protect
        (let ((session (beads-agent-backend-test--make-session
                        :id "beads.el#1"
                        :proj-name "beads.el"
                        :project-dir "/home/user/beads.el"
                        :agent-type-name "Task"
                        :backend-name "mock"
                        :buffer buf)))
          ;; Should return existing buffer name, not generate new
          (should (equal (beads-agent--generate-buffer-name-for-project-session session)
                         "*beads-agent: beads.el/Task#1*"))
          ;; Call again - should still be the same
          (should (equal (beads-agent--generate-buffer-name-for-project-session session)
                         "*beads-agent: beads.el/Task#1*")))
      (kill-buffer buf))))

(ert-deftest beads-agent-backend-test-generate-buffer-name-for-project-session-worktree ()
  "Test beads-agent--generate-buffer-name-for-project-session prefers worktree."
  (let ((beads-agent--typed-instance-counters (make-hash-table :test #'equal)))
    (let ((session (beads-agent-backend-test--make-session
                    :id "beads.el#1"
                    :proj-name "beads.el"
                    :project-dir "/home/user/beads.el"
                    :worktree-dir "/home/user/beads.el-wt"
                    :agent-type-name "Task"
                    :backend-name "mock")))
      ;; Should use worktree dir name
      (should (equal (beads-agent--generate-buffer-name-for-project-session session)
                     "*beads-agent: beads.el-wt/Task#1*")))))

;;; ========================================
;;; Session Instance Number Extraction Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-extract-instance-number-from-id ()
  "Test beads-agent--session-instance-number extracts from ID."
  (let ((session (beads-agent-backend-test--make-session :id "proj#5")))
    (should (= (beads-agent--session-instance-number session) 5))))

(ert-deftest beads-agent-backend-test-extract-instance-number-default ()
  "Test beads-agent--session-instance-number returns 1 for invalid ID."
  (let ((session (beads-agent-backend-test--make-session :id "no-number")))
    (should (= (beads-agent--session-instance-number session) 1))))

;;; ========================================
;;; Wait for Buffer Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-wait-for-buffer-immediate ()
  "Test beads-agent--wait-for-buffer returns immediately when found."
  (let ((buf (get-buffer-create "*test-wait-buffer*")))
    (unwind-protect
        (let ((result (beads-agent--wait-for-buffer
                       (lambda () buf)
                       1.0 0.1)))
          (should (eq result buf)))
      (kill-buffer buf))))

(ert-deftest beads-agent-backend-test-wait-for-buffer-timeout ()
  "Test beads-agent--wait-for-buffer returns nil on timeout."
  (let ((result (beads-agent--wait-for-buffer
                 (lambda () nil)
                 0.2 0.05)))
    (should (null result))))

;;; ========================================
;;; Find Buffers Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-find-buffers-by-issue ()
  "Test beads-agent--find-buffers-by-issue finds matching buffers."
  (let ((buf1 (get-buffer-create "*beads-agent: proj/Task#1 bd-42*"))
        (buf2 (get-buffer-create "*beads-agent: proj/Review#1 bd-42*"))
        (buf3 (get-buffer-create "*beads-agent: proj/Task#1 bd-99*")))
    (unwind-protect
        (let ((found (beads-agent--find-buffers-by-issue "bd-42")))
          (should (= (length found) 2))
          (should (member buf1 found))
          (should (member buf2 found))
          (should-not (member buf3 found)))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

(ert-deftest beads-agent-backend-test-find-buffers-by-type ()
  "Test beads-agent--find-buffers-by-type finds matching buffers."
  (let ((buf1 (get-buffer-create "*beads-agent: proj/Task#1*"))
        (buf2 (get-buffer-create "*beads-agent: proj/Task#2*"))
        (buf3 (get-buffer-create "*beads-agent: proj/Review#1*")))
    (unwind-protect
        (let ((found (beads-agent--find-buffers-by-type "Task")))
          (should (= (length found) 2))
          (should (member buf1 found))
          (should (member buf2 found))
          (should-not (member buf3 found)))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (kill-buffer buf3))))

;;; ========================================
;;; State Change Hook Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-run-state-change-hook ()
  "Test beads-agent--run-state-change-hook runs hook functions."
  (let ((beads-agent--issue-outcomes (make-hash-table :test #'equal))
        (beads-agent-state-change-hook nil)
        (hook-called nil)
        (hook-action nil)
        (hook-session nil))
    (add-hook 'beads-agent-state-change-hook
              (lambda (action session)
                (setq hook-called t
                      hook-action action
                      hook-session session)))
    (let ((session (beads-agent-backend-test--make-session
                    :issue-id "bd-42"
                    :agent-type-name "Task")))
      (beads-agent--run-state-change-hook 'started session)
      (should hook-called)
      (should (eq hook-action 'started))
      (should (eq hook-session session)))))

(ert-deftest beads-agent-backend-test-run-state-change-hook-records-outcome ()
  "Test beads-agent--run-state-change-hook records outcomes."
  (let ((beads-agent--issue-outcomes (make-hash-table :test #'equal))
        (beads-agent-state-change-hook nil))
    (let ((session (beads-agent-backend-test--make-session
                    :issue-id "bd-42"
                    :agent-type-name "Task")))
      ;; Failed action should record outcome
      (beads-agent--run-state-change-hook 'failed session)
      (let ((outcome (beads-agent--get-issue-outcome "bd-42")))
        (should (consp outcome))
        (should (equal (car outcome) "T"))  ; First letter of Task
        (should (eq (cdr outcome) 'failed))))))

(ert-deftest beads-agent-backend-test-run-state-change-hook-clears-on-start ()
  "Test beads-agent--run-state-change-hook clears outcome on start."
  (let ((beads-agent--issue-outcomes (make-hash-table :test #'equal))
        (beads-agent-state-change-hook nil))
    ;; Pre-set an outcome
    (puthash "bd-42" 'finished beads-agent--issue-outcomes)
    (let ((session (beads-agent-backend-test--make-session
                    :issue-id "bd-42")))
      (beads-agent--run-state-change-hook 'started session)
      (should (null (beads-agent--get-issue-outcome "bd-42"))))))

;;; ========================================
;;; Generate Session ID Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-generate-session-id ()
  "Test beads-agent--generate-session-id creates correct format."
  ;; Mock sesman-sessions to return empty list
  (cl-letf (((symbol-function 'sesman-sessions)
             (lambda (_) nil)))
    (should (equal (beads-agent--generate-session-id "bd-42") "bd-42#1"))))

(ert-deftest beads-agent-backend-test-generate-project-session-id ()
  "Test beads-agent--generate-project-session-id creates correct format."
  ;; Mock sesman-sessions to return empty list
  (cl-letf (((symbol-function 'sesman-sessions)
             (lambda (_) nil)))
    (should (equal (beads-agent--generate-project-session-id "/home/user/beads.el")
                   "beads.el#1"))))

;;; ========================================
;;; Pop to Buffer Test
;;; ========================================

(ert-deftest beads-agent-backend-test-pop-to-buffer-other-window ()
  "Test beads-agent--pop-to-buffer-other-window calls pop-to-buffer."
  (let ((pop-called nil)
        (pop-arg nil))
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (buf)
                 (setq pop-called t
                       pop-arg buf))))
      (beads-agent--pop-to-buffer-other-window (current-buffer))
      (should pop-called)
      (should (eq pop-arg (current-buffer))))))

;;; ========================================
;;; Backend Class Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-backend-class-slots ()
  "Test beads-agent-backend class has expected slots."
  (let ((backend (beads-agent-backend-test--mock-backend
                  :name "test"
                  :priority 25
                  :description "Test backend")))
    (should (equal (oref backend name) "test"))
    (should (= (oref backend priority) 25))
    (should (equal (oref backend description) "Test backend"))))

(ert-deftest beads-agent-backend-test-backend-default-priority ()
  "Test beads-agent-backend default priority."
  (let ((backend (beads-agent-backend-test--mock-backend
                  :name "test")))
    (should (= (oref backend priority) 50))))

;;; ========================================
;;; Session Class Slot Tests
;;; ========================================

(ert-deftest beads-agent-backend-test-session-class-all-slots ()
  "Test beads-agent-session class has all expected slots."
  (let ((session (beads-agent-session
                  :id "test#1"
                  :project-dir "/tmp/test"
                  :proj-name "test"
                  :instance-number 1
                  :current-issue "bd-42"
                  :touched-issues '("bd-42" "bd-43")
                  :issue-id "bd-42"
                  :backend-name "mock"
                  :agent-type-name "Task"
                  :worktree-dir "/tmp/worktree"
                  :started-at "2025-01-01T00:00:00Z"
                  :backend-session 'some-handle
                  :buffer nil)))
    (should (equal (oref session id) "test#1"))
    (should (equal (oref session project-dir) "/tmp/test"))
    (should (equal (oref session proj-name) "test"))
    (should (= (oref session instance-number) 1))
    (should (equal (oref session current-issue) "bd-42"))
    (should (equal (oref session touched-issues) '("bd-42" "bd-43")))
    (should (equal (oref session issue-id) "bd-42"))
    (should (equal (oref session backend-name) "mock"))
    (should (equal (oref session agent-type-name) "Task"))
    (should (equal (oref session worktree-dir) "/tmp/worktree"))
    (should (equal (oref session started-at) "2025-01-01T00:00:00Z"))
    (should (eq (oref session backend-session) 'some-handle))))

(provide 'beads-agent-backend-test)

;;; beads-agent-backend-test.el ends here
