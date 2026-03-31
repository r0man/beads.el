;;; beads-live-ui-test.el --- Live Emacs UI integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Live Emacs UI integration tests for beads.el.
;;
;; These tests run in a live, non-graphical Emacs process (daemon) and
;; exercise the full stack: transient menus, completing-read readers,
;; command classes, and buffer output.
;;
;; Unlike unit tests (which mock beads-command-execute), these tests run
;; real bd CLI commands against temporary beads/git repos and drive
;; Emacs interactively via emacsclient evaluations.
;;
;; Design: docs/live-ui-test-design.md
;; Design bead: be-mol-rdrg
;;
;; RUNNING THESE TESTS:
;;
;;   # One-time: start dev Emacs daemon
;;   WORKTREE=$(pwd)
;;   emacs --daemon=beads-live -Q --eval \
;;     "(progn (add-to-list 'load-path \"$WORKTREE/lisp\") (require 'beads))"
;;
;;   # Run all live tests
;;   emacsclient -s beads-live -e \
;;     '(ert-run-tests-batch ":live")'
;;
;;   # Stop daemon
;;   emacsclient -s beads-live -e '(kill-emacs)'
;;
;; TAGS:
;;   :live        - Requires live Emacs daemon + bd binary
;;   :transient   - Tests transient menu rendering
;;   :reader      - Tests completing-read readers
;;   :integration - Runs real bd CLI commands
;;
;; NOTE: Do NOT include these tests in the default eldev test run.
;; They are intended for manual invocation and CI workflows that
;; have an Emacs daemon available.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads)
(require 'beads-integration-test)
(require 'beads-reader)

;;; ============================================================
;;; Infrastructure: Skip helper
;;; ============================================================

(defun beads-live-test--interactive-p ()
  "Return non-nil when running interactively (not in batch mode).
Transient tests require interactive context for menu rendering.
They are skipped in the normal `eldev test` batch run and should
only be exercised via `ert-run-tests-interactively' with the :live tag."
  (not noninteractive))

;;; ============================================================
;;; Infrastructure: Transient Helpers
;;; ============================================================

(defun beads-live-test--transient-active-p ()
  "Return non-nil if a transient menu is currently active."
  (and (boundp 'transient--prefix)
       transient--prefix))

(defun beads-live-test--open-transient-and-quit (prefix-sym)
  "Open transient PREFIX-SYM and immediately quit.
Returns t if the transient was active before quitting.
Signals an error if the transient fails to open."
  (transient-setup prefix-sym)
  (let ((active (beads-live-test--transient-active-p)))
    (when active
      ;; Use ignore-errors: if quit fails (e.g. transient already exited
      ;; itself during setup), the test environment must not be left dirty.
      ;; beads-test--clear-transient-state in the macro cleanup handles any
      ;; residual state, but we should not propagate a quit error here.
      (ignore-errors (transient-quit-all)))
    active))

(defun beads-live-test--with-completing-read (choice thunk)
  "Call THUNK with `completing-read' mocked to return CHOICE."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) choice)))
    (funcall thunk)))

(defun beads-live-test--with-read-string (value thunk)
  "Call THUNK with `read-string' mocked to return VALUE."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _args) value)))
    (funcall thunk)))

;;; ============================================================
;;; Scenario 1: Main Menu Renders
;;; ============================================================

(ert-deftest beads-live-test-main-menu-renders ()
  "Live: M-x beads opens the main transient menu without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads))))

(ert-deftest beads-live-test-more-menu-renders ()
  "Live: beads-more-menu opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-more-menu))))

;;; ============================================================
;;; Scenario 2: Create Issue End-to-End
;;; ============================================================

(ert-deftest beads-live-test-create-via-command-class ()
  "Live: beads-command-create! creates an issue and returns it."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (let ((issue (beads-command-create!
                  :title "Test bug from live test"
                  :issue-type "bug"
                  :priority 1)))
      (should issue)
      (should (stringp (oref issue id)))
      (should (equal "Test bug from live test" (oref issue title)))
      (should (equal "bug" (oref issue issue-type))))))

(ert-deftest beads-live-test-create-transient-renders ()
  "Live: beads-create transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-create))))

;;; ============================================================
;;; Scenario 3: List Issues With Filter
;;; ============================================================

(ert-deftest beads-live-test-list-returns-issues ()
  "Live: beads-command-list! returns issues from temp repo."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Open issue"     :issue-type "bug"  :priority 1)
       (:title "Another issue"  :issue-type "task" :priority 2))
    (let ((issues (beads-command-list!)))
      (should (listp issues))
      (should (= 2 (length issues))))))

(ert-deftest beads-live-test-list-filter-by-status ()
  "Live: beads-command-list! with :status filter returns matching issues."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Open issue"  :issue-type "bug"  :priority 1)
       (:title "Closed issue" :issue-type "task" :priority 2))
    ;; Close the "Closed issue" by title to avoid ordering assumptions
    (let* ((issues (beads-command-list!))
           (to-close (cl-find "Closed issue" issues
                              :key (lambda (i) (oref i title))
                              :test #'equal)))
      (should to-close)
      (beads-command-close!
       :issue-ids (list (oref to-close id))
       :reason "Completed")
      ;; Now list only open ones
      (let ((open (beads-command-list! :status "open")))
        (should (= 1 (length open)))
        (should (equal "Open issue" (oref (car open) title)))))))

(ert-deftest beads-live-test-list-transient-renders ()
  "Live: beads-list transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-list))))

;;; ============================================================
;;; Scenario 4: Show Issue and Action Menu
;;; ============================================================

(ert-deftest beads-live-test-show-renders-buffer ()
  "Live: beads-show displays issue in *beads-show* buffer."
  :tags '(:live :integration)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Show test issue" :issue-type "feature" :priority 2))
    (let* ((issues (beads-command-list!))
           (issue-id (oref (car issues) id)))
      (beads-show issue-id)
      ;; Buffer name format is *beads-show[context]/id title*;
      ;; search all buffers for one showing this issue.
      (let ((buf (cl-find-if
                  (lambda (b)
                    (string-match-p
                     (regexp-quote issue-id) (buffer-name b)))
                  (buffer-list))))
        (should buf)
        ;; Use unwind-protect so the buffer is killed even if assertions
        ;; fail — otherwise it leaks and may interfere with subsequent tests.
        (unwind-protect
            (with-current-buffer buf
              (should (string-match-p "Show test issue" (buffer-string))))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(ert-deftest beads-live-test-show-actions-transient-renders ()
  "Live: beads-show-actions transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-show-actions))))

;;; ============================================================
;;; Scenario 5: Close Issue
;;; ============================================================

(ert-deftest beads-live-test-close-changes-status ()
  "Live: beads-command-close! changes issue status to closed."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Issue to close" :issue-type "task" :priority 3))
    (let* ((issues (beads-command-list!))
           (issue-id (oref (car issues) id)))
      (beads-command-close! :issue-ids (list issue-id) :reason "Done")
      (let ((refreshed (beads-command-show! :issue-ids (list issue-id))))
        (should (equal "closed" (oref refreshed status)))))))

;;; ============================================================
;;; Scenario 6: Dependency Add/Remove
;;; ============================================================

(ert-deftest beads-live-test-dep-add-and-remove ()
  "Live: add and remove a dependency between two issues."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (beads-test-bd-has-subcommand-p "dep"))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Blocker issue" :issue-type "bug"  :priority 0)
       (:title "Blocked issue" :issue-type "task" :priority 2))
    (let* ((issues (beads-command-list!))
           (blocker-issue (cl-find "Blocker issue" issues
                                   :key (lambda (i) (oref i title))
                                   :test #'equal))
           (blocked-issue (cl-find "Blocked issue" issues
                                   :key (lambda (i) (oref i title))
                                   :test #'equal))
           (blocker-id (oref blocker-issue id))
           (blocked-id (oref blocked-issue id)))
      ;; Add dependency: "Blocked issue" depends on "Blocker issue"
      (beads-command-dep-add!
       :issue-id blocked-id
       :depends-on blocker-id
       :dep-type "blocks")
      ;; Verify it appears in dep list
      (let ((deps (beads-command-dep-list! :issue-id blocked-id)))
        (should (listp deps))
        (should (>= (length deps) 1)))
      ;; Remove it
      (beads-command-dep-remove!
       :issue-id blocked-id
       :depends-on blocker-id)
      ;; Verify the dep was removed
      (let ((deps-after (beads-command-dep-list! :issue-id blocked-id)))
        (should (zerop (length deps-after)))))))

(ert-deftest beads-live-test-dep-transient-renders ()
  "Live: beads-dep transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-dep))))

(ert-deftest beads-live-test-dep-add-menu-renders ()
  "Live: beads-dep-add--menu transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-dep-add--menu))))

(ert-deftest beads-live-test-dep-remove-menu-renders ()
  "Live: beads-dep-remove--menu transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-dep-remove--menu))))

;;; ============================================================
;;; Scenario 7: Update Readers
;;; ============================================================

(ert-deftest beads-live-test-update-status-reader ()
  "Live: beads-reader-update-status returns mocked status choice."
  :tags '(:live :reader)
  (beads-live-test--with-completing-read "in_progress"
    (lambda ()
      (let ((result (beads-reader-update-status "Status: " nil nil)))
        (should (equal "in_progress" result))))))

(ert-deftest beads-live-test-update-type-reader ()
  "Live: beads-reader-update-type returns mocked type choice."
  :tags '(:live :reader)
  (beads-live-test--with-completing-read "feature"
    (lambda ()
      (let ((result (beads-reader-update-type "Type: " nil nil)))
        (should (equal "feature" result))))))

(ert-deftest beads-live-test-update-title-reader ()
  "Live: beads-reader-update-title returns mocked string."
  :tags '(:live :reader)
  (beads-live-test--with-read-string "New title"
    (lambda ()
      (let ((result (beads-reader-update-title "Title: " nil nil)))
        (should (equal "New title" result))))))

(ert-deftest beads-live-test-update-assignee-reader ()
  "Live: beads-reader-update-assignee returns mocked string."
  :tags '(:live :reader)
  (beads-live-test--with-read-string "alice"
    (lambda ()
      (let ((result (beads-reader-update-assignee "Assignee: " nil nil)))
        (should (equal "alice" result))))))

(ert-deftest beads-live-test-update-transient-renders ()
  "Live: beads-update--menu transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-update--menu))))

;;; ============================================================
;;; Scenario 8: At-Point Issue Detection
;;; ============================================================

(ert-deftest beads-live-test-issue-at-point-in-list-buffer ()
  "Live: beads-issue-at-point detects issue in *beads-list* buffer."
  :tags '(:live :reader :integration)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Detectable issue" :issue-type "task" :priority 2))
    (let* ((issues (beads-command-list!))
           (issue-id (oref (car issues) id)))
      ;; Kill any stale beads-list-mode buffers from prior test runs.
      ;; They have deleted temp dirs as default-directory; leaving them
      ;; alive causes beads-list--find-buffer-for-project to fail with
      ;; file-missing when it calls with-current-buffer on each one.
      (dolist (b (buffer-list))
        (when (with-current-buffer b
                (derived-mode-p 'beads-list-mode))
          (kill-buffer b)))
      ;; Open the list buffer (actual name is *beads-list[project]*)
      (beads-list-all)
      (let ((list-buf (cl-find-if
                       (lambda (b)
                         (with-current-buffer b
                           (derived-mode-p 'beads-list-mode)))
                       (buffer-list))))
        (should list-buf)
        ;; Use unwind-protect so the buffer is killed even if assertions
        ;; fail — otherwise it leaks and may interfere with subsequent tests.
        (unwind-protect
            (with-current-buffer list-buf
              ;; Move to first issue line (past tabulated-list header)
              (goto-char (point-min))
              (forward-line 1)
              ;; At-point detection must return the issue ID on an entry row
              (let ((detected (beads-issue-at-point)))
                (should (equal issue-id detected))))
          (when (buffer-live-p list-buf)
            (kill-buffer list-buf)))))))

;;; ============================================================
;;; Scenario 9: Priority Reader Choices
;;; ============================================================

(ert-deftest beads-live-test-priority-reader-high ()
  "Live: priority reader converts '1 - High' selection to \"1\"."
  :tags '(:live :reader)
  (beads-live-test--with-completing-read "1 - High"
    (lambda ()
      (let ((result (beads-reader-priority-level "Priority: " nil nil)))
        (should (equal "1" result))))))

(ert-deftest beads-live-test-priority-reader-critical ()
  "Live: priority reader converts '0 - Critical' to \"0\"."
  :tags '(:live :reader)
  (beads-live-test--with-completing-read "0 - Critical"
    (lambda ()
      (let ((result (beads-reader-priority-level "Priority: " nil nil)))
        (should (equal "0" result))))))

(ert-deftest beads-live-test-priority-reader-backlog ()
  "Live: priority reader converts '4 - Backlog' to \"4\"."
  :tags '(:live :reader)
  (beads-live-test--with-completing-read "4 - Backlog"
    (lambda ()
      (let ((result (beads-reader-priority-level "Priority: " nil nil)))
        (should (equal "4" result))))))

;;; ============================================================
;;; Scenario 10: P2 Transient Menu Smoke Tests
;;; ============================================================

(ert-deftest beads-live-test-state-menu-renders ()
  "Live: beads-state-menu transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-state-menu))))

(ert-deftest beads-live-test-label-menu-renders ()
  "Live: beads-label-menu transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-label-menu))))

(ert-deftest beads-live-test-label-add-renders ()
  "Live: beads-label-add transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-label-add))))

(ert-deftest beads-live-test-label-remove-renders ()
  "Live: beads-label-remove transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-label-remove))))

(ert-deftest beads-live-test-formula-menu-renders ()
  "Live: beads-formula-menu transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-formula-menu))))

(ert-deftest beads-live-test-mol-menu-renders ()
  "Live: beads-mol transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-mol))))

(ert-deftest beads-live-test-dolt-menu-renders ()
  "Live: beads-dolt transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-dolt))))

(ert-deftest beads-live-test-config-menu-renders ()
  "Live: beads-config transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-config))))

(ert-deftest beads-live-test-edit-menu-renders ()
  "Live: beads-edit--menu transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-edit--menu))))

(ert-deftest beads-live-test-compose-metadata-renders ()
  "Live: beads-compose-metadata transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-compose-metadata))))

(ert-deftest beads-live-test-list-advanced-renders ()
  "Live: beads-list-advanced transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-list-advanced))))

(ert-deftest beads-live-test-list-filter-menu-renders ()
  "Live: beads-list-filter-menu transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-list-filter-menu))))

(ert-deftest beads-live-test-advanced-menu-renders ()
  "Live: beads-advanced-menu transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-advanced-menu))))

(ert-deftest beads-live-test-ops-menu-renders ()
  "Live: beads-ops-menu transient opens without error."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo (:init-beads t)
    (should (beads-live-test--open-transient-and-quit 'beads-ops-menu))))

;;; ============================================================
;;; Scenario 11: Creation Readers
;;; ============================================================

(ert-deftest beads-live-test-reader-issue-title ()
  "Live: beads-reader-issue-title returns mocked string."
  :tags '(:live :reader)
  (beads-live-test--with-read-string "My new issue"
    (lambda ()
      (let ((result (beads-reader-issue-title "Title: " nil nil)))
        (should (equal "My new issue" result))))))

(ert-deftest beads-live-test-reader-issue-type ()
  "Live: beads-reader-issue-type returns mocked type."
  :tags '(:live :reader)
  (beads-live-test--with-completing-read "epic"
    (lambda ()
      (let ((result (beads-reader-issue-type "Type: " nil nil)))
        (should (equal "epic" result))))))

(ert-deftest beads-live-test-reader-issue-priority ()
  "Live: beads-reader-issue-priority returns numeric string."
  :tags '(:live :reader)
  (beads-live-test--with-completing-read "2 - Medium"
    (lambda ()
      (let ((result (beads-reader-issue-priority "Priority: " nil nil)))
        (should (equal "2" result))))))

(ert-deftest beads-live-test-reader-create-custom-id ()
  "Live: beads-reader-create-custom-id returns mocked string."
  :tags '(:live :reader)
  (beads-live-test--with-read-string "my-custom-id"
    (lambda ()
      (let ((result (beads-reader-create-custom-id "ID: " nil nil)))
        (should (equal "my-custom-id" result))))))

(ert-deftest beads-live-test-reader-dep-type ()
  "Live: beads-reader-dep-type returns mocked dep type."
  :tags '(:live :reader)
  (beads-live-test--with-completing-read "blocks"
    (lambda ()
      (let ((result (beads-reader-dep-type "Type: " nil nil)))
        (should (equal "blocks" result))))))

;;; ============================================================
;;; Scenario 12: Workflow Commands
;;; ============================================================

(ert-deftest beads-live-test-ready-command ()
  "Live: beads-command-ready! returns unblocked open issues."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Ready task" :issue-type "task" :priority 2))
    (let ((ready (beads-command-ready!)))
      (should (listp ready))
      (should (>= (length ready) 1)))))

(ert-deftest beads-live-test-blocked-command ()
  "Live: beads-command-blocked! returns blocked issues."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (beads-test-bd-has-subcommand-p "blocked"))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Blocker" :issue-type "bug"  :priority 0)
       (:title "Blocked" :issue-type "task" :priority 2))
    (let* ((issues (beads-command-list!))
           (blocker (cl-find "Blocker" issues
                             :key (lambda (i) (oref i title))
                             :test #'equal))
           (blocked (cl-find "Blocked" issues
                             :key (lambda (i) (oref i title))
                             :test #'equal))
           (blocker-id (oref blocker id))
           (blocked-id (oref blocked id)))
      (beads-command-dep-add!
       :issue-id blocked-id
       :depends-on blocker-id
       :dep-type "blocks")
      (let ((blocked-list (beads-command-blocked!)))
        (should (listp blocked-list))
        (should (cl-find blocked-id blocked-list
                         :key (lambda (i) (oref i id))
                         :test #'equal))))))

(ert-deftest beads-live-test-reopen-issue ()
  "Live: reopen a closed issue changes status back to open."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (beads-test-bd-has-subcommand-p "reopen"))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Issue to reopen" :issue-type "task" :priority 2))
    (let* ((issues (beads-command-list!))
           (issue-id (oref (car issues) id)))
      (beads-command-close! :issue-ids (list issue-id) :reason "Done")
      (beads-command-reopen! :issue-ids (list issue-id))
      (let ((refreshed (beads-command-show! :issue-ids (list issue-id))))
        (should (equal "open" (oref refreshed status)))))))

;;; ============================================================
;;; End of test file
;;; ============================================================

(provide 'beads-live-ui-test)
;;; beads-live-ui-test.el ends here
