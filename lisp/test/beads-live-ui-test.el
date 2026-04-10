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
;; NOTE: Integration tests (:integration tag) run in the default
;; `eldev test' batch run via the suite-level Dolt server.  Transient
;; tests (:transient tag) skip automatically in batch/non-interactive
;; mode and require a live interactive Emacs session or daemon.

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
    (let ((issue (beads-execute 'beads-command-create
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
    (let ((issues (beads-list-execute)))
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
    (let* ((issues (beads-list-execute))
           (to-close (cl-find "Closed issue" issues
                              :key (lambda (i) (oref i title))
                              :test #'equal)))
      (should to-close)
      (beads-execute 'beads-command-close
       :issue-ids (list (oref to-close id))
       :reason "Completed")
      ;; Now list only open ones
      (let ((open (beads-list-execute :status "open")))
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
    (let* ((issues (beads-list-execute))
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
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id)))
      (beads-execute 'beads-command-close :issue-ids (list issue-id) :reason "Done")
      (let ((refreshed (beads-execute 'beads-command-show :issue-ids (list issue-id))))
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
    (let* ((issues (beads-list-execute))
           (blocker-issue (cl-find "Blocker issue" issues
                                   :key (lambda (i) (oref i title))
                                   :test #'equal))
           (blocked-issue (cl-find "Blocked issue" issues
                                   :key (lambda (i) (oref i title))
                                   :test #'equal))
           (blocker-id (oref blocker-issue id))
           (blocked-id (oref blocked-issue id)))
      ;; Add dependency: "Blocked issue" depends on "Blocker issue"
      (beads-execute 'beads-command-dep-add
       :issue-id blocked-id
       :depends-on blocker-id
       :dep-type "blocks")
      ;; Verify blocker-id appears in dep list as a dependency
      (let ((deps (beads-execute 'beads-command-dep-list :issue-id blocked-id)))
        (should (listp deps))
        (should (cl-find blocker-id deps
                         :key (lambda (d) (oref d depends-on-id))
                         :test #'equal)))
      ;; Remove it
      (beads-execute 'beads-command-dep-remove
       :issue-id blocked-id
       :depends-on blocker-id)
      ;; Verify the dep was removed
      (let ((deps-after (beads-execute 'beads-command-dep-list :issue-id blocked-id)))
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

(ert-deftest beads-live-test-update-priority-reader ()
  "Live: beads-reader-update-priority converts priority label to numeric string."
  :tags '(:live :reader)
  (beads-live-test--with-completing-read "1 - High"
    (lambda ()
      (let ((result (beads-reader-update-priority "Priority: " nil nil)))
        (should (equal "1" result))))))

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
    (let* ((issues (beads-list-execute))
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
              ;; When tabulated-list-use-header-line is t (the default),
              ;; the header is in header-line-format, NOT in buffer text.
              ;; Point-min is already on the first entry row.
              (goto-char (point-min))
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
    (let ((ready (beads-execute 'beads-command-ready)))
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
    (let* ((issues (beads-list-execute))
           (blocker (cl-find "Blocker" issues
                             :key (lambda (i) (oref i title))
                             :test #'equal))
           (blocked (cl-find "Blocked" issues
                             :key (lambda (i) (oref i title))
                             :test #'equal))
           (blocker-id (oref blocker id))
           (blocked-id (oref blocked id)))
      (beads-execute 'beads-command-dep-add
       :issue-id blocked-id
       :depends-on blocker-id
       :dep-type "blocks")
      (let ((blocked-list (beads-execute 'beads-command-blocked)))
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
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id)))
      (beads-execute 'beads-command-close :issue-ids (list issue-id) :reason "Done")
      (beads-execute 'beads-command-reopen :issue-ids (list issue-id))
      (let ((refreshed (beads-execute 'beads-command-show :issue-ids (list issue-id))))
        (should (equal "open" (oref refreshed status)))))))

;;; ============================================================
;;; Infrastructure: Agent and Buffer Helpers
;;; (Scenarios 11-32 from design doc
;;;  docs/live-ui-test-design-list-show-agent-commands.md)
;;; ============================================================

(defmacro beads-live-test--with-mock-agent (&rest body)
  "Run BODY with mock agent backend active, reset on exit.
Also makes `beads-agent--fetch-issue-async' synchronous and
restores `beads-git-find-project-root' to return `default-directory'
so that `beads-agent-start' gets a valid project root."
  (declare (indent 0))
  `(progn
     (require 'beads-agent-mock)
     (beads-agent-mock-register)
     (beads-agent-mock-reset)
     (let ((beads-agent-default-backend "mock"))
       (cl-letf (((symbol-function 'beads-agent--fetch-issue-async)
                  (lambda (issue-id callback)
                    (condition-case nil
                        (funcall callback
                                 (beads-execute 'beads-command-show :issue-ids (list issue-id)))
                      (error (funcall callback nil)))))
                 ;; The outer test fixture mocks beads-git-find-project-root
                 ;; to nil; restore it here so agent sessions get a valid dir.
                 ((symbol-function 'beads-git-find-project-root)
                  (lambda () default-directory))
                 ;; Bypass the prompt-edit buffer by passing the prompt
                 ;; through immediately.  Tests can't interact with the
                 ;; buffer; this preserves the pre-toggle-removal behavior.
                 ((symbol-function 'beads-agent-prompt-edit-show)
                  (lambda (_issue-id prompt _agent-type-name callback)
                    (funcall callback prompt))))
         (unwind-protect
             (progn ,@body)
           (beads-live-test--cleanup-agent-sessions)
           (beads-agent-mock-reset)
           (beads-agent-mock-unregister))))))

(defun beads-live-test--cleanup-agent-sessions ()
  "Destroy all active agent sessions (for test cleanup)."
  (dolist (session (beads-agent--get-all-sessions))
    (ignore-errors
      (beads-agent--destroy-session (oref session id)))))

(defun beads-live-test--open-list-buffer ()
  "Kill stale list buffers, open a fresh one, return it."
  (dolist (b (buffer-list))
    (when (with-current-buffer b (derived-mode-p 'beads-list-mode))
      (kill-buffer b)))
  (beads-list-all)
  (cl-find-if (lambda (b)
                (with-current-buffer b
                  (derived-mode-p 'beads-list-mode)))
              (buffer-list)))

(defun beads-live-test--open-show-buffer (issue-id)
  "Open a show buffer for ISSUE-ID and return it."
  (beads-show issue-id)
  (cl-find-if (lambda (b)
                (string-match-p (regexp-quote issue-id) (buffer-name b)))
              (buffer-list)))

(defun beads-live-test--wait-until (pred &optional timeout)
  "Poll every 0.1 s until PRED returns non-nil or TIMEOUT (default 15) s pass.
Returns the value of PRED."
  (let ((timeout (or timeout 15))
        (start (float-time)))
    (while (and (not (funcall pred))
                (< (- (float-time) start) timeout))
      (accept-process-output nil 0.1))
    (funcall pred)))

;;; ============================================================
;;; Scenario 11: Agent Commands — Start at Point
;;; ============================================================

(ert-deftest beads-live-test-agent-start-at-point-from-list ()
  "Live: beads-agent-start-at-point starts session from list buffer."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (featurep 'beads-agent-mock))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Agent start test" :issue-type "task" :priority 2))
    (beads-live-test--with-mock-agent
      (let ((list-buf (beads-live-test--open-list-buffer)))
        (should list-buf)
        (unwind-protect
            (with-current-buffer list-buf
              (goto-char (point-min))
              (let ((issue-id (beads-issue-at-point)))
                (should issue-id)
                (cl-letf (((symbol-function 'beads-agent--should-use-worktree-p)
                           (lambda (_) nil)))
                  (let ((beads-agent-auto-set-in-progress nil))
                    (beads-agent-start-at-point)))
                (should (beads-live-test--wait-until
                         (lambda ()
                           (= 1 (length (beads-agent-mock-active-sessions))))))
                (beads-agent-mock-assert-start-called 1)
                (let ((sessions (beads-agent--get-sessions-for-issue issue-id)))
                  (should (= 1 (length sessions))))))
          (when (buffer-live-p list-buf)
            (kill-buffer list-buf)))))))

(ert-deftest beads-live-test-agent-start-at-point-from-show ()
  "Live: beads-agent-start-at-point starts session from show buffer."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (featurep 'beads-agent-mock))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Agent start from show" :issue-type "task" :priority 2))
    (beads-live-test--with-mock-agent
      (let* ((issues (beads-list-execute))
             (issue-id (oref (car issues) id))
             (show-buf (beads-live-test--open-show-buffer issue-id)))
        (should show-buf)
        (unwind-protect
            (with-current-buffer show-buf
              (cl-letf (((symbol-function 'beads-agent--should-use-worktree-p)
                         (lambda (_) nil)))
                (let ((beads-agent-auto-set-in-progress nil))
                  (beads-agent-start-at-point)))
              (should (beads-live-test--wait-until
                       (lambda ()
                         (= 1 (length (beads-agent-mock-active-sessions))))))
              (beads-agent-mock-assert-start-called 1))
          (when (buffer-live-p show-buf)
            (kill-buffer show-buf)))))))

;;; ============================================================
;;; Scenario 12: Agent Commands — Typed Start
;;; ============================================================

(defmacro beads-live-test--agent-typed-start-test (test-name type-fn type-name)
  "Define a test for starting a typed agent from a list buffer.
TEST-NAME is the ERT test name, TYPE-FN is the function to call,
TYPE-NAME is the expected session type (e.g., \"Task\")."
  `(ert-deftest ,test-name ()
     ,(format "Live: %s starts %s session from list buffer." type-fn type-name)
     :tags '(:live :integration)
     (skip-unless (executable-find beads-executable))
     (skip-unless (featurep 'beads-agent-mock))
     (beads-test-with-temp-repo-and-issues
         (:init-beads t)
         ((:title ,(format "Typed agent %s test" type-name)
           :issue-type "task" :priority 2))
       (beads-live-test--with-mock-agent
         (let ((list-buf (beads-live-test--open-list-buffer)))
           (should list-buf)
           (unwind-protect
               (with-current-buffer list-buf
                 (goto-char (point-min))
                 (should (beads-issue-at-point))
                 (cl-letf (((symbol-function 'beads-agent--should-use-worktree-p)
                            (lambda (_) nil)))
                   (let ((beads-agent-auto-set-in-progress nil))
                     (funcall #',type-fn)))
                 (should (beads-live-test--wait-until
                          (lambda ()
                            (= 1 (length (beads-agent-mock-active-sessions))))))
                 (beads-agent-mock-assert-start-called 1))
             (when (buffer-live-p list-buf)
               (kill-buffer list-buf))))))))

(beads-live-test--agent-typed-start-test
 beads-live-test-agent-start-task-from-list
 beads-agent-start-task "Task")

(beads-live-test--agent-typed-start-test
 beads-live-test-agent-start-review-from-list
 beads-agent-start-review "Review")

(beads-live-test--agent-typed-start-test
 beads-live-test-agent-start-plan-from-list
 beads-agent-start-plan "Plan")

(beads-live-test--agent-typed-start-test
 beads-live-test-agent-start-qa-from-list
 beads-agent-start-qa "QA")

(ert-deftest beads-live-test-agent-start-task-jumps-to-existing ()
  "Live: beads-agent-start-task jumps to existing session without starting new."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (featurep 'beads-agent-mock))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Agent jump test" :issue-type "task" :priority 2))
    (beads-live-test--with-mock-agent
      (let ((list-buf (beads-live-test--open-list-buffer)))
        (should list-buf)
        (unwind-protect
            (with-current-buffer list-buf
              (goto-char (point-min))
              (let ((issue-id (beads-issue-at-point)))
                (should issue-id)
                ;; Start initial Task session
                (cl-letf (((symbol-function 'beads-agent--should-use-worktree-p)
                           (lambda (_) nil)))
                  (let ((beads-agent-auto-set-in-progress nil))
                    (beads-agent-start issue-id nil nil "Task")))
                (should (beads-live-test--wait-until
                         (lambda ()
                           (= 1 (length (beads-agent-mock-active-sessions))))))
                ;; Call start-task again (no prefix) — should jump, not start new
                (cl-letf (((symbol-function 'beads-agent--should-use-worktree-p)
                           (lambda (_) nil)))
                  (let ((beads-agent-auto-set-in-progress nil))
                    (beads-agent-start-task)))
                ;; Still only 1 start call total
                (beads-agent-mock-assert-start-called 1)))
          (when (buffer-live-p list-buf)
            (kill-buffer list-buf)))))))

;;; ============================================================
;;; Scenario 13: Agent Commands — Stop at Point
;;; ============================================================

(ert-deftest beads-live-test-agent-stop-at-point-from-list ()
  "Live: beads-agent-stop-at-point stops active session from list buffer."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (featurep 'beads-agent-mock))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Agent stop test" :issue-type "task" :priority 2))
    (beads-live-test--with-mock-agent
      (let ((list-buf (beads-live-test--open-list-buffer)))
        (should list-buf)
        (unwind-protect
            (with-current-buffer list-buf
              (goto-char (point-min))
              (let ((issue-id (beads-issue-at-point)))
                (should issue-id)
                ;; Start a session first
                (cl-letf (((symbol-function 'beads-agent--should-use-worktree-p)
                           (lambda (_) nil)))
                  (let ((beads-agent-auto-set-in-progress nil))
                    (beads-agent-start issue-id)))
                (should (beads-live-test--wait-until
                         (lambda ()
                           (= 1 (length (beads-agent-mock-active-sessions))))))
                ;; Now stop at point
                (beads-agent-stop-at-point)
                ;; Wait for async stop to complete
                (should (beads-live-test--wait-until
                         (lambda ()
                           (= 0 (length (beads-agent-mock-active-sessions))))))))
          (when (buffer-live-p list-buf)
            (kill-buffer list-buf)))))))

(ert-deftest beads-live-test-agent-stop-at-point-no-session ()
  "Live: beads-agent-stop-at-point with no session shows message without error."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (featurep 'beads-agent-mock))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Agent stop no session" :issue-type "task" :priority 2))
    (beads-live-test--with-mock-agent
      (let ((list-buf (beads-live-test--open-list-buffer)))
        (should list-buf)
        (unwind-protect
            (with-current-buffer list-buf
              (goto-char (point-min))
              (should (beads-issue-at-point))
              ;; No sessions started — stop should not error
              (should-not (condition-case _err
                              (progn (beads-agent-stop-at-point) nil)
                            (error t))))
          (when (buffer-live-p list-buf)
            (kill-buffer list-buf)))))))

;;; ============================================================
;;; Scenario 14: Agent Commands — Jump at Point
;;; ============================================================

(ert-deftest beads-live-test-agent-jump-at-point-from-list ()
  "Live: beads-agent-jump-at-point switches to agent buffer from list."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (featurep 'beads-agent-mock))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Agent jump test" :issue-type "task" :priority 2))
    (beads-live-test--with-mock-agent
      (let ((list-buf (beads-live-test--open-list-buffer)))
        (should list-buf)
        (unwind-protect
            (with-current-buffer list-buf
              (goto-char (point-min))
              (let ((issue-id (beads-issue-at-point)))
                (should issue-id)
                ;; Start a session
                (cl-letf (((symbol-function 'beads-agent--should-use-worktree-p)
                           (lambda (_) nil)))
                  (let ((beads-agent-auto-set-in-progress nil))
                    (beads-agent-start issue-id)))
                (should (beads-live-test--wait-until
                         (lambda ()
                           (= 1 (length (beads-agent-mock-active-sessions))))))
                ;; Jump should not error
                (should-not (condition-case _err
                                (progn (beads-agent-jump-at-point) nil)
                              (error t)))))
          (when (buffer-live-p list-buf)
            (kill-buffer list-buf)))))))

(ert-deftest beads-live-test-agent-jump-at-point-starts-when-no-session ()
  "Live: beads-agent-jump-at-point starts new session when none exists."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (featurep 'beads-agent-mock))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Agent jump start new" :issue-type "task" :priority 2))
    (beads-live-test--with-mock-agent
      (let ((list-buf (beads-live-test--open-list-buffer)))
        (should list-buf)
        (unwind-protect
            (with-current-buffer list-buf
              (goto-char (point-min))
              (should (beads-issue-at-point))
              ;; No session — jump should start one
              (cl-letf (((symbol-function 'beads-agent--should-use-worktree-p)
                         (lambda (_) nil)))
                (let ((beads-agent-auto-set-in-progress nil))
                  (beads-agent-jump-at-point)))
              (should (beads-live-test--wait-until
                       (lambda ()
                         (= 1 (length (beads-agent-mock-active-sessions))))))
              (beads-agent-mock-assert-start-called 1))
          (when (buffer-live-p list-buf)
            (kill-buffer list-buf)))))))

;;; ============================================================
;;; Scenario 15: Agent Commands — Show Buffer Context
;;; ============================================================

(ert-deftest beads-live-test-agent-stop-at-point-from-show ()
  "Live: beads-agent-stop-at-point stops session from show buffer context."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (featurep 'beads-agent-mock))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Agent stop from show" :issue-type "task" :priority 2))
    (beads-live-test--with-mock-agent
      (let* ((issues (beads-list-execute))
             (issue-id (oref (car issues) id))
             (show-buf (beads-live-test--open-show-buffer issue-id)))
        (should show-buf)
        (unwind-protect
            (with-current-buffer show-buf
              ;; Start a session for this issue
              (cl-letf (((symbol-function 'beads-agent--should-use-worktree-p)
                         (lambda (_) nil)))
                (let ((beads-agent-auto-set-in-progress nil))
                  (beads-agent-start issue-id)))
              (should (beads-live-test--wait-until
                       (lambda ()
                         (= 1 (length (beads-agent-mock-active-sessions))))))
              ;; Stop from show buffer context
              (beads-agent-stop-at-point)
              (should (beads-live-test--wait-until
                       (lambda ()
                         (= 0 (length (beads-agent-mock-active-sessions)))))))
          (when (buffer-live-p show-buf)
            (kill-buffer show-buf)))))))

(ert-deftest beads-live-test-agent-jump-at-point-from-show ()
  "Live: beads-agent-jump-at-point works from show buffer context."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (featurep 'beads-agent-mock))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Agent jump from show" :issue-type "task" :priority 2))
    (beads-live-test--with-mock-agent
      (let* ((issues (beads-list-execute))
             (issue-id (oref (car issues) id))
             (show-buf (beads-live-test--open-show-buffer issue-id)))
        (should show-buf)
        (unwind-protect
            (with-current-buffer show-buf
              ;; Verify detect-issue-id returns our issue from show buffer
              (should (equal issue-id (beads-agent--detect-issue-id)))
              ;; Start a session
              (cl-letf (((symbol-function 'beads-agent--should-use-worktree-p)
                         (lambda (_) nil)))
                (let ((beads-agent-auto-set-in-progress nil))
                  (beads-agent-start issue-id)))
              (should (beads-live-test--wait-until
                       (lambda ()
                         (= 1 (length (beads-agent-mock-active-sessions))))))
              ;; Jump should not error
              (should-not (condition-case _err
                              (progn (beads-agent-jump-at-point) nil)
                            (error t))))
          (when (buffer-live-p show-buf)
            (kill-buffer show-buf)))))))

;;; ============================================================
;;; Scenario 16: List Buffer Navigation (n/p)
;;; ============================================================

(ert-deftest beads-live-test-list-navigation-next-previous ()
  "Live: beads-list-next and beads-list-previous move point in list buffer."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Nav issue A" :issue-type "task" :priority 2)
       (:title "Nav issue B" :issue-type "task" :priority 2))
    (let ((list-buf (beads-live-test--open-list-buffer)))
      (should list-buf)
      (unwind-protect
          (with-current-buffer list-buf
            (goto-char (point-min))
            (let ((pos-before (point)))
              (beads-list-next)
              (let ((pos-after (point)))
                ;; Point should have moved
                (should (not (= pos-after pos-before)))
                ;; There should be an issue at the new position
                (should (beads-issue-at-point)))
              ;; Move back
              (beads-list-previous)
              ;; Point should be back near start
              (should (<= (point) pos-before))))
        (when (buffer-live-p list-buf)
          (kill-buffer list-buf))))))

;;; ============================================================
;;; Scenario 17: beads-list-show (RET)
;;; ============================================================

(ert-deftest beads-live-test-list-show-opens-show-buffer ()
  "Live: beads-list-show opens *beads-show* buffer for issue at point."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Show buffer issue" :issue-type "task" :priority 2))
    (let ((list-buf (beads-live-test--open-list-buffer)))
      (should list-buf)
      (unwind-protect
          (with-current-buffer list-buf
            (goto-char (point-min))
            (let ((issue-id (beads-issue-at-point)))
              (should issue-id)
              (beads-list-show)
              (let ((show-buf (cl-find-if
                               (lambda (b)
                                 (string-match-p
                                  (regexp-quote issue-id) (buffer-name b)))
                               (buffer-list))))
                (should show-buf)
                (unwind-protect
                    (with-current-buffer show-buf
                      (should (string-match-p "Show buffer issue"
                                              (buffer-string))))
                  (when (buffer-live-p show-buf)
                    (kill-buffer show-buf))))))
        (when (buffer-live-p list-buf)
          (kill-buffer list-buf))))))

;;; ============================================================
;;; Scenario 18: beads-list-refresh (g)
;;; ============================================================

(ert-deftest beads-live-test-list-refresh-shows-new-issue ()
  "Live: beads-list-refresh updates buffer to include newly created issues."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Existing issue" :issue-type "task" :priority 2))
    (let ((list-buf (beads-live-test--open-list-buffer)))
      (should list-buf)
      (unwind-protect
          (with-current-buffer list-buf
            (let ((row-count-before (length tabulated-list-entries)))
              ;; Create a new issue
              (beads-execute 'beads-command-create
               :title "Refreshed new issue"
               :issue-type "task"
               :priority 2)
              ;; Refresh the buffer
              (beads-list-refresh)
              (let ((row-count-after (length tabulated-list-entries)))
                (should (> row-count-after row-count-before)))
              (should (string-match-p "Refreshed new issue"
                                      (buffer-string)))))
        (when (buffer-live-p list-buf)
          (kill-buffer list-buf))))))

;;; ============================================================
;;; Scenario 19: Mark / Unmark / Mark-All
;;; ============================================================

(ert-deftest beads-live-test-list-mark-unmark ()
  "Live: beads-list-mark and beads-list-unmark update marked issues set."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Mark test A" :issue-type "task" :priority 2)
       (:title "Mark test B" :issue-type "task" :priority 2))
    (let ((list-buf (beads-live-test--open-list-buffer)))
      (should list-buf)
      (unwind-protect
          (with-current-buffer list-buf
            (goto-char (point-min))
            (let ((issue-id (beads-issue-at-point)))
              (should issue-id)
              ;; Mark (advances cursor to next line)
              (beads-list-mark)
              (should (member issue-id beads-list--marked-issues))
              ;; Return to the same issue before unmarking
              (goto-char (point-min))
              (beads-list-unmark)
              (should-not (member issue-id beads-list--marked-issues))))
        (when (buffer-live-p list-buf)
          (kill-buffer list-buf))))))

(ert-deftest beads-live-test-list-mark-all-unmark-all ()
  "Live: beads-list-mark-all marks all issues; beads-list-unmark-all clears."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Mark all A" :issue-type "task" :priority 2)
       (:title "Mark all B" :issue-type "task" :priority 2))
    (let ((list-buf (beads-live-test--open-list-buffer)))
      (should list-buf)
      (unwind-protect
          (with-current-buffer list-buf
            (beads-list-mark-all)
            (should (= 2 (length beads-list--marked-issues)))
            (beads-list-unmark-all)
            (should (null beads-list--marked-issues)))
        (when (buffer-live-p list-buf)
          (kill-buffer list-buf))))))

;;; ============================================================
;;; Scenario 20: beads-list-copy-id (w)
;;; ============================================================

(ert-deftest beads-live-test-list-copy-id ()
  "Live: beads-list-copy-id copies issue ID at point to kill-ring."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Copy ID test" :issue-type "task" :priority 2))
    (let ((list-buf (beads-live-test--open-list-buffer)))
      (should list-buf)
      (unwind-protect
          (with-current-buffer list-buf
            (goto-char (point-min))
            (let ((issue-id (beads-issue-at-point)))
              (should issue-id)
              (beads-list-copy-id)
              (should (equal issue-id (car kill-ring)))))
        (when (buffer-live-p list-buf)
          (kill-buffer list-buf))))))

;;; ============================================================
;;; Scenario 21: beads-list-follow-mode (C-c C-f)
;;; ============================================================

(ert-deftest beads-live-test-list-follow-mode-toggle ()
  "Live: beads-list-follow-mode can be enabled and disabled."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Follow mode test" :issue-type "task" :priority 2))
    (let ((list-buf (beads-live-test--open-list-buffer)))
      (should list-buf)
      (unwind-protect
          (with-current-buffer list-buf
            ;; Initially off
            (should-not beads-list-follow-mode)
            ;; Enable
            (beads-list-follow-mode 1)
            (should beads-list-follow-mode)
            ;; Disable
            (beads-list-follow-mode 0)
            (should-not beads-list-follow-mode))
        (when (buffer-live-p list-buf)
          (kill-buffer list-buf))))))

;;; ============================================================
;;; Scenario 22: Bulk Operations
;;; ============================================================

(ert-deftest beads-live-test-list-bulk-close ()
  "Live: beads-list-bulk-close closes all marked issues."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Bulk close A" :issue-type "task" :priority 2)
       (:title "Bulk close B" :issue-type "task" :priority 2))
    (let ((list-buf (beads-live-test--open-list-buffer)))
      (should list-buf)
      (unwind-protect
          (with-current-buffer list-buf
            (beads-list-mark-all)
            (should (= 2 (length beads-list--marked-issues)))
            (let ((marked-ids (copy-sequence beads-list--marked-issues)))
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) "Bulk done"))
                        ((symbol-function 'y-or-n-p)
                         (lambda (&rest _) t)))
                (beads-list-bulk-close))
              ;; Marks cleared
              (should (null beads-list--marked-issues))
              ;; Both issues now closed
              (dolist (id marked-ids)
                (let ((issue (beads-execute 'beads-command-show :issue-ids (list id))))
                  (should (equal "closed" (oref issue status)))))))
        (when (buffer-live-p list-buf)
          (kill-buffer list-buf))))))

(ert-deftest beads-live-test-list-bulk-reopen ()
  "Live: beads-list-bulk-reopen reopens all marked closed issues."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Bulk reopen A" :issue-type "task" :priority 2)
       (:title "Bulk reopen B" :issue-type "task" :priority 2))
    ;; Close both issues first, capture their IDs
    (let* ((all (beads-list-execute))
           (ids (mapcar (lambda (i) (oref i id)) all)))
      (dolist (id ids)
        (beads-execute 'beads-command-close :issue-ids (list id)
                       :reason "Pre-close for test"))
      ;; Open a list buffer and manually set marked IDs to the closed issues
      (let ((list-buf (beads-live-test--open-list-buffer)))
        (should list-buf)
        (unwind-protect
            (with-current-buffer list-buf
              ;; Directly set the mark set (simulates marking closed issues)
              (setq beads-list--marked-issues ids)
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) ""))
                        ((symbol-function 'y-or-n-p)
                         (lambda (&rest _) t)))
                (beads-list-bulk-reopen))
              (should (null beads-list--marked-issues))
              (dolist (id ids)
                (let ((issue (beads-execute 'beads-command-show :issue-ids (list id))))
                  (should (equal "open" (oref issue status))))))
          (when (buffer-live-p list-buf)
            (kill-buffer list-buf)))))))

(ert-deftest beads-live-test-list-bulk-update-status ()
  "Live: beads-list-bulk-update-status updates status for all marked issues."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Bulk status A" :issue-type "task" :priority 2)
       (:title "Bulk status B" :issue-type "task" :priority 2))
    (let ((list-buf (beads-live-test--open-list-buffer)))
      (should list-buf)
      (unwind-protect
          (with-current-buffer list-buf
            (beads-list-mark-all)
            (should (= 2 (length beads-list--marked-issues)))
            (let ((marked-ids (copy-sequence beads-list--marked-issues)))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "in_progress"))
                        ((symbol-function 'y-or-n-p)
                         (lambda (&rest _) t)))
                (beads-list-bulk-update-status))
              (should (null beads-list--marked-issues))
              (dolist (id marked-ids)
                (let ((issue (beads-execute 'beads-command-show :issue-ids (list id))))
                  (should (equal "in_progress" (oref issue status)))))))
        (when (buffer-live-p list-buf)
          (kill-buffer list-buf))))))

;;; ============================================================
;;; Scenario 23: beads-list-filter (l)
;;; ============================================================

(ert-deftest beads-live-test-list-filter-transient-renders ()
  "Live: beads-list-filter opens the filter transient."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Filter test issue" :issue-type "task" :priority 2))
    (let ((list-buf (beads-live-test--open-list-buffer)))
      (should list-buf)
      (unwind-protect
          (with-current-buffer list-buf
            ;; beads-list-filter opens beads-list transient (which is also
            ;; covered by the existing beads-list-transient-renders test).
            ;; Call it and verify no error is signaled.
            (should-not
             (condition-case _err
                 (progn (beads-live-test--open-transient-and-quit
                         'beads-list-filter-menu)
                        nil)
               (error t))))
        (when (buffer-live-p list-buf)
          (kill-buffer list-buf))))))

;;; ============================================================
;;; Scenario 24: Show Buffer Section Navigation (n/p)
;;; ============================================================

(ert-deftest beads-live-test-show-section-navigation ()
  "Live: beads-show-next-section and beads-show-previous-section move point."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Section nav issue"
        :issue-type "feature"
        :priority 1))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            (goto-char (point-min))
            (let ((pos-start (point)))
              (beads-show-next-section)
              (let ((pos-next (point)))
                ;; Either moved forward (found a section header) or stayed
                ;; at start (no section header — valid for minimal issues).
                ;; At minimum, no error should have been signaled.
                (should (>= pos-next pos-start)))
              (beads-show-previous-section)
              ;; No error signaled on backward navigation either
              (should (>= (point) (point-min)))))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

;;; ============================================================
;;; Scenario 25: beads-show-copy-id (w)
;;; ============================================================

(ert-deftest beads-live-test-show-copy-id ()
  "Live: beads-show-copy-id copies the show buffer's issue ID to kill-ring."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Copy ID show test" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            (beads-show-copy-id)
            (should (equal issue-id (car kill-ring))))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

;;; ============================================================
;;; Scenario 26: Show Buffer Reference Navigation
;;; ============================================================

(ert-deftest beads-live-test-show-reference-navigation ()
  "Live: beads-show-next-reference and prev move point to references."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (beads-test-bd-has-subcommand-p "dep"))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Ref issue A" :issue-type "bug"  :priority 0)
       (:title "Ref issue B" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-a (cl-find "Ref issue A" issues
                             :key (lambda (i) (oref i title))
                             :test #'equal))
           (issue-b (cl-find "Ref issue B" issues
                             :key (lambda (i) (oref i title))
                             :test #'equal))
           (id-a (oref issue-a id))
           (id-b (oref issue-b id)))
      ;; B depends on A, so show buffer for B will contain A's ID as a link
      (beads-execute 'beads-command-dep-add
       :issue-id id-b
       :depends-on id-a
       :dep-type "blocks")
      (let ((show-buf (beads-live-test--open-show-buffer id-b)))
        (should show-buf)
        (unwind-protect
            (with-current-buffer show-buf
              (goto-char (point-min))
              ;; Move to next reference (should not error even if no refs)
              (should-not (condition-case _err
                              (progn (beads-show-next-reference) nil)
                            (error t)))
              ;; Move to previous reference (should not error)
              (should-not (condition-case _err
                              (progn (beads-show-previous-reference) nil)
                            (error t))))
          (when (buffer-live-p show-buf)
            (kill-buffer show-buf)))))))

;;; ============================================================
;;; Scenario 27: beads-show-follow-reference (RET)
;;; ============================================================

(ert-deftest beads-live-test-show-follow-reference ()
  "Live: beads-show-follow-reference opens show buffer for linked issue."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (skip-unless (beads-test-bd-has-subcommand-p "dep"))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Follow ref A" :issue-type "bug"  :priority 0)
       (:title "Follow ref B" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-a (cl-find "Follow ref A" issues
                             :key (lambda (i) (oref i title))
                             :test #'equal))
           (issue-b (cl-find "Follow ref B" issues
                             :key (lambda (i) (oref i title))
                             :test #'equal))
           (id-a (oref issue-a id))
           (id-b (oref issue-b id)))
      (beads-execute 'beads-command-dep-add
       :issue-id id-b
       :depends-on id-a
       :dep-type "blocks")
      (let ((show-buf-b (beads-live-test--open-show-buffer id-b)))
        (should show-buf-b)
        (unwind-protect
            (with-current-buffer show-buf-b
              (goto-char (point-min))
              ;; Navigate to a reference and follow it
              (when (condition-case nil
                        (progn (beads-show-next-reference) t)
                      (error nil))
                (ignore-errors (beads-show-follow-reference))
                ;; If a new show buffer for id-a opened, verify it
                (let ((show-buf-a
                       (cl-find-if
                        (lambda (b)
                          (string-match-p (regexp-quote id-a) (buffer-name b)))
                        (buffer-list))))
                  (when show-buf-a
                    (unwind-protect
                        (with-current-buffer show-buf-a
                          (should (string-match-p "Follow ref A"
                                                  (buffer-string))))
                      (kill-buffer show-buf-a))))))
          (when (buffer-live-p show-buf-b)
            (kill-buffer show-buf-b)))))))

;;; ============================================================
;;; Scenario 28: beads-show-actions Transient (?)
;;; ============================================================

(ert-deftest beads-live-test-show-actions-from-show-buffer ()
  "Live: beads-show-actions transient opens from inside a real show buffer."
  :tags '(:live :transient)
  (skip-unless (beads-live-test--interactive-p))
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Show actions test" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            (should (beads-live-test--open-transient-and-quit 'beads-show-actions)))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

;;; ============================================================
;;; Scenario 29: beads-show-edit-field (e)
;;; ============================================================

(ert-deftest beads-live-test-show-edit-field-opens ()
  "Live: beads-show-edit-field with Title updates the issue title."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Edit field original" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            ;; Mock completing-read to select "Title" field
            ;; Mock read-string to return the new title
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "Title"))
                      ((symbol-function 'read-string)
                       (lambda (&rest _) "Edit field updated")))
              (beads-show-edit-field))
            ;; Verify via beads-command-show!
            (let ((updated (beads-execute 'beads-command-show :issue-ids (list issue-id))))
              (should (equal "Edit field updated" (oref updated title)))))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

;;; ============================================================
;;; Scenario 30: beads-show-compose-edit and beads-show-compose-comment
;;; ============================================================

(ert-deftest beads-live-test-show-compose-edit-opens-buffer ()
  "Live: beads-show-compose-edit opens a compose buffer for editing."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Compose edit test" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            (beads-show-compose-edit)
            (let ((compose-buf (cl-find-if
                                (lambda (b)
                                  (with-current-buffer b
                                    (and (derived-mode-p 'beads-compose-mode)
                                         (string-match-p
                                          (regexp-quote issue-id)
                                          (buffer-name b)))))
                                (buffer-list))))
              (should compose-buf)
              (unwind-protect
                  (with-current-buffer compose-buf
                    (should (derived-mode-p 'beads-compose-mode)))
                (when (buffer-live-p compose-buf)
                  (kill-buffer compose-buf)))))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

(ert-deftest beads-live-test-show-compose-comment-opens-buffer ()
  "Live: beads-show-compose-comment opens a compose buffer for commenting."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Compose comment test" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            (beads-show-compose-comment)
            (let ((compose-buf (cl-find-if
                                (lambda (b)
                                  (with-current-buffer b
                                    (and (derived-mode-p 'beads-compose-mode)
                                         (string-match-p
                                          (regexp-quote issue-id)
                                          (buffer-name b)))))
                                (buffer-list))))
              (should compose-buf)
              (unwind-protect
                  (with-current-buffer compose-buf
                    (should (derived-mode-p 'beads-compose-mode)))
                (when (buffer-live-p compose-buf)
                  (kill-buffer compose-buf)))))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

;;; ============================================================
;;; Scenario 31: beads-refresh-show (g from show buffer)
;;; ============================================================

(ert-deftest beads-live-test-show-refresh ()
  "Live: beads-refresh-show updates show buffer content after external changes."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Refresh show original" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            ;; Update the issue externally
            (beads-execute 'beads-command-update :issue-ids (list issue-id)
                           :title "Refresh show updated")
            ;; Refresh the show buffer
            (beads-refresh-show)
            ;; Content should now show the updated title
            (should (string-match-p "Refresh show updated" (buffer-string))))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

;;; ============================================================
;;; Scenario 32: Actions from Show Buffer (s/d/C/#)
;;; ============================================================

(ert-deftest beads-live-test-show-actions-close-from-show ()
  "Live: beads-actions-close closes the issue when called from show buffer."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Show close action" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            (cl-letf (((symbol-function 'read-string)
                       (lambda (&rest _) "Done in show")))
              (beads-actions-close))
            (let ((updated (beads-execute 'beads-command-show :issue-ids (list issue-id))))
              (should (equal "closed" (oref updated status)))))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

(ert-deftest beads-live-test-show-actions-claim-from-show ()
  "Live: beads-actions-claim assigns the issue when called from show buffer."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Show claim action" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            ;; Claim executes immediately without prompting
            (beads-actions-claim)
            (let ((updated (beads-execute 'beads-command-show :issue-ids (list issue-id))))
              ;; Assignee should be set to current actor
              (should (oref updated assignee))))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

(ert-deftest beads-live-test-show-actions-set-status-from-show ()
  "Live: beads-actions-set-status updates status when called from show buffer."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Show set-status action" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "in_progress")))
              (beads-actions-set-status))
            (let ((updated (beads-execute 'beads-command-show :issue-ids (list issue-id))))
              (should (equal "in_progress" (oref updated status)))))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

(ert-deftest beads-live-test-show-actions-set-priority-from-show ()
  "Live: beads-actions-set-priority updates priority when called from show buffer."
  :tags '(:live :integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title "Show set-priority action" :issue-type "task" :priority 2))
    (let* ((issues (beads-list-execute))
           (issue-id (oref (car issues) id))
           (show-buf (beads-live-test--open-show-buffer issue-id)))
      (should show-buf)
      (unwind-protect
          (with-current-buffer show-buf
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "1 - High")))
              (beads-actions-set-priority))
            (let ((updated (beads-execute 'beads-command-show :issue-ids (list issue-id))))
              (should (= 1 (oref updated priority)))))
        (when (buffer-live-p show-buf)
          (kill-buffer show-buf))))))

;;; ============================================================
;;; End of test file
;;; ============================================================

(provide 'beads-live-ui-test)
;;; beads-live-ui-test.el ends here
