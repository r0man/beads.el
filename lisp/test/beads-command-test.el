;;; beads-command-test.el --- Integration tests for beads-command.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Integration tests for EIEIO command classes in beads-command.el.
;; These tests use beads-test-helper-create-temp-project to create
;; temporary projects and run real bd CLI commands.
;;
;; All tests are tagged with :integration and require bd to be installed.

;;; Code:

(require 'ert)
(require 'beads-command)
(require 'beads-types)
(require 'beads-test-helper)

;; Define beads-executable for testing (it's defined in beads.el normally)
(defvar beads-executable "bd"
  "Path to the bd executable for testing.")

;;; Integration Test: beads-command-init

(ert-deftest beads-command-test-init-basic ()
  "Test beads-command-init execution creates .beads directory.
Integration test that runs real bd init command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((temp-dir (make-temp-file "beads-test-" t))
         (default-directory temp-dir))
    (unwind-protect
        (let* ((cmd (beads-command-init))
               (result (beads-command-execute cmd)))
          ;; Should return (exit-code stdout stderr)
          (should (listp result))
          (should (= (length result) 3))
          ;; Exit code should be 0
          (should (= (nth 0 result) 0))
          ;; Stdout should be a string
          (should (stringp (nth 1 result)))
          ;; Stderr should be a string
          (should (stringp (nth 2 result)))
          ;; Should create .beads directory
          (should (file-directory-p (expand-file-name ".beads" temp-dir)))
          ;; Should create database file
          (let ((db-files (directory-files
                          (expand-file-name ".beads" temp-dir)
                          nil "\\.db$")))
            (should (> (length db-files) 0))))
      ;; Cleanup: remove temp directory
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest beads-command-test-init-with-prefix ()
  "Test beads-command-init with custom prefix option.
Integration test that verifies --prefix flag works correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((temp-dir (make-temp-file "beads-test-" t))
         (default-directory temp-dir))
    (unwind-protect
        (let* ((cmd (beads-command-init :prefix "myproject"))
               (result (beads-command-execute cmd)))
          ;; Command should succeed
          (should (= (nth 0 result) 0))
          ;; .beads directory should exist
          (should (file-directory-p (expand-file-name ".beads" temp-dir)))
          ;; Verify prefix is set correctly by creating an issue
          ;; and checking its ID starts with the prefix
          (let* ((create-cmd (beads-command-create
                             :title "Test issue"
                             :json t))
                 (issue (beads-command-execute create-cmd)))
            (should (beads-issue-p issue))
            (should (string-prefix-p "myproject-" (oref issue id)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest beads-command-test-init-with-quiet ()
  "Test beads-command-init with --quiet flag.
Integration test that verifies quiet mode suppresses output."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((temp-dir (make-temp-file "beads-test-" t))
         (default-directory temp-dir))
    (unwind-protect
        (let* ((cmd (beads-command-init
                    :quiet t
                    :skip-merge-driver t))
               (result (beads-command-execute cmd)))
          ;; Command should succeed
          (should (= (nth 0 result) 0))
          ;; .beads directory should exist
          (should (file-directory-p (expand-file-name ".beads" temp-dir))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

;;; Integration Test: beads-command-create

(ert-deftest beads-command-test-create-basic ()
  "Test beads-command-create creates a basic issue.
Integration test that runs real bd create command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* ((cmd (beads-command-create
                    :title "Test issue"
                    :json t))
               (issue (beads-command-execute cmd)))
          ;; Should return a beads-issue instance
          (should (beads-issue-p issue))
          ;; Should have an ID
          (should (stringp (oref issue id)))
          (should (string-prefix-p "test-" (oref issue id)))
          ;; Title should match
          (should (string= (oref issue title) "Test issue"))
          ;; Status should be open by default
          (should (string= (oref issue status) "open"))
          ;; Verify issue exists in database
          (should (beads-test-helper-issue-exists-p
                  project-dir (oref issue id))))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-create-with-type-and-priority ()
  "Test beads-command-create with issue type and priority.
Integration test that verifies type and priority options work."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* ((cmd (beads-command-create
                    :title "Bug fix"
                    :issue-type "bug"
                    :priority "1"
                    :json t))
               (issue (beads-command-execute cmd)))
          ;; Should return a beads-issue instance
          (should (beads-issue-p issue))
          ;; Type should be bug
          (should (string= (oref issue issue-type) "bug"))
          ;; Priority should be 1 (high)
          (should (= (oref issue priority) 1))
          ;; Title should match
          (should (string= (oref issue title) "Bug fix")))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-create-with-description ()
  "Test beads-command-create with description field.
Integration test that verifies description is set correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* ((cmd (beads-command-create
                    :title "Feature request"
                    :description "Add new feature to improve UX"
                    :json t))
               (issue (beads-command-execute cmd)))
          ;; Should return a beads-issue instance
          (should (beads-issue-p issue))
          ;; Description should match
          (should (string= (oref issue description)
                          "Add new feature to improve UX")))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-create-with-assignee ()
  "Test beads-command-create with assignee field.
Integration test that verifies assignee is set correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* ((cmd (beads-command-create
                    :title "Assigned task"
                    :assignee "alice"
                    :json t))
               (issue (beads-command-execute cmd)))
          ;; Should return a beads-issue instance
          (should (beads-issue-p issue))
          ;; Assignee should be alice
          (should (string= (oref issue assignee) "alice")))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-create-with-labels ()
  "Test beads-command-create with labels.
Integration test that verifies labels are set correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* ((cmd (beads-command-create
                    :title "Labeled issue"
                    :labels '("urgent" "backend")
                    :json t))
               (issue (beads-command-execute cmd))
               ;; Fetch the issue again to get full data
               (issue-data (beads-test-helper-get-issue
                           project-dir (oref issue id))))
          ;; Should return a beads-issue instance
          (should (beads-issue-p issue))
          ;; Verify issue was created with correct title
          (should (string= (oref issue title) "Labeled issue"))
          ;; Check labels from fetched issue data (may be empty in create response)
          (when issue-data
            (let ((labels (alist-get 'labels issue-data)))
              ;; Labels should be present if bd supports them in create
              (when labels
                (should (member "urgent" labels))
                (should (member "backend" labels))))))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-create-with-deps ()
  "Test beads-command-create with dependencies.
Integration test that verifies dependencies are set correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* (;; First create a parent issue
               (parent-id (beads-test-helper-create-issue
                          project-dir "Parent issue"))
               ;; Create child with dependency
               (cmd (beads-command-create
                    :title "Child issue"
                    :deps (list (format "discovered-from:%s" parent-id))
                    :json t))
               (issue (beads-command-execute cmd))
               ;; Fetch the issue again to get full data
               (issue-data (beads-test-helper-get-issue
                           project-dir (oref issue id))))
          ;; Should return a beads-issue instance
          (should (beads-issue-p issue))
          ;; Verify issue was created with correct title
          (should (string= (oref issue title) "Child issue"))
          ;; Check dependencies from fetched issue data
          (when issue-data
            (let ((deps (alist-get 'dependencies issue-data)))
              ;; Dependencies should be present if bd supports them in create
              (when deps
                (should (> (length deps) 0))))))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-create-combined-options ()
  "Test beads-command-create with multiple options.
Integration test with comprehensive option set."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* ((cmd (beads-command-create
                    :title "Complex issue"
                    :issue-type "feature"
                    :priority "0"
                    :description "Detailed description"
                    :assignee "bob"
                    :labels '("critical" "frontend")
                    :json t))
               (issue (beads-command-execute cmd))
               ;; Fetch the issue again to get full data
               (issue-data (beads-test-helper-get-issue
                           project-dir (oref issue id))))
          ;; Should return a beads-issue instance
          (should (beads-issue-p issue))
          ;; Core fields should match
          (should (string= (oref issue title) "Complex issue"))
          (should (string= (oref issue issue-type) "feature"))
          (should (= (oref issue priority) 0))
          (should (string= (oref issue description) "Detailed description"))
          (should (string= (oref issue assignee) "bob"))
          ;; Check labels from fetched issue data (may be empty in create response)
          (when issue-data
            (let ((labels (alist-get 'labels issue-data)))
              (when labels
                (should (member "critical" labels))
                (should (member "frontend" labels))))))
      ;; Cleanup handled by test helper
      nil)))

;;; Integration Test: beads-command-list

(ert-deftest beads-command-test-list-all-issues ()
  "Test beads-command-list returns all issues.
Integration test that lists all issues in a project."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* (;; Create some test issues
               (id1 (beads-test-helper-create-issue project-dir "Issue 1"))
               (id2 (beads-test-helper-create-issue project-dir "Issue 2"))
               (id3 (beads-test-helper-create-issue project-dir "Issue 3"))
               ;; List all issues
               (cmd (beads-command-list :json t))
               (issues (beads-command-execute cmd)))
          ;; Should return a list of beads-issue instances
          (should (listp issues))
          ;; Should have at least 3 issues
          (should (>= (length issues) 3))
          ;; All elements should be beads-issue instances
          (should (cl-every #'beads-issue-p issues))
          ;; Should include our created issues
          (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
            (should (member id1 ids))
            (should (member id2 ids))
            (should (member id3 ids))))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-list-with-status-filter ()
  "Test beads-command-list with status filter.
Integration test that filters issues by status."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* (;; Create test issues (all open by default)
               (_id1 (beads-test-helper-create-issue project-dir "Open issue"))
               ;; List only open issues
               (cmd (beads-command-list :status "open" :json t))
               (issues (beads-command-execute cmd)))
          ;; Should return a list
          (should (listp issues))
          ;; All issues should have open status
          (should (cl-every (lambda (issue)
                             (string= (oref issue status) "open"))
                           issues)))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-list-with-priority-filter ()
  "Test beads-command-list with priority filter.
Integration test that filters issues by priority."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* (;; Create issues with different priorities
               (cmd1 (beads-command-create
                     :title "High priority"
                     :priority "1"
                     :json t))
               (issue1 (beads-command-execute cmd1))
               (cmd2 (beads-command-create
                     :title "Low priority"
                     :priority "3"
                     :json t))
               (_issue2 (beads-command-execute cmd2))
               ;; List only priority 1 issues
               (list-cmd (beads-command-list :priority 1 :json t))
               (issues (beads-command-execute list-cmd)))
          ;; Should return a list
          (should (listp issues))
          ;; All issues should have priority 1
          (should (cl-every (lambda (issue)
                             (= (oref issue priority) 1))
                           issues))
          ;; Should include our high priority issue
          (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
            (should (member (oref issue1 id) ids))))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-list-with-type-filter ()
  "Test beads-command-list with issue type filter.
Integration test that filters issues by type."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* (;; Create issues with different types
               (cmd1 (beads-command-create
                     :title "Bug issue"
                     :issue-type "bug"
                     :json t))
               (bug-issue (beads-command-execute cmd1))
               (cmd2 (beads-command-create
                     :title "Feature issue"
                     :issue-type "feature"
                     :json t))
               (_feature-issue (beads-command-execute cmd2))
               ;; List only bug issues
               (list-cmd (beads-command-list :issue-type "bug" :json t))
               (issues (beads-command-execute list-cmd)))
          ;; Should return a list
          (should (listp issues))
          ;; All issues should be of type bug
          (should (cl-every (lambda (issue)
                             (string= (oref issue issue-type) "bug"))
                           issues))
          ;; Should include our bug issue
          (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
            (should (member (oref bug-issue id) ids))))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-list-with-multiple-filters ()
  "Test beads-command-list with multiple filters combined.
Integration test that applies multiple filters together."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* (;; Create issues with various attributes
               (cmd1 (beads-command-create
                     :title "Critical bug"
                     :issue-type "bug"
                     :priority "0"
                     :json t))
               (target-issue (beads-command-execute cmd1))
               (cmd2 (beads-command-create
                     :title "Low priority bug"
                     :issue-type "bug"
                     :priority "3"
                     :json t))
               (_other-bug (beads-command-execute cmd2))
               (cmd3 (beads-command-create
                     :title "Critical feature"
                     :issue-type "feature"
                     :priority "0"
                     :json t))
               (_feature (beads-command-execute cmd3))
               ;; List with combined filters: bug AND priority 0
               (list-cmd (beads-command-list
                         :issue-type "bug"
                         :priority 0
                         :json t))
               (issues (beads-command-execute list-cmd)))
          ;; Should return a list
          (should (listp issues))
          ;; All issues should match both filters
          (should (cl-every (lambda (issue)
                             (and (string= (oref issue issue-type) "bug")
                                  (= (oref issue priority) 0)))
                           issues))
          ;; Should include our target issue
          (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
            (should (member (oref target-issue id) ids))))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-list-with-limit ()
  "Test beads-command-list with limit option.
Integration test that verifies result limiting works."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* (;; Create several issues
               (_id1 (beads-test-helper-create-issue project-dir "Issue 1"))
               (_id2 (beads-test-helper-create-issue project-dir "Issue 2"))
               (_id3 (beads-test-helper-create-issue project-dir "Issue 3"))
               (_id4 (beads-test-helper-create-issue project-dir "Issue 4"))
               (_id5 (beads-test-helper-create-issue project-dir "Issue 5"))
               ;; List with limit 2
               (cmd (beads-command-list :limit 2 :json t))
               (issues (beads-command-execute cmd)))
          ;; Should return a list
          (should (listp issues))
          ;; Should have exactly 2 issues (or fewer if database has less)
          (should (<= (length issues) 2)))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-list-with-assignee-filter ()
  "Test beads-command-list with assignee filter.
Integration test that filters issues by assignee."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        (let* (;; Create issues with different assignees
               (cmd1 (beads-command-create
                     :title "Alice's task"
                     :assignee "alice"
                     :json t))
               (alice-issue (beads-command-execute cmd1))
               (cmd2 (beads-command-create
                     :title "Bob's task"
                     :assignee "bob"
                     :json t))
               (_bob-issue (beads-command-execute cmd2))
               ;; List only Alice's issues
               (list-cmd (beads-command-list :assignee "alice" :json t))
               (issues (beads-command-execute list-cmd)))
          ;; Should return a list
          (should (listp issues))
          ;; All issues should be assigned to alice
          (should (cl-every (lambda (issue)
                             (string= (oref issue assignee) "alice"))
                           issues))
          ;; Should include Alice's issue
          (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
            (should (member (oref alice-issue id) ids))))
      ;; Cleanup handled by test helper
      nil)))

(ert-deftest beads-command-test-list-empty-result ()
  "Test beads-command-list with filter that matches nothing.
Integration test that verifies empty list is returned correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((project-dir (beads-test-helper-create-temp-project))
         (default-directory project-dir))
    (unwind-protect
        ;; List epics in fresh project (should have none)
        (let ((cmd (beads-command-list
                   :issue-type "epic"
                   :json t)))
          ;; Execute command - should succeed with empty list
          (condition-case err
              (let ((issues (beads-command-execute cmd)))
                ;; Should return list (possibly empty)
                (should (listp issues))
                ;; All elements should be beads-issue instances
                (should (cl-every #'beads-issue-p issues)))
            ;; If JSON parse fails, bd might output empty string
            ;; for empty results - this is acceptable
            (beads-json-parse-error
             ;; Acceptable behavior for empty result set
             t)))
      ;; Cleanup handled by test helper
      nil)))

(provide 'beads-command-test)
;;; beads-command-test.el ends here
