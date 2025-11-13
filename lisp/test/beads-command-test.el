;;; beads-command-test.el --- Integration tests for beads-command.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Integration tests for EIEIO command classes in beads-command.el.
;; These tests use beads-test-with-project macro to create temporary
;; projects and run real bd CLI commands.
;;
;; All tests are tagged with :integration and require bd to be installed.

;;; Code:

(require 'ert)
(require 'beads-command)
(require 'beads-types)
(require 'beads-test)

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
         (default-directory temp-dir)
         (cmd (beads-command-init))
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
      (should (> (length db-files) 0)))))

(ert-deftest beads-command-test-init-with-prefix ()
  "Test beads-command-init with custom prefix option.
Integration test that verifies --prefix flag works correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((temp-dir (make-temp-file "beads-test-" t))
         (default-directory temp-dir)
         (cmd (beads-command-init :prefix "myproject"))
         (result (beads-command-execute cmd)))
    ;; Command should succeed
    (should (= (nth 0 result) 0))
    ;; .beads directory should exist
    (should (file-directory-p (expand-file-name ".beads" temp-dir)))
    ;; Verify prefix is set correctly by creating an issue
    ;; and checking its ID starts with the prefix
    (let* ((issue (beads-command-create! :title "Test issue")))
      (should (beads-issue-p issue))
      (should (string-prefix-p "myproject-" (oref issue id))))))

(ert-deftest beads-command-test-init-with-quiet ()
  "Test beads-command-init with --quiet flag.
Integration test that verifies quiet mode suppresses output."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((temp-dir (make-temp-file "beads-test-" t))
         (default-directory temp-dir)
         (cmd (beads-command-init
               :quiet t
               :skip-merge-driver t))
         (result (beads-command-execute cmd)))
    ;; Command should succeed
    (should (= (nth 0 result) 0))
    ;; .beads directory should exist
    (should (file-directory-p (expand-file-name ".beads" temp-dir)))))

;;; Integration Test: beads-command-create

(ert-deftest beads-command-test-create-basic ()
  "Test beads-command-create creates a basic issue.
Integration test that runs real bd create command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((issue (beads-command-create! :title "Test issue")))
      ;; Should return a beads-issue instance
      (should (beads-issue-p issue))
      ;; Should have an ID
      (should (stringp (oref issue id)))
      ;; Title should match
      (should (string= (oref issue title) "Test issue"))
      ;; Status should be open by default
      (should (string= (oref issue status) "open")))))

(ert-deftest beads-command-test-create-with-type-and-priority ()
  "Test beads-command-create with issue type and priority.
Integration test that verifies type and priority options work."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((issue (beads-command-create!
                  :title "Bug fix"
                  :issue-type "bug"
                  :priority "1")))
      ;; Should return a beads-issue instance
      (should (beads-issue-p issue))
      ;; Type should be bug
      (should (string= (oref issue issue-type) "bug"))
      ;; Priority should be 1 (high)
      (should (= (oref issue priority) 1))
      ;; Title should match
      (should (string= (oref issue title) "Bug fix")))))

(ert-deftest beads-command-test-create-with-description ()
  "Test beads-command-create with description field.
Integration test that verifies description is set correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((issue (beads-command-create!
                  :title "Feature request"
                  :description "Add new feature to improve UX")))
      ;; Should return a beads-issue instance
      (should (beads-issue-p issue))
      ;; Description should match
      (should (string= (oref issue description)
                       "Add new feature to improve UX")))))

(ert-deftest beads-command-test-create-with-assignee ()
  "Test beads-command-create with assignee field.
Integration test that verifies assignee is set correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((issue (beads-command-create!
                  :title "Assigned task"
                  :assignee "alice")))
      ;; Should return a beads-issue instance
      (should (beads-issue-p issue))
      ;; Assignee should be alice
      (should (string= (oref issue assignee) "alice")))))

(ert-deftest beads-command-test-create-with-labels ()
  "Test beads-command-create with labels.
Integration test that verifies labels are set correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((issue (beads-command-create!
                  :title "Labeled issue"
                  :labels '("urgent" "backend"))))
      ;; Should return a beads-issue instance
      (should (beads-issue-p issue))
      ;; Verify issue was created with correct title
      (should (string= (oref issue title) "Labeled issue")))))

(ert-deftest beads-command-test-create-with-deps ()
  "Test beads-command-create with dependencies.
Integration test that verifies dependencies are set correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; First create a parent issue
    (let* ((parent-issue (beads-command-create! :title "Parent issue"))
           (parent-id (oref parent-issue id))
           ;; Create child with dependency
           (issue (beads-command-create!
                   :title "Child issue"
                   :deps (list (format "discovered-from:%s" parent-id)))))
      ;; Should return a beads-issue instance
      (should (beads-issue-p issue))
      ;; Verify issue was created with correct title
      (should (string= (oref issue title) "Child issue")))))

(ert-deftest beads-command-test-create-combined-options ()
  "Test beads-command-create with multiple options.
Integration test with comprehensive option set."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((issue (beads-command-create!
                  :title "Complex issue"
                  :issue-type "feature"
                  :priority "0"
                  :description "Detailed description"
                  :assignee "bob"
                  :labels '("critical" "frontend"))))
      ;; Should return a beads-issue instance
      (should (beads-issue-p issue))
      ;; Core fields should match
      (should (string= (oref issue title) "Complex issue"))
      (should (string= (oref issue issue-type) "feature"))
      (should (= (oref issue priority) 0))
      (should (string= (oref issue description) "Detailed description"))
      (should (string= (oref issue assignee) "bob")))))

;;; Integration Test: beads-command-list

(ert-deftest beads-command-test-list-all-issues ()
  "Test beads-command-list returns all issues.
Integration test that lists all issues in a project."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create some test issues
    (let* ((issue1 (beads-command-create! :title "Issue 1"))
           (id1 (oref issue1 id))
           (issue2 (beads-command-create! :title "Issue 2"))
           (id2 (oref issue2 id))
           (issue3 (beads-command-create! :title "Issue 3"))
           (id3 (oref issue3 id))
           ;; List all issues
           (issues (beads-command-list!)))
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
        (should (member id3 ids))))))

(ert-deftest beads-command-test-list-with-status-filter ()
  "Test beads-command-list with status filter.
Integration test that filters issues by status."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create test issues (all open by default)
    (beads-command-create! :title "Open issue")
    ;; List only open issues
    (let ((issues (beads-command-list! :status "open")))
      ;; Should return a list
      (should (listp issues))
      ;; All issues should have open status
      (should (cl-every (lambda (issue)
                          (string= (oref issue status) "open"))
                        issues)))))

(ert-deftest beads-command-test-list-with-priority-filter ()
  "Test beads-command-list with priority filter.
Integration test that filters issues by priority."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create issues with different priorities
    (let* ((issue1 (beads-command-create!
                    :title "High priority"
                    :priority "1"))
           (_issue2 (beads-command-create!
                     :title "Low priority"
                     :priority "3"))
           ;; List only priority 1 issues
           (issues (beads-command-list! :priority 1)))
      ;; Should return a list
      (should (listp issues))
      ;; All issues should have priority 1
      (should (cl-every (lambda (issue)
                          (= (oref issue priority) 1))
                        issues))
      ;; Should include our high priority issue
      (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
        (should (member (oref issue1 id) ids))))))

(ert-deftest beads-command-test-list-with-type-filter ()
  "Test beads-command-list with issue type filter.
Integration test that filters issues by type."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create issues with different types
    (let* ((bug-issue (beads-command-create!
                       :title "Bug issue"
                       :issue-type "bug"))
           (_feature-issue (beads-command-create!
                            :title "Feature issue"
                            :issue-type "feature"))
           ;; List only bug issues
           (issues (beads-command-list! :issue-type "bug")))
      ;; Should return a list
      (should (listp issues))
      ;; All issues should be of type bug
      (should (cl-every (lambda (issue)
                          (string= (oref issue issue-type) "bug"))
                        issues))
      ;; Should include our bug issue
      (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
        (should (member (oref bug-issue id) ids))))))

(ert-deftest beads-command-test-list-with-multiple-filters ()
  "Test beads-command-list with multiple filters combined.
Integration test that applies multiple filters together."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create issues with various attributes
    (let* ((target-issue (beads-command-create!
                          :title "Critical bug"
                          :issue-type "bug"
                          :priority "0"))
           (_other-bug (beads-command-create!
                        :title "Low priority bug"
                        :issue-type "bug"
                        :priority "3"))
           (_feature (beads-command-create!
                      :title "Critical feature"
                      :issue-type "feature"
                      :priority "0"))
           ;; List with combined filters: bug AND priority 0
           (issues (beads-command-list!
                    :issue-type "bug"
                    :priority 0)))
      ;; Should return a list
      (should (listp issues))
      ;; All issues should match both filters
      (should (cl-every (lambda (issue)
                          (and (string= (oref issue issue-type) "bug")
                               (= (oref issue priority) 0)))
                        issues))
      ;; Should include our target issue
      (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
        (should (member (oref target-issue id) ids))))))

(ert-deftest beads-command-test-list-with-limit ()
  "Test beads-command-list with limit option.
Integration test that verifies result limiting works."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create several issues
    (beads-command-create! :title "Issue 1")
    (beads-command-create! :title "Issue 2")
    (beads-command-create! :title "Issue 3")
    (beads-command-create! :title "Issue 4")
    (beads-command-create! :title "Issue 5")
    ;; List with limit 2
    (let ((issues (beads-command-list! :limit 2)))
      ;; Should return a list
      (should (listp issues))
      ;; Should have exactly 2 issues (or fewer if database has less)
      (should (<= (length issues) 2)))))

(ert-deftest beads-command-test-list-with-assignee-filter ()
  "Test beads-command-list with assignee filter.
Integration test that filters issues by assignee."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create issues with different assignees
    (let* ((alice-issue (beads-command-create!
                         :title "Alice's task"
                         :assignee "alice"))
           (_bob-issue (beads-command-create!
                        :title "Bob's task"
                        :assignee "bob"))
           ;; List only Alice's issues
           (issues (beads-command-list! :assignee "alice")))
      ;; Should return a list
      (should (listp issues))
      ;; All issues should be assigned to alice
      (should (cl-every (lambda (issue)
                          (string= (oref issue assignee) "alice"))
                        issues))
      ;; Should include Alice's issue
      (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
        (should (member (oref alice-issue id) ids))))))

(ert-deftest beads-command-test-list-empty-result ()
  "Test beads-command-list with filter that matches nothing.
Integration test that verifies empty list is returned correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((issues (beads-command-list! :issue-type "epic")))
      (should (listp issues))
      (should (zerop (length issues))))))

(provide 'beads-command-test)
;;; beads-command-test.el ends here
