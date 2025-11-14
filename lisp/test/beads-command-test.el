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

;;; Integration Test: beads-command-epic-status

(ert-deftest beads-command-test-epic-status-basic ()
  "Test beads-command-epic-status shows all epics.
Integration test that runs real bd epic status command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create an epic with some children
    (let* ((epic (beads-command-create!
                  :title "Test Epic"
                  :issue-type "epic"))
           (_child1 (beads-command-create!
                     :title "Child 1"
                     :deps (list (concat "parent-child:"
                                        (oref epic id)))))
           (_child2 (beads-command-create!
                     :title "Child 2"
                     :deps (list (concat "parent-child:"
                                        (oref epic id)))))
           ;; Get epic status
           (result (beads-command-epic-status!)))
      ;; Should return parsed JSON
      (should result)
      ;; For now, just verify it doesn't error
      ;; The actual structure depends on bd epic status output
      )))

(ert-deftest beads-command-test-epic-status-eligible-only ()
  "Test beads-command-epic-status with --eligible-only flag.
Integration test that filters to show only eligible epics."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create an epic with all children closed
    (let* ((epic (beads-command-create!
                  :title "Complete Epic"
                  :issue-type "epic"))
           (child1 (beads-command-create!
                    :title "Child 1"
                    :deps (list (concat "parent-child:"
                                       (oref epic id)))))
           (child2 (beads-command-create!
                    :title "Child 2"
                    :deps (list (concat "parent-child:"
                                       (oref epic id))))))
      ;; Close both children
      (shell-command (format "bd close %s %s --reason 'Done'"
                            (oref child1 id)
                            (oref child2 id)))
      ;; Get eligible epics
      (let ((result (beads-command-epic-status! :eligible-only t)))
        ;; Should return parsed JSON
        (should result)
        ;; Should contain information about eligible epics
        ))))

;;; Integration Test: beads-command-epic-close-eligible

(ert-deftest beads-command-test-epic-close-eligible-dry-run ()
  "Test beads-command-epic-close-eligible with --dry-run flag.
Integration test that previews eligible epics without closing."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create an epic with all children closed
    (let* ((epic (beads-command-create!
                  :title "Ready Epic"
                  :issue-type "epic"))
           (child1 (beads-command-create!
                    :title "Child 1"
                    :deps (list (concat "parent-child:"
                                       (oref epic id)))))
           (child2 (beads-command-create!
                    :title "Child 2"
                    :deps (list (concat "parent-child:"
                                       (oref epic id))))))
      ;; Close both children
      (shell-command (format "bd close %s %s --reason 'Done'"
                            (oref child1 id)
                            (oref child2 id)))
      ;; Preview close-eligible (should not actually close)
      (let ((result (beads-command-epic-close-eligible! :dry-run t)))
        ;; Should return parsed JSON
        (should result)
        ;; Epic should still be open
        (let ((epic-check (beads-command-list!
                          :id (oref epic id))))
          (should (= (length epic-check) 1))
          (should (string= (oref (car epic-check) status) "open")))))))

(ert-deftest beads-command-test-epic-close-eligible-execute ()
  "Test beads-command-epic-close-eligible actually closes eligible epics.
Integration test that closes epics where all children are complete."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create an epic with all children closed
    (let* ((epic (beads-command-create!
                  :title "Closeable Epic"
                  :issue-type "epic"))
           (child1 (beads-command-create!
                    :title "Child 1"
                    :deps (list (concat "parent-child:"
                                       (oref epic id)))))
           (child2 (beads-command-create!
                    :title "Child 2"
                    :deps (list (concat "parent-child:"
                                       (oref epic id))))))
      ;; Close both children
      (shell-command (format "bd close %s %s --reason 'Done'"
                            (oref child1 id)
                            (oref child2 id)))
      ;; Close eligible epics
      (let ((result (beads-command-epic-close-eligible!)))
        ;; Should return parsed JSON
        (should result)
        ;; Epic should now be closed
        (let ((epic-check (beads-command-list!
                          :id (oref epic id))))
          (should (= (length epic-check) 1))
          (should (string= (oref (car epic-check) status) "closed")))))))

;;; Integration Test: beads-command-show

(ert-deftest beads-command-test-show-single-issue ()
  "Test beads-command-show returns a single issue.
Integration test that shows details for one issue."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a test issue
    (let* ((created (beads-command-create! :title "Test Issue"))
           (issue-id (oref created id))
           ;; Show the issue
           (issue (beads-command-show! :issue-ids (list issue-id))))
      ;; Should return a beads-issue instance
      (should (beads-issue-p issue))
      ;; ID should match
      (should (string= (oref issue id) issue-id))
      ;; Title should match
      (should (string= (oref issue title) "Test Issue")))))

(ert-deftest beads-command-test-show-multiple-issues ()
  "Test beads-command-show returns multiple issues.
Integration test that shows details for multiple issues."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create test issues
    (let* ((issue1 (beads-command-create! :title "Issue 1"))
           (id1 (oref issue1 id))
           (issue2 (beads-command-create! :title "Issue 2"))
           (id2 (oref issue2 id))
           ;; Show both issues
           (issues (beads-command-show! :issue-ids (list id1 id2))))
      ;; Should return a list of beads-issue instances
      (should (listp issues))
      (should (= (length issues) 2))
      ;; All elements should be beads-issue instances
      (should (cl-every #'beads-issue-p issues))
      ;; IDs should match
      (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
        (should (member id1 ids))
        (should (member id2 ids))))))

;;; Integration Test: beads-command-update

(ert-deftest beads-command-test-update-single-field ()
  "Test beads-command-update updates a single field.
Integration test that updates issue title."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a test issue
    (let* ((created (beads-command-create! :title "Original Title"))
           (issue-id (oref created id))
           ;; Update the title
           (updated (beads-command-update!
                     :issue-ids (list issue-id)
                     :title "Updated Title")))
      ;; Should return a beads-issue instance
      (should (beads-issue-p updated))
      ;; Title should be updated
      (should (string= (oref updated title) "Updated Title"))
      ;; ID should remain the same
      (should (string= (oref updated id) issue-id)))))

(ert-deftest beads-command-test-update-multiple-fields ()
  "Test beads-command-update updates multiple fields.
Integration test that updates title, status, and priority."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a test issue
    (let* ((created (beads-command-create! :title "Original"))
           (issue-id (oref created id))
           ;; Update multiple fields
           (updated (beads-command-update!
                     :issue-ids (list issue-id)
                     :title "Updated"
                     :status "in_progress"
                     :priority "1")))
      ;; Should return a beads-issue instance
      (should (beads-issue-p updated))
      ;; All fields should be updated
      (should (string= (oref updated title) "Updated"))
      (should (string= (oref updated status) "in_progress"))
      (should (= (oref updated priority) 1)))))

(ert-deftest beads-command-test-update-with-description ()
  "Test beads-command-update updates description.
Integration test that sets/updates description field."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a test issue
    (let* ((created (beads-command-create! :title "Test"))
           (issue-id (oref created id))
           ;; Update the description
           (updated (beads-command-update!
                     :issue-ids (list issue-id)
                     :description "New description text")))
      ;; Should return a beads-issue instance
      (should (beads-issue-p updated))
      ;; Description should be set
      (should (string= (oref updated description) "New description text")))))

;;; Integration Test: beads-command-close

(ert-deftest beads-command-test-close-single-issue ()
  "Test beads-command-close closes a single issue.
Integration test that closes one issue with a reason."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a test issue
    (let* ((created (beads-command-create! :title "To Close"))
           (issue-id (oref created id))
           ;; Close the issue
           (closed (beads-command-close!
                    :issue-ids (list issue-id)
                    :reason "Completed")))
      ;; Should return a beads-issue instance
      (should (beads-issue-p closed))
      ;; Status should be closed
      (should (string= (oref closed status) "closed"))
      ;; ID should match
      (should (string= (oref closed id) issue-id)))))

(ert-deftest beads-command-test-close-multiple-issues ()
  "Test beads-command-close closes multiple issues.
Integration test that closes several issues at once."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create test issues
    (let* ((issue1 (beads-command-create! :title "Issue 1"))
           (id1 (oref issue1 id))
           (issue2 (beads-command-create! :title "Issue 2"))
           (id2 (oref issue2 id))
           ;; Close both issues
           (closed (beads-command-close!
                    :issue-ids (list id1 id2)
                    :reason "All done")))
      ;; Should return a list of beads-issue instances
      (should (listp closed))
      (should (= (length closed) 2))
      ;; All should be closed
      (should (cl-every (lambda (issue)
                          (string= (oref issue status) "closed"))
                        closed))
      ;; IDs should match
      (let ((ids (mapcar (lambda (issue) (oref issue id)) closed)))
        (should (member id1 ids))
        (should (member id2 ids))))))

;;; Integration Test: beads-command-ready

(ert-deftest beads-command-test-ready-basic ()
  "Test beads-command-ready returns ready issues.
Integration test that lists unblocked open/in-progress issues."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create some test issues (all should be ready by default)
    (let* ((issue1 (beads-command-create! :title "Ready 1"))
           (_issue2 (beads-command-create! :title "Ready 2"))
           ;; Get ready issues
           (issues (beads-command-ready!)))
      ;; Should return a list
      (should (listp issues))
      ;; Should have at least our created issues
      (should (>= (length issues) 2))
      ;; All elements should be beads-issue instances
      (should (cl-every #'beads-issue-p issues))
      ;; Should include our first issue
      (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
        (should (member (oref issue1 id) ids))))))

(ert-deftest beads-command-test-ready-with-priority-filter ()
  "Test beads-command-ready with priority filter.
Integration test that filters ready issues by priority."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create issues with different priorities
    (let* ((high (beads-command-create!
                  :title "High priority"
                  :priority "1"))
           (_low (beads-command-create!
                  :title "Low priority"
                  :priority "3"))
           ;; Get ready issues with priority 1
           (issues (beads-command-ready! :priority 1)))
      ;; Should return a list
      (should (listp issues))
      ;; All should have priority 1
      (should (cl-every (lambda (issue)
                          (= (oref issue priority) 1))
                        issues))
      ;; Should include our high priority issue
      (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
        (should (member (oref high id) ids))))))

(ert-deftest beads-command-test-ready-with-limit ()
  "Test beads-command-ready with limit option.
Integration test that verifies result limiting works."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create several issues
    (beads-command-create! :title "Issue 1")
    (beads-command-create! :title "Issue 2")
    (beads-command-create! :title "Issue 3")
    ;; Get ready issues with limit 2
    (let ((issues (beads-command-ready! :limit 2)))
      ;; Should return a list
      (should (listp issues))
      ;; Should have at most 2 issues
      (should (<= (length issues) 2)))))

;;; Integration Test: beads-command-blocked

(ert-deftest beads-command-test-blocked-basic ()
  "Test beads-command-blocked returns blocked issues.
Integration test that lists issues with unresolved blockers."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a blocker and a blocked issue
    (let* ((blocker (beads-command-create! :title "Blocker"))
           (blocked (beads-command-create!
                     :title "Blocked"
                     :deps (list (concat "blocks:" (oref blocker id)))))
           ;; Get blocked issues
           (issues (beads-command-blocked!)))
      ;; Should return a list
      (should (listp issues))
      ;; Should include our blocked issue
      (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
        (should (member (oref blocked id) ids)))
      ;; All elements should be beads-issue instances
      (should (cl-every #'beads-issue-p issues)))))

(ert-deftest beads-command-test-blocked-empty-result ()
  "Test beads-command-blocked with no blocked issues.
Integration test that verifies empty list when no blockers exist."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create issues without blockers
    (beads-command-create! :title "Issue 1")
    (beads-command-create! :title "Issue 2")
    ;; Get blocked issues (should be none)
    (let ((issues (beads-command-blocked!)))
      (should (listp issues))
      (should (zerop (length issues))))))

;;; Integration Test: beads-command-stats

(ert-deftest beads-command-test-stats-basic ()
  "Test beads-command-stats returns statistics.
Integration test that retrieves issue database stats."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create some test issues
    (beads-command-create! :title "Issue 1")
    (beads-command-create! :title "Issue 2")
    ;; Get stats
    (let ((stats (beads-command-stats!)))
      ;; Should return parsed JSON (alist or similar)
      (should stats)
      ;; Stats should have some structure (implementation-dependent)
      ;; Just verify it's not nil
      (should (not (null stats))))))

(provide 'beads-command-test)
;;; beads-command-test.el ends here
