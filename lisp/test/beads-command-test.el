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
(require 'beads-command-close)
(require 'beads-command-create)
(require 'beads-command-delete)
(require 'beads-command-dep)
(require 'beads-command-epic)
(require 'beads-command-init)
(require 'beads-command-list)
(require 'beads-command-blocked)
(require 'beads-command-ready)
(require 'beads-command-quickstart)
(require 'beads-command-show)
(require 'beads-command-status)
(require 'beads-command-update)
(require 'beads-types)
(require 'beads-test)
(require 'beads-integration-test)

;; Define beads-executable for testing (it's defined in beads.el normally)
(defvar beads-executable "bd"
  "Path to the bd executable for testing.")

;;; Integration Test: beads-command-init

(ert-deftest beads-command-test-init-basic ()
  "Test beads-command-init execution creates .beads directory.
Integration test that runs real bd init command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo ()
    (let* ((prefix (beads-test--generate-unique-prefix))
           (cmd (beads-command-init :prefix prefix)))
      ;; Should execute without error
      (beads-command-execute cmd)
      ;; Should create .beads directory
      (should (file-directory-p (expand-file-name ".beads" default-directory))))))

(ert-deftest beads-command-test-init-with-prefix ()
  "Test beads-command-init with custom prefix option.
Integration test that verifies --prefix flag works correctly."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let ((prefix (beads-test--generate-unique-prefix)))
    (beads-test-with-temp-repo (:init-beads t :prefix prefix)
      ;; .beads directory should exist
      (should (file-directory-p (expand-file-name ".beads" default-directory)))
      ;; Verify prefix is set correctly by creating an issue
      ;; and checking its ID starts with the prefix
      (let* ((issue (beads-execute 'beads-command-create :title "Test issue")))
        (should (beads-issue-p issue))
        (should (string-prefix-p (concat prefix "-") (oref issue id)))))))

(ert-deftest beads-command-test-init-with-quiet ()
  "Test beads-command-init with --quiet flag.
Integration test that verifies quiet mode suppresses output."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-temp-repo ()
    (let* ((prefix (beads-test--generate-unique-prefix))
           (cmd (beads-command-init
                 :prefix prefix
                 :quiet t
                 :skip-hooks t))
           (max-retries 3)
           (attempt 0)
           exec)
      ;; Retry on transient Dolt connection failures (common on CI).
      (while (not exec)
        (setq attempt (1+ attempt))
        (condition-case err
            (setq exec (beads-command-execute cmd))
          (beads-command-error
           (if (>= attempt max-retries)
               (signal (car err) (cdr err))
             (sleep-for 2)))))
      ;; .beads directory should exist
      (should (file-directory-p
               (expand-file-name ".beads" default-directory))))))

;;; Integration Test: beads-command-quickstart

(ert-deftest beads-command-test-quickstart-basic ()
  "Test beads-command-quickstart execution returns quickstart guide.
Integration test that runs real bd quickstart command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((cmd (beads-command-quickstart))
         (result (beads-command-execute cmd)))
    ;; Result should be a string (quickstart guide text)
    (should (stringp result))
    (should (> (length result) 0))
    ;; Should contain common quickstart keywords
    (should (or (string-match-p "quick" (downcase result))
                (string-match-p "start" (downcase result))
                (string-match-p "bd" result)))))

(ert-deftest beads-command-test-quickstart-helper ()
  "Test beads-command-quickstart! helper function.
Integration test that verifies the convenience function works."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let ((result (beads-execute 'beads-command-quickstart)))
    ;; Should return a string (quickstart guide text)
    (should (stringp result))
    (should (> (length result) 0))))

;;; Integration Test: beads-command-create

(ert-deftest beads-command-test-create-basic ()
  "Test beads-command-create creates a basic issue.
Integration test that runs real bd create command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    (let ((issue (beads-execute 'beads-command-create :title "Test issue")))
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
  (beads-test-with-shared-project
    (let ((issue (beads-execute 'beads-command-create
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
  (beads-test-with-shared-project
    (let ((issue (beads-execute 'beads-command-create
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
  (beads-test-with-shared-project
    (let ((issue (beads-execute 'beads-command-create
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
  (beads-test-with-shared-project
    (let ((issue (beads-execute 'beads-command-create
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
  (beads-test-with-shared-project
    ;; First create a parent issue
    (let* ((parent-issue (beads-execute 'beads-command-create :title "Parent issue"))
           (parent-id (oref parent-issue id))
           ;; Create child with dependency
           (issue (beads-execute 'beads-command-create
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
  (beads-test-with-shared-project
    (let ((issue (beads-execute 'beads-command-create
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
  (beads-test-with-shared-project
    ;; Create some test issues
    (let* ((issue1 (beads-execute 'beads-command-create :title "Issue 1"))
           (id1 (oref issue1 id))
           (issue2 (beads-execute 'beads-command-create :title "Issue 2"))
           (id2 (oref issue2 id))
           (issue3 (beads-execute 'beads-command-create :title "Issue 3"))
           (id3 (oref issue3 id))
           ;; List all issues
           (issues (beads-list-execute)))
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
  (beads-test-with-shared-project
    ;; Create test issues (all open by default)
    (beads-execute 'beads-command-create :title "Open issue")
    ;; List only open issues
    (let ((issues (beads-list-execute :status "open")))
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
  (beads-test-with-shared-project
    ;; Create issues with different priorities
    (let* ((issue1 (beads-execute 'beads-command-create
                    :title "High priority"
                    :priority "1"))
           (_issue2 (beads-execute 'beads-command-create
                     :title "Low priority"
                     :priority "3"))
           ;; List only priority 1 issues
           (issues (beads-list-execute :priority "1")))
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
  (beads-test-with-shared-project
    ;; Create issues with different types
    (let* ((bug-issue (beads-execute 'beads-command-create
                       :title "Bug issue"
                       :issue-type "bug"))
           (_feature-issue (beads-execute 'beads-command-create
                            :title "Feature issue"
                            :issue-type "feature"))
           ;; List only bug issues
           (issues (beads-list-execute :issue-type "bug")))
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
  (beads-test-with-shared-project
    ;; Create issues with various attributes
    (let* ((target-issue (beads-execute 'beads-command-create
                          :title "Critical bug"
                          :issue-type "bug"
                          :priority "0"))
           (_other-bug (beads-execute 'beads-command-create
                        :title "Low priority bug"
                        :issue-type "bug"
                        :priority "3"))
           (_feature (beads-execute 'beads-command-create
                      :title "Critical feature"
                      :issue-type "feature"
                      :priority "0"))
           ;; List with combined filters: bug AND priority 0
           (issues (beads-list-execute
                    :issue-type "bug"
                    :priority "0")))
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
  (beads-test-with-shared-project
    ;; Create several issues
    (beads-execute 'beads-command-create :title "Issue 1")
    (beads-execute 'beads-command-create :title "Issue 2")
    (beads-execute 'beads-command-create :title "Issue 3")
    (beads-execute 'beads-command-create :title "Issue 4")
    (beads-execute 'beads-command-create :title "Issue 5")
    ;; List with limit 2
    (let ((issues (beads-list-execute :limit 2)))
      ;; Should return a list
      (should (listp issues))
      ;; Should have exactly 2 issues (or fewer if database has less)
      (should (<= (length issues) 2)))))

(ert-deftest beads-command-test-list-with-assignee-filter ()
  "Test beads-command-list with assignee filter.
Integration test that filters issues by assignee."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    ;; Create issues with different assignees
    (let* ((alice-issue (beads-execute 'beads-command-create
                         :title "Alice's task"
                         :assignee "alice"))
           (_bob-issue (beads-execute 'beads-command-create
                        :title "Bob's task"
                        :assignee "bob"))
           ;; List only Alice's issues
           (issues (beads-list-execute :assignee "alice")))
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
  (beads-test-with-shared-project
    (let ((issues (beads-list-execute :issue-type "epic")))
      (should (listp issues))
      (should (zerop (length issues))))))

;;; Integration Test: beads-command-epic-status

(ert-deftest beads-command-test-epic-status-basic ()
  "Test beads-command-epic-status shows all epics.
Integration test that runs real bd epic status command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    ;; Create an epic with some children
    (let* ((epic (beads-execute 'beads-command-create
                  :title "Test Epic"
                  :issue-type "epic"))
           (_child1 (beads-execute 'beads-command-create
                     :title "Child 1"
                     :deps (list (concat "parent-child:"
                                        (oref epic id)))))
           (_child2 (beads-execute 'beads-command-create
                     :title "Child 2"
                     :deps (list (concat "parent-child:"
                                        (oref epic id)))))
           ;; Get epic status
           (result (beads-execute 'beads-command-epic-status)))
      ;; Should return parsed JSON
      (should result)
      ;; For now, just verify it doesn't error
      ;; The actual structure depends on bd epic status output
      )))

(ert-deftest beads-command-test-epic-status-eligible-only ()
  "Test beads-command-epic-status with --eligible-only flag.
Integration test that filters to show only eligible epics.
Note: bd now auto-closes epics when all children close, so closing
all children leaves no eligible epics (they are already closed)."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    ;; Create an epic with some children, only close one
    (let* ((epic (beads-execute 'beads-command-create
                  :title "Partial Epic"
                  :issue-type "epic"))
           (child1 (beads-execute 'beads-command-create
                    :title "Child 1"
                    :deps (list (concat "parent-child:"
                                       (oref epic id)))))
           (_child2 (beads-execute 'beads-command-create
                     :title "Child 2"
                     :deps (list (concat "parent-child:"
                                        (oref epic id))))))
      ;; Close only one child — epic should NOT be auto-closed
      (shell-command (format "bd close %s --reason 'Done'"
                            (oref child1 id)))
      ;; Get epic status (with partial children still open)
      (let ((result (beads-execute 'beads-command-epic-status)))
        ;; Should return parsed JSON
        (should result)))))

;;; Integration Test: beads-command-epic-close-eligible

(ert-deftest beads-command-test-epic-close-eligible-dry-run ()
  "Test beads-command-epic-close-eligible with --dry-run flag.
Integration test that verifies auto-close behavior: when all children
of an epic are closed, bd auto-closes the epic."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    ;; Create an epic with all children closed
    (let* ((epic (beads-execute 'beads-command-create
                  :title "Ready Epic"
                  :issue-type "epic"))
           (child1 (beads-execute 'beads-command-create
                    :title "Child 1"
                    :deps (list (concat "parent-child:"
                                       (oref epic id)))))
           (child2 (beads-execute 'beads-command-create
                    :title "Child 2"
                    :deps (list (concat "parent-child:"
                                       (oref epic id))))))
      ;; Close both children — bd auto-closes the epic
      (shell-command (format "bd close %s %s --reason 'Done'"
                            (oref child1 id)
                            (oref child2 id)))
      ;; Epic should be auto-closed now
      (let ((epic-check (beads-list-execute
                        :id (oref epic id)
                        :status "all")))
        (should (= (length epic-check) 1))
        (should (string= (oref (car epic-check) status) "closed"))))))

(ert-deftest beads-command-test-epic-close-eligible-execute ()
  "Test beads-command-epic-close-eligible actually closes eligible epics.
Integration test that closes epics where all children are complete."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    ;; Create an epic with all children closed
    (let* ((epic (beads-execute 'beads-command-create
                  :title "Closeable Epic"
                  :issue-type "epic"))
           (child1 (beads-execute 'beads-command-create
                    :title "Child 1"
                    :deps (list (concat "parent-child:"
                                       (oref epic id)))))
           (child2 (beads-execute 'beads-command-create
                    :title "Child 2"
                    :deps (list (concat "parent-child:"
                                       (oref epic id))))))
      ;; Close both children
      (shell-command (format "bd close %s %s --reason 'Done'"
                            (oref child1 id)
                            (oref child2 id)))
      ;; Close eligible epics
      (let ((result (beads-execute 'beads-command-epic-close-eligible)))
        ;; Should return parsed JSON
        (should result)
        ;; Epic should now be closed
        ;; Use beads-command-show! since closed issues don't appear in list
        (let ((epic-check (beads-execute 'beads-command-show
                           :issue-ids (list (oref epic id)))))
          (should (beads-issue-p epic-check))
          (should (string= (oref epic-check status) "closed")))))))

;;; Integration Test: beads-command-show

(ert-deftest beads-command-test-show-single-issue ()
  "Test beads-command-show returns a single issue.
Integration test that shows details for one issue."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    ;; Create a test issue
    (let* ((created (beads-execute 'beads-command-create :title "Test Issue"))
           (issue-id (oref created id))
           ;; Show the issue
           (issue (beads-execute 'beads-command-show :issue-ids (list issue-id))))
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
  (beads-test-with-shared-project
    ;; Create test issues
    (let* ((issue1 (beads-execute 'beads-command-create :title "Issue 1"))
           (id1 (oref issue1 id))
           (issue2 (beads-execute 'beads-command-create :title "Issue 2"))
           (id2 (oref issue2 id))
           ;; Show both issues
           (issues (beads-execute 'beads-command-show :issue-ids (list id1 id2))))
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
  (beads-test-with-shared-project
    ;; Create a test issue
    (let* ((created (beads-execute 'beads-command-create :title "Original Title"))
           (issue-id (oref created id))
           ;; Update the title
           (updated (car (beads-execute 'beads-command-update
                          :issue-ids (list issue-id)
                          :title "Updated Title"))))
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
  (beads-test-with-shared-project
    ;; Create a test issue
    (let* ((created (beads-execute 'beads-command-create :title "Original"))
           (issue-id (oref created id))
           ;; Update multiple fields
           (updated (car (beads-execute 'beads-command-update
                          :issue-ids (list issue-id)
                          :title "Updated"
                          :status "in_progress"
                          :priority "1"))))
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
  (beads-test-with-shared-project
    ;; Create a test issue
    (let* ((created (beads-execute 'beads-command-create :title "Test"))
           (issue-id (oref created id))
           ;; Update the description
           (updated (car (beads-execute 'beads-command-update
                          :issue-ids (list issue-id)
                          :description "New description text"))))
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
  (beads-test-with-shared-project
    ;; Create a test issue
    (let* ((created (beads-execute 'beads-command-create :title "To Close"))
           (issue-id (oref created id))
           ;; Close the issue
           (closed (car (beads-execute 'beads-command-close
                         :issue-ids (list issue-id)
                         :reason "Completed"))))
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
  (beads-test-with-shared-project
    ;; Create test issues
    (let* ((issue1 (beads-execute 'beads-command-create :title "Issue 1"))
           (id1 (oref issue1 id))
           (issue2 (beads-execute 'beads-command-create :title "Issue 2"))
           (id2 (oref issue2 id))
           ;; Close both issues
           (closed (beads-execute 'beads-command-close
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
  (beads-test-with-shared-project
    ;; Create some test issues (all should be ready by default)
    (let* ((issue1 (beads-execute 'beads-command-create :title "Ready 1"))
           (_issue2 (beads-execute 'beads-command-create :title "Ready 2"))
           ;; Get ready issues
           (issues (beads-execute 'beads-command-ready)))
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
  (beads-test-with-shared-project
    ;; Create issues with different priorities
    (let* ((high (beads-execute 'beads-command-create
                  :title "High priority"
                  :priority "1"))
           (_low (beads-execute 'beads-command-create
                  :title "Low priority"
                  :priority "3"))
           ;; Get ready issues with priority 1
           (issues (beads-execute 'beads-command-ready :priority 1)))
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
  (beads-test-with-shared-project
    ;; Create several issues
    (beads-execute 'beads-command-create :title "Issue 1")
    (beads-execute 'beads-command-create :title "Issue 2")
    (beads-execute 'beads-command-create :title "Issue 3")
    ;; Get ready issues with limit 2
    (let ((issues (beads-execute 'beads-command-ready :limit 2)))
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
  (beads-test-with-shared-project
    ;; Create two issues: blocker blocks blocked-issue
    ;; deps "blocks:X" on issue A means "A blocks X", so X is blocked
    (let* ((blocked-issue (beads-execute 'beads-command-create :title "Blocked"))
           (_blocker (beads-execute 'beads-command-create
                      :title "Blocker"
                      :deps (list (concat "blocks:" (oref blocked-issue id)))))
           ;; Get blocked issues
           (issues (beads-execute 'beads-command-blocked)))
      ;; Should return a list
      (should (listp issues))
      ;; Should include our blocked issue (not the blocker)
      (let ((ids (mapcar (lambda (issue) (oref issue id)) issues)))
        (should (member (oref blocked-issue id) ids)))
      ;; All elements should be beads-blocked-issue instances
      (should (cl-every #'beads-blocked-issue-p issues)))))

(ert-deftest beads-command-test-blocked-empty-result ()
  "Test beads-command-blocked with no blocked issues.
Integration test that verifies empty list when no blockers exist."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    ;; Create issues without blockers
    (beads-execute 'beads-command-create :title "Issue 1")
    (beads-execute 'beads-command-create :title "Issue 2")
    ;; Get blocked issues (should be none)
    (let ((issues (beads-execute 'beads-command-blocked)))
      (should (listp issues))
      (should (zerop (length issues))))))

;;; Integration Test: beads-command-status

(ert-deftest beads-command-test-stats-basic ()
  "Test beads-command-status returns statistics.
Integration test that retrieves issue database stats."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    ;; Create some test issues
    (beads-execute 'beads-command-create :title "Issue 1")
    (beads-execute 'beads-command-create :title "Issue 2")
    ;; Get stats
    (let ((stats (beads-execute 'beads-command-status)))
      ;; Should return parsed JSON (alist or similar)
      (should stats)
      ;; Stats should have some structure (implementation-dependent)
      ;; Just verify it's not nil
      (should (not (null stats))))))

;;; Unit Tests: beads-command-line

;; Unit tests for command-line building that don't require bd CLI.
;; These tests verify argument construction without executing commands.

(ert-deftest beads-command-test-unit-list-command-line-basic ()
  "Unit test: beads-command-list builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-list
               :json t
               :status "open"
               :priority "1"
               :sandbox t))
         (args (beads-command-line cmd)))
    (should (listp args))
    (should (member "list" args))
    (should (member "--status" args))
    (should (member "open" args))
    (should (member "--priority" args))
    (should (member "1" args))
    (should (member "--sandbox" args))
    (should (member "--json" args))
    ;; --flat must NOT be present; bd 0.58.0 --json works without it
    (should-not (member "--flat" args))))

(ert-deftest beads-command-test-unit-list-command-line-no-flat-with-json ()
  "Unit test: --flat is NOT added to bd list when json=t (bd 0.58.0)."
  :tags '(:unit)
  (let* ((cmd (beads-command-list :json t))
         (args (beads-command-line cmd)))
    (should (member "--json" args))
    (should-not (member "--flat" args))))

(ert-deftest beads-command-test-unit-list-command-line-no-flat-without-json ()
  "Unit test: --flat is NOT added to bd list when json=nil."
  :tags '(:unit)
  (let* ((cmd (beads-command-list :json nil))
         (args (beads-command-line cmd)))
    (should-not (member "--flat" args))))

(ert-deftest beads-command-test-unit-list-command-line-all-filters ()
  "Unit test: beads-command-list with all filter options."
  :tags '(:unit)
  (let* ((cmd (beads-command-list
               :status "in_progress"
               :priority "2"
               :issue-type "bug"
               :assignee "alice"
               :limit 10))
         (args (beads-command-line cmd)))
    (should (member "list" args))
    (should (member "--status" args))
    (should (member "in_progress" args))
    (should (member "--priority" args))
    (should (member "2" args))
    (should (member "--type" args))
    (should (member "bug" args))
    (should (member "--assignee" args))
    (should (member "alice" args))
    (should (member "--limit" args))
    (should (member "10" args))))

(ert-deftest beads-command-test-unit-create-command-line-basic ()
  "Unit test: beads-command-create builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-create
               :json t
               :title "Test issue"
               :priority "1"
               :issue-type "bug"))
         (args (beads-command-line cmd)))
    (should (member "create" args))
    (should (member "Test issue" args))
    (should (member "--priority" args))
    (should (member "1" args))
    (should (member "--type" args))
    (should (member "bug" args))
    (should (member "--json" args))))

(ert-deftest beads-command-test-unit-create-command-line-with-description ()
  "Unit test: beads-command-create with multiline description."
  :tags '(:unit)
  (let* ((desc "Line 1\nLine 2\nLine 3")
         (cmd (beads-command-create
               :title "Test"
               :description desc))
         (args (beads-command-line cmd)))
    (should (member "create" args))
    (should (member "--description" args))
    (should (member desc args))))

(ert-deftest beads-command-test-unit-create-command-line-with-deps ()
  "Unit test: beads-command-create with dependencies."
  :tags '(:unit)
  (let* ((cmd (beads-command-create
               :title "Test"
               :deps '("blocks:bd-123" "depends-on:bd-456")))
         (args (beads-command-line cmd)))
    (should (member "--deps" args))
    (should (member "blocks:bd-123,depends-on:bd-456" args))))

(ert-deftest beads-command-test-unit-update-command-line-basic ()
  "Unit test: beads-command-update builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-update
               :issue-ids '("bd-123" "bd-456")
               :status "in_progress"
               :priority "2"))
         (args (beads-command-line cmd)))
    (should (member "update" args))
    (should (member "bd-123" args))
    (should (member "bd-456" args))
    (should (member "--status" args))
    (should (member "in_progress" args))
    (should (member "--priority" args))
    (should (member "2" args))))

(ert-deftest beads-command-test-unit-close-command-line-basic ()
  "Unit test: beads-command-close builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-close
               :json t
               :issue-ids '("bd-789")
               :reason "Completed"))
         (args (beads-command-line cmd)))
    (should (member "close" args))
    (should (member "bd-789" args))
    (should (member "--reason" args))
    (should (member "Completed" args))
    (should (member "--json" args))))

(ert-deftest beads-command-test-unit-init-command-line-all-options ()
  "Unit test: beads-command-init with all options."
  :tags '(:unit)
  (let* ((cmd (beads-command-init
               :branch "develop"
               :prefix "myproj"
               :quiet t
               :contributor t
               :skip-hooks t))
         (args (beads-command-line cmd)))
    (should (member "init" args))
    (should (member "--branch" args))
    (should (member "develop" args))
    (should (member "--prefix" args))
    (should (member "myproj" args))
    (should (member "--quiet" args))
    (should (member "--contributor" args))
    (should (member "--skip-hooks" args))))

;;; Unit Tests: beads-command-validate

;; Unit tests for validation logic that don't require bd CLI.

(ert-deftest beads-command-test-unit-create-validate-title-required ()
  "Unit test: beads-command-create validation requires title."
  :tags '(:unit)
  (let ((cmd (beads-command-create :title nil)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "title" (downcase (beads-command-validate cmd))))))

(ert-deftest beads-command-test-unit-create-validate-title-empty ()
  "Unit test: beads-command-create rejects empty title."
  :tags '(:unit)
  (let ((cmd (beads-command-create :title "")))
    ;; Empty string should fail validation
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "empty" (downcase (beads-command-validate cmd))))))

(ert-deftest beads-command-test-unit-create-validate-title-whitespace ()
  "Unit test: beads-command-create rejects whitespace-only title."
  :tags '(:unit)
  (let ((cmd (beads-command-create :title "   ")))
    ;; Whitespace-only string should fail validation
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "empty" (downcase (beads-command-validate cmd))))))

(ert-deftest beads-command-test-unit-create-validate-priority-range ()
  "Unit test: beads-command-create validates priority range."
  :tags '(:unit)
  ;; Valid priorities (0-4)
  (dolist (p '(0 1 2 3 4))
    (let ((cmd (beads-command-create
                :title "Test"
                :priority p)))
      (should-not (beads-command-validate cmd))))
  ;; Invalid priorities - should fail validation
  (dolist (p '(-1 5 10))
    (let ((cmd (beads-command-create
                :title "Test"
                :priority p)))
      (should (stringp (beads-command-validate cmd)))
      (should (string-match-p "priority" (downcase (beads-command-validate cmd)))))))

(ert-deftest beads-command-test-unit-create-validate-type-valid ()
  "Unit test: beads-command-create validates type values."
  :tags '(:unit)
  ;; Valid types
  (dolist (type '("bug" "feature" "task" "epic" "chore"))
    (let ((cmd (beads-command-create
                :title "Test"
                :issue-type type)))
      (should-not (beads-command-validate cmd))))
  ;; Invalid type - should fail validation
  (let ((cmd (beads-command-create
              :title "Test"
              :issue-type "invalid-type")))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "type" (downcase (beads-command-validate cmd))))))

(ert-deftest beads-command-test-unit-create-validate-deps-format ()
  "Unit test: beads-command-create validates dependency format."
  :tags '(:unit)
  ;; Valid dependency formats (as lists)
  (dolist (deps '(("blocks:bd-123")
                  ("depends-on:bd-456")
                  ("blocks:bd-1" "depends-on:bd-2")
                  ("discovered-from:bd-789")
                  ;; Plain issue IDs without type prefix are also valid
                  ("bd-123")
                  ("invalid")
                  ("blocks-bd-123")))
    (let ((cmd (beads-command-create
                :title "Test"
                :deps deps)))
      (should-not (beads-command-validate cmd))))
  ;; Invalid formats - should fail validation
  (dolist (deps '(("@invalid")
                  ("blocks:")
                  (":bd-123")))
    (let ((cmd (beads-command-create
                :title "Test"
                :deps deps)))
      (should (stringp (beads-command-validate cmd)))
      (should (string-match-p "depend" (downcase (beads-command-validate cmd)))))))

(ert-deftest beads-command-test-unit-close-validate-issue-id-required ()
  "Unit test: beads-command-close validation requires issue ID."
  :tags '(:unit)
  (let ((cmd (beads-command-close :issue-ids nil)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-unit-close-validate-issue-id-empty ()
  "Unit test: beads-command-close validation rejects empty issue ID."
  :tags '(:unit)
  (let ((cmd (beads-command-close :issue-ids '(""))))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-unit-init-validate-option-conflicts ()
  "Unit test: beads-command-init validates option conflicts."
  :tags '(:unit)
  ;; Can't use both --contributor and --team
  (let ((cmd (beads-command-init
              :contributor t
              :team t)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "contributor\\|team"
                            (downcase (beads-command-validate cmd))))))

;;; Unit Tests: Edge Cases

;; Unit tests for edge cases: unicode, special characters, nil values.

(ert-deftest beads-command-test-unit-create-unicode-title ()
  "Unit test: beads-command-create handles unicode in title."
  :tags '(:unit)
  (let* ((cmd (beads-command-create
               :title "测试 Issue 日本語 🚀"))
         (args (beads-command-line cmd)))
    (should (member "测试 Issue 日本語 🚀" args))))

(ert-deftest beads-command-test-unit-create-special-chars-title ()
  "Unit test: beads-command-create handles special characters."
  :tags '(:unit)
  (let* ((cmd (beads-command-create
               :title "Issue with \"quotes\" and 'apostrophes'"))
         (args (beads-command-line cmd)))
    (should (member "Issue with \"quotes\" and 'apostrophes'" args))))

(ert-deftest beads-command-test-unit-create-very-long-title ()
  "Unit test: beads-command-create handles very long title."
  :tags '(:unit)
  (let* ((long-title (make-string 500 ?x))
         (cmd (beads-command-create :title long-title))
         (args (beads-command-line cmd)))
    (should (member long-title args))))

(ert-deftest beads-command-test-unit-create-nil-optional-fields ()
  "Unit test: beads-command-create handles nil optional fields."
  :tags '(:unit)
  (let* ((cmd (beads-command-create
               :title "Test"
               :description nil
               :assignee nil
               :labels nil
               :deps nil))
         (args (beads-command-line cmd)))
    ;; Should not include nil optional fields in args
    (should-not (member nil args))
    ;; But should still include required fields
    (should (member "create" args))
    (should (member "Test" args))))

(ert-deftest beads-command-test-unit-list-nil-filters ()
  "Unit test: beads-command-list handles nil filters."
  :tags '(:unit)
  (let* ((cmd (beads-command-list
               :status nil
               :priority nil
               :issue-type nil))
         (args (beads-command-line cmd)))
    ;; Should not include nil filters
    (should-not (member nil args))
    ;; Should still include base command
    (should (member "list" args))))

(ert-deftest beads-command-test-unit-create-newlines-in-description ()
  "Unit test: beads-command-create handles newlines in description."
  :tags '(:unit)
  (let* ((desc "Line 1\n\nLine 3 with  spaces\n\tTabbed line")
         (cmd (beads-command-create
               :title "Test"
               :description desc))
         (args (beads-command-line cmd)))
    (should (member desc args))))

;;; Unit Tests: Class Instantiation

;; Unit tests for class instantiation and defaults.

(ert-deftest beads-command-test-unit-create-instantiation-defaults ()
  "Unit test: beads-command-create default values."
  :tags '(:unit)
  (let ((cmd (beads-command-create :title "Test")))
    (should (beads-command-create-p cmd))
    (should (string= (oref cmd title) "Test"))
    (should-not (oref cmd json))
    (should-not (oref cmd priority))
    (should-not (oref cmd issue-type))
    (should-not (oref cmd description))))

(ert-deftest beads-command-test-unit-list-instantiation-defaults ()
  "Unit test: beads-command-list default values."
  :tags '(:unit)
  (let ((cmd (beads-command-list)))
    (should (beads-command-list-p cmd))
    (should-not (oref cmd json))
    (should-not (oref cmd status))
    (should-not (oref cmd priority))
    ;; Class initform is nil; default limit applied by beads-command-list!
    (should-not (oref cmd limit))))

(ert-deftest beads-command-test-unit-init-instantiation-defaults ()
  "Unit test: beads-command-init default values."
  :tags '(:unit)
  (let ((cmd (beads-command-init)))
    (should (beads-command-init-p cmd))
    (should-not (oref cmd branch))
    (should-not (oref cmd prefix))
    (should-not (oref cmd quiet))
    (should-not (oref cmd contributor))
    (should-not (oref cmd team))))

(ert-deftest beads-command-test-unit-command-inheritance ()
  "Unit test: verify command class inheritance chain."
  :tags '(:unit)
  (let ((cmd (beads-command-create :title "Test")))
    ;; Should be instance of itself
    (should (beads-command-create-p cmd))
    ;; Should be instance of parent classes (using cl-typep since EIEIO
    ;; predicates don't work for parent classes)
    (should (cl-typep cmd 'beads-command))
    (should (cl-typep cmd 'beads-command))
    ;; Should be an EIEIO object
    (should (eieio-object-p cmd))))

(ert-deftest beads-command-test-unit-update-instantiation-slots ()
  "Unit test: beads-command-update slot values."
  :tags '(:unit)
  (let ((cmd (beads-command-update
              :issue-ids '("bd-1" "bd-2")
              :title "New Title"
              :status "closed"
              :priority "3")))
    (should (equal (oref cmd issue-ids) '("bd-1" "bd-2")))
    (should (string= (oref cmd title) "New Title"))
    (should (string= (oref cmd status) "closed"))
    (should (string= (oref cmd priority) "3"))))

;; Additional validation tests

(ert-deftest beads-command-test-list-validate-priority-conflicts ()
  "List validation detects priority conflicts."
  :tags '(:unit)
  (let ((cmd (beads-command-list :priority "1" :priority-min "0")))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "priority" (beads-command-validate cmd)))))

(ert-deftest beads-command-test-list-validate-assignee-conflicts ()
  "List validation detects assignee conflicts."
  :tags '(:unit)
  (let ((cmd (beads-command-list :assignee "user" :no-assignee t)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "assignee" (beads-command-validate cmd)))))

(ert-deftest beads-command-test-list-validate-label-conflicts ()
  "List validation detects label conflicts."
  :tags '(:unit)
  (let ((cmd (beads-command-list :label '("bug") :no-labels t)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "label" (beads-command-validate cmd)))))

(ert-deftest beads-command-test-list-validate-priority-range ()
  "List validation checks priority range."
  :tags '(:unit)
  (let ((cmd1 (beads-command-list :priority "5"))
        (cmd2 (beads-command-list :priority-min "-1"))
        (cmd3 (beads-command-list :priority-max "10")))
    (should (stringp (beads-command-validate cmd1)))
    (should (stringp (beads-command-validate cmd2)))
    (should (stringp (beads-command-validate cmd3)))))

(ert-deftest beads-command-test-list-validate-valid ()
  "List validation accepts valid configuration."
  :tags '(:unit)
  (let ((cmd (beads-command-list :priority "1" :status "open")))
    (should-not (beads-command-validate cmd))))

(ert-deftest beads-command-test-update-validate-issue-ids ()
  "Update validation requires issue IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-update)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "issue" (beads-command-validate cmd)))))

(ert-deftest beads-command-test-ready-validate-priority-range ()
  "Ready validation checks priority range."
  :tags '(:unit)
  (let ((cmd (beads-command-ready :priority 5)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "Priority" (beads-command-validate cmd)))))

(ert-deftest beads-command-test-show-validate-issue-ids ()
  "Show validation requires issue IDs."
  :tags '(:unit)
  (let* ((cmd (beads-command-show))
         (errors (beads-command-validate cmd)))
    (should errors)
    (should (cl-some (lambda (e) (string-match-p "issue" e)) errors))))

;; Additional argument building tests

(ert-deftest beads-command-test-list-command-line-with-filters ()
  "List command-line includes all filters."
  :tags '(:unit)
  (let* ((cmd (beads-command-list
               :json t
               :status "open"
               :priority "1"
               :assignee "user"
               :label '("bug" "feature")))
         (args (beads-command-line cmd)))
    (should (member "list" args))
    (should (member "--status" args))
    (should (member "open" args))
    (should (member "--priority" args))
    (should (member "1" args))
    (should (member "--assignee" args))
    (should (member "user" args))
    (should (member "--label" args))
    (should (member "bug" args))
    (should (member "feature" args))
    (should (member "--json" args))))

(ert-deftest beads-command-test-list-command-line-with-boolean-flags ()
  "List command-line includes boolean flags."
  :tags '(:unit)
  (let* ((cmd (beads-command-list
               :no-assignee t
               :no-labels t))
         (args (beads-command-line cmd)))
    (should (member "--no-assignee" args))
    (should (member "--no-labels" args))))

(ert-deftest beads-command-test-update-command-line-multiple-issues ()
  "Update command-line handles multiple issue IDs."
  :tags '(:unit)
  (let* ((cmd (beads-command-update
               :issue-ids '("bd-1" "bd-2" "bd-3")
               :status "closed"))
         (args (beads-command-line cmd)))
    (should (member "update" args))
    (should (member "bd-1" args))
    (should (member "bd-2" args))
    (should (member "bd-3" args))
    (should (member "--status" args))
    (should (member "closed" args))))

(ert-deftest beads-command-test-ready-command-line-with-limit ()
  "Ready command-line includes limit."
  :tags '(:unit)
  (let* ((cmd (beads-command-ready :limit 10))
         (args (beads-command-line cmd)))
    (should (member "ready" args))
    (should (member "--limit" args))
    (should (member "10" args))))

(ert-deftest beads-command-test-create-command-line-string-priority ()
  "Create command-line handles string priority."
  :tags '(:unit)
  (let* ((cmd (beads-command-create
               :title "Test"
               :priority "2"))
         (args (beads-command-line cmd)))
    (should (member "--priority" args))
    (should (member "2" args))))

;;; Integration Tests: beads-command-dep-add

(ert-deftest beads-command-test-dep-add-basic ()
  "Test beads-command-dep-add adds a dependency.
Integration test that adds a blocks dependency between two issues."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    ;; Create two test issues
    (let* ((issue1 (beads-execute 'beads-command-create :title "Issue 1"))
           (issue2 (beads-execute 'beads-command-create :title "Issue 2"))
           (id1 (oref issue1 id))
           (id2 (oref issue2 id)))
      ;; Add dependency: issue2 blocks issue1
      (let ((result (beads-execute 'beads-command-dep-add
                     :issue-id id1
                     :depends-on id2)))
        (should result)))))

(ert-deftest beads-command-test-dep-add-with-type ()
  "Test beads-command-dep-add with custom dependency type.
Integration test that adds a related dependency."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    (let* ((issue1 (beads-execute 'beads-command-create :title "Issue 1"))
           (issue2 (beads-execute 'beads-command-create :title "Issue 2"))
           (id1 (oref issue1 id))
           (id2 (oref issue2 id)))
      (let ((result (beads-execute 'beads-command-dep-add
                     :issue-id id1
                     :depends-on id2
                     :dep-type "related")))
        (should result)))))

;;; Integration Tests: beads-command-dep-remove

(ert-deftest beads-command-test-dep-remove-basic ()
  "Test beads-command-dep-remove removes a dependency.
Integration test that removes a dependency between two issues."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    (let* ((issue1 (beads-execute 'beads-command-create :title "Issue 1"))
           (issue2 (beads-execute 'beads-command-create :title "Issue 2"))
           (id1 (oref issue1 id))
           (id2 (oref issue2 id)))
      ;; Add dependency first
      (beads-execute 'beads-command-dep-add :issue-id id1 :depends-on id2)
      ;; Then remove it
      (let ((result (beads-execute 'beads-command-dep-remove
                     :issue-id id1
                     :depends-on id2)))
        (should result)))))

;;; Integration Tests: beads-command-dep-tree

(ert-deftest beads-command-test-dep-tree-basic ()
  "Test beads-command-dep-tree shows dependency tree.
Integration test that retrieves the dependency tree for an issue."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    (let* ((issue1 (beads-execute 'beads-command-create :title "Issue 1"))
           (id1 (oref issue1 id)))
      (let ((tree (beads-execute 'beads-command-dep-tree :issue-id id1)))
        (should tree)))))

(ert-deftest beads-command-test-dep-tree-with-max-depth ()
  "Test beads-command-dep-tree with max-depth option.
Integration test that retrieves dependency tree with depth limit."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    (let* ((issue1 (beads-execute 'beads-command-create :title "Issue 1"))
           (id1 (oref issue1 id)))
      (let ((tree (beads-execute 'beads-command-dep-tree
                   :issue-id id1
                   :max-depth 5)))
        (should tree)))))

(ert-deftest beads-command-test-dep-tree-reverse ()
  "Test beads-command-dep-tree with direction up.
Integration test that shows what depends on this issue."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    (let* ((issue1 (beads-execute 'beads-command-create :title "Issue 1"))
           (id1 (oref issue1 id)))
      (let ((tree (beads-execute 'beads-command-dep-tree
                   :issue-id id1
                   :direction "up")))
        (should tree)))))

;;; Integration Tests: beads-command-dep-cycles

(ert-deftest beads-command-test-dep-cycles-basic ()
  "Test beads-command-dep-cycles detects cycles.
Integration test that checks for dependency cycles."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-shared-project
    (let ((result (beads-execute 'beads-command-dep-cycles)))
      (should (not (null result))))))

;;; Unit Tests: beads-command-dep-* command-line building

(ert-deftest beads-command-test-dep-add-command-line-basic ()
  "Unit test: beads-command-dep-add builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-add
               :issue-id "bd-1"
               :depends-on "bd-2"))
         (args (beads-command-line cmd)))
    (should (member "dep" args))
    (should (member "add" args))
    (should (member "bd-1" args))
    (should (member "bd-2" args))))

(ert-deftest beads-command-test-dep-add-command-line-with-type ()
  "Unit test: beads-command-dep-add includes dep-type."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-add
               :issue-id "bd-1"
               :depends-on "bd-2"
               :dep-type "related"))
         (args (beads-command-line cmd)))
    (should (member "--type" args))
    (should (member "related" args))))

(ert-deftest beads-command-test-dep-remove-command-line-basic ()
  "Unit test: beads-command-dep-remove builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-remove
               :issue-id "bd-1"
               :depends-on "bd-2"))
         (args (beads-command-line cmd)))
    (should (member "dep" args))
    (should (member "remove" args))
    (should (member "bd-1" args))
    (should (member "bd-2" args))))

(ert-deftest beads-command-test-dep-tree-command-line-basic ()
  "Unit test: beads-command-dep-tree builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-tree :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "dep" args))
    (should (member "tree" args))
    (should (member "bd-1" args))))

(ert-deftest beads-command-test-dep-tree-command-line-with-format ()
  "Unit test: beads-command-dep-tree includes format option."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-tree
               :issue-id "bd-1"
               :format "mermaid"))
         (args (beads-command-line cmd)))
    (should (member "--format" args))
    (should (member "mermaid" args))))

(ert-deftest beads-command-test-dep-tree-command-line-with-max-depth ()
  "Unit test: beads-command-dep-tree includes max-depth option."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-tree
               :issue-id "bd-1"
               :max-depth 10))
         (args (beads-command-line cmd)))
    (should (member "--max-depth" args))
    (should (member "10" args))))

(ert-deftest beads-command-test-dep-tree-command-line-with-reverse ()
  "Unit test: beads-command-dep-tree includes direction flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-tree
               :issue-id "bd-1"
               :direction "up"))
         (args (beads-command-line cmd)))
    (should (member "--direction" args))
    (should (member "up" args))))

(ert-deftest beads-command-test-dep-tree-command-line-with-show-all-paths ()
  "Unit test: beads-command-dep-tree includes show-all-paths flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-tree
               :issue-id "bd-1"
               :show-all-paths t))
         (args (beads-command-line cmd)))
    (should (member "--show-all-paths" args))))

(ert-deftest beads-command-test-dep-cycles-command-line-basic ()
  "Unit test: beads-command-dep-cycles builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-cycles))
         (args (beads-command-line cmd)))
    (should (member "dep" args))
    (should (member "cycles" args))))

;;; Unit Tests: beads-command-dep-* validation

(ert-deftest beads-command-test-dep-add-validation-missing-issue-id ()
  "Unit test: dep-add validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-add
              :depends-on "bd-2")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-add-validation-missing-depends-on ()
  "Unit test: dep-add validation fails without depends-on-id."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-add
              :issue-id "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-add-validation-success ()
  "Unit test: dep-add validation succeeds with required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-add
              :issue-id "bd-1"
              :depends-on "bd-2")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-test-dep-remove-validation-missing-issue-id ()
  "Unit test: dep-remove validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-remove
              :depends-on "bd-2")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-remove-validation-success ()
  "Unit test: dep-remove validation succeeds with required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-remove
              :issue-id "bd-1"
              :depends-on "bd-2")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-test-dep-tree-validation-missing-issue-id ()
  "Unit test: dep-tree validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-tree)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-tree-validation-success ()
  "Unit test: dep-tree validation succeeds with issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-tree :issue-id "bd-1")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-test-dep-cycles-validation-success ()
  "Unit test: dep-cycles validation always succeeds (no required fields)."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-cycles)))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-dep-list

(ert-deftest beads-command-test-dep-list-command-line-basic ()
  "Unit test: dep-list builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-list :issue-id "bd-1"))
         (args (beads-command-line cmd)))
    (should (member "dep" args))
    (should (member "list" args))
    (should (member "bd-1" args))))

(ert-deftest beads-command-test-dep-list-command-line-with-direction ()
  "Unit test: dep-list includes direction option."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-list :issue-id "bd-1" :direction "up"))
         (args (beads-command-line cmd)))
    (should (member "--direction" args))
    (should (member "up" args))))

(ert-deftest beads-command-test-dep-list-command-line-with-type ()
  "Unit test: dep-list includes type option."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-list :issue-id "bd-1" :dep-type "tracks"))
         (args (beads-command-line cmd)))
    (should (member "--type" args))
    (should (member "tracks" args))))

(ert-deftest beads-command-test-dep-list-validation-missing-issue-id ()
  "Unit test: dep-list validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-list)))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-list-validation-success ()
  "Unit test: dep-list validation succeeds with issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-list :issue-id "bd-1")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-test-dep-list-validation-invalid-direction ()
  "Unit test: dep-list validation allows any direction string.
The CLI will validate the direction value - not Emacs.
This is intentional - keeping validation simple."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-list :issue-id "bd-1" :direction "invalid")))
    ;; Validation only checks issue-id is present, not direction values
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-dep-relate

(ert-deftest beads-command-test-dep-relate-command-line-basic ()
  "Unit test: dep-relate builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-relate :id1 "bd-1" :id2 "bd-2"))
         (args (beads-command-line cmd)))
    (should (member "dep" args))
    (should (member "relate" args))
    (should (member "bd-1" args))
    (should (member "bd-2" args))))

(ert-deftest beads-command-test-dep-relate-validation-missing-id1 ()
  "Unit test: dep-relate validation fails without id1."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-relate :id2 "bd-2")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-relate-validation-missing-id2 ()
  "Unit test: dep-relate validation fails without id2."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-relate :id1 "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-relate-validation-same-ids ()
  "Unit test: dep-relate validation fails with same IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-relate :id1 "bd-1" :id2 "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-relate-validation-success ()
  "Unit test: dep-relate validation succeeds with both IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-relate :id1 "bd-1" :id2 "bd-2")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-dep-unrelate

(ert-deftest beads-command-test-dep-unrelate-command-line-basic ()
  "Unit test: dep-unrelate builds correct command line."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-unrelate :id1 "bd-1" :id2 "bd-2"))
         (args (beads-command-line cmd)))
    (should (member "dep" args))
    (should (member "unrelate" args))
    (should (member "bd-1" args))
    (should (member "bd-2" args))))

(ert-deftest beads-command-test-dep-unrelate-validation-missing-id1 ()
  "Unit test: dep-unrelate validation fails without id1."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-unrelate :id2 "bd-2")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-unrelate-validation-missing-id2 ()
  "Unit test: dep-unrelate validation fails without id2."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-unrelate :id1 "bd-1")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-unrelate-validation-success ()
  "Unit test: dep-unrelate validation succeeds with both IDs."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-unrelate :id1 "bd-1" :id2 "bd-2")))
    (should (null (beads-command-validate cmd)))))

;;; Unit Tests: beads-command-delete

(ert-deftest beads-command-test-delete-command-line-basic ()
  "Unit test: delete command-line with just issue-ids.
Regression test for bug where issue-id was prepended instead of appended."
  :tags '(:unit)
  (let* ((cmd (beads-command-delete :issue-ids '("bd-42")))
         (args (beads-command-line cmd)))
    ;; Should be: ("bd" "delete" <...global-flags...> "bd-42")
    ;; NOT: ("bd" "bd-42" "delete" <...global-flags...>)
    (should (equal (car args) beads-executable))
    (should (equal (nth 1 args) "delete"))
    (should (member "bd-42" args))
    ;; Issue ID should come AFTER "delete"
    (should (> (cl-position "bd-42" args :test #'equal)
               (cl-position "delete" args :test #'equal)))))

(ert-deftest beads-command-test-delete-command-line-with-force ()
  "Unit test: delete command-line with --force flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-delete :issue-ids '("bd-123") :force t))
         (args (beads-command-line cmd)))
    ;; Should contain all elements in correct order
    (should (equal (car args) beads-executable))
    (should (equal (nth 1 args) "delete"))
    (should (member "bd-123" args))
    (should (member "--force" args))
    ;; Order should be: bd delete ... bd-123 ... --force
    (let ((delete-pos (cl-position "delete" args :test #'equal))
          (id-pos (cl-position "bd-123" args :test #'equal))
          (force-pos (cl-position "--force" args :test #'equal)))
      (should (< delete-pos id-pos))
      (should (< id-pos force-pos)))))

(ert-deftest beads-command-test-delete-command-line-without-force ()
  "Unit test: delete command-line without --force should not include flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-delete :issue-ids '("bd-99") :force nil))
         (args (beads-command-line cmd)))
    (should (equal (car args) beads-executable))
    (should (equal (nth 1 args) "delete"))
    (should (member "bd-99" args))
    (should-not (member "--force" args))))

(ert-deftest beads-command-test-delete-validation-requires-issue-id ()
  "Unit test: delete validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-delete)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "issue ID" (beads-command-validate cmd)))))

(ert-deftest beads-command-test-delete-validation-empty-issue-id ()
  "Unit test: delete validation fails with empty issue-ids list."
  :tags '(:unit)
  (let ((cmd (beads-command-delete :issue-ids '())))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "issue ID" (beads-command-validate cmd)))))

(ert-deftest beads-command-test-delete-validation-success ()
  "Unit test: delete validation succeeds with valid issue-ids."
  :tags '(:unit)
  (let ((cmd (beads-command-delete :issue-ids '("bd-42"))))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-test-delete-without-force-returns-preview-text ()
  "Unit test: delete without :force returns raw preview text, not JSON.
Regression test for be-3l0.4: beads-command-delete! without :force
would fail with beads-json-parse-error because bd returns preview
text (not JSON) when --force is not passed."
  :tags '(:unit)
  (let ((preview-text "Issue: bd-42 - Test Issue\n\nThis will delete 1 issue.\n"))
    (cl-letf (((symbol-function 'process-file)
               (beads-test--mock-call-process 0 preview-text)))
      (let ((result (beads-execute 'beads-command-delete :issue-ids '("bd-42"))))
        ;; Should return raw preview string, not signal JSON parse error
        (should (stringp result))
        (should (string-match-p "bd-42" result))))))

(ert-deftest beads-command-test-delete-with-force-parses-json ()
  "Unit test: delete with :force parses JSON response correctly."
  :tags '(:unit)
  (let ((json-output (json-encode '((deleted . "bd-42")
                                     (dependencies_removed . 0)
                                     (references_updated . 0)))))
    (cl-letf (((symbol-function 'process-file)
               (beads-test--mock-call-process 0 json-output)))
      (let ((result (beads-execute 'beads-command-delete :issue-ids '("bd-42") :force t)))
        ;; Should return parsed alist
        (should (consp result))
        (should (equal "bd-42" (alist-get 'deleted result)))))))

;;; Tests for beads-list-default-limit

(ert-deftest beads-command-test-list-class-has-nil-initform ()
  "Test that beads-command-list class has nil as default limit.
The default limit behavior is in beads-command-list!, not the class."
  :tags '(:unit)
  (let* ((cmd (beads-command-list))
         (args (beads-command-line cmd)))
    ;; Class initform is nil, so no --limit flag
    (should (null (oref cmd limit)))
    (should-not (member "--limit" args))))

(ert-deftest beads-command-test-list-bang-uses-default-limit ()
  "Test that beads-command-list! applies beads-list-default-limit."
  :tags '(:unit)
  (let* ((beads-list-default-limit 0)
         (executed-cmd nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (setq executed-cmd cmd)
                 nil)))
      (beads-list-execute)
      (should (eql (oref executed-cmd limit) 0)))))

(ert-deftest beads-command-test-list-bang-custom-default-limit ()
  "Test that a custom beads-list-default-limit is used by beads-command-list!."
  :tags '(:unit)
  (let* ((beads-list-default-limit 25)
         (executed-cmd nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (setq executed-cmd cmd)
                 nil)))
      (beads-list-execute)
      (should (eql (oref executed-cmd limit) 25))
      (let ((args (beads-command-line executed-cmd)))
        (should (member "--limit" args))
        (should (member "25" args))))))

(ert-deftest beads-command-test-list-explicit-limit-overrides ()
  "Test that explicit :limit overrides beads-list-default-limit."
  :tags '(:unit)
  (let* ((beads-list-default-limit 100)
         (executed-cmd nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (setq executed-cmd cmd)
                 nil)))
      (beads-list-execute :limit 5)
      (should (eql (oref executed-cmd limit) 5))
      (let ((args (beads-command-line executed-cmd)))
        (should (member "--limit" args))
        (should (member "5" args))))))

(ert-deftest beads-command-test-list-explicit-nil-limit ()
  "Test that :limit nil in beads-command-list! passes no --limit flag."
  :tags '(:unit)
  (let* ((beads-list-default-limit 100)
         (executed-cmd nil))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (setq executed-cmd cmd)
                 nil)))
      (beads-list-execute :limit nil)
      (should (null (oref executed-cmd limit)))
      (let ((args (beads-command-line executed-cmd)))
        (should-not (member "--limit" args))))))

;;; beads-command--process-environment tests

(ert-deftest beads-command-test-process-environment-nil-port ()
  "Test that nil beads-dolt-port returns process-environment unchanged."
  :tags '(:unit)
  (let ((beads-dolt-port nil)
        (process-environment '("FOO=bar")))
    (should (equal (beads-command--process-environment)
                   '("FOO=bar")))))

(ert-deftest beads-command-test-process-environment-with-port ()
  "Test that integer beads-dolt-port prepends BEADS_DOLT_PORT."
  :tags '(:unit)
  (let ((beads-dolt-port 3307)
        (process-environment '("FOO=bar")))
    (let ((env (beads-command--process-environment)))
      (should (equal (car env) "BEADS_DOLT_PORT=3307"))
      (should (member "FOO=bar" env)))))

(ert-deftest beads-command-test-process-environment-port-3308 ()
  "Test that beads-dolt-port 3308 produces correct env string."
  :tags '(:unit)
  (let ((beads-dolt-port 3308)
        (process-environment nil))
    (should (equal (beads-command--process-environment)
                   '("BEADS_DOLT_PORT=3308")))))

(provide 'beads-command-test)
;;; beads-command-test.el ends here
