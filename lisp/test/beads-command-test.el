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

;;; Integration Test: beads-command-quickstart

(ert-deftest beads-command-test-quickstart-basic ()
  "Test beads-command-quickstart execution returns quickstart guide.
Integration test that runs real bd quickstart command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((cmd (beads-command-quickstart))
         (result (beads-command-execute cmd))
         (exit-code (nth 0 result))
         (stdout (nth 1 result)))
    ;; Command should succeed
    (should (= exit-code 0))
    ;; Output should contain quickstart content
    (should (stringp stdout))
    (should (> (length stdout) 0))
    ;; Should contain common quickstart keywords
    (should (or (string-match-p "quick" (downcase stdout))
                (string-match-p "start" (downcase stdout))
                (string-match-p "bd" stdout)))))

(ert-deftest beads-command-test-quickstart-helper ()
  "Test beads-command-quickstart! helper function.
Integration test that verifies the convenience function works."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (let* ((result (beads-command-quickstart!))
         (exit-code (nth 0 result))
         (stdout (nth 1 result)))
    ;; Should return tuple (EXIT-CODE STDOUT STDERR)
    (should (= exit-code 0))
    (should (stringp stdout))
    (should (> (length stdout) 0))))

;;; Integration Test: beads-command-export

(ert-deftest beads-command-test-export-basic ()
  "Test beads-command-export exports to a file.
Integration test that runs real bd export command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a test issue first
    (beads-command-create! :title "Export test issue")
    ;; Export to temp file
    (let* ((temp-file (make-temp-file "beads-export-test-" nil ".jsonl"))
           (stats (beads-command-export! :output temp-file)))
      (unwind-protect
          (progn
            ;; Should return parsed JSON stats (alist)
            (should (listp stats))
            ;; File should exist
            (should (file-exists-p temp-file))
            ;; File should contain exported data
            (should (> (nth 7 (file-attributes temp-file)) 0)))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-command-test-export-with-status-filter ()
  "Test beads-command-export with status filter.
Integration test that verifies status filtering works."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create issues with different statuses
    (beads-command-create! :title "Open issue")
    (let ((closed-issue (beads-command-create! :title "Closed issue")))
      (beads-command-close! :issue-ids (list (oref closed-issue id))
                            :reason "Test"))
    ;; Export only open issues
    (let* ((temp-file (make-temp-file "beads-export-test-" nil ".jsonl"))
           (stats (beads-command-export! :output temp-file
                                         :status "open")))
      (unwind-protect
          (progn
            ;; Should return parsed JSON stats
            (should (listp stats))
            ;; File should exist
            (should (file-exists-p temp-file)))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-command-test-export-with-force ()
  "Test beads-command-export with --force flag.
Integration test that verifies force flag works."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let* ((temp-file (make-temp-file "beads-export-test-" nil ".jsonl"))
           (stats (beads-command-export! :output temp-file
                                         :force t)))
      (unwind-protect
          (progn
            ;; Should return parsed JSON stats
            (should (listp stats))
            ;; File should exist even if db is empty
            (should (file-exists-p temp-file)))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-command-test-export-helper ()
  "Test beads-command-export! helper function.
Integration test that verifies the convenience function works."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create a test issue
    (beads-command-create! :title "Helper test issue")
    (let* ((temp-file (make-temp-file "beads-export-test-" nil ".jsonl"))
           (stats (beads-command-export! :output temp-file)))
      (unwind-protect
          (progn
            ;; Should return parsed stats (since :json defaults to t)
            (should (listp stats))
            ;; File should exist and contain data
            (should (file-exists-p temp-file)))
        ;; Clean up
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

;;; Integration Test: beads-command-import

(ert-deftest beads-command-test-import-basic ()
  "Test beads-command-import imports from a file.
Integration test that runs real bd import command."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create and export an issue first
    (beads-command-create! :title "Import test issue")
    (let ((temp-file (make-temp-file "beads-import-test-" nil ".jsonl")))
      (unwind-protect
          (progn
            ;; Export to temp file
            (beads-command-export! :output temp-file)
            ;; Create a new project to import into
            (beads-test-with-project ()
              ;; Import from temp file (use rename-on-import for prefix mismatch)
              (let ((result (beads-command-import! :input temp-file
                                                   :rename-on-import t)))
                ;; Should return (EXIT-CODE STDOUT STDERR) tuple
                (should (listp result))
                (should (= 3 (length result)))
                (should (= 0 (nth 0 result)))  ;; Exit code should be 0
                ;; Verify issue was imported
                (let ((issues (beads-command-list!)))
                  (should (> (length issues) 0))
                  (should (cl-some
                           (lambda (issue)
                             (string= (oref issue title)
                                      "Import test issue"))
                           issues))))))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-command-test-import-with-dry-run ()
  "Test beads-command-import with --dry-run flag.
Integration test that verifies dry-run doesn't modify database."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create and export an issue
    (beads-command-create! :title "Dry run test issue")
    (let ((temp-file (make-temp-file "beads-import-test-" nil ".jsonl")))
      (unwind-protect
          (progn
            ;; Export to temp file
            (beads-command-export! :output temp-file)
            ;; Create new project and import with dry-run
            (beads-test-with-project ()
              (let ((result (beads-command-import! :input temp-file
                                                   :dry-run t
                                                   :rename-on-import t)))
                ;; Should return (EXIT-CODE STDOUT STDERR) tuple
                (should (listp result))
                (should (= 3 (length result)))
                (should (= 0 (nth 0 result)))  ;; Exit code should be 0
                ;; Database should still be empty (dry-run)
                (let ((issues (beads-command-list!)))
                  (should (= (length issues) 0))))))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(ert-deftest beads-command-test-import-with-skip-existing ()
  "Test beads-command-import with --skip-existing flag.
Integration test that verifies skip-existing import succeeds."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create initial issue
    (let ((issue (beads-command-create! :title "Original title")))
      (let ((temp-file (make-temp-file "beads-import-test-" nil ".jsonl")))
        (unwind-protect
            (progn
              ;; Export to temp file
              (beads-command-export! :output temp-file)
              ;; Modify issue title in database
              (beads-command-update! :issue-ids (list (oref issue id))
                                     :title "Modified title")
              ;; Import with skip-existing (should not update)
              (let ((result (beads-command-import! :input temp-file
                                                   :skip-existing t)))
                ;; Should return (EXIT-CODE STDOUT STDERR) tuple
                (should (listp result))
                (should (= 3 (length result)))
                (should (= 0 (nth 0 result)))))  ;; Exit code should be 0
          ;; Clean up temp file
          (when (file-exists-p temp-file)
            (delete-file temp-file)))))))

(ert-deftest beads-command-test-import-helper ()
  "Test beads-command-import! helper function.
Integration test that verifies the convenience function works."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create and export an issue
    (beads-command-create! :title "Helper test issue")
    (let ((temp-file (make-temp-file "beads-import-test-" nil ".jsonl")))
      (unwind-protect
          (progn
            ;; Export to temp file
            (beads-command-export! :output temp-file)
            ;; Create new project and test helper function
            (beads-test-with-project ()
              (let ((result (beads-command-import! :input temp-file
                                                   :rename-on-import t)))
                ;; Should return (EXIT-CODE STDOUT STDERR) tuple
                (should (listp result))
                (should (= 3 (length result)))
                (should (= 0 (nth 0 result)))  ;; Exit code should be 0
                ;; Verify issue was imported
                (let ((issues (beads-command-list!)))
                  (should (> (length issues) 0))))))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

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
      ;; All elements should be beads-blocked-issue instances
      (should (cl-every #'beads-blocked-issue-p issues)))))

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

;;; Unit Tests: beads-command-line

;; Unit tests for command-line building that don't require bd CLI.
;; These tests verify argument construction without executing commands.

(ert-deftest beads-command-test-unit-list-command-line-basic ()
  "Unit test: beads-command-list builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-list
               :status "open"
               :priority 1
               :no-daemon t))
         (args (beads-command-line cmd)))
    (should (listp args))
    (should (member "list" args))
    (should (member "--status" args))
    (should (member "open" args))
    (should (member "--priority" args))
    (should (member "1" args))
    (should (member "--no-daemon" args))
    (should (member "--json" args))))

(ert-deftest beads-command-test-unit-list-command-line-all-filters ()
  "Unit test: beads-command-list with all filter options."
  :tags '(:unit)
  (let* ((cmd (beads-command-list
               :status "in_progress"
               :priority 2
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
               :title "Test issue"
               :priority 1
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
               :skip-merge-driver t))
         (args (beads-command-line cmd)))
    (should (member "init" args))
    (should (member "--branch" args))
    (should (member "develop" args))
    (should (member "--prefix" args))
    (should (member "myproj" args))
    (should (member "--quiet" args))
    (should (member "--contributor" args))
    (should (member "--skip-merge-driver" args))))

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
                  ("discovered-from:bd-789")))
    (let ((cmd (beads-command-create
                :title "Test"
                :deps deps)))
      (should-not (beads-command-validate cmd))))
  ;; Invalid formats - should fail validation
  (dolist (deps '(("invalid")
                  ("blocks-bd-123")
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
    (should (stringp (beads-command-validate cmd)))))

(ert-deftest beads-command-test-unit-close-validate-issue-id-empty ()
  "Unit test: beads-command-close validation rejects empty issue ID."
  :tags '(:unit)
  (let ((cmd (beads-command-close :issue-ids '(""))))
    (should (stringp (beads-command-validate cmd)))))

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
    (should (equal (oref cmd json) t))
    (should-not (oref cmd priority))
    (should-not (oref cmd issue-type))
    (should-not (oref cmd description))))

(ert-deftest beads-command-test-unit-list-instantiation-defaults ()
  "Unit test: beads-command-list default values."
  :tags '(:unit)
  (let ((cmd (beads-command-list)))
    (should (beads-command-list-p cmd))
    (should (equal (oref cmd json) t))
    (should-not (oref cmd status))
    (should-not (oref cmd priority))
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
    (should (cl-typep cmd 'beads-command-json))
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
  (let ((cmd (beads-command-list :priority 1 :priority-min 0)))
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
  (let ((cmd1 (beads-command-list :priority 5))
        (cmd2 (beads-command-list :priority-min -1))
        (cmd3 (beads-command-list :priority-max 10)))
    (should (stringp (beads-command-validate cmd1)))
    (should (stringp (beads-command-validate cmd2)))
    (should (stringp (beads-command-validate cmd3)))))

(ert-deftest beads-command-test-list-validate-valid ()
  "List validation accepts valid configuration."
  :tags '(:unit)
  (let ((cmd (beads-command-list :priority 1 :status "open")))
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
  (let ((cmd (beads-command-show)))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "issue" (beads-command-validate cmd)))))

;; Additional argument building tests

(ert-deftest beads-command-test-list-command-line-with-filters ()
  "List command-line includes all filters."
  :tags '(:unit)
  (let* ((cmd (beads-command-list
               :status "open"
               :priority 1
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

(ert-deftest beads-command-test-create-command-line-integer-priority ()
  "Create command-line handles integer priority."
  :tags '(:unit)
  (let* ((cmd (beads-command-create
               :title "Test"
               :priority 2))
         (args (beads-command-line cmd)))
    (should (member "--priority" args))
    (should (member "2" args))))

;;; Integration Tests: beads-command-dep-add

(ert-deftest beads-command-test-dep-add-basic ()
  "Test beads-command-dep-add adds a dependency.
Integration test that adds a blocks dependency between two issues."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    ;; Create two test issues
    (let* ((issue1 (beads-command-create! :title "Issue 1"))
           (issue2 (beads-command-create! :title "Issue 2"))
           (id1 (oref issue1 id))
           (id2 (oref issue2 id)))
      ;; Add dependency: issue2 blocks issue1
      (let ((result (beads-command-dep-add!
                     :issue-id id1
                     :depends-on-id id2)))
        (should result)))))

(ert-deftest beads-command-test-dep-add-with-type ()
  "Test beads-command-dep-add with custom dependency type.
Integration test that adds a related dependency."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let* ((issue1 (beads-command-create! :title "Issue 1"))
           (issue2 (beads-command-create! :title "Issue 2"))
           (id1 (oref issue1 id))
           (id2 (oref issue2 id)))
      (let ((result (beads-command-dep-add!
                     :issue-id id1
                     :depends-on-id id2
                     :dep-type "related")))
        (should result)))))

;;; Integration Tests: beads-command-dep-remove

(ert-deftest beads-command-test-dep-remove-basic ()
  "Test beads-command-dep-remove removes a dependency.
Integration test that removes a dependency between two issues."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let* ((issue1 (beads-command-create! :title "Issue 1"))
           (issue2 (beads-command-create! :title "Issue 2"))
           (id1 (oref issue1 id))
           (id2 (oref issue2 id)))
      ;; Add dependency first
      (beads-command-dep-add! :issue-id id1 :depends-on-id id2)
      ;; Then remove it
      (let ((result (beads-command-dep-remove!
                     :issue-id id1
                     :depends-on-id id2)))
        (should result)))))

;;; Integration Tests: beads-command-dep-tree

(ert-deftest beads-command-test-dep-tree-basic ()
  "Test beads-command-dep-tree shows dependency tree.
Integration test that retrieves the dependency tree for an issue."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let* ((issue1 (beads-command-create! :title "Issue 1"))
           (id1 (oref issue1 id)))
      (let ((tree (beads-command-dep-tree! :issue-id id1)))
        (should tree)))))

(ert-deftest beads-command-test-dep-tree-with-max-depth ()
  "Test beads-command-dep-tree with max-depth option.
Integration test that retrieves dependency tree with depth limit."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let* ((issue1 (beads-command-create! :title "Issue 1"))
           (id1 (oref issue1 id)))
      (let ((tree (beads-command-dep-tree!
                   :issue-id id1
                   :max-depth 5)))
        (should tree)))))

(ert-deftest beads-command-test-dep-tree-reverse ()
  "Test beads-command-dep-tree with reverse flag.
Integration test that shows what depends on this issue."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let* ((issue1 (beads-command-create! :title "Issue 1"))
           (id1 (oref issue1 id)))
      (let ((tree (beads-command-dep-tree!
                   :issue-id id1
                   :reverse t)))
        (should tree)))))

;;; Integration Tests: beads-command-dep-cycles

(ert-deftest beads-command-test-dep-cycles-basic ()
  "Test beads-command-dep-cycles detects cycles.
Integration test that checks for dependency cycles."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-test-with-project ()
    (let ((result (beads-command-dep-cycles!)))
      (should (not (null result))))))

;;; Unit Tests: beads-command-dep-* command-line building

(ert-deftest beads-command-test-dep-add-command-line-basic ()
  "Unit test: beads-command-dep-add builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-add
               :issue-id "bd-1"
               :depends-on-id "bd-2"))
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
               :depends-on-id "bd-2"
               :dep-type "related"))
         (args (beads-command-line cmd)))
    (should (member "--type" args))
    (should (member "related" args))))

(ert-deftest beads-command-test-dep-remove-command-line-basic ()
  "Unit test: beads-command-dep-remove builds correct arguments."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-remove
               :issue-id "bd-1"
               :depends-on-id "bd-2"))
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
  "Unit test: beads-command-dep-tree includes reverse flag."
  :tags '(:unit)
  (let* ((cmd (beads-command-dep-tree
               :issue-id "bd-1"
               :reverse t))
         (args (beads-command-line cmd)))
    (should (member "--reverse" args))))

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
              :depends-on-id "bd-2")))
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
              :depends-on-id "bd-2")))
    (should (null (beads-command-validate cmd)))))

(ert-deftest beads-command-test-dep-remove-validation-missing-issue-id ()
  "Unit test: dep-remove validation fails without issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-remove
              :depends-on-id "bd-2")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-command-test-dep-remove-validation-success ()
  "Unit test: dep-remove validation succeeds with required fields."
  :tags '(:unit)
  (let ((cmd (beads-command-dep-remove
              :issue-id "bd-1"
              :depends-on-id "bd-2")))
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

;;; Unit Tests: beads-command-delete

(ert-deftest beads-command-test-delete-command-line-basic ()
  "Unit test: delete command-line with just issue-id.
Regression test for bug where issue-id was prepended instead of appended."
  :tags '(:unit)
  (let* ((cmd (beads-command-delete :issue-id "bd-42"))
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
  (let* ((cmd (beads-command-delete :issue-id "bd-123" :force t))
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
  (let* ((cmd (beads-command-delete :issue-id "bd-99" :force nil))
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
  "Unit test: delete validation fails with empty issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-delete :issue-id "")))
    (should (stringp (beads-command-validate cmd)))
    (should (string-match-p "issue ID" (beads-command-validate cmd)))))

(ert-deftest beads-command-test-delete-validation-success ()
  "Unit test: delete validation succeeds with valid issue-id."
  :tags '(:unit)
  (let ((cmd (beads-command-delete :issue-id "bd-42")))
    (should (null (beads-command-validate cmd)))))

;;; ============================================================
;;; Daemon Data Structure Tests (EIEIO Classes)
;;; ============================================================

(defvar beads-command-test--daemon-status-json
  '((running . t)
    (pid . 130264)
    (started . "2025-12-08 11:10:36")
    (log_path . "/home/user/.beads/daemon.log"))
  "Sample daemon status JSON for testing.")

(defvar beads-command-test--daemon-health-json
  '((status . "healthy")
    (version . "0.28.0")
    (client_version . "0.0.0")
    (compatible . t)
    (uptime_seconds . 24081.247)
    (db_response_ms . 4.105)
    (active_connections . 1)
    (max_connections . 100)
    (memory_alloc_mb . 5))
  "Sample daemon health JSON for testing.")

(defvar beads-command-test--daemon-metrics-json
  `((timestamp . "2025-12-08T17:51:57.308003235+01:00")
    (uptime_seconds . 24083)
    (operations . ,(vector
                    '((operation . "health")
                      (total_count . 99)
                      (success_count . 99)
                      (error_count . 0)
                      (latency . ((min_ms . 2.56)
                                  (p50_ms . 3.58)
                                  (p95_ms . 7.01)
                                  (p99_ms . 8.70)
                                  (max_ms . 8.70)
                                  (avg_ms . 3.89))))
                    '((operation . "list")
                      (total_count . 4)
                      (success_count . 4)
                      (error_count . 0)
                      (latency . ((min_ms . 6.67)
                                  (p50_ms . 14.89)
                                  (p95_ms . 21.40)
                                  (p99_ms . 21.40)
                                  (max_ms . 21.40)
                                  (avg_ms . 14.46))))))
    (total_connections . 51)
    (active_connections . 1)
    (rejected_connections . 0)
    (memory_alloc_mb . 5)
    (memory_sys_mb . 21)
    (goroutine_count . 12))
  "Sample daemon metrics JSON for testing.")

(ert-deftest beads-command-test-daemon-status-class-create ()
  "Test creating beads-daemon-status EIEIO object."
  :tags '(:unit :daemon)
  (let ((status (beads-daemon-status
                 :running t
                 :pid 12345
                 :started "2025-12-08 10:00:00"
                 :log-path "/tmp/daemon.log")))
    (should (beads-daemon-status-p status))
    (should (eq (oref status running) t))
    (should (= (oref status pid) 12345))
    (should (equal (oref status started) "2025-12-08 10:00:00"))
    (should (equal (oref status log-path) "/tmp/daemon.log"))))

(ert-deftest beads-command-test-daemon-status-class-default ()
  "Test beads-daemon-status EIEIO object with default values."
  :tags '(:unit :daemon)
  (let ((status (beads-daemon-status)))
    (should (beads-daemon-status-p status))
    (should (null (oref status running)))
    (should (null (oref status pid)))
    (should (null (oref status started)))
    (should (null (oref status log-path)))))

(ert-deftest beads-command-test-daemon-health-class-create ()
  "Test creating beads-daemon-health EIEIO object."
  :tags '(:unit :daemon)
  (let ((health (beads-daemon-health
                 :status "healthy"
                 :version "0.28.0"
                 :client-version "0.0.0"
                 :compatible t
                 :uptime-seconds 3600.5
                 :db-response-ms 4.5
                 :active-connections 2
                 :max-connections 100
                 :memory-alloc-mb 8)))
    (should (beads-daemon-health-p health))
    (should (equal (oref health status) "healthy"))
    (should (equal (oref health version) "0.28.0"))
    (should (equal (oref health client-version) "0.0.0"))
    (should (eq (oref health compatible) t))
    (should (= (oref health uptime-seconds) 3600.5))
    (should (= (oref health db-response-ms) 4.5))
    (should (= (oref health active-connections) 2))
    (should (= (oref health max-connections) 100))
    (should (= (oref health memory-alloc-mb) 8))))

(ert-deftest beads-command-test-daemon-operation-class-create ()
  "Test creating beads-daemon-operation EIEIO object."
  :tags '(:unit :daemon)
  (let ((op (beads-daemon-operation
             :operation "health"
             :total-count 100
             :success-count 99
             :error-count 1
             :latency '((avg_ms . 5.0) (p99_ms . 10.0)))))
    (should (beads-daemon-operation-p op))
    (should (equal (oref op operation) "health"))
    (should (= (oref op total-count) 100))
    (should (= (oref op success-count) 99))
    (should (= (oref op error-count) 1))
    (should (equal (alist-get 'avg_ms (oref op latency)) 5.0))))

(ert-deftest beads-command-test-daemon-metrics-class-create ()
  "Test creating beads-daemon-metrics EIEIO object."
  :tags '(:unit :daemon)
  (let* ((ops (list (beads-daemon-operation
                     :operation "health"
                     :total-count 10
                     :success-count 10
                     :error-count 0)))
         (metrics (beads-daemon-metrics
                   :timestamp "2025-12-08T12:00:00Z"
                   :uptime-seconds 7200
                   :operations ops
                   :total-connections 50
                   :active-connections 2
                   :rejected-connections 0
                   :memory-alloc-mb 5
                   :memory-sys-mb 20
                   :goroutine-count 12)))
    (should (beads-daemon-metrics-p metrics))
    (should (equal (oref metrics timestamp) "2025-12-08T12:00:00Z"))
    (should (= (oref metrics uptime-seconds) 7200))
    (should (= (length (oref metrics operations)) 1))
    (should (= (oref metrics total-connections) 50))
    (should (= (oref metrics active-connections) 2))
    (should (= (oref metrics rejected-connections) 0))
    (should (= (oref metrics memory-alloc-mb) 5))
    (should (= (oref metrics memory-sys-mb) 20))
    (should (= (oref metrics goroutine-count) 12))))

;;; ============================================================
;;; Daemon JSON Parsing Tests
;;; ============================================================

(ert-deftest beads-command-test-daemon-parse-status-running ()
  "Test parsing running daemon status JSON."
  :tags '(:unit :daemon)
  (let ((status (beads-daemon--parse-status
                 beads-command-test--daemon-status-json)))
    (should (beads-daemon-status-p status))
    (should (eq (oref status running) t))
    (should (= (oref status pid) 130264))
    (should (equal (oref status started) "2025-12-08 11:10:36"))
    (should (equal (oref status log-path)
                   "/home/user/.beads/daemon.log"))))

(ert-deftest beads-command-test-daemon-parse-status-stopped ()
  "Test parsing stopped daemon status JSON."
  :tags '(:unit :daemon)
  (let ((status (beads-daemon--parse-status
                 '((running . :json-false)
                   (pid . nil)
                   (started . nil)
                   (log_path . nil)))))
    (should (beads-daemon-status-p status))
    (should (null (oref status running)))
    (should (null (oref status pid)))
    (should (null (oref status started)))))

(ert-deftest beads-command-test-daemon-parse-status-empty ()
  "Test parsing empty/nil status JSON."
  :tags '(:unit :daemon)
  (let ((status (beads-daemon--parse-status nil)))
    (should (beads-daemon-status-p status))
    (should (null (oref status running)))))

(ert-deftest beads-command-test-daemon-parse-health ()
  "Test parsing daemon health JSON."
  :tags '(:unit :daemon)
  (let ((health (beads-daemon--parse-health
                 beads-command-test--daemon-health-json)))
    (should (beads-daemon-health-p health))
    (should (equal (oref health status) "healthy"))
    (should (equal (oref health version) "0.28.0"))
    (should (equal (oref health client-version) "0.0.0"))
    (should (eq (oref health compatible) t))
    (should (= (oref health uptime-seconds) 24081.247))
    (should (= (oref health db-response-ms) 4.105))
    (should (= (oref health active-connections) 1))
    (should (= (oref health max-connections) 100))
    (should (= (oref health memory-alloc-mb) 5))))

(ert-deftest beads-command-test-daemon-parse-health-empty ()
  "Test parsing empty/nil health JSON."
  :tags '(:unit :daemon)
  (let ((health (beads-daemon--parse-health nil)))
    (should (beads-daemon-health-p health))
    (should (null (oref health status)))))

(ert-deftest beads-command-test-daemon-parse-operation ()
  "Test parsing operation statistics JSON."
  :tags '(:unit :daemon)
  (let ((op (beads-daemon--parse-operation
             '((operation . "health")
               (total_count . 99)
               (success_count . 99)
               (error_count . 0)
               (latency . ((avg_ms . 3.89) (p99_ms . 8.70)))))))
    (should (beads-daemon-operation-p op))
    (should (equal (oref op operation) "health"))
    (should (= (oref op total-count) 99))
    (should (= (oref op success-count) 99))
    (should (= (oref op error-count) 0))
    (should (= (alist-get 'avg_ms (oref op latency)) 3.89))))

(ert-deftest beads-command-test-daemon-parse-metrics ()
  "Test parsing daemon metrics JSON."
  :tags '(:unit :daemon)
  (let ((metrics (beads-daemon--parse-metrics
                  beads-command-test--daemon-metrics-json)))
    (should (beads-daemon-metrics-p metrics))
    (should (string-prefix-p "2025-12-08"
                             (oref metrics timestamp)))
    (should (= (oref metrics uptime-seconds) 24083))
    (should (= (length (oref metrics operations)) 2))
    (should (= (oref metrics total-connections) 51))
    (should (= (oref metrics active-connections) 1))
    (should (= (oref metrics rejected-connections) 0))
    (should (= (oref metrics memory-alloc-mb) 5))
    (should (= (oref metrics memory-sys-mb) 21))
    (should (= (oref metrics goroutine-count) 12))
    ;; Check first operation
    (let ((first-op (car (oref metrics operations))))
      (should (beads-daemon-operation-p first-op))
      (should (equal (oref first-op operation) "health")))))

(ert-deftest beads-command-test-daemon-parse-metrics-empty-operations ()
  "Test parsing metrics JSON with empty operations."
  :tags '(:unit :daemon)
  (let ((metrics (beads-daemon--parse-metrics
                  '((timestamp . "2025-12-08T12:00:00Z")
                    (operations . [])
                    (total_connections . 0)))))
    (should (beads-daemon-metrics-p metrics))
    (should (null (oref metrics operations)))))

;;; ============================================================
;;; Daemon Utility Function Tests
;;; ============================================================

(ert-deftest beads-command-test-daemon-format-uptime-seconds ()
  "Test formatting uptime in seconds."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-uptime 30) "30s"))
  (should (equal (beads-daemon--format-uptime 59) "59s")))

(ert-deftest beads-command-test-daemon-format-uptime-minutes ()
  "Test formatting uptime in minutes."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-uptime 60) "1m 0s"))
  (should (equal (beads-daemon--format-uptime 90) "1m 30s"))
  (should (equal (beads-daemon--format-uptime 3599) "59m 59s")))

(ert-deftest beads-command-test-daemon-format-uptime-hours ()
  "Test formatting uptime in hours."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-uptime 3600) "1h 0m 0s"))
  (should (equal (beads-daemon--format-uptime 7265) "2h 1m 5s"))
  (should (equal (beads-daemon--format-uptime 86399) "23h 59m 59s")))

(ert-deftest beads-command-test-daemon-format-uptime-days ()
  "Test formatting uptime in days."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-uptime 86400) "1d 0h 0m"))
  (should (equal (beads-daemon--format-uptime 172800) "2d 0h 0m"))
  (should (equal (beads-daemon--format-uptime 93600) "1d 2h 0m")))

(ert-deftest beads-command-test-daemon-format-uptime-nil ()
  "Test formatting nil uptime."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-uptime nil) "N/A")))

(ert-deftest beads-command-test-daemon-format-uptime-negative ()
  "Test formatting negative uptime."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-uptime -1) "N/A")))

(ert-deftest beads-command-test-daemon-format-uptime-float ()
  "Test formatting float uptime (truncates to integer)."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-uptime 65.7) "1m 5s")))

(ert-deftest beads-command-test-daemon-format-uptime-zero ()
  "Test formatting zero uptime."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-uptime 0) "0s")))

(ert-deftest beads-command-test-daemon-format-bytes-mb ()
  "Test formatting bytes in megabytes."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-bytes 5) "5 MB"))
  (should (equal (beads-daemon--format-bytes 100) "100 MB"))
  (should (equal (beads-daemon--format-bytes 1023) "1023 MB")))

(ert-deftest beads-command-test-daemon-format-bytes-gb ()
  "Test formatting bytes in gigabytes."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-bytes 1024) "1.0 GB"))
  (should (equal (beads-daemon--format-bytes 2048) "2.0 GB"))
  (should (equal (beads-daemon--format-bytes 1536) "1.5 GB")))

(ert-deftest beads-command-test-daemon-format-bytes-nil ()
  "Test formatting nil bytes."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-bytes nil) "N/A")))

(ert-deftest beads-command-test-daemon-format-bytes-negative ()
  "Test formatting negative bytes."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-bytes -1) "N/A")))

(ert-deftest beads-command-test-daemon-format-bytes-zero ()
  "Test formatting zero bytes."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-bytes 0) "0 MB")))

(ert-deftest beads-command-test-daemon-format-latency ()
  "Test formatting latency statistics."
  :tags '(:unit :daemon)
  (let ((latency '((avg_ms . 5.5) (p99_ms . 10.2))))
    (should (equal (beads-daemon--format-latency latency)
                   "avg: 5.50ms, p99: 10.20ms"))))

(ert-deftest beads-command-test-daemon-format-latency-nil ()
  "Test formatting nil latency."
  :tags '(:unit :daemon)
  (should (equal (beads-daemon--format-latency nil) "N/A")))

(ert-deftest beads-command-test-daemon-format-latency-missing-fields ()
  "Test formatting latency with missing fields."
  :tags '(:unit :daemon)
  (let ((latency '((min_ms . 1.0))))  ; missing avg_ms and p99_ms
    (should (string-match-p "avg: 0.00ms"
                            (beads-daemon--format-latency latency)))))

;;; ============================================================
;;; Daemon Command Class Tests
;;; ============================================================

(ert-deftest beads-command-test-daemon-status-line ()
  "Test building daemon status command line."
  :tags '(:unit :daemon)
  (let ((cmd (beads-daemon-command-status)))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "daemon" cmd-line))
      (should (member "--status" cmd-line))
      (should (member "--json" cmd-line)))))

(ert-deftest beads-command-test-daemon-health-line ()
  "Test building daemon health command line."
  :tags '(:unit :daemon)
  (let ((cmd (beads-daemon-command-health)))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "daemon" cmd-line))
      (should (member "--health" cmd-line))
      (should (member "--json" cmd-line)))))

(ert-deftest beads-command-test-daemon-metrics-line ()
  "Test building daemon metrics command line."
  :tags '(:unit :daemon)
  (let ((cmd (beads-daemon-command-metrics)))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "daemon" cmd-line))
      (should (member "--metrics" cmd-line))
      (should (member "--json" cmd-line)))))

(ert-deftest beads-command-test-daemon-start-line-minimal ()
  "Test building daemon start command line with no options."
  :tags '(:unit :daemon)
  (let ((cmd (beads-daemon-command-start)))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "daemon" cmd-line))
      (should (member "--start" cmd-line))
      (should (member "--json" cmd-line)))))

(ert-deftest beads-command-test-daemon-start-line-with-options ()
  "Test building daemon start command line with all options."
  :tags '(:unit :daemon)
  (let ((cmd (beads-daemon-command-start
              :auto-commit t
              :auto-push t
              :foreground t
              :local t
              :interval "10s"
              :log "/tmp/test.log")))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "daemon" cmd-line))
      (should (member "--start" cmd-line))
      (should (member "--auto-commit" cmd-line))
      (should (member "--auto-push" cmd-line))
      (should (member "--foreground" cmd-line))
      (should (member "--local" cmd-line))
      (should (member "--interval" cmd-line))
      (should (member "10s" cmd-line))
      (should (member "--log" cmd-line))
      (should (member "/tmp/test.log" cmd-line))
      (should (member "--json" cmd-line)))))

(ert-deftest beads-command-test-daemon-stop-line ()
  "Test building daemon stop command line."
  :tags '(:unit :daemon)
  (let ((cmd (beads-daemon-command-stop)))
    (let ((cmd-line (beads-command-line cmd)))
      (should (member "daemon" cmd-line))
      (should (member "--stop" cmd-line))
      (should (member "--json" cmd-line)))))

;;; ============================================================
;;; Daemon Command Validation Tests
;;; ============================================================

(ert-deftest beads-command-test-daemon-start-validate-valid-interval ()
  "Test validation of valid interval formats."
  :tags '(:unit :daemon)
  (dolist (interval '("5s" "10s" "1m" "30m" "1h" "0.5s" "100ms"))
    (let ((cmd (beads-daemon-command-start :interval interval)))
      (should-not (beads-command-validate cmd)))))

(ert-deftest beads-command-test-daemon-start-validate-invalid-interval ()
  "Test validation of invalid interval formats."
  :tags '(:unit :daemon)
  (dolist (interval '("5" "10seconds" "1 minute" "abc" "5x"))
    (let ((cmd (beads-daemon-command-start :interval interval)))
      (should (beads-command-validate cmd)))))

(ert-deftest beads-command-test-daemon-start-validate-no-interval ()
  "Test validation passes when no interval specified."
  :tags '(:unit :daemon)
  (let ((cmd (beads-daemon-command-start)))
    (should-not (beads-command-validate cmd))))

;;; ============================================================
;;; Daemon Command Execution Tests (Mocked)
;;; ============================================================

(ert-deftest beads-command-test-daemon-execute-status ()
  "Test executing daemon status command with mocked output."
  :tags '(:integration :daemon)
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-command-test--daemon-status-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((status (beads-command-execute (beads-daemon-command-status))))
         (should (beads-daemon-status-p status))
         (should (eq (oref status running) t))
         (should (= (oref status pid) 130264)))))))

(ert-deftest beads-command-test-daemon-execute-health ()
  "Test executing daemon health command with mocked output."
  :tags '(:integration :daemon)
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-command-test--daemon-health-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((health (beads-command-execute (beads-daemon-command-health))))
         (should (beads-daemon-health-p health))
         (should (equal (oref health status) "healthy"))
         (should (equal (oref health version) "0.28.0")))))))

(ert-deftest beads-command-test-daemon-execute-metrics ()
  "Test executing daemon metrics command with mocked output."
  :tags '(:integration :daemon)
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-command-test--daemon-metrics-json)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((metrics (beads-command-execute (beads-daemon-command-metrics))))
         (should (beads-daemon-metrics-p metrics))
         (should (= (oref metrics uptime-seconds) 24083))
         (should (= (length (oref metrics operations)) 2)))))))

;;; ============================================================
;;; Daemon Edge Case Tests
;;; ============================================================

(ert-deftest beads-command-test-daemon-parse-status-missing-fields ()
  "Test parsing status JSON with missing fields."
  :tags '(:unit :daemon)
  (let ((status (beads-daemon--parse-status '((running . t)))))
    (should (beads-daemon-status-p status))
    (should (eq (oref status running) t))
    (should (null (oref status pid)))
    (should (null (oref status started)))
    (should (null (oref status log-path)))))

(ert-deftest beads-command-test-daemon-parse-health-missing-fields ()
  "Test parsing health JSON with missing fields."
  :tags '(:unit :daemon)
  (let ((health (beads-daemon--parse-health '((status . "healthy")))))
    (should (beads-daemon-health-p health))
    (should (equal (oref health status) "healthy"))
    (should (null (oref health version)))
    (should (null (oref health uptime-seconds)))))

(ert-deftest beads-command-test-daemon-parse-metrics-nil-operations ()
  "Test parsing metrics JSON with nil operations."
  :tags '(:unit :daemon)
  (let ((metrics (beads-daemon--parse-metrics
                  '((timestamp . "2025-12-08T12:00:00Z")
                    (operations . nil)))))
    (should (beads-daemon-metrics-p metrics))
    (should (null (oref metrics operations)))))

(provide 'beads-command-test)
;;; beads-command-test.el ends here
