;;; beads-quickstart-workflow-test.el --- Quickstart guide as executable test -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: lisp, tools, test

;;; Commentary:

;; This file implements the bd quickstart guide as an executable ERT test
;; using the EIEIO-based test DSL.  It serves as both validation of the
;; DSL and executable documentation of how to use beads.el.
;;
;; The test exercises the complete workflow:
;; 1. Initialize project (done automatically by test harness)
;; 2. Create issues with various types and priorities
;; 3. List issues with filters
;; 4. Show issue details
;; 5. Update issue status
;; 6. Add dependencies between issues
;; 7. Close issues with reasons
;; 8. View project statistics
;;
;; IMPORTANT: All assertions use dynamic ID lookup because bd uses
;; hash-based IDs (e.g., myproject-a3f8) not sequential IDs.
;;
;; This test requires the bd CLI to be installed and will be skipped
;; if bd is not available.

;;; Code:

(require 'ert)
(require 'beads-test-dsl)
(require 'beads-test)
(require 'beads)
(require 'beads-command)

;;; ============================================================
;;; Helper Functions for Dynamic ID Lookup
;;; ============================================================

(defun beads-quickstart--get-first-created-id (ctx)
  "Get the first created issue ID from CTX (in creation order)."
  (car (reverse (oref ctx created-issues))))

(defun beads-quickstart--get-second-created-id (ctx)
  "Get the second created issue ID from CTX (in creation order)."
  (cadr (reverse (oref ctx created-issues))))

(defun beads-quickstart--get-third-created-id (ctx)
  "Get the third created issue ID from CTX (in creation order)."
  (caddr (reverse (oref ctx created-issues))))

(defun beads-quickstart--get-issue-by-title (title)
  "Find issue with TITLE in the current project, return its ID."
  (let ((issues (beads-command-list!)))
    (when-let ((issue (seq-find (lambda (i)
                                  (equal (oref i title) title))
                                issues)))
      (oref issue id))))

;;; ============================================================
;;; Individual Step Tests
;;; ============================================================

(beads-define-workflow-test beads-quickstart-step-1-create-first-issue
  :description "Step 1: Create first issue - Set up CI/CD pipeline"
  :tags (:integration :quickstart)
  :steps
  ((beads-test-action-create-issue
    :title "Set up CI/CD pipeline"
    :type "task"
    :priority 1))
  :assert
  ((beads-test-assert-issue-created ctx
                                    :title "Set up CI/CD pipeline"
                                    :type "task"
                                    :priority 1)
   (beads-test-assert-completion-cache-invalidated ctx)))

(beads-define-workflow-test beads-quickstart-step-2-create-related-issues
  :description "Step 2: Create related issues with different types and priorities"
  :tags (:integration :quickstart)
  :steps
  ((beads-test-action-create-issue
    :title "Write unit tests"
    :type "task"
    :priority 2)
   (beads-test-action-create-issue
    :title "Fix login bug"
    :type "bug"
    :priority 0))
  :assert
  ((beads-test-assert-issue-count ctx 2)
   (beads-test-assert-issue-created ctx :title "Write unit tests" :type "task")
   (beads-test-assert-issue-created ctx :title "Fix login bug" :type "bug")))

(beads-define-workflow-test beads-quickstart-step-3-list-issues
  :description "Step 3: List issues with various filters"
  :tags (:integration :quickstart)
  :setup
  (lambda (ctx)
    ;; Create a few issues to list
    (let ((issue1 (beads-command-create! :title "Open issue 1"
                                         :issue-type "task"
                                         :priority 1))
          (issue2 (beads-command-create! :title "Open issue 2"
                                         :issue-type "bug"
                                         :priority 0)))
      (push (oref issue1 id) (oref ctx created-issues))
      (push (oref issue2 id) (oref ctx created-issues))))
  :steps
  ((beads-test-action-open-list :filter-status "open"))
  :assert
  ((should (derived-mode-p 'beads-list-mode))
   (beads-test-assert-buffer-exists ctx "\\*beads-list")))

(beads-define-workflow-test beads-quickstart-step-4-show-issue
  :description "Step 4: Show issue details using dynamic ID lookup"
  :tags (:integration :quickstart)
  :setup
  (lambda (ctx)
    (let ((issue (beads-command-create! :title "Show test issue"
                                        :issue-type "feature"
                                        :priority 2
                                        :description "Test description")))
      (push (oref issue id) (oref ctx created-issues))))
  :steps
  ((beads-test-action-show-issue
    :issue-id (lambda (ctx) (car (oref ctx created-issues)))))
  :assert
  ((should (derived-mode-p 'beads-show-mode))
   (beads-test-assert-buffer-exists ctx "\\*beads-show")))

(beads-define-workflow-test beads-quickstart-step-5-update-status
  :description "Step 5: Update issue status to in_progress"
  :tags (:integration :quickstart)
  :setup
  (lambda (ctx)
    (let ((issue (beads-command-create! :title "Update test"
                                        :issue-type "task"
                                        :priority 1)))
      (push (oref issue id) (oref ctx created-issues))))
  :steps
  ((beads-test-action-update-issue
    :issue-id (lambda (ctx) (car (oref ctx created-issues)))
    :changes '((status . "in_progress"))))
  :assert
  ((let ((issue-id (car (oref ctx created-issues))))
     (beads-test-assert-issue-has ctx issue-id :status "in_progress"))))

(beads-define-workflow-test beads-quickstart-step-6-add-dependency
  :description "Step 6: Add dependency between issues"
  :tags (:integration :quickstart)
  :setup
  (lambda (ctx)
    (let ((issue1 (beads-command-create! :title "Blocking issue"
                                         :issue-type "task"
                                         :priority 1))
          (issue2 (beads-command-create! :title "Blocked issue"
                                         :issue-type "task"
                                         :priority 2)))
      ;; Store in creation order (issue1 first, then issue2)
      (push (oref issue1 id) (oref ctx created-issues))
      (push (oref issue2 id) (oref ctx created-issues))))
  :steps
  ((beads-test-action-add-dependency
    :issue-id (lambda (ctx) (car (oref ctx created-issues)))
    :depends-on-id (lambda (ctx) (cadr (oref ctx created-issues)))
    :dep-type "blocks"))
  :assert
  ;; Verify dependency was added by checking blocked issues
  ((let* ((issues (beads-command-blocked!))
          (blocked-id (car (oref ctx created-issues))))
     ;; The blocked issue should appear in blocked list
     (should (seq-find (lambda (i) (equal (oref i id) blocked-id))
                       issues)))))

(beads-define-workflow-test beads-quickstart-step-7-close-issue
  :description "Step 7: Close issue with reason"
  :tags (:integration :quickstart)
  :setup
  (lambda (ctx)
    (let ((issue (beads-command-create! :title "Close test"
                                        :issue-type "task")))
      (push (oref issue id) (oref ctx created-issues))))
  :steps
  ((beads-test-action-close-issue
    :issue-id (lambda (ctx) (car (oref ctx created-issues)))
    :reason "Task completed successfully"))
  :assert
  ((let ((issue-id (car (oref ctx created-issues))))
     (beads-test-assert-issue-has ctx issue-id :status "closed"))))

(beads-define-workflow-test beads-quickstart-step-8-view-stats
  :description "Step 8: View project statistics"
  :tags (:integration :quickstart)
  :setup
  (lambda (ctx)
    ;; Create some issues to have stats for
    (let ((issue1 (beads-command-create! :title "Stats test 1"
                                         :issue-type "task"))
          (issue2 (beads-command-create! :title "Stats test 2"
                                         :issue-type "bug")))
      (push (oref issue1 id) (oref ctx created-issues))
      (push (oref issue2 id) (oref ctx created-issues))))
  :steps
  ((beads-test-action-show-stats))
  :assert
  ;; Stats command should return data with issue counts
  ((let ((trace (oref ctx execution-trace)))
     ;; Last trace entry should have stats result
     (when-let* ((last-entry (car trace))
                 (result (caddr last-entry)))
       (should (listp result))))))

;;; ============================================================
;;; Complete Quickstart Guide - Full Workflow
;;; ============================================================

(beads-define-workflow-test beads-quickstart-full-guide
  :description "Execute the complete bd quickstart guide as an integration test"
  :tags (:integration :quickstart :lifecycle)
  :steps
  ;; Step 1: Create first issue
  ((beads-test-action-create-issue
    :title "Set up CI/CD pipeline"
    :type "task"
    :priority 1)

   ;; Step 2: Create related issues
   (beads-test-action-create-issue
    :title "Write unit tests"
    :type "task"
    :priority 2)

   (beads-test-action-create-issue
    :title "Fix login bug"
    :type "bug"
    :priority 0)

   ;; Step 3: List issues (open list buffer)
   (beads-test-action-open-list :filter-status "open")

   ;; Step 4: Show first issue details (using dynamic ID)
   (beads-test-action-show-issue
    :issue-id (lambda (ctx) (beads-quickstart--get-first-created-id ctx)))

   ;; Step 5: Update first issue status
   (beads-test-action-update-issue
    :issue-id (lambda (ctx) (beads-quickstart--get-first-created-id ctx))
    :changes '((status . "in_progress")))

   ;; Step 6: Add dependency (second issue blocks first)
   (beads-test-action-add-dependency
    :issue-id (lambda (ctx) (beads-quickstart--get-first-created-id ctx))
    :depends-on-id (lambda (ctx) (beads-quickstart--get-second-created-id ctx))
    :dep-type "blocks")

   ;; Step 7: Close the first issue
   (beads-test-action-close-issue
    :issue-id (lambda (ctx) (beads-quickstart--get-first-created-id ctx))
    :reason "CI/CD pipeline completed")

   ;; Step 8: View statistics
   (beads-test-action-show-stats))

  :assert
  ;; Verify final state
  ((beads-test-assert-issue-count ctx 3)

   ;; First issue should be closed
   (let ((first-id (beads-quickstart--get-first-created-id ctx)))
     (beads-test-assert-issue-has ctx first-id :status "closed"))

   ;; Second issue should still be open
   (let ((second-id (beads-quickstart--get-second-created-id ctx)))
     (beads-test-assert-issue-has ctx second-id :status "open"))

   ;; Third issue (bug) should still be open
   (let ((third-id (beads-quickstart--get-third-created-id ctx)))
     (beads-test-assert-issue-has ctx third-id :status "open"))))

;;; ============================================================
;;; Additional Quickstart Variations
;;; ============================================================

(beads-define-workflow-test beads-quickstart-create-update-close-lifecycle
  :description "Test complete issue lifecycle: create, update, close"
  :tags (:integration :quickstart :lifecycle)
  :steps
  ((beads-test-action-create-issue
    :title "Lifecycle test issue"
    :type "feature"
    :priority 2
    :description "Testing the complete issue lifecycle")

   (beads-test-action-update-issue
    :issue-id (lambda (ctx) (car (oref ctx created-issues)))
    :changes '((status . "in_progress") (priority . 1)))

   (beads-test-action-close-issue
    :issue-id (lambda (ctx) (car (oref ctx created-issues)))
    :reason "Feature implemented and tested"))

  :assert
  ((beads-test-assert-issue-count ctx 1)
   (let ((id (car (oref ctx created-issues))))
     (beads-test-assert-issue-has ctx id :status "closed"))))

(beads-define-workflow-test beads-quickstart-multiple-issue-types
  :description "Create issues of all types (bug, feature, task, epic, chore)"
  :tags (:integration :quickstart)
  :steps
  ((beads-test-action-create-issue :title "Bug issue" :type "bug" :priority 0)
   (beads-test-action-create-issue :title "Feature issue" :type "feature"
                                   :priority 1)
   (beads-test-action-create-issue :title "Task issue" :type "task" :priority 2)
   (beads-test-action-create-issue :title "Epic issue" :type "epic" :priority 1)
   (beads-test-action-create-issue :title "Chore issue" :type "chore"
                                   :priority 3))

  :assert
  ((beads-test-assert-issue-count ctx 5)
   (beads-test-assert-issue-created ctx :type "bug")
   (beads-test-assert-issue-created ctx :type "feature")
   (beads-test-assert-issue-created ctx :type "task")
   (beads-test-assert-issue-created ctx :type "epic")
   (beads-test-assert-issue-created ctx :type "chore")))

(beads-define-workflow-test beads-quickstart-priority-levels
  :description "Create issues with all priority levels (0-4)"
  :tags (:integration :quickstart)
  :steps
  ((beads-test-action-create-issue :title "Critical (P0)" :priority 0)
   (beads-test-action-create-issue :title "High (P1)" :priority 1)
   (beads-test-action-create-issue :title "Medium (P2)" :priority 2)
   (beads-test-action-create-issue :title "Low (P3)" :priority 3)
   (beads-test-action-create-issue :title "Backlog (P4)" :priority 4))

  :assert
  ((beads-test-assert-issue-count ctx 5)
   (beads-test-assert-issue-created ctx :title "Critical (P0)" :priority 0)
   (beads-test-assert-issue-created ctx :title "High (P1)" :priority 1)
   (beads-test-assert-issue-created ctx :title "Medium (P2)" :priority 2)
   (beads-test-assert-issue-created ctx :title "Low (P3)" :priority 3)
   (beads-test-assert-issue-created ctx :title "Backlog (P4)" :priority 4)))

(provide 'beads-quickstart-workflow-test)
;;; beads-quickstart-workflow-test.el ends here
