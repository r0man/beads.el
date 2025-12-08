;;; beads-workflow-tests.el --- Example workflow tests using the DSL -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: lisp, tools

;;; Commentary:

;; This file demonstrates the EIEIO-based test DSL for beads.el.
;; It contains example workflow tests that exercise various user
;; interaction patterns.

;;; Code:

(require 'ert)
(require 'beads-test-dsl)
(require 'beads-test)
(require 'beads)
(require 'beads-command)

;;; ============================================================
;;; Basic Issue Creation Workflows
;;; ============================================================

(beads-define-workflow-test beads-workflow-create-minimal-issue
  :description "Create an issue with only required fields"
  :tags (:integration :workflow)
  :steps
  ((beads-test-action-create-issue
    :title "Minimal test issue"))
  :assert
  ((beads-test-assert-issue-created ctx :title "Minimal test issue")
   (beads-test-assert-completion-cache-invalidated ctx)))

(beads-define-workflow-test beads-workflow-create-bug-issue
  :description "Create a bug issue with type and priority"
  :tags (:integration :workflow)
  :steps
  ((beads-test-action-create-issue
    :title "Fix login validation"
    :type "bug"
    :priority 1))
  :assert
  ((beads-test-assert-issue-created ctx
     :title "Fix login validation"
     :type "bug"
     :priority 1)))

(beads-define-workflow-test beads-workflow-create-feature-with-description
  :description "Create a feature issue with full description"
  :tags (:integration :workflow)
  :steps
  ((beads-test-action-create-issue
    :title "Add dark mode support"
    :type "feature"
    :priority 2
    :description "Implement dark mode theme switching"))
  :assert
  ((beads-test-assert-issue-created ctx
     :title "Add dark mode support"
     :type "feature")))

;;; ============================================================
;;; Transient Interaction Workflows
;;; ============================================================

(beads-define-workflow-test beads-workflow-transient-step-by-step
  :description "Create issue using step-by-step transient interaction"
  :tags (:integration :workflow)
  :steps
  ((beads-test-action-open-transient :command 'beads-create)
   (beads-test-action-set-infix :key "--title=" :value "Step by step issue")
   (beads-test-action-set-infix :key "--type=" :value "task")
   (beads-test-action-set-infix :key "--priority=" :value 2)
   (beads-test-action-confirm :response nil)  ; Don't show after
   (beads-test-action-execute-suffix :suffix-key "x"))
  :assert
  ((beads-test-assert-issue-created ctx :title "Step by step issue")))

(beads-define-workflow-test beads-workflow-preview-before-create
  :description "Preview command before creating issue"
  :tags (:integration :workflow)
  :steps
  ((beads-test-action-open-transient :command 'beads-create)
   (beads-test-action-set-infix :key "--title=" :value "Preview test")
   (beads-test-action-set-infix :key "--type=" :value "bug")
   (beads-test-action-preview)
   (beads-test-action-confirm :response nil)
   (beads-test-action-execute-suffix))
  :assert
  ((beads-test-assert-issue-created ctx :title "Preview test")))

;;; ============================================================
;;; Update Workflows
;;; ============================================================

(beads-define-workflow-test beads-workflow-update-status
  :description "Update an issue's status"
  :tags (:integration :workflow)
  :setup
  (lambda (ctx)
    ;; Create initial issue
    (let ((issue (beads-command-create! :title "Update test" :issue-type "task")))
      ;; Store for later reference
      (push (oref issue id) (oref ctx created-issues))))
  :steps
  ((beads-test-action-update-issue
    :issue-id (lambda (ctx) (car (oref ctx created-issues)))
    :changes '((status . "in_progress"))))
  :assert
  ((let ((issue-id (car (oref ctx created-issues))))
     (beads-test-assert-issue-has ctx issue-id :status "in_progress"))))

(beads-define-workflow-test beads-workflow-update-priority
  :description "Update an issue's priority"
  :tags (:integration :workflow)
  :setup
  (lambda (ctx)
    (let ((issue (beads-command-create! :title "Priority test" :priority 3)))
      (push (oref issue id) (oref ctx created-issues))))
  :steps
  ((beads-test-action-update-issue
    :issue-id (lambda (ctx) (car (oref ctx created-issues)))
    :changes '((priority . 0))))
  :assert
  ((let ((issue-id (car (oref ctx created-issues))))
     (beads-test-assert-issue-has ctx issue-id :priority 0))))

;;; ============================================================
;;; Close Workflows
;;; ============================================================

(beads-define-workflow-test beads-workflow-close-issue
  :description "Close an issue with a reason"
  :tags (:integration :workflow)
  :setup
  (lambda (ctx)
    (let ((issue (beads-command-create! :title "Close test")))
      (push (oref issue id) (oref ctx created-issues))))
  :steps
  ((beads-test-action-close-issue
    :issue-id (lambda (ctx) (car (oref ctx created-issues)))
    :reason "Work completed"))
  :assert
  ((let ((issue-id (car (oref ctx created-issues))))
     (beads-test-assert-issue-has ctx issue-id :status "closed"))))

;;; ============================================================
;;; Multi-Step Workflows
;;; ============================================================

(beads-define-workflow-test beads-workflow-create-update-close
  :description "Full lifecycle: create, update, and close an issue"
  :tags (:integration :workflow :lifecycle)
  :steps
  ((beads-test-action-create-issue
    :title "Lifecycle test"
    :type "task"
    :priority 2)
   ;; Get the created issue ID and update it
   (beads-test-action-update-issue
    :issue-id (lambda (_ctx)
                ;; Find the issue we just created
                (let ((issues (beads-command-list!)))
                  (oref (seq-find (lambda (i)
                                    (equal (oref i title) "Lifecycle test"))
                                  issues)
                        id)))
    :changes '((status . "in_progress") (priority . 1)))
   ;; Now close it
   (beads-test-action-close-issue
    :issue-id (lambda (_ctx)
                (let ((issues (beads-command-list!)))
                  (oref (seq-find (lambda (i)
                                    (equal (oref i title) "Lifecycle test"))
                                  issues)
                        id)))
    :reason "Lifecycle complete"))
  :assert
  ((beads-test-assert-issue-created ctx :title "Lifecycle test")
   (progn
     (ignore ctx)  ; Available for context-aware assertions
     (let ((issues (beads-command-list!)))
       (let ((issue (seq-find (lambda (i)
                                (equal (oref i title) "Lifecycle test"))
                              issues)))
         (should (equal (oref issue status) "closed")))))))

;;; ============================================================
;;; Error Handling Workflows
;;; ============================================================

(beads-define-workflow-test beads-workflow-validation-error-empty-title
  :description "Verify validation error for empty title"
  :tags (:integration :workflow :error)
  :steps
  ((beads-test-action-open-transient :command 'beads-create)
   (beads-test-action-set-infix :key "--title=" :value "")
   (beads-test-action-execute-suffix :expect-error 'user-error))
  :assert
  ((beads-test-assert-error-signaled ctx 'user-error)))

(beads-define-workflow-test beads-workflow-validation-error-invalid-type
  :description "Verify validation error for invalid issue type"
  :tags (:integration :workflow :error)
  :steps
  ((beads-test-action-open-transient :command 'beads-create)
   (beads-test-action-set-infix :key "--title=" :value "Test")
   (beads-test-action-set-infix :key "--type=" :value "invalid-type")
   (beads-test-action-execute-suffix :expect-error 'user-error))
  :assert
  ((beads-test-assert-error-signaled ctx 'user-error)))

;;; ============================================================
;;; Multiple Issue Workflows
;;; ============================================================

(beads-define-workflow-test beads-workflow-create-multiple-issues
  :description "Create multiple issues in sequence"
  :tags (:integration :workflow)
  :steps
  ((beads-test-action-create-issue :title "First issue" :priority 1)
   (beads-test-action-create-issue :title "Second issue" :priority 2)
   (beads-test-action-create-issue :title "Third issue" :priority 3))
  :assert
  ((beads-test-assert-issue-count ctx 3)
   (beads-test-assert-issue-created ctx :title "First issue")
   (beads-test-assert-issue-created ctx :title "Second issue")
   (beads-test-assert-issue-created ctx :title "Third issue")))

;;; ============================================================
;;; Reset Workflow
;;; ============================================================

(beads-define-workflow-test beads-workflow-reset-transient
  :description "Reset transient state and start over"
  :tags (:integration :workflow)
  :steps
  ((beads-test-action-open-transient :command 'beads-create)
   (beads-test-action-set-infix :key "--title=" :value "Will be reset")
   (beads-test-action-set-infix :key "--type=" :value "bug")
   (beads-test-action-reset-transient :confirm t)
   ;; After reset, set new values
   (beads-test-action-set-infix :key "--title=" :value "After reset")
   (beads-test-action-confirm :response nil)
   (beads-test-action-execute-suffix))
  :assert
  ((beads-test-assert-issue-created ctx :title "After reset")))

(provide 'beads-workflow-tests)
;;; beads-workflow-tests.el ends here
