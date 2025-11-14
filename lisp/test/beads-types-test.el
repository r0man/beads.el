;;; beads-types-test.el --- Tests for beads-types.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-types.el including:
;; - Constants and enumerations
;; - EIEIO class creation and slot access
;; - JSON conversion functions (from-json)
;; - Alist conversion (to-alist for backwards compatibility)
;; - Validation functions
;; - Utility functions (predicates, formatters)
;; - CLI integration functions (with mocking)

;;; Code:

(require 'ert)
(require 'json)
(require 'beads-types)
(require 'beads)
(require 'beads-list)


;;; ========================================
;;; Test Fixtures and Utilities
;;; ========================================

(defvar beads-types-test--sample-issue-json
  '((id . "bd-123")
    (title . "Test Issue")
    (description . "Test description")
    (design . "Test design")
    (acceptance_criteria . "Must pass tests")
    (notes . "Some notes")
    (status . "open")
    (priority . 2)
    (issue_type . "feature")
    (assignee . "alice")
    (estimated_minutes . 120)
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T11:00:00Z")
    (closed_at)
    (external_ref . "gh-42")
    (compaction_level . 0)
    (compacted_at)
    (compacted_at_commit)
    (original_size . 0)
    (labels . ["bug" "urgent"])
    (dependencies . [])
    (comments . []))
  "Sample issue JSON for testing.")

(defvar beads-types-test--sample-dependency-json
  '((issue_id . "bd-123")
    (depends_on_id . "bd-456")
    (type . "blocks")
    (created_at . "2025-01-15T10:00:00Z")
    (created_by . "alice"))
  "Sample dependency JSON for testing.")

(defvar beads-types-test--sample-comment-json
  '((id . 42)
    (issue_id . "bd-123")
    (author . "bob")
    (text . "This is a comment")
    (created_at . "2025-01-15T12:00:00Z"))
  "Sample comment JSON for testing.")

(defvar beads-types-test--sample-event-json
  '((id . 100)
    (issue_id . "bd-123")
    (event_type . "status_changed")
    (actor . "alice")
    (old_value . "open")
    (new_value . "in_progress")
    (comment)
    (created_at . "2025-01-15T13:00:00Z"))
  "Sample event JSON for testing.")

(defvar beads-types-test--sample-blocked-issue-json
  '((id . "bd-789")
    (title . "Blocked Issue")
    (description . "This is blocked")
    (status . "blocked")
    (priority . 3)
    (issue_type . "task")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T11:00:00Z")
    (blocked_by_count . 2)
    (blocked_by . ["bd-100" "bd-101"]))
  "Sample blocked issue JSON for testing.")

(defvar beads-types-test--sample-statistics-json
  '((total_issues . 100)
    (open_issues . 30)
    (in_progress_issues . 20)
    (closed_issues . 45)
    (blocked_issues . 5)
    (ready_issues . 25)
    (epics_eligible_for_closure . 3)
    (average_lead_time_hours . 48.5))
  "Sample statistics JSON for testing.")

(defmacro beads-types-test--with-mock-command (output &rest body)
  "Execute BODY with mocked command helper functions returning OUTPUT."
  `(cl-letf (((symbol-function 'beads-command-show!)
              (lambda (&rest _args)
                ;; beads-command-show! returns a list, extract first element
                (let ((result (if (vectorp ,output) (aref ,output 0) ,output)))
                  (list (beads-issue-from-json result)))))
             ((symbol-function 'beads-command-list!)
              (lambda (&rest _args)
                (when (vectorp ,output)
                  (mapcar #'beads-issue-from-json (append ,output nil)))))
             ((symbol-function 'beads-command-blocked!)
              (lambda (&rest _args)
                (when (vectorp ,output)
                  (mapcar #'beads-blocked-issue-from-json (append ,output nil)))))
             ((symbol-function 'beads-command-ready!)
              (lambda (&rest _args)
                (when (vectorp ,output)
                  (mapcar #'beads-issue-from-json (append ,output nil))))))
     ,@body))


;;; ========================================
;;; Constants Tests
;;; ========================================

(ert-deftest beads-types-test-status-constants ()
  "Test that status constants are defined correctly."
  (should (string= beads-status-open "open"))
  (should (string= beads-status-in-progress "in_progress"))
  (should (string= beads-status-blocked "blocked"))
  (should (string= beads-status-closed "closed"))
  (should (equal beads-status-values
                 '("open" "in_progress" "blocked" "closed"))))

(ert-deftest beads-types-test-issue-type-constants ()
  "Test that issue type constants are defined correctly."
  (should (string= beads-type-bug "bug"))
  (should (string= beads-type-feature "feature"))
  (should (string= beads-type-task "task"))
  (should (string= beads-type-epic "epic"))
  (should (string= beads-type-chore "chore"))
  (should (equal beads-issue-type-values
                 '("bug" "feature" "task" "epic" "chore"))))

(ert-deftest beads-types-test-dependency-type-constants ()
  "Test that dependency type constants are defined correctly."
  (should (string= beads-dep-blocks "blocks"))
  (should (string= beads-dep-related "related"))
  (should (string= beads-dep-parent-child "parent-child"))
  (should (string= beads-dep-discovered-from "discovered-from"))
  (should (equal beads-dependency-type-values
                 '("blocks" "related" "parent-child" "discovered-from"))))

(ert-deftest beads-types-test-event-type-constants ()
  "Test that event type constants are defined correctly."
  (should (string= beads-event-created "created"))
  (should (string= beads-event-updated "updated"))
  (should (string= beads-event-status-changed "status_changed"))
  (should (string= beads-event-closed "closed"))
  (should (member beads-event-created beads-event-type-values))
  (should (member beads-event-updated beads-event-type-values)))

(ert-deftest beads-types-test-sort-policy-constants ()
  "Test that sort policy constants are defined correctly."
  (should (string= beads-sort-hybrid "hybrid"))
  (should (string= beads-sort-priority "priority"))
  (should (string= beads-sort-oldest "oldest"))
  (should (equal beads-sort-policy-values
                 '("hybrid" "priority" "oldest"))))


;;; ========================================
;;; Validation Predicate Tests
;;; ========================================

(ert-deftest beads-types-test-status-valid-p ()
  "Test `beads-status-valid-p' validation function."
  (should (beads-status-valid-p "open"))
  (should (beads-status-valid-p "in_progress"))
  (should (beads-status-valid-p "blocked"))
  (should (beads-status-valid-p "closed"))
  (should-not (beads-status-valid-p "invalid"))
  (should-not (beads-status-valid-p "")))

(ert-deftest beads-types-test-issue-type-valid-p ()
  "Test `beads-issue-type-valid-p' validation function."
  (should (beads-issue-type-valid-p "bug"))
  (should (beads-issue-type-valid-p "feature"))
  (should (beads-issue-type-valid-p "task"))
  (should (beads-issue-type-valid-p "epic"))
  (should (beads-issue-type-valid-p "chore"))
  (should-not (beads-issue-type-valid-p "invalid"))
  (should-not (beads-issue-type-valid-p "")))

(ert-deftest beads-types-test-dependency-type-valid-p ()
  "Test `beads-dependency-type-valid-p' validation function."
  (should (beads-dependency-type-valid-p "blocks"))
  (should (beads-dependency-type-valid-p "related"))
  (should (beads-dependency-type-valid-p "parent-child"))
  (should (beads-dependency-type-valid-p "discovered-from"))
  (should-not (beads-dependency-type-valid-p "invalid")))

(ert-deftest beads-types-test-sort-policy-valid-p ()
  "Test `beads-sort-policy-valid-p' validation function."
  (should (beads-sort-policy-valid-p "hybrid"))
  (should (beads-sort-policy-valid-p "priority"))
  (should (beads-sort-policy-valid-p "oldest"))
  (should (beads-sort-policy-valid-p nil))
  (should (beads-sort-policy-valid-p ""))
  (should-not (beads-sort-policy-valid-p "invalid")))


;;; ========================================
;;; EIEIO Class Creation Tests
;;; ========================================

(ert-deftest beads-types-test-issue-creation ()
  "Test creating a beads-issue object."
  (let ((issue (beads-issue
                :id "bd-1"
                :title "Test"
                :description "Desc"
                :status beads-status-open
                :priority 2
                :issue-type beads-type-task
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T10:00:00Z")))
    (should (beads-issue-p issue))
    (should (string= (oref issue id) "bd-1"))
    (should (string= (oref issue title) "Test"))
    (should (string= (oref issue status) "open"))
    (should (= (oref issue priority) 2))))

(ert-deftest beads-types-test-dependency-creation ()
  "Test creating a beads-dependency object."
  (let ((dep (beads-dependency
              :issue-id "bd-1"
              :depends-on-id "bd-2"
              :type beads-dep-blocks
              :created-at "2025-01-15T10:00:00Z"
              :created-by "alice")))
    (should (beads-dependency-p dep))
    (should (string= (oref dep issue-id) "bd-1"))
    (should (string= (oref dep depends-on-id) "bd-2"))
    (should (string= (oref dep type) "blocks"))))

(ert-deftest beads-types-test-comment-creation ()
  "Test creating a beads-comment object."
  (let ((comment (beads-comment
                  :id 42
                  :issue-id "bd-1"
                  :author "bob"
                  :text "Comment text"
                  :created-at "2025-01-15T10:00:00Z")))
    (should (beads-comment-p comment))
    (should (= (oref comment id) 42))
    (should (string= (oref comment author) "bob"))
    (should (string= (oref comment text) "Comment text"))))

(ert-deftest beads-types-test-event-creation ()
  "Test creating a beads-event object."
  (let ((event (beads-event
                :id 100
                :issue-id "bd-1"
                :event-type beads-event-status-changed
                :actor "alice"
                :old-value "open"
                :new-value "in_progress"
                :created-at "2025-01-15T10:00:00Z")))
    (should (beads-event-p event))
    (should (= (oref event id) 100))
    (should (string= (oref event event-type) "status_changed"))
    (should (string= (oref event actor) "alice"))))

(ert-deftest beads-types-test-blocked-issue-creation ()
  "Test creating a beads-blocked-issue object (inherits from beads-issue)."
  (let ((blocked (beads-blocked-issue
                  :id "bd-1"
                  :title "Blocked"
                  :status beads-status-blocked
                  :priority 2
                  :issue-type beads-type-task
                  :created-at "2025-01-15T10:00:00Z"
                  :updated-at "2025-01-15T10:00:00Z"
                  :blocked-by-count 2
                  :blocked-by '("bd-2" "bd-3"))))
    (should (beads-blocked-issue-p blocked))
    ;; Check inheritance from beads-issue using object-of-class-p
    (should (object-of-class-p blocked 'beads-issue))
    (should (= (oref blocked blocked-by-count) 2))
    (should (equal (oref blocked blocked-by) '("bd-2" "bd-3")))))

(ert-deftest beads-types-test-statistics-creation ()
  "Test creating a beads-statistics object."
  (let ((stats (beads-statistics
                :total-issues 100
                :open-issues 30
                :in-progress-issues 20
                :closed-issues 45
                :blocked-issues 5
                :ready-issues 25
                :epics-eligible-for-closure 3
                :average-lead-time 48.5)))
    (should (beads-statistics-p stats))
    (should (= (oref stats total-issues) 100))
    (should (= (oref stats open-issues) 30))
    (should (= (oref stats average-lead-time) 48.5))))


;;; ========================================
;;; JSON Conversion Tests
;;; ========================================

(ert-deftest beads-types-test-issue-from-json ()
  "Test converting JSON to beads-issue object."
  (let ((issue (beads-issue-from-json
                beads-types-test--sample-issue-json)))
    (should (beads-issue-p issue))
    (should (string= (oref issue id) "bd-123"))
    (should (string= (oref issue title) "Test Issue"))
    (should (string= (oref issue description) "Test description"))
    (should (string= (oref issue design) "Test design"))
    (should (string= (oref issue status) "open"))
    (should (= (oref issue priority) 2))
    (should (string= (oref issue issue-type) "feature"))
    (should (string= (oref issue assignee) "alice"))
    (should (= (oref issue estimated-minutes) 120))
    (should (string= (oref issue external-ref) "gh-42"))
    (should (equal (oref issue labels) '("bug" "urgent")))))

(ert-deftest beads-types-test-issue-from-json-minimal ()
  "Test converting minimal JSON to beads-issue object."
  (let* ((minimal-json '((id . "bd-1")
                         (title . "Minimal")
                         (status . "open")
                         (priority . 2)
                         (issue_type . "task")
                         (created_at . "2025-01-15T10:00:00Z")
                         (updated_at . "2025-01-15T10:00:00Z")))
         (issue (beads-issue-from-json minimal-json)))
    (should (beads-issue-p issue))
    (should (string= (oref issue id) "bd-1"))
    (should (string= (oref issue title) "Minimal"))
    (should (string= (oref issue description) ""))
    (should (null (oref issue design)))
    (should (null (oref issue assignee)))))

(ert-deftest beads-types-test-dependency-from-json ()
  "Test converting JSON to beads-dependency object."
  (let ((dep (beads-dependency-from-json
              beads-types-test--sample-dependency-json)))
    (should (beads-dependency-p dep))
    (should (string= (oref dep issue-id) "bd-123"))
    (should (string= (oref dep depends-on-id) "bd-456"))
    (should (string= (oref dep type) "blocks"))
    (should (string= (oref dep created-by) "alice"))))

(ert-deftest beads-types-test-comment-from-json ()
  "Test converting JSON to beads-comment object."
  (let ((comment (beads-comment-from-json
                  beads-types-test--sample-comment-json)))
    (should (beads-comment-p comment))
    (should (= (oref comment id) 42))
    (should (string= (oref comment issue-id) "bd-123"))
    (should (string= (oref comment author) "bob"))
    (should (string= (oref comment text) "This is a comment"))))

(ert-deftest beads-types-test-event-from-json ()
  "Test converting JSON to beads-event object."
  (let ((event (beads-event-from-json
                beads-types-test--sample-event-json)))
    (should (beads-event-p event))
    (should (= (oref event id) 100))
    (should (string= (oref event issue-id) "bd-123"))
    (should (string= (oref event event-type) "status_changed"))
    (should (string= (oref event actor) "alice"))
    (should (string= (oref event old-value) "open"))
    (should (string= (oref event new-value) "in_progress"))))

(ert-deftest beads-types-test-blocked-issue-from-json ()
  "Test converting JSON to beads-blocked-issue object."
  (let ((blocked (beads-blocked-issue-from-json
                  beads-types-test--sample-blocked-issue-json)))
    (should (beads-blocked-issue-p blocked))
    (should (string= (oref blocked id) "bd-789"))
    (should (string= (oref blocked title) "Blocked Issue"))
    (should (= (oref blocked blocked-by-count) 2))
    (should (equal (oref blocked blocked-by) '("bd-100" "bd-101")))))

(ert-deftest beads-types-test-statistics-from-json ()
  "Test converting JSON to beads-statistics object."
  (let ((stats (beads-statistics-from-json
                beads-types-test--sample-statistics-json)))
    (should (beads-statistics-p stats))
    (should (= (oref stats total-issues) 100))
    (should (= (oref stats open-issues) 30))
    (should (= (oref stats in-progress-issues) 20))
    (should (= (oref stats closed-issues) 45))
    (should (= (oref stats blocked-issues) 5))
    (should (= (oref stats ready-issues) 25))
    (should (= (oref stats epics-eligible-for-closure) 3))
    (should (= (oref stats average-lead-time) 48.5))))


;;; ========================================
;;; Alist Conversion Tests (Backwards Compatibility)
;;; ========================================

(ert-deftest beads-types-test-issue-to-alist ()
  "Test converting beads-issue object to alist."
  (let* ((issue (beads-issue
                 :id "bd-1"
                 :title "Test"
                 :description "Desc"
                 :design "Design"
                 :acceptance-criteria "AC"
                 :notes "Notes"
                 :status beads-status-open
                 :priority 2
                 :issue-type beads-type-task
                 :assignee "alice"
                 :estimated-minutes 60
                 :created-at "2025-01-15T10:00:00Z"
                 :updated-at "2025-01-15T11:00:00Z"
                 :closed-at nil
                 :external-ref "EXT-1"))
         (alist (beads-issue-to-alist issue)))
    (should (listp alist))
    (should (string= (alist-get 'id alist) "bd-1"))
    (should (string= (alist-get 'title alist) "Test"))
    (should (string= (alist-get 'description alist) "Desc"))
    (should (string= (alist-get 'status alist) "open"))
    (should (= (alist-get 'priority alist) 2))
    (should (string= (alist-get 'issue-type alist) "task"))
    (should (string= (alist-get 'assignee alist) "alice"))
    (should (= (alist-get 'estimated-minutes alist) 60))
    (should (string= (alist-get 'external-ref alist) "EXT-1"))))


;;; ========================================
;;; Validation Tests
;;; ========================================

(ert-deftest beads-types-test-validate-valid-issue ()
  "Test validation of a valid issue."
  (let ((issue (beads-issue
                :id "bd-1"
                :title "Valid Issue"
                :status beads-status-open
                :priority 2
                :issue-type beads-type-task
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T10:00:00Z")))
    (should (null (beads-validate issue)))))

(ert-deftest beads-types-test-validate-empty-title ()
  "Test validation fails for empty title."
  (let ((issue (beads-issue
                :id "bd-1"
                :title ""
                :status beads-status-open
                :priority 2
                :issue-type beads-type-task
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T10:00:00Z")))
    (should (string-match-p "title is required" (beads-validate issue)))))

(ert-deftest beads-types-test-validate-long-title ()
  "Test validation fails for title > 500 characters."
  (let ((issue (beads-issue
                :id "bd-1"
                :title (make-string 501 ?x)
                :status beads-status-open
                :priority 2
                :issue-type beads-type-task
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T10:00:00Z")))
    (should (string-match-p "title must be 500 characters or less"
                            (beads-validate issue)))))

(ert-deftest beads-types-test-validate-invalid-priority ()
  "Test validation fails for invalid priority."
  (let ((issue (beads-issue
                :id "bd-1"
                :title "Test"
                :status beads-status-open
                :priority 5  ; Invalid, should be 0-4
                :issue-type beads-type-task
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T10:00:00Z")))
    (should (string-match-p "priority must be between 0 and 4"
                            (beads-validate issue)))))

(ert-deftest beads-types-test-validate-invalid-status ()
  "Test validation fails for invalid status."
  (let ((issue (beads-issue
                :id "bd-1"
                :title "Test"
                :status "invalid-status"
                :priority 2
                :issue-type beads-type-task
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T10:00:00Z")))
    (should (string-match-p "invalid status" (beads-validate issue)))))

(ert-deftest beads-types-test-validate-closed-without-timestamp ()
  "Test validation fails for closed issue without closed_at."
  (let ((issue (beads-issue
                :id "bd-1"
                :title "Test"
                :status beads-status-closed
                :priority 2
                :issue-type beads-type-task
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T10:00:00Z"
                :closed-at nil)))
    (should (string-match-p "closed issues must have closed_at timestamp"
                            (beads-validate issue)))))

(ert-deftest beads-types-test-validate-open-with-closed-timestamp ()
  "Test validation fails for open issue with closed_at."
  (let ((issue (beads-issue
                :id "bd-1"
                :title "Test"
                :status beads-status-open
                :priority 2
                :issue-type beads-type-task
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T10:00:00Z"
                :closed-at "2025-01-15T12:00:00Z")))
    (should (string-match-p "non-closed issues cannot have closed_at"
                            (beads-validate issue)))))

(ert-deftest beads-types-test-validate-negative-estimated-minutes ()
  "Test validation fails for negative estimated minutes."
  (let ((issue (beads-issue
                :id "bd-1"
                :title "Test"
                :status beads-status-open
                :priority 2
                :issue-type beads-type-task
                :estimated-minutes -10
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T10:00:00Z")))
    (should (string-match-p "estimated_minutes cannot be negative"
                            (beads-validate issue)))))


;;; ========================================
;;; Utility Function Tests
;;; ========================================

(ert-deftest beads-types-test-issue-closed-p ()
  "Test `beads-issue-closed-p' predicate."
  (let ((closed (beads-issue
                 :id "bd-1"
                 :title "Closed"
                 :status beads-status-closed
                 :priority 2
                 :issue-type beads-type-task
                 :created-at "2025-01-15T10:00:00Z"
                 :updated-at "2025-01-15T10:00:00Z"
                 :closed-at "2025-01-15T12:00:00Z"))
        (open (beads-issue
               :id "bd-2"
               :title "Open"
               :status beads-status-open
               :priority 2
               :issue-type beads-type-task
               :created-at "2025-01-15T10:00:00Z"
               :updated-at "2025-01-15T10:00:00Z")))
    (should (beads-issue-closed-p closed))
    (should-not (beads-issue-closed-p open))))

(ert-deftest beads-types-test-issue-open-p ()
  "Test `beads-issue-open-p' predicate."
  (let ((open (beads-issue
               :id "bd-1"
               :title "Open"
               :status beads-status-open
               :priority 2
               :issue-type beads-type-task
               :created-at "2025-01-15T10:00:00Z"
               :updated-at "2025-01-15T10:00:00Z"))
        (closed (beads-issue
                 :id "bd-2"
                 :title "Closed"
                 :status beads-status-closed
                 :priority 2
                 :issue-type beads-type-task
                 :created-at "2025-01-15T10:00:00Z"
                 :updated-at "2025-01-15T10:00:00Z"
                 :closed-at "2025-01-15T12:00:00Z")))
    (should (beads-issue-open-p open))
    (should-not (beads-issue-open-p closed))))

(ert-deftest beads-types-test-issue-in-progress-p ()
  "Test `beads-issue-in-progress-p' predicate."
  (let ((in-progress (beads-issue
                      :id "bd-1"
                      :title "In Progress"
                      :status beads-status-in-progress
                      :priority 2
                      :issue-type beads-type-task
                      :created-at "2025-01-15T10:00:00Z"
                      :updated-at "2025-01-15T10:00:00Z"))
        (open (beads-issue
               :id "bd-2"
               :title "Open"
               :status beads-status-open
               :priority 2
               :issue-type beads-type-task
               :created-at "2025-01-15T10:00:00Z"
               :updated-at "2025-01-15T10:00:00Z")))
    (should (beads-issue-in-progress-p in-progress))
    (should-not (beads-issue-in-progress-p open))))

(ert-deftest beads-types-test-issue-blocked-p ()
  "Test `beads-issue-blocked-p' predicate."
  (let ((blocked (beads-issue
                  :id "bd-1"
                  :title "Blocked"
                  :status beads-status-blocked
                  :priority 2
                  :issue-type beads-type-task
                  :created-at "2025-01-15T10:00:00Z"
                  :updated-at "2025-01-15T10:00:00Z"))
        (open (beads-issue
               :id "bd-2"
               :title "Open"
               :status beads-status-open
               :priority 2
               :issue-type beads-type-task
               :created-at "2025-01-15T10:00:00Z"
               :updated-at "2025-01-15T10:00:00Z")))
    (should (beads-issue-blocked-p blocked))
    (should-not (beads-issue-blocked-p open))))

(ert-deftest beads-types-test-issue-epic-p ()
  "Test `beads-issue-epic-p' predicate."
  (let ((epic (beads-issue
               :id "bd-1"
               :title "Epic"
               :status beads-status-open
               :priority 2
               :issue-type beads-type-epic
               :created-at "2025-01-15T10:00:00Z"
               :updated-at "2025-01-15T10:00:00Z"))
        (task (beads-issue
               :id "bd-2"
               :title "Task"
               :status beads-status-open
               :priority 2
               :issue-type beads-type-task
               :created-at "2025-01-15T10:00:00Z"
               :updated-at "2025-01-15T10:00:00Z")))
    (should (beads-issue-epic-p epic))
    (should-not (beads-issue-epic-p task))))

(ert-deftest beads-types-test-issue-has-label-p ()
  "Test `beads-issue-has-label-p' predicate."
  (let ((issue (beads-issue
                :id "bd-1"
                :title "Test"
                :status beads-status-open
                :priority 2
                :issue-type beads-type-task
                :created-at "2025-01-15T10:00:00Z"
                :updated-at "2025-01-15T10:00:00Z"
                :labels '("bug" "urgent"))))
    (should (beads-issue-has-label-p issue "bug"))
    (should (beads-issue-has-label-p issue "urgent"))
    (should-not (beads-issue-has-label-p issue "feature"))))

(ert-deftest beads-types-test-format-timestamp ()
  "Test `beads-format-timestamp' formatter."
  (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
                          (beads-format-timestamp "2025-01-15T10:00:00Z")))
  (should (null (beads-format-timestamp nil)))
  (should (null (beads-format-timestamp ""))))

(ert-deftest beads-types-test-format-priority ()
  "Test `beads-format-priority' formatter."
  (should (string= (beads-format-priority 0) "P0 (lowest)"))
  (should (string= (beads-format-priority 1) "P1"))
  (should (string= (beads-format-priority 2) "P2"))
  (should (string= (beads-format-priority 3) "P3"))
  (should (string= (beads-format-priority 4) "P4 (highest)"))
  (should (string= (beads-format-priority 5) "P5")))


;;; ========================================
;;; CLI Integration Tests (with mocking)
;;; ========================================

(ert-deftest beads-types-test-issue-read ()
  "Test `beads-issue-read' CLI integration."
  (beads-types-test--with-mock-command
   beads-types-test--sample-issue-json
   (let ((issue (beads-issue-read "bd-123")))
     (should (beads-issue-p issue))
     (should (string= (oref issue id) "bd-123"))
     (should (string= (oref issue title) "Test Issue")))))

(ert-deftest beads-types-test-issue-list ()
  "Test `beads-issue-list' CLI integration."
  (beads-types-test--with-mock-command
   (vector beads-types-test--sample-issue-json
           beads-types-test--sample-issue-json)
   (let ((issues (beads-issue-list)))
     (should (= (length issues) 2))
     (should (beads-issue-p (car issues)))
     (should (string= (oref (car issues) id) "bd-123")))))

(ert-deftest beads-types-test-issue-list-with-status ()
  "Test `beads-issue-list' with status filter."
  (beads-types-test--with-mock-command
   (vector beads-types-test--sample-issue-json)
   (let ((issues (beads-issue-list "open")))
     (should (= (length issues) 1))
     (should (beads-issue-p (car issues))))))

(ert-deftest beads-types-test-blocked-issue-list ()
  "Test `beads-blocked-issue-list' CLI integration."
  (beads-types-test--with-mock-command
   (vector beads-types-test--sample-blocked-issue-json)
   (let ((issues (beads-blocked-issue-list)))
     (should (= (length issues) 1))
     (should (beads-blocked-issue-p (car issues)))
     (should (string= (oref (car issues) id) "bd-789"))
     (should (= (oref (car issues) blocked-by-count) 2)))))

(ert-deftest beads-types-test-issue-ready ()
  "Test `beads-issue-ready' CLI integration."
  (beads-types-test--with-mock-command
   (vector beads-types-test--sample-issue-json)
   (let ((issues (beads-issue-ready)))
     (should (= (length issues) 1))
     (should (beads-issue-p (car issues))))))

(ert-deftest beads-types-test-issue-ready-with-limit ()
  "Test `beads-issue-ready' with limit."
  (beads-types-test--with-mock-command
   (vector beads-types-test--sample-issue-json)
   (let ((issues (beads-issue-ready 5)))
     (should (= (length issues) 1))
     (should (beads-issue-p (car issues))))))


;;; ========================================
;;; Integration Tests (with real bd CLI)
;;; ========================================

(ert-deftest beads-types-test-integration-issue-read ()
  "Integration test: read an issue by ID using real bd CLI."
  :tags '(:integration)
  (let* ((project-dir (make-temp-file "beads-test-" t))
         (default-directory project-dir))
    (unwind-protect
        (progn
          ;; Initialize beads project
          (call-process "bd" nil nil nil "init" "--prefix" "test")

          ;; Create a test issue
          (with-temp-buffer
            (call-process "bd" nil t nil "create"
                          "Integration Test Issue"
                          "--description" "Test description"
                          "--priority" "2"
                          "--type" "task"
                          "--json")
            (goto-char (point-min))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (result (json-read))
                   (issue-id (alist-get 'id result)))

              ;; Test: Read the issue using beads-issue-read
              (let ((issue (beads-issue-read issue-id)))
                (should (beads-issue-p issue))
                (should (string= (oref issue id) issue-id))
                (should (string= (oref issue title) "Integration Test Issue"))
                (should (string= (oref issue description) "Test description"))
                (should (= (oref issue priority) 2))
                (should (string= (oref issue issue-type) "task"))
                (should (string= (oref issue status) "open"))
                (should (null (oref issue closed-at)))))))
      ;; Cleanup
      (when (file-directory-p project-dir)
        (delete-directory project-dir t)))))

(ert-deftest beads-types-test-integration-issue-list-by-status ()
  "Integration test: list issues filtered by status using real bd CLI."
  :tags '(:integration)
  (let* ((project-dir (make-temp-file "beads-test-" t))
         (default-directory project-dir))
    (unwind-protect
        (progn
          ;; Initialize beads project
          (call-process "bd" nil nil nil "init" "--prefix" "test")

          ;; Create issues with different statuses
          (with-temp-buffer
            (call-process "bd" nil t nil "create" "Open Issue"
                          "--json")
            (goto-char (point-min))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (result (json-read))
                   (issue-id (alist-get 'id result)))
              ;; Update one to in_progress
              (call-process "bd" nil nil nil "update" issue-id
                            "--status" "in_progress")))

          (call-process "bd" nil nil nil "create" "Another Open Issue")

          (with-temp-buffer
            (call-process "bd" nil t nil "create" "To Be Closed"
                          "--json")
            (goto-char (point-min))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (result (json-read))
                   (issue-id (alist-get 'id result)))
              ;; Close this issue
              (call-process "bd" nil nil nil "close" issue-id
                            "--reason" "Done")))

          ;; Test: List only open issues
          (let ((open-issues (beads-issue-list "open")))
            (should (>= (length open-issues) 1))
            (should (cl-every (lambda (i)
                                (string= (oref i status) "open"))
                              open-issues)))

          ;; Test: List only in_progress issues
          (let ((in-progress-issues (beads-issue-list "in_progress")))
            (should (= (length in-progress-issues) 1))
            (should (cl-every (lambda (i)
                                (string= (oref i status) "in_progress"))
                              in-progress-issues)))

          ;; Test: List only closed issues
          (let ((closed-issues (beads-issue-list "closed")))
            (should (= (length closed-issues) 1))
            (should (cl-every (lambda (i)
                                (string= (oref i status) "closed"))
                              closed-issues))
            (should (cl-every (lambda (i)
                                (not (null (oref i closed-at))))
                              closed-issues))))
      ;; Cleanup
      (when (file-directory-p project-dir)
        (delete-directory project-dir t)))))

(ert-deftest beads-types-test-integration-issue-ready ()
  "Integration test: get ready work using real bd CLI."
  :tags '(:integration)
  (let* ((project-dir (make-temp-file "beads-test-" t))
         (default-directory project-dir))
    (unwind-protect
        (progn
          ;; Initialize beads project
          (call-process "bd" nil nil nil "init" "--prefix" "test")

          ;; Create a ready issue (open with no blockers)
          (call-process "bd" nil nil nil "create" "Ready Issue 1"
                        "--priority" "3")
          (call-process "bd" nil nil nil "create" "Ready Issue 2"
                        "--priority" "2")

          ;; Test: Get ready work
          (let ((ready-issues (beads-issue-ready)))
            (should (>= (length ready-issues) 2))
            (should (cl-every #'beads-issue-p ready-issues))

            ;; Verify all are open
            (should (cl-every (lambda (i)
                                (string= (oref i status) "open"))
                              ready-issues)))

          ;; Test: Get ready work with limit
          (let ((limited-issues (beads-issue-ready 1)))
            (should (= (length limited-issues) 1))
            (should (beads-issue-p (car limited-issues)))))
      ;; Cleanup
      (when (file-directory-p project-dir)
        (delete-directory project-dir t)))))

(ert-deftest beads-types-test-integration-blocked-issue-list ()
  "Integration test: list blocked issues using real bd CLI."
  :tags '(:integration)
  (let* ((project-dir (make-temp-file "beads-test-" t))
         (default-directory project-dir))
    (unwind-protect
        (progn
          ;; Initialize beads project
          (call-process "bd" nil nil nil "init" "--prefix" "test")

          ;; Create two issues and make one depend on the other
          (with-temp-buffer
            (call-process "bd" nil t nil "create" "Blocking Issue"
                          "--json")
            (goto-char (point-min))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (result1 (json-read))
                   (blocking-id (alist-get 'id result1)))

              (erase-buffer)
              (call-process "bd" nil t nil "create" "Blocked Issue"
                            "--json")
              (goto-char (point-min))
              (let* ((result2 (json-read))
                     (blocked-id (alist-get 'id result2)))

                ;; Add blocking dependency
                (call-process "bd" nil nil nil "dep" "add"
                              blocked-id blocking-id "--type" "blocks")

                ;; Test: List blocked issues
                (let ((blocked-issues (beads-blocked-issue-list)))
                  (should (>= (length blocked-issues) 1))
                  (should (cl-every #'beads-blocked-issue-p blocked-issues))

                  ;; Find our blocked issue
                  (let ((our-issue (cl-find-if
                                    (lambda (i)
                                      (string= (oref i id) blocked-id))
                                    blocked-issues)))
                    (should our-issue)
                    (should (> (oref our-issue blocked-by-count) 0))
                    (should (member blocking-id
                                    (oref our-issue blocked-by)))))))))
      ;; Cleanup
      (when (file-directory-p project-dir)
        (delete-directory project-dir t)))))


(provide 'beads-types-test)
;;; beads-types-test.el ends here
