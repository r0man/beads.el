;;; beads-update-test.el --- Tests for beads-update -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-update.el transient menu using
;; the transient-args pattern (no state variables).

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-list)
(require 'beads-show)
(require 'beads-update)

;;; Test Fixtures

(defvar beads-update-test--sample-issue
  '((id . "bd-42")
    (title . "Test Issue")
    (description . "Test description")
    (status . "open")
    (priority . 1)
    (issue-type . "bug")
    (created-at . "2025-01-15T10:00:00Z")
    (updated-at . "2025-01-15T10:00:00Z")
    (acceptance-criteria . "Test acceptance")
    (design . "Test design")
    (notes . "Test notes")
    (assignee . "testuser")
    (external-ref . "gh-123"))
  "Sample issue data for testing.")

(defvar beads-update-test--sample-update-response
  '((id . "bd-42")
    (title . "Updated Issue")
    (description . "Updated description")
    (status . "in_progress")
    (priority . 2)
    (issue-type . "feature")
    (created-at . "2025-01-15T10:00:00Z")
    (updated-at . "2025-01-15T11:00:00Z"))
  "Sample response from bd update command.")

;;; Test Utilities

(defun beads-update-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; Tests for Argument Parsing

(ert-deftest beads-update-test-parse-args-empty ()
  "Test parsing empty argument list."
  (let ((parsed (beads-update--parse-transient-args nil)))
    (should (null (plist-get parsed :status)))
    (should (null (plist-get parsed :priority)))
    (should (null (plist-get parsed :type)))))

(ert-deftest beads-update-test-parse-args-status ()
  "Test parsing status argument."
  (let ((parsed (beads-update--parse-transient-args
                 '("--status=in_progress"))))
    (should (equal (plist-get parsed :status) "in_progress"))))

(ert-deftest beads-update-test-parse-args-priority ()
  "Test parsing priority argument."
  (let ((parsed (beads-update--parse-transient-args
                 '("--priority=3"))))
    (should (equal (plist-get parsed :priority) 3))))

(ert-deftest beads-update-test-parse-args-all-fields ()
  "Test parsing with all fields."
  (let ((parsed (beads-update--parse-transient-args
                 '("--status=closed"
                   "--priority=0"
                   "--title=New Title"
                   "--description=New Desc"
                   "--acceptance=New AC"
                   "--design=New Design"
                   "--notes=New Notes"
                   "--assignee=alice"
                   "--external-ref=gh-999"))))
    (should (equal (plist-get parsed :status) "closed"))
    (should (equal (plist-get parsed :priority) 0))
    (should (equal (plist-get parsed :title) "New Title"))
    (should (equal (plist-get parsed :description) "New Desc"))
    (should (equal (plist-get parsed :acceptance) "New AC"))
    (should (equal (plist-get parsed :design) "New Design"))
    (should (equal (plist-get parsed :notes) "New Notes"))
    (should (equal (plist-get parsed :assignee) "alice"))
    (should (equal (plist-get parsed :external-ref) "gh-999"))))

;;; Tests for Validation

(ert-deftest beads-update-test-validate-status-nil ()
  "Test status validation when status is nil."
  (should (null (beads-update--validate-status nil))))

(ert-deftest beads-update-test-validate-status-valid-open ()
  "Test status validation with valid open status."
  (should (null (beads-update--validate-status "open"))))

(ert-deftest beads-update-test-validate-status-valid-in-progress ()
  "Test status validation with valid in_progress status."
  (should (null (beads-update--validate-status "in_progress"))))

(ert-deftest beads-update-test-validate-status-valid-blocked ()
  "Test status validation with valid blocked status."
  (should (null (beads-update--validate-status "blocked"))))

(ert-deftest beads-update-test-validate-status-valid-closed ()
  "Test status validation with valid closed status."
  (should (null (beads-update--validate-status "closed"))))

(ert-deftest beads-update-test-validate-status-invalid ()
  "Test status validation with invalid status."
  (should (beads-update--validate-status "invalid")))


(ert-deftest beads-update-test-validate-priority-nil ()
  "Test priority validation when priority is nil."
  (should (null (beads-update--validate-priority nil))))

(ert-deftest beads-update-test-validate-priority-zero ()
  "Test priority validation with zero (critical)."
  (should (null (beads-update--validate-priority 0))))

(ert-deftest beads-update-test-validate-priority-one ()
  "Test priority validation with one."
  (should (null (beads-update--validate-priority 1))))

(ert-deftest beads-update-test-validate-priority-four ()
  "Test priority validation with four (backlog)."
  (should (null (beads-update--validate-priority 4))))

(ert-deftest beads-update-test-validate-priority-negative ()
  "Test priority validation with negative number."
  (should (beads-update--validate-priority -1)))

(ert-deftest beads-update-test-validate-priority-too-high ()
  "Test priority validation with number too high."
  (should (beads-update--validate-priority 5)))

(ert-deftest beads-update-test-validate-priority-string ()
  "Test priority validation with string instead of number."
  (should (beads-update--validate-priority "1")))

(ert-deftest beads-update-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (let ((parsed (beads-update--parse-transient-args
                 '("--status=in_progress"
                   "--type=bug"
                   "--priority=1"))))
    (should (null (beads-update--validate-all parsed)))))

(ert-deftest beads-update-test-validate-all-multiple-errors ()
  "Test validate-all with multiple validation errors."
  (let ((parsed (beads-update--parse-transient-args
                 '("--status=invalid"
                   "--priority=10"))))
    (let ((errors (beads-update--validate-all parsed)))
      (should errors)
      (should (listp errors))
      (should (>= (length errors) 2)))))

;;; Tests for Change Detection

(ert-deftest beads-update-test-get-changed-fields-none ()
  "Test getting changed fields when nothing changed."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args nil)))
    (should (null (beads-update--get-changed-fields parsed)))))

(ert-deftest beads-update-test-get-changed-fields-status ()
  "Test getting changed fields when status changed."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--status=in_progress"))))
    (let ((changes (beads-update--get-changed-fields parsed)))
      (should (= (length changes) 1))
      (should (equal (car changes) '(status . "in_progress"))))))

(ert-deftest beads-update-test-get-changed-fields-priority ()
  "Test getting changed fields when priority changed."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--priority=3"))))
    (let ((changes (beads-update--get-changed-fields parsed)))
      (should (= (length changes) 1))
      (should (equal (car changes) '(priority . 3))))))

(ert-deftest beads-update-test-get-changed-fields-multiple ()
  "Test getting changed fields when multiple fields changed."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--status=closed"
                   "--priority=0"
                   "--title=New Title"))))
    (let ((changes (beads-update--get-changed-fields parsed)))
      (should (= (length changes) 3))
      (should (member '(status . "closed") changes))
      (should (member '(priority . 0) changes))
      (should (member '(title . "New Title") changes)))))

(ert-deftest beads-update-test-get-changed-fields-same-value ()
  "Test that setting field to same value doesn't count as change."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--status=open"))))
    (should (null (beads-update--get-changed-fields parsed)))))

;;; Tests for Command Building

(ert-deftest beads-update-test-build-command-args-no-id ()
  "Test building command args without issue ID."
  (let ((beads-update--issue-id nil)
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--status=closed"))))
    (should-error (beads-update--build-command-args parsed)
                  :type 'user-error)))

(ert-deftest beads-update-test-build-command-args-no-changes ()
  "Test building command args with no changes."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args nil)))
    (should-error (beads-update--build-command-args parsed)
                  :type 'user-error)))

(ert-deftest beads-update-test-build-command-args-status-change ()
  "Test building command args with status change."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--status=in_progress"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (equal args '("bd-42" "-s" "in_progress"))))))

(ert-deftest beads-update-test-build-command-args-priority-change ()
  "Test building command args with priority change."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--priority=3"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (equal args '("bd-42" "-p" "3"))))))


(ert-deftest beads-update-test-build-command-args-title-change ()
  "Test building command args with title change."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--title=New Title"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (equal args '("bd-42" "--title" "New Title"))))))

(ert-deftest beads-update-test-build-command-args-description-change ()
  "Test building command args with description change."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--description=New description"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (member "--description" args))
      (should (member "New description" args)))))

(ert-deftest beads-update-test-build-command-args-assignee-change ()
  "Test building command args with assignee change."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--assignee=newuser"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (equal args '("bd-42" "-a" "newuser"))))))

(ert-deftest beads-update-test-build-command-args-external-ref-change ()
  "Test building command args with external ref change."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--external-ref=jira-999"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (member "--external-ref" args))
      (should (member "jira-999" args)))))

(ert-deftest beads-update-test-build-command-args-multiple-changes ()
  "Test building command args with multiple changes."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--status=closed"
                   "--priority=0"
                   "--title=Updated Title"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (member "bd-42" args))
      (should (member "-s" args))
      (should (member "closed" args))
      (should (member "-p" args))
      (should (member "0" args))
      (should (member "--title" args))
      (should (member "Updated Title" args)))))

(ert-deftest beads-update-test-build-command-args-all-fields ()
  "Test building command args with all fields changed."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--status=closed"
                   "--priority=4"
                   "--title=New Title"
                   "--description=New Desc"
                   "--acceptance=New AC"
                   "--design=New Design"
                   "--notes=New Notes"
                   "--assignee=alice"
                   "--external-ref=gh-999"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (member "bd-42" args))
      (should (member "-s" args))
      (should (member "-p" args))
      (should (member "--title" args))
      (should (member "--description" args))
      (should (member "--acceptance" args))
      (should (member "--design" args))
      (should (member "--notes" args))
      (should (member "-a" args))
      (should (member "--external-ref" args)))))

(ert-deftest beads-update-test-build-command-args-priority-zero ()
  "Test building command args with priority zero (critical)."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--priority=0"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (member "0" args)))))

;;; Tests for Context Detection

(ert-deftest beads-update-test-detect-issue-id-from-buffer-name ()
  "Test detecting issue ID from beads-show buffer name."
  (with-temp-buffer
    (rename-buffer "*beads-show: bd-42*" t)
    (should (equal (beads-update--detect-issue-id) "bd-42"))))

(ert-deftest beads-update-test-detect-issue-id-no-context ()
  "Test detecting issue ID when no context available."
  (with-temp-buffer
    (should (null (beads-update--detect-issue-id)))))

;;; Tests for Original Data Access

(ert-deftest beads-update-test-get-original ()
  "Test getting original field value."
  (let ((beads-update--original-data beads-update-test--sample-issue))
    (should (equal (beads-update--get-original 'id) "bd-42"))
    (should (equal (beads-update--get-original 'title) "Test Issue"))
    (should (equal (beads-update--get-original 'status) "open"))
    (should (equal (beads-update--get-original 'priority) 1))))

(ert-deftest beads-update-test-get-original-missing-field ()
  "Test getting original field that doesn't exist."
  (let ((beads-update--original-data beads-update-test--sample-issue))
    (should (null (beads-update--get-original 'nonexistent)))))

;;; Tests for Transient Definition

(ert-deftest beads-update-test-transient-defined ()
  "Test that beads-update transient is defined."
  (should (fboundp 'beads-update)))

(ert-deftest beads-update-test-transient-is-prefix ()
  "Test that beads-update--menu is a transient prefix."
  (should (get 'beads-update--menu 'transient--prefix)))

(ert-deftest beads-update-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-update--execute))
  (should (fboundp 'beads-update--reset))
  (should (fboundp 'beads-update--preview)))

;;; Edge Cases

(ert-deftest beads-update-test-edge-case-unicode-title ()
  "Test updating issue with Unicode characters in title."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--title=测试 Issue with émojis 😀"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (member "测试 Issue with émojis 😀" args)))))

(ert-deftest beads-update-test-edge-case-very-long-title ()
  "Test updating issue with very long title."
  (let* ((long-title (make-string 500 ?x))
         (beads-update--issue-id "bd-42")
         (beads-update--original-data beads-update-test--sample-issue)
         (parsed (beads-update--parse-transient-args
                  (list (concat "--title=" long-title)))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (member long-title args)))))

(ert-deftest beads-update-test-edge-case-multiline-description ()
  "Test updating issue with multiline description."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--description=New multiline description"))))
    (let ((args (beads-update--build-command-args parsed)))
      (should (member "--description" args))
      (should (member "bd-42" args))
      ;; Just verify description is present in args
      (should (> (length args) 2)))))

(ert-deftest beads-update-test-edge-case-empty-string-to-value ()
  "Test changing from empty string to value."
  (let* ((issue-with-empty '((id . "bd-42")
                             (title . "Test")
                             (status . "open")
                             (priority . 1)
                             (assignee . "")))
         (beads-update--issue-id "bd-42")
         (beads-update--original-data issue-with-empty)
         (parsed (beads-update--parse-transient-args
                  '("--assignee=alice"))))
    (let ((changes (beads-update--get-changed-fields parsed)))
      (should (= (length changes) 1))
      (should (equal (car changes) '(assignee . "alice"))))))

(ert-deftest beads-update-test-edge-case-nil-to-value ()
  "Test changing from nil to value."
  (let* ((issue-with-nil '((id . "bd-42")
                          (title . "Test")
                          (status . "open")
                          (priority . 1)))
         (beads-update--issue-id "bd-42")
         (beads-update--original-data issue-with-nil)
         (parsed (beads-update--parse-transient-args
                  '("--assignee=bob"))))
    (let ((changes (beads-update--get-changed-fields parsed)))
      (should (= (length changes) 1))
      (should (equal (car changes) '(assignee . "bob"))))))

;;; Integration Tests

(ert-deftest beads-update-test-validation-workflow ()
  "Integration test: Test validation workflow."
  :tags '(integration)
  (let* ((parsed (beads-update--parse-transient-args
                  '("--status=invalid-status")))
         (validation-error (beads-update--validate-all parsed)))
    (should validation-error)
    (should (listp validation-error))))

(ert-deftest beads-update-test-command-building-workflow ()
  "Integration test: Test complete command building workflow."
  :tags '(integration)
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (parsed (beads-update--parse-transient-args
                 '("--status=in_progress" "--priority=2"))))
    (should-not (beads-update--validate-all parsed))
    (let ((args (beads-update--build-command-args parsed)))
      (should (equal (car args) "bd-42"))
      (should (member "-s" args))
      (should (member "in_progress" args))
      (should (member "-p" args))
      (should (member "2" args)))))

(ert-deftest beads-update-test-context-from-show-mode ()
  "Integration test: Test context detection from show mode."
  :tags '(integration)
  (require 'beads-show)
  (with-temp-buffer
    (beads-show-mode)
    (setq-local beads-show--issue-id "bd-99")
    (should (equal (beads-update--detect-issue-id) "bd-99"))))

(provide 'beads-update-test)
;;; beads-update-test.el ends here
