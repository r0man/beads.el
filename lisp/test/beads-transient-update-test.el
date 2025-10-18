;;; beads-transient-update-test.el --- Tests for beads-update -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-update.el transient menu.
;; Tests cover context detection, issue loading, change tracking,
;; validation, command construction, and execution.

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

(defun beads-update-test--set-state (state-alist)
  "Set transient state from STATE-ALIST.
STATE-ALIST is an alist of (variable . value) pairs."
  (setq beads-update--issue-id nil
        beads-update--original-data nil
        beads-update--status nil
        beads-update--priority nil
        beads-update--type nil
        beads-update--title nil
        beads-update--description nil
        beads-update--acceptance-criteria nil
        beads-update--design nil
        beads-update--notes nil
        beads-update--assignee nil
        beads-update--external-ref nil)
  (dolist (binding state-alist)
    (set (car binding) (cdr binding))))

(defmacro beads-update-test-with-state (state &rest body)
  "Execute BODY with beads-update transient state set to STATE.
STATE is an alist expression of (variable . value) pairs."
  (declare (indent 1))
  `(progn
     (beads-update-test--set-state ,state)
     ,@body))

(defun beads-update-test--mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; Tests for State Management

(ert-deftest beads-update-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-update-test-with-state
   '((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ((id . "bd-42")))
     (beads-update--status . "in_progress")
     (beads-update--priority . 2)
     (beads-update--type . "feature")
     (beads-update--title . "New Title")
     (beads-update--description . "New Desc")
     (beads-update--acceptance-criteria . "New AC")
     (beads-update--design . "New Design")
     (beads-update--notes . "New Notes")
     (beads-update--assignee . "newuser")
     (beads-update--external-ref . "gh-999"))
   (beads-update--reset-state)
   (should (null beads-update--issue-id))
   (should (null beads-update--original-data))
   (should (null beads-update--status))
   (should (null beads-update--priority))
   (should (null beads-update--type))
   (should (null beads-update--title))
   (should (null beads-update--description))
   (should (null beads-update--acceptance-criteria))
   (should (null beads-update--design))
   (should (null beads-update--notes))
   (should (null beads-update--assignee))
   (should (null beads-update--external-ref))))

(ert-deftest beads-update-test-reset-state-from-nil ()
  "Test that reset-state works when variables are already nil."
  (beads-update-test-with-state nil
   (beads-update--reset-state)
   (should (null beads-update--issue-id))
   (should (null beads-update--original-data))))

;;; Tests for Value Formatting

(ert-deftest beads-update-test-format-current-value-unchanged ()
  "Test formatting when value hasn't been changed."
  (let ((result (beads-update--format-current-value nil "original")))
    (should (stringp result))
    (should (string-match-p "original" result))
    (should (get-text-property 0 'face result))))

(ert-deftest beads-update-test-format-current-value-changed ()
  "Test formatting when value has been changed."
  (let ((result (beads-update--format-current-value "new" "original")))
    (should (stringp result))
    (should (string-match-p "new" result))
    (should (eq (get-text-property 0 'face result) 'transient-value))))

(ert-deftest beads-update-test-format-current-value-both-nil ()
  "Test formatting when both values are nil."
  (let ((result (beads-update--format-current-value nil nil)))
    (should (stringp result))
    (should (string-match-p "unset" result))))

(ert-deftest beads-update-test-format-current-value-long-string ()
  "Test formatting with long string value."
  (let* ((long-value (make-string 100 ?x))
         (result (beads-update--format-current-value long-value nil)))
    (should (stringp result))
    (should (string-match-p "\\.\\.\\." result))
    (should (< (length result) 60))))

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
  (beads-update-test-with-state
   `((beads-update--original-data . ,beads-update-test--sample-issue))
   (should (equal (beads-update--get-original 'id) "bd-42"))
   (should (equal (beads-update--get-original 'title) "Test Issue"))
   (should (equal (beads-update--get-original 'status) "open"))
   (should (equal (beads-update--get-original 'priority) 1))))

(ert-deftest beads-update-test-get-original-missing-field ()
  "Test getting original field that doesn't exist."
  (beads-update-test-with-state
   `((beads-update--original-data . ,beads-update-test--sample-issue))
   (should (null (beads-update--get-original 'nonexistent)))))

;;; Tests for Change Tracking

(ert-deftest beads-update-test-get-changed-fields-none ()
  "Test getting changed fields when nothing changed."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue))
   (let ((changes (beads-update--get-changed-fields)))
     (should (null changes)))))

(ert-deftest beads-update-test-get-changed-fields-status ()
  "Test getting changed fields when status changed."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "in_progress"))
   (let ((changes (beads-update--get-changed-fields)))
     (should (= (length changes) 1))
     (should (equal (car changes) '(status . "in_progress"))))))

(ert-deftest beads-update-test-get-changed-fields-priority ()
  "Test getting changed fields when priority changed."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--priority . 3))
   (let ((changes (beads-update--get-changed-fields)))
     (should (= (length changes) 1))
     (should (equal (car changes) '(priority . 3))))))

(ert-deftest beads-update-test-get-changed-fields-multiple ()
  "Test getting changed fields when multiple fields changed."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "closed")
     (beads-update--priority . 0)
     (beads-update--title . "New Title"))
   (let ((changes (beads-update--get-changed-fields)))
     (should (= (length changes) 3))
     (should (member '(status . "closed") changes))
     (should (member '(priority . 0) changes))
     (should (member '(title . "New Title") changes)))))

(ert-deftest beads-update-test-get-changed-fields-same-value ()
  "Test that setting field to same value doesn't count as change."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "open"))  ; Same as original
   (let ((changes (beads-update--get-changed-fields)))
     (should (null changes)))))

(ert-deftest beads-update-test-get-changed-fields-all-fields ()
  "Test getting changed fields when all fields changed."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "blocked")
     (beads-update--priority . 0)
     (beads-update--type . "epic")
     (beads-update--title . "New Title")
     (beads-update--description . "New Desc")
     (beads-update--acceptance-criteria . "New AC")
     (beads-update--design . "New Design")
     (beads-update--notes . "New Notes")
     (beads-update--assignee . "newuser")
     (beads-update--external-ref . "jira-999"))
   (let ((changes (beads-update--get-changed-fields)))
     (should (= (length changes) 10)))))

;;; Tests for Validation

(ert-deftest beads-update-test-validate-status-nil ()
  "Test status validation when status is nil."
  (beads-update-test-with-state nil
   (should (null (beads-update--validate-status)))))

(ert-deftest beads-update-test-validate-status-valid-open ()
  "Test status validation with valid open status."
  (beads-update-test-with-state '((beads-update--status . "open"))
   (should (null (beads-update--validate-status)))))

(ert-deftest beads-update-test-validate-status-valid-in-progress ()
  "Test status validation with valid in_progress status."
  (beads-update-test-with-state
   '((beads-update--status . "in_progress"))
   (should (null (beads-update--validate-status)))))

(ert-deftest beads-update-test-validate-status-valid-blocked ()
  "Test status validation with valid blocked status."
  (beads-update-test-with-state '((beads-update--status . "blocked"))
   (should (null (beads-update--validate-status)))))

(ert-deftest beads-update-test-validate-status-valid-closed ()
  "Test status validation with valid closed status."
  (beads-update-test-with-state '((beads-update--status . "closed"))
   (should (null (beads-update--validate-status)))))

(ert-deftest beads-update-test-validate-status-invalid ()
  "Test status validation with invalid status."
  (beads-update-test-with-state '((beads-update--status . "invalid"))
   (should (beads-update--validate-status))))

(ert-deftest beads-update-test-validate-type-nil ()
  "Test type validation when type is nil."
  (beads-update-test-with-state nil
   (should (null (beads-update--validate-type)))))

(ert-deftest beads-update-test-validate-type-valid-bug ()
  "Test type validation with valid bug type."
  (beads-update-test-with-state '((beads-update--type . "bug"))
   (should (null (beads-update--validate-type)))))

(ert-deftest beads-update-test-validate-type-valid-feature ()
  "Test type validation with valid feature type."
  (beads-update-test-with-state '((beads-update--type . "feature"))
   (should (null (beads-update--validate-type)))))

(ert-deftest beads-update-test-validate-type-valid-task ()
  "Test type validation with valid task type."
  (beads-update-test-with-state '((beads-update--type . "task"))
   (should (null (beads-update--validate-type)))))

(ert-deftest beads-update-test-validate-type-valid-epic ()
  "Test type validation with valid epic type."
  (beads-update-test-with-state '((beads-update--type . "epic"))
   (should (null (beads-update--validate-type)))))

(ert-deftest beads-update-test-validate-type-valid-chore ()
  "Test type validation with valid chore type."
  (beads-update-test-with-state '((beads-update--type . "chore"))
   (should (null (beads-update--validate-type)))))

(ert-deftest beads-update-test-validate-type-invalid ()
  "Test type validation with invalid type."
  (beads-update-test-with-state '((beads-update--type . "invalid"))
   (should (beads-update--validate-type))))

(ert-deftest beads-update-test-validate-priority-nil ()
  "Test priority validation when priority is nil."
  (beads-update-test-with-state nil
   (should (null (beads-update--validate-priority)))))

(ert-deftest beads-update-test-validate-priority-zero ()
  "Test priority validation with zero (critical)."
  (beads-update-test-with-state '((beads-update--priority . 0))
   (should (null (beads-update--validate-priority)))))

(ert-deftest beads-update-test-validate-priority-one ()
  "Test priority validation with one."
  (beads-update-test-with-state '((beads-update--priority . 1))
   (should (null (beads-update--validate-priority)))))

(ert-deftest beads-update-test-validate-priority-four ()
  "Test priority validation with four (backlog)."
  (beads-update-test-with-state '((beads-update--priority . 4))
   (should (null (beads-update--validate-priority)))))

(ert-deftest beads-update-test-validate-priority-negative ()
  "Test priority validation with negative number."
  (beads-update-test-with-state '((beads-update--priority . -1))
   (should (beads-update--validate-priority))))

(ert-deftest beads-update-test-validate-priority-too-high ()
  "Test priority validation with number too high."
  (beads-update-test-with-state '((beads-update--priority . 5))
   (should (beads-update--validate-priority))))

(ert-deftest beads-update-test-validate-priority-string ()
  "Test priority validation with string instead of number."
  (beads-update-test-with-state '((beads-update--priority . "1"))
   (should (beads-update--validate-priority))))

(ert-deftest beads-update-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (beads-update-test-with-state
   '((beads-update--status . "in_progress")
     (beads-update--type . "bug")
     (beads-update--priority . 1))
   (should (null (beads-update--validate-all)))))

(ert-deftest beads-update-test-validate-all-multiple-errors ()
  "Test validate-all with multiple validation errors."
  (beads-update-test-with-state
   '((beads-update--status . "invalid")
     (beads-update--type . "wrong")
     (beads-update--priority . 10))
   (let ((errors (beads-update--validate-all)))
     (should errors)
     (should (listp errors))
     (should (>= (length errors) 3)))))

;;; Tests for Command Building

(ert-deftest beads-update-test-build-command-args-no-id ()
  "Test building command args without issue ID."
  (beads-update-test-with-state nil
   (should-error (beads-update--build-command-args) :type 'user-error)))

(ert-deftest beads-update-test-build-command-args-no-changes ()
  "Test building command args with no changes."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue))
   (should-error (beads-update--build-command-args) :type 'user-error)))

(ert-deftest beads-update-test-build-command-args-status-change ()
  "Test building command args with status change."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "in_progress"))
   (let ((args (beads-update--build-command-args)))
     (should (equal args '("bd-42" "-s" "in_progress"))))))

(ert-deftest beads-update-test-build-command-args-priority-change ()
  "Test building command args with priority change."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--priority . 3))
   (let ((args (beads-update--build-command-args)))
     (should (equal args '("bd-42" "-p" "3"))))))

(ert-deftest beads-update-test-build-command-args-type-change ()
  "Test building command args with type change."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--type . "feature"))
   (let ((args (beads-update--build-command-args)))
     (should (equal args '("bd-42" "-t" "feature"))))))

(ert-deftest beads-update-test-build-command-args-title-change ()
  "Test building command args with title change."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--title . "New Title"))
   (let ((args (beads-update--build-command-args)))
     (should (equal args '("bd-42" "--title" "New Title"))))))

(ert-deftest beads-update-test-build-command-args-description-change ()
  "Test building command args with description change."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--description . "New description"))
   (let ((args (beads-update--build-command-args)))
     (should (member "--description" args))
     (should (member "New description" args)))))

(ert-deftest beads-update-test-build-command-args-assignee-change ()
  "Test building command args with assignee change."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--assignee . "newuser"))
   (let ((args (beads-update--build-command-args)))
     (should (equal args '("bd-42" "-a" "newuser"))))))

(ert-deftest beads-update-test-build-command-args-external-ref-change ()
  "Test building command args with external ref change."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--external-ref . "jira-999"))
   (let ((args (beads-update--build-command-args)))
     (should (member "--external-ref" args))
     (should (member "jira-999" args)))))

(ert-deftest beads-update-test-build-command-args-multiple-changes ()
  "Test building command args with multiple changes."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "closed")
     (beads-update--priority . 0)
     (beads-update--title . "Updated Title"))
   (let ((args (beads-update--build-command-args)))
     (should (member "bd-42" args))
     (should (member "-s" args))
     (should (member "closed" args))
     (should (member "-p" args))
     (should (member "0" args))
     (should (member "--title" args))
     (should (member "Updated Title" args)))))

(ert-deftest beads-update-test-build-command-args-all-fields ()
  "Test building command args with all fields changed."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "closed")
     (beads-update--priority . 4)
     (beads-update--type . "chore")
     (beads-update--title . "New Title")
     (beads-update--description . "New Desc")
     (beads-update--acceptance-criteria . "New AC")
     (beads-update--design . "New Design")
     (beads-update--notes . "New Notes")
     (beads-update--assignee . "alice")
     (beads-update--external-ref . "gh-999"))
   (let ((args (beads-update--build-command-args)))
     (should (member "bd-42" args))
     (should (member "-s" args))
     (should (member "-p" args))
     (should (member "-t" args))
     (should (member "--title" args))
     (should (member "--description" args))
     (should (member "--acceptance-criteria" args))
     (should (member "--design" args))
     (should (member "--notes" args))
     (should (member "-a" args))
     (should (member "--external-ref" args)))))

(ert-deftest beads-update-test-build-command-args-priority-zero ()
  "Test building command args with priority zero (critical)."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--priority . 0))
   (let ((args (beads-update--build-command-args)))
     (should (member "0" args)))))

;;; Tests for Execution

(ert-deftest beads-update-test-execute-success ()
  "Test successful issue update."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "in_progress")
     (beads-update--priority . 2))
   (let ((json-output (json-encode
                       beads-update-test--sample-update-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-update-test--mock-call-process 0 json-output)))
       (should-not (beads-update--execute))
       ;; State should be reset after successful execution
       (should (null beads-update--issue-id))
       (should (null beads-update--status))
       (should (null beads-update--priority))))))

(ert-deftest beads-update-test-execute-validation-failure ()
  "Test execution fails with validation error."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "invalid"))
   (should-error (beads-update--execute) :type 'user-error)))

(ert-deftest beads-update-test-execute-no-changes ()
  "Test execution fails when no changes made."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue))
   (should-error (beads-update--execute) :type 'user-error)))

(ert-deftest beads-update-test-execute-command-failure ()
  "Test execution handles bd command failure."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "closed"))
   (cl-letf (((symbol-function 'call-process)
              (beads-update-test--mock-call-process 1 "Error: failed")))
     ;; Should not propagate error, just display message
     (should (stringp (beads-update--execute))))))

;;; Tests for Preview

(ert-deftest beads-update-test-preview-valid ()
  "Test preview command with valid changes."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "in_progress"))
   ;; Preview returns a message string
   (should (stringp (beads-update--preview)))))

(ert-deftest beads-update-test-preview-validation-failure ()
  "Test preview shows validation errors."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "invalid"))
   ;; Preview returns a message string even with validation errors
   (should (stringp (beads-update--preview)))))

(ert-deftest beads-update-test-preview-no-changes ()
  "Test preview with no changes."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue))
   ;; Should return error message
   (should (stringp (beads-update--preview)))))

;;; Tests for Transient Definition

(ert-deftest beads-update-test-transient-defined ()
  "Test that beads-update transient is defined."
  (should (fboundp 'beads-update)))

(ert-deftest beads-update-test-transient-is-prefix ()
  "Test that beads-update--menu is a transient prefix."
  (should (get 'beads-update--menu 'transient--prefix)))

(ert-deftest beads-update-test-infix-commands-defined ()
  "Test that all infix commands are defined."
  (should (fboundp 'beads-update--infix-status))
  (should (fboundp 'beads-update--infix-priority))
  (should (fboundp 'beads-update--infix-type))
  (should (fboundp 'beads-update--infix-title))
  (should (fboundp 'beads-update--infix-assignee))
  (should (fboundp 'beads-update--infix-external-ref))
  (should (fboundp 'beads-update--infix-description))
  (should (fboundp 'beads-update--infix-acceptance-multiline))
  (should (fboundp 'beads-update--infix-design-multiline))
  (should (fboundp 'beads-update--infix-notes-multiline)))

(ert-deftest beads-update-test-suffix-commands-defined ()
  "Test that all suffix commands are defined."
  (should (fboundp 'beads-update--execute))
  (should (fboundp 'beads-update--reset))
  (should (fboundp 'beads-update--preview)))

;;; Integration Tests

(ert-deftest beads-update-test-integration-full-workflow ()
  "Test complete workflow from loading to update."
  (beads-update-test-with-state nil
   ;; Load issue data
   (setq beads-update--issue-id "bd-42")
   (setq beads-update--original-data beads-update-test--sample-issue)

   ;; Make changes
   (setq beads-update--status "closed")
   (setq beads-update--priority 0)

   ;; Validate
   (should (null (beads-update--validate-all)))

   ;; Build command
   (let ((args (beads-update--build-command-args)))
     (should (member "bd-42" args))
     (should (member "-s" args))
     (should (member "closed" args))
     (should (member "-p" args))
     (should (member "0" args)))

   ;; Execute (mocked)
   (let ((json-output (json-encode
                       beads-update-test--sample-update-response)))
     (cl-letf (((symbol-function 'call-process)
                (beads-update-test--mock-call-process 0 json-output)))
       (should-not (beads-update--execute))
       ;; Verify state was reset
       (should (null beads-update--issue-id))))))

(ert-deftest beads-update-test-integration-reset-changes ()
  "Test resetting changes."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "closed")
     (beads-update--priority . 0))

   ;; Mock y-or-n-p to return yes
   (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
     (beads-update--reset)
     (should (null beads-update--status))
     (should (null beads-update--priority))
     ;; Original data should remain
     (should (equal beads-update--issue-id "bd-42"))
     (should beads-update--original-data))))

(ert-deftest beads-update-test-integration-multiple-field-update ()
  "Test updating multiple fields at once."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "blocked")
     (beads-update--priority . 0)
     (beads-update--title . "Critical Issue")
     (beads-update--assignee . "alice"))

   ;; Validate
   (should (null (beads-update--validate-all)))

   ;; Check changes detected
   (let ((changes (beads-update--get-changed-fields)))
     (should (= (length changes) 4)))

   ;; Build command
   (let ((args (beads-update--build-command-args)))
     (should (member "-s" args))
     (should (member "blocked" args))
     (should (member "-p" args))
     (should (member "0" args))
     (should (member "--title" args))
     (should (member "Critical Issue" args))
     (should (member "-a" args))
     (should (member "alice" args)))))

;;; Edge Cases

(ert-deftest beads-update-test-edge-case-unicode-title ()
  "Test updating issue with Unicode characters in title."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--title . "测试 Issue with émojis 😀"))
   (let ((args (beads-update--build-command-args)))
     (should (member "测试 Issue with émojis 😀" args)))))

(ert-deftest beads-update-test-edge-case-very-long-title ()
  "Test updating issue with very long title."
  (let ((long-title (make-string 500 ?x)))
    (beads-update-test-with-state
     `((beads-update--issue-id . "bd-42")
       (beads-update--original-data . ,beads-update-test--sample-issue)
       (beads-update--title . ,long-title))
     (let ((args (beads-update--build-command-args)))
       (should (member long-title args))))))

(ert-deftest beads-update-test-edge-case-multiline-description ()
  "Test updating issue with multiline description."
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--description . "Line 1\nLine 2\nLine 3"))
   (let ((args (beads-update--build-command-args)))
     (should (member "--description" args))
     (let ((desc (nth (1+ (cl-position "--description" args
                                       :test #'equal))
                     args)))
       (should (string-match-p "\n" desc))))))

(ert-deftest beads-update-test-edge-case-empty-string-to-value ()
  "Test changing from empty string to value."
  (let ((issue-with-empty '((id . "bd-42")
                            (title . "Test")
                            (status . "open")
                            (priority . 1)
                            (assignee . ""))))
    (beads-update-test-with-state
     `((beads-update--issue-id . "bd-42")
       (beads-update--original-data . ,issue-with-empty)
       (beads-update--assignee . "alice"))
     (let ((changes (beads-update--get-changed-fields)))
       (should (= (length changes) 1))
       (should (equal (car changes) '(assignee . "alice")))))))

(ert-deftest beads-update-test-edge-case-nil-to-value ()
  "Test changing from nil to value."
  (let ((issue-with-nil '((id . "bd-42")
                         (title . "Test")
                         (status . "open")
                         (priority . 1))))
    (beads-update-test-with-state
     `((beads-update--issue-id . "bd-42")
       (beads-update--original-data . ,issue-with-nil)
       (beads-update--assignee . "bob"))
     (let ((changes (beads-update--get-changed-fields)))
       (should (= (length changes) 1))
       (should (equal (car changes) '(assignee . "bob")))))))

;;; Performance Tests

(ert-deftest beads-update-test-performance-change-detection ()
  "Test change detection performance."
  :tags '(:performance)
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "closed")
     (beads-update--priority . 3)
     (beads-update--title . "New"))
   (let ((start-time (current-time)))
     (dotimes (_ 1000)
       (beads-update--get-changed-fields))
     (let ((elapsed (float-time (time-subtract (current-time)
                                               start-time))))
       ;; Should detect changes 1000 times in under 0.5 seconds
       (should (< elapsed 0.5))))))

(ert-deftest beads-update-test-performance-command-building ()
  "Test command building performance."
  :tags '(:performance)
  (beads-update-test-with-state
   `((beads-update--issue-id . "bd-42")
     (beads-update--original-data . ,beads-update-test--sample-issue)
     (beads-update--status . "closed")
     (beads-update--priority . 0))
   (let ((start-time (current-time)))
     (dotimes (_ 1000)
       (beads-update--build-command-args))
     (let ((elapsed (float-time (time-subtract (current-time)
                                               start-time))))
       ;; Should build 1000 commands in under 0.5 seconds
       (should (< elapsed 0.5))))))

(ert-deftest beads-update-test-performance-validation ()
  "Test validation performance."
  :tags '(:performance)
  (beads-update-test-with-state
   `((beads-update--status . "in_progress")
     (beads-update--type . "bug")
     (beads-update--priority . 1))
   (let ((start-time (current-time)))
     (dotimes (_ 1000)
       (beads-update--validate-all))
     (let ((elapsed (float-time (time-subtract (current-time)
                                               start-time))))
       ;; Should validate 1000 times in under 0.5 seconds
       (should (< elapsed 0.5))))))

(provide 'beads-transient-update-test)
;;; beads-transient-update-test.el ends here
