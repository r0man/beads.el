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
(require 'beads-types)
(require 'beads-list)
(require 'beads-show)
(require 'beads-update)

;;; Test Fixtures

(defvar beads-update-test--sample-issue
  (beads-issue
   :id "bd-42"
   :title "Test Issue"
   :description "Test description"
   :status "open"
   :priority 1
   :issue-type "bug"
   :created-at "2025-01-15T10:00:00Z"
   :updated-at "2025-01-15T10:00:00Z"
   :acceptance-criteria "Test acceptance"
   :design "Test design"
   :notes "Test notes"
   :assignee "testuser"
   :external-ref "gh-123")
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
  (let ((cmd (beads-update--parse-transient-args nil)))
    (should (beads-command-update-p cmd))
    (should (null (oref cmd status)))
    (should (null (oref cmd priority)))
    (should (null (oref cmd title)))))

(ert-deftest beads-update-test-parse-args-status ()
  "Test parsing status argument."
  (let ((cmd (beads-update--parse-transient-args
              '("--status=in_progress"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd status) "in_progress"))))

(ert-deftest beads-update-test-parse-args-priority ()
  "Test parsing priority argument."
  (let ((cmd (beads-update--parse-transient-args
              '("--priority=3"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd priority) 3))))

(ert-deftest beads-update-test-parse-args-all-fields ()
  "Test parsing with all fields."
  (let ((cmd (beads-update--parse-transient-args
              '("--status=closed"
                "--priority=0"
                "--title=New Title"
                "--description=New Desc"
                "--acceptance=New AC"
                "--design=New Design"
                "--notes=New Notes"
                "--assignee=alice"
                "--external-ref=gh-999"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd status) "closed"))
    (should (equal (oref cmd priority) 0))
    (should (equal (oref cmd title) "New Title"))
    (should (equal (oref cmd description) "New Desc"))
    (should (equal (oref cmd acceptance) "New AC"))
    (should (equal (oref cmd design) "New Design"))
    (should (equal (oref cmd notes) "New Notes"))
    (should (equal (oref cmd assignee) "alice"))
    (should (equal (oref cmd external-ref) "gh-999"))))

;;; Tests for Validation

(ert-deftest beads-update-test-validate-all-success ()
  "Test validate-all with all valid parameters."
  (let* ((beads-update--issue-id "bd-42")
         (beads-update--original-data beads-update-test--sample-issue)
         (cmd (beads-update--parse-transient-args
               '("--status=in_progress"
                 "--priority=1"))))
    (should (null (beads-update--validate-all cmd)))))

(ert-deftest beads-update-test-validate-all-multiple-errors ()
  "Test validate-all with multiple validation errors."
  (let* ((beads-update--issue-id nil)  ; No issue ID
         (beads-update--original-data beads-update-test--sample-issue)
         (cmd (beads-update--parse-transient-args nil)))  ; No changes
    (let ((errors (beads-update--validate-all cmd)))
      (should errors)
      (should (listp errors))
      (should (>= (length errors) 1)))))

;;; Tests for Change Detection

(ert-deftest beads-update-test-get-changed-fields-none ()
  "Test getting changed fields when nothing changed."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (cmd (beads-update--parse-transient-args nil)))
    (should (null (beads-update--get-changed-fields cmd)))))

(ert-deftest beads-update-test-get-changed-fields-status ()
  "Test getting changed fields when status changed."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (cmd (beads-update--parse-transient-args
              '("--status=in_progress"))))
    (let ((changes (beads-update--get-changed-fields cmd)))
      (should (= (length changes) 1))
      (should (equal (car changes) '(status . "in_progress"))))))

(ert-deftest beads-update-test-get-changed-fields-priority ()
  "Test getting changed fields when priority changed."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (cmd (beads-update--parse-transient-args
              '("--priority=3"))))
    (let ((changes (beads-update--get-changed-fields cmd)))
      (should (= (length changes) 1))
      (should (equal (car changes) '(priority . 3))))))

(ert-deftest beads-update-test-get-changed-fields-multiple ()
  "Test getting changed fields when multiple fields changed."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (cmd (beads-update--parse-transient-args
              '("--status=closed"
                "--priority=0"
                "--title=New Title"))))
    (let ((changes (beads-update--get-changed-fields cmd)))
      (should (= (length changes) 3))
      (should (member '(status . "closed") changes))
      (should (member '(priority . 0) changes))
      (should (member '(title . "New Title") changes)))))

(ert-deftest beads-update-test-get-changed-fields-same-value ()
  "Test that setting field to same value doesn't count as change."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (cmd (beads-update--parse-transient-args
              '("--status=open"))))
    (should (null (beads-update--get-changed-fields cmd)))))

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
        (cmd (beads-update--parse-transient-args
              '("--title=测试 Issue with émojis 😀"))))
    (let ((changes (beads-update--get-changed-fields cmd)))
      (should (= (length changes) 1))
      (should (equal (cdr (car changes)) "测试 Issue with émojis 😀")))))

(ert-deftest beads-update-test-edge-case-very-long-title ()
  "Test updating issue with very long title."
  (let* ((long-title (make-string 500 ?x))
         (beads-update--issue-id "bd-42")
         (beads-update--original-data beads-update-test--sample-issue)
         (cmd (beads-update--parse-transient-args
               (list (concat "--title=" long-title)))))
    (let ((changes (beads-update--get-changed-fields cmd)))
      (should (= (length changes) 1))
      (should (equal (cdr (car changes)) long-title)))))

(ert-deftest beads-update-test-edge-case-multiline-description ()
  "Test updating issue with multiline description."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (cmd (beads-update--parse-transient-args
              '("--description=New multiline description"))))
    (let ((changes (beads-update--get-changed-fields cmd)))
      (should (= (length changes) 1))
      (should (equal (car changes) '(description . "New multiline description"))))))

(ert-deftest beads-update-test-edge-case-empty-string-to-value ()
  "Test changing from empty string to value."
  (let* ((issue-with-empty (beads-issue
                            :id "bd-42"
                            :title "Test"
                            :status "open"
                            :priority 1
                            :assignee ""))
         (beads-update--issue-id "bd-42")
         (beads-update--original-data issue-with-empty)
         (cmd (beads-update--parse-transient-args
               '("--assignee=alice"))))
    (let ((changes (beads-update--get-changed-fields cmd)))
      (should (= (length changes) 1))
      (should (equal (car changes) '(assignee . "alice"))))))

(ert-deftest beads-update-test-edge-case-nil-to-value ()
  "Test changing from nil to value."
  (let* ((issue-with-nil (beads-issue
                          :id "bd-42"
                          :title "Test"
                          :status "open"
                          :priority 1))
         (beads-update--issue-id "bd-42")
         (beads-update--original-data issue-with-nil)
         (cmd (beads-update--parse-transient-args
               '("--assignee=bob"))))
    (let ((changes (beads-update--get-changed-fields cmd)))
      (should (= (length changes) 1))
      (should (equal (car changes) '(assignee . "bob"))))))

;;; Integration Tests

(ert-deftest beads-update-test-validation-workflow ()
  "Integration test: Test validation workflow."
  :tags '(integration)
  (let* ((beads-update--issue-id nil)  ; Missing issue ID should fail
         (beads-update--original-data beads-update-test--sample-issue)
         (cmd (beads-update--parse-transient-args nil))  ; No changes
         (validation-error (beads-update--validate-all cmd)))
    (should validation-error)
    (should (listp validation-error))))

(ert-deftest beads-update-test-command-building-workflow ()
  "Integration test: Test complete workflow with validation and changes."
  :tags '(integration)
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue))
    (let ((cmd (beads-update--parse-transient-args
                '("--status=in_progress" "--priority=2"))))
      (should-not (beads-update--validate-all cmd))
      (let ((changes (beads-update--get-changed-fields cmd)))
        (should (= (length changes) 2))
        (should (member '(status . "in_progress") changes))
        (should (member '(priority . 2) changes))))))

(ert-deftest beads-update-test-context-from-show-mode ()
  "Integration test: Test context detection from show mode."
  :tags '(integration)
  (require 'beads-show)
  (with-temp-buffer
    (beads-show-mode)
    (setq-local beads-show--issue-id "bd-99")
    (should (equal (beads-update--detect-issue-id) "bd-99"))))

(ert-deftest beads-update-test-full-update-workflow ()
  "Integration test: Full workflow of creating and updating an issue.
Tests the complete flow: create issue, update multiple fields, verify changes."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (require 'beads-test-helper)
  ;; Create temporary project
  (let ((project-dir (beads-test-helper-create-temp-project)))
    (unwind-protect
        (let ((default-directory project-dir))
          ;; Create a test issue
          (let* ((created-issue (beads-command-create!
                                 :json t
                                 :title "Test Issue for Update"
                                 :description "Original description"
                                 :priority 2
                                 :issue-type "task"))
                 (issue-id (oref created-issue id)))

            ;; Verify issue was created
            (should (stringp issue-id))
            (should (string-match-p "^test-" issue-id))

            ;; Update the issue using beads-command-update
            (let ((updated-issue (beads-command-update!
                                  :json t
                                  :issue-ids (list issue-id)
                                  :title "Updated Test Issue"
                                  :description "Updated description"
                                  :status "in_progress"
                                  :priority 1
                                  :notes "Added some notes")))

              ;; Verify the update returned an issue
              (should (beads-issue-p updated-issue))
              (should (equal (oref updated-issue id) issue-id))

              ;; Verify fields were updated
              (should (equal (oref updated-issue title) "Updated Test Issue"))
              (should (equal (oref updated-issue description)
                           "Updated description"))
              (should (equal (oref updated-issue status) "in_progress"))
              (should (equal (oref updated-issue priority) 1))
              (should (equal (oref updated-issue notes) "Added some notes"))

              ;; Fetch the issue again to verify persistence
              (let ((fetched-issue (beads-command-show!
                                    :json t
                                    :issue-ids (list issue-id))))
                (should (beads-issue-p fetched-issue))
                (should (equal (oref fetched-issue title)
                             "Updated Test Issue"))
                (should (equal (oref fetched-issue status) "in_progress"))
                (should (equal (oref fetched-issue priority) 1))))))

      ;; Cleanup
      (when (file-directory-p project-dir)
        (delete-directory project-dir t)))))

(provide 'beads-update-test)
;;; beads-update-test.el ends here
