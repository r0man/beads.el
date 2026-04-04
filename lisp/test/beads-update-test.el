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
(require 'beads-command-list)
(require 'beads-command-show)
(require 'beads-command-update)

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
    (rename-buffer "*beads-show[proj]/bd-42*" t)
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
  :tags '(:integration)
  (let* ((beads-update--issue-id nil)  ; Missing issue ID should fail
         (beads-update--original-data beads-update-test--sample-issue)
         (cmd (beads-update--parse-transient-args nil))  ; No changes
         (validation-error (beads-update--validate-all cmd)))
    (should validation-error)
    (should (listp validation-error))))

(ert-deftest beads-update-test-command-building-workflow ()
  "Integration test: Test complete workflow with validation and changes."
  :tags '(:integration)
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
  :tags '(:integration)
  (require 'beads-command-show)
  (with-temp-buffer
    (beads-show-mode)
    (setq-local beads-show--issue-id "bd-99")
    (should (equal (beads-update--detect-issue-id) "bd-99"))))

;;; Tests for Execute Command

(ert-deftest beads-update-test-execute-validation-error ()
  "Test that execute returns validation error message."
  (let ((beads-update--issue-id nil)  ; Missing ID triggers validation error
        (beads-update--original-data beads-update-test--sample-issue))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) nil)))
      (should-error (beads-update--execute)
                    :type 'user-error))))

(ert-deftest beads-update-test-execute-success ()
  "Test that execute runs and returns nil on success."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (beads-auto-refresh nil)
        (executed nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--status=in_progress")))
              ((symbol-function 'beads-command-execute)
               (lambda (cmd)
                 (setq executed cmd)
                 beads-update-test--sample-issue))
              ((symbol-function 'beads--invalidate-completion-cache) #'ignore))
      (should-not (beads-update--execute))
      (should executed)
      (should (beads-command-update-p executed)))))

(ert-deftest beads-update-test-execute-with-auto-refresh ()
  "Test that execute refreshes beads buffers when auto-refresh is on."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (beads-auto-refresh t)
        (refreshed-list nil)
        (refreshed-show nil))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--status=closed")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) beads-update-test--sample-issue))
              ((symbol-function 'beads--invalidate-completion-cache) #'ignore)
              ((symbol-function 'beads-list-refresh)
               (lambda (&optional _silent) (setq refreshed-list t)))
              ((symbol-function 'beads-refresh-show)
               (lambda () (setq refreshed-show t))))
      ;; Create a mock list buffer
      (with-temp-buffer
        (beads-list-mode)
        ;; Set beads-list--command so the buffer is considered valid for refresh
        (setq-local beads-list--command 'list)
        (should-not (beads-update--execute))
        (should refreshed-list)))))

(ert-deftest beads-update-test-execute-handles-error ()
  "Test that execute returns error message on failure."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--status=in_progress")))
              ((symbol-function 'beads-command-execute)
               (lambda (_) (error "Command failed"))))
      (let ((result (beads-update--execute)))
        (should result)
        (should (string-match-p "Failed to update issue" result))))))

;;; Tests for Preview Command

(ert-deftest beads-update-test-preview-success ()
  "Test that preview returns command string."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--status=in_progress")))
              ((symbol-function 'beads-command-line)
               (lambda (_) '("update" "--status=in_progress" "bd-42"))))
      (let ((result (beads-update--preview)))
        (should result)
        (should (string-match-p "Command:" result))
        (should (string-match-p "Changes:" result))))))

(ert-deftest beads-update-test-preview-validation-error ()
  "Test that preview returns validation error message."
  (let ((beads-update--issue-id nil)  ; Missing ID triggers validation error
        (beads-update--original-data beads-update-test--sample-issue))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) nil)))
      (let ((result (beads-update--preview)))
        (should result)
        (should (string-match-p "Validation errors" result))))))

(ert-deftest beads-update-test-preview-handles-error ()
  "Test that preview handles errors gracefully."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_) '("--status=in_progress")))
              ((symbol-function 'beads-command-line)
               (lambda (_) (error "Build failed"))))
      (let ((result (beads-update--preview)))
        (should result)
        (should (string-match-p "Error:" result))))))

;;; Tests for Fetch Issue

(ert-deftest beads-update-test-fetch-issue-success ()
  "Test fetching issue data."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (_cmd) beads-update-test--sample-issue)))
    (let ((issue (beads-update--fetch-issue "bd-42")))
      (should (beads-issue-p issue))
      (should (equal (oref issue id) "bd-42")))))

(ert-deftest beads-update-test-fetch-issue-error ()
  "Test that fetch-issue signals error on failure."
  (cl-letf (((symbol-function 'beads-command-execute)
             (lambda (_cmd) (error "Not found"))))
    (should-error (beads-update--fetch-issue "bd-999")
                  :type 'user-error)))

;;; Tests for Load Issue Data

(ert-deftest beads-update-test-load-issue-data ()
  "Test loading issue data into state."
  (cl-letf (((symbol-function 'beads-update--fetch-issue)
             (lambda (id)
               (should (equal id "bd-42"))
               beads-update-test--sample-issue)))
    (beads-update--load-issue-data "bd-42")
    (should (equal beads-update--issue-id "bd-42"))
    (should (equal beads-update--original-data beads-update-test--sample-issue))))

;;; Tests for Get Original with Nil Data

(ert-deftest beads-update-test-get-original-nil-data ()
  "Test getting original field when no data loaded."
  (let ((beads-update--original-data nil))
    (should (null (beads-update--get-original 'title)))))

;;; Tests for Priority 0 Edge Case

(ert-deftest beads-update-test-parse-args-priority-zero ()
  "Test parsing priority 0 (valid value)."
  (let ((cmd (beads-update--parse-transient-args '("--priority=0"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd priority) 0))))

(ert-deftest beads-update-test-get-changed-fields-priority-zero ()
  "Test detecting change to priority 0."
  (let ((beads-update--original-data beads-update-test--sample-issue)
        (cmd (beads-update--parse-transient-args '("--priority=0"))))
    (let ((changes (beads-update--get-changed-fields cmd)))
      (should (= (length changes) 1))
      (should (equal (car changes) '(priority . 0))))))

;;; Tests for Context Detection from beads-list

(ert-deftest beads-update-test-detect-issue-id-from-list-mode ()
  "Test detecting issue ID from beads-list buffer."
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-123")))
      (should (equal (beads-update--detect-issue-id) "bd-123")))))

;;; Tests for beads-update Entry Point

(ert-deftest beads-update-test-entry-point-no-issue-id ()
  "Test that beads-update errors when no issue ID."
  (cl-letf (((symbol-function 'beads-check-executable) #'ignore)
            ((symbol-function 'beads-update--detect-issue-id)
             (lambda () nil)))
    (should-error (beads-update nil) :type 'user-error)))

(ert-deftest beads-update-test-entry-point-loads-data ()
  "Test that beads-update loads issue data before showing menu."
  (let ((loaded-id nil))
    (cl-letf (((symbol-function 'beads-check-executable) #'ignore)
              ((symbol-function 'beads-update--load-issue-data)
               (lambda (id) (setq loaded-id id)))
              ((symbol-function 'beads-update--menu) #'ignore))
      (beads-update "bd-42")
      (should (equal loaded-id "bd-42")))))

(ert-deftest beads-update-test-full-update-workflow ()
  "Integration test: Full workflow of creating and updating an issue.
Tests the complete flow: create issue, update multiple fields, verify changes."
  :tags '(:integration :slow)
  (skip-unless (executable-find beads-executable))
  (require 'beads-test)
  ;; Create temporary project using beads-test-with-shared-project
  (beads-test-with-shared-project
    ;; Create a test issue
    (let* ((created-issue (beads-execute 'beads-command-create
                           :json t
                           :title "Test Issue for Update"
                           :description "Original description"
                           :priority 2
                           :issue-type "task"))
           (issue-id (oref created-issue id)))

      ;; Verify issue was created
      (should (stringp issue-id))
      (should (string-match-p "^bt[A-Za-z0-9]+-" issue-id))

      ;; Update the issue using beads-command-update
      (let ((updated-issue (beads-execute 'beads-command-update
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
        (let ((fetched-issue (beads-execute 'beads-command-show
                              :json t
                              :issue-ids (list issue-id))))
          (should (beads-issue-p fetched-issue))
          (should (equal (oref fetched-issue title)
                         "Updated Test Issue"))
          (should (equal (oref fetched-issue status) "in_progress"))
          (should (equal (oref fetched-issue priority) 1)))))))

;;; Tests for Reset Function

(ert-deftest beads-update-test-reset-confirmed ()
  "Test reset when user confirms."
  (let ((reset-called nil)
        (message-output nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) t))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t)))
              ((symbol-function 'transient--redisplay)
               (lambda ()))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-update--reset)
      (should reset-called)
      (should (string-match-p "reset" message-output)))))

(ert-deftest beads-update-test-reset-declined ()
  "Test reset when user declines."
  (let ((reset-called nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) nil))
              ((symbol-function 'transient-reset)
               (lambda () (setq reset-called t))))
      (beads-update--reset)
      (should-not reset-called))))

;;; Regression Tests for Bug bde-6d0v

(ert-deftest beads-update-test-command-line-has-status-dashes ()
  "Test that update command line includes --status with dashes.
Regression test for bug bde-6d0v."
  (let ((beads-executable "bd")
        (cmd (beads-command-update :issue-ids '("bd-42")
                                    :status "in_progress")))
    (let ((cmd-line (beads-command-line cmd)))
      ;; Should contain --status (with dashes)
      (should (member "--status" cmd-line))
      ;; Value should follow
      (let ((status-pos (cl-position "--status" cmd-line :test #'equal)))
        (should status-pos)
        (should (equal "in_progress" (nth (1+ status-pos) cmd-line)))))))

(ert-deftest beads-update-test-show-update-field-status ()
  "Test that beads-show--update-field builds correct command.
Regression test for bug bde-6d0v."
  (require 'beads-command-show)
  (let ((beads-executable "bd")
        (beads-show--issue-id "bd-42")
        (slot-keyword :status)
        (new-value "open"))
    ;; Test the mapping and command creation (without executing)
    (let ((cmd (apply #'beads-command-update
                      :issue-ids (list beads-show--issue-id)
                      slot-keyword new-value
                      nil)))
      ;; Status should be set
      (should (equal "open" (oref cmd status)))
      ;; Validation should pass
      (should (null (beads-command-validate cmd)))
      ;; Command line should include --status
      (let ((cmd-line (beads-command-line cmd)))
        (should (member "--status" cmd-line))))))

;;; Regression Tests for Bug bde-z65s

(ert-deftest beads-update-test-transient-args-uses-correct-prefix ()
  "Test that execute and preview use correct transient prefix name.
Regression test for bug bde-z65s: 'Not a transient prefix: beads-update'."
  (let ((beads-update--issue-id "bd-42")
        (beads-update--original-data beads-update-test--sample-issue)
        (beads-auto-refresh nil)
        (transient-args-called-with nil))
    ;; Mock transient-args to capture what it's called with
    (cl-letf (((symbol-function 'transient-args)
               (lambda (prefix)
                 (setq transient-args-called-with prefix)
                 '("--status=in_progress")))
              ((symbol-function 'beads-command-execute)
               (lambda (_cmd) beads-update-test--sample-issue))
              ((symbol-function 'beads--invalidate-completion-cache) #'ignore))
      ;; Test execute
      (beads-update--execute)
      (should (eq transient-args-called-with 'beads-update--menu))
      ;; Reset and test preview
      (setq transient-args-called-with nil)
      (cl-letf (((symbol-function 'beads-command-line)
                 (lambda (_) '("update" "bd-42" "--status" "in_progress"))))
        (beads-update--preview)
        (should (eq transient-args-called-with 'beads-update--menu))))))

;;; Tests for beads-command-update parse method

(ert-deftest beads-update-test-parse-json-single-issue ()
  "Test parse method returns single issue for single ID."
  (let* ((cmd (beads-command-update :json t :issue-ids '("bd-42")
                                     :status "open")))
    (let ((result (beads-command-parse cmd "[{\"id\":\"bd-42\",\"title\":\"Test\"}]")))
      (should (beads-issue-p result))
      (should (string= (oref result id) "bd-42")))))

(ert-deftest beads-update-test-parse-json-multiple-issues ()
  "Test parse method returns list for multiple IDs."
  (let* ((cmd (beads-command-update :json t :issue-ids '("bd-1" "bd-2")
                                     :status "open")))
    (let ((result (beads-command-parse cmd "[{\"id\":\"bd-1\",\"title\":\"A\"},{\"id\":\"bd-2\",\"title\":\"B\"}]")))
      (should (listp result))
      (should (= (length result) 2))
      (should (beads-issue-p (car result))))))

(ert-deftest beads-update-test-parse-json-nil ()
  "Test parse method with json disabled returns raw string."
  (let* ((cmd (beads-command-update :json nil :issue-ids '("bd-42")
                                     :status "open")))
    (let ((result (beads-command-parse cmd "Updated bd-42")))
      (should (stringp result)))))

(ert-deftest beads-update-test-parse-json-error ()
  "Test parse signals error on bad JSON."
  (let* ((cmd (beads-command-update :json t :issue-ids '("bd-42")
                                     :status "open")))
    (should-error (beads-command-parse cmd "not json")
                  :type 'beads-json-parse-error)))

;;; Tests for beads-command-update execute-interactive

(ert-deftest beads-update-test-execute-interactive-single ()
  "Test execute-interactive messages for single issue update."
  (let* ((issue (beads-issue :id "bd-42" :title "Test"))
         (cmd (beads-command-update :json t :issue-ids '("bd-42")
                                    :status "open")))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) issue))
              ((symbol-function 'beads--invalidate-completion-cache)
               #'ignore))
      (beads-command-execute-interactive cmd)
      (should t))))

(ert-deftest beads-update-test-execute-interactive-no-result ()
  "Test execute-interactive handles nil result."
  (let* ((cmd (beads-command-update :json t :issue-ids '("bd-42")
                                    :status "open")))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (_cmd) nil))
              ((symbol-function 'beads--invalidate-completion-cache)
               #'ignore))
      (beads-command-execute-interactive cmd)
      (should t))))

;;; Tests for beads-update--get-changed-fields

(ert-deftest beads-update-test-get-changed-fields-no-changes ()
  "Test get-changed-fields returns nil when nothing changed."
  (let ((beads-update--original-data
         '((title . "Test") (description . "Desc"))))
    (cl-letf (((symbol-function 'beads-update--get-original)
               (lambda (key)
                 (alist-get key beads-update--original-data))))
      ;; Create a command with same values as original
      (let ((cmd (beads-command-update :issue-ids '("bd-42")
                                        :title "Test")))
        (should (null (beads-update--get-changed-fields cmd)))))))

(ert-deftest beads-update-test-get-changed-fields-with-changes ()
  "Test get-changed-fields detects modified fields."
  (let ((beads-update--original-data
         '((title . "Old") (description . "Old desc"))))
    (cl-letf (((symbol-function 'beads-update--get-original)
               (lambda (key)
                 (alist-get key beads-update--original-data))))
      ;; Create a command with different title and new design
      (let ((cmd (beads-command-update :issue-ids '("bd-42")
                                        :title "New Title"
                                        :design "New design")))
        (let ((changes (beads-update--get-changed-fields cmd)))
          (should (assoc 'title changes))
          (should (assoc 'design changes)))))))

;;; Tests for beads-command-update validation

(ert-deftest beads-update-test-validate-no-issue-id ()
  "Test validation fails with no issue IDs."
  (let ((cmd (beads-command-update :issue-ids nil :status "open")))
    (should (beads-command-validate cmd))))

(ert-deftest beads-update-test-validate-no-fields ()
  "Test validation fails with no fields to update."
  (let ((cmd (beads-command-update :issue-ids '("bd-42"))))
    (should (beads-command-validate cmd))))

(ert-deftest beads-update-test-validate-invalid-labels ()
  "Test non-string label list is rejected.
EIEIO enforces (list-of string) at construction time."
  (should-error (beads-command-update :issue-ids '("bd-42")
                                      :add-label '("valid" 42))))

;;; Tests for beads-update--get-changed-fields (acceptance, design, notes,
;;; assignee, external-ref fields — lines 687-702)

(ert-deftest beads-update-test-collect-changes-acceptance ()
  "Test get-changed-fields detects changed acceptance-criteria."
  :tags '(:unit)
  (let ((beads-update--original-data
         (beads-issue :id "bd-42" :title "T"
                      :acceptance-criteria "old AC")))
    (let ((cmd (beads-command-update :issue-ids '("bd-42")
                                      :acceptance "new AC")))
      (let ((changes (beads-update--get-changed-fields cmd)))
        (should (= (length changes) 1))
        (should (equal (car changes) '(acceptance . "new AC")))))))

(ert-deftest beads-update-test-collect-changes-design ()
  "Test get-changed-fields detects changed design."
  :tags '(:unit)
  (let ((beads-update--original-data
         (beads-issue :id "bd-42" :title "T"
                      :design "old design")))
    (let ((cmd (beads-command-update :issue-ids '("bd-42")
                                      :design "new design")))
      (let ((changes (beads-update--get-changed-fields cmd)))
        (should (= (length changes) 1))
        (should (equal (car changes) '(design . "new design")))))))

(ert-deftest beads-update-test-collect-changes-notes ()
  "Test get-changed-fields detects changed notes."
  :tags '(:unit)
  (let ((beads-update--original-data
         (beads-issue :id "bd-42" :title "T"
                      :notes "old notes")))
    (let ((cmd (beads-command-update :issue-ids '("bd-42")
                                      :notes "new notes")))
      (let ((changes (beads-update--get-changed-fields cmd)))
        (should (= (length changes) 1))
        (should (equal (car changes) '(notes . "new notes")))))))

(ert-deftest beads-update-test-collect-changes-assignee ()
  "Test get-changed-fields detects changed assignee."
  :tags '(:unit)
  (let ((beads-update--original-data
         (beads-issue :id "bd-42" :title "T"
                      :assignee "old-user")))
    (let ((cmd (beads-command-update :issue-ids '("bd-42")
                                      :assignee "new-user")))
      (let ((changes (beads-update--get-changed-fields cmd)))
        (should (= (length changes) 1))
        (should (equal (car changes) '(assignee . "new-user")))))))

(ert-deftest beads-update-test-collect-changes-external-ref ()
  "Test get-changed-fields detects changed external-ref."
  :tags '(:unit)
  (let ((beads-update--original-data
         (beads-issue :id "bd-42" :title "T"
                      :external-ref "gh-100")))
    (let ((cmd (beads-command-update :issue-ids '("bd-42")
                                      :external-ref "gh-200")))
      (let ((changes (beads-update--get-changed-fields cmd)))
        (should (= (length changes) 1))
        (should (equal (car changes) '(external-ref . "gh-200")))))))

(ert-deftest beads-update-test-collect-changes-multiple-text-fields ()
  "Test get-changed-fields detects multiple text field changes at once."
  :tags '(:unit)
  (let ((beads-update--original-data
         (beads-issue :id "bd-42" :title "T"
                      :acceptance-criteria "old AC"
                      :design "old design"
                      :notes "old notes"
                      :assignee "old-user"
                      :external-ref "gh-100")))
    (let ((cmd (beads-command-update :issue-ids '("bd-42")
                                      :acceptance "new AC"
                                      :design "new design"
                                      :notes "new notes"
                                      :assignee "new-user"
                                      :external-ref "gh-200")))
      (let ((changes (beads-update--get-changed-fields cmd)))
        (should (= (length changes) 5))
        (should (assoc 'acceptance changes))
        (should (assoc 'design changes))
        (should (assoc 'notes changes))
        (should (assoc 'assignee changes))
        (should (assoc 'external-ref changes))))))

(ert-deftest beads-update-test-collect-changes-same-values-no-change ()
  "Test get-changed-fields ignores fields set to their original values."
  :tags '(:unit)
  (let ((beads-update--original-data
         (beads-issue :id "bd-42" :title "T"
                      :acceptance-criteria "same AC"
                      :design "same design"
                      :notes "same notes"
                      :assignee "same-user"
                      :external-ref "gh-100")))
    (let ((cmd (beads-command-update :issue-ids '("bd-42")
                                      :acceptance "same AC"
                                      :design "same design"
                                      :notes "same notes"
                                      :assignee "same-user"
                                      :external-ref "gh-100")))
      (should (null (beads-update--get-changed-fields cmd))))))

;;; Tests for beads-command-parse unexpected JSON structure (lines 532-536)

(ert-deftest beads-update-test-parse-json-unexpected-structure ()
  "Test parse signals beads-json-parse-error on non-vector JSON."
  :tags '(:unit)
  (let* ((cmd (beads-command-update :json t :issue-ids '("bd-42")
                                     :status "open")))
    ;; A JSON object (not an array) parses as an alist, not a vector
    (should-error (beads-command-parse cmd "{\"id\":\"bd-42\",\"title\":\"Test\"}")
                  :type 'beads-json-parse-error)))

;;; Tests for beads-update--parse-transient-args additional fields (be-rzf)

(ert-deftest beads-update-test-parse-args-add-label ()
  "Test parsing --add-label argument."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args '("--add-label=bug"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd add-label) '("bug")))))

(ert-deftest beads-update-test-parse-args-remove-label ()
  "Test parsing --remove-label argument."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args '("--remove-label=wontfix"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd remove-label) '("wontfix")))))

(ert-deftest beads-update-test-parse-args-due ()
  "Test parsing --due argument."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args '("--due=2026-12-31"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd due) "2026-12-31"))))

(ert-deftest beads-update-test-parse-args-estimate ()
  "Test parsing --estimate argument."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args '("--estimate=3"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd estimate) 3))))

(ert-deftest beads-update-test-parse-args-claim ()
  "Test parsing --claim argument."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args '("--claim"))))
    (should (beads-command-update-p cmd))
    (should (oref cmd claim))))

(ert-deftest beads-update-test-parse-args-no-claim-when-absent ()
  "Test --claim is nil when not in args."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args '("--status=open"))))
    (should (beads-command-update-p cmd))
    (should (null (oref cmd claim)))))

(ert-deftest beads-update-test-parse-args-parent ()
  "Test parsing --parent argument."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args '("--parent=bd-10"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd parent) "bd-10"))))

(ert-deftest beads-update-test-parse-args-defer ()
  "Test parsing --defer argument."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args '("--defer=2026-06-01"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd defer) "2026-06-01"))))

(ert-deftest beads-update-test-parse-args-set-labels ()
  "Test parsing --set-labels argument."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args '("--set-labels=urgent"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd set-labels) '("urgent")))))

(ert-deftest beads-update-test-parse-args-body-file ()
  "Test parsing --body-file argument."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args '("--body-file=/tmp/desc.txt"))))
    (should (beads-command-update-p cmd))
    (should (equal (oref cmd body-file) "/tmp/desc.txt"))))

(ert-deftest beads-update-test-parse-args-nil-when-absent ()
  "Test that unspecified fields are nil after parsing empty args."
  :tags '(:unit)
  (let ((cmd (beads-update--parse-transient-args nil)))
    (should (null (oref cmd add-label)))
    (should (null (oref cmd remove-label)))
    (should (null (oref cmd due)))
    (should (null (oref cmd estimate)))
    (should (null (oref cmd claim)))
    (should (null (oref cmd parent)))
    (should (null (oref cmd defer)))
    (should (null (oref cmd set-labels)))
    (should (null (oref cmd body-file)))))

(provide 'beads-update-test)
;;; beads-update-test.el ends here
