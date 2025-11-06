;;; beads-reader-test.el --- Tests for beads-reader -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-reader.el input functions.
;; Tests cover all reader functions including:
;; - Common readers (issue-id, string, file, choice, priority)
;; - Create readers (title, type, priority, dependencies, etc.)
;; - Update readers (status, priority, type, etc.)
;; - Close/reopen/sync readers
;; - Dependency readers
;; - Export/import/init readers
;;
;; Tests verify:
;; - Function existence and callability
;; - Return types and behavior
;; - Priority conversion logic
;; - Integration with transient reader protocol

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-reader)

;;; Test Fixtures

(defvar beads-reader-test--mock-issues
  '(("bd-1" . "First issue")
    ("bd-2" . "Second issue")
    ("worker-1" . "Worker task"))
  "Mock issue completion table.")

;;; ============================================================
;;; Tests for Common Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-issue-id ()
  "Test reading issue ID with completion."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "bd-1"))
            ((symbol-function 'beads--issue-completion-table)
             (lambda () beads-reader-test--mock-issues)))
    (let ((result (beads-reader-issue-id "Issue: ")))
      (should (equal result "bd-1")))))

(ert-deftest beads-reader-test-string-returns-lambda ()
  "Test that beads-reader-string returns a lambda function."
  (let ((reader (beads-reader-string "Prompt: " "default")))
    (should (functionp reader))))

(ert-deftest beads-reader-test-string-calls-read-string ()
  "Test that beads-reader-string lambda calls read-string."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _args) "user input")))
    (let* ((reader (beads-reader-string "Prompt: " "default"))
           (result (funcall reader nil nil nil)))
      (should (equal result "user input")))))

(ert-deftest beads-reader-test-file-returns-lambda ()
  "Test that beads-reader-file returns a lambda function."
  (let ((reader (beads-reader-file "File: " "/tmp/file")))
    (should (functionp reader))))

(ert-deftest beads-reader-test-file-calls-read-file-name ()
  "Test that beads-reader-file lambda calls read-file-name."
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (&rest _args) "/path/to/file")))
    (let* ((reader (beads-reader-file "File: " "/tmp/file"))
           (result (funcall reader nil nil nil)))
      (should (equal result "/path/to/file")))))

(ert-deftest beads-reader-test-choice-returns-lambda ()
  "Test that beads-reader-choice returns a lambda function."
  (let ((reader (beads-reader-choice "Choice: " '("a" "b" "c"))))
    (should (functionp reader))))

(ert-deftest beads-reader-test-choice-calls-completing-read ()
  "Test that beads-reader-choice lambda calls completing-read."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "b")))
    (let* ((reader (beads-reader-choice "Choice: " '("a" "b" "c")))
           (result (funcall reader nil nil nil)))
      (should (equal result "b")))))

(ert-deftest beads-reader-test-priority-returns-lambda ()
  "Test that beads-reader-priority returns a lambda function."
  (let ((beads-create--priority 2))
    (let ((reader (beads-reader-priority "Priority: "
                                         'beads-create--priority)))
      (should (functionp reader)))))

(ert-deftest beads-reader-test-priority-converts-to-string ()
  "Test that beads-reader-priority converts number to string."
  (let ((beads-create--priority 2))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "1 - High")))
      (let* ((reader (beads-reader-priority "Priority: "
                                            'beads-create--priority))
             (result (funcall reader nil nil nil)))
        (should (stringp result))
        (should (equal result "1"))))))

(ert-deftest beads-reader-test-priority-all-levels ()
  "Test priority reader converts all levels correctly."
  (let ((beads-create--priority nil))
    (dolist (test-case '(("0 - Critical" . "0")
                         ("1 - High" . "1")
                         ("2 - Medium" . "2")
                         ("3 - Low" . "3")
                         ("4 - Backlog" . "4")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args) (car test-case))))
        (let* ((reader (beads-reader-priority "Priority: "
                                              'beads-create--priority))
               (result (funcall reader nil nil nil)))
          (should (equal result (cdr test-case))))))))

;;; ============================================================
;;; Tests for beads-create Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-create-title-exists ()
  "Test that beads-reader-create-title is defined."
  (should (fboundp 'beads-reader-create-title)))

(ert-deftest beads-reader-test-create-title ()
  "Test reading title for issue creation."
  (let ((beads-create--title "Initial Title"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) "New Title")))
      (let ((result (beads-reader-create-title nil nil nil)))
        (should (equal result "New Title"))))))

(ert-deftest beads-reader-test-create-type-exists ()
  "Test that beads-reader-create-type is defined."
  (should (fboundp 'beads-reader-create-type)))

(ert-deftest beads-reader-test-create-type ()
  "Test reading type for issue creation."
  (let ((beads-create--type "bug"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "feature")))
      (let ((result (beads-reader-create-type nil nil nil)))
        (should (equal result "feature"))))))

(ert-deftest beads-reader-test-create-priority-exists ()
  "Test that beads-reader-create-priority is defined."
  (should (fboundp 'beads-reader-create-priority)))

(ert-deftest beads-reader-test-create-custom-id-exists ()
  "Test that beads-reader-create-custom-id is defined."
  (should (fboundp 'beads-reader-create-custom-id)))

(ert-deftest beads-reader-test-create-dependencies-exists ()
  "Test that beads-reader-create-dependencies is defined."
  (should (fboundp 'beads-reader-create-dependencies)))

(ert-deftest beads-reader-test-create-assignee-exists ()
  "Test that beads-reader-create-assignee is defined."
  (should (fboundp 'beads-reader-create-assignee)))

(ert-deftest beads-reader-test-create-external-ref-exists ()
  "Test that beads-reader-create-external-ref is defined."
  (should (fboundp 'beads-reader-create-external-ref)))

(ert-deftest beads-reader-test-create-labels-exists ()
  "Test that beads-reader-create-labels is defined."
  (should (fboundp 'beads-reader-create-labels)))

(ert-deftest beads-reader-test-create-parent-exists ()
  "Test that beads-reader-create-parent is defined."
  (should (fboundp 'beads-reader-create-parent)))

(ert-deftest beads-reader-test-create-repo-exists ()
  "Test that beads-reader-create-repo is defined."
  (should (fboundp 'beads-reader-create-repo)))

(ert-deftest beads-reader-test-create-from-template-exists ()
  "Test that beads-reader-create-from-template is defined."
  (should (fboundp 'beads-reader-create-from-template)))

(ert-deftest beads-reader-test-create-file-exists ()
  "Test that beads-reader-create-file is defined."
  (should (fboundp 'beads-reader-create-file)))

;;; ============================================================
;;; Tests for beads-update Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-update-status-exists ()
  "Test that beads-reader-update-status is defined."
  (should (fboundp 'beads-reader-update-status)))

(ert-deftest beads-reader-test-update-status ()
  "Test reading status for issue update."
  (let ((beads-update--status "open"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "in_progress")))
      (let ((result (beads-reader-update-status nil nil nil)))
        (should (equal result "in_progress"))))))

(ert-deftest beads-reader-test-update-priority-exists ()
  "Test that beads-reader-update-priority is defined."
  (should (fboundp 'beads-reader-update-priority)))

(ert-deftest beads-reader-test-update-type-exists ()
  "Test that beads-reader-update-type is defined."
  (should (fboundp 'beads-reader-update-type)))

(ert-deftest beads-reader-test-update-title-exists ()
  "Test that beads-reader-update-title is defined."
  (should (fboundp 'beads-reader-update-title)))

(ert-deftest beads-reader-test-update-assignee-exists ()
  "Test that beads-reader-update-assignee is defined."
  (should (fboundp 'beads-reader-update-assignee)))

(ert-deftest beads-reader-test-update-external-ref-exists ()
  "Test that beads-reader-update-external-ref is defined."
  (should (fboundp 'beads-reader-update-external-ref)))

;;; ============================================================
;;; Tests for beads-close Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-close-issue-id-exists ()
  "Test that beads-reader-close-issue-id is defined."
  (should (fboundp 'beads-reader-close-issue-id)))

(ert-deftest beads-reader-test-close-issue-id ()
  "Test reading issue ID to close."
  (let ((beads-close--issue-id "bd-1"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "bd-2"))
              ((symbol-function 'beads--issue-completion-table)
               (lambda () beads-reader-test--mock-issues)))
      (let ((result (beads-reader-close-issue-id nil nil nil)))
        (should (equal result "bd-2"))))))

;;; ============================================================
;;; Tests for beads-reopen Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-reopen-issue-id-exists ()
  "Test that beads-reader-reopen-issue-id is defined."
  (should (fboundp 'beads-reader-reopen-issue-id)))

(ert-deftest beads-reader-test-reopen-issue-id ()
  "Test reading issue ID to reopen."
  (let ((beads-reopen--issue-id "bd-1"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "bd-3"))
              ((symbol-function 'beads--issue-completion-table)
               (lambda () beads-reader-test--mock-issues)))
      (let ((result (beads-reader-reopen-issue-id nil nil nil)))
        (should (equal result "bd-3"))))))

;;; ============================================================
;;; Tests for beads-sync Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-sync-message-exists ()
  "Test that beads-reader-sync-message is defined."
  (should (fboundp 'beads-reader-sync-message)))

(ert-deftest beads-reader-test-sync-message ()
  "Test reading commit message for sync operation."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _args) "Sync changes")))
    (let ((result (beads-reader-sync-message nil nil nil)))
      (should (equal result "Sync changes")))))

;;; ============================================================
;;; Tests for beads-dep Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-dep-add-issue-id-exists ()
  "Test that beads-reader-dep-add-issue-id is defined."
  (should (fboundp 'beads-reader-dep-add-issue-id)))

(ert-deftest beads-reader-test-dep-add-depends-on-id-exists ()
  "Test that beads-reader-dep-add-depends-on-id is defined."
  (should (fboundp 'beads-reader-dep-add-depends-on-id)))

(ert-deftest beads-reader-test-dep-add-type-exists ()
  "Test that beads-reader-dep-add-type is defined."
  (should (fboundp 'beads-reader-dep-add-type)))

(ert-deftest beads-reader-test-dep-remove-issue-id-exists ()
  "Test that beads-reader-dep-remove-issue-id is defined."
  (should (fboundp 'beads-reader-dep-remove-issue-id)))

(ert-deftest beads-reader-test-dep-remove-depends-on-id-exists ()
  "Test that beads-reader-dep-remove-depends-on-id is defined."
  (should (fboundp 'beads-reader-dep-remove-depends-on-id)))

(ert-deftest beads-reader-test-dep-from-exists ()
  "Test that beads-reader-dep-from is defined."
  (should (fboundp 'beads-reader-dep-from)))

(ert-deftest beads-reader-test-dep-to-exists ()
  "Test that beads-reader-dep-to is defined."
  (should (fboundp 'beads-reader-dep-to)))

(ert-deftest beads-reader-test-dep-type-exists ()
  "Test that beads-reader-dep-type is defined."
  (should (fboundp 'beads-reader-dep-type)))

(ert-deftest beads-reader-test-dep-type ()
  "Test reading dependency type."
  (let ((beads-dep--dep-type "blocks"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "parent-child")))
      (let ((result (beads-reader-dep-type nil nil nil)))
        (should (equal result "parent-child"))))))

;;; ============================================================
;;; Tests for beads-misc (export/import/init) Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-export-output-exists ()
  "Test that beads-reader-export-output is defined."
  (should (fboundp 'beads-reader-export-output)))

(ert-deftest beads-reader-test-import-input-exists ()
  "Test that beads-reader-import-input is defined."
  (should (fboundp 'beads-reader-import-input)))

(ert-deftest beads-reader-test-init-prefix-exists ()
  "Test that beads-reader-init-prefix is defined."
  (should (fboundp 'beads-reader-init-prefix)))

(ert-deftest beads-reader-test-init-db-exists ()
  "Test that beads-reader-init-db is defined."
  (should (fboundp 'beads-reader-init-db)))

;;; ============================================================
;;; Tests for Edge Cases and Integration
;;; ============================================================

(ert-deftest beads-reader-test-priority-with-nil-default ()
  "Test priority reader when default variable is nil."
  (let ((beads-create--priority nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "3 - Low")))
      (let* ((reader (beads-reader-priority "Priority: "
                                            'beads-create--priority))
             (result (funcall reader nil nil nil)))
        (should (equal result "3"))))))

(ert-deftest beads-reader-test-file-with-mustmatch ()
  "Test file reader with mustmatch parameter."
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (&rest _args) "/existing/file")))
    (let* ((reader (beads-reader-file "File: " "/tmp/file" t))
           (result (funcall reader nil nil nil)))
      (should (equal result "/existing/file")))))

(ert-deftest beads-reader-test-choice-with-default ()
  "Test choice reader with default value."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "default-choice")))
    (let* ((reader (beads-reader-choice "Choose: "
                                        '("a" "b" "c")
                                        "default-choice"))
           (result (funcall reader nil nil nil)))
      (should (equal result "default-choice")))))

(ert-deftest beads-reader-test-create-title-with-nil-default ()
  "Test create-title with nil default value."
  (let ((beads-create--title nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) "New Title")))
      (let ((result (beads-reader-create-title nil nil nil)))
        (should (equal result "New Title"))))))

(provide 'beads-reader-test)
;;; beads-reader-test.el ends here
