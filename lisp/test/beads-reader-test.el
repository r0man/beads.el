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
(require 'beads-agent-backend)

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
  (let ((beads-update--priority 2))
    (let ((reader (beads-reader-priority "Priority: "
                                         'beads-update--priority)))
      (should (functionp reader)))))

(ert-deftest beads-reader-test-priority-converts-to-string ()
  "Test that beads-reader-priority converts number to string."
  (let ((beads-update--priority 2))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "1 - High")))
      (let* ((reader (beads-reader-priority "Priority: "
                                            'beads-update--priority))
             (result (funcall reader nil nil nil)))
        (should (stringp result))
        (should (equal result "1"))))))

(ert-deftest beads-reader-test-priority-all-levels ()
  "Test priority reader converts all levels correctly."
  (let ((beads-update--priority nil))
    (dolist (test-case '(("0 - Critical" . "0")
                         ("1 - High" . "1")
                         ("2 - Medium" . "2")
                         ("3 - Low" . "3")
                         ("4 - Backlog" . "4")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args) (car test-case))))
        (let* ((reader (beads-reader-priority "Priority: "
                                              'beads-update--priority))
               (result (funcall reader nil nil nil)))
          (should (equal result (cdr test-case))))))))

;;; ============================================================
;;; Tests for beads-create Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-create-title-exists ()
  "Test that beads-reader-issue-title is defined."
  (should (fboundp 'beads-reader-issue-title)))

(ert-deftest beads-reader-test-create-title ()
  "Test reading title for issue creation."
  (let ((beads-create--title "Initial Title"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) "New Title")))
      (let ((result (beads-reader-issue-title nil nil nil)))
        (should (equal result "New Title"))))))

(ert-deftest beads-reader-test-create-type-exists ()
  "Test that beads-reader-issue-type is defined."
  (should (fboundp 'beads-reader-issue-type)))

(ert-deftest beads-reader-test-create-type ()
  "Test reading type for issue creation."
  (let ((beads-create--type "bug"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "feature")))
      (let ((result (beads-reader-issue-type nil nil nil)))
        (should (equal result "feature"))))))

(ert-deftest beads-reader-test-create-priority-exists ()
  "Test that beads-reader-issue-priority is defined."
  (should (fboundp 'beads-reader-issue-priority)))

(ert-deftest beads-reader-test-create-custom-id-exists ()
  "Test that beads-reader-create-custom-id is defined."
  (should (fboundp 'beads-reader-create-custom-id)))

(ert-deftest beads-reader-test-create-dependencies-exists ()
  "Test that beads-reader-create-dependencies is defined."
  (should (fboundp 'beads-reader-create-dependencies)))

(ert-deftest beads-reader-test-create-assignee-exists ()
  "Test that beads-reader-issue-assignee is defined."
  (should (fboundp 'beads-reader-issue-assignee)))

(ert-deftest beads-reader-test-create-external-ref-exists ()
  "Test that beads-reader-issue-external-ref is defined."
  (should (fboundp 'beads-reader-issue-external-ref)))

(ert-deftest beads-reader-test-create-labels-exists ()
  "Test that beads-reader-issue-labels is defined."
  (should (fboundp 'beads-reader-issue-labels)))

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
;;; Tests for Export/Import/Init/Quickstart Reader Functions
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
  (let ((beads-update--priority nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "3 - Low")))
      (let* ((reader (beads-reader-priority "Priority: "
                                            'beads-update--priority))
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
      (let ((result (beads-reader-issue-title nil nil nil)))
        (should (equal result "New Title"))))))

;;; ============================================================
;;; Tests for Label Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-list-label-with-labels ()
  "Test reading list label with auto-completion when labels exist."
  (cl-letf (((symbol-function 'beads--label-completion-table)
             (lambda () '("backend" "frontend" "api")))
            ((symbol-function 'completing-read)
             (lambda (prompt table &rest _args)
               (should (equal prompt "Label (AND): "))
               (should (member "backend" table))
               "backend")))
    (let ((result (beads-reader-list-label nil nil nil)))
      (should (equal result "backend")))))

(ert-deftest beads-reader-test-list-label-no-labels ()
  "Test reading list label falls back to read-string when no labels."
  (cl-letf (((symbol-function 'beads--label-completion-table)
             (lambda () nil))
            ((symbol-function 'read-string)
             (lambda (prompt) "custom-label")))
    (let ((result (beads-reader-list-label nil nil nil)))
      (should (equal result "custom-label")))))

(ert-deftest beads-reader-test-list-labels-with-labels ()
  "Test reading multiple labels with auto-completion when labels exist."
  (cl-letf (((symbol-function 'beads--label-completion-table)
             (lambda () '("backend" "frontend" "api")))
            ((symbol-function 'completing-read-multiple)
             (lambda (prompt table &rest _args)
               (should (equal prompt "Labels (comma-separated): "))
               (should (member "backend" table))
               '("backend" "frontend"))))
    (let ((result (beads-reader-list-labels nil nil nil)))
      (should (equal result "backend,frontend")))))

(ert-deftest beads-reader-test-list-labels-no-labels ()
  "Test reading multiple labels falls back to read-string when no labels."
  (cl-letf (((symbol-function 'beads--label-completion-table)
             (lambda () nil))
            ((symbol-function 'read-string)
             (lambda (prompt) "custom1,custom2")))
    (let ((result (beads-reader-list-labels nil nil nil)))
      (should (equal result "custom1,custom2")))))

(ert-deftest beads-reader-test-issue-labels-with-labels ()
  "Test reading issue labels with auto-completion when labels exist."
  (cl-letf (((symbol-function 'beads--label-completion-table)
             (lambda () '("bug" "urgent" "security")))
            ((symbol-function 'completing-read-multiple)
             (lambda (prompt table &rest _args)
               (should (equal prompt "Labels (comma-separated): "))
               (should (member "bug" table))
               '("bug" "urgent"))))
    (let ((result (beads-reader-issue-labels nil nil nil)))
      (should (equal result "bug,urgent")))))

(ert-deftest beads-reader-test-issue-labels-no-labels ()
  "Test reading issue labels falls back to read-string when no labels."
  (cl-letf (((symbol-function 'beads--label-completion-table)
             (lambda () nil))
            ((symbol-function 'read-string)
             (lambda (prompt) "label1,label2,label3")))
    (let ((result (beads-reader-issue-labels nil nil nil)))
      (should (equal result "label1,label2,label3")))))

(ert-deftest beads-reader-test-issue-labels-empty-selection ()
  "Test reading issue labels when user selects nothing."
  (cl-letf (((symbol-function 'beads--label-completion-table)
             (lambda () '("backend" "frontend")))
            ((symbol-function 'completing-read-multiple)
             (lambda (&rest _args) '())))
    (let ((result (beads-reader-issue-labels nil nil nil)))
      (should (equal result "")))))

;;; ============================================================
;;; Tests for Label Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-label-issue-ids-detected ()
  "Test label issue IDs reader with detected issue from context."
  (cl-letf (((symbol-function 'beads-label--detect-issue-id)
             (lambda () "bd-42")))
    (let ((result (beads-reader-label-issue-ids nil nil nil)))
      (should (equal result "bd-42")))))

(ert-deftest beads-reader-test-label-issue-ids-single ()
  "Test label issue IDs reader selecting single issue."
  (cl-letf (((symbol-function 'beads-label--detect-issue-id)
             (lambda () nil))
            ((symbol-function 'beads--issue-completion-table)
             (lambda () '("bd-1" "bd-2" "bd-3")))
            ((symbol-function 'completing-read-multiple)
             (lambda (&rest _args) '("bd-1"))))
    (let ((result (beads-reader-label-issue-ids nil nil nil)))
      (should (equal result "bd-1")))))

(ert-deftest beads-reader-test-label-issue-ids-multiple ()
  "Test label issue IDs reader selecting multiple issues."
  (cl-letf (((symbol-function 'beads-label--detect-issue-id)
             (lambda () nil))
            ((symbol-function 'beads--issue-completion-table)
             (lambda () '("bd-1" "bd-2" "bd-3")))
            ((symbol-function 'completing-read-multiple)
             (lambda (&rest _args) '("bd-1" "bd-2" "bd-3"))))
    (let ((result (beads-reader-label-issue-ids nil nil nil)))
      (should (equal result "bd-1,bd-2,bd-3")))))

(ert-deftest beads-reader-test-label-issue-ids-empty ()
  "Test label issue IDs reader when nothing selected."
  (cl-letf (((symbol-function 'beads-label--detect-issue-id)
             (lambda () nil))
            ((symbol-function 'beads--issue-completion-table)
             (lambda () '("bd-1" "bd-2" "bd-3")))
            ((symbol-function 'completing-read-multiple)
             (lambda (&rest _args) '())))
    (let ((result (beads-reader-label-issue-ids nil nil nil)))
      (should (equal result "")))))

(ert-deftest beads-reader-test-label-name-with-labels ()
  "Test label name reader with available labels."
  (cl-letf (((symbol-function 'beads--label-completion-table)
             (lambda () '("backend" "frontend" "urgent")))
            ((symbol-function 'completing-read)
             (lambda (&rest _args) "backend")))
    (let ((result (beads-reader-label-name nil nil nil)))
      (should (equal result "backend")))))

(ert-deftest beads-reader-test-label-name-no-labels ()
  "Test label name reader falls back when no labels available."
  (cl-letf (((symbol-function 'beads--label-completion-table)
             (lambda () nil))
            ((symbol-function 'read-string)
             (lambda (&rest _args) "new-label")))
    (let ((result (beads-reader-label-name nil nil nil)))
      (should (equal result "new-label")))))

;;; ============================================================
;;; Agent Reader Tests
;;; ============================================================

;; Mock backend for testing
(defclass beads-reader-test--mock-backend (beads-agent-backend)
  ((available :initarg :available :initform t))
  :documentation "Mock backend for reader tests.")

(cl-defmethod beads-agent-backend-available-p
  ((backend beads-reader-test--mock-backend))
  "Return availability status for mock BACKEND."
  (oref backend available))

(defun beads-reader-test--make-mock-backends ()
  "Create mock backends for testing."
  (list
   (beads-reader-test--mock-backend
    :name "test-backend" :priority 10 :available t)
   (beads-reader-test--mock-backend
    :name "unavailable-backend" :priority 99 :available nil)))

(ert-deftest beads-reader-test-agent-backend ()
  "Test agent backend reader with available backends."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             #'beads-reader-test--make-mock-backends)
            ((symbol-function 'beads-agent--get-available-backends)
             (lambda ()
               (seq-filter #'beads-agent-backend-available-p
                           (beads-reader-test--make-mock-backends))))
            ((symbol-function 'completing-read)
             (lambda (_prompt _table &rest _args)
               "test-backend")))
    (let ((result (beads-reader-agent-backend nil nil nil)))
      (should (equal result "test-backend")))))

(ert-deftest beads-reader-test-agent-backend-no-backends ()
  "Test agent backend reader with no registered backends."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             (lambda () nil))
            ((symbol-function 'beads-agent--get-available-backends)
             (lambda () nil)))
    (should-error (beads-reader-agent-backend nil nil nil)
                  :type 'user-error)))

(ert-deftest beads-reader-test-agent-backend-none-available ()
  "Test agent backend reader with backends registered but none available."
  (cl-letf (((symbol-function 'beads-agent--get-all-backends)
             (lambda ()
               (list (beads-reader-test--mock-backend
                      :name "unavailable" :priority 10 :available nil))))
            ((symbol-function 'beads-agent--get-available-backends)
             (lambda () nil)))
    (should-error (beads-reader-agent-backend nil nil nil)
                  :type 'user-error)))

(ert-deftest beads-reader-test-agent-backend-predicate-filters ()
  "Test that completion predicate filters to available backends only."
  (let ((predicate-called nil)
        (predicate-result nil))
    (cl-letf (((symbol-function 'beads-agent--get-all-backends)
               #'beads-reader-test--make-mock-backends)
              ((symbol-function 'beads-agent--get-available-backends)
               (lambda ()
                 (seq-filter #'beads-agent-backend-available-p
                             (beads-reader-test--make-mock-backends))))
              ((symbol-function 'completing-read)
               (lambda (_prompt _table pred &rest _args)
                 ;; Test the predicate
                 (setq predicate-called t)
                 (setq predicate-result
                       (list (funcall pred "test-backend")
                             (funcall pred "unavailable-backend")))
                 "test-backend")))
      (beads-reader-agent-backend nil nil nil)
      (should predicate-called)
      ;; First should be truthy (available), second should be nil (unavailable)
      (should (car predicate-result))
      (should-not (cadr predicate-result)))))

(ert-deftest beads-reader-test-agent-backend-respects-show-unavailable-setting ()
  "Test that reader respects beads-completion-show-unavailable-backends."
  (let ((table-called-with-setting nil))
    (cl-letf (((symbol-function 'beads-agent--get-all-backends)
               #'beads-reader-test--make-mock-backends)
              ((symbol-function 'beads-agent--get-available-backends)
               (lambda ()
                 (seq-filter #'beads-agent-backend-available-p
                             (beads-reader-test--make-mock-backends))))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _args)
                 ;; Get candidates from the table
                 (let ((candidates (all-completions "" table nil)))
                   (setq table-called-with-setting (length candidates)))
                 "test-backend")))
      ;; Test with show-unavailable = nil
      (let ((beads-completion-show-unavailable-backends nil))
        (beads-reader-agent-backend nil nil nil)
        ;; Should only see available backend
        (should (= 1 table-called-with-setting)))
      ;; Test with show-unavailable = t
      (let ((beads-completion-show-unavailable-backends t))
        (beads-reader-agent-backend nil nil nil)
        ;; Should see all backends
        (should (= 2 table-called-with-setting))))))

;;; ============================================================
;;; Tests for List Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-list-assignee ()
  "Test reading assignee for list filter."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "john.doe")))
    (let ((result (beads-reader-list-assignee nil nil nil)))
      (should (equal result "john.doe")))))

(ert-deftest beads-reader-test-list-date ()
  "Test reading date for list filter."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "2025-01-15")))
    (let ((result (beads-reader-list-date nil nil nil)))
      (should (equal result "2025-01-15")))))

(ert-deftest beads-reader-test-list-date-rfc3339 ()
  "Test reading RFC3339 date for list filter."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "2025-01-15T10:30:00Z")))
    (let ((result (beads-reader-list-date nil nil nil)))
      (should (equal result "2025-01-15T10:30:00Z")))))

(ert-deftest beads-reader-test-list-desc-contains ()
  "Test reading description search text for list filter."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "authentication")))
    (let ((result (beads-reader-list-desc-contains nil nil nil)))
      (should (equal result "authentication")))))

(ert-deftest beads-reader-test-list-format ()
  "Test reading output format for list."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "digraph")))
    (let ((result (beads-reader-list-format nil nil nil)))
      (should (equal result "digraph")))))

(ert-deftest beads-reader-test-list-format-dot ()
  "Test reading dot output format for list."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "dot")))
    (let ((result (beads-reader-list-format nil nil nil)))
      (should (equal result "dot")))))

(ert-deftest beads-reader-test-list-id ()
  "Test reading comma-separated issue IDs for list filter."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "bd-1,bd-2,bd-3")))
    (let ((result (beads-reader-list-id nil nil nil)))
      (should (equal result "bd-1,bd-2,bd-3")))))

(ert-deftest beads-reader-test-list-limit-number ()
  "Test reading limit as number for list results."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "50")))
    (let ((result (beads-reader-list-limit nil nil nil)))
      (should (equal result "50")))))

(ert-deftest beads-reader-test-list-limit-empty ()
  "Test reading empty limit for list results."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "")))
    (let ((result (beads-reader-list-limit nil nil nil)))
      (should (equal result "")))))

(ert-deftest beads-reader-test-list-notes-contains ()
  "Test reading notes search text for list filter."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "meeting notes")))
    (let ((result (beads-reader-list-notes-contains nil nil nil)))
      (should (equal result "meeting notes")))))

(ert-deftest beads-reader-test-list-priority ()
  "Test reading priority for list filter."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "2 - Medium")))
    (let ((result (beads-reader-list-priority nil nil nil)))
      (should (equal result "2")))))

(ert-deftest beads-reader-test-list-priority-critical ()
  "Test reading critical priority for list filter."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "0 - Critical")))
    (let ((result (beads-reader-list-priority nil nil nil)))
      (should (equal result "0")))))

(ert-deftest beads-reader-test-list-priority-min ()
  "Test reading minimum priority for list filter."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "1 - High")))
    (let ((result (beads-reader-list-priority-min nil nil nil)))
      (should (equal result "1")))))

(ert-deftest beads-reader-test-list-priority-max ()
  "Test reading maximum priority for list filter."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "4 - Backlog")))
    (let ((result (beads-reader-list-priority-max nil nil nil)))
      (should (equal result "4")))))

(ert-deftest beads-reader-test-list-status ()
  "Test reading status for list filter."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "in_progress")))
    (let ((result (beads-reader-list-status nil nil nil)))
      (should (equal result "in_progress")))))

(ert-deftest beads-reader-test-list-status-blocked ()
  "Test reading blocked status for list filter."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "blocked")))
    (let ((result (beads-reader-list-status nil nil nil)))
      (should (equal result "blocked")))))

(ert-deftest beads-reader-test-list-title ()
  "Test reading title text for list filter."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "Bug in login")))
    (let ((result (beads-reader-list-title nil nil nil)))
      (should (equal result "Bug in login")))))

(ert-deftest beads-reader-test-list-title-contains ()
  "Test reading title search text for list filter."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "authentication")))
    (let ((result (beads-reader-list-title-contains nil nil nil)))
      (should (equal result "authentication")))))

(ert-deftest beads-reader-test-list-type ()
  "Test reading type for list filter."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "bug")))
    (let ((result (beads-reader-list-type nil nil nil)))
      (should (equal result "bug")))))

(ert-deftest beads-reader-test-list-type-feature ()
  "Test reading feature type for list filter."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "feature")))
    (let ((result (beads-reader-list-type nil nil nil)))
      (should (equal result "feature")))))

(ert-deftest beads-reader-test-list-type-epic ()
  "Test reading epic type for list filter."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "epic")))
    (let ((result (beads-reader-list-type nil nil nil)))
      (should (equal result "epic")))))

;;; ============================================================
;;; Tests for Create Reader Functions (additional)
;;; ============================================================

(ert-deftest beads-reader-test-create-custom-id ()
  "Test reading custom ID for issue creation."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "custom-123")))
    (let ((result (beads-reader-create-custom-id nil nil nil)))
      (should (equal result "custom-123")))))

(ert-deftest beads-reader-test-create-dependencies ()
  "Test reading dependencies for issue creation."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "blocks:bd-1,related:bd-2")))
    (let ((result (beads-reader-create-dependencies nil nil nil)))
      (should (equal result "blocks:bd-1,related:bd-2")))))

(ert-deftest beads-reader-test-create-repo ()
  "Test reading target repository for issue creation."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "my-org/my-repo")))
    (let ((result (beads-reader-create-repo nil nil nil)))
      (should (equal result "my-org/my-repo")))))

(ert-deftest beads-reader-test-create-from-template ()
  "Test reading template name for issue creation."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "epic")))
    (let ((result (beads-reader-create-from-template nil nil nil)))
      (should (equal result "epic")))))

(ert-deftest beads-reader-test-create-file ()
  "Test reading markdown file path for bulk issue creation."
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (_prompt _dir _default _mustmatch) "/path/to/issues.md")))
    (let ((result (beads-reader-create-file nil nil nil)))
      (should (equal result "/path/to/issues.md")))))

(ert-deftest beads-reader-test-issue-assignee ()
  "Test reading assignee for issue."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "jane.smith")))
    (let ((result (beads-reader-issue-assignee nil nil nil)))
      (should (equal result "jane.smith")))))

(ert-deftest beads-reader-test-issue-external-ref ()
  "Test reading external reference for issue."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "gh-42")))
    (let ((result (beads-reader-issue-external-ref nil nil nil)))
      (should (equal result "gh-42")))))

(ert-deftest beads-reader-test-issue-external-ref-jira ()
  "Test reading JIRA external reference for issue."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "PROJ-123")))
    (let ((result (beads-reader-issue-external-ref nil nil nil)))
      (should (equal result "PROJ-123")))))

(ert-deftest beads-reader-test-issue-priority ()
  "Test reading priority for issue."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "2 - Medium")))
    (let ((result (beads-reader-issue-priority nil nil nil)))
      (should (equal result "2")))))

(ert-deftest beads-reader-test-issue-priority-high ()
  "Test reading high priority for issue."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "1 - High")))
    (let ((result (beads-reader-issue-priority nil nil nil)))
      (should (equal result "1")))))

;;; ============================================================
;;; Tests for Export/Import/Init Reader Functions (additional)
;;; ============================================================

(ert-deftest beads-reader-test-export-output ()
  "Test reading output file path for export operation."
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (_prompt _dir _default) "/tmp/export.jsonl")))
    (let ((result (beads-reader-export-output nil nil nil)))
      (should (equal result "/tmp/export.jsonl")))))

(ert-deftest beads-reader-test-import-input ()
  "Test reading input file path for import operation."
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (_prompt _dir _default _mustmatch) "/data/import.jsonl")))
    (let ((result (beads-reader-import-input nil nil nil)))
      (should (equal result "/data/import.jsonl")))))

(ert-deftest beads-reader-test-init-prefix ()
  "Test reading issue ID prefix for init operation."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt _initial) "myproj")))
    (let ((result (beads-reader-init-prefix nil nil nil)))
      (should (equal result "myproj")))))

(ert-deftest beads-reader-test-init-db ()
  "Test reading database path for init operation."
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (_prompt _dir _default) "/path/to/beads.db")))
    (let ((result (beads-reader-init-db nil nil nil)))
      (should (equal result "/path/to/beads.db")))))

;;; ============================================================
;;; Tests for Dependency Reader Functions (additional)
;;; ============================================================

(ert-deftest beads-reader-test-dep-add-type ()
  "Test reading dependency type for add operation."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "blocks")))
    (let ((result (beads-reader-dep-add-type "Type: " nil nil)))
      (should (equal result "blocks")))))

(ert-deftest beads-reader-test-dep-add-type-related ()
  "Test reading related dependency type for add operation."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "related")))
    (let ((result (beads-reader-dep-add-type "Type: " nil nil)))
      (should (equal result "related")))))

(ert-deftest beads-reader-test-dep-add-type-parent-child ()
  "Test reading parent-child dependency type for add operation."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _choices &rest _args) "parent-child")))
    (let ((result (beads-reader-dep-add-type "Type: " nil nil)))
      (should (equal result "parent-child")))))

(ert-deftest beads-reader-test-dep-from ()
  "Test reading source issue ID for dependency operations."
  (let ((beads-dep--from-issue "bd-1"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "bd-5"))
              ((symbol-function 'beads--issue-completion-table)
               (lambda () beads-reader-test--mock-issues)))
      (let ((result (beads-reader-dep-from nil nil nil)))
        (should (equal result "bd-5"))))))

(ert-deftest beads-reader-test-dep-to ()
  "Test reading target issue ID for dependency operations."
  (let ((beads-dep--to-issue "bd-2"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) "bd-10"))
              ((symbol-function 'beads--issue-completion-table)
               (lambda () beads-reader-test--mock-issues)))
      (let ((result (beads-reader-dep-to nil nil nil)))
        (should (equal result "bd-10"))))))

;;; ============================================================
;;; Tests for Worktree Reader Functions
;;; ============================================================

(ert-deftest beads-reader-test-worktree-name-exists ()
  "Test that beads-reader-worktree-name is defined."
  (should (fboundp 'beads-reader-worktree-name)))

(ert-deftest beads-reader-test-worktree-name ()
  "Test reading worktree name suggests issue IDs."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "bd-123"))
            ((symbol-function 'beads--issue-completion-table)
             (lambda () beads-reader-test--mock-issues)))
    (let ((result (beads-reader-worktree-name nil nil nil)))
      (should (equal result "bd-123")))))

(ert-deftest beads-reader-test-worktree-name-custom ()
  "Test reading worktree name with custom input."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "feature-auth"))
            ((symbol-function 'beads--issue-completion-table)
             (lambda () beads-reader-test--mock-issues)))
    (let ((result (beads-reader-worktree-name nil nil nil)))
      (should (equal result "feature-auth")))))

(ert-deftest beads-reader-test-worktree-branch-exists ()
  "Test that beads-reader-worktree-branch is defined."
  (should (fboundp 'beads-reader-worktree-branch)))

(ert-deftest beads-reader-test-worktree-branch ()
  "Test reading worktree branch from git branches."
  (cl-letf (((symbol-function 'beads-reader--get-git-branches)
             (lambda () '("main" "develop" "feature/auth")))
            ((symbol-function 'completing-read)
             (lambda (_prompt branches &rest _args)
               (should (member "main" branches))
               (should (member "develop" branches))
               "develop")))
    (let ((result (beads-reader-worktree-branch nil nil nil)))
      (should (equal result "develop")))))

(ert-deftest beads-reader-test-worktree-branch-empty ()
  "Test reading worktree branch when no branches available."
  (cl-letf (((symbol-function 'beads-reader--get-git-branches)
             (lambda () nil))
            ((symbol-function 'completing-read)
             (lambda (_prompt branches &rest _args)
               (should (null branches))
               "new-branch")))
    (let ((result (beads-reader-worktree-branch nil nil nil)))
      (should (equal result "new-branch")))))

(ert-deftest beads-reader-test-get-git-branches-exists ()
  "Test that beads-reader--get-git-branches is defined."
  (should (fboundp 'beads-reader--get-git-branches)))

(ert-deftest beads-reader-test-get-git-branches-parses-output ()
  "Test that beads-reader--get-git-branches parses git branch output."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile destination _display &rest _args)
               ;; When destination is t, insert into current buffer
               (when (eq destination t)
                 (insert "* main\n")
                 (insert "  develop\n")
                 (insert "+ feature/auth\n")
                 (insert "  bugfix/login\n"))
               0)))
    (let ((branches (beads-reader--get-git-branches)))
      (should (member "main" branches))
      (should (member "develop" branches))
      (should (member "feature/auth" branches))
      (should (member "bugfix/login" branches))
      (should (= 4 (length branches))))))

(ert-deftest beads-reader-test-get-git-branches-strips-markers ()
  "Test that beads-reader--get-git-branches strips branch markers."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile destination _display &rest _args)
               ;; When destination is t, insert into current buffer
               (when (eq destination t)
                 (insert "* main\n")
                 (insert "+ worktree-branch\n"))
               0)))
    (let ((branches (beads-reader--get-git-branches)))
      ;; Should not contain * or + markers
      (should-not (seq-find (lambda (b) (string-prefix-p "*" b)) branches))
      (should-not (seq-find (lambda (b) (string-prefix-p "+" b)) branches)))))

(ert-deftest beads-reader-test-get-git-branches-handles-error ()
  "Test that beads-reader--get-git-branches handles git errors gracefully."
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _args)
               (error "Git not found"))))
    (let ((branches (beads-reader--get-git-branches)))
      (should (null branches)))))

(ert-deftest beads-reader-test-get-git-branches-handles-non-zero-exit ()
  "Test that beads-reader--get-git-branches handles non-zero exit code."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile destination _display &rest _args)
               ;; When destination is t, insert into current buffer
               (when (eq destination t)
                 (insert "fatal: not a git repository\n"))
               128)))
    (let ((branches (beads-reader--get-git-branches)))
      (should (null branches)))))

(ert-deftest beads-reader-test-worktree-existing-exists ()
  "Test that beads-reader-worktree-existing is defined."
  (should (fboundp 'beads-reader-worktree-existing)))

(ert-deftest beads-reader-test-worktree-existing ()
  "Test reading existing worktree name."
  (cl-letf (((symbol-function 'beads-completion-read-worktree)
             (lambda (prompt &rest _args)
               (should (string-match-p "Worktree:" prompt))
               "feature-auth")))
    (let ((result (beads-reader-worktree-existing nil nil nil)))
      (should (equal result "feature-auth")))))

(ert-deftest beads-reader-test-worktree-existing-requires-match ()
  "Test that worktree-existing requires exact match."
  (let ((require-match-arg nil))
    (cl-letf (((symbol-function 'beads-completion-read-worktree)
               (lambda (_prompt _pred require-match &rest _args)
                 (setq require-match-arg require-match)
                 "beads.el")))
      (beads-reader-worktree-existing nil nil nil)
      (should require-match-arg))))

(provide 'beads-reader-test)
;;; beads-reader-test.el ends here
