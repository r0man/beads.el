;;; beads-delete-test.el --- Tests for beads-delete -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-delete.el.
;; Tests the preview-based deletion workflow.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-delete)

;; Load test utilities
(unless (featurep 'beads-test-helper)
  (load (expand-file-name "beads-test-helper"
                          (file-name-directory
                           (or load-file-name buffer-file-name)))))

;;; Test Fixtures

(defvar beads-delete-test--sample-issue
  '((id . "bd-42")
    (title . "Test Issue")
    (status . "open")
    (priority . 1))
  "Sample issue for testing.")

(defvar beads-delete-test--preview-output
  "Issue: bd-42 - Test Issue

Dependencies to be removed:
  bd-42 blocks bd-43
  bd-41 blocks bd-42

Text references to be updated:
  bd-41: Will update reference to [deleted:bd-42]
  bd-43: Will update reference to [deleted:bd-42]
"
  "Sample preview output.")

;;; ============================================================
;;; Preview Tests
;;; ============================================================

(ert-deftest beads-delete-test-get-preview-success ()
  "Test getting deletion preview successfully."
  (cl-letf (((symbol-function 'process-file)
             (beads-test-helper-mock-call-process
              0 beads-delete-test--preview-output)))
    (let ((preview (beads-delete--get-preview "bd-42")))
      (should (stringp preview))
      (should (string-match-p "bd-42" preview)))))

(ert-deftest beads-delete-test-get-preview-failure ()
  "Test getting preview handles bd command failure."
  (cl-letf (((symbol-function 'process-file)
             (beads-test-helper-mock-call-process 1 "Error: issue not found")))
    (should-error (beads-delete--get-preview "bd-999"))))

(ert-deftest beads-delete-test-show-preview-creates-buffer ()
  "Test that show-preview creates a buffer with preview text."
  (let* ((issue-id "bd-42")
         (preview-text "Test preview content")
         (buffer (beads-delete--show-preview issue-id preview-text)))
    (unwind-protect
        (progn
          (should (bufferp buffer))
          (should (string-match-p issue-id (buffer-name buffer)))
          (with-current-buffer buffer
            (should (string-match-p preview-text (buffer-string)))
            (should buffer-read-only)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest beads-delete-test-show-preview-content-only ()
  "Test that preview buffer shows only bd command output."
  (let* ((buffer (beads-delete--show-preview
                  "bd-42" "Test content from bd")))
    (unwind-protect
        (with-current-buffer buffer
          (should (string= "Test content from bd" (buffer-string))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;; ============================================================
;;; Execution Tests
;;; ============================================================

(ert-deftest beads-delete-test-execute-deletion-success ()
  "Test successful deletion execution."
  (let ((json-output (json-encode '((id . "bd-42")
                                    (deleted . t)))))
    (cl-letf (((symbol-function 'process-file)
               (beads-test-helper-mock-call-process 0 json-output))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (let ((result (beads-delete--execute-deletion "bd-42")))
        (should result)))))

(ert-deftest beads-delete-test-execute-deletion-command-failure ()
  "Test deletion handles bd command failure."
  (cl-letf (((symbol-function 'process-file)
             (beads-test-helper-mock-call-process 1 "Error")))
    (should-error (beads-delete--execute-deletion "bd-42"))))

(ert-deftest beads-delete-test-execute-invalidates-cache ()
  "Test that deletion invalidates completion cache."
  (let ((json-output (json-encode '((id . "bd-42") (deleted . t))))
        (cache-invalidated nil))
    (cl-letf (((symbol-function 'process-file)
               (beads-test-helper-mock-call-process 0 json-output))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () (setq cache-invalidated t))))
      (beads-delete--execute-deletion "bd-42")
      (should cache-invalidated))))

(ert-deftest beads-delete-test-execute-closes-show-buffer ()
  "Test that deletion closes the show buffer for deleted issue."
  (let ((json-output (json-encode '((id . "bd-42") (deleted . t)))))
    ;; Create a show buffer first
    (with-current-buffer (get-buffer-create "*beads-show bd-42*")
      (special-mode))
    (cl-letf (((symbol-function 'process-file)
               (beads-test-helper-mock-call-process 0 json-output))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-delete--execute-deletion "bd-42")
      (should-not (get-buffer "*beads-show bd-42*")))))

(ert-deftest beads-delete-test-execute-closes-preview-buffer ()
  "Test that deletion closes the preview buffer."
  (let ((json-output (json-encode '((id . "bd-42") (deleted . t)))))
    ;; Create a preview buffer first
    (with-current-buffer
        (get-buffer-create "*beads-delete-preview: bd-42*")
      (special-mode))
    (cl-letf (((symbol-function 'process-file)
               (beads-test-helper-mock-call-process 0 json-output))
              ((symbol-function 'beads--invalidate-completion-cache)
               (lambda () nil)))
      (beads-delete--execute-deletion "bd-42")
      (should-not (get-buffer "*beads-delete-preview: bd-42*")))))

;;; ============================================================
;;; Main Command Tests
;;; ============================================================

(ert-deftest beads-delete-test-command-defined ()
  "Test that beads-delete function is defined."
  (should (fboundp 'beads-delete)))

;;; ============================================================
;;; Context Detection Tests
;;; ============================================================

(ert-deftest beads-delete-test-detect-issue-id-from-list-mode ()
  "Test detecting issue ID from beads-list-mode."
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "bd-42")))
      (should (equal (beads-delete--detect-issue-id) "bd-42")))))

(ert-deftest beads-delete-test-detect-issue-id-from-show-mode ()
  "Test detecting issue ID from beads-show-mode."
  (require 'beads-show)
  (with-temp-buffer
    (beads-show-mode)
    (setq-local beads-show--issue-id "bd-99")
    (should (equal (beads-delete--detect-issue-id) "bd-99"))))

(ert-deftest beads-delete-test-detect-issue-id-from-buffer-name ()
  "Test detecting issue ID from buffer name."
  (with-current-buffer (get-buffer-create "*beads-show: bd-123*")
    (should (equal (beads-delete--detect-issue-id) "bd-123"))
    (kill-buffer)))

(ert-deftest beads-delete-test-detect-issue-id-no-context ()
  "Test detecting issue ID when no context available."
  (with-temp-buffer
    (should (null (beads-delete--detect-issue-id)))))

;;; ============================================================
;;; List View Integration Tests
;;; ============================================================

(ert-deftest beads-delete-test-list-delete-function-defined ()
  "Test that beads-list-delete function is defined."
  (require 'beads-list)
  (should (fboundp 'beads-list-delete)))

(ert-deftest beads-delete-test-list-delete-with-issue ()
  "Test deleting from list view with issue at point."
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (let ((beads-delete-called nil)
          (beads-delete-called-with nil))
      (cl-letf (((symbol-function 'beads-list--current-issue-id)
                 (lambda () "bd-42"))
                ((symbol-function 'beads-delete)
                 (lambda (id)
                   (setq beads-delete-called t)
                   (setq beads-delete-called-with id))))
        (beads-list-delete)
        (should beads-delete-called)
        (should (equal beads-delete-called-with "bd-42"))))))

(ert-deftest beads-delete-test-list-delete-no-issue ()
  "Test deleting from list view with no issue at point."
  (require 'beads-list)
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () nil)))
      (should-error (beads-list-delete) :type 'user-error))))

(ert-deftest beads-delete-test-list-keybinding-exists ()
  "Test that D keybinding exists in beads-list-mode-map."
  (require 'beads-list)
  (let ((binding (lookup-key beads-list-mode-map (kbd "D"))))
    (should (eq binding 'beads-list-delete))))

(ert-deftest beads-delete-test-list-delete-passes-id ()
  "Test that beads-list-delete passes the issue ID to beads-delete."
  (require 'beads-list)
  (let ((received-id nil))
    (cl-letf (((symbol-function 'beads-list--current-issue-id)
               (lambda () "test-123"))
              ((symbol-function 'beads-delete)
               (lambda (id) (setq received-id id))))
      (beads-list-delete)
      (should (equal received-id "test-123")))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(ert-deftest beads-delete-test-different-issue-id-formats ()
  "Test deletion with different issue ID formats."
  (let ((issue-ids '("bd-1" "bd-999" "worker-42" "test-123")))
    (dolist (id issue-ids)
      (let ((json-output (json-encode (list (cons 'id id)
                                            (cons 'deleted t)))))
        (cl-letf (((symbol-function 'process-file)
                   (beads-test-helper-mock-call-process 0 json-output))
                  ((symbol-function 'beads--invalidate-completion-cache)
                   (lambda () nil)))
          (should (beads-delete--execute-deletion id)))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-delete-test-integration-keybinding-exists ()
  "Integration test: Verify D keybinding exists in list mode."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "D"))))
      (should (eq binding 'beads-list-delete)))))

(provide 'beads-delete-test)
;;; beads-delete-test.el ends here
