;;; beads-integration-test.el --- Integration tests for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Integration tests that use real bd CLI and temporary projects.
;; Each test creates its own isolated project in a temp directory.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-list)
(require 'beads-delete)

;;; Test Helpers

(defvar beads-integration-test--temp-dirs nil
  "List of temporary directories created during tests for cleanup.")

(defun beads-integration-test--create-temp-project ()
  "Create a temporary project directory with beads initialized.
Returns the project directory path."
  (let* ((temp-dir (make-temp-file "beads-test-" t))
         (db-path (expand-file-name ".beads/test.db" temp-dir)))
    ;; Track for cleanup
    (push temp-dir beads-integration-test--temp-dirs)
    ;; Initialize beads in the temp directory
    (let ((default-directory temp-dir))
      (call-process "bd" nil nil nil "init" "--prefix" "test"))
    temp-dir))

(defun beads-integration-test--cleanup-temp-projects ()
  "Clean up all temporary project directories."
  (dolist (dir beads-integration-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq beads-integration-test--temp-dirs nil))

(defun beads-integration-test--create-issue (project-dir title)
  "Create an issue in PROJECT-DIR with TITLE.
Returns the issue ID."
  (let ((default-directory project-dir))
    (with-temp-buffer
      (call-process "bd" nil t nil "create" title "--json")
      (goto-char (point-min))
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (result (json-read)))
        (alist-get 'id result)))))

(defun beads-integration-test--issue-exists-p (project-dir issue-id)
  "Check if ISSUE-ID exists in PROJECT-DIR.
Returns non-nil if the issue exists."
  (let ((default-directory project-dir))
    (with-temp-buffer
      (let ((exit-code (call-process "bd" nil t nil "show" issue-id "--json")))
        (= exit-code 0)))))

(defun beads-integration-test--delete-issue (project-dir issue-id)
  "Delete ISSUE-ID in PROJECT-DIR using bd CLI directly."
  (let ((default-directory project-dir))
    (call-process "bd" nil nil nil "delete" "--force" issue-id)))

;;; Integration Tests

(ert-deftest beads-integration-test-delete-from-list-view ()
  "Integration test: Create issue, delete from list view, verify deletion."
  :tags '(integration)
  (skip-unless (executable-find "bd"))

  (let ((project-dir (beads-integration-test--create-temp-project))
        (issue-id nil)
        (test-passed nil))

    (unwind-protect
        (progn
          ;; Step 1: Create a test issue
          (setq issue-id (beads-integration-test--create-issue
                          project-dir "Test issue for deletion"))
          (should issue-id)
          (should (beads-integration-test--issue-exists-p project-dir issue-id))

          ;; Step 2: Open list view in the project
          (let ((default-directory project-dir)
                (beads-auto-refresh nil)) ; Disable auto-refresh for testing

            ;; Get the list of issues
            (beads-list)
            (switch-to-buffer "*beads-list*")

            ;; Verify we're in list mode
            (should (derived-mode-p 'beads-list-mode))

            ;; Find the issue in the list
            (goto-char (point-min))
            (should (search-forward issue-id nil t))
            (beginning-of-line)

            ;; Step 3: Verify the issue ID is detected
            (let ((detected-id (beads-list--current-issue-id)))
              (should (equal detected-id issue-id))

              ;; Step 4: Call beads-delete (simulating D key press)
              (let ((delete-called nil)
                    (delete-called-with nil))
                ;; Mock the transient to capture what happens
                (cl-letf (((symbol-function 'beads-delete)
                           (lambda (id)
                             (setq delete-called t)
                             (setq delete-called-with id)
                             ;; Actually delete the issue
                             (beads-integration-test--delete-issue
                              project-dir id))))

                  ;; Simulate pressing D
                  (beads-list-delete)

                  ;; Verify beads-delete was called with correct ID
                  (should delete-called)
                  (should (equal delete-called-with issue-id))))))

          ;; Step 5: Verify the issue was actually deleted
          (should-not (beads-integration-test--issue-exists-p
                       project-dir issue-id))

          (setq test-passed t))

      ;; Cleanup
      (when (get-buffer "*beads-list*")
        (kill-buffer "*beads-list*"))
      (beads-integration-test--cleanup-temp-projects)

      ;; Final assertion
      (should test-passed))))

(ert-deftest beads-integration-test-delete-with-force-flag ()
  "Integration test: Delete with force flag using transient."
  :tags '(integration)
  (skip-unless (executable-find "bd"))

  (let ((project-dir (beads-integration-test--create-temp-project))
        (issue-id nil))

    (unwind-protect
        (let ((default-directory project-dir))
          ;; Create a test issue
          (setq issue-id (beads-integration-test--create-issue
                          project-dir "Test force deletion"))
          (should issue-id)
          (should (beads-integration-test--issue-exists-p project-dir issue-id))

          ;; Reset delete state
          (beads-delete--reset-state)

          ;; Set up the delete
          (setq beads-delete--issue-id issue-id)
          (setq beads-delete--force t)

          ;; Mock confirmation
          (cl-letf (((symbol-function 'beads-delete--confirm-deletion)
                     (lambda (_id) t)))

            ;; Execute deletion
            (beads-delete--execute-deletion)

            ;; Verify issue is deleted
            (should-not (beads-integration-test--issue-exists-p
                         project-dir issue-id))))

      ;; Cleanup
      (beads-integration-test--cleanup-temp-projects))))

(ert-deftest beads-integration-test-delete-keybinding-exists ()
  "Integration test: Verify D keybinding exists in list mode."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "D"))))
      (should (eq binding 'beads-list-delete)))))

;;; Cleanup hook

(defun beads-integration-test--cleanup-all ()
  "Cleanup function to run after all tests."
  (beads-integration-test--cleanup-temp-projects))

;; Register cleanup
(add-hook 'ert-runner-reporter-run-ended-functions
          #'beads-integration-test--cleanup-all)

(provide 'beads-integration-test)
;;; beads-integration-test.el ends here
