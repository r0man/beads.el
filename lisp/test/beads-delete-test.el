;;; beads-delete-test.el --- Tests for beads-delete -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-delete.el.
;; Tests cover all aspects of the bd delete command interface.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-delete)

;; Load test utilities
(unless (featurep 'beads-testing)
  (load (expand-file-name "beads-testing"
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

;;; Test Utilities

(defmacro beads-delete-test-with-state (state &rest body)
  "Execute BODY with beads-delete state set to STATE."
  (declare (indent 1))
  `(progn
     (setq beads-delete--issue-id nil)
     ,@(mapcar (lambda (binding)
                 `(setq ,(car binding) ,(cdr binding)))
               (eval state))
     ,@body))

;;; ============================================================
;;; State Management Tests
;;; ============================================================

(ert-deftest beads-delete-test-reset-state ()
  "Test that reset-state clears all variables."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . "bd-42")
     (beads-delete--force . t))
   (beads-delete--reset-state)
   (should (null beads-delete--issue-id))
   (should (null beads-delete--force))))

;;; ============================================================
;;; Validation Tests
;;; ============================================================

(ert-deftest beads-delete-test-validate-issue-id-nil ()
  "Test issue ID validation when nil."
  (beads-delete-test-with-state nil
   (should (beads-delete--validate-issue-id))))

(ert-deftest beads-delete-test-validate-issue-id-empty ()
  "Test issue ID validation when empty."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . ""))
   (should (beads-delete--validate-issue-id))))

(ert-deftest beads-delete-test-validate-issue-id-valid ()
  "Test issue ID validation when valid."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . "bd-42"))
   (should (null (beads-delete--validate-issue-id)))))

(ert-deftest beads-delete-test-validate-all-success ()
  "Test validate-all with valid parameters."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . "bd-42"))
   (should (null (beads-delete--validate-all)))))

(ert-deftest beads-delete-test-validate-all-failure ()
  "Test validate-all with missing parameters."
  (beads-delete-test-with-state nil
   (should (beads-delete--validate-all))))

;;; ============================================================
;;; Command Building Tests
;;; ============================================================

(ert-deftest beads-delete-test-build-command-args-without-force ()
  "Test building command arguments without force flag."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . "bd-42")
     (beads-delete--force . nil))
   (let ((args (beads-delete--build-command-args)))
     (should (equal args '("bd-42"))))))

(ert-deftest beads-delete-test-build-command-args-with-force ()
  "Test building command arguments with force flag."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . "bd-42")
     (beads-delete--force . t))
   (let ((args (beads-delete--build-command-args)))
     (should (equal args '("--force" "bd-42"))))))

;;; ============================================================
;;; Confirmation Tests
;;; ============================================================

(ert-deftest beads-delete-test-confirm-with-yes ()
  "Test confirmation with 'yes' response."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "yes")))
    (should (beads-delete--confirm-deletion "bd-42"))))

(ert-deftest beads-delete-test-confirm-with-issue-id ()
  "Test confirmation with exact issue ID."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "bd-42")))
    (should (beads-delete--confirm-deletion "bd-42"))))

(ert-deftest beads-delete-test-confirm-with-wrong-response ()
  "Test confirmation with wrong response."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "no")))
    (should-not (beads-delete--confirm-deletion "bd-42"))))

(ert-deftest beads-delete-test-confirm-with-wrong-issue-id ()
  "Test confirmation with wrong issue ID."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "bd-99")))
    (should-not (beads-delete--confirm-deletion "bd-42"))))

(ert-deftest beads-delete-test-confirm-case-sensitive ()
  "Test confirmation is case-sensitive."
  (cl-letf (((symbol-function 'read-string)
             (lambda (_prompt) "YES")))
    (should-not (beads-delete--confirm-deletion "bd-42"))))

;;; ============================================================
;;; Execution Tests
;;; ============================================================

(ert-deftest beads-delete-test-execute-deletion-success ()
  "Test successful deletion execution."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . "bd-42"))
   (let ((json-output (json-encode '((id . "bd-42")
                                    (deleted . t)))))
     (cl-letf (((symbol-function 'call-process)
                (beads-testing-mock-call-process 0 json-output))
               ((symbol-function 'read-string) (lambda (_prompt) "yes"))
               ((symbol-function 'beads--invalidate-completion-cache)
                (lambda () nil)))
       (let ((result (beads-delete--execute-deletion)))
         (should result))))))

(ert-deftest beads-delete-test-execute-deletion-cancelled ()
  "Test deletion cancelled by user."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . "bd-42"))
   (cl-letf (((symbol-function 'read-string) (lambda (_prompt) "no")))
     (should-error (beads-delete--execute-deletion)
                   :type 'user-error))))

(ert-deftest beads-delete-test-execute-deletion-command-failure ()
  "Test deletion handles bd command failure."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . "bd-42"))
   (cl-letf (((symbol-function 'call-process)
              (beads-testing-mock-call-process 1 "Error"))
             ((symbol-function 'read-string) (lambda (_prompt) "yes")))
     (should-error (beads-delete--execute-deletion)))))

(ert-deftest beads-delete-test-execute-invalidates-cache ()
  "Test that deletion invalidates completion cache."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . "bd-42"))
   (let ((json-output (json-encode '((id . "bd-42") (deleted . t))))
         (cache-invalidated nil))
     (cl-letf (((symbol-function 'call-process)
                (beads-testing-mock-call-process 0 json-output))
               ((symbol-function 'read-string) (lambda (_prompt) "yes"))
               ((symbol-function 'beads--invalidate-completion-cache)
                (lambda () (setq cache-invalidated t))))
       (beads-delete--execute-deletion)
       (should cache-invalidated)))))

(ert-deftest beads-delete-test-execute-closes-show-buffer ()
  "Test that deletion closes the show buffer for deleted issue."
  (beads-delete-test-with-state
   '((beads-delete--issue-id . "bd-42"))
   (let ((json-output (json-encode '((id . "bd-42") (deleted . t)))))
     ;; Create a show buffer first
     (with-current-buffer (get-buffer-create "*beads-show bd-42*")
       (special-mode))
     (cl-letf (((symbol-function 'call-process)
                (beads-testing-mock-call-process 0 json-output))
               ((symbol-function 'read-string) (lambda (_prompt) "yes"))
               ((symbol-function 'beads--invalidate-completion-cache)
                (lambda () nil)))
       (beads-delete--execute-deletion)
       (should-not (get-buffer "*beads-show bd-42*"))))))

;;; ============================================================
;;; Transient Tests
;;; ============================================================

(ert-deftest beads-delete-test-transient-defined ()
  "Test that beads-delete function is defined."
  (should (fboundp 'beads-delete)))

(ert-deftest beads-delete-test-transient-menu-is-prefix ()
  "Test that beads-delete--menu is a transient prefix."
  (should (fboundp 'beads-delete--menu))
  (should (get 'beads-delete--menu 'transient--prefix)))

(ert-deftest beads-delete-test-infix-commands-defined ()
  "Test that delete infix commands are defined."
  (should (fboundp 'beads-delete--infix-issue-id))
  (should (fboundp 'beads-delete--infix-force)))

(ert-deftest beads-delete-test-suffix-commands-defined ()
  "Test that delete suffix commands are defined."
  (should (fboundp 'beads-delete--execute))
  (should (fboundp 'beads-delete--reset)))

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
;;; Edge Cases and Integration Tests
;;; ============================================================

(ert-deftest beads-delete-test-different-issue-id-formats ()
  "Test deletion with different issue ID formats."
  (let ((issue-ids '("bd-1" "bd-999" "worker-42" "test-123")))
    (dolist (id issue-ids)
      (setq beads-delete--issue-id id)
      (let ((json-output (json-encode (list (cons 'id id)
                                            (cons 'deleted t)))))
        (cl-letf (((symbol-function 'call-process)
                   (beads-testing-mock-call-process 0 json-output))
                  ((symbol-function 'read-string)
                   (lambda (_prompt) "yes"))
                  ((symbol-function 'beads--invalidate-completion-cache)
                   (lambda () nil)))
          (should (beads-delete--execute-deletion)))))))

(ert-deftest beads-delete-test-confirmation-with-exact-issue-id ()
  "Test confirmation requires exact issue ID match."
  (let ((test-cases '(("bd-42" "bd-42" t)    ; exact match
                     ("bd-42" "BD-42" nil)   ; wrong case
                     ("bd-42" "bd-43" nil)   ; wrong number
                     ("bd-42" "bd-4" nil)    ; partial
                     ("bd-42" "42" nil))))   ; missing prefix
    (dolist (case test-cases)
      (let ((issue-id (nth 0 case))
            (response (nth 1 case))
            (expected (nth 2 case)))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_prompt) response)))
          (if expected
              (should (beads-delete--confirm-deletion issue-id))
            (should-not (beads-delete--confirm-deletion issue-id))))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest beads-delete-test-integration-delete-from-list-view ()
  "Integration test: Create issue, delete from list view, verify
deletion."
  :tags '(integration)
  (skip-unless (executable-find "bd"))

  (let ((project-dir (beads-testing-create-temp-project))
        (issue-id nil)
        (test-passed nil))

    (unwind-protect
        (progn
          ;; Step 1: Create a test issue
          (setq issue-id (beads-testing-create-issue
                          project-dir "Test issue for deletion"))
          (should issue-id)
          (should (beads-testing-issue-exists-p project-dir issue-id))

          ;; Step 2: Open list view in the project
          (let ((default-directory project-dir)
                (beads-auto-refresh nil)) ; Disable auto-refresh

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
                             (beads-testing-delete-issue
                              project-dir id))))

                  ;; Simulate pressing D
                  (beads-list-delete)

                  ;; Verify beads-delete was called with correct ID
                  (should delete-called)
                  (should (equal delete-called-with issue-id))))))

          ;; Step 5: Verify the issue was actually deleted
          (should-not (beads-testing-issue-exists-p
                       project-dir issue-id))

          (setq test-passed t))

      ;; Cleanup
      (when (get-buffer "*beads-list*")
        (kill-buffer "*beads-list*"))
      (beads-testing-cleanup-temp-projects)

      ;; Final assertion
      (should test-passed))))

(ert-deftest beads-delete-test-integration-delete-with-force-flag ()
  "Integration test: Delete with force flag using transient."
  :tags '(integration)
  (skip-unless (executable-find "bd"))

  (let ((project-dir (beads-testing-create-temp-project))
        (issue-id nil))

    (unwind-protect
        (let ((default-directory project-dir))
          ;; Create a test issue
          (setq issue-id (beads-testing-create-issue
                          project-dir "Test force deletion"))
          (should issue-id)
          (should (beads-testing-issue-exists-p project-dir issue-id))

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
            (should-not (beads-testing-issue-exists-p
                         project-dir issue-id))))

      ;; Cleanup
      (beads-testing-cleanup-temp-projects))))

(ert-deftest beads-delete-test-integration-keybinding-exists ()
  "Integration test: Verify D keybinding exists in list mode."
  :tags '(integration)
  (with-temp-buffer
    (beads-list-mode)
    (let ((binding (lookup-key beads-list-mode-map (kbd "D"))))
      (should (eq binding 'beads-list-delete)))))

(provide 'beads-delete-test)
;;; beads-delete-test.el ends here
