;;; beads-integration-test-test.el --- Tests for beads-integration-test.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for the integration test infrastructure itself.
;; These tests verify that the `beads-test-with-temp-repo' macro
;; and related functions work correctly.

;;; Code:

(require 'ert)
(require 'beads-integration-test)

;;; Test: beads-test--generate-unique-prefix

(ert-deftest beads-integration-test-generate-prefix-format ()
  "Test that generated prefix has correct format."
  (let ((prefix (beads-test--generate-unique-prefix)))
    ;; Should start with beadsTest
    (should (string-prefix-p "beadsTest" prefix))
    ;; Should have 6 random chars after prefix
    (should (= (length prefix) 15))  ; "beadsTest" (9) + 6 random
    ;; Should not contain hyphens
    (should-not (string-match-p "-" prefix))))

(ert-deftest beads-integration-test-generate-prefix-unique ()
  "Test that generated prefixes are unique."
  (let ((prefixes (cl-loop repeat 10 collect (beads-test--generate-unique-prefix))))
    ;; All should be unique
    (should (= (length prefixes) (length (delete-dups (copy-sequence prefixes)))))))

;;; Test: beads-test--init-git-repo

(ert-deftest beads-integration-test-init-git-repo ()
  "Test that git repo initialization works."
  (let* ((temp-dir (make-temp-file "beads-test-git-" t))
         (result (beads-test--init-git-repo temp-dir)))
    (unwind-protect
        (progn
          ;; Should return the directory
          (should (equal result temp-dir))
          ;; Should create .git directory
          (should (file-directory-p (expand-file-name ".git" temp-dir)))
          ;; Should set user.email
          (let ((default-directory temp-dir))
            (should (string-match-p
                     "test@beads-integration.local"
                     (shell-command-to-string "git config user.email")))))
      (delete-directory temp-dir t))))

;;; Test: beads-test-create-temp-repo

(ert-deftest beads-integration-test-create-temp-repo-basic ()
  "Test basic temp repo creation without beads."
  (let ((temp-dir (beads-test-create-temp-repo)))
    (unwind-protect
        (progn
          ;; Should return a directory
          (should (file-directory-p temp-dir))
          ;; Should have .git
          (should (file-directory-p (expand-file-name ".git" temp-dir)))
          ;; Should NOT have .beads (not initialized)
          (should-not (file-directory-p (expand-file-name ".beads" temp-dir))))
      (delete-directory temp-dir t))))

(ert-deftest beads-integration-test-create-temp-repo-with-beads ()
  "Test temp repo creation with beads initialization."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  (let ((temp-dir (beads-test-create-temp-repo :init-beads t)))
    (unwind-protect
        (progn
          ;; Should have .beads directory
          (should (file-directory-p (expand-file-name ".beads" temp-dir))))
      (delete-directory temp-dir t))))

(ert-deftest beads-integration-test-create-temp-repo-with-prefix ()
  "Test temp repo creation with custom prefix."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  (let ((temp-dir (beads-test-create-temp-repo :init-beads t :prefix "testpfx")))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Should have .beads directory
          (should (file-directory-p ".beads")))
      (delete-directory temp-dir t))))

;;; Test: beads-test-with-temp-repo macro

(ert-deftest beads-integration-test-with-temp-repo-basic ()
  "Test beads-test-with-temp-repo creates git repo."
  (beads-test-with-temp-repo ()
    ;; default-directory should be set
    (should (file-directory-p default-directory))
    ;; Should have .git
    (should (file-directory-p ".git"))
    ;; Should NOT have .beads
    (should-not (file-directory-p ".beads"))))

(ert-deftest beads-integration-test-with-temp-repo-with-beads ()
  "Test beads-test-with-temp-repo with beads initialization."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  (beads-test-with-temp-repo (:init-beads t)
    ;; Should have .beads
    (should (file-directory-p ".beads"))))

(ert-deftest beads-integration-test-with-temp-repo-isolation ()
  "Test that caches are isolated between tests."
  (beads-test-with-temp-repo ()
    ;; Project cache should be empty
    (should (hash-table-empty-p beads--project-cache))
    ;; Add something to cache
    (puthash "test-key" "test-value" beads--project-cache)
    (should (gethash "test-key" beads--project-cache)))
  ;; After macro, cache should be different (new hash table)
  (beads-test-with-temp-repo ()
    ;; Should have fresh cache
    (should (hash-table-empty-p beads--project-cache))))

(ert-deftest beads-integration-test-with-temp-repo-unwind ()
  "Test that cleanup happens even on error."
  (let ((transient-cleared nil))
    (cl-letf (((symbol-function 'beads-test--clear-transient-state)
               (lambda ()
                 (setq transient-cleared t))))
      (ignore-errors
        (beads-test-with-temp-repo ()
          (error "Intentional test error")))
      ;; Cleanup should have run despite error
      (should transient-cleared))))

;;; Test: beads-test-skip-unless-bd

(ert-deftest beads-integration-test-skip-unless-bd-present ()
  "Test skip-unless-bd when bd is present."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  ;; If we get here, bd is present and skip-unless-bd should not skip
  (beads-test-skip-unless-bd)
  (should t))

(provide 'beads-integration-test-test)
;;; beads-integration-test-test.el ends here
