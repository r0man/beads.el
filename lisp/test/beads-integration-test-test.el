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
    ;; Should start with bt
    (should (string-prefix-p "bt" prefix))
    ;; Should have 6 random chars after prefix
    (should (= (length prefix) 8))  ; "bt" (2) + 6 random
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
        ;; Should have .beads directory
        (should (file-directory-p (expand-file-name ".beads" temp-dir)))
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

;;; Isolation helpers

(defun beads-integration-test--effective-env-var (name env)
  "Return the effective value of NAME from process-environment list ENV.
The effective value is the first matching entry:
- \"NAME=VALUE\" → returns VALUE string
- \"NAME\" (no =) → returns nil (variable is unset)
- no match → returns :not-found"
  (let ((prefix (concat name "=")))
    (catch 'found
      (dolist (entry env)
        (cond
         ((string= entry name) (throw 'found nil))
         ((string-prefix-p prefix entry)
          (throw 'found (substring entry (length prefix))))))
      :not-found)))

;;; Test: production Dolt server isolation

(ert-deftest beads-integration-test-create-temp-repo-no-production-leak ()
  "Test that beads-test-create-temp-repo unsets BEADS_DOLT_PORT.
When BEADS_DOLT_PORT=3307 is set in the outer environment (e.g. a
Gas Town shell), the test infrastructure must defensively unset it
so bd uses its embedded Dolt engine in the temp repo's
.beads/embeddeddolt/ directory and never reaches a production
sql-server."
  (let* ((captured-env nil)
         ;; Simulate Gas Town production environment
         (process-environment
          (cons "BEADS_DOLT_PORT=3307" process-environment)))
    ;; Mock beads-test--init-beads to capture process-environment
    (cl-letf (((symbol-function 'beads-test--init-beads)
               (lambda (_dir &optional _prefix _quiet)
                 (setq captured-env process-environment)
                 (setq beads-test--last-init-prefix "test-noleak"))))
      (let ((temp-dir (beads-test-create-temp-repo :init-beads t)))
        (unwind-protect
            (progn
              ;; init must have been called — captured-env was set
              (should captured-env)
              ;; The effective BEADS_DOLT_PORT must be nil ("unset"
              ;; sentinel from -effective-env-var) — never the
              ;; production port, never any other value.
              (let ((effective (beads-integration-test--effective-env-var
                                "BEADS_DOLT_PORT" captured-env)))
                (should (null effective))))
          (delete-directory temp-dir t))))))

(ert-deftest beads-integration-test-with-temp-repo-no-production-leak ()
  "Test that beads-test-with-temp-repo unsets BEADS_DOLT_PORT.
Even when BEADS_DOLT_PORT=3307 is present in the outer environment,
the macro must unset the variable so bd commands inside the body
use embedded Dolt and never connect to a production server."
  ;; Simulate Gas Town environment with production port set
  (let ((process-environment
         (cons "BEADS_DOLT_PORT=3307" process-environment)))
    (beads-test-with-temp-repo ()
      ;; Inside the macro, the effective BEADS_DOLT_PORT must be
      ;; unset (nil), not just rerouted to a different port.
      (let ((effective (beads-integration-test--effective-env-var
                        "BEADS_DOLT_PORT" process-environment)))
        (should (null effective))))))

(provide 'beads-integration-test-test)
;;; beads-integration-test-test.el ends here
