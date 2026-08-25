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

;;; Test: production store and Dolt server isolation

;; bde-dww: a Gas Town agent shell exports BEADS_DIR plus three Dolt
;; port variables.  BEADS_DIR is the load-bearing one -- it names an
;; absolute store path that overrides repo-local discovery, so `bd
;; init' inside a temp repo resolves the production store, reports
;; "This workspace is already initialized", and exits 1 before
;; .beads/embeddeddolt/ is ever created.  Every test touching bd then
;; fails with that one signature.
;;
;; These guards assert over `beads-test-isolation-env-vars' rather
;; than one hardcoded name, so narrowing the constant fails a test
;; instead of silently narrowing coverage.

(ert-deftest beads-integration-test-isolation-env-vars-names-store-and-ports ()
  "Test that `beads-test-isolation-env-vars' names every leaking variable.
BEADS_DIR is the one that actually reroutes `bd init' to the
production store; the port variables are defence in depth.  Dropping
any of them reintroduces bde-dww, so pin the whole set here -- the
other guards iterate over this constant and would quietly weaken
along with it."
  (dolist (var '("BEADS_DIR"
                 "BEADS_DOLT_PORT"
                 "BEADS_DOLT_SERVER_PORT"
                 "GC_DOLT_PORT"))
    (should (member var beads-test-isolation-env-vars))))

(ert-deftest beads-integration-test-isolated-process-environment-unsets-all ()
  "Test that `beads-test-isolated-process-environment' unsets every variable.
Each must read back as unset rather than rerouted to another value."
  (beads-test-assert-isolated
   (beads-test-isolated-process-environment
    (beads-test-leaking-process-environment))))

(ert-deftest beads-integration-test-env-effective-value-semantics ()
  "Test `beads-test-env-effective-value' distinguishes unset from absent.
The other guards rest on this distinction: an entry with no \"=\" is
the unset sentinel and must shadow a later exported value."
  (let ((env '("SET=value" "UNSET" "SHADOWED=later")))
    (should (equal (beads-test-env-effective-value "SET" env) "value"))
    (should (null (beads-test-env-effective-value "UNSET" env)))
    (should (eq (beads-test-env-effective-value "ABSENT" env) :not-found))
    ;; First match wins, so the unset sentinel shadows the export.
    (should (null (beads-test-env-effective-value
                   "SHADOWED" (cons "SHADOWED" env))))))

(ert-deftest beads-integration-test-create-temp-repo-no-production-leak ()
  "Test that `beads-test-create-temp-repo' unsets the isolation variables.
When a Gas Town shell exports all of them, the fixture must unset
each one so bd uses its embedded Dolt engine in the temp repo's
.beads/embeddeddolt/ directory and never reaches the production
store."
  (let* ((captured-env nil)
         (process-environment (beads-test-leaking-process-environment)))
    ;; Mock beads-test--init-beads to capture process-environment
    (cl-letf (((symbol-function 'beads-test--init-beads)
               (lambda (_dir &optional _prefix _quiet)
                 (setq captured-env process-environment)
                 (setq beads-test--last-init-prefix "test-noleak"))))
      (let ((temp-dir (beads-test-create-temp-repo :init-beads t)))
        (unwind-protect
            (progn
              ;; init must have been called -- captured-env was set
              (should captured-env)
              (beads-test-assert-isolated captured-env))
          (delete-directory temp-dir t))))))

(ert-deftest beads-integration-test-with-temp-repo-no-production-leak ()
  "Test that `beads-test-with-temp-repo' unsets the isolation variables.
Even with all of them present in the outer environment, the macro
must unset each so bd commands inside the body use embedded Dolt and
never reach the production store."
  (let ((process-environment (beads-test-leaking-process-environment)))
    (beads-test-with-temp-repo ()
      (beads-test-assert-isolated process-environment))))

(ert-deftest beads-integration-test-with-temp-repo-init-survives-beads-dir ()
  "Test that `bd init' succeeds while BEADS_DIR names an initialized store.
This is bde-dww end to end.  Without the unset, bd resolves the store
BEADS_DIR points at, sees it already initialized, and exits 1, so the
temp repo never gets its own .beads/embeddeddolt/.  A real
initialized store stands in for the production one, which is what
makes bd take that branch."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  (let ((decoy (beads-test-create-temp-repo :init-beads t :quiet t)))
    (unwind-protect
        (let ((process-environment
               (cons (concat "BEADS_DIR=" (expand-file-name ".beads" decoy))
                     process-environment)))
          (beads-test-with-temp-repo (:init-beads t)
            ;; The store bd created must be repo-local, not the decoy.
            (should (file-directory-p ".beads/embeddeddolt"))
            (should-not (file-equal-p default-directory decoy))))
      (delete-directory decoy t))))

(provide 'beads-integration-test-test)
;;; beads-integration-test-test.el ends here
