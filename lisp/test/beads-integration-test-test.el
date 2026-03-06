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
  (let* ((temp-dir (beads-test-create-temp-repo :init-beads t))
         (prefix beads-test--last-init-prefix))
    (unwind-protect
        (progn
          ;; Should have .beads directory
          (should (file-directory-p (expand-file-name ".beads" temp-dir))))
      (beads-test--drop-dolt-database prefix)
      (delete-directory temp-dir t))))

(ert-deftest beads-integration-test-create-temp-repo-with-prefix ()
  "Test temp repo creation with custom prefix."
  :tags '(:integration)
  (skip-unless (executable-find "bd"))
  (let* ((temp-dir (beads-test-create-temp-repo :init-beads t :prefix "testpfx"))
         (prefix beads-test--last-init-prefix))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Should have .beads directory
          (should (file-directory-p ".beads")))
      (beads-test--drop-dolt-database prefix)
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

;;; Test: beads-test--find-free-port

(ert-deftest beads-integration-test-find-free-port-returns-integer ()
  "Test that find-free-port returns an integer port number."
  (let ((port (beads-test--find-free-port)))
    (should (integerp port))
    (should (> port 0))
    (should (< port 65536))))

(ert-deftest beads-integration-test-find-free-port-above-1024 ()
  "Test that find-free-port returns a port above the privileged range."
  (let ((port (beads-test--find-free-port)))
    (should (> port 1024))))

(ert-deftest beads-integration-test-find-free-port-is-free ()
  "Test that the returned port is actually free (bindable)."
  (let* ((port (beads-test--find-free-port))
         (server (make-network-process
                  :name "beads-test-port-verify"
                  :server t
                  :host "127.0.0.1"
                  :service port
                  :family 'ipv4
                  :noquery t)))
    (unwind-protect
        (should server)
      (delete-process server))))

;;; Test: beads-test--dolt-server-ready-p

(ert-deftest beads-integration-test-dolt-server-ready-p-false ()
  "Test that dolt-server-ready-p returns nil for a port not in use."
  ;; Find a free port and verify it reports as not ready (nothing listening)
  (let ((port (beads-test--find-free-port)))
    ;; Port is free so nothing is listening — should not be ready
    (should-not (beads-test--dolt-server-ready-p port))))

(ert-deftest beads-integration-test-dolt-server-ready-p-true ()
  "Test that dolt-server-ready-p returns t for a port that is in use."
  (let* ((port (beads-test--find-free-port))
         (server (make-network-process
                  :name "beads-test-ready-verify"
                  :server t
                  :host "127.0.0.1"
                  :service port
                  :family 'ipv4
                  :noquery t)))
    (unwind-protect
        (should (beads-test--dolt-server-ready-p port))
      (delete-process server))))

;;; Test: beads-test--start/stop-test-dolt-server

(ert-deftest beads-integration-test-dolt-server-start-stop ()
  "Test that the test Dolt server can be started and stopped."
  :tags '(:integration)
  (skip-unless (executable-find "dolt"))
  ;; Ensure clean state before test
  (let ((beads-test--dolt-server-process nil)
        (beads-test--dolt-server-port nil)
        (beads-test--dolt-server-data-dir nil))
    (unwind-protect
        (progn
          ;; Start the server
          (beads-test--start-test-dolt-server)
          ;; State variables should be set
          (should beads-test--dolt-server-process)
          (should (integerp beads-test--dolt-server-port))
          (should (> beads-test--dolt-server-port 0))
          (should beads-test--dolt-server-data-dir)
          ;; Server should be accepting connections
          (should (beads-test--dolt-server-ready-p beads-test--dolt-server-port))
          ;; Idempotent: starting again should not change the port
          (let ((port-before beads-test--dolt-server-port))
            (beads-test--start-test-dolt-server)
            (should (= beads-test--dolt-server-port port-before)))
          ;; Stop the server
          (beads-test--stop-test-dolt-server)
          ;; State should be cleared
          (should-not beads-test--dolt-server-process)
          (should-not beads-test--dolt-server-port)
          (should-not beads-test--dolt-server-data-dir))
      ;; Cleanup in case of failure
      (beads-test--stop-test-dolt-server))))

(ert-deftest beads-integration-test-dolt-server-stop-when-not-running ()
  "Test that stopping the server when not running is a no-op."
  (let ((beads-test--dolt-server-process nil)
        (beads-test--dolt-server-port nil)
        (beads-test--dolt-server-data-dir nil))
    ;; Should not signal an error
    (should (null (beads-test--stop-test-dolt-server)))))

(provide 'beads-integration-test-test)
;;; beads-integration-test-test.el ends here
