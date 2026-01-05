;;; beads-integration-test.el --- Integration test infrastructure -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; This module provides infrastructure for beads.el integration tests.
;; Integration tests run real bd CLI commands against temporary git
;; repositories, testing the full stack from Emacs to the bd binary.
;;
;; The main entry point is `beads-test-with-temp-repo', a macro that:
;; - Creates a temporary directory
;; - Initializes git with test user config
;; - Optionally initializes beads
;; - Runs test code in that context
;; - Cleans up all state afterward
;;
;; Usage:
;;
;;   ;; Basic usage - creates git repo, no beads init
;;   (beads-test-with-temp-repo ()
;;     (should (file-exists-p ".git")))
;;
;;   ;; With beads initialization
;;   (beads-test-with-temp-repo (:init-beads t)
;;     (should (file-directory-p ".beads")))
;;
;;   ;; With custom beads prefix
;;   (beads-test-with-temp-repo (:init-beads t :prefix "mytest")
;;     (let ((issue (beads-command-create! :title "Test")))
;;       (should (string-prefix-p "mytest-" (oref issue id)))))
;;
;; All integration tests should use `:tags '(:integration)' and
;; `(skip-unless (executable-find beads-executable))' to allow
;; running tests without bd installed.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Forward declarations to avoid load-time dependencies
(declare-function beads-command-init "beads-command")
(declare-function beads-command-execute "beads-command")
(declare-function transient-quit-all "transient")

;; Variables that may not be loaded yet
(defvar beads--project-cache)
(defvar beads--completion-cache)
(defvar beads-executable)

;;; ============================================================
;;; Temporary Repository Creation
;;; ============================================================

(defun beads-test--generate-unique-prefix ()
  "Generate a unique test prefix without hyphens.
Uses a format like `beadsTestXXXXXX' where XXXXXX is random.
This format works with bd's --rename-on-import flag which parses
issue IDs by splitting on the first hyphen."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
        (suffix ""))
    (dotimes (_ 6)
      (setq suffix (concat suffix (string (aref chars (random (length chars)))))))
    (concat "beadsTest" suffix)))

(defun beads-test--init-git-repo (dir)
  "Initialize a git repository in DIR with test user config.
Returns DIR for convenience."
  (let ((default-directory dir))
    (call-process "git" nil nil nil "init" "-q")
    (call-process "git" nil nil nil "config" "user.email" "test@beads-integration.local")
    (call-process "git" nil nil nil "config" "user.name" "Beads Integration Test"))
  dir)

(defun beads-test--init-beads (dir &optional prefix quiet)
  "Initialize beads in DIR with optional PREFIX.
If QUIET is non-nil, suppress bd output.
Returns DIR for convenience."
  (require 'beads-command)
  (let* ((default-directory dir)
         (effective-prefix (or prefix (beads-test--generate-unique-prefix)))
         (cmd (beads-command-init :prefix effective-prefix
                                  :quiet quiet
                                  :skip-merge-driver t)))
    (beads-command-execute cmd))
  dir)

(defun beads-test--clear-caches ()
  "Clear all beads caches to ensure test isolation."
  ;; Clear project cache
  (when (boundp 'beads--project-cache)
    (clrhash beads--project-cache))
  ;; Clear completion cache
  (when (boundp 'beads--completion-cache)
    (setq beads--completion-cache nil)))

(defun beads-test--clear-transient-state ()
  "Clear any active transient state to ensure test isolation."
  ;; Exit transient mode if active
  (ignore-errors
    (when (fboundp 'transient-quit-all)
      (transient-quit-all)))
  ;; Reset transient internal state
  (dolist (var '(transient--prefix
                 transient--suffixes
                 transient-current-prefix
                 transient-current-suffixes
                 transient-current-command
                 transient--exitp
                 transient--stack
                 transient--buffer-name
                 transient--window
                 transient--showp
                 overriding-terminal-local-map))
    (when (boundp var)
      (set var nil))))

(defun beads-test-create-temp-repo (&rest args)
  "Create a temporary git repository and return its directory.
ARGS is a plist with optional keys:

  :init-beads - If non-nil, initialize beads in the repo
  :prefix     - Custom prefix for beads (requires :init-beads)
  :quiet      - Suppress bd output during init

Example:
  (beads-test-create-temp-repo :init-beads t :prefix \"mytest\")

The repository is initialized with git and test user config.
Caller is responsible for cleanup."
  (let* ((temp-dir (make-temp-file "beads-integration-" t))
         (init-beads (plist-get args :init-beads))
         (prefix (plist-get args :prefix))
         (quiet (plist-get args :quiet)))
    ;; Initialize git
    (beads-test--init-git-repo temp-dir)
    ;; Optionally initialize beads
    (when init-beads
      (beads-test--init-beads temp-dir prefix quiet))
    temp-dir))

;;; ============================================================
;;; Main Test Macro
;;; ============================================================

(defmacro beads-test-with-temp-repo (args &rest body)
  "Execute BODY with `default-directory' set to a temporary git repo.

ARGS is a plist with optional keys:
  :init-beads - If non-nil, initialize beads in the repo (default nil)
  :prefix     - Custom prefix for beads (requires :init-beads)
  :quiet      - Suppress bd output during init (default t)

This macro:
1. Creates a temporary directory
2. Initializes git with test user config
3. Optionally initializes beads (if :init-beads is non-nil)
4. Clears caches and transient state for isolation
5. Mocks project discovery to use temp dir
6. Executes BODY
7. Cleans up state (caches, transient) afterward

The temporary directory is NOT automatically deleted to aid debugging.
Use `delete-directory' in tests if cleanup is needed.

Examples:

  ;; Basic git repo without beads
  (beads-test-with-temp-repo ()
    (should (file-exists-p \".git\")))

  ;; With beads initialized
  (beads-test-with-temp-repo (:init-beads t)
    (should (file-directory-p \".beads\")))

  ;; With custom prefix
  (beads-test-with-temp-repo (:init-beads t :prefix \"test\")
    (let ((issue (beads-command-create! :title \"Test\")))
      (should (string-prefix-p \"test-\" (oref issue id)))))

  ;; Integration test pattern
  (ert-deftest my-integration-test ()
    :tags \\='(:integration)
    (skip-unless (executable-find beads-executable))
    (beads-test-with-temp-repo (:init-beads t)
      ;; test code here
      ))"
  (declare (indent 1) (debug (form body)))
  (let ((temp-dir (make-symbol "temp-dir"))
        (init-beads (plist-get args :init-beads))
        (prefix (plist-get args :prefix))
        (quiet (if (plist-member args :quiet)
                   (plist-get args :quiet)
                 t)))  ; Default quiet to t
    `(let* ((,temp-dir (beads-test-create-temp-repo
                        ,@(when init-beads '(:init-beads t))
                        ,@(when prefix `(:prefix ,prefix))
                        :quiet ,quiet))
            (default-directory ,temp-dir)
            ;; Fresh caches for isolation
            (beads--project-cache (make-hash-table :test 'equal))
            (beads--completion-cache nil))
       ;; Clear state before test
       (beads-test--clear-transient-state)
       ;; Mock project discovery to use temp dir
       (cl-letf (((symbol-function 'beads-git-find-project-root)
                  (lambda () nil)))
         (unwind-protect
             (progn ,@body)
           ;; Clear state after test
           (beads-test--clear-transient-state)
           (beads-test--clear-caches))))))

;;; ============================================================
;;; Helper Functions for Integration Tests
;;; ============================================================

(defun beads-test-skip-unless-bd ()
  "Skip test if bd executable is not available.
Use at the start of integration tests:

  (ert-deftest my-test ()
    :tags \\='(:integration)
    (beads-test-skip-unless-bd)
    ...)"
  (unless (executable-find (if (boundp 'beads-executable)
                               beads-executable
                             "bd"))
    (ert-skip "bd executable not found")))

(defmacro beads-test-with-temp-repo-and-issues (args issues &rest body)
  "Execute BODY in a temp repo with pre-created ISSUES.

ARGS is passed to `beads-test-with-temp-repo'.
ISSUES is a list of plists, each with at least :title.

Example:
  (beads-test-with-temp-repo-and-issues
      (:init-beads t)
      ((:title \"Issue 1\" :type \"bug\")
       (:title \"Issue 2\" :type \"task\"))
    (should (= 2 (length (beads-command-list!)))))"
  (declare (indent 2) (debug (form form body)))
  `(beads-test-with-temp-repo ,args
     (require 'beads-command)
     ;; Create each issue
     (dolist (issue-args ',issues)
       (apply #'beads-command-create! issue-args))
     ,@body))

(provide 'beads-integration-test)
;;; beads-integration-test.el ends here
