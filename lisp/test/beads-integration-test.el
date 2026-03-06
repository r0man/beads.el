;;; beads-integration-test.el --- Integration test infrastructure -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

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
(defvar beads-test-no-daemon)

(defvar beads-test--last-init-prefix nil
  "Prefix used by the most recent `beads-test--init-beads' call.
Set as a side effect so callers can retrieve the prefix for cleanup.")

;; Enforce BD_NO_DAEMON=1 globally at load time.  Ensures all
;; integration tests use file-backed storage and never connect to
;; the production Dolt server on port 3307.
(when (bound-and-true-p beads-test-no-daemon)
  (setenv "BD_NO_DAEMON" "1"))

;;; ============================================================
;;; Test-Local Dolt Server Management
;;; ============================================================

(defvar beads-test--dolt-server-process nil
  "Process object for the test-local Dolt SQL server.
Non-nil when the server is running.  Tests that need the Dolt
backend use this server instead of the production server on
port 3307.")

(defvar beads-test--dolt-server-port nil
  "TCP port number for the test-local Dolt SQL server.
Set by `beads-test--start-test-dolt-server' and cleared by
`beads-test--stop-test-dolt-server'.")

(defvar beads-test--dolt-server-data-dir nil
  "Temporary data directory for the test-local Dolt SQL server.
Deleted when the server is stopped via
`beads-test--stop-test-dolt-server'.")

(defun beads-test--find-free-port ()
  "Find a free TCP port by binding to port 0 and reading the assigned port.
Returns the port number as an integer.  The port is guaranteed to
have been free at the moment this function returns, though it may
be taken by another process immediately afterward."
  (let* ((server (make-network-process
                  :name "beads-test-port-probe"
                  :server t
                  :host "127.0.0.1"
                  :service 0
                  :family 'ipv4
                  :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defun beads-test--dolt-server-ready-p (port)
  "Return non-nil if a server is accepting TCP connections on PORT.
Used to poll for the Dolt SQL server startup."
  (condition-case nil
      (let ((conn (open-network-stream
                   "beads-test-dolt-probe" nil "127.0.0.1" port)))
        (delete-process conn)
        t)
    (error nil)))

(defun beads-test--wait-for-dolt-server (port &optional max-wait-ms)
  "Wait until the Dolt server on PORT accepts connections.
MAX-WAIT-MS is the maximum wait time in milliseconds (default 10000).
Signals an error if the server does not become ready in time."
  (let* ((max-ms (or max-wait-ms 10000))
         (interval-ms 200)
         (attempts (/ max-ms interval-ms))
         (ready nil))
    (while (and (> attempts 0) (not ready))
      (setq ready (beads-test--dolt-server-ready-p port))
      (unless ready
        (sleep-for 0 interval-ms)
        (setq attempts (1- attempts))))
    (unless ready
      (error "Dolt test server did not start on port %d within %dms"
             port max-ms))))

(defun beads-test--start-test-dolt-server ()
  "Start a test-local Dolt SQL server on a free port.
Sets `beads-test--dolt-server-process', `beads-test--dolt-server-port',
and `beads-test--dolt-server-data-dir'.  Idempotent: does nothing if
the server is already running.  Signals an error if dolt is not
installed.

The server uses a temporary data directory and a dynamically allocated
port, so it never conflicts with the production server on port 3307.
All test databases are automatically destroyed when the server stops."
  (unless beads-test--dolt-server-process
    (unless (executable-find "dolt")
      (error "dolt not found in PATH; cannot start test Dolt server"))
    (let* ((port (beads-test--find-free-port))
           (data-dir (make-temp-file "beads-test-dolt-" t))
           (proc (start-process
                  "beads-test-dolt" nil
                  "dolt" "sql-server"
                  "--port" (number-to-string port)
                  "--data-dir" data-dir)))
      (set-process-query-on-exit-flag proc nil)
      (setq beads-test--dolt-server-process proc
            beads-test--dolt-server-port port
            beads-test--dolt-server-data-dir data-dir)
      (beads-test--wait-for-dolt-server port))))

(defun beads-test--stop-test-dolt-server ()
  "Stop the test-local Dolt SQL server and clean up its data directory.
Resets `beads-test--dolt-server-process', `beads-test--dolt-server-port',
and `beads-test--dolt-server-data-dir' to nil.  Idempotent: does nothing
if no server is running.

Stopping the server destroys all test databases automatically — no
explicit DROP DATABASE cleanup is needed."
  (when beads-test--dolt-server-process
    (ignore-errors (delete-process beads-test--dolt-server-process))
    (setq beads-test--dolt-server-process nil))
  (when beads-test--dolt-server-data-dir
    (ignore-errors (delete-directory beads-test--dolt-server-data-dir t))
    (setq beads-test--dolt-server-data-dir nil))
  (setq beads-test--dolt-server-port nil)
  nil)

;;; ============================================================
;;; CLI Feature Detection
;;; ============================================================

(defun beads-test-bd-has-subcommand-p (subcommand)
  "Return non-nil if bd CLI supports SUBCOMMAND.
Uses `bd SUBCOMMAND --help' to test availability."
  (zerop (call-process (or (bound-and-true-p beads-executable) "bd")
                       nil nil nil subcommand "--help")))

;;; ============================================================
;;; Temporary Repository Creation
;;; ============================================================

(defun beads-test--generate-unique-prefix ()
  "Generate a unique test prefix without hyphens.
Uses a format like `btXXXXXX' where XXXXXX is random alphanumeric.
The `bt' prefix identifies test databases for easy cleanup.
This format works with bd's --rename-on-import flag which parses
issue IDs by splitting on the first hyphen."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
        (suffix ""))
    (dotimes (_ 6)
      (setq suffix (concat suffix (string (aref chars (random (length chars)))))))
    (concat "bt" suffix)))

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
Sets `beads-test--last-init-prefix' as a side effect.

When `beads-test-no-daemon' is non-nil (or BD_NO_DAEMON=1 is set),
uses file-backed storage (SQLite) and never contacts a Dolt server.

When Dolt mode is active (no-daemon not set), automatically starts the
test-local Dolt server (if not already running) and configures this
repo to use it, ensuring the production server on port 3307 is never
touched.

Returns DIR for convenience."
  (require 'beads-command)
  (let* ((default-directory dir)
         (effective-prefix (or prefix (beads-test--generate-unique-prefix)))
         (no-daemon (or (bound-and-true-p beads-test-no-daemon)
                        (getenv "BD_NO_DAEMON")))
         (cmd (beads-command-init :prefix effective-prefix
                                  :quiet quiet
                                  :skip-hooks t))
         ;; Isolate from production Dolt server
         (process-environment (if no-daemon
                                  (cons "BD_NO_DAEMON=1"
                                        process-environment)
                                process-environment)))
    (setq beads-test--last-init-prefix effective-prefix)
    (beads-command-execute cmd)
    ;; When running in Dolt mode, auto-start the test server and
    ;; configure this repo to use it, never port 3307.
    (unless no-daemon
      (beads-test--start-test-dolt-server)
      (call-process (or (bound-and-true-p beads-executable) "bd")
                    nil nil nil
                    "dolt" "set" "port"
                    (number-to-string beads-test--dolt-server-port))))
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
  :cleanup    - If nil, don't cleanup temp dir after BODY (default t)

This macro:
1. Creates a temporary directory
2. Initializes git with test user config
3. Optionally initializes beads (if :init-beads is non-nil)
4. Clears caches and transient state for isolation
5. Mocks project discovery to use temp dir
6. Executes BODY
7. Cleans up state (caches, transient) afterward

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
        (cleanup (if (plist-member args :cleanup)
                     (plist-get args :cleanup)
                   t))
        (quiet (if (plist-member args :quiet)
                   (plist-get args :quiet)
                 t)))  ; Default quiet to t
    `(let* ((beads-test--last-init-prefix nil)
            (,temp-dir (beads-test-create-temp-repo
                        ,@(when init-beads '(:init-beads t))
                        ,@(when prefix `(:prefix ,prefix))
                        :quiet ,quiet))
            (default-directory ,temp-dir)
            ;; Fresh caches for isolation
            (beads--project-cache (make-hash-table :test 'equal))
            (beads--completion-cache nil)
            ;; Isolate from production Dolt server
            (process-environment
             (if (bound-and-true-p beads-test-no-daemon)
                 (cons "BD_NO_DAEMON=1" process-environment)
               process-environment)))
       ;; Clear state before test
       (beads-test--clear-transient-state)
       ;; Mock project discovery to use temp dir
       (cl-letf (((symbol-function 'beads-git-find-project-root)
                  (lambda () nil)))
         (unwind-protect
             (progn ,@body)
           ;; Clear state after test
           (beads-test--clear-transient-state)
           (beads-test--clear-caches)
           ;; Optionally cleanup temp dir
           (when ,cleanup
             (delete-directory ,temp-dir t)))))))

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
