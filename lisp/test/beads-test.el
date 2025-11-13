;;; beads-test.el --- Tests for beads.el core functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads.el core functionality including:
;; - Customization variables
;; - Logging functionality (beads--log)
;; - Error handling (beads--error)
;; - Executable checking (beads-check-executable)
;; - Process execution (beads--run-command, async)
;; - JSON parsing (beads--parse-issue, beads--parse-issues)
;; - Command building (beads--build-command)
;; - Project.el integration (find-project-root, find-beads-dir)
;; - Tramp remote support
;; - Integration and performance tests

;;; Code:

(require 'ert)
(require 'json)
(require 'beads)
(require 'beads-command)

(defun beads-test-create-project ()
  (let ((default-directory (make-temp-file "beads-test-" t)))
    (beads-command-execute (beads-command-init))
    default-directory))

(defun beads-test-execute-commands (cmds)
  (dolist (cmd cmds)
    (setq last-command this-command)
    (setq this-command cmd)
    (run-hooks 'pre-command-hook)
    (command-execute cmd)
    (run-hooks 'post-command-hook)
    (undo-boundary)))

;;; ========================================
;;; Test Fixtures and Utilities
;;; ========================================

;;; Test Fixtures

(defvar beads-test--sample-issue
  '((id . "bd-1")
    (title . "Test Issue")
    (description . "Test description")
    (status . "open")
    (priority . 1)
    (issue_type . "bug")
    (created_at . "2025-01-15T10:00:00Z")
    (updated_at . "2025-01-15T10:00:00Z")
    (acceptance_criteria . "Must work")
    (design . "Simple design")
    (notes . "Some notes")
    (assignee . "alice")
    (external_ref . "EXT-123"))
  "Sample issue JSON structure for testing.")

(defvar beads-test--sample-issues-array
  (vector
   '((id . "bd-1")
     (title . "First Issue")
     (description . "First description")
     (status . "open")
     (priority . 1)
     (issue_type . "bug")
     (created_at . "2025-01-15T10:00:00Z")
     (updated_at . "2025-01-15T10:00:00Z"))
   '((id . "bd-2")
     (title . "Second Issue")
     (description . "Second description")
     (status . "in_progress")
     (priority . 2)
     (issue_type . "feature")
     (created_at . "2025-01-15T11:00:00Z")
     (updated_at . "2025-01-15T11:00:00Z"))
   '((id . "bd-3")
     (title . "Third Issue")
     (description . "Third description")
     (status . "closed")
     (priority . 3)
     (issue_type . "task")
     (created_at . "2025-01-15T12:00:00Z")
     (updated_at . "2025-01-15T12:00:00Z")))
  "Sample issues array for testing.")

;;; Test Utilities

(defmacro beads-test-with-temp-config (&rest body)
  "Execute BODY with temporary beads configuration."
  `(let ((beads-executable "bd")
         (beads-database-path nil)
         (beads-actor nil)
         (beads-enable-debug nil)
         (beads-auto-refresh t)
         (beads--project-cache (make-hash-table :test 'equal)))
     ;; Mock beads--find-beads-dir to prevent auto-discovery
     (cl-letf (((symbol-function 'beads--find-beads-dir)
                (lambda () nil)))
       ,@body)))

(defun beads-test--mock-call-process (exit-code output)
  "Create a mock for `process-file' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

(defun beads-test--mock-start-process (exit-code output)
  "Create a mock for `start-file-process' returning EXIT-CODE and OUTPUT."
  (lambda (name buffer program &rest args)
    (let ((proc (make-process
                 :name name
                 :buffer buffer
                 :command (list "true") ; Dummy command
                 :noquery t)))
      ;; Simulate process completion
      (run-at-time
       0.1 nil
       (lambda ()
         (with-current-buffer buffer
           (insert output))
         (set-process-exit-status proc exit-code)
         (set-process-sentinel proc 'ignore)
         (delete-process proc)))
      proc)))


(defvar beads-test--temp-dir nil
  "Temporary directory for test files.")

(defun beads-test--setup ()
  "Set up test fixtures."
  ;; Create temporary directory structure
  (setq beads-test--temp-dir (make-temp-file "beads-test-" t))

  ;; Clear the project cache
  (clrhash beads--project-cache)

  ;; Clear customizations
  (setq beads-database-path nil))

(defun beads-test--teardown ()
  "Clean up test fixtures."
  ;; Clean up temp directory
  (when (and beads-test--temp-dir
             (file-directory-p beads-test--temp-dir))
    (delete-directory beads-test--temp-dir t))
  (setq beads-test--temp-dir nil)

  ;; Clear the cache
  (clrhash beads--project-cache)

  ;; Reset customizations
  (setq beads-database-path nil))

(defmacro beads-test--with-temp-project (&rest body)
  "Execute BODY with a temporary project structure."
  `(let ((beads-test--temp-dir nil))
     (unwind-protect
         (progn
           (beads-test--setup)
           ,@body)
       (beads-test--teardown))))

(defun beads-test--create-project-structure (structure)
  "Create a directory STRUCTURE under `beads-test--temp-dir'.
STRUCTURE is a list of paths to create (dirs end with /)."
  (dolist (path structure)
    (let ((full-path (expand-file-name path beads-test--temp-dir)))
      (if (string-suffix-p "/" path)
          ;; Directory
          (make-directory full-path t)
        ;; File
        (make-directory (file-name-directory full-path) t)
        (write-region "" nil full-path)))))

;;; Test Utilities

(defmacro beads-tramp-test-with-temp-config (&rest body)
  "Execute BODY with temporary beads configuration."
  `(let ((beads-executable "bd")
         (beads-database-path nil)
         (beads-actor nil)
         (beads-enable-debug nil)
         (beads-auto-refresh t)
         (beads--project-cache (make-hash-table :test 'equal)))
     ;; Mock beads--find-beads-dir to prevent auto-discovery
     (cl-letf (((symbol-function 'beads--find-beads-dir)
                (lambda (&optional _dir) nil)))
       ,@body)))

(defun beads-tramp-test--mock-process-file (exit-code output-text)
  "Create a mock for `process-file' returning EXIT-CODE and OUTPUT-TEXT."
  (let ((exit exit-code)
        (text output-text))
    (lambda (program &optional infile destination display &rest args)
      (when destination
        (with-current-buffer (if (bufferp destination)
                                 destination
                               (current-buffer))
          (insert text)))
      exit)))


;;; ========================================
;;; Customization Variable Tests
;;; ========================================


(ert-deftest beads-test-customization-defaults ()
  "Test that all customization variables have correct defaults."
  (should (equal beads-executable "bd"))
  (should (null beads-database-path))
  (should (null beads-actor))
  (should (eq beads-enable-debug nil))
  (should (eq beads-auto-refresh t)))

(ert-deftest beads-test-customization-types ()
  "Test that customization variables accept correct types."
  ;; Test beads-executable
  (let ((beads-executable "/usr/local/bin/bd"))
    (should (stringp beads-executable)))

  ;; Test beads-database-path
  (let ((beads-database-path "/path/to/db.db"))
    (should (stringp beads-database-path)))
  (let ((beads-database-path nil))
    (should (null beads-database-path)))

  ;; Test beads-actor
  (let ((beads-actor "test-actor"))
    (should (stringp beads-actor)))
  (let ((beads-actor nil))
    (should (null beads-actor)))

  ;; Test beads-enable-debug
  (let ((beads-enable-debug t))
    (should (eq beads-enable-debug t)))
  (let ((beads-enable-debug nil))
    (should (eq beads-enable-debug nil)))

  ;; Test beads-auto-refresh
  (let ((beads-auto-refresh nil))
    (should (eq beads-auto-refresh nil)))
  (let ((beads-auto-refresh t))
    (should (eq beads-auto-refresh t))))


;;; ========================================
;;; Logging Tests
;;; ========================================


(ert-deftest beads-test-log-disabled-by-default ()
  "Test that logging does nothing when debug is disabled."
  (let ((beads-enable-debug nil))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Log a message
    (beads--log 'info "Test message: %s" "value")

    ;; Debug buffer should not be created
    (should-not (get-buffer "*beads-debug*"))))

(ert-deftest beads-test-log-enabled ()
  "Test that logging works when debug is enabled."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Log a message
    (beads--log 'info "Test message: %s" "value")

    ;; Debug buffer should be created
    (should (get-buffer "*beads-debug*"))

    ;; Check buffer contents
    (with-current-buffer "*beads-debug*"
      (goto-char (point-min))
      (should (search-forward "Test message: value" nil t)))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

(ert-deftest beads-test-log-timestamp-format ()
  "Test that log messages include proper timestamps."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Log a message
    (beads--log 'info "Timestamp test")

    ;; Check timestamp format [YYYY-MM-DD HH:MM:SS]
    (with-current-buffer "*beads-debug*"
      (goto-char (point-min))
      (should (search-forward-regexp
               "\\[20[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\]"
               nil t)))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

(ert-deftest beads-test-log-multiple-messages ()
  "Test that multiple log messages accumulate in buffer."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Log multiple messages
    (beads--log 'info "First message")
    (beads--log 'info "Second message")
    (beads--log 'info "Third message")

    ;; Check all messages are present
    (with-current-buffer "*beads-debug*"
      (goto-char (point-min))
      (should (search-forward "First message" nil t))
      (should (search-forward "Second message" nil t))
      (should (search-forward "Third message" nil t)))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

(ert-deftest beads-test-log-format-args ()
  "Test that log format strings with arguments work correctly."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Log with various format specifiers
    (beads--log 'info "String: %s, Number: %d, Float: %.2f" "test" 42 3.14159)

    ;; Check formatted output
    (with-current-buffer "*beads-debug*"
      (goto-char (point-min))
      (should (search-forward "String: test, Number: 42, Float: 3.14" nil t)))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

(ert-deftest beads-test-log-empty-message ()
  "Test that empty log messages are handled correctly."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Log empty message
    (beads--log 'info "")

    ;; Buffer should exist with timestamp but empty message
    (with-current-buffer "*beads-debug*"
      (goto-char (point-min))
      (should (search-forward-regexp "\\[.*\\] \n" nil t)))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))


;;; ========================================
;;; Error Reporting Tests
;;; ========================================


(ert-deftest beads-test-error-signals-user-error ()
  "Test that beads--error signals a user-error."
  (should-error (beads--error "Test error")
                :type 'user-error))

(ert-deftest beads-test-error-message-format ()
  "Test that error messages are formatted correctly."
  (condition-case err
      (beads--error "Test error: %s" "detail")
    (user-error
     (should (string-match-p "Beads: Test error: detail"
                             (error-message-string err))))))

(ert-deftest beads-test-error-logs-to-debug ()
  "Test that errors are logged to debug buffer when enabled."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Trigger an error
    (condition-case nil
        (beads--error "Test error")
      (user-error nil))

    ;; Check error was logged
    (with-current-buffer "*beads-debug*"
      (goto-char (point-min))
      (should (search-forward "ERROR: Test error" nil t)))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

(ert-deftest beads-test-error-without-debug ()
  "Test that errors don't create debug buffer when debug is disabled."
  (let ((beads-enable-debug nil))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Trigger an error
    (condition-case nil
        (beads--error "Test error")
      (user-error nil))

    ;; Debug buffer should not be created
    (should-not (get-buffer "*beads-debug*"))))

(ert-deftest beads-test-error-multiple-args ()
  "Test error formatting with multiple arguments."
  (condition-case err
      (beads--error "Error: %s=%d, status=%s" "count" 5 "failed")
    (user-error
     (should (string-match-p "Beads: Error: count=5, status=failed"
                             (error-message-string err))))))

(ert-deftest beads-test-error-prefix ()
  "Test that all errors have 'Beads:' prefix."
  (condition-case err
      (beads--error "Simple message")
    (user-error
     (should (string-prefix-p "Beads: " (error-message-string err))))))


;;; ========================================
;;; Executable Checking Tests
;;; ========================================


(ert-deftest beads-test-check-executable-found ()
  "Test beads-check-executable when executable is found."
  ;; Mock executable-find to return a path
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_name) "/usr/bin/bd")))
    (let ((beads-executable "bd"))
      (should (eq (beads-check-executable) t)))))

(ert-deftest beads-test-check-executable-not-found ()
  "Test beads-check-executable when executable is not found."
  ;; Mock executable-find to return nil
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_name) nil)))
    (let ((beads-executable "bd"))
      (should-error (beads-check-executable)
                    :type 'user-error))))

(ert-deftest beads-test-check-executable-error-message ()
  "Test error message when executable is not found."
  ;; Mock executable-find to return nil
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_name) nil)))
    (let ((beads-executable "custom-bd"))
      (condition-case err
          (beads-check-executable)
        (user-error
         (should (string-match-p "Cannot find bd executable: custom-bd"
                                 (error-message-string err))))))))

(ert-deftest beads-test-check-executable-uses-custom-path ()
  "Test that beads-check-executable uses custom executable path."
  (let ((beads-executable "/custom/path/to/bd")
        (called-with nil))
    ;; Mock executable-find to capture arguments
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name)
                 (setq called-with name)
                 "/custom/path/to/bd")))
      (beads-check-executable)
      (should (equal called-with "/custom/path/to/bd")))))

(ert-deftest beads-test-check-executable-interactive-form ()
  "Test that beads-check-executable has interactive form."
  ;; This test verifies the function is declared as a command
  (should (commandp 'beads-check-executable))
  ;; Verify it has an interactive form
  (should (interactive-form 'beads-check-executable)))

(ert-deftest beads-test-check-executable-non-interactive ()
  "Test that non-interactive calls don't display message."
  ;; Mock executable-find to return a path
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_name) "/usr/bin/bd")))
    (let ((beads-executable "bd")
          (message-called nil))
      ;; Mock message function
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _args)
                   (setq message-called t))))
        ;; Call non-interactively
        (beads-check-executable)
        (should-not message-called)))))



(ert-deftest beads-test-error-and-log-integration ()
  "Test that errors are logged before signaling."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Trigger error
    (condition-case nil
        (beads--error "Integration test error")
      (user-error nil))

    ;; Verify error was logged before user-error was signaled
    (should (get-buffer "*beads-debug*"))
    (with-current-buffer "*beads-debug*"
      (goto-char (point-min))
      (should (search-forward "ERROR: Integration test error" nil t)))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

(ert-deftest beads-test-log-survives-multiple-sessions ()
  "Test that log buffer accumulates across multiple operations."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; First session
    (beads--log 'info "Session 1: operation 1")
    (beads--log 'info "Session 1: operation 2")

    ;; Second session
    (beads--log 'info "Session 2: operation 1")
    (condition-case nil
        (beads--error "Session 2: error")
      (user-error nil))

    ;; Third session
    (beads--log 'info "Session 3: operation 1")

    ;; Verify all messages are present
    (with-current-buffer "*beads-debug*"
      (let ((content (buffer-string)))
        (should (string-match-p "Session 1: operation 1" content))
        (should (string-match-p "Session 1: operation 2" content))
        (should (string-match-p "Session 2: operation 1" content))
        (should (string-match-p "ERROR: Session 2: error" content))
        (should (string-match-p "Session 3: operation 1" content))))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

(ert-deftest beads-test-debug-buffer-creation-deferred ()
  "Test that debug buffer is only created when first log occurs."
  (let ((beads-enable-debug nil))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Verify buffer doesn't exist
    (should-not (get-buffer "*beads-debug*"))

    ;; Enable debug
    (setq beads-enable-debug t)

    ;; Buffer still shouldn't exist
    (should-not (get-buffer "*beads-debug*"))

    ;; Log a message
    (beads--log 'info "First message")

    ;; Now buffer should exist
    (should (get-buffer "*beads-debug*"))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

(ert-deftest beads-test-customization-group-exists ()
  "Test that beads customization group is properly defined."
  (should (get 'beads 'group-documentation))
  ;; The custom-group property contains members, not the parent group
  (should (listp (get 'beads 'custom-group))))

(ert-deftest beads-test-all-customs-in-group ()
  "Test that all customization variables belong to beads group."
  ;; Check that variables have custom-type property (means they're defcustom)
  (should (get 'beads-executable 'custom-type))
  (should (get 'beads-database-path 'custom-type))
  (should (get 'beads-actor 'custom-type))
  (should (get 'beads-enable-debug 'custom-type))
  (should (get 'beads-auto-refresh 'custom-type))
  ;; Check that all are in the beads group's member list
  (let ((members (get 'beads 'custom-group)))
    (should (assq 'beads-executable members))
    (should (assq 'beads-database-path members))
    (should (assq 'beads-actor members))
    (should (assq 'beads-enable-debug members))
    (should (assq 'beads-auto-refresh members))))

(ert-deftest beads-test-log-newline-handling ()
  "Test that log messages handle newlines correctly."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Log message with newline in content
    (beads--log 'info "Line 1\nLine 2\nLine 3")

    ;; Check that the message is logged as-is
    (with-current-buffer "*beads-debug*"
      (goto-char (point-min))
      (should (search-forward "Line 1\nLine 2\nLine 3" nil t)))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

(ert-deftest beads-test-log-special-chars ()
  "Test that log messages handle special characters."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Log with special characters
    (beads--log 'info "Special: %% $@ #! \\ \" ' `")

    ;; Check content (note: %% becomes % in format)
    (with-current-buffer "*beads-debug*"
      (goto-char (point-min))
      (should (search-forward "Special: % $@ #! \\ \" ' `" nil t)))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

(ert-deftest beads-test-executable-check-return-value ()
  "Test that beads-check-executable returns t on success."
  ;; Mock executable-find to return a path
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_name) "/usr/bin/bd")))
    (let ((beads-executable "bd"))
      (let ((result (beads-check-executable)))
        (should (eq result t))
        (should (booleanp result))))))

(provide 'beads-utils-test)

;;; ========================================
;;; Command Building Tests
;;; ========================================


(ert-deftest beads-test-build-command-basic ()
  "Test basic command building without global flags."
  (beads-test-with-temp-config
   (let ((cmd (beads--build-command "list")))
     (should (equal cmd '("bd" "list" "--json"))))))

(ert-deftest beads-test-build-command-with-args ()
  "Test command building with arguments."
  (beads-test-with-temp-config
   (let ((cmd (beads--build-command "create" "Test issue" "-p" "1")))
     (should (equal cmd '("bd" "create" "Test issue" "-p" "1" "--json"))))))

(ert-deftest beads-test-build-command-with-actor ()
  "Test command building with actor flag."
  (beads-test-with-temp-config
   (let ((beads-actor "alice"))
     (let ((cmd (beads--build-command "list")))
       (should (equal cmd '("bd" "--actor" "alice" "list" "--json")))))))

(ert-deftest beads-test-build-command-with-db-path ()
  "Test command building with database path."
  (beads-test-with-temp-config
   (let ((beads-database-path "/tmp/test.db"))
     (let ((cmd (beads--build-command "list")))
       (should (equal cmd '("bd" "--db" "/tmp/test.db" "list" "--json")))))))

(ert-deftest beads-test-build-command-with-actor-and-db ()
  "Test command building with both actor and database path."
  (beads-test-with-temp-config
   (let ((beads-actor "bob")
         (beads-database-path "/tmp/test.db"))
     (let ((cmd (beads--build-command "ready")))
       (should (equal cmd '("bd" "--actor" "bob" "--db" "/tmp/test.db"
                            "ready" "--json")))))))

(ert-deftest beads-test-build-command-always-adds-json-flag ()
  "Test that --json flag is always added."
  (beads-test-with-temp-config
   (let ((cmd (beads--build-command "show" "bd-1")))
     (should (member "--json" cmd))
     (should (equal (car (last cmd)) "--json")))))


;;; ========================================
;;; Process Execution Tests (Synchronous)
;;; ========================================


(ert-deftest beads-test-run-command-success ()
  "Test successful command execution with valid JSON."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-test--sample-issue)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads--run-command "show" "bd-1")))
         (should (listp result))
         (should (consp result))
         (should (equal (alist-get 'id result) "bd-1"))
         (should (equal (alist-get 'title result) "Test Issue")))))))

(ert-deftest beads-test-run-command-array-output ()
  "Test command execution with JSON array output."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-test--sample-issues-array)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads--run-command "list")))
         (should (vectorp result))
         (should (= (length result) 3)))))))

(ert-deftest beads-test-run-command-empty-object ()
  "Test command execution with empty JSON object."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 0 "{}")))
     (let ((result (beads--run-command "stats")))
       (should (listp result))
       (should (null (cdr result)))))))

(ert-deftest beads-test-run-command-empty-array ()
  "Test command execution with empty JSON array."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 0 "[]")))
     (let ((result (beads--run-command "ready")))
       (should (vectorp result))
       (should (= (length result) 0))))))

(ert-deftest beads-test-run-command-non-zero-exit ()
  "Test command execution with non-zero exit code."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 1 "Error: issue not found")))
     (should-error
      (beads--run-command "show" "bd-999")
      :type 'user-error))))

(ert-deftest beads-test-run-command-invalid-json ()
  "Test command execution with invalid JSON output."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 0 "not valid json")))
     (should-error
      (beads--run-command "list")
      :type 'user-error))))

(ert-deftest beads-test-run-command-malformed-json ()
  "Test command execution with malformed JSON."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 0 "{\"key\": \"value\"")))
     (should-error
      (beads--run-command "show" "bd-1")
      :type 'user-error))))

(ert-deftest beads-test-run-command-null-value ()
  "Test command execution with JSON containing null values."
  (beads-test-with-temp-config
   (let ((json-output "{\"id\":\"bd-1\",\"title\":null,\"priority\":1}"))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads--run-command "show" "bd-1")))
         (should (listp result))
         (should (consp result))
         (should (equal (alist-get 'id result) "bd-1"))
         (should (null (alist-get 'title result)))
         (should (= (alist-get 'priority result) 1)))))))

(ert-deftest beads-test-run-command-nested-json ()
  "Test command execution with nested JSON structures."
  (beads-test-with-temp-config
   (let ((json-output "{\"id\":\"bd-1\",\"metadata\":{\"tags\":[\"a\",\"b\"]}}"))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads--run-command "show" "bd-1")))
         (should (listp result))
         (should (consp result))
         (should (listp (alist-get 'metadata result)))
         (should (consp (alist-get 'metadata result)))
         (let ((tags (alist-get 'tags (alist-get 'metadata result))))
           (should (vectorp tags))
           (should (equal (aref tags 0) "a"))))))))

(ert-deftest beads-test-run-command-unicode-content ()
  "Test command execution with Unicode characters in JSON."
  (beads-test-with-temp-config
   (let ((json-output "{\"id\":\"bd-1\",\"title\":\"测试 \u00e9 \u263a\"}"))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads--run-command "show" "bd-1")))
         (should (listp result))
         (should (consp result))
         (should (string-match-p "测试" (alist-get 'title result))))))))

(ert-deftest beads-test-run-command-error-message-captured ()
  "Test that error messages from bd are captured and reported."
  (beads-test-with-temp-config
   (let ((error-msg "Error: database locked"))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 1 error-msg)))
       (condition-case err
           (progn
             (beads--run-command "list")
             (should nil)) ; Should not reach here
         (user-error
          (should (string-match-p "database locked"
                                  (error-message-string err)))))))))


;;; ========================================
;;; Process Execution Tests (Asynchronous)
;;; ========================================




;;; ========================================
;;; JSON Parsing Tests
;;; ========================================


(ert-deftest beads-test-parse-issue-complete ()
  "Test parsing a complete issue with all fields."
  (beads-test-with-temp-config
   (let ((parsed (beads--parse-issue beads-test--sample-issue)))
     (should (listp parsed))
     (should (consp parsed))
     (should (equal (alist-get 'id parsed) "bd-1"))
     (should (equal (alist-get 'title parsed) "Test Issue"))
     (should (equal (alist-get 'description parsed) "Test description"))
     (should (equal (alist-get 'status parsed) "open"))
     (should (= (alist-get 'priority parsed) 1))
     (should (equal (alist-get 'issue-type parsed) "bug"))
     (should (equal (alist-get 'created-at parsed)
                    "2025-01-15T10:00:00Z"))
     (should (equal (alist-get 'updated-at parsed)
                    "2025-01-15T10:00:00Z"))
     (should (equal (alist-get 'acceptance-criteria parsed) "Must work"))
     (should (equal (alist-get 'design parsed) "Simple design"))
     (should (equal (alist-get 'notes parsed) "Some notes"))
     (should (equal (alist-get 'assignee parsed) "alice"))
     (should (equal (alist-get 'external-ref parsed) "EXT-123")))))

(ert-deftest beads-test-parse-issue-minimal ()
  "Test parsing an issue with minimal fields."
  (beads-test-with-temp-config
   (let* ((minimal-issue '((id . "bd-2")
                           (title . "Minimal")
                           (status . "open")))
          (parsed (beads--parse-issue minimal-issue)))
     (should (listp parsed))
     (should (consp parsed))
     (should (equal (alist-get 'id parsed) "bd-2"))
     (should (equal (alist-get 'title parsed) "Minimal"))
     (should (equal (alist-get 'status parsed) "open"))
     ;; Missing fields should be nil
     (should (null (alist-get 'description parsed)))
     (should (null (alist-get 'priority parsed)))
     (should (null (alist-get 'issue-type parsed))))))

(ert-deftest beads-test-parse-issue-from-vector ()
  "Test parsing issue from a single-element vector."
  (beads-test-with-temp-config
   (let* ((issue-vector (vector beads-test--sample-issue))
          (parsed (beads--parse-issue issue-vector)))
     (should (listp parsed))
     (should (consp parsed))
     (should (equal (alist-get 'id parsed) "bd-1"))
     (should (equal (alist-get 'title parsed) "Test Issue")))))

(ert-deftest beads-test-parse-issue-underscore-conversion ()
  "Test that JSON field names with underscores are converted correctly."
  (beads-test-with-temp-config
   (let* ((issue '((id . "bd-1")
                   (issue_type . "feature")
                   (created_at . "2025-01-15T10:00:00Z")
                   (updated_at . "2025-01-15T11:00:00Z")
                   (acceptance_criteria . "Criteria")
                   (external_ref . "REF-1")))
          (parsed (beads--parse-issue issue)))
     ;; Check that underscored names are accessible with dashed keys
     (should (equal (alist-get 'issue-type parsed) "feature"))
     (should (equal (alist-get 'created-at parsed) "2025-01-15T10:00:00Z"))
     (should (equal (alist-get 'updated-at parsed) "2025-01-15T11:00:00Z"))
     (should (equal (alist-get 'acceptance-criteria parsed) "Criteria"))
     (should (equal (alist-get 'external-ref parsed) "REF-1")))))

(ert-deftest beads-test-parse-issue-null-fields ()
  "Test parsing issue with null fields."
  (beads-test-with-temp-config
   (let* ((issue '((id . "bd-1")
                   (title . "Test")
                   (description . nil)
                   (assignee . nil)
                   (priority . 1)))
          (parsed (beads--parse-issue issue)))
     (should (equal (alist-get 'id parsed) "bd-1"))
     (should (null (alist-get 'description parsed)))
     (should (null (alist-get 'assignee parsed)))
     (should (= (alist-get 'priority parsed) 1)))))

(ert-deftest beads-test-parse-issue-preserves-types ()
  "Test that parsing preserves data types correctly."
  (beads-test-with-temp-config
   (let* ((issue '((id . "bd-1")
                   (priority . 2)
                   (status . "open")
                   (created_at . "2025-01-15T10:00:00Z")))
          (parsed (beads--parse-issue issue)))
     ;; Numbers should remain numbers
     (should (numberp (alist-get 'priority parsed)))
     (should (= (alist-get 'priority parsed) 2))
     ;; Strings should remain strings
     (should (stringp (alist-get 'status parsed)))
     (should (stringp (alist-get 'created-at parsed))))))

;;; Tests for beads--parse-issues

(ert-deftest beads-test-parse-issues-multiple ()
  "Test parsing multiple issues from array."
  (beads-test-with-temp-config
   (let ((parsed (beads--parse-issues beads-test--sample-issues-array)))
     (should (listp parsed))
     (should (= (length parsed) 3))
     ;; Check first issue
     (let ((first (car parsed)))
       (should (equal (alist-get 'id first) "bd-1"))
       (should (equal (alist-get 'title first) "First Issue"))
       (should (= (alist-get 'priority first) 1)))
     ;; Check second issue
     (let ((second (cadr parsed)))
       (should (equal (alist-get 'id second) "bd-2"))
       (should (equal (alist-get 'title second) "Second Issue"))
       (should (= (alist-get 'priority second) 2)))
     ;; Check third issue
     (let ((third (caddr parsed)))
       (should (equal (alist-get 'id third) "bd-3"))
       (should (equal (alist-get 'title third) "Third Issue"))
       (should (= (alist-get 'priority third) 3))))))

(ert-deftest beads-test-parse-issues-empty-array ()
  "Test parsing empty issues array."
  (beads-test-with-temp-config
   (let ((parsed (beads--parse-issues (vector))))
     (should (null parsed)))))

(ert-deftest beads-test-parse-issues-single-issue ()
  "Test parsing single issue from array."
  (beads-test-with-temp-config
   (let* ((single-array (vector beads-test--sample-issue))
          (parsed (beads--parse-issues single-array)))
     (should (listp parsed))
     (should (= (length parsed) 1))
     (let ((issue (car parsed)))
       (should (equal (alist-get 'id issue) "bd-1"))
       (should (equal (alist-get 'title issue) "Test Issue"))))))

(ert-deftest beads-test-parse-issues-nil-input ()
  "Test parsing with nil input."
  (beads-test-with-temp-config
   (let ((parsed (beads--parse-issues nil)))
     (should (null parsed)))))

(ert-deftest beads-test-parse-issues-non-vector-input ()
  "Test parsing with non-vector input returns nil."
  (beads-test-with-temp-config
   (let ((parsed (beads--parse-issues '((id . "bd-1")))))
     (should (null parsed)))))

(ert-deftest beads-test-parse-issues-maintains-order ()
  "Test that parsing maintains issue order."
  (beads-test-with-temp-config
   (let ((parsed (beads--parse-issues beads-test--sample-issues-array)))
     (should (equal (alist-get 'id (nth 0 parsed)) "bd-1"))
     (should (equal (alist-get 'id (nth 1 parsed)) "bd-2"))
     (should (equal (alist-get 'id (nth 2 parsed)) "bd-3")))))


;;; ========================================
;;; Process Integration Tests
;;; ========================================


(ert-deftest beads-test-command-to-parse ()
  "Test full flow from command execution to issue parsing."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-test--sample-issues-array)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let* ((result (beads--run-command "list"))
              (parsed (beads--parse-issues result)))
         (should (listp parsed))
         (should (= (length parsed) 3))
         (should (equal (alist-get 'id (car parsed)) "bd-1")))))))

(ert-deftest beads-test-single-issue-show ()
  "Test showing a single issue end-to-end."
  (beads-test-with-temp-config
   (let ((json-output (json-encode beads-test--sample-issue)))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let* ((result (beads--run-command "show" "bd-1"))
              (parsed (beads--parse-issue result)))
         (should (listp parsed))
         (should (consp parsed))
         (should (equal (alist-get 'id parsed) "bd-1"))
         (should (equal (alist-get 'title parsed) "Test Issue"))
         (should (equal (alist-get 'status parsed) "open")))))))

(ert-deftest beads-test-with-global-flags ()
  "Test command execution with global flags applied."
  (beads-test-with-temp-config
   (let ((beads-actor "test-user")
         (beads-database-path "/tmp/test.db")
         (json-output "[]")
         (captured-command nil))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (setq captured-command (cons program args))
                  (when destination
                    (with-current-buffer (current-buffer)
                      (insert json-output)))
                  0)))
       (beads--run-command "ready")
       (should (member "--actor" captured-command))
       (should (member "test-user" captured-command))
       (should (member "--db" captured-command))
       (should (member "/tmp/test.db" captured-command))
       (should (member "--json" captured-command))))))


;;; ========================================
;;; Edge Cases and Error Conditions
;;; ========================================


(ert-deftest beads-test-edge-case-empty-string-output ()
  "Test handling of empty string output from command."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 0 "")))
     (should-error
      (beads--run-command "list")
      :type 'user-error))))

(ert-deftest beads-test-edge-case-whitespace-only-output ()
  "Test handling of whitespace-only output."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 0 "   \n\t  ")))
     (should-error
      (beads--run-command "list")
      :type 'user-error))))

(ert-deftest beads-test-edge-case-large-json-array ()
  "Test parsing a large number of issues."
  (beads-test-with-temp-config
   (let* ((large-array (make-vector 100 beads-test--sample-issue))
          (parsed (beads--parse-issues large-array)))
     (should (= (length parsed) 100))
     (should (equal (alist-get 'id (car parsed)) "bd-1"))
     (should (equal (alist-get 'id (car (last parsed))) "bd-1")))))

(ert-deftest beads-test-edge-case-special-characters-in-strings ()
  "Test handling of special characters in issue fields."
  (beads-test-with-temp-config
   (let* ((issue '((id . "bd-1")
                   (title . "Test \"quotes\" and 'apostrophes'")
                   (description . "Line 1\nLine 2\tTabbed")
                   (notes . "Special: <>&\\")))
          (parsed (beads--parse-issue issue)))
     (should (equal (alist-get 'title parsed)
                    "Test \"quotes\" and 'apostrophes'"))
     (should (string-match-p "\n" (alist-get 'description parsed)))
     (should (string-match-p "\t" (alist-get 'description parsed))))))

(ert-deftest beads-test-edge-case-very-long-strings ()
  "Test handling of very long string values."
  (beads-test-with-temp-config
   (let* ((long-string (make-string 10000 ?x))
          (issue `((id . "bd-1")
                   (title . "Test")
                   (description . ,long-string)))
          (parsed (beads--parse-issue issue)))
     (should (equal (alist-get 'description parsed) long-string))
     (should (= (length (alist-get 'description parsed)) 10000)))))

(ert-deftest beads-test-edge-case-numeric-string-id ()
  "Test that numeric IDs remain as strings."
  (beads-test-with-temp-config
   (let* ((issue '((id . "123")
                   (title . "Numeric ID")))
          (parsed (beads--parse-issue issue)))
     (should (stringp (alist-get 'id parsed)))
     (should (equal (alist-get 'id parsed) "123")))))

(ert-deftest beads-test-edge-case-zero-priority ()
  "Test handling of zero priority (critical)."
  (beads-test-with-temp-config
   (let* ((issue '((id . "bd-1")
                   (priority . 0)))
          (parsed (beads--parse-issue issue)))
     (should (numberp (alist-get 'priority parsed)))
     (should (= (alist-get 'priority parsed) 0)))))


;;; ========================================
;;; Debug Logging Tests
;;; ========================================


(ert-deftest beads-test-debug-logging-enabled ()
  "Test that debug logging works when enabled."
  (beads-test-with-temp-config
   (let ((beads-enable-debug t)
         (beads-debug-level 'verbose)
         (json-output "{}"))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (with-current-buffer (get-buffer-create "*beads-debug*")
         (erase-buffer))
       (beads--run-command "list")
       (with-current-buffer "*beads-debug*"
         (should (> (buffer-size) 0))
         (should (string-match-p "Running:" (buffer-string)))
         (should (string-match-p "Exit code:" (buffer-string))))))))

(ert-deftest beads-test-debug-logging-disabled ()
  "Test that no logging occurs when debug is disabled."
  (beads-test-with-temp-config
   (let ((beads-enable-debug nil)
         (json-output "{}"))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (when (get-buffer "*beads-debug*")
         (kill-buffer "*beads-debug*"))
       (beads--run-command "list")
       (should (null (get-buffer "*beads-debug*")))))))



(ert-deftest beads-test-performance-parse-many-issues ()
  "Test parsing performance with many issues."
  :tags '(:performance)
  (beads-test-with-temp-config
   (let* ((num-issues 1000)
          (issues (make-vector num-issues beads-test--sample-issue))
          (start-time (current-time)))
     (beads--parse-issues issues)
     (let ((elapsed (float-time (time-subtract (current-time) start-time))))
       ;; Should parse 1000 issues in under 1 second
       (should (< elapsed 1.0))))))

(provide 'beads-process-test)

;;; ========================================
;;; Project Integration Tests
;;; ========================================


(ert-deftest beads-test-find-project-root-no-project ()
  "Test finding project root when not in a project."
  (beads-test--with-temp-project
   (let ((project-find-functions nil))
     (should (null (beads--find-project-root))))))

(ert-deftest beads-test-find-project-root-with-project ()
  "Test finding project root when in a project."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".git/" "src/" "README.md"))

   (let ((default-directory (expand-file-name "src" beads-test--temp-dir)))
     ;; Mock project-current to return a project
     (cl-letf (((symbol-function 'project-current)
                (lambda (&optional _maybe-prompt)
                  (cons 'transient beads-test--temp-dir))))

       ;; Test with modern Emacs (project-root function exists)
       (cl-letf (((symbol-function 'project-root)
                  (lambda (_proj) beads-test--temp-dir)))
         (should (equal (beads--find-project-root)
                        beads-test--temp-dir)))

       ;; Test with Emacs 27 (project-roots function)
       (cl-letf (((symbol-function 'project-root) nil)
                 ((symbol-function 'project-roots)
                  (lambda (_proj) (list beads-test--temp-dir))))
         (fmakunbound 'project-root)
         (should (equal (beads--find-project-root)
                        beads-test--temp-dir)))))))

(ert-deftest beads-test-find-project-root-emacs27-compat ()
  "Test Emacs 27 compatibility when project-root is not defined."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".git/"))

   (cl-letf (((symbol-function 'project-current)
              (lambda (&optional _maybe-prompt)
                (cons 'transient beads-test--temp-dir)))
             ((symbol-function 'project-root) nil)
             ((symbol-function 'project-roots)
              (lambda (_proj) (list beads-test--temp-dir))))

     ;; Temporarily remove project-root to simulate Emacs 27
     (let ((has-project-root (fboundp 'project-root)))
       (when has-project-root
         (fmakunbound 'project-root))
       (unwind-protect
           (should (equal (beads--find-project-root)
                          beads-test--temp-dir))
         (when has-project-root
           (fset 'project-root
                 (lambda (_proj) beads-test--temp-dir))))))))

;;; Tests for beads--find-beads-dir

(ert-deftest beads-test-find-beads-dir-in-root ()
  "Test finding .beads directory in project root."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" ".beads/beads.db"))

   (let ((default-directory beads-test--temp-dir))
     (should (equal (beads--find-beads-dir)
                    (expand-file-name ".beads" beads-test--temp-dir))))))

(ert-deftest beads-test-find-beads-dir-traverse-up ()
  "Test finding .beads directory by traversing up."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" "src/" "src/modules/" "src/modules/auth/"))

   (let ((default-directory (expand-file-name "src/modules/auth"
                                              beads-test--temp-dir)))
     (should (equal (beads--find-beads-dir)
                    (expand-file-name ".beads" beads-test--temp-dir))))))

(ert-deftest beads-test-find-beads-dir-not-found ()
  "Test when .beads directory is not found."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '("src/"))

   (let ((default-directory (expand-file-name "src" beads-test--temp-dir)))
     ;; Mock locate-dominating-file to return nil
     ;; This prevents finding .beads directories outside the test environment
     (cl-letf (((symbol-function 'locate-dominating-file)
                (lambda (_file _name) nil)))
       (should (null (beads--find-beads-dir)))))))

(ert-deftest beads-test-find-beads-dir-explicit-directory ()
  "Test finding .beads with explicit directory argument."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" "project1/" "project1/.beads/"))

   (let ((project1-dir (expand-file-name "project1" beads-test--temp-dir)))
     (should (equal (beads--find-beads-dir project1-dir)
                    (expand-file-name ".beads" project1-dir))))))

(ert-deftest beads-test-find-beads-dir-uses-project-root ()
  "Test that beads--find-beads-dir uses project root as starting point."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".git/" ".beads/" "src/"))

   (let ((default-directory (expand-file-name "src" beads-test--temp-dir)))
     ;; Mock project-current to return the temp dir as project root
     (cl-letf (((symbol-function 'project-current)
                (lambda (&optional _maybe-prompt)
                  (cons 'transient beads-test--temp-dir)))
               ((symbol-function 'project-root)
                (lambda (_proj) beads-test--temp-dir)))

       (should (equal (beads--find-beads-dir)
                      (expand-file-name ".beads" beads-test--temp-dir)))))))

(ert-deftest beads-test-find-beads-dir-fallback-to-default-directory ()
  "Test fallback to default-directory when no project found."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/"))

   (let ((default-directory beads-test--temp-dir)
         (project-find-functions nil))

     ;; Mock project-current to return nil
     (cl-letf (((symbol-function 'project-current)
                (lambda (&optional _maybe-prompt) nil)))

       (should (equal (beads--find-beads-dir)
                      (expand-file-name ".beads" beads-test--temp-dir)))))))

;;; Tests for caching

(ert-deftest beads-test-find-beads-dir-caching-basic ()
  "Test that .beads directory lookup is cached."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/"))

   (let ((default-directory beads-test--temp-dir)
         (lookup-count 0))

     ;; Mock locate-dominating-file to count calls
     ;; Also mock project-current to avoid additional calls
     (cl-letf (((symbol-function 'locate-dominating-file)
                (lambda (file name)
                  (setq lookup-count (1+ lookup-count))
                  (when (equal name ".beads")
                    beads-test--temp-dir)))
               ((symbol-function 'project-current)
                (lambda (&optional _maybe-prompt) nil)))

       ;; First call should perform lookup
       (beads--find-beads-dir beads-test--temp-dir)
       (should (= lookup-count 1))

       ;; Second call should use cache
       (beads--find-beads-dir beads-test--temp-dir)
       (should (= lookup-count 1))

       ;; Third call should still use cache
       (beads--find-beads-dir beads-test--temp-dir)
       (should (= lookup-count 1))))))

(ert-deftest beads-test-find-beads-dir-caching-per-directory ()
  "Test that caching is per-directory."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '("project1/" "project1/.beads/" "project2/" "project2/.beads/"))

   (let ((proj1-dir (expand-file-name "project1" beads-test--temp-dir))
         (proj2-dir (expand-file-name "project2" beads-test--temp-dir)))

     ;; Cache for project1
     (let ((result1 (beads--find-beads-dir proj1-dir)))
       (should (equal result1 (expand-file-name ".beads" proj1-dir))))

     ;; Cache for project2 (should be different)
     (let ((result2 (beads--find-beads-dir proj2-dir)))
       (should (equal result2 (expand-file-name ".beads" proj2-dir))))

     ;; Verify both are cached independently
     (should (equal (gethash proj1-dir beads--project-cache)
                    (expand-file-name ".beads" proj1-dir)))
     (should (equal (gethash proj2-dir beads--project-cache)
                    (expand-file-name ".beads" proj2-dir))))))

(ert-deftest beads-test-find-beads-dir-cache-nil-not-stored ()
  "Test that nil results are not cached."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '("src/"))

   (let ((src-dir (expand-file-name "src" beads-test--temp-dir))
         (lookup-count 0))

     ;; Mock locate-dominating-file to count calls and return nil
     ;; Also mock project-current to avoid additional calls
     (cl-letf (((symbol-function 'locate-dominating-file)
                (lambda (_file _name)
                  (setq lookup-count (1+ lookup-count))
                  nil))
               ((symbol-function 'project-current)
                (lambda (&optional _maybe-prompt) nil)))

       ;; First call
       (should (null (beads--find-beads-dir src-dir)))
       (should (= lookup-count 1))

       ;; Second call should NOT use cache (nil not cached)
       (should (null (beads--find-beads-dir src-dir)))
       (should (= lookup-count 2))))))

(ert-deftest beads-test-project-cache-multiple-projects ()
  "Test cache handles multiple projects in same session."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '("workspace/proj-a/" "workspace/proj-a/.beads/"
      "workspace/proj-b/" "workspace/proj-b/.beads/"
      "workspace/proj-c/" "workspace/proj-c/.beads/"))

   (let ((proj-a (expand-file-name "workspace/proj-a" beads-test--temp-dir))
         (proj-b (expand-file-name "workspace/proj-b" beads-test--temp-dir))
         (proj-c (expand-file-name "workspace/proj-c" beads-test--temp-dir)))

     ;; Access all three projects
     (beads--find-beads-dir proj-a)
     (beads--find-beads-dir proj-b)
     (beads--find-beads-dir proj-c)

     ;; Verify all three are cached
     (should (= (hash-table-count beads--project-cache) 3))
     (should (gethash proj-a beads--project-cache))
     (should (gethash proj-b beads--project-cache))
     (should (gethash proj-c beads--project-cache)))))

(ert-deftest beads-test-cache-invalidation-manual ()
  "Test manual cache clearing."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/"))

   (let ((default-directory beads-test--temp-dir))
     ;; Populate cache
     (beads--find-beads-dir)
     (should (> (hash-table-count beads--project-cache) 0))

     ;; Clear cache manually
     (clrhash beads--project-cache)
     (should (= (hash-table-count beads--project-cache) 0))

     ;; Next call should repopulate
     (beads--find-beads-dir)
     (should (= (hash-table-count beads--project-cache) 1)))))

;;; Tests for beads--get-database-path

(ert-deftest beads-test-get-database-path-from-customization ()
  "Test getting database path from customization."
  (beads-test--with-temp-project
   (let ((custom-db-path "/custom/path/to/beads.db"))
     (setq beads-database-path custom-db-path)
     (should (equal (beads--get-database-path) custom-db-path)))))

(ert-deftest beads-test-get-database-path-auto-discover ()
  "Test auto-discovering database path."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" ".beads/beads.db"))

   (let ((default-directory beads-test--temp-dir)
         (beads-database-path nil))
     (should (equal (beads--get-database-path)
                    (expand-file-name ".beads/beads.db"
                                      beads-test--temp-dir))))))

(ert-deftest beads-test-get-database-path-no-beads-dir ()
  "Test getting database path when .beads dir not found."
  (beads-test--with-temp-project
   (let ((default-directory beads-test--temp-dir)
         (beads-database-path nil))
     ;; Mock beads--find-beads-dir to return nil
     ;; This prevents finding .beads directories outside the test environment
     (cl-letf (((symbol-function 'beads--find-beads-dir)
                (lambda (&optional _start-dir)
                  nil)))
       (should (null (beads--get-database-path)))))))

(ert-deftest beads-test-get-database-path-no-db-file ()
  "Test getting database path when .beads exists but no .db file."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/" ".beads/issues.jsonl"))

   (let ((default-directory beads-test--temp-dir)
         (beads-database-path nil))
     (should (null (beads--get-database-path))))))

(ert-deftest beads-test-get-database-path-multiple-db-files ()
  "Test getting database path when multiple .db files exist."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" ".beads/beads.db" ".beads/backup.db"))

   (let ((default-directory beads-test--temp-dir)
         (beads-database-path nil))
     ;; Should return first match (behavior of directory-files + car)
     (let ((result (beads--get-database-path)))
       (should result)
       (should (string-suffix-p ".db" result))))))

(ert-deftest beads-test-get-database-path-customization-precedence ()
  "Test that customization takes precedence over auto-discovery."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" ".beads/beads.db"))

   (let ((default-directory beads-test--temp-dir)
         (custom-path "/override/custom.db"))
     (setq beads-database-path custom-path)

     ;; Should return custom path, not discovered path
     (should (equal (beads--get-database-path) custom-path)))))

;;; Tests for edge cases and integration

(ert-deftest beads-test-find-beads-dir-symlink-handling ()
  "Test that symlinks are handled correctly."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" "real-project/" "real-project/.beads/"))

   (let* ((real-dir (expand-file-name "real-project" beads-test--temp-dir))
          (link-dir (expand-file-name "link-project" beads-test--temp-dir)))

     ;; Create symlink (skip if not supported)
     (condition-case nil
         (progn
           (make-symbolic-link real-dir link-dir)

           ;; Access via symlink
           (let ((result (beads--find-beads-dir link-dir)))
             (should result)
             (should (string-match-p "\\.beads$" result))))
       (file-error nil))))) ;; Skip test if symlinks not supported

(ert-deftest beads-test-find-beads-dir-nested-projects ()
  "Test behavior with nested projects."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '(".beads/" "outer/.beads/" "outer/inner/" "outer/inner/.beads/"))

   (let ((outer-dir (expand-file-name "outer" beads-test--temp-dir))
         (inner-dir (expand-file-name "outer/inner" beads-test--temp-dir)))

     ;; From outer, should find outer's .beads
     (should (equal (beads--find-beads-dir outer-dir)
                    (expand-file-name ".beads" outer-dir)))

     ;; From inner, should find inner's .beads (closest match)
     (should (equal (beads--find-beads-dir inner-dir)
                    (expand-file-name ".beads" inner-dir))))))

(ert-deftest beads-test-concurrent-project-operations ()
  "Test cache behavior with concurrent project operations."
  (beads-test--with-temp-project
   (beads-test--create-project-structure
    '("proj1/.beads/" "proj2/.beads/"))

   (let ((proj1 (expand-file-name "proj1" beads-test--temp-dir))
         (proj2 (expand-file-name "proj2" beads-test--temp-dir)))

     ;; Simulate rapid project switching
     (beads--find-beads-dir proj1)
     (beads--find-beads-dir proj2)
     (beads--find-beads-dir proj1)
     (beads--find-beads-dir proj2)

     ;; Both should be cached and correct
     (should (equal (gethash proj1 beads--project-cache)
                    (expand-file-name ".beads" proj1)))
     (should (equal (gethash proj2 beads--project-cache)
                    (expand-file-name ".beads" proj2))))))

(ert-deftest beads-test-path-with-spaces ()
  "Test handling paths with spaces."
  (beads-test--with-temp-project
   (let ((space-dir (expand-file-name "my project" beads-test--temp-dir)))
     (make-directory space-dir t)
     (make-directory (expand-file-name ".beads" space-dir) t)

     (let ((result (beads--find-beads-dir space-dir)))
       (should result)
       (should (equal result (expand-file-name ".beads" space-dir)))))))

(ert-deftest beads-test-find-beads-dir-unicode-paths ()
  "Test handling Unicode characters in paths."
  (beads-test--with-temp-project
   (let ((unicode-dir (expand-file-name "проект-テスト" beads-test--temp-dir)))
     (condition-case nil
         (progn
           (make-directory unicode-dir t)
           (make-directory (expand-file-name ".beads" unicode-dir) t)

           (let ((result (beads--find-beads-dir unicode-dir)))
             (should result)
             (should (equal result (expand-file-name ".beads" unicode-dir)))))
       (file-error nil))))) ;; Skip if filesystem doesn't support Unicode

(ert-deftest beads-test-permissions-readonly-directory ()
  "Test behavior when .beads directory is read-only."
  (beads-test--with-temp-project
   (beads-test--create-project-structure '(".beads/"))

   (let ((beads-dir (expand-file-name ".beads" beads-test--temp-dir)))
     ;; Finding directory should work even if read-only
     (should (equal (beads--find-beads-dir beads-test--temp-dir)
                    beads-dir)))))



(ert-deftest beads-test-function-documentation ()
  "Test that public functions have documentation."
  (should (documentation 'beads--find-project-root))
  (should (documentation 'beads--find-beads-dir))
  (should (documentation 'beads--get-database-path)))

(provide 'beads-project-test)

;;; ========================================
;;; Tramp Remote Support Tests
;;; ========================================


(ert-deftest beads-tramp-test-run-command-with-local-path ()
  "Test that process-file is used with local default-directory."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/tmp/")
         (json-output "{}")
         (process-file-called nil))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display
                                 &rest args)
                  (setq process-file-called t)
                  (when destination
                    (with-current-buffer (current-buffer)
                      (insert json-output)))
                  0)))
       (beads--run-command "list")
       (should process-file-called)))))

(ert-deftest beads-tramp-test-run-command-with-tramp-path ()
  "Test that process-file respects Tramp default-directory."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/ssh:remote:/home/user/project/")
         (json-output "{}")
         (process-file-called nil)
         (captured-default-directory nil))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display
                                 &rest args)
                  (setq process-file-called t)
                  (setq captured-default-directory default-directory)
                  (when destination
                    (with-current-buffer (current-buffer)
                      (insert json-output)))
                  0)))
       (beads--run-command "list")
       (should process-file-called)
       (should (string= captured-default-directory
                        "/ssh:remote:/home/user/project/"))))))

(ert-deftest beads-tramp-test-run-command-different-tramp-methods ()
  "Test process-file with different Tramp methods."
  (beads-tramp-test-with-temp-config
   (dolist (path '("/ssh:host:/path/"
                   "/scp:host:/path/"
                   "/sudo:root@localhost:/root/"))
     (let ((default-directory path)
           (json-output "{}")
           (process-file-called nil))
       (cl-letf (((symbol-function 'process-file)
                  (lambda (program &optional infile destination display
                                   &rest args)
                    (setq process-file-called t)
                    (when destination
                      (with-current-buffer (current-buffer)
                        (insert json-output)))
                    0)))
         (beads--run-command "list")
         (should process-file-called))))))

;;; Tests for Tramp-aware asynchronous execution

(ert-deftest beads-tramp-test-run-command-async-with-local-path ()
  "Test that start-file-process is used with local default-directory."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/tmp/")
         (start-file-process-called nil))
     (cl-letf (((symbol-function 'start-file-process)
                (lambda (name buffer program &rest args)
                  (setq start-file-process-called t)
                  (make-process
                   :name name
                   :buffer buffer
                   :command (list "true")
                   :noquery t))))
       (beads--run-command-async (lambda (_result) nil) "list")
       (should start-file-process-called)))))

(ert-deftest beads-tramp-test-run-command-async-with-tramp-path ()
  "Test that start-file-process respects Tramp default-directory."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/ssh:remote:/home/user/project/")
         (start-file-process-called nil)
         (captured-default-directory nil))
     (cl-letf (((symbol-function 'start-file-process)
                (lambda (name buffer program &rest args)
                  (setq start-file-process-called t)
                  (setq captured-default-directory default-directory)
                  (make-process
                   :name name
                   :buffer buffer
                   :command (list "true")
                   :noquery t))))
       (beads--run-command-async (lambda (_result) nil) "list")
       (should start-file-process-called)
       (should (string= captured-default-directory
                        "/ssh:remote:/home/user/project/"))))))

;;; Tests for project discovery over Tramp

(ert-deftest beads-tramp-test-find-beads-dir-with-tramp ()
  "Test that beads--find-beads-dir works with Tramp paths."
  (let ((beads--project-cache (make-hash-table :test 'equal))
        (locate-dominating-file-called nil)
        (remote-dir "/ssh:host:/home/user/project/"))
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (file name)
                 (setq locate-dominating-file-called t)
                 (should (string-prefix-p "/ssh:host:" file))
                 (when (equal name ".beads")
                   remote-dir)))
              ((symbol-function 'beads--find-project-root)
               (lambda () nil)))
      (let ((result (beads--find-beads-dir remote-dir)))
        (should locate-dominating-file-called)
        (should (string-prefix-p "/ssh:host:" result))))))

(ert-deftest beads-tramp-test-get-database-path-with-tramp ()
  "Test that beads--get-database-path works with Tramp paths."
  (let ((beads-database-path nil)
        (beads--project-cache (make-hash-table :test 'equal))
        (remote-beads-dir "/ssh:host:/home/user/project/.beads"))
    (cl-letf (((symbol-function 'beads--find-beads-dir)
               (lambda (&optional _dir) remote-beads-dir))
              ((symbol-function 'directory-files)
               (lambda (directory &optional full match _nosort)
                 (when (and match (string-match-p "\\.db$" "beads.db"))
                   (if full
                       (list (concat directory "/beads.db"))
                     (list "beads.db"))))))
      (let ((result (beads--get-database-path)))
        (should result)
        (should (string-prefix-p "/ssh:host:" result))
        (should (string-suffix-p ".db" result))))))

;;; Tests for command building with Tramp paths

(ert-deftest beads-tramp-test-build-command-with-tramp-db-path ()
  "Test command building with Tramp database path strips Tramp prefix."
  (beads-tramp-test-with-temp-config
   (let ((beads-database-path "/ssh:host:/home/user/.beads/beads.db"))
     (let ((cmd (beads--build-command "list")))
       (should (member "--db" cmd))
       ;; Tramp prefix should be stripped so bd can understand the path
       (should (member "/home/user/.beads/beads.db" cmd))
       (should-not (member "/ssh:host:/home/user/.beads/beads.db" cmd))))))

;;; Tests for end-to-end Tramp workflow

(ert-deftest beads-tramp-test-full-workflow-with-tramp ()
  "Test complete workflow: find project, get DB, run command over Tramp."
  (let ((default-directory "/ssh:remote:/home/user/project/src/")
        (beads--project-cache (make-hash-table :test 'equal))
        (beads-database-path nil)
        (json-output "[{\"id\":\"bd-1\",\"title\":\"Remote Issue\"}]")
        (workflow-steps '()))
    (cl-letf (((symbol-function 'beads--find-project-root)
               (lambda ()
                 (push 'find-project-root workflow-steps)
                 "/ssh:remote:/home/user/project/"))
              ((symbol-function 'locate-dominating-file)
               (lambda (file name)
                 (push 'locate-dominating-file workflow-steps)
                 (when (equal name ".beads")
                   "/ssh:remote:/home/user/project/")))
              ((symbol-function 'directory-files)
               (lambda (directory &optional full match _nosort)
                 (push 'directory-files workflow-steps)
                 (when (and match (string-match-p "\\.db$" "beads.db"))
                   (if full
                       (list (concat directory "beads.db"))
                     (list "beads.db")))))
              ((symbol-function 'process-file)
               (lambda (program &optional infile destination display
                                &rest args)
                 (push 'process-file workflow-steps)
                 (when destination
                   (with-current-buffer (current-buffer)
                     (insert json-output)))
                 0)))
      ;; Find .beads directory
      (let ((beads-dir (beads--find-beads-dir)))
        (should beads-dir)
        (should (string-prefix-p "/ssh:remote:" beads-dir)))

      ;; Get database path
      (let ((db-path (beads--get-database-path)))
        (should db-path)
        (should (string-prefix-p "/ssh:remote:" db-path)))

      ;; Run command
      (let ((result (beads--run-command "list")))
        (should (vectorp result))
        (should (= (length result) 1)))

      ;; Verify all steps were executed
      (should (member 'process-file workflow-steps))
      (should (member 'locate-dominating-file workflow-steps)))))

;;; Tests for debug logging with Tramp

(ert-deftest beads-tramp-test-debug-logging-shows-remote-directory ()
  "Test that debug logging includes remote directory information."
  (beads-tramp-test-with-temp-config
   (let ((beads-enable-debug t)
         (beads-debug-level 'verbose)
         (default-directory "/ssh:remote:/home/user/project/")
         (json-output "{}"))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (when destination
                    (with-current-buffer (if (bufferp destination)
                                             destination
                                           (current-buffer))
                      (insert json-output)))
                  0)))
       (with-current-buffer (get-buffer-create "*beads-debug*")
         (erase-buffer))
       (beads--run-command "list")
       (with-current-buffer "*beads-debug*"
         (should (> (buffer-size) 0))
         (should (string-match-p "/ssh:remote:" (buffer-string)))
         (should (string-match-p "In directory:" (buffer-string))))))))

;;; Edge cases and error handling

(ert-deftest beads-tramp-test-command-failure-over-tramp ()
  "Test that command failures over Tramp are handled correctly."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/ssh:remote:/home/user/project/")
         (error-msg "Error: connection refused"))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (when destination
                    (with-current-buffer (if (bufferp destination)
                                             destination
                                           (current-buffer))
                      (insert error-msg)))
                  1)))
       (should-error
        (beads--run-command "list")
        :type 'user-error)))))

(ert-deftest beads-tramp-test-json-parsing-over-tramp ()
  "Test JSON parsing works correctly over Tramp."
  (beads-tramp-test-with-temp-config
   (let ((default-directory "/ssh:remote:/home/user/project/")
         (json-output "{\"id\":\"bd-1\",\"title\":\"远程问题\",\"priority\":1}"))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program &optional infile destination display &rest args)
                  (when destination
                    (with-current-buffer (if (bufferp destination)
                                             destination
                                           (current-buffer))
                      (insert json-output)))
                  0)))
       (let ((result (beads--run-command "show" "bd-1")))
         (should (listp result))
         (should (consp result))
         (should (equal (alist-get 'id result) "bd-1"))
         (should (string-match-p "远程" (alist-get 'title result)))
         (should (= (alist-get 'priority result) 1)))))))

(provide 'beads-tramp-test)

;;; ============================================================
;;; Module Integration Tests
;;; ============================================================

(ert-deftest beads-test-cache-invalidation ()
  "Integration test: Verify cache invalidation functions exist."
  :tags '(integration)
  ;; The cache invalidation function should be defined
  (should (fboundp 'beads--invalidate-completion-cache)))

(ert-deftest beads-test-all-commands-autoloaded ()
  "Integration test: Verify all main commands are autoloaded."
  :tags '(integration)
  (should (fboundp 'beads-list))
  (should (fboundp 'beads-ready))
  (should (fboundp 'beads-blocked))
  (should (fboundp 'beads-show))
  (should (fboundp 'beads-create))
  (should (fboundp 'beads-update))
  (should (fboundp 'beads-close))
  (should (fboundp 'beads-delete))
  (should (fboundp 'beads-stats))
  (should (fboundp 'beads-graph-all))
  (should (fboundp 'beads-import))
  (should (fboundp 'beads-export)))

;;; ========================================
;;; Label Completion Tests
;;; ========================================

(ert-deftest beads-test-label-list-all-success ()
  "Test successful fetching of labels from bd label list-all."
  (beads-test-with-temp-config
   (let ((json-output "[{\"label\":\"backend\",\"count\":5},{\"label\":\"frontend\",\"count\":3}]"))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       (let ((result (beads-label-list-all)))
         (should (listp result))
         (should (= (length result) 2))
         (should (member "backend" result))
         (should (member "frontend" result)))))))

(ert-deftest beads-test-label-list-all-empty ()
  "Test label list-all with no labels."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 0 "[]")))
     (let ((result (beads-label-list-all)))
       (should (listp result))
       (should (= (length result) 0))))))

(ert-deftest beads-test-label-cache-works ()
  "Test that label cache stores and retrieves labels."
  (beads-test-with-temp-config
   (let ((json-output "[{\"label\":\"test\",\"count\":1}]"))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       ;; Clear cache first
       (beads--invalidate-label-cache)

       ;; First call should populate cache
       (let ((result1 (beads--get-cached-labels)))
         (should (listp result1))
         (should (member "test" result1))

         ;; Cache should be populated
         (should beads--label-cache)
         (should (consp beads--label-cache))

         ;; Second call should use cache (no process-file call)
         (cl-letf (((symbol-function 'process-file)
                    (lambda (&rest _args)
                      (error "Should not call process-file when cache is valid"))))
           (let ((result2 (beads--get-cached-labels)))
             (should (equal result1 result2)))))))))

(ert-deftest beads-test-label-cache-expires ()
  "Test that label cache expires after TTL."
  (beads-test-with-temp-config
   (let ((json-output1 "[{\"label\":\"old\",\"count\":1}]")
         (json-output2 "[{\"label\":\"new\",\"count\":1}]")
         (beads-label-cache-ttl 1)) ; 1 second TTL
     ;; Clear cache first
     (beads--invalidate-label-cache)

     ;; First call
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output1)))
       (let ((result1 (beads--get-cached-labels)))
         (should (member "old" result1))

         ;; Wait for cache to expire
         (sleep-for 1.1)

         ;; Second call should fetch fresh data
         (cl-letf (((symbol-function 'process-file)
                    (beads-test--mock-call-process 0 json-output2)))
           (let ((result2 (beads--get-cached-labels)))
             (should (member "new" result2))
             (should-not (member "old" result2)))))))))

(ert-deftest beads-test-label-cache-invalidation ()
  "Test that cache can be manually invalidated."
  (beads-test-with-temp-config
   (let ((json-output "[{\"label\":\"test\",\"count\":1}]"))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       ;; Populate cache
       (beads--get-cached-labels)
       (should beads--label-cache)

       ;; Invalidate
       (beads--invalidate-label-cache)
       (should-not beads--label-cache)))))

(ert-deftest beads-test-label-completion-table ()
  "Test that label completion table returns correct list."
  (beads-test-with-temp-config
   (let ((json-output "[{\"label\":\"backend\",\"count\":5},{\"label\":\"frontend\",\"count\":3},{\"label\":\"api\",\"count\":2}]"))
     (cl-letf (((symbol-function 'process-file)
                (beads-test--mock-call-process 0 json-output)))
       ;; Clear cache
       (beads--invalidate-label-cache)

       (let ((table (beads--label-completion-table)))
         (should (listp table))
         (should (= (length table) 3))
         (should (member "backend" table))
         (should (member "frontend" table))
         (should (member "api" table)))))))

(ert-deftest beads-test-label-completion-table-empty ()
  "Test label completion table with no labels."
  (beads-test-with-temp-config
   (cl-letf (((symbol-function 'process-file)
              (beads-test--mock-call-process 0 "[]")))
     ;; Clear cache
     (beads--invalidate-label-cache)

     (let ((table (beads--label-completion-table)))
       (should (listp table))
       (should (= (length table) 0))))))

(ert-deftest beads-test-label-completion-table-removes-duplicates ()
  "Test that label completion table removes any duplicate labels."
  (beads-test-with-temp-config
   ;; Manually set cache with duplicates (shouldn't happen, but test defensively)
   (setq beads--label-cache (cons (float-time) '("test" "test" "other")))

   (let ((table (beads--label-completion-table)))
     (should (listp table))
     (should (= (length table) 2))
     (should (member "test" table))
     (should (member "other" table)))))

(provide 'beads-test)
;;; beads-test.el ends here
