;;; beads-utils-test.el --- Tests for beads utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors

;;; Commentary:

;; Comprehensive ERT tests for beads.el utility functions including:
;; - Logging functionality (beads--log)
;; - Error reporting (beads--error)
;; - Executable checking (beads-check-executable)
;; - Customization variables

;;; Code:

(require 'ert)
(require 'beads)

;;; Customization Variable Tests

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

;;; Logging Tests

(ert-deftest beads-test-log-disabled-by-default ()
  "Test that logging does nothing when debug is disabled."
  (let ((beads-enable-debug nil))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Log a message
    (beads--log "Test message: %s" "value")

    ;; Debug buffer should not be created
    (should-not (get-buffer "*beads-debug*"))))

(ert-deftest beads-test-log-enabled ()
  "Test that logging works when debug is enabled."
  (let ((beads-enable-debug t))
    ;; Kill debug buffer if it exists
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))

    ;; Log a message
    (beads--log "Test message: %s" "value")

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
    (beads--log "Timestamp test")

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
    (beads--log "First message")
    (beads--log "Second message")
    (beads--log "Third message")

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
    (beads--log "String: %s, Number: %d, Float: %.2f" "test" 42 3.14159)

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
    (beads--log "")

    ;; Buffer should exist with timestamp but empty message
    (with-current-buffer "*beads-debug*"
      (goto-char (point-min))
      (should (search-forward-regexp "\\[.*\\] \n" nil t)))

    ;; Cleanup
    (kill-buffer "*beads-debug*")))

;;; Error Reporting Tests

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

;;; Executable Checking Tests

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

;;; Integration Tests

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
    (beads--log "Session 1: operation 1")
    (beads--log "Session 1: operation 2")

    ;; Second session
    (beads--log "Session 2: operation 1")
    (condition-case nil
        (beads--error "Session 2: error")
      (user-error nil))

    ;; Third session
    (beads--log "Session 3: operation 1")

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
    (beads--log "First message")

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
    (beads--log "Line 1\nLine 2\nLine 3")

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
    (beads--log "Special: %% $@ #! \\ \" ' `")

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
;;; beads-utils-test.el ends here
