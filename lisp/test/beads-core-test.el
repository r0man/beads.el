;;; beads-core-test.el --- Tests for core beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for core beads.el functionality including logging,
;; error handling, and utility functions.

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-custom)

;;; Tests for Logging Functions

(ert-deftest beads-core-test-log-enabled ()
  "Test that logging works when enabled."
  (let ((beads-enable-debug t)
        (beads-debug-level 'verbose))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer-create)
                 (lambda (_name) (current-buffer))))
        (beads--log 'info "Test message")
        (should (string-match-p "INFO.*Test message" (buffer-string)))))))

(ert-deftest beads-core-test-log-disabled ()
  "Test that logging is silent when disabled."
  (let ((beads-enable-debug nil)
        (buffer-output nil))
    (cl-letf (((symbol-function 'get-buffer-create)
               (lambda (_name) (setq buffer-output t) (get-buffer-create "*test*"))))
      (beads--log 'info "Test message")
      (should-not buffer-output))))

(ert-deftest beads-core-test-log-level-error ()
  "Test that error level logs are always shown when debug enabled."
  (let ((beads-enable-debug t)
        (beads-debug-level 'info))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer-create)
                 (lambda (_name) (current-buffer))))
        (beads--log 'error "Error message")
        (should (string-match-p "ERROR.*Error message" (buffer-string)))))))

(ert-deftest beads-core-test-log-level-filtering ()
  "Test that verbose messages are filtered at info level."
  (let ((beads-enable-debug t)
        (beads-debug-level 'info))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer-create)
                 (lambda (_name) (current-buffer))))
        (beads--log 'verbose "Verbose message")
        (should (= (point-min) (point-max)))))))

(ert-deftest beads-core-test-log-verbose-level ()
  "Test that verbose messages show at verbose level."
  (let ((beads-enable-debug t)
        (beads-debug-level 'verbose))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer-create)
                 (lambda (_name) (current-buffer))))
        (beads--log 'verbose "Verbose message")
        (should (string-match-p "DEBUG.*Verbose message" (buffer-string)))))))

;;; Tests for Error Functions

(ert-deftest beads-core-test-error-signals-user-error ()
  "Test that beads--error signals user-error."
  (let ((beads-enable-debug nil))
    (should-error
     (beads--error "Test error")
     :type 'user-error)))

(ert-deftest beads-core-test-error-includes-message ()
  "Test that beads--error includes message in error."
  (let ((beads-enable-debug nil))
    (condition-case err
        (beads--error "Custom error message")
      (user-error
       (should (string-match-p "Custom error message"
                               (error-message-string err)))))))

(ert-deftest beads-core-test-error-formats-args ()
  "Test that beads--error formats arguments."
  (let ((beads-enable-debug nil))
    (condition-case err
        (beads--error "Error %d: %s" 42 "test")
      (user-error
       (should (string-match-p "Error 42: test"
                               (error-message-string err)))))))

;;; Tests for Error Buffer Display

(ert-deftest beads-core-test-display-error-buffer-creates-buffer ()
  "Test that display-error-buffer creates error buffer."
  (let ((displayed-buffer nil))
    (cl-letf (((symbol-function 'display-buffer)
               (lambda (buf) (setq displayed-buffer buf))))
      (beads--display-error-buffer "bd list" 1 "stdout" "stderr")
      (should displayed-buffer)
      (should (string= (buffer-name displayed-buffer) "*beads-errors*"))
      (kill-buffer displayed-buffer))))

(ert-deftest beads-core-test-display-error-buffer-content ()
  "Test that display-error-buffer shows correct content."
  (cl-letf (((symbol-function 'display-buffer) #'ignore))
    (beads--display-error-buffer "bd test-cmd" 42 "test stdout" "test stderr")
    (with-current-buffer "*beads-errors*"
      (let ((content (buffer-string)))
        (should (string-match-p "Command Error" content))
        (should (string-match-p "bd test-cmd" content))
        (should (string-match-p "42" content))
        (should (string-match-p "test stdout" content))
        (should (string-match-p "test stderr" content))))
    (kill-buffer "*beads-errors*")))

(ert-deftest beads-core-test-display-error-buffer-empty-output ()
  "Test that display-error-buffer handles empty output."
  (cl-letf (((symbol-function 'display-buffer) #'ignore))
    (beads--display-error-buffer "bd cmd" 1 "" "")
    (with-current-buffer "*beads-errors*"
      (let ((content (buffer-string)))
        (should (string-match-p "(empty)" content))))
    (kill-buffer "*beads-errors*")))

;;; Tests for Debug Mode

(ert-deftest beads-core-test-debug-mode-defined ()
  "Test that beads-debug-mode is defined."
  (should (fboundp 'beads-debug-mode)))

(ert-deftest beads-core-test-debug-mode-keymap ()
  "Test that debug mode has expected keybindings."
  (should (keymapp beads-debug-mode-map))
  (should (eq (lookup-key beads-debug-mode-map (kbd "g")) 'beads-show-debug-buffer))
  (should (eq (lookup-key beads-debug-mode-map (kbd "c")) 'beads-clear-debug-buffer))
  (should (eq (lookup-key beads-debug-mode-map (kbd "q")) 'quit-window)))

(ert-deftest beads-core-test-show-debug-buffer-enables-debug ()
  "Test that show-debug-buffer enables debug logging."
  (let ((saved-value beads-enable-debug))
    (unwind-protect
        (progn
          (setq beads-enable-debug nil)
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (beads-show-debug-buffer)
            (should beads-enable-debug)))
      (setq beads-enable-debug saved-value)
      (when (get-buffer "*beads-debug*")
        (kill-buffer "*beads-debug*")))))

(ert-deftest beads-core-test-show-debug-buffer-creates-buffer ()
  "Test that show-debug-buffer creates debug buffer."
  (let ((saved-value beads-enable-debug))
    (when (get-buffer "*beads-debug*")
      (kill-buffer "*beads-debug*"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            (beads-show-debug-buffer)
            (should (get-buffer "*beads-debug*"))))
      (setq beads-enable-debug saved-value)
      (when (get-buffer "*beads-debug*")
        (kill-buffer "*beads-debug*")))))

(ert-deftest beads-core-test-clear-debug-buffer ()
  "Test that clear-debug-buffer clears content."
  (let ((buf (get-buffer-create "*beads-debug*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert "Some debug content")))
    (beads-clear-debug-buffer)
    (with-current-buffer buf
      (should (= (point-min) (point-max))))
    (kill-buffer buf)))

(ert-deftest beads-core-test-clear-debug-buffer-no-buffer ()
  "Test that clear-debug-buffer handles missing buffer."
  (when (get-buffer "*beads-debug*")
    (kill-buffer "*beads-debug*"))
  ;; Should not error
  (beads-clear-debug-buffer))

;;; Tests for Project Cache

(ert-deftest beads-core-test-project-cache-exists ()
  "Test that project cache hash table exists."
  (should (hash-table-p beads--project-cache)))

;;; Tests for Constants

(ert-deftest beads-core-test-display-value-max-length ()
  "Test that display value max length constant is defined."
  (should (boundp 'beads-display-value-max-length))
  (should (integerp beads-display-value-max-length))
  (should (> beads-display-value-max-length 0)))

(ert-deftest beads-core-test-separator-line-width ()
  "Test that separator line width constant is defined."
  (should (boundp 'beads-separator-line-width))
  (should (integerp beads-separator-line-width))
  (should (> beads-separator-line-width 0)))

(ert-deftest beads-core-test-stats-separator-width ()
  "Test that stats separator width constant is defined."
  (should (boundp 'beads-stats-separator-width))
  (should (integerp beads-stats-separator-width))
  (should (> beads-stats-separator-width 0)))

(ert-deftest beads-core-test-graph-label-max-length ()
  "Test that graph label max length constant is defined."
  (should (boundp 'beads-graph-label-max-length))
  (should (integerp beads-graph-label-max-length))
  (should (> beads-graph-label-max-length 0)))

;;; Tests for Parse Functions

(ert-deftest beads-core-test-parse-issue-basic ()
  "Test parsing a basic issue from JSON."
  (let ((json '((id . "bd-42")
                (title . "Test Issue")
                (status . "open")
                (priority . 1)
                (issue_type . "task"))))
    (let ((issue (beads--parse-issue json)))
      (should (beads-issue-p issue))
      (should (equal (oref issue id) "bd-42"))
      (should (equal (oref issue title) "Test Issue"))
      (should (equal (oref issue status) "open"))
      (should (= (oref issue priority) 1)))))

(ert-deftest beads-core-test-parse-issue-with-optional-fields ()
  "Test parsing issue with optional fields."
  (let ((json '((id . "bd-42")
                (title . "Test Issue")
                (status . "open")
                (priority . 1)
                (issue_type . "bug")
                (description . "Description text")
                (assignee . "alice")
                (labels . ["bug" "urgent"]))))
    (let ((issue (beads--parse-issue json)))
      (should (beads-issue-p issue))
      (should (equal (oref issue description) "Description text"))
      (should (equal (oref issue assignee) "alice"))
      (should (equal (oref issue labels) '("bug" "urgent"))))))

(ert-deftest beads-core-test-parse-issue-nil-fields ()
  "Test parsing issue with nil/missing fields."
  (let ((json '((id . "bd-42")
                (title . nil)
                (status . "open")
                (priority . 1)
                (issue_type . "task"))))
    (let ((issue (beads--parse-issue json)))
      (should (beads-issue-p issue))
      (should (null (oref issue title))))))

(ert-deftest beads-core-test-parse-issues-array ()
  "Test parsing array of issues."
  (let ((json-array '[((id . "bd-1") (title . "Issue 1") (status . "open") (priority . 1) (issue_type . "task"))
                      ((id . "bd-2") (title . "Issue 2") (status . "closed") (priority . 2) (issue_type . "bug"))]))
    (let ((issues (beads--parse-issues json-array)))
      (should (listp issues))
      (should (= 2 (length issues)))
      (should (beads-issue-p (car issues)))
      (should (beads-issue-p (cadr issues))))))

(ert-deftest beads-core-test-parse-issues-empty ()
  "Test parsing empty array of issues."
  (let ((json-array '[]))
    (let ((issues (beads--parse-issues json-array)))
      (should (listp issues))
      (should (= 0 (length issues))))))

;;; Tests for Check Executable

(ert-deftest beads-core-test-check-executable-found ()
  "Test check-executable when executable exists."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (name) (when (equal name "bd") "/usr/bin/bd"))))
    ;; Should not error
    (beads-check-executable)
    (should t)))

(ert-deftest beads-core-test-check-executable-not-found ()
  "Test check-executable when executable missing."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_name) nil)))
    (should-error (beads-check-executable) :type 'user-error)))

;;; Tests for Find Beads Directory

(ert-deftest beads-core-test-find-beads-dir-in-project ()
  "Test finding .beads directory in project."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".beads" temp-dir))
          (let ((default-directory temp-dir))
            (let ((result (beads--find-beads-dir)))
              (should (stringp result))
              (should (string-match-p "\\.beads$" result)))))
      (delete-directory temp-dir t))))

(ert-deftest beads-core-test-find-beads-dir-not-found ()
  "Test that find-beads-dir returns nil when not found."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Clear cache
          (clrhash beads--project-cache)
          ;; Mock project-root to return temp-dir
          (cl-letf (((symbol-function 'project-root)
                     (lambda (_proj) temp-dir))
                    ((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt _dir) t)))
            (let ((result (beads--find-beads-dir)))
              (should (null result)))))
      (delete-directory temp-dir t))))

;;; Tests for Customization Group

(ert-deftest beads-core-test-customization-group-exists ()
  "Test that beads customization group exists."
  (should (get 'beads 'group-documentation)))

;;; Tests for beads-executable Variable

(ert-deftest beads-core-test-executable-variable ()
  "Test that beads-executable variable exists."
  (should (boundp 'beads-executable))
  (should (stringp beads-executable)))

(provide 'beads-core-test)
;;; beads-core-test.el ends here
