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
          ;; Mock both locate-dominating-file and beads-git-find-main-repo
          ;; to simulate a directory with no .beads anywhere
          (cl-letf (((symbol-function 'locate-dominating-file)
                     (lambda (_file _name) nil))
                    ((symbol-function 'beads-git-find-main-repo)
                     (lambda () nil)))
            ;; With both mocked to return nil, should return nil
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

;;; Tests for beads--get-database-path

(ert-deftest beads-core-test-get-database-path-finds-db-file ()
  "Test that get-database-path returns .db file when present."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let* ((beads-dir (expand-file-name ".beads" temp-dir))
               (db-file (expand-file-name "beads.db" beads-dir)))
          (make-directory beads-dir)
          (write-region "" nil db-file)
          (clrhash beads--project-cache)
          (let ((default-directory temp-dir)
                (beads-database-path nil))
            (should (equal (beads--get-database-path) db-file))))
      (delete-directory temp-dir t))))

(ert-deftest beads-core-test-get-database-path-nil-without-db ()
  "Test that get-database-path returns nil when no .db file."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let* ((beads-dir (expand-file-name ".beads" temp-dir)))
          (make-directory beads-dir)
          (clrhash beads--project-cache)
          (let ((default-directory temp-dir)
                (beads-database-path nil))
            (should (null (beads--get-database-path)))))
      (delete-directory temp-dir t))))

(ert-deftest beads-core-test-get-database-path-follows-redirect ()
  "Test that get-database-path follows .beads/redirect to find .db file."
  (let ((main-dir (make-temp-file "beads-main-" t))
        (worktree-dir (make-temp-file "beads-wt-" t)))
    (unwind-protect
        (let* ((main-beads (expand-file-name ".beads" main-dir))
               (wt-beads (expand-file-name ".beads" worktree-dir))
               (db-file (expand-file-name "beads.db" main-beads))
               ;; redirect is relative to the worktree project root
               (redirect-target
                (file-relative-name main-beads worktree-dir)))
          (make-directory main-beads)
          (make-directory wt-beads)
          (write-region "" nil db-file)
          ;; Write redirect file in worktree's .beads dir
          (write-region redirect-target nil
                        (expand-file-name "redirect" wt-beads))
          (clrhash beads--project-cache)
          (let ((default-directory worktree-dir)
                (beads-database-path nil))
            (should (equal (beads--get-database-path) db-file))))
      (delete-directory main-dir t)
      (delete-directory worktree-dir t))))

(ert-deftest beads-core-test-get-database-path-redirect-no-db ()
  "Test that get-database-path returns nil when redirect target has no .db."
  (let ((main-dir (make-temp-file "beads-main-" t))
        (worktree-dir (make-temp-file "beads-wt-" t)))
    (unwind-protect
        (let* ((main-beads (expand-file-name ".beads" main-dir))
               (wt-beads (expand-file-name ".beads" worktree-dir))
               (redirect-target
                (file-relative-name main-beads worktree-dir)))
          (make-directory main-beads)
          (make-directory wt-beads)
          ;; No .db file in main-beads
          (write-region redirect-target nil
                        (expand-file-name "redirect" wt-beads))
          (clrhash beads--project-cache)
          (let ((default-directory worktree-dir)
                (beads-database-path nil))
            (should (null (beads--get-database-path)))))
      (delete-directory main-dir t)
      (delete-directory worktree-dir t))))

(provide 'beads-core-test)
;;; beads-core-test.el ends here
