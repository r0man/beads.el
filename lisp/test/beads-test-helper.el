;;; beads-test-helper.el --- Test helpers for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Shared test utilities and helpers for beads.el test suite.
;; Provides both unit test mocking helpers and integration test
;; utilities for creating temporary projects and running real bd CLI.

;;; Code:

(require 'ert)
(require 'json)

;;; ============================================================
;;; Mock Helpers for Unit Tests
;;; ============================================================

(defun beads-test-helper-mock-call-process (exit-code output)
  "Create a mock for `call-process' returning EXIT-CODE and OUTPUT."
  (lambda (program &optional infile destination display &rest args)
    (when destination
      (with-current-buffer (if (bufferp destination)
                               destination
                             (current-buffer))
        (insert output)))
    exit-code))

;;; ============================================================
;;; Integration Test Helpers
;;; ============================================================

(defun beads-test-helper-run-commands (cmds)
  "Execute a sequence of interactive commands CMDS.
This simulates user interaction by properly setting `last-command'
and `this-command', running hooks, and adding undo boundaries.

CMDS should be a list of command symbols or lists of the form
\(COMMAND . ARGS) where COMMAND is a symbol and ARGS are passed
as interactive arguments.

Example:
  (beads-test-helper-run-commands
   \\='(beads-create
     (beads-update . (\"bd-1\"))))

Based on simple-tests--exec pattern from:
https://emacs.stackexchange.com/questions/55386"
  (dolist (cmd cmds)
    (let ((command (if (consp cmd) (car cmd) cmd))
          (args (if (consp cmd) (cdr cmd) nil)))
      (setq last-command this-command)
      (setq this-command command)
      (run-hooks 'pre-command-hook)
      (if args
          (apply #'funcall-interactively command args)
        (command-execute command))
      (run-hooks 'post-command-hook)
      (undo-boundary))))

(defvar beads-test-helper--temp-dirs nil
  "List of temporary directories created during tests for cleanup.")

(defun beads-test-helper-create-temp-project ()
  "Create a temporary project directory with beads initialized.
Returns the project directory path."
  (let* ((temp-dir (make-temp-file "beads-test-" t))
         (db-path (expand-file-name ".beads/test.db" temp-dir)))
    ;; Track for cleanup
    (push temp-dir beads-test-helper--temp-dirs)
    ;; Initialize beads in the temp directory
    (let ((default-directory temp-dir))
      (call-process "bd" nil nil nil "init" "--prefix" "test"))
    temp-dir))

(defun beads-test-helper-cleanup-temp-projects ()
  "Clean up all temporary project directories."
  (dolist (dir beads-test-helper--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq beads-test-helper--temp-dirs nil))

(defun beads-test-helper-create-issue (project-dir title)
  "Create an issue in PROJECT-DIR with TITLE.
Returns the issue ID."
  (let ((default-directory project-dir))
    (with-temp-buffer
      (call-process "bd" nil t nil "create" title "--json")
      (goto-char (point-min))
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (result (json-read)))
        (alist-get 'id result)))))

(defun beads-test-helper-issue-exists-p (project-dir issue-id)
  "Check if ISSUE-ID exists in PROJECT-DIR.
Returns non-nil if the issue exists."
  (let ((default-directory project-dir))
    (with-temp-buffer
      (let ((exit-code (call-process "bd" nil t nil "show" issue-id
                                     "--json")))
        (= exit-code 0)))))

(defun beads-test-helper-delete-issue (project-dir issue-id)
  "Delete ISSUE-ID in PROJECT-DIR using bd CLI directly."
  (let ((default-directory project-dir))
    (call-process "bd" nil nil nil "delete" "--force" issue-id)))

(defun beads-test-helper-get-issue (project-dir issue-id)
  "Get issue data for ISSUE-ID in PROJECT-DIR.
Returns the parsed JSON issue object."
  (let ((default-directory project-dir))
    (with-temp-buffer
      (when (= 0 (call-process "bd" nil t nil "show" issue-id "--json"))
        (goto-char (point-min))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'symbol))
          (json-read))))))

(defun beads-kbd-do (keys)
  (execute-kbd-macro (kbd (string-join keys " "))))

;; (beads-kbd-do
;;  '("M-x beads-create RET"
;;    "t my-title RET"
;;    "T epic RET"
;;    "d 1 2 3 C-c C-c"
;;    "c"))

;;; Cleanup hook

(defun beads-test-helper--cleanup-all ()
  "Cleanup function to run after all tests."
  (beads-test-helper-cleanup-temp-projects))

;; Register cleanup
(add-hook 'ert-runner-reporter-run-ended-functions
          #'beads-test-helper--cleanup-all)

(provide 'beads-test-helper)
;;; beads-test-helper.el ends here
