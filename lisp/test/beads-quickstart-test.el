;;; beads-quickstart-test.el --- Tests for beads-quickstart -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; Integration tests for beads-quickstart function.
;;
;; These tests verify that the beads-quickstart user-facing function
;; correctly displays the quickstart guide in a buffer.

;;; Code:

(require 'ert)
(require 'beads-buffer-name)
(require 'beads-quickstart)

;;; Test Utilities

(defun beads-quickstart-test--with-mock-project (body)
  "Execute BODY with mocked git functions for consistent naming."
  (cl-letf (((symbol-function 'beads-git-get-project-name)
             (lambda () "test-proj"))
            ((symbol-function 'beads-git-in-worktree-p)
             (lambda () nil)))
    (funcall body)))

(defun beads-quickstart-test--get-buffer ()
  "Get the quickstart buffer name."
  (beads-buffer-name-utility "quickstart"))

;;; Integration Tests

(ert-deftest beads-quickstart-test-displays-buffer ()
  "Test beads-quickstart creates and displays buffer.
Integration test that verifies buffer is created and displayed."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-quickstart-test--with-mock-project
   (lambda ()
     (let ((buf-name (beads-quickstart-test--get-buffer)))
       ;; Clean up any existing buffer
       (when (get-buffer buf-name)
         (kill-buffer buf-name))
       ;; Execute quickstart
       (beads-quickstart)
       ;; Buffer should exist
       (should (get-buffer buf-name))
       ;; Buffer should contain content
       (with-current-buffer buf-name
         (should (> (buffer-size) 0))
         ;; Should be in special-mode
         (should (eq major-mode 'special-mode))
         ;; Should have header line
         (should header-line-format))
       ;; Clean up
       (kill-buffer buf-name)))))

(ert-deftest beads-quickstart-test-buffer-content ()
  "Test beads-quickstart buffer contains expected content.
Integration test that verifies buffer has quickstart guide text."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-quickstart-test--with-mock-project
   (lambda ()
     (let ((buf-name (beads-quickstart-test--get-buffer)))
       ;; Clean up any existing buffer
       (when (get-buffer buf-name)
         (kill-buffer buf-name))
       ;; Execute quickstart
       (beads-quickstart)
       ;; Check buffer content
       (with-current-buffer buf-name
         (let ((content (buffer-string)))
           ;; Should contain quickstart-related keywords
           (should (or (string-match-p "quick" (downcase content))
                       (string-match-p "start" (downcase content))
                       (string-match-p "bd" content)))))
       ;; Clean up
       (kill-buffer buf-name)))))

(ert-deftest beads-quickstart-test-buffer-keybindings ()
  "Test beads-quickstart buffer has correct keybindings.
Integration test that verifies buffer keybindings are set up."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-quickstart-test--with-mock-project
   (lambda ()
     (let ((buf-name (beads-quickstart-test--get-buffer)))
       ;; Clean up any existing buffer
       (when (get-buffer buf-name)
         (kill-buffer buf-name))
       ;; Execute quickstart
       (beads-quickstart)
       ;; Check keybindings
       (with-current-buffer buf-name
         ;; Should have 'q' bound to quit-window
         (should (eq (lookup-key (current-local-map) (kbd "q")) 'quit-window))
         ;; Should have 'g' bound to refresh
         (should (lookup-key (current-local-map) (kbd "g"))))
       ;; Clean up
       (kill-buffer buf-name)))))

(ert-deftest beads-quickstart-test-visual-line-mode ()
  "Test beads-quickstart enables visual-line-mode.
Integration test that verifies visual-line-mode is enabled for readability."
  :tags '(:integration)
  (skip-unless (executable-find beads-executable))
  (beads-quickstart-test--with-mock-project
   (lambda ()
     (let ((buf-name (beads-quickstart-test--get-buffer)))
       ;; Clean up any existing buffer
       (when (get-buffer buf-name)
         (kill-buffer buf-name))
       ;; Execute quickstart
       (beads-quickstart)
       ;; Check visual-line-mode is enabled
       (with-current-buffer buf-name
         (should visual-line-mode))
       ;; Clean up
       (kill-buffer buf-name)))))

(provide 'beads-quickstart-test)
;;; beads-quickstart-test.el ends here
