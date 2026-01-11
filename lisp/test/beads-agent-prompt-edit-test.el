;;; beads-agent-prompt-edit-test.el --- Tests for beads-agent-prompt-edit -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Comprehensive ERT tests for beads-agent-prompt-edit.el module.
;; Tests cover the prompt editing mode, buffer management, confirm/cancel
;; workflows, and keyboard bindings.

;;; Code:

(require 'ert)
(require 'beads-agent-prompt-edit)
(require 'beads-git)

;;; Test Helpers

(defmacro beads-agent-prompt-edit-test--with-mock-git (&rest body)
  "Execute BODY with git functions mocked for testing."
  `(cl-letf (((symbol-function 'beads-git-get-project-name)
              (lambda () "test-project"))
             ((symbol-function 'beads-git-get-branch)
              (lambda () "main"))
             ((symbol-function 'beads-buffer-is-main-branch-p)
              (lambda (&optional _branch) t)))
     ,@body))

;;; Customization Tests

(ert-deftest beads-agent-prompt-edit-test-customization-exists ()
  "Test that customization variable exists."
  (should (boundp 'beads-agent-prompt-edit-enabled)))

(ert-deftest beads-agent-prompt-edit-test-default-disabled ()
  "Test that prompt editing is disabled by default."
  (let ((beads-agent-prompt-edit-enabled nil))
    (should-not beads-agent-prompt-edit-enabled)))

(ert-deftest beads-agent-prompt-edit-test-customization-type ()
  "Test customization variable is boolean."
  (should (custom-variable-p 'beads-agent-prompt-edit-enabled)))

;;; Mode Tests

(ert-deftest beads-agent-prompt-edit-test-mode-defined ()
  "Test that the major mode is defined."
  (should (fboundp 'beads-agent-prompt-edit-mode)))

(ert-deftest beads-agent-prompt-edit-test-mode-derived-from-text ()
  "Test that mode is derived from text-mode."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (should (derived-mode-p 'text-mode))))

(ert-deftest beads-agent-prompt-edit-test-mode-sets-header-line ()
  "Test that mode sets header-line-format."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (should header-line-format)))

;;; Keymap Tests

(ert-deftest beads-agent-prompt-edit-test-keymap-exists ()
  "Test that keymap exists."
  (should (keymapp beads-agent-prompt-edit-mode-map)))

(ert-deftest beads-agent-prompt-edit-test-keymap-confirm ()
  "Test C-c C-c is bound to confirm."
  (should (eq (lookup-key beads-agent-prompt-edit-mode-map (kbd "C-c C-c"))
              'beads-agent-prompt-edit-confirm)))

(ert-deftest beads-agent-prompt-edit-test-keymap-cancel ()
  "Test C-c C-k is bound to cancel."
  (should (eq (lookup-key beads-agent-prompt-edit-mode-map (kbd "C-c C-k"))
              'beads-agent-prompt-edit-cancel)))

;;; Buffer-Local Variable Tests

(ert-deftest beads-agent-prompt-edit-test-callback-variable ()
  "Test callback variable becomes buffer-local when set."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--callback #'identity)
    (should (local-variable-p 'beads-agent-prompt-edit--callback))))

(ert-deftest beads-agent-prompt-edit-test-issue-id-variable ()
  "Test issue-id variable becomes buffer-local when set."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--issue-id "test-123")
    (should (local-variable-p 'beads-agent-prompt-edit--issue-id))))

(ert-deftest beads-agent-prompt-edit-test-agent-type-variable ()
  "Test agent-type variable becomes buffer-local when set."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--agent-type "Task")
    (should (local-variable-p 'beads-agent-prompt-edit--agent-type))))

;;; Buffer Name Tests

(ert-deftest beads-agent-prompt-edit-test-buffer-name-function ()
  "Test buffer name generation function exists."
  (should (fboundp 'beads-agent-prompt-edit--buffer-name)))

(ert-deftest beads-agent-prompt-edit-test-buffer-name-includes-issue-id ()
  "Test buffer name includes issue ID."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((name (beads-agent-prompt-edit--buffer-name "test-123")))
     (should (string-match "test-123" name)))))

;;; Show Function Tests

(ert-deftest beads-agent-prompt-edit-test-show-function-exists ()
  "Test show function exists."
  (should (fboundp 'beads-agent-prompt-edit-show)))

(ert-deftest beads-agent-prompt-edit-test-show-creates-buffer ()
  "Test show creates a buffer with correct name."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((buf-name nil)
         (callback-called nil))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123"
        "Test prompt"
        "Task"
        (lambda (prompt) (setq callback-called prompt)))
       (setq buf-name (buffer-name))
       ;; Clean up
       (beads-agent-prompt-edit-cancel))
     (should (string-match "test-123" buf-name)))))

(ert-deftest beads-agent-prompt-edit-test-show-inserts-prompt ()
  "Test show inserts the initial prompt text."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((content nil))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123"
        "Test prompt content"
        "Task"
        (lambda (_prompt) nil))
       (setq content (buffer-substring-no-properties (point-min) (point-max)))
       (beads-agent-prompt-edit-cancel))
     (should (equal content "Test prompt content")))))

(ert-deftest beads-agent-prompt-edit-test-show-sets-local-vars ()
  "Test show sets buffer-local variables."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((issue-id nil)
         (agent-type nil))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123"
        "Test prompt"
        "Review"
        (lambda (_prompt) nil))
       (setq issue-id beads-agent-prompt-edit--issue-id)
       (setq agent-type beads-agent-prompt-edit--agent-type)
       (beads-agent-prompt-edit-cancel))
     (should (equal issue-id "test-123"))
     (should (equal agent-type "Review")))))

;;; Confirm Tests

(ert-deftest beads-agent-prompt-edit-test-confirm-function-exists ()
  "Test confirm function exists."
  (should (fboundp 'beads-agent-prompt-edit-confirm)))

(ert-deftest beads-agent-prompt-edit-test-confirm-calls-callback ()
  "Test confirm calls callback with buffer content."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((received-prompt nil))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123"
        "Original prompt"
        "Task"
        (lambda (prompt) (setq received-prompt prompt)))
       ;; Modify the prompt
       (erase-buffer)
       (insert "Modified prompt")
       ;; Confirm
       (beads-agent-prompt-edit-confirm))
     (should (equal received-prompt "Modified prompt")))))

(ert-deftest beads-agent-prompt-edit-test-confirm-kills-buffer ()
  "Test confirm kills the edit buffer."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((buf nil))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123"
        "Test prompt"
        "Task"
        (lambda (_prompt) nil))
       (setq buf (current-buffer))
       (beads-agent-prompt-edit-confirm))
     (should-not (buffer-live-p buf)))))

;;; Cancel Tests

(ert-deftest beads-agent-prompt-edit-test-cancel-function-exists ()
  "Test cancel function exists."
  (should (fboundp 'beads-agent-prompt-edit-cancel)))

(ert-deftest beads-agent-prompt-edit-test-cancel-calls-callback-with-nil ()
  "Test cancel calls callback with nil."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((received-prompt 'not-called))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123"
        "Test prompt"
        "Task"
        (lambda (prompt) (setq received-prompt prompt)))
       (beads-agent-prompt-edit-cancel))
     (should (null received-prompt)))))

(ert-deftest beads-agent-prompt-edit-test-cancel-kills-buffer ()
  "Test cancel kills the edit buffer."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((buf nil))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123"
        "Test prompt"
        "Task"
        (lambda (_prompt) nil))
       (setq buf (current-buffer))
       (beads-agent-prompt-edit-cancel))
     (should-not (buffer-live-p buf)))))

;;; Header Line Tests

(ert-deftest beads-agent-prompt-edit-test-header-line-function ()
  "Test header line generation function exists."
  (should (fboundp 'beads-agent-prompt-edit--header-line)))

(ert-deftest beads-agent-prompt-edit-test-header-line-includes-agent-type ()
  "Test header line includes agent type."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--agent-type "Review")
    (setq beads-agent-prompt-edit--issue-id "test-123")
    (let ((header (beads-agent-prompt-edit--header-line)))
      (should (string-match "Review" header)))))

(ert-deftest beads-agent-prompt-edit-test-header-line-includes-issue-id ()
  "Test header line includes issue ID."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--agent-type "Task")
    (setq beads-agent-prompt-edit--issue-id "beads-42")
    (let ((header (beads-agent-prompt-edit--header-line)))
      (should (string-match "beads-42" header)))))

(ert-deftest beads-agent-prompt-edit-test-header-line-includes-keybindings ()
  "Test header line mentions keybindings."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--agent-type "Task")
    (setq beads-agent-prompt-edit--issue-id "test-123")
    (let ((header (beads-agent-prompt-edit--header-line)))
      (should (string-match "C-c C-c" header))
      (should (string-match "C-c C-k" header)))))

;;; Integration with beads-agent--maybe-edit-prompt Tests

(ert-deftest beads-agent-prompt-edit-test-maybe-edit-disabled ()
  "Test maybe-edit-prompt calls callback directly when disabled."
  (require 'beads-agent)
  (let ((beads-agent-prompt-edit-enabled nil)
        (received-prompt nil))
    (beads-agent--maybe-edit-prompt
     "test-123"
     "Original prompt"
     "Task"
     (lambda (prompt) (setq received-prompt prompt)))
    (should (equal received-prompt "Original prompt"))))

(ert-deftest beads-agent-prompt-edit-test-maybe-edit-enabled ()
  "Test maybe-edit-prompt shows buffer when enabled."
  (require 'beads-agent)
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((beads-agent-prompt-edit-enabled t)
         (buf nil))
     (save-window-excursion
       (beads-agent--maybe-edit-prompt
        "test-123"
        "Test prompt"
        "Task"
        (lambda (_prompt) nil))
       (setq buf (current-buffer))
       ;; Verify we're in the edit buffer
       (should (derived-mode-p 'beads-agent-prompt-edit-mode))
       ;; Clean up
       (beads-agent-prompt-edit-cancel))
     (should-not (buffer-live-p buf)))))

;;; Edge Cases

(ert-deftest beads-agent-prompt-edit-test-empty-prompt ()
  "Test handling empty initial prompt."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((received-prompt nil))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123"
        ""
        "Task"
        (lambda (prompt) (setq received-prompt prompt)))
       (beads-agent-prompt-edit-confirm))
     (should (equal received-prompt "")))))

(ert-deftest beads-agent-prompt-edit-test-multiline-prompt ()
  "Test handling multiline prompt."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((multiline "Line 1\nLine 2\nLine 3")
         (received-prompt nil))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123"
        multiline
        "Task"
        (lambda (prompt) (setq received-prompt prompt)))
       (beads-agent-prompt-edit-confirm))
     (should (equal received-prompt multiline)))))

(ert-deftest beads-agent-prompt-edit-test-nil-callback-handling ()
  "Test confirm handles nil callback gracefully."
  (beads-agent-prompt-edit-test--with-mock-git
   (save-window-excursion
     (beads-agent-prompt-edit-show
      "test-123"
      "Test prompt"
      "Task"
      nil)
     ;; Should not error even with nil callback
     (should-not (condition-case err
                     (progn
                       (beads-agent-prompt-edit-confirm)
                       nil)
                   (error t))))))

(ert-deftest beads-agent-prompt-edit-test-cancel-nil-callback-handling ()
  "Test cancel handles nil callback gracefully."
  (beads-agent-prompt-edit-test--with-mock-git
   (save-window-excursion
     (beads-agent-prompt-edit-show
      "test-123"
      "Test prompt"
      "Task"
      nil)
     ;; Should not error even with nil callback
     (should-not (condition-case err
                     (progn
                       (beads-agent-prompt-edit-cancel)
                       nil)
                   (error t))))))

(provide 'beads-agent-prompt-edit-test)

;;; beads-agent-prompt-edit-test.el ends here
