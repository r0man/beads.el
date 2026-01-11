;;; beads-ready-test.el --- Tests for beads-ready.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Comprehensive tests for beads-ready.el CLI-like ready view.
;; Covers buffer creation, rendering, navigation, and commands.

;;; Code:

(require 'beads-command)
(require 'beads-ready)
(require 'beads-test)
(require 'ert)

;;; Test Data

(defvar beads-ready-test--sample-issues
  (list
   (beads-issue
    :id "bd-1"
    :title "First ready issue"
    :status "open"
    :priority 0
    :issue-type "task")
   (beads-issue
    :id "bd-2"
    :title "Second ready issue"
    :status "in_progress"
    :priority 1
    :issue-type "bug")
   (beads-issue
    :id "bd-3"
    :title "Third ready issue"
    :status "open"
    :priority 2
    :issue-type "feature"))
  "Sample issue data for testing.")

;;; Helper Functions

(defmacro beads-ready-test--with-temp-buffer (issues &rest body)
  "Create a temp ready buffer with ISSUES, then run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (beads-ready-mode)
     (setq beads-ready--issues ,issues)
     (setq beads-ready--project-dir default-directory)
     (beads-ready--render-buffer)
     ,@body))

;;; Mode Activation Tests

(ert-deftest beads-ready-test-mode-activation ()
  "Test that beads-ready-mode activates correctly."
  (with-temp-buffer
    (beads-ready-mode)
    (should (eq major-mode 'beads-ready-mode))
    (should (derived-mode-p 'special-mode))))

(ert-deftest beads-ready-test-mode-read-only ()
  "Test that beads-ready-mode buffer is read-only."
  (with-temp-buffer
    (beads-ready-mode)
    (should buffer-read-only)))

;;; Buffer Naming Tests

(ert-deftest beads-ready-test-buffer-naming ()
  "Test that buffers are named correctly."
  ;; Use unique project name to avoid conflicts
  (let ((buf-name "*beads-ready[naming]*"))
    (when-let ((old (get-buffer buf-name)))
      (kill-buffer old))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (&rest _)
                 (beads-test--mock-command-result (vector))))
              ((symbol-function 'beads-check-executable)
               (lambda () t))
              ((symbol-function 'beads-git-get-project-name)
               (lambda () "naming"))
              ((symbol-function 'beads-git-get-branch)
               (lambda () "main"))
              ((symbol-function 'beads-buffer-is-main-branch-p)
               (lambda (&optional _) t))
              ((symbol-function 'beads-git-in-worktree-p)
               (lambda () nil))
              ((symbol-function 'beads-git-find-project-root)
               (lambda () default-directory))
              ((symbol-function 'beads-command-ready!)
               (lambda () nil)))
      (beads-ready)
      (let ((buf (get-buffer buf-name)))
        (should buf)
        (kill-buffer buf)))))

;;; Rendering Tests

(ert-deftest beads-ready-test-render-header ()
  "Test header rendering with count."
  (beads-ready-test--with-temp-buffer beads-ready-test--sample-issues
    (goto-char (point-min))
    (should (search-forward "Ready work" nil t))
    (should (search-forward "(3 issues with no blockers)" nil t))))

(ert-deftest beads-ready-test-render-issue ()
  "Test individual issue rendering."
  (beads-ready-test--with-temp-buffer beads-ready-test--sample-issues
    (goto-char (point-min))
    ;; Check first issue renders
    (should (search-forward "bd-1" nil t))
    (should (search-forward "First ready issue" nil t))
    ;; Check priority indicator
    (goto-char (point-min))
    (should (search-forward "P0" nil t))))

(ert-deftest beads-ready-test-render-all-issues ()
  "Test all issues are rendered."
  (beads-ready-test--with-temp-buffer beads-ready-test--sample-issues
    ;; Each issue should appear
    (goto-char (point-min))
    (should (search-forward "bd-1" nil t))
    (goto-char (point-min))
    (should (search-forward "bd-2" nil t))
    (goto-char (point-min))
    (should (search-forward "bd-3" nil t))))

(ert-deftest beads-ready-test-render-empty ()
  "Test empty state rendering."
  (beads-ready-test--with-temp-buffer nil
    (goto-char (point-min))
    (should (search-forward "Ready work" nil t))
    (should (search-forward "(0 issues)" nil t))
    (should (search-forward "No ready issues found" nil t))))

(ert-deftest beads-ready-test-numbered-issues ()
  "Test issues are numbered sequentially."
  (beads-ready-test--with-temp-buffer beads-ready-test--sample-issues
    (goto-char (point-min))
    (should (search-forward "1." nil t))
    (should (search-forward "2." nil t))
    (should (search-forward "3." nil t))))

;;; Priority Face Tests

(ert-deftest beads-ready-test-priority-face-critical ()
  "Test priority 0 gets critical face."
  (let ((face (beads-ready--priority-face 0)))
    (should (eq face 'beads-ready-priority-critical))))

(ert-deftest beads-ready-test-priority-face-high ()
  "Test priority 1 gets high face."
  (let ((face (beads-ready--priority-face 1)))
    (should (eq face 'beads-ready-priority-high))))

(ert-deftest beads-ready-test-priority-face-medium ()
  "Test priority 2 gets medium face."
  (let ((face (beads-ready--priority-face 2)))
    (should (eq face 'beads-ready-priority-medium))))

(ert-deftest beads-ready-test-priority-face-low ()
  "Test priority 3-4 get low face."
  (should (eq (beads-ready--priority-face 3) 'beads-ready-priority-low))
  (should (eq (beads-ready--priority-face 4) 'beads-ready-priority-low)))

;;; Navigation Tests

(ert-deftest beads-ready-test-next-issue ()
  "Test navigating to next issue."
  (beads-ready-test--with-temp-buffer beads-ready-test--sample-issues
    ;; Should start at first issue
    (should (equal (beads-ready--current-issue-id) "bd-1"))
    ;; Move to next
    (beads-ready-next-issue)
    (should (equal (beads-ready--current-issue-id) "bd-2"))
    ;; Move to next again
    (beads-ready-next-issue)
    (should (equal (beads-ready--current-issue-id) "bd-3"))))

(ert-deftest beads-ready-test-previous-issue ()
  "Test navigating to previous issue."
  (beads-ready-test--with-temp-buffer beads-ready-test--sample-issues
    ;; Move to last issue
    (goto-char (point-max))
    (beads-ready-previous-issue)
    (beads-ready-previous-issue)
    (should (equal (beads-ready--current-issue-id) "bd-2"))))

(ert-deftest beads-ready-test-current-issue-id ()
  "Test getting current issue ID."
  (beads-ready-test--with-temp-buffer beads-ready-test--sample-issues
    ;; At first issue
    (should (equal (beads-ready--current-issue-id) "bd-1"))))

(ert-deftest beads-ready-test-current-issue-id-empty ()
  "Test getting current issue ID when no issues."
  (beads-ready-test--with-temp-buffer nil
    (should (null (beads-ready--current-issue-id)))))

;;; Button Tests

(ert-deftest beads-ready-test-issue-button ()
  "Test that issue IDs are buttons."
  (beads-ready-test--with-temp-buffer beads-ready-test--sample-issues
    (goto-char (point-min))
    (search-forward "bd-1")
    (backward-char 1)
    (let ((button (button-at (point))))
      (should button)
      (should (equal (button-get button 'issue-id) "bd-1")))))

;;; Copy ID Tests

(ert-deftest beads-ready-test-copy-id ()
  "Test copying issue ID to kill ring."
  (beads-ready-test--with-temp-buffer beads-ready-test--sample-issues
    (setq kill-ring nil)
    (beads-ready-copy-id)
    (should (equal (car kill-ring) "bd-1"))))

(ert-deftest beads-ready-test-copy-id-no-issue ()
  "Test copy ID when no issue at point."
  (beads-ready-test--with-temp-buffer nil
    (should-error (beads-ready-copy-id) :type 'user-error)))

;;; Position Tracking Tests

(ert-deftest beads-ready-test-issue-positions ()
  "Test that issue positions are tracked."
  (beads-ready-test--with-temp-buffer beads-ready-test--sample-issues
    (should (= (length beads-ready--issue-positions) 3))
    (should (assoc "bd-1" beads-ready--issue-positions))
    (should (assoc "bd-2" beads-ready--issue-positions))
    (should (assoc "bd-3" beads-ready--issue-positions))))

;;; Keymap Tests

(ert-deftest beads-ready-test-keymap ()
  "Test keymap bindings exist."
  (with-temp-buffer
    (beads-ready-mode)
    (should (eq (lookup-key beads-ready-mode-map (kbd "n"))
                'beads-ready-next-issue))
    (should (eq (lookup-key beads-ready-mode-map (kbd "p"))
                'beads-ready-previous-issue))
    (should (eq (lookup-key beads-ready-mode-map (kbd "RET"))
                'beads-ready-show))
    (should (eq (lookup-key beads-ready-mode-map (kbd "g"))
                'beads-ready-refresh))
    (should (eq (lookup-key beads-ready-mode-map (kbd "q"))
                'beads-ready-quit))
    (should (eq (lookup-key beads-ready-mode-map (kbd "w"))
                'beads-ready-copy-id))))

;;; Integration Tests

(ert-deftest beads-ready-test-full-command ()
  "Test beads-ready command with mocked bd output."
  ;; Use unique project name to avoid conflicts
  (let ((buf-name "*beads-ready[fullcmd]*"))
    ;; Clean up any existing buffer
    (when-let ((old-buf (get-buffer buf-name)))
      (kill-buffer old-buf))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (&rest _)
                 (beads-test--mock-command-result (vector))))
              ((symbol-function 'beads-check-executable)
               (lambda () t))
              ((symbol-function 'beads-git-get-project-name)
               (lambda () "fullcmd"))
              ((symbol-function 'beads-git-get-branch)
               (lambda () "main"))
              ((symbol-function 'beads-buffer-is-main-branch-p)
               (lambda (&optional _) t))
              ((symbol-function 'beads-git-in-worktree-p)
               (lambda () nil))
              ((symbol-function 'beads-git-find-project-root)
               (lambda () default-directory))
              ((symbol-function 'beads-command-ready!)
               (lambda () beads-ready-test--sample-issues)))
      (beads-ready)
      ;; Get the buffer that was created
      (let ((buf (get-buffer buf-name)))
        (should buf)
        (with-current-buffer buf
          (should (derived-mode-p 'beads-ready-mode))
          (should (search-forward "bd-1" nil t)))
        (kill-buffer buf)))))

(provide 'beads-ready-test)
;;; beads-ready-test.el ends here
