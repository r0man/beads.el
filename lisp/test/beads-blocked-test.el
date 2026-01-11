;;; beads-blocked-test.el --- Tests for beads-blocked.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Comprehensive tests for beads-blocked.el CLI-like blocked view.
;; Covers buffer creation, rendering, navigation, and commands.

;;; Code:

(require 'beads-command)
(require 'beads-blocked)
(require 'beads-test)
(require 'ert)

;;; Test Data

(defvar beads-blocked-test--sample-issues
  (list
   (beads-blocked-issue
    :id "bd-1"
    :title "First blocked issue"
    :status "open"
    :priority 0
    :issue-type "task"
    :blocked-by-count 2
    :blocked-by '("bd-10" "bd-11"))
   (beads-blocked-issue
    :id "bd-2"
    :title "Second blocked issue"
    :status "in_progress"
    :priority 1
    :issue-type "bug"
    :blocked-by-count 1
    :blocked-by '("bd-20"))
   (beads-blocked-issue
    :id "bd-3"
    :title "Third blocked issue"
    :status "open"
    :priority 2
    :issue-type "feature"
    :blocked-by-count 3
    :blocked-by '("bd-30" "bd-31" "bd-32")))
  "Sample blocked issue data for testing.")

;;; Helper Functions

(defmacro beads-blocked-test--with-temp-buffer (issues &rest body)
  "Create a temp blocked buffer with ISSUES, then run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (beads-blocked-mode)
     (setq beads-blocked--issues ,issues)
     (setq beads-blocked--project-dir default-directory)
     (beads-blocked--render-buffer)
     ,@body))

;;; Mode Activation Tests

(ert-deftest beads-blocked-test-mode-activation ()
  "Test that beads-blocked-mode activates correctly."
  (with-temp-buffer
    (beads-blocked-mode)
    (should (eq major-mode 'beads-blocked-mode))
    (should (derived-mode-p 'special-mode))))

(ert-deftest beads-blocked-test-mode-read-only ()
  "Test that beads-blocked-mode buffer is read-only."
  (with-temp-buffer
    (beads-blocked-mode)
    (should buffer-read-only)))

;;; Buffer Naming Tests

(ert-deftest beads-blocked-test-buffer-naming ()
  "Test that buffers are named correctly."
  ;; Use unique project name to avoid conflicts
  (let ((buf-name "*beads-blocked[naming]*"))
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
              ((symbol-function 'beads-command-blocked!)
               (lambda () nil)))
      (beads-blocked)
      (let ((buf (get-buffer buf-name)))
        (should buf)
        (kill-buffer buf)))))

;;; Rendering Tests

(ert-deftest beads-blocked-test-render-header ()
  "Test header rendering with count."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    (goto-char (point-min))
    (should (search-forward "Blocked issues" nil t))
    (should (search-forward "(3)" nil t))))

(ert-deftest beads-blocked-test-render-issue ()
  "Test individual issue rendering."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    (goto-char (point-min))
    ;; Check first issue renders
    (should (search-forward "bd-1" nil t))
    (should (search-forward "First blocked issue" nil t))
    ;; Check priority indicator
    (goto-char (point-min))
    (should (search-forward "P0" nil t))))

(ert-deftest beads-blocked-test-render-blockers ()
  "Test blocker information rendering."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    (goto-char (point-min))
    ;; Check blocker line
    (should (search-forward "Blocked by 2 open dependencies" nil t))
    (should (search-forward "bd-10" nil t))
    (should (search-forward "bd-11" nil t))))

(ert-deftest beads-blocked-test-render-all-issues ()
  "Test all issues are rendered."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    ;; Each issue should appear
    (goto-char (point-min))
    (should (search-forward "bd-1" nil t))
    (goto-char (point-min))
    (should (search-forward "bd-2" nil t))
    (goto-char (point-min))
    (should (search-forward "bd-3" nil t))))

(ert-deftest beads-blocked-test-render-empty ()
  "Test empty state rendering."
  (beads-blocked-test--with-temp-buffer nil
    (goto-char (point-min))
    (should (search-forward "Blocked issues" nil t))
    (should (search-forward "(0)" nil t))
    (should (search-forward "No blocked issues found" nil t))))

(ert-deftest beads-blocked-test-single-blocker ()
  "Test singular form for single blocker."
  (let ((single-blocker-issues
         (list (beads-blocked-issue
                :id "bd-99"
                :title "Single blocker issue"
                :status "open"
                :priority 1
                :issue-type "task"
                :blocked-by-count 1
                :blocked-by '("bd-100")))))
    (beads-blocked-test--with-temp-buffer single-blocker-issues
      (goto-char (point-min))
      (should (search-forward "Blocked by 1 open dependency:" nil t)))))

;;; Priority Face Tests

(ert-deftest beads-blocked-test-priority-face-critical ()
  "Test priority 0 gets critical face."
  (let ((face (beads-blocked--priority-face 0)))
    (should (eq face 'beads-blocked-priority-critical))))

(ert-deftest beads-blocked-test-priority-face-high ()
  "Test priority 1 gets high face."
  (let ((face (beads-blocked--priority-face 1)))
    (should (eq face 'beads-blocked-priority-high))))

(ert-deftest beads-blocked-test-priority-face-medium ()
  "Test priority 2 gets medium face."
  (let ((face (beads-blocked--priority-face 2)))
    (should (eq face 'beads-blocked-priority-medium))))

(ert-deftest beads-blocked-test-priority-face-low ()
  "Test priority 3-4 get low face."
  (should (eq (beads-blocked--priority-face 3) 'beads-blocked-priority-low))
  (should (eq (beads-blocked--priority-face 4) 'beads-blocked-priority-low)))

;;; Navigation Tests

(ert-deftest beads-blocked-test-next-issue ()
  "Test navigating to next issue."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    ;; Should start at first issue
    (should (equal (beads-blocked--current-issue-id) "bd-1"))
    ;; Move to next
    (beads-blocked-next-issue)
    (should (equal (beads-blocked--current-issue-id) "bd-2"))
    ;; Move to next again
    (beads-blocked-next-issue)
    (should (equal (beads-blocked--current-issue-id) "bd-3"))))

(ert-deftest beads-blocked-test-previous-issue ()
  "Test navigating to previous issue."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    ;; Move to last issue
    (goto-char (point-max))
    (beads-blocked-previous-issue)
    (beads-blocked-previous-issue)
    (should (equal (beads-blocked--current-issue-id) "bd-2"))))

(ert-deftest beads-blocked-test-current-issue-id ()
  "Test getting current issue ID."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    ;; At first issue
    (should (equal (beads-blocked--current-issue-id) "bd-1"))))

(ert-deftest beads-blocked-test-current-issue-id-empty ()
  "Test getting current issue ID when no issues."
  (beads-blocked-test--with-temp-buffer nil
    (should (null (beads-blocked--current-issue-id)))))

;;; Button Tests

(ert-deftest beads-blocked-test-issue-button ()
  "Test that issue IDs are buttons."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    (goto-char (point-min))
    (search-forward "bd-1")
    (backward-char 1)
    (let ((button (button-at (point))))
      (should button)
      (should (equal (button-get button 'issue-id) "bd-1")))))

(ert-deftest beads-blocked-test-blocker-button ()
  "Test that blocker IDs are buttons."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    (goto-char (point-min))
    ;; Find the first blocker ID (bd-10)
    (search-forward "bd-10")
    (backward-char 1)
    (let ((button (button-at (point))))
      (should button)
      (should (equal (button-get button 'issue-id) "bd-10")))))

;;; Copy ID Tests

(ert-deftest beads-blocked-test-copy-id ()
  "Test copying issue ID to kill ring."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    (setq kill-ring nil)
    (beads-blocked-copy-id)
    (should (equal (car kill-ring) "bd-1"))))

(ert-deftest beads-blocked-test-copy-id-no-issue ()
  "Test copy ID when no issue at point."
  (beads-blocked-test--with-temp-buffer nil
    (should-error (beads-blocked-copy-id) :type 'user-error)))

;;; Position Tracking Tests

(ert-deftest beads-blocked-test-issue-positions ()
  "Test that issue positions are tracked."
  (beads-blocked-test--with-temp-buffer beads-blocked-test--sample-issues
    (should (= (length beads-blocked--issue-positions) 3))
    (should (assoc "bd-1" beads-blocked--issue-positions))
    (should (assoc "bd-2" beads-blocked--issue-positions))
    (should (assoc "bd-3" beads-blocked--issue-positions))))

;;; Keymap Tests

(ert-deftest beads-blocked-test-keymap ()
  "Test keymap bindings exist."
  (with-temp-buffer
    (beads-blocked-mode)
    (should (eq (lookup-key beads-blocked-mode-map (kbd "n"))
                'beads-blocked-next-issue))
    (should (eq (lookup-key beads-blocked-mode-map (kbd "p"))
                'beads-blocked-previous-issue))
    (should (eq (lookup-key beads-blocked-mode-map (kbd "RET"))
                'beads-blocked-show))
    (should (eq (lookup-key beads-blocked-mode-map (kbd "g"))
                'beads-blocked-refresh))
    (should (eq (lookup-key beads-blocked-mode-map (kbd "q"))
                'beads-blocked-quit))
    (should (eq (lookup-key beads-blocked-mode-map (kbd "w"))
                'beads-blocked-copy-id))))

;;; Integration Tests

(ert-deftest beads-blocked-test-full-command ()
  "Test beads-blocked command with mocked bd output."
  ;; Use unique project name to avoid conflicts
  (let ((buf-name "*beads-blocked[blockfull]*"))
    ;; Clean up any existing buffer
    (when-let ((old-buf (get-buffer buf-name)))
      (kill-buffer old-buf))
    (cl-letf (((symbol-function 'beads-command-execute)
               (lambda (&rest _)
                 (beads-test--mock-command-result (vector))))
              ((symbol-function 'beads-check-executable)
               (lambda () t))
              ((symbol-function 'beads-git-get-project-name)
               (lambda () "blockfull"))
              ((symbol-function 'beads-git-get-branch)
               (lambda () "main"))
              ((symbol-function 'beads-buffer-is-main-branch-p)
               (lambda (&optional _) t))
              ((symbol-function 'beads-git-in-worktree-p)
               (lambda () nil))
              ((symbol-function 'beads-git-find-project-root)
               (lambda () default-directory))
              ((symbol-function 'beads-command-blocked!)
               (lambda () beads-blocked-test--sample-issues)))
      (beads-blocked)
      ;; Get the buffer that was created
      (let ((buf (get-buffer buf-name)))
        (should buf)
        (with-current-buffer buf
          (should (derived-mode-p 'beads-blocked-mode))
          (should (search-forward "bd-1" nil t)))
        (kill-buffer buf)))))

(provide 'beads-blocked-test)
;;; beads-blocked-test.el ends here
