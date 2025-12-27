;;; beads-git-test.el --- Tests for beads-git.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Unit tests for git and worktree support functions in beads-git.el:
;; - Core git functions: project root, branch, worktree detection
;; - Agent worktree functions: creation, discovery, async operations

;;; Code:

(require 'ert)
(require 'beads-git)

;;; Test beads-git-in-worktree-p

(ert-deftest beads-git-test-in-worktree-p-with-git-file ()
  "Test that beads-git-in-worktree-p returns t when .git is a file."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Create a .git file (like in a worktree)
          (with-temp-file (expand-file-name ".git" temp-dir)
            (insert "gitdir: /path/to/main/.git/worktrees/test\n"))
          (should (beads-git-in-worktree-p)))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-git-test-in-worktree-p-with-git-directory ()
  "Test that beads-git-in-worktree-p returns nil when .git is a directory."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Create a .git directory (like in a normal repo)
          (make-directory (expand-file-name ".git" temp-dir))
          (should-not (beads-git-in-worktree-p)))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-git-test-in-worktree-p-no-git ()
  "Test that beads-git-in-worktree-p returns nil when no .git exists."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; No .git file or directory
          (should-not (beads-git-in-worktree-p)))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-git-test-in-worktree-p-nested-directory ()
  "Test beads-git-in-worktree-p works from nested directories."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (progn
          ;; Create a .git file at root (worktree)
          (with-temp-file (expand-file-name ".git" temp-dir)
            (insert "gitdir: /path/to/main/.git/worktrees/test\n"))
          ;; Create nested directory
          (let ((nested-dir (expand-file-name "src/lib" temp-dir)))
            (make-directory nested-dir t)
            (let ((default-directory nested-dir))
              (should (beads-git-in-worktree-p)))))
      ;; Cleanup
      (delete-directory temp-dir t))))

;;; Test beads-git-find-main-repo

(ert-deftest beads-git-test-find-main-repo-not-worktree ()
  "Test beads-git-find-main-repo returns nil in normal repo."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Create a .git directory (normal repo)
          (make-directory (expand-file-name ".git" temp-dir))
          (should-not (beads-git-find-main-repo)))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-git-test-find-main-repo-from-worktree ()
  "Test beads-git-find-main-repo finds main repo path."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Mock beads-git-in-worktree-p to return t
          (cl-letf (((symbol-function 'beads-git-in-worktree-p)
                     (lambda () t)))
            (cl-letf (((symbol-function 'beads-git-find-project-root)
                       (lambda () temp-dir)))
              (cl-letf (((symbol-function 'process-file)
                         (lambda (_program _infile buffer _display &rest _args)
                           ;; When buffer is t, output goes to current buffer
                           (when (or (eq buffer t) (bufferp buffer))
                             (insert "/path/to/main/.git\n"))
                           0)))
                (should (equal (beads-git-find-main-repo)
                               "/path/to/main/"))))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-git-test-find-main-repo-git-error ()
  "Test beads-git-find-main-repo handles git errors."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Mock worktree detection and process-file to return error
          (cl-letf (((symbol-function 'beads-git-in-worktree-p)
                     (lambda () t)))
            (cl-letf (((symbol-function 'beads-git-find-project-root)
                       (lambda () temp-dir)))
              (cl-letf (((symbol-function 'process-file)
                         (lambda (_program _infile buffer _display &rest _args)
                           (when (or (eq buffer t) (bufferp buffer))
                             (insert "fatal: not a git repository\n"))
                           128)))
                (should-not (beads-git-find-main-repo))))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-git-test-find-main-repo-fatal-output ()
  "Test beads-git-find-main-repo handles fatal: prefix."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Mock worktree detection and process-file to return fatal message
          (cl-letf (((symbol-function 'beads-git-in-worktree-p)
                     (lambda () t)))
            (cl-letf (((symbol-function 'beads-git-find-project-root)
                       (lambda () temp-dir)))
              (cl-letf (((symbol-function 'process-file)
                         (lambda (_program _infile buffer _display &rest _args)
                           (when (or (eq buffer t) (bufferp buffer))
                             (insert "fatal: some error\n"))
                           0)))
                (should-not (beads-git-find-main-repo))))))
      ;; Cleanup
      (delete-directory temp-dir t))))

;;; Test beads-git-command

(ert-deftest beads-git-test-command-success ()
  "Test beads-git-command returns output on success."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/tmp"))
            ((symbol-function 'call-process)
             (lambda (_program _infile buffer _display &rest _args)
               (when buffer
                 (with-current-buffer (if (eq buffer t) (current-buffer) buffer)
                   (insert "main\n")))
               0)))
    (should (equal (beads-git-command "rev-parse" "--abbrev-ref" "HEAD")
                   "main"))))

(ert-deftest beads-git-test-command-failure ()
  "Test beads-git-command returns nil on error."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/tmp"))
            ((symbol-function 'call-process)
             (lambda (_program _infile _buffer _display &rest _args)
               128)))
    (should-not (beads-git-command "status"))))

;;; Test beads-git-list-worktrees

(ert-deftest beads-git-test-list-worktrees ()
  "Test beads-git-list-worktrees parses porcelain output."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-command)
             (lambda (&rest _args)
               "worktree /path/to/main\nbranch refs/heads/main\n\nworktree /path/to/feature\nbranch refs/heads/feature-1")))
    (let ((worktrees (beads-git-list-worktrees)))
      (should (equal (length worktrees) 2))
      (should (equal (car worktrees) '("/path/to/main" "main")))
      (should (equal (cadr worktrees) '("/path/to/feature" "feature-1"))))))

(ert-deftest beads-git-test-list-worktrees-empty ()
  "Test beads-git-list-worktrees returns nil when git fails."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-command)
             (lambda (&rest _args) nil)))
    (should-not (beads-git-list-worktrees))))

;;; Test beads-git-find-worktree-for-issue

(ert-deftest beads-git-test-find-worktree-by-path ()
  "Test finding worktree by issue ID in path."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-list-worktrees)
             (lambda ()
               '(("/path/to/main" "main")
                 ("/path/to/beads-123" "beads-123")))))
    (should (equal (beads-git-find-worktree-for-issue "beads-123")
                   "/path/to/beads-123"))))

(ert-deftest beads-git-test-find-worktree-by-branch ()
  "Test finding worktree by branch name matching issue ID."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-list-worktrees)
             (lambda ()
               '(("/path/to/main" "main")
                 ("/custom/path/foo" "beads-456")))))
    (should (equal (beads-git-find-worktree-for-issue "beads-456")
                   "/custom/path/foo"))))

(ert-deftest beads-git-test-find-worktree-not-found ()
  "Test beads-git-find-worktree-for-issue returns nil when not found."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-list-worktrees)
             (lambda ()
               '(("/path/to/main" "main")))))
    (should-not (beads-git-find-worktree-for-issue "beads-999"))))

;;; Test beads-git-worktree-path-for-issue

(ert-deftest beads-git-test-worktree-path-default-parent ()
  "Test worktree path uses parent of main repo by default."
  :tags '(:unit)
  (let ((beads-agent-worktree-parent nil))
    (cl-letf (((symbol-function 'beads-git-main-repo-root)
               (lambda () "/home/user/projects/myrepo/")))
      (should (equal (beads-git-worktree-path-for-issue "beads-42")
                     "/home/user/projects/beads-42")))))

(ert-deftest beads-git-test-worktree-path-custom-parent ()
  "Test worktree path uses custom parent when set."
  :tags '(:unit)
  (let ((beads-agent-worktree-parent "/custom/worktrees/"))
    (cl-letf (((symbol-function 'beads-git-main-repo-root)
               (lambda () "/home/user/projects/myrepo/")))
      (should (equal (beads-git-worktree-path-for-issue "beads-42")
                     "/custom/worktrees/beads-42")))))

;;; Test beads-git-should-use-worktree-p

(ert-deftest beads-git-test-should-use-worktree-always ()
  "Test beads-git-should-use-worktree-p returns t when always enabled."
  :tags '(:unit)
  (let ((beads-agent-use-worktrees t))
    (should (beads-git-should-use-worktree-p "beads-1"))))

(ert-deftest beads-git-test-should-use-worktree-never ()
  "Test beads-git-should-use-worktree-p returns nil when disabled."
  :tags '(:unit)
  (let ((beads-agent-use-worktrees nil))
    (should-not (beads-git-should-use-worktree-p "beads-1"))))

(ert-deftest beads-git-test-should-use-worktree-ask-yes ()
  "Test beads-git-should-use-worktree-p prompts user when 'ask."
  :tags '(:unit)
  (let ((beads-agent-use-worktrees 'ask))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t)))
      (should (beads-git-should-use-worktree-p "beads-1")))))

(ert-deftest beads-git-test-should-use-worktree-ask-no ()
  "Test beads-git-should-use-worktree-p returns nil when user declines."
  :tags '(:unit)
  (let ((beads-agent-use-worktrees 'ask))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt) nil)))
      (should-not (beads-git-should-use-worktree-p "beads-1")))))

;;; Test beads-git-ensure-worktree

(ert-deftest beads-git-test-ensure-worktree-existing ()
  "Test beads-git-ensure-worktree returns existing worktree."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-worktree-for-issue)
             (lambda (_id) "/path/to/existing"))
            ((symbol-function 'beads-git-create-worktree)
             (lambda (_id) (error "Should not create"))))
    (should (equal (beads-git-ensure-worktree "beads-1")
                   "/path/to/existing"))))

(ert-deftest beads-git-test-ensure-worktree-creates-new ()
  "Test beads-git-ensure-worktree creates new worktree."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-worktree-for-issue)
             (lambda (_id) nil))
            ((symbol-function 'beads-git-create-worktree)
             (lambda (_id) "/path/to/new")))
    (should (equal (beads-git-ensure-worktree "beads-1")
                   "/path/to/new"))))

;;; Test beads-git-in-linked-worktree-p

(ert-deftest beads-git-test-in-linked-worktree-true ()
  "Test beads-git-in-linked-worktree-p returns t in linked worktree."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-command)
             (lambda (&rest args)
               (pcase (car args)
                 ("rev-parse" "/path/to/worktree")
                 (_ nil))))
            ((symbol-function 'beads-git-main-repo-root)
             (lambda () "/path/to/main/")))
    (should (beads-git-in-linked-worktree-p))))

(ert-deftest beads-git-test-in-linked-worktree-false-main ()
  "Test beads-git-in-linked-worktree-p returns nil in main worktree."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-command)
             (lambda (&rest args)
               (pcase (car args)
                 ("rev-parse" "/path/to/main")
                 (_ nil))))
            ((symbol-function 'beads-git-main-repo-root)
             (lambda () "/path/to/main")))
    ;; file-equal-p should return t for same paths
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (a b) (equal a b))))
      (should-not (beads-git-in-linked-worktree-p)))))

;;; Test compatibility aliases (ensure they exist and work)

(ert-deftest beads-git-test-compatibility-alias-project-root ()
  "Test that beads--find-project-root alias exists and works."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/test/path")))
    ;; The alias should call through to the new function
    (with-no-warnings
      (should (equal (beads--find-project-root) "/test/path")))))

(ert-deftest beads-git-test-compatibility-alias-in-worktree ()
  "Test that beads--in-git-worktree-p alias exists and works."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-in-worktree-p)
             (lambda () t)))
    (with-no-warnings
      (should (beads--in-git-worktree-p)))))

(provide 'beads-git-test)

;;; beads-git-test.el ends here
