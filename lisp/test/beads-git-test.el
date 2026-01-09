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
  "Test finding worktree by issue ID in path suffix."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-worktree-list!)
             (lambda ()
               (list (beads-worktree :name "main" :path "/path/to/main"
                                     :branch "main" :is-main t)
                     (beads-worktree :name "beads-123" :path "/path/to/beads-123"
                                     :branch "beads-123" :is-main nil)))))
    (should (equal (beads-git-find-worktree-for-issue "beads-123")
                   "/path/to/beads-123"))))

(ert-deftest beads-git-test-find-worktree-by-branch ()
  "Test finding worktree by branch name matching issue ID."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-worktree-list!)
             (lambda ()
               (list (beads-worktree :name "main" :path "/path/to/main"
                                     :branch "main" :is-main t)
                     (beads-worktree :name "foo" :path "/custom/path/foo"
                                     :branch "beads-456" :is-main nil)))))
    (should (equal (beads-git-find-worktree-for-issue "beads-456")
                   "/custom/path/foo"))))

(ert-deftest beads-git-test-find-worktree-not-found ()
  "Test beads-git-find-worktree-for-issue returns nil when not found."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-worktree-list!)
             (lambda ()
               (list (beads-worktree :name "main" :path "/path/to/main"
                                     :branch "main" :is-main t)))))
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

;;; Test beads-git-find-project-root

(ert-deftest beads-git-test-find-project-root-with-project ()
  "Test beads-git-find-project-root when in a project."
  :tags '(:unit)
  (cl-letf (((symbol-function 'project-current)
             (lambda () '(vc Git "/path/to/project/")))
            ((symbol-function 'project-root)
             (lambda (_proj) "/path/to/project/")))
    (should (equal (beads-git-find-project-root) "/path/to/project/"))))

(ert-deftest beads-git-test-find-project-root-no-project ()
  "Test beads-git-find-project-root when not in a project."
  :tags '(:unit)
  (cl-letf (((symbol-function 'project-current)
             (lambda () nil)))
    (should-not (beads-git-find-project-root))))

(ert-deftest beads-git-test-find-project-root-emacs27-compatibility ()
  "Test beads-git-find-project-root with Emacs 27 API."
  :tags '(:unit)
  (cl-letf (((symbol-function 'project-current)
             (lambda () '(vc Git "/path/to/project/")))
            ((symbol-function 'fboundp)
             (lambda (fn) (not (eq fn 'project-root))))
            ((symbol-function 'project-roots)
             (lambda (_proj) '("/path/to/project/"))))
    ;; When project-root is not bound, it falls back to project-roots
    (should (stringp (beads-git-find-project-root)))))

;;; Test beads-git-get-project-name

(ert-deftest beads-git-test-get-project-name-success ()
  "Test beads-git-get-project-name returns basename of project root."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/home/user/projects/myproject/")))
    (should (equal (beads-git-get-project-name) "myproject"))))

(ert-deftest beads-git-test-get-project-name-no-project ()
  "Test beads-git-get-project-name returns nil when no project."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () nil)))
    (should-not (beads-git-get-project-name))))

(ert-deftest beads-git-test-get-project-name-nested-path ()
  "Test beads-git-get-project-name with deeply nested project."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/very/long/path/to/project/name/")))
    (should (equal (beads-git-get-project-name) "name"))))

;;; Test beads-git-get-branch

(ert-deftest beads-git-test-get-branch-success ()
  "Test beads-git-get-branch returns current branch."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/tmp"))
            ((symbol-function 'process-file)
             (lambda (_program _infile buffer _display &rest _args)
               (when (or (eq buffer t) (bufferp buffer))
                 (let ((buf (if (eq buffer t) (current-buffer) buffer)))
                   (with-current-buffer buf
                     (insert "main\n"))))
               0)))
    (should (equal (beads-git-get-branch) "main"))))

(ert-deftest beads-git-test-get-branch-feature-branch ()
  "Test beads-git-get-branch returns feature branch name."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/tmp"))
            ((symbol-function 'process-file)
             (lambda (_program _infile buffer _display &rest _args)
               (when (or (eq buffer t) (bufferp buffer))
                 (let ((buf (if (eq buffer t) (current-buffer) buffer)))
                   (with-current-buffer buf
                     (insert "feature/my-feature\n"))))
               0)))
    (should (equal (beads-git-get-branch) "feature/my-feature"))))

(ert-deftest beads-git-test-get-branch-detached-head ()
  "Test beads-git-get-branch returns nil for detached HEAD."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/tmp"))
            ((symbol-function 'process-file)
             (lambda (_program _infile buffer _display &rest _args)
               (when (or (eq buffer t) (bufferp buffer))
                 (let ((buf (if (eq buffer t) (current-buffer) buffer)))
                   (with-current-buffer buf
                     (insert "HEAD\n"))))
               0)))
    (should-not (beads-git-get-branch))))

(ert-deftest beads-git-test-get-branch-not-git-repo ()
  "Test beads-git-get-branch returns nil when not in git repo."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () "/tmp"))
            ((symbol-function 'process-file)
             (lambda (_program _infile _buffer _display &rest _args)
               128)))
    (should-not (beads-git-get-branch))))

;;; Test beads-git-command-async

(ert-deftest beads-git-test-command-async-success ()
  "Test beads-git-command-async calls callback on success."
  :tags '(:unit)
  (let ((callback-called nil)
        (callback-success nil)
        (callback-output nil)
        (stored-sentinel nil))
    (cl-letf (((symbol-function 'beads-git-find-project-root)
               (lambda () "/tmp"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((sentinel (plist-get args :sentinel))
                       (buffer (plist-get args :buffer))
                       (proc (make-symbol "mock-proc")))
                   (setq stored-sentinel sentinel)
                   ;; Simulate output in buffer
                   (with-current-buffer buffer
                     (insert "main\n"))
                   ;; Mock process functions
                   (cl-letf (((symbol-function 'process-status)
                              (lambda (_p) 'exit))
                             ((symbol-function 'process-exit-status)
                              (lambda (_p) 0)))
                     (funcall sentinel proc "finished\n"))
                   proc))))
      (beads-git-command-async
       (lambda (success output)
         (setq callback-called t
               callback-success success
               callback-output output))
       "rev-parse" "--abbrev-ref" "HEAD"))
    (should callback-called)
    (should callback-success)
    (should (equal callback-output "main"))))

(ert-deftest beads-git-test-command-async-failure ()
  "Test beads-git-command-async calls callback on failure."
  :tags '(:unit)
  (let ((callback-called nil)
        (callback-success nil)
        (callback-output nil))
    (cl-letf (((symbol-function 'beads-git-find-project-root)
               (lambda () "/tmp"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((sentinel (plist-get args :sentinel))
                       (buffer (plist-get args :buffer))
                       (proc (make-symbol "mock-proc")))
                   (with-current-buffer buffer
                     (insert "fatal: not a git repository\n"))
                   (cl-letf (((symbol-function 'process-status)
                              (lambda (_p) 'exit))
                             ((symbol-function 'process-exit-status)
                              (lambda (_p) 128)))
                     (funcall sentinel proc "exited abnormally\n"))
                   proc))))
      (beads-git-command-async
       (lambda (success output)
         (setq callback-called t
               callback-success success
               callback-output output))
       "status"))
    (should callback-called)
    (should-not callback-success)
    (should (equal callback-output "fatal: not a git repository"))))

(ert-deftest beads-git-test-command-async-signal ()
  "Test beads-git-command-async handles signal termination."
  :tags '(:unit)
  (let ((callback-called nil))
    (cl-letf (((symbol-function 'beads-git-find-project-root)
               (lambda () "/tmp"))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((sentinel (plist-get args :sentinel))
                       (proc (make-symbol "mock-proc")))
                   (cl-letf (((symbol-function 'process-status)
                              (lambda (_p) 'signal))
                             ((symbol-function 'process-exit-status)
                              (lambda (_p) 9)))
                     (funcall sentinel proc "killed\n"))
                   proc))))
      (beads-git-command-async
       (lambda (_success _output)
         (setq callback-called t))
       "status"))
    (should callback-called)))

;;; Test beads-git-main-repo-root

(ert-deftest beads-git-test-main-repo-root-strips-git-suffix ()
  "Test beads-git-main-repo-root strips .git suffix."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-command)
             (lambda (&rest _args) "/home/user/myrepo/.git/")))
    (should (equal (beads-git-main-repo-root) "/home/user/myrepo/"))))

(ert-deftest beads-git-test-main-repo-root-handles-worktrees-path ()
  "Test beads-git-main-repo-root with worktrees subdirectory."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-command)
             (lambda (&rest _args) "/home/user/myrepo/.git/worktrees/feature/")))
    ;; When the path doesn't end with /.git/, it returns the directory part
    (let ((result (beads-git-main-repo-root)))
      (should (stringp result)))))

(ert-deftest beads-git-test-main-repo-root-nil-on-error ()
  "Test beads-git-main-repo-root returns nil on git error."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-command)
             (lambda (&rest _args) nil)))
    (should-not (beads-git-main-repo-root))))

;;; Test beads-git-create-worktree

(ert-deftest beads-git-test-create-worktree-success ()
  "Test beads-git-create-worktree returns path on success."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-worktree-create!)
             (lambda (name &rest _args)
               (beads-worktree :name name :path "/path/to/beads-123"
                               :branch "beads-123" :is-main nil))))
    (should (equal (beads-git-create-worktree "beads-123")
                   "/path/to/beads-123"))))

(ert-deftest beads-git-test-create-worktree-error ()
  "Test beads-git-create-worktree propagates errors."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-command-worktree-create!)
             (lambda (_name &rest _args)
               (error "Failed to create worktree"))))
    (should-error (beads-git-create-worktree "beads-123")
                  :type 'error)))

;;; Test beads-git-list-worktrees edge cases

(ert-deftest beads-git-test-list-worktrees-no-branch ()
  "Test beads-git-list-worktrees handles worktree without branch."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-command)
             (lambda (&rest _args)
               "worktree /path/to/detached\nHEAD abc123")))
    (let ((worktrees (beads-git-list-worktrees)))
      (should (equal (length worktrees) 1))
      (should (equal (car worktrees) '("/path/to/detached" nil))))))

(ert-deftest beads-git-test-list-worktrees-multiple-fields ()
  "Test beads-git-list-worktrees handles all porcelain fields."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-command)
             (lambda (&rest _args)
               "worktree /path/to/main\nHEAD abc123\nbranch refs/heads/main\nbare\n\nworktree /path/to/feature\nHEAD def456\nbranch refs/heads/feature")))
    (let ((worktrees (beads-git-list-worktrees)))
      (should (equal (length worktrees) 2))
      (should (equal (car worktrees) '("/path/to/main" "main")))
      (should (equal (cadr worktrees) '("/path/to/feature" "feature"))))))

;;; Test beads-git-should-use-worktree-p edge cases

(ert-deftest beads-git-test-should-use-worktree-truthy-value ()
  "Test beads-git-should-use-worktree-p treats other truthy values."
  :tags '(:unit)
  (let ((beads-agent-use-worktrees 'some-other-symbol))
    (should (beads-git-should-use-worktree-p "beads-1"))))

(ert-deftest beads-git-test-should-use-worktree-string-value ()
  "Test beads-git-should-use-worktree-p treats string as truthy."
  :tags '(:unit)
  (let ((beads-agent-use-worktrees "yes"))
    (should (beads-git-should-use-worktree-p "beads-1"))))

;;; Integration Tests

(ert-deftest beads-git-test-integration-find-worktree-path-suffix ()
  "Integration test: finding worktree by path suffix."
  :tags '(:integration)
  (cl-letf (((symbol-function 'beads-command-worktree-list!)
             (lambda ()
               (list (beads-worktree :name "main" :path "/home/user/projects/main"
                                     :branch "main" :is-main t)
                     (beads-worktree :name "beads.el-42"
                                     :path "/home/user/worktrees/beads.el-42"
                                     :branch "beads.el-42" :is-main nil)
                     (beads-worktree :name "beads.el-99"
                                     :path "/home/user/worktrees/beads.el-99"
                                     :branch "feature" :is-main nil)))))
    ;; Find by path suffix
    (should (equal (beads-git-find-worktree-for-issue "beads.el-42")
                   "/home/user/worktrees/beads.el-42"))
    ;; Find by branch name
    (should (equal (beads-git-find-worktree-for-issue "feature")
                   "/home/user/worktrees/beads.el-99"))
    ;; Not found
    (should-not (beads-git-find-worktree-for-issue "nonexistent"))))

;;; Additional Coverage Tests

(ert-deftest beads-git-test-in-worktree-p-function-exists ()
  "Test beads-git-in-worktree-p function exists."
  :tags '(:unit)
  (should (fboundp 'beads-git-in-worktree-p)))

(ert-deftest beads-git-test-get-project-name-function-exists ()
  "Test beads-git-get-project-name function exists."
  :tags '(:unit)
  (should (fboundp 'beads-git-get-project-name)))

(ert-deftest beads-git-test-get-branch-function-exists ()
  "Test beads-git-get-branch function exists."
  :tags '(:unit)
  (should (fboundp 'beads-git-get-branch)))

(ert-deftest beads-git-test-find-main-repo-function-exists ()
  "Test beads-git-find-main-repo function exists."
  :tags '(:unit)
  (should (fboundp 'beads-git-find-main-repo)))

(ert-deftest beads-git-test-command-function-exists ()
  "Test beads-git-command function exists."
  :tags '(:unit)
  (should (fboundp 'beads-git-command)))

(ert-deftest beads-git-test-get-project-name-nil-root ()
  "Test get-project-name returns nil when no root."
  :tags '(:unit)
  (cl-letf (((symbol-function 'beads-git-find-project-root)
             (lambda () nil)))
    (should-not (beads-git-get-project-name))))

;;; Async Worktree Tests

(ert-deftest beads-git-test-create-worktree-async-success ()
  "Test create worktree async calls bd and invokes callback on success."
  :tags '(:unit)
  (let ((callback-called nil)
        (callback-success nil)
        (callback-path nil))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (cmd callback)
                 ;; Simulate successful command execution
                 (oset cmd exit-code 0)
                 (oset cmd data (beads-worktree :name (oref cmd name)
                                                :path "/path/to/test-branch"
                                                :branch "test-branch" :is-main nil))
                 (funcall callback cmd))))
      (beads-git-create-worktree-async
       "test-branch"
       (lambda (success path)
         (setq callback-called t)
         (setq callback-success success)
         (setq callback-path path)))
      (should callback-called)
      (should callback-success)
      (should (equal callback-path "/path/to/test-branch")))))

(ert-deftest beads-git-test-create-worktree-async-failure ()
  "Test create worktree async passes error to callback on failure."
  :tags '(:unit)
  (let ((callback-called nil)
        (callback-success nil)
        (callback-error nil))
    (cl-letf (((symbol-function 'beads-command-execute-async)
               (lambda (cmd callback)
                 ;; Simulate failed command execution
                 (oset cmd exit-code 1)
                 (oset cmd stderr "Worktree already exists")
                 (funcall callback cmd))))
      (beads-git-create-worktree-async
       "test-branch"
       (lambda (success error)
         (setq callback-called t)
         (setq callback-success success)
         (setq callback-error error)))
      (should callback-called)
      (should-not callback-success)
      (should (equal callback-error "Worktree already exists")))))

(ert-deftest beads-git-test-ensure-worktree-async-existing ()
  "Test ensure-worktree-async returns existing worktree."
  :tags '(:unit)
  (let ((callback-called nil)
        (callback-success nil)
        (callback-path nil))
    (cl-letf (((symbol-function 'beads-git-find-worktree-for-issue)
               (lambda (_id) "/path/to/existing"))
              ((symbol-function 'beads-git-create-worktree-async)
               (lambda (_id _cb) (error "Should not create"))))
      (beads-git-ensure-worktree-async
       "beads-123"
       (lambda (success path)
         (setq callback-called t)
         (setq callback-success success)
         (setq callback-path path)))
      (should callback-called)
      (should callback-success)
      (should (equal callback-path "/path/to/existing")))))

(ert-deftest beads-git-test-ensure-worktree-async-creates-new ()
  "Test ensure-worktree-async creates new worktree when none exists."
  :tags '(:unit)
  (let ((create-called nil))
    (cl-letf (((symbol-function 'beads-git-find-worktree-for-issue)
               (lambda (_id) nil))
              ((symbol-function 'beads-git-create-worktree-async)
               (lambda (id callback)
                 (setq create-called t)
                 (funcall callback t "/new/path"))))
      (beads-git-ensure-worktree-async
       "beads-123"
       (lambda (_success _path) nil))
      (should create-called))))

(provide 'beads-git-test)

;;; beads-git-test.el ends here
