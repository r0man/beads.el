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

;;; Test BD Worktree Support Functions

(ert-deftest beads-git-test-bd-worktree-available-function-exists ()
  "Test that beads-git-bd-worktree-available-p function exists."
  :tags '(:unit)
  (should (fboundp 'beads-git-bd-worktree-available-p)))

(ert-deftest beads-git-test-bd-worktree-available-p-caches-result ()
  "Test that beads-git-bd-worktree-available-p caches its result."
  :tags '(:unit)
  (let ((beads-git--bd-worktree-available nil)
        (beads-executable "bd")
        (call-count 0))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args)
                 (setq call-count (1+ call-count))
                 0)))
      ;; First call should invoke call-process
      (should (beads-git-bd-worktree-available-p))
      (should (= call-count 1))
      ;; Second call should use cached value
      (should (beads-git-bd-worktree-available-p))
      (should (= call-count 1)))))

(ert-deftest beads-git-test-bd-worktree-available-p-unavailable ()
  "Test beads-git-bd-worktree-available-p returns nil when bd unavailable."
  :tags '(:unit)
  (let ((beads-git--bd-worktree-available nil)
        (beads-executable "bd"))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args) 1)))  ; Non-zero exit = failure
      (should-not (beads-git-bd-worktree-available-p))
      ;; Should cache the unavailable result
      (should (eq beads-git--bd-worktree-available 'unavailable)))))

(ert-deftest beads-git-test-bd-worktree-available-p-no-executable ()
  "Test beads-git-bd-worktree-available-p returns nil when no executable."
  :tags '(:unit)
  (let ((beads-git--bd-worktree-available nil)
        (beads-executable nil))
    (should-not (beads-git-bd-worktree-available-p))))

(ert-deftest beads-git-test-clear-bd-cache ()
  "Test beads-git-clear-bd-cache clears the cache."
  :tags '(:unit)
  (let ((beads-git--bd-worktree-available t))
    (beads-git-clear-bd-cache)
    (should (null beads-git--bd-worktree-available))))

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
  "Test finding worktree by issue ID in path (git fallback)."
  :tags '(:unit)
  ;; Force git fallback path
  (cl-letf (((symbol-function 'beads-git-bd-worktree-available-p)
             (lambda () nil))
            ((symbol-function 'beads-git-list-worktrees)
             (lambda ()
               '(("/path/to/main" "main")
                 ("/path/to/beads-123" "beads-123")))))
    (should (equal (beads-git-find-worktree-for-issue "beads-123")
                   "/path/to/beads-123"))))

(ert-deftest beads-git-test-find-worktree-by-branch ()
  "Test finding worktree by branch name matching issue ID (git fallback)."
  :tags '(:unit)
  ;; Force git fallback path
  (cl-letf (((symbol-function 'beads-git-bd-worktree-available-p)
             (lambda () nil))
            ((symbol-function 'beads-git-list-worktrees)
             (lambda ()
               '(("/path/to/main" "main")
                 ("/custom/path/foo" "beads-456")))))
    (should (equal (beads-git-find-worktree-for-issue "beads-456")
                   "/custom/path/foo"))))

(ert-deftest beads-git-test-find-worktree-not-found ()
  "Test beads-git-find-worktree-for-issue returns nil when not found."
  :tags '(:unit)
  ;; Force git fallback path
  (cl-letf (((symbol-function 'beads-git-bd-worktree-available-p)
             (lambda () nil))
            ((symbol-function 'beads-git-list-worktrees)
             (lambda ()
               '(("/path/to/main" "main")))))
    (should-not (beads-git-find-worktree-for-issue "beads-999"))))

(ert-deftest beads-git-test-find-worktree-uses-bd-when-available ()
  "Test beads-git-find-worktree-for-issue uses bd when available."
  :tags '(:unit)
  (let ((bd-called nil)
        (git-called nil))
    (cl-letf (((symbol-function 'beads-git-bd-worktree-available-p)
               (lambda () t))
              ((symbol-function 'beads-git--find-worktree-for-issue-bd)
               (lambda (_id) (setq bd-called t) "/path/to/worktree"))
              ((symbol-function 'beads-git--find-worktree-for-issue-git)
               (lambda (_id) (setq git-called t) nil)))
      (should (equal (beads-git-find-worktree-for-issue "test-123")
                     "/path/to/worktree"))
      (should bd-called)
      (should-not git-called))))

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
  "Test beads-git-create-worktree returns path on success (git fallback)."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (let ((expected-path (expand-file-name "beads-123" temp-dir)))
          ;; Force git fallback path
          (cl-letf (((symbol-function 'beads-git-bd-worktree-available-p)
                     (lambda () nil))
                    ((symbol-function 'beads-git-worktree-path-for-issue)
                     (lambda (_id) expected-path))
                    ((symbol-function 'beads-git-main-repo-root)
                     (lambda () temp-dir))
                    ((symbol-function 'call-process)
                     (lambda (_program _infile _buffer _display &rest _args)
                       0)))
            (should (equal (beads-git-create-worktree "beads-123")
                           expected-path))))
      (delete-directory temp-dir t))))

(ert-deftest beads-git-test-create-worktree-uses-bd-when-available ()
  "Test beads-git-create-worktree uses bd when available."
  :tags '(:unit)
  (let ((bd-called nil))
    (cl-letf (((symbol-function 'beads-git-bd-worktree-available-p)
               (lambda () t))
              ((symbol-function 'beads-git--create-worktree-bd)
               (lambda (_id) (setq bd-called t) "/path/to/worktree")))
      (should (equal (beads-git-create-worktree "test-123")
                     "/path/to/worktree"))
      (should bd-called))))

(ert-deftest beads-git-test-create-worktree-path-exists ()
  "Test beads-git-create-worktree errors when path exists (git fallback)."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (let ((worktree-path (expand-file-name "existing" temp-dir)))
          (make-directory worktree-path)
          ;; Force git fallback path
          (cl-letf (((symbol-function 'beads-git-bd-worktree-available-p)
                     (lambda () nil))
                    ((symbol-function 'beads-git-worktree-path-for-issue)
                     (lambda (_id) worktree-path))
                    ((symbol-function 'beads-git-main-repo-root)
                     (lambda () temp-dir)))
            (should-error (beads-git-create-worktree "existing")
                          :type 'error)))
      (delete-directory temp-dir t))))

(ert-deftest beads-git-test-create-worktree-fallback-existing-branch ()
  "Test beads-git-create-worktree tries existing branch on -b failure (git)."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t))
        (first-attempt t))
    (unwind-protect
        (let ((expected-path (expand-file-name "beads-123" temp-dir)))
          ;; Force git fallback path
          (cl-letf (((symbol-function 'beads-git-bd-worktree-available-p)
                     (lambda () nil))
                    ((symbol-function 'beads-git-worktree-path-for-issue)
                     (lambda (_id) expected-path))
                    ((symbol-function 'beads-git-main-repo-root)
                     (lambda () temp-dir))
                    ((symbol-function 'call-process)
                     (lambda (_program _infile _buffer _display &rest args)
                       (if (and first-attempt (member "-b" args))
                           (progn (setq first-attempt nil) 1)  ; First attempt fails
                         0))))  ; Second attempt succeeds
            (should (equal (beads-git-create-worktree "beads-123")
                           expected-path))))
      (delete-directory temp-dir t))))

(ert-deftest beads-git-test-create-worktree-both-attempts-fail ()
  "Test beads-git-create-worktree errors when both attempts fail (git)."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-git-test-" t)))
    (unwind-protect
        (let ((expected-path (expand-file-name "beads-123" temp-dir)))
          ;; Force git fallback path
          (cl-letf (((symbol-function 'beads-git-bd-worktree-available-p)
                     (lambda () nil))
                    ((symbol-function 'beads-git-worktree-path-for-issue)
                     (lambda (_id) expected-path))
                    ((symbol-function 'beads-git-main-repo-root)
                     (lambda () temp-dir))
                    ((symbol-function 'call-process)
                     (lambda (_program _infile buffer _display &rest _args)
                       (when buffer
                         (with-current-buffer (if (eq buffer t)
                                                  (current-buffer)
                                                buffer)
                           (insert "fatal: error\n")))
                       1)))
            (should-error (beads-git-create-worktree "beads-123")
                          :type 'error)))
      (delete-directory temp-dir t))))

;;; Test beads-git-ensure-worktree-async

(ert-deftest beads-git-test-ensure-worktree-async-existing ()
  "Test beads-git-ensure-worktree-async uses existing worktree."
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
  "Test beads-git-ensure-worktree-async creates new worktree."
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
  "Integration test: finding worktree by path suffix (git fallback)."
  :tags '(:integration)
  ;; Force git fallback path
  (cl-letf (((symbol-function 'beads-git-bd-worktree-available-p)
             (lambda () nil))
            ((symbol-function 'beads-git-list-worktrees)
             (lambda ()
               '(("/home/user/projects/main" "main")
                 ("/home/user/worktrees/beads.el-42" "beads.el-42")
                 ("/home/user/worktrees/beads.el-99" "feature")))))
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

(defmacro beads-git-test--wait-for (condition &optional timeout)
  "Wait for CONDITION to become non-nil.
TIMEOUT is max seconds to wait (default 10)."
  `(let ((deadline (+ (float-time) (or ,timeout 10))))
     (while (and (not ,condition)
                 (< (float-time) deadline))
       (accept-process-output nil 0.1))))

(ert-deftest beads-git-test-create-worktree-async-success ()
  "Integration test: create worktree asynchronously."
  :tags '(:integration :slow :async)
  (skip-unless (executable-find "git"))
  (let* ((temp-dir (make-temp-file "beads-git-test-" t))
         (worktree-parent (expand-file-name "worktrees" temp-dir))
         (repo-dir (expand-file-name "repo" temp-dir))
         (result nil)
         (callback-called nil))
    (unwind-protect
        (progn
          ;; Create directories
          (make-directory worktree-parent t)
          (make-directory repo-dir t)
          ;; Initialize git repo with a commit
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "init")
            (call-process "git" nil nil nil "config" "user.email" "test@test.com")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (with-temp-file (expand-file-name "README.md" repo-dir)
              (insert "# Test\n"))
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil "commit" "-m" "Initial"))
          ;; Set up worktree config
          (let ((beads-agent-worktree-parent worktree-parent)
                (default-directory repo-dir))
            ;; Mock main-repo-root to return our test repo
            (cl-letf (((symbol-function 'beads-git-main-repo-root)
                       (lambda () repo-dir)))
              ;; Call async function
              (beads-git-create-worktree-async
               "test-branch"
               (lambda (success path-or-error)
                 (setq result (list success path-or-error))
                 (setq callback-called t)))
              ;; Wait for callback
              (beads-git-test--wait-for callback-called)
              ;; Verify result
              (should callback-called)
              (should (car result))  ; success = t
              (should (stringp (cadr result)))  ; path
              (should (file-directory-p (cadr result))))))
      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest beads-git-test-create-worktree-async-path-exists ()
  "Integration test: async worktree creation fails when path exists."
  :tags '(:integration :slow :async)
  (skip-unless (executable-find "git"))
  (let* ((temp-dir (make-temp-file "beads-git-test-" t))
         (worktree-parent (expand-file-name "worktrees" temp-dir))
         (repo-dir (expand-file-name "repo" temp-dir))
         (result nil)
         (callback-called nil))
    (unwind-protect
        (progn
          ;; Create directories
          (make-directory worktree-parent t)
          (make-directory repo-dir t)
          ;; Create the worktree path so it already exists
          (let ((existing-path (expand-file-name "test-branch" worktree-parent)))
            (make-directory existing-path t)
            ;; Initialize git repo
            (let ((default-directory repo-dir))
              (call-process "git" nil nil nil "init")
              (call-process "git" nil nil nil "config" "user.email" "test@test.com")
              (call-process "git" nil nil nil "config" "user.name" "Test")
              (with-temp-file (expand-file-name "README.md" repo-dir)
                (insert "# Test\n"))
              (call-process "git" nil nil nil "add" ".")
              (call-process "git" nil nil nil "commit" "-m" "Initial"))
            ;; Set up config
            (let ((beads-agent-worktree-parent worktree-parent)
                  (default-directory repo-dir))
              (cl-letf (((symbol-function 'beads-git-main-repo-root)
                         (lambda () repo-dir)))
                ;; Call async function - should fail because path exists
                (beads-git-create-worktree-async
                 "test-branch"
                 (lambda (success path-or-error)
                   (setq result (list success path-or-error))
                   (setq callback-called t)))
                ;; This should be synchronous (path check)
                (should callback-called)
                (should-not (car result))  ; success = nil
                (should (string-match-p "already exists" (cadr result)))))))
      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest beads-git-test-ensure-worktree-async-existing-integration ()
  "Integration test: ensure-worktree-async returns existing worktree."
  :tags '(:integration :slow :async)
  (skip-unless (executable-find "git"))
  (let* ((temp-dir (make-temp-file "beads-git-test-" t))
         (worktree-parent (expand-file-name "worktrees" temp-dir))
         (repo-dir (expand-file-name "repo" temp-dir))
         (result nil)
         (callback-called nil))
    (unwind-protect
        (progn
          ;; Create directories
          (make-directory worktree-parent t)
          (make-directory repo-dir t)
          ;; Initialize git repo with a commit
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "init")
            (call-process "git" nil nil nil "config" "user.email" "test@test.com")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (with-temp-file (expand-file-name "README.md" repo-dir)
              (insert "# Test\n"))
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil "commit" "-m" "Initial")
            ;; Create a worktree synchronously first
            (let ((worktree-path (expand-file-name "existing-issue" worktree-parent)))
              (call-process "git" nil nil nil
                            "worktree" "add" "-b" "existing-issue" worktree-path)))
          ;; Now test ensure-worktree-async finds the existing one
          (let ((beads-agent-worktree-parent worktree-parent)
                (default-directory repo-dir))
            (cl-letf (((symbol-function 'beads-git-main-repo-root)
                       (lambda () repo-dir)))
              (beads-git-ensure-worktree-async
               "existing-issue"
               (lambda (success path-or-error)
                 (setq result (list success path-or-error))
                 (setq callback-called t)))
              ;; Should be synchronous since worktree exists
              (should callback-called)
              (should (car result))  ; success = t
              (should (string-match-p "existing-issue" (cadr result))))))
      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest beads-git-test-create-worktree-existing-branch-async ()
  "Integration test: create worktree for existing branch asynchronously."
  :tags '(:integration :slow :async)
  (skip-unless (executable-find "git"))
  (let* ((temp-dir (make-temp-file "beads-git-test-" t))
         (worktree-parent (expand-file-name "worktrees" temp-dir))
         (repo-dir (expand-file-name "repo" temp-dir))
         (result nil)
         (callback-called nil))
    (unwind-protect
        (progn
          ;; Create directories
          (make-directory worktree-parent t)
          (make-directory repo-dir t)
          ;; Initialize git repo with a commit and create a branch
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "init")
            (call-process "git" nil nil nil "config" "user.email" "test@test.com")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (with-temp-file (expand-file-name "README.md" repo-dir)
              (insert "# Test\n"))
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil "commit" "-m" "Initial")
            ;; Create branch but don't check it out
            (call-process "git" nil nil nil "branch" "feature-branch"))
          ;; Test creating worktree for existing branch
          (let ((beads-agent-worktree-parent worktree-parent)
                (default-directory repo-dir)
                (worktree-path (expand-file-name "feature-branch" worktree-parent)))
            (cl-letf (((symbol-function 'beads-git-main-repo-root)
                       (lambda () repo-dir)))
              ;; Directly call the existing-branch variant
              (beads-git--create-worktree-existing-branch-async
               "feature-branch"
               worktree-path
               (lambda (success path-or-error)
                 (setq result (list success path-or-error))
                 (setq callback-called t)))
              ;; Wait for callback
              (beads-git-test--wait-for callback-called)
              ;; Verify result
              (should callback-called)
              (should (car result))  ; success = t
              (should (file-directory-p (cadr result))))))
      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest beads-git-test-create-worktree-existing-branch-async-failure ()
  "Integration test: worktree creation fails for nonexistent branch."
  :tags '(:integration :slow :async)
  (skip-unless (executable-find "git"))
  (let* ((temp-dir (make-temp-file "beads-git-test-" t))
         (worktree-parent (expand-file-name "worktrees" temp-dir))
         (repo-dir (expand-file-name "repo" temp-dir))
         (result nil)
         (callback-called nil))
    (unwind-protect
        (progn
          ;; Create directories
          (make-directory worktree-parent t)
          (make-directory repo-dir t)
          ;; Initialize git repo with a commit (no extra branches)
          (let ((default-directory repo-dir))
            (call-process "git" nil nil nil "init")
            (call-process "git" nil nil nil "config" "user.email" "test@test.com")
            (call-process "git" nil nil nil "config" "user.name" "Test")
            (with-temp-file (expand-file-name "README.md" repo-dir)
              (insert "# Test\n"))
            (call-process "git" nil nil nil "add" ".")
            (call-process "git" nil nil nil "commit" "-m" "Initial"))
          ;; Test creating worktree for nonexistent branch
          (let ((beads-agent-worktree-parent worktree-parent)
                (default-directory repo-dir)
                (worktree-path (expand-file-name "nonexistent" worktree-parent)))
            (cl-letf (((symbol-function 'beads-git-main-repo-root)
                       (lambda () repo-dir)))
              ;; Try to create worktree for branch that doesn't exist
              (beads-git--create-worktree-existing-branch-async
               "nonexistent-branch"
               worktree-path
               (lambda (success path-or-error)
                 (setq result (list success path-or-error))
                 (setq callback-called t)))
              ;; Wait for callback
              (beads-git-test--wait-for callback-called)
              ;; Verify failure
              (should callback-called)
              (should-not (car result))  ; success = nil
              (should (stringp (cadr result))))))  ; error message
      ;; Cleanup
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(provide 'beads-git-test)

;;; beads-git-test.el ends here
