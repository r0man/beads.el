;;; beads-worktree-test.el --- Tests for git worktree support -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Unit tests for git worktree support functions:
;; - beads--in-git-worktree-p
;; - beads--find-main-repo-from-worktree
;; - beads--find-beads-dir (enhanced)

;;; Code:

(require 'ert)
(require 'beads)

;;; Test beads--in-git-worktree-p

(ert-deftest beads-worktree-test-in-worktree-p-with-git-file ()
  "Test that beads--in-git-worktree-p returns t when .git is a file."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-worktree-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Create a .git file (like in a worktree)
          (with-temp-file (expand-file-name ".git" temp-dir)
            (insert "gitdir: /path/to/main/.git/worktrees/test\n"))
          (should (beads--in-git-worktree-p)))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-worktree-test-in-worktree-p-with-git-directory ()
  "Test that beads--in-git-worktree-p returns nil when .git is a directory."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-worktree-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Create a .git directory (like in a normal repo)
          (make-directory (expand-file-name ".git" temp-dir))
          (should-not (beads--in-git-worktree-p)))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-worktree-test-in-worktree-p-no-git ()
  "Test that beads--in-git-worktree-p returns nil when no .git exists."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-worktree-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; No .git file or directory
          (should-not (beads--in-git-worktree-p)))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-worktree-test-in-worktree-p-nested-directory ()
  "Test beads--in-git-worktree-p works from nested directories."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-worktree-test-" t)))
    (unwind-protect
        (progn
          ;; Create a .git file at root (worktree)
          (with-temp-file (expand-file-name ".git" temp-dir)
            (insert "gitdir: /path/to/main/.git/worktrees/test\n"))
          ;; Create nested directory
          (let ((nested-dir (expand-file-name "src/lib" temp-dir)))
            (make-directory nested-dir t)
            (let ((default-directory nested-dir))
              (should (beads--in-git-worktree-p)))))
      ;; Cleanup
      (delete-directory temp-dir t))))

;;; Test beads--find-main-repo-from-worktree

(ert-deftest beads-worktree-test-find-main-repo-not-worktree ()
  "Test beads--find-main-repo-from-worktree returns nil in normal repo."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-worktree-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Create a .git directory (normal repo)
          (make-directory (expand-file-name ".git" temp-dir))
          (should-not (beads--find-main-repo-from-worktree)))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-worktree-test-find-main-repo-from-worktree ()
  "Test beads--find-main-repo-from-worktree finds main repo path."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-worktree-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Mock beads--in-git-worktree-p to return t (testing the git command logic)
          (cl-letf (((symbol-function 'beads--in-git-worktree-p)
                     (lambda () t)))
            (cl-letf (((symbol-function 'beads--find-project-root)
                       (lambda () temp-dir)))
              (cl-letf (((symbol-function 'process-file)
                         (lambda (_program _infile buffer _display &rest _args)
                           ;; When buffer is t, output goes to current buffer
                           (when (or (eq buffer t) (bufferp buffer))
                             (insert "/path/to/main/.git\n"))
                           0)))
                (should (equal (beads--find-main-repo-from-worktree)
                               "/path/to/main/"))))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-worktree-test-find-main-repo-git-error ()
  "Test beads--find-main-repo-from-worktree handles git errors."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-worktree-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Mock worktree detection and process-file to return error
          (cl-letf (((symbol-function 'beads--in-git-worktree-p)
                     (lambda () t)))
            (cl-letf (((symbol-function 'beads--find-project-root)
                       (lambda () temp-dir)))
              (cl-letf (((symbol-function 'process-file)
                         (lambda (_program _infile buffer _display &rest _args)
                           (when (or (eq buffer t) (bufferp buffer))
                             (insert "fatal: not a git repository\n"))
                           128)))
                (should-not (beads--find-main-repo-from-worktree))))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-worktree-test-find-main-repo-fatal-output ()
  "Test beads--find-main-repo-from-worktree handles fatal: prefix."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-worktree-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Mock worktree detection and process-file to return fatal message
          (cl-letf (((symbol-function 'beads--in-git-worktree-p)
                     (lambda () t)))
            (cl-letf (((symbol-function 'beads--find-project-root)
                       (lambda () temp-dir)))
              (cl-letf (((symbol-function 'process-file)
                         (lambda (_program _infile buffer _display &rest _args)
                           (when (or (eq buffer t) (bufferp buffer))
                             (insert "fatal: some error\n"))
                           0)))
                (should-not (beads--find-main-repo-from-worktree))))))
      ;; Cleanup
      (delete-directory temp-dir t))))

;;; Test beads--find-beads-dir with worktree support

(ert-deftest beads-worktree-test-find-beads-dir-local ()
  "Test beads--find-beads-dir finds local .beads directory."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-worktree-test-" t))
        (beads--project-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; Create .beads directory locally
          (make-directory (expand-file-name ".beads" temp-dir))
          (should (equal (beads--find-beads-dir)
                         (expand-file-name ".beads" temp-dir))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-worktree-test-find-beads-dir-from-worktree ()
  "Test beads--find-beads-dir finds .beads in main repo from worktree."
  :tags '(:unit)
  (let ((main-repo (make-temp-file "beads-main-" t))
        (worktree (make-temp-file "beads-worktree-" t))
        (beads--project-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Create .beads in main repo
          (make-directory (expand-file-name ".beads" main-repo))
          ;; Create .git file in worktree
          (with-temp-file (expand-file-name ".git" worktree)
            (insert (format "gitdir: %s/.git/worktrees/test\n" main-repo)))
          (let ((default-directory worktree))
            ;; Mock the main repo discovery
            (cl-letf (((symbol-function 'beads--find-main-repo-from-worktree)
                       (lambda () main-repo))
                      ((symbol-function 'beads--find-project-root)
                       (lambda () nil)))
              (should (equal (beads--find-beads-dir)
                             (expand-file-name ".beads" main-repo))))))
      ;; Cleanup
      (delete-directory main-repo t)
      (delete-directory worktree t))))

(ert-deftest beads-worktree-test-find-beads-dir-not-found ()
  "Test beads--find-beads-dir returns nil when no .beads anywhere."
  :tags '(:unit)
  (let ((temp-dir (make-temp-file "beads-worktree-test-" t))
        (beads--project-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (let ((default-directory temp-dir))
          ;; No .beads directory
          (cl-letf (((symbol-function 'beads--find-main-repo-from-worktree)
                     (lambda () nil))
                    ((symbol-function 'beads--find-project-root)
                     (lambda () nil)))
            (should-not (beads--find-beads-dir))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest beads-worktree-test-find-beads-dir-caches-worktree-path ()
  "Test beads--find-beads-dir caches worktree paths correctly."
  :tags '(:unit)
  (let ((main-repo (make-temp-file "beads-main-" t))
        (worktree (make-temp-file "beads-worktree-" t))
        (beads--project-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Create .beads in main repo
          (make-directory (expand-file-name ".beads" main-repo))
          ;; Create .git file in worktree
          (with-temp-file (expand-file-name ".git" worktree)
            (insert (format "gitdir: %s/.git/worktrees/test\n" main-repo)))
          (let ((default-directory worktree)
                (main-repo-call-count 0))
            ;; Mock the main repo discovery
            (cl-letf (((symbol-function 'beads--find-main-repo-from-worktree)
                       (lambda ()
                         (setq main-repo-call-count (1+ main-repo-call-count))
                         main-repo))
                      ((symbol-function 'beads--find-project-root)
                       (lambda () nil)))
              ;; First call should populate cache
              (beads--find-beads-dir)
              (should (= main-repo-call-count 1))
              ;; Second call should use cache (not call main-repo fn again)
              (beads--find-beads-dir)
              (should (= main-repo-call-count 1)))))
      ;; Cleanup
      (delete-directory main-repo t)
      (delete-directory worktree t))))

(ert-deftest beads-worktree-test-find-beads-dir-main-repo-no-beads ()
  "Test beads--find-beads-dir returns nil when main repo has no .beads."
  :tags '(:unit)
  (let ((main-repo (make-temp-file "beads-main-" t))
        (worktree (make-temp-file "beads-worktree-" t))
        (beads--project-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; No .beads in main repo
          ;; Create .git file in worktree
          (with-temp-file (expand-file-name ".git" worktree)
            (insert (format "gitdir: %s/.git/worktrees/test\n" main-repo)))
          (let ((default-directory worktree))
            ;; Mock the main repo discovery
            (cl-letf (((symbol-function 'beads--find-main-repo-from-worktree)
                       (lambda () main-repo))
                      ((symbol-function 'beads--find-project-root)
                       (lambda () nil)))
              (should-not (beads--find-beads-dir)))))
      ;; Cleanup
      (delete-directory main-repo t)
      (delete-directory worktree t))))

;;; Test beads--find-beads-dir worktree preference

(ert-deftest beads-worktree-test-find-beads-dir-prefers-local-over-main ()
  "Test beads--find-beads-dir prefers worktree-local .beads over main repo."
  :tags '(:unit)
  (let ((main-repo (make-temp-file "beads-main-" t))
        (worktree (make-temp-file "beads-worktree-" t))
        (beads--project-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Create .beads in BOTH main repo AND worktree
          (make-directory (expand-file-name ".beads" main-repo))
          (make-directory (expand-file-name ".beads" worktree))
          ;; Create .git file in worktree
          (with-temp-file (expand-file-name ".git" worktree)
            (insert (format "gitdir: %s/.git/worktrees/test\n" main-repo)))
          (let ((default-directory worktree))
            ;; Should find worktree's .beads, not main repo's
            (should (equal (beads--find-beads-dir)
                           (expand-file-name ".beads" worktree)))))
      ;; Cleanup
      (delete-directory main-repo t)
      (delete-directory worktree t))))

(ert-deftest beads-worktree-test-find-beads-dir-nested-in-worktree ()
  "Test beads--find-beads-dir finds worktree .beads from nested directory."
  :tags '(:unit)
  (let ((main-repo (make-temp-file "beads-main-" t))
        (worktree (make-temp-file "beads-worktree-" t))
        (beads--project-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Create .beads in both places
          (make-directory (expand-file-name ".beads" main-repo))
          (make-directory (expand-file-name ".beads" worktree))
          ;; Create .git file in worktree
          (with-temp-file (expand-file-name ".git" worktree)
            (insert (format "gitdir: %s/.git/worktrees/test\n" main-repo)))
          ;; Create nested directory in worktree
          (let ((nested-dir (expand-file-name "src/lib/util" worktree)))
            (make-directory nested-dir t)
            (let ((default-directory nested-dir))
              ;; Even from nested dir, should find worktree's .beads
              (should (equal (beads--find-beads-dir)
                             (expand-file-name ".beads" worktree))))))
      ;; Cleanup
      (delete-directory main-repo t)
      (delete-directory worktree t))))

;;; Test beads-global-no-daemon

(ert-deftest beads-worktree-test-global-no-daemon-adds-flag ()
  "Test beads--build-command adds --no-daemon when global option set."
  :tags '(:unit)
  (let ((beads-global-no-daemon t)
        (beads-global-no-auto-flush nil)
        (beads-global-no-auto-import nil)
        (beads-global-no-db nil)
        (beads-global-sandbox nil)
        (beads-global-actor nil)
        (beads-global-db nil)
        (beads-actor nil)
        (beads-database-path nil))
    (cl-letf (((symbol-function 'beads--in-git-worktree-p)
               (lambda () nil))
              ((symbol-function 'beads--get-database-path)
               (lambda () nil)))
      (let ((cmd (beads--build-command "list")))
        (should (member "--no-daemon" cmd))))))

(ert-deftest beads-worktree-test-no-daemon-not-added-by-default ()
  "Test beads--build-command does not add --no-daemon by default."
  :tags '(:unit)
  (let ((beads-global-no-daemon nil)
        (beads-global-no-auto-flush nil)
        (beads-global-no-auto-import nil)
        (beads-global-no-db nil)
        (beads-global-sandbox nil)
        (beads-global-actor nil)
        (beads-global-db nil)
        (beads-actor nil)
        (beads-database-path nil))
    (cl-letf (((symbol-function 'beads--in-git-worktree-p)
               (lambda () t))  ; Even in worktree, should not add
              ((symbol-function 'beads--get-database-path)
               (lambda () nil)))
      (let ((cmd (beads--build-command "list")))
        (should-not (member "--no-daemon" cmd))))))

;;; Test beads-info

(ert-deftest beads-worktree-test-info-normal-repo ()
  "Test beads-info displays correct info for normal repo."
  :tags '(:unit)
  (let ((beads-global-no-daemon nil)
        (message-output nil))
    (cl-letf (((symbol-function 'beads--in-git-worktree-p)
               (lambda () nil))
              ((symbol-function 'beads--find-main-repo-from-worktree)
               (lambda () nil))
              ((symbol-function 'beads--find-beads-dir)
               (lambda () "/path/to/.beads"))
              ((symbol-function 'beads--get-database-path)
               (lambda () "/path/to/.beads/test.db"))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-info)
      (should (string-match-p "In worktree: no" message-output))
      (should (string-match-p "Main repo: N/A" message-output))
      (should (string-match-p "\\.beads dir: /path/to/\\.beads" message-output))
      (should (string-match-p "Database: /path/to/\\.beads/test.db" message-output))
      (should (string-match-p "--no-daemon: disabled" message-output)))))

(ert-deftest beads-worktree-test-info-worktree ()
  "Test beads-info displays correct info for worktree."
  :tags '(:unit)
  (let ((beads-global-no-daemon nil)
        (message-output nil))
    (cl-letf (((symbol-function 'beads--in-git-worktree-p)
               (lambda () t))
              ((symbol-function 'beads--find-main-repo-from-worktree)
               (lambda () "/main/repo/"))
              ((symbol-function 'beads--find-beads-dir)
               (lambda () "/main/repo/.beads"))
              ((symbol-function 'beads--get-database-path)
               (lambda () "/main/repo/.beads/test.db"))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-info)
      (should (string-match-p "In worktree: yes" message-output))
      (should (string-match-p "Main repo: /main/repo/" message-output)))))

(ert-deftest beads-worktree-test-info-no-daemon-global ()
  "Test beads-info shows --no-daemon via transient."
  :tags '(:unit)
  (let ((beads-global-no-daemon t)
        (message-output nil))
    (cl-letf (((symbol-function 'beads--in-git-worktree-p)
               (lambda () nil))
              ((symbol-function 'beads--find-main-repo-from-worktree)
               (lambda () nil))
              ((symbol-function 'beads--find-beads-dir)
               (lambda () nil))
              ((symbol-function 'beads--get-database-path)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-output (apply #'format fmt args)))))
      (beads-info)
      (should (string-match-p "--no-daemon: enabled (via transient)" message-output)))))

(provide 'beads-worktree-test)
;;; beads-worktree-test.el ends here
