;;; beads-git.el --- Git and worktree support for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, git

;;; Commentary:

;; This module provides git and worktree integration for beads.el.
;; It centralizes all git-related functionality used by both the core
;; beads commands and the agent system.
;;
;; Key features:
;; - Project root discovery via project.el
;; - Git branch information
;; - Git worktree detection and management
;; - Async git operations for non-blocking workflows
;;
;; The module provides two tiers of functions:
;; 1. Core git functions (used by beads.el):
;;    - `beads-git-find-project-root'
;;    - `beads-git-get-project-name'
;;    - `beads-git-get-branch'
;;    - `beads-git-in-worktree-p'
;;    - `beads-git-find-main-repo'
;;
;; 2. Agent worktree functions (used by beads-agent.el):
;;    - `beads-git-main-repo-root'
;;    - `beads-git-list-worktrees'
;;    - `beads-git-find-worktree-for-issue'
;;    - `beads-git-create-worktree-async'

;;; Code:

(require 'cl-lib)

;;; Forward Declarations

;; Customization variables defined elsewhere
(defvar beads-agent-use-worktrees)
(defvar beads-agent-worktree-parent)

;;; Core Git Functions
;;
;; These functions are used by beads.el for project context and .beads discovery.

(defun beads-git-find-project-root ()
  "Find the project root directory.
Returns nil if not in a project."
  (when-let* ((proj (project-current)))
    (if (fboundp 'project-root)
        (project-root proj)
      ;; Emacs 27 compatibility - project-roots is obsolete but needed for old Emacs
      (with-no-warnings
        (car (project-roots proj))))))

(defun beads-git-get-project-name ()
  "Return project name for current context.
Uses the basename of the project root directory.
Returns nil if not in a project."
  (when-let ((root (beads-git-find-project-root)))
    (file-name-nondirectory (directory-file-name root))))

(defun beads-git-get-branch ()
  "Return current git branch name, or nil if not in a git repo.
This is METADATA for display, not identity.  Works over Tramp."
  (let ((default-directory (or (beads-git-find-project-root)
                               default-directory)))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil
                                 "rev-parse" "--abbrev-ref" "HEAD"))
        (let ((branch (string-trim (buffer-string))))
          (unless (string= branch "HEAD")  ; detached HEAD
            branch))))))

(defun beads-git-in-worktree-p ()
  "Return non-nil if current directory is in a git worktree.
In worktrees, .git is a file containing `gitdir: ...' instead of a directory.
Works from nested directories within the worktree."
  (when-let ((git-dir (locate-dominating-file default-directory ".git")))
    (let ((dot-git (expand-file-name ".git" git-dir)))
      (and (file-exists-p dot-git)
           (not (file-directory-p dot-git))))))

(defun beads-git-find-main-repo ()
  "Find the main git repository path when in a worktree.
Uses `git rev-parse --git-common-dir' which returns the shared .git directory.
Returns the main repository path, or nil if not in a worktree or on error."
  (when (beads-git-in-worktree-p)
    (let ((default-directory (or (beads-git-find-project-root)
                                 default-directory)))
      (with-temp-buffer
        (when (zerop (process-file "git" nil t nil
                                   "rev-parse" "--git-common-dir"))
          (let ((git-common-dir (string-trim (buffer-string))))
            (when (and git-common-dir
                       (not (string-empty-p git-common-dir))
                       (not (string-prefix-p "fatal:" git-common-dir)))
              ;; git-common-dir is the .git directory, we need its parent
              (file-name-directory
               (directory-file-name (expand-file-name git-common-dir))))))))))

;;; Agent Git Functions
;;
;; These functions are used by beads-agent.el for worktree management.

(defun beads-git-command (&rest args)
  "Run git with ARGS and return trimmed output, or nil on error."
  (let ((default-directory (or (beads-git-find-project-root)
                               default-directory)))
    (with-temp-buffer
      (when (zerop (apply #'call-process "git" nil t nil args))
        (string-trim (buffer-string))))))

(defun beads-git-command-async (callback &rest args)
  "Run git with ARGS asynchronously and call CALLBACK with result.
CALLBACK receives (success output) where success is t/nil.
Returns the process object, which can be used to cancel the operation."
  (let* ((default-directory (or (beads-git-find-project-root)
                                default-directory))
         (output-buffer (generate-new-buffer " *beads-git-async*")))
    (make-process
     :name "beads-git"
     :buffer output-buffer
     :command (cons "git" args)
     :connection-type 'pipe
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (let ((success (zerop (process-exit-status proc)))
                   (output (when (buffer-live-p output-buffer)
                             (with-current-buffer output-buffer
                               (string-trim (buffer-string))))))
               (funcall callback success (or output "")))
           ;; Always kill buffer, even on error
           (when (buffer-live-p output-buffer)
             (kill-buffer output-buffer))))))))

(defun beads-git-main-repo-root ()
  "Find the main repository root, even from within a worktree.
Returns the path to the main repo, not the worktree."
  (when-let* ((git-dir (beads-git-command
                        "rev-parse" "--path-format=absolute" "--git-common-dir")))
    ;; git-common-dir returns path to .git directory of main repo
    ;; Strip trailing .git or /worktrees/... to get repo root
    (let ((dir (file-name-directory (directory-file-name git-dir))))
      (if (string-suffix-p "/.git/" dir)
          (substring dir 0 -6)  ; Remove /.git/
        dir))))

(defun beads-git-in-linked-worktree-p ()
  "Return non-nil if current directory is inside a git linked worktree.
Distinguishes between the main working tree and linked worktrees."
  (let ((toplevel (beads-git-command "rev-parse" "--show-toplevel"))
        (main-root (beads-git-main-repo-root)))
    (and toplevel main-root
         (not (file-equal-p toplevel main-root)))))

(defun beads-git-should-use-worktree-p (issue-id)
  "Determine whether to use a worktree for ISSUE-ID.
Resolves the value of `beads-agent-use-worktrees':
- t: Return t (always use worktrees)
- nil: Return nil (never use worktrees)
- \\='ask: Prompt the user and return their choice"
  (pcase beads-agent-use-worktrees
    ('t t)
    ('nil nil)
    ('ask
     (yes-or-no-p (format "Use git worktree for agent on %s? " issue-id)))
    ;; Unknown value: treat as truthy for backwards compatibility
    (_ (and beads-agent-use-worktrees t))))

(defun beads-git-list-worktrees ()
  "Return list of (path branch) pairs for all worktrees."
  (when-let* ((output (beads-git-command "worktree" "list" "--porcelain")))
    (let ((worktrees nil)
          (current-path nil)
          (current-branch nil))
      (dolist (line (split-string output "\n" t))
        (cond
         ((string-prefix-p "worktree " line)
          ;; Save previous worktree if we have one
          (when current-path
            (push (list current-path current-branch) worktrees))
          (setq current-path (substring line 9))
          (setq current-branch nil))
         ((string-prefix-p "branch " line)
          (setq current-branch
                (replace-regexp-in-string
                 "^refs/heads/" "" (substring line 7))))))
      ;; Save last worktree
      (when current-path
        (push (list current-path current-branch) worktrees))
      (nreverse worktrees))))

(defun beads-git-find-worktree-for-issue (issue-id)
  "Find existing worktree for ISSUE-ID.
Returns the worktree path or nil if not found."
  (let ((worktrees (beads-git-list-worktrees)))
    (cl-loop for (path branch) in worktrees
             when (or (string-suffix-p (concat "/" issue-id) path)
                      (equal branch issue-id))
             return path)))

(defun beads-git-worktree-path-for-issue (issue-id)
  "Calculate the worktree path for ISSUE-ID.
Does not check if it exists."
  (let* ((main-root (beads-git-main-repo-root))
         (parent (or beads-agent-worktree-parent
                     (file-name-directory (directory-file-name main-root)))))
    (expand-file-name issue-id parent)))

(defun beads-git-create-worktree (issue-id)
  "Create a git worktree for ISSUE-ID.
Creates a new branch based on the current HEAD.
Returns the worktree path on success, signals error on failure."
  (let* ((worktree-path (beads-git-worktree-path-for-issue issue-id))
         (main-root (beads-git-main-repo-root))
         (default-directory main-root))
    ;; Check if path already exists
    (when (file-exists-p worktree-path)
      (error "Worktree path already exists: %s" worktree-path))
    ;; Create worktree with new branch
    (with-temp-buffer
      (let ((exit-code (call-process "git" nil t nil
                                     "worktree" "add"
                                     "-b" issue-id
                                     worktree-path)))
        (unless (zerop exit-code)
          ;; Branch might already exist, try without -b
          (erase-buffer)
          (setq exit-code (call-process "git" nil t nil
                                        "worktree" "add"
                                        worktree-path
                                        issue-id))
          (unless (zerop exit-code)
            (error "Failed to create worktree: %s" (buffer-string))))))
    (message "Created worktree for %s at %s" issue-id worktree-path)
    worktree-path))

(defun beads-git-ensure-worktree (issue-id)
  "Ensure a worktree exists for ISSUE-ID.
Creates one if it doesn't exist.  Returns the worktree path."
  (or (beads-git-find-worktree-for-issue issue-id)
      (beads-git-create-worktree issue-id)))

(defun beads-git-ensure-worktree-async (issue-id callback)
  "Ensure a worktree exists for ISSUE-ID asynchronously.
CALLBACK receives (success worktree-path-or-error)."
  (if-let ((existing (beads-git-find-worktree-for-issue issue-id)))
      ;; Worktree already exists
      (funcall callback t existing)
    ;; Need to create worktree
    (beads-git-create-worktree-async issue-id callback)))

(defun beads-git-create-worktree-async (issue-id callback)
  "Create a git worktree for ISSUE-ID asynchronously.
CALLBACK receives (success worktree-path-or-error)."
  (let* ((worktree-path (beads-git-worktree-path-for-issue issue-id))
         (main-root (beads-git-main-repo-root))
         (default-directory main-root))
    ;; Check if path already exists
    (if (file-exists-p worktree-path)
        (funcall callback nil (format "Worktree path already exists: %s"
                                      worktree-path))
      ;; Create worktree with new branch
      (let ((output-buffer (generate-new-buffer " *beads-worktree*")))
        (make-process
         :name "beads-worktree"
         :buffer output-buffer
         :command (list "git" "worktree" "add" "-b" issue-id worktree-path)
         :connection-type 'pipe
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (unwind-protect
                 (let ((success (zerop (process-exit-status proc))))
                   (if success
                       ;; Worktree created - uses shared database from main repo
                       (progn
                         (message "Created worktree for %s at %s"
                                  issue-id worktree-path)
                         (funcall callback t worktree-path))
                     ;; Branch might already exist, try without -b
                     (beads-git--create-worktree-existing-branch-async
                      issue-id worktree-path callback)))
               ;; Always kill buffer, even on error
               (when (buffer-live-p output-buffer)
                 (kill-buffer output-buffer))))))))))

(defun beads-git--create-worktree-existing-branch-async (issue-id worktree-path
                                                                  callback)
  "Create worktree for existing branch ISSUE-ID at WORKTREE-PATH.
CALLBACK receives (success worktree-path-or-error)."
  (let* ((main-root (beads-git-main-repo-root))
         (default-directory main-root)
         (output-buffer (generate-new-buffer " *beads-worktree*")))
    (make-process
     :name "beads-worktree"
     :buffer output-buffer
     :command (list "git" "worktree" "add" worktree-path issue-id)
     :connection-type 'pipe
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (let ((success (zerop (process-exit-status proc)))
                   (output (when (buffer-live-p output-buffer)
                             (with-current-buffer output-buffer
                               (string-trim (buffer-string))))))
               (if success
                   ;; Worktree created - uses shared database from main repo
                   (progn
                     (message "Created worktree for %s at %s"
                              issue-id worktree-path)
                     (funcall callback t worktree-path))
                 (funcall callback nil (format "Failed to create worktree: %s"
                                               (or output "unknown error")))))
           ;; Always kill buffer, even on error
           (when (buffer-live-p output-buffer)
             (kill-buffer output-buffer))))))))

;;; Compatibility Aliases
;;
;; These aliases provide backwards compatibility during migration.
;; They should be removed in a future release.

(define-obsolete-function-alias 'beads--find-project-root
  'beads-git-find-project-root "0.5.0")

(define-obsolete-function-alias 'beads--get-project-name
  'beads-git-get-project-name "0.5.0")

(define-obsolete-function-alias 'beads--get-git-branch
  'beads-git-get-branch "0.5.0")

(define-obsolete-function-alias 'beads--in-git-worktree-p
  'beads-git-in-worktree-p "0.5.0")

(define-obsolete-function-alias 'beads--find-main-repo-from-worktree
  'beads-git-find-main-repo "0.5.0")

(provide 'beads-git)

;;; beads-git.el ends here
