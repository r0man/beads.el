;;; beads-command-worktree.el --- Worktree command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd worktree' operations,
;; providing an object-oriented interface to bd worktree commands.
;;
;; The bd worktree commands manage git worktrees with proper beads
;; configuration.  When creating a worktree, beads automatically sets up
;; a redirect file so all worktrees share the same .beads database.
;;
;; Command classes:
;; - beads-command-worktree-create: Create worktree with beads redirect
;; - beads-command-worktree-list: List all git worktrees
;; - beads-command-worktree-remove: Remove worktree with safety checks
;; - beads-command-worktree-info: Show info about current worktree
;;
;; Domain types:
;; - beads-worktree: Represents a git worktree with beads state
;; - beads-worktree-info: Information about current worktree context
;;
;; Usage:
;;   ;; Create a worktree
;;   (beads-command-worktree-create! :name "feature-auth")
;;   (beads-command-worktree-create! :name "bugfix" :branch "fix-login")
;;
;;   ;; List all worktrees
;;   (beads-command-worktree-list!)
;;
;;   ;; Remove a worktree
;;   (beads-command-worktree-remove! :name "feature-auth")
;;   (beads-command-worktree-remove! :name "stale-work" :force t)
;;
;;   ;; Get info about current worktree
;;   (beads-command-worktree-info!)

;;; Code:

(require 'eieio)
(require 'beads-command)
(require 'cl-lib)

;;; ============================================================
;;; Domain Type: beads-worktree
;;; ============================================================

(defclass beads-worktree ()
  ((name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Worktree name (directory name).")
   (path
    :initarg :path
    :type (or null string)
    :initform nil
    :documentation "Absolute path to the worktree directory.")
   (branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Git branch checked out in this worktree.")
   (is-main
    :initarg :is-main
    :type boolean
    :initform nil
    :documentation "Whether this is the main worktree (not a linked worktree).")
   (beads-state
    :initarg :beads-state
    :type (or null string)
    :initform nil
    :documentation "Beads configuration state.
Possible values:
- \"shared\": Main repository with .beads directory
- \"redirect\": Worktree with redirect to main .beads
- \"local\": Has its own .beads (not recommended)
- \"none\": No beads configuration"))
  :documentation "Represents a git worktree with beads configuration.")

(defun beads-worktree-from-json (json)
  "Create a beads-worktree instance from JSON alist.
JSON is an alist with keys: name, path, branch, is_main, beads_state.
For create command output which lacks `name', derives it from `path'."
  (let* ((path (alist-get 'path json))
         (name (or (alist-get 'name json)
                   (and path (file-name-nondirectory
                              (directory-file-name path))))))
    (beads-worktree
     :name name
     :path path
     :branch (alist-get 'branch json)
     :is-main (eq t (alist-get 'is_main json))
     :beads-state (alist-get 'beads_state json))))

;;; ============================================================
;;; Domain Type: beads-worktree-info
;;; ============================================================

(defclass beads-worktree-info ()
  ((is-worktree
    :initarg :is-worktree
    :type boolean
    :initform nil
    :documentation "Whether current directory is in a worktree.")
   (name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Worktree name (if in a worktree).")
   (path
    :initarg :path
    :type (or null string)
    :initform nil
    :documentation "Worktree path (if in a worktree).")
   (branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Branch name (if in a worktree).")
   (main-path
    :initarg :main-path
    :type (or null string)
    :initform nil
    :documentation "Path to main repository (if in a linked worktree).")
   (beads-state
    :initarg :beads-state
    :type (or null string)
    :initform nil
    :documentation "Beads configuration state."))
  :documentation "Information about the current worktree context.")

(defun beads-worktree-info-from-json (json)
  "Create a beads-worktree-info instance from JSON alist.
JSON is an alist from `bd worktree info --json'."
  (beads-worktree-info
   :is-worktree (eq t (alist-get 'is_worktree json))
   :name (alist-get 'name json)
   :path (alist-get 'path json)
   :branch (alist-get 'branch json)
   :main-path (alist-get 'main_path json)
   :beads-state (alist-get 'beads_state json)))

;;; ============================================================
;;; Command Class: beads-command-worktree-create
;;; ============================================================

(beads-defcommand beads-command-worktree-create (beads-command-json)
  ((name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Worktree name (also used as directory name)."
    :positional 1)
   (branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Branch name for the worktree (--branch).
Default: same as worktree name."
    :long-option "--branch"
    :option-type :string
    :transient-key "-b"
    :transient-description "--branch"
    :transient-class transient-option
    :transient-argument "--branch="
    :transient-prompt "Branch name: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd worktree create command.
Creates a git worktree with beads redirect configuration.")

(cl-defmethod beads-command-subcommand ((_command beads-command-worktree-create))
  "Return \"worktree create\" as the CLI subcommand."
  "worktree create")

(cl-defmethod beads-command-validate ((command beads-command-worktree-create))
  "Validate worktree create COMMAND.
Requires name to be set."
  (with-slots (name) command
    (cond
     ((not name) "Worktree name is required")
     ((string-empty-p name) "Worktree name cannot be empty")
     (t nil))))

(cl-defmethod beads-command-parse ((command beads-command-worktree-create))
  "Parse worktree create COMMAND output.
Returns beads-worktree instance on success."
  (with-slots (json) command
    (if (not json)
        (cl-call-next-method)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (beads-worktree-from-json parsed-json)
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-worktree: %s"
                                 (error-message-string err))
                         :exit-code (oref command exit-code)
                         :parsed-json parsed-json
                         :stderr (oref command stderr)
                         :parse-error err))))))))

;;; ============================================================
;;; Command Class: beads-command-worktree-list
;;; ============================================================

(beads-defcommand beads-command-worktree-list (beads-command-json)
  ()
  :documentation "Represents bd worktree list command.
Lists all git worktrees with their beads configuration state.")

(cl-defmethod beads-command-subcommand ((_command beads-command-worktree-list))
  "Return \"worktree list\" as the CLI subcommand."
  "worktree list")

(cl-defmethod beads-command-parse ((command beads-command-worktree-list))
  "Parse worktree list COMMAND output.
Returns list of beads-worktree instances."
  (with-slots (json) command
    (if (not json)
        (cl-call-next-method)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (mapcar #'beads-worktree-from-json (append parsed-json nil))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-worktree list: %s"
                                 (error-message-string err))
                         :exit-code (oref command exit-code)
                         :parsed-json parsed-json
                         :stderr (oref command stderr)
                         :parse-error err))))))))

;;; ============================================================
;;; Command Class: beads-command-worktree-remove
;;; ============================================================

(beads-defcommand beads-command-worktree-remove (beads-command-json)
  ((name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Worktree name to remove."
    :positional 1)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Skip safety checks (--force).
By default, removal checks for uncommitted changes, unpushed commits,
and stashes."
    :long-option "--force"
    :option-type :boolean
    :transient-key "-f"
    :transient-description "--force"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd worktree remove command.
Removes a worktree with safety checks (unless --force is used).")

(cl-defmethod beads-command-subcommand ((_command beads-command-worktree-remove))
  "Return \"worktree remove\" as the CLI subcommand."
  "worktree remove")

(cl-defmethod beads-command-validate ((command beads-command-worktree-remove))
  "Validate worktree remove COMMAND.
Requires name to be set."
  (with-slots (name) command
    (cond
     ((not name) "Worktree name is required")
     ((string-empty-p name) "Worktree name cannot be empty")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-worktree-info
;;; ============================================================

(beads-defcommand beads-command-worktree-info (beads-command-json)
  ()
  :documentation "Represents bd worktree info command.
Shows information about the current worktree context.")

(cl-defmethod beads-command-subcommand ((_command beads-command-worktree-info))
  "Return \"worktree info\" as the CLI subcommand."
  "worktree info")

(cl-defmethod beads-command-parse ((command beads-command-worktree-info))
  "Parse worktree info COMMAND output.
Returns beads-worktree-info instance."
  (with-slots (json) command
    (if (not json)
        (cl-call-next-method)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (beads-worktree-info-from-json parsed-json)
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-worktree-info: %s"
                                 (error-message-string err))
                         :exit-code (oref command exit-code)
                         :parsed-json parsed-json
                         :stderr (oref command stderr)
                         :parse-error err))))))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun beads-worktree-find-by-name (name)
  "Find a worktree by NAME from the list of worktrees.
Returns beads-worktree instance or nil if not found."
  (seq-find (lambda (wt)
              (string= (oref wt name) name))
            (beads-command-worktree-list!)))

(defun beads-worktree-find-by-branch (branch)
  "Find a worktree by BRANCH name from the list of worktrees.
Returns beads-worktree instance or nil if not found."
  (seq-find (lambda (wt)
              (string= (oref wt branch) branch))
            (beads-command-worktree-list!)))

(defun beads-worktree-main ()
  "Return the main worktree (the one with is-main = t).
Returns beads-worktree instance or nil if not found."
  (seq-find (lambda (wt)
              (oref wt is-main))
            (beads-command-worktree-list!)))

(provide 'beads-command-worktree)
;;; beads-command-worktree.el ends here
