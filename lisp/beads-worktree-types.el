;;; beads-worktree-types.el --- Domain types for worktree commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines domain types for git worktree operations.
;; These types are separated from beads-command-worktree.el to avoid
;; circular dependencies when other modules need to use them.
;;
;; Domain types:
;; - beads-worktree: Represents a git worktree with beads state
;; - beads-worktree-info: Information about current worktree context

;;; Code:

(require 'eieio)

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

(provide 'beads-worktree-types)
;;; beads-worktree-types.el ends here
