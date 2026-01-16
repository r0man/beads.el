;;; beads-command-comments.el --- Comments command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd comments' operations.
;; Comments allows viewing and adding comments on issues.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-comments (list)
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-comments (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to list comments for."
    :positional 1))
  :documentation "Represents bd comments command.
Lists all comments on an issue."))

(cl-defmethod beads-command-subcommand ((_command beads-command-comments))
  "Return \"comments\" as the CLI subcommand."
  "comments")

(cl-defmethod beads-command-validate ((command beads-command-comments))
  "Validate comments COMMAND.  Requires issue-id."
  (with-slots (issue-id) command
    (cond
     ((not issue-id) "Issue ID is required")
     ((string-empty-p issue-id) "Issue ID cannot be empty")
     (t nil))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-comments))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-comments-add
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-comments-add (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to add comment to."
    :positional 1)
   (text
    :initarg :text
    :type (or null string)
    :initform nil
    :documentation "Comment text."
    :positional 2)
   (file
    :initarg :file
    :type (or null string)
    :initform nil
    :documentation "Read comment from file."
    :long-option "file"
    :short-option "f"
    :option-type :string
    :transient-key "f"
    :transient-description "--file"
    :transient-class transient-option
    :transient-argument "--file="
    :transient-prompt "File path: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (author
    :initarg :author
    :type (or null string)
    :initform nil
    :documentation "Add author to comment."
    :long-option "author"
    :short-option "a"
    :option-type :string
    :transient-key "a"
    :transient-description "--author"
    :transient-class transient-option
    :transient-argument "--author="
    :transient-prompt "Author: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd comments add command.
Adds a comment to an issue."))

(cl-defmethod beads-command-subcommand ((_command beads-command-comments-add))
  "Return \"comments add\" as the CLI subcommand."
  "comments add")

(cl-defmethod beads-command-validate ((command beads-command-comments-add))
  "Validate comments add COMMAND.  Requires issue-id and text or file."
  (with-slots (issue-id text file) command
    (cond
     ((not issue-id) "Issue ID is required")
     ((string-empty-p issue-id) "Issue ID cannot be empty")
     ((and (not text) (not file)) "Comment text or file is required")
     (t nil))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-comments-add))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-comments "beads-command-comments" nil t)
(beads-meta-define-transient beads-command-comments "beads-comments"
  "List all comments on an issue.

Shows all comments with their timestamps and authors."
  beads-option-global-section)

;;;###autoload (autoload 'beads-comments-add "beads-command-comments" nil t)
(beads-meta-define-transient beads-command-comments-add "beads-comments-add"
  "Add a comment to an issue.

Comment can be provided as text argument or read from a file."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-comments-menu "beads-command-comments" nil t)
(transient-define-prefix beads-comments-menu ()
  "Manage issue comments."
  ["Comments Commands"
   ("l" "List comments" beads-comments)
   ("a" "Add comment" beads-comments-add)])

(provide 'beads-command-comments)
;;; beads-command-comments.el ends here
