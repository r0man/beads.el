;;; beads-command-comments.el --- Comments command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd comments' operations.
;; Comments allows viewing and adding comments on issues.
;;
;; bd comments - List all comments on an issue
;; bd comments add - Add a comment to an issue
;;
;; Usage:
;;   (beads-command-comments! :issue-id "bd-1")           ; List comments
;;   (beads-command-comments-add! :issue-id "bd-1"
;;                                :text "Comment text")  ; Add comment

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'transient)

;; Forward declarations
(declare-function beads--invalidate-completion-cache "beads")
(declare-function beads-reader-issue-id "beads-reader")
(defvar beads-auto-refresh)

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
      ;; CLI properties
      :positional 1
      :option-type :string
      ;; Transient properties
      :key "i"
      :transient "Issue ID (required)"
      :class transient-option
      :argument "--issue-id="
      :prompt "Issue ID: "
      :transient-reader beads-reader-issue-id
      :transient-group "List Comments"
      :level 1
      :order 1
      ;; Validation
      :required t))
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
      ;; CLI properties
      :positional 1
      :option-type :string
      ;; Transient properties
      :key "i"
      :transient "Issue ID (required)"
      :class transient-option
      :argument "--issue-id="
      :prompt "Issue ID: "
      :transient-reader beads-reader-issue-id
      :transient-group "Add Comment"
      :level 1
      :order 1
      ;; Validation
      :required t)
     (text
      :initarg :text
      :type (or null string)
      :initform nil
      :documentation "Comment text."
      ;; CLI properties
      :positional 2
      :option-type :string
      ;; Transient properties
      :key "t"
      :transient "Comment text"
      :class beads-create-transient-multiline
      :argument "--text="
      :field-name "Comment"
      :transient-group "Add Comment"
      :level 1
      :order 2)
     (file
      :initarg :file
      :type (or null string)
      :initform nil
      :documentation "Read comment from file."
      ;; CLI properties
      :long-option "file"
      :short-option "f"
      :option-type :string
      ;; Transient properties
      :key "f"
      :transient "--file"
      :class transient-option
      :argument "--file="
      :prompt "File path: "
      :transient-group "Add Comment"
      :level 2
      :order 3)
     (author
      :initarg :author
      :type (or null string)
      :initform nil
      :documentation "Add author to comment."
      ;; CLI properties
      :long-option "author"
      :short-option "a"
      :option-type :string
      ;; Transient properties
      :key "a"
      :transient "--author"
      :class transient-option
      :argument "--author="
      :prompt "Author: "
      :transient-group "Add Comment"
      :level 2
      :order 4))
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
     ((and (or (not text) (string-empty-p text))
           (or (not file) (string-empty-p file)))
      "Comment text or file is required")
     (t nil))))

(cl-defmethod beads-command-execute-interactive
    ((cmd beads-command-comments-add))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  ;; Invalidate cache since comment may affect issue display
  (beads--invalidate-completion-cache)
  (cl-call-next-method))

;;; ============================================================
;;; Transient Menus
;;; ============================================================

;;;###autoload (autoload 'beads-comments "beads-command-comments" nil t)
(beads-meta-define-transient beads-command-comments "beads-comments"
  "List all comments on an issue.

Shows all comments with their timestamps and authors.
Requires an issue ID to be specified."
  beads-option-global-section)

;;;###autoload (autoload 'beads-comments-add "beads-command-comments" nil t)
(beads-meta-define-transient beads-command-comments-add "beads-comments-add"
  "Add a comment to an issue.

Comment can be provided as text directly or read from a file.
The author can be overridden with the --author flag.

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Issue ID, comment text
  Level 2: File, author"
  beads-option-global-section)

;;; ============================================================
;;; Parent Transient Menu
;;; ============================================================

;;;###autoload (autoload 'beads-comments-menu "beads-command-comments" nil t)
(transient-define-prefix beads-comments-menu ()
  "Manage issue comments.

Comments allow adding notes, updates, and discussions to issues.
Each comment includes a timestamp and optional author attribution."
  ["Comments Commands"
   ("l" "List comments" beads-comments)
   ("a" "Add comment" beads-comments-add)]
  ["Quick Actions"
   ("q" "Quit" transient-quit-one)])

(provide 'beads-command-comments)
;;; beads-command-comments.el ends here
