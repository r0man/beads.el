;;; beads-spec.el --- Filter spec objects for beads list views -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO filter spec objects for beads list views,
;; following the pattern of Forge's `forge--topics-spec'.
;;
;; ## Main Types
;;
;; - `beads-issue-spec' — captures filter/sort/limit criteria for list queries
;;
;; ## Conversion
;;
;; - `beads-issue-spec--to-args' — converts a spec to bd CLI argument strings
;;
;; ## Customization
;;
;; - `beads-list-default-spec' — default spec applied to new list buffers
;;   (status=open, order=newest, limit=50)
;;
;; ## Per-Buffer State
;;
;; - `beads-list--spec' — buffer-local spec for the current list buffer
;;   When nil, `beads-list-default-spec' is used.
;;
;; ## Refresh
;;
;; - `beads-list--refresh' — fetch issues using a spec and populate buffer
;;
;; ## Usage
;;
;;   ;; Create a spec for high-priority open bugs
;;   (let ((spec (beads-issue-spec :status "open" :type "bug" :priority 0)))
;;     (beads-issue-spec--to-args spec))
;;   ;; => ("--status=open" "--type=bug" "--priority=0"
;;   ;;     "--sort=created" "--reverse" "--limit=50")
;;
;;   ;; Refresh the current list buffer with a custom spec
;;   (beads-list--refresh (beads-issue-spec :status "open" :order 'priority))

;;; Code:

(require 'eieio)
(require 'beads-custom)
(require 'beads-command)
(require 'beads-command-list)

;;; beads-issue-spec Class

(defclass beads-issue-spec ()
  ((status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status: open, closed, in_progress, blocked, deferred.
Nil means no status filter.")
   (type
    :initarg :type
    :type (or null string)
    :initform nil
    :documentation "Filter by type: bug, feature, task, epic, chore.
Nil means no type filter.")
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Filter by priority: 0 (highest) to 4 (lowest).
Nil means no priority filter.")
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee name.
Nil means no assignee filter.")
   (label
    :initarg :label
    :type (or null string)
    :initform nil
    :documentation "Filter by label.
Nil means no label filter.")
   (order
    :initarg :order
    :type symbol
    :initform 'newest
    :documentation "Sort order.  One of:
- `newest'   — newest first (--sort=created --reverse)
- `oldest'   — oldest first (--sort=created)
- `priority' — by priority ascending (--sort=priority)
- `updated'  — most recently updated first (--sort=updated)")
   (limit
    :initarg :limit
    :type integer
    :initform 50
    :documentation "Maximum number of results to return (--limit).
Default 50; use 0 for unlimited.")
   (ready-only
    :initarg :ready-only
    :type boolean
    :initform nil
    :documentation "When non-nil, show only ready (unblocked) issues (--ready)."))
  "Filter spec for a beads list view.

Captures all criteria needed to construct a `bd list' command:
status, type, priority, assignee, label, sort order, result limit,
and whether to restrict to ready-only issues.

Use `beads-issue-spec--to-args' to convert a spec to CLI argument
strings, or pass a spec to `beads-list--refresh' to populate the
current list buffer.")

;;; Conversion

(defun beads-issue-spec--to-args (spec)
  "Convert SPEC to a list of bd CLI argument strings.

SPEC is a `beads-issue-spec' instance.  Returns a list of strings
suitable for appending to a bd list command, for example:

  (\"--status=open\" \"--sort=created\" \"--reverse\" \"--limit=50\")

The `order' slot maps to --sort / --reverse flags:
  newest   → --sort=created --reverse
  oldest   → --sort=created
  priority → --sort=priority
  updated  → --sort=updated"
  (let (args)
    ;; Status filter
    (when-let ((s (oref spec status)))
      (push (format "--status=%s" s) args))
    ;; Type filter
    (when-let ((tp (oref spec type)))
      (push (format "--type=%s" tp) args))
    ;; Priority filter
    (when-let ((p (oref spec priority)))
      (push (format "--priority=%d" p) args))
    ;; Assignee filter
    (when-let ((a (oref spec assignee)))
      (push (format "--assignee=%s" a) args))
    ;; Label filter
    (when-let ((l (oref spec label)))
      (push (format "--label=%s" l) args))
    ;; Order → sort/reverse
    (pcase (oref spec order)
      ('newest
       (push "--sort=created" args)
       (push "--reverse" args))
      ('oldest
       (push "--sort=created" args))
      ('priority
       (push "--sort=priority" args))
      ('updated
       (push "--sort=updated" args)))
    ;; Limit
    (push (format "--limit=%d" (oref spec limit)) args)
    ;; Ready-only
    (when (oref spec ready-only)
      (push "--ready" args))
    (nreverse args)))

;;; Customization

(defcustom beads-list-default-spec
  (beads-issue-spec :status "open" :order 'newest :limit 50)
  "Default filter spec applied to new beads list buffers.

A `beads-issue-spec' instance.  When a list buffer is opened without
an explicit spec, this value is used.  The default shows open issues
sorted by newest first, limited to 50 results."
  :type '(restricted-sexp :match-alternatives (beads-issue-spec-p))
  :group 'beads)

;;; Per-Buffer State

(defvar-local beads-list--spec nil
  "Buffer-local filter spec for the current beads list buffer.

A `beads-issue-spec' instance or nil.  When nil,
`beads-list-default-spec' is used by `beads-list--refresh'.
Set by `beads-list--refresh' after each fetch.")

;;; Refresh

(defun beads-list--refresh (&optional spec)
  "Fetch issues using SPEC and populate the current beads list buffer.

SPEC is a `beads-issue-spec' instance.  When nil, falls back to the
buffer-local `beads-list--spec', then to `beads-list-default-spec'.

Builds a `beads-command-list' from the effective spec, executes it,
stores the spec in `beads-list--spec', and calls
`beads-list--populate-buffer' with the resulting issues."
  (let* ((effective-spec (or spec beads-list--spec beads-list-default-spec))
         (cmd (beads-command-list :json t)))
    ;; Apply spec slots to the command object
    (when-let ((s (oref effective-spec status)))
      (oset cmd status s))
    (when-let ((tp (oref effective-spec type)))
      (oset cmd issue-type tp))
    (when-let ((p (oref effective-spec priority)))
      (oset cmd priority (number-to-string p)))
    (when-let ((a (oref effective-spec assignee)))
      (oset cmd assignee a))
    (when-let ((l (oref effective-spec label)))
      (oset cmd label (list l)))
    ;; Order → sort/reverse
    (pcase (oref effective-spec order)
      ('newest
       (oset cmd sort "created")
       (oset cmd reverse t))
      ('oldest
       (oset cmd sort "created"))
      ('priority
       (oset cmd sort "priority"))
      ('updated
       (oset cmd sort "updated")))
    ;; Limit
    (oset cmd limit (oref effective-spec limit))
    ;; Ready-only
    (when (oref effective-spec ready-only)
      (oset cmd ready t))
    ;; Execute and populate
    (let ((issues (oref (beads-command-execute cmd) result)))
      (setq beads-list--spec effective-spec)
      (beads-list--populate-buffer issues 'list cmd))))

(provide 'beads-spec)
;;; beads-spec.el ends here
