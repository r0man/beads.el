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
;; ## Filter Menu
;;
;; - `beads-list-filter-menu' — transient filter menu for list buffers
;;   Pre-populates from the buffer's current `beads-list--spec'.
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
;;
;;   ;; Open filter menu (pre-populated from current buffer spec)
;;   (beads-list-filter-menu)

;;; Code:

(require 'eieio)
(require 'transient)
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
    (let ((issues (beads-command-execute cmd)))
      (setq beads-list--spec effective-spec)
      (beads-list--populate-buffer issues 'list cmd))))

;;; Transient Argument Conversion

(defun beads--transient-args-to-plist (args)
  "Convert transient ARGS to a property list.

ARGS is a list of strings as returned by `transient-args'.
Each \"--key=value\" becomes :key \"value\"; each \"--switch\"
becomes :switch t.  Hyphenated keys are preserved as hyphenated
keywords (e.g. \"--created-after=X\" → :created-after \"X\").

Returns nil for nil or empty ARGS."
  (let (plist)
    (dolist (arg args)
      (cond
       ;; --key=value
       ((string-match "\\`--\\([^=]+\\)=\\(.*\\)\\'" arg)
        (let ((key (intern (concat ":" (match-string 1 arg))))
              (val (match-string 2 arg)))
          (setq plist (plist-put plist key val))))
       ;; --switch (no =)
       ((string-match "\\`--\\(.+\\)\\'" arg)
        (let ((key (intern (concat ":" (match-string 1 arg)))))
          (setq plist (plist-put plist key t))))))
    plist))

(defun beads--transient-args-to-spec (args)
  "Convert transient ARGS to a `beads-issue-spec'.

ARGS is a list of strings as returned by `transient-args'.
Recognized keys: --status=, --type=, --priority=, --assignee=,
--label=, --order=, --limit=, --ready.
Unrecognized keys are ignored.  Unspecified fields receive their
class defaults."
  (let ((plist (beads--transient-args-to-plist args)))
    (beads-issue-spec
     :status (plist-get plist :status)
     :type (plist-get plist :type)
     :priority (when-let ((p (plist-get plist :priority)))
                 (string-to-number p))
     :assignee (plist-get plist :assignee)
     :label (plist-get plist :label)
     :order (if-let ((o (plist-get plist :order)))
                (intern o)
              'newest)
     :limit (if-let ((l (plist-get plist :limit)))
                (string-to-number l)
              50)
     :ready-only (and (plist-get plist :ready) t))))

;;; Filter Menu

;; The filter menu uses --filter-X= argument prefixes internally to
;; avoid collisions with the global --X= options used elsewhere.

(defun beads-list-filter--spec-to-args (spec)
  "Convert SPEC to transient argument strings for `beads-list-filter-menu'.

Returns a list of strings like (\"--filter-status=open\"
\"--filter-limit=50\") suitable for use as the initial transient
value.  Only non-nil/non-default fields are included."
  (let (args)
    (when-let ((s (oref spec status)))
      (push (format "--filter-status=%s" s) args))
    (when-let ((tp (oref spec type)))
      (push (format "--filter-type=%s" tp) args))
    (when-let ((p (oref spec priority)))
      (push (format "--filter-priority=%d" p) args))
    (when-let ((a (oref spec assignee)))
      (push (format "--filter-assignee=%s" a) args))
    (when-let ((l (oref spec label)))
      (push (format "--filter-label=%s" l) args))
    (unless (eq (oref spec order) 'newest)
      (push (format "--filter-order=%s" (oref spec order)) args))
    (push (format "--filter-limit=%d" (oref spec limit)) args)
    (when (oref spec ready-only)
      (push "--filter-ready-only" args))
    (nreverse args)))

(defun beads-list-filter--args-to-spec (args)
  "Parse transient ARGS into a `beads-issue-spec'.

ARGS is the list returned by `(transient-args \\='beads-list-filter-menu)'.
Unspecified fields receive their class defaults."
  (let ((status nil)
        (type nil)
        (priority nil)
        (assignee nil)
        (label nil)
        (order 'newest)
        (limit 50)
        (ready-only nil))
    (dolist (arg args)
      (cond
       ((string-prefix-p "--filter-status=" arg)
        (setq status (substring arg (length "--filter-status="))))
       ((string-prefix-p "--filter-type=" arg)
        (setq type (substring arg (length "--filter-type="))))
       ((string-prefix-p "--filter-priority=" arg)
        (setq priority
              (string-to-number (substring arg (length "--filter-priority=")))))
       ((string-prefix-p "--filter-assignee=" arg)
        (setq assignee (substring arg (length "--filter-assignee="))))
       ((string-prefix-p "--filter-label=" arg)
        (setq label (substring arg (length "--filter-label="))))
       ((string-prefix-p "--filter-order=" arg)
        (setq order
              (intern (substring arg (length "--filter-order=")))))
       ((string-prefix-p "--filter-limit=" arg)
        (setq limit
              (string-to-number (substring arg (length "--filter-limit=")))))
       ((string= arg "--filter-ready-only")
        (setq ready-only t))))
    (beads-issue-spec
     :status status
     :type type
     :priority priority
     :assignee assignee
     :label label
     :order order
     :limit limit
     :ready-only ready-only)))

;; Helper functions used by suffix commands to operate on a saved buffer.

(defun beads-list-filter--apply-with-spec (buf spec)
  "Refresh BUF using SPEC.
BUF must be a live buffer with `beads-list-mode' active."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (beads-list--refresh spec))))

(defun beads-list-filter--reset-buffer (buf)
  "Reset filters in BUF to `beads-list-default-spec'."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (beads-list--refresh beads-list-default-spec))))

;; Buffer saved before opening the transient so suffixes can refresh it.

(defvar beads-list-filter--buffer nil
  "The list buffer to refresh when the filter menu is applied or reset.")

;; Custom transient prefix class that initialises from the buffer spec.

(defclass beads-list-filter-prefix (transient-prefix) ()
  "Transient prefix class for `beads-list-filter-menu'.

Overrides `transient-init-value' to pre-populate the menu from the
calling buffer's `beads-list--spec'.")

(cl-defmethod transient-init-value ((obj beads-list-filter-prefix))
  "Initialise OBJ value from the current buffer's spec."
  (setq beads-list-filter--buffer (current-buffer))
  (let* ((spec (or (and (boundp 'beads-list--spec) beads-list--spec)
                   beads-list-default-spec)))
    (oset obj value (beads-list-filter--spec-to-args spec))))

;;; Infixes

(transient-define-infix beads-list-filter--status ()
  :class 'transient-option
  :description "Status"
  :argument "--filter-status="
  :choices '("open" "closed" "in_progress" "blocked" "deferred" "hooked"))

(transient-define-infix beads-list-filter--type ()
  :class 'transient-option
  :description "Type"
  :argument "--filter-type="
  :choices '("bug" "feature" "task" "epic" "chore"))

(transient-define-infix beads-list-filter--priority ()
  :class 'transient-option
  :description "Max priority (0-4)"
  :argument "--filter-priority="
  :choices '("0" "1" "2" "3" "4"))

(transient-define-infix beads-list-filter--assignee ()
  :class 'transient-option
  :description "Assignee"
  :argument "--filter-assignee=")

(transient-define-infix beads-list-filter--label ()
  :class 'transient-option
  :description "Label"
  :argument "--filter-label=")

(transient-define-infix beads-list-filter--order ()
  :class 'transient-option
  :description "Sort order"
  :argument "--filter-order="
  :choices '("newest" "oldest" "priority" "updated"))

(transient-define-infix beads-list-filter--limit ()
  :class 'transient-option
  :description "Result limit"
  :argument "--filter-limit="
  :reader (lambda (prompt _initial-input history)
            (number-to-string
             (read-number prompt 50 history))))

(transient-define-infix beads-list-filter--ready-only ()
  :class 'transient-switch
  :description "Ready issues only"
  :argument "--filter-ready-only")

;;; Suffixes

(transient-define-suffix beads-list-filter--apply ()
  "Apply filters and refresh the list buffer."
  :description "Apply filters"
  (interactive)
  (let* ((args (transient-args 'beads-list-filter-menu))
         (spec (beads-list-filter--args-to-spec args))
         (buf beads-list-filter--buffer))
    (beads-list-filter--apply-with-spec buf spec)))

(transient-define-suffix beads-list-filter--reset ()
  "Reset filters to `beads-list-default-spec' and refresh."
  :description "Reset to defaults"
  (interactive)
  (beads-list-filter--reset-buffer beads-list-filter--buffer))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-list-filter-menu "beads-spec" nil t)
(transient-define-prefix beads-list-filter-menu ()
  "Adjust filters for the current beads list buffer.

Pre-populates from the buffer's `beads-list--spec' (or
`beads-list-default-spec').  Press RET to apply the new filters
and refresh the list; press R to reset to defaults."
  :class beads-list-filter-prefix
  [["Filter by"
    ("s" "Status" beads-list-filter--status)
    ("t" "Type" beads-list-filter--type)
    ("p" "Priority" beads-list-filter--priority)
    ("a" "Assignee" beads-list-filter--assignee)
    ("l" "Label" beads-list-filter--label)]
   ["Options"
    ("r" "Ready only" beads-list-filter--ready-only)
    ("n" "Limit" beads-list-filter--limit)
    ("o" "Order" beads-list-filter--order)]
   ["Actions"
    ("RET" "Apply" beads-list-filter--apply :transient nil)
    ("R" "Reset to defaults" beads-list-filter--reset :transient nil)]])

(provide 'beads-spec)
;;; beads-spec.el ends here
