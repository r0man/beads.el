;;; beads-section.el --- magit-section-mode base and section hooks -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides magit-section-mode infrastructure for beads.el.
;; It defines `beads-section-mode' (derived from `magit-section-mode'),
;; EIEIO section types for issues and issue groups, a status section hook,
;; and insert functions that populate beads status buffers.
;;
;; ## Section Types
;;
;; - `beads-issues-section'  — container for a group of issues
;; - `beads-issue-section'   — a single issue (with `:issue' slot)
;; - `beads-blocked-section' — container for blocked issues
;; - `beads-ready-section'   — container for ready (unblocked) issues
;;
;; ## Status Sections Hook
;;
;; `beads-status-sections-hook' lists insert functions called when
;; refreshing a status buffer.  The default value inserts open issues,
;; blocked issues, and ready work.  Add custom sections with
;; `magit-add-section-hook'.
;;
;; ## Insert Functions
;;
;; - `beads-insert-open-issues'    — fetch open issues; insert sorted by priority
;; - `beads-insert-blocked-issues' — fetch and insert blocked issues
;; - `beads-insert-ready-work'     — fetch and insert ready (unblocked) issues
;; - `beads--insert-issue-line'    — render one issue as a section line
;;
;; ## Navigation
;;
;; `beads-section-mode' inherits n/p navigation from `magit-section-mode'.
;; RET on an issue section calls `beads-show' via `beads-section-visit-issue'.
;;
;; ## Dependencies
;;
;; Requires the `magit-section' package (part of `emacs-magit' in Guix).

;;; Code:

(require 'eieio)
(require 'magit-section)
(require 'beads-command)
(require 'beads-command-blocked)
(require 'beads-command-list)
(require 'beads-command-ready)
(require 'beads-types)

;;; Forward Declarations

(declare-function beads-show "beads-command-show" (&optional issue-id))

;;; Section Classes

(defclass beads-issues-section (magit-section)
  ()
  "Section containing a group of open issues.")

(defclass beads-issue-section (magit-section)
  ((keymap
    :initform 'beads-section-issue-map)
   (issue
    :initarg :issue
    :initform nil
    :documentation "The `beads-issue' object for this section line."))
  "Section representing a single beads issue.
The `keymap' slot defaults to `beads-section-issue-map' so that RET
on any issue line calls `beads-section-visit-issue'.")

(defclass beads-blocked-section (magit-section)
  ()
  "Section containing blocked issues.")

(defclass beads-ready-section (magit-section)
  ()
  "Section containing ready (unblocked) issues.")

;;; Section Keymaps

(defvar beads-section-issue-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'beads-section-visit-issue)
    map)
  "Keymap active on individual issue section lines.")

;;; Mode

(define-derived-mode beads-section-mode magit-section-mode "Beads"
  "Major mode for browsing beads issues using magit-section.

Provides collapsible section groups for issue categories with
keyboard navigation.  Sections are populated by running the
functions in `beads-status-sections-hook'.

Key bindings (inherited from `magit-section-mode'):
  TAB       — Toggle section visibility
  n / p     — Move to next / previous section
  RET       — Visit issue at point (on issue lines)"
  :interactive nil)

;;; Status Sections Hook

(defcustom beads-status-sections-hook
  '(beads-insert-open-issues
    beads-insert-blocked-issues
    beads-insert-ready-work)
  "Hook listing functions that insert sections into a beads status buffer.

Each function takes no arguments and inserts one or more sections at
point.  Use `magit-add-section-hook' to add custom insert functions
relative to existing ones.

Default functions:
- `beads-insert-open-issues'    — collapsible group of open issues
- `beads-insert-blocked-issues' — collapsible group of blocked issues
- `beads-insert-ready-work'     — collapsible group of ready work"
  :group 'beads
  :type 'hook)

;;; Insert Functions

(defun beads-insert-open-issues ()
  "Insert a collapsible section with open issues grouped by priority.

Fetches issues via `bd list --status open --json' and renders them
sorted by priority (lowest number first).  Does nothing when there
are no open issues."
  (let* ((cmd (beads-command-list :status "open" :json t))
         (issues (oref (beads-command-execute cmd) result)))
    (when issues
      (magit-insert-section (beads-issues-section)
        (magit-insert-heading "Open Issues")
        (dolist (issue (seq-sort-by
                        (lambda (i) (or (oref i priority) 99))
                        #'< issues))
          (beads--insert-issue-line issue))))))

(defun beads-insert-blocked-issues ()
  "Insert a collapsible section with blocked issues.

Fetches issues via `bd blocked --json'.  Does nothing when there are
no blocked issues."
  (let* ((cmd (beads-command-blocked :json t))
         (issues (oref (beads-command-execute cmd) result)))
    (when issues
      (magit-insert-section (beads-blocked-section)
        (magit-insert-heading "Blocked Issues")
        (dolist (issue issues)
          (beads--insert-issue-line issue))))))

(defun beads-insert-ready-work ()
  "Insert a collapsible section with ready (unblocked) issues.

Fetches issues via `bd ready --json'.  Does nothing when there is no
ready work."
  (let* ((cmd (beads-command-ready :json t))
         (issues (oref (beads-command-execute cmd) result)))
    (when issues
      (magit-insert-section (beads-ready-section)
        (magit-insert-heading "Ready Work")
        (dolist (issue issues)
          (beads--insert-issue-line issue))))))

(defun beads--insert-issue-line (issue)
  "Insert ISSUE as a single-line `beads-issue-section'.

Renders a line containing the issue id, priority indicator (P0–P4
or --), type, status, and title.  The section keymap enables RET to
visit the issue."
  (let* ((id       (or (oref issue id) ""))
         (title    (or (oref issue title) ""))
         (priority (oref issue priority))
         (type     (or (oref issue issue-type) ""))
         (status   (or (oref issue status) ""))
         (prio-str (if priority (format "P%d" priority) "--")))
    (magit-insert-section section (beads-issue-section)
      (oset section issue issue)
      (insert (format "  %-14s %-4s %-10s %-14s %s\n"
                      id prio-str type status title)))))

;;; Commands

;;;###autoload
(defun beads-section-visit-issue ()
  "Visit the beads issue at point.

Reads the issue id from the `beads-issue-section' at point and calls
`beads-show'.  Does nothing when point is not on an issue section."
  (interactive)
  (when-let* ((section (magit-current-section))
              (_ (object-of-class-p section 'beads-issue-section))
              (issue (oref section issue))
              (id (oref issue id)))
    (beads-show id)))

(provide 'beads-section)
;;; beads-section.el ends here
