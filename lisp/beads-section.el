;;; beads-section.el --- vui-based rendering for beads issue sections -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides vui.el-based UI infrastructure for beads.el.
;; It replaces the former magit-section-mode implementation with a
;; declarative, component-based approach using vui.el.
;;
;; ## Section Data Classes
;;
;; Pure EIEIO data containers (no magit-section inheritance):
;; - `beads-issues-section'  — container for a group of issues
;; - `beads-issue-section'   — a single issue (with `:issue' slot)
;; - `beads-blocked-section' — container for blocked issues
;; - `beads-ready-section'   — container for ready (unblocked) issues
;;
;; ## Context Detection
;;
;; Issue context is stored as a `beads-section' text property on each
;; rendered line, mirroring the pattern used in gastown-status-buffer.
;; `beads-section-issue-id-at-point' reads this property.
;;
;; ## Status Sections Hook
;;
;; `beads-status-sections-hook' lists functions that RETURN vui vnodes.
;; Each function returns a vnode or nil (when no data to show).
;;
;; ## Collapsible Section Component
;;
;; `beads-section--issue-group' is a `vui-defcomponent' with `:state'
;; that tracks expanded/collapsed state.  Clicking the section header
;; toggles visibility without a full re-fetch.
;;
;; ## Mode
;;
;; `beads-section-mode' is derived from `vui-mode'.  It inherits TAB
;; navigation from `widget-keymap' (through vui-mode).  RET on an
;; issue line triggers `beads-section-visit-issue'.
;;
;; ## Dependencies
;;
;; Requires the `vui' package (available from MELPA and Guix).

;;; Code:

(require 'eieio)
(require 'vui)
(require 'beads-command)
(require 'beads-command-blocked)
(require 'beads-command-list)
(require 'beads-command-ready)
(require 'beads-types)

;;; Forward Declarations

(declare-function beads-show "beads-command-show" (&optional issue-id))

;;; Section Data Classes

(defclass beads-issues-section ()
  nil
  "Data container for a group of open issues.")

(defclass beads-issue-section ()
  ((issue
    :initarg :issue
    :initform nil
    :documentation "The `beads-issue' object for this section line."))
  "Data container for a single beads issue.")

(defclass beads-blocked-section ()
  nil
  "Data container for blocked issues.")

(defclass beads-ready-section ()
  nil
  "Data container for ready (unblocked) issues.")

;;; Context Detection

(defun beads-section--propertize (str section)
  "Return STR with SECTION stored as the `beads-section' text property."
  (propertize str 'beads-section section))

(defun beads-section-issue-id-at-point ()
  "Return the issue ID at point via text property, or nil.
Reads the `beads-section' text property and extracts the issue ID
from a `beads-issue-section' data object."
  (when-let* ((sec (get-text-property (point) 'beads-section))
              (_ (object-of-class-p sec 'beads-issue-section))
              (issue (oref sec issue)))
    (oref issue id)))

;;; vui Components

(defun beads-section--issue-line-vnode (issue)
  "Return a vui button vnode for a single ISSUE.
The button displays the issue id, priority, type, status, and title.
Its label carries a `beads-section' text property for context
detection at point via `beads-section-issue-id-at-point'."
  (let* ((id       (or (oref issue id) ""))
         (title    (or (oref issue title) ""))
         (priority (oref issue priority))
         (type     (or (oref issue issue-type) ""))
         (status   (or (oref issue status) ""))
         (prio-str (if priority (format "P%d" priority) "--"))
         (label    (beads-section--propertize
                    (format "  %-14s %-4s %-10s %-14s %s"
                            id prio-str type status title)
                    (beads-issue-section :issue issue))))
    (vui-button label
      :no-decoration t
      :on-click (let ((issue-id id))
                  (lambda () (beads-show issue-id))))))

(vui-defcomponent beads-section--issue-group (title issues)
  "Collapsible vui component showing a group of ISSUES under TITLE.
Clicking the header toggles expanded/collapsed state.
When ISSUES is nil this component renders nothing."
  :state ((expanded t))
  :render
  (when issues
    (vui-vstack
     (vui-button
      (format "%s %s (%d)"
              (if expanded "▼" "▶")
              title
              (length issues))
      :no-decoration t
      :face 'bold
      :help-echo (if expanded
                     (format "Collapse %s" title)
                   (format "Expand %s" title))
      :on-click (lambda () (vui-set-state :expanded (not expanded))))
     (when expanded
       (apply #'vui-vstack
              (mapcar #'beads-section--issue-line-vnode issues))))))

;;; Mode

(defvar-keymap beads-section-mode-map
  :parent vui-mode-map
  "RET" #'beads-section-visit-issue)

(define-derived-mode beads-section-mode vui-mode "Beads"
  "Major mode for browsing beads issues using vui.el.

Provides collapsible section groups for issue categories with
keyboard navigation.  Sections are rendered by collecting vnodes
from `beads-status-sections-hook' and mounting them via vui.

Key bindings:
  TAB     — Move to next widget (button/field)
  S-TAB   — Move to previous widget
  RET     — Visit issue at point (on issue lines)"
  :interactive nil)

;;; Status Sections Hook

(defcustom beads-status-sections-hook
  '(beads-insert-open-issues
    beads-insert-blocked-issues
    beads-insert-ready-work)
  "Hook listing functions that return vui vnodes for beads status buffers.

Each function takes no arguments and returns a vui vnode or nil.
The return values are collected and assembled into the status buffer.

Default functions:
- `beads-insert-open-issues'    — collapsible group of open issues
- `beads-insert-blocked-issues' — collapsible group of blocked issues
- `beads-insert-ready-work'     — collapsible group of ready work"
  :group 'beads
  :type 'hook)

;;; Insert Functions (return vui vnodes)

(defun beads-insert-open-issues ()
  "Return a collapsible vui vnode for open issues, or nil when none.
Fetches issues via `bd list --status open --json' and sorts by
priority (lowest number first)."
  (let* ((cmd (beads-command-list :status "open" :json t))
         (issues (oref (beads-command-execute cmd) result)))
    (when issues
      (vui-component 'beads-section--issue-group
        :title "Open Issues"
        :issues (seq-sort-by (lambda (i) (or (oref i priority) 99))
                             #'< issues)))))

(defun beads-insert-blocked-issues ()
  "Return a collapsible vui vnode for blocked issues, or nil when none.
Fetches issues via `bd blocked --json'."
  (let* ((cmd (beads-command-blocked :json t))
         (issues (oref (beads-command-execute cmd) result)))
    (when issues
      (vui-component 'beads-section--issue-group
        :title "Blocked Issues"
        :issues issues))))

(defun beads-insert-ready-work ()
  "Return a collapsible vui vnode for ready work, or nil when none.
Fetches issues via `bd ready --json'."
  (let* ((cmd (beads-command-ready :json t))
         (issues (oref (beads-command-execute cmd) result)))
    (when issues
      (vui-component 'beads-section--issue-group
        :title "Ready Work"
        :issues issues))))

;;; Section Tree Builder

(defun beads-section-build-vnode ()
  "Build the complete section vnode tree from `beads-status-sections-hook'.
Calls each hook function, collects non-nil results, and assembles
them into a `vui-vstack' with spacing between sections."
  (let ((vnodes (delq nil (mapcar #'funcall beads-status-sections-hook))))
    (apply #'vui-vstack :spacing 1 vnodes)))

;;; Commands

;;;###autoload
(defun beads-section-visit-issue ()
  "Visit the beads issue at point.

Reads the issue id from the `beads-section' text property at point
and calls `beads-show'.  Does nothing when point is not on an issue
line with a `beads-issue-section' context."
  (interactive)
  (when-let* ((id (beads-section-issue-id-at-point)))
    (beads-show id)))

(provide 'beads-section)
;;; beads-section.el ends here
