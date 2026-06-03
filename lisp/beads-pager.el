;;; beads-pager.el --- Pagination for tabulated-list-mode buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides `beads-pager-mode', a minor mode that adds
;; pagination to tabulated-list-mode buffers.  It works by storing
;; the complete entry list in a buffer-local variable and displaying
;; only the slice for the current page.
;;
;; Page size defaults to the number of rows that fit in the current
;; window (window-body-height minus header overhead).  It is
;; recomputed automatically when the frame is resized.
;;
;; Usage:
;;   (beads-pager-mode 1)          ; Enable in a tabulated-list buffer
;;   (beads-pager-set-entries entries)  ; Set entries (call instead of
;;                                      ; setting tabulated-list-entries)
;;
;; Keybindings (active when beads-pager-mode is on):
;;   ]   - Next page
;;   [   - Previous page
;;   G   - Go to page N (prompt)

;;; Code:

(require 'cl-lib)
(require 'seq)

;;; Buffer-Local State

(defvar-local beads-pager--all-entries nil
  "Complete list of tabulated-list entries before pagination.
When nil, no pagination is active (all entries fit on one page).")

(defvar-local beads-pager--page 1
  "Current 1-based page number.")

(defvar-local beads-pager--page-size nil
  "Entries per page.
When nil, computed dynamically from `window-body-height'.")

;;; Pure Pagination Core (public, buffer-agnostic)
;;
;; These four functions take all their inputs as arguments and read no
;; buffer-local state, so any tabulated-list (or other list) buffer can
;; reuse them as a single source of truth for page arithmetic.  The
;; buffer-stateful helpers below are re-implemented on top of them.

(defun beads-pager-window-page-size (&optional window)
  "Return the page size that fits WINDOW's body (selected window if nil).
Subtracts 3 to account for the tabulated-list header row, the
mode-line, and a small margin.  Minimum is 5."
  (max 5 (- (window-body-height window) 3)))

(defun beads-pager-page-count (total page-size)
  "Return the number of pages needed for TOTAL entries at PAGE-SIZE each.
Always at least 1, even when TOTAL is 0."
  (max 1 (ceiling (float total) page-size)))

(defun beads-pager-page-bounds (page page-size total)
  "Return (START . END), 0-based half-open indices, for 1-based PAGE.
START is the index of PAGE's first entry; END is the exclusive end.
Both are clamped to TOTAL, so a PAGE past the end yields an empty
range rather than an out-of-bounds index.  PAGE-SIZE is the number of
entries per page."
  (let* ((start (min (* (1- page) page-size) total))
         (end (min (+ start page-size) total)))
    (cons start end)))

(defun beads-pager-slice (entries page page-size)
  "Return the sublist of ENTRIES shown on 1-based PAGE with PAGE-SIZE each."
  (let ((bounds (beads-pager-page-bounds page page-size (length entries))))
    (seq-subseq entries (car bounds) (cdr bounds))))

;;; Buffer-Stateful Layer (private, built on the pure core above)

(defun beads-pager--effective-page-size ()
  "Return the effective page size (explicit or computed from window)."
  (or beads-pager--page-size (beads-pager-window-page-size)))

(defun beads-pager--total-pages ()
  "Return total number of pages for the current entry list."
  (beads-pager-page-count (length beads-pager--all-entries)
                          (beads-pager--effective-page-size)))

(defun beads-pager--page-start ()
  "Return 0-based start index for the current page."
  (car (beads-pager-page-bounds beads-pager--page
                                (beads-pager--effective-page-size)
                                (length beads-pager--all-entries))))

(defun beads-pager--page-end ()
  "Return 0-based exclusive end index for the current page."
  (cdr (beads-pager-page-bounds beads-pager--page
                                (beads-pager--effective-page-size)
                                (length beads-pager--all-entries))))

;;; Total Count for Mode-Line

(defun beads-pager--total-count ()
  "Return total entry count regardless of current page.
When pager mode is active and all-entries is set, returns the full
count.  Otherwise returns the length of `tabulated-list-entries'."
  (if (and (bound-and-true-p beads-pager-mode)
           beads-pager--all-entries)
      (length beads-pager--all-entries)
    (length tabulated-list-entries)))

;;; Core: Apply Current Page

(defun beads-pager--apply ()
  "Slice `beads-pager--all-entries' to the current page and redisplay.
Clamps `beads-pager--page' to the valid range before slicing."
  (when beads-pager--all-entries
    ;; Clamp page number
    (let ((total-pages (beads-pager--total-pages)))
      (setq beads-pager--page
            (max 1 (min beads-pager--page total-pages))))
    (setq tabulated-list-entries
          (beads-pager-slice beads-pager--all-entries
                             beads-pager--page
                             (beads-pager--effective-page-size)))
    (tabulated-list-print t)
    (force-mode-line-update)))

;;; Public API

(defun beads-pager-set-entries (entries)
  "Set all ENTRIES for pagination and display page 1.
Call this instead of setting `tabulated-list-entries' directly.
When `beads-pager-mode' is not active this falls back to setting
`tabulated-list-entries' and calling `tabulated-list-print'."
  (if (bound-and-true-p beads-pager-mode)
      (progn
        (setq beads-pager--all-entries entries
              beads-pager--page 1)
        (beads-pager--apply))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)))

;;; Navigation Commands

(defun beads-pager-next-page ()
  "Advance to the next page."
  (interactive)
  (if (< beads-pager--page (beads-pager--total-pages))
      (progn
        (cl-incf beads-pager--page)
        (beads-pager--apply)
        (goto-char (point-min)))
    (message "Already on last page (%d/%d)"
             beads-pager--page (beads-pager--total-pages))))

(defun beads-pager-prev-page ()
  "Go back to the previous page."
  (interactive)
  (if (> beads-pager--page 1)
      (progn
        (cl-decf beads-pager--page)
        (beads-pager--apply)
        (goto-char (point-min)))
    (message "Already on first page")))

(defun beads-pager-goto-page (n)
  "Go to page N (1-based), clamped to the valid range."
  (interactive "nGo to page: ")
  (setq beads-pager--page
        (max 1 (min n (beads-pager--total-pages))))
  (beads-pager--apply)
  (goto-char (point-min)))

;;; Mode-Line Fragment

(defun beads-pager--mode-line-fragment ()
  "Return a pagination status string for use in mode-line, or nil.
Returns nil when pager mode is off or there is only one page."
  (when (and (bound-and-true-p beads-pager-mode)
             beads-pager--all-entries
             (> (beads-pager--total-pages) 1))
    (let* ((total (length beads-pager--all-entries))
           (total-pages (beads-pager--total-pages))
           (start (1+ (beads-pager--page-start)))
           (end (beads-pager--page-end)))
      (format "  [Page %d/%d, items %d-%d of %d]"
              beads-pager--page total-pages start end total))))

;;; Window Resize Hook

(defun beads-pager--on-window-resize (window-or-frame)
  "Recompute and redisplay page after a resize event.
WINDOW-OR-FRAME is a window when called from a buffer-local hook,
or a frame when called from the global hook.  Buffer-local hooks on
`window-size-change-functions' receive the changed window, not
the frame.

For each affected window, sync buffer-point with the window's
`window-point' before re-paginating, then propagate the new
buffer-point back.  `tabulated-list-print' inside
`beads-pager--apply' calls `erase-buffer', which clamps the
`window-point' of any non-selected window to `(point-min)' and only
restores buffer-point — without this dance the cursor jumps to
the first row whenever the user pops up another buffer (e.g.
pressing RET on an issue in `beads-list')."
  (cl-flet ((apply-window
             (window)
             (when (window-live-p window)
               (with-current-buffer (window-buffer window)
                 (when (and (bound-and-true-p beads-pager-mode)
                            beads-pager--all-entries)
                   (goto-char (window-point window))
                   (beads-pager--apply)
                   (set-window-point window (point)))))))
    (if (windowp window-or-frame)
        (apply-window window-or-frame)
      (dolist (window (window-list window-or-frame))
        (apply-window window)))))

;;; Keymap

(defvar beads-pager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "]") #'beads-pager-next-page)
    (define-key map (kbd "[") #'beads-pager-prev-page)
    (define-key map (kbd "G") #'beads-pager-goto-page)
    map)
  "Keymap for `beads-pager-mode'.
Active in any tabulated-list buffer where pager mode is enabled.")

;;; Minor Mode

;;;###autoload
(define-minor-mode beads-pager-mode
  "Minor mode for paginating `tabulated-list-mode' buffers.

When enabled, use `beads-pager-set-entries' to populate the buffer
instead of setting `tabulated-list-entries' directly.  The page
size is computed from the window height and adjusts automatically
when the frame is resized.

\\{beads-pager-mode-map}"
  :lighter ""
  :keymap beads-pager-mode-map
  (if beads-pager-mode
      (add-hook 'window-size-change-functions
                #'beads-pager--on-window-resize nil t)
    (remove-hook 'window-size-change-functions
                 #'beads-pager--on-window-resize t)
    ;; Restore full entry list when mode is disabled
    (when beads-pager--all-entries
      (setq tabulated-list-entries beads-pager--all-entries
            beads-pager--all-entries nil)
      (tabulated-list-print t))))

(provide 'beads-pager)
;;; beads-pager.el ends here
