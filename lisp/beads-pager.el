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

;;; Page Size

(defun beads-pager--compute-page-size ()
  "Compute page size from current window body height.
Subtracts 3 to account for the tabulated-list header row, the
mode-line, and a small margin.  Minimum is 5."
  (max 5 (- (window-body-height) 3)))

(defun beads-pager--effective-page-size ()
  "Return the effective page size (explicit or computed from window)."
  (or beads-pager--page-size (beads-pager--compute-page-size)))

;;; Page Arithmetic

(defun beads-pager--total-pages ()
  "Return total number of pages for the current entry list."
  (let ((total (length beads-pager--all-entries))
        (ps (beads-pager--effective-page-size)))
    (max 1 (ceiling (float total) ps))))

(defun beads-pager--page-start ()
  "Return 0-based start index for the current page."
  (* (1- beads-pager--page) (beads-pager--effective-page-size)))

(defun beads-pager--page-end ()
  "Return 0-based exclusive end index for the current page."
  (min (+ (beads-pager--page-start) (beads-pager--effective-page-size))
       (length beads-pager--all-entries)))

;;; Total Count for Mode-Line

(defun beads-pager--total-count ()
  "Return total entry count regardless of current page.
When pager mode is active and all-entries is set, returns the full
count.  Otherwise returns `(length tabulated-list-entries)'."
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
          (seq-subseq beads-pager--all-entries
                      (beads-pager--page-start)
                      (beads-pager--page-end)))
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

(defun beads-pager--on-window-resize (frame)
  "Recompute and redisplay page when FRAME is resized.
Iterates over all windows in FRAME and refreshes any buffer
where `beads-pager-mode' is active."
  (dolist (window (window-list frame))
    (with-current-buffer (window-buffer window)
      (when (and (bound-and-true-p beads-pager-mode)
                 beads-pager--all-entries)
        (beads-pager--apply)))))

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
  "Minor mode for paginating tabulated-list-mode buffers.

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
