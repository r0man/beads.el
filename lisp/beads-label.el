;;; beads-label.el --- Label management for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides label management functionality for beads.el,
;; including:
;; - Label fetching and caching
;; - Label completion for reader functions
;; - Label add/remove/list commands (transient menus)
;; - Integration with beads-list and beads-show buffers
;;
;; The module follows the pattern established in other beads modules
;; (beads-dep, beads-misc, etc.) with state variables, reader functions,
;; and transient-based command interfaces.
;;
;; Usage:
;;
;;   ;; Get label completion table
;;   (beads--label-completion-table)
;;
;;   ;; Invalidate cache after label changes
;;   (beads--invalidate-label-cache)
;;
;;   ;; Fetch all labels
;;   (beads-label-list-all)

;;; Code:

(require 'beads)

;;; Customization

(defgroup beads-label nil
  "Label management for Beads issue tracker."
  :group 'beads
  :prefix "beads-label-")

(defcustom beads-label-cache-ttl 300
  "Time-to-live for label cache in seconds.
Default is 300 seconds (5 minutes) since labels change less
frequently than issues."
  :type 'integer
  :group 'beads-label)

;;; Variables

(defvar beads--label-cache nil
  "Cache for label list used in completion.
Format: (TIMESTAMP . LABELS-LIST)")

;;; Label Fetching and Caching

(defun beads-label-list-all ()
  "Fetch all labels from bd label list-all.
Returns a list of label name strings."
  (let* ((json (beads--run-command "label" "list-all"))
         ;; JSON is array of {\"label\": \"name\", \"count\": N}
         (labels (append json nil)))
    (mapcar (lambda (entry) (alist-get 'label entry)) labels)))

(defun beads--get-cached-labels ()
  "Get cached label list, refreshing if stale.
Returns list of label name strings or nil on error."
  (let ((now (float-time)))
    (when (or (null beads--label-cache)
              (> (- now (car beads--label-cache))
                 beads-label-cache-ttl))
      (condition-case nil
          (setq beads--label-cache
                (cons now (beads-label-list-all)))
        (error
         (setq beads--label-cache nil))))
    (cdr beads--label-cache)))

(defun beads--invalidate-label-cache ()
  "Invalidate the label cache.
Call this after adding or removing labels."
  (setq beads--label-cache nil))

(defun beads--label-completion-table ()
  "Return completion table of unique label names.
Returns a simple list of label strings for use with `completing-read'."
  (let ((labels (beads--get-cached-labels)))
    ;; Remove duplicates (shouldn't be any from bd, but be safe)
    (delete-dups labels)))

(provide 'beads-label)
;;; beads-label.el ends here
