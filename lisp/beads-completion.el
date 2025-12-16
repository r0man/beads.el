;;; beads-completion.el --- Completion support for beads -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides completion table, custom completion style, and
;; annotation functions for beads issue selection.
;;
;; Key features:
;; - Custom completion style `beads-issue-title' that matches both ID and title
;; - Text properties on candidates for title-aware matching
;; - Annotation function showing [P#] [type] status - Title
;; - Group function for sorting by status
;; - Works with all completion UIs (Vertico, Ivy, Helm, default)
;;
;; The candidates remain pure IDs, but title is stored as a text property
;; for matching.  This means history and saved completions remain clean.

;;; Code:

(require 'beads-command)

;;; Completion Cache

(defvar beads-completion--cache nil
  "Cache for issue list.  Format: (TIMESTAMP . ISSUES-LIST).")

(defvar beads-completion--cache-ttl 5
  "Time-to-live for completion cache in seconds.")

(defun beads-completion--get-cached-issues ()
  "Get cached issue list, refreshing if stale."
  (let ((now (float-time)))
    (when (or (null beads-completion--cache)
              (> (- now (car beads-completion--cache))
                 beads-completion--cache-ttl))
      (condition-case nil
          (setq beads-completion--cache
                (cons now (beads-command-list!)))
        (error (setq beads-completion--cache nil))))
    (cdr beads-completion--cache)))

(defun beads-completion-invalidate-cache ()
  "Invalidate the completion cache."
  (setq beads-completion--cache nil))

;;; Issue Completion Table

(defun beads-completion-issue-table ()
  "Return completion table for issue IDs with title-aware matching."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (category . beads-issue)
          (annotation-function . beads-completion--issue-annotate)
          (group-function . beads-completion--issue-group))
      (let ((issues (beads-completion--get-cached-issues)))
        (complete-with-action
         action
         (mapcar (lambda (i)
                   (propertize (oref i id)
                               'beads-title (oref i title)
                               'beads-issue i))
                 issues)
         string pred)))))

;;; Issue Annotation Function

(defun beads-completion--issue-annotate (candidate)
  "Annotate issue CANDIDATE with [P#] [type] status - title."
  (condition-case nil
      (let ((issue (get-text-property 0 'beads-issue candidate)))
        (when issue
          (let ((status (oref issue status))
                (title (oref issue title))
                (priority (oref issue priority))
                (type (oref issue issue-type)))
            (format " [P%s] [%s] %s - %s"
                    priority
                    (or type "task")
                    (propertize (upcase status)
                                'face (pcase status
                                        ("open" 'success)
                                        ("in_progress" 'warning)
                                        ("blocked" 'error)
                                        ("closed" 'shadow)
                                        (_ 'default)))
                    (beads-completion--truncate-string title 50)))))
    (error "")))

;;; Issue Group Function

(defun beads-completion--issue-group (candidate transform)
  "Group issue CANDIDATE by status.  If TRANSFORM is non-nil, return CANDIDATE."
  (if transform
      candidate
    (let ((issue (get-text-property 0 'beads-issue candidate)))
      (if issue
          (pcase (oref issue status)
            ("open" "Open")
            ("in_progress" "In Progress")
            ("blocked" "Blocked")
            ("closed" "Closed")
            (_ "Other"))
        "Other"))))

;;; Issue Completion Style

(defun beads-completion--issue-style-try (string table pred point)
  "Try completion of STRING with title-aware matching.
TABLE is the completion table, PRED is the predicate, POINT is the position.
Return nil if no matches, t if STRING is exact unique match, single match
string if only one match, or STRING itself if multiple matches."
  (let ((matches (beads-completion--issue-style-all string table pred point)))
    (cond
     ((null matches) nil)
     ((and (= (length matches) 1)
           (string= string (substring-no-properties (car matches))))
      t)
     ((= (length matches) 1)
      ;; Return plain string - properties cause issues with completion machinery
      (substring-no-properties (car matches)))
     (t string))))  ;; Multiple matches - return input unchanged

(defun beads-completion--issue-style-all (string table pred point)
  "Return issue completions matching STRING against ID or title.
TABLE is the completion table, PRED is the predicate, POINT is ignored."
  (ignore point)
  (let* ((all (all-completions "" table pred))
         (pattern (regexp-quote string))
         (case-fold-search t))
    ;; Return plain strings - keep properties only for filtering
    (mapcar #'substring-no-properties
            (seq-filter
             (lambda (candidate)
               (let ((title (get-text-property 0 'beads-title candidate)))
                 (or (string-match-p pattern candidate)
                     (and title (string-match-p pattern title)))))
             all))))

;;; Shared Utilities

(defun beads-completion--truncate-string (str max-len)
  "Truncate STR to MAX-LEN characters with ellipsis."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 3)) "...")
    (or str "")))

;;; Registration

;; Custom completion style for title-aware matching.
;; Temporarily disabled due to compatibility issues with completion machinery.
;; The basic style works fine for now.
;;
;; (unless (assq 'beads-issue-title completion-styles-alist)
;;   (add-to-list 'completion-styles-alist
;;                '(beads-issue-title
;;                  beads-completion--issue-style-try
;;                  beads-completion--issue-style-all
;;                  "Match beads issue by ID or title.")))
;;
;; (unless (assq 'beads-issue completion-category-overrides)
;;   (add-to-list 'completion-category-overrides
;;                '(beads-issue (styles beads-issue-title basic))))

;;; Completion-at-Point (CAPF) Support

(defun beads-completion-at-point ()
  "Completion-at-point function for beads issue IDs.
Detects partial issue ID at point and offers completions.
Triggers when there are 2+ characters starting with a letter."
  (let ((case-fold-search nil))
    (save-excursion
      ;; Move backward to find start of potential issue ID
      (skip-chars-backward "a-zA-Z0-9._-")
      (let ((start (point)))
        (skip-chars-forward "a-zA-Z0-9._-")
        (let* ((end (point))
               (len (- end start)))
          ;; Trigger with 2+ chars starting with a letter
          (when (and (>= len 2)
                     (save-excursion
                       (goto-char start)
                       (looking-at "[a-zA-Z]")))
            (list start end
                  (beads-completion-issue-table)
                  :exclusive 'no)))))))

(defun beads-completion--setup ()
  "Add beads CAPF to current buffer."
  (unless (memq #'beads-completion-at-point completion-at-point-functions)
    (setq-local completion-at-point-functions
                (cons #'beads-completion-at-point completion-at-point-functions))))

(defun beads-completion--teardown ()
  "Remove beads CAPF from current buffer."
  (setq-local completion-at-point-functions
              (remq #'beads-completion-at-point completion-at-point-functions)))

;;;###autoload
(define-minor-mode beads-completion-mode
  "Global minor mode for beads issue ID in-buffer completion.
When enabled, issue IDs can be completed in any buffer using
\\[completion-at-point] (typically M-TAB or C-M-i)."
  :global t
  :group 'beads
  :lighter nil
  (if beads-completion-mode
      (progn
        ;; Add to all existing buffers
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (beads-completion--setup)))
        ;; Add to future buffers
        (add-hook 'after-change-major-mode-hook #'beads-completion--setup))
    ;; Remove from all buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (beads-completion--teardown)))
    (remove-hook 'after-change-major-mode-hook #'beads-completion--setup)))

(provide 'beads-completion)
;;; beads-completion.el ends here
