;;; beads-eldoc.el --- Eldoc support for Beads issue references -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;;; Commentary:

;; Provides eldoc integration for Beads issue references.
;;
;; When the cursor is positioned on a beads issue reference (like
;; beads.el-7bea or bd-a1b2), eldoc will display:
;; - Issue title and status in the echo area
;; - Full issue metadata in the eldoc buffer
;;
;; This works in all major modes: code comments, org files, markdown,
;; etc.
;;
;; Usage:
;;
;;   (beads-eldoc-mode 1)  ; Enable globally
;;
;; The issue data is cached for performance and automatically
;; invalidated when issues are modified.

;;; Code:

(require 'beads)
(require 'eldoc)

;;; Customization

(defgroup beads-eldoc nil
  "Eldoc support for Beads issue references."
  :group 'beads
  :prefix "beads-eldoc-")

(defcustom beads-eldoc-cache-ttl 300
  "Time-to-live for eldoc issue cache in seconds.
Default is 5 minutes (300 seconds)."
  :type 'integer
  :group 'beads-eldoc)

(defcustom beads-eldoc-issue-pattern
  "\\b\\([a-zA-Z][a-zA-Z0-9._-]*-[0-9a-fA-F]+\\(?:\\.[0-9]+\\)*\\)\\b"
  "Regular expression pattern for matching beads issue references.
Matches issue IDs in the format PROJECT-HASH[.CHILD], where PROJECT can
contain letters, numbers, dots, underscores, and hyphens, HASH is a
hexadecimal string (4-8 characters), and optional .CHILD for hierarchical
child IDs (can be nested like .1.2.3).
Examples: beads.el-7bea, bd-a1b2.1, worker-f14c.2.3, api-3e7a."
  :type 'string
  :group 'beads-eldoc)

;;; Variables

(defvar beads-eldoc--cache (make-hash-table :test 'equal)
  "Cache of issue IDs to issue data.
Format: Hash table with keys as issue-id strings and values as
plists (:timestamp FLOAT :issue ALIST).")

;;; Cache Management

(defun beads-eldoc--get-cached-issue (issue-id)
  "Get cached issue data for ISSUE-ID.
Returns the issue alist if cached and not stale, nil otherwise."
  (when-let* ((entry (gethash issue-id beads-eldoc--cache))
              (timestamp (plist-get entry :timestamp))
              (issue (plist-get entry :issue)))
    (if (< (- (float-time) timestamp) beads-eldoc-cache-ttl)
        issue
      ;; Cache entry is stale, remove it
      (remhash issue-id beads-eldoc--cache)
      nil)))

(defun beads-eldoc--cache-issue (issue-id issue)
  "Cache ISSUE data for ISSUE-ID."
  (puthash issue-id
           (list :timestamp (float-time)
                 :issue issue)
           beads-eldoc--cache))

(defun beads-eldoc--invalidate-cache (&optional issue-id)
  "Invalidate eldoc cache.
If ISSUE-ID is provided, only invalidate that entry.
Otherwise, clear the entire cache."
  (if issue-id
      (remhash issue-id beads-eldoc--cache)
    (clrhash beads-eldoc--cache)))

;;; Issue Reference Detection

(defun beads-eldoc--issue-id-at-point ()
  "Return the beads issue ID at point, or nil if none.
First checks for button properties (from beads-show buffers),
then falls back to pattern matching.
Recognizes issue IDs like beads.el-7bea, bd-a1b2.1, worker-f14c.2, etc."
  (or
   ;; First try button property (works in beads-show buffers)
   (when-let* ((button (button-at (point))))
     (button-get button 'issue-id))

   ;; Fall back to pattern matching (works in any text)
   (save-excursion
     (let ((case-fold-search nil))
       ;; Move to beginning of potential issue ID
       (skip-chars-backward "a-zA-Z0-9._-")
       (when (looking-at beads-eldoc-issue-pattern)
         (match-string 1))))))

;;; Issue Fetching

(defun beads-eldoc--fetch-issue (issue-id)
  "Fetch issue data for ISSUE-ID.
Returns issue alist or nil on error.
Results are cached for performance."
  (or (beads-eldoc--get-cached-issue issue-id)
      (condition-case err
          (let ((issue (beads--parse-issue
                        (beads--run-command "show" issue-id))))
            (beads-eldoc--cache-issue issue-id issue)
            issue)
        (error
         ;; Log error but don't show to user (eldoc should be silent)
         (beads--log 'verbose "Failed to fetch issue %s: %s"
                     issue-id (error-message-string err))
         nil))))

;;; Eldoc Documentation Function

(defun beads-eldoc--format-echo-area (issue)
  "Format ISSUE for display in echo area.
Returns a short string with issue title and status."
  (let ((id (alist-get 'id issue))
        (title (alist-get 'title issue))
        (status (alist-get 'status issue)))
    (format "%s [%s]: %s" id status title)))

(defun beads-eldoc--format-doc-buffer (issue)
  "Format ISSUE for display in eldoc documentation buffer.
Returns a detailed string with all issue metadata."
  (let ((id (alist-get 'id issue))
        (title (alist-get 'title issue))
        (description (alist-get 'description issue))
        (status (alist-get 'status issue))
        (priority (alist-get 'priority issue))
        (issue-type (alist-get 'issue-type issue))
        (created-at (alist-get 'created-at issue))
        (updated-at (alist-get 'updated-at issue))
        (assignee (alist-get 'assignee issue))
        (notes (alist-get 'notes issue)))
    (concat
     (format "Issue: %s\n" id)
     (format "Title: %s\n" title)
     (format "Status: %s\n" status)
     (format "Type: %s  Priority: %s\n" issue-type priority)
     (when assignee (format "Assignee: %s\n" assignee))
     (when created-at (format "Created: %s\n" created-at))
     (when updated-at (format "Updated: %s\n" updated-at))
     (when (and description (not (string-empty-p description)))
       (format "\nDescription:\n%s\n" description))
     (when (and notes (not (string-empty-p notes)))
       (format "\nNotes:\n%s\n" notes)))))

(defun beads-eldoc-function (callback &rest _)
  "Eldoc documentation function for beads issue references.
CALLBACK is called with documentation string if available.
This function is designed for `eldoc-documentation-functions'."
  (when-let* ((issue-id (beads-eldoc--issue-id-at-point))
              (issue (beads-eldoc--fetch-issue issue-id)))
    (funcall callback
             (beads-eldoc--format-echo-area issue)
             :thing issue-id
             :face 'font-lock-constant-face
             :echo (beads-eldoc--format-echo-area issue)
             :buffer (beads-eldoc--format-doc-buffer issue))))

;;; Minor Mode

;;;###autoload
(define-minor-mode beads-eldoc-mode
  "Global minor mode to enable eldoc support for beads issue references.
When enabled, moving the cursor over a beads issue reference
\(like beads.el-7bea, bd-a1b2.1, or bd-f14c.2.3) will display issue
information in the echo area and eldoc buffer."
  :global t
  :group 'beads-eldoc
  :lighter nil
  (if beads-eldoc-mode
      (progn
        ;; Add our eldoc function to the documentation functions
        (add-hook 'eldoc-documentation-functions
                  #'beads-eldoc-function nil nil)
        ;; Invalidate cache when issues are modified
        (advice-add 'beads--invalidate-completion-cache
                    :after #'beads-eldoc--invalidate-cache))
    ;; Disable mode
    (remove-hook 'eldoc-documentation-functions
                 #'beads-eldoc-function)
    (advice-remove 'beads--invalidate-completion-cache
                   #'beads-eldoc--invalidate-cache)
    ;; Clear cache
    (beads-eldoc--invalidate-cache)))

;;; Autoload

;;;###autoload
(autoload 'beads-eldoc-mode "beads-eldoc"
  "Toggle eldoc support for beads issue references." t)

(provide 'beads-eldoc)

;;; beads-eldoc.el ends here
