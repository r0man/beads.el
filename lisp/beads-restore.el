;;; beads-restore.el --- Restore compacted issues from git history -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Provides interface for restoring full history of compacted issues.
;; The bd restore command reads the compacted_at_commit from the
;; database, temporarily checks out that git commit, reads the full
;; issue from JSONL at that point in history, and displays it.
;;
;; This is a read-only operation that does not modify database or git
;; state.
;;
;; Usage:
;;   M-x beads-restore RET bd-N RET
;;
;; The command will:
;; - Check if the issue is compacted
;; - Error if issue is not compacted
;; - Display restored issue in *beads-restore: bd-N* buffer
;; - Show full historical content with a header banner
;; - Context-aware: works from show buffers

;;; Code:

(require 'beads)

;;; Forward declarations
(declare-function beads-show--issue-id "beads-show")

;;; Customization

(defgroup beads-restore nil
  "Restore compacted issue history settings."
  :group 'beads
  :prefix "beads-restore-")

;;; Faces

(defface beads-restore-header-banner-face
  '((t :inherit warning :weight bold :height 1.2))
  "Face for historical view banner in restore buffer."
  :group 'beads-restore)

(defface beads-restore-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headers in restore buffer."
  :group 'beads-restore)

(defface beads-restore-label-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for metadata labels in restore buffer."
  :group 'beads-restore)

(defface beads-restore-value-face
  '((t :inherit default))
  "Face for metadata values in restore buffer."
  :group 'beads-restore)

;;; Variables

(defvar-local beads-restore--issue-id nil
  "Issue ID displayed in current restore buffer.")

(defvar-local beads-restore--issue-data nil
  "Full restored issue data for current restore buffer.")

;;; Utility Functions

(defun beads-restore--detect-issue-id ()
  "Detect issue ID from current context.
Returns issue ID string or nil if not found."
  (or
   ;; From beads-show buffer variable
   (when (boundp 'beads-show--issue-id)
     beads-show--issue-id)
   ;; From buffer name (*beads-show: bd-N* or *beads-restore: bd-N*)
   (when (string-match "\\*beads-\\(?:show\\|restore\\): \\([a-z]+-[0-9]+\\)\\*"
                       (buffer-name))
     (match-string 1 (buffer-name)))))

(defun beads-restore--insert-header (label value &optional value-face)
  "Insert a metadata header line with LABEL and VALUE.
Optional VALUE-FACE can be used for custom face."
  (insert (propertize label 'face 'beads-restore-label-face))
  (insert ": ")
  (when value
    (insert (propertize (format "%s" value)
                       'face (or value-face
                                 'beads-restore-value-face))))
  (insert "\n"))

(defun beads-restore--insert-section (title content)
  "Insert a section with TITLE and CONTENT.
CONTENT can be a string or nil (empty sections are skipped)."
  (when (and content (not (string-empty-p (string-trim content))))
    (insert "\n\n")
    (insert (propertize title 'face 'beads-restore-header-face))
    (insert "\n")
    (insert (propertize (make-string (length title) ?─)
                       'face 'beads-restore-header-face))
    (insert "\n\n")
    (insert content)
    (insert "\n")))

(defun beads-restore--format-date (date-string)
  "Format DATE-STRING for display."
  (if date-string
      (let* ((cleaned (replace-regexp-in-string "\\.[0-9]+.*$" ""
                                                date-string))
             (without-z (replace-regexp-in-string "Z$" "" cleaned)))
        (replace-regexp-in-string "T" " " without-z))
    "N/A"))

(defun beads-restore--render-issue (issue)
  "Render restored ISSUE data into current buffer."
  (let ((inhibit-read-only t)
        (id (alist-get 'id issue))
        (title (alist-get 'title issue))
        (status (alist-get 'status issue))
        (priority (alist-get 'priority issue))
        (type (alist-get 'issue-type issue))
        (created (alist-get 'created-at issue))
        (updated (alist-get 'updated-at issue))
        (closed (alist-get 'closed-at issue))
        (compacted (alist-get 'compacted-at issue))
        (assignee (alist-get 'assignee issue))
        (external-ref (alist-get 'external-ref issue))
        (description (alist-get 'description issue))
        (acceptance (alist-get 'acceptance-criteria issue))
        (design (alist-get 'design issue))
        (notes (alist-get 'notes issue))
        (events (alist-get 'events issue)))

    (erase-buffer)

    ;; Historical view banner
    (insert (propertize
             "HISTORICAL VIEW - Restored from Git"
             'face 'beads-restore-header-banner-face))
    (insert "\n")
    (insert (propertize (make-string 60 ?═)
                       'face 'beads-restore-header-banner-face))
    (insert "\n")
    (insert (propertize
             "This is a read-only historical snapshot from before compaction."
             'face 'shadow))
    (insert "\n\n")

    ;; Title
    (insert (propertize (or title "Untitled")
                       'face '(:inherit bold :height 1.5)))
    (insert "\n")
    (insert (propertize (make-string (length (or title "Untitled")) ?═)
                       'face 'beads-restore-header-face))
    (insert "\n\n")

    ;; Metadata
    (beads-restore--insert-header "ID" id)
    (beads-restore--insert-header "Status" (upcase (or status "unknown")))
    (when priority
      (beads-restore--insert-header "Priority" priority))
    (when type
      (beads-restore--insert-header "Type" (upcase type)))
    (when assignee
      (beads-restore--insert-header "Assignee" assignee))
    (when external-ref
      (beads-restore--insert-header "External Ref" external-ref))
    (beads-restore--insert-header "Created"
                                 (beads-restore--format-date created))
    (beads-restore--insert-header "Updated"
                                 (beads-restore--format-date updated))
    (when (and closed (not (string-empty-p closed)))
      (beads-restore--insert-header "Closed"
                                   (beads-restore--format-date closed)))
    (when (and compacted (not (string-empty-p compacted)))
      (beads-restore--insert-header "Compacted"
                                   (beads-restore--format-date compacted)))

    ;; Text sections
    (beads-restore--insert-section "Description" description)
    (beads-restore--insert-section "Acceptance Criteria" acceptance)
    (beads-restore--insert-section "Design" design)
    (beads-restore--insert-section "Notes" notes)

    ;; Events section (if present)
    (when (and events (vectorp events) (> (length events) 0))
      (insert "\n\n")
      (insert (propertize "Event History"
                         'face 'beads-restore-header-face))
      (insert "\n")
      (insert (propertize (make-string 13 ?─)
                         'face 'beads-restore-header-face))
      (insert "\n\n")
      (dotimes (i (length events))
        (let* ((event (aref events i))
               (event-type (alist-get 'event_type event))
               (actor (alist-get 'actor event))
               (timestamp (alist-get 'timestamp event))
               (details (alist-get 'details event)))
          (insert (format "%d. [%s] %s by %s\n"
                         (1+ i)
                         (beads-restore--format-date timestamp)
                         (or event-type "event")
                         (or actor "unknown")))
          (when details
            (insert (format "   %s\n" details))))))

    ;; Footer
    (insert "\n\n")
    (insert (propertize "Press 'q' to quit, 'g' to re-restore"
                       'face 'shadow))
    (insert "\n")

    (goto-char (point-min))))

;;; Mode Definition

(defvar beads-restore-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'beads-refresh-restore)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `beads-restore-mode'.")

(define-derived-mode beads-restore-mode special-mode "Beads-Restore"
  "Major mode for displaying restored Beads issue history.

Key bindings:
\\{beads-restore-mode-map}"
  (setq buffer-read-only t))

;;; Commands

;;;###autoload
(defun beads-restore (issue-id)
  "Restore full history of compacted issue ISSUE-ID from git.

This command reads the compacted_at_commit from the database,
temporarily checks out that commit, reads the full issue from
JSONL at that point in history, and displays it.

This is a read-only operation that does not modify the database
or git state."
  (interactive
   (list (or (beads-restore--detect-issue-id)
             (completing-read "Restore issue: "
                            (beads--issue-completion-table)
                            nil t nil 'beads--issue-id-history))))
  (beads-check-executable)
  (let* ((buffer-name (format "*beads-restore: %s*" issue-id))
         (buffer (get-buffer-create buffer-name))
         (project-dir default-directory))
    (with-current-buffer buffer
      (beads-restore-mode)
      (setq beads-restore--issue-id issue-id)
      (setq default-directory project-dir)

      (condition-case err
          (let* ((result (beads--run-command "restore" issue-id))
                 (issue (beads--parse-issue result)))
            (setq beads-restore--issue-data issue)
            (beads-restore--render-issue issue))
        (error
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize "Error restoring issue\n\n"
                             'face 'error))
           (insert (format "%s" (error-message-string err)))
           (insert "\n\n")
           (insert (propertize "Possible reasons:\n" 'face 'bold))
           (insert "- Issue is not compacted\n")
           (insert "- Git commit is not available\n")
           (insert "- JSONL file is missing at that commit\n")
           (goto-char (point-min))))))

    (switch-to-buffer buffer)))

;;;###autoload
(defun beads-refresh-restore ()
  "Refresh the current restore buffer from bd CLI."
  (interactive)
  (unless (derived-mode-p 'beads-restore-mode)
    (user-error "Not in a beads-restore buffer"))
  (unless beads-restore--issue-id
    (user-error "No issue ID associated with this buffer"))

  (let ((pos (point)))
    (condition-case err
        (let* ((result (beads--run-command "restore"
                                          beads-restore--issue-id))
               (issue (beads--parse-issue result)))
          (setq beads-restore--issue-data issue)
          (beads-restore--render-issue issue)
          (goto-char (min pos (point-max)))
          (message "Refreshed %s" beads-restore--issue-id)
          nil)
      (error
       (message "Failed to refresh: %s" (error-message-string err))
       nil))))

;;; Footer

(provide 'beads-restore)
;;; beads-restore.el ends here
