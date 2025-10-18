;;; beads-list.el --- Tabulated list mode for Beads issues -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, project, issues

;;; Commentary:

;; beads-list.el provides tabulated-list-mode buffers for displaying
;; Beads issues in a sortable, colorized table format.
;;
;; Commands:
;;   M-x beads-list      ; Show all issues
;;   M-x beads-ready     ; Show ready work
;;   M-x beads-blocked   ; Show blocked issues
;;
;; Key bindings in beads-list-mode:
;;   n/p     - Next/previous issue
;;   RET     - Show issue details
;;   g       - Refresh buffer
;;   q       - Quit buffer
;;   m/u     - Mark/unmark issue
;;   U       - Unmark all issues
;;   * !     - Mark all issues (ibuffer-style)
;;   * u     - Unmark all issues (ibuffer-style)
;;   c/+     - Create new issue
;;   e       - Edit/update issue at point
;;   d/k     - Close issue at point
;;   w       - Copy issue ID to kill ring
;;   S       - Sort by column
;;   / /     - Filter by text
;;   / s     - Filter by status
;;   / p     - Filter by priority
;;   / t     - Filter by type
;;   / c     - Clear all filters
;;   B s     - Bulk update status for marked issues
;;   B p     - Bulk update priority for marked issues
;;   B c     - Bulk close marked issues

;;; Code:

(require 'beads)

;;; Customization

(defgroup beads-list nil
  "Tabulated list display for Beads issues."
  :group 'beads
  :prefix "beads-list-")

(defcustom beads-list-id-width 12
  "Width of ID column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-status-width 12
  "Width of Status column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-priority-width 8
  "Width of Priority column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-type-width 10
  "Width of Type column in issue lists."
  :type 'integer
  :group 'beads-list)

(defcustom beads-list-title-width 60
  "Width of Title column in issue lists."
  :type 'integer
  :group 'beads-list)

;;; Faces

(defface beads-list-status-open
  '((t :inherit font-lock-keyword-face))
  "Face for open status."
  :group 'beads-list)

(defface beads-list-status-in-progress
  '((t :inherit font-lock-warning-face))
  "Face for in_progress status."
  :group 'beads-list)

(defface beads-list-status-blocked
  '((t :inherit error))
  "Face for blocked status."
  :group 'beads-list)

(defface beads-list-status-closed
  '((t :inherit shadow))
  "Face for closed status."
  :group 'beads-list)

(defface beads-list-priority-critical
  '((t :inherit error :weight bold))
  "Face for priority 0 (critical)."
  :group 'beads-list)

(defface beads-list-priority-high
  '((t :inherit warning :weight bold))
  "Face for priority 1 (high)."
  :group 'beads-list)

(defface beads-list-priority-medium
  '((t :inherit default))
  "Face for priority 2 (medium)."
  :group 'beads-list)

(defface beads-list-priority-low
  '((t :inherit shadow))
  "Face for priority 3-4 (low/backlog)."
  :group 'beads-list)

;;; Variables

(defvar-local beads-list--command nil
  "The bd command used to populate this buffer (list, ready, or blocked).")

(defvar-local beads-list--raw-issues nil
  "Raw issue data for the current buffer.")

(defvar-local beads-list--marked-issues nil
  "List of marked issue IDs.")

(defvar-local beads-list--filter-status nil
  "Current status filter (nil means no filter).")

(defvar-local beads-list--filter-priority nil
  "Current priority filter (nil means no filter).")

(defvar-local beads-list--filter-type nil
  "Current issue type filter (nil means no filter).")

(defvar-local beads-list--filter-text nil
  "Current text search filter (nil means no filter).")

;;; Utilities

(defun beads-list--status-face (status)
  "Return face for STATUS."
  (pcase status
    ("open" 'beads-list-status-open)
    ("in_progress" 'beads-list-status-in-progress)
    ("blocked" 'beads-list-status-blocked)
    ("closed" 'beads-list-status-closed)
    (_ 'default)))

(defun beads-list--priority-face (priority)
  "Return face for PRIORITY."
  (pcase priority
    (0 'beads-list-priority-critical)
    (1 'beads-list-priority-high)
    (2 'beads-list-priority-medium)
    ((or 3 4) 'beads-list-priority-low)
    (_ 'default)))

(defun beads-list--format-status (status)
  "Format STATUS with appropriate face."
  (let ((status-str (or status "")))
    (propertize status-str 'face (beads-list--status-face status-str))))

(defun beads-list--format-priority (priority)
  "Format PRIORITY with appropriate face."
  (let ((priority-str (if priority (format "%d" priority) "")))
    (propertize priority-str 'face (beads-list--priority-face priority))))

(defun beads-list--issue-to-entry (issue)
  "Convert ISSUE alist to tabulated-list entry."
  (let* ((id (alist-get 'id issue))
         (title (or (alist-get 'title issue) ""))
         (status (alist-get 'status issue))
         (priority (alist-get 'priority issue))
         (type (or (alist-get 'issue-type issue) "")))
    (list id
          (vector id
                  (beads-list--format-status status)
                  (beads-list--format-priority priority)
                  type
                  title))))

(defun beads-list--apply-filters (issues)
  "Apply active filters to ISSUES and return filtered list."
  (let ((filtered issues))
    ;; Filter by status
    (when beads-list--filter-status
      (setq filtered
            (seq-filter (lambda (issue)
                         (string= (alist-get 'status issue)
                                 beads-list--filter-status))
                       filtered)))
    ;; Filter by priority
    (when beads-list--filter-priority
      (setq filtered
            (seq-filter (lambda (issue)
                         (equal (alist-get 'priority issue)
                               beads-list--filter-priority))
                       filtered)))
    ;; Filter by type
    (when beads-list--filter-type
      (setq filtered
            (seq-filter (lambda (issue)
                         (string= (alist-get 'issue-type issue)
                                 beads-list--filter-type))
                       filtered)))
    ;; Filter by text search (searches id, title, description)
    (when (and beads-list--filter-text
               (not (string-empty-p beads-list--filter-text)))
      (setq filtered
            (seq-filter (lambda (issue)
                         (or (string-match-p beads-list--filter-text
                                            (alist-get 'id issue))
                             (string-match-p beads-list--filter-text
                                            (or (alist-get 'title issue) ""))
                             (string-match-p beads-list--filter-text
                                            (or (alist-get 'description issue) ""))))
                       filtered)))
    filtered))

(defun beads-list--format-filter-string ()
  "Format current filters for mode-line display."
  (let ((filters nil))
    (when beads-list--filter-status
      (push (format "status=%s" beads-list--filter-status) filters))
    (when beads-list--filter-priority
      (push (format "priority=%s" beads-list--filter-priority) filters))
    (when beads-list--filter-type
      (push (format "type=%s" beads-list--filter-type) filters))
    (when (and beads-list--filter-text
               (not (string-empty-p beads-list--filter-text)))
      (push (format "text=%s" beads-list--filter-text) filters))
    (if filters
        (concat " [Filters: " (string-join filters ", ") "]")
      "")))

(defun beads-list--populate-buffer (issues command)
  "Populate current buffer with ISSUES using COMMAND for refresh."
  (setq beads-list--command command
        beads-list--raw-issues issues)
  (let ((filtered (beads-list--apply-filters issues)))
    (setq tabulated-list-entries
          (mapcar #'beads-list--issue-to-entry filtered))
    (tabulated-list-print t)))

(defun beads-list--current-issue-id ()
  "Return the ID of the issue at point, or nil."
  (tabulated-list-get-id))

(defun beads-list--get-issue-by-id (id)
  "Return issue alist for ID from current buffer's raw issues."
  (seq-find (lambda (issue)
              (string= (alist-get 'id issue) id))
            beads-list--raw-issues))

;;; Commands

(defun beads-list-refresh ()
  "Refresh the current issue list buffer."
  (interactive)
  (unless beads-list--command
    (user-error "No command associated with this buffer"))
  (let* ((issues (pcase beads-list--command
                   ('list (beads--parse-issues
                          (beads--run-command "list")))
                   ('ready (beads--parse-issues
                           (beads--run-command "ready")))
                   ('blocked (beads--parse-issues
                             (beads--run-command "blocked")))
                   (_ (error "Unknown command: %s" beads-list--command))))
         (pos (point)))
    (if (not issues)
        (progn
          (setq tabulated-list-entries nil)
          (tabulated-list-print t)
          (message "No issues found"))
      (beads-list--populate-buffer issues beads-list--command)
      (goto-char pos)
      (message "Refreshed %d issue%s"
               (length issues)
               (if (= (length issues) 1) "" "s")))))

(defun beads-list-show ()
  "Show details for the issue at point."
  (interactive)
  (if-let ((id (beads-list--current-issue-id)))
      (let ((project-dir default-directory))
        (require 'beads-show)
        ;; Preserve project context when showing issue
        (let ((default-directory project-dir))
          (beads-show id)))
    (user-error "No issue at point")))

(defun beads-list-quit ()
  "Quit the current issue list buffer."
  (interactive)
  (quit-window t))

(defun beads-list-next ()
  "Move to next issue."
  (interactive)
  (forward-line 1))

(defun beads-list-previous ()
  "Move to previous issue."
  (interactive)
  (forward-line -1))

(defun beads-list-mark ()
  "Mark the issue at point."
  (interactive)
  (when-let ((id (beads-list--current-issue-id)))
    (unless (member id beads-list--marked-issues)
      (push id beads-list--marked-issues))
    (tabulated-list-put-tag ">" t)))

(defun beads-list-unmark ()
  "Unmark the issue at point."
  (interactive)
  (when-let ((id (beads-list--current-issue-id)))
    (setq beads-list--marked-issues
          (delete id beads-list--marked-issues))
    (tabulated-list-put-tag " " t)))

(defun beads-list-mark-all ()
  "Mark all issues in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (beads-list--current-issue-id)
        (beads-list-mark))
      (forward-line 0))))  ; Don't advance, beads-list-mark does that

(defun beads-list-unmark-all ()
  "Unmark all issues in the current buffer."
  (interactive)
  (setq beads-list--marked-issues nil)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (beads-list--current-issue-id)
        (tabulated-list-put-tag " "))
      (forward-line 1))))

(defun beads-list-create ()
  "Create a new issue using the beads-create transient menu."
  (interactive)
  (require 'beads-create)
  (call-interactively #'beads-create))

(defun beads-list-update ()
  "Update the issue at point using the beads-update transient menu."
  (interactive)
  (if-let ((id (beads-list--current-issue-id)))
      (progn
        (require 'beads-update)
        ;; beads-update will auto-detect the issue ID from beads-list context
        (call-interactively #'beads-update))
    (user-error "No issue at point")))

(defun beads-list-close ()
  "Close the issue at point using the beads-close transient menu."
  (interactive)
  (if-let ((id (beads-list--current-issue-id)))
      (progn
        (require 'beads-misc)
        ;; beads-close will auto-detect the issue ID from beads-list context
        (call-interactively #'beads-close))
    (user-error "No issue at point")))

(defun beads-list-copy-id ()
  "Copy the issue ID at point to the kill ring."
  (interactive)
  (if-let ((id (beads-list--current-issue-id)))
      (progn
        (kill-new id)
        (message "Copied issue ID: %s" id))
    (user-error "No issue at point")))

(defun beads-list-sort ()
  "Sort the issue list by column.
Uses tabulated-list built-in sorting."
  (interactive)
  (call-interactively #'tabulated-list-sort))

(defun beads-list-filter-by-status ()
  "Filter issues by status."
  (interactive)
  (let ((status (completing-read "Filter by status (empty to clear): "
                                 '("open" "in_progress" "blocked" "closed")
                                 nil t)))
    (setq beads-list--filter-status
          (if (string-empty-p status) nil status))
    (beads-list--populate-buffer beads-list--raw-issues beads-list--command)
    (message "Filter: %s" (or (beads-list--format-filter-string) "cleared"))))

(defun beads-list-filter-by-priority ()
  "Filter issues by priority."
  (interactive)
  (let ((priority (completing-read "Filter by priority (empty to clear): "
                                   '("0" "1" "2" "3" "4")
                                   nil t)))
    (setq beads-list--filter-priority
          (if (string-empty-p priority) nil (string-to-number priority)))
    (beads-list--populate-buffer beads-list--raw-issues beads-list--command)
    (message "Filter: %s" (or (beads-list--format-filter-string) "cleared"))))

(defun beads-list-filter-by-type ()
  "Filter issues by type."
  (interactive)
  (let ((type (completing-read "Filter by type (empty to clear): "
                               '("bug" "feature" "task" "epic" "chore")
                               nil t)))
    (setq beads-list--filter-type
          (if (string-empty-p type) nil type))
    (beads-list--populate-buffer beads-list--raw-issues beads-list--command)
    (message "Filter: %s" (or (beads-list--format-filter-string) "cleared"))))

(defun beads-list-filter-by-text ()
  "Filter issues by text search (searches id, title, description)."
  (interactive)
  (let ((text (read-string "Filter by text (empty to clear): "
                           beads-list--filter-text)))
    (setq beads-list--filter-text
          (if (string-empty-p text) nil text))
    (beads-list--populate-buffer beads-list--raw-issues beads-list--command)
    (message "Filter: %s" (or (beads-list--format-filter-string) "cleared"))))

(defun beads-list-clear-filters ()
  "Clear all active filters."
  (interactive)
  (setq beads-list--filter-status nil
        beads-list--filter-priority nil
        beads-list--filter-type nil
        beads-list--filter-text nil)
  (beads-list--populate-buffer beads-list--raw-issues beads-list--command)
  (message "All filters cleared"))

;;; Bulk Operations

(defun beads-list-bulk-update-status ()
  "Update status for all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let ((status (completing-read
                   (format "Set status for %d issue(s): "
                          (length beads-list--marked-issues))
                   '("open" "in_progress" "blocked")
                   nil t)))
      (when (and (not (string-empty-p status))
                 (y-or-n-p (format "Update status to '%s' for %d issue(s)? "
                                  status (length beads-list--marked-issues))))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (beads--run-command "update" id "--status" status)
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to update %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Updated %d issue(s), %d failed" success-count fail-count))))))

(defun beads-list-bulk-update-priority ()
  "Update priority for all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let* ((choices '(("0 - Critical" . 0)
                     ("1 - High" . 1)
                     ("2 - Medium" . 2)
                     ("3 - Low" . 3)
                     ("4 - Backlog" . 4)))
           (selection (completing-read
                      (format "Set priority for %d issue(s): "
                             (length beads-list--marked-issues))
                      choices nil t))
           (priority (cdr (assoc selection choices))))
      (when (and priority
                 (y-or-n-p (format "Update priority to %d for %d issue(s)? "
                                  priority (length beads-list--marked-issues))))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (beads--run-command "update" id "--priority"
                                     (number-to-string priority))
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to update %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Updated %d issue(s), %d failed" success-count fail-count))))))

(defun beads-list-bulk-close ()
  "Close all marked issues."
  (interactive)
  (if (null beads-list--marked-issues)
      (user-error "No issues marked")
    (let ((reason (read-string
                   (format "Reason for closing %d issue(s): "
                          (length beads-list--marked-issues)))))
      (when (y-or-n-p (format "Close %d issue(s)? "
                             (length beads-list--marked-issues)))
        (let ((success-count 0)
              (fail-count 0))
          (dolist (id beads-list--marked-issues)
            (condition-case err
                (progn
                  (if (and reason (not (string-empty-p (string-trim reason))))
                      (beads--run-command "close" id "--reason" reason)
                    (beads--run-command "close" id))
                  (setq success-count (1+ success-count)))
              (error
               (message "Failed to close %s: %s" id
                       (error-message-string err))
               (setq fail-count (1+ fail-count)))))
          (beads-list-unmark-all)
          (beads--invalidate-completion-cache)
          (beads-list-refresh)
          (message "Closed %d issue(s), %d failed" success-count fail-count))))))

;;; Mode Definition

(defvar beads-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; Navigation (following standard conventions)
    (define-key map (kbd "n") #'beads-list-next)
    (define-key map (kbd "p") #'beads-list-previous)
    (define-key map (kbd "RET") #'beads-list-show)

    ;; Refresh/quit (like Magit, dired)
    (define-key map (kbd "g") #'beads-list-refresh)
    (define-key map (kbd "q") #'beads-list-quit)

    ;; Marking (like dired/ibuffer)
    (define-key map (kbd "m") #'beads-list-mark)
    (define-key map (kbd "u") #'beads-list-unmark)
    (define-key map (kbd "U") #'beads-list-unmark-all)
    (define-key map (kbd "* !") #'beads-list-mark-all)     ; ibuffer-style
    (define-key map (kbd "* *") #'beads-list-mark-all)     ; alternative
    (define-key map (kbd "* u") #'beads-list-unmark-all)   ; ibuffer-style

    ;; CRUD operations (following Emacs conventions)
    (define-key map (kbd "c") #'beads-list-create)         ; create (like many modes)
    (define-key map (kbd "+") #'beads-list-create)         ; alternative
    (define-key map (kbd "e") #'beads-list-update)         ; edit (more intuitive)
    (define-key map (kbd "d") #'beads-list-close)          ; delete/done (mark for closing)
    (define-key map (kbd "k") #'beads-list-close)          ; kill (alternative)

    ;; Utilities
    (define-key map (kbd "w") #'beads-list-copy-id)        ; copy (like eww, info)
    (define-key map (kbd "S") #'beads-list-sort)           ; sort menu

    ;; Filtering (like ibuffer) - create prefix map for /
    (let ((filter-map (make-sparse-keymap)))
      (define-key filter-map (kbd "s") #'beads-list-filter-by-status)
      (define-key filter-map (kbd "p") #'beads-list-filter-by-priority)
      (define-key filter-map (kbd "t") #'beads-list-filter-by-type)
      (define-key filter-map (kbd "/") #'beads-list-filter-by-text)
      (define-key filter-map (kbd "c") #'beads-list-clear-filters)
      (define-key map (kbd "/") filter-map))

    ;; Bulk operations (like Magit) - create prefix map for B
    (let ((bulk-map (make-sparse-keymap)))
      (define-key bulk-map (kbd "s") #'beads-list-bulk-update-status)
      (define-key bulk-map (kbd "p") #'beads-list-bulk-update-priority)
      (define-key bulk-map (kbd "c") #'beads-list-bulk-close)
      (define-key map (kbd "B") bulk-map))
    map)
  "Keymap for `beads-list-mode'.")

(define-derived-mode beads-list-mode tabulated-list-mode "Beads-List"
  "Major mode for displaying Beads issues in a tabulated list.

\\{beads-list-mode-map}"
  (setq tabulated-list-format
        (vector (list "ID" beads-list-id-width t)
                (list "Status" beads-list-status-width t)
                (list "Priority" beads-list-priority-width t
                      :right-align t)
                (list "Type" beads-list-type-width t)
                (list "Title" beads-list-title-width t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Priority" nil))
  (tabulated-list-init-header))

;;; Public Commands

;;;###autoload
(defun beads-list ()
  "Display all Beads issues in a tabulated list."
  (interactive)
  (beads-check-executable)
  (let ((issues (beads--parse-issues (beads--run-command "list")))
        (buffer (get-buffer-create "*beads-list*"))
        (project-dir default-directory))  ; Capture project context
    (with-current-buffer buffer
      (beads-list-mode)
      ;; Preserve project context in list buffer
      (setq default-directory project-dir)
      (if (not issues)
          (progn
            (setq tabulated-list-entries nil)
            (tabulated-list-print t)
            (setq mode-line-format
                  '("%e" mode-line-front-space
                    mode-line-buffer-identification
                    "  No issues found"))
            (message "No issues found"))
        (beads-list--populate-buffer issues 'list)
        (setq mode-line-format
              '("%e" mode-line-front-space
                mode-line-buffer-identification
                (:eval (format "  %d issue%s%s%s"
                             (length tabulated-list-entries)
                             (if (= (length tabulated-list-entries) 1) "" "s")
                             (if beads-list--marked-issues
                                 (format " [%d marked]"
                                        (length beads-list--marked-issues))
                               "")
                             (beads-list--format-filter-string)))))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun beads-ready ()
  "Display ready Beads issues in a tabulated list."
  (interactive)
  (beads-check-executable)
  (let ((issues (beads--parse-issues (beads--run-command "ready")))
        (buffer (get-buffer-create "*beads-ready*"))
        (project-dir default-directory))  ; Capture project context
    (with-current-buffer buffer
      (beads-list-mode)
      ;; Preserve project context in list buffer
      (setq default-directory project-dir)
      (if (not issues)
          (progn
            (setq tabulated-list-entries nil)
            (tabulated-list-print t)
            (setq mode-line-format
                  '("%e" mode-line-front-space
                    mode-line-buffer-identification
                    "  No ready issues"))
            (message "No ready issues found"))
        (beads-list--populate-buffer issues 'ready)
        (setq mode-line-format
              '("%e" mode-line-front-space
                mode-line-buffer-identification
                (:eval (format "  %d ready issue%s%s%s"
                             (length tabulated-list-entries)
                             (if (= (length tabulated-list-entries) 1) "" "s")
                             (if beads-list--marked-issues
                                 (format " [%d marked]"
                                        (length beads-list--marked-issues))
                               "")
                             (beads-list--format-filter-string)))))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun beads-blocked ()
  "Display blocked Beads issues in a tabulated list."
  (interactive)
  (beads-check-executable)
  (let ((issues (beads--parse-issues (beads--run-command "blocked")))
        (buffer (get-buffer-create "*beads-blocked*"))
        (project-dir default-directory))  ; Capture project context
    (with-current-buffer buffer
      (beads-list-mode)
      ;; Preserve project context in list buffer
      (setq default-directory project-dir)
      (if (not issues)
          (progn
            (setq tabulated-list-entries nil)
            (tabulated-list-print t)
            (setq mode-line-format
                  '("%e" mode-line-front-space
                    mode-line-buffer-identification
                    "  No blocked issues"))
            (message "No blocked issues found"))
        (beads-list--populate-buffer issues 'blocked)
        (setq mode-line-format
              '("%e" mode-line-front-space
                mode-line-buffer-identification
                (:eval (format "  %d blocked issue%s%s%s"
                             (length tabulated-list-entries)
                             (if (= (length tabulated-list-entries) 1) "" "s")
                             (if beads-list--marked-issues
                                 (format " [%d marked]"
                                        (length beads-list--marked-issues))
                               "")
                             (beads-list--format-filter-string)))))))
    (pop-to-buffer buffer)))

;;; Footer

(provide 'beads-list)
;;; beads-list.el ends here
