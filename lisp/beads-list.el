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
;;   M/U     - Mark all/unmark all issues
;;   C       - Create new issue
;;   c       - Update issue at point

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

(defun beads-list--populate-buffer (issues command)
  "Populate current buffer with ISSUES using COMMAND for refresh."
  (setq beads-list--command command
        beads-list--raw-issues issues)
  (setq tabulated-list-entries
        (mapcar #'beads-list--issue-to-entry issues))
  (tabulated-list-print t))

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
      (progn
        (require 'beads-show)
        (beads-show id))
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
        (beads-update id))
    (user-error "No issue at point")))

;;; Mode Definition

(defvar beads-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "n") #'beads-list-next)
    (define-key map (kbd "p") #'beads-list-previous)
    (define-key map (kbd "RET") #'beads-list-show)
    (define-key map (kbd "g") #'beads-list-refresh)
    (define-key map (kbd "q") #'beads-list-quit)
    (define-key map (kbd "m") #'beads-list-mark)
    (define-key map (kbd "u") #'beads-list-unmark)
    (define-key map (kbd "M") #'beads-list-mark-all)
    (define-key map (kbd "U") #'beads-list-unmark-all)
    (define-key map (kbd "C") #'beads-list-create)
    (define-key map (kbd "c") #'beads-list-update)
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
        (buffer (get-buffer-create "*beads-list*")))
    (with-current-buffer buffer
      (beads-list-mode)
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
              `("%e" mode-line-front-space
                mode-line-buffer-identification
                "  " ,(format "%d issue%s"
                             (length issues)
                             (if (= (length issues) 1) "" "s"))))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun beads-ready ()
  "Display ready Beads issues in a tabulated list."
  (interactive)
  (beads-check-executable)
  (let ((issues (beads--parse-issues (beads--run-command "ready")))
        (buffer (get-buffer-create "*beads-ready*")))
    (with-current-buffer buffer
      (beads-list-mode)
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
              `("%e" mode-line-front-space
                mode-line-buffer-identification
                "  " ,(format "%d ready issue%s"
                             (length issues)
                             (if (= (length issues) 1) "" "s"))))))
    (pop-to-buffer buffer)))

;;;###autoload
(defun beads-blocked ()
  "Display blocked Beads issues in a tabulated list."
  (interactive)
  (beads-check-executable)
  (let ((issues (beads--parse-issues (beads--run-command "blocked")))
        (buffer (get-buffer-create "*beads-blocked*")))
    (with-current-buffer buffer
      (beads-list-mode)
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
              `("%e" mode-line-front-space
                mode-line-buffer-identification
                "  " ,(format "%d blocked issue%s"
                             (length issues)
                             (if (= (length issues) 1) "" "s"))))))
    (pop-to-buffer buffer)))

;;; Footer

(provide 'beads-list)
;;; beads-list.el ends here
