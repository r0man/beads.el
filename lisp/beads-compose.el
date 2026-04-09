;;; beads-compose.el --- Buffer-based editing for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Buffer-based editing for issue creation, editing, and commenting
;; (Pattern 3 in the beads.el UX hierarchy).
;;
;; Three buffer types:
;; - Create: *beads-create[PROJECT]* — first line = title, rest = description
;; - Edit:   *beads-edit[PROJECT]/ISSUE-ID* — pre-populated with current description
;; - Comment: *beads-comment[PROJECT]/ISSUE-ID* — empty buffer for new comment
;;
;; All buffers use beads-compose-mode with:
;; - C-c C-c to submit
;; - C-c C-k to cancel
;; - C-c C-a to open metadata transient (create only)
;;
;; Metadata (type, priority, labels, assignee, parent) is stored in
;; buffer-local variables and edited via a small transient sidebar.

;;; Code:

(require 'beads-command)
(require 'beads-types)
(require 'transient)

;; Forward declarations
(declare-function beads-check-executable "beads-util")
(declare-function beads-git-get-project-name "beads-git")
(declare-function beads--invalidate-completion-cache "beads-util")
(declare-function beads-list-refresh-all "beads-command-list")
(declare-function beads-refresh-show "beads-command-show")
(declare-function beads-issue-at-point-or-read "beads")
(declare-function beads-command-update "beads-command-update")
(declare-function beads-command-comments-add "beads-command-comments")

;;; Buffer-Local Metadata

(defvar-local beads-compose--action nil
  "The compose action: `create', `edit', or `comment'.")

(defvar-local beads-compose--issue-id nil
  "Issue ID for edit/comment actions.")

(defvar-local beads-compose--type "task"
  "Issue type for create action.")

(defvar-local beads-compose--priority 2
  "Issue priority for create action (0-4).")

(defvar-local beads-compose--assignee nil
  "Issue assignee for create action.")

(defvar-local beads-compose--labels nil
  "Issue labels for create action (list of strings).")

(defvar-local beads-compose--parent nil
  "Parent issue ID for create action.")

(defvar-local beads-compose--window-config nil
  "Window configuration to restore on cancel.")

;;; Title/Body Extraction

(defun beads-compose--extract-title ()
  "Extract the title from the first line of the current buffer.
Strips leading `# ' marker and trims whitespace.
Returns nil if the first line is empty."
  (save-excursion
    (goto-char (point-min))
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      ;; Strip leading # marker
      (when (string-prefix-p "# " line)
        (setq line (substring line 2)))
      (setq line (string-trim line))
      (if (string-empty-p line) nil line))))

(defun beads-compose--extract-body ()
  "Extract the body (everything after the first line).
Skips blank lines immediately after the title.
Returns nil if there is no body."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    ;; Skip blank lines after title
    (while (and (not (eobp))
                (looking-at-p "^[[:space:]]*$"))
      (forward-line 1))
    (if (eobp)
        nil
      (let ((body (buffer-substring-no-properties (point) (point-max))))
        (setq body (string-trim-right body))
        (if (string-empty-p body) nil body)))))

;;; Mode Definition

(defvar beads-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-compose-submit)
    (define-key map (kbd "C-c C-k") #'beads-compose-cancel)
    (define-key map (kbd "C-c C-a") #'beads-compose-metadata)
    map)
  "Keymap for `beads-compose-mode'.")

(define-derived-mode beads-compose-mode text-mode "Beads-Compose"
  "Major mode for composing beads issues, edits, and comments.

First line is the title (for create), rest is description/body.
Use \\[beads-compose-submit] to submit, \\[beads-compose-cancel] to cancel,
\\[beads-compose-metadata] for metadata.

\\{beads-compose-mode-map}"
  (visual-line-mode 1))

;;; Buffer Creation

(defun beads-compose--buffer-name (action &optional issue-id)
  "Return the buffer name for ACTION and optional ISSUE-ID.
ACTION is `create', `edit', or `comment'."
  (let ((project (or (beads-git-get-project-name) "unknown")))
    (pcase action
      ('create (format "*beads-create[%s]*" project))
      ('edit (format "*beads-edit[%s]/%s*" project issue-id))
      ('comment (format "*beads-comment[%s]/%s*" project issue-id)))))

(defun beads-compose--setup-buffer (buf action &optional issue-id)
  "Set up BUF as a compose buffer for ACTION with ISSUE-ID."
  (with-current-buffer buf
    (unless (derived-mode-p 'beads-compose-mode)
      (beads-compose-mode))
    (setq beads-compose--action action)
    (setq beads-compose--issue-id issue-id)
    (setq beads-compose--window-config (current-window-configuration))
    (setq header-line-format
          (pcase action
            ('create "Create issue: C-c C-c submit, C-c C-k cancel, C-c C-a metadata")
            ('edit (format "Edit %s: C-c C-c save, C-c C-k cancel" issue-id))
            ('comment (format "Comment on %s: C-c C-c submit, C-c C-k cancel" issue-id)))))
  buf)

;;;###autoload
(defun beads-compose-create ()
  "Open a compose buffer to create a new issue.
Returns the buffer.  If a create buffer already exists for this
project, switch to it instead of creating a new one."
  (interactive)
  (beads-check-executable)
  (let* ((name (beads-compose--buffer-name 'create))
         (existing (get-buffer name))
         (buf (or existing (generate-new-buffer name))))
    (beads-compose--setup-buffer buf 'create)
    (unless existing
      (with-current-buffer buf
        (insert "# \n\n")))
    (pop-to-buffer buf)
    (unless existing
      (with-current-buffer buf
        (goto-char (point-min))
        (end-of-line)))
    buf))

;;;###autoload
(defun beads-compose-edit (issue-id description)
  "Open a compose buffer to edit ISSUE-ID's DESCRIPTION.
Returns the buffer."
  (interactive
   (let ((id (beads-issue-at-point-or-read "Edit issue: ")))
     (list id nil)))
  (beads-check-executable)
  (let* ((name (beads-compose--buffer-name 'edit issue-id))
         (existing (get-buffer name))
         (buf (or existing (generate-new-buffer name))))
    (beads-compose--setup-buffer buf 'edit issue-id)
    (unless existing
      (with-current-buffer buf
        (when description
          (insert description))))
    (pop-to-buffer buf)
    buf))

;;;###autoload
(defun beads-compose-comment (issue-id)
  "Open a compose buffer to add a comment to ISSUE-ID.
Returns the buffer."
  (interactive
   (list (beads-issue-at-point-or-read "Comment on issue: ")))
  (beads-check-executable)
  (let* ((name (beads-compose--buffer-name 'comment issue-id))
         (existing (get-buffer name))
         (buf (or existing (generate-new-buffer name))))
    (beads-compose--setup-buffer buf 'comment issue-id)
    (pop-to-buffer buf)
    buf))

;;; Submit / Cancel

(defun beads-compose-submit ()
  "Submit the compose buffer contents.
For create: calls `beads-execute' with title + description + metadata.
For edit: calls `beads-command-update' to update the description.
For comment: calls `beads-command-comments-add' to add a comment."
  (interactive)
  (pcase beads-compose--action
    ('create (beads-compose--submit-create))
    ('edit (beads-compose--submit-edit))
    ('comment (beads-compose--submit-comment))
    (_ (user-error "Unknown compose action: %s" beads-compose--action))))

(defun beads-compose--submit-create ()
  "Submit the create buffer."
  (let ((title (beads-compose--extract-title))
        (body (beads-compose--extract-body)))
    (unless title
      (user-error "Title is required (first line must not be empty)"))
    (require 'beads-command-create)
    (let* ((args (list :title title
                       :issue-type beads-compose--type
                       :priority (number-to-string beads-compose--priority)))
           (args (if body (append args (list :description body)) args))
           (args (if beads-compose--assignee
                     (append args (list :assignee beads-compose--assignee))
                   args))
           (args (if beads-compose--labels
                     (append args (list :labels beads-compose--labels))
                   args))
           (args (if beads-compose--parent
                     (append args (list :parent beads-compose--parent))
                   args))
           (issue (apply #'beads-execute 'beads-command-create args))
           (win-config beads-compose--window-config))
      (kill-buffer)
      (when win-config
        (set-window-configuration win-config))
      (beads--invalidate-completion-cache)
      (beads-list-refresh-all)
      (if (beads-issue-p issue)
          (message "Created issue %s: %s"
                   (oref issue id) (oref issue title))
        (message "Issue created")))))

(defun beads-compose--submit-edit ()
  "Submit the edit buffer."
  (let ((text (string-trim (buffer-substring-no-properties
                            (point-min) (point-max))))
        (issue-id beads-compose--issue-id)
        (win-config beads-compose--window-config))
    (require 'beads-command-update)
    (beads-command-execute
     (beads-command-update :issue-ids (list issue-id)
                           :description text))
    (kill-buffer)
    (when win-config
      (set-window-configuration win-config))
    (beads--invalidate-completion-cache)
    ;; Try to refresh the show buffer if visible
    (when (fboundp 'beads-refresh-show)
      (dolist (buf (buffer-list))
        (when (and (buffer-live-p buf)
                   (string-match-p (regexp-quote issue-id) (buffer-name buf)))
          (with-current-buffer buf
            (when (and (boundp 'beads-show--issue-id)
                       (equal (buffer-local-value 'beads-show--issue-id buf)
                              issue-id))
              (ignore-errors (beads-refresh-show)))))))
    (message "Updated description for %s" issue-id)))

(defun beads-compose--submit-comment ()
  "Submit the comment buffer."
  (let ((text (string-trim (buffer-substring-no-properties
                            (point-min) (point-max))))
        (issue-id beads-compose--issue-id)
        (win-config beads-compose--window-config))
    (when (string-empty-p text)
      (user-error "Comment text is required"))
    (require 'beads-command-comments)
    (beads-command-execute
     (beads-command-comments-add :issue-id issue-id :text text))
    (kill-buffer)
    (when win-config
      (set-window-configuration win-config))
    (message "Added comment to %s" issue-id)))

(defun beads-compose-cancel ()
  "Cancel the compose buffer.
Kills the buffer and restores the previous window configuration."
  (interactive)
  (let ((win-config beads-compose--window-config)
        (modified (buffer-modified-p)))
    (when (or (not modified)
              (y-or-n-p "Discard unsaved changes? "))
      (set-buffer-modified-p nil)
      (kill-buffer)
      (when win-config
        (set-window-configuration win-config)))))

;;; Metadata Transient

;;;###autoload (autoload 'beads-compose-metadata "beads-compose" nil t)
(transient-define-prefix beads-compose-metadata ()
  "Set metadata for the current compose buffer."
  :transient-suffix 'transient--do-stay
  ["Metadata"
   ("t" beads-compose--set-type)
   ("p" beads-compose--set-priority)
   ("a" beads-compose--set-assignee)
   ("l" beads-compose--set-labels)
   ("P" beads-compose--set-parent)]
  ["Actions"
   ("q" "Done" transient-quit-one)])

(transient-define-suffix beads-compose--set-type ()
  :key "t"
  :description (lambda ()
                 (format "Type: %s" (or beads-compose--type "task")))
  (interactive)
  (setq beads-compose--type
        (completing-read "Type: "
                         '("bug" "feature" "task" "epic" "chore")
                         nil t nil nil beads-compose--type)))

(transient-define-suffix beads-compose--set-priority ()
  :key "p"
  :description (lambda ()
                 (format "Priority: %s" beads-compose--priority))
  (interactive)
  (setq beads-compose--priority
        (string-to-number
         (completing-read "Priority (0-4): "
                          '("0" "1" "2" "3" "4")
                          nil t nil nil
                          (number-to-string beads-compose--priority)))))

(transient-define-suffix beads-compose--set-assignee ()
  :key "a"
  :description (lambda ()
                 (format "Assignee: %s"
                         (or beads-compose--assignee "(none)")))
  (interactive)
  (setq beads-compose--assignee
        (let ((val (read-string "Assignee: " beads-compose--assignee)))
          (if (string-empty-p val) nil val))))

(transient-define-suffix beads-compose--set-labels ()
  :key "l"
  :description (lambda ()
                 (format "Labels: %s"
                         (if beads-compose--labels
                             (string-join beads-compose--labels ", ")
                           "(none)")))
  (interactive)
  (setq beads-compose--labels
        (let ((val (read-string "Labels (comma-separated): "
                                (when beads-compose--labels
                                  (string-join beads-compose--labels ",")))))
          (if (string-empty-p val)
              nil
            (split-string val "," t "[[:space:]]*")))))

(transient-define-suffix beads-compose--set-parent ()
  :key "P"
  :description (lambda ()
                 (format "Parent: %s"
                         (or beads-compose--parent "(none)")))
  (interactive)
  (setq beads-compose--parent
        (let ((val (read-string "Parent issue ID: " beads-compose--parent)))
          (if (string-empty-p val) nil val))))

(provide 'beads-compose)
;;; beads-compose.el ends here
