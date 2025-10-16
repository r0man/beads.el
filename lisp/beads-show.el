;;; beads-show.el --- Issue detail view for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides issue detail display functionality for beads.el.
;; It implements a special-mode-derived buffer that shows full issue
;; details with markdown-like formatting and clickable bd-N references.
;;
;; Key features:
;; - Formatted display of issue metadata and text fields
;; - Clickable bd-N references using buttons
;; - Basic text fontification (bold, colors)
;; - Markdown-mode-style outline navigation (C-c C-n/p/f/b/u)
;; - Inline field editing (C-c C-e)
;; - Simple section navigation (n/p)
;; - Reference and button navigation ([, ], TAB)
;; - Keyboard navigation and refresh commands
;; - Handles missing optional fields gracefully
;;
;; Outline navigation:
;; - C-c C-n: Next heading (any level)
;; - C-c C-p: Previous heading (any level)
;; - C-c C-f: Next heading at same level
;; - C-c C-b: Previous heading at same level
;; - C-c C-u: Up to parent heading
;;
;; Field editing:
;; - C-c C-e: Edit field (prompts for field selection)

;;; Code:

(require 'beads)
(require 'button)

;;; Customization

(defgroup beads-show nil
  "Issue detail view settings."
  :group 'beads
  :prefix "beads-show-")

(defcustom beads-show-section-separator "\n\n"
  "Separator between sections in show buffer."
  :type 'string
  :group 'beads-show)

(defcustom beads-show-wrap-lines t
  "Whether to wrap long lines in show buffers."
  :type 'boolean
  :group 'beads-show)

;;; Faces

(defface beads-show-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headers in show buffer."
  :group 'beads-show)

(defface beads-show-label-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for metadata labels in show buffer."
  :group 'beads-show)

(defface beads-show-value-face
  '((t :inherit default))
  "Face for metadata values in show buffer."
  :group 'beads-show)

(defface beads-show-status-open-face
  '((t :inherit success :weight bold))
  "Face for open status."
  :group 'beads-show)

(defface beads-show-status-in-progress-face
  '((t :inherit warning :weight bold))
  "Face for in_progress status."
  :group 'beads-show)

(defface beads-show-status-blocked-face
  '((t :inherit error :weight bold))
  "Face for blocked status."
  :group 'beads-show)

(defface beads-show-status-closed-face
  '((t :inherit shadow :weight bold))
  "Face for closed status."
  :group 'beads-show)

(defface beads-show-priority-critical-face
  '((t :inherit error :weight bold))
  "Face for priority 0 (critical)."
  :group 'beads-show)

(defface beads-show-priority-high-face
  '((t :inherit warning :weight bold))
  "Face for priority 1 (high)."
  :group 'beads-show)

(defface beads-show-priority-medium-face
  '((t :inherit default))
  "Face for priority 2 (medium)."
  :group 'beads-show)

(defface beads-show-priority-low-face
  '((t :inherit shadow))
  "Face for priority 3-4 (low/backlog)."
  :group 'beads-show)

(defface beads-show-code-face
  '((t :inherit font-lock-string-face))
  "Face for inline code in markdown."
  :group 'beads-show)

(defface beads-show-heading-face
  '((t :inherit font-lock-function-name-face :weight bold :height 1.2))
  "Face for markdown headings."
  :group 'beads-show)

;;; Variables

(defvar-local beads-show--issue-id nil
  "Issue ID displayed in current show buffer.")

(defvar-local beads-show--issue-data nil
  "Full issue data for current show buffer.")

;;; Mode Definition

(defvar beads-show-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Refresh/quit
    (define-key map (kbd "g") #'beads-refresh-show)
    (define-key map (kbd "q") #'quit-window)

    ;; Section navigation (simple)
    (define-key map (kbd "n") #'beads-show-next-section)
    (define-key map (kbd "p") #'beads-show-previous-section)

    ;; Outline navigation (markdown-mode style)
    (define-key map (kbd "C-c C-n") #'beads-show-outline-next)
    (define-key map (kbd "C-c C-p") #'beads-show-outline-previous)
    (define-key map (kbd "C-c C-f") #'beads-show-outline-next-same-level)
    (define-key map (kbd "C-c C-b") #'beads-show-outline-previous-same-level)
    (define-key map (kbd "C-c C-u") #'beads-show-outline-up)

    ;; Field editing
    (define-key map (kbd "C-c C-e") #'beads-show-edit-field)

    ;; Reference navigation (like compilation-mode)
    (define-key map (kbd "[") #'beads-show-previous-reference)
    (define-key map (kbd "]") #'beads-show-next-reference)
    (define-key map (kbd "TAB") #'beads-show-next-button)
    (define-key map (kbd "<backtab>") #'beads-show-previous-button)
    (define-key map (kbd "S-TAB") #'beads-show-previous-button)

    ;; Follow reference
    (define-key map (kbd "RET") #'beads-show-follow-reference)
    (define-key map (kbd "o") #'beads-show-follow-reference-other-window)
    map)
  "Keymap for `beads-show-mode'.")

(define-derived-mode beads-show-mode special-mode "Beads-Show"
  "Major mode for displaying Beads issue details.

Key bindings:
\\{beads-show-mode-map}"
  (setq truncate-lines (not beads-show-wrap-lines))
  (setq buffer-read-only t))

;;; Utility Functions

(defun beads-show--format-status (status)
  "Return formatted STATUS string with appropriate face."
  (let ((text (upcase (or status "UNKNOWN")))
        (face (pcase status
                ("open" 'beads-show-status-open-face)
                ("in_progress" 'beads-show-status-in-progress-face)
                ("blocked" 'beads-show-status-blocked-face)
                ("closed" 'beads-show-status-closed-face)
                (_ 'default))))
    (propertize text 'face face)))

(defun beads-show--format-priority (priority)
  "Return formatted PRIORITY string with appropriate face."
  (when priority
    (let* ((text (format "%d" priority))
           (label (pcase priority
                    (0 "Critical")
                    (1 "High")
                    (2 "Medium")
                    (3 "Low")
                    (4 "Backlog")
                    (_ "Unknown")))
           (face (pcase priority
                   (0 'beads-show-priority-critical-face)
                   (1 'beads-show-priority-high-face)
                   (2 'beads-show-priority-medium-face)
                   (_ 'beads-show-priority-low-face))))
      (propertize (format "%s (%s)" text label) 'face face))))

(defun beads-show--format-date (date-string)
  "Format DATE-STRING for display."
  (if date-string
      (let* ((cleaned (replace-regexp-in-string "\\.[0-9]+.*$" ""
                                                date-string))
             (without-z (replace-regexp-in-string "Z$" "" cleaned)))
        (replace-regexp-in-string "T" " " without-z))
    "N/A"))

(defun beads-show--insert-header (label value &optional value-face)
  "Insert a metadata header line with LABEL and VALUE.
Optional VALUE-FACE can be used for custom face."
  (insert (propertize label 'face 'beads-show-label-face))
  (insert ": ")
  (when value
    (insert (propertize (format "%s" value)
                       'face (or value-face 'beads-show-value-face))))
  (insert "\n"))

(defun beads-show--insert-section (title content)
  "Insert a section with TITLE and CONTENT.
CONTENT can be a string or nil (empty sections are skipped)."
  (when (and content (not (string-empty-p (string-trim content))))
    (insert beads-show-section-separator)
    (insert (propertize title 'face 'beads-show-header-face))
    (insert "\n")
    (insert (propertize (make-string (length title) ?─)
                       'face 'beads-show-header-face))
    (insert "\n\n")
    (let ((start (point)))
      (insert content)
      (insert "\n")
      ;; Apply markdown-like fontification
      (beads-show--fontify-markdown start (point))
      ;; Make bd-N references clickable
      (beads-show--buttonize-references start (point)))))

(defun beads-show--fontify-markdown (start end)
  "Apply basic markdown fontification between START and END."
  (save-excursion
    (goto-char start)
    ;; Headings (## Header)
    (while (re-search-forward "^\\(#+\\)\\s-+\\(.+\\)$" end t)
      (let ((level (length (match-string 1)))
            (heading-start (match-beginning 2))
            (heading-end (match-end 2)))
        (put-text-property heading-start heading-end
                          'face 'beads-show-heading-face)))

    ;; Inline code (`code`)
    (goto-char start)
    (while (re-search-forward "`\\([^`\n]+\\)`" end t)
      (put-text-property (match-beginning 1) (match-end 1)
                        'face 'beads-show-code-face))

    ;; Bold (**text** or __text__)
    (goto-char start)
    (while (re-search-forward "\\(?:\\*\\*\\|__\\)\\([^*_\n]+\\)\\(?:\\*\\*\\|__\\)"
                             end t)
      (put-text-property (match-beginning 1) (match-end 1)
                        'face 'bold))

    ;; Italic (*text* or _text_)
    (goto-char start)
    (while (re-search-forward "\\(?:[^*]\\|^\\)\\(\\*\\([^*\n]+\\)\\*\\)" end t)
      (put-text-property (match-beginning 2) (match-end 2)
                        'face 'italic))

    ;; Lists (- item or * item)
    (goto-char start)
    (while (re-search-forward "^\\s-*\\([-*+]\\)\\s-+" end t)
      (put-text-property (match-beginning 1) (match-end 1)
                        'face 'font-lock-builtin-face))))

(defun beads-show--buttonize-references (start end)
  "Make bd-N references clickable between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\(bd-[0-9]+\\)" end t)
      (let ((issue-id (match-string 1)))
        (make-button (match-beginning 1) (match-end 1)
                    'issue-id issue-id
                    'action #'beads-show--button-action
                    'follow-link t
                    'help-echo (format "Jump to %s" issue-id)
                    'face 'link)))))

(defun beads-show--button-action (button)
  "Action for clicking on a bd-N reference BUTTON."
  (let ((issue-id (button-get button 'issue-id)))
    (beads-show issue-id)))

(defun beads-show--extract-issue-at-point ()
  "Extract bd-N issue reference at point.
Returns the issue ID or nil if none found."
  (let ((case-fold-search nil)
        (original-point (point)))
    (or
     ;; First try to see if we're on a button
     (when-let ((button (button-at original-point)))
       (when-let ((id (button-get button 'issue-id)))
         (when (string-match "^bd-[0-9]+$" id)
           id)))

     ;; Try to find bd-N on current line around point
     (save-excursion
       (let ((line-start (line-beginning-position))
             (line-end (line-end-position))
             (result nil))
         (goto-char line-start)
         (while (and (not result)
                    (re-search-forward "\\(bd-[0-9]+\\)" line-end t))
           (let ((match-start (match-beginning 1))
                 (match-end (match-end 1)))
             (when (and (>= original-point match-start)
                       (<= original-point match-end))
               (setq result (match-string 1)))))
         result)))))

;;; Outline Navigation

(defun beads-show--section-level ()
  "Return the outline level of the section at point.
Returns:
  0 - Title section (═══ underline)
  1 - Major section (─── underline)
  2+ - Markdown heading (## = 2, ### = 3, etc.)
  nil - Not at a heading"
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Check if we're on a ═══ underline (title level 0)
     ((looking-at "^═+$")
      0)
     ;; Check if we're on the title text line (followed by ═══)
     ((and (not (eobp))
           (save-excursion
             (forward-line 1)
             (looking-at "^═+$")))
      0)
     ;; Check if we're on a ─── underline (major section level 1)
     ((looking-at "^─+$")
      1)
     ;; Check if we're on a major section heading (followed by ───)
     ((and (looking-at "^[A-Z][a-zA-Z ]+$")
           (not (eobp))
           (save-excursion
             (forward-line 1)
             (looking-at "^─+$")))
      1)
     ;; Check if we're on a markdown heading (##+ )
     ((looking-at "^\\(#+\\)\\s-+")
      (length (match-string 1)))
     ;; Not at a heading
     (t nil))))

(defun beads-show-outline-next ()
  "Move to next heading at any level."
  (interactive)
  (let ((start-pos (point))
        (start-level (beads-show--section-level))
        (found nil))
    ;; Skip current heading entirely (including underline if present)
    (forward-line 1)
    (when (and start-level (looking-at "^[═─]+$"))
      (forward-line 1))
    ;; Search for next heading
    (while (and (not found) (not (eobp)))
      (let ((level (beads-show--section-level)))
        (when level
          ;; Make sure it's a real heading line, not an underline
          (unless (looking-at "^[═─]+$")
            (setq found t))))
      (unless found
        (forward-line 1)))
    (if found
        (recenter-top-bottom 0)
      (goto-char start-pos)
      (message "No next heading"))))

(defun beads-show-outline-previous ()
  "Move to previous heading at any level."
  (interactive)
  (let ((start-pos (point))
        (start-level (beads-show--section-level))
        (found nil))
    ;; Move back at least one line to start search
    (beginning-of-line)
    (forward-line -1)
    ;; If we were on an underline, skip past its heading text
    (when (and start-level (looking-at "^[═─]+$"))
      (forward-line -1))
    ;; Search for previous heading
    (while (and (not found) (not (bobp)))
      (let ((level (beads-show--section-level)))
        (when level
          ;; Make sure it's a real heading line, not an underline
          (unless (looking-at "^[═─]+$")
            (setq found t))))
      (unless found
        (forward-line -1)))
    (if found
        (recenter-top-bottom 0)
      (goto-char start-pos)
      (message "No previous heading"))))

(defun beads-show-outline-next-same-level ()
  "Move to next heading at the same level."
  (interactive)
  (let ((current-level (beads-show--section-level))
        (start-pos (point))
        (found nil))
    (unless current-level
      (user-error "Not at a heading"))
    ;; Skip current heading entirely (including underline if present)
    (forward-line 1)
    (when (looking-at "^[═─]+$")
      (forward-line 1))
    ;; Search for next same-level heading
    (while (and (not found) (not (eobp)))
      (let ((level (beads-show--section-level)))
        (cond
         ((and (eq level current-level) (not (looking-at "^[═─]+$")))
          (setq found t))
         ((and level (< level current-level) (not (looking-at "^[═─]+$")))
          ;; Hit a higher-level heading, stop searching
          (setq found 'stop))
         (t
          (forward-line 1)))))
    (if (eq found t)
        (recenter-top-bottom 0)
      (goto-char start-pos)
      (message "No next heading at same level"))))

(defun beads-show-outline-previous-same-level ()
  "Move to previous heading at the same level."
  (interactive)
  (let ((current-level (beads-show--section-level))
        (start-pos (point))
        (found nil))
    (unless current-level
      (user-error "Not at a heading"))
    ;; Move back at least one line
    (beginning-of-line)
    (forward-line -1)
    ;; If on underline, skip past it
    (when (looking-at "^[═─]+$")
      (forward-line -1))
    ;; Search backwards
    (while (and (not found) (not (bobp)))
      (let ((level (beads-show--section-level)))
        (cond
         ((and (eq level current-level) (not (looking-at "^[═─]+$")))
          (setq found t))
         ((and level (< level current-level) (not (looking-at "^[═─]+$")))
          ;; Hit a higher-level heading, stop searching
          (setq found 'stop))
         (t
          (forward-line -1)))))
    (if (eq found t)
        (recenter-top-bottom 0)
      (goto-char start-pos)
      (message "No previous heading at same level"))))

(defun beads-show-outline-up ()
  "Move to parent heading (next higher level)."
  (interactive)
  (let ((current-level (beads-show--section-level))
        (start-pos (point))
        (found nil))
    (unless current-level
      (user-error "Not at a heading"))
    (when (zerop current-level)
      (user-error "Already at top level"))
    ;; Move back at least one line
    (beginning-of-line)
    (forward-line -1)
    ;; If on underline, skip past it
    (when (looking-at "^[═─]+$")
      (forward-line -1))
    ;; Search backwards for parent
    (while (and (not found) (not (bobp)))
      (let ((level (beads-show--section-level)))
        (when (and level (< level current-level) (not (looking-at "^[═─]+$")))
          (setq found t)))
      (unless found
        (forward-line -1)))
    (if found
        (recenter-top-bottom 0)
      (goto-char start-pos)
      (message "No parent heading"))))

;;; Buffer Rendering

(defun beads-show--render-issue (issue)
  "Render ISSUE data into current buffer."
  (let ((inhibit-read-only t)
        (id (alist-get 'id issue))
        (title (alist-get 'title issue))
        (status (alist-get 'status issue))
        (priority (alist-get 'priority issue))
        (type (alist-get 'issue-type issue))
        (created (alist-get 'created-at issue))
        (updated (alist-get 'updated-at issue))
        (closed (alist-get 'closed-at issue))
        (assignee (alist-get 'assignee issue))
        (external-ref (alist-get 'external-ref issue))
        (description (alist-get 'description issue))
        (acceptance (alist-get 'acceptance-criteria issue))
        (design (alist-get 'design issue))
        (notes (alist-get 'notes issue)))

    (erase-buffer)

    ;; Title
    (insert (propertize (or title "Untitled")
                       'face '(:inherit bold :height 1.5)))
    (insert "\n")
    (insert (propertize (make-string (length (or title "Untitled")) ?═)
                       'face 'beads-show-header-face))
    (insert "\n\n")

    ;; Metadata
    (beads-show--insert-header "ID" id)
    (beads-show--insert-header "Status"
                              (beads-show--format-status status)
                              'beads-show-value-face)
    (when priority
      (beads-show--insert-header "Priority"
                                (beads-show--format-priority priority)
                                'beads-show-value-face))
    (when type
      (beads-show--insert-header "Type" (upcase type)))
    (when assignee
      (beads-show--insert-header "Assignee" assignee))
    (when external-ref
      (beads-show--insert-header "External Ref" external-ref))
    (beads-show--insert-header "Created" (beads-show--format-date created))
    (beads-show--insert-header "Updated" (beads-show--format-date updated))
    (when (and closed (not (string-empty-p closed)))
      (beads-show--insert-header "Closed" (beads-show--format-date closed)))

    ;; Text sections
    (beads-show--insert-section "Description" description)
    (beads-show--insert-section "Acceptance Criteria" acceptance)
    (beads-show--insert-section "Design" design)
    (beads-show--insert-section "Notes" notes)

    ;; Footer
    (insert beads-show-section-separator)
    (insert (propertize "Press 'g' to refresh, 'q' to quit, RET on bd-N to jump"
                       'face 'shadow))
    (insert "\n")

    (goto-char (point-min))))

;;; Commands

;;;###autoload
(defun beads-show (issue-id)
  "Show detailed view of issue with ISSUE-ID.
Creates or switches to a buffer showing the full issue details."
  (interactive
   (list (completing-read "Show issue: "
                         (beads--issue-completion-table)
                         nil t nil 'beads--issue-id-history)))
  (let* ((buffer-name (format "*beads-show: %s*" issue-id))
         (buffer (get-buffer-create buffer-name))
         (project-dir default-directory))  ; Capture current project context
    (with-current-buffer buffer
      (beads-show-mode)
      (setq beads-show--issue-id issue-id)
      ;; Preserve project context in show buffer
      (setq default-directory project-dir)

      (condition-case err
          (let ((issue-json (beads--run-command "show" issue-id)))
            (setq beads-show--issue-data (beads--parse-issue issue-json))
            (beads-show--render-issue beads-show--issue-data))
        (error
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize "Error loading issue\n\n"
                             'face 'error))
           (insert (format "%s" (error-message-string err)))
           (goto-char (point-min))))))

    (switch-to-buffer buffer)))

;;;###autoload
(defun beads-show-at-point ()
  "Show issue detail for bd-N reference at point.
Extracts the issue ID from text at point and calls `beads-show'."
  (interactive)
  (if-let ((issue-id (beads-show--extract-issue-at-point)))
      (beads-show issue-id)
    (user-error "No issue reference found at point")))

;;;###autoload
(defun beads-refresh-show ()
  "Refresh the current show buffer from bd CLI."
  (interactive)
  (unless (derived-mode-p 'beads-show-mode)
    (user-error "Not in a beads-show buffer"))
  (unless beads-show--issue-id
    (user-error "No issue ID associated with this buffer"))

  (let ((pos (point)))
    (condition-case err
        (let ((issue-json (beads--run-command "show" beads-show--issue-id)))
          (setq beads-show--issue-data (beads--parse-issue issue-json))
          (beads-show--render-issue beads-show--issue-data)
          (goto-char (min pos (point-max)))
          (message "Refreshed %s" beads-show--issue-id))
      (error
       (message "Failed to refresh: %s" (error-message-string err))))))

(defun beads-show-next-section ()
  "Move to the next section in the show buffer."
  (interactive)
  (let ((section-regexp "^[A-Z][a-zA-Z ]+\n─+$"))
    (if (re-search-forward section-regexp nil t)
        (progn
          (forward-line 2)
          (recenter-top-bottom 0))
      (message "No next section"))))

(defun beads-show-previous-section ()
  "Move to the previous section in the show buffer."
  (interactive)
  (let ((section-regexp "^[A-Z][a-zA-Z ]+\n─+$"))
    (if (re-search-backward section-regexp nil t)
        (progn
          (forward-line 2)
          (recenter-top-bottom 0))
      (message "No previous section"))))

(defun beads-show-follow-reference ()
  "Follow bd-N reference at point or on current line."
  (interactive)
  (if-let ((issue-id (beads-show--extract-issue-at-point)))
      (beads-show issue-id)
    (message "No issue reference at point")))

(defun beads-show-follow-reference-other-window ()
  "Follow bd-N reference at point in other window."
  (interactive)
  (if-let ((issue-id (beads-show--extract-issue-at-point)))
      (let ((buffer-name (format "*beads-show: %s*" issue-id)))
        (if-let ((buf (get-buffer buffer-name)))
            (switch-to-buffer-other-window buf)
          ;; Buffer doesn't exist, create it
          (let ((project-dir default-directory))
            (with-current-buffer (get-buffer-create buffer-name)
              (beads-show-mode)
              (setq beads-show--issue-id issue-id)
              (setq default-directory project-dir)
              (condition-case err
                  (let ((issue-json (beads--run-command "show" issue-id)))
                    (setq beads-show--issue-data (beads--parse-issue issue-json))
                    (beads-show--render-issue beads-show--issue-data))
                (error
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert (propertize "Error loading issue\n\n"
                                     'face 'error))
                   (insert (format "%s" (error-message-string err)))
                   (goto-char (point-min))))))
            (switch-to-buffer-other-window buffer-name))))
    (message "No issue reference at point")))

(defun beads-show-next-reference ()
  "Jump to next bd-N reference in buffer."
  (interactive)
  (let ((start-pos (point))
        (found nil))
    (save-excursion
      ;; Move past current reference if we're on one
      (when (beads-show--extract-issue-at-point)
        (re-search-forward "bd-[0-9]+" nil t))
      ;; Search for next reference
      (when (re-search-forward "\\(bd-[0-9]+\\)" nil t)
        (setq found (match-beginning 1))))
    (if found
        (goto-char found)
      (message "No next reference"))))

(defun beads-show-previous-reference ()
  "Jump to previous bd-N reference in buffer."
  (interactive)
  (let ((start-pos (point))
        (found nil))
    (save-excursion
      ;; Move before current reference if we're on one
      (when (beads-show--extract-issue-at-point)
        (goto-char (line-beginning-position)))
      ;; Search for previous reference
      (when (re-search-backward "\\(bd-[0-9]+\\)" nil t)
        (setq found (match-beginning 1))))
    (if found
        (goto-char found)
      (message "No previous reference"))))

(defun beads-show-next-button ()
  "Jump to next clickable element (button) in buffer."
  (interactive)
  (let ((next-button (next-button (point))))
    (if next-button
        (goto-char (button-start next-button))
      ;; Wrap around to beginning
      (let ((first-button (next-button (point-min))))
        (if first-button
            (progn
              (goto-char (button-start first-button))
              (message "Wrapped to first button"))
          (message "No buttons in buffer"))))))

(defun beads-show-previous-button ()
  "Jump to previous clickable element (button) in buffer."
  (interactive)
  (let ((prev-button (previous-button (point))))
    (if prev-button
        (goto-char (button-start prev-button))
      ;; Wrap around to end
      (let ((last-button (previous-button (point-max))))
        (if last-button
            (progn
              (goto-char (button-start last-button))
              (message "Wrapped to last button"))
          (message "No buttons in buffer"))))))

;;; Field Editing

(defun beads-show--edit-field-multiline (field-name current-value callback)
  "Edit FIELD-NAME in a multiline buffer.
CURRENT-VALUE is the initial text, CALLBACK is called with result."
  (let* ((buffer-name (format "*beads-edit-%s*" (downcase field-name)))
         (buffer (generate-new-buffer buffer-name))
         (parent-buffer (current-buffer))
         (parent-issue-id beads-show--issue-id)
         (project-dir default-directory))
    (switch-to-buffer buffer)
    (when current-value
      (insert current-value))
    ;; Use markdown-mode if available, otherwise text-mode
    (if (fboundp 'markdown-mode)
        (markdown-mode)
      (text-mode))
    ;; Enable visual-line-mode for better editing
    (visual-line-mode 1)
    (setq header-line-format
          (format "Edit %s: C-c C-c to save, C-c C-k to cancel"
                  field-name))
    (setq default-directory project-dir)
    ;; Set up keybindings
    (let ((finish-func (lambda ()
                        (interactive)
                        (let ((text (buffer-substring-no-properties
                                   (point-min) (point-max))))
                          (kill-buffer)
                          (switch-to-buffer parent-buffer)
                          (funcall callback text)
                          (message "%s saved" field-name))))
          (cancel-func (lambda ()
                        (interactive)
                        (kill-buffer)
                        (switch-to-buffer parent-buffer)
                        (message "%s edit cancelled" field-name))))
      (local-set-key (kbd "C-c C-c") finish-func)
      (local-set-key (kbd "C-c C-k") cancel-func))
    (message "Edit %s. C-c C-c to save, C-c C-k to cancel." field-name)))

(defun beads-show--update-field (field-name field-flag new-value)
  "Update FIELD-NAME using FIELD-FLAG with NEW-VALUE via bd update."
  (unless beads-show--issue-id
    (user-error "No issue ID in current buffer"))
  (condition-case err
      (progn
        (beads--run-command "update" beads-show--issue-id
                           field-flag new-value)
        ;; Update local issue data
        (let ((field-symbol (intern (concat ":"
                                           (replace-regexp-in-string
                                            "-" "_"
                                            (downcase field-name))))))
          (setf (alist-get (intern (replace-regexp-in-string
                                   "-" "_"
                                   (downcase field-name)))
                          beads-show--issue-data)
                new-value))
        ;; Refresh the display
        (beads-refresh-show)
        (message "%s updated" field-name))
    (error
     (message "Failed to update %s: %s"
              field-name (error-message-string err)))))

;;;###autoload
(defun beads-show-edit-field ()
  "Edit a field of the current issue.
Prompts for field to edit, opens editing buffer with C-c C-c to save."
  (interactive)
  (unless (derived-mode-p 'beads-show-mode)
    (user-error "Not in a beads-show buffer"))
  (unless beads-show--issue-id
    (user-error "No issue ID associated with this buffer"))
  (unless beads-show--issue-data
    (user-error "No issue data available"))

  (let* ((fields '(("Title" . title)
                  ("Description" . description)
                  ("Acceptance Criteria" . acceptance-criteria)
                  ("Design" . design)
                  ("Notes" . notes)))
         (field-name (completing-read "Edit field: "
                                     (mapcar #'car fields)
                                     nil t))
         (field-key (cdr (assoc field-name fields)))
         (current-value (alist-get field-key beads-show--issue-data))
         (is-title (eq field-key 'title)))

    (if is-title
        ;; Title is single-line
        (let ((new-value (read-string (format "%s: " field-name)
                                      current-value)))
          (beads-show--update-field field-name "--title" new-value))
      ;; Other fields are multiline
      (beads-show--edit-field-multiline
       field-name
       current-value
       (lambda (new-value)
         (let ((flag (pcase field-key
                      ('description "-d")
                      ('acceptance-criteria "--acceptance-criteria")
                      ('design "--design")
                      ('notes "--notes"))))
           (beads-show--update-field field-name flag new-value)))))))

;;; Footer

(provide 'beads-show)
;;; beads-show.el ends here
