;;; beads-show.el --- Issue detail view for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides issue detail display functionality for beads.el.
;; It implements a special-mode-derived buffer that shows full issue
;; details with markdown-like formatting and clickable issue references.
;;
;; Key features:
;; - Formatted display of issue metadata and text fields
;; - Clickable issue references using buttons
;; - Basic text fontification (bold, colors)
;; - Markdown-mode-style outline navigation (C-c C-n/p/f/b/u)
;; - Inline field editing (C-c C-e)
;; - Simple section navigation (n/p)
;; - Reference and button navigation ([, ], TAB)
;; - Copy issue ID to clipboard (w or C-w)
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
;;
;; Sesman session management (C-c C-s prefix):
;; - C-c C-s s: Start new session
;; - C-c C-s q: Quit current session
;; - C-c C-s r: Restart current session
;; - C-c C-s b: Open session browser
;; - C-c C-s i: Show session info
;; - C-c C-s l: Link session to buffer

;;; Code:

(require 'beads)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-completion)
(require 'beads-agent)
(require 'beads-sesman)
(require 'button)
(require 'cl-lib)
(require 'goto-addr)
(require 'xref)
(require 'bookmark)

;;; Customization

(defgroup beads-show nil
  "Issue detail view settings."
  :group 'beads
  :prefix "beads-show-")

(defcustom beads-show-section-separator "\n"
  "Separator between sections in show buffer."
  :type 'string
  :group 'beads-show)

(defcustom beads-show-wrap-lines t
  "Whether to wrap long lines in show buffers."
  :type 'boolean
  :group 'beads-show)

(defcustom beads-show-dependency-title-max-length 50
  "Maximum length for dependency titles before truncation.
Set to nil to disable truncation."
  :type '(choice (integer :tag "Maximum length")
                 (const :tag "No truncation" nil))
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

(defface beads-show-sub-issue-id-face
  '((t :inherit link))
  "Face for sub-issue IDs in epic view."
  :group 'beads-show)

(defface beads-show-sub-issue-title-face
  '((t :inherit default))
  "Face for sub-issue titles in epic view."
  :group 'beads-show)

(defface beads-show-sub-issue-progress-face
  '((t :inherit font-lock-comment-face))
  "Face for sub-issue progress summary."
  :group 'beads-show)

(defface beads-show-label-tag-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for label tags/badges."
  :group 'beads-show)

(defface beads-show-dependency-arrow-face
  '((t :inherit font-lock-keyword-face))
  "Face for dependency arrows (→, ↳)."
  :group 'beads-show)

;;; Variables

(defvar-local beads-show--issue-id nil
  "Issue ID displayed in current show buffer.")

(defvar-local beads-show--issue-data nil
  "Full issue data for current show buffer.")

(defvar-local beads-show--project-dir nil
  "Project directory for bd command execution.
THIS IS PART OF THE IDENTITY - buffer is keyed by (issue-id, project-dir).
Set from the caller's directory when the buffer is created or refreshed.
Used to ensure commands run in the correct project context, which is
essential for git worktree support.")

;; Directory-aware buffer identity (beads.el-4pgx)
;; Key principle: Directory is identity, branch is metadata.

(defvar-local beads-show--branch nil
  "Git branch name.
This is METADATA for display, not identity.
Updated on refresh to reflect current branch.")

(defvar-local beads-show--proj-name nil
  "Project name for display.
Used when multiple projects have show buffers open.")

;;; Buffer Lookup by Project Directory
;;
;; Show buffers are identified by (issue-id, project-dir) pair.
;; Same issue in different directories = different buffers.
;; This is important for git worktrees.

(defun beads-show--normalize-directory (dir)
  "Normalize DIR for consistent comparison.
Strips trailing slashes and expands to absolute path.
Uses `expand-file-name' (not `file-truename') for Tramp compatibility."
  (directory-file-name (expand-file-name dir)))

(defun beads-show--find-buffer-for-issue (issue-id &optional project-dir)
  "Find existing show buffer for ISSUE-ID in PROJECT-DIR.
PROJECT-DIR defaults to the current project root.
Return buffer or nil if not found."
  (let ((normalized-dir (beads-show--normalize-directory
                         (or project-dir
                             (beads-git-find-project-root)
                             default-directory))))
    (cl-find-if
     (lambda (buf)
       (with-current-buffer buf
         (and (derived-mode-p 'beads-show-mode)
              beads-show--project-dir
              beads-show--issue-id
              (equal beads-show--issue-id issue-id)
              (equal (beads-show--normalize-directory beads-show--project-dir)
                     normalized-dir))))
     (buffer-list))))

(defun beads-show--find-visible-buffer (&optional project-dir)
  "Find a visible beads-show buffer for PROJECT-DIR in current frame.
If PROJECT-DIR is nil, use current project.
Returns the buffer or nil if none is visible."
  (let* ((proj-dir (beads-show--normalize-directory
                    (or project-dir
                        (beads-git-find-project-root)
                        default-directory))))
    (seq-find (lambda (buf)
                (and (buffer-live-p buf)
                     (get-buffer-window buf)
                     (with-current-buffer buf
                       (and (derived-mode-p 'beads-show-mode)
                            beads-show--project-dir
                            (equal (beads-show--normalize-directory
                                    beads-show--project-dir)
                                   proj-dir)))))
              (buffer-list))))

(defun beads-show--get-or-create-buffer (issue-id &optional title)
  "Get or create show buffer for ISSUE-ID.
TITLE is used for buffer name display (truncated if too long).
Reuses existing buffer for same (project-dir, issue-id) pair.
Buffer is named *beads-show[PROJECT]/ISSUE-ID TITLE*."
  (let* ((project-dir (or (beads-git-find-project-root) default-directory))
         (existing (beads-show--find-buffer-for-issue issue-id project-dir)))
    (or existing
        (let* ((proj-name (beads-git-get-project-name))
               (buf-name (beads-buffer-name-show issue-id title proj-name))
               (buffer (get-buffer-create buf-name)))
          (with-current-buffer buffer
            (setq beads-show--project-dir project-dir)
            (setq beads-show--issue-id issue-id)
            (setq beads-show--branch (beads-git-get-branch))
            (setq beads-show--proj-name proj-name))
          buffer))))

(defun beads-show-update-buffer (issue-id buffer)
  "Update BUFFER to display ISSUE-ID without selecting it.
This is used by `beads-list-follow-mode' to update the show buffer when
navigating in beads-list.  Returns BUFFER."
  (with-current-buffer buffer
    (unless (derived-mode-p 'beads-show-mode)
      (beads-show-mode))
    (setq beads-show--issue-id issue-id)
    (condition-case err
        (let ((issue (beads-command-show! :issue-ids (list issue-id))))
          (setq beads-show--issue-data issue)
          ;; Rename buffer to include title
          (let* ((title (oref issue title))
                 (new-name (beads-buffer-name-show
                            issue-id title beads-show--proj-name)))
            (unless (string= (buffer-name) new-name)
              (rename-buffer new-name t)))
          (beads-show--render-issue beads-show--issue-data))
      (error
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (propertize "Error loading issue\n\n" 'face 'error))
         (insert (format "%s" (error-message-string err)))
         (goto-char (point-min))))))
  buffer)

;;; Worktree Session Integration
;;
;; Show buffers are registered with worktree sessions for lifecycle management.
;; Sessions are keyed by directory, not branch.

(defun beads-show--register-with-session ()
  "Register current buffer with worktree session.
Should be called after the buffer is fully initialized."
  (when-let ((session (beads-sesman--ensure-worktree-session)))
    (beads-worktree-session-add-buffer session (current-buffer))))

(defun beads-show--unregister-from-session ()
  "Remove current buffer from worktree session.
Called from `kill-buffer-hook' to clean up session state."
  (when-let ((session (beads-sesman--buffer-worktree-session (current-buffer))))
    (beads-worktree-session-remove-buffer session (current-buffer))
    (beads-sesman--maybe-cleanup-worktree-session session)))

;;; Mode Definition

(defvar beads-show-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Refresh/quit
    (define-key map (kbd "g") #'beads-refresh-show)
    (define-key map (kbd "q") #'quit-window)

    ;; Copy issue ID
    (define-key map (kbd "w") #'beads-show-copy-id)        ; copy (like eww, info)
    (define-key map (kbd "C-w") #'beads-show-copy-id)      ; copy (override kill-region)

    ;; Section navigation (simple)
    (define-key map (kbd "n") #'beads-show-next-section)
    (define-key map (kbd "p") #'beads-show-previous-section)

    ;; Paragraph navigation (markdown-mode style)
    (define-key map (kbd "M-{") #'beads-show-backward-paragraph)
    (define-key map (kbd "M-}") #'beads-show-forward-paragraph)
    (define-key map (kbd "M-h") #'beads-show-mark-paragraph)

    ;; Block navigation (markdown-mode style)
    (define-key map (kbd "C-M-{") #'beads-show-backward-block)
    (define-key map (kbd "C-M-}") #'beads-show-forward-block)

    ;; Section boundary navigation (markdown-mode style)
    (define-key map (kbd "C-M-a") #'beads-show-beginning-of-section)
    (define-key map (kbd "C-M-e") #'beads-show-end-of-section)
    (define-key map (kbd "C-M-h") #'beads-show-mark-section)

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

    ;; Markdown-mode-style aliases for reference navigation
    (define-key map (kbd "M-n") #'beads-show-next-reference)
    (define-key map (kbd "M-p") #'beads-show-previous-reference)

    ;; Follow reference
    (define-key map (kbd "RET") #'beads-show-follow-reference)
    (define-key map (kbd "C-c C-o") #'beads-show-follow-reference)  ; markdown-mode alias
    (define-key map (kbd "o") #'beads-show-follow-reference-other-window)

    ;; AI Agent type commands
    (define-key map (kbd "T") #'beads-agent-start-task)     ; Task agent
    (define-key map (kbd "R") #'beads-agent-start-review)   ; Review agent
    (define-key map (kbd "P") #'beads-agent-start-plan)     ; Plan agent
    (define-key map (kbd "Q") #'beads-agent-start-qa)       ; QA agent
    (define-key map (kbd "C") #'beads-agent-start-custom)   ; Custom agent
    (define-key map (kbd "X") #'beads-agent-stop-at-point)  ; Stop agent
    (define-key map (kbd "J") #'beads-agent-jump-at-point)  ; Jump to agent
    (define-key map (kbd "A") #'beads-agent-start-at-point) ; Backward compat

    ;; Sesman session management (CIDER/ESS convention)
    (define-key map (kbd "C-c C-s") beads-sesman-map)

    ;; Quick actions transient
    (define-key map (kbd "?") #'beads-show-actions)
    (define-key map (kbd "s") #'beads-show-set-status)
    (define-key map (kbd "e") #'beads-show-edit-field)
    map)
  "Keymap for `beads-show-mode'.")

;; Imenu expression must be defined before mode definition
(defvar beads-show-imenu-expression
  '(("Sections" "^\\([A-Z][A-Z ]+\\)$" 1)
    ("Headings" "^\\(##+ .+\\)$" 1))
  "Imenu generic expression for beads-show buffers.")

(define-derived-mode beads-show-mode special-mode "Beads-Show"
  "Major mode for displaying Beads issue details.

Key bindings:
\\{beads-show-mode-map}"
  (setq truncate-lines (not beads-show-wrap-lines))
  (setq buffer-read-only t)
  ;; Register cleanup hook for worktree session integration
  (add-hook 'kill-buffer-hook #'beads-show--unregister-from-session nil t)
  ;; Imenu integration for section navigation
  (setq-local imenu-generic-expression beads-show-imenu-expression)
  (setq-local imenu-create-index-function #'beads-show--imenu-create-index)
  ;; Which-func-mode integration
  (setq-local which-func-functions '(beads-show--which-func))
  ;; Eldoc integration for issue ID hover
  (add-hook 'eldoc-documentation-functions #'beads-show--eldoc-function nil t)
  ;; Xref backend for M-. on issue IDs
  (add-hook 'xref-backend-functions #'beads-show--xref-backend nil t)
  ;; Outline-minor-mode support for folding
  (setq-local outline-regexp "^[A-Z][A-Z ]+$\\|^##+ ")
  (setq-local outline-level #'beads-show--outline-level)
  ;; Bookmark support
  (setq-local bookmark-make-record-function #'beads-show--bookmark-make-record))

;;; Imenu Integration

(defun beads-show--imenu-create-index ()
  "Create imenu index for beads-show buffer.
Returns alist of (NAME . POSITION) for sections."
  (let ((index nil))
    (save-excursion
      (goto-char (point-min))
      ;; Find main sections (DEPENDS ON, CHILDREN, BLOCKS, Notes, etc.)
      (while (re-search-forward "^\\([A-Z][A-Za-z ]+\\)$" nil t)
        (let ((name (match-string 1))
              (pos (match-beginning 0)))
          ;; Skip the title line separator (═══)
          (unless (string-match "^═+$" name)
            (push (cons name pos) index))))
      ;; Find markdown headings in content
      (goto-char (point-min))
      (while (re-search-forward "^\\(##+ .+\\)$" nil t)
        (let ((name (match-string 1))
              (pos (match-beginning 0)))
          (push (cons name pos) index))))
    (nreverse index)))

;;; Which-func-mode Integration

(defun beads-show--which-func ()
  "Return name of current section for which-func-mode."
  (save-excursion
    (beginning-of-line)
    ;; Search backward for section header
    (when (re-search-backward "^\\([A-Z][A-Za-z ]+\\)$\\|^\\(##+ .+\\)$" nil t)
      (or (match-string 1) (match-string 2)))))

;;; Eldoc Integration

(defun beads-show--eldoc-function (callback)
  "Eldoc function for issue IDs in beads-show buffers.
CALLBACK is called with the documentation string."
  (when-let* ((issue-id (beads-show--extract-issue-at-point)))
    (condition-case nil
        (let* ((issue (beads-command-show! :issue-ids (list issue-id)))
               (title (oref issue title))
               (status (oref issue status))
               (priority (oref issue priority))
               (doc (format "%s: %s [%s P%s]"
                            issue-id
                            (or title "Untitled")
                            (upcase (or status "?"))
                            (or priority "?"))))
          (funcall callback doc))
      (error nil))))

;;; Xref Integration

(defun beads-show--xref-backend ()
  "Return xref backend for beads-show mode."
  'beads-show)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql beads-show)))
  "Return issue ID at point for xref."
  (beads-show--extract-issue-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql beads-show)) identifier)
  "Return xref definition for IDENTIFIER (issue ID)."
  (when identifier
    (list (xref-make
           (format "Issue %s" identifier)
           (xref-make-buffer-location
            (beads-show--get-or-create-buffer identifier)
            1)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql beads-show)))
  "Return completion table for issue IDs."
  (beads-completion-issue-table))

;;; Outline Integration

(defun beads-show--outline-level ()
  "Return outline level for current line."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Main sections (DEPENDS ON, CHILDREN, etc.) are level 1
     ((looking-at "^[A-Z][A-Z ]+$") 1)
     ;; Markdown headings: ## = 2, ### = 3, etc.
     ((looking-at "^\\(#+\\) ")
      (1+ (length (match-string 1))))
     ;; Default
     (t 0))))

;;; Bookmark Integration

(defun beads-show--bookmark-make-record ()
  "Create bookmark record for current beads-show buffer."
  `(,(format "beads:%s" beads-show--issue-id)
    (issue-id . ,beads-show--issue-id)
    (project-dir . ,beads-show--project-dir)
    (handler . beads-show--bookmark-handler)))

(defun beads-show--bookmark-handler (bookmark)
  "Handler to restore BOOKMARK for beads-show buffer."
  (let ((issue-id (bookmark-prop-get bookmark 'issue-id))
        (project-dir (bookmark-prop-get bookmark 'project-dir)))
    (when issue-id
      (let ((default-directory (or project-dir default-directory)))
        (beads-show issue-id)))))

;;; Desktop Integration

(defun beads-show--desktop-buffer-misc-data (_desktop-dirname)
  "Return misc data for desktop save."
  (list beads-show--issue-id beads-show--project-dir))

(defun beads-show--desktop-restore-buffer (_file-name _buffer-name misc-data)
  "Restore beads-show buffer from desktop MISC-DATA."
  (when misc-data
    (let ((issue-id (car misc-data))
          (project-dir (cadr misc-data)))
      (when issue-id
        (let ((default-directory (or project-dir default-directory)))
          (beads-show issue-id)
          (current-buffer))))))

(add-to-list 'desktop-buffer-mode-handlers
             '(beads-show-mode . beads-show--desktop-restore-buffer))

;;; Org-link Integration

(with-eval-after-load 'org
  (org-link-set-parameters
   "beads"
   :follow #'beads-show--org-link-follow
   :export #'beads-show--org-link-export
   :store #'beads-show--org-link-store))

(defun beads-show--org-link-follow (issue-id _arg)
  "Follow beads link to ISSUE-ID."
  (beads-show issue-id))

(defun beads-show--org-link-export (issue-id description backend _info)
  "Export beads link for ISSUE-ID with DESCRIPTION for BACKEND."
  (let ((desc (or description issue-id)))
    (pcase backend
      ('html (format "<code>%s</code>" desc))
      ('latex (format "\\texttt{%s}" desc))
      ('ascii desc)
      (_ desc))))

(defun beads-show--org-link-store ()
  "Store org link to current issue."
  (when (derived-mode-p 'beads-show-mode)
    (when beads-show--issue-id
      (let ((title (when beads-show--issue-data
                     (oref beads-show--issue-data title))))
        (org-link-store-props
         :type "beads"
         :link (concat "beads:" beads-show--issue-id)
         :description (or title beads-show--issue-id))))))

;;; Utility Functions

(defun beads-show--status-icon (status)
  "Return Unicode status icon for STATUS.
Icons match the bd CLI output:
  ○ = open
  ◐ = in_progress
  ● = closed
  ✗ = blocked"
  (pcase status
    ("open" "○")
    ("in_progress" "◐")
    ("closed" "●")
    ("blocked" "✗")
    (_ "?")))

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

(defun beads-show--format-date (date-string &optional short)
  "Format DATE-STRING for display.
If SHORT is non-nil, return just the date portion (YYYY-MM-DD).
The full timestamp is available via help-echo."
  (if date-string
      (let* ((cleaned (replace-regexp-in-string "\\.[0-9]+.*$" ""
                                                date-string))
             (without-z (replace-regexp-in-string "Z$" "" cleaned))
             (full-date (replace-regexp-in-string "T" " " without-z))
             (short-date (if (string-match "^\\([0-9]+-[0-9]+-[0-9]+\\)" full-date)
                             (match-string 1 full-date)
                           full-date)))
        (if short
            (propertize short-date
                        'help-echo (concat "Full timestamp: " full-date)
                        'mouse-face 'highlight)
          full-date))
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
CONTENT can be a string or nil (empty sections are skipped).
Section header is uppercase without underline, matching DEPENDS ON style."
  (when (and content (not (string-empty-p (string-trim content))))
    (insert beads-show-section-separator)
    (insert (propertize (upcase title) 'face 'beads-show-header-face))
    (insert "\n\n")
    (let ((start (point)))
      (insert content)
      (insert "\n")
      ;; Apply markdown-like fontification
      (beads-show--fontify-markdown start (point))
      ;; Make issue references clickable
      (beads-show--buttonize-references start (point)))))

(defun beads-show--insert-agent-section (issue-id)
  "Insert agent sessions section for ISSUE-ID if sessions exist."
  (when (and (fboundp 'beads-agent--get-sessions-for-issue)
             (fboundp 'beads-agent--session-active-p))
    (let ((sessions (beads-agent--get-sessions-for-issue issue-id)))
      (when sessions
        (insert beads-show-section-separator)
        (insert (propertize "Agent Sessions" 'face 'beads-show-header-face))
        (insert "\n")
        (insert (propertize (make-string 14 ?─) 'face 'beads-show-header-face))
        (insert "\n\n")
        (dolist (session sessions)
          (let* ((backend (or (beads-agent-session-backend-name session)
                              "unknown"))
                 (started (beads-agent-session-started-at session))
                 (active (beads-agent--session-active-p session))
                 (status-str (if active
                                 (propertize "active" 'face 'success)
                               (propertize "stopped" 'face 'shadow))))
            (insert (format "  %s: %s [%s]\n"
                            (propertize backend 'face 'font-lock-constant-face)
                            (beads-show--format-date started)
                            status-str))))
        (insert "\n")))))

(defun beads-show--get-sub-issues (epic-id)
  "Fetch sub-issues for EPIC-ID.
Returns a list of alists containing sub-issue data, or nil if no sub-issues.
Each alist contains: id, title, status, priority, issue_type."
  (condition-case nil
      (let* ((tree-data (beads-command-dep-tree!
                         :issue-id epic-id
                         :reverse t
                         :max-depth 1))
             ;; tree-data is a vector or list of alists
             (items (if (vectorp tree-data)
                        (append tree-data nil)
                      tree-data)))
        ;; Filter to only include direct children (depth = 1)
        (seq-filter (lambda (item)
                      (let ((depth (alist-get 'depth item)))
                        (and depth (= depth 1))))
                    items))
    (error nil)))

(defun beads-show--format-sub-issue-status (status)
  "Return formatted STATUS string with face, like completion annotations."
  (propertize (upcase status)
              'face (pcase status
                      ("open" 'success)
                      ("in_progress" 'warning)
                      ("blocked" 'error)
                      ("closed" 'shadow)
                      (_ 'default))))

(defun beads-show--truncate-title (title max-len)
  "Truncate TITLE to MAX-LEN characters with ellipsis if needed."
  (if (and title (> (length title) max-len))
      (concat (substring title 0 (- max-len 3)) "...")
    (or title "Untitled")))

(defun beads-show--format-progress-bar (completed total &optional width)
  "Format a visual progress bar for COMPLETED out of TOTAL items.
WIDTH defaults to 20 characters.  Returns a string like [████░░░░░░] 5/10."
  (let* ((w (or width 20))
         (pct (if (zerop total) 0 (/ (* 100.0 completed) total)))
         (filled (round (* w (/ pct 100.0))))
         (empty (- w filled))
         (bar (concat (make-string filled ?█) (make-string empty ?░))))
    (concat
     (propertize "[" 'face 'shadow)
     (propertize bar 'face (if (= completed total) 'success 'warning))
     (propertize "]" 'face 'shadow)
     " "
     (propertize (format "%d/%d" completed total)
                 'face 'beads-show-sub-issue-progress-face))))

(defun beads-show--insert-sub-issues-section (epic-id)
  "Insert sub-issues section for EPIC-ID if it has children.
Shows direct child issues in CLI-style format with progress bar:
  ↳ ○ ID: Title ● P#"
  (when-let* ((sub-issues (beads-show--get-sub-issues epic-id)))
    (let* ((total (length sub-issues))
           (closed (seq-count (lambda (item)
                                (equal (alist-get 'status item) "closed"))
                              sub-issues))
           ;; Group by status for organized display
           (by-status (seq-group-by (lambda (item)
                                      (alist-get 'status item))
                                    sub-issues))
           (status-order '("in_progress" "open" "blocked" "closed")))
      (insert beads-show-section-separator)
      (insert (propertize "CHILDREN" 'face 'beads-show-header-face))
      (insert "  ")
      ;; Visual progress bar
      (insert (beads-show--format-progress-bar closed total 15))
      (insert "\n\n")
      ;; Display sub-issues grouped by status - CLI style
      (dolist (status status-order)
        (when-let* ((issues (alist-get status by-status nil nil #'equal)))
          (dolist (issue issues)
            (let* ((id (alist-get 'id issue))
                   (title (alist-get 'title issue))
                   (issue-status (alist-get 'status issue))
                   (priority (or (alist-get 'priority issue) 2))
                   (issue-type (or (alist-get 'issue_type issue) "task"))
                   (icon (beads-show--status-icon issue-status))
                   (icon-face (pcase issue-status
                                ("open" 'beads-show-status-open-face)
                                ("in_progress" 'beads-show-status-in-progress-face)
                                ("blocked" 'beads-show-status-blocked-face)
                                ("closed" 'beads-show-status-closed-face)
                                (_ 'default))))
              ;; CLI-style format: ↳ ○ ID: Title ● P#
              (insert "  ")
              (insert (propertize "↳" 'face 'beads-show-dependency-arrow-face))
              (insert " ")
              (insert (propertize icon 'face icon-face))
              (insert " ")
              ;; Insert ID as button
              (let ((id-start (point)))
                (insert id)
                (make-button id-start (point)
                             'issue-id id
                             'action #'beads-show--button-action
                             'follow-link t
                             'help-echo (format "Show %s" id)
                             'face 'link))
              (insert ": ")
              ;; Type badge for non-task types
              (unless (string= issue-type "task")
                (insert (propertize (format "(%s) " (upcase issue-type))
                                    'face 'beads-show-label-tag-face)))
              ;; Title (truncated)
              (insert (beads-show--truncate-title title 45))
              ;; Priority badge
              (insert " ")
              (let ((priority-face (pcase priority
                                     (0 'beads-show-priority-critical-face)
                                     (1 'beads-show-priority-high-face)
                                     (2 'beads-show-priority-medium-face)
                                     (_ 'beads-show-priority-low-face))))
                (insert (propertize (format "● P%d" priority)
                                    'face priority-face)))
              (insert "\n"))))))))

(defun beads-show--fontify-markdown (start end)
  "Apply basic markdown fontification between START and END."
  (save-excursion
    (goto-char start)
    ;; Headings (## Header)
    (while (re-search-forward "^\\(#+\\)\\s-+\\(.+\\)$" end t)
      (let ((_level (length (match-string 1)))
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
  "Make issue references clickable between START and END.
Recognizes issue IDs in the format PROJECT-HASH[.CHILD], where PROJECT can
contain letters, numbers, dots, underscores, and hyphens, HASH is a
hexadecimal string (4-8 characters), and optional .CHILD for hierarchical
child IDs (can be nested like .1.2.3).
Examples: beads.el-7bea, bd-a1b2.1, worker-f14c.2.3, api-3e7a."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\b\\([a-zA-Z][a-zA-Z0-9._-]*-[0-9a-fA-F]+\\(?:\\.[0-9]+\\)*\\)\\b" end t)
      (let ((issue-id (match-string 1)))
        (make-button (match-beginning 1) (match-end 1)
                    'issue-id issue-id
                    'action #'beads-show--button-action
                    'follow-link t
                    'help-echo (format "Jump to %s" issue-id)
                    'face 'link)))))

(defun beads-show--button-action (button)
  "Action for clicking on an issue reference BUTTON."
  (let ((issue-id (button-get button 'issue-id)))
    (beads-show issue-id)))

(defun beads-show--extract-issue-at-point ()
  "Extract issue reference at point.
Returns the issue ID or nil if none found.
Recognizes issue IDs like beads.el-7bea, bd-a1b2.1, worker-f14c.2, etc."
  (let ((case-fold-search nil)
        (original-point (point)))
    (or
     ;; First try to see if we're on a button
     (when-let* ((button (button-at original-point)))
       (button-get button 'issue-id))

     ;; Try to find issue ID on current line around point
     (save-excursion
       (let ((line-start (line-beginning-position))
             (line-end (line-end-position))
             (result nil))
         (goto-char line-start)
         (while (and (not result)
                    (re-search-forward "\\b\\([a-zA-Z][a-zA-Z0-9._-]*-[0-9a-fA-F]+\\(?:\\.[0-9]+\\)*\\)\\b" line-end t))
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
  0 - Title section (first two lines: id: title + status line)
  1 - Major section (UPPERCASE header like DEPENDS ON, DESCRIPTION)
  2+ - Markdown heading (## = 2, ### = 3, etc.)
  nil - Not at a heading"
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Check if we're on line 1 or 2 (title header)
     ;; Line 1: id: title, Line 2: ○ Status  Priority  Type
     ((and (< (line-number-at-pos) 3)
           (or
            ;; Line 1: id: title (starts with word chars followed by colon)
            (looking-at "^[a-zA-Z0-9_.-]+: ")
            ;; Line 2: starts with status icon (○, ◐, ●, ✓, ✗)
            (looking-at "^[○◐●✓✗] ")))
      0)
     ;; Check if we're on a major section heading (UPPERCASE letters/spaces)
     ;; Examples: DEPENDS ON, DESCRIPTION, CHILDREN, BLOCKS
     ((looking-at "^[A-Z][A-Z ]+$")
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
    ;; Skip current heading entirely
    ;; For title section (level 0), skip past title, status, and blank line
    (if (and start-level (= start-level 0))
        (progn
          ;; Skip to line 4 (past title, status, blank line)
          (goto-char (point-min))
          (forward-line 3))
      ;; For other headings, just skip current line
      (forward-line 1))
    ;; Search for next heading
    (while (and (not found) (not (eobp)))
      (let ((level (beads-show--section-level)))
        (when (and level (> level 0))
          (setq found t)))
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
        (found nil))
    ;; Move back at least one line to start search
    (beginning-of-line)
    (forward-line -1)
    ;; Search for previous heading
    (while (and (not found) (not (bobp)))
      (let ((level (beads-show--section-level)))
        (when level
          (setq found t)))
      (unless found
        (forward-line -1)))
    ;; If we found the title section (level 0), go to line 1
    (when (and found (= (beads-show--section-level) 0))
      (goto-char (point-min)))
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

(defun beads-show--format-title-line (id title status priority issue-type
                                      &optional owner)
  "Format the two-line issue header.
ID is the issue ID, TITLE is the issue title, STATUS is the status string,
PRIORITY is the priority level (0-4), ISSUE-TYPE is the type (task, etc),
and OWNER is the issue creator/owner.

Returns a two-line string:
  Line 1: id: title
  Line 2: ○ Open · P1 · Epic · Owner"
  (let* ((icon (beads-show--status-icon status))
         (icon-face (pcase status
                      ("open" 'beads-show-status-open-face)
                      ("in_progress" 'beads-show-status-in-progress-face)
                      ("blocked" 'beads-show-status-blocked-face)
                      ("closed" 'beads-show-status-closed-face)
                      (_ 'default)))
         (status-display (pcase status
                           ("open" "Open")
                           ("in_progress" "In Progress")
                           ("blocked" "Blocked")
                           ("closed" "Closed")
                           (_ (or status "Unknown"))))
         (priority-str (when priority (format "P%d" priority)))
         (type-str (when issue-type
                     (capitalize issue-type)))
         ;; Build metadata parts for second line
         (meta-parts (delq nil
                           (list
                            (propertize (concat icon " " status-display)
                                        'face icon-face)
                            (when priority-str
                              (propertize priority-str 'face icon-face))
                            (when type-str
                              (propertize type-str
                                          'face 'beads-show-label-tag-face))
                            (when owner
                              (propertize owner 'face 'shadow))))))
    (concat
     ;; Line 1: id: title
     (propertize id 'face 'font-lock-constant-face)
     ": "
     (propertize (or title "Untitled") 'face '(:inherit bold :height 1.3))
     "\n"
     ;; Line 2: metadata
     (string-join meta-parts "  "))))

(defun beads-show--insert-labels (labels)
  "Insert LABELS as visually distinct badges."
  (when labels
    (insert (propertize "Labels" 'face 'beads-show-label-face))
    (insert ": ")
    (let ((first t))
      (dolist (label labels)
        (unless first (insert " "))
        (insert (propertize (format "[%s]" label)
                           'face 'beads-show-label-tag-face))
        (setq first nil)))
    (insert "\n")))

(defun beads-show--insert-dependency-line (dep-id title status priority arrow)
  "Insert a dependency/dependent line with ARROW prefix.
DEP-ID is the issue ID, TITLE is the issue title, STATUS and PRIORITY
are used for display.  ARROW is either \"→\" for dependencies or \"↳\" for
children.

If DEP-ID is nil or empty, the line is not inserted."
  ;; Validate inputs - skip if no valid dep-id
  (when (and dep-id (not (string-empty-p dep-id)))
    (insert "  ")
    (insert (propertize arrow 'face 'beads-show-dependency-arrow-face))
    (insert " ")
    ;; Status icon
    (let ((icon (beads-show--status-icon (or status "unknown")))
          (icon-face (pcase status
                       ("open" 'beads-show-status-open-face)
                       ("in_progress" 'beads-show-status-in-progress-face)
                       ("blocked" 'beads-show-status-blocked-face)
                       ("closed" 'beads-show-status-closed-face)
                       (_ 'default))))
      (insert (propertize icon 'face icon-face)))
    (insert " ")
    ;; Issue ID as button
    (let ((id-start (point)))
      (insert dep-id)
      (make-button id-start (point)
                   'issue-id dep-id
                   'action #'beads-show--button-action
                   'follow-link t
                   'help-echo (format "Show %s" dep-id)
                   'face 'link))
    ;; Title
    (when (and title (not (string-empty-p title)))
      (insert ": ")
      (insert (if beads-show-dependency-title-max-length
                  (beads-show--truncate-title
                   title beads-show-dependency-title-max-length)
                title)))
    ;; Priority badge with appropriate face
    (when priority
      (insert " ")
      (let ((priority-face (pcase priority
                             (0 'beads-show-priority-critical-face)
                             (1 'beads-show-priority-high-face)
                             (2 'beads-show-priority-medium-face)
                             (_ 'beads-show-priority-low-face))))
        (insert (propertize (format "● P%d" priority) 'face priority-face))))
    (insert "\n")))

(defun beads-show--insert-blocker-line (dep-id title status priority issue-type)
  "Insert a line for an issue blocked by the current issue.
DEP-ID is the issue ID, TITLE is the issue title, STATUS, PRIORITY,
and ISSUE-TYPE are used for display.  Uses ← arrow to indicate reverse
relationship (this issue blocks that one).

Format matches CLI: ← ○ ID: (TYPE) Title ● P#"
  (when (and dep-id (not (string-empty-p dep-id)))
    (insert "  ")
    (insert (propertize "←" 'face 'beads-show-dependency-arrow-face))
    (insert " ")
    ;; Status icon
    (let ((icon (beads-show--status-icon (or status "unknown")))
          (icon-face (pcase status
                       ("open" 'beads-show-status-open-face)
                       ("in_progress" 'beads-show-status-in-progress-face)
                       ("blocked" 'beads-show-status-blocked-face)
                       ("closed" 'beads-show-status-closed-face)
                       (_ 'default))))
      (insert (propertize icon 'face icon-face)))
    (insert " ")
    ;; Issue ID as button
    (let ((id-start (point)))
      (insert dep-id)
      (make-button id-start (point)
                   'issue-id dep-id
                   'action #'beads-show--button-action
                   'follow-link t
                   'help-echo (format "Show %s" dep-id)
                   'face 'link))
    (insert ": ")
    ;; Type badge if available
    (when issue-type
      (insert (propertize (format "(%s) " (upcase issue-type))
                          'face 'beads-show-label-tag-face)))
    ;; Title
    (when (and title (not (string-empty-p title)))
      (insert (if beads-show-dependency-title-max-length
                  (beads-show--truncate-title
                   title beads-show-dependency-title-max-length)
                title)))
    ;; Priority badge
    (when priority
      (insert " ")
      (let ((priority-face (pcase priority
                             (0 'beads-show-priority-critical-face)
                             (1 'beads-show-priority-high-face)
                             (2 'beads-show-priority-medium-face)
                             (_ 'beads-show-priority-low-face))))
        (insert (propertize (format "● P%d" priority) 'face priority-face))))
    (insert "\n")))

(defun beads-show--insert-dependencies-section (dependencies)
  "Insert DEPENDS ON section for blocking DEPENDENCIES.
Uses dependency info from bd show --json output directly,
avoiding N+1 queries.  The dependency objects from bd show include
full issue details (title, status, priority)."
  (when dependencies
    ;; Filter for blocking dependencies (blocks and parent-child types)
    (let ((blocking-deps (seq-filter
                          (lambda (dep)
                            (let ((type (oref dep type)))
                              (or (string= type "blocks")
                                  (string= type "parent-child"))))
                          dependencies)))
      (when blocking-deps
        (insert beads-show-section-separator)
        (insert (propertize "DEPENDS ON" 'face 'beads-show-header-face))
        (insert "\n\n")
        (dolist (dep blocking-deps)
          ;; Use dependency info directly from bd show --json
          ;; which includes full issue details via IssueWithDependencyMetadata
          (let ((dep-id (oref dep depends-on-id))
                (title (oref dep title))
                (status (oref dep status))
                (priority (oref dep priority)))
            (beads-show--insert-dependency-line
             dep-id title (or status "unknown") priority "→")))))))

(defun beads-show--insert-blocks-section (dependents)
  "Insert BLOCKS section showing issues blocked by this one.
DEPENDENTS is a list of beads-dependency objects for issues that
depend on the current issue (i.e., issues this one blocks)."
  (when dependents
    ;; Filter for blocking relationships (blocks and parent-child types)
    ;; These are issues that DEPEND ON the current issue
    (let ((blocked-issues (seq-filter
                           (lambda (dep)
                             (let ((type (oref dep type)))
                               (or (string= type "blocks")
                                   (string= type "parent-child"))))
                           dependents)))
      (when blocked-issues
        (insert beads-show-section-separator)
        (insert (propertize "BLOCKS" 'face 'beads-show-header-face))
        (insert "\n\n")
        (dolist (dep blocked-issues)
          ;; For dependents, the dependent issue's ID was mapped to depends-on-id
          ;; by beads-dependency-from-json (it uses 'id' from JSON for this)
          (let ((dep-id (oref dep depends-on-id))
                (title (oref dep title))
                (status (oref dep status))
                (priority (oref dep priority))
                (issue-type (oref dep issue-type)))
            (beads-show--insert-blocker-line
             dep-id title status priority issue-type)))))))

(defun beads-show--render-issue (issue)
  "Render ISSUE data into current buffer.
ISSUE must be a `beads-issue' EIEIO object.
Section order matches CLI: DEPENDS ON → CHILDREN → BLOCKS → text sections."
  (let ((inhibit-read-only t)
        (id (oref issue id))
        (title (oref issue title))
        (status (oref issue status))
        (priority (oref issue priority))
        (type (oref issue issue-type))
        (created (oref issue created-at))
        (updated (oref issue updated-at))
        (closed (oref issue closed-at))
        (assignee (oref issue assignee))
        (owner (oref issue created-by))
        (external-ref (oref issue external-ref))
        (labels (oref issue labels))
        (dependencies (oref issue dependencies))
        (dependents (oref issue dependents))
        (description (oref issue description))
        (acceptance (oref issue acceptance-criteria))
        (design (oref issue design))
        (notes (oref issue notes)))

    (erase-buffer)

    ;; Two-line header:
    ;; bde-go3g: beads.el: Magit-like Emacs interface for Beads
    ;; ○ Open  P1  Epic  Roman Scherer
    (insert (beads-show--format-title-line id title status priority type owner))
    (insert "\n\n")  ; Blank line after header

    ;; Created/Updated line - use short dates with help-echo for full timestamp
    (insert (propertize "Created: " 'face 'shadow))
    (insert (beads-show--format-date created t))
    (insert (propertize "  Updated: " 'face 'shadow))
    (insert (beads-show--format-date updated t))
    (when (and closed (not (string-empty-p closed)))
      (insert (propertize "  Closed: " 'face 'shadow))
      (insert (beads-show--format-date closed t)))
    (insert "\n")

    ;; Assignee if set
    (when assignee
      (beads-show--insert-header "Assignee" assignee))

    ;; External ref if set
    (when external-ref
      (beads-show--insert-header "External Ref" external-ref))

    ;; Labels section
    (beads-show--insert-labels labels)

    ;; === Section order matches CLI ===
    ;; 1. DEPENDS ON - what this issue depends on
    (beads-show--insert-dependencies-section dependencies)

    ;; 2. CHILDREN - sub-issues for epics (with progress bar)
    (when (equal type "epic")
      (beads-show--insert-sub-issues-section id))

    ;; 3. BLOCKS - issues blocked by this one
    (beads-show--insert-blocks-section dependents)

    ;; Text sections
    (beads-show--insert-section "Description" description)
    (beads-show--insert-section "Acceptance Criteria" acceptance)
    (beads-show--insert-section "Design" design)
    (beads-show--insert-section "Notes" notes)

    ;; Agent sessions (if any)
    (beads-show--insert-agent-section id)

    ;; Footer with keybinding hints
    (insert beads-show-section-separator)
    (insert (propertize
             "g:refresh  q:quit  RET:follow  ?:actions  M-.:xref  TAB:next"
             'face 'shadow))
    (insert "\n")

    ;; Enable URL linkification
    (goto-address-mode 1)

    (goto-char (point-min))))

;;; Commands

;;;###autoload
(defun beads-show (issue-id)
  "Show detailed view of issue with ISSUE-ID.
Creates or switches to a buffer showing the full issue details.
Buffer is named *beads-show[PROJECT]/ISSUE-ID TITLE* and is keyed
by (project-dir, issue-id) pair - each issue gets its own buffer.

Commands are executed in the caller's directory context, ensuring
correct project detection (important for git worktrees)."
  (interactive
   (list (beads-completion-read-issue "Show issue: " nil t nil
                                      'beads--issue-id-history)))
  ;; Capture caller's directory for command execution context
  (let* ((caller-dir default-directory)
         (project-dir (or (beads-git-find-project-root) default-directory))
         ;; Get or create buffer keyed by (project-dir, issue-id)
         (buffer (beads-show--get-or-create-buffer issue-id)))
    (with-current-buffer buffer
      (setq default-directory caller-dir)
      (unless (derived-mode-p 'beads-show-mode)
        (beads-show-mode))
      ;; Update directory-aware state
      (setq beads-show--issue-id issue-id
            beads-show--project-dir project-dir
            beads-show--branch (beads-git-get-branch)
            beads-show--proj-name (beads-git-get-project-name))
      ;; Register with worktree session for lifecycle management
      (beads-show--register-with-session)
      (condition-case err
          ;; Execute command in caller's directory context
          (let ((default-directory caller-dir)
                (issue (beads-command-show! :issue-ids (list issue-id))))
            (setq beads-show--issue-data issue)
            ;; Rename buffer to include title now that we have it
            (let* ((title (oref issue title))
                   (new-name (beads-buffer-name-show
                              issue-id title beads-show--proj-name)))
              (unless (string= (buffer-name) new-name)
                (rename-buffer new-name t)))
            (beads-show--render-issue beads-show--issue-data))
        (error
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize "Error loading issue\n\n"
                             'face 'error))
           (insert (format "%s" (error-message-string err)))
           (goto-char (point-min))))))

    (beads-buffer-display-detail buffer 'beads-show-mode)))

;;;###autoload
(defun beads-show-at-point ()
  "Show issue detail for issue reference at point.
Extracts the issue ID from text at point and calls `beads-show'."
  (interactive)
  (if-let* ((issue-id (beads-show--extract-issue-at-point)))
      (beads-show issue-id)
    (user-error "No issue reference found at point")))

(defun beads-show-copy-id ()
  "Copy the current issue ID to the kill ring."
  (interactive)
  (if beads-show--issue-id
      (progn
        (kill-new beads-show--issue-id)
        (message "Copied issue ID: %s" beads-show--issue-id))
    (user-error "No issue ID in current buffer")))

;;;###autoload
(defun beads-refresh-show ()
  "Refresh the current show buffer from bd CLI.
Uses the stored project directory for command execution."
  (interactive)
  (unless (derived-mode-p 'beads-show-mode)
    (user-error "Not in a beads-show buffer"))
  (unless beads-show--issue-id
    (user-error "No issue ID associated with this buffer"))

  (let ((pos (point))
        (project-dir (or beads-show--project-dir default-directory)))
    (condition-case err
        (let* ((default-directory project-dir)
               (issue (beads-command-show! :issue-ids (list beads-show--issue-id))))
          (setq beads-show--issue-data issue)
          (beads-show--render-issue beads-show--issue-data)
          (goto-char (min pos (point-max)))
          (message "Refreshed %s" beads-show--issue-id))
      (error
       (message "Failed to refresh: %s" (error-message-string err))))))

(defun beads-show-next-section ()
  "Move to the next section in the show buffer."
  (interactive)
  ;; Match UPPERCASE section headers (DEPENDS ON, DESCRIPTION, etc.)
  (let ((section-regexp "^[A-Z][A-Z ]+$"))
    (forward-line 1)  ; Move past current line to find next
    (if (re-search-forward section-regexp nil t)
        (progn
          (beginning-of-line)
          (recenter-top-bottom 0))
      (message "No next section"))))

(defun beads-show-previous-section ()
  "Move to the previous section in the show buffer."
  (interactive)
  ;; Match UPPERCASE section headers (DEPENDS ON, DESCRIPTION, etc.)
  (let ((section-regexp "^[A-Z][A-Z ]+$"))
    (beginning-of-line)
    (if (re-search-backward section-regexp nil t)
        (progn
          (beginning-of-line)
          (recenter-top-bottom 0))
      (message "No previous section"))))

(defun beads-show-forward-paragraph ()
  "Move forward by one paragraph."
  (interactive)
  (forward-paragraph 1))

(defun beads-show-backward-paragraph ()
  "Move backward by one paragraph."
  (interactive)
  (backward-paragraph 1))

(defun beads-show-mark-paragraph ()
  "Mark the current paragraph.
Set mark at beginning of paragraph, move point to end, and activate region."
  (interactive)
  (mark-paragraph)
  (activate-mark)
  (message "Paragraph marked"))

;;; Block Navigation

(defun beads-show--at-block-boundary ()
  "Return the type of block at point, or nil if not at a block boundary.
Block types:
  `fenced-code' - ``` or ~~~ fence
  `list' - list item (-, *, +, or numbered)
  `blockquote' - line starting with >
  `indented-code' - line with 4+ spaces
  `blank' - blank line (block separator)"
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Blank line
     ((looking-at "^[[:space:]]*$")
      'blank)
     ;; Fenced code block
     ((looking-at "^\\(?:```\\|~~~\\)")
      'fenced-code)
     ;; List items
     ((looking-at "^[[:space:]]*\\(?:[-*+]\\|[0-9]+\\.\\)[[:space:]]")
      'list)
     ;; Blockquote
     ((looking-at "^[[:space:]]*>")
      'blockquote)
     ;; Indented code (4+ spaces)
     ((looking-at "^    \\|^\t")
      'indented-code)
     ;; Not at a block boundary
     (t nil))))

(defun beads-show--skip-blank-lines-forward ()
  "Skip forward over blank lines.  Return t if moved, nil otherwise."
  (let ((start (point)))
    (while (and (not (eobp))
                (looking-at "^[[:space:]]*$"))
      (forward-line 1))
    (not (eq start (point)))))

(defun beads-show--skip-blank-lines-backward ()
  "Skip backward over blank lines.  Return t if moved, nil otherwise."
  (let ((start (point)))
    (while (and (not (bobp))
                (save-excursion
                  (forward-line -1)
                  (looking-at "^[[:space:]]*$")))
      (forward-line -1))
    (not (eq start (point)))))

(defun beads-show--in-fenced-code-block ()
  "Return t if point is inside a fenced code block."
  (save-excursion
    (let ((fence-count 0))
      (goto-char (point-min))
      (while (< (point) (point))
        (when (looking-at "^\\(?:```\\|~~~\\)")
          (setq fence-count (1+ fence-count)))
        (forward-line 1))
      ;; Odd fence count means we're inside a block
      (cl-oddp fence-count))))

(defun beads-show-forward-block ()
  "Move forward to the next block boundary.
Blocks include fenced code, lists, blockquotes, and indented code."
  (interactive)
  (let ((start-pos (point))
        (moved nil))
    (beginning-of-line)

    ;; Skip current block if we're at the start of one
    (let ((block-type (beads-show--at-block-boundary)))
      (when block-type
        (cond
         ;; Fenced code block - skip to closing fence
         ((eq block-type 'fenced-code)
          (let ((fence-marker (buffer-substring-no-properties
                              (point) (+ (point) 3))))
            (forward-line 1)
            (while (and (not (eobp))
                       (not (looking-at (concat "^" (regexp-quote fence-marker)))))
              (forward-line 1))
            (when (not (eobp))
              (forward-line 1))
            (setq moved t)))

         ;; List - skip consecutive list items
         ((eq block-type 'list)
          (while (and (not (eobp))
                     (or (looking-at "^[[:space:]]*\\(?:[-*+]\\|[0-9]+\\.\\)[[:space:]]")
                         (looking-at "^[[:space:]]*$")
                         ;; Continuation lines (indented)
                         (and (not (looking-at "^[[:space:]]*$"))
                              (looking-at "^[[:space:]]+"))))
            (forward-line 1))
          (setq moved t))

         ;; Blockquote - skip consecutive quoted lines
         ((eq block-type 'blockquote)
          (while (and (not (eobp))
                     (looking-at "^[[:space:]]*>"))
            (forward-line 1))
          (setq moved t))

         ;; Indented code - skip consecutive indented lines
         ((eq block-type 'indented-code)
          (while (and (not (eobp))
                     (or (looking-at "^    \\|^\t")
                         (looking-at "^[[:space:]]*$")))
            (forward-line 1))
          (setq moved t))

         ;; Blank line - skip consecutive blanks
         ((eq block-type 'blank)
          (beads-show--skip-blank-lines-forward)
          (setq moved t)))))

    ;; If we didn't move yet, skip to next block
    (unless moved
      ;; Skip current paragraph/text
      (while (and (not (eobp))
                 (not (beads-show--at-block-boundary)))
        (forward-line 1))
      ;; Skip blank lines
      (beads-show--skip-blank-lines-forward))

    (if (not (eq (point) start-pos))
        (progn
          (beginning-of-line)
          (recenter-top-bottom 0))
      (goto-char start-pos)
      (message "No next block"))))

(defun beads-show-backward-block ()
  "Move backward to the previous block boundary.
Blocks include fenced code, lists, blockquotes, and indented code."
  (interactive)
  (let ((start-pos (point))
        (moved nil))

    ;; Move back at least one line
    (forward-line -1)

    ;; Skip blank lines
    (while (and (not (bobp))
               (looking-at "^[[:space:]]*$"))
      (forward-line -1))

    (when (not (bobp))
      ;; Find the start of the current block
      (let ((block-type (beads-show--at-block-boundary)))
        (cond
         ;; Fenced code - find opening fence
         ((eq block-type 'fenced-code)
          ;; Move to closing fence
          (while (and (not (bobp))
                     (not (looking-at "^\\(?:```\\|~~~\\)")))
            (forward-line -1))
          ;; Now find opening fence
          (when (looking-at "^\\(?:```\\|~~~\\)")
            (let ((fence-marker (buffer-substring-no-properties
                                (point) (+ (point) 3))))
              (forward-line -1)
              (while (and (not (bobp))
                         (not (looking-at (concat "^" (regexp-quote fence-marker)))))
                (forward-line -1))))
          (setq moved t))

         ;; List - find start of list
         ((eq block-type 'list)
          (while (and (not (bobp))
                     (or (looking-at "^[[:space:]]*\\(?:[-*+]\\|[0-9]+\\.\\)[[:space:]]")
                         (looking-at "^[[:space:]]*$")
                         (and (not (looking-at "^[[:space:]]*$"))
                              (looking-at "^[[:space:]]+"))))
            (forward-line -1))
          ;; Move forward one line if we went too far
          (unless (bobp)
            (forward-line 1))
          (setq moved t))

         ;; Blockquote - find start
         ((eq block-type 'blockquote)
          (while (and (not (bobp))
                     (looking-at "^[[:space:]]*>"))
            (forward-line -1))
          (unless (bobp)
            (forward-line 1))
          (setq moved t))

         ;; Indented code - find start
         ((eq block-type 'indented-code)
          (while (and (not (bobp))
                     (or (looking-at "^    \\|^\t")
                         (looking-at "^[[:space:]]*$")))
            (forward-line -1))
          (unless (bobp)
            (forward-line 1))
          (setq moved t))

         ;; Regular text - scan backward to find a block
         (t
          (while (and (not (bobp))
                     (not (beads-show--at-block-boundary)))
            (forward-line -1))
          (when (beads-show--at-block-boundary)
            (setq moved t))))))

    (if moved
        (progn
          (beginning-of-line)
          (recenter-top-bottom 0))
      (goto-char start-pos)
      (message "No previous block"))))

(defun beads-show-beginning-of-section ()
  "Move to beginning of current section.
If already at section header, stay there."
  (interactive)
  (let ((current-level (beads-show--section-level)))
    (if current-level
        ;; Already at a heading, we're at the beginning
        (beginning-of-line)
      ;; Not at a heading, find the current section's beginning
      (let ((start-pos (point))
            (found nil))
        (beginning-of-line)
        (while (and (not found) (not (bobp)))
          (when (beads-show--section-level)
            ;; Make sure we're on the heading line, not underline
            (when (looking-at "^[═─]+$")
              (forward-line -1))
            (setq found t))
          (unless found
            (forward-line -1)))
        (if found
            (beginning-of-line)
          (goto-char start-pos)
          (message "No section found"))))))

(defun beads-show-end-of-section ()
  "Move to end of current section (before next section or EOF)."
  (interactive)
  (let ((current-level (beads-show--section-level)))
    (if current-level
        ;; At a heading, move past underline if present then find next section
        (progn
          (forward-line 1)
          (when (looking-at "^[═─]+$")
            (forward-line 1))
          (let ((target-level current-level)
                (found-next nil))
            (while (and (not found-next) (not (eobp)))
              (let ((level (beads-show--section-level)))
                (when (and level (<= level target-level)
                          (not (looking-at "^[═─]+$")))
                  (setq found-next t)))
              (unless found-next
                (forward-line 1)))
            (when found-next
              (forward-line -1))
            (end-of-line)))
      ;; Not at a heading, move to end of current section
      (beads-show-beginning-of-section)
      (beads-show-end-of-section))))

(defun beads-show-mark-section ()
  "Mark the current section.
Set mark at beginning of section, move point to end, and activate region."
  (interactive)
  (let ((_start-pos (point)))
    ;; Move to beginning of section
    (beads-show-beginning-of-section)
    (let ((begin (point)))
      ;; Set mark at beginning and activate it
      (push-mark begin t t)
      ;; Move to end
      (beads-show-end-of-section)
      ;; Ensure mark is activated (important for batch mode)
      (activate-mark)
      (message "Section marked"))))

(defun beads-show-follow-reference ()
  "Follow issue reference at point or on current line."
  (interactive)
  (if-let* ((issue-id (beads-show--extract-issue-at-point)))
      (beads-show issue-id)
    (message "No issue reference at point")))

(defun beads-show-follow-reference-other-window ()
  "Follow issue reference at point in other window."
  (interactive)
  (if-let* ((issue-id (beads-show--extract-issue-at-point)))
      (let ((buffer (beads-show--get-or-create-buffer issue-id)))
        (beads-show-update-buffer issue-id buffer)
        (beads-buffer-display-other-window buffer))
    (message "No issue reference at point")))

(defun beads-show-next-reference ()
  "Jump to next issue reference in buffer."
  (interactive)
  (let ((_start-pos (point))
        (found nil))
    (save-excursion
      ;; Move past current reference if we're on one
      (when (beads-show--extract-issue-at-point)
        (re-search-forward "[a-zA-Z][a-zA-Z0-9._-]*-[0-9a-fA-F]+\\(?:\\.[0-9]+\\)*" nil t))
      ;; Search for next reference
      (when (re-search-forward "\\([a-zA-Z][a-zA-Z0-9._-]*-[0-9a-fA-F]+\\(?:\\.[0-9]+\\)*\\)" nil t)
        (setq found (match-beginning 1))))
    (if found
        (goto-char found)
      (message "No next reference"))))

(defun beads-show-previous-reference ()
  "Jump to previous issue reference in buffer."
  (interactive)
  (let ((_start-pos (point))
        (found nil))
    (save-excursion
      ;; Move before current reference if we're on one
      (when (beads-show--extract-issue-at-point)
        (goto-char (line-beginning-position)))
      ;; Search for previous reference
      (when (re-search-backward "\\([a-zA-Z][a-zA-Z0-9._-]*-[0-9a-fA-F]+\\(?:\\.[0-9]+\\)*\\)" nil t)
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
         (_parent-issue-id beads-show--issue-id))
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
        ;; Map field-flag to command class slot
        (let* ((slot-keyword
                (pcase field-flag
                  ("--title" :title)
                  ("--description" :description)
                  ("--status" :status)
                  ("--priority" :priority)
                  ("--assignee" :assignee)
                  ("--acceptance" :acceptance)
                  ("--design" :design)
                  ("--notes" :notes)
                  ("--external-ref" :external-ref)
                  (_ (error "Unknown field flag: %s" field-flag))))
               (cmd (apply #'beads-command-update
                           :issue-ids (list beads-show--issue-id)
                           slot-keyword new-value
                           nil)))
          (beads-command-execute cmd)
          ;; Invalidate completion cache since issue data changed
          (beads-completion-invalidate-cache))
        ;; Refetch and refresh the display to get latest data
        (beads-refresh-show)
        (message "%s updated" field-name))
    (error
     (message "Failed to update %s: %s"
              field-name (error-message-string err)))))

;;;###autoload
(defun beads-show-edit-field ()
  "Edit a field of the current issue.
Prompts for field to edit and opens an editing buffer."
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
         (current-value (pcase field-key
                         ('title (oref beads-show--issue-data title))
                         ('description (oref beads-show--issue-data description))
                         ('acceptance-criteria (oref beads-show--issue-data acceptance-criteria))
                         ('design (oref beads-show--issue-data design))
                         ('notes (oref beads-show--issue-data notes))))
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
                      ('description "--description")
                      ('acceptance-criteria "--acceptance")
                      ('design "--design")
                      ('notes "--notes"))))
           (beads-show--update-field field-name flag new-value)))))))

;;; Quick Actions Transient

(require 'transient)

(transient-define-prefix beads-show-actions ()
  "Quick actions for current issue."
  :transient-suffix 'transient--do-stay
  ["Issue Actions"
   ["Status"
    ("o" "Open" beads-show-set-status-open)
    ("p" "In Progress" beads-show-set-status-in-progress)
    ("b" "Blocked" beads-show-set-status-blocked)
    ("c" "Close" beads-show-set-status-closed)]
   ["Navigate"
    ("d" "Dependencies" beads-show-goto-depends)
    ("B" "Blocks" beads-show-goto-blocks)
    ("P" "Parent" beads-show-goto-parent)
    ("C" "Children" beads-show-goto-children)]
   ["Edit"
    ("e" "Edit field" beads-show-edit-field)
    ("n" "Add note" beads-show-add-note)
    ("L" "Add label" beads-show-add-label)]
   ["Other"
    ("g" "Refresh" beads-refresh-show :transient t)
    ("w" "Copy ID" beads-show-copy-id)
    ("q" "Quit" transient-quit-one)]])

(defun beads-show-set-status (status)
  "Set current issue STATUS."
  (interactive
   (list (completing-read "Status: "
                          '("open" "in_progress" "blocked" "closed")
                          nil t)))
  (when beads-show--issue-id
    (beads-show--update-field "Status" "--status" status)))

(defun beads-show-set-status-open ()
  "Set current issue status to open."
  (interactive)
  (beads-show-set-status "open"))

(defun beads-show-set-status-in-progress ()
  "Set current issue status to in_progress."
  (interactive)
  (beads-show-set-status "in_progress"))

(defun beads-show-set-status-blocked ()
  "Set current issue status to blocked."
  (interactive)
  (beads-show-set-status "blocked"))

(defun beads-show-set-status-closed ()
  "Close current issue."
  (interactive)
  (when beads-show--issue-id
    (let ((reason (read-string "Close reason: ")))
      (condition-case err
          (progn
            (beads-command-close! :issue-ids (list beads-show--issue-id)
                                  :reason reason)
            (beads-completion-invalidate-cache)
            (beads-refresh-show)
            (message "Issue closed"))
        (error
         (message "Failed to close: %s" (error-message-string err)))))))

(defun beads-show-add-note ()
  "Add a note to current issue."
  (interactive)
  (when beads-show--issue-id
    (let* ((current (when beads-show--issue-data
                      (oref beads-show--issue-data notes)))
           (new-note (read-string "Add note: "))
           (updated (if current
                        (concat current "\n\n" new-note)
                      new-note)))
      (beads-show--update-field "Notes" "--notes" updated))))

(defun beads-show-add-label ()
  "Add a label to current issue."
  (interactive)
  (when beads-show--issue-id
    (let ((label (read-string "Label: ")))
      (condition-case err
          (progn
            (beads-command-label-add! :issue-id beads-show--issue-id
                                      :label label)
            (beads-refresh-show)
            (message "Label added: %s" label))
        (error
         (message "Failed to add label: %s" (error-message-string err)))))))

(defun beads-show-goto-depends ()
  "Go to first dependency of current issue."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "^DEPENDS ON$" nil t)
      (forward-line 2)
    (message "No dependencies section")))

(defun beads-show-goto-blocks ()
  "Go to BLOCKS section of current issue."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "^BLOCKS$" nil t)
      (forward-line 2)
    (message "No blocks section")))

(defun beads-show-goto-parent ()
  "Go to parent issue."
  (interactive)
  (when beads-show--issue-data
    (let ((deps (oref beads-show--issue-data dependencies)))
      (when-let* ((parent (seq-find
                           (lambda (d) (string= (oref d type) "parent-child"))
                           deps)))
        (beads-show (oref parent depends-on-id))))))

(defun beads-show-goto-children ()
  "Go to CHILDREN section of current issue."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "^CHILDREN" nil t)
      (forward-line 2)
    (message "No children section")))

;;; Footer

(provide 'beads-show)
;;; beads-show.el ends here
