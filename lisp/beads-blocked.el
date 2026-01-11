;;; beads-blocked.el --- CLI-like blocked view for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides a CLI-like view for "bd blocked" output.  Unlike
;; the tabulated-list-mode approach, this renders issues in a format
;; similar to the bd CLI, with priority badges, clickable issue IDs,
;; blocker information, and standard navigation.
;;
;; The blocked view shows issues that have unresolved blockers.
;;
;; Key bindings:
;;   n/p     - Next/previous issue
;;   RET     - Show issue details
;;   g       - Refresh buffer
;;   q       - Quit buffer
;;   w/C-w   - Copy issue ID to kill ring
;;   T/R/P/Q/C - Start agent (Task/Review/Plan/QA/Custom)
;;
;; Rendering format:
;;
;;   🚫 Blocked issues (25)
;;
;;   [● P1] bde-63: Integration test: Labels workflow
;;     └── Blocked by 1 open dependencies: [bde-64]
;;
;;   [● P1] bde-koh7.2: US1: Access Main Menu
;;     └── Blocked by 1 open dependencies: [bde-koh7.1]

;;; Code:

(require 'beads)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-command-blocked)
(require 'beads-git)
(require 'beads-sesman)
(require 'beads-types)
(require 'button)
(require 'cl-lib)

;;; Forward Declarations

(declare-function beads-show "beads-show" (issue-id))
(declare-function beads-agent-start-task "beads-agent" (&optional arg))
(declare-function beads-agent-start-review "beads-agent" (&optional arg))
(declare-function beads-agent-start-plan "beads-agent" (&optional arg))
(declare-function beads-agent-start-qa "beads-agent" (&optional arg))
(declare-function beads-agent-start-custom "beads-agent" (&optional arg))
(declare-function beads-agent-stop-at-point "beads-agent")
(declare-function beads-agent-jump-at-point "beads-agent")
(declare-function beads-agent-start-at-point "beads-agent")

;;; Customization

(defgroup beads-blocked nil
  "CLI-like blocked view for Beads."
  :group 'beads
  :prefix "beads-blocked-")

;;; Faces

(defface beads-blocked-header-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.2))
  "Face for the header line in blocked view."
  :group 'beads-blocked)

(defface beads-blocked-count-face
  '((t :inherit font-lock-comment-face))
  "Face for the issue count in header."
  :group 'beads-blocked)

(defface beads-blocked-priority-critical
  '((t :inherit error :weight bold))
  "Face for priority 0 (critical) - red."
  :group 'beads-blocked)

(defface beads-blocked-priority-high
  '((t :inherit warning :weight bold))
  "Face for priority 1 (high) - orange."
  :group 'beads-blocked)

(defface beads-blocked-priority-medium
  '((t :inherit default))
  "Face for priority 2 (medium) - default."
  :group 'beads-blocked)

(defface beads-blocked-priority-low
  '((t :inherit shadow))
  "Face for priority 3-4 (low/backlog) - gray."
  :group 'beads-blocked)

(defface beads-blocked-id-face
  '((t :inherit link))
  "Face for issue IDs."
  :group 'beads-blocked)

(defface beads-blocked-title-face
  '((t :inherit default))
  "Face for issue titles."
  :group 'beads-blocked)

(defface beads-blocked-blocker-line-face
  '((t :inherit font-lock-comment-face))
  "Face for blocker info line."
  :group 'beads-blocked)

(defface beads-blocked-blocker-id-face
  '((t :inherit link))
  "Face for blocker issue IDs."
  :group 'beads-blocked)

(defface beads-blocked-empty-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for empty state message."
  :group 'beads-blocked)

;;; Variables

(defvar-local beads-blocked--issues nil
  "List of beads-blocked-issue objects in current buffer.")

(defvar-local beads-blocked--project-dir nil
  "Project directory for bd command execution.")

(defvar-local beads-blocked--branch nil
  "Git branch name (metadata for display).")

(defvar-local beads-blocked--proj-name nil
  "Project name for buffer identification.")

(defvar-local beads-blocked--issue-positions nil
  "Alist of (ISSUE-ID . POSITION) for navigation.")

;;; Buffer Identity

(defun beads-blocked--normalize-directory (dir)
  "Normalize DIR for consistent comparison."
  (directory-file-name (expand-file-name dir)))

(defun beads-blocked--find-buffer-for-project (project-dir)
  "Find existing blocked buffer for PROJECT-DIR.
Return buffer or nil if not found."
  (let ((normalized-dir (beads-blocked--normalize-directory project-dir)))
    (cl-find-if
     (lambda (buf)
       (with-current-buffer buf
         (and (derived-mode-p 'beads-blocked-mode)
              beads-blocked--project-dir
              (equal (beads-blocked--normalize-directory
                      beads-blocked--project-dir)
                     normalized-dir))))
     (buffer-list))))

(defun beads-blocked--get-or-create-buffer ()
  "Get or create blocked buffer for current project context."
  (let* ((project-dir (or (beads-git-find-project-root) default-directory))
         (existing (beads-blocked--find-buffer-for-project project-dir)))
    (or existing
        (let* ((proj-name (beads-git-get-project-name))
               (buf-name (beads-buffer-list "blocked" nil proj-name))
               (buffer (get-buffer-create buf-name)))
          (with-current-buffer buffer
            (setq beads-blocked--project-dir project-dir)
            (setq beads-blocked--branch (beads-git-get-branch))
            (setq beads-blocked--proj-name proj-name))
          buffer))))

;;; Rendering Helpers

(defun beads-blocked--priority-face (priority)
  "Return face for PRIORITY level."
  (pcase priority
    (0 'beads-blocked-priority-critical)
    (1 'beads-blocked-priority-high)
    (2 'beads-blocked-priority-medium)
    ((or 3 4) 'beads-blocked-priority-low)
    (_ 'beads-blocked-priority-medium)))

(defun beads-blocked--priority-indicator (priority)
  "Return priority indicator string for PRIORITY."
  (let ((face (beads-blocked--priority-face priority)))
    (propertize (format "● P%d" (or priority 2)) 'face face)))

(defun beads-blocked--button-action (button)
  "Action for clicking on an issue reference BUTTON."
  (let ((issue-id (button-get button 'issue-id)))
    (beads-show issue-id)))

(defun beads-blocked--render-header (count)
  "Render header line for COUNT issues."
  (insert (propertize "🚫 Blocked issues" 'face 'beads-blocked-header-face))
  (insert " ")
  (insert (propertize (format "(%d)" count) 'face 'beads-blocked-count-face))
  (insert "\n\n"))

(defun beads-blocked--render-blockers (blocked-by blocked-by-count)
  "Render blocker information line.
BLOCKED-BY is a list of blocker IDs.
BLOCKED-BY-COUNT is the total count of blockers."
  (insert "  ")
  (insert (propertize "└── " 'face 'beads-blocked-blocker-line-face))
  (insert (propertize (format "Blocked by %d open dependenc%s: "
                              blocked-by-count
                              (if (= blocked-by-count 1) "y" "ies"))
                      'face 'beads-blocked-blocker-line-face))
  ;; Render each blocker ID as a button
  (insert "[")
  (let ((first t))
    (dolist (blocker-id blocked-by)
      (unless first
        (insert ", "))
      (setq first nil)
      (let ((id-start (point)))
        (insert blocker-id)
        (make-button id-start (point)
                     'issue-id blocker-id
                     'action #'beads-blocked--button-action
                     'follow-link t
                     'help-echo (format "Show %s" blocker-id)
                     'face 'beads-blocked-blocker-id-face))))
  (insert "]")
  (insert "\n"))

(defun beads-blocked--render-issue (issue)
  "Render blocked ISSUE with blocker information.
Returns the position of the issue line start."
  (let ((pos (point))
        (id (oref issue id))
        (title (or (oref issue title) "Untitled"))
        (priority (oref issue priority))
        (blocked-by (oref issue blocked-by))
        (blocked-by-count (oref issue blocked-by-count)))
    ;; Priority badge
    (insert "[")
    (insert (beads-blocked--priority-indicator priority))
    (insert "] ")
    ;; Issue ID as button
    (let ((id-start (point)))
      (insert id)
      (make-button id-start (point)
                   'issue-id id
                   'action #'beads-blocked--button-action
                   'follow-link t
                   'help-echo (format "Show %s" id)
                   'face 'beads-blocked-id-face))
    ;; Title
    (insert ": ")
    (insert (propertize title 'face 'beads-blocked-title-face))
    (insert "\n")
    ;; Blocker info line
    (when (and blocked-by (> blocked-by-count 0))
      (beads-blocked--render-blockers blocked-by blocked-by-count))
    (insert "\n")
    ;; Store position for navigation
    (cons id pos)))

(defun beads-blocked--render-empty ()
  "Render empty state message."
  (insert (propertize "🚫 Blocked issues" 'face 'beads-blocked-header-face))
  (insert " ")
  (insert (propertize "(0)" 'face 'beads-blocked-count-face))
  (insert "\n\n")
  (insert (propertize "No blocked issues found.\n"
                      'face 'beads-blocked-empty-face))
  (insert (propertize "All issues are ready or closed."
                      'face 'beads-blocked-empty-face)))

(defun beads-blocked--render-buffer ()
  "Render all issues into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq beads-blocked--issue-positions nil)
    (if (or (null beads-blocked--issues)
            (= (length beads-blocked--issues) 0))
        (beads-blocked--render-empty)
      ;; Render header
      (beads-blocked--render-header (length beads-blocked--issues))
      ;; Render each issue
      (dolist (issue beads-blocked--issues)
        (let ((pos-pair (beads-blocked--render-issue issue)))
          (push pos-pair beads-blocked--issue-positions)))
      ;; Footer
      (insert (propertize "Press 'g' to refresh, 'q' to quit, RET on issue to show"
                          'face 'shadow)))
    (setq beads-blocked--issue-positions
          (nreverse beads-blocked--issue-positions))
    (goto-char (point-min))
    ;; Move to first issue if any
    (when beads-blocked--issue-positions
      (goto-char (cdr (car beads-blocked--issue-positions))))))

;;; Navigation

(defun beads-blocked--current-issue-id ()
  "Return issue ID at point, or nil."
  (let ((pos (point)))
    ;; First check if we're on a button
    (or (when-let ((button (button-at pos)))
          (button-get button 'issue-id))
        ;; Otherwise find which issue line we're on
        (catch 'found
          (let ((current-id nil))
            (dolist (pair beads-blocked--issue-positions)
              (if (<= (cdr pair) pos)
                  (setq current-id (car pair))
                (throw 'found current-id)))
            current-id)))))

(defun beads-blocked-next-issue ()
  "Move to next issue."
  (interactive)
  (let ((current-pos (point))
        (next-pos nil))
    (dolist (pair beads-blocked--issue-positions)
      (when (and (> (cdr pair) current-pos)
                 (null next-pos))
        (setq next-pos (cdr pair))))
    (if next-pos
        (goto-char next-pos)
      (message "No next issue"))))

(defun beads-blocked-previous-issue ()
  "Move to previous issue."
  (interactive)
  (let ((current-pos (point))
        (prev-pos nil))
    (dolist (pair beads-blocked--issue-positions)
      (when (< (cdr pair) current-pos)
        (setq prev-pos (cdr pair))))
    (if prev-pos
        (goto-char prev-pos)
      (message "No previous issue"))))

;;; Commands

(defun beads-blocked-show ()
  "Show details for issue at point."
  (interactive)
  (if-let ((id (beads-blocked--current-issue-id)))
      (beads-show id)
    (user-error "No issue at point")))

(defun beads-blocked-refresh ()
  "Refresh the blocked buffer."
  (interactive)
  (let ((project-dir (or beads-blocked--project-dir default-directory)))
    (condition-case err
        (let* ((default-directory project-dir)
               (issues (beads-command-blocked!)))
          (setq beads-blocked--issues issues)
          (beads-blocked--render-buffer)
          (message "Refreshed %d blocked issue%s"
                   (length issues)
                   (if (= (length issues) 1) "" "s")))
      (error
       (message "Failed to refresh: %s" (error-message-string err))))))

(defun beads-blocked-copy-id ()
  "Copy issue ID at point to kill ring."
  (interactive)
  (if-let ((id (beads-blocked--current-issue-id)))
      (progn
        (kill-new id)
        (message "Copied issue ID: %s" id))
    (user-error "No issue at point")))

(defun beads-blocked-quit ()
  "Quit the blocked buffer."
  (interactive)
  (quit-window t))

;;; Mode Definition

(defvar beads-blocked-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "n") #'beads-blocked-next-issue)
    (define-key map (kbd "p") #'beads-blocked-previous-issue)
    (define-key map (kbd "RET") #'beads-blocked-show)

    ;; Refresh/quit
    (define-key map (kbd "g") #'beads-blocked-refresh)
    (define-key map (kbd "q") #'beads-blocked-quit)

    ;; Copy
    (define-key map (kbd "w") #'beads-blocked-copy-id)
    (define-key map (kbd "C-w") #'beads-blocked-copy-id)

    ;; AI Agent commands
    (define-key map (kbd "T") #'beads-agent-start-task)
    (define-key map (kbd "R") #'beads-agent-start-review)
    (define-key map (kbd "P") #'beads-agent-start-plan)
    (define-key map (kbd "Q") #'beads-agent-start-qa)
    (define-key map (kbd "C") #'beads-agent-start-custom)
    (define-key map (kbd "X") #'beads-agent-stop-at-point)
    (define-key map (kbd "J") #'beads-agent-jump-at-point)
    (define-key map (kbd "A") #'beads-agent-start-at-point)

    ;; Sesman
    (define-key map (kbd "C-c C-s") beads-sesman-map)
    map)
  "Keymap for `beads-blocked-mode'.")

(define-derived-mode beads-blocked-mode special-mode "Beads-Blocked"
  "Major mode for displaying blocked Beads issues in CLI-like format.

\\{beads-blocked-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil))

;;; Public Command

;;;###autoload
(defun beads-blocked ()
  "Display blocked Beads issues in CLI-like format.
Shows issues with unresolved blockers."
  (interactive)
  (beads-check-executable)
  (let* ((caller-dir default-directory)
         (project-dir (or (beads-git-find-project-root) default-directory))
         (buffer (beads-blocked--get-or-create-buffer))
         (issues (beads-command-blocked!)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-blocked-mode)
        (beads-blocked-mode))
      ;; Update state
      (setq beads-blocked--project-dir project-dir)
      (setq beads-blocked--branch (beads-git-get-branch))
      (setq beads-blocked--proj-name (beads-git-get-project-name))
      (setq beads-blocked--issues issues)
      (setq default-directory caller-dir)
      ;; Render
      (beads-blocked--render-buffer)
      (if issues
          (message "Found %d blocked issue%s"
                   (length issues)
                   (if (= (length issues) 1) "" "s"))
        (message "No blocked issues found")))
    (beads-buffer-display-same-or-reuse buffer)))

(provide 'beads-blocked)
;;; beads-blocked.el ends here
