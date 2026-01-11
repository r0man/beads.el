;;; beads-ready.el --- CLI-like ready view for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides a CLI-like view for "bd ready" output.  Unlike
;; the tabulated-list-mode approach, this renders issues in a format
;; similar to the bd CLI, with priority badges, clickable issue IDs,
;; and standard navigation.
;;
;; The ready view shows issues with no blockers that are actionable.
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
;;   📋 Ready work (10 issues with no blockers)
;;
;;    1. [● P0] [task] beads.el-q35o: Title here...
;;    2. [● P0] [task] beads.el-rsja: Another title...

;;; Code:

(require 'beads)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-command-ready)
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

(defgroup beads-ready nil
  "CLI-like ready view for Beads."
  :group 'beads
  :prefix "beads-ready-")

;;; Faces

(defface beads-ready-header-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.2))
  "Face for the header line in ready view."
  :group 'beads-ready)

(defface beads-ready-count-face
  '((t :inherit font-lock-comment-face))
  "Face for the issue count in header."
  :group 'beads-ready)

(defface beads-ready-number-face
  '((t :inherit font-lock-comment-face))
  "Face for issue numbers (1., 2., etc.)."
  :group 'beads-ready)

(defface beads-ready-priority-critical
  '((t :inherit error :weight bold))
  "Face for priority 0 (critical) - red."
  :group 'beads-ready)

(defface beads-ready-priority-high
  '((t :inherit warning :weight bold))
  "Face for priority 1 (high) - orange."
  :group 'beads-ready)

(defface beads-ready-priority-medium
  '((t :inherit default))
  "Face for priority 2 (medium) - default."
  :group 'beads-ready)

(defface beads-ready-priority-low
  '((t :inherit shadow))
  "Face for priority 3-4 (low/backlog) - gray."
  :group 'beads-ready)

(defface beads-ready-type-face
  '((t :inherit font-lock-type-face))
  "Face for issue type."
  :group 'beads-ready)

(defface beads-ready-id-face
  '((t :inherit link))
  "Face for issue IDs."
  :group 'beads-ready)

(defface beads-ready-title-face
  '((t :inherit default))
  "Face for issue titles."
  :group 'beads-ready)

(defface beads-ready-empty-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for empty state message."
  :group 'beads-ready)

;;; Variables

(defvar-local beads-ready--issues nil
  "List of beads-issue objects in current buffer.")

(defvar-local beads-ready--project-dir nil
  "Project directory for bd command execution.")

(defvar-local beads-ready--branch nil
  "Git branch name (metadata for display).")

(defvar-local beads-ready--proj-name nil
  "Project name for buffer identification.")

(defvar-local beads-ready--issue-positions nil
  "Alist of (ISSUE-ID . POSITION) for navigation.")

;;; Buffer Identity

(defun beads-ready--normalize-directory (dir)
  "Normalize DIR for consistent comparison."
  (directory-file-name (expand-file-name dir)))

(defun beads-ready--find-buffer-for-project (project-dir)
  "Find existing ready buffer for PROJECT-DIR.
Return buffer or nil if not found."
  (let ((normalized-dir (beads-ready--normalize-directory project-dir)))
    (cl-find-if
     (lambda (buf)
       (with-current-buffer buf
         (and (derived-mode-p 'beads-ready-mode)
              beads-ready--project-dir
              (equal (beads-ready--normalize-directory beads-ready--project-dir)
                     normalized-dir))))
     (buffer-list))))

(defun beads-ready--get-or-create-buffer ()
  "Get or create ready buffer for current project context."
  (let* ((project-dir (or (beads-git-find-project-root) default-directory))
         (existing (beads-ready--find-buffer-for-project project-dir)))
    (or existing
        (let* ((proj-name (beads-git-get-project-name))
               (buf-name (beads-buffer-list "ready" nil proj-name))
               (buffer (get-buffer-create buf-name)))
          (with-current-buffer buffer
            (setq beads-ready--project-dir project-dir)
            (setq beads-ready--branch (beads-git-get-branch))
            (setq beads-ready--proj-name proj-name))
          buffer))))

;;; Rendering Helpers

(defun beads-ready--priority-face (priority)
  "Return face for PRIORITY level."
  (pcase priority
    (0 'beads-ready-priority-critical)
    (1 'beads-ready-priority-high)
    (2 'beads-ready-priority-medium)
    ((or 3 4) 'beads-ready-priority-low)
    (_ 'beads-ready-priority-medium)))

(defun beads-ready--priority-indicator (priority)
  "Return priority indicator string for PRIORITY."
  (let ((face (beads-ready--priority-face priority)))
    (propertize (format "● P%d" (or priority 2)) 'face face)))

(defun beads-ready--button-action (button)
  "Action for clicking on an issue reference BUTTON."
  (let ((issue-id (button-get button 'issue-id)))
    (beads-show issue-id)))

(defun beads-ready--render-header (count)
  "Render header line for COUNT issues."
  (insert (propertize "📋 Ready work" 'face 'beads-ready-header-face))
  (insert " ")
  (insert (propertize (format "(%d issue%s with no blockers)"
                              count
                              (if (= count 1) "" "s"))
                      'face 'beads-ready-count-face))
  (insert "\n\n"))

(defun beads-ready--render-issue (issue number)
  "Render ISSUE with NUMBER prefix.
Returns the position of the issue line start."
  (let ((pos (point))
        (id (oref issue id))
        (title (or (oref issue title) "Untitled"))
        (priority (oref issue priority))
        (issue-type (or (oref issue issue-type) "task")))
    ;; Number prefix
    (insert (propertize (format "%3d. " number) 'face 'beads-ready-number-face))
    ;; Priority badge
    (insert "[")
    (insert (beads-ready--priority-indicator priority))
    (insert "] ")
    ;; Type
    (insert "[")
    (insert (propertize issue-type 'face 'beads-ready-type-face))
    (insert "] ")
    ;; Issue ID as button
    (let ((id-start (point)))
      (insert id)
      (make-button id-start (point)
                   'issue-id id
                   'action #'beads-ready--button-action
                   'follow-link t
                   'help-echo (format "Show %s" id)
                   'face 'beads-ready-id-face))
    ;; Title
    (insert ": ")
    (insert (propertize title 'face 'beads-ready-title-face))
    (insert "\n")
    ;; Store position for navigation
    (cons id pos)))

(defun beads-ready--render-empty ()
  "Render empty state message."
  (insert (propertize "📋 Ready work" 'face 'beads-ready-header-face))
  (insert " ")
  (insert (propertize "(0 issues)" 'face 'beads-ready-count-face))
  (insert "\n\n")
  (insert (propertize "No ready issues found.\n" 'face 'beads-ready-empty-face))
  (insert (propertize "All issues may be blocked or closed."
                      'face 'beads-ready-empty-face)))

(defun beads-ready--render-buffer ()
  "Render all issues into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq beads-ready--issue-positions nil)
    (if (or (null beads-ready--issues)
            (= (length beads-ready--issues) 0))
        (beads-ready--render-empty)
      ;; Render header
      (beads-ready--render-header (length beads-ready--issues))
      ;; Render each issue
      (let ((num 1))
        (dolist (issue beads-ready--issues)
          (let ((pos-pair (beads-ready--render-issue issue num)))
            (push pos-pair beads-ready--issue-positions))
          (setq num (1+ num))))
      ;; Footer
      (insert "\n")
      (insert (propertize "Press 'g' to refresh, 'q' to quit, RET on issue to show"
                          'face 'shadow)))
    (setq beads-ready--issue-positions (nreverse beads-ready--issue-positions))
    (goto-char (point-min))
    ;; Move to first issue if any
    (when beads-ready--issue-positions
      (goto-char (cdr (car beads-ready--issue-positions))))))

;;; Navigation

(defun beads-ready--current-issue-id ()
  "Return issue ID at point, or nil."
  (let ((pos (point)))
    ;; First check if we're on a button
    (when-let ((button (button-at pos)))
      (button-get button 'issue-id))
    ;; Otherwise find which issue line we're on
    (or (when-let ((button (button-at pos)))
          (button-get button 'issue-id))
        (catch 'found
          (let ((current-id nil))
            (dolist (pair beads-ready--issue-positions)
              (if (<= (cdr pair) pos)
                  (setq current-id (car pair))
                (throw 'found current-id)))
            current-id)))))

(defun beads-ready-next-issue ()
  "Move to next issue."
  (interactive)
  (let ((current-pos (point))
        (next-pos nil))
    (dolist (pair beads-ready--issue-positions)
      (when (and (> (cdr pair) current-pos)
                 (null next-pos))
        (setq next-pos (cdr pair))))
    (if next-pos
        (goto-char next-pos)
      (message "No next issue"))))

(defun beads-ready-previous-issue ()
  "Move to previous issue."
  (interactive)
  (let ((current-pos (point))
        (prev-pos nil))
    (dolist (pair beads-ready--issue-positions)
      (when (< (cdr pair) current-pos)
        (setq prev-pos (cdr pair))))
    (if prev-pos
        (goto-char prev-pos)
      (message "No previous issue"))))

;;; Commands

(defun beads-ready-show ()
  "Show details for issue at point."
  (interactive)
  (if-let ((id (beads-ready--current-issue-id)))
      (beads-show id)
    (user-error "No issue at point")))

(defun beads-ready-refresh ()
  "Refresh the ready buffer."
  (interactive)
  (let ((project-dir (or beads-ready--project-dir default-directory)))
    (condition-case err
        (let* ((default-directory project-dir)
               (issues (beads-command-ready!)))
          (setq beads-ready--issues issues)
          (beads-ready--render-buffer)
          (message "Refreshed %d ready issue%s"
                   (length issues)
                   (if (= (length issues) 1) "" "s")))
      (error
       (message "Failed to refresh: %s" (error-message-string err))))))

(defun beads-ready-copy-id ()
  "Copy issue ID at point to kill ring."
  (interactive)
  (if-let ((id (beads-ready--current-issue-id)))
      (progn
        (kill-new id)
        (message "Copied issue ID: %s" id))
    (user-error "No issue at point")))

(defun beads-ready-quit ()
  "Quit the ready buffer."
  (interactive)
  (quit-window t))

;;; Mode Definition

(defvar beads-ready-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "n") #'beads-ready-next-issue)
    (define-key map (kbd "p") #'beads-ready-previous-issue)
    (define-key map (kbd "RET") #'beads-ready-show)

    ;; Refresh/quit
    (define-key map (kbd "g") #'beads-ready-refresh)
    (define-key map (kbd "q") #'beads-ready-quit)

    ;; Copy
    (define-key map (kbd "w") #'beads-ready-copy-id)
    (define-key map (kbd "C-w") #'beads-ready-copy-id)

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
  "Keymap for `beads-ready-mode'.")

(define-derived-mode beads-ready-mode special-mode "Beads-Ready"
  "Major mode for displaying ready Beads issues in CLI-like format.

\\{beads-ready-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil))

;;; Public Command

;;;###autoload
(defun beads-ready ()
  "Display ready Beads issues in CLI-like format.
Shows issues with no blockers that are ready for work."
  (interactive)
  (beads-check-executable)
  (let* ((caller-dir default-directory)
         (project-dir (or (beads-git-find-project-root) default-directory))
         (buffer (beads-ready--get-or-create-buffer))
         (issues (beads-command-ready!)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'beads-ready-mode)
        (beads-ready-mode))
      ;; Update state
      (setq beads-ready--project-dir project-dir)
      (setq beads-ready--branch (beads-git-get-branch))
      (setq beads-ready--proj-name (beads-git-get-project-name))
      (setq beads-ready--issues issues)
      (setq default-directory caller-dir)
      ;; Render
      (beads-ready--render-buffer)
      (if issues
          (message "Found %d ready issue%s"
                   (length issues)
                   (if (= (length issues) 1) "" "s"))
        (message "No ready issues found")))
    (beads-buffer-display-same-or-reuse buffer)))

(provide 'beads-ready)
;;; beads-ready.el ends here
