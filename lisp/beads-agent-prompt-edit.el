;;; beads-agent-prompt-edit.el --- Edit agent prompts before launch -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides a mode for reviewing and editing agent prompts
;; before launching the agent.  When enabled via `beads-agent-prompt-edit-enabled',
;; users can modify the auto-generated prompt, then confirm with C-c C-c
;; or cancel with C-c C-k.
;;
;; Usage:
;;   (setq beads-agent-prompt-edit-enabled t)
;;   Then call `beads-agent-start' - a prompt editing buffer will appear.

;;; Code:

(require 'beads-buffer)

;;; Customization

(defgroup beads-agent-prompt-edit nil
  "Agent prompt editing settings."
  :group 'beads-agent
  :prefix "beads-agent-prompt-edit-")

(defcustom beads-agent-prompt-edit-enabled nil
  "When non-nil, show prompt editing buffer before agent launch.
This allows reviewing and modifying the auto-generated prompt."
  :type 'boolean
  :group 'beads-agent-prompt-edit)

;;; Buffer-local Variables

(defvar-local beads-agent-prompt-edit--callback nil
  "Callback function to invoke with the final prompt.")

(defvar-local beads-agent-prompt-edit--issue-id nil
  "Issue ID for the prompt being edited.")

(defvar-local beads-agent-prompt-edit--agent-type nil
  "Agent type name for display purposes.")

;;; Keymap

(defvar beads-agent-prompt-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-agent-prompt-edit-confirm)
    (define-key map (kbd "C-c C-k") #'beads-agent-prompt-edit-cancel)
    map)
  "Keymap for `beads-agent-prompt-edit-mode'.")

;;; Mode Definition

(define-derived-mode beads-agent-prompt-edit-mode text-mode "Beads-Prompt"
  "Major mode for editing agent prompts before launch.

\\<beads-agent-prompt-edit-mode-map>
Press \\[beads-agent-prompt-edit-confirm] to confirm and launch the agent.
Press \\[beads-agent-prompt-edit-cancel] to cancel without launching."
  :group 'beads-agent-prompt-edit
  (setq-local header-line-format
              '(:eval (beads-agent-prompt-edit--header-line))))

(defun beads-agent-prompt-edit--header-line ()
  "Generate header line for prompt edit buffer."
  (format " %s prompt for %s  |  C-c C-c: Confirm  |  C-c C-k: Cancel"
          (or beads-agent-prompt-edit--agent-type "Agent")
          (or beads-agent-prompt-edit--issue-id "issue")))

;;; Buffer Management

(defun beads-agent-prompt-edit--buffer-name (issue-id)
  "Generate buffer name for prompt editing for ISSUE-ID."
  (beads-buffer-utility "prompt-edit" issue-id))

(defun beads-agent-prompt-edit-show (issue-id prompt agent-type-name callback)
  "Show prompt editing buffer for ISSUE-ID.
PROMPT is the initial prompt text to edit.
AGENT-TYPE-NAME is the name of the agent type (for display).
CALLBACK is called with the final prompt text when confirmed,
or called with nil when cancelled."
  (let* ((buf-name (beads-agent-prompt-edit--buffer-name issue-id))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (beads-agent-prompt-edit-mode)
      (erase-buffer)
      (insert prompt)
      (goto-char (point-min))
      (setq beads-agent-prompt-edit--callback callback)
      (setq beads-agent-prompt-edit--issue-id issue-id)
      (setq beads-agent-prompt-edit--agent-type agent-type-name)
      (set-buffer-modified-p nil))
    (pop-to-buffer buf)
    (message "Edit prompt, then C-c C-c to confirm or C-c C-k to cancel")))

;;; Commands

(defun beads-agent-prompt-edit-confirm ()
  "Confirm prompt and launch agent."
  (interactive)
  (let ((prompt (buffer-substring-no-properties (point-min) (point-max)))
        (callback beads-agent-prompt-edit--callback))
    (beads-agent-prompt-edit--cleanup)
    (when callback
      (funcall callback prompt))))

(defun beads-agent-prompt-edit-cancel ()
  "Cancel prompt editing without launching agent."
  (interactive)
  (let ((callback beads-agent-prompt-edit--callback))
    (beads-agent-prompt-edit--cleanup)
    (when callback
      (funcall callback nil))
    (message "Agent launch cancelled")))

(defun beads-agent-prompt-edit--cleanup ()
  "Clean up prompt edit buffer."
  (let ((buf (current-buffer)))
    (quit-window t)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(provide 'beads-agent-prompt-edit)
;;; beads-agent-prompt-edit.el ends here
