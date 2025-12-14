;;; mock-claude-code-ide.el --- Mock claude-code-ide for testing -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal mock of claude-code-ide.el for testing beads-agent integration.

;;; Code:

(defvar mock-claude-code-ide--sessions (make-hash-table :test 'equal)
  "Mock sessions by project directory.")

(defvar mock-claude-code-ide--current-buffer nil
  "Mock current Claude Code buffer.")

(defun claude-code-ide ()
  "Mock: Start Claude Code IDE session."
  (interactive)
  (let ((project-dir default-directory))
    (puthash project-dir t mock-claude-code-ide--sessions)
    (setq mock-claude-code-ide--current-buffer
          (get-buffer-create (format "*Claude Code: %s*"
                                     (file-name-nondirectory
                                      (directory-file-name project-dir)))))
    (with-current-buffer mock-claude-code-ide--current-buffer
      (erase-buffer)
      (insert (format "Mock Claude Code session for %s\n\n" project-dir))
      (insert "Ready for prompts...\n"))
    (switch-to-buffer mock-claude-code-ide--current-buffer)
    (message "Mock: Started Claude Code session in %s" project-dir)))

(defun claude-code-ide-stop ()
  "Mock: Stop Claude Code IDE session."
  (interactive)
  (let ((project-dir default-directory))
    (remhash project-dir mock-claude-code-ide--sessions)
    (when mock-claude-code-ide--current-buffer
      (kill-buffer mock-claude-code-ide--current-buffer))
    (setq mock-claude-code-ide--current-buffer nil)
    (message "Mock: Stopped Claude Code session")))

(defun claude-code-ide-switch-to-buffer ()
  "Mock: Switch to Claude Code buffer."
  (interactive)
  (if mock-claude-code-ide--current-buffer
      (switch-to-buffer mock-claude-code-ide--current-buffer)
    (message "Mock: No active Claude Code session")))

(defun claude-code-ide-send-prompt (prompt)
  "Mock: Send PROMPT to Claude Code."
  (interactive "sPrompt: ")
  (if mock-claude-code-ide--current-buffer
      (with-current-buffer mock-claude-code-ide--current-buffer
        (goto-char (point-max))
        (insert (format "\n> %s\n" prompt))
        (insert "Mock response: Processing...\n")
        (message "Mock: Sent prompt"))
    (message "Mock: No active Claude Code session")))

(defun claude-code-ide-mcp--get-session-for-project (project-dir)
  "Mock: Get session for PROJECT-DIR."
  (gethash project-dir mock-claude-code-ide--sessions))

(provide 'claude-code-ide)
(provide 'claude-code-ide-mcp)

;;; mock-claude-code-ide.el ends here
