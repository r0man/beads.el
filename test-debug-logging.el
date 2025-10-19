;;; test-debug-logging.el --- Test debug logging

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp"
                                           (file-name-directory
                                            (or load-file-name buffer-file-name))))

(require 'beads)

(with-current-buffer (get-buffer-create "*Debug Test Instructions*")
  (erase-buffer)
  (insert "BEADS DEBUG LOGGING TEST\n")
  (insert "========================\n\n")

  (insert "Debug logging commands:\n")
  (insert "-----------------------\n")
  (insert "M-x beads-toggle-debug       - Toggle debug on/off\n")
  (insert "M-x beads-show-debug-buffer  - Show *beads-debug* buffer\n")
  (insert "M-x beads-clear-debug-buffer - Clear debug buffer\n\n")

  (insert "Customize logging level:\n")
  (insert "------------------------\n")
  (insert "M-x customize-variable RET beads-debug-level RET\n")
  (insert "  - error:   Only errors\n")
  (insert "  - info:    Commands and events (default)\n")
  (insert "  - verbose: Everything including output\n\n")

  (insert "Quick test:\n")
  (insert "-----------\n")
  (insert "1. Enable debug: M-: (setq beads-enable-debug t)\n")
  (insert "2. Set level:    M-: (setq beads-debug-level 'info)\n")
  (insert "3. Run command:  M-x beads-list\n")
  (insert "4. View debug:   M-x beads-show-debug-buffer\n\n")

  (insert "What you'll see:\n")
  (insert "----------------\n")
  (insert "[YYYY-MM-DD HH:MM:SS] [INFO] Running: bd list --json\n")
  (insert "[YYYY-MM-DD HH:MM:SS] [VERBOSE] In directory: /path/...\n")
  (insert "[YYYY-MM-DD HH:MM:SS] [VERBOSE] Exit code: 0\n")
  (insert "[YYYY-MM-DD HH:MM:SS] [VERBOSE] Output: {...}\n\n")

  (insert "Debug buffer keybindings:\n")
  (insert "-------------------------\n")
  (insert "g - Refresh\n")
  (insert "c - Clear buffer\n")
  (insert "q - Quit window\n")

  (display-buffer (current-buffer)))

(message "")
(message "=== DEBUG LOGGING TEST ===")
(message "See *Debug Test Instructions* buffer")
(message "Try: M-x beads-toggle-debug")
(message "==========================")
(message "")

;;; test-debug-logging.el ends here
