;;; beads-agent-prompt-edit-test.el --- Tests for beads-agent-prompt-edit -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for the two-region prompt editor (Phase 1a-ii rewrite,
;; bde-xle9.2).  The two-region SEMANTICS (split parse, blank-system
;; -> nil, cancel sentinel, marker robustness, whitespace trimming,
;; read-only headings, confirm-time default-directory) are covered in
;; beads-agent-phase-1a-ii-test.el; this file covers the module's
;; structural API and lifecycle.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-prompt-edit)
(require 'beads-agent-types)
(require 'beads-git)

;;; Test Helpers

(defmacro beads-agent-prompt-edit-test--with-mock-git (&rest body)
  "Execute BODY with git functions mocked for testing."
  `(cl-letf (((symbol-function 'beads-git-get-project-name)
              (lambda () "test-project"))
             ((symbol-function 'beads-git-get-branch)
              (lambda () "main"))
             ((symbol-function 'beads-buffer-is-main-branch-p)
              (lambda (&optional _branch) t)))
     ,@body))

;;; Mode Tests

(ert-deftest beads-agent-prompt-edit-test-mode-defined ()
  "The major mode is defined."
  (should (fboundp 'beads-agent-prompt-edit-mode)))

(ert-deftest beads-agent-prompt-edit-test-mode-derived-from-text ()
  "Mode is derived from `text-mode'."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (should (derived-mode-p 'text-mode))))

(ert-deftest beads-agent-prompt-edit-test-mode-sets-header-line ()
  "Mode sets a header line."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (should header-line-format)))

;;; Keymap Tests

(ert-deftest beads-agent-prompt-edit-test-keymap-exists ()
  "The keymap exists."
  (should (keymapp beads-agent-prompt-edit-mode-map)))

(ert-deftest beads-agent-prompt-edit-test-keymap-confirm ()
  "C-c C-c is bound to confirm."
  (should (eq (lookup-key beads-agent-prompt-edit-mode-map (kbd "C-c C-c"))
              #'beads-agent-prompt-edit-confirm)))

(ert-deftest beads-agent-prompt-edit-test-keymap-cancel ()
  "C-c C-k is bound to cancel."
  (should (eq (lookup-key beads-agent-prompt-edit-mode-map (kbd "C-c C-k"))
              #'beads-agent-prompt-edit-cancel)))

;;; Buffer-local Variable Tests

(ert-deftest beads-agent-prompt-edit-test-callback-variable ()
  "The callback variable becomes buffer-local when set."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--callback #'ignore)
    (should (local-variable-p 'beads-agent-prompt-edit--callback))))

(ert-deftest beads-agent-prompt-edit-test-issue-id-variable ()
  "The issue-id variable becomes buffer-local when set."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--issue-id "x")
    (should (local-variable-p 'beads-agent-prompt-edit--issue-id))))

;;; Buffer Name

(ert-deftest beads-agent-prompt-edit-test-buffer-name-function ()
  "The buffer-name function exists."
  (should (fboundp 'beads-agent-prompt-edit--buffer-name)))

(ert-deftest beads-agent-prompt-edit-test-buffer-name-includes-issue-id ()
  "The buffer name includes the issue id."
  (beads-agent-prompt-edit-test--with-mock-git
   (let ((name (beads-agent-prompt-edit--buffer-name "test-123")))
     (should (string-match "test-123" name)))))

;;; Show Tests

(ert-deftest beads-agent-prompt-edit-test-show-function-exists ()
  "The show function exists."
  (should (fboundp 'beads-agent-prompt-edit-show)))

(ert-deftest beads-agent-prompt-edit-test-show-creates-buffer-with-headings ()
  "Show creates a buffer containing both region headings."
  (beads-agent-prompt-edit-test--with-mock-git
   (let (buf-name content)
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123" "SYS" "USR" "Task" #'ignore)
       (setq buf-name (buffer-name)
             content (buffer-substring-no-properties
                      (point-min) (point-max)))
       (beads-agent-prompt-edit-cancel))
     (should (string-match "test-123" buf-name))
     (should (string-match "## System prompt" content))
     (should (string-match "## User prompt" content))
     (should (string-match "SYS" content))
     (should (string-match "USR" content)))))

(ert-deftest beads-agent-prompt-edit-test-show-sets-local-vars ()
  "Show sets the buffer-local issue-id and agent-type."
  (beads-agent-prompt-edit-test--with-mock-git
   (let (issue-id agent-type)
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123" "SYS" "USR" "Review" #'ignore)
       (setq issue-id beads-agent-prompt-edit--issue-id
             agent-type beads-agent-prompt-edit--agent-type)
       (beads-agent-prompt-edit-cancel))
     (should (equal issue-id "test-123"))
     (should (equal agent-type "Review")))))

;;; Confirm / Cancel lifecycle

(ert-deftest beads-agent-prompt-edit-test-confirm-kills-buffer ()
  "Confirm kills the edit buffer."
  (beads-agent-prompt-edit-test--with-mock-git
   (let (buf)
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123" "SYS" "USR" "Task" (lambda (_s _u) nil))
       (setq buf (current-buffer))
       (beads-agent-prompt-edit-confirm))
     (should-not (buffer-live-p buf)))))

(ert-deftest beads-agent-prompt-edit-test-cancel-kills-buffer ()
  "Cancel kills the edit buffer."
  (beads-agent-prompt-edit-test--with-mock-git
   (let (buf)
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123" "SYS" "USR" "Task" (lambda (_s _u) nil))
       (setq buf (current-buffer))
       (beads-agent-prompt-edit-cancel))
     (should-not (buffer-live-p buf)))))

(ert-deftest beads-agent-prompt-edit-test-confirm-kills-buffer-before-callback ()
  "Confirm kills the prompt-edit buffer BEFORE invoking the callback.
Otherwise async work spawned from the callback captures a buffer
about to die and the result is silently dropped (regression for
bde-d3eg)."
  (beads-agent-prompt-edit-test--with-mock-git
   (let (buf (callback-buf 'unset))
     (save-window-excursion
       (beads-agent-prompt-edit-show
        "test-123" "SYS" "USR" "Task"
        (lambda (_s _u) (setq callback-buf (current-buffer))))
       (setq buf (current-buffer))
       (beads-agent-prompt-edit-confirm))
     (should-not (buffer-live-p buf))
     (should (buffer-live-p callback-buf))
     (should-not (eq callback-buf buf)))))

(ert-deftest beads-agent-prompt-edit-test-nil-callback-handling ()
  "Confirm handles a nil callback gracefully."
  (beads-agent-prompt-edit-test--with-mock-git
   (save-window-excursion
     (beads-agent-prompt-edit-show "test-123" "S" "U" "Task" nil)
     (should-not (condition-case _err
                     (progn (beads-agent-prompt-edit-confirm) nil)
                   (error t))))))

(ert-deftest beads-agent-prompt-edit-test-cancel-nil-callback-handling ()
  "Cancel handles a nil callback gracefully."
  (beads-agent-prompt-edit-test--with-mock-git
   (save-window-excursion
     (beads-agent-prompt-edit-show "test-123" "S" "U" "Task" nil)
     (should-not (condition-case _err
                     (progn (beads-agent-prompt-edit-cancel) nil)
                   (error t))))))

;;; Header Line

(ert-deftest beads-agent-prompt-edit-test-header-line-function ()
  "The header-line function exists."
  (should (fboundp 'beads-agent-prompt-edit--header-line)))

(ert-deftest beads-agent-prompt-edit-test-header-line-includes-agent-type ()
  "The header line includes the agent type."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--agent-type "Review")
    (setq beads-agent-prompt-edit--issue-id "test-123")
    (should (string-match "Review" (beads-agent-prompt-edit--header-line)))))

(ert-deftest beads-agent-prompt-edit-test-header-line-includes-keybindings ()
  "The header line mentions the keybindings."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--agent-type "Task")
    (setq beads-agent-prompt-edit--issue-id "test-123")
    (let ((h (beads-agent-prompt-edit--header-line)))
      (should (string-match "C-c C-c" h))
      (should (string-match "C-c C-k" h)))))

(ert-deftest beads-agent-prompt-edit-test-header-line-prefixes-icon-in-gui ()
  "Header line prefixes the type name with the role icon under GUI."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--agent-type "Task")
    (setq beads-agent-prompt-edit--issue-id "test-123")
    (let ((beads-agent-display-use-icons t)
          (beads-agent-display-type-icons nil))
      (let* ((type (beads-agent-type-get "Task"))
             (icon (and type (slot-boundp type 'icon) (oref type icon))))
        (skip-unless icon)
        (let ((h (beads-agent-prompt-edit--header-line)))
          (should (string-match-p (regexp-quote icon) h))
          ;; Icon precedes the type name in the header.
          (should (< (string-match-p (regexp-quote icon) h)
                     (string-match-p "Task" h))))))))

(ert-deftest beads-agent-prompt-edit-test-header-line-letter-fallback-in-tty ()
  "Header line omits the icon (no prefix) when icons are disabled."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--agent-type "Task")
    (setq beads-agent-prompt-edit--issue-id "test-123")
    (let ((beads-agent-display-use-icons nil)
          (beads-agent-display-type-icons nil))
      (let* ((type (beads-agent-type-get "Task"))
             (icon (and type (slot-boundp type 'icon) (oref type icon)))
             (letter (and type (oref type letter)))
             (h (beads-agent-prompt-edit--header-line)))
        (skip-unless icon)
        ;; No emoji icon when use-icons is nil.
        (should-not (string-match-p (regexp-quote icon) h))
        ;; Letter prefix appears before the type name ("T Task ...").
        (should (string-match-p
                 (concat (regexp-quote letter) " Task prompt") h))))))

(ert-deftest beads-agent-prompt-edit-test-header-line-unregistered-type ()
  "Header line works for unregistered type names without crashing."
  (with-temp-buffer
    (beads-agent-prompt-edit-mode)
    (setq beads-agent-prompt-edit--agent-type "NoSuchType")
    (setq beads-agent-prompt-edit--issue-id "test-123")
    (let ((h (beads-agent-prompt-edit--header-line)))
      (should (stringp h))
      (should (string-match-p "NoSuchType" h)))))

(provide 'beads-agent-prompt-edit-test)
;;; beads-agent-prompt-edit-test.el ends here
