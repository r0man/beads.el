;;; beads-agent-claudemacs.el --- claudemacs backend for beads-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides the claudemacs backend for beads-agent.
;; It integrates with the claudemacs package (cpoile/claudemacs) to enable
;; AI-assisted issue work through Claude Code.
;;
;; claudemacs is an AI pair programming package for Emacs that uses the
;; `eat' terminal emulator and supports multiple AI tools (Claude, Codex,
;; Gemini).  It features system notifications, session resume, transient
;; menus, and context-aware file references.
;;
;; The backend implements all required protocol methods:
;; - beads-agent-backend-available-p
;; - beads-agent-backend-start
;; - beads-agent-backend-stop
;; - beads-agent-backend-session-active-p
;; - beads-agent-backend-switch-to-buffer
;; - beads-agent-backend-send-prompt
;; - beads-agent-backend-get-buffer
;;
;; Usage:
;;   The backend is automatically registered when this file is loaded.
;;   Ensure claudemacs is installed from:
;;   https://github.com/cpoile/claudemacs
;;
;; Buffer naming convention:
;;   Claudemacs uses *claudemacs:TOOL:SESSION-ID* or *claudemacs:TOOL-N:SESSION-ID*
;;   where TOOL is claude/codex/gemini, N is instance number, and SESSION-ID
;;   is the workspace/project name.

;;; Code:

(require 'beads-agent-backend)
(require 'seq)

;;; Eat GV-Setter Fix
;;
;; The claudemacs package uses `setf' with `eat-term-parameter', which requires
;; eat's `gv-setter' to be defined.  If claudemacs was byte-compiled without eat
;; loaded, the setf expansion fails with "(void-function (setf eat-term-parameter))".
;;
;; This fix ensures:
;; 1. Eat is loaded before claudemacs (so `gv-setter' is available at load time)
;; 2. A fallback advice that uses `eat-term-set-parameter' directly if setf fails

(declare-function eat-term-set-parameter "eat")
(declare-function claudemacs--get-buffer "claudemacs")
(declare-function claudemacs--bell-handler "claudemacs")

(defvar beads-agent-claudemacs--bell-handler-advice-installed nil
  "Non-nil if the bell handler advice has been installed.")

(defun beads-agent-claudemacs--setup-bell-handler-fixed ()
  "Set up the bell handler using direct setter instead of setf.
This avoids the `gv-setter' issue when claudemacs is byte-compiled
without eat loaded."
  (when (and (fboundp 'claudemacs--get-buffer)
             (claudemacs--get-buffer))
    (with-current-buffer (claudemacs--get-buffer)
      (when (and (boundp 'eat-terminal) (symbol-value 'eat-terminal))
        ;; Use the direct setter function instead of setf
        (eat-term-set-parameter (symbol-value 'eat-terminal)
                                'ring-bell-function
                                (when (fboundp 'claudemacs--bell-handler)
                                  #'claudemacs--bell-handler))))))

(defun beads-agent-claudemacs--advice-setup-bell-handler (orig-fun)
  "Advice for `claudemacs-setup-bell-handler' to handle `gv-setter' issues.
ORIG-FUN is the original function.  If it fails with the setf error,
we use the direct setter instead."
  (condition-case err
      (funcall orig-fun)
    (void-function
     ;; The escaped symbol `\(setf\ eat-term-parameter\)' represents the
     ;; generalized variable setter function name "(setf eat-term-parameter)".
     ;; Emacs creates these symbols for setf expansions.
     (if (eq (cadr err) '\(setf\ eat-term-parameter\))
         (beads-agent-claudemacs--setup-bell-handler-fixed)
       (signal (car err) (cdr err))))))

(defun beads-agent-claudemacs--install-bell-handler-advice ()
  "Install advice on `claudemacs-setup-bell-handler' if not already installed."
  (unless beads-agent-claudemacs--bell-handler-advice-installed
    (when (fboundp 'claudemacs-setup-bell-handler)
      (advice-add 'claudemacs-setup-bell-handler :around
                  #'beads-agent-claudemacs--advice-setup-bell-handler)
      (setq beads-agent-claudemacs--bell-handler-advice-installed t))))

(defun beads-agent-claudemacs--ensure-eat-gv-setter ()
  "Ensure eat's `gv-setter' for `eat-term-parameter' is available.
This must be called before claudemacs is loaded."
  ;; Load eat first to ensure gv-setter is defined
  (when (or (featurep 'eat) (require 'eat nil t))
    ;; Verify the gv-setter is available by checking if eat-term-set-parameter exists
    (unless (fboundp 'eat-term-set-parameter)
      (warn "eat-term-set-parameter not found - bell handler may fail"))
    ;; Install advice as fallback if claudemacs is already loaded
    ;; Otherwise, advice will be installed after require in beads-agent-backend-start
    (when (featurep 'claudemacs)
      (beads-agent-claudemacs--install-bell-handler-advice))))

;;; External Function Declarations

;; From claudemacs.el - session management
(declare-function claudemacs--start "claudemacs")
(declare-function claudemacs--project-root "claudemacs")
(declare-function claudemacs--list-sessions-for-workspace "claudemacs")
(declare-function claudemacs--get-current-session-buffer "claudemacs")
(declare-function claudemacs--is-claudemacs-buffer-p "claudemacs")

;; From claudemacs.el - user commands
(declare-function claudemacs-kill "claudemacs")
(declare-function claudemacs-switch-to-session "claudemacs")

;; From claudemacs.el - messaging
(declare-function claudemacs--send-message-to-claude "claudemacs")

;; From eat.el - terminal process management
(declare-function eat-kill-process "eat")

;; Declare external variables to avoid compiler warnings
(defvar claudemacs-default-tool)
(defvar claudemacs--cwd)

;;; Customization

(defgroup beads-agent-claudemacs nil
  "Claudemacs backend for beads-agent."
  :group 'beads-agent
  :prefix "beads-agent-claudemacs-")

(defcustom beads-agent-claudemacs-tool 'claude
  "Which claudemacs tool to use for agent sessions.
Must be a symbol registered in `claudemacs-tool-registry'.
Common values are `claude', `codex', or `gemini'."
  :type '(choice (const :tag "Claude" claude)
                 (const :tag "Codex" codex)
                 (const :tag "Gemini" gemini)
                 (symbol :tag "Other tool"))
  :group 'beads-agent-claudemacs)

;;; Helper Functions

(defun beads-agent-claudemacs--find-buffers (dir)
  "Find claudemacs buffers for directory DIR.
Claudemacs buffers are named `*claudemacs:TOOL:SESSION-ID*' or
`*claudemacs:TOOL-N:SESSION-ID*' where SESSION-ID is derived from
the workspace/project name.

Returns a list of matching buffers, most recent first."
  ;; Normalize directory: expand, resolve symlinks, strip trailing slash
  (let* ((normalized-dir (directory-file-name
                          (file-truename (expand-file-name dir))))
         (result nil))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (boundp 'claudemacs--cwd)
                     claudemacs--cwd
                     (string= (directory-file-name
                               (file-truename (expand-file-name claudemacs--cwd)))
                              normalized-dir))
            (push buf result)))))
    (nreverse result)))

(defun beads-agent-claudemacs--buffer-has-process-p (buf)
  "Return non-nil if buffer BUF has an active process."
  (and (buffer-live-p buf)
       (get-buffer-process buf)))

;;; Backend Class

(defclass beads-agent-backend-claudemacs (beads-agent-backend)
  ((name :initform "claudemacs")
   (priority :initform 35)
   (description :initform "AI pair programming via eat terminal"))
  :documentation "Backend for claudemacs integration.
Uses cpoile's claudemacs package for AI pair programming sessions.")

;;; Protocol Implementation

(cl-defmethod beads-agent-backend-available-p
    ((_backend beads-agent-backend-claudemacs))
  "Check if claudemacs is available.
Verifies the package is loaded and key functions exist."
  (and ;; Check eat terminal emulator FIRST (needed for gv-setter)
       (or (featurep 'eat)
           (require 'eat nil t))
       ;; Ensure gv-setter workaround is in place
       (progn (beads-agent-claudemacs--ensure-eat-gv-setter) t)
       ;; Check claudemacs package
       (or (featurep 'claudemacs)
           (require 'claudemacs nil t))
       (fboundp 'claudemacs--start)
       (fboundp 'claudemacs-kill)
       (fboundp 'claudemacs--send-message-to-claude)
       ;; Verify claude command is available in PATH
       (executable-find "claude")))

(cl-defmethod beads-agent-backend-start
    ((_backend beads-agent-backend-claudemacs) _issue prompt)
  "Start claudemacs session with PROMPT.
ISSUE is ignored as claudemacs works per-project/workspace.
The working directory is determined by the caller (may be a worktree).
Returns the claudemacs buffer as the session handle."
  ;; Pre-flight checks with helpful error messages
  ;; Load eat FIRST to ensure gv-setter for eat-term-parameter is available
  (unless (or (featurep 'eat) (require 'eat nil t))
    (error "Eat terminal emulator not found.  Install from MELPA: M-x package-install RET eat"))
  ;; Ensure gv-setter workaround is in place before loading claudemacs
  (beads-agent-claudemacs--ensure-eat-gv-setter)
  (unless (or (featurep 'claudemacs) (require 'claudemacs nil t))
    (error "Claudemacs not found.  Install from: https://github.com/cpoile/claudemacs"))
  (unless (executable-find "claude")
    (error "Claude command not found in PATH.  Install: npm install -g @anthropic-ai/claude-code"))
  (require 'claudemacs)
  ;; Install bell handler advice now that claudemacs is loaded
  (beads-agent-claudemacs--install-bell-handler-advice)
  ;; default-directory is set by beads-agent-start (may be worktree)
  ;; Bind BD_NO_DAEMON=1 to disable bd daemon (not supported in worktrees)
  (let* ((working-dir default-directory)
         (process-environment (cons "BD_NO_DAEMON=1" process-environment))
         buffer)
    ;; Start claudemacs session in the working directory
    ;; Pass prompt as CLI argument via the &rest args parameter.
    ;; claudemacs--start signature: (work-dir &optional tool instance-num &rest args)
    ;; The claude CLI accepts: claude [options] [prompt]
    (condition-case err
        (progn
          ;; Pass nil for tool (use default), nil for instance-num (auto),
          ;; then "--" and prompt as args to pass to the CLI
          (claudemacs--start working-dir nil nil "--" prompt)
          ;; Get the buffer that was just created
          (setq buffer (car (beads-agent-claudemacs--find-buffers working-dir)))
          (unless buffer
            (error "Claudemacs session started but buffer not found"))
          buffer)
      (error
       (error "Failed to start claudemacs: %s"
              (error-message-string err))))))

(cl-defmethod beads-agent-backend-stop
    ((_backend beads-agent-backend-claudemacs) session)
  "Stop claudemacs SESSION."
  (require 'claudemacs)
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-claudemacs--find-buffers working-dir)))
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          ;; Bind inhibit-read-only to allow process sentinel to modify buffer.
          ;; Claudemacs buffers may be read-only, but the process sentinel needs
          ;; to write cleanup messages when the process terminates.
          (let ((inhibit-read-only t))
            ;; Kill the terminal process first
            (when (get-buffer-process buf)
              (condition-case nil
                  (eat-kill-process)
                (error nil)))
            ;; Then kill the buffer
            (kill-buffer buf)))))))

(cl-defmethod beads-agent-backend-session-active-p
    ((_backend beads-agent-backend-claudemacs) session)
  "Check if claudemacs SESSION is active.
Returns non-nil if a claudemacs buffer for the session's directory exists
and has a live process."
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-claudemacs--find-buffers working-dir)))
    (and buffers
         (cl-some #'beads-agent-claudemacs--buffer-has-process-p buffers))))

(cl-defmethod beads-agent-backend-switch-to-buffer
    ((_backend beads-agent-backend-claudemacs) session)
  "Switch to claudemacs buffer for SESSION.
If no claudemacs session exists, starts a new one automatically."
  ;; First try the session's stored buffer (renamed to beads format)
  (let ((stored-buffer (beads-agent-session-buffer session)))
    (if (and stored-buffer (buffer-live-p stored-buffer))
        (pop-to-buffer stored-buffer)
      ;; Fall back to claudemacs's normal switching behavior
      (require 'claudemacs)
      (let* ((working-dir (beads-agent-session-working-dir session))
             (buffers (beads-agent-claudemacs--find-buffers working-dir))
             (active-buf (cl-find-if #'beads-agent-claudemacs--buffer-has-process-p
                                     buffers)))
        (if active-buf
            ;; Session exists - switch to it
            (pop-to-buffer active-buf)
          ;; No active session - start a new one
          (message "Claudemacs session expired, starting new one...")
          (let ((default-directory working-dir))
            (claudemacs--start working-dir)))))))

(cl-defmethod beads-agent-backend-send-prompt
    ((_backend beads-agent-backend-claudemacs) session prompt)
  "Send PROMPT to claudemacs for SESSION."
  (require 'claudemacs)
  (let* ((working-dir (beads-agent-session-working-dir session))
         (buffers (beads-agent-claudemacs--find-buffers working-dir))
         (active-buf (cl-find-if #'beads-agent-claudemacs--buffer-has-process-p
                                 buffers)))
    (if active-buf
        (with-current-buffer active-buf
          (claudemacs--send-message-to-claude prompt))
      (error "No active claudemacs session for %s" working-dir))))

(cl-defmethod beads-agent-backend-get-buffer
    ((_backend beads-agent-backend-claudemacs) session)
  "Return the claudemacs buffer for SESSION, or nil if not available.
First checks if the session has a stored buffer (after renaming),
then falls back to pattern-based buffer lookup."
  ;; First check the session's stored buffer (set after renaming)
  (let ((stored-buffer (beads-agent-session-buffer session)))
    (if (and stored-buffer (buffer-live-p stored-buffer))
        stored-buffer
      ;; Fall back to pattern-based lookup
      (let* ((working-dir (beads-agent-session-working-dir session))
             (buffers (beads-agent-claudemacs--find-buffers working-dir)))
        (cl-find-if #'beads-agent-claudemacs--buffer-has-process-p buffers)))))

;;; Registration

(beads-agent--register-backend
 (beads-agent-backend-claudemacs))

(provide 'beads-agent-claudemacs)
;;; beads-agent-claudemacs.el ends here
