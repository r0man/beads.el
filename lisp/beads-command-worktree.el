;;; beads-command-worktree.el --- Worktree command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd worktree' operations,
;; providing an object-oriented interface to bd worktree commands.
;;
;; The bd worktree commands manage git worktrees with proper beads
;; configuration.  When creating a worktree, beads automatically sets up
;; a redirect file so all worktrees share the same .beads database.
;;
;; Command classes:
;; - beads-command-worktree-create: Create worktree with beads redirect
;; - beads-command-worktree-list: List all git worktrees
;; - beads-command-worktree-remove: Remove worktree with safety checks
;; - beads-command-worktree-info: Show info about current worktree
;;
;; Domain types are defined in beads-worktree-types.el:
;; - beads-worktree: Represents a git worktree with beads state
;; - beads-worktree-info: Information about current worktree context
;;
;; Usage:
;;   ;; Create a worktree
;;   (beads-command-worktree-create! :name "feature-auth")
;;   (beads-command-worktree-create! :name "bugfix" :branch "fix-login")
;;
;;   ;; List all worktrees
;;   (beads-command-worktree-list!)
;;
;;   ;; Remove a worktree
;;   (beads-command-worktree-remove! :name "feature-auth")
;;   (beads-command-worktree-remove! :name "stale-work" :force t)
;;
;;   ;; Get info about current worktree
;;   (beads-command-worktree-info!)

;;; Code:

(require 'beads)
(require 'beads-buffer)
(require 'beads-completion)
(require 'beads-worktree-types)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-reader)
(require 'cl-lib)
(require 'transient)

;;; ============================================================
;;; State Variables (for transient menu compatibility)
;;; ============================================================

(defvar beads-worktree--name nil
  "Worktree name for create operation.")

(defvar beads-worktree--branch nil
  "Branch name for worktree create operation.")

(defvar beads-worktree--force nil
  "Force flag for worktree remove operation.")

(defvar beads-worktree--target nil
  "Target worktree name for remove operation.")

;;; ============================================================
;;; Reset Functions
;;; ============================================================

(defun beads-worktree--reset-create-state ()
  "Reset state variables for create operation."
  (setq beads-worktree--name nil
        beads-worktree--branch nil))

(defun beads-worktree--reset-remove-state ()
  "Reset state variables for remove operation."
  (setq beads-worktree--force nil
        beads-worktree--target nil))

(defun beads-worktree--reset-all-state ()
  "Reset all worktree state variables."
  (beads-worktree--reset-create-state)
  (beads-worktree--reset-remove-state))

;;; ============================================================
;;; Validation Functions
;;; ============================================================

(defun beads-worktree--validate-create ()
  "Validate parameters for worktree create.
Returns error message string or nil if valid."
  (cond
   ((not beads-worktree--name)
    "Worktree name is required")
   ((string-empty-p beads-worktree--name)
    "Worktree name cannot be empty")
   (t nil)))

(defun beads-worktree--validate-remove ()
  "Validate parameters for worktree remove.
Returns error message string or nil if valid."
  (cond
   ((not beads-worktree--target)
    "Worktree name is required")
   ((string-empty-p beads-worktree--target)
    "Worktree name cannot be empty")
   (t nil)))

;;; ============================================================
;;; Infix Commands (for transient menu)
;;; ============================================================

(transient-define-infix beads-worktree--infix-name ()
  "Set worktree name for creation."
  :class 'transient-lisp-variable
  :variable 'beads-worktree--name
  :key "-n"
  :description "Name"
  :prompt "Worktree name: "
  :reader (lambda (prompt _initial-input _history)
            (beads-reader-worktree-name prompt nil nil)))

(transient-define-infix beads-worktree--infix-branch ()
  "Set branch name for worktree creation."
  :class 'transient-lisp-variable
  :variable 'beads-worktree--branch
  :key "-b"
  :description "Branch"
  :prompt "Branch name: "
  :reader (lambda (prompt _initial-input _history)
            (beads-reader-worktree-branch prompt nil nil)))

(transient-define-infix beads-worktree--infix-force ()
  "Toggle force flag for worktree removal."
  :class 'transient-lisp-variable
  :variable 'beads-worktree--force
  :key "-f"
  :description "Force (skip safety checks)"
  :reader (lambda (&rest _) (not beads-worktree--force)))

(transient-define-infix beads-worktree--infix-target ()
  "Set target worktree for removal."
  :class 'transient-lisp-variable
  :variable 'beads-worktree--target
  :key "-t"
  :description "Target worktree"
  :prompt "Worktree to remove: "
  :reader (lambda (prompt _initial-input _history)
            (beads-reader-worktree-existing prompt nil nil)))

;;; ============================================================
;;; Suffix Commands (for transient menu)
;;; ============================================================

(transient-define-suffix beads-worktree--create ()
  "Create a new worktree with beads redirect configuration."
  :key "c"
  :description "Create worktree"
  (interactive)
  (let ((error-msg (beads-worktree--validate-create)))
    (if error-msg
        (user-error "Validation failed: %s" error-msg)
      (condition-case err
          (let* ((result (beads-command-worktree-create!
                          :name beads-worktree--name
                          :branch beads-worktree--branch))
                 (path (oref result path))
                 (branch (oref result branch)))
            (message "Created worktree: %s (branch: %s, path: %s)"
                     beads-worktree--name
                     (or branch beads-worktree--name)
                     path)
            (beads-completion-invalidate-worktree-cache)
            (beads-worktree--reset-create-state))
        (error
         (user-error "Failed to create worktree: %s"
                     (error-message-string err)))))))

(transient-define-suffix beads-worktree--list ()
  "List all worktrees with their beads configuration state."
  :key "l"
  :description "List worktrees"
  (interactive)
  (condition-case err
      (let ((worktrees (beads-command-worktree-list!)))
        (if (null worktrees)
            (message "No worktrees found")
          (beads-worktree--display-list worktrees)))
    (error
     (user-error "Failed to list worktrees: %s"
                 (error-message-string err)))))

(transient-define-suffix beads-worktree--remove ()
  "Remove a worktree with safety checks."
  :key "r"
  :description "Remove worktree"
  (interactive)
  (unless beads-worktree--target
    (setq beads-worktree--target
          (beads-reader-worktree-existing "Worktree to remove: " nil nil)))
  (let ((error-msg (beads-worktree--validate-remove)))
    (if error-msg
        (user-error "Validation failed: %s" error-msg)
      (when (or beads-worktree--force
                (y-or-n-p (format "Remove worktree '%s'? "
                                  beads-worktree--target)))
        (condition-case err
            (progn
              (beads-command-worktree-remove!
               :name beads-worktree--target
               :force beads-worktree--force)
              (message "Removed worktree: %s" beads-worktree--target)
              (beads-completion-invalidate-worktree-cache)
              (beads-worktree--reset-remove-state))
          (error
           (user-error "Failed to remove worktree: %s"
                       (error-message-string err))))))))

(transient-define-suffix beads-worktree--info ()
  "Show information about the current worktree context."
  :key "i"
  :description "Info (current)"
  (interactive)
  (condition-case err
      (let ((info (beads-command-worktree-info!)))
        (if (not (oref info is-worktree))
            (message "Not in a worktree (main repository)")
          (message "Worktree: %s\nBranch: %s\nPath: %s\nMain: %s\nBeads state: %s"
                   (or (oref info name) "unknown")
                   (or (oref info branch) "unknown")
                   (or (oref info path) "unknown")
                   (or (oref info main-path) "N/A")
                   (or (oref info beads-state) "unknown"))))
    (error
     (user-error "Failed to get worktree info: %s"
                 (error-message-string err)))))

(transient-define-suffix beads-worktree--reset ()
  "Reset all worktree parameters."
  :key "R"
  :description "Reset all"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-worktree--reset-all-state)
    (message "All fields reset")))

;;; ============================================================
;;; Command Class: beads-command-worktree-create
;;; ============================================================

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-worktree-create (beads-command-json)
  ((name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Worktree name (also used as directory name)."
    :positional 1)
   (branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Branch name for the worktree (--branch).
Default: same as worktree name."
    :long-option "branch"
    :option-type :string
    :transient-key "b"
    :transient-description "--branch"
    :transient-class transient-option
    :transient-argument "--branch="
    :transient-prompt "Branch name: "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd worktree create command.
Creates a git worktree with beads redirect configuration."))

(cl-defmethod beads-command-subcommand ((_command beads-command-worktree-create))
  "Return \"worktree create\" as the CLI subcommand."
  "worktree create")

(cl-defmethod beads-command-validate ((command beads-command-worktree-create))
  "Validate worktree create COMMAND.
Requires name to be set."
  (with-slots (name) command
    (cond
     ((not name) "Worktree name is required")
     ((string-empty-p name) "Worktree name cannot be empty")
     (t nil))))

(cl-defmethod beads-command-parse ((command beads-command-worktree-create) execution)
  "Parse worktree create COMMAND output from EXECUTION.
Returns beads-worktree instance on success."
  (with-slots (json) command
    (if (not json)
        (cl-call-next-method)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (beads-worktree-from-json parsed-json)
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-worktree: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :parsed-json parsed-json
                         :stderr (oref execution stderr)
                         :parse-error err))))))))

;;; ============================================================
;;; Command Class: beads-command-worktree-list
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-worktree-list (beads-command-json)
  ()
  :documentation "Represents bd worktree list command.
Lists all git worktrees with their beads configuration state."))

(cl-defmethod beads-command-subcommand ((_command beads-command-worktree-list))
  "Return \"worktree list\" as the CLI subcommand."
  "worktree list")

(cl-defmethod beads-command-parse ((command beads-command-worktree-list) execution)
  "Parse worktree list COMMAND output from EXECUTION.
Returns list of beads-worktree instances."
  (with-slots (json) command
    (if (not json)
        (cl-call-next-method)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (mapcar #'beads-worktree-from-json (append parsed-json nil))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-worktree list: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :parsed-json parsed-json
                         :stderr (oref execution stderr)
                         :parse-error err))))))))

;;; ============================================================
;;; Command Class: beads-command-worktree-remove
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-worktree-remove (beads-command-json)
  ((name
    :initarg :name
    :type (or null string)
    :initform nil
    :documentation "Worktree name to remove."
    :positional 1)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Skip safety checks (--force).
By default, removal checks for uncommitted changes, unpushed commits,
and stashes."
    :long-option "force"
    :option-type :boolean
    :transient-key "f"
    :transient-description "--force"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd worktree remove command.
Removes a worktree with safety checks (unless --force is used)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-worktree-remove))
  "Return \"worktree remove\" as the CLI subcommand."
  "worktree remove")

(cl-defmethod beads-command-validate ((command beads-command-worktree-remove))
  "Validate worktree remove COMMAND.
Requires name to be set."
  (with-slots (name) command
    (cond
     ((not name) "Worktree name is required")
     ((string-empty-p name) "Worktree name cannot be empty")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-worktree-info
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-worktree-info (beads-command-json)
  ()
  :documentation "Represents bd worktree info command.
Shows information about the current worktree context."))

(cl-defmethod beads-command-subcommand ((_command beads-command-worktree-info))
  "Return \"worktree info\" as the CLI subcommand."
  "worktree info")

(cl-defmethod beads-command-parse ((command beads-command-worktree-info) execution)
  "Parse worktree info COMMAND output from EXECUTION.
Returns beads-worktree-info instance."
  (with-slots (json) command
    (if (not json)
        (cl-call-next-method)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (beads-worktree-info-from-json parsed-json)
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-worktree-info: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :parsed-json parsed-json
                         :stderr (oref execution stderr)
                         :parse-error err))))))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun beads-worktree-find-by-name (name)
  "Find a worktree by NAME from the list of worktrees.
Returns beads-worktree instance or nil if not found."
  (ignore-errors
    (seq-find (lambda (wt)
                (string= (oref wt name) name))
              (beads-command-worktree-list!))))

(defun beads-worktree-find-by-branch (branch)
  "Find a worktree by BRANCH name from the list of worktrees.
Returns beads-worktree instance or nil if not found."
  (seq-find (lambda (wt)
              (string= (oref wt branch) branch))
            (beads-command-worktree-list!)))

(defun beads-worktree-main ()
  "Return the main worktree (the one with is-main = t).
Returns beads-worktree instance or nil if not found."
  (seq-find (lambda (wt)
              (oref wt is-main))
            (beads-command-worktree-list!)))

;;; ============================================================
;;; Interactive Execute Methods
;;; ============================================================

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-worktree-create))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-worktree-list))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-worktree-remove))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-worktree-info))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Transient Menus
;;; ============================================================

;; Individual subcommand transients
;;;###autoload (autoload 'beads-worktree-create "beads-command-worktree" nil t)
(beads-meta-define-transient beads-command-worktree-create "beads-worktree-create"
  "Create a new git worktree with beads redirect.

When creating a worktree, beads automatically sets up a redirect file
so all worktrees share the same .beads database.

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Branch name option"
  beads-option-global-section)

;;;###autoload (autoload 'beads-worktree-list "beads-command-worktree" nil t)
(beads-meta-define-transient beads-command-worktree-list "beads-worktree-list"
  "List all git worktrees.

Shows all worktrees with their beads configuration state:
- shared: Main repository with .beads directory
- redirect: Worktree with redirect to main .beads
- local: Has its own .beads (not recommended)
- none: No beads configuration"
  beads-option-global-section)

;;;###autoload (autoload 'beads-worktree-remove "beads-command-worktree" nil t)
(beads-meta-define-transient beads-command-worktree-remove "beads-worktree-remove"
  "Remove a git worktree.

By default, checks for uncommitted changes, unpushed commits,
and stashes before removing. Use --force to skip safety checks.

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Force option"
  beads-option-global-section)

;;;###autoload (autoload 'beads-worktree-show-info "beads-command-worktree" nil t)
(beads-meta-define-transient beads-command-worktree-info "beads-worktree-show-info"
  "Show information about the current worktree.

Displays whether the current directory is in a worktree,
the worktree name, branch, path to main repository,
and beads configuration state."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-worktree-menu "beads-command-worktree" nil t)
(transient-define-prefix beads-worktree-menu ()
  "Manage git worktrees with beads configuration.

Git worktrees allow parallel development on multiple branches.
Beads automatically sets up redirect files so all worktrees
share the same .beads database."
  ["Worktree Commands"
   ("c" "Create worktree" beads-worktree-create)
   ("l" "List worktrees" beads-worktree-list-display)
   ("i" "Worktree info" beads-worktree-show-info)]
  ["Danger"
   ("d" "Remove worktree" beads-worktree-remove)])

;;; ============================================================
;;; Worktree List Display Mode
;;; ============================================================

(defvar beads-worktree-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'beads-worktree-list-info)
    (define-key map (kbd "d") #'beads-worktree-list-remove)
    (define-key map (kbd "g") #'beads-worktree-list-refresh)
    (define-key map (kbd "c") #'beads-worktree-list-create)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `beads-worktree-list-mode'.")

(define-derived-mode beads-worktree-list-mode tabulated-list-mode
  "Beads-Worktrees"
  "Major mode for displaying beads worktrees.

\\{beads-worktree-list-mode-map}"
  (setq tabulated-list-format
        [("Name" 20 t)
         ("Branch" 25 t)
         ("State" 10 t)
         ("Main" 6 t)
         ("Path" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

(defun beads-worktree--format-entry (worktree)
  "Format WORKTREE as a tabulated-list entry."
  (let ((name (oref worktree name))
        (branch (or (oref worktree branch) ""))
        (state (or (oref worktree beads-state) "none"))
        (is-main (oref worktree is-main))
        (path (or (oref worktree path) "")))
    (list name
          (vector
           (propertize name 'face 'font-lock-keyword-face)
           (propertize branch 'face 'font-lock-string-face)
           (propertize state 'face
                       (pcase state
                         ("shared" 'success)
                         ("redirect" 'font-lock-type-face)
                         ("local" 'warning)
                         (_ 'shadow)))
           (if is-main
               (propertize "Yes" 'face 'font-lock-constant-face)
             "")
           (propertize path 'face 'shadow)))))

(defun beads-worktree--display-list (worktrees)
  "Display WORKTREES in a tabulated list buffer."
  (let* ((buf-name (beads-buffer-name-utility "worktrees"))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (beads-worktree-list-mode)
      (setq tabulated-list-entries
            (mapcar #'beads-worktree--format-entry worktrees))
      (tabulated-list-print t))
    (pop-to-buffer buffer)))

(defun beads-worktree-list-info ()
  "Show info for worktree at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (if name
        (let ((worktree (beads-worktree-find-by-name name)))
          (if worktree
              (message "Worktree: %s\nBranch: %s\nPath: %s\nState: %s%s"
                       (oref worktree name)
                       (or (oref worktree branch) "unknown")
                       (or (oref worktree path) "unknown")
                       (or (oref worktree beads-state) "unknown")
                       (if (oref worktree is-main) " (main)" ""))
            (message "Worktree '%s' not found" name)))
      (message "No worktree at point"))))

(defun beads-worktree-list-remove ()
  "Remove worktree at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (if name
        (when (y-or-n-p (format "Remove worktree '%s'? " name))
          (condition-case err
              (progn
                (beads-command-worktree-remove! :name name)
                (message "Removed worktree: %s" name)
                (beads-completion-invalidate-worktree-cache)
                (beads-worktree-list-refresh))
            (error
             (user-error "Failed to remove worktree: %s"
                         (error-message-string err)))))
      (message "No worktree at point"))))

(defun beads-worktree-list-refresh ()
  "Refresh the worktree list."
  (interactive)
  (beads-completion-invalidate-worktree-cache)
  (condition-case err
      (let ((worktrees (beads-command-worktree-list!)))
        (setq tabulated-list-entries
              (mapcar #'beads-worktree--format-entry worktrees))
        (tabulated-list-print t)
        (message "Worktree list refreshed"))
    (error
     (user-error "Failed to refresh worktrees: %s"
                 (error-message-string err)))))

(defun beads-worktree-list-create ()
  "Create a new worktree from the list view."
  (interactive)
  (call-interactively #'beads-worktree-menu))

;;;###autoload
(defun beads-worktree-list-display ()
  "Display all worktrees in a tabulated list buffer."
  (interactive)
  (beads-check-executable)
  (condition-case err
      (let ((worktrees (beads-command-worktree-list!)))
        (beads-worktree--display-list worktrees))
    (error
     (user-error "Failed to list worktrees: %s"
                 (error-message-string err)))))

(provide 'beads-command-worktree)
;;; beads-command-worktree.el ends here
