;;; beads-worktree.el --- Transient menu for worktree management -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides a transient menu interface for managing git worktrees with
;; beads configuration.  This module integrates with `bd worktree'
;; commands to create, list, remove, and inspect worktrees that share
;; the same beads database.
;;
;; When creating a worktree, beads automatically sets up a redirect file
;; so all worktrees share the same .beads database.  This enables
;; parallel development (e.g., multiple agents or features) while
;; maintaining consistent issue state.
;;
;; Usage:
;;   M-x beads-worktree RET
;;
;; The menu provides:
;; - Create: Create new worktree with optional branch name
;; - List: Display all worktrees in tabulated format
;; - Remove: Remove worktree with safety checks (or force)
;; - Info: Show information about current worktree
;;
;; This module uses the EIEIO command classes from beads-command-worktree.el
;; for executing bd worktree commands.

;;; Code:

(require 'beads)
(require 'beads-buffer)
(require 'beads-command-worktree)
(require 'beads-completion)
(require 'beads-reader)
(require 'transient)

;;; State Variables

(defvar beads-worktree--name nil
  "Worktree name for create operation.")

(defvar beads-worktree--branch nil
  "Branch name for worktree create operation.")

(defvar beads-worktree--force nil
  "Force flag for worktree remove operation.")

(defvar beads-worktree--target nil
  "Target worktree name for remove operation.")

;;; Reset Functions

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

;;; Validation Functions

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

;;; Infix Commands

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

;;; Suffix Commands

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
            ;; Invalidate worktree cache
            (beads-completion-invalidate-worktree-cache)
            ;; Reset state after successful creation
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
  ;; If no target set, prompt for one
  (unless beads-worktree--target
    (setq beads-worktree--target
          (beads-reader-worktree-existing "Worktree to remove: " nil nil)))
  (let ((error-msg (beads-worktree--validate-remove)))
    (if error-msg
        (user-error "Validation failed: %s" error-msg)
      ;; Confirm removal unless force is set
      (when (or beads-worktree--force
                (y-or-n-p (format "Remove worktree '%s'? "
                                  beads-worktree--target)))
        (condition-case err
            (progn
              (beads-command-worktree-remove!
               :name beads-worktree--target
               :force beads-worktree--force)
              (message "Removed worktree: %s" beads-worktree--target)
              ;; Invalidate worktree cache
              (beads-completion-invalidate-worktree-cache)
              ;; Reset state after successful removal
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

;;; Helper Functions

(defun beads-worktree-find-by-name (name)
  "Find worktree by NAME in the current worktree list.
Returns a beads-worktree object or nil if not found."
  (condition-case nil
      (let ((worktrees (beads-command-worktree-list!)))
        (seq-find (lambda (wt) (equal (oref wt name) name)) worktrees))
    (error nil)))

;;; Worktree List Display

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

;;; Main Transient Menu

;;;###autoload (autoload 'beads-worktree-menu "beads-worktree" nil t)
(transient-define-prefix beads-worktree-menu ()
  "Transient menu for worktree management.

Worktrees allow multiple working directories sharing the same git
repository, enabling parallel development (e.g., multiple agents or
features).

When creating a worktree, beads automatically sets up a redirect file
so all worktrees share the same .beads database."
  ["Create Worktree"
   :description "Create new worktree"
   (beads-worktree--infix-name)
   (beads-worktree--infix-branch)]
  ["Remove Worktree"
   :description "Remove existing worktree"
   (beads-worktree--infix-target)
   (beads-worktree--infix-force)]
  ["Actions"
   :description "Worktree operations"
   ("c" beads-worktree--create)
   ("l" beads-worktree--list)
   ("r" beads-worktree--remove)
   ("i" beads-worktree--info)]
  ["Other"
   ("R" beads-worktree--reset)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-worktree)
;;; beads-worktree.el ends here
