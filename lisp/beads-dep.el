;;; beads-dep.el --- Dependency management for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))

;;; Commentary:

;; This module provides transient menu interfaces for managing
;; dependencies in Beads.  It supports:
;;
;; - Adding dependencies with type selection
;; - Removing dependencies
;; - Viewing dependency trees
;; - Detecting dependency cycles
;;
;; Commands are context-aware and can detect the current issue ID
;; from beads-list or beads-show buffers.
;;
;; Usage:
;;   M-x beads-dep RET    ; Open dependency management menu

;;; Code:

(require 'beads)
(require 'transient)

;;; Variables

(defvar beads-dep-add--issue-id nil
  "Issue ID for add dependency operation.")

(defvar beads-dep-add--depends-on-id nil
  "Depends-on ID for add dependency operation.")

(defvar beads-dep-add--type "blocks"
  "Dependency type for add operation.")

(defvar beads-dep-remove--issue-id nil
  "Issue ID for remove dependency operation.")

(defvar beads-dep-remove--depends-on-id nil
  "Depends-on ID for remove dependency operation.")

(defvar beads-dep-tree--issue-id nil
  "Issue ID for tree display.")

;;; Utility Functions

(defun beads-dep--detect-issue-id ()
  "Detect current issue ID from context.
Returns issue ID if in beads-list or beads-show buffer, nil otherwise."
  (cond
   ;; In beads-list buffer
   ((and (derived-mode-p 'beads-list-mode)
         (fboundp 'beads-list--current-issue-id))
    (beads-list--current-issue-id))
   ;; In beads-show buffer
   ((and (derived-mode-p 'beads-show-mode)
         (boundp 'beads-show--current-issue-id)
         beads-show--current-issue-id)
    beads-show--current-issue-id)
   ;; No context
   (t nil)))

(defun beads-dep--format-dependency (dep)
  "Format dependency DEP for display.
DEP is an alist with keys: issue_id, depends_on_id, type, status."
  (let ((issue-id (alist-get 'issue_id dep))
        (depends-on-id (alist-get 'depends_on_id dep))
        (type (alist-get 'type dep))
        (status (alist-get 'status dep)))
    (format "%s: %s %s %s"
            (propertize (or status "unknown") 'face 'success)
            (propertize issue-id 'face 'font-lock-constant-face)
            (propertize type 'face 'font-lock-keyword-face)
            (propertize depends-on-id 'face 'font-lock-constant-face))))

;;; Add Dependency

(transient-define-infix beads-dep-add--infix-issue-id ()
  "Specify issue ID for add dependency."
  :class 'transient-option
  :key "i"
  :description "Issue ID"
  :argument "--issue-id="
  :prompt "Issue ID: "
  :reader (lambda (prompt _initial-input _history)
            (let ((id (completing-read prompt (beads--issue-completion-table)
                                      nil nil nil 'beads--issue-id-history)))
              (setq beads-dep-add--issue-id id)
              id)))

(transient-define-infix beads-dep-add--infix-depends-on-id ()
  "Specify depends-on ID for add dependency."
  :class 'transient-option
  :key "d"
  :description "Depends on ID"
  :argument "--depends-on="
  :prompt "Depends on issue ID: "
  :reader (lambda (prompt _initial-input _history)
            (let ((id (completing-read prompt (beads--issue-completion-table)
                                      nil nil nil 'beads--issue-id-history)))
              (setq beads-dep-add--depends-on-id id)
              id)))

(transient-define-infix beads-dep-add--infix-type ()
  "Specify dependency type for add operation."
  :class 'transient-option
  :key "t"
  :description "Dependency type"
  :argument "--type="
  :choices '("blocks" "related" "parent-child" "discovered-from")
  :reader (lambda (prompt _initial-input _history)
            (let ((type (completing-read prompt
                                        '("blocks" "related" "parent-child"
                                          "discovered-from")
                                        nil t nil
                                        'beads--dependency-type-history
                                        "blocks")))
              (setq beads-dep-add--type type)
              type)))

(defun beads-dep-add--validate ()
  "Validate add dependency parameters.
Returns error message string if invalid, nil if valid."
  (cond
   ((or (null beads-dep-add--issue-id)
        (string-empty-p beads-dep-add--issue-id))
    "Issue ID is required")
   ((or (null beads-dep-add--depends-on-id)
        (string-empty-p beads-dep-add--depends-on-id))
    "Depends-on ID is required")
   ((string= beads-dep-add--issue-id beads-dep-add--depends-on-id)
    "Issue cannot depend on itself")
   ((or (null beads-dep-add--type)
        (string-empty-p beads-dep-add--type))
    "Dependency type is required")
   (t nil)))

(transient-define-suffix beads-dep-add--execute ()
  "Execute add dependency command."
  :description "Add dependency"
  (interactive)
  (let ((error-msg (beads-dep-add--validate)))
    (if error-msg
        (user-error "Cannot add dependency: %s" error-msg)
      (condition-case err
          (let* ((result (beads--run-command "dep" "add"
                                             beads-dep-add--issue-id
                                             beads-dep-add--depends-on-id
                                             "--type" beads-dep-add--type))
                 (formatted (beads-dep--format-dependency result)))
            (message "Dependency added: %s" formatted)
            (beads--invalidate-completion-cache)
            (beads-dep-add--reset)
            (when beads-auto-refresh
              (beads-list--refresh-all-buffers)
              (beads-show--refresh-all-buffers)))
        (error
         (message "Failed to add dependency: %s"
                  (error-message-string err)))))))

(transient-define-suffix beads-dep-add--preview ()
  "Preview add dependency command."
  :description "Preview command"
  :transient t
  (interactive)
  (let ((error-msg (beads-dep-add--validate)))
    (if error-msg
        (message "Invalid: %s" error-msg)
      (message "bd dep add %s %s --type %s"
               beads-dep-add--issue-id
               beads-dep-add--depends-on-id
               beads-dep-add--type))))

(defun beads-dep-add--reset ()
  "Reset add dependency state."
  (setq beads-dep-add--issue-id nil
        beads-dep-add--depends-on-id nil
        beads-dep-add--type "blocks"))

(transient-define-suffix beads-dep-add--reset-interactive ()
  "Reset add dependency fields interactively."
  :description "Reset fields"
  :transient t
  (interactive)
  (beads-dep-add--reset)
  (message "Add dependency fields reset"))

;;;###autoload (autoload 'beads-dep-add "beads-dep" nil t)
(transient-define-prefix beads-dep-add (&optional issue-id)
  "Add a dependency to an issue.
If ISSUE-ID is provided, use it as the default issue ID.
Otherwise, detect from context if possible."
  :value (lambda () nil)
  (interactive (list (beads-dep--detect-issue-id)))
  (when issue-id
    (setq beads-dep-add--issue-id issue-id))
  (beads-check-executable)
  ["Add Dependency"
   (beads-dep-add--infix-issue-id)
   (beads-dep-add--infix-depends-on-id)
   (beads-dep-add--infix-type)]
  ["Actions"
   ("a" "Add dependency" beads-dep-add--execute)
   ("P" "Preview command" beads-dep-add--preview)
   ("R" "Reset fields" beads-dep-add--reset-interactive)
   ("q" "Quit" transient-quit-one)])

;;; Remove Dependency

(transient-define-infix beads-dep-remove--infix-issue-id ()
  "Specify issue ID for remove dependency."
  :class 'transient-option
  :key "i"
  :description "Issue ID"
  :argument "--issue-id="
  :prompt "Issue ID: "
  :reader (lambda (prompt _initial-input _history)
            (let ((id (completing-read prompt (beads--issue-completion-table)
                                      nil nil nil 'beads--issue-id-history)))
              (setq beads-dep-remove--issue-id id)
              id)))

(transient-define-infix beads-dep-remove--infix-depends-on-id ()
  "Specify depends-on ID for remove dependency."
  :class 'transient-option
  :key "d"
  :description "Depends on ID"
  :argument "--depends-on="
  :prompt "Depends on issue ID: "
  :reader (lambda (prompt _initial-input _history)
            (let ((id (completing-read prompt (beads--issue-completion-table)
                                      nil nil nil 'beads--issue-id-history)))
              (setq beads-dep-remove--depends-on-id id)
              id)))

(defun beads-dep-remove--validate ()
  "Validate remove dependency parameters.
Returns error message string if invalid, nil if valid."
  (cond
   ((or (null beads-dep-remove--issue-id)
        (string-empty-p beads-dep-remove--issue-id))
    "Issue ID is required")
   ((or (null beads-dep-remove--depends-on-id)
        (string-empty-p beads-dep-remove--depends-on-id))
    "Depends-on ID is required")
   (t nil)))

(transient-define-suffix beads-dep-remove--execute ()
  "Execute remove dependency command."
  :description "Remove dependency"
  (interactive)
  (let ((error-msg (beads-dep-remove--validate)))
    (if error-msg
        (user-error "Cannot remove dependency: %s" error-msg)
      (condition-case err
          (let* ((result (beads--run-command "dep" "remove"
                                             beads-dep-remove--issue-id
                                             beads-dep-remove--depends-on-id))
                 (status (alist-get 'status result)))
            (message "Dependency %s: %s -> %s"
                     (propertize status 'face 'success)
                     beads-dep-remove--issue-id
                     beads-dep-remove--depends-on-id)
            (beads--invalidate-completion-cache)
            (beads-dep-remove--reset)
            (when beads-auto-refresh
              (beads-list--refresh-all-buffers)
              (beads-show--refresh-all-buffers)))
        (error
         (message "Failed to remove dependency: %s"
                  (error-message-string err)))))))

(transient-define-suffix beads-dep-remove--preview ()
  "Preview remove dependency command."
  :description "Preview command"
  :transient t
  (interactive)
  (let ((error-msg (beads-dep-remove--validate)))
    (if error-msg
        (message "Invalid: %s" error-msg)
      (message "bd dep remove %s %s"
               beads-dep-remove--issue-id
               beads-dep-remove--depends-on-id))))

(defun beads-dep-remove--reset ()
  "Reset remove dependency state."
  (setq beads-dep-remove--issue-id nil
        beads-dep-remove--depends-on-id nil))

(transient-define-suffix beads-dep-remove--reset-interactive ()
  "Reset remove dependency fields interactively."
  :description "Reset fields"
  :transient t
  (interactive)
  (beads-dep-remove--reset)
  (message "Remove dependency fields reset"))

;;;###autoload (autoload 'beads-dep-remove "beads-dep" nil t)
(transient-define-prefix beads-dep-remove (&optional issue-id)
  "Remove a dependency from an issue.
If ISSUE-ID is provided, use it as the default issue ID.
Otherwise, detect from context if possible."
  :value (lambda () nil)
  (interactive (list (beads-dep--detect-issue-id)))
  (when issue-id
    (setq beads-dep-remove--issue-id issue-id))
  (beads-check-executable)
  ["Remove Dependency"
   (beads-dep-remove--infix-issue-id)
   (beads-dep-remove--infix-depends-on-id)]
  ["Actions"
   ("r" "Remove dependency" beads-dep-remove--execute)
   ("P" "Preview command" beads-dep-remove--preview)
   ("R" "Reset fields" beads-dep-remove--reset-interactive)
   ("q" "Quit" transient-quit-one)])

;;; Show Dependency Tree

(defvar beads-dep-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'beads-dep-tree-refresh)
    (define-key map (kbd "RET") #'beads-dep-tree-show-issue)
    map)
  "Keymap for `beads-dep-tree-mode'.")

(define-derived-mode beads-dep-tree-mode special-mode "Beads-Dep-Tree"
  "Major mode for displaying Beads dependency trees.

\\{beads-dep-tree-mode-map}"
  :group 'beads
  (setq truncate-lines nil)
  (setq buffer-read-only t))

(defun beads-dep-tree--render-issue (issue)
  "Render ISSUE in dependency tree format."
  (let ((id (alist-get 'id issue))
        (title (alist-get 'title issue))
        (status (alist-get 'status issue))
        (depth (or (alist-get 'depth issue) 0))
        (truncated (alist-get 'truncated issue)))
    ;; Indentation
    (insert (make-string (* depth 2) ?\s))
    ;; Tree connector
    (when (> depth 0)
      (insert "└─ "))
    ;; Issue ID
    (insert (propertize id 'face 'font-lock-constant-face
                       'beads-issue-id id))
    (insert " ")
    ;; Status badge
    (insert (propertize (upcase status)
                       'face (pcase status
                              ("open" 'success)
                              ("in_progress" 'warning)
                              ("blocked" 'error)
                              ("closed" 'shadow)
                              (_ 'default))))
    (insert " ")
    ;; Title
    (insert (propertize title 'face 'default))
    (when truncated
      (insert " " (propertize "[truncated]" 'face 'shadow)))
    (insert "\n")))

(defun beads-dep-tree--render (issues issue-id)
  "Render ISSUES tree into current buffer for ISSUE-ID."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Header
    (insert (propertize (format "Dependency Tree for %s" issue-id)
                       'face 'bold)
            "\n")
    (insert (propertize (make-string 60 ?═) 'face 'shadow) "\n\n")
    ;; Issues
    (if (zerop (length issues))
        (insert (propertize "No dependencies found" 'face 'shadow) "\n")
      (mapc #'beads-dep-tree--render-issue issues))
    (insert "\n")
    ;; Footer
    (insert (propertize "Commands:" 'face 'bold) "\n")
    (insert "  RET - show issue at point\n")
    (insert "  g   - refresh\n")
    (insert "  q   - quit\n"))
  (goto-char (point-min)))

(defun beads-dep-tree--get-issue-at-point ()
  "Get issue ID at point in dependency tree buffer."
  (get-text-property (point) 'beads-issue-id))

(defun beads-dep-tree-show-issue ()
  "Show issue at point in dependency tree."
  (interactive)
  (if-let ((id (beads-dep-tree--get-issue-at-point)))
      (if (fboundp 'beads-show)
          (beads-show id)
        (message "Issue: %s" id))
    (user-error "No issue at point")))

(defun beads-dep-tree-refresh ()
  "Refresh dependency tree display."
  (interactive)
  (when (and (derived-mode-p 'beads-dep-tree-mode)
             beads-dep-tree--issue-id)
    (message "Refreshing dependency tree...")
    (let* ((issues (beads--run-command "dep" "tree"
                                       beads-dep-tree--issue-id)))
      (beads-dep-tree--render (append issues nil)
                              beads-dep-tree--issue-id)
      (message "Dependency tree refreshed"))))

;;;###autoload
(defun beads-dep-tree (&optional issue-id)
  "Display dependency tree for ISSUE-ID.
If ISSUE-ID is not provided, prompt for it or detect from context."
  (interactive
   (list (or (beads-dep--detect-issue-id)
             (completing-read "Issue ID: " (beads--issue-completion-table)
                            nil nil nil 'beads--issue-id-history))))
  (beads-check-executable)
  (when (or (null issue-id) (string-empty-p issue-id))
    (user-error "Issue ID is required"))
  (let* ((issues (beads--run-command "dep" "tree" issue-id))
         (buffer (get-buffer-create (format "*beads-dep-tree: %s*"
                                           issue-id))))
    (with-current-buffer buffer
      (beads-dep-tree-mode)
      (setq-local beads-dep-tree--issue-id issue-id)
      (beads-dep-tree--render (append issues nil) issue-id))
    (pop-to-buffer buffer)))

;;; Check Dependency Cycles

(defvar beads-dep-cycles-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'beads-dep-cycles-refresh)
    map)
  "Keymap for `beads-dep-cycles-mode'.")

(define-derived-mode beads-dep-cycles-mode special-mode "Beads-Dep-Cycles"
  "Major mode for displaying Beads dependency cycles.

\\{beads-dep-cycles-mode-map}"
  :group 'beads
  (setq truncate-lines nil)
  (setq buffer-read-only t))

(defun beads-dep-cycles--render (cycles)
  "Render CYCLES into current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Header
    (insert (propertize "Dependency Cycles" 'face 'bold) "\n")
    (insert (propertize (make-string 60 ?═) 'face 'shadow) "\n\n")
    ;; Cycles
    (if (zerop (length cycles))
        (insert (propertize "✓ No dependency cycles detected"
                          'face 'success)
                "\n")
      (insert (propertize (format "⚠ Found %d cycle(s):"
                                 (length cycles))
                        'face 'error)
              "\n\n")
      (dotimes (i (length cycles))
        (let ((cycle (aref cycles i)))
          (insert (propertize (format "Cycle %d: " (1+ i))
                            'face 'warning))
          (insert (mapconcat (lambda (id)
                              (propertize id
                                        'face 'font-lock-constant-face))
                            cycle
                            " → "))
          (insert " → " (propertize (aref cycle 0)
                                   'face 'font-lock-constant-face))
          (insert "\n"))))
    (insert "\n")
    ;; Footer
    (insert (propertize "Commands:" 'face 'bold) "\n")
    (insert "  g - refresh\n")
    (insert "  q - quit\n"))
  (goto-char (point-min)))

(defun beads-dep-cycles-refresh ()
  "Refresh dependency cycles display."
  (interactive)
  (when (derived-mode-p 'beads-dep-cycles-mode)
    (message "Checking for dependency cycles...")
    (let ((cycles (beads--run-command "dep" "cycles")))
      (beads-dep-cycles--render cycles)
      (message "Dependency cycles check complete"))))

;;;###autoload
(defun beads-dep-cycles ()
  "Check for dependency cycles and display results."
  (interactive)
  (beads-check-executable)
  (let* ((cycles (beads--run-command "dep" "cycles"))
         (buffer (get-buffer-create "*beads-dep-cycles*")))
    (with-current-buffer buffer
      (beads-dep-cycles-mode)
      (beads-dep-cycles--render cycles))
    (pop-to-buffer buffer)))

;;; Main Menu

;;;###autoload (autoload 'beads-dep "beads-dep" nil t)
(transient-define-prefix beads-dep ()
  "Manage dependencies in Beads.

This menu provides access to all dependency management operations:
- Add dependencies with type selection
- Remove dependencies
- View dependency trees
- Detect dependency cycles"
  ["Dependency Operations"
   ("a" "Add dependency" beads-dep-add)
   ("r" "Remove dependency" beads-dep-remove)
   ("t" "Show dependency tree" beads-dep-tree)
   ("c" "Check for cycles" beads-dep-cycles)]
  ["Other"
   ("q" "Quit" transient-quit-one)])

(provide 'beads-dep)
;;; beads-dep.el ends here
