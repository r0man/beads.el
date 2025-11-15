;;; beads-dep.el --- Dependency management for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

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
(require 'beads-command)
(require 'beads-option)
(require 'transient)

;;; Variables

(defvar beads-dep-tree--issue-id nil
  "Issue ID for tree display (buffer-local).")

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

(defun beads-dep-add--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:issue-id STRING :depends-on-id STRING :type STRING)."
  (let* ((issue-id (transient-arg-value "--issue-id=" args))
         (depends-on-id (transient-arg-value "--depends-on=" args))
         (type (transient-arg-value "--type=" args)))
    (list :issue-id issue-id
          :depends-on-id depends-on-id
          :type type)))

(defun beads-dep-add--validate (issue-id depends-on-id type)
  "Validate add dependency parameters.
ISSUE-ID, DEPENDS-ON-ID, and TYPE are the dependency parameters.
Returns error message string if invalid, nil if valid."
  (cond
   ((or (null issue-id)
        (string-empty-p issue-id))
    "Issue ID is required")
   ((or (null depends-on-id)
        (string-empty-p depends-on-id))
    "Depends-on ID is required")
   ((string= issue-id depends-on-id)
    "Issue cannot depend on itself")
   ((or (null type)
        (string-empty-p type))
    "Dependency type is required")
   (t nil)))

(transient-define-suffix beads-dep-add--execute ()
  "Execute add dependency command."
  :description "Add dependency"
  (interactive)
  (let* ((args (transient-args 'beads-dep-add))
         (parsed (beads-dep-add--parse-transient-args args))
         (issue-id (plist-get parsed :issue-id))
         (depends-on-id (plist-get parsed :depends-on-id))
         (type (plist-get parsed :type))
         (error-msg (beads-dep-add--validate issue-id depends-on-id type)))
    (if error-msg
        (user-error "Cannot add dependency: %s" error-msg)
      (condition-case err
          (let* ((result (beads-command-dep-add!
                          :issue-id issue-id
                          :depends-on-id depends-on-id
                          :dep-type type))
                 (formatted (beads-dep--format-dependency result)))
            (message "Dependency added: %s" formatted)
            (beads--invalidate-completion-cache)
            (when beads-auto-refresh
              ;; TODO: Implement buffer refresh functions
              ;; (beads-list--refresh-all-buffers)
              ;; (beads-show--refresh-all-buffers)
              nil))
        (error
         (message "Failed to add dependency: %s"
                  (error-message-string err)))))))

(transient-define-suffix beads-dep-add--preview ()
  "Preview add dependency command."
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-dep-add))
         (parsed (beads-dep-add--parse-transient-args args))
         (issue-id (plist-get parsed :issue-id))
         (depends-on-id (plist-get parsed :depends-on-id))
         (type (plist-get parsed :type))
         (error-msg (beads-dep-add--validate issue-id depends-on-id type)))
    (if error-msg
        (message "Invalid: %s" error-msg)
      (message "bd dep add %s %s --type %s"
               issue-id
               depends-on-id
               type))))

(transient-define-suffix beads-dep-add--reset ()
  "Reset add dependency fields."
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    ;; Clear transient's argument state using transient-reset
    (transient-reset)
    ;; Refresh the transient display to show cleared state
    (transient--redisplay)
    (message "All fields reset")))

(transient-define-prefix beads-dep-add--menu ()
  "Transient menu for adding a dependency."
  :value (lambda ()
           (let ((detected-id (beads-dep--detect-issue-id)))
             (when detected-id
               (list (concat "--issue-id=" detected-id)))))
  (interactive)
  (beads-check-executable)
  ["Add Dependency"
   (beads-option-dep-add-issue-id)
   (beads-option-dep-add-depends-on-id)
   (beads-option-dep-add-type)]
  ["Actions"
   ("a" "Add dependency" beads-dep-add--execute)
   ("P" "Preview command" beads-dep-add--preview)
   ("R" "Reset fields" beads-dep-add--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-dep-add (&optional issue-id)
  "Add a dependency to an issue.

This function provides an interactive interface for adding
dependencies via a transient menu.  The function is context-aware
and automatically detects the issue ID from beads-list or beads-show
buffers.

If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user."
  (interactive (list (beads-dep--detect-issue-id)))
  ;; Suppress unused argument warning
  (ignore issue-id)
  ;; Show the transient menu
  (beads-dep-add--menu))

;;; Remove Dependency

(defun beads-dep-remove--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:issue-id STRING :depends-on-id STRING)."
  (let* ((issue-id (transient-arg-value "--issue-id=" args))
         (depends-on-id (transient-arg-value "--depends-on=" args)))
    (list :issue-id issue-id
          :depends-on-id depends-on-id)))

(defun beads-dep-remove--validate (issue-id depends-on-id)
  "Validate remove dependency parameters.
ISSUE-ID and DEPENDS-ON-ID are the dependency parameters.
Returns error message string if invalid, nil if valid."
  (cond
   ((or (null issue-id)
        (string-empty-p issue-id))
    "Issue ID is required")
   ((or (null depends-on-id)
        (string-empty-p depends-on-id))
    "Depends-on ID is required")
   (t nil)))

(transient-define-suffix beads-dep-remove--execute ()
  "Execute remove dependency command."
  :description "Remove dependency"
  (interactive)
  (let* ((args (transient-args 'beads-dep-remove))
         (parsed (beads-dep-remove--parse-transient-args args))
         (issue-id (plist-get parsed :issue-id))
         (depends-on-id (plist-get parsed :depends-on-id))
         (error-msg (beads-dep-remove--validate issue-id depends-on-id)))
    (if error-msg
        (user-error "Cannot remove dependency: %s" error-msg)
      (condition-case err
          (let* ((result (beads-command-dep-remove!
                          :issue-id issue-id
                          :depends-on-id depends-on-id))
                 (status (alist-get 'status result)))
            (message "Dependency %s: %s -> %s"
                     (propertize status 'face 'success)
                     issue-id
                     depends-on-id)
            (beads--invalidate-completion-cache)
            (when beads-auto-refresh
              ;; TODO: Implement buffer refresh functions
              ;; (beads-list--refresh-all-buffers)
              ;; (beads-show--refresh-all-buffers)
              nil))
        (error
         (message "Failed to remove dependency: %s"
                  (error-message-string err)))))))

(transient-define-suffix beads-dep-remove--preview ()
  "Preview remove dependency command."
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-dep-remove))
         (parsed (beads-dep-remove--parse-transient-args args))
         (issue-id (plist-get parsed :issue-id))
         (depends-on-id (plist-get parsed :depends-on-id))
         (error-msg (beads-dep-remove--validate issue-id depends-on-id)))
    (if error-msg
        (message "Invalid: %s" error-msg)
      (message "bd dep remove %s %s"
               issue-id
               depends-on-id))))

(transient-define-suffix beads-dep-remove--reset ()
  "Reset remove dependency fields."
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    ;; Clear transient's argument state using transient-reset
    (transient-reset)
    ;; Refresh the transient display to show cleared state
    (transient--redisplay)
    (message "All fields reset")))

(transient-define-prefix beads-dep-remove--menu ()
  "Transient menu for removing a dependency."
  :value (lambda ()
           (let ((detected-id (beads-dep--detect-issue-id)))
             (when detected-id
               (list (concat "--issue-id=" detected-id)))))
  (interactive)
  (beads-check-executable)
  ["Remove Dependency"
   (beads-option-dep-remove-issue-id)
   (beads-option-dep-remove-depends-on-id)]
  ["Actions"
   ("r" "Remove dependency" beads-dep-remove--execute)
   ("P" "Preview command" beads-dep-remove--preview)
   ("R" "Reset fields" beads-dep-remove--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-dep-remove (&optional issue-id)
  "Remove a dependency from an issue.

This function provides an interactive interface for removing
dependencies via a transient menu.  The function is context-aware
and automatically detects the issue ID from beads-list or beads-show
buffers.

If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user."
  (interactive (list (beads-dep--detect-issue-id)))
  ;; Suppress unused argument warning
  (ignore issue-id)
  ;; Show the transient menu
  (beads-dep-remove--menu))

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
  (if-let* ((id (beads-dep-tree--get-issue-at-point)))
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
    (let* ((issues (beads-command-dep-tree!
                    :issue-id beads-dep-tree--issue-id)))
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
  (let* ((issues (beads-command-dep-tree! :issue-id issue-id))
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
    (let ((cycles (beads-command-dep-cycles!)))
      (beads-dep-cycles--render cycles)
      (message "Dependency cycles check complete"))))

;;;###autoload
(defun beads-dep-cycles ()
  "Check for dependency cycles and display results."
  (interactive)
  (beads-check-executable)
  (let* ((cycles (beads-command-dep-cycles!))
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
