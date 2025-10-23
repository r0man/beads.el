;;; beads-create.el --- Transient menu for creating issues -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides a transient menu interface for creating new issues in Beads.
;; This module uses the transient library to create an interactive menu
;; that allows users to set all parameters for `bd create` command.
;;
;; Usage:
;;   M-x beads-create RET
;;
;; The menu allows setting:
;; - Issue type (bug, feature, task, epic, chore)
;; - Priority (0-4)
;; - Description (multiline)
;; - Custom ID (optional, for parallel workers)
;; - Dependencies (optional, format: type:issue-id)
;;
;; After setting parameters, the suffix command executes bd create
;; and displays the created issue.

;;; Code:

(require 'beads)
(require 'transient)

;;; Transient State Variables (Only for Multi-line Fields)

(defvar beads-create--description nil
  "Multi-line description content.")

(defvar beads-create--acceptance nil
  "Multi-line acceptance criteria content.")

(defvar beads-create--design nil
  "Multi-line design notes content.")

;;; Utility Functions

(defun beads-create--reset-state ()
  "Reset only multi-line state variables."
  (setq beads-create--description nil
        beads-create--acceptance nil
        beads-create--design nil))

;;; Multi-line Field Editing

(defun beads-create--edit-text-multiline (current-value callback field-name)
  "Edit text in a multiline buffer.
CURRENT-VALUE is the initial text, CALLBACK is called with the result,
FIELD-NAME is shown in the buffer name and messages.
After editing, the transient menu is re-displayed."
  (let* ((buffer-name (format "*beads-%s*" (downcase field-name)))
         (buffer (generate-new-buffer buffer-name))
         (parent-buffer (current-buffer)))
    (switch-to-buffer buffer)
    (when current-value
      (insert current-value))
    ;; Use markdown-mode if available, otherwise text-mode
    (if (fboundp 'markdown-mode)
        (markdown-mode)
      (text-mode))
    ;; Enable visual-line-mode for better editing
    (visual-line-mode 1)
    (setq header-line-format
          (format "Edit %s: C-c C-c to finish, C-c C-k to cancel"
                  field-name))
    ;; Set up keybindings
    (let ((finish-func (lambda ()
                        (interactive)
                        (let ((text (buffer-substring-no-properties
                                   (point-min) (point-max))))
                          (kill-buffer)
                          (switch-to-buffer parent-buffer)
                          (funcall callback text)
                          (message "%s saved" field-name))))
          (cancel-func (lambda ()
                        (interactive)
                        (kill-buffer)
                        (switch-to-buffer parent-buffer)
                        (message "%s edit cancelled" field-name))))
      (local-set-key (kbd "C-c C-c") finish-func)
      (local-set-key (kbd "C-c C-k") cancel-func))
    (message "Edit %s. C-c C-c to finish, C-c C-k to cancel." field-name)))

(transient-define-suffix beads-create:--description ()
  "Edit multi-line description."
  :transient t
  :key "-d"
  :description "Description"
  (interactive)
  (beads-create--edit-text-multiline
   beads-create--description
   (lambda (text) (setq beads-create--description text))
   "Description"))

(transient-define-suffix beads-create:--acceptance ()
  "Edit multi-line acceptance criteria."
  :transient t
  :key "-A"
  :description "Acceptance"
  (interactive)
  (beads-create--edit-text-multiline
   beads-create--acceptance
   (lambda (text) (setq beads-create--acceptance text))
   "Acceptance Criteria"))

(transient-define-suffix beads-create:--design ()
  "Edit multi-line design notes."
  :transient t
  :key "-G"
  :description "Design"
  (interactive)
  (beads-create--edit-text-multiline
   beads-create--design
   (lambda (text) (setq beads-create--design text))
   "Design Notes"))

;;; Suffix Commands

(transient-define-suffix beads-create--execute ()
  "Execute bd create with all arguments."
  :key "x"
  :description "Create issue"
  (interactive)
  (let* ((args (transient-args 'beads-create))
         (title (transient-arg-value "--title=" args))
         (file (transient-arg-value "--file=" args))
         (cmd-args '()))

    ;; Validation
    (unless (or title file)
      (user-error "Title is required (or use --file)"))

    ;; Build command: title as positional, rest as flags
    (when title
      (push title cmd-args)
      ;; Remove --title= from args
      (setq args (seq-remove
                  (lambda (a) (string-prefix-p "--title=" a))
                  args)))

    ;; Add multi-line fields
    (when beads-create--description
      (setq args (append args (list "-d" beads-create--description))))
    (when beads-create--acceptance
      (setq args (append args (list "--acceptance"
                                     beads-create--acceptance))))
    (when beads-create--design
      (setq args (append args (list "--design" beads-create--design))))

    ;; Execute bd create
    (condition-case err
        (let* ((result (apply #'beads--run-command
                             "create"
                             (append cmd-args args)))
               (issue (beads--parse-issue result))
               (issue-id (alist-get 'id issue)))
          ;; Reset state
          (beads-create--reset-state)
          (transient-reset)  ; Reset transient args too
          (message "Created issue: %s - %s"
                   issue-id
                   (alist-get 'title issue))
          ;; Optionally show issue
          (when (y-or-n-p (format "Show issue %s? " issue-id))
            (beads-show issue-id)))
      (error
       (message "Failed to create issue: %s"
                (error-message-string err))))))

(transient-define-suffix beads-create--reset ()
  "Reset all arguments and state."
  :key "R"
  :description "Reset all"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (beads-create--reset-state)
    (transient-reset)  ; Reset transient-managed args
    (message "All fields reset")))

(transient-define-suffix beads-create--preview ()
  "Preview the bd create command."
  :key "p"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-create))
         (title (transient-arg-value "--title=" args))
         (cmd-args '()))
    (when title
      (push title cmd-args)
      (setq args (seq-remove
                  (lambda (a) (string-prefix-p "--title=" a))
                  args)))
    (when beads-create--description
      (setq args (append args (list "-d" "<description>"))))
    (when beads-create--acceptance
      (setq args (append args (list "--acceptance" "<acceptance>"))))
    (when beads-create--design
      (setq args (append args (list "--design" "<design>"))))
    (let ((cmd (apply #'beads--build-command
                     "create"
                     (append cmd-args args))))
      (message "Command: %s"
               (mapconcat #'shell-quote-argument cmd " ")))))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-create "beads-create" nil t)
(transient-define-prefix beads-create ()
  "Create a new issue in Beads.

This transient menu provides an interactive interface for setting
all parameters of the bd create command.  Required fields are
validated before execution."
  ;; Required
  ["Required"
   ("T" "Title" "--title="
    :prompt "Issue title: "
    :always-read t)]

  ;; Simple arguments (inline string definitions)
  ["Arguments"
   ("-t" "Type" "--type="
    :choices ("bug" "feature" "task" "epic" "chore"))
   ("-p" "Priority (0-4)" "--priority="
    :choices ("0" "1" "2" "3" "4"))
   ("-a" "Assignee" "--assignee="
    :prompt "Assignee: ")
   ("-l" "Labels" "--labels="
    :prompt "Labels (comma-separated): ")
   ("-e" "External ref" "--external-ref="
    :prompt "External reference (e.g., gh-123): ")]

  ;; Multi-line fields (need custom suffixes)
  ["Multi-line Fields"
   ("-d" "Description" beads-create:--description)
   ("-A" "Acceptance" beads-create:--acceptance)
   ("-G" "Design" beads-create:--design)]

  ;; Advanced options
  ["Advanced Options"
   :level 5
   ("-i" "Custom ID" "--id="
    :prompt "Custom ID (e.g., worker1-100): ")
   ("-D" "Dependencies" "--deps="
    :prompt "Dependencies (type:id, e.g., blocks:bd-1): ")
   ("-f" "From file" "--file="
    :reader transient-read-file
    :prompt "Markdown file: ")
   ("-F" "Force" "--force")]  ; Switch (no '=')

  ;; Actions
  ["Actions"
   ("x" "Create issue" beads-create--execute)
   ("p" "Preview command" beads-create--preview :transient t)
   ("R" "Reset all" beads-create--reset :transient t)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-create)
;;; beads-create.el ends here
