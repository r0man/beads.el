;;; beads-label.el --- Label management for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides label management functionality for beads.el,
;; including:
;; - Label fetching and caching
;; - Label completion for reader functions
;; - Label add/remove/list commands (transient menus)
;; - Integration with beads-list and beads-show buffers
;;
;; The module follows the pattern established in other beads modules
;; (beads-dep, beads-export, beads-import, etc.) with state variables,
;; reader functions, and transient-based command interfaces.
;;
;; Usage:
;;
;;   ;; Get label completion table
;;   (beads--label-completion-table)
;;
;;   ;; Invalidate cache after label changes
;;   (beads--invalidate-label-cache)
;;
;;   ;; Fetch all labels
;;   (beads-label-list-all)

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'transient)

;; Forward declarations
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-list--populate-buffer "beads-list")
(declare-function beads-list-mode "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-show-refresh "beads-show")
(declare-function beads-reader-label-issue-ids "beads-reader")
(declare-function beads-reader-label-name "beads-reader")
(defvar beads-show--issue-id)
(defvar beads-list--marked-issues)

;;; Customization

(defgroup beads-label nil
  "Label management for Beads issue tracker."
  :group 'beads
  :prefix "beads-label-")

(defcustom beads-label-cache-ttl 300
  "Time-to-live for label cache in seconds.
Default is 300 seconds (5 minutes) since labels change less
frequently than issues."
  :type 'integer
  :group 'beads-label)

;;; Variables

(defvar beads--label-cache nil
  "Cache for label list used in completion.
Format: (TIMESTAMP . LABELS-LIST)")

;;; Label Fetching and Caching

(defun beads-label-list-all ()
  "Fetch all labels from bd label list-all.
Returns a list of label objects, each with \\='label and \\='count fields."
  (let* ((cmd (beads-command-label-list-all))
         ;; Execute command, returns parsed JSON array
         (json (beads-command-execute cmd))
         ;; JSON is array of {\"label\": \"name\", \"count\": N}
         (labels (append json nil)))
    labels))

(defun beads--get-cached-labels ()
  "Get cached label list, refreshing if stale.
Returns list of label objects (with \\='label and \\='count fields) or nil on error."
  (let ((now (float-time)))
    (when (or (null beads--label-cache)
              (> (- now (car beads--label-cache))
                 beads-label-cache-ttl))
      (condition-case nil
          (setq beads--label-cache
                (cons now (beads-label-list-all)))
        (error
         (setq beads--label-cache nil))))
    (cdr beads--label-cache)))

(defun beads--invalidate-label-cache ()
  "Invalidate the label cache.
Call this after adding or removing labels."
  (setq beads--label-cache nil))

(defun beads--label-completion-table ()
  "Return completion table of unique label names.
Returns a simple list of label strings for use with `completing-read'."
  (let ((label-objects (beads--get-cached-labels)))
    ;; Extract label names from objects and remove duplicates
    (delete-dups
     (mapcar (lambda (entry) (alist-get 'label entry))
             label-objects))))

;;; Context Detection

(defun beads-label--detect-issue-id ()
  "Detect issue ID from current context.
Returns issue ID string or nil if not found."
  (or
   ;; From beads-list buffer
   (when (derived-mode-p 'beads-list-mode)
     (beads-list--current-issue-id))
   ;; From beads-show buffer
   (when (derived-mode-p 'beads-show-mode)
     beads-show--issue-id)
   ;; From buffer name (*beads-show: bd-N*)
   (when (string-match "\\*beads-show: \\(bd-[0-9]+\\)\\*"
                      (buffer-name))
     (match-string 1 (buffer-name)))))

;;; Label Add Command

(defun beads-label-add--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:issue-ids STRING :label STRING)."
  (let* ((issue-ids (transient-arg-value "--issue-ids=" args))
         (label (transient-arg-value "--label=" args)))
    (list :issue-ids issue-ids
          :label label)))

(defun beads-label-add--validate-issue-ids (issue-ids)
  "Validate that ISSUE-IDS is set.
Returns error message string if invalid, nil if valid."
  (when (or (null issue-ids)
            (string-empty-p (string-trim issue-ids)))
    "Issue ID(s) required"))

(defun beads-label-add--validate-label (label)
  "Validate that LABEL is set.
Returns error message string if invalid, nil if valid."
  (when (or (null label)
            (string-empty-p (string-trim label)))
    "Label is required"))

(defun beads-label-add--validate-all (parsed)
  "Validate all parameters from PARSED plist.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-label-add--validate-issue-ids
               (plist-get parsed :issue-ids))
              (beads-label-add--validate-label
               (plist-get parsed :label)))))

(defun beads-label-add--build-command-args (parsed)
  "Build command arguments from PARSED plist.
Returns list of arguments for bd label add command."
  (let* ((issue-ids (plist-get parsed :issue-ids))
         (label (plist-get parsed :label))
         (ids-list (split-string (string-trim issue-ids) "[, ]+" t))
         args)
    ;; Build args: [issue-id...] [label]
    (setq args (append ids-list (list label)))
    args))

;;; Suffix Commands

(transient-define-suffix beads-label-add--execute ()
  "Execute the bd label add command with current parameters."
  :key "x"
  :description "Add label"
  (interactive)
  (let* ((args (transient-args 'beads-label-add))
         (parsed (beads-label-add--parse-transient-args args))
         (errors (beads-label-add--validate-all parsed)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (let* ((issue-ids-str (plist-get parsed :issue-ids))
                 (label (plist-get parsed :label))
                 (ids-list (split-string (string-trim issue-ids-str)
                                        "[, ]+" t))
                 (count (length ids-list))
                 (cmd (beads-command-label-add
                       :issue-ids ids-list
                       :label label)))
            (beads-command-execute cmd)
            (message "Added label '%s' to %d issue%s"
                     label count (if (= count 1) "" "s"))
            ;; Invalidate label cache
            (beads--invalidate-label-cache)
            ;; Invalidate issue cache (labels may have changed)
            (beads--invalidate-completion-cache)
            ;; Refresh current buffer if in list/show mode
            (when (derived-mode-p 'beads-list-mode)
              (beads-list-refresh))
            (when (derived-mode-p 'beads-show-mode)
              (beads-show-refresh))
            nil)
        (error
         (let ((err-msg (format "Failed to add label: %s"
                                (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-label-add--reset ()
  "Reset all parameters to their default values."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "All fields reset")))

(transient-define-suffix beads-label-add--preview ()
  "Preview the bd label add command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-label-add))
         (parsed (beads-label-add--parse-transient-args args))
         (errors (beads-label-add--validate-all parsed)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                               (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (let* ((cmd-args (beads-label-add--build-command-args parsed))
             (cmd (apply #'beads--build-command "label" "add" cmd-args))
             (cmd-string (mapconcat #'shell-quote-argument cmd " "))
             (preview-msg (format "Command: %s" cmd-string)))
        (message "%s" preview-msg)
        preview-msg))))

;;; Main Transient Menu

;; Transient menu definition moved to beads-option.el
;; to avoid circular dependency (beads-option requires beads-label)
;; See beads-label-add in beads-option.el

;;; Label Remove Command

(defun beads-label-remove--parse-transient-args (args)
  "Parse transient ARGS list into a plist.
Returns (:issue-ids STRING :label STRING)."
  (let* ((issue-ids (transient-arg-value "--issue-ids=" args))
         (label (transient-arg-value "--label=" args)))
    (list :issue-ids issue-ids
          :label label)))

(defun beads-label-remove--validate-issue-ids (issue-ids)
  "Validate that ISSUE-IDS is set.
Returns error message string if invalid, nil if valid."
  (when (or (null issue-ids)
            (string-empty-p (string-trim issue-ids)))
    "Issue ID(s) required"))

(defun beads-label-remove--validate-label (label)
  "Validate that LABEL is set.
Returns error message string if invalid, nil if valid."
  (when (or (null label)
            (string-empty-p (string-trim label)))
    "Label is required"))

(defun beads-label-remove--validate-all (parsed)
  "Validate all parameters from PARSED plist.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-label-remove--validate-issue-ids
               (plist-get parsed :issue-ids))
              (beads-label-remove--validate-label
               (plist-get parsed :label)))))

(defun beads-label-remove--build-command-args (parsed)
  "Build command arguments from PARSED plist.
Returns list of arguments for bd label remove command."
  (let* ((issue-ids (plist-get parsed :issue-ids))
         (label (plist-get parsed :label))
         (ids-list (split-string (string-trim issue-ids) "[, ]+" t))
         args)
    ;; Build args: [issue-id...] [label]
    (setq args (append ids-list (list label)))
    args))

;;; Suffix Commands

(transient-define-suffix beads-label-remove--execute ()
  "Execute the bd label remove command with current parameters."
  :key "x"
  :description "Remove label"
  (interactive)
  (let* ((args (transient-args 'beads-label-remove))
         (parsed (beads-label-remove--parse-transient-args args))
         (errors (beads-label-remove--validate-all parsed)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (let* ((issue-ids-str (plist-get parsed :issue-ids))
                 (label (plist-get parsed :label))
                 (ids-list (split-string (string-trim issue-ids-str)
                                        "[, ]+" t))
                 (count (length ids-list))
                 (cmd (beads-command-label-remove
                       :issue-ids ids-list
                       :label label)))
            (beads-command-execute cmd)
            (message "Removed label '%s' from %d issue%s"
                     label count (if (= count 1) "" "s"))
            ;; Invalidate label cache
            (beads--invalidate-label-cache)
            ;; Invalidate issue cache (labels may have changed)
            (beads--invalidate-completion-cache)
            ;; Refresh current buffer if in list/show mode
            (when (derived-mode-p 'beads-list-mode)
              (beads-list-refresh))
            (when (derived-mode-p 'beads-show-mode)
              (beads-show-refresh))
            nil)
        (error
         (let ((err-msg (format "Failed to remove label: %s"
                                (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-label-remove--reset ()
  "Reset all parameters to their default values."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "All fields reset")))

(transient-define-suffix beads-label-remove--preview ()
  "Preview the bd label remove command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-label-remove))
         (parsed (beads-label-remove--parse-transient-args args))
         (errors (beads-label-remove--validate-all parsed)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                               (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (let* ((cmd-args (beads-label-remove--build-command-args parsed))
             (cmd (apply #'beads--build-command "label" "remove"
                         cmd-args))
             (cmd-string (mapconcat #'shell-quote-argument cmd " "))
             (preview-msg (format "Command: %s" cmd-string)))
        (message "%s" preview-msg)
        preview-msg))))

;;; Main Transient Menu

;; Transient menu definition moved to beads-option.el
;; to avoid circular dependency (beads-option requires beads-label)
;; See beads-label-remove in beads-option.el

;;; Label List Command

(defun beads-label-list (issue-id)
  "List all labels for ISSUE-ID.
Returns list of label strings."
  (let ((cmd (beads-command-label-list :issue-id issue-id)))
    (beads-command-execute cmd)))

;;;###autoload
(defun beads-label-list-interactive (issue-id)
  "Interactively list labels for ISSUE-ID.
If called interactively, prompts for issue ID.
If called from beads-list or beads-show buffers, uses current issue."
  (interactive
   (list (or (beads-label--detect-issue-id)
             (completing-read "Issue ID: "
                            (beads--issue-completion-table)
                            nil t))))
  (let ((labels (beads-label-list issue-id)))
    (if labels
        (message "Labels for %s: %s" issue-id
                 (mapconcat #'identity labels ", "))
      (message "No labels for %s" issue-id))))

;;; Label List-All View Command

(defun beads-label-list-all--current-label ()
  "Return the label name at point, or nil."
  ;; Use symbol-function + funcall to prevent byte-compiler inlining.
  ;; This ensures cl-letf can properly mock tabulated-list-get-id in tests.
  (funcall (symbol-function 'tabulated-list-get-id)))

(defun beads-label-list-all-show-issues ()
  "Show all issues with the label at point in a beads-list buffer."
  (interactive)
  ;; Save label before leaving the buffer
  (let ((label (beads-label-list-all--current-label)))
    (unless label
      (user-error "No label at point"))
    (require 'beads-list)
    (require 'beads-command)
    (let* ((cmd (beads-command-list :label (list label)))
           (issues (beads-command-execute cmd))
           (buffer (get-buffer-create (format "*beads-list: label=%s*" label)))
           (project-dir default-directory))
      (with-current-buffer buffer
        (beads-list-mode)
        (setq default-directory project-dir)
        (if (not issues)
            (progn
              (setq tabulated-list-entries nil)
              (tabulated-list-print t)
              (setq mode-line-format
                    (list "%e" 'mode-line-front-space
                          'mode-line-buffer-identification
                          (format "  No issues with label '%s'" label)))
              (message "No issues found with label '%s'" label))
          (beads-list--populate-buffer issues 'list cmd)
          (setq mode-line-format
                (list "%e" 'mode-line-front-space
                      'mode-line-buffer-identification
                      '(:eval (format "  %d issue%s with label '%s'%s"
                                    (length tabulated-list-entries)
                                    (if (= (length tabulated-list-entries) 1) "" "s")
                                    label
                                    (if beads-list--marked-issues
                                        (format " [%d marked]"
                                               (length beads-list--marked-issues))
                                      "")))))))
      (pop-to-buffer buffer))))

(defvar beads-label-list-all-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'beads-label-list-all-show-issues)
    (define-key map (kbd "g") #'beads-label-list-all-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `beads-label-list-all-mode'.")

(define-derived-mode beads-label-list-all-mode tabulated-list-mode
  "Beads-Labels"
  "Major mode for displaying all beads labels.

Key bindings:
\\<beads-label-list-all-mode-map>
\\[beads-label-list-all-show-issues] - Show issues with the label at point
\\[beads-label-list-all-refresh] - Refresh the label list
\\[quit-window] - Quit the label list window

\\{beads-label-list-all-mode-map}"
  (setq tabulated-list-format
        (vector (list "Label" 40 t)
                (list "Count" 10 t :right-align t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Label" nil))
  (tabulated-list-init-header))

(defun beads-label-list-all-refresh ()
  "Refresh the label list-all view."
  (interactive)
  (let ((labels (beads-label-list-all)))
    (setq tabulated-list-entries
          (mapcar (lambda (entry)
                    (let ((label (alist-get 'label entry))
                          (count (alist-get 'count entry)))
                      (list label
                            (vector label
                                    (number-to-string count)))))
                  labels))
    (tabulated-list-print t)))

;;;###autoload
(defun beads-label-list-all-view ()
  "Display all labels in a tabulated list buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*beads-labels*")))
    (with-current-buffer buffer
      (beads-label-list-all-mode)
      (beads-label-list-all-refresh))
    (pop-to-buffer buffer)))

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-label "beads-label" nil t)
(transient-define-prefix beads-label ()
  "Manage labels for issues.

This transient menu provides access to all label management commands:
- Add labels to issues
- Remove labels from issues
- List labels for an issue
- List all labels in the database"
  ["Label Commands"
   ("a" "Add label to issue(s)" beads-label-add)
   ("r" "Remove label from issue(s)" beads-label-remove)
   ("l" "List labels for issue" beads-label-list-interactive)
   ("L" "List all labels" beads-label-list-all-view)])

(provide 'beads-label)
;;; beads-label.el ends here
