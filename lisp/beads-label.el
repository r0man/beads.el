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
;; (beads-dep, beads-misc, etc.) with state variables, reader functions,
;; and transient-based command interfaces.
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
(declare-function beads--detect-issue-id "beads")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-show-refresh "beads-show")
(declare-function beads-option-read-issue-ids-for-label "beads-option")
(declare-function beads-option-read-label-name "beads-option")

;; Circular dependency note: beads-option requires beads-label for
;; beads--label-completion-table.  We avoid the cycle by only forward-declaring
;; the reader functions and loading beads-option dynamically.

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
Returns a list of label name strings."
  (let* ((cmd (beads-command-label-list-all))
         ;; Execute command, returns parsed JSON array
         (json (beads-command-execute cmd))
         ;; JSON is array of {\"label\": \"name\", \"count\": N}
         (labels (append json nil)))
    (mapcar (lambda (entry) (alist-get 'label entry)) labels)))

(defun beads--get-cached-labels ()
  "Get cached label list, refreshing if stale.
Returns list of label name strings or nil on error."
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
  (let ((labels (beads--get-cached-labels)))
    ;; Remove duplicates (shouldn't be any from bd, but be safe)
    (delete-dups labels)))

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
  :key "a"
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
  :key "r"
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

;;;###autoload (autoload 'beads-label-add "beads-label" nil t)
(transient-define-prefix beads-label-add ()
  "Add a label to one or more issues.

This transient menu provides an interactive interface for adding
labels to issues using the bd label add command."
  :init-value (lambda (_) (require 'beads-option))
  ["Arguments"
   ("i" "Issue ID(s)" "--issue-ids="
    :reader beads-option-read-issue-ids-for-label
    :prompt "Issue ID(s) (comma-separated): ")
   ("l" "Label" "--label="
    :reader beads-option-read-label-name
    :prompt "Label name: ")]
  beads-option-global-section
  ["Actions"
   ("a" "Add label" beads-label-add--execute)
   ("P" "Preview command" beads-label-add--preview)
   ("r" "Reset fields" beads-label-add--reset)])

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
  :key "r"
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

;;;###autoload (autoload 'beads-label-remove "beads-label" nil t)
(transient-define-prefix beads-label-remove ()
  "Remove a label from one or more issues.

This transient menu provides an interactive interface for removing
labels from issues using the bd label remove command."
  :init-value (lambda (_) (require 'beads-option))
  ["Arguments"
   ("i" "Issue ID(s)" "--issue-ids="
    :reader beads-option-read-issue-ids-for-label
    :prompt "Issue ID(s) (comma-separated): ")
   ("l" "Label" "--label="
    :reader beads-option-read-label-name
    :prompt "Label name: ")]
  beads-option-global-section
  ["Actions"
   ("r" "Remove label" beads-label-remove--execute)
   ("P" "Preview command" beads-label-remove--preview)
   ("R" "Reset fields" beads-label-remove--reset)])

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
   (list (or (beads--detect-issue-id)
             (completing-read "Issue ID: "
                            (beads--issue-completion-table)
                            nil t))))
  (let ((labels (beads-label-list issue-id)))
    (if labels
        (message "Labels for %s: %s" issue-id
                 (mapconcat #'identity labels ", "))
      (message "No labels for %s" issue-id))))

;;; Label List-All View Command

(defvar beads-label-list-all-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'beads-label-list-all-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `beads-label-list-all-mode'.")

(define-derived-mode beads-label-list-all-mode tabulated-list-mode
  "Beads-Labels"
  "Major mode for displaying all beads labels.

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
