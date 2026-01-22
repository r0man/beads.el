;;; beads-command-label.el --- Label command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO classes for all `bd label' subcommands
;; and provides label management functionality for beads.el.
;;
;; Classes include full slot metadata for automatic transient menu
;; generation via `beads-meta-define-transient'.
;;
;; Commands included:
;; - beads-command-label-add: Add a label to one or more issues
;; - beads-command-label-remove: Remove a label from one or more issues
;; - beads-command-label-list: List labels for an issue
;; - beads-command-label-list-all: List all unique labels in the database
;;
;; Features:
;; - Label fetching and caching
;; - Label completion for reader functions
;; - Label add/remove/list commands (transient menus)
;; - Integration with beads-list and beads-show buffers
;; - beads-label-list-all-mode for tabulated display
;;
;; Usage:
;;   (beads-command-execute
;;    (beads-command-label-add :issue-ids '("bd-42") :label "bug"))
;;
;;   ;; Get label completion table
;;   (beads--label-completion-table)
;;
;;   ;; Invalidate cache after label changes
;;   (beads--invalidate-label-cache)

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'transient)

;; Forward declarations
(declare-function beads--invalidate-completion-cache "beads")
(declare-function beads--string-blank-p "beads")
(declare-function beads--build-command "beads")
(declare-function beads-buffer-name-list "beads-buffer")
(declare-function beads-buffer-name-utility "beads-buffer")
(declare-function beads-buffer-parse-show "beads-buffer")
(declare-function beads-completion-read-issue "beads-completion")
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-list--populate-buffer "beads-list")
(declare-function beads-list-mode "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-refresh-show "beads-command-show")
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

;;; Label Add Command

(eval-and-compile
  (beads-defcommand beads-command-label-add (beads-command-json)
    ((issue-ids
      :initarg :issue-ids
      :type (or null list)
      :initform nil
      :documentation "One or more issue IDs to add the label to (positional)."
      ;; CLI properties
      :positional 1
      :option-type :list
      :option-separator " "
      ;; Transient properties
      :key "i"
      :transient "Issue IDs"
      :class transient-option
      :argument "--id="
      :prompt "Issue ID(s): "
      :reader beads-reader-issue-id
      :transient-group "Add Label"
      :level 1
      :order 1
      :required t)
     (label
      :initarg :label
      :type (or null string)
      :initform nil
      :documentation "Label to add (positional, last argument)."
      ;; CLI properties
      :positional 2
      :option-type :string
      ;; Transient properties
      :key "l"
      :transient "Label"
      :class transient-option
      :argument "--label="
      :prompt "Label: "
      :transient-group "Add Label"
      :level 1
      :order 2
      :required t))
    :documentation "Represents bd label add command.
  Adds a label to one or more issues."))

(cl-defmethod beads-command-subcommand ((_command beads-command-label-add))
  "Return \"label add\" as the CLI subcommand name."
  "label add")

(cl-defmethod beads-command-validate ((command beads-command-label-add))
  "Validate label add COMMAND.
Checks that issue ID(s) and label are provided.
Returns error string or nil if valid."
  (with-slots (issue-ids label) command
    (cond
     ((or (null issue-ids) (zerop (length issue-ids)))
      "Must provide at least one issue ID")
     ((or (null label) (string-empty-p label))
      "Must provide a label")
     (t (beads-command--validate-string-list issue-ids "issue-ids")))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-label-add))
  "Execute CMD to add label and show result."
  (let ((result (beads-command-execute cmd)))
    (beads--invalidate-completion-cache)
    (message "Added label '%s' to %d issue%s"
             (oref cmd label)
             (length (oref cmd issue-ids))
             (if (= (length (oref cmd issue-ids)) 1) "" "s"))
    result))

;;; Label Remove Command

(eval-and-compile
  (beads-defcommand beads-command-label-remove (beads-command-json)
    ((issue-ids
      :initarg :issue-ids
      :type (or null list)
      :initform nil
      :documentation "One or more issue IDs to remove the label from (positional)."
      ;; CLI properties
      :positional 1
      :option-type :list
      :option-separator " "
      ;; Transient properties
      :key "i"
      :transient "Issue IDs"
      :class transient-option
      :argument "--id="
      :prompt "Issue ID(s): "
      :reader beads-reader-issue-id
      :transient-group "Remove Label"
      :level 1
      :order 1
      :required t)
     (label
      :initarg :label
      :type (or null string)
      :initform nil
      :documentation "Label to remove (positional, last argument)."
      ;; CLI properties
      :positional 2
      :option-type :string
      ;; Transient properties
      :key "l"
      :transient "Label"
      :class transient-option
      :argument "--label="
      :prompt "Label: "
      :transient-group "Remove Label"
      :level 1
      :order 2
      :required t))
    :documentation "Represents bd label remove command.
  Removes a label from one or more issues."))

(cl-defmethod beads-command-subcommand ((_command beads-command-label-remove))
  "Return \"label remove\" as the CLI subcommand name."
  "label remove")

(cl-defmethod beads-command-validate ((command beads-command-label-remove))
  "Validate label remove COMMAND.
Checks that issue ID(s) and label are provided.
Returns error string or nil if valid."
  (with-slots (issue-ids label) command
    (cond
     ((or (null issue-ids) (zerop (length issue-ids)))
      "Must provide at least one issue ID")
     ((or (null label) (string-empty-p label))
      "Must provide a label")
     (t (beads-command--validate-string-list issue-ids "issue-ids")))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-label-remove))
  "Execute CMD to remove label and show result."
  (let ((result (beads-command-execute cmd)))
    (beads--invalidate-completion-cache)
    (message "Removed label '%s' from %d issue%s"
             (oref cmd label)
             (length (oref cmd issue-ids))
             (if (= (length (oref cmd issue-ids)) 1) "" "s"))
    result))

;;; Label List Command

(eval-and-compile
  (beads-defcommand beads-command-label-list (beads-command-json)
    ((issue-id
      :initarg :issue-id
      :type (or null string)
      :initform nil
      :documentation "Issue ID to list labels for (positional)."
      ;; CLI properties
      :positional 1
      :option-type :string
      ;; Transient properties
      :key "i"
      :transient "Issue ID"
      :class transient-option
      :argument "--id="
      :prompt "Issue ID: "
      :reader beads-reader-issue-id
      :transient-group "List Labels"
      :level 1
      :order 1
      :required t))
    :documentation "Represents bd label list command.
  Lists labels for a specific issue."))

(cl-defmethod beads-command-subcommand ((_command beads-command-label-list))
  "Return \"label list\" as the CLI subcommand name."
  "label list")

(cl-defmethod beads-command-validate ((command beads-command-label-list))
  "Validate label list COMMAND.
Checks that issue ID is provided.
Returns error string or nil if valid."
  (with-slots (issue-id) command
    (if (or (null issue-id) (string-empty-p issue-id))
        "Must provide issue ID"
      nil)))

;;; Label List All Command

(eval-and-compile
  (beads-defcommand beads-command-label-list-all (beads-command-json)
    ()
    :documentation "Represents bd label list-all command.
  Lists all unique labels in the database."))

(cl-defmethod beads-command-subcommand ((_command beads-command-label-list-all))
  "Return \"label list-all\" as the CLI subcommand name."
  "label list-all")

(cl-defmethod beads-command-validate ((_command beads-command-label-list-all))
  "Validate label list-all COMMAND.
No required fields.
Returns nil (always valid)."
  nil)

;;; Auto-Generated Transient Menus

;;;###autoload (autoload 'beads-label-add-transient "beads-command-label" nil t)
(beads-meta-define-transient beads-command-label-add "beads-label-add-transient"
  "Add a label to one or more issues (auto-generated menu).

See `beads-label-add' for the full user-facing transient menu."
  beads-option-global-section)

;;;###autoload (autoload 'beads-label-remove-transient "beads-command-label" nil t)
(beads-meta-define-transient beads-command-label-remove
  "beads-label-remove-transient"
  "Remove a label from one or more issues (auto-generated menu).

See `beads-label-remove' for the full user-facing transient menu."
  beads-option-global-section)

;;;###autoload (autoload 'beads-label-list-transient "beads-command-label" nil t)
(beads-meta-define-transient beads-command-label-list
  "beads-label-list-transient"
  "List labels for a specific issue (auto-generated menu)."
  beads-option-global-section)

;;;###autoload (autoload 'beads-label-list-all-transient "beads-command-label" nil t)
(beads-meta-define-transient beads-command-label-list-all
  "beads-label-list-all-transient"
  "List all unique labels in the database (auto-generated menu)."
  beads-option-global-section)

;;; Label Fetching and Caching

(defun beads-label-list-all ()
  "Fetch all labels from bd label list-all.
Returns a list of label objects, each with \\='label and \\='count fields."
  (let* ((cmd (beads-command-label-list-all))
         ;; Execute command and get result from execution object
         (exec (beads-command-execute cmd))
         (json (oref exec result))
         ;; JSON is array of {\"label\": \"name\", \"count\": N}
         (labels (append json nil)))
    labels))

(defun beads--get-cached-labels ()
  "Get cached label list, refreshing if stale.
Returns list of label objects (with \\='label and \\='count fields) or nil."
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
   ;; From buffer name (*beads-show[PROJECT]/ISSUE-ID*)
   (when-let ((parsed (beads-buffer-parse-show (buffer-name))))
     (plist-get parsed :issue-id))))

;;; Label Add Workflow

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
  (when (beads--string-blank-p issue-ids)
    "Issue ID(s) required"))

(defun beads-label-add--validate-label (label)
  "Validate that LABEL is set.
Returns error message string if invalid, nil if valid."
  (when (beads--string-blank-p label)
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

;;; Label Add Suffix Commands

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
            (when (and (derived-mode-p 'beads-list-mode)
                       (bound-and-true-p beads-list--command))
              (beads-list-refresh t))
            (when (derived-mode-p 'beads-show-mode)
              (beads-refresh-show))
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

;;; Label Remove Workflow

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
  (when (beads--string-blank-p issue-ids)
    "Issue ID(s) required"))

(defun beads-label-remove--validate-label (label)
  "Validate that LABEL is set.
Returns error message string if invalid, nil if valid."
  (when (beads--string-blank-p label)
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

;;; Label Remove Suffix Commands

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
            (when (and (derived-mode-p 'beads-list-mode)
                       (bound-and-true-p beads-list--command))
              (beads-list-refresh t))
            (when (derived-mode-p 'beads-show-mode)
              (beads-refresh-show))
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

;;; Label List Interactive Command

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
             (beads-completion-read-issue "Issue ID: " nil t))))
  (let ((labels (beads-label-list issue-id)))
    (if labels
        (message "Labels for %s: %s" issue-id
                 (mapconcat #'identity labels ", "))
      (message "No labels for %s" issue-id))))

;;; Label List-All View Mode

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
    (require 'beads-command-list)
    (require 'beads-command)
    (let* ((cmd (beads-command-list :label (list label)))
           (issues (beads-command-execute cmd))
           (buf-name (beads-buffer-name-list nil (format "label=%s" label)))
           (buffer (get-buffer-create buf-name)))
      (with-current-buffer buffer
        (beads-list-mode)
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
                                    (if (= (length tabulated-list-entries) 1)
                                        "" "s")
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
  (tabulated-list-init-header)
  (hl-line-mode 1))

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
  (let* ((caller-dir default-directory)
         (buf-name (beads-buffer-name-utility "labels"))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (setq default-directory caller-dir)
      (beads-label-list-all-mode)
      (beads-label-list-all-refresh))
    (pop-to-buffer buffer)))

;;; User-Facing Transient Menus

;;;###autoload (autoload 'beads-label-add "beads-command-label" nil t)
(transient-define-prefix beads-label-add ()
  "Add a label to one or more issues.

This transient menu provides an interactive interface for adding
labels to issues using the bd label add command."
  ["Arguments"
   ("i" "Issue ID(s)" "--issue-ids="
    :reader beads-reader-label-issue-ids
    :prompt "Issue ID(s) (comma-separated): ")
   ("l" "Label" "--label="
    :reader beads-reader-label-name
    :prompt "Label name: ")]
  beads-option-global-section
  ["Actions"
   (beads-label-add--execute)
   (beads-label-add--preview)
   (beads-label-add--reset)])

;;;###autoload (autoload 'beads-label-remove "beads-command-label" nil t)
(transient-define-prefix beads-label-remove ()
  "Remove a label from one or more issues.

This transient menu provides an interactive interface for removing
labels from issues using the bd label remove command."
  ["Arguments"
   ("i" "Issue ID(s)" "--issue-ids="
    :reader beads-reader-label-issue-ids
    :prompt "Issue ID(s) (comma-separated): ")
   ("l" "Label" "--label="
    :reader beads-reader-label-name
    :prompt "Label name: ")]
  beads-option-global-section
  ["Actions"
   (beads-label-remove--execute)
   (beads-label-remove--preview)
   (beads-label-remove--reset)])

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-label "beads-command-label" nil t)
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

(provide 'beads-command-label)
;;; beads-command-label.el ends here
