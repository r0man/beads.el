;;; beads-command-dep.el --- Dependency command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO classes for all `bd dep' subcommands.
;; Classes include full slot metadata for automatic transient menu
;; generation via `beads-defcommand'.
;;
;; Commands included:
;; - beads-command-dep-add: Add a dependency between issues
;; - beads-command-dep-remove: Remove a dependency
;; - beads-command-dep-list: List dependencies or dependents
;; - beads-command-dep-tree: Show dependency tree
;; - beads-command-dep-cycles: Detect dependency cycles
;;
;; Usage:
;;   (beads-command-execute
;;    (beads-command-dep-add :issue-id "bd-42" :depends-on "bd-41"))

;;; Code:

(require 'beads-command)
(require 'beads-error)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'transient)

;; Forward declarations
(declare-function beads--invalidate-completion-cache "beads")
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-buffer-name-utility "beads-buffer")
(declare-function beads-buffer-parse-show "beads-buffer")
(declare-function beads-completion-read-issue "beads-completion")
(declare-function beads-check-executable "beads")
(declare-function beads-show "beads-show")
(defvar beads-show--current-issue-id)
(defvar beads-auto-refresh)
(defvar beads--issue-id-history)

;;; Dependency Add Command

(beads-defcommand beads-command-dep-add (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :short-option "i"
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads-reader-issue-id
    :group "Add Dependency"
    :level 1
    :order 1
    :required t)
   (depends-on
    :positional 2
    :type (or null string)
    :short-option "d"
    :argument "--depends-on="
    :prompt "Depends on: "
    :reader beads-reader-issue-id
    :group "Add Dependency"
    :level 1
    :order 2
    :required t)
   (blocked-by
    :type (or null string)
    :short-option "b"
    :prompt "Blocked by: "
    :reader beads-reader-issue-id
    :group "Add Dependency"
    :level 2
    :order 1)
   (dep-type
    :long-option "type"
    :short-option "t"
    :type (or null string)
    :prompt "Dependency type: "
    :group "Add Dependency"
    :level 1
    :order 3))
  :documentation "Represents bd dep add command.
Adds a dependency between two issues."
  :result beads-dep-op-result
  :transient :manual)


(cl-defmethod beads-command-validate ((command beads-command-dep-add))
  "Validate dep add COMMAND.
Checks that both issue IDs are provided.
Returns error string or nil if valid."
  (with-slots (issue-id depends-on blocked-by) command
    (cond
     ((or (null issue-id) (string-empty-p issue-id))
      "Must provide issue ID")
     ((and (or (null depends-on) (string-empty-p depends-on))
           (or (null blocked-by) (string-empty-p blocked-by)))
      "Must provide depends-on or blocked-by issue ID")
     (t nil))))

;; Parse override removed: the base method handles JSON-to-domain
;; parsing automatically via :result beads-dep-op-result.

;;; Dependency Remove Command

(beads-defcommand beads-command-dep-remove (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :short-option "i"
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads-reader-issue-id
    :group "Remove Dependency"
    :level 1
    :order 1
    :required t)
   (depends-on
    :positional 2
    :type (or null string)
    :short-option "d"
    :argument "--depends-on="
    :prompt "Remove dependency on: "
    :reader beads-reader-issue-id
    :group "Remove Dependency"
    :level 1
    :order 2
    :required t))
  :documentation "Represents bd dep remove command.
Removes a dependency between two issues."
  :result beads-dep-op-result
  :transient :manual)

;; Parse override removed: the base method handles JSON-to-domain
;; parsing automatically via :result beads-dep-op-result.

;;; Dependency List Command

(beads-defcommand beads-command-dep-list (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :short-option "i"
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads-reader-issue-id
    :group "List Dependencies"
    :level 1
    :order 1
    :required t)
   (direction
    :type (or null string)
    :short-option "D"
    :prompt "Direction (down/up): "
    :group "List Dependencies"
    :level 1
    :order 2)
   (dep-type
    :long-option "type"
    :short-option "t"
    :type (or null string)
    :prompt "Dependency type: "
    :group "List Dependencies"
    :level 1
    :order 3))
  :documentation "Represents bd dep list command.
Lists dependencies or dependents of an issue."
  :result (list-of beads-dependency)
  :transient :manual)

(cl-defmethod beads-command-parse ((command beads-command-dep-list) stdout)
  "Parse dep list COMMAND output from STDOUT.
Returns list of beads-dependency instances.
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, returns list of beads-dependency instances which
include full issue details plus the dependency_type field.
Does not modify any slots."
  (with-slots (json issue-id) command
    (if (not json)
        (cl-call-next-method)
      ;; Base method parses JSON and coerces via :result (list-of beads-dependency).
      ;; We just need to set issue-id on each dependency since bd dep list
      ;; doesn't include it (it's implicit from the command argument).
      (let ((deps (cl-call-next-method)))
        (dolist (dep deps)
          (oset dep issue-id issue-id))
        deps))))

;;; Dependency Tree Command

(beads-defcommand beads-command-dep-tree (beads-command-global-options)
  ((issue-id
    :positional 1
    :type (or null string)
    :short-option "i"
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads-reader-issue-id
    :group "Dependency Tree"
    :level 1
    :order 1
    :required t)
   (direction
    :type (or null string)
    :short-option "D"
    :prompt "Direction (down/up/both): "
    :group "Dependency Tree"
    :level 1
    :order 2)
   (max-depth
    :short-option "d"
    :type (or null string integer)
    :prompt "Max depth: "
    :group "Dependency Tree"
    :level 2
    :order 1)
   (status
    :type (or null string)
    :short-option "s"
    :prompt "Status filter: "
    :reader beads-reader-list-status
    :group "Dependency Tree"
    :level 1
    :order 3)
   (dep-type
    :long-option "type"
    :short-option "t"
    :type (or null string)
    :prompt "Dependency type: "
    :group "Dependency Tree"
    :level 2
    :order 2)
   (format
    :type (or null string)
    :short-option "F"
    :prompt "Format (mermaid): "
    :group "Dependency Tree"
    :level 2
    :order 3)
   (show-all-paths
    :type boolean
    :short-option "ap"
    :group "Dependency Tree"
    :level 3
    :order 1))
  :documentation "Represents bd dep tree command.
Shows dependency tree rooted at the given issue."
  :transient :manual)

(cl-defmethod beads-command-parse ((command beads-command-dep-tree) stdout)
  "Parse dep tree COMMAND output from STDOUT.
Return list of beads-tree-node objects."
  (with-slots (json) command
    (if (not json)
        (cl-call-next-method)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (if (eq (type-of parsed-json) 'vector)
                (mapcar (lambda (j) (beads-from-json 'beads-tree-node j))
                        (append parsed-json nil))
              (signal 'beads-json-parse-error
                      (list "Unexpected JSON structure from dep tree"
                            :stdout stdout
                            :parsed-json parsed-json)))
          (beads-json-parse-error (signal (car err) (cdr err)))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-tree-node: %s"
                                 (error-message-string err))
                         :stdout stdout
                         :parsed-json parsed-json))))))))


;;; Dependency Cycles Command

(beads-defcommand beads-command-dep-cycles (beads-command-global-options)
  ()
  :documentation "Represents bd dep cycles command.
Detects dependency cycles in the issue graph."
  :transient :manual)


;; Validate override removed: base handles slot-level validation.

;;; Dep Relate Command

;;;###autoload (autoload 'beads-dep-relate "beads-command-dep" nil t)
(beads-defcommand beads-command-dep-relate (beads-command-global-options)
  ((id1
    :positional 1
    :required t)
   (id2
    :positional 2
    :required t))
  :documentation "Represents bd dep relate command.
Creates a bidirectional relates_to link between two issues.")

(cl-defmethod beads-command-validate ((command beads-command-dep-relate))
  "Validate dep relate COMMAND."
  (with-slots (id1 id2) command
    (cond
     ((or (null id1) (string-empty-p id1))
      "First issue ID is required")
     ((or (null id2) (string-empty-p id2))
      "Second issue ID is required")
     ((string= id1 id2)
      "Cannot relate issue to itself")
     (t nil))))

;;; Dep Unrelate Command

;;;###autoload (autoload 'beads-dep-unrelate "beads-command-dep" nil t)
(beads-defcommand beads-command-dep-unrelate (beads-command-global-options)
  ((id1
    :positional 1
    :required t)
   (id2
    :positional 2
    :required t))
  :documentation "Represents bd dep unrelate command.
Removes a relates_to link between two issues.")

;;; Transient Menus

;;;###autoload (autoload 'beads-dep-add-transient "beads-command-dep" nil t)
(beads-meta-define-transient beads-command-dep-add "beads-dep-add-transient"
  "Add a dependency between two issues.

The first issue will depend on (be blocked by) the second issue.
Use --type to specify the dependency type (default: blocks).

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Issue IDs, dependency type
  Level 2: Alternative blocked-by flag"
  beads-option-global-section)

;;;###autoload (autoload 'beads-dep-remove-transient "beads-command-dep" nil t)
(beads-meta-define-transient beads-command-dep-remove "beads-dep-remove-transient"
  "Remove a dependency between two issues."
  beads-option-global-section)

;;;###autoload (autoload 'beads-dep-list-transient "beads-command-dep" nil t)
(beads-meta-define-transient beads-command-dep-list "beads-dep-list-transient"
  "List dependencies or dependents of an issue.

By default shows dependencies (what this issue depends on).
Use --direction=up to show dependents (what depends on this issue).
Use --type to filter by dependency type."
  beads-option-global-section)

;; Enable context-aware pre-population of the Issue ID field.
;; The default-value function is called by transient when initializing
;; the prefix, allowing beads-dep-list and buffer-context detection
;; to pre-populate --id= via beads-dep--pending-issue-id.
(when-let ((proto (get 'beads-dep-list-transient 'transient--prefix)))
  (oset proto default-value
        (lambda ()
          (when-let ((id (beads-dep--detect-issue-id)))
            (list (concat "--id=" id))))))

;;;###autoload (autoload 'beads-dep-tree-transient "beads-command-dep" nil t)
(beads-meta-define-transient beads-command-dep-tree "beads-dep-tree-transient"
  "Show dependency tree rooted at an issue.

By default shows dependencies (what blocks this issue).
Use --direction=up to show dependents (what this issue blocks).
Use --status to filter by issue status.

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Issue ID, direction, status
  Level 2: Max depth, type filter, format
  Level 3: Show all paths"
  beads-option-global-section)

;;;###autoload (autoload 'beads-dep-cycles-transient "beads-command-dep" nil t)
(beads-meta-define-transient beads-command-dep-cycles "beads-dep-cycles-transient"
  "Detect dependency cycles in the issue graph."
  beads-option-global-section)

;;; Interactive Dependency Management

(defvar beads-dep-tree--issue-id nil
  "Issue ID for tree display (buffer-local).")

(defvar beads-dep--pending-issue-id nil
  "Issue ID to pre-populate in dependency transient menus.
Set as a dynamic binding when invoking dependency transients
programmatically with an explicit issue ID.")

(defun beads-dep--detect-issue-id ()
  "Detect current issue ID from context.
Checks `beads-dep--pending-issue-id' first (for programmatic calls),
then falls back to detecting from beads-list or beads-show buffers."
  (or beads-dep--pending-issue-id
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
       (t nil))))

(defun beads-dep--format-dependency (dep)
  "Format dependency DEP (beads-dep-op-result object) for display."
  (let ((issue-id (oref dep issue-id))
        (depends-on-id (oref dep depends-on-id))
        (dep-type (oref dep dep-type))
        (op-status (oref dep op-status)))
    (format "%s: %s %s %s"
            (propertize (or op-status "unknown") 'face 'success)
            (propertize (or issue-id "") 'face 'font-lock-constant-face)
            (propertize (or dep-type "") 'face 'font-lock-keyword-face)
            (propertize (or depends-on-id "") 'face 'font-lock-constant-face))))

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
   ((or (null issue-id) (string-empty-p issue-id))
    "Issue ID is required")
   ((or (null depends-on-id) (string-empty-p depends-on-id))
    "Depends-on ID is required")
   ((string= issue-id depends-on-id)
    "Issue cannot depend on itself")
   ((or (null type) (string-empty-p type))
    "Dependency type is required")
   (t nil)))

(transient-define-suffix beads-dep-add--execute ()
  "Execute add dependency command."
  :key "x"
  :description "Add dependency"
  (interactive)
  (let* ((args (transient-args 'beads-dep-add--menu))
         (parsed (beads-dep-add--parse-transient-args args))
         (issue-id (plist-get parsed :issue-id))
         (depends-on-id (plist-get parsed :depends-on-id))
         (type (plist-get parsed :type))
         (error-msg (beads-dep-add--validate issue-id depends-on-id type)))
    (if error-msg
        (user-error "Cannot add dependency: %s" error-msg)
      (condition-case err
          (let* ((result (beads-execute 'beads-command-dep-add
                          :issue-id issue-id
                          :depends-on depends-on-id
                          :dep-type type))
                 (formatted (beads-dep--format-dependency result)))
            (message "Dependency added: %s" formatted)
            (beads--invalidate-completion-cache)
            nil)
        (beads-command-error
         (message "Failed to add dependency: %s"
                  (beads-error-extract-message err)))
        (error
         (message "Failed to add dependency: %s"
                  (error-message-string err)))))))

(transient-define-suffix beads-dep-add--preview ()
  "Preview add dependency command."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-dep-add--menu))
         (parsed (beads-dep-add--parse-transient-args args))
         (issue-id (plist-get parsed :issue-id))
         (depends-on-id (plist-get parsed :depends-on-id))
         (type (plist-get parsed :type))
         (error-msg (beads-dep-add--validate issue-id depends-on-id type)))
    (if error-msg
        (message "Invalid: %s" error-msg)
      (message "bd dep add %s %s --type %s"
               issue-id depends-on-id type))))

(transient-define-suffix beads-dep-add--reset ()
  "Reset add dependency fields."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "All fields reset")))

(transient-define-prefix beads-dep-add--menu ()
  "Transient menu for adding a dependency."
  :value (lambda ()
           (let ((detected-id (beads-dep--detect-issue-id)))
             (when detected-id
               (list (concat "--issue-id=" detected-id)))))
  ["Add Dependency"
   (beads-option-dep-add-issue-id)
   (beads-option-dep-add-depends-on-id)
   (beads-option-dep-add-type)]
  ["Actions"
   (beads-dep-add--execute)
   (beads-dep-add--preview)
   (beads-dep-add--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-dep-add (&optional issue-id)
  "Add a dependency to an issue.
If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user."
  (interactive (list (beads-dep--detect-issue-id)))
  (beads-check-executable)
  (ignore issue-id)
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
   ((or (null issue-id) (string-empty-p issue-id))
    "Issue ID is required")
   ((or (null depends-on-id) (string-empty-p depends-on-id))
    "Depends-on ID is required")
   (t nil)))

(transient-define-suffix beads-dep-remove--execute ()
  "Execute remove dependency command."
  :key "x"
  :description "Remove dependency"
  (interactive)
  (let* ((args (transient-args 'beads-dep-remove--menu))
         (parsed (beads-dep-remove--parse-transient-args args))
         (issue-id (plist-get parsed :issue-id))
         (depends-on-id (plist-get parsed :depends-on-id))
         (error-msg (beads-dep-remove--validate issue-id depends-on-id)))
    (if error-msg
        (user-error "Cannot remove dependency: %s" error-msg)
      (condition-case err
          (let* ((result (beads-execute 'beads-command-dep-remove
                          :issue-id issue-id
                          :depends-on depends-on-id))
                 (op-status (oref result op-status)))
            (message "Dependency %s: %s -> %s"
                     (propertize op-status 'face 'success)
                     issue-id depends-on-id)
            (beads--invalidate-completion-cache)
            nil)
        (beads-command-error
         (message "Failed to remove dependency: %s"
                  (beads-error-extract-message err)))
        (error
         (message "Failed to remove dependency: %s"
                  (error-message-string err)))))))

(transient-define-suffix beads-dep-remove--preview ()
  "Preview remove dependency command."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-dep-remove--menu))
         (parsed (beads-dep-remove--parse-transient-args args))
         (issue-id (plist-get parsed :issue-id))
         (depends-on-id (plist-get parsed :depends-on-id))
         (error-msg (beads-dep-remove--validate issue-id depends-on-id)))
    (if error-msg
        (message "Invalid: %s" error-msg)
      (message "bd dep remove %s %s" issue-id depends-on-id))))

(transient-define-suffix beads-dep-remove--reset ()
  "Reset remove dependency fields."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "All fields reset")))

(transient-define-prefix beads-dep-remove--menu ()
  "Transient menu for removing a dependency."
  :value (lambda ()
           (let ((detected-id (beads-dep--detect-issue-id)))
             (when detected-id
               (list (concat "--issue-id=" detected-id)))))
  ["Remove Dependency"
   (beads-option-dep-remove-issue-id)
   (beads-option-dep-remove-depends-on-id)]
  ["Actions"
   (beads-dep-remove--execute)
   (beads-dep-remove--preview)
   (beads-dep-remove--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-dep-remove (&optional issue-id)
  "Remove a dependency from an issue.
If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user."
  (interactive (list (beads-dep--detect-issue-id)))
  (beads-check-executable)
  (ignore issue-id)
  (beads-dep-remove--menu))

;;; Dependency Tree View

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
  (let ((id (oref issue id))
        (title (oref issue title))
        (status (oref issue status))
        (depth (or (oref issue depth) 0))
        (truncated (oref issue truncated)))
    ;; Indentation
    (insert (make-string (* depth 2) ?\s))
    ;; Tree connector
    (when (> depth 0)
      (insert "- "))
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
    (insert (propertize (make-string 60 ?=) 'face 'shadow) "\n\n")
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
    (let* ((issues (beads-execute 'beads-command-dep-tree
                    :issue-id beads-dep-tree--issue-id)))
      (beads-dep-tree--render issues beads-dep-tree--issue-id)
      (message "Dependency tree refreshed"))))

;;;###autoload
(defun beads-dep-tree (&optional issue-id)
  "Display dependency tree for ISSUE-ID.
If ISSUE-ID is not provided, prompt for it or detect from context.
Completion matches on both issue ID and title."
  (interactive
   (list (or (beads-dep--detect-issue-id)
             (beads-completion-read-issue "Issue ID: " nil nil nil
                                          'beads--issue-id-history))))
  (beads-check-executable)
  (when (or (null issue-id) (string-empty-p issue-id))
    (user-error "Issue ID is required"))
  (let* ((caller-dir default-directory)
         (issues (beads-execute 'beads-command-dep-tree :issue-id issue-id))
         (buf-name (beads-buffer-name-utility "dep-tree" issue-id))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (setq default-directory caller-dir)
      (beads-dep-tree-mode)
      (setq-local beads-dep-tree--issue-id issue-id)
      (beads-dep-tree--render issues issue-id))
    (pop-to-buffer buffer)))

;;; Dependency List View

;;;###autoload
(defun beads-dep-list (&optional issue-id)
  "List dependencies of ISSUE-ID using a transient menu.
If ISSUE-ID is not provided, prompt for it or detect from context.
When ISSUE-ID is provided, the transient menu pre-populates the
Issue ID field with that value."
  (interactive
   (list (or (beads-dep--detect-issue-id)
             (beads-completion-read-issue "Issue ID: " nil nil nil
                                          'beads--issue-id-history))))
  (beads-check-executable)
  (when (or (null issue-id) (string-empty-p issue-id))
    (user-error "Issue ID is required"))
  (let ((beads-dep--pending-issue-id issue-id))
    (beads-dep-list-transient)))

;;; Dependency Cycles View

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
    (insert (propertize (make-string 60 ?=) 'face 'shadow) "\n\n")
    ;; Cycles
    (if (zerop (length cycles))
        (insert (propertize "No dependency cycles detected"
                          'face 'success)
                "\n")
      (insert (propertize (format "Found %d cycle(s):"
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
                            " -> "))
          (insert " -> " (propertize (aref cycle 0)
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
    (let ((cycles (beads-execute 'beads-command-dep-cycles)))
      (beads-dep-cycles--render cycles)
      (message "Dependency cycles check complete"))))

;;;###autoload
(defun beads-dep-cycles ()
  "Check for dependency cycles and display results."
  (interactive)
  (beads-check-executable)
  (let* ((caller-dir default-directory)
         (cycles (beads-execute 'beads-command-dep-cycles))
         (buf-name (beads-buffer-name-utility "dep-cycles"))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (setq default-directory caller-dir)
      (beads-dep-cycles-mode)
      (beads-dep-cycles--render cycles))
    (pop-to-buffer buffer)))

;;; Main Menu

;;;###autoload (autoload 'beads-dep "beads-command-dep" nil t)
(transient-define-prefix beads-dep ()
  "Manage dependencies in Beads.

This menu provides access to all dependency management operations:
- Add/remove dependencies with type selection
- Create/remove bidirectional relates_to links
- List dependencies for an issue
- View dependency trees
- Detect dependency cycles"
  ["Dependency Operations"
   ("a" "Add dependency" beads-dep-add)
   ("r" "Remove dependency" beads-dep-remove)
   ("l" "List dependencies" beads-dep-list)
   ("t" "Show dependency tree" beads-dep-tree)
   ("c" "Check for cycles" beads-dep-cycles)
   ("R" "Relate issues" beads-dep-relate)
   ("U" "Unrelate issues" beads-dep-unrelate)]
  ["Other"
   ("q" "Quit" transient-quit-one)])

(provide 'beads-command-dep)
;;; beads-command-dep.el ends here
