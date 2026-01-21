;;; beads-command-dep.el --- Dependency command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO classes for all `bd dep' subcommands.
;; Classes include full slot metadata for automatic transient menu
;; generation via `beads-meta-define-transient'.
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

(eval-and-compile
(beads-defcommand beads-command-dep-add (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue that will depend on another (positional arg 1)."
    ;; CLI properties
    :positional 1
    :option-type :string
    ;; Transient properties
    :key "i"
    :transient "Issue ID"
    :class transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Add Dependency"
    :level 1
    :order 1
    :required t)
   (depends-on
    :initarg :depends-on
    :type (or null string)
    :initform nil
    :documentation "Issue that blocks the first issue (positional arg 2).
Can also use --blocked-by or --depends-on flags.
Can be a local issue ID or external:project:capability."
    ;; CLI properties
    :positional 2
    :option-type :string
    ;; Transient properties
    :key "d"
    :transient "Depends on"
    :class transient-option
    :argument "--depends-on="
    :prompt "Depends on: "
    :transient-reader beads-reader-issue-id
    :transient-group "Add Dependency"
    :level 1
    :order 2
    :required t)
   (blocked-by
    :initarg :blocked-by
    :type (or null string)
    :initform nil
    :documentation "Issue ID that blocks the first issue (--blocked-by).
Alternative to positional arg.  Alias for --depends-on."
    ;; CLI properties
    :long-option "blocked-by"
    :option-type :string
    ;; Transient properties
    :key "b"
    :transient "--blocked-by"
    :class transient-option
    :argument "--blocked-by="
    :prompt "Blocked by: "
    :transient-reader beads-reader-issue-id
    :transient-group "Add Dependency"
    :level 2
    :order 1)
   (dep-type
    :initarg :dep-type
    :type (or null string)
    :initform nil
    :documentation "Dependency type (-t, --type).
Values: blocks, tracks, related, parent-child, discovered-from,
until, caused-by, validates, relates-to, supersedes.
Default: blocks."
    ;; CLI properties
    :long-option "type"
    :short-option "t"
    :option-type :string
    ;; Transient properties
    :key "t"
    :transient "--type"
    :class transient-option
    :argument "--type="
    :prompt "Dependency type: "
    :transient-group "Add Dependency"
    :level 1
    :order 3))
  :documentation "Represents bd dep add command.
Adds a dependency between two issues."))

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-add))
  "Return \"dep add\" as the CLI subcommand name."
  "dep add")

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

;;; Dependency Remove Command

(eval-and-compile
(beads-defcommand beads-command-dep-remove (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue with the dependency (positional arg 1)."
    ;; CLI properties
    :positional 1
    :option-type :string
    ;; Transient properties
    :key "i"
    :transient "Issue ID"
    :class transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Remove Dependency"
    :level 1
    :order 1
    :required t)
   (depends-on
    :initarg :depends-on
    :type (or null string)
    :initform nil
    :documentation "Issue to remove from dependencies (positional arg 2)."
    ;; CLI properties
    :positional 2
    :option-type :string
    ;; Transient properties
    :key "d"
    :transient "Depends on"
    :class transient-option
    :argument "--depends-on="
    :prompt "Remove dependency on: "
    :transient-reader beads-reader-issue-id
    :transient-group "Remove Dependency"
    :level 1
    :order 2
    :required t))
  :documentation "Represents bd dep remove command.
Removes a dependency between two issues."))

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-remove))
  "Return \"dep remove\" as the CLI subcommand name."
  "dep remove")

(cl-defmethod beads-command-validate ((command beads-command-dep-remove))
  "Validate dep remove COMMAND.
Checks that both issue IDs are provided.
Returns error string or nil if valid."
  (with-slots (issue-id depends-on) command
    (cond
     ((or (null issue-id) (string-empty-p issue-id))
      "Must provide issue ID")
     ((or (null depends-on) (string-empty-p depends-on))
      "Must provide depends-on issue ID")
     (t nil))))

;;; Dependency List Command

(eval-and-compile
(beads-defcommand beads-command-dep-list (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue to list dependencies for (positional arg)."
    ;; CLI properties
    :positional 1
    :option-type :string
    ;; Transient properties
    :key "i"
    :transient "Issue ID"
    :class transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "List Dependencies"
    :level 1
    :order 1
    :required t)
   (direction
    :initarg :direction
    :type (or null string)
    :initform nil
    :documentation "Direction to list (--direction).
Values: down (dependencies), up (dependents).
Default: down."
    ;; CLI properties
    :long-option "direction"
    :option-type :string
    ;; Transient properties
    :key "D"
    :transient "--direction"
    :class transient-option
    :argument "--direction="
    :prompt "Direction (down/up): "
    :transient-group "List Dependencies"
    :level 1
    :order 2)
   (dep-type
    :initarg :dep-type
    :type (or null string)
    :initform nil
    :documentation "Filter by dependency type (-t, --type).
Values: tracks, blocks, parent-child, etc."
    ;; CLI properties
    :long-option "type"
    :short-option "t"
    :option-type :string
    ;; Transient properties
    :key "t"
    :transient "--type"
    :class transient-option
    :argument "--type="
    :prompt "Dependency type: "
    :transient-group "List Dependencies"
    :level 1
    :order 3))
  :documentation "Represents bd dep list command.
Lists dependencies or dependents of an issue."))

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-list))
  "Return \"dep list\" as the CLI subcommand name."
  "dep list")

(cl-defmethod beads-command-validate ((command beads-command-dep-list))
  "Validate dep list COMMAND.
Checks that issue ID is provided.
Returns error string or nil if valid."
  (with-slots (issue-id) command
    (if (or (null issue-id) (string-empty-p issue-id))
        "Must provide issue ID"
      nil)))

(cl-defmethod beads-command-parse ((command beads-command-dep-list) execution)
  "Parse dep list COMMAND output from EXECUTION.
Returns list of beads-issue instances.
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, returns list of beads-issue instances.
Does not modify any slots."
  (with-slots (json) command
    (if (not json)
        (cl-call-next-method)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (cond
             ((eq (type-of parsed-json) 'vector)
              (mapcar #'beads-issue-from-json (append parsed-json nil)))
             ((or (null parsed-json) (eq parsed-json :null))
              nil)
             (t
              (signal 'beads-json-parse-error
                      (list "Unexpected JSON structure from bd dep list"
                            :exit-code (oref execution exit-code)
                            :parsed-json parsed-json
                            :stderr (oref execution stderr)))))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to parse dep list result: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :parsed-json parsed-json
                         :stderr (oref execution stderr)
                         :parse-error err))))))))

;;; Dependency Tree Command

(eval-and-compile
(beads-defcommand beads-command-dep-tree (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Root issue for the tree (positional arg)."
    ;; CLI properties
    :positional 1
    :option-type :string
    ;; Transient properties
    :key "i"
    :transient "Issue ID"
    :class transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Dependency Tree"
    :level 1
    :order 1
    :required t)
   (direction
    :initarg :direction
    :type (or null string)
    :initform nil
    :documentation "Tree direction (--direction).
Values: down (dependencies), up (dependents), both.
Default: down."
    ;; CLI properties
    :long-option "direction"
    :option-type :string
    ;; Transient properties
    :key "D"
    :transient "--direction"
    :class transient-option
    :argument "--direction="
    :prompt "Direction (down/up/both): "
    :transient-group "Dependency Tree"
    :level 1
    :order 2)
   (max-depth
    :initarg :max-depth
    :type (or null integer)
    :initform nil
    :documentation "Maximum tree depth (-d, --max-depth).
Safety limit, default: 50."
    ;; CLI properties
    :long-option "max-depth"
    :short-option "d"
    :option-type :integer
    ;; Transient properties
    :key "d"
    :transient "--max-depth"
    :class transient-option
    :argument "--max-depth="
    :prompt "Max depth: "
    :transient-group "Dependency Tree"
    :level 2
    :order 1)
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter to only show issues with this status (--status).
Values: open, in_progress, blocked, deferred, closed."
    ;; CLI properties
    :long-option "status"
    :option-type :string
    ;; Transient properties
    :key "s"
    :transient "--status"
    :class transient-option
    :argument "--status="
    :prompt "Status filter: "
    :transient-reader beads-reader-list-status
    :transient-group "Dependency Tree"
    :level 1
    :order 3)
   (dep-type
    :initarg :dep-type
    :type (or null string)
    :initform nil
    :documentation "Filter to only show dependencies of this type (-t, --type).
Values: tracks, blocks, parent-child, etc."
    ;; CLI properties
    :long-option "type"
    :short-option "t"
    :option-type :string
    ;; Transient properties
    :key "t"
    :transient "--type"
    :class transient-option
    :argument "--type="
    :prompt "Dependency type: "
    :transient-group "Dependency Tree"
    :level 2
    :order 2)
   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Output format (--format).
Values: 'mermaid' for Mermaid.js flowchart."
    ;; CLI properties
    :long-option "format"
    :option-type :string
    ;; Transient properties
    :key "F"
    :transient "--format"
    :class transient-option
    :argument "--format="
    :prompt "Format (mermaid): "
    :transient-group "Dependency Tree"
    :level 2
    :order 3)
   (show-all-paths
    :initarg :show-all-paths
    :type boolean
    :initform nil
    :documentation "Show all paths to nodes (--show-all-paths).
No deduplication for diamond dependencies."
    ;; CLI properties
    :long-option "show-all-paths"
    :option-type :boolean
    ;; Transient properties
    :key "ap"
    :transient "--show-all-paths"
    :class transient-switch
    :argument "--show-all-paths"
    :transient-group "Dependency Tree"
    :level 3
    :order 1))
  :documentation "Represents bd dep tree command.
Shows dependency tree rooted at the given issue."))

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-tree))
  "Return \"dep tree\" as the CLI subcommand name."
  "dep tree")

(cl-defmethod beads-command-validate ((command beads-command-dep-tree))
  "Validate dep tree COMMAND.
Checks that issue ID is provided.
Returns error string or nil if valid."
  (with-slots (issue-id) command
    (if (or (null issue-id) (string-empty-p issue-id))
        "Must provide issue ID"
      nil)))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-dep-tree))
  "Execute CMD in terminal buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Dependency Cycles Command

(eval-and-compile
(beads-defcommand beads-command-dep-cycles (beads-command-json)
  ()
  :documentation "Represents bd dep cycles command.
Detects dependency cycles in the issue graph."))

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-cycles))
  "Return \"dep cycles\" as the CLI subcommand name."
  "dep cycles")

(cl-defmethod beads-command-validate ((_command beads-command-dep-cycles))
  "Validate dep cycles COMMAND.
No required fields.
Returns nil (always valid)."
  nil)

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
          (let* ((result (beads-command-dep-add!
                          :issue-id issue-id
                          :depends-on depends-on-id
                          :dep-type type))
                 (formatted (beads-dep--format-dependency result)))
            (message "Dependency added: %s" formatted)
            (beads--invalidate-completion-cache)
            nil)
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
          (let* ((result (beads-command-dep-remove!
                          :issue-id issue-id
                          :depends-on depends-on-id))
                 (status (alist-get 'status result)))
            (message "Dependency %s: %s -> %s"
                     (propertize status 'face 'success)
                     issue-id depends-on-id)
            (beads--invalidate-completion-cache)
            nil)
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
  (let ((id (alist-get 'id issue))
        (title (alist-get 'title issue))
        (status (alist-get 'status issue))
        (depth (or (alist-get 'depth issue) 0))
        (truncated (alist-get 'truncated issue)))
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
    (let* ((issues (beads-command-dep-tree!
                    :issue-id beads-dep-tree--issue-id)))
      (beads-dep-tree--render (append issues nil)
                              beads-dep-tree--issue-id)
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
         (issues (beads-command-dep-tree! :issue-id issue-id))
         (buf-name (beads-buffer-name-utility "dep-tree" issue-id))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (setq default-directory caller-dir)
      (beads-dep-tree-mode)
      (setq-local beads-dep-tree--issue-id issue-id)
      (beads-dep-tree--render (append issues nil) issue-id))
    (pop-to-buffer buffer)))

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
    (let ((cycles (beads-command-dep-cycles!)))
      (beads-dep-cycles--render cycles)
      (message "Dependency cycles check complete"))))

;;;###autoload
(defun beads-dep-cycles ()
  "Check for dependency cycles and display results."
  (interactive)
  (beads-check-executable)
  (let* ((caller-dir default-directory)
         (cycles (beads-command-dep-cycles!))
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

(provide 'beads-command-dep)
;;; beads-command-dep.el ends here
