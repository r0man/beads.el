;;; beads-option.el --- Transient infix options for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module centralizes all transient infix definitions and state
;; variables used across beads.el commands.  By consolidating these
;; definitions in one place, we:
;;
;; - Avoid duplication across modules
;; - Make it easier to maintain consistent behavior
;; - Provide a single source of truth for option definitions
;;
;; This module includes:
;; - Custom transient classes (beads-create-transient-multiline)
;; - Utility functions for formatting values
;; - All transient-define-infix declarations
;; - All state variables for transient menus

;;; Code:

(require 'beads)
(require 'transient)

;; Forward declare reader functions (loaded later to avoid circular deps)
(declare-function beads-reader-create-title "beads-reader")
(declare-function beads-reader-create-type "beads-reader")
(declare-function beads-reader-create-priority "beads-reader")
(declare-function beads-reader-create-custom-id "beads-reader")
(declare-function beads-reader-create-dependencies "beads-reader")
(declare-function beads-reader-create-assignee "beads-reader")
(declare-function beads-reader-create-external-ref "beads-reader")
(declare-function beads-reader-create-labels "beads-reader")
(declare-function beads-reader-update-status "beads-reader")
(declare-function beads-reader-update-priority "beads-reader")
(declare-function beads-reader-update-type "beads-reader")
(declare-function beads-reader-update-title "beads-reader")
(declare-function beads-reader-update-assignee "beads-reader")
(declare-function beads-reader-update-external-ref "beads-reader")
(declare-function beads-reader-close-issue-id "beads-reader")
(declare-function beads-reader-reopen-issue-id "beads-reader")
(declare-function beads-reader-sync-message "beads-reader")
(declare-function beads-reader-dep-add-issue-id "beads-reader")
(declare-function beads-reader-dep-add-depends-on-id "beads-reader")
(declare-function beads-reader-dep-add-type "beads-reader")
(declare-function beads-reader-dep-remove-issue-id "beads-reader")
(declare-function beads-reader-dep-remove-depends-on-id "beads-reader")
(declare-function beads-reader-dep-from "beads-reader")
(declare-function beads-reader-dep-to "beads-reader")
(declare-function beads-reader-dep-type "beads-reader")
(declare-function beads-reader-export-output "beads-reader")
(declare-function beads-reader-import-input "beads-reader")
(declare-function beads-reader-init-prefix "beads-reader")
(declare-function beads-reader-init-db "beads-reader")

;;; ============================================================
;;; Custom Transient Classes
;;; ============================================================

(defclass beads-option-global (transient-lisp-variable)
  ((pad-keys :initarg :pad-keys :initform nil))
  "Transient infix class for global bd flags.
Inherits from transient-lisp-variable for automatic variable persistence,
with custom display formatting to match transient-option.")

(defclass beads-option-global-switch (transient-lisp-variable)
  ((pad-keys :initarg :pad-keys :initform nil))
  "Transient switch class for global bd boolean flags.
Inherits from transient-lisp-variable for automatic variable persistence,
with custom display formatting to match transient-switch.")

(cl-defmethod transient-infix-read ((obj beads-option-global))
  "Read a string value for global option OBJ.
Uses `read-string' to ensure the value is always a string, not a symbol."
  (let ((prompt (concat (oref obj prompt)))
        (initial-input (oref obj value)))
    (read-string prompt initial-input)))

(cl-defmethod transient-infix-read ((obj beads-option-global-switch))
  "Toggle the boolean value for switch OBJ."
  (let ((current (oref obj value)))
    ;; Toggle: nil -> t, t -> nil
    (not current)))

(cl-defmethod transient-infix-value ((_obj beads-option-global))
  "Return nil to prevent these from being included in transient arguments.
Values are read directly from variables by beads--build-command."
  nil)

(cl-defmethod transient-infix-value ((_obj beads-option-global-switch))
  "Return nil to prevent these from being included in transient arguments.
Values are read directly from variables by beads--build-command."
  nil)

(cl-defmethod transient-format-value ((obj beads-option-global))
  "Format value for OBJ to match transient-option display.
Shows the full argument with value in parentheses.
Grey when unset, green when set."
  (let ((value (oref obj value))
        (arg (oref obj argument)))
    (concat "("
            (if (and value (not (equal value "")))
                (propertize (concat arg value) 'face 'transient-value)
              (propertize arg 'face 'transient-inactive-value))
            ")")))

(cl-defmethod transient-format-value ((obj beads-option-global-switch))
  "Format value for OBJ to match transient-switch display.
Shows the argument in parentheses, green when enabled, grey when disabled."
  (let ((value (oref obj value))
        (arg (oref obj argument)))
    (concat "("
            (if value
                (propertize arg 'face 'transient-value)
              (propertize arg 'face 'transient-inactive-value))
            ")")))

(defclass beads-create-transient-multiline (transient-option)
  ((multi-line :initarg :multi-line :initform t)
   (field-name :initarg :field-name :initform "Text"))
  "Transient infix class for multiline text fields.
This class provides an editor buffer for multiline text entry,
similar to git commit message editing.")


(cl-defmethod transient-infix-read ((obj beads-create-transient-multiline))
  "Read multiline text value for OBJ using a dedicated buffer."
  (let* ((value (oref obj value))
         (field-name (oref obj field-name))
         (transient-buf (current-buffer))
         (result nil))
    ;; Create a temporary buffer for editing
    (let* ((buffer-name (format "*beads-%s*" (downcase field-name)))
           (buffer (generate-new-buffer buffer-name)))
      (switch-to-buffer buffer)
      (when value
        (insert value))
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
      (let ((finish-func
             (lambda ()
               (interactive)
               (setq result (buffer-substring-no-properties
                            (point-min) (point-max)))
               (kill-buffer)
               (switch-to-buffer transient-buf)
               (exit-recursive-edit)))
            (cancel-func
             (lambda ()
               (interactive)
               (setq result nil)
               (kill-buffer)
               (switch-to-buffer transient-buf)
               (exit-recursive-edit))))
        (local-set-key (kbd "C-c C-c") finish-func)
        (local-set-key (kbd "C-c C-k") cancel-func)
        (message "Edit %s. C-c C-c to finish, C-c C-k to cancel."
                 field-name)
        (recursive-edit)))
    result))


(cl-defmethod transient-format-value ((obj beads-create-transient-multiline))
  "Format the value of multiline OBJ for display in transient menu.
Shows argument with truncated first line.  Grey when unset, green when set."
  (let ((value (oref obj value))
        (arg (oref obj argument)))
    (if (and value (not (string-empty-p (string-trim value))))
        (let* ((first-line (car (split-string value "\n" t)))
               (trimmed (string-trim first-line))
               (display (if (> (length trimmed) 40)
                           (concat (substring trimmed 0 40) "...")
                         trimmed)))
          (propertize (concat arg display) 'face 'transient-value))
      (propertize arg 'face 'transient-inactive-value))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun beads-option--format-value (value)
  "Format VALUE for display in transient menu.
Shows the value in brackets with appropriate face, or [unset] if nil."
  (if (and value (not (string-empty-p (string-trim value))))
      (let ((display (if (> (length value) 40)
                        (concat (substring value 0 40) "...")
                      value)))
        (propertize (format " [%s]" display) 'face 'transient-value))
    (propertize " [unset]" 'face 'transient-inactive-value)))

;;; ============================================================
;;; State Variables
;;; ============================================================

;;; Global state variables (shared across all commands)

(defvar beads-global-actor nil
  "Global actor name override for audit trail.
When set, overrides $BD_ACTOR or $USER.")

(defvar beads-global-db nil
  "Global database path override.
When set, bypasses auto-discovery of .beads/*.db.")

(defvar beads-global-json nil
  "Global JSON output flag.
When non-nil, forces JSON output format.")

(defvar beads-global-no-auto-flush nil
  "Global no-auto-flush flag.
When non-nil, disables automatic JSONL sync after CRUD operations.")

(defvar beads-global-no-auto-import nil
  "Global no-auto-import flag.
When non-nil, disables automatic JSONL import when newer than DB.")

(defvar beads-global-no-daemon nil
  "Global no-daemon flag.
When non-nil, forces direct storage mode, bypassing daemon.")

(defvar beads-global-no-db nil
  "Global no-db flag.
When non-nil, uses no-db mode: load from JSONL only, no SQLite.")

(defvar beads-global-sandbox nil
  "Global sandbox flag.
When non-nil, enables sandbox mode: disables daemon and auto-sync.")

;;; beads-create state variables

(defvar beads-create--title nil
  "Title for the issue being created.")

(defvar beads-create--type nil
  "Type for the issue being created (bug, feature, task, epic, chore).")

(defvar beads-create--priority nil
  "Priority for the issue being created (0-4).")

(defvar beads-create--description nil
  "Description for the issue being created.")

(defvar beads-create--custom-id nil
  "Custom ID for the issue being created.")

(defvar beads-create--dependencies nil
  "Dependencies for the issue being created (format: type:id,...).")

(defvar beads-create--acceptance nil
  "Acceptance criteria for the issue being created.")

(defvar beads-create--assignee nil
  "Assignee for the issue being created.")

(defvar beads-create--design nil
  "Design notes for the issue being created.")

(defvar beads-create--external-ref nil
  "External reference for the issue being created.")

(defvar beads-create--labels nil
  "Labels for the issue being created.")

(defvar beads-create--force nil
  "Force flag for create operation (override validations).")

;;; beads-update state variables

(defvar beads-update--issue-id nil
  "Issue ID being updated.")

(defvar beads-update--original-data nil
  "Original issue data as fetched from bd.")

(defvar beads-update--status nil
  "New status for the issue.")

(defvar beads-update--priority nil
  "New priority for the issue.")

(defvar beads-update--type nil
  "New type for the issue.")

(defvar beads-update--title nil
  "New title for the issue.")

(defvar beads-update--description nil
  "New description for the issue.")

(defvar beads-update--acceptance-criteria nil
  "New acceptance criteria for the issue.")

(defvar beads-update--design nil
  "New design notes for the issue.")

(defvar beads-update--notes nil
  "New notes for the issue.")

(defvar beads-update--assignee nil
  "New assignee for the issue.")

(defvar beads-update--external-ref nil
  "New external reference for the issue.")

;;; beads-close state variables

(defvar beads-close--issue-id nil
  "Issue ID to close.")

(defvar beads-close--reason nil
  "Reason for closing the issue.")

;;; beads-reopen state variables

(defvar beads-reopen--issue-id nil
  "Issue ID to reopen.")

(defvar beads-reopen--reason nil
  "Reason for reopening the issue.")

;;; beads-sync state variables

(defvar beads-sync--dry-run nil
  "Whether to run in dry-run mode (preview without changes).")

(defvar beads-sync--message nil
  "Custom commit message for sync operation.")

(defvar beads-sync--no-pull nil
  "Whether to skip pulling from remote.")

(defvar beads-sync--no-push nil
  "Whether to skip pushing to remote.")

;;; beads-dep state variables

(defvar beads-dep-add--issue-id nil
  "Issue ID for add dependency operation.")

(defvar beads-dep-add--depends-on-id nil
  "Depends-on ID for add dependency operation.")

(defvar beads-dep-add--type nil
  "Dependency type for add operation.")

(defvar beads-dep-remove--issue-id nil
  "Issue ID for remove dependency operation.")

(defvar beads-dep-remove--depends-on-id nil
  "Depends-on ID for remove dependency operation.")

(defvar beads-dep--from-issue nil
  "Source issue ID for dependency operations.")

(defvar beads-dep--to-issue nil
  "Target issue ID for dependency operations.")

(defvar beads-dep--dep-type nil
  "Dependency type (blocks, related, parent-child, discovered-from).")

;;; beads-misc (export, import, init) state variables

(defvar beads-export--output nil
  "Output file path for export.")

(defvar beads-export--no-auto-flush nil
  "Whether to disable auto-flush.")

(defvar beads-import--input nil
  "Input file path for import.")

(defvar beads-import--dry-run nil
  "Whether to run in dry-run mode.")

(defvar beads-import--resolve-collisions nil
  "Whether to auto-resolve collisions.")

(defvar beads-init--prefix nil
  "Issue ID prefix for new project.")

(defvar beads-init--db-path nil
  "Database path for new project.")

;;; ============================================================
;;; Transient Infix Definitions - beads-create
;;; ============================================================

(transient-define-infix beads-option-create-title ()
  "Set the title of the issue."
  :class 'transient-option
  :description "Title (required)"
  :key "t"
  :argument "--title="
  :prompt "Issue title: "
  :reader #'beads-reader-create-title)

(transient-define-infix beads-option-create-type ()
  "Set the type of the issue."
  :class 'transient-option
  :description "Type"
  :key "-t"
  :argument "-t="
  :prompt "Type: "
  :choices '("bug" "feature" "task" "epic" "chore")
  :reader #'beads-reader-create-type)

(transient-define-infix beads-option-create-priority ()
  "Set the priority of the issue."
  :class 'transient-option
  :description "Priority"
  :key "-p"
  :argument "-p="
  :prompt "Priority: "
  :reader #'beads-reader-create-priority)

(transient-define-infix beads-option-create-description ()
  "Set the description using a multiline editor."
  :class 'beads-create-transient-multiline
  :field-name "Description"
  :description "Description"
  :key "-d"
  :argument "--description=")

(transient-define-infix beads-option-create-custom-id ()
  "Set a custom ID for the issue."
  :class 'transient-option
  :description "Custom ID"
  :key "-i"
  :argument "--id="
  :prompt "Custom ID: "
  :reader #'beads-reader-create-custom-id)

(transient-define-infix beads-option-create-dependencies ()
  "Set dependencies for the issue."
  :class 'transient-option
  :description "Dependencies"
  :key "-D"
  :argument "--deps="
  :prompt "Dependencies (type:id,...): "
  :reader #'beads-reader-create-dependencies)

(transient-define-infix beads-option-create-acceptance ()
  "Set acceptance criteria using a multiline editor."
  :class 'beads-create-transient-multiline
  :field-name "Acceptance Criteria"
  :description "Acceptance criteria"
  :key "-A"
  :argument "--acceptance=")

(transient-define-infix beads-option-create-assignee ()
  "Set the assignee of the issue."
  :class 'transient-option
  :description "Assignee"
  :key "-a"
  :argument "-a="
  :prompt "Assignee: "
  :reader #'beads-reader-create-assignee)

(transient-define-infix beads-option-create-design ()
  "Set design notes using a multiline editor."
  :class 'beads-create-transient-multiline
  :field-name "Design"
  :description "Design notes"
  :key "-G"
  :argument "--design=")

(transient-define-infix beads-option-create-external-ref ()
  "Set external reference for the issue."
  :class 'transient-option
  :description "External reference"
  :key "-x"
  :argument "--external-ref="
  :prompt "External reference: "
  :reader #'beads-reader-create-external-ref)

(transient-define-infix beads-option-create-labels ()
  "Set labels for the issue."
  :class 'transient-option
  :description "Labels"
  :key "-l"
  :argument "-l="
  :prompt "Labels (comma-separated): "
  :reader #'beads-reader-create-labels)

(transient-define-infix beads-option-create-force ()
  "Toggle force flag for create operation."
  :class 'transient-switch
  :description "Force creation"
  :key "f"
  :argument "--force")

;;; ============================================================
;;; Transient Infix Definitions - beads-update
;;; ============================================================

(transient-define-infix beads-option-update-status ()
  "Set the status of the issue."
  :class 'transient-option
  :description "-s, --status"
  :key "s"
  :argument "-s="
  :prompt "Status: "
  :choices '("open" "in_progress" "blocked" "closed")
  :reader #'beads-reader-update-status)

(transient-define-infix beads-option-update-priority ()
  "Set the priority of the issue."
  :class 'transient-option
  :description "-p, --priority"
  :key "p"
  :argument "-p="
  :prompt "Priority: "
  :reader #'beads-reader-update-priority)

(transient-define-infix beads-option-update-type ()
  "Set the type of the issue."
  :class 'transient-option
  :description "-t, --type"
  :key "T"
  :argument "-t="
  :prompt "Type: "
  :choices '("bug" "feature" "task" "epic" "chore")
  :reader #'beads-reader-update-type)

(transient-define-infix beads-option-update-title ()
  "Set the title of the issue."
  :class 'transient-option
  :description "--title"
  :key "t"
  :argument "--title="
  :prompt "Issue title: "
  :reader #'beads-reader-update-title)

(transient-define-infix beads-option-update-assignee ()
  "Set the assignee of the issue."
  :class 'transient-option
  :description "-a, --assignee"
  :key "a"
  :argument "-a="
  :prompt "Assignee: "
  :reader #'beads-reader-update-assignee)

(transient-define-infix beads-option-update-external-ref ()
  "Set the external reference of the issue."
  :class 'transient-option
  :description "--external-ref"
  :key "x"
  :argument "--external-ref="
  :prompt "External reference: "
  :reader #'beads-reader-update-external-ref)

(transient-define-infix beads-option-update-description ()
  "Set the description using a multiline editor."
  :class 'beads-create-transient-multiline
  :field-name "Description"
  :description "-d, --description"
  :key "d"
  :argument "--description=")

(transient-define-infix beads-option-update-acceptance-multiline ()
  "Set the acceptance criteria using a multiline editor."
  :class 'beads-create-transient-multiline
  :field-name "Acceptance Criteria"
  :description "--acceptance-criteria"
  :key "A"
  :argument "--acceptance-criteria=")

(transient-define-infix beads-option-update-design-multiline ()
  "Set the design notes using a multiline editor."
  :class 'beads-create-transient-multiline
  :field-name "Design"
  :description "--design"
  :key "G"
  :argument "--design=")

(transient-define-infix beads-option-update-notes-multiline ()
  "Set the notes using a multiline editor."
  :class 'beads-create-transient-multiline
  :field-name "Notes"
  :description "--notes"
  :key "N"
  :argument "--notes=")

;;; ============================================================
;;; Transient Infix Definitions - beads-close
;;; ============================================================

(transient-define-infix beads-option-close-issue-id ()
  "Set the issue ID to close."
  :class 'transient-option
  :description "Issue ID (required)"
  :key "i"
  :argument "--id="
  :prompt "Issue ID: "
  :reader #'beads-reader-close-issue-id)

(transient-define-infix beads-option-close-reason ()
  "Set the reason for closing using a multiline editor."
  :class 'beads-create-transient-multiline
  :field-name "Close Reason"
  :description "--reason"
  :key "r"
  :argument "--reason=")

;;; ============================================================
;;; Transient Infix Definitions - beads-reopen
;;; ============================================================

(transient-define-infix beads-option-reopen-issue-id ()
  "Set the issue ID to reopen."
  :class 'transient-option
  :description "Issue ID (required)"
  :key "i"
  :argument "--id="
  :prompt "Issue ID: "
  :reader #'beads-reader-reopen-issue-id)

(transient-define-infix beads-option-reopen-reason ()
  "Set the reason for reopening using a multiline editor."
  :class 'beads-create-transient-multiline
  :field-name "Reopen Reason"
  :description "--reason"
  :key "r"
  :argument "--reason=")

;;; ============================================================
;;; Transient Infix Definitions - beads-sync
;;; ============================================================

(transient-define-infix beads-option-sync-dry-run ()
  "Toggle dry-run mode."
  :class 'transient-switch
  :description "--dry-run"
  :key "d"
  :argument "--dry-run")

(transient-define-infix beads-option-sync-message ()
  "Set custom commit message."
  :class 'transient-option
  :description "-m, --message"
  :key "m"
  :argument "-m="
  :prompt "Commit message: "
  :reader #'beads-reader-sync-message)

(transient-define-infix beads-option-sync-no-pull ()
  "Toggle skip pull flag."
  :class 'transient-switch
  :description "--no-pull"
  :key "P"
  :argument "--no-pull")

(transient-define-infix beads-option-sync-no-push ()
  "Toggle skip push flag."
  :class 'transient-switch
  :description "--no-push"
  :key "p"
  :argument "--no-push")

;;; ============================================================
;;; Transient Infix Definitions - beads-dep
;;; ============================================================

(transient-define-infix beads-option-dep-add-issue-id ()
  "Specify issue ID for add dependency."
  :class 'transient-option
  :key "i"
  :description "Issue ID"
  :argument "--issue-id="
  :prompt "Issue ID: "
  :reader #'beads-reader-dep-add-issue-id)

(transient-define-infix beads-option-dep-add-depends-on-id ()
  "Specify depends-on ID for add dependency."
  :class 'transient-option
  :key "d"
  :description "Depends on ID"
  :argument "--depends-on="
  :prompt "Depends on issue ID: "
  :reader #'beads-reader-dep-add-depends-on-id)

(transient-define-infix beads-option-dep-add-type ()
  "Specify dependency type for add operation."
  :class 'transient-option
  :key "t"
  :description "Dependency type"
  :argument "--type="
  :choices '("blocks" "related" "parent-child" "discovered-from")
  :reader #'beads-reader-dep-add-type)

(transient-define-infix beads-option-dep-remove-issue-id ()
  "Specify issue ID for remove dependency."
  :class 'transient-option
  :key "i"
  :description "Issue ID"
  :argument "--issue-id="
  :prompt "Issue ID: "
  :reader #'beads-reader-dep-remove-issue-id)

(transient-define-infix beads-option-dep-remove-depends-on-id ()
  "Specify depends-on ID for remove dependency."
  :class 'transient-option
  :key "d"
  :description "Depends on ID"
  :argument "--depends-on="
  :prompt "Depends on issue ID: "
  :reader #'beads-reader-dep-remove-depends-on-id)

(transient-define-infix beads-option-dep-from ()
  "Set the source issue ID."
  :class 'transient-option
  :description "--from"
  :key "f"
  :argument "--from="
  :prompt "From issue: "
  :reader #'beads-reader-dep-from)

(transient-define-infix beads-option-dep-to ()
  "Set the target issue ID."
  :class 'transient-option
  :description "--to"
  :key "t"
  :argument "--to="
  :prompt "To issue: "
  :reader #'beads-reader-dep-to)

(transient-define-infix beads-option-dep-type ()
  "Set the dependency type."
  :class 'transient-option
  :description "-t, --type"
  :key "T"
  :argument "-t="
  :prompt "Dependency type: "
  :choices '("blocks" "related" "parent-child" "discovered-from")
  :reader #'beads-reader-dep-type)

;;; ============================================================
;;; Transient Infix Definitions - beads-misc (export/import/init)
;;; ============================================================

(transient-define-infix beads-option-export-output ()
  "Set the output file path."
  :class 'transient-option
  :description "-o, --output"
  :key "o"
  :argument "-o="
  :prompt "Output file: "
  :reader #'beads-reader-export-output)

(transient-define-infix beads-option-export-no-auto-flush ()
  "Toggle no-auto-flush flag."
  :class 'transient-switch
  :description "--no-auto-flush"
  :key "n"
  :argument "--no-auto-flush")

(transient-define-infix beads-option-import-input ()
  "Set the input file path."
  :class 'transient-option
  :description "-i, --input (required)"
  :key "i"
  :argument "-i="
  :prompt "Input file: "
  :reader #'beads-reader-import-input)

(transient-define-infix beads-option-import-dry-run ()
  "Toggle dry-run flag."
  :class 'transient-switch
  :description "--dry-run"
  :key "d"
  :argument "--dry-run")

(transient-define-infix beads-option-import-resolve-collisions ()
  "Toggle resolve-collisions flag."
  :class 'transient-switch
  :description "--resolve-collisions"
  :key "r"
  :argument "--resolve-collisions")

(transient-define-infix beads-option-init-prefix ()
  "Set the issue ID prefix."
  :class 'transient-option
  :description "-p, --prefix"
  :key "p"
  :argument "-p="
  :prompt "Issue ID prefix: "
  :reader #'beads-reader-init-prefix)

(transient-define-infix beads-option-init-db ()
  "Set the database path."
  :class 'transient-option
  :description "--db"
  :key "d"
  :argument "--db="
  :prompt "Database path: "
  :reader #'beads-reader-init-db)

;;; ============================================================
;;; Transient Infix Definitions - Global Options
;;; ============================================================

(transient-define-infix beads-option-global-actor ()
  "Override actor name for audit trail."
  :class 'beads-option-global
  :variable 'beads-global-actor
  :description "Actor name override"
  :key "=a"
  :argument "--actor="
  :prompt "Actor name: ")

(transient-define-infix beads-option-global-db ()
  "Override database path."
  :class 'beads-option-global
  :variable 'beads-global-db
  :description "Database path override"
  :key "=d"
  :argument "--db="
  :prompt "Database path: ")

(transient-define-infix beads-option-global-json ()
  "Force JSON output format."
  :class 'beads-option-global-switch
  :variable 'beads-global-json
  :description "Force JSON output"
  :key "=j"
  :argument "--json")

(transient-define-infix beads-option-global-no-auto-flush ()
  "Disable automatic JSONL sync."
  :class 'beads-option-global-switch
  :variable 'beads-global-no-auto-flush
  :description "No auto-flush"
  :key "=F"
  :argument "--no-auto-flush")

(transient-define-infix beads-option-global-no-auto-import ()
  "Disable automatic JSONL import."
  :class 'beads-option-global-switch
  :variable 'beads-global-no-auto-import
  :description "No auto-import"
  :key "=I"
  :argument "--no-auto-import")

(transient-define-infix beads-option-global-no-daemon ()
  "Force direct storage mode."
  :class 'beads-option-global-switch
  :variable 'beads-global-no-daemon
  :description "No daemon"
  :key "=D"
  :argument "--no-daemon")

(transient-define-infix beads-option-global-no-db ()
  "Use no-db mode (JSONL only)."
  :class 'beads-option-global-switch
  :variable 'beads-global-no-db
  :description "No-db mode"
  :key "=N"
  :argument "--no-db")

(transient-define-infix beads-option-global-sandbox ()
  "Enable sandbox mode."
  :class 'beads-option-global-switch
  :variable 'beads-global-sandbox
  :description "Sandbox mode"
  :key "=s"
  :argument "--sandbox")

;; Load reader functions now that state variables are defined
(require 'beads-reader)

(provide 'beads-option)
;;; beads-option.el ends here
