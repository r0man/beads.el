;;; beads-option.el --- Transient infix options for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module centralizes all transient infix definitions used
;; across beads.el commands.  By consolidating these definitions
;; in one place, we:
;;
;; - Avoid duplication across modules
;; - Make it easier to maintain consistent behavior
;; - Provide a single source of truth for option definitions
;;
;; This module includes:
;; - Custom transient classes (beads-create-transient-multiline)
;; - Utility functions for formatting values
;; - All transient-define-infix declarations
;;
;; State variables are defined in beads-state.el to avoid circular
;; dependencies with beads-reader.el.

;;; Code:

(require 'beads)
(require 'beads-state)
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
(declare-function beads-reader-create-parent "beads-reader")
(declare-function beads-reader-create-repo "beads-reader")
(declare-function beads-reader-create-from-template "beads-reader")
(declare-function beads-reader-create-file "beads-reader")
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
  "Format value for OBJ, displaying with appropriate faces and parentheses.
Shows \"(argument value)\" with argument and value separately colored."
  (let ((value (oref obj value))
        (arg (oref obj argument)))
    (if (and value (not (equal value "")))
        (concat "("
                (propertize arg 'face 'transient-argument)
                (propertize value 'face 'transient-value)
                ")")
      (propertize (concat "(" arg ")") 'face 'transient-inactive-argument))))

(cl-defmethod transient-format-value ((obj beads-option-global-switch))
  "Format value for OBJ, displaying with appropriate faces and parentheses.
Shows \"(argument)\" with proper face based on whether it's set."
  (let ((value (oref obj value))
        (arg (oref obj argument)))
    (propertize (concat "(" arg ")") 'face (if value
                                               'transient-argument
                                             'transient-inactive-argument))))

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
Shows argument and value with appropriate faces, like transient-option.
Multiline text is escaped to single line and truncated at 40 chars."
  (let ((value (oref obj value))
        (arg (oref obj argument)))
    (if (and value (not (string-empty-p (string-trim value))))
        (let* ((trimmed (string-trim value))
               ;; Replace newlines with escaped \n for display
               (escaped (replace-regexp-in-string "\n" "\\\\n" trimmed))
               (display (if (> (length escaped)
                               beads-display-value-max-length)
                           (concat (substring escaped 0
                                             beads-display-value-max-length)
                                  "...")
                         escaped)))
          (concat (propertize arg 'face 'transient-argument)
                  (propertize display 'face 'transient-value)))
      (propertize arg 'face 'transient-inactive-argument))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun beads-option--format-value (value)
  "Format VALUE for display in transient menu.
Shows the value in brackets with appropriate face, or [unset] if nil."
  (if (and value (not (string-empty-p (string-trim value))))
      (let ((display (if (> (length value) beads-display-value-max-length)
                        (concat (substring value 0
                                          beads-display-value-max-length)
                               "...")
                      value)))
        (propertize (format " [%s]" display) 'face 'transient-value))
    (propertize " [unset]" 'face 'transient-inactive-value)))

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
  :argument "--type="
  :prompt "Type: "
  :choices '("bug" "feature" "task" "epic" "chore")
  :reader #'beads-reader-create-type)

(transient-define-infix beads-option-create-priority ()
  "Set the priority of the issue."
  :class 'transient-option
  :description "Priority"
  :key "-p"
  :argument "--priority="
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
  :argument "--assignee="
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
  :argument "--labels="
  :prompt "Labels (comma-separated): "
  :reader #'beads-reader-create-labels)

(transient-define-infix beads-option-create-force ()
  "Toggle force flag for create operation."
  :class 'transient-switch
  :description "Force creation"
  :key "-f"
  :argument "--force")

(transient-define-infix beads-option-create-parent ()
  "Set parent issue ID for hierarchical child."
  :class 'transient-option
  :description "Parent issue ID"
  :key "-P"
  :argument "--parent="
  :prompt "Parent issue ID (e.g., bd-a3f8e9): "
  :reader #'beads-reader-create-parent)

(transient-define-infix beads-option-create-repo ()
  "Set target repository for issue (overrides auto-routing)."
  :class 'transient-option
  :description "Target repository"
  :key "-r"
  :argument "--repo="
  :prompt "Target repository: "
  :reader #'beads-reader-create-repo)

(transient-define-infix beads-option-create-from-template ()
  "Set template for issue creation."
  :class 'transient-option
  :description "From template"
  :key "-T"
  :argument "--from-template="
  :prompt "Template (epic, bug, feature): "
  :reader #'beads-reader-create-from-template)

(transient-define-infix beads-option-create-file ()
  "Set markdown file for bulk issue creation."
  :class 'transient-option
  :description "Create from file"
  :key "-F"
  :argument "--file="
  :prompt "Markdown file: "
  :reader #'beads-reader-create-file)

;;; ============================================================
;;; Transient Infix Definitions - beads-update
;;; ============================================================

(transient-define-infix beads-option-update-status ()
  "Set the status of the issue."
  :class 'transient-option
  :description "--status"
  :key "s"
  :argument "--status="
  :prompt "Status: "
  :choices '("open" "in_progress" "blocked" "closed")
  :reader #'beads-reader-update-status)

(transient-define-infix beads-option-update-priority ()
  "Set the priority of the issue."
  :class 'transient-option
  :description "--priority"
  :key "p"
  :argument "--priority="
  :prompt "Priority: "
  :reader #'beads-reader-update-priority)

(transient-define-infix beads-option-update-type ()
  "Set the type of the issue."
  :class 'transient-option
  :description "--type"
  :key "T"
  :argument "--type="
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
  :description "--assignee"
  :key "a"
  :argument "--assignee="
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
  :description "--description"
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
  :description "--message"
  :key "m"
  :argument "--message="
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
  :description "--type"
  :key "T"
  :argument "--type="
  :prompt "Dependency type: "
  :choices '("blocks" "related" "parent-child" "discovered-from")
  :reader #'beads-reader-dep-type)

;;; ============================================================
;;; Transient Infix Definitions - beads-misc (export/import/init)
;;; ============================================================

(transient-define-infix beads-option-export-output ()
  "Set the output file path."
  :class 'transient-option
  :description "--output"
  :key "o"
  :argument "--output="
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
  :description "--input (required)"
  :key "i"
  :argument "--input="
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
  :description "--prefix"
  :key "p"
  :argument "--prefix="
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

(transient-define-group beads-option-global-section
  [:level 5 "Global Options"
          (beads-option-global-actor)
          (beads-option-global-db)
          (beads-option-global-json)
          (beads-option-global-no-auto-flush)
          (beads-option-global-no-auto-import)
          (beads-option-global-no-daemon)
          (beads-option-global-no-db)
          (beads-option-global-sandbox)])

;; Load reader functions now that state variables are defined
(require 'beads-reader)

(provide 'beads-option)
;;; beads-option.el ends here
