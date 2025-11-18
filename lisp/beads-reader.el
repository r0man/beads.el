;;; beads-reader.el --- Reader functions for transient infixes -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides reader functions used by transient infix
;; definitions in beads-option.el.  By extracting reader functions
;; into named functions, we:
;;
;; - Make the code more maintainable and testable
;; - Provide a path toward making readers stateless
;; - Reduce duplication and improve reusability
;; - Make infix definitions more readable
;;
;; Reader functions follow the transient reader protocol:
;; They receive (prompt initial-input history) arguments and return
;; a string value to be used by the transient infix.
;;
;; State variables used by reader functions are defined in
;; beads-state.el, which eliminates circular dependencies between
;; this module and beads-option.el.

;;; Code:

(require 'beads)
(require 'beads-state)
(require 'beads-label)

;;; ============================================================
;;; Common Reader Functions
;;; ============================================================

(defun beads-reader-issue-id (prompt &optional _initial-input _history)
  "Read an issue ID using completion.
PROMPT is shown to the user."
  (completing-read prompt (beads--issue-completion-table)
                   nil nil nil 'beads--issue-id-history))

(defun beads-reader-string (prompt default)
  "Read a string with PROMPT, using DEFAULT as initial input."
  (lambda (_prompt _initial-input _history)
    (read-string prompt default)))

(defun beads-reader-file (prompt default &optional mustmatch)
  "Read a file path with PROMPT, using DEFAULT as initial input.
If MUSTMATCH is non-nil, file must exist."
  (lambda (_prompt _initial-input _history)
    (read-file-name prompt nil default mustmatch)))

(defun beads-reader-choice (prompt choices &optional default)
  "Read a choice with PROMPT from CHOICES list.
DEFAULT is the initial selection if provided."
  (lambda (_prompt _initial-input _history)
    (completing-read prompt choices nil t default)))

(defun beads-reader-priority (prompt default-var)
  "Read a priority level with PROMPT.
DEFAULT-VAR is the variable holding the current priority value."
  (lambda (_prompt _initial-input _history)
    (let* ((choices '(("0 - Critical" . 0)
                      ("1 - High" . 1)
                      ("2 - Medium" . 2)
                      ("3 - Low" . 3)
                      ("4 - Backlog" . 4)))
           (default-val (symbol-value default-var))
           (selection (completing-read
                       prompt choices nil t
                       (when default-val
                         (car (rassoc default-val choices)))))
           (priority (cdr (assoc selection choices))))
      (number-to-string priority))))

;;; ============================================================
;;; beads-create Reader Functions
;;; ============================================================

(defun beads-reader-issue-title (_prompt _initial-input _history)
  "Read title for an issue."
  (read-string "Issue title: "))

(defun beads-reader-issue-type (_prompt _initial-input _history)
  "Read type for an issue."
  (completing-read "Type: "
                   '("bug" "feature" "task" "epic" "chore")
                   nil t))

(defun beads-reader-issue-priority (_prompt _initial-input _history)
  "Read priority for an issue."
  (let* ((choices '(("0 - Critical" . 0)
                    ("1 - High" . 1)
                    ("2 - Medium" . 2)
                    ("3 - Low" . 3)
                    ("4 - Backlog" . 4)))
         (selection (completing-read "Priority: " choices nil t))
         (priority (cdr (assoc selection choices))))
    (number-to-string priority)))

(defun beads-reader-create-custom-id (_prompt _initial-input _history)
  "Read custom ID for issue creation."
  (read-string "Custom ID: "))

(defun beads-reader-create-dependencies (_prompt _initial-input _history)
  "Read dependencies for issue creation."
  (read-string "Dependencies (e.g., blocks:bd-a1b2,related:bd-f14c): "))

(defun beads-reader-issue-assignee (_prompt _initial-input _history)
  "Read assignee for an issue."
  (read-string "Assignee: "))

(defun beads-reader-issue-external-ref (_prompt _initial-input _history)
  "Read external reference for an issue."
  (read-string "External reference (e.g., gh-9, jira-ABC): "))

(defun beads-reader-issue-labels (_prompt _initial-input _history)
  "Read labels for an issue with auto-completion."
  (let ((labels (beads--label-completion-table)))
    (if labels
        (mapconcat #'identity
                   (completing-read-multiple "Labels (comma-separated): " labels)
                   ",")
      (read-string "Labels (comma-separated): "))))

(defun beads-reader-create-parent (_prompt _initial-input _history)
  "Read parent issue ID for hierarchical child."
  (completing-read "Parent issue ID: "
                   (beads--issue-completion-table)
                   nil nil))

(defun beads-reader-create-repo (_prompt _initial-input _history)
  "Read target repository for issue."
  (read-string "Target repository: "))

(defun beads-reader-create-from-template (_prompt _initial-input _history)
  "Read template name for issue creation."
  (completing-read "Template: "
                   '("epic" "bug" "feature")
                   nil nil))

(defun beads-reader-create-file (_prompt _initial-input _history)
  "Read markdown file path for bulk issue creation."
  (read-file-name "Markdown file: " nil nil t))

;;; ============================================================
;;; beads-update Reader Functions
;;; ============================================================

(defun beads-reader-update-status (_prompt _initial-input _history)
  "Read status for issue update."
  (completing-read "Status: "
                   '("open" "in_progress" "blocked" "closed")
                   nil t beads-update--status))

(defun beads-reader-update-priority (_prompt _initial-input _history)
  "Read priority for issue update."
  (funcall (beads-reader-priority "Priority: " 'beads-update--priority)
           nil nil nil))

(defun beads-reader-update-type (_prompt _initial-input _history)
  "Read type for issue update."
  (completing-read "Type: "
                   '("bug" "feature" "task" "epic" "chore")
                   nil t beads-update--type))

(defun beads-reader-update-title (_prompt _initial-input _history)
  "Read title for issue update."
  (read-string "Issue title: " beads-update--title))

(defun beads-reader-update-assignee (_prompt _initial-input _history)
  "Read assignee for issue update."
  (read-string "Assignee: " beads-update--assignee))

(defun beads-reader-update-external-ref (_prompt _initial-input _history)
  "Read external reference for issue update."
  (read-string "External reference (e.g., gh-9, jira-ABC): "
               beads-update--external-ref))

;;; ============================================================
;;; beads-close Reader Functions
;;; ============================================================

(defun beads-reader-close-issue-id (_prompt _initial-input _history)
  "Read issue ID to close."
  (completing-read "Issue ID to close: "
                   (beads--issue-completion-table)
                   nil t beads-close--issue-id
                   'beads--issue-id-history))

;;; ============================================================
;;; beads-reopen Reader Functions
;;; ============================================================

(defun beads-reader-reopen-issue-id (_prompt _initial-input _history)
  "Read issue ID to reopen."
  (completing-read "Issue ID to reopen: "
                   (beads--issue-completion-table)
                   nil t beads-reopen--issue-id
                   'beads--issue-id-history))

;;; ============================================================
;;; beads-sync Reader Functions
;;; ============================================================

(defun beads-reader-sync-message (_prompt _initial-input _history)
  "Read commit message for sync operation."
  (read-string "Commit message: "))

;;; ============================================================
;;; beads-dep Reader Functions
;;; ============================================================

(defun beads-reader-dep-add-issue-id (prompt _initial-input _history)
  "Read issue ID for add dependency operation.
PROMPT is shown to the user."
  (completing-read prompt (beads--issue-completion-table)
                   nil nil nil 'beads--issue-id-history))

(defun beads-reader-dep-add-depends-on-id (prompt _initial-input _history)
  "Read depends-on ID for add dependency operation.
PROMPT is shown to the user."
  (completing-read prompt (beads--issue-completion-table)
                   nil nil nil 'beads--issue-id-history))

(defun beads-reader-dep-add-type (prompt _initial-input _history)
  "Read dependency type for add operation.
PROMPT is shown to the user."
  (completing-read prompt
                   '("blocks" "related" "parent-child"
                     "discovered-from")
                   nil t nil
                   'beads--dependency-type-history
                   "blocks"))

(defun beads-reader-dep-remove-issue-id (prompt _initial-input _history)
  "Read issue ID for remove dependency operation.
PROMPT is shown to the user."
  (completing-read prompt (beads--issue-completion-table)
                   nil nil nil 'beads--issue-id-history))

(defun beads-reader-dep-remove-depends-on-id (prompt _initial-input _history)
  "Read depends-on ID for remove dependency operation.
PROMPT is shown to the user."
  (completing-read prompt (beads--issue-completion-table)
                   nil nil nil 'beads--issue-id-history))

(defun beads-reader-dep-from (_prompt _initial-input _history)
  "Read source issue ID for dependency operations."
  (completing-read "From issue: "
                   (beads--issue-completion-table)
                   nil nil beads-dep--from-issue))

(defun beads-reader-dep-to (_prompt _initial-input _history)
  "Read target issue ID for dependency operations."
  (completing-read "To issue: "
                   (beads--issue-completion-table)
                   nil nil beads-dep--to-issue))

(defun beads-reader-dep-type (_prompt _initial-input _history)
  "Read dependency type."
  (completing-read "Type: "
                   '("blocks" "related" "parent-child"
                     "discovered-from")
                   nil t beads-dep--dep-type))

;;; ============================================================
;;; Export/Import/Init/Quickstart Reader Functions
;;; ============================================================

(defun beads-reader-export-output (_prompt _initial-input _history)
  "Read output file path for export operation."
  (read-file-name "Output file: " nil nil))

(defun beads-reader-import-input (_prompt _initial-input _history)
  "Read input file path for import operation."
  (read-file-name "Input file: " nil nil t))

(defun beads-reader-init-prefix (_prompt _initial-input _history)
  "Read issue ID prefix for init operation."
  (read-string "Issue ID prefix (e.g., bd): " nil))

(defun beads-reader-init-db (_prompt _initial-input _history)
  "Read database path for init operation."
  (read-file-name "Database path: " nil nil))

;;; ============================================================
;;; beads-list Reader Functions
;;; ============================================================

(defun beads-reader-list-assignee (_prompt _initial-input _history)
  "Read assignee for list filter."
  (read-string "Filter by assignee: "))

(defun beads-reader-list-date (_prompt _initial-input _history)
  "Read date for list filter (YYYY-MM-DD or RFC3339)."
  (read-string "Date (YYYY-MM-DD or RFC3339): "))

(defun beads-reader-list-desc-contains (_prompt _initial-input _history)
  "Read description search text for list filter."
  (read-string "Description contains: "))

(defun beads-reader-list-format (_prompt _initial-input _history)
  "Read output format for list."
  (completing-read "Output format: "
                   '("digraph" "dot" "template")
                   nil t))

(defun beads-reader-list-id (_prompt _initial-input _history)
  "Read comma-separated issue IDs for list filter."
  (read-string "Issue IDs (comma-separated): "))

(defun beads-reader-list-label (_prompt _initial-input _history)
  "Read label for list filter with auto-completion."
  (let ((labels (beads--label-completion-table)))
    (if labels
        (completing-read "Label (AND): " labels nil nil)
      (read-string "Label (AND): "))))

(defun beads-reader-list-labels (_prompt _initial-input _history)
  "Read comma-separated labels for list filter with auto-completion."
  (let ((labels (beads--label-completion-table)))
    (if labels
        (mapconcat #'identity
                   (completing-read-multiple "Labels (comma-separated): " labels)
                   ",")
      (read-string "Labels (comma-separated): "))))

(defun beads-reader-list-limit (_prompt _initial-input _history)
  "Read limit for list results."
  (let ((input (read-string "Limit (number of results): ")))
    (if (string-empty-p input)
        ""
      (number-to-string (string-to-number input)))))

(defun beads-reader-list-notes-contains (_prompt _initial-input _history)
  "Read notes search text for list filter."
  (read-string "Notes contains: "))

(defun beads-reader-list-priority (_prompt _initial-input _history)
  "Read priority for list filter."
  (let* ((choices '(("0 - Critical" . 0)
                    ("1 - High" . 1)
                    ("2 - Medium" . 2)
                    ("3 - Low" . 3)
                    ("4 - Backlog" . 4)))
         (selection (completing-read "Priority: " choices nil t))
         (priority (cdr (assoc selection choices))))
    (number-to-string priority)))

(defun beads-reader-list-priority-min (_prompt _initial-input _history)
  "Read minimum priority for list filter."
  (let* ((choices '(("0 - Critical" . 0)
                    ("1 - High" . 1)
                    ("2 - Medium" . 2)
                    ("3 - Low" . 3)
                    ("4 - Backlog" . 4)))
         (selection (completing-read "Minimum priority: " choices nil t))
         (priority (cdr (assoc selection choices))))
    (number-to-string priority)))

(defun beads-reader-list-priority-max (_prompt _initial-input _history)
  "Read maximum priority for list filter."
  (let* ((choices '(("0 - Critical" . 0)
                    ("1 - High" . 1)
                    ("2 - Medium" . 2)
                    ("3 - Low" . 3)
                    ("4 - Backlog" . 4)))
         (selection (completing-read "Maximum priority: " choices nil t))
         (priority (cdr (assoc selection choices))))
    (number-to-string priority)))

(defun beads-reader-list-status (_prompt _initial-input _history)
  "Read status for list filter."
  (completing-read "Status: "
                   '("open" "in_progress" "blocked" "closed")
                   nil t))

(defun beads-reader-list-title (_prompt _initial-input _history)
  "Read title text for list filter."
  (read-string "Title: "))

(defun beads-reader-list-title-contains (_prompt _initial-input _history)
  "Read title search text for list filter."
  (read-string "Title contains: "))

(defun beads-reader-list-type (_prompt _initial-input _history)
  "Read type for list filter."
  (completing-read "Type: "
                   '("bug" "feature" "task" "epic" "chore")
                   nil t))

;;; ============================================================
;;; Label-specific Reader Functions
;;; ============================================================

(defun beads-reader-label-issue-ids (_prompt _initial-input _history)
  "Read issue ID(s) for label operations, with context detection.
Detects current issue from beads-show or beads-list buffer,
falls back to `completing-read-multiple' for selecting multiple issue IDs."
  (let ((detected-id (beads-label--detect-issue-id)))
    (if detected-id
        detected-id
      (let ((selected (completing-read-multiple
                       "Issue ID(s) (use crm-separator to select multiple): "
                       (beads--issue-completion-table)
                       nil nil)))
        (if selected
            (mapconcat #'identity selected ",")
          "")))))

(defun beads-reader-label-name (_prompt _initial-input _history)
  "Read label name with auto-completion from existing labels."
  (let ((labels (beads--label-completion-table)))
    (if labels
        (completing-read "Label name: " labels nil nil)
      (read-string "Label name: "))))

(provide 'beads-reader)
;;; beads-reader.el ends here
