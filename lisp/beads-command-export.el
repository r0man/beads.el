;;; beads-command-export.el --- Export command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-export' EIEIO class for the
;; `bd export' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd export command exports issues to JSON Lines or Obsidian Tasks
;; markdown format.  Issues are sorted by ID for consistent diffs.
;;
;; Features:
;; - Export to jsonl (default) or obsidian format
;; - Filter by status, type, assignee, priority, labels
;; - Date range filters (created, updated)
;; - Output to stdout or file
;;
;; Usage:
;;   (beads-command-execute (beads-command-export :format "jsonl"))
;;   (beads-command-export!)  ; convenience function

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;; Forward declarations
(declare-function beads--sanitize-string "beads")
(declare-function beads--string-blank-p "beads")
(declare-function beads--find-beads-dir "beads")
(declare-function beads-buffer-name-utility "beads-buffer")
(defvar beads-executable)

;;; Export Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-export (beads-command-json)
  ((format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Export format (-f, --format).
Values: jsonl (default), obsidian."
    ;; CLI properties
    :long-option "format"
    :short-option "f"
    :option-type :string
    ;; Transient properties
    :transient-key "f"
    :transient-description "--format"
    :transient-class transient-option
    :transient-argument "--format="
    :transient-prompt "Format (jsonl/obsidian): "
    :transient-group "Export Options"
    :transient-level 1
    :transient-order 1)
   (output
    :initarg :output
    :type (or null string)
    :initform nil
    :documentation "Output file (-o, --output).
Default: stdout.  For obsidian format, defaults to ai_docs/changes-log.md."
    ;; CLI properties
    :long-option "output"
    :short-option "o"
    :option-type :string
    ;; Transient properties
    :transient-key "o"
    :transient-description "--output"
    :transient-class transient-option
    :transient-argument "--output="
    :transient-prompt "Output file: "
    :transient-reader transient-read-file
    :transient-group "Export Options"
    :transient-level 1
    :transient-order 2)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force export even if database is empty (--force)."
    ;; CLI properties
    :long-option "force"
    :option-type :boolean
    ;; Transient properties
    :transient-key "F"
    :transient-description "--force"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Export Options"
    :transient-level 2
    :transient-order 1)

   ;; === Filters ===
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status (-s, --status)."
    ;; CLI properties
    :long-option "status"
    :short-option "s"
    :option-type :string
    ;; Transient properties
    :transient-key "s"
    :transient-description "--status"
    :transient-class transient-option
    :transient-argument "--status="
    :transient-prompt "Status: "
    :transient-reader beads-reader-list-status
    :transient-group "Filters"
    :transient-level 1
    :transient-order 1)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Filter by type (-t, --type).
Values: bug, feature, task, epic, chore, merge-request, molecule, gate.
Aliases: mr→merge-request, feat→feature, mol→molecule."
    ;; CLI properties
    :long-option "type"
    :short-option "t"
    :option-type :string
    ;; Transient properties
    :transient-key "t"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Type: "
    :transient-reader beads-reader-issue-type
    :transient-group "Filters"
    :transient-level 1
    :transient-order 2)
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee (-a, --assignee)."
    ;; CLI properties
    :long-option "assignee"
    :short-option "a"
    :option-type :string
    ;; Transient properties
    :transient-key "a"
    :transient-description "--assignee"
    :transient-class transient-option
    :transient-argument "--assignee="
    :transient-prompt "Assignee: "
    :transient-group "Filters"
    :transient-level 1
    :transient-order 3)
   (priority
    :initarg :priority
    :type (or null string)
    :initform nil
    :documentation "Filter by priority (-p, --priority).
Values: 0-4 or P0-P4 (0=highest)."
    ;; CLI properties
    :long-option "priority"
    :short-option "p"
    :option-type :string
    ;; Transient properties
    :transient-key "p"
    :transient-description "--priority"
    :transient-class transient-option
    :transient-argument "--priority="
    :transient-prompt "Priority (0-4 or P0-P4): "
    :transient-reader beads-reader-priority
    :transient-group "Filters"
    :transient-level 1
    :transient-order 4)
   (label
    :initarg :label
    :type (or null list)
    :initform nil
    :documentation "Filter by labels - AND logic (-l, --label).
Must have ALL specified labels."
    ;; CLI properties
    :long-option "label"
    :short-option "l"
    :option-type :list
    ;; Transient properties
    :transient-key "l"
    :transient-description "--label"
    :transient-class transient-option
    :transient-argument "--label="
    :transient-prompt "Labels (AND): "
    :transient-reader beads-reader-issue-labels
    :transient-group "Filters"
    :transient-level 1
    :transient-order 5)
   (label-any
    :initarg :label-any
    :type (or null list)
    :initform nil
    :documentation "Filter by labels - OR logic (--label-any).
Must have AT LEAST ONE of specified labels."
    ;; CLI properties
    :long-option "label-any"
    :option-type :list
    ;; Transient properties
    :transient-key "L"
    :transient-description "--label-any"
    :transient-class transient-option
    :transient-argument "--label-any="
    :transient-prompt "Labels (OR): "
    :transient-reader beads-reader-issue-labels
    :transient-group "Filters"
    :transient-level 2
    :transient-order 1)
   (priority-min
    :initarg :priority-min
    :type (or null string)
    :initform nil
    :documentation "Filter by minimum priority (--priority-min).
Inclusive, 0-4 or P0-P4."
    ;; CLI properties
    :long-option "priority-min"
    :option-type :string
    ;; Transient properties
    :transient-key "pm"
    :transient-description "--priority-min"
    :transient-class transient-option
    :transient-argument "--priority-min="
    :transient-prompt "Min priority (0-4 or P0-P4): "
    :transient-reader beads-reader-priority
    :transient-group "Filters"
    :transient-level 2
    :transient-order 2)
   (priority-max
    :initarg :priority-max
    :type (or null string)
    :initform nil
    :documentation "Filter by maximum priority (--priority-max).
Inclusive, 0-4 or P0-P4."
    ;; CLI properties
    :long-option "priority-max"
    :option-type :string
    ;; Transient properties
    :transient-key "px"
    :transient-description "--priority-max"
    :transient-class transient-option
    :transient-argument "--priority-max="
    :transient-prompt "Max priority (0-4 or P0-P4): "
    :transient-reader beads-reader-priority
    :transient-group "Filters"
    :transient-level 2
    :transient-order 3)

   ;; === Date Filters ===
   (created-after
    :initarg :created-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues created after date (--created-after).
Format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "created-after"
    :option-type :string
    ;; Transient properties
    :transient-key "ca"
    :transient-description "--created-after"
    :transient-class transient-option
    :transient-argument "--created-after="
    :transient-prompt "Created after (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 2
    :transient-order 1)
   (created-before
    :initarg :created-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues created before date (--created-before).
Format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "created-before"
    :option-type :string
    ;; Transient properties
    :transient-key "cb"
    :transient-description "--created-before"
    :transient-class transient-option
    :transient-argument "--created-before="
    :transient-prompt "Created before (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 2
    :transient-order 2)
   (updated-after
    :initarg :updated-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated after date (--updated-after).
Format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "updated-after"
    :option-type :string
    ;; Transient properties
    :transient-key "ua"
    :transient-description "--updated-after"
    :transient-class transient-option
    :transient-argument "--updated-after="
    :transient-prompt "Updated after (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 2
    :transient-order 3)
   (updated-before
    :initarg :updated-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated before date (--updated-before).
Format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "updated-before"
    :option-type :string
    ;; Transient properties
    :transient-key "ub"
    :transient-description "--updated-before"
    :transient-class transient-option
    :transient-argument "--updated-before="
    :transient-prompt "Updated before (YYYY-MM-DD): "
    :transient-group "Date Filters"
    :transient-level 2
    :transient-order 4))
  :documentation "Represents bd export command.
Exports issues to JSON Lines or Obsidian Tasks markdown format.
When executed with :json t, returns export statistics."))

(cl-defmethod beads-command-subcommand ((_command beads-command-export))
  "Return \"export\" as the CLI subcommand name."
  "export")

(cl-defmethod beads-command-validate ((_command beads-command-export))
  "Validate export COMMAND.
Export command has no required fields.
Returns nil (always valid)."
  nil)

(cl-defmethod beads-command-execute-interactive ((_cmd beads-command-export))
  "Execute CMD in terminal buffer with human-readable output."
  ;; Call the default implementation
  (cl-call-next-method))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-export-transient "beads-command-export" nil t)
(beads-meta-define-transient beads-command-export "beads-export-transient"
  "Export issues to JSON Lines or Obsidian Tasks format (auto-generated menu).

See `beads-export' for the full user-facing transient menu."
  beads-option-global-section)

;;; Interactive Export Workflow

(defun beads-export--get-default-output ()
  "Get default output path (.beads/issues.jsonl)."
  (when-let* ((beads-dir (beads--find-beads-dir)))
    (expand-file-name "issues.jsonl" beads-dir)))

(defun beads-export--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-export instance.
Returns a beads-command-export object populated with values from ARGS.

This uses transient's standard argument parsing with dash-style flags."
  ;; Note: transient 0.12.0 can return `t' instead of "" for empty option
  ;; values.  We use beads--sanitize-string to convert non-string values
  ;; to nil.
  (let* ((format (beads--sanitize-string
                  (transient-arg-value "--format=" args)))
         (output (beads--sanitize-string
                  (transient-arg-value "--output=" args)))
         (force (transient-arg-value "--force" args))
         (status (beads--sanitize-string
                  (transient-arg-value "--status=" args)))
         (type (beads--sanitize-string
                (transient-arg-value "--type=" args)))
         (assignee (beads--sanitize-string
                    (transient-arg-value "--assignee=" args)))
         (priority (beads--sanitize-string
                    (transient-arg-value "--priority=" args)))
         (label (beads--sanitize-string
                 (transient-arg-value "--label=" args))))
    (beads-command-export
     :format format
     :output output
     :force force
     :status status
     :issue-type type
     :assignee assignee
     :priority priority
     :label (when label (split-string label ",")))))

(defun beads-export--validate-output (output)
  "Validate that OUTPUT path is set.
Returns error message string if invalid, nil if valid."
  (when (beads--string-blank-p output)
    "Output file path is required"))

(defun beads-export--validate-format (format)
  "Validate that FORMAT is a valid export format.
Returns error message string if invalid, nil if valid."
  (when (and format
             (not (member format '("jsonl" "obsidian"))))
    (format "Invalid format '%s' (must be jsonl or obsidian)" format)))

(defun beads-export--validate-status (status)
  "Validate that STATUS is a valid status.
Returns error message string if invalid, nil if valid."
  (when (and status
             (not (member status '("open" "in_progress" "blocked"
                                   "deferred" "closed"))))
    (format "Invalid status '%s'" status)))

(defun beads-export--validate-all (cmd)
  "Validate all parameters in CMD.
Returns list of error messages, or nil if all valid."
  (with-slots (output format status) cmd
    (delq nil (list (beads-export--validate-output output)
                    (beads-export--validate-format format)
                    (beads-export--validate-status status)))))

(defun beads-export--execute (cmd)
  "Execute export command CMD.
CMD should be a beads-command-export instance.
Validates the command and executes it.
Handles errors gracefully by calling `beads--error'."
  (let ((errors (beads-export--validate-all cmd)))
    (if errors
        (beads--error "Export validation failed: %s"
                      (string-join errors ", "))
      (condition-case err
          (beads-command-execute cmd)
        (error
         (beads--error "Export failed: %s" (error-message-string err)))))))

;;; Suffix Commands

(transient-define-suffix beads-export--execute-command ()
  "Execute the bd export command."
  :key "x"
  :description "Export"
  (interactive)
  (let* ((args (transient-args 'beads-export))
         (cmd (beads-export--parse-transient-args args)))
    ;; Set default output if not specified
    (unless (oref cmd output)
      (oset cmd output (beads-export--get-default-output)))
    ;; Execute using beads-export--execute (validates and executes)
    (beads-export--execute cmd)
    ;; After execution, show result
    (let* ((stdout (oref cmd stdout))
           (output (oref cmd output)))
      (if output
          (message "Exported to: %s" output)
        ;; Show output in buffer
        (let ((buf (get-buffer-create
                    (beads-buffer-name-utility "export"))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (or stdout ""))
              (goto-char (point-min))
              (special-mode)
              (local-set-key (kbd "q") 'quit-window)))
          (display-buffer buf)
          (message "Exported (see buffer)"))))))

(transient-define-suffix beads-export--preview ()
  "Preview the bd export command without executing."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-export))
         (cmd (beads-export--parse-transient-args args))
         (cmd-line (beads-command-line cmd))
         (full-cmd (cons beads-executable cmd-line)))
    (message "Command: %s" (string-join full-cmd " "))))

(transient-define-suffix beads-export--reset ()
  "Reset all export parameters."
  :key "R"
  :description "Reset all fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-export "beads-command-export" nil t)
(transient-define-prefix beads-export ()
  "Transient menu for exporting issues from Beads."
  :value (lambda () nil)
  ["Export Parameters"
   (beads-option-export-format)
   (beads-option-export-output)]
  ["Filters"
   (beads-option-export-status)
   (beads-option-export-issue-type)
   (beads-option-export-assignee)
   (beads-option-export-priority)
   (beads-option-export-label)]
  ["Actions"
   (beads-export--execute-command)
   (beads-export--preview)
   (beads-export--reset)])

(provide 'beads-command-export)
;;; beads-command-export.el ends here
