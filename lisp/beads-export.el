;;; beads-export.el --- Export issues to JSONL -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides interface for exporting Beads issues to JSONL format.
;;
;; The bd export command exports all issues to JSON Lines format
;; (one JSON object per line).  Issues are sorted by ID for consistent
;; diffs.
;;
;; Usage:
;;   M-x beads-export

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-option)
(require 'transient)

;;; Utility Functions

(defun beads-export--get-default-output ()
  "Get default output path (.beads/issues.jsonl)."
  (when-let* ((beads-dir (beads--find-beads-dir)))
    (expand-file-name "issues.jsonl" beads-dir)))

(defun beads-export--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-export instance.
Returns a beads-command-export object populated with values from ARGS.

This uses transient's standard argument parsing with dash-style flags."
  (let* ((output (transient-arg-value "--output=" args))
         (format (transient-arg-value "--format=" args))
         (status (transient-arg-value "--status=" args))
         (force (transient-arg-value "--force" args))
         (no-auto-flush (transient-arg-value "--no-auto-flush" args)))
    (beads-command-export
     :output output
     :format format
     :status status
     :force force
     :no-auto-flush no-auto-flush)))

(defun beads-export--validate-output (output)
  "Validate that OUTPUT path is set.
Returns error message string if invalid, nil if valid."
  (when (beads--string-blank-p output)
    "Output file path is required"))

(defun beads-export--validate-all (command)
  "Validate all parameters in COMMAND.
Returns list of error messages, or nil if all valid."
  (with-slots (output) command
    (delq nil (list (beads-export--validate-output output)
                    (beads-command-validate command)))))

;;; Suffix Commands

(defun beads-export--execute (command)
  "Execute bd export using COMMAND object."
  (condition-case err
      (progn
        ;; Execute the export command (returns parsed JSON stats when :json t)
        (beads-command-execute command)
        (with-slots (output) command
          (message "Exported to: %s" output))
        nil)
    (error
     (beads--error "Failed to export: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-export--execute-command ()
  "Execute the bd export command."
  :key "x"
  :description "Export"
  (interactive)
  (let* ((args (transient-args 'beads-export))
         (command (beads-export--parse-transient-args args)))
    ;; Set default output if not provided
    (when (not (oref command output))
      (oset command output (beads-export--get-default-output)))
    ;; Validate
    (let ((errors (beads-export--validate-all command)))
      (if errors
          (user-error "Validation failed: %s" (string-join errors "; "))
        (beads-export--execute command)))))

(transient-define-suffix beads-export--preview ()
  "Preview the bd export command without executing."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-export))
         (command (beads-export--parse-transient-args args)))
    ;; Set default output if not provided
    (when (not (oref command output))
      (oset command output (beads-export--get-default-output)))
    (let ((cmd-line (beads-command-line command)))
      (message "Command: %s" (string-join cmd-line " ")))))

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

;;;###autoload
(transient-define-prefix beads-export ()
  "Transient menu for exporting issues to JSONL."
  :value (lambda () nil)
  ["Export Parameters"
   (beads-option-export-output)
   (beads-option-export-format)
   (beads-option-export-status)]
  ["Options"
   (beads-option-export-force)
   (beads-option-export-no-auto-flush)]
  ["Actions"
   (beads-export--execute-command)
   (beads-export--preview)
   (beads-export--reset)])

;;; Footer

(provide 'beads-export)
;;; beads-export.el ends here
