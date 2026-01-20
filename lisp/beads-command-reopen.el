;;; beads-command-reopen.el --- Reopen command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-reopen' EIEIO class for the
;; `bd reopen' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd reopen command reopens closed issues by setting their status
;; to 'open' and clearing the closed_at timestamp.  This is more explicit
;; than 'bd update --status open' and emits a Reopened event.
;;
;; Features:
;; - Reopen single or multiple issues at once
;; - Optional reason field for documentation
;; - Invalidates completion cache after reopening
;;
;; Usage:
;;   (beads-command-execute (beads-command-reopen :issue-ids '("bd-1")
;;                                                 :reason "Needs more work"))
;;   (beads-command-reopen!)  ; convenience function

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'transient)

;; Forward declarations
(declare-function beads--invalidate-completion-cache "beads")
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-refresh-show "beads-show")
(declare-function beads-buffer-parse-show "beads-buffer")
(declare-function beads-completion-read-issue "beads-completion")
(declare-function beads-check-executable "beads")
(declare-function beads--sanitize-string "beads")
(declare-function beads--string-blank-p "beads")
(defvar beads-show--issue-id)
(defvar beads-auto-refresh)
(defvar beads-executable)

;;; Reopen Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-reopen (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs to reopen (positional arguments).
Example: '(\"bd-1\" \"bd-2\")"
    ;; CLI properties
    :positional 1
    :option-type :list
    :option-separator " "
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue IDs (required)"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue ID(s): "
    :transient-reader beads-reader-reopen-issue-id
    :transient-group "Reopen Issue"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)
   (reason
    :initarg :reason
    :type (or null string)
    :initform nil
    :documentation "Reason for reopening (-r, --reason).
Optional, but recommended for documentation."
    ;; CLI properties
    :long-option "reason"
    :short-option "r"
    :option-type :string
    ;; Transient properties
    :transient-key "r"
    :transient-description "--reason"
    :transient-class transient-option
    :transient-argument "--reason="
    :transient-prompt "Reason: "
    :transient-group "Reopen Issue"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd reopen command.
Reopens one or more closed issues with optional reason.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-reopen))
  "Return \"reopen\" as the CLI subcommand name."
  "reopen")

(cl-defmethod beads-command-validate ((command beads-command-reopen))
  "Validate reopen COMMAND.
Checks that at least one issue ID is provided.
Returns error string or nil if valid."
  (with-slots (issue-ids) command
    (or
     ;; Must have at least one issue ID
     (and (or (null issue-ids) (zerop (length issue-ids)))
          "Must provide at least one issue ID")
     ;; Validate list content types
     (beads-command--validate-string-list issue-ids "issue-ids"))))

(cl-defmethod beads-command-parse ((command beads-command-reopen) execution)
  "Parse reopen COMMAND output from EXECUTION.
Returns reopened issue(s).
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, returns beads-issue instance (or list when multiple IDs).
Does not modify any slots."
  (with-slots (json issue-ids) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON, then convert to beads-issue instance(s)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (if (eq (type-of parsed-json) 'vector)
                ;; bd reopen returns array - convert to issue objects
                (let ((issues (mapcar #'beads-issue-from-json
                                      (append parsed-json nil))))
                  ;; Return single issue if only one ID, list otherwise
                  (if (= (length issue-ids) 1)
                      (car issues)
                    issues))
              ;; Unexpected JSON structure
              (signal 'beads-json-parse-error
                      (list "Unexpected JSON structure from bd reopen"
                            :exit-code (oref execution exit-code)
                            :parsed-json parsed-json
                            :stderr (oref execution stderr))))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instance: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :parsed-json parsed-json
                         :stderr (oref execution stderr)
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-reopen))
  "Execute CMD to reopen issue and show result.
Overrides default `compilation-mode' behavior."
  (let* ((result (oref (beads-command-execute cmd) result))
         (issues (cond
                  ((null result) nil)
                  ((cl-typep result 'beads-issue) (list result))
                  ((and (listp result)
                        (not (null result))
                        (cl-typep (car result) 'beads-issue))
                   result)
                  (t nil))))
    ;; Invalidate completion cache after reopening
    (beads--invalidate-completion-cache)
    (if issues
        (message "Reopened %d issue%s: %s"
                 (length issues)
                 (if (= (length issues) 1) "" "s")
                 (mapconcat (lambda (i) (oref i id)) issues ", "))
      (message "No issues reopened"))))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-reopen-transient "beads-command-reopen" nil t)
(beads-meta-define-transient beads-command-reopen "beads-reopen-transient"
  "Reopen one or more closed issues.

Reopening an issue sets its status to 'open' and clears the
closed_at timestamp.  This emits a Reopened event for audit trail.

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Issue IDs, reason"
  beads-option-global-section)

;;; Interactive Reopen Workflow

(defun beads-reopen--detect-issue-id ()
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

(defun beads-reopen--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-reopen instance.
Returns a beads-command-reopen object populated with values from ARGS.

This uses transient's standard argument parsing with dash-style flags."
  ;; Note: transient 0.12.0 can return `t' instead of "" for empty option
  ;; values.  We use beads--sanitize-string to convert non-string values
  ;; to nil.
  (let* ((issue-id (beads--sanitize-string
                    (transient-arg-value "--id=" args)))
         (reason (beads--sanitize-string
                  (transient-arg-value "--reason=" args))))
    (beads-command-reopen
     :issue-ids (when issue-id (list issue-id))
     :reason reason)))

(defun beads-reopen--validate-issue-id (issue-id)
  "Validate that ISSUE-ID is set.
Returns error message string if invalid, nil if valid."
  (when (beads--string-blank-p issue-id)
    "Issue ID is required"))

(defun beads-reopen--validate-all (cmd)
  "Validate all parameters from CMD beads-command-reopen instance.
Returns list of error messages, or nil if all valid."
  (delq nil
        (list (beads-reopen--validate-issue-id
               (when (oref cmd issue-ids)
                 (car (oref cmd issue-ids)))))))

;;; Suffix Commands

(transient-define-suffix beads-reopen--execute ()
  "Execute the bd reopen command with current parameters."
  :key "x"
  :description "Reopen issue"
  (interactive)
  (let* ((args (transient-args 'beads-reopen--menu))
         (cmd (beads-reopen--parse-transient-args args))
         (errors (beads-reopen--validate-all cmd)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (let* ((exec (beads-command-execute cmd))
                 (issue (oref exec result)))
            (message "Reopened issue: %s - %s"
                     (oref issue id)
                     (oref issue title))
            ;; Invalidate completion cache
            (beads--invalidate-completion-cache)
            ;; Refresh any open beads buffers
            (when beads-auto-refresh
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (cond
                   ((and (derived-mode-p 'beads-list-mode)
                         (bound-and-true-p beads-list--command))
                    (beads-list-refresh t))
                   ((and (derived-mode-p 'beads-show-mode)
                         (string= beads-show--issue-id (oref issue id)))
                    (beads-refresh-show)))))))
        (error
         (let ((err-msg (format "Failed to reopen issue: %s"
                                (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-reopen--reset ()
  "Reset all parameters to their default values."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    ;; Clear transient's argument state using transient-reset
    (transient-reset)
    ;; Refresh the transient display to show cleared state
    (transient--redisplay)
    (message "All fields reset")))

(transient-define-suffix beads-reopen--preview ()
  "Preview the bd reopen command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-reopen--menu))
         (cmd (beads-reopen--parse-transient-args args))
         (errors (beads-reopen--validate-all cmd)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                              (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (let* ((cmd-list (beads-command-line cmd))
             (cmd-string (mapconcat #'shell-quote-argument cmd-list " "))
             (preview-msg (format "Command: %s" cmd-string)))
        (message "%s" preview-msg)
        preview-msg))))

;;; Main Transient Menu

(transient-define-prefix beads-reopen--menu ()
  "Transient menu for reopening an issue in Beads."
  ["Reopen Issue"
   (beads-option-reopen-issue-id)
   (beads-option-reopen-reason)]
  ["Actions"
   (beads-reopen--execute)
   (beads-reopen--preview)
   (beads-reopen--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-reopen (&optional issue-id)
  "Reopen a closed issue in Beads.

This function provides an interactive interface for reopening closed
issues via a transient menu.  The function is context-aware and
automatically detects the issue ID from beads-list or beads-show
buffers.

If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user."
  (interactive
   (list (or (beads-reopen--detect-issue-id)
            (beads-completion-read-issue
             "Reopen issue: " nil t nil 'beads--issue-id-history))))
  ;; Check executable
  (beads-check-executable)
  ;; Show the transient menu with initial issue ID if provided
  (if issue-id
      ;; Set initial value using transient-args
      (transient-setup 'beads-reopen--menu nil nil
                       :value (list (concat "--id=" issue-id)))
    (transient-setup 'beads-reopen--menu)))

(provide 'beads-command-reopen)
;;; beads-command-reopen.el ends here
