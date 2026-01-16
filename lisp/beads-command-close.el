;;; beads-command-close.el --- Close command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-close' EIEIO class for the
;; `bd close' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd close command closes one or more issues with a required
;; reason.  Closing an issue marks it as complete and records the
;; reason for closure.
;;
;; Features:
;; - Close single or multiple issues at once
;; - Required reason field for documentation
;; - Invalidates completion cache after closing
;;
;; Usage:
;;   (beads-command-execute (beads-command-close :issue-ids '("bd-1")
;;                                               :reason "Done"))
;;   (beads-command-close!)  ; convenience function

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)

;;; Close Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-close (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs to close (positional arguments).
Example: '(\"bd-1\" \"bd-2\")"
    ;; CLI properties
    :positional 1
    :option-type :list
    :option-separator " "
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID (required)"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue ID: "
    :transient-reader beads-reader-close-issue-id
    :transient-group "Close Issue"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)
   (reason
    :initarg :reason
    :type (or null string)
    :initform nil
    :documentation "Reason for closing (-r, --reason).
Required field."
    ;; CLI properties
    :long-option "reason"
    :short-option "r"
    :option-type :string
    ;; Transient properties
    :transient-key "r"
    :transient-description "--reason"
    :transient-class beads-create-transient-multiline
    :transient-argument "--reason="
    :transient-field-name "Close Reason"
    :transient-group "Close Issue"
    :transient-level 1
    :transient-order 2
    ;; Validation
    :required t))
  :documentation "Represents bd close command.
Closes one or more issues with a required reason.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-close))
  "Return \"close\" as the CLI subcommand name."
  "close")

(cl-defmethod beads-command-validate ((command beads-command-close))
  "Validate close COMMAND.
Checks that at least one issue ID and a reason are provided.
Returns error string or nil if valid."
  (with-slots (issue-ids reason) command
    (or
     ;; Must have at least one issue ID
     (and (or (null issue-ids) (zerop (length issue-ids)))
          "Must provide at least one issue ID")
     ;; Must have a reason
     (and (or (null reason) (string-empty-p reason))
          "Must provide a reason for closing")
     ;; Validate list content types
     (beads-command--validate-string-list issue-ids "issue-ids"))))

(cl-defmethod beads-command-parse ((command beads-command-close))
  "Parse close COMMAND output and return closed issue(s).
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, returns beads-issue instance (or list when multiple IDs).
Does not modify command slots."
  (with-slots (json issue-ids) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON, then convert to beads-issue instance(s)
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (if (eq (type-of parsed-json) 'vector)
                ;; bd close returns array - convert to issue objects
                (let ((issues (mapcar #'beads-issue-from-json
                                      (append parsed-json nil))))
                  ;; Return single issue if only one ID, list otherwise
                  (if (= (length issue-ids) 1)
                      (car issues)
                    issues))
              ;; Unexpected JSON structure
              (signal 'beads-json-parse-error
                      (list "Unexpected JSON structure from bd close"
                            :exit-code (oref command exit-code)
                            :parsed-json parsed-json
                            :stderr (oref command stderr))))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instance: %s"
                                 (error-message-string err))
                         :exit-code (oref command exit-code)
                         :parsed-json parsed-json
                         :stderr (oref command stderr)
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-close))
  "Execute CMD to close issue and show result.
Overrides default `compilation-mode' behavior."
  (let* ((result (oref (beads-command-execute cmd) data))
         (issues (cond
                  ((null result) nil)
                  ((cl-typep result 'beads-issue) (list result))
                  ((and (listp result)
                        (not (null result))
                        (cl-typep (car result) 'beads-issue))
                   result)
                  (t nil))))
    ;; Invalidate completion cache after closing
    (beads--invalidate-completion-cache)
    (if issues
        (message "Closed %d issue%s: %s"
                 (length issues)
                 (if (= (length issues) 1) "" "s")
                 (mapconcat (lambda (i) (oref i id)) issues ", "))
      (message "No issues closed"))))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-close "beads-command-close" nil t)
(beads-meta-define-transient beads-command-close "beads-close"
  "Close an issue with a reason.

Closes one or more issues and records the reason for closure.
Both issue ID and reason are required fields.

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Issue ID and reason fields"
  beads-option-global-section)

(provide 'beads-command-close)
;;; beads-command-close.el ends here
