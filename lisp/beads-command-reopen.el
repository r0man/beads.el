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
;;   (beads-execute 'beads-command-reopen)  ; convenience function

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'transient)

;; Forward declarations
(declare-function beads--invalidate-completion-cache "beads")
(declare-function beads-reader-reopen-issue-id "beads-reader")
(defvar beads-auto-refresh)

;;; Reopen Command

(beads-defcommand beads-command-reopen (beads-command-global-options)
  ((issue-ids
    :positional 1
    :option-type :list
    :separator " "
    :short-option "i"
    :transient transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads-reader-reopen-issue-id
    :group "Reopen Issue"
    :level 1
    :order 1
    :required t)
   (reason
    :option-type :string
    :short-option "r"
    :transient beads-transient-multiline
    :documentation "Reopen Reason"
    :group "Reopen Issue"
    :level 1
    :order 2))
  :documentation "Represents bd reopen command.
Reopens one or more closed issues with optional reason.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided).")


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

(cl-defmethod beads-command-parse ((command beads-command-reopen) stdout)
  "Parse reopen COMMAND output from STDOUT.
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
                (let ((issues (mapcar (lambda (j) (beads-from-json 'beads-issue j))
                                      (append parsed-json nil))))
                  ;; Return single issue if only one ID, list otherwise
                  (if (= (length issue-ids) 1)
                      (car issues)
                    issues))
              ;; Unexpected JSON structure
              (signal 'beads-json-parse-error
                      (list "Unexpected JSON structure from bd reopen"
                            :stdout stdout
                            :parsed-json parsed-json)))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instance: %s"
                                 (error-message-string err))
                         :stdout stdout
                         :parsed-json parsed-json
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-reopen))
  "Execute CMD to reopen issue and show result.
Overrides default `compilation-mode' behavior."
  (oset cmd json t)
  (let* ((result (beads-command-execute cmd))
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
;;;###autoload (autoload 'beads-reopen "beads-command-reopen" nil t)
(beads-meta-define-transient beads-command-reopen "beads-reopen"
  "Reopen one or more closed issues.

Reopening an issue sets its status to 'open' and clears the
closed_at timestamp.  This emits a Reopened event for audit trail.

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Issue ID, reason"
  beads-option-global-section)

(provide 'beads-command-reopen)
;;; beads-command-reopen.el ends here
