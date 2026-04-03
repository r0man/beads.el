;;; beads-command-close.el --- Close command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-close' EIEIO class for the
;; `bd close' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-defcommand'.
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
;;   (beads-execute 'beads-command-close)  ; convenience function
;;   (beads-close)           ; invoke transient menu

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)

;;; Close Command

;;;###autoload (autoload 'beads-close "beads-command-close" nil t)
(beads-defcommand beads-command-close (beads-command-global-options)
  ((issue-ids
    :positional 1
    :option-type :list
    :option-separator " "
    :key "i"
    :transient transient-option
    :argument "--id="
    :prompt "Issue ID: "
    :reader beads--read-issue-at-point-or-prompt
    :group "Close Issue"
    :level 1
    :order 1
    :required t)
   (reason
    :option-type :string
    :short-option "r"
    :key "r"
    :transient beads-transient-multiline
    :field-name "Close Reason"
    :group "Close Issue"
    :level 1
    :order 2
    :required t))
  :documentation "Close one or more issues with a required reason.
  When executed with :json t, returns beads-issue instance (or list
  of instances when multiple IDs provided)."
  :result (list-of beads-issue))


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

(cl-defmethod beads-command-parse ((command beads-command-close) stdout)
  "Parse close COMMAND output from STDOUT.
Returns closed issue(s).
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
                            :stdout stdout
                            :parsed-json parsed-json)))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instance: %s"
                                 (error-message-string err))
                         :stdout stdout
                         :parsed-json parsed-json
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-close))
  "Execute CMD to close issue and show result.
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
    ;; Invalidate completion cache after closing
    (beads--invalidate-completion-cache)
    (if issues
        (message "Closed %d issue%s: %s"
                 (length issues)
                 (if (= (length issues) 1) "" "s")
                 (mapconcat (lambda (i) (oref i id)) issues ", "))
      (message "No issues closed"))))

;; Note: Transient menu `beads-close' is auto-generated by
;; `beads-defcommand' above (default :transient t behavior).
;; The transient docstring is extracted from the first sentence
;; of :documentation.

(provide 'beads-command-close)
;;; beads-command-close.el ends here
