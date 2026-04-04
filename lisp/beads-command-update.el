;;; beads-command-update.el --- Update command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-update' EIEIO class for the
;; `bd update' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd update command updates one or more issues with new field
;; values.  It supports modifying status, priority, title, description,
;; labels, and many other fields.
;;
;; Features:
;; - Update single or multiple issues at once
;; - Status transitions
;; - Label management (add, remove, set)
;; - Due dates and deferral
;; - Atomic claim operation
;; - Time estimates
;;
;; Usage:
;;   (beads-command-execute (beads-command-update :issue-ids '("bd-1")
;;                                                 :status "closed"))
;;   (beads-execute 'beads-command-update)  ; convenience function

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)
(require 'transient)

;; Forward declaration
(declare-function beads--invalidate-completion-cache "beads")

;;; Update Command

(beads-defcommand beads-command-update (beads-command-global-options)
  ((issue-ids
    :positional 1
    :type (list-of string)
    :separator " "
    :short-option "i"
    :argument "--id="
    :prompt "Issue ID(s): "
    :reader beads--read-issue-at-point-or-prompt
    :group "Issue"
    :level 1
    :order 1)

   ;; Status & Priority
   (status
    :short-option "s"
    :type (or null string)
    :prompt "Status: "
    :choices ("open" "in_progress" "blocked" "closed")
    :reader beads-reader-update-status
    :group "Status & Priority"
    :level 1
    :order 1)
   (priority
    :type (or null string integer)
    :short-option "p"
    :type (or null string)
    :prompt "Priority: "
    :reader beads-reader-issue-priority
    :group "Status & Priority"
    :level 1
    :order 2)
   (claim
    :type boolean
    :short-option "c"
    :group "Status & Priority"
    :level 2
    :order 3)

   ;; Basic Info
   (title
    :type (or null string)
    :short-option "t"
    :prompt "Issue title: "
    :reader beads-reader-issue-title
    :group "Basic Info"
    :level 2
    :order 1)
   (issue-type
    :long-option "type"
    :short-option "t"
    :type (or null string)
    :transient-key "y"
    :prompt "Type: "
    :choices ("bug" "feature" "task" "epic" "chore"
                        "merge-request" "molecule" "gate" "agent"
                        "role" "rig" "convoy" "event" "slot")
    :reader beads-reader-issue-type
    :group "Basic Info"
    :level 2
    :order 2)
   (assignee
    :short-option "a"
    :type (or null string)
    :prompt "Assignee: "
    :reader beads-reader-issue-assignee
    :group "Basic Info"
    :level 2
    :order 3)
   (external-ref
    :type (or null string)
    :short-option "x"
    :prompt "External reference: "
    :reader beads-reader-issue-external-ref
    :group "Basic Info"
    :level 3
    :order 4)
   (parent
    :type (or null string)
    :short-option "P"
    :prompt "Parent issue ID: "
    :reader beads-reader-issue-id
    :group "Basic Info"
    :level 3
    :order 5)

   ;; Content
   (description
    :short-option "d"
    :type (or null string)
    :transient beads-transient-multiline
    :documentation "Description"
    :group "Content"
    :level 3
    :order 1)
   (acceptance
    :type (or null string)
    :short-option "A"
    :transient beads-transient-multiline
    :documentation "Acceptance Criteria"
    :group "Content"
    :level 3
    :order 2)
   (design
    :type (or null string)
    :short-option "G"
    :transient beads-transient-multiline
    :documentation "Design"
    :group "Content"
    :level 3
    :order 3)
   (notes
    :type (or null string)
    :short-option "N"
    :transient beads-transient-multiline
    :documentation "Notes"
    :group "Content"
    :level 3
    :order 4)
   (body-file
    :type (or null string)
    :short-option "B"
    :prompt "Body file: "
    :reader transient-read-file
    :group "Content"
    :level 4
    :order 5)

   ;; Labels
   (add-label
    :type (list-of string)
    :separator nil
    :short-option "l"
    :prompt "Add label: "
    :reader beads-reader-issue-labels
    :group "Labels"
    :level 2
    :order 1)
   (remove-label
    :type (list-of string)
    :separator nil
    :short-option "L"
    :prompt "Remove label: "
    :reader beads-reader-issue-labels
    :group "Labels"
    :level 2
    :order 2)
   (set-labels
    :type (list-of string)
    :separator nil
    :short-option "S"
    :prompt "Set labels: "
    :reader beads-reader-issue-labels
    :group "Labels"
    :level 3
    :order 3)

   ;; Time Management
   (estimate
    :short-option "e"
    :type (or null string integer)
    :prompt "Estimate (minutes): "
    :group "Time"
    :level 3
    :order 1)
   (due
    :type (or null string)
    :short-option "D"
    :prompt "Due date: "
    :group "Time"
    :level 3
    :order 2)
   (defer
    :type (or null string)
    :short-option "E"
    :prompt "Defer until: "
    :group "Time"
    :level 3
    :order 3)

   ;; Advanced
   (await-id
    :type (or null string)
    :short-option "W"
    :prompt "Await ID: "
    :group "Advanced"
    :level 4
    :order 1)
   (session
    :type (or null string)
    :short-option "I"
    :prompt "Session ID: "
    :group "Advanced"
    :level 4
    :order 2)

   ;; Additional flags from CLI
   (allow-empty-description
    :type boolean
    :long-option "allow-empty-description"
    :group "Flags"
    :level 4
    :order 1)
   (append-notes
    :type (or null string)
    :long-option "append-notes"
    :transient beads-transient-multiline
    :documentation "Append Notes"
    :group "Content"
    :level 3
    :order 5)
   (design-file
    :type (or null string)
    :long-option "design-file"
    :prompt "Design file: "
    :reader transient-read-file
    :group "Content"
    :level 4
    :order 6)
   (ephemeral
    :type boolean
    :long-option "ephemeral"
    :group "Flags"
    :level 4
    :order 2)
   (history
    :type boolean
    :long-option "history"
    :group "Flags"
    :level 4
    :order 3)
   (metadata
    :type (or null string)
    :long-option "metadata"
    :prompt "Metadata (JSON or @file.json): "
    :group "Advanced"
    :level 4
    :order 3)
   (no-history
    :type boolean
    :long-option "no-history"
    :group "Flags"
    :level 4
    :order 4)
   (persistent
    :type boolean
    :long-option "persistent"
    :group "Flags"
    :level 4
    :order 5)
   (set-metadata
    :type (list-of string)
    :separator nil
    :long-option "set-metadata"
    :prompt "Set metadata (key=value): "
    :group "Advanced"
    :level 4
    :order 4)
   (unset-metadata
    :type (list-of string)
    :separator nil
    :long-option "unset-metadata"
    :prompt "Unset metadata key: "
    :group "Advanced"
    :level 4
    :order 5)
   (spec-id
    :type (or null string)
    :long-option "spec-id"
    :prompt "Spec ID: "
    :group "Advanced"
    :level 4
    :order 6)
   (stdin
    :type boolean
    :long-option "stdin"
    :group "Content"
    :level 5
    :order 7))
  :documentation "Represents bd update command.
Updates one or more issues with new field values.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided)."
  :transient :manual)


(cl-defmethod beads-command-validate ((command beads-command-update))
  "Validate update COMMAND.
Checks that at least one issue ID and one field to update is provided.
Returns error string or nil if valid."
  (with-slots (issue-ids acceptance assignee description design
                         external-ref notes priority status title
                         issue-type parent add-label remove-label
                         set-labels estimate due defer await-id
                         session body-file claim) command
    (or
     ;; Must have at least one issue ID
     (and (or (null issue-ids) (zerop (length issue-ids)))
          "Must provide at least one issue ID")
     ;; Must have at least one field to update
     (and (not (or acceptance assignee description design external-ref
                   notes priority status title issue-type parent
                   add-label remove-label set-labels estimate due
                   defer await-id session body-file claim))
          "Must provide at least one field to update")
     ;; Validate list content types
     (beads-command--validate-string-list issue-ids "issue-ids")
     (beads-command--validate-string-list add-label "add-label")
     (beads-command--validate-string-list remove-label "remove-label")
     (beads-command--validate-string-list set-labels "set-labels"))))

(cl-defmethod beads-command-parse ((command beads-command-update) stdout)
  "Parse update COMMAND output from STDOUT.
Returns updated issue(s).
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
            (if (vectorp parsed-json)
                ;; bd update returns array - convert to issue objects
                (let ((issues (mapcar (lambda (j) (beads-from-json 'beads-issue j))
                                      (append parsed-json nil))))
                  ;; Return single issue if only one ID, list otherwise
                  (if (and issue-ids (= (length issue-ids) 1))
                      (car issues)
                    issues))
              ;; Unexpected JSON structure
              (signal 'beads-json-parse-error
                      (list "Unexpected JSON structure from bd update"
                            :stdout stdout
                            :parsed-json parsed-json)))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instance: %s"
                                 (error-message-string err))
                         :stdout stdout
                         :parsed-json parsed-json
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-update))
  "Execute CMD to update issue and show result.
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
    ;; Invalidate completion cache after updating
    (beads--invalidate-completion-cache)
    (if issues
        (message "Updated %d issue%s: %s"
                 (length issues)
                 (if (= (length issues) 1) "" "s")
                 (mapconcat (lambda (i) (oref i id)) issues ", "))
      (message "No issues updated"))))

;;; Transient Menu - State Management

(defvar-local beads-update--issue-id nil
  "Issue ID being updated (detected from context).
Buffer-local to prevent state from leaking between buffers when
update transient is invoked from different beads buffers.")

(defvar-local beads-update--original-data nil
  "Original issue data alist (for showing current values in menu).
Buffer-local to prevent state from leaking between buffers when
update transient is invoked from different beads buffers.")

;;; Transient Menu - Utility Functions

(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-buffer-parse-show "beads-buffer")
(declare-function beads-completion-read-issue "beads-completion")
(declare-function beads-check-executable "beads")
(declare-function beads-refresh-show "beads-show")
(defvar beads-show--issue-id)
(defvar beads-auto-refresh)

(defun beads-update--detect-issue-id ()
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

(defun beads-update--fetch-issue (issue-id)
  "Fetch issue data for ISSUE-ID from bd.
Returns parsed issue alist or signals error."
  (condition-case err
      (beads-execute 'beads-command-show :json t :issue-ids (list issue-id))
    (error
     (beads--error "Failed to fetch issue %s: %s"
                   issue-id
                   (error-message-string err)))))

(defun beads-update--load-issue-data (issue-id)
  "Load issue data for ISSUE-ID and populate state.
Sets beads-update--issue-id and beads-update--original-data."
  (setq beads-update--issue-id issue-id)
  (setq beads-update--original-data
        (beads-update--fetch-issue issue-id)))

(defun beads-update--get-original (field)
  "Get original value of FIELD from original-data.
FIELD is a symbol like `status', `priority', etc.
Original-data is a beads-issue instance."
  (when beads-update--original-data
    (condition-case nil
        (eieio-oref beads-update--original-data field)
      (invalid-slot-name nil))))

(defun beads-update--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-update instance.
Returns a beads-command-update command object with populated slots
from the parsed transient arguments."
  ;; Note: transient 0.12.0 can return `t' instead of "" for empty option
  ;; values.  We use beads--sanitize-string to convert non-string values
  ;; to nil.
  (let* ((status (beads--sanitize-string
                  (transient-arg-value "--status=" args)))
         (priority-str (beads--sanitize-string
                        (transient-arg-value "--priority=" args)))
         (priority (when priority-str (string-to-number priority-str)))
         (title (beads--sanitize-string
                 (transient-arg-value "--title=" args)))
         (issue-type (beads--sanitize-string
                      (transient-arg-value "--type=" args)))
         (description (beads--sanitize-string
                       (transient-arg-value "--description=" args)))
         (acceptance (beads--sanitize-string
                      (transient-arg-value "--acceptance=" args)))
         (design (beads--sanitize-string
                  (transient-arg-value "--design=" args)))
         (notes (beads--sanitize-string
                 (transient-arg-value "--notes=" args)))
         (assignee (beads--sanitize-string
                    (transient-arg-value "--assignee=" args)))
         (external-ref (beads--sanitize-string
                        (transient-arg-value "--external-ref=" args)))
         (parent (beads--sanitize-string
                  (transient-arg-value "--parent=" args)))
         (body-file (beads--sanitize-string
                     (transient-arg-value "--body-file=" args)))
         (add-label-str (beads--sanitize-string
                         (transient-arg-value "--add-label=" args)))
         (add-label (when add-label-str (list add-label-str)))
         (remove-label-str (beads--sanitize-string
                            (transient-arg-value "--remove-label=" args)))
         (remove-label (when remove-label-str (list remove-label-str)))
         (set-labels-str (beads--sanitize-string
                          (transient-arg-value "--set-labels=" args)))
         (set-labels (when set-labels-str (list set-labels-str)))
         (estimate-str (beads--sanitize-string
                        (transient-arg-value "--estimate=" args)))
         (estimate (when estimate-str (string-to-number estimate-str)))
         (due (beads--sanitize-string
               (transient-arg-value "--due=" args)))
         (defer (beads--sanitize-string
                 (transient-arg-value "--defer=" args)))
         (await-id (beads--sanitize-string
                    (transient-arg-value "--await-id=" args)))
         (session (beads--sanitize-string
                   (transient-arg-value "--session=" args)))
         (claim (member "--claim" args)))
    (beads-command-update
     :json t
     :issue-ids (when beads-update--issue-id
                  (list beads-update--issue-id))
     :status status
     :priority priority
     :title title
     :issue-type issue-type
     :description description
     :acceptance acceptance
     :design design
     :notes notes
     :assignee assignee
     :external-ref external-ref
     :parent parent
     :body-file body-file
     :add-label add-label
     :remove-label remove-label
     :set-labels set-labels
     :estimate estimate
     :due due
     :defer defer
     :await-id await-id
     :session session
     :claim (when claim t))))

(defun beads-update--get-changed-fields (cmd)
  "Return alist of fields that have been changed in CMD.
Only includes fields where current value differs from original."
  (let ((changes nil))
    (with-slots (status priority title description acceptance design
                        notes assignee external-ref) cmd
      (when status
        (unless (equal status (beads-update--get-original 'status))
          (push (cons 'status status) changes)))
      (when priority
        (unless (equal priority (beads-update--get-original 'priority))
          (push (cons 'priority priority) changes)))
      (when title
        (unless (equal title (beads-update--get-original 'title))
          (push (cons 'title title) changes)))
      (when description
        (unless (equal description
                       (beads-update--get-original 'description))
          (push (cons 'description description) changes)))
      (when acceptance
        (unless (equal acceptance
                       (beads-update--get-original 'acceptance-criteria))
          (push (cons 'acceptance acceptance) changes)))
      (when design
        (unless (equal design (beads-update--get-original 'design))
          (push (cons 'design design) changes)))
      (when notes
        (unless (equal notes (beads-update--get-original 'notes))
          (push (cons 'notes notes) changes)))
      (when assignee
        (unless (equal assignee (beads-update--get-original 'assignee))
          (push (cons 'assignee assignee) changes)))
      (when external-ref
        (unless (equal external-ref
                       (beads-update--get-original 'external-ref))
          (push (cons 'external-ref external-ref) changes)))
      (nreverse changes))))

(defun beads-update--validate-all (cmd)
  "Validate all parameters from CMD command instance.
Returns list of error messages, or nil if all valid.
Uses beads-command-validate for the authoritative check, which
covers all fields including claim, add-label, etc."
  (let ((error-msg (beads-command-validate cmd)))
    (delq nil (list error-msg))))

;;; Transient Menu - Suffix Commands

(transient-define-suffix beads-update--execute ()
  "Execute the bd update command with changed parameters."
  :key "x"
  :description "Update issue"
  (interactive)
  (let* ((args (transient-args 'beads-update--menu))
         (cmd (beads-update--parse-transient-args args))
         (errors (beads-update--validate-all cmd))
         (changes (beads-update--get-changed-fields cmd)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (condition-case err
          (progn
            (let ((_issue (beads-command-execute cmd)))
              (message "Updated issue: %s (changed %d field%s)"
                       beads-update--issue-id
                       (length changes)
                       (if (= (length changes) 1) "" "s"))
              ;; Invalidate completion cache
              (beads--invalidate-completion-cache)
              ;; Refresh any open beads buffers
              (when beads-auto-refresh
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    (cond
                     ;; Only refresh list buffers that have a command set
                     ((and (derived-mode-p 'beads-list-mode)
                           (bound-and-true-p beads-list--command))
                      (beads-list-refresh t))
                     ((and (derived-mode-p 'beads-show-mode)
                           (string= beads-show--issue-id
                                    beads-update--issue-id))
                      (beads-refresh-show))))))
              ;; Reset state
              (setq beads-update--issue-id nil
                    beads-update--original-data nil))
            nil)
        (error
         (let ((err-msg (format "Failed to update issue: %s"
                                (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

(transient-define-suffix beads-update--reset ()
  "Reset all changed parameters to their original values."
  :key "R"
  :description "Reset all changes"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all changes? ")
    ;; Clear transient's argument state using transient-reset
    (transient-reset)
    ;; Refresh the transient display to show cleared state
    (transient--redisplay)
    (message "All changes reset")))

(transient-define-suffix beads-update--preview ()
  "Preview the bd update command that will be executed."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-update--menu))
         (cmd (beads-update--parse-transient-args args))
         (errors (beads-update--validate-all cmd)))
    (if errors
        (let ((err-msg (format "Validation errors: %s"
                               (string-join errors "; "))))
          (message "%s" err-msg)
          err-msg)
      (condition-case err
          (let* ((cmd-line (beads-command-line cmd))
                 (cmd-string (mapconcat #'shell-quote-argument cmd-line " "))
                 (changes (beads-update--get-changed-fields cmd))
                 (preview-msg (format "Command: %s\nChanges: %s"
                                      cmd-string
                                      (mapconcat (lambda (c)
                                                   (format "%s=%s"
                                                           (car c)
                                                           (cdr c)))
                                                 changes ", "))))
            (message "%s" preview-msg)
            preview-msg)
        (error
         (let ((err-msg (format "Error: %s" (error-message-string err))))
           (message "%s" err-msg)
           err-msg))))))

;;; Transient Menu - Main Menu

(transient-define-prefix beads-update--menu ()
  "Transient menu for updating an issue in Beads."
  ["Issue Details"
   ["Status & Priority"
    (beads-option-update-status)
    (beads-option-issue-priority)]
   ["Basic Info"
    (beads-option-issue-title)
    (beads-option-issue-assignee)
    (beads-option-issue-external-ref)]]
  ["Content Fields"
   (beads-option-issue-description)
   (beads-option-issue-acceptance)
   (beads-option-issue-design)
   (beads-option-update-notes-multiline)]
  beads-option-global-section
  ["Actions"
   (beads-update--execute)
   (beads-update--preview)
   (beads-update--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-update (&optional issue-id)
  "Update an existing issue in Beads.

This function provides an interactive interface for updating all fields
of an existing issue via a transient menu.  The function is context-aware
and automatically detects the issue ID from beads-list or beads-show
buffers.

If ISSUE-ID is provided, use it directly.  Otherwise, detect from
context or prompt the user."
  (interactive
   (list (or (beads-update--detect-issue-id)
             (beads-completion-read-issue
              "Update issue: " nil t nil 'beads--issue-id-history))))
  ;; Load issue data before showing menu
  (beads-check-executable)
  (unless issue-id
    (user-error "No issue ID specified"))
  ;; Reset stale state from any prior cancelled/errored session
  (setq beads-update--issue-id nil
        beads-update--original-data nil)
  (beads-update--load-issue-data issue-id)
  ;; Show the transient menu
  (beads-update--menu))

;; Auto-generated transient for testing/internal use
(beads-meta-define-transient beads-command-update "beads-update-transient"
  "Update one or more issues (auto-generated menu).

See `beads-update' for the full user-facing transient menu."
  beads-option-global-section)

(provide 'beads-command-update)
;;; beads-command-update.el ends here
