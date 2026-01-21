;;; beads-command-delete.el --- Delete command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-delete' EIEIO class for the
;; `bd delete' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd delete command permanently deletes one or more issues and
;; cleans up all references to them.  This is a destructive operation
;; that:
;; 1. Removes all dependency links (any type, both directions)
;; 2. Updates text references to "[deleted:ID]" in connected issues
;; 3. Deletes the issues from the database (creates tombstones by default)
;;
;; Features:
;; - Delete single or multiple issues at once
;; - Cascade mode to delete all dependents
;; - Dry-run preview before deletion
;; - Hard delete bypasses tombstones
;; - Reason field for audit trail
;;
;; Usage:
;;   (beads-command-execute (beads-command-delete :issue-ids '("bd-1")
;;                                                 :force t))
;;   (beads-command-delete!)  ; convenience function

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
(declare-function beads-buffer-name-utility "beads-buffer")
(declare-function beads-buffer-name-find-show-buffers "beads-buffer")
(declare-function beads-buffer-parse-show "beads-buffer")
(declare-function beads-completion-read-issue "beads-completion")
(declare-function beads-check-executable "beads")
(declare-function beads--get-database-path "beads")
(declare-function beads--error "beads")
(defvar beads-show--issue-id)
(defvar beads-auto-refresh)
(defvar beads-actor)
(defvar beads-executable)

;;; Delete Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-delete (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs to delete (positional arguments).
Example: '(\"bd-1\" \"bd-2\" \"bd-3\")"
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
    :transient-reader beads-reader-issue-id
    :transient-group "Delete Issue"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)
   (cascade
    :initarg :cascade
    :type boolean
    :initform nil
    :documentation "Recursively delete all dependent issues (--cascade)."
    ;; CLI properties
    :long-option "cascade"
    :option-type :boolean
    ;; Transient properties
    :transient-key "c"
    :transient-description "--cascade"
    :transient-class transient-switch
    :transient-argument "--cascade"
    :transient-group "Dependency Handling"
    :transient-level 2
    :transient-order 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview what would be deleted without making changes
(--dry-run)."
    ;; CLI properties
    :long-option "dry-run"
    :option-type :boolean
    ;; Transient properties
    :transient-key "n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Actually delete without preview (-f, --force).
Without this flag, shows preview only."
    ;; CLI properties
    :long-option "force"
    :short-option "f"
    :option-type :boolean
    ;; Transient properties
    :transient-key "f"
    :transient-description "--force"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Options"
    :transient-level 1
    :transient-order 3)
   (from-file
    :initarg :from-file
    :type (or null string)
    :initform nil
    :documentation "Read issue IDs from file, one per line (--from-file)."
    ;; CLI properties
    :long-option "from-file"
    :option-type :string
    ;; Transient properties
    :transient-key "F"
    :transient-description "--from-file"
    :transient-class transient-option
    :transient-argument "--from-file="
    :transient-prompt "File path: "
    :transient-reader transient-read-file
    :transient-group "Batch Delete"
    :transient-level 3
    :transient-order 1)
   (hard
    :initarg :hard
    :type boolean
    :initform nil
    :documentation "Permanently delete, skip tombstone (--hard).
Cannot be recovered via sync.  Use only when certain issues
will not resurrect from remote branches."
    ;; CLI properties
    :long-option "hard"
    :option-type :boolean
    ;; Transient properties
    :transient-key "H"
    :transient-description "--hard (permanent)"
    :transient-class transient-switch
    :transient-argument "--hard"
    :transient-group "Options"
    :transient-level 3
    :transient-order 2)
   (reason
    :initarg :reason
    :type (or null string)
    :initform nil
    :documentation "Reason for deletion (--reason).
Stored in tombstone for audit trail."
    ;; CLI properties
    :long-option "reason"
    :option-type :string
    ;; Transient properties
    :transient-key "r"
    :transient-description "--reason"
    :transient-class transient-option
    :transient-argument "--reason="
    :transient-prompt "Reason: "
    :transient-group "Delete Issue"
    :transient-level 2
    :transient-order 2))
  :documentation "Represents bd delete command.
Deletes one or more issues and cleans up all references to them.
When executed with :json t, returns deleted issue(s) data."))

(cl-defmethod beads-command-subcommand ((_command beads-command-delete))
  "Return \"delete\" as the CLI subcommand name."
  "delete")

(cl-defmethod beads-command-validate ((command beads-command-delete))
  "Validate delete COMMAND.
Checks that at least one issue ID is provided (via issue-ids or from-file).
Returns error string or nil if valid."
  (with-slots (issue-ids from-file) command
    (cond
     ;; Must have issue-ids or from-file
     ((and (or (null issue-ids) (zerop (length issue-ids)))
           (or (null from-file) (string-empty-p from-file)))
      "Must provide at least one issue ID or --from-file")
     ;; Validate list content types
     ((and issue-ids (beads-command--validate-string-list issue-ids "issue-ids"))
      (beads-command--validate-string-list issue-ids "issue-ids"))
     ;; Otherwise valid
     (t nil))))

(cl-defmethod beads-command-parse ((command beads-command-delete) execution)
  "Parse delete COMMAND output from EXECUTION.
Returns deleted issue info.
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, returns alist with deletion info.
Does not modify any slots.

The bd CLI returns different JSON formats:

Direct mode (single issue):
  {\"deleted\": \"issue-id\", \"dependencies_removed\": N,
   \"references_updated\": N}

Direct mode (batch):
  {\"deleted\": [\"id1\", \"id2\"], \"deleted_count\": N, ...}

Daemon RPC mode:
  {\"deleted_count\": N, \"total_count\": N}

Returns the parsed alist or nil."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON
      (let ((parsed-json (cl-call-next-method)))
        (cond
         ;; Empty result
         ((null parsed-json) nil)
         ;; Vector of results (multiple deletions)
         ((vectorp parsed-json)
          (append parsed-json nil))
         ;; Single object with 'deleted' field (direct mode CLI format)
         ((alist-get 'deleted parsed-json)
          parsed-json)
         ;; Legacy format with 'id' field (for backwards compatibility)
         ((alist-get 'id parsed-json)
          parsed-json)
         ;; Daemon RPC format with 'deleted_count' field (no 'deleted')
         ((alist-get 'deleted_count parsed-json)
          parsed-json)
         ;; Unexpected format
         (t
          (signal 'beads-json-parse-error
                  (list "Unexpected JSON structure from bd delete"
                        :exit-code (oref execution exit-code)
                        :parsed-json parsed-json
                        :stderr (oref execution stderr)))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-delete))
  "Execute CMD in terminal buffer with human-readable output.
Disables JSON mode for interactive display with colors."
  ;; Set json to nil for human-readable colored output
  (oset cmd json nil)
  ;; Call the default implementation
  (cl-call-next-method))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-delete-transient "beads-command-delete" nil t)
(beads-meta-define-transient beads-command-delete "beads-delete-transient"
  "Delete one or more issues with optional flags.

This is a DESTRUCTIVE operation that:
- Removes all dependency links (any type, both directions)
- Updates text references to \"[deleted:ID]\" in connected issues
- Deletes the issues from the database

Use --dry-run to preview what would be deleted.
Use --force to actually delete (without preview).
Use --cascade to recursively delete all dependents.
Use --hard for permanent deletion (bypasses tombstones).

Transient levels control which options are visible (cycle with C-x l):
  Level 1: Issue IDs, dry-run, force
  Level 2: Cascade, reason
  Level 3: From-file, hard"
  beads-option-global-section)

;;; Interactive Deletion Workflow

(defun beads-delete--detect-issue-id ()
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

(defun beads-delete--get-preview (issue-id)
  "Get deletion preview for ISSUE-ID.
Runs bd delete without --force to show what will be affected.
Returns preview output as string."
  (let ((db (beads--get-database-path))
        (args nil))
    ;; Build arguments list: delete <issue-id> [--db PATH] [--actor ACTOR]
    (push "delete" args)
    (push issue-id args)
    (when db
      (push "--db" args)
      (push (file-local-name db) args))
    (when beads-actor
      (push "--actor" args)
      (push beads-actor args))
    ;; Reverse to get correct order
    (setq args (nreverse args))
    (with-temp-buffer
      (let ((exit-code (apply #'process-file
                              beads-executable nil t nil args)))
        (if (zerop exit-code)
            (buffer-string)
          (beads--error "Failed to get preview (exit %d): %s"
                        exit-code (buffer-string)))))))

(defun beads-delete--show-preview (issue-id preview-text)
  "Show deletion preview for ISSUE-ID in a buffer.
PREVIEW-TEXT is the output from bd delete without --force.
Returns the preview buffer."
  (let* ((buf-name (beads-buffer-name-utility "delete-preview" issue-id))
         (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert preview-text)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (special-mode))
    buffer))

(defun beads-delete--execute-deletion (issue-id)
  "Execute the deletion of ISSUE-ID with --force flag."
  (let ((result (beads-command-delete! :issue-ids (list issue-id) :force t)))
    (message "Deleted issue %s" issue-id)
    ;; Invalidate completion cache
    (beads--invalidate-completion-cache)
    ;; Close show buffers for the deleted issue (may have different titles)
    (dolist (buf (beads-buffer-name-find-show-buffers nil issue-id))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; Close preview buffer
    (let ((preview-buf-name (beads-buffer-name-utility "delete-preview"
                                                       issue-id)))
      (when-let ((preview-buffer (get-buffer preview-buf-name)))
        (kill-buffer preview-buffer)))
    ;; Refresh any open beads buffers
    (when beads-auto-refresh
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'beads-list-mode)
                     (bound-and-true-p beads-list--command))
            (beads-list-refresh t)))))
    result))

;;;###autoload
(defun beads-delete (&optional issue-id)
  "Delete an issue with preview and confirmation.

This is a DESTRUCTIVE operation that:
- Removes all dependency links
- Updates text references to \\='[deleted:ID]\\='
- Deletes the issue from the database

Workflow:
1. Prompts for issue ID (or uses ISSUE-ID if provided)
2. Shows preview of what will be deleted (bd delete without --force)
3. Asks for confirmation (yes/no)
4. If yes: executes deletion (bd delete --force)
5. If no: cancels operation

If called from beads-list or beads-show buffer, detects issue
from context."
  (interactive
   (list (or (beads-delete--detect-issue-id)
             (beads-completion-read-issue "Delete issue: "
                              nil t nil 'beads--issue-id-history))))
  ;; Check executable
  (beads-check-executable)

  ;; Get issue ID if not provided
  (unless issue-id
    (setq issue-id (beads-completion-read-issue "Delete issue: "
                                    nil t nil
                                    'beads--issue-id-history)))

  ;; Get preview
  (message "Getting deletion preview for %s..." issue-id)
  (condition-case err
      (let* ((preview-text (beads-delete--get-preview issue-id))
             (preview-buffer (beads-delete--show-preview
                              issue-id preview-text)))
        ;; Show preview buffer
        (pop-to-buffer preview-buffer)

        ;; Ask for confirmation and close preview buffer
        (unwind-protect
            (when (yes-or-no-p
                   (format "Delete issue %s? " issue-id))
              ;; Execute deletion
              (beads-delete--execute-deletion issue-id))
          ;; Always close preview buffer after answering
          (when (buffer-live-p preview-buffer)
            (kill-buffer preview-buffer))))
    (error
     (message "Delete operation failed: %s"
              (error-message-string err)))))

(provide 'beads-command-delete)
;;; beads-command-delete.el ends here
