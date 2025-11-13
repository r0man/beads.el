;;; beads-command.el --- EIEIO command classes for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; This module defines EIEIO classes for all Beads CLI commands,
;; providing an object-oriented interface to bd command execution.
;;
;; The class hierarchy mirrors bd command structure:
;; - beads-command: Base class with global flags (--actor, --db, etc.)
;;   - beads-command-json: Commands that support --json flag (abstract)
;;     - beads-command-create: bd create command
;;     - beads-command-update: bd update command (future)
;;     - beads-command-list: bd list command
;;   - beads-command-init: bd init command (no JSON support)
;;
;; Each command class:
;; - Has slots for all applicable flags
;; - Implements beads-command-execute method
;; - Provides beads-command-line for building full command line
;; - Supports validation via beads-command-validate
;;
;; Usage:
;;
;;   ;; Create and execute an init command (no JSON support)
;;   (let ((cmd (beads-command-init
;;               :prefix "myproject"
;;               :branch "main"
;;               :quiet t)))
;;     (beads-command-execute cmd))
;;
;;   ;; Build full command line without execution
;;   (beads-command-line cmd)
;;   ;; => ("bd" "init" "--prefix" "myproject" "--branch" "main" "--quiet")

;;; Code:

(require 'eieio)
(require 'beads-types)
(require 'beads-error)
(require 'cl-lib)
(require 'json)

;; Forward declarations
(defvar beads-executable)
(declare-function beads--log "beads")

;;; Base Command Class

(defclass beads-command ()
  ((actor
    :initarg :actor
    :type (or null string)
    :initform nil
    :documentation "Actor name for audit trail (--actor).
Overrides $BD_ACTOR or $USER.")
   (db
    :initarg :db
    :type (or null string)
    :initform nil
    :documentation "Database path (--db).
Overrides auto-discovery of .beads/*.db.")
   (no-auto-flush
    :initarg :no-auto-flush
    :type boolean
    :initform nil
    :documentation "Disable automatic JSONL sync (--no-auto-flush).
Prevents auto-export after CRUD operations.")
   (no-auto-import
    :initarg :no-auto-import
    :type boolean
    :initform nil
    :documentation "Disable automatic JSONL import (--no-auto-import).
Prevents auto-import when JSONL is newer than DB.")
   (no-daemon
    :initarg :no-daemon
    :type boolean
    :initform nil
    :documentation "Force direct storage mode (--no-daemon).
Bypass daemon if running.")
   (no-db
    :initarg :no-db
    :type boolean
    :initform nil
    :documentation "Use no-db mode (--no-db).
Load from JSONL, no SQLite database.")
   (sandbox
    :initarg :sandbox
    :type boolean
    :initform nil
    :documentation "Sandbox mode (--sandbox).
Disables daemon and auto-sync."))
  :abstract t
  :documentation "Abstract base class for all bd commands.
Contains slots for global flags that apply to all commands.
Subclasses should implement beads-command-execute method.")

;;; Generic Methods

(cl-defgeneric beads-command-execute (command)
  "Execute COMMAND by building arguments and running bd CLI.
Always returns a list: (EXIT-CODE STDOUT STDERR)

For non-JSON commands:
  STDOUT is the command's standard output as a string

For JSON commands with :json t:
  STDOUT is the parsed JSON (alist/vector)

For JSON commands with :json nil:
  STDOUT is the command's standard output as a string

EXIT-CODE is the process exit code (integer)
STDERR is the command's standard error as a string

Signals errors:
  - beads-validation-error: Command validation failed
  - beads-command-error: Command execution failed (non-zero exit)
  - beads-json-parse-error: JSON parsing failed

Subclasses should not override this; implementations are
provided for beads-command and beads-json-command.")

(cl-defgeneric beads-command-line (command)
  "Build full command line from COMMAND object.
Returns a list of strings starting with the executable (e.g., \"bd\"),
followed by command name and all flags.
Example: (\"bd\" \"list\" \"--json\" \"--status\" \"open\")")

(cl-defgeneric beads-command-validate (command)
  "Validate COMMAND and return error string or nil if valid.
Subclasses should override to add command-specific validation.")

;;; Base Implementation - Global Flags

(cl-defmethod beads-command-line :around ((_command beads-command))
  "Prepend executable to command line built by primary method.
This :around method ensures all command lines start with beads-executable."
  (cons beads-executable (cl-call-next-method)))

(cl-defmethod beads-command-line ((command beads-command))
  "Build global flag arguments from COMMAND.
Returns list of global flag strings (without executable).
Subclasses should call this via `cl-call-next-method' and append
their command-specific arguments."
  (with-slots (actor db no-auto-flush no-auto-import
                     no-daemon no-db sandbox) command
    (let (args)
      ;; Boolean flags
      (when no-auto-flush (push "--no-auto-flush" args))
      (when no-auto-import (push "--no-auto-import" args))
      (when no-daemon (push "--no-daemon" args))
      (when no-db (push "--no-db" args))
      (when sandbox (push "--sandbox" args))
      ;; String options
      (when actor
        (push "--actor" args)
        (push actor args))
      (when db
        (push "--db" args)
        (push db args))
      ;; Return in correct order (reverse since we pushed)
      (nreverse args))))

(cl-defmethod beads-command-validate ((_command beads-command))
  "Validate base COMMAND.
Default implementation returns nil (valid).
Subclasses override to add validation."
  nil)

;;; Base Command Execution - Non-JSON Commands

(cl-defmethod beads-command-execute ((command beads-command))
  "Execute non-JSON COMMAND and return (EXIT-CODE STDOUT STDERR).
STDOUT is the command output as a string.
Signals beads-validation-error or beads-command-error on failure."
  ;; Validate first
  (when-let ((error (beads-command-validate command)))
    (signal 'beads-validation-error
            (list (format "Command validation failed: %s" error)
                  :command command
                  :error error)))

  ;; Build full command line
  (let* ((cmd (beads-command-line command))
         (cmd-string (mapconcat #'shell-quote-argument cmd " "))
         (stderr-file (make-temp-file "beads-stderr-"))
         (start-time (current-time)))

    (when (fboundp 'beads--log)
      (beads--log 'info "Running: %s" cmd-string)
      (beads--log 'verbose "In directory: %s" default-directory))

    (unwind-protect
        (with-temp-buffer
          (let* ((exit-code (apply #'process-file
                                  (car cmd) nil
                                  (list (current-buffer) stderr-file)
                                  nil (cdr cmd)))
                 (end-time (current-time))
                 (elapsed (float-time (time-subtract end-time start-time)))
                 (stdout (buffer-string))
                 (stderr (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (buffer-string))))

          (when (fboundp 'beads--log)
            (beads--log 'info "Command completed in %.3fs" elapsed)
            (beads--log 'verbose "Exit code: %d" exit-code)
            (beads--log 'verbose "Stdout: %s" stdout)
            (beads--log 'verbose "Stderr: %s" stderr))

            (if (zerop exit-code)
                (list exit-code stdout stderr)
              ;; Signal error with complete information
              (signal 'beads-command-error
                      (list (format "Command failed with exit code %d" exit-code)
                            :command cmd-string
                            :exit-code exit-code
                            :stdout stdout
                            :stderr stderr)))))

      ;; Cleanup temp file
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

;;; JSON Command

(defclass beads-command-json (beads-command)
  ((json
    :initarg :json
    :type boolean
    :initform t
    :documentation "Output in JSON format (--json).
Enables machine-readable output."))
  :abstract t
  :documentation "Abstract base class for bd commands that support JSON output.
Inherits from beads-command and adds --json flag support.
Use this as parent class for commands that support --json flag.")

(cl-defmethod beads-command-line ((command beads-command-json))
  "Build command arguments including --json flag for JSON COMMAND.
Calls parent method and adds --json if enabled."
  (with-slots (json) command
    (let ((args (cl-call-next-method)))
      ;; Add --json flag if enabled
      (when json
        (setq args (append args (list "--json"))))
      args)))

(cl-defmethod beads-command-execute ((command beads-command-json))
  "Execute JSON COMMAND and return (EXIT-CODE STDOUT STDERR).
When :json is t, STDOUT is parsed JSON (alist/vector).
When :json is nil, falls back to parent (STDOUT is string).
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent, then parse JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (stdout (nth 1 result))
             (stderr (nth 2 result)))
        ;; If successful, parse JSON from stdout
        (if (zerop exit-code)
            (condition-case err
                (let* ((json-object-type 'alist)
                       (json-array-type 'vector)
                       (json-key-type 'symbol)
                       (parsed-json (json-read-from-string stdout)))
                  (list exit-code parsed-json stderr))
              (error
               (signal 'beads-json-parse-error
                       (list (format "Failed to parse JSON: %s"
                                    (error-message-string err))
                             :exit-code exit-code
                             :stdout stdout
                             :stderr stderr
                             :parse-error err))))
          ;; Non-zero exit code: parent already signaled error,
          ;; but we shouldn't reach here
          result)))))

;;; Init Command

(defclass beads-command-init (beads-command)
  ((branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Git branch for beads commits (-b, --branch).
Default: current branch.")
   (contributor
    :initarg :contributor
    :type boolean
    :initform nil
    :documentation "Run OSS contributor setup wizard (--contributor).")
   (prefix
    :initarg :prefix
    :type (or null string)
    :initform nil
    :documentation "Issue prefix (-p, --prefix).
Default: current directory name.")
   (quiet
    :initarg :quiet
    :type boolean
    :initform nil
    :documentation "Suppress output (-q, --quiet).")
   (skip-merge-driver
    :initarg :skip-merge-driver
    :type boolean
    :initform nil
    :documentation "Skip git merge driver setup (--skip-merge-driver).
Non-interactive mode.")
   (team
    :initarg :team
    :type boolean
    :initform nil
    :documentation "Run team workflow setup wizard (--team)."))
  :documentation "Represents bd init command.
Initializes bd in the current directory by creating .beads/ directory
and database file.")

(cl-defmethod beads-command-line ((command beads-command-init))
  "Build command arguments for init COMMAND (without executable).
Returns list: (\"init\" ...global-flags... ...init-flags...)."
  (with-slots (branch contributor prefix quiet
                      skip-merge-driver team) command
    (let ((args (list "init"))
          (global-args (cl-call-next-method)))
      ;; Append global flags
      (setq args (append args global-args))
      ;; Init-specific boolean flags
      (when contributor (push "--contributor" args))
      (when quiet (push "--quiet" args))
      (when skip-merge-driver (push "--skip-merge-driver" args))
      (when team (push "--team" args))
      ;; Init-specific string options
      (when branch
        (push "--branch" args)
        (push branch args))
      (when prefix
        (push "--prefix" args)
        (push prefix args))
      ;; Note: args are in reverse order due to push
      ;; But "init" is at the front, so we need to handle this carefully
      ;; Let's rebuild in correct order
      (let ((cmd-args nil))
        ;; Command name first
        (push "init" cmd-args)
        ;; Global args
        (setq cmd-args (append cmd-args global-args))
        ;; Init-specific args
        (when branch
          (setq cmd-args (append cmd-args (list "--branch" branch))))
        (when contributor
          (setq cmd-args (append cmd-args (list "--contributor"))))
        (when prefix
          (setq cmd-args (append cmd-args (list "--prefix" prefix))))
        (when quiet
          (setq cmd-args (append cmd-args (list "--quiet"))))
        (when skip-merge-driver
          (setq cmd-args (append cmd-args (list "--skip-merge-driver"))))
        (when team
          (setq cmd-args (append cmd-args (list "--team"))))
        cmd-args))))

(cl-defmethod beads-command-validate ((command beads-command-init))
  "Validate init COMMAND.
Checks for conflicts between options.
Returns error string or nil if valid."
  (with-slots (contributor team) command
    (cond
     ;; Can't use both --contributor and --team
     ((and contributor team)
      "Cannot use both --contributor and --team flags")
     ;; Otherwise valid
     (t nil))))

;;; List Command

(defclass beads-command-list (beads-command-json)
  ((all
    :initarg :all
    :type boolean
    :initform nil
    :documentation "Show all issues (--all).
Default behavior, provided for CLI familiarity.")
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee (-a, --assignee).")
   (closed-after
    :initarg :closed-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed after date (--closed-after).
Date format: YYYY-MM-DD or RFC3339.")
   (closed-before
    :initarg :closed-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed before date (--closed-before).
Date format: YYYY-MM-DD or RFC3339.")
   (created-after
    :initarg :created-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues created after date (--created-after).
Date format: YYYY-MM-DD or RFC3339.")
   (created-before
    :initarg :created-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues created before date (--created-before).
Date format: YYYY-MM-DD or RFC3339.")
   (desc-contains
    :initarg :desc-contains
    :type (or null string)
    :initform nil
    :documentation "Filter by description substring (--desc-contains).
Case-insensitive.")
   (empty-description
    :initarg :empty-description
    :type boolean
    :initform nil
    :documentation "Filter issues with empty description (--empty-description).")
   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Output format (--format).
Values: 'digraph', 'dot', or Go template.")
   (id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Filter by specific issue IDs (--id).
Comma-separated, e.g., 'bd-1,bd-5,bd-10'.")
   (label
    :initarg :label
    :type (or null list)
    :initform nil
    :documentation "Filter by labels, AND logic (-l, --label).
Must have ALL labels. Can combine with --label-any.")
   (label-any
    :initarg :label-any
    :type (or null list)
    :initform nil
    :documentation "Filter by labels, OR logic (--label-any).
Must have AT LEAST ONE label. Can combine with --label.")
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit results (-n, --limit).")
   (long
    :initarg :long
    :type boolean
    :initform nil
    :documentation "Show detailed multi-line output (--long).")
   (no-assignee
    :initarg :no-assignee
    :type boolean
    :initform nil
    :documentation "Filter issues with no assignee (--no-assignee).")
   (no-labels
    :initarg :no-labels
    :type boolean
    :initform nil
    :documentation "Filter issues with no labels (--no-labels).")
   (notes-contains
    :initarg :notes-contains
    :type (or null string)
    :initform nil
    :documentation "Filter by notes substring (--notes-contains).
Case-insensitive.")
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Filter by priority (-p, --priority).
Values: 0-4 (0=critical, 1=high, 2=medium, 3=low, 4=backlog).")
   (priority-max
    :initarg :priority-max
    :type (or null integer)
    :initform nil
    :documentation "Filter by maximum priority (--priority-max).
Inclusive.")
   (priority-min
    :initarg :priority-min
    :type (or null integer)
    :initform nil
    :documentation "Filter by minimum priority (--priority-min).
Inclusive.")
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status (-s, --status).
Values: open, in_progress, blocked, closed.")
   (title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Filter by title text (--title).
Case-insensitive substring match.")
   (title-contains
    :initarg :title-contains
    :type (or null string)
    :initform nil
    :documentation "Filter by title substring (--title-contains).
Case-insensitive.")
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Filter by type (-t, --type).
Values: bug, feature, task, epic, chore.")
   (updated-after
    :initarg :updated-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated after date (--updated-after).
Date format: YYYY-MM-DD or RFC3339.")
   (updated-before
    :initarg :updated-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated before date (--updated-before).
Date format: YYYY-MM-DD or RFC3339."))
  :documentation "Represents bd list command.
Lists issues with optional filtering, sorting, and formatting.
When executed with :json t, returns a list of beads-issue instances.")

(cl-defmethod beads-command-line ((command beads-command-list))
  "Build command arguments for list COMMAND (without executable).
Returns list: (\"list\" ...global-flags... ...list-flags...)."
  (with-slots (all assignee closed-after closed-before
                   created-after created-before desc-contains
                   empty-description format id label label-any
                   limit long no-assignee no-labels notes-contains
                   priority priority-max priority-min status
                   title title-contains issue-type
                   updated-after updated-before) command
    (let ((args (list "list"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Boolean flags
      (when all
        (setq args (append args (list "--all"))))
      (when empty-description
        (setq args (append args (list "--empty-description"))))
      (when long
        (setq args (append args (list "--long"))))
      (when no-assignee
        (setq args (append args (list "--no-assignee"))))
      (when no-labels
        (setq args (append args (list "--no-labels"))))

      ;; String options
      (when assignee
        (setq args (append args (list "--assignee" assignee))))
      (when closed-after
        (setq args (append args (list "--closed-after" closed-after))))
      (when closed-before
        (setq args (append args (list "--closed-before" closed-before))))
      (when created-after
        (setq args (append args (list "--created-after" created-after))))
      (when created-before
        (setq args (append args (list "--created-before" created-before))))
      (when desc-contains
        (setq args (append args (list "--desc-contains" desc-contains))))
      (when format
        (setq args (append args (list "--format" format))))
      (when id
        (setq args (append args (list "--id" id))))
      (when notes-contains
        (setq args (append args (list "--notes-contains" notes-contains))))
      (when status
        (setq args (append args (list "--status" status))))
      (when title
        (setq args (append args (list "--title" title))))
      (when title-contains
        (setq args (append args (list "--title-contains" title-contains))))
      (when issue-type
        (setq args (append args (list "--type" issue-type))))
      (when updated-after
        (setq args (append args (list "--updated-after" updated-after))))
      (when updated-before
        (setq args (append args (list "--updated-before" updated-before))))

      ;; Integer options
      (when limit
        (setq args (append args (list "--limit" (number-to-string limit)))))
      (when priority
        (setq args (append args (list "--priority"
                                      (number-to-string priority)))))
      (when priority-max
        (setq args (append args (list "--priority-max"
                                      (number-to-string priority-max)))))
      (when priority-min
        (setq args (append args (list "--priority-min"
                                      (number-to-string priority-min)))))

      ;; List options (multiple values)
      (when label
        (dolist (lbl label)
          (setq args (append args (list "--label" lbl)))))
      (when label-any
        (dolist (lbl label-any)
          (setq args (append args (list "--label-any" lbl)))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-list))
  "Validate list COMMAND.
Checks for conflicts between options.
Returns error string or nil if valid."
  (with-slots (priority priority-max priority-min
                        assignee no-assignee
                        label label-any no-labels) command
    (cond
     ;; Can't use --priority with --priority-min/max
     ((and priority (or priority-max priority-min))
      "Cannot use --priority with --priority-min/--priority-max")
     ;; Can't use --assignee with --no-assignee
     ((and assignee no-assignee)
      "Cannot use both --assignee and --no-assignee")
     ;; Can't use --label/--label-any with --no-labels
     ((and no-labels (or label label-any))
      "Cannot use --label/--label-any with --no-labels")
     ;; Validate priority range
     ((and priority (not (<= 0 priority 4)))
      "Priority must be between 0 and 4")
     ((and priority-min (not (<= 0 priority-min 4)))
      "Priority-min must be between 0 and 4")
     ((and priority-max (not (<= 0 priority-max 4)))
      "Priority-max must be between 0 and 4")
     ;; Otherwise valid
     (t nil))))

(cl-defmethod beads-command-execute ((command beads-command-list))
  "Execute list COMMAND and return list of beads-issue instances.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns list of beads-issue instances.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON, then convert
      ;; to beads-issue instances using beads-issue-from-json
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result))
             (stderr (nth 2 result)))
        ;; Convert JSON array to list of beads-issue instances
        (if (zerop exit-code)
            (condition-case err
                (let ((issues (mapcar #'beads-issue-from-json
                                     (append parsed-json nil))))
                  issues)
              (error
               (signal 'beads-json-parse-error
                       (list (format "Failed to create beads-issue instances: %s"
                                    (error-message-string err))
                             :exit-code exit-code
                             :parsed-json parsed-json
                             :stderr stderr
                             :parse-error err))))
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Create Command

(defclass beads-command-create (beads-command-json)
  ((title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Issue title (positional or --title).
First positional argument or explicit --title flag.")
   (acceptance
    :initarg :acceptance
    :type (or null string)
    :initform nil
    :documentation "Acceptance criteria (--acceptance).")
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Assignee (-a, --assignee).")
   (deps
    :initarg :deps
    :type (or null list)
    :initform nil
    :documentation "Dependencies (--deps).
List of strings in format 'type:id' or 'id'.
Examples: 'discovered-from:bd-20', 'blocks:bd-15', 'bd-20'.")
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Issue description (-d, --description).")
   (design
    :initarg :design
    :type (or null string)
    :initform nil
    :documentation "Design notes (--design).")
   (external-ref
    :initarg :external-ref
    :type (or null string)
    :initform nil
    :documentation "External reference (--external-ref).
Examples: 'gh-9', 'jira-ABC'.")
   (file
    :initarg :file
    :type (or null string)
    :initform nil
    :documentation "Create multiple issues from markdown file (-f, --file).")
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force creation even if prefix doesn't match (--force).")
   (from-template
    :initarg :from-template
    :type (or null string)
    :initform nil
    :documentation "Create issue from template (--from-template).
Examples: 'epic', 'bug', 'feature'.")
   (id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Explicit issue ID (--id).
Example: 'bd-42' for partitioning.")
   (labels
    :initarg :labels
    :type (or null list)
    :initform nil
    :documentation "Labels (-l, --labels).
List of label strings.")
   (parent
    :initarg :parent
    :type (or null string)
    :initform nil
    :documentation "Parent issue ID for hierarchical child (--parent).
Example: 'bd-a3f8e9'.")
   (priority
    :initarg :priority
    :type (or null string)
    :initform nil
    :documentation "Priority (-p, --priority).
Values: 0-4 or P0-P4 (0=highest). Default: '2'.")
   (repo
    :initarg :repo
    :type (or null string)
    :initform nil
    :documentation "Target repository for issue (--repo).
Overrides auto-routing.")
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Issue type (-t, --type).
Values: bug, feature, task, epic, chore. Default: 'task'."))
  :documentation "Represents bd create command.
Creates a new issue (or multiple issues from markdown file).
When executed with :json t, returns the created beads-issue instance(s).")

(cl-defmethod beads-command-line ((command beads-command-create))
  "Build command arguments for create COMMAND (without executable).
Returns list: (\"create\" ...global-flags... ...create-flags...)."
  (with-slots (title acceptance assignee deps description design
                     external-ref file force from-template id labels
                     parent priority repo issue-type) command
    (let ((args (list "create"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Title (positional argument if provided)
      (when title
        (setq args (append args (list title))))

      ;; Boolean flags
      (when force
        (setq args (append args (list "--force"))))

      ;; String options
      (when acceptance
        (setq args (append args (list "--acceptance" acceptance))))
      (when assignee
        (setq args (append args (list "--assignee" assignee))))
      (when description
        (setq args (append args (list "--description" description))))
      (when design
        (setq args (append args (list "--design" design))))
      (when external-ref
        (setq args (append args (list "--external-ref" external-ref))))
      (when file
        (setq args (append args (list "--file" file))))
      (when from-template
        (setq args (append args (list "--from-template" from-template))))
      (when id
        (setq args (append args (list "--id" id))))
      (when parent
        (setq args (append args (list "--parent" parent))))
      (when priority
        (setq args (append args (list "--priority" priority))))
      (when repo
        (setq args (append args (list "--repo" repo))))
      (when issue-type
        (setq args (append args (list "--type" issue-type))))

      ;; List options (multiple values)
      (when deps
        (setq args (append args (list "--deps" (mapconcat #'identity deps ",")))))
      (when labels
        (setq args (append args (list "--labels" (mapconcat #'identity labels ",")))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-create))
  "Validate create COMMAND.
Checks for required fields and conflicts between options.
Returns error string or nil if valid."
  (with-slots (title file) command
    (cond
     ;; Must have either title or file
     ((and (not title) (not file))
      "Must provide either title or --file")
     ;; Can't use both title and file
     ((and title file)
      "Cannot use both title and --file")
     ;; Otherwise valid
     (t nil))))

(cl-defmethod beads-command-execute ((command beads-command-create))
  "Execute create COMMAND and return created issue(s).
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns beads-issue instance (or list of instances
when creating from file).
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON, then convert
      ;; to beads-issue instance(s) using beads-issue-from-json
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result))
             (stderr (nth 2 result)))
        ;; Convert JSON to beads-issue instance(s)
        (if (zerop exit-code)
            (condition-case err
                (cond
                 ;; Single issue (JSON object)
                 ((eq (type-of parsed-json) 'cons)
                  (beads-issue-from-json parsed-json))
                 ;; Multiple issues from file (JSON array)
                 ((eq (type-of parsed-json) 'vector)
                  (mapcar #'beads-issue-from-json
                          (append parsed-json nil)))
                 ;; Unexpected JSON structure
                 (t
                  (signal 'beads-json-parse-error
                          (list "Unexpected JSON structure from bd create"
                                :exit-code exit-code
                                :parsed-json parsed-json
                                :stderr stderr))))
              (error
               (signal 'beads-json-parse-error
                       (list (format "Failed to create beads-issue instance: %s"
                                    (error-message-string err))
                             :exit-code exit-code
                             :parsed-json parsed-json
                             :stderr stderr
                             :parse-error err))))
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Utility Functions

(defun beads-command-init-from-options (options)
  "Create beads-command-init from OPTIONS plist.
OPTIONS should be a plist with keys matching slot names:
  :branch, :contributor, :prefix, :quiet, :skip-merge-driver, :team
Plus global flags:
  :actor, :db, :no-auto-flush, :no-auto-import,
  :no-daemon, :no-db, :sandbox"
  (apply #'beads-command-init options))

;;; Execute Helper Functions

(defun beads-command-init! (&rest args)
  "Create and execute a beads-command-init with ARGS.
Returns the result of executing the command.
See `beads-command-init' for available arguments."
  (beads-command-execute (apply #'beads-command-init args)))

(defun beads-command-create! (&rest args)
  "Create and execute a beads-command-create with ARGS.
Returns the parsed issue object.
See `beads-command-create' for available arguments."
  (beads-command-execute (apply #'beads-command-create args)))

(defun beads-command-list! (&rest args)
  "Create and execute a beads-command-list with ARGS.
Returns a list of parsed issue objects.
See `beads-command-list' for available arguments."
  (beads-command-execute (apply #'beads-command-list args)))

(provide 'beads-command)
;;; beads-command.el ends here
