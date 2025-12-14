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

;; Forward declarations to avoid circular dependency
;; (beads.el requires beads-command, so we can't require beads here)
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
Disables daemon and auto-sync.")
   ;; Async execution result slots
   (exit-code
    :initarg :exit-code
    :type (or null integer)
    :initform nil
    :documentation "Exit code from async execution.
Set by `beads-command-execute-async' when command completes.")
   (stdout
    :initarg :stdout
    :type (or null string)
    :initform nil
    :documentation "Standard output from async execution.
Set by `beads-command-execute-async' when command completes.")
   (stderr
    :initarg :stderr
    :type (or null string)
    :initform nil
    :documentation "Standard error from async execution.
Set by `beads-command-execute-async' when command completes."))
  :abstract t
  :documentation "Abstract base class for all bd commands.
Contains slots for global flags that apply to all commands.
Subclasses should implement beads-command-execute method.")

;;; Helper Functions

(defun beads-command--validate-string-list (value field-name)
  "Validate that VALUE is nil or a list of strings.
FIELD-NAME is used in error messages.
Returns error string or nil if valid."
  (cond
   ((null value) nil)
   ((not (listp value))
    (format "%s must be a list, got %s" field-name (type-of value)))
   ((not (cl-every #'stringp value))
    (format "%s must contain only strings" field-name))
   (t nil)))

;;; Generic Methods

(cl-defgeneric beads-command-execute (command)
  "Execute COMMAND by building arguments and running bd CLI.

Return value depends on command type and :json slot:

For non-JSON commands (beads-command):
  Returns (EXIT-CODE STDOUT STDERR) tuple
  STDOUT is the command's standard output as a string

For JSON commands (beads-command-json and subclasses):
  DEFAULT BEHAVIOR (:json t, the default):
    Most subclasses return domain objects, NOT raw tuples:
    - beads-command-list: Returns list of beads-issue instances
    - beads-command-create: Returns beads-issue instance (or list)
    - beads-command-show: Returns beads-issue instance (or list)
    - beads-command-update: Returns beads-issue instance (or list)
    - beads-command-close: Returns beads-issue instance (or list)
    - beads-command-ready: Returns list of beads-issue instances
    - beads-command-blocked: Returns list of beads-issue instances
    - beads-command-stats: Returns parsed JSON alist
    - beads-command-epic-*: Returns parsed JSON

  With :json nil:
    Returns (EXIT-CODE STDOUT STDERR) like non-JSON commands
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
Enables machine-readable output.")
   (data
    :initarg :data
    :initform nil
    :documentation "Parsed JSON data after async execution.
This slot is populated by `beads-command-execute-async' when the
command completes successfully and JSON parsing succeeds."))
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

;;; Async Command Execution

(cl-defgeneric beads-command-execute-async (command &optional callback)
  "Execute COMMAND asynchronously without blocking Emacs.

CALLBACK is called with COMMAND when execution completes.
The command object contains the result data in its slots.

Signals `beads-validation-error' immediately if validation fails.

Return value: process object (use `delete-process' to cancel).

For all commands, these slots are populated:
  - `exit-code': Process exit code (0 = success)
  - `stdout': Standard output as string
  - `stderr': Standard error as string

For JSON commands (beads-command-json with :json t):
  - `data': Parsed JSON on success, nil on failure

Example usage:

  (beads-command-execute-async
   (beads-command-list :status \"open\")
   (lambda (cmd)
     (if (oref cmd data)
         (message \"Got %d issues\" (length (oref cmd data)))
       (message \"Failed: %s\" (oref cmd stderr)))))")

(cl-defmethod beads-command-execute-async ((command beads-command)
                                           &optional callback)
  "Execute non-JSON COMMAND asynchronously.
CALLBACK receives the COMMAND object when complete.
The command's `exit-code', `stdout', and `stderr' slots are populated.
Signals `beads-validation-error' immediately if validation fails.
Returns process object."
  ;; Validate first - raise error immediately
  (when-let ((validation-error (beads-command-validate command)))
    (signal 'beads-validation-error
            (list (format "Command validation failed: %s" validation-error)
                  :command command
                  :error validation-error)))

  ;; Validation passed - build command line and execute
  (let* ((cmd (beads-command-line command))
         (cmd-string (mapconcat #'shell-quote-argument cmd " "))
         (stdout-buffer (generate-new-buffer " *beads-async-stdout*"))
         (stderr-buffer (generate-new-buffer " *beads-async-stderr*"))
         (start-time (current-time))
         process)

    (when (fboundp 'beads--log)
      (beads--log 'info "Running async: %s" cmd-string)
      (beads--log 'verbose "In directory: %s" default-directory))

    (setq process
          (make-process
           :name "beads-async"
           :buffer stdout-buffer
           :stderr stderr-buffer
           :command cmd
           :connection-type 'pipe
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let* ((proc-exit-code (process-exit-status proc))
                      (end-time (current-time))
                      (elapsed (float-time (time-subtract end-time start-time)))
                      (proc-stdout (with-current-buffer stdout-buffer
                                     (buffer-string)))
                      (proc-stderr (with-current-buffer stderr-buffer
                                     (buffer-string))))

                 (when (fboundp 'beads--log)
                   (beads--log 'info "Async command completed in %.3fs" elapsed)
                   (beads--log 'verbose "Exit code: %d" proc-exit-code)
                   (beads--log 'verbose "Stdout: %s" proc-stdout)
                   (beads--log 'verbose "Stderr: %s" proc-stderr))

                 ;; Clean up buffers (suppress kill queries for these temp buffers)
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer stdout-buffer)
                   (kill-buffer stderr-buffer))

                 ;; Store results in command object
                 (oset command exit-code proc-exit-code)
                 (oset command stdout proc-stdout)
                 (oset command stderr proc-stderr)

                 ;; Call callback with command object
                 (when callback
                   (funcall callback command)))))))
    process))

(cl-defmethod beads-command-execute-async ((command beads-command-json)
                                           &optional callback)
  "Execute JSON COMMAND asynchronously with JSON parsing.
CALLBACK receives the COMMAND object when complete.
On success, the `data' slot is set to the parsed JSON.
The `exit-code', `stdout', and `stderr' slots are always populated.
Returns process object."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Wrap callback to parse JSON and set data slot
      (cl-call-next-method
       command
       (lambda (cmd)
         ;; Parse JSON and set data slot if execution succeeded
         (when (zerop (oref cmd exit-code))
           (condition-case nil
               (let* ((json-object-type 'alist)
                      (json-array-type 'vector)
                      (json-key-type 'symbol)
                      (parsed-json (json-read-from-string (oref cmd stdout))))
                 (oset cmd data parsed-json))
             (error nil)))  ;; JSON parsing failed, leave data as nil
         ;; Call original callback with command object
         (when callback
           (funcall callback cmd)))))))

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
    (let ((cmd-args (list "init"))
          (global-args (cl-call-next-method)))
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
      cmd-args)))

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

;;; Quickstart Command

(defclass beads-command-quickstart (beads-command)
  ()
  :documentation "Represents bd quickstart command.
Displays a quick start guide showing common bd workflows and patterns.
This command has no command-specific flags, only global flags.")

(cl-defmethod beads-command-line ((_command beads-command-quickstart))
  "Build command arguments for quickstart command (without executable).
Returns list: (\"quickstart\" ...global-flags...)."
  (let ((cmd-args (list "quickstart"))
        (global-args (cl-call-next-method)))
    ;; Global args
    (setq cmd-args (append cmd-args global-args))
    cmd-args))

(cl-defmethod beads-command-validate ((_command beads-command-quickstart))
  "Validate quickstart COMMAND.
No validation needed (no command-specific arguments).
Returns nil (always valid)."
  nil)

;;; Export Command

(defclass beads-command-export (beads-command-json)
  ((force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force export even if database is empty (--force).")
   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Export format (-f, --format).
Default: jsonl.")
   (output
    :initarg :output
    :type (or null string)
    :initform nil
    :documentation "Output file (-o, --output).
Default: stdout.")
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status (-s, --status)."))
  :documentation "Represents bd export command.
Export all issues to JSON Lines format (one JSON object per line).
Issues are sorted by ID for consistent diffs.")

(cl-defmethod beads-command-line ((command beads-command-export))
  "Build command arguments for export COMMAND (without executable).
Returns list: (\"export\" ...flags... ...global-flags...)."
  (with-slots (force format output status) command
    (let ((cmd-args (list "export"))
          (global-args (cl-call-next-method)))
      ;; Command-specific flags
      (when force
        (setq cmd-args (append cmd-args (list "--force"))))
      (when format
        (setq cmd-args (append cmd-args (list "-f" format))))
      (when output
        (setq cmd-args (append cmd-args (list "-o" output))))
      (when status
        (setq cmd-args (append cmd-args (list "-s" status))))
      ;; Global args (includes --json if enabled)
      (setq cmd-args (append cmd-args global-args))
      cmd-args)))

(cl-defmethod beads-command-validate ((command beads-command-export))
  "Validate export COMMAND.
Returns error string or nil if valid."
  (with-slots (format status) command
    (cond
     ;; Validate format if provided
     ((and format (not (member format '("jsonl"))))
      (format "Invalid format: %s (must be jsonl)" format))
     ;; Validate status if provided
     ((and status (not (member status '("open" "in_progress" "closed" "blocked"))))
      (format "Invalid status: %s (must be one of: open, in_progress, closed, blocked)" status))
     ;; Otherwise valid
     (t nil))))

(cl-defmethod beads-command-execute ((command beads-command-export))
  "Execute export COMMAND and return result.
Unlike most commands, bd export writes JSON stats to stderr, not stdout.
When :json is nil, returns (EXIT-CODE STDOUT STDERR).
When :json is t, returns parsed JSON from STDERR (not STDOUT).
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
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
              (with-slots (json) command
                (if (not json)
                    ;; No JSON parsing, return raw output
                    (list exit-code stdout stderr)
                  ;; Parse JSON from stderr (not stdout!)
                  (condition-case err
                      (let* ((json-object-type 'alist)
                             (json-array-type 'vector)
                             (json-key-type 'symbol)
                             (parsed-json (json-read-from-string stderr)))
                        (list exit-code parsed-json stderr))
                    (error
                     (signal 'beads-json-parse-error
                             (list (format "Failed to parse JSON from stderr: %s"
                                           (error-message-string err))
                                   :exit-code exit-code
                                   :stdout stdout
                                   :stderr stderr
                                   :parse-error err))))))
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

;;; Import Command

(defclass beads-command-import (beads-command-json)
  ((json
    :initarg :json
    :type boolean
    :initform nil
    :documentation "Output in JSON format (--json).
NOTE: As of bd v0.x, import does not actually output JSON stats yet,
so this defaults to nil. When JSON output is implemented, set to t.")
   (clear-duplicate-external-refs
    :initarg :clear-duplicate-external-refs
    :type boolean
    :initform nil
    :documentation "Clear duplicate external_ref values
(--clear-duplicate-external-refs).
Keeps first occurrence.")
   (dedupe-after
    :initarg :dedupe-after
    :type boolean
    :initform nil
    :documentation "Detect and report content duplicates after import
(--dedupe-after).")
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview collision detection without making changes
(--dry-run).")
   (input
    :initarg :input
    :type (or null string)
    :initform nil
    :documentation "Input file (-i, --input).
Default: stdin.")
   (orphan-handling
    :initarg :orphan-handling
    :type (or null string)
    :initform nil
    :documentation "How to handle missing parent issues
(--orphan-handling).
Options: strict, resurrect, skip, allow.
Default: use config or 'allow'.")
   (rename-on-import
    :initarg :rename-on-import
    :type boolean
    :initform nil
    :documentation "Rename imported issues to match database prefix
(--rename-on-import).
Updates all references.")
   (skip-existing
    :initarg :skip-existing
    :type boolean
    :initform nil
    :documentation "Skip existing issues instead of updating them
(-s, --skip-existing).")
   (strict
    :initarg :strict
    :type boolean
    :initform nil
    :documentation "Fail on dependency errors instead of treating them
as warnings (--strict)."))
  :documentation "Represents bd import command.
Import issues from JSON Lines format (one JSON object per line).
NOTE: Import requires direct database access and automatically uses
--no-daemon.")

(cl-defmethod beads-command-line ((command beads-command-import))
  "Build command arguments for import COMMAND (without executable).
Returns list: (\"import\" ...flags... ...global-flags...)."
  (with-slots (clear-duplicate-external-refs dedupe-after dry-run
               input orphan-handling rename-on-import
               skip-existing strict) command
    (let ((cmd-args (list "import"))
          (global-args (cl-call-next-method)))
      ;; Command-specific flags
      (when clear-duplicate-external-refs
        (setq cmd-args (append cmd-args
                               (list "--clear-duplicate-external-refs"))))
      (when dedupe-after
        (setq cmd-args (append cmd-args (list "--dedupe-after"))))
      (when dry-run
        (setq cmd-args (append cmd-args (list "--dry-run"))))
      (when input
        (setq cmd-args (append cmd-args (list "-i" input))))
      (when orphan-handling
        (setq cmd-args (append cmd-args
                               (list "--orphan-handling" orphan-handling))))
      (when rename-on-import
        (setq cmd-args (append cmd-args (list "--rename-on-import"))))
      (when skip-existing
        (setq cmd-args (append cmd-args (list "-s"))))
      (when strict
        (setq cmd-args (append cmd-args (list "--strict"))))
      ;; Global args (includes --json if enabled)
      (setq cmd-args (append cmd-args global-args))
      cmd-args)))

(cl-defmethod beads-command-validate ((command beads-command-import))
  "Validate import COMMAND.
Returns error string or nil if valid."
  (with-slots (orphan-handling) command
    (cond
     ;; Validate orphan-handling if provided
     ((and orphan-handling
           (not (member orphan-handling
                        '("strict" "resurrect" "skip" "allow"))))
      (format "Invalid orphan-handling: %s (must be one of: strict, \
resurrect, skip, allow)"
              orphan-handling))
     ;; Otherwise valid
     (t nil))))

(cl-defmethod beads-command-execute ((command beads-command-import))
  "Execute import COMMAND and return result.
Unlike most commands, bd import writes JSON stats to stderr, not stdout.
When :json is nil, returns (EXIT-CODE STDOUT STDERR).
When :json is t, returns parsed JSON from STDERR (not STDOUT).
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  ;; Import automatically uses --no-daemon, ensure it's set
  (with-slots (no-daemon) command
    (setf no-daemon t))

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
              (with-slots (json) command
                (if (not json)
                    ;; No JSON parsing, return raw output
                    (list exit-code stdout stderr)
                  ;; Parse JSON from stderr (not stdout!)
                  (condition-case err
                      (let* ((json-object-type 'alist)
                             (json-array-type 'vector)
                             (json-key-type 'symbol)
                             (parsed-json (json-read-from-string stderr)))
                        (list exit-code parsed-json stderr))
                    (error
                     (signal 'beads-json-parse-error
                             (list (format "Failed to parse JSON from stderr: %s"
                                           (error-message-string err))
                                   :exit-code exit-code
                                   :stdout stdout
                                   :stderr stderr
                                   :parse-error err))))))
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
    (or
     ;; Can't use --priority with --priority-min/max
     (and priority (or priority-max priority-min)
          "Cannot use --priority with --priority-min/--priority-max")
     ;; Can't use --assignee with --no-assignee
     (and assignee no-assignee
          "Cannot use both --assignee and --no-assignee")
     ;; Can't use --label/--label-any with --no-labels
     (and no-labels (or label label-any)
          "Cannot use --label/--label-any with --no-labels")
     ;; Validate priority range
     (and priority (not (<= 0 priority 4))
          "Priority must be between 0 and 4")
     (and priority-min (not (<= 0 priority-min 4))
          "Priority-min must be between 0 and 4")
     (and priority-max (not (<= 0 priority-max 4))
          "Priority-max must be between 0 and 4")
     ;; Validate list content types
     (beads-command--validate-string-list label "label")
     (beads-command--validate-string-list label-any "label-any"))))

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
    :type (or null string integer)
    :initform nil
    :documentation "Priority (-p, --priority).
Values: 0-4 or P0-P4 (0=highest). Default: '2'.
Accepts both integer (1) and string (\"1\" or \"P1\") formats.")
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
        (setq args (append args (list "--priority"
                                      (beads-command--priority-to-string priority)))))
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
  (with-slots (title file deps labels issue-type priority) command
    (or
     ;; Must have either title or file
     (and (not title) (not file)
          "Must provide either title or --file")
     ;; Can't use both title and file
     (and title file
          "Cannot use both title and --file")
     ;; Title validation (if provided, cannot be empty)
     (and title (string-empty-p (string-trim title))
          "Title cannot be empty")
     ;; Type validation
     (and issue-type
          (not (member issue-type '("bug" "feature" "task" "epic" "chore")))
          "Type must be one of: bug, feature, task, epic, chore")
     ;; Priority validation (accepts number or string)
     (and priority
          (let ((p (if (stringp priority) (string-to-number priority) priority)))
            (not (and (numberp p) (>= p 0) (<= p 4))))
          "Priority must be a number between 0 and 4")
     ;; Dependency format validation
     (and deps
          (not (seq-every-p
                (lambda (dep)
                  (string-match-p "^[a-z-]+:[A-Za-z0-9._-]+$" dep))
                deps))
          "Dependencies must be in format: type:issue-id")
     ;; Validate list content types
     (beads-command--validate-string-list deps "deps")
     (beads-command--validate-string-list labels "labels"))))

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

;;; Epic Commands

(defclass beads-command-epic-close-eligible (beads-command-json)
  ((dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview what would be closed without making changes
(--dry-run)."))
  :documentation "Represents bd epic close-eligible command.
Close epics where all children are complete.
When executed with :json t, returns list of closed/eligible epic IDs.")

(cl-defmethod beads-command-line ((command
                                   beads-command-epic-close-eligible))
  "Build command arguments for epic close-eligible COMMAND.
Returns list: (\"epic\" \"close-eligible\" ...global-flags... ...flags...)."
  (with-slots (dry-run) command
    (let ((args (list "epic" "close-eligible"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Boolean flag
      (when dry-run
        (setq args (append args (list "--dry-run"))))

      args)))

(cl-defmethod beads-command-validate ((_command
                                       beads-command-epic-close-eligible))
  "Validate epic close-eligible COMMAND.
Default implementation returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute ((command
                                      beads-command-epic-close-eligible))
  "Execute epic close-eligible COMMAND and return result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns parsed JSON (list of epic IDs or status objects).
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result)))
        ;; Return parsed JSON directly (no conversion needed)
        (if (zerop exit-code)
            parsed-json
          ;; Non-zero exit code: parent already signaled error
          result)))))

(defclass beads-command-epic-status (beads-command-json)
  ((eligible-only
    :initarg :eligible-only
    :type boolean
    :initform nil
    :documentation "Show only epics eligible for closure
(--eligible-only)."))
  :documentation "Represents bd epic status command.
Show epic completion status for all epics or only eligible ones.
When executed with :json t, returns list of epic status objects.")

(cl-defmethod beads-command-line ((command beads-command-epic-status))
  "Build command arguments for epic status COMMAND.
Returns list: (\"epic\" \"status\" ...global-flags... ...flags...)."
  (with-slots (eligible-only) command
    (let ((args (list "epic" "status"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Boolean flag
      (when eligible-only
        (setq args (append args (list "--eligible-only"))))

      args)))

(cl-defmethod beads-command-validate ((_command beads-command-epic-status))
  "Validate epic status COMMAND.
Default implementation returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute ((command beads-command-epic-status))
  "Execute epic status COMMAND and return result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns list of beads-epic-status instances.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON, then convert
      ;; to beads-epic-status instance(s)
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result))
             (stderr (nth 2 result)))
        ;; Convert JSON to beads-epic-status instance(s)
        (if (zerop exit-code)
            (condition-case err
                (cond
                 ;; Empty result (nil)
                 ((null parsed-json)
                  nil)
                 ;; JSON array of epic status objects
                 ((eq (type-of parsed-json) 'vector)
                  (mapcar #'beads-epic-status-from-json
                          (append parsed-json nil)))
                 ;; Single epic status (unlikely, but handle it)
                 ((eq (type-of parsed-json) 'cons)
                  (list (beads-epic-status-from-json parsed-json)))
                 ;; Unexpected JSON structure
                 (t
                  (signal 'beads-json-parse-error
                          (list "Unexpected JSON structure from bd epic status"
                                :exit-code exit-code
                                :parsed-json parsed-json
                                :stderr stderr))))
              (error
               (signal 'beads-json-parse-error
                       (list (format "Failed to create beads-epic-status instance: %s"
                                    (error-message-string err))
                             :exit-code exit-code
                             :parsed-json parsed-json
                             :stderr stderr
                             :parse-error err))))
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Show Command

(defclass beads-command-show (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs to show (positional arguments).
Example: '(\"bd-1\" \"bd-2\")"))
  :documentation "Represents bd show command.
Shows detailed information for one or more issues.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided).")

(cl-defmethod beads-command-line ((command beads-command-show))
  "Build command arguments for show COMMAND (without executable).
Returns list: (\"show\" ...global-flags... ...issue-ids...)."
  (with-slots (issue-ids) command
    (let ((args (list "show"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Append issue IDs (positional arguments)
      (when issue-ids
        (setq args (append args issue-ids)))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-show))
  "Validate show COMMAND.
Checks that at least one issue ID is provided.
Returns error string or nil if valid."
  (with-slots (issue-ids) command
    (or
     ;; Must have at least one issue ID
     (and (or (null issue-ids) (zerop (length issue-ids)))
          "Must provide at least one issue ID")
     ;; Validate list content types
     (beads-command--validate-string-list issue-ids "issue-ids"))))

(cl-defmethod beads-command-execute ((command beads-command-show))
  "Execute show COMMAND and return issue(s).
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns beads-issue instance (or list of instances
when multiple IDs provided).
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json issue-ids) command
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
        ;; Note: bd show always returns an array in JSON mode,
        ;; even for a single issue ID
        (if (zerop exit-code)
            (condition-case err
                (if (eq (type-of parsed-json) 'vector)
                    ;; bd show returns array - convert to issue objects
                    (let ((issues (mapcar #'beads-issue-from-json
                                         (append parsed-json nil))))
                      ;; Return single issue if only one ID, list otherwise
                      (if (= (length issue-ids) 1)
                          (car issues)
                        issues))
                  ;; Unexpected JSON structure
                  (signal 'beads-json-parse-error
                          (list "Unexpected JSON structure from bd show"
                                :exit-code exit-code
                                :parsed-json parsed-json
                                :stderr stderr)))
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

;;; Update Command

(defclass beads-command-update (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs to update (positional arguments).
Example: '(\"bd-1\" \"bd-2\")")
   (acceptance
    :initarg :acceptance
    :type (or null string)
    :initform nil
    :documentation "Acceptance criteria (--acceptance).")
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "New assignee (-a, --assignee).")
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
   (notes
    :initarg :notes
    :type (or null string)
    :initform nil
    :documentation "Additional notes (--notes).")
   (priority
    :initarg :priority
    :type (or null string integer)
    :initform nil
    :documentation "New priority (-p, --priority).
Values: 0-4 or P0-P4.
Accepts both integer (1) and string (\"1\" or \"P1\") formats.")
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "New status (-s, --status).
Values: open, in_progress, blocked, closed.")
   (title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "New title (--title)."))
  :documentation "Represents bd update command.
Updates one or more issues with new field values.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided).")

(cl-defmethod beads-command-line ((command beads-command-update))
  "Build command arguments for update COMMAND (without executable).
Returns list: (\"update\" ...global-flags... ...issue-ids... ...flags...)."
  (with-slots (issue-ids acceptance assignee description design
                         external-ref notes priority status title) command
    (let ((args (list "update"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Append issue IDs (positional arguments)
      (when issue-ids
        (setq args (append args issue-ids)))

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
      (when notes
        (setq args (append args (list "--notes" notes))))
      (when priority
        (setq args (append args (list "--priority"
                                      (beads-command--priority-to-string priority)))))
      (when status
        (setq args (append args (list "--status" status))))
      (when title
        (setq args (append args (list "--title" title))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-update))
  "Validate update COMMAND.
Checks that at least one issue ID and one field to update is provided.
Returns error string or nil if valid."
  (with-slots (issue-ids acceptance assignee description design
                         external-ref notes priority status title) command
    (or
     ;; Must have at least one issue ID
     (and (or (null issue-ids) (zerop (length issue-ids)))
          "Must provide at least one issue ID")
     ;; Must have at least one field to update
     (and (not (or acceptance assignee description design external-ref
                   notes priority status title))
          "Must provide at least one field to update")
     ;; Validate list content types
     (beads-command--validate-string-list issue-ids "issue-ids"))))

(cl-defmethod beads-command-execute ((command beads-command-update))
  "Execute update COMMAND and return updated issue(s).
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns beads-issue instance (or list of instances
when multiple IDs provided).
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json issue-ids) command
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
        ;; Note: bd update always returns an array in JSON mode,
        ;; even for a single issue ID
        (if (zerop exit-code)
            (condition-case err
                (if (eq (type-of parsed-json) 'vector)
                    ;; bd update returns array - convert to issue objects
                    (let ((issues (mapcar #'beads-issue-from-json
                                         (append parsed-json nil))))
                      ;; Return single issue if only one ID, list otherwise
                      (if (= (length issue-ids) 1)
                          (car issues)
                        issues))
                  ;; Unexpected JSON structure
                  (signal 'beads-json-parse-error
                          (list "Unexpected JSON structure from bd update"
                                :exit-code exit-code
                                :parsed-json parsed-json
                                :stderr stderr)))
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

;;; Close Command

(defclass beads-command-close (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs to close (positional arguments).
Example: '(\"bd-1\" \"bd-2\")")
   (reason
    :initarg :reason
    :type (or null string)
    :initform nil
    :documentation "Reason for closing (-r, --reason).
Required field."))
  :documentation "Represents bd close command.
Closes one or more issues with a required reason.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided).")

(cl-defmethod beads-command-line ((command beads-command-close))
  "Build command arguments for close COMMAND (without executable).
Returns list: (\"close\" ...global-flags... ...issue-ids... --reason ...)."
  (with-slots (issue-ids reason) command
    (let ((args (list "close"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Append issue IDs (positional arguments)
      (when issue-ids
        (setq args (append args issue-ids)))

      ;; Append reason (required)
      (when reason
        (setq args (append args (list "--reason" reason))))

      args)))

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

(cl-defmethod beads-command-execute ((command beads-command-close))
  "Execute close COMMAND and return closed issue(s).
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns beads-issue instance (or list of instances
when multiple IDs provided).
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json issue-ids) command
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
        ;; Note: bd close always returns an array in JSON mode,
        ;; even for a single issue ID
        (if (zerop exit-code)
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
                                :exit-code exit-code
                                :parsed-json parsed-json
                                :stderr stderr)))
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

;;; Reopen Command

(defclass beads-command-reopen (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs to reopen (positional arguments).
Example: '(\"bd-1\" \"bd-2\")")
   (reason
    :initarg :reason
    :type (or null string)
    :initform nil
    :documentation "Optional reason for reopening (-r, --reason)."))
  :documentation "Represents bd reopen command.
Reopens one or more closed issues with an optional reason.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided).")

(cl-defmethod beads-command-line ((command beads-command-reopen))
  "Build command arguments for reopen COMMAND (without executable).
Returns list: (\"reopen\" ...global-flags... ...issue-ids... --reason ...)."
  (with-slots (issue-ids reason) command
    (let ((args (list "reopen"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Append issue IDs (positional arguments)
      (when issue-ids
        (setq args (append args issue-ids)))

      ;; Append reason (optional)
      (when reason
        (setq args (append args (list "--reason" reason))))

      args)))

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

(cl-defmethod beads-command-execute ((command beads-command-reopen))
  "Execute reopen COMMAND and return reopened issue(s).
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns beads-issue instance (or list of instances
when multiple IDs provided).
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json issue-ids) command
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
        ;; Note: bd reopen always returns an array in JSON mode,
        ;; even for a single issue ID
        (if (zerop exit-code)
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
                                :exit-code exit-code
                                :parsed-json parsed-json
                                :stderr stderr)))
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

;;; Delete Command

(defclass beads-command-delete (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to delete (positional argument).
Example: \"bd-1\"")
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force deletion without preview (--force flag)."))
  :documentation "Represents bd delete command.
Deletes an issue with optional --force flag.
When executed, returns (EXIT-CODE PARSED-JSON STDERR) tuple where
PARSED-JSON is the deleted issue object.")

(cl-defmethod beads-command-line ((command beads-command-delete))
  "Build command arguments for delete COMMAND (without executable).
Returns list: (\"delete\" ...global-flags... issue-id [--force])."
  (with-slots (issue-id force) command
    (let ((args (list "delete"))
          (global-args (cl-call-next-method)))
      ;; Append global flags
      (setq args (append args global-args))

      ;; Append issue ID (positional argument)
      (when issue-id
        (setq args (append args (list issue-id))))

      ;; Append --force flag if set
      (when force
        (setq args (append args (list "--force"))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-delete))
  "Validate delete COMMAND.
Checks that issue ID is provided.
Returns error string or nil if valid."
  (with-slots (issue-id) command
    (when (or (null issue-id) (string-empty-p issue-id))
      "Must provide an issue ID")))

(defun beads-command-delete! (&rest args)
  "Create and execute a beads-command-delete with ARGS.
Returns (EXIT-CODE STDOUT STDERR) tuple.
See `beads-command-delete' for available arguments."
  (beads-command-execute (apply #'beads-command-delete args)))

;;; Ready Command

(defclass beads-command-ready (beads-command-json)
  ((assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee (-a, --assignee).")
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
    :documentation "Maximum issues to show (-n, --limit).
Default: 10.")
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Filter by priority (-p, --priority).
Values: 0-4.")
   (sort
    :initarg :sort
    :type (or null string)
    :initform nil
    :documentation "Sort policy (-s, --sort).
Values: hybrid (default), priority, oldest."))
  :documentation "Represents bd ready command.
Shows ready work (no blockers, open or in-progress).
When executed with :json t, returns list of beads-issue instances.")

(cl-defmethod beads-command-line ((command beads-command-ready))
  "Build command arguments for ready COMMAND (without executable).
Returns list: (\"ready\" ...global-flags... ...flags...)."
  (with-slots (assignee label label-any limit priority sort) command
    (let ((args (list "ready"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; String options
      (when assignee
        (setq args (append args (list "--assignee" assignee))))
      (when sort
        (setq args (append args (list "--sort" sort))))

      ;; Integer options
      (when limit
        (setq args (append args (list "--limit" (number-to-string limit)))))
      (when priority
        (setq args (append args (list "--priority"
                                      (number-to-string priority)))))

      ;; List options (multiple values)
      (when label
        (dolist (lbl label)
          (setq args (append args (list "--label" lbl)))))
      (when label-any
        (dolist (lbl label-any)
          (setq args (append args (list "--label-any" lbl)))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-ready))
  "Validate ready COMMAND.
Checks for valid sort and priority values.
Returns error string or nil if valid."
  (with-slots (sort priority label label-any) command
    (or
     ;; Validate sort value
     (and sort (not (member sort '("hybrid" "priority" "oldest")))
          "Sort must be one of: hybrid, priority, oldest")
     ;; Validate priority range
     (and priority (not (<= 0 priority 4))
          "Priority must be between 0 and 4")
     ;; Validate list content types
     (beads-command--validate-string-list label "label")
     (beads-command--validate-string-list label-any "label-any"))))

(cl-defmethod beads-command-execute ((command beads-command-ready))
  "Execute ready COMMAND and return list of beads-issue instances.
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

;;; Blocked Command

(defclass beads-command-blocked (beads-command-json)
  ()
  :documentation "Represents bd blocked command.
Shows blocked issues (issues with unresolved blockers).
When executed with :json t, returns list of beads-issue instances.")

(cl-defmethod beads-command-line ((_command beads-command-blocked))
  "Build command arguments for blocked COMMAND (without executable).
Returns list: (\"blocked\" ...global-flags...)."
  (let ((args (list "blocked"))
        (global-args (cl-call-next-method)))
    ;; Append global flags (includes --json if enabled)
    (setq args (append args global-args))
    args))

(cl-defmethod beads-command-validate ((_command beads-command-blocked))
  "Validate blocked COMMAND.
Default implementation returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute ((command beads-command-blocked))
  "Execute blocked COMMAND and return list of beads-blocked-issue instances.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns list of beads-blocked-issue instances.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON, then convert
      ;; to beads-blocked-issue instances using beads-blocked-issue-from-json
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result))
             (stderr (nth 2 result)))
        ;; Convert JSON array to list of beads-blocked-issue instances
        (if (zerop exit-code)
            (condition-case err
                (let ((issues (mapcar #'beads-blocked-issue-from-json
                                     (append parsed-json nil))))
                  issues)
              (error
               (signal 'beads-json-parse-error
                       (list (format "Failed to create beads-blocked-issue instances: %s"
                                    (error-message-string err))
                             :exit-code exit-code
                             :parsed-json parsed-json
                             :stderr stderr
                             :parse-error err))))
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Stats Command

(defclass beads-command-stats (beads-command-json)
  ()
  :documentation "Represents bd stats command.
Shows statistics about the issue database.
When executed with :json t, returns parsed JSON stats object.")

(cl-defmethod beads-command-line ((_command beads-command-stats))
  "Build command arguments for stats COMMAND (without executable).
Returns list: (\"stats\" ...global-flags...)."
  (let ((args (list "stats"))
        (global-args (cl-call-next-method)))
    ;; Append global flags (includes --json if enabled)
    (setq args (append args global-args))
    args))

(cl-defmethod beads-command-validate ((_command beads-command-stats))
  "Validate stats COMMAND.
Default implementation returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute ((command beads-command-stats))
  "Execute stats COMMAND and return parsed JSON result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns parsed JSON stats object.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result)))
        ;; Return parsed JSON directly (no conversion needed)
        (if (zerop exit-code)
            parsed-json
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Dep Add Command

(defclass beads-command-dep-add (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Source issue ID (required positional argument).")
   (depends-on-id
    :initarg :depends-on-id
    :type (or null string)
    :initform nil
    :documentation "Target dependency issue ID (required positional argument).")
   (dep-type
    :initarg :dep-type
    :type (or null string)
    :initform nil
    :documentation "Dependency type (-t, --type).
Values: blocks, related, parent-child, discovered-from.
Default: blocks."))
  :documentation "Represents bd dep add command.
Adds a dependency relationship between two issues.
When executed with :json t, returns parsed JSON result.")

(cl-defmethod beads-command-line ((command beads-command-dep-add))
  "Build command arguments for dep add COMMAND (without executable).
Returns list: (\"dep\" \"add\" ...global-flags... issue-id depends-on-id ...)."
  (with-slots (issue-id depends-on-id dep-type) command
    (let ((args (list "dep" "add"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Positional arguments
      (when issue-id
        (setq args (append args (list issue-id))))
      (when depends-on-id
        (setq args (append args (list depends-on-id))))

      ;; Type flag
      (when dep-type
        (setq args (append args (list "--type" dep-type))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-dep-add))
  "Validate dep add COMMAND.
Checks that both issue IDs are provided and type is valid.
Returns error string or nil if valid."
  (with-slots (issue-id depends-on-id dep-type) command
    (or
     ;; Must have issue-id
     (and (or (null issue-id) (string-empty-p issue-id))
          "Must provide issue-id")
     ;; Must have depends-on-id
     (and (or (null depends-on-id) (string-empty-p depends-on-id))
          "Must provide depends-on-id")
     ;; Validate dep-type if provided
     (and dep-type
          (not (member dep-type '("blocks" "related" "parent-child" "discovered-from")))
          "Type must be one of: blocks, related, parent-child, discovered-from"))))

(cl-defmethod beads-command-execute ((command beads-command-dep-add))
  "Execute dep add COMMAND and return result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns parsed JSON result.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result)))
        ;; Return parsed JSON directly (no conversion needed)
        (if (zerop exit-code)
            parsed-json
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Dep Remove Command

(defclass beads-command-dep-remove (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Source issue ID (required positional argument).")
   (depends-on-id
    :initarg :depends-on-id
    :type (or null string)
    :initform nil
    :documentation "Target dependency issue ID (required positional argument)."))
  :documentation "Represents bd dep remove command.
Removes a dependency relationship between two issues.
When executed with :json t, returns parsed JSON result.")

(cl-defmethod beads-command-line ((command beads-command-dep-remove))
  "Build command arguments for dep remove COMMAND (without executable).
Returns list: (\"dep\" \"remove\" ...global-flags... issue-id depends-on-id)."
  (with-slots (issue-id depends-on-id) command
    (let ((args (list "dep" "remove"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Positional arguments
      (when issue-id
        (setq args (append args (list issue-id))))
      (when depends-on-id
        (setq args (append args (list depends-on-id))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-dep-remove))
  "Validate dep remove COMMAND.
Checks that both issue IDs are provided.
Returns error string or nil if valid."
  (with-slots (issue-id depends-on-id) command
    (or
     ;; Must have issue-id
     (and (or (null issue-id) (string-empty-p issue-id))
          "Must provide issue-id")
     ;; Must have depends-on-id
     (and (or (null depends-on-id) (string-empty-p depends-on-id))
          "Must provide depends-on-id"))))

(cl-defmethod beads-command-execute ((command beads-command-dep-remove))
  "Execute dep remove COMMAND and return result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns parsed JSON result.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result)))
        ;; Return parsed JSON directly (no conversion needed)
        (if (zerop exit-code)
            parsed-json
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Dep Tree Command

(defclass beads-command-dep-tree (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to show tree for (required positional argument).")
   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Output format (--format).
Value: mermaid for Mermaid.js flowchart.")
   (max-depth
    :initarg :max-depth
    :type (or null integer)
    :initform nil
    :documentation "Maximum tree depth (-d, --max-depth).
Safety limit, default: 50.")
   (reverse
    :initarg :reverse
    :type boolean
    :initform nil
    :documentation "Show dependent tree instead of dependency tree (--reverse).
Shows what was discovered from this instead of what blocks this.")
   (show-all-paths
    :initarg :show-all-paths
    :type boolean
    :initform nil
    :documentation "Show all paths to nodes (--show-all-paths).
No deduplication for diamond dependencies."))
  :documentation "Represents bd dep tree command.
Shows dependency tree for an issue.
When executed with :json t, returns parsed JSON tree structure.")

(cl-defmethod beads-command-line ((command beads-command-dep-tree))
  "Build command arguments for dep tree COMMAND (without executable).
Returns list: (\"dep\" \"tree\" ...global-flags... issue-id ...)."
  (with-slots (issue-id format max-depth reverse show-all-paths) command
    (let ((args (list "dep" "tree"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Positional argument
      (when issue-id
        (setq args (append args (list issue-id))))

      ;; String option
      (when format
        (setq args (append args (list "--format" format))))

      ;; Integer option
      (when max-depth
        (setq args (append args (list "--max-depth" (number-to-string max-depth)))))

      ;; Boolean flags
      (when reverse
        (setq args (append args (list "--reverse"))))
      (when show-all-paths
        (setq args (append args (list "--show-all-paths"))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-dep-tree))
  "Validate dep tree COMMAND.
Checks that issue ID is provided and max-depth is positive.
Returns error string or nil if valid."
  (with-slots (issue-id max-depth) command
    (or
     ;; Must have issue-id
     (and (or (null issue-id) (string-empty-p issue-id))
          "Must provide issue-id")
     ;; Validate max-depth if provided
     (and max-depth (< max-depth 1)
          "Max depth must be positive"))))

(cl-defmethod beads-command-execute ((command beads-command-dep-tree))
  "Execute dep tree COMMAND and return result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns parsed JSON tree structure.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result)))
        ;; Return parsed JSON directly (no conversion needed)
        (if (zerop exit-code)
            parsed-json
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Dep Cycles Command

(defclass beads-command-dep-cycles (beads-command-json)
  ()
  :documentation "Represents bd dep cycles command.
Detects dependency cycles in the issue database.
When executed with :json t, returns parsed JSON with cycle information.")

(cl-defmethod beads-command-line ((_command beads-command-dep-cycles))
  "Build command arguments for dep cycles COMMAND (without executable).
Returns list: (\"dep\" \"cycles\" ...global-flags...)."
  (let ((args (list "dep" "cycles"))
        (global-args (cl-call-next-method)))
    ;; Append global flags (includes --json if enabled)
    (setq args (append args global-args))
    args))

(cl-defmethod beads-command-validate ((_command beads-command-dep-cycles))
  "Validate dep cycles COMMAND.
Default implementation returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute ((command beads-command-dep-cycles))
  "Execute dep cycles COMMAND and return result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns parsed JSON cycle information.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result)))
        ;; Return parsed JSON directly (no conversion needed)
        (if (zerop exit-code)
            parsed-json
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Label List-All Command

(defclass beads-command-label-list-all (beads-command-json)
  ()
  :documentation "Represents bd label list-all command.
Shows all labels with usage counts.
When executed with :json t, returns parsed JSON label list.")

(cl-defmethod beads-command-line ((_command beads-command-label-list-all))
  "Build command arguments for label list-all COMMAND (without executable).
Returns list: (\"label\" \"list-all\" ...global-flags...)."
  (let ((args (list "label" "list-all"))
        (global-args (cl-call-next-method)))
    ;; Append global flags (includes --json if enabled)
    (setq args (append args global-args))
    args))

(cl-defmethod beads-command-validate ((_command beads-command-label-list-all))
  "Validate label list-all COMMAND.
Default implementation returns nil (valid)."
  nil)

(cl-defmethod beads-command-execute ((command beads-command-label-list-all))
  "Execute label list-all COMMAND and return result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns parsed JSON array of label objects.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result)))
        ;; Return parsed JSON directly (no conversion needed)
        (if (zerop exit-code)
            parsed-json
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Label Add Command

(defclass beads-command-label-add (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs (positional arguments).
Example: '(\"bd-1\" \"bd-2\")")
   (label
    :initarg :label
    :type (or null string)
    :initform nil
    :documentation "Label name to add (positional argument)."))
  :documentation "Represents bd label add command.
Adds a label to one or more issues.
When executed with :json t, returns parsed JSON result.")

(cl-defmethod beads-command-line ((command beads-command-label-add))
  "Build command arguments for label add COMMAND (without executable).
Returns list: (\"label\" \"add\" ...global-flags... ...issue-ids... label)."
  (with-slots (issue-ids label) command
    (let ((args (list "label" "add"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Append issue IDs (positional arguments)
      (when issue-ids
        (setq args (append args issue-ids)))

      ;; Append label (positional argument)
      (when label
        (setq args (append args (list label))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-label-add))
  "Validate label add COMMAND.
Checks that at least one issue ID and a label are provided.
Returns error string or nil if valid."
  (with-slots (issue-ids label) command
    (or
     ;; Must have at least one issue ID
     (and (or (null issue-ids) (zerop (length issue-ids)))
          "Must provide at least one issue ID")
     ;; Must have a label
     (and (or (null label) (string-empty-p label))
          "Must provide a label name")
     ;; Validate list content types
     (beads-command--validate-string-list issue-ids "issue-ids"))))

(cl-defmethod beads-command-execute ((command beads-command-label-add))
  "Execute label add COMMAND and return result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns parsed JSON result.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result)))
        ;; Return parsed JSON directly (no conversion needed)
        (if (zerop exit-code)
            parsed-json
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Label Remove Command

(defclass beads-command-label-remove (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs (positional arguments).
Example: '(\"bd-1\" \"bd-2\")")
   (label
    :initarg :label
    :type (or null string)
    :initform nil
    :documentation "Label name to remove (positional argument)."))
  :documentation "Represents bd label remove command.
Removes a label from one or more issues.
When executed with :json t, returns parsed JSON result.")

(cl-defmethod beads-command-line ((command beads-command-label-remove))
  "Build command arguments for label remove COMMAND (without executable).
Returns list: (\"label\" \"remove\" ...global-flags... ...issue-ids... label)."
  (with-slots (issue-ids label) command
    (let ((args (list "label" "remove"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Append issue IDs (positional arguments)
      (when issue-ids
        (setq args (append args issue-ids)))

      ;; Append label (positional argument)
      (when label
        (setq args (append args (list label))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-label-remove))
  "Validate label remove COMMAND.
Checks that at least one issue ID and a label are provided.
Returns error string or nil if valid."
  (with-slots (issue-ids label) command
    (or
     ;; Must have at least one issue ID
     (and (or (null issue-ids) (zerop (length issue-ids)))
          "Must provide at least one issue ID")
     ;; Must have a label
     (and (or (null label) (string-empty-p label))
          "Must provide a label name")
     ;; Validate list content types
     (beads-command--validate-string-list issue-ids "issue-ids"))))

(cl-defmethod beads-command-execute ((command beads-command-label-remove))
  "Execute label remove COMMAND and return result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns parsed JSON result.
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result)))
        ;; Return parsed JSON directly (no conversion needed)
        (if (zerop exit-code)
            parsed-json
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Label List Command

(defclass beads-command-label-list (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to list labels for (positional argument).
Example: \"bd-1\""))
  :documentation "Represents bd label list command.
Lists all labels for a specific issue.
When executed with :json t, returns parsed JSON result.")

(cl-defmethod beads-command-line ((command beads-command-label-list))
  "Build command arguments for label list COMMAND (without executable).
Returns list: (\"label\" \"list\" ...global-flags... issue-id)."
  (with-slots (issue-id) command
    (let ((args (list "label" "list"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Append issue ID (positional argument)
      (when issue-id
        (setq args (append args (list issue-id))))

      args)))

(cl-defmethod beads-command-validate ((command beads-command-label-list))
  "Validate label list COMMAND.
Checks that an issue ID is provided.
Returns error string or nil if valid."
  (with-slots (issue-id) command
    ;; Must have an issue ID
    (and (or (null issue-id) (string-empty-p issue-id))
         "Must provide an issue ID")))

(cl-defmethod beads-command-execute ((command beads-command-label-list))
  "Execute label list COMMAND and return result.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) like parent.
When :json is t, returns parsed JSON result (array of label strings).
Signals beads-validation-error, beads-command-error, or
beads-json-parse-error on failure."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; JSON execution: call parent to get parsed JSON
      (let* ((result (cl-call-next-method))
             (exit-code (nth 0 result))
             (parsed-json (nth 1 result)))
        ;; Return parsed JSON directly (no conversion needed)
        (if (zerop exit-code)
            parsed-json
          ;; Non-zero exit code: parent already signaled error
          result)))))

;;; Utility Functions

(defun beads-command--priority-to-string (priority)
  "Convert PRIORITY integer to string for bd command line.
If PRIORITY is already a string, return it unchanged.
Returns nil if PRIORITY is nil."
  (cond
   ((null priority) nil)
   ((stringp priority) priority)
   ((integerp priority) (number-to-string priority))
   (t (error "Priority must be integer, string, or nil"))))

(defun beads-command--priority-to-integer (priority)
  "Convert PRIORITY to integer for validation and comparison.
If PRIORITY is already an integer, return it unchanged.
If PRIORITY is a string like \"2\" or \"P2\", convert to integer.
Returns nil if PRIORITY is nil.
Signals error if PRIORITY cannot be converted."
  (cond
   ((null priority) nil)
   ((integerp priority) priority)
   ((stringp priority)
    (if (string-prefix-p "P" priority)
        ;; Handle "P0", "P1", etc. format
        (string-to-number (substring priority 1))
      ;; Handle "0", "1", etc. format
      (string-to-number priority)))
   (t (error "Priority must be integer, string, or nil"))))

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

(defun beads-command-quickstart! (&rest args)
  "Create and execute a beads-command-quickstart with ARGS.
Returns (EXIT-CODE STDOUT STDERR) tuple.
STDOUT contains the quickstart guide text.
See `beads-command-quickstart' for available arguments."
  (beads-command-execute (apply #'beads-command-quickstart args)))

(defun beads-command-export! (&rest args)
  "Create and execute a beads-command-export with ARGS.
When :json is t (default), returns parsed JSON export statistics.
When :json is nil, returns (EXIT-CODE STDOUT STDERR) tuple.
See `beads-command-export' for available arguments."
  (beads-command-execute (apply #'beads-command-export args)))

(defun beads-command-import! (&rest args)
  "Create and execute a beads-command-import with ARGS.
Returns (EXIT-CODE STDOUT STDERR) tuple.
NOTE: bd import does not currently output JSON stats, so :json defaults to nil.
See `beads-command-import' for available arguments."
  (beads-command-execute (apply #'beads-command-import args)))

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

(defun beads-command-epic-close-eligible! (&rest args)
  "Create and execute a beads-command-epic-close-eligible with ARGS.
Returns the parsed JSON result (list of epic IDs or status objects).
See `beads-command-epic-close-eligible' for available arguments."
  (beads-command-execute (apply #'beads-command-epic-close-eligible args)))

(defun beads-command-epic-status! (&rest args)
  "Create and execute a beads-command-epic-status with ARGS.
Returns the parsed JSON result (list of epic status objects).
See `beads-command-epic-status' for available arguments."
  (beads-command-execute (apply #'beads-command-epic-status args)))

(defun beads-command-show! (&rest args)
  "Create and execute a beads-command-show with ARGS.
Returns the parsed issue object (or list of issue objects).
See `beads-command-show' for available arguments."
  (beads-command-execute (apply #'beads-command-show args)))

(defun beads-command-update! (&rest args)
  "Create and execute a beads-command-update with ARGS.
Returns the parsed issue object (or list of issue objects).
See `beads-command-update' for available arguments."
  (beads-command-execute (apply #'beads-command-update args)))

(defun beads-command-close! (&rest args)
  "Create and execute a beads-command-close with ARGS.
Returns the parsed issue object (or list of issue objects).
See `beads-command-close' for available arguments."
  (beads-command-execute (apply #'beads-command-close args)))

(defun beads-command-reopen! (&rest args)
  "Create and execute a beads-command-reopen with ARGS.
Returns the parsed issue object (or list of issue objects).
See `beads-command-reopen' for available arguments."
  (beads-command-execute (apply #'beads-command-reopen args)))

(defun beads-command-ready! (&rest args)
  "Create and execute a beads-command-ready with ARGS.
Returns a list of parsed issue objects.
See `beads-command-ready' for available arguments."
  (beads-command-execute (apply #'beads-command-ready args)))

(defun beads-command-blocked! (&rest args)
  "Create and execute a beads-command-blocked with ARGS.
Returns a list of parsed issue objects.
See `beads-command-blocked' for available arguments."
  (beads-command-execute (apply #'beads-command-blocked args)))

(defun beads-command-stats! (&rest args)
  "Create and execute a beads-command-stats with ARGS.
Returns the parsed JSON stats object.
See `beads-command-stats' for available arguments."
  (beads-command-execute (apply #'beads-command-stats args)))

(defun beads-command-dep-add! (&rest args)
  "Create and execute a beads-command-dep-add with ARGS.
Returns the parsed JSON result.
See `beads-command-dep-add' for available arguments."
  (beads-command-execute (apply #'beads-command-dep-add args)))

(defun beads-command-dep-remove! (&rest args)
  "Create and execute a beads-command-dep-remove with ARGS.
Returns the parsed JSON result.
See `beads-command-dep-remove' for available arguments."
  (beads-command-execute (apply #'beads-command-dep-remove args)))

(defun beads-command-dep-tree! (&rest args)
  "Create and execute a beads-command-dep-tree with ARGS.
Returns the parsed JSON tree structure.
See `beads-command-dep-tree' for available arguments."
  (beads-command-execute (apply #'beads-command-dep-tree args)))

(defun beads-command-dep-cycles! (&rest args)
  "Create and execute a beads-command-dep-cycles with ARGS.
Returns the parsed JSON with cycle information.
See `beads-command-dep-cycles' for available arguments."
  (beads-command-execute (apply #'beads-command-dep-cycles args)))

(defun beads-command-label-list-all! (&rest args)
  "Create and execute a beads-command-label-list-all with ARGS.
Returns the parsed JSON array of label objects.
See `beads-command-label-list-all' for available arguments."
  (beads-command-execute (apply #'beads-command-label-list-all args)))

(defun beads-command-label-add! (&rest args)
  "Create and execute a beads-command-label-add with ARGS.
Returns the parsed JSON result.
See `beads-command-label-add' for available arguments."
  (beads-command-execute (apply #'beads-command-label-add args)))

(defun beads-command-label-remove! (&rest args)
  "Create and execute a beads-command-label-remove with ARGS.
Returns the parsed JSON result.
See `beads-command-label-remove' for available arguments."
  (beads-command-execute (apply #'beads-command-label-remove args)))

(defun beads-command-label-list! (&rest args)
  "Create and execute a beads-command-label-list with ARGS.
Returns the parsed JSON array of label strings.
See `beads-command-label-list' for available arguments."
  (beads-command-execute (apply #'beads-command-label-list args)))

(provide 'beads-command)
;;; beads-command.el ends here
