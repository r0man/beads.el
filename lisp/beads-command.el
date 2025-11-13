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
;;     - beads-command-create: bd create command (future)
;;     - beads-command-update: bd update command (future)
;;     - beads-command-list: bd list command (future)
;;   - beads-command-init: bd init command (no JSON support)
;;
;; Each command class:
;; - Has slots for all applicable flags
;; - Implements beads-command-execute method
;; - Provides beads-command-to-args for building CLI arguments
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
;;   ;; Build command arguments without execution
;;   (beads-command-to-args cmd)
;;   ;; => ("init" "--prefix" "myproject" "--branch" "main" "--quiet")

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'json)

;; Forward declarations
(defvar beads-executable)
(declare-function beads--log "beads")

;;; Error Definitions

(define-error 'beads-error
  "Beads error"
  'error)

(define-error 'beads-command-error
  "Beads command execution error"
  'beads-error)

(define-error 'beads-json-parse-error
  "Beads JSON parse error"
  'beads-error)

(define-error 'beads-validation-error
  "Beads command validation error"
  'beads-error)

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

(cl-defgeneric beads-command-to-args (command)
  "Build command-line arguments from COMMAND object.
Returns a list of strings suitable for passing to beads--run-command.
Includes global flags and command-specific arguments.")

(cl-defgeneric beads-command-validate (command)
  "Validate COMMAND and return error string or nil if valid.
Subclasses should override to add command-specific validation.")

;;; Base Implementation - Global Flags

(cl-defmethod beads-command-to-args ((command beads-command))
  "Build global flag arguments from COMMAND.
Returns list of global flag strings.
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

  ;; Build command arguments
  (let* ((args (beads-command-to-args command))
         (cmd (cons beads-executable args))
         (cmd-string (mapconcat #'shell-quote-argument cmd " "))
         (stderr-file (make-temp-file "beads-stderr-")))

    (when (fboundp 'beads--log)
      (beads--log 'info "Running: %s" cmd-string)
      (beads--log 'verbose "In directory: %s" default-directory))

    (unwind-protect
        (with-temp-buffer
          (let* ((exit-code (apply #'process-file
                                  (car cmd) nil
                                  (list (current-buffer) stderr-file)
                                  nil (cdr cmd)))
                 (stdout (buffer-string))
                 (stderr (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (buffer-string))))

          (when (fboundp 'beads--log)
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
    :initform nil
    :documentation "Output in JSON format (--json).
Enables machine-readable output."))
  :abstract t
  :documentation "Abstract base class for bd commands that support JSON output.
Inherits from beads-command and adds --json flag support.
Use this as parent class for commands that support --json flag.")

(cl-defmethod beads-command-to-args ((command beads-command-json))
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

(cl-defmethod beads-command-to-args ((command beads-command-init))
  "Build full command arguments for init COMMAND.
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

;;; Utility Functions

(defun beads-command-init-from-options (options)
  "Create beads-command-init from OPTIONS plist.
OPTIONS should be a plist with keys matching slot names:
  :branch, :contributor, :prefix, :quiet, :skip-merge-driver, :team
Plus global flags:
  :actor, :db, :no-auto-flush, :no-auto-import,
  :no-daemon, :no-db, :sandbox"
  (apply #'beads-command-init options))

(provide 'beads-command)
;;; beads-command.el ends here
