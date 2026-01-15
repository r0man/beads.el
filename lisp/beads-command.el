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
(require 'beads-meta)  ; Must be before defclass to install advice
(require 'beads-types)
(require 'beads-error)
(require 'cl-lib)
(require 'json)

;; Forward declarations to avoid circular dependency
;; (beads.el requires beads-command, so we can't require beads here)
(defvar beads-executable)
(defvar beads-list-default-limit)
(declare-function beads--log "beads")
(declare-function beads--find-beads-dir "beads")
(declare-function beads--invalidate-completion-cache "beads")
(declare-function beads-show "beads-show")

;; Forward declarations for optional terminal packages
;; These suppress byte-compiler warnings for optional dependencies
(defvar vterm-shell)
(defvar vterm-buffer-name)
(declare-function vterm "vterm")
(declare-function eat-mode "eat")
(declare-function eat-exec "eat")
(declare-function term-mode "term")
(declare-function term-exec "term")
(declare-function term-char-mode "term")
(defvar compilation-filter-start)
(declare-function ansi-color-apply-on-region "ansi-color")

;;; ============================================================
;;; Command Definition Macro
;;; ============================================================

(defmacro beads-defcommand (name superclasses slots &rest options)
  "Define a beads command class with auto-generated ! convenience function.

NAME is the class name (a symbol like `beads-command-foo').
SUPERCLASSES is the list of parent classes.
SLOTS is the list of slot definitions.
OPTIONS are additional class options like :documentation.

This macro:
1. Defines the class using `defclass'
2. Generates a NAME! convenience function that executes the command
   and returns the data slot

Example:
  (beads-defcommand beads-command-foo (beads-command-json)
    ((name :initarg :name)
     (force :initarg :force :type boolean))
    :documentation \"Foo command.\")

This generates:
  (defclass beads-command-foo ...)
  (defun beads-command-foo! (&rest args) ...)

Usage:
  (beads-command-foo! :name \"test\" :force t)"
  (declare (indent 2))
  (let ((bang-fn (intern (concat (symbol-name name) "!"))))
    `(progn
       (defclass ,name ,superclasses ,slots ,@options)
       (defun ,bang-fn (&rest args)
         ,(format "Execute %s and return result data.\n\nARGS are passed to the constructor." name)
         (oref (beads-command-execute (apply #',name args)) data)))))

;;; Terminal Backend Customization

(defgroup beads-terminal nil
  "Terminal settings for beads command execution."
  :group 'beads
  :prefix "beads-terminal-")

(defcustom beads-terminal-backend nil
  "Backend to use for interactive command execution.

When nil (auto-detect), tries backends in order: vterm, eat, term.
The first available backend is used.

Available backends:
- nil: Auto-detect best available backend (vterm > eat > term).
- `vterm': Use vterm (libvterm-based terminal).
  Fast and feature-complete, requires vterm package.
- `eat': Use Eat (Emulate A Terminal).
  Pure Emacs Lisp terminal, requires eat package.
- `term': Use built-in `term-mode' terminal emulator.
  Full terminal support, no external dependencies.

Note: Compilation mode is not included because it cannot handle
interactive input (prompts, confirmations) that some bd commands
like `bd doctor --fix' may require."
  :type '(choice (const :tag "Auto-detect (vterm > eat > term)" nil)
                 (const :tag "Vterm (requires vterm package)" vterm)
                 (const :tag "Eat (requires eat package)" eat)
                 (const :tag "Term mode (built-in)" term))
  :group 'beads-terminal)

;;; Terminal Backend Implementations

(defun beads-command--run-compile (cmd-string buffer-name default-dir)
  "Run CMD-STRING in compilation buffer BUFFER-NAME from DEFAULT-DIR."
  (let ((default-directory default-dir)
        (process-environment (cons "CLICOLOR_FORCE=1" process-environment)))
    (compile cmd-string)
    (when (get-buffer "*compilation*")
      (with-current-buffer "*compilation*"
        (rename-buffer buffer-name t)
        (add-hook 'compilation-filter-hook
                  #'beads-command--ansi-color-filter nil t)))))

(defun beads-command--run-term (cmd-string buffer-name default-dir)
  "Run CMD-STRING in term buffer BUFFER-NAME from DEFAULT-DIR."
  (require 'term)
  (let* ((default-directory default-dir)
         (process-environment (cons "CLICOLOR_FORCE=1" process-environment))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'term-mode)
        (term-mode))
      (let ((proc (get-buffer-process buf)))
        (when (and proc (process-live-p proc))
          (delete-process proc)))
      (erase-buffer)
      (term-exec buf buffer-name shell-file-name nil
                 (list "-c" (concat "cd " (shell-quote-argument default-dir)
                                    " && " cmd-string "; exit")))
      (term-char-mode))
    (pop-to-buffer buf)))

(defun beads-command--vterm-available-p ()
  "Return non-nil if vterm is available."
  (require 'vterm nil t))

(defun beads-command--run-vterm (cmd-string buffer-name default-dir)
  "Run CMD-STRING in vterm buffer BUFFER-NAME from DEFAULT-DIR."
  (unless (beads-command--vterm-available-p)
    (user-error "Vterm package not installed.  Install it or change `beads-terminal-backend'"))
  (let* ((default-directory default-dir)
         (process-environment (cons "CLICOLOR_FORCE=1" process-environment))
         (vterm-shell (format "%s -c %s"
                              shell-file-name
                              (shell-quote-argument
                               (concat "cd " (shell-quote-argument default-dir)
                                       " && " cmd-string))))
         (vterm-buffer-name buffer-name)
         (buf (vterm buffer-name)))
    ;; Set vterm-kill-buffer-on-exit buffer-local to keep buffer after exit.
    ;; This must be done after buffer creation since the sentinel checks the
    ;; buffer-local value when the process terminates.
    (with-current-buffer buf
      (setq-local vterm-kill-buffer-on-exit nil))
    buf))

(defun beads-command--eat-available-p ()
  "Return non-nil if eat is available."
  (require 'eat nil t))

(defun beads-command--run-eat (cmd-string buffer-name default-dir)
  "Run CMD-STRING in eat buffer BUFFER-NAME from DEFAULT-DIR."
  (unless (beads-command--eat-available-p)
    (user-error "Eat package not installed.  Install it or change `beads-terminal-backend'"))
  (let* ((default-directory default-dir)
         (process-environment (cons "CLICOLOR_FORCE=1" process-environment))
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'eat-mode)
        (eat-mode))
      (let ((proc (get-buffer-process buf)))
        (when (and proc (process-live-p proc))
          (delete-process proc)))
      (eat-exec buf buffer-name shell-file-name nil
                (list "-c" (concat "cd " (shell-quote-argument default-dir)
                                   " && " cmd-string "; exit"))))
    (pop-to-buffer buf)))

(defun beads-command--detect-best-backend ()
  "Detect the best available terminal backend.
Tries in order: vterm, eat, term.  Falls back to term (built-in)."
  (cond
   ((beads-command--vterm-available-p) 'vterm)
   ((beads-command--eat-available-p) 'eat)
   (t 'term)))

(defun beads-command--run-in-terminal (cmd-string buffer-name default-dir)
  "Run CMD-STRING in terminal buffer BUFFER-NAME from DEFAULT-DIR.
Uses the backend specified by `beads-terminal-backend'.
When nil, auto-detects best available backend."
  (let ((backend (or beads-terminal-backend
                     (beads-command--detect-best-backend))))
    (pcase backend
      ('vterm (beads-command--run-vterm cmd-string buffer-name default-dir))
      ('eat (beads-command--run-eat cmd-string buffer-name default-dir))
      ('term (beads-command--run-term cmd-string buffer-name default-dir))
      ('compile (beads-command--run-compile cmd-string buffer-name default-dir))
      (_ (beads-command--run-term cmd-string buffer-name default-dir)))))

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
Set by `beads-command-execute-async' when command completes.")
   (data
    :initarg :data
    :initform nil
    :documentation "Parsed/processed result data after command execution.
For non-JSON commands: raw stdout string.
For JSON commands: parsed JSON data.
For subclass commands: domain objects (e.g., beads-issue instances).
Set by `beads-command-parse' after execution."))
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

Returns the COMMAND object with populated slots:
  - `exit-code': Process exit code (0 = success)
  - `stdout': Standard output string
  - `stderr': Standard error string
  - `data': Parsed result (see below)

The `data' slot contents depend on command type:

For non-JSON commands (beads-command):
  `data' contains raw stdout string

For JSON commands (beads-command-json and subclasses):
  DEFAULT BEHAVIOR (:json t, the default):
    `data' contains domain objects:
    - beads-command-list: list of beads-issue instances
    - beads-command-create: beads-issue instance (or list)
    - beads-command-show: beads-issue instance (or list)
    - beads-command-update: beads-issue instance (or list)
    - beads-command-close: beads-issue instance (or list)
    - beads-command-ready: list of beads-issue instances
    - beads-command-blocked: list of beads-blocked-issue instances
    - beads-command-stats: parsed JSON alist
    - beads-command-epic-*: parsed JSON

  With :json nil:
    `data' contains raw stdout string

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
  "Build command arguments from COMMAND using slot metadata.
If `beads-command-subcommand' returns a subcommand name, builds:
  (SUBCOMMAND... ...global-flags... ...metadata-args...)
Supports multi-word subcommands like \"worktree create\".
Otherwise returns just global flags (for abstract classes)."
  (with-slots (actor db no-auto-flush no-auto-import
                     no-daemon no-db sandbox) command
    (let ((global-args nil)
          (subcommand (beads-command-subcommand command)))
      ;; Build global flags
      (when no-auto-flush (push "--no-auto-flush" global-args))
      (when no-auto-import (push "--no-auto-import" global-args))
      (when no-daemon (push "--no-daemon" global-args))
      (when no-db (push "--no-db" global-args))
      (when sandbox (push "--sandbox" global-args))
      (when actor
        (push "--actor" global-args)
        (push actor global-args))
      (when db
        (push "--db" global-args)
        (push db global-args))
      (setq global-args (nreverse global-args))
      ;; Build full command line
      (if subcommand
          ;; Use metadata-based building
          (append (split-string subcommand)
                  global-args
                  (beads-meta-build-command-line command))
        ;; No subcommand - just return global flags
        global-args))))

(cl-defmethod beads-command-validate ((_command beads-command))
  "Validate base COMMAND.
Default implementation returns nil (valid).
Subclasses override to add validation."
  nil)

(cl-defgeneric beads-command-subcommand (command)
  "Return the CLI subcommand name for COMMAND.
For example, \"create\", \"update\", \"doctor\", etc.
Subclasses should override this to return their subcommand string.
Returns nil by default, which disables metadata-based command building.")

(cl-defmethod beads-command-subcommand ((_command beads-command))
  "Return nil (no subcommand) for base command class."
  nil)

;;; Interactive Execution Methods
;;
;; These generic methods provide default behavior for transient menus.
;; Override in subclasses for custom post-execution behavior.

(cl-defgeneric beads-command-execute-interactive (command)
  "Execute COMMAND interactively, showing output to user.

This is the primary entry point for transient menu execution.
Default implementation runs the command in a `compilation-mode' buffer.

Override this method in subclasses for custom behavior, such as:
- Showing a created issue in a dedicated buffer
- Refreshing a list after update/close
- Custom success messages

The method should:
1. Execute the command (typically via `beads-command-execute')
2. Handle the result appropriately for the command type
3. Provide user feedback (messages, buffer display, etc.)

Signals errors from `beads-command-execute' on failure.")

(defun beads-command--ansi-color-filter ()
  "Apply ANSI color codes in compilation output.
Compatibility wrapper for Emacs 27+.
Also strips OSC escape sequences (terminal queries)."
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    ;; Strip OSC sequences (e.g., ]11;? for background color query)
    (save-excursion
      (goto-char compilation-filter-start)
      (while (re-search-forward "\033\\]\\([0-9]+\\);[^\007\033]*\\(\007\\|\033\\\\\\)?" nil t)
        (replace-match "")))
    ;; Apply ANSI colors
    (ansi-color-apply-on-region compilation-filter-start (point))))

(cl-defmethod beads-command-execute-interactive ((command beads-command))
  "Default: run COMMAND in terminal buffer.
Uses the backend specified by `beads-terminal-backend'.
Runs from the beads project root and enables ANSI color support."
  (let* ((cmd-line (beads-command-line command))
         (cmd-string (mapconcat #'shell-quote-argument cmd-line " "))
         (buffer-name (format "*bd %s*" (nth 1 cmd-line)))
         ;; Find beads project root (parent of .beads directory)
         (beads-dir (beads--find-beads-dir))
         (project-root (when beads-dir
                         (file-name-directory
                          (directory-file-name beads-dir))))
         (default-dir (or project-root default-directory)))
    (beads-command--run-in-terminal cmd-string buffer-name default-dir)))

(cl-defgeneric beads-command-preview (command)
  "Preview what COMMAND would execute without running it.

Returns a string representation of the full command line.
Default implementation builds the command line and formats it.

Override in subclasses to add command-specific preview information,
such as showing which fields are set or validation warnings.")

(cl-defmethod beads-command-preview ((command beads-command))
  "Default: return formatted command line for COMMAND."
  (let* ((cmd-line (beads-command-line command))
         (cmd-string (mapconcat #'shell-quote-argument cmd-line " ")))
    (message "Command: %s" cmd-string)
    cmd-string))

(cl-defgeneric beads-command-parse (command)
  "Parse the output stored in COMMAND slots and return the parsed result.
COMMAND must have been executed (stdout/stderr/exit-code slots populated).

This method is called by `beads-command-execute' after process execution.
It should:
1. Read from stdout/stderr slots (depending on command type)
2. Parse/transform the data as appropriate
3. Return the parsed result (EIEIO objects, lists, etc.)

IMPORTANT: This method must NOT modify any command slots.  The caller
\(`beads-command-execute' or `beads-command-execute-async') is responsible
for setting the `data' slot to the returned value.

Dispatches based on command type:
- `beads-command': Returns raw stdout string
- `beads-command-json': Parses JSON from stdout, returns parsed alist/vector
- Subclasses may override to transform parsed JSON into domain objects

Signals `beads-json-parse-error' if JSON parsing fails.")

(cl-defmethod beads-command-parse ((command beads-command))
  "Parse non-JSON COMMAND output.
Returns stdout string.  Does not modify command slots."
  (oref command stdout))

;;; Base Command Execution - Non-JSON Commands

(cl-defmethod beads-command-execute ((command beads-command))
  "Execute COMMAND and return the command object with populated slots.
Runs the bd CLI command, populates slots (exit-code, stdout, stderr),
calls `beads-command-parse' to process output, and returns the command.

After execution, these slots are populated:
- `exit-code': Process exit code (0 = success)
- `stdout': Standard output string
- `stderr': Standard error string
- `data': Parsed result (raw stdout, parsed JSON, or domain objects)

Returns the COMMAND object itself, allowing slot access after execution.
This matches the behavior of `beads-command-execute-async'.

Signals `beads-validation-error' if command validation fails.
Signals `beads-command-error' if process exits with non-zero code.
Signals `beads-json-parse-error' if JSON parsing fails (for JSON commands)."
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
          (let* ((proc-exit-code (apply #'process-file
                                        (car cmd) nil
                                        (list (current-buffer) stderr-file)
                                        nil (cdr cmd)))
                 (end-time (current-time))
                 (elapsed (float-time (time-subtract end-time start-time)))
                 (proc-stdout (buffer-string))
                 (proc-stderr (with-temp-buffer
                                (insert-file-contents stderr-file)
                                (buffer-string))))

            (when (fboundp 'beads--log)
              (beads--log 'info "Command completed in %.3fs" elapsed)
              (beads--log 'verbose "Exit code: %d" proc-exit-code)
              (beads--log 'verbose "Stdout: %s" proc-stdout)
              (beads--log 'verbose "Stderr: %s" proc-stderr))

            ;; Populate command slots
            (oset command exit-code proc-exit-code)
            (oset command stdout proc-stdout)
            (oset command stderr proc-stderr)

            (if (zerop proc-exit-code)
                ;; Success: parse output, set data slot, return command
                (progn
                  (oset command data (beads-command-parse command))
                  command)
              ;; Signal error with complete information
              (signal 'beads-command-error
                      (list (format "Command failed with exit code %d"
                                    proc-exit-code)
                            :command cmd-string
                            :exit-code proc-exit-code
                            :stdout proc-stdout
                            :stderr proc-stderr)))))

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
Enables machine-readable output."
    ;; CLI properties - handled by beads-meta-build-command-line
    :long-option "--json"
    :option-type :boolean))
  :abstract t
  :documentation "Abstract base class for bd commands that support JSON output.
Inherits from beads-command and adds --json flag support.
Use this as parent class for commands that support --json flag.")

(cl-defmethod beads-command-parse ((command beads-command-json))
  "Parse JSON output from COMMAND.
When :json is t, parses JSON from stdout slot and returns it.
When :json is nil, falls back to parent (returns raw stdout).
Does not modify command slots.
Signals `beads-json-parse-error' if JSON parsing fails."
  (with-slots (json stdout) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Parse JSON from stdout
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'vector)
                 (json-key-type 'symbol))
            (json-read-from-string stdout))
        (error
         (signal 'beads-json-parse-error
                 (list (format "Failed to parse JSON: %s"
                               (error-message-string err))
                       :exit-code (oref command exit-code)
                       :stdout stdout
                       :stderr (oref command stderr)
                       :parse-error err)))))))

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

                 ;; Parse output and set data slot (matches sync behavior)
                 (when (zerop proc-exit-code)
                   (oset command data (beads-command-parse command)))

                 ;; Call callback with command object
                 (when callback
                   (funcall callback command)))))))
    process))


;;; Init Command

(beads-defcommand beads-command-init (beads-command)
  ((branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Git branch for beads commits (-b, --branch). Default: current branch."
    ;; CLI properties
    :long-option "--branch"
    :short-option "-b"
    :option-type :string)
   (contributor
    :initarg :contributor
    :type boolean
    :initform nil
    :documentation "Run OSS contributor setup wizard (--contributor)."
    ;; CLI properties
    :long-option "--contributor"
    :option-type :boolean)
   (prefix
    :initarg :prefix
    :type (or null string)
    :initform nil
    :documentation "Issue prefix (-p, --prefix). Default: current directory name."
    ;; CLI properties
    :long-option "--prefix"
    :short-option "-p"
    :option-type :string)
   (quiet
    :initarg :quiet
    :type boolean
    :initform nil
    :documentation "Suppress output (-q, --quiet)."
    ;; CLI properties
    :long-option "--quiet"
    :short-option "-q"
    :option-type :boolean)
   (skip-merge-driver
    :initarg :skip-merge-driver
    :type boolean
    :initform nil
    :documentation "Skip git merge driver setup (--skip-merge-driver). Non-interactive mode."
    ;; CLI properties
    :long-option "--skip-merge-driver"
    :option-type :boolean)
   (team
    :initarg :team
    :type boolean
    :initform nil
    :documentation "Run team workflow setup wizard (--team)."
    ;; CLI properties
    :long-option "--team"
    :option-type :boolean))
  :documentation "Represents bd init command.
Initializes bd in the current directory by creating .beads/ directory
and database file.")

(cl-defmethod beads-command-subcommand ((_command beads-command-init))
  "Return \"init\" as the CLI subcommand name."
  "init")

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

(beads-defcommand beads-command-quickstart (beads-command)
  ()
  :documentation "Represents bd quickstart command.
Displays a quick start guide showing common bd workflows and patterns.
This command has no command-specific flags, only global flags.")

(cl-defmethod beads-command-subcommand ((_command beads-command-quickstart))
  "Return \"quickstart\" as the CLI subcommand name."
  "quickstart")

(cl-defmethod beads-command-validate ((_command beads-command-quickstart))
  "Validate quickstart COMMAND.
No required fields, returns nil (valid)."
  nil)

;;; Export Command

(beads-defcommand beads-command-export (beads-command-json)
  ((force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force export even if database is empty (--force)."
    ;; CLI properties
    :long-option "--force"
    :option-type :boolean)
   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Export format (-f, --format). Default: jsonl."
    ;; CLI properties
    :long-option "--format"
    :short-option "-f"
    :option-type :string)
   (output
    :initarg :output
    :type (or null string)
    :initform nil
    :documentation "Output file (-o, --output). Default: stdout."
    ;; CLI properties
    :long-option "--output"
    :short-option "-o"
    :option-type :string)
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status (-s, --status)."
    ;; CLI properties
    :long-option "--status"
    :short-option "-s"
    :option-type :string))
  :documentation "Represents bd export command.
Export all issues to JSON Lines format (one JSON object per line).
Issues are sorted by ID for consistent diffs.")

(cl-defmethod beads-command-subcommand ((_command beads-command-export))
  "Return \"export\" as the CLI subcommand name."
  "export")

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

(cl-defmethod beads-command-parse ((command beads-command-export))
  "Parse export COMMAND output.
Unlike most commands, bd export writes JSON stats to stderr, not stdout.
When :json is nil, returns raw stderr.
When :json is t, parses JSON from stderr.
Does not modify command slots."
  (with-slots (json stderr) command
    (if (not json)
        ;; No JSON parsing, return raw stderr
        stderr
      ;; Parse JSON from stderr (not stdout!)
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'vector)
                 (json-key-type 'symbol))
            (json-read-from-string stderr))
        (error
         (signal 'beads-json-parse-error
                 (list (format "Failed to parse JSON from stderr: %s"
                               (error-message-string err))
                       :exit-code (oref command exit-code)
                       :stdout (oref command stdout)
                       :stderr stderr
                       :parse-error err)))))))

;;; Import Command

(beads-defcommand beads-command-import (beads-command-json)
  ((json
    :initarg :json
    :type boolean
    :initform nil
    :documentation "Output in JSON format (--json).
Defaults to nil because import command does not produce JSON output.")
   (clear-duplicate-external-refs
    :initarg :clear-duplicate-external-refs
    :type boolean
    :initform nil
    :documentation "Clear duplicate external_ref values (--clear-duplicate-external-refs). Keeps first occurrence."
    ;; CLI properties
    :long-option "--clear-duplicate-external-refs"
    :option-type :boolean)
   (dedupe-after
    :initarg :dedupe-after
    :type boolean
    :initform nil
    :documentation "Detect and report content duplicates after import (--dedupe-after)."
    ;; CLI properties
    :long-option "--dedupe-after"
    :option-type :boolean)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview collision detection without making changes (--dry-run)."
    ;; CLI properties
    :long-option "--dry-run"
    :option-type :boolean)
   (input
    :initarg :input
    :type (or null string)
    :initform nil
    :documentation "Input file (-i, --input). Default: stdin."
    ;; CLI properties
    :long-option "--input"
    :short-option "-i"
    :option-type :string)
   (orphan-handling
    :initarg :orphan-handling
    :type (or null string)
    :initform nil
    :documentation "How to handle missing parent issues (--orphan-handling). Options: strict, resurrect, skip, allow. Default: use config or 'allow'."
    ;; CLI properties
    :long-option "--orphan-handling"
    :option-type :string)
   (rename-on-import
    :initarg :rename-on-import
    :type boolean
    :initform nil
    :documentation "Rename imported issues to match database prefix (--rename-on-import). Updates all references."
    ;; CLI properties
    :long-option "--rename-on-import"
    :option-type :boolean)
   (skip-existing
    :initarg :skip-existing
    :type boolean
    :initform nil
    :documentation "Skip existing issues instead of updating them (-s, --skip-existing)."
    ;; CLI properties
    :long-option "--skip-existing"
    :short-option "-s"
    :option-type :boolean)
   (strict
    :initarg :strict
    :type boolean
    :initform nil
    :documentation "Fail on dependency errors instead of treating them as warnings (--strict)."
    ;; CLI properties
    :long-option "--strict"
    :option-type :boolean))
  :documentation "Represents bd import command.
Import issues from JSON Lines format (one JSON object per line).
NOTE: Import requires direct database access and automatically uses
--no-daemon.")

(cl-defmethod beads-command-subcommand ((_command beads-command-import))
  "Return \"import\" as the CLI subcommand name."
  "import")

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

(cl-defmethod beads-command-execute :before ((command beads-command-import))
  "Set no-daemon flag for import COMMAND before execution.
Import always uses --no-daemon to avoid daemon issues."
  (oset command no-daemon t))

(cl-defmethod beads-command-parse ((command beads-command-import))
  "Parse import COMMAND output.
Unlike most commands, bd import writes JSON stats to stderr, not stdout.
When :json is nil, returns raw stderr.
When :json is t, parses JSON from stderr.
Does not modify command slots."
  (with-slots (json stderr) command
    (if (not json)
        ;; No JSON parsing, return raw stderr
        stderr
      ;; Parse JSON from stderr (not stdout!)
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'vector)
                 (json-key-type 'symbol))
            (json-read-from-string stderr))
        (error
         (signal 'beads-json-parse-error
                 (list (format "Failed to parse JSON from stderr: %s"
                               (error-message-string err))
                       :exit-code (oref command exit-code)
                       :stdout (oref command stdout)
                       :stderr stderr
                       :parse-error err)))))))

;;; List Command

(beads-defcommand beads-command-list (beads-command-json)
  ((status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter by status (-s, --status).
Values: open, in_progress, blocked, closed."
    ;; CLI properties
    :long-option "--status"
    :short-option "-s"
    :option-type :string
    ;; Transient properties
    :transient-key "-s"
    :transient-description "--status"
    :transient-class transient-option
    :transient-argument "--status="
    :transient-prompt "Status: "
    :transient-reader beads-reader-list-status
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 1)
   (priority
    :initarg :priority
    :type (or null integer)
    :initform nil
    :documentation "Filter by priority (-p, --priority).
Values: 0-4 (0=critical, 1=high, 2=medium, 3=low, 4=backlog)."
    ;; CLI properties
    :long-option "--priority"
    :short-option "-p"
    :option-type :integer
    ;; Transient properties
    :transient-key "-P"
    :transient-description "--priority"
    :transient-class transient-option
    :transient-argument "--priority="
    :transient-prompt "Priority: "
    :transient-reader beads-reader-list-priority
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 2)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Filter by type (-t, --type).
Values: bug, feature, task, epic, chore."
    ;; CLI properties
    :long-option "--type"
    :short-option "-t"
    :option-type :string
    ;; Transient properties
    :transient-key "-T"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Type: "
    :transient-reader beads-reader-list-type
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 3)
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Filter by assignee (-a, --assignee)."
    ;; CLI properties
    :long-option "--assignee"
    :short-option "-a"
    :option-type :string
    ;; Transient properties
    :transient-key "-a"
    :transient-description "--assignee"
    :transient-class transient-option
    :transient-argument "--assignee="
    :transient-prompt "Assignee: "
    :transient-reader beads-reader-list-assignee
    :transient-group "Basic Filters"
    :transient-level 1
    :transient-order 4)
   (title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Filter by title text (--title).
Case-insensitive substring match."
    ;; CLI properties
    :long-option "--title"
    :option-type :string
    ;; Transient properties
    :transient-key "-ti"
    :transient-description "--title"
    :transient-class transient-option
    :transient-argument "--title="
    :transient-prompt "Title: "
    :transient-reader beads-reader-list-title
    :transient-group "Text Search"
    :transient-level 2
    :transient-order 1)
   (title-contains
    :initarg :title-contains
    :type (or null string)
    :initform nil
    :documentation "Filter by title substring (--title-contains).
Case-insensitive."
    ;; CLI properties
    :long-option "--title-contains"
    :option-type :string
    ;; Transient properties
    :transient-key "-tc"
    :transient-description "--title-contains"
    :transient-class transient-option
    :transient-argument "--title-contains="
    :transient-prompt "Title contains: "
    :transient-reader beads-reader-list-title-contains
    :transient-group "Text Search"
    :transient-level 2
    :transient-order 2)
   (desc-contains
    :initarg :desc-contains
    :type (or null string)
    :initform nil
    :documentation "Filter by description substring (--desc-contains).
Case-insensitive."
    ;; CLI properties
    :long-option "--desc-contains"
    :option-type :string
    ;; Transient properties
    :transient-key "-d"
    :transient-description "--desc-contains"
    :transient-class transient-option
    :transient-argument "--desc-contains="
    :transient-prompt "Description contains: "
    :transient-reader beads-reader-list-desc-contains
    :transient-group "Text Search"
    :transient-level 2
    :transient-order 3)
   (notes-contains
    :initarg :notes-contains
    :type (or null string)
    :initform nil
    :documentation "Filter by notes substring (--notes-contains).
Case-insensitive."
    ;; CLI properties
    :long-option "--notes-contains"
    :option-type :string
    ;; Transient properties
    :transient-key "-nc"
    :transient-description "--notes-contains"
    :transient-class transient-option
    :transient-argument "--notes-contains="
    :transient-prompt "Notes contains: "
    :transient-reader beads-reader-list-notes-contains
    :transient-group "Text Search"
    :transient-level 2
    :transient-order 4)
   (created-after
    :initarg :created-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues created after date (--created-after).
Date format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "--created-after"
    :option-type :string
    ;; Transient properties
    :transient-key "-Ca"
    :transient-description "--created-after"
    :transient-class transient-option
    :transient-argument "--created-after="
    :transient-prompt "Created after: "
    :transient-reader beads-reader-list-date
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 1)
   (created-before
    :initarg :created-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues created before date (--created-before).
Date format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "--created-before"
    :option-type :string
    ;; Transient properties
    :transient-key "-Cb"
    :transient-description "--created-before"
    :transient-class transient-option
    :transient-argument "--created-before="
    :transient-prompt "Created before: "
    :transient-reader beads-reader-list-date
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 2)
   (updated-after
    :initarg :updated-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated after date (--updated-after).
Date format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "--updated-after"
    :option-type :string
    ;; Transient properties
    :transient-key "-ua"
    :transient-description "--updated-after"
    :transient-class transient-option
    :transient-argument "--updated-after="
    :transient-prompt "Updated after: "
    :transient-reader beads-reader-list-date
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 3)
   (updated-before
    :initarg :updated-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues updated before date (--updated-before).
Date format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "--updated-before"
    :option-type :string
    ;; Transient properties
    :transient-key "-ub"
    :transient-description "--updated-before"
    :transient-class transient-option
    :transient-argument "--updated-before="
    :transient-prompt "Updated before: "
    :transient-reader beads-reader-list-date
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 4)
   (closed-after
    :initarg :closed-after
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed after date (--closed-after).
Date format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "--closed-after"
    :option-type :string
    ;; Transient properties
    :transient-key "-ca"
    :transient-description "--closed-after"
    :transient-class transient-option
    :transient-argument "--closed-after="
    :transient-prompt "Closed after: "
    :transient-reader beads-reader-list-date
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 5)
   (closed-before
    :initarg :closed-before
    :type (or null string)
    :initform nil
    :documentation "Filter issues closed before date (--closed-before).
Date format: YYYY-MM-DD or RFC3339."
    ;; CLI properties
    :long-option "--closed-before"
    :option-type :string
    ;; Transient properties
    :transient-key "-cb"
    :transient-description "--closed-before"
    :transient-class transient-option
    :transient-argument "--closed-before="
    :transient-prompt "Closed before: "
    :transient-reader beads-reader-list-date
    :transient-group "Date Filters"
    :transient-level 3
    :transient-order 6)
   (priority-min
    :initarg :priority-min
    :type (or null integer)
    :initform nil
    :documentation "Filter by minimum priority (--priority-min).
Inclusive."
    ;; CLI properties
    :long-option "--priority-min"
    :option-type :integer
    ;; Transient properties
    :transient-key "-p<"
    :transient-description "--priority-min"
    :transient-class transient-option
    :transient-argument "--priority-min="
    :transient-prompt "Min priority: "
    :transient-reader beads-reader-list-priority-min
    :transient-group "Advanced Filters"
    :transient-level 4
    :transient-order 1)
   (priority-max
    :initarg :priority-max
    :type (or null integer)
    :initform nil
    :documentation "Filter by maximum priority (--priority-max).
Inclusive."
    ;; CLI properties
    :long-option "--priority-max"
    :option-type :integer
    ;; Transient properties
    :transient-key "-p>"
    :transient-description "--priority-max"
    :transient-class transient-option
    :transient-argument "--priority-max="
    :transient-prompt "Max priority: "
    :transient-reader beads-reader-list-priority-max
    :transient-group "Advanced Filters"
    :transient-level 4
    :transient-order 2)
   (label
    :initarg :label
    :type (or null list)
    :initform nil
    :documentation "Filter by labels, AND logic (-l, --label).
Must have ALL labels. Can combine with --label-any."
    ;; CLI properties
    :long-option "--label"
    :short-option "-l"
    :option-type :list
    :option-separator nil  ; Each label is a separate --label arg
    ;; Transient properties
    :transient-key "-l"
    :transient-description "--label"
    :transient-class transient-option
    :transient-argument "--label="
    :transient-prompt "Label (AND): "
    :transient-reader beads-reader-list-label
    :transient-group "Advanced Filters"
    :transient-level 4
    :transient-order 3)
   (label-any
    :initarg :label-any
    :type (or null list)
    :initform nil
    :documentation "Filter by labels, OR logic (--label-any).
Must have AT LEAST ONE label. Can combine with --label."
    ;; CLI properties
    :long-option "--label-any"
    :option-type :list
    :option-separator nil  ; Each label is a separate --label-any arg
    ;; Transient properties
    :transient-key "-L"
    :transient-description "--label-any"
    :transient-class transient-option
    :transient-argument "--label-any="
    :transient-prompt "Label (OR): "
    :transient-reader beads-reader-list-label
    :transient-group "Advanced Filters"
    :transient-level 4
    :transient-order 4)
   (id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Filter by specific issue IDs (--id).
Comma-separated, e.g., 'bd-1,bd-5,bd-10'."
    ;; CLI properties
    :long-option "--id"
    :option-type :string
    ;; Transient properties
    :transient-key "-i"
    :transient-description "--id"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue IDs: "
    :transient-reader beads-reader-list-id
    :transient-group "Advanced Filters"
    :transient-level 4
    :transient-order 5)
   (no-assignee
    :initarg :no-assignee
    :type boolean
    :initform nil
    :documentation "Filter issues with no assignee (--no-assignee)."
    ;; CLI properties
    :long-option "--no-assignee"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-A"
    :transient-description "--no-assignee"
    :transient-class transient-switch
    :transient-argument "--no-assignee"
    :transient-group "Advanced Filters"
    :transient-level 4
    :transient-order 6)
   (empty-description
    :initarg :empty-description
    :type boolean
    :initform nil
    :documentation "Filter issues with empty description (--empty-description)."
    ;; CLI properties
    :long-option "--empty-description"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-E"
    :transient-description "--empty-description"
    :transient-class transient-switch
    :transient-argument "--empty-description"
    :transient-group "Advanced Filters"
    :transient-level 4
    :transient-order 7)
   (no-labels
    :initarg :no-labels
    :type boolean
    :initform nil
    :documentation "Filter issues with no labels (--no-labels)."
    ;; CLI properties
    :long-option "--no-labels"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-N"
    :transient-description "--no-labels"
    :transient-class transient-switch
    :transient-argument "--no-labels"
    :transient-group "Advanced Filters"
    :transient-level 4
    :transient-order 8)
   (limit
    :initarg :limit
    :type (or null integer)
    :initform nil
    :documentation "Limit results (-n, --limit).
When using `beads-command-list!', defaults to `beads-list-default-limit' if not set.
Set to 0 for no limit, or a positive integer to limit results.
Pass `:limit nil' explicitly to disable the default."
    ;; CLI properties
    :long-option "--limit"
    :short-option "-n"
    :option-type :integer
    ;; Transient properties
    :transient-key "-n"
    :transient-description "--limit"
    :transient-class transient-option
    :transient-argument "--limit="
    :transient-prompt "Limit: "
    :transient-reader beads-reader-list-limit
    :transient-group "Output Options"
    :transient-level 5
    :transient-order 1)
   (long
    :initarg :long
    :type boolean
    :initform nil
    :documentation "Show detailed multi-line output (--long)."
    ;; CLI properties
    :long-option "--long"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-Lo"
    :transient-description "--long"
    :transient-class transient-switch
    :transient-argument "--long"
    :transient-group "Output Options"
    :transient-level 5
    :transient-order 2)
   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Output format (--format).
Values: 'digraph', 'dot', or Go template."
    ;; CLI properties
    :long-option "--format"
    :option-type :string
    ;; Transient properties
    :transient-key "-f"
    :transient-description "--format"
    :transient-class transient-option
    :transient-argument "--format="
    :transient-prompt "Format: "
    :transient-reader beads-reader-list-format
    :transient-group "Output Options"
    :transient-level 5
    :transient-order 3)
   (all
    :initarg :all
    :type boolean
    :initform nil
    :documentation "Show all issues (--all).
Default behavior, provided for CLI familiarity."
    ;; CLI properties
    :long-option "--all"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-al"
    :transient-description "--all"
    :transient-class transient-switch
    :transient-argument "--all"
    :transient-group "Output Options"
    :transient-level 5
    :transient-order 4))
  :documentation "Represents bd list command.
Lists issues with optional filtering, sorting, and formatting.
When executed with :json t, returns a list of beads-issue instances.")

(cl-defmethod beads-command-subcommand ((_command beads-command-list))
  "Return \"list\" as the CLI subcommand name."
  "list")

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

(cl-defmethod beads-command-parse ((command beads-command-list))
  "Parse list COMMAND output and return list of beads-issue instances.
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, converts parsed JSON to beads-issue instances.
Does not modify command slots."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON, then convert to beads-issue instances
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (mapcar #'beads-issue-from-json (append parsed-json nil))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instances: %s"
                                 (error-message-string err))
                         :exit-code (oref command exit-code)
                         :parsed-json parsed-json
                         :stderr (oref command stderr)
                         :parse-error err))))))))

;; Override auto-generated beads-command-list! to apply default limit.
;; Note: Using initialize-instance doesn't work well because:
;; 1. EIEIO validates :initform types at class definition time (symbol fails)
;; 2. :around/:after methods have complex slot argument handling
;; The function override is the cleanest solution that reliably works.
(defun beads-command-list! (&rest args)
  "Execute `beads-command-list' and return result data.

ARGS are passed to the constructor.  When :limit is not specified,
uses `beads-list-default-limit' as the default value.  Pass `:limit nil'
explicitly to disable the limit.

This function overrides the auto-generated version to support
the `beads-list-default-limit' customization variable."
  (unless (plist-member args :limit)
    (setq args (plist-put args :limit beads-list-default-limit)))
  (oref (beads-command-execute (apply #'beads-command-list args)) data))

;;; Create Command

(beads-defcommand beads-command-create (beads-command-json)
  ((title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "Issue title (positional or --title).
First positional argument or explicit --title flag."
    ;; CLI properties - title is a positional argument
    :positional 1
    ;; Transient properties
    :transient-key "t"
    :transient-description "Title (required)"
    :transient-class transient-option
    :transient-argument "--title="
    :transient-prompt "Issue title: "
    :transient-reader beads-reader-issue-title
    :transient-group "Required"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)
   (acceptance
    :initarg :acceptance
    :type (or null string)
    :initform nil
    :documentation "Acceptance criteria (--acceptance)."
    ;; CLI properties
    :long-option "--acceptance"
    :option-type :string
    ;; Transient properties
    :transient-key "-A"
    :transient-description "Acceptance criteria"
    :transient-class beads-create-transient-multiline
    :transient-argument "--acceptance="
    :transient-field-name "Acceptance Criteria"
    :transient-group "Content"
    :transient-level 3
    :transient-order 2)
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "Assignee (-a, --assignee)."
    ;; CLI properties
    :long-option "--assignee"
    :short-option "-a"
    :option-type :string
    ;; Transient properties
    :transient-key "-a"
    :transient-description "Assignee"
    :transient-class transient-option
    :transient-argument "--assignee="
    :transient-prompt "Assignee: "
    :transient-reader beads-reader-issue-assignee
    :transient-group "Issue attributes"
    :transient-level 2
    :transient-order 3)
   (deps
    :initarg :deps
    :type (or null list)
    :initform nil
    :documentation "Dependencies (--deps).
List of strings in format 'type:id' or 'id'.
Examples: 'discovered-from:bd-20', 'blocks:bd-15', 'bd-20'."
    ;; CLI properties
    :long-option "--deps"
    :option-type :list
    :option-separator ","
    ;; Transient properties
    :transient-key "-D"
    :transient-description "Dependencies"
    :transient-class transient-option
    :transient-argument "--deps="
    :transient-prompt "Dependencies (type:id,...): "
    :transient-reader beads-reader-create-dependencies
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 3)
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Issue description (-d, --description)."
    ;; CLI properties
    :long-option "--description"
    :short-option "-d"
    :option-type :string
    ;; Transient properties
    :transient-key "-d"
    :transient-description "Description"
    :transient-class beads-create-transient-multiline
    :transient-argument "--description="
    :transient-field-name "Description"
    :transient-group "Content"
    :transient-level 3
    :transient-order 1)
   (design
    :initarg :design
    :type (or null string)
    :initform nil
    :documentation "Design notes (--design)."
    ;; CLI properties
    :long-option "--design"
    :option-type :string
    ;; Transient properties
    :transient-key "-G"
    :transient-description "Design notes"
    :transient-class beads-create-transient-multiline
    :transient-argument "--design="
    :transient-field-name "Design"
    :transient-group "Content"
    :transient-level 3
    :transient-order 3)
   (external-ref
    :initarg :external-ref
    :type (or null string)
    :initform nil
    :documentation "External reference (--external-ref).
Examples: 'gh-9', 'jira-ABC'."
    ;; CLI properties
    :long-option "--external-ref"
    :option-type :string
    ;; Transient properties
    :transient-key "-x"
    :transient-description "External reference"
    :transient-class transient-option
    :transient-argument "--external-ref="
    :transient-prompt "External reference: "
    :transient-reader beads-reader-issue-external-ref
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 1)
   (file
    :initarg :file
    :type (or null string)
    :initform nil
    :documentation "Create multiple issues from markdown file (-f, --file)."
    ;; CLI properties
    :long-option "--file"
    :short-option "-f"
    :option-type :string
    ;; Transient properties
    :transient-key "-F"
    :transient-description "Create from file"
    :transient-class transient-option
    :transient-argument "--file="
    :transient-prompt "Markdown file: "
    :transient-reader beads-reader-create-file
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 7)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force creation even if prefix doesn't match (--force)."
    ;; CLI properties
    :long-option "--force"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-f"
    :transient-description "Force creation"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 8)
   (from-template
    :initarg :from-template
    :type (or null string)
    :initform nil
    :documentation "Create issue from template (--from-template).
Examples: 'epic', 'bug', 'feature'."
    ;; CLI properties
    :long-option "--from-template"
    :option-type :string
    ;; Transient properties
    :transient-key "-T"
    :transient-description "From template"
    :transient-class transient-option
    :transient-argument "--from-template="
    :transient-prompt "Template (epic, bug, feature): "
    :transient-reader beads-reader-create-from-template
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 6)
   (id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Explicit issue ID (--id).
Example: 'bd-42' for partitioning."
    ;; CLI properties
    :long-option "--id"
    :option-type :string
    ;; Transient properties
    :transient-key "-i"
    :transient-description "Custom ID"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Custom ID: "
    :transient-reader beads-reader-create-custom-id
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 2)
   (labels
    :initarg :labels
    :type (or null list)
    :initform nil
    :documentation "Labels (-l, --labels).
List of label strings."
    ;; CLI properties
    :long-option "--labels"
    :short-option "-l"
    :option-type :list
    :option-separator ","
    ;; Transient properties
    :transient-key "-l"
    :transient-description "Labels"
    :transient-class transient-option
    :transient-argument "--labels="
    :transient-prompt "Labels (comma-separated): "
    :transient-reader beads-reader-issue-labels
    :transient-group "Issue attributes"
    :transient-level 2
    :transient-order 4)
   (parent
    :initarg :parent
    :type (or null string)
    :initform nil
    :documentation "Parent issue ID for hierarchical child (--parent).
Example: 'bd-a3f8e9'."
    ;; CLI properties
    :long-option "--parent"
    :option-type :string
    ;; Transient properties
    :transient-key "-P"
    :transient-description "Parent issue ID"
    :transient-class transient-option
    :transient-argument "--parent="
    :transient-prompt "Parent issue ID (e.g., bd-a3f8e9): "
    :transient-reader beads-reader-create-parent
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 4)
   (priority
    :initarg :priority
    :type (or null string integer)
    :initform nil
    :documentation "Priority (-p, --priority).
Values: 0-4 or P0-P4 (0=highest). Default: '2'.
Accepts both integer (1) and string (\"1\" or \"P1\") formats."
    ;; CLI properties
    :long-option "--priority"
    :short-option "-p"
    :option-type :string  ; Keep as string for P0-P4 format
    ;; Transient properties
    :transient-key "-p"
    :transient-description "Priority"
    :transient-class transient-option
    :transient-argument "--priority="
    :transient-prompt "Priority: "
    :transient-reader beads-reader-issue-priority
    :transient-group "Issue attributes"
    :transient-level 2
    :transient-order 2)
   (repo
    :initarg :repo
    :type (or null string)
    :initform nil
    :documentation "Target repository for issue (--repo).
Overrides auto-routing."
    ;; CLI properties
    :long-option "--repo"
    :option-type :string
    ;; Transient properties
    :transient-key "-r"
    :transient-description "Target repository"
    :transient-class transient-option
    :transient-argument "--repo="
    :transient-prompt "Target repository: "
    :transient-reader beads-reader-create-repo
    :transient-group "Advanced"
    :transient-level 4
    :transient-order 5)
   (issue-type
    :initarg :issue-type
    :type (or null string)
    :initform nil
    :documentation "Issue type (-t, --type).
Values: bug, feature, task, epic, chore. Default: 'task'."
    ;; CLI properties
    :long-option "--type"
    :short-option "-t"
    :option-type :string
    ;; Transient properties
    :transient-key "-t"
    :transient-description "Type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Type: "
    :transient-choices ("bug" "feature" "task" "epic" "chore")
    :transient-reader beads-reader-issue-type
    :transient-group "Issue attributes"
    :transient-level 2
    :transient-order 1))
  :documentation "Represents bd create command.
Creates a new issue (or multiple issues from markdown file).
When executed with :json t, returns the created beads-issue instance(s).")

(cl-defmethod beads-command-subcommand ((_command beads-command-create))
  "Return \"create\" as the CLI subcommand name."
  "create")

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
     (and title (beads--string-blank-p title)
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

(cl-defmethod beads-command-parse ((command beads-command-create))
  "Parse create COMMAND output and return created issue(s).
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, returns beads-issue instance (or list when creating
from file).
Does not modify command slots."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON, then convert to beads-issue instance(s)
      (let ((parsed-json (cl-call-next-method)))
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
                            :exit-code (oref command exit-code)
                            :parsed-json parsed-json
                            :stderr (oref command stderr)))))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-issue instance: %s"
                                 (error-message-string err))
                         :exit-code (oref command exit-code)
                         :parsed-json parsed-json
                         :stderr (oref command stderr)
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-create))
  "Execute CMD to create issue and offer to show it.
Overrides default `compilation-mode' behavior with issue-specific UX."
  (let* ((result (oref (beads-command-execute cmd) data))
         ;; Handle both single-issue and multi-issue responses
         (issues (cond
                  ((null result) nil)
                  ((cl-typep result 'beads-issue) (list result))
                  ((and (listp result)
                        (not (null result))
                        (cl-typep (car result) 'beads-issue))
                   result)
                  (t nil)))
         (first-issue (car issues)))
    ;; Invalidate completion cache after creating issues
    (beads--invalidate-completion-cache)
    (cond
     ((null first-issue)
      (message "No issues created"))
     ((= (length issues) 1)
      (message "Created issue: %s - %s"
               (oref first-issue id)
               (oref first-issue title))
      (when (y-or-n-p (format "Show issue %s? " (oref first-issue id)))
        (beads-show (oref first-issue id))))
     (t
      (message "Created %d issues from file (first: %s)"
               (length issues) (oref first-issue id))))))

;;; Epic Commands

(beads-defcommand beads-command-epic-close-eligible (beads-command-json)
  ((dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview what would be closed without making changes (--dry-run)."
    ;; CLI properties
    :long-option "--dry-run"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-n"
    :transient-description "--dry-run"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd epic close-eligible command.
Close epics where all children are complete.
When executed with :json t, returns list of closed/eligible epic IDs.")

(cl-defmethod beads-command-subcommand ((_command beads-command-epic-close-eligible))
  "Return \"epic close-eligible\" as the CLI subcommand name."
  "epic close-eligible")

(cl-defmethod beads-command-validate ((_command beads-command-epic-close-eligible))
  "Validate epic close-eligible COMMAND.
Default implementation returns nil (valid)."
  nil)

;; No custom parse needed for epic-close-eligible - uses parent JSON parse

(beads-defcommand beads-command-epic-status (beads-command-json)
  ((eligible-only
    :initarg :eligible-only
    :type boolean
    :initform nil
    :documentation "Show only epics eligible for closure (--eligible-only)."
    ;; CLI properties
    :long-option "--eligible-only"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-e"
    :transient-description "--eligible-only"
    :transient-class transient-switch
    :transient-argument "--eligible-only"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd epic status command.
Show epic completion status for all epics or only eligible ones.
When executed with :json t, returns list of epic status objects.")

(cl-defmethod beads-command-subcommand ((_command beads-command-epic-status))
  "Return \"epic status\" as the CLI subcommand name."
  "epic status")

(cl-defmethod beads-command-validate ((_command beads-command-epic-status))
  "Validate epic status COMMAND.
Default implementation returns nil (valid)."
  nil)

(cl-defmethod beads-command-parse ((command beads-command-epic-status))
  "Parse epic status COMMAND output and return beads-epic-status instances.
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, returns list of beads-epic-status instances.
Does not modify command slots."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON, then convert to beads-epic-status
      (let ((parsed-json (cl-call-next-method)))
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
                            :exit-code (oref command exit-code)
                            :parsed-json parsed-json
                            :stderr (oref command stderr)))))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-epic-status instance: %s"
                                 (error-message-string err))
                         :exit-code (oref command exit-code)
                         :parsed-json parsed-json
                         :stderr (oref command stderr)
                         :parse-error err))))))))

;;; Show Command

(beads-defcommand beads-command-show (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs to show (positional arguments).
Example: '(\"bd-1\" \"bd-2\")"
    ;; CLI properties
    :positional 1
    :option-type :list
    :option-separator " "  ; Each ID is a separate positional arg
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID (required)"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Show Issue"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t))
  :documentation "Represents bd show command.
Shows detailed information for one or more issues.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided).")

(cl-defmethod beads-command-subcommand ((_command beads-command-show))
  "Return \"show\" as the CLI subcommand name."
  "show")

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

(cl-defmethod beads-command-parse ((command beads-command-show))
  "Parse show COMMAND output and return issue(s).
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

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-show))
  "Execute CMD to show issue in a dedicated buffer.
Overrides default `compilation-mode' behavior."
  (let* ((result (oref (beads-command-execute cmd) data))
         (issue (if (listp result) (car result) result)))
    (if issue
        (beads-show (oref issue id))
      (message "Issue not found"))))

;;; Update Command

(beads-defcommand beads-command-update (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "One or more issue IDs to update (positional arguments).
Example: '(\"bd-1\" \"bd-2\")"
    ;; CLI properties
    :positional 1
    :option-type :list
    :option-separator " ")
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "New status (-s, --status).
Values: open, in_progress, blocked, closed."
    ;; CLI properties
    :long-option "--status"
    :short-option "-s"
    :option-type :string
    ;; Transient properties
    :transient-key "s"
    :transient-description "Status"
    :transient-class transient-option
    :transient-argument "--status="
    :transient-prompt "Status: "
    :transient-choices ("open" "in_progress" "blocked" "closed")
    :transient-reader beads-reader-update-status
    :transient-group "Status & Priority"
    :transient-level 1
    :transient-order 1)
   (priority
    :initarg :priority
    :type (or null string integer)
    :initform nil
    :documentation "New priority (-p, --priority).
Values: 0-4 or P0-P4.
Accepts both integer (1) and string (\"1\" or \"P1\") formats."
    ;; CLI properties
    :long-option "--priority"
    :short-option "-p"
    :option-type :string
    ;; Transient properties
    :transient-key "p"
    :transient-description "Priority"
    :transient-class transient-option
    :transient-argument "--priority="
    :transient-prompt "Priority: "
    :transient-reader beads-reader-issue-priority
    :transient-group "Status & Priority"
    :transient-level 1
    :transient-order 2)
   (title
    :initarg :title
    :type (or null string)
    :initform nil
    :documentation "New title (--title)."
    ;; CLI properties
    :long-option "--title"
    :option-type :string
    ;; Transient properties
    :transient-key "t"
    :transient-description "Title"
    :transient-class transient-option
    :transient-argument "--title="
    :transient-prompt "Issue title: "
    :transient-reader beads-reader-issue-title
    :transient-group "Basic Info"
    :transient-level 2
    :transient-order 1)
   (assignee
    :initarg :assignee
    :type (or null string)
    :initform nil
    :documentation "New assignee (-a, --assignee)."
    ;; CLI properties
    :long-option "--assignee"
    :short-option "-a"
    :option-type :string
    ;; Transient properties
    :transient-key "a"
    :transient-description "Assignee"
    :transient-class transient-option
    :transient-argument "--assignee="
    :transient-prompt "Assignee: "
    :transient-reader beads-reader-issue-assignee
    :transient-group "Basic Info"
    :transient-level 2
    :transient-order 2)
   (external-ref
    :initarg :external-ref
    :type (or null string)
    :initform nil
    :documentation "External reference (--external-ref).
Examples: 'gh-9', 'jira-ABC'."
    ;; CLI properties
    :long-option "--external-ref"
    :option-type :string
    ;; Transient properties
    :transient-key "x"
    :transient-description "External reference"
    :transient-class transient-option
    :transient-argument "--external-ref="
    :transient-prompt "External reference: "
    :transient-reader beads-reader-issue-external-ref
    :transient-group "Basic Info"
    :transient-level 2
    :transient-order 3)
   (description
    :initarg :description
    :type (or null string)
    :initform nil
    :documentation "Issue description (-d, --description)."
    ;; CLI properties
    :long-option "--description"
    :short-option "-d"
    :option-type :string
    ;; Transient properties
    :transient-key "d"
    :transient-description "Description"
    :transient-class beads-create-transient-multiline
    :transient-argument "--description="
    :transient-field-name "Description"
    :transient-group "Content"
    :transient-level 3
    :transient-order 1)
   (acceptance
    :initarg :acceptance
    :type (or null string)
    :initform nil
    :documentation "Acceptance criteria (--acceptance)."
    ;; CLI properties
    :long-option "--acceptance"
    :option-type :string
    ;; Transient properties
    :transient-key "A"
    :transient-description "Acceptance criteria"
    :transient-class beads-create-transient-multiline
    :transient-argument "--acceptance="
    :transient-field-name "Acceptance Criteria"
    :transient-group "Content"
    :transient-level 3
    :transient-order 2)
   (design
    :initarg :design
    :type (or null string)
    :initform nil
    :documentation "Design notes (--design)."
    ;; CLI properties
    :long-option "--design"
    :option-type :string
    ;; Transient properties
    :transient-key "G"
    :transient-description "Design notes"
    :transient-class beads-create-transient-multiline
    :transient-argument "--design="
    :transient-field-name "Design"
    :transient-group "Content"
    :transient-level 3
    :transient-order 3)
   (notes
    :initarg :notes
    :type (or null string)
    :initform nil
    :documentation "Additional notes (--notes)."
    ;; CLI properties
    :long-option "--notes"
    :option-type :string
    ;; Transient properties
    :transient-key "N"
    :transient-description "Notes"
    :transient-class beads-create-transient-multiline
    :transient-argument "--notes="
    :transient-field-name "Notes"
    :transient-group "Content"
    :transient-level 3
    :transient-order 4))
  :documentation "Represents bd update command.
Updates one or more issues with new field values.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided).")

(cl-defmethod beads-command-subcommand ((_command beads-command-update))
  "Return \"update\" as the CLI subcommand name."
  "update")

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

(cl-defmethod beads-command-parse ((command beads-command-update))
  "Parse update COMMAND output and return updated issue(s).
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

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-update))
  "Execute CMD to update issue and show result.
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
    ;; Invalidate completion cache after updating
    (beads--invalidate-completion-cache)
    (if issues
        (message "Updated %d issue%s: %s"
                 (length issues)
                 (if (= (length issues) 1) "" "s")
                 (mapconcat (lambda (i) (oref i id)) issues ", "))
      (message "No issues updated"))))

;;; Close Command

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
    :long-option "--reason"
    :short-option "-r"
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
of instances when multiple IDs provided).")

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

;;; Reopen Command

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
    :option-separator nil
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID (required)"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue ID: "
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
    :documentation "Optional reason for reopening (-r, --reason)."
    ;; CLI properties
    :long-option "--reason"
    :short-option "-r"
    :option-type :string
    ;; Transient properties
    :transient-key "-r"
    :transient-description "--reason"
    :transient-class beads-create-transient-multiline
    :transient-argument "--reason="
    :transient-field-name "Reopen Reason"
    :transient-group "Reopen Issue"
    :transient-level 1
    :transient-order 2))
  :documentation "Represents bd reopen command.
Reopens one or more closed issues with an optional reason.
When executed with :json t, returns beads-issue instance (or list
of instances when multiple IDs provided).")

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

(cl-defmethod beads-command-parse ((command beads-command-reopen))
  "Parse reopen COMMAND output and return reopened issue(s).
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

;;; Delete Command

(beads-defcommand beads-command-delete (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to delete (positional argument).
Example: \"bd-1\""
    ;; CLI properties
    :positional 1
    :option-type :string)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force deletion without preview (--force flag)."
    ;; CLI properties
    :long-option "--force"
    :option-type :boolean))
  :documentation "Represents bd delete command.
Deletes an issue with optional --force flag.
When executed, returns (EXIT-CODE PARSED-JSON STDERR) tuple where
PARSED-JSON is the deleted issue object.")

(cl-defmethod beads-command-subcommand ((_command beads-command-delete))
  "Return \"delete\" as the CLI subcommand name."
  "delete")

(cl-defmethod beads-command-validate ((command beads-command-delete))
  "Validate delete COMMAND.
Checks that issue ID is provided.
Returns error string or nil if valid."
  (with-slots (issue-id) command
    (when (or (null issue-id) (string-empty-p issue-id))
      "Must provide an issue ID")))

;; Ready Command: See beads-command-ready.el for the class definition.
;; The require is at the end of this file to avoid circular dependencies.

;; Blocked Command: See beads-command-blocked.el for the class definition.
;; The require is at the end of this file to avoid circular dependencies.

;;; Stats Command

(beads-defcommand beads-command-stats (beads-command-json)
  ()
  :documentation "Represents bd stats command.
Shows statistics about the issue database.
When executed with :json t, returns parsed JSON stats object.")

(cl-defmethod beads-command-subcommand ((_command beads-command-stats))
  "Return \"stats\" as the CLI subcommand name."
  "stats")

(cl-defmethod beads-command-validate ((_command beads-command-stats))
  "Validate stats COMMAND.
No required fields, returns nil (valid)."
  nil)

;; No custom parse needed for stats - uses parent JSON parse

;;; Dep Add Command

(beads-defcommand beads-command-dep-add (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Source issue ID (required positional argument)."
    ;; CLI properties
    :positional 1
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID"
    :transient-class transient-option
    :transient-argument "--issue-id="
    :transient-prompt "Issue ID: "
    :transient-reader beads-reader-dep-add-issue-id
    :transient-group "Add Dependency"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)
   (depends-on-id
    :initarg :depends-on-id
    :type (or null string)
    :initform nil
    :documentation "Target dependency issue ID (required positional argument)."
    ;; CLI properties
    :positional 2
    ;; Transient properties
    :transient-key "d"
    :transient-description "Depends on ID"
    :transient-class transient-option
    :transient-argument "--depends-on="
    :transient-prompt "Depends on issue ID: "
    :transient-reader beads-reader-dep-add-depends-on-id
    :transient-group "Add Dependency"
    :transient-level 1
    :transient-order 2
    ;; Validation
    :required t)
   (dep-type
    :initarg :dep-type
    :type (or null string)
    :initform nil
    :documentation "Dependency type (-t, --type).
Values: blocks, related, parent-child, discovered-from.
Default: blocks."
    ;; CLI properties
    :long-option "--type"
    :short-option "-t"
    :option-type :string
    ;; Transient properties
    :transient-key "-t"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Dependency type: "
    :transient-choices ("blocks" "related" "parent-child" "discovered-from")
    :transient-group "Options"
    :transient-level 2
    :transient-order 1))
  :documentation "Represents bd dep add command.
Adds a dependency relationship between two issues.
When executed with :json t, returns parsed JSON result.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-add))
  "Return \"dep add\" as the CLI subcommand name."
  "dep add")

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

;; No custom parse needed for dep-add - uses parent JSON parse

;;; Dep Remove Command

(beads-defcommand beads-command-dep-remove (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Source issue ID (required positional argument)."
    ;; CLI properties
    :positional 1
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID"
    :transient-class transient-option
    :transient-argument "--issue-id="
    :transient-prompt "Issue ID: "
    :transient-reader beads-reader-dep-remove-issue-id
    :transient-group "Remove Dependency"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)
   (depends-on-id
    :initarg :depends-on-id
    :type (or null string)
    :initform nil
    :documentation "Target dependency issue ID (required positional argument)."
    ;; CLI properties
    :positional 2
    ;; Transient properties
    :transient-key "d"
    :transient-description "Depends on ID"
    :transient-class transient-option
    :transient-argument "--depends-on="
    :transient-prompt "Depends on issue ID: "
    :transient-reader beads-reader-dep-remove-depends-on-id
    :transient-group "Remove Dependency"
    :transient-level 1
    :transient-order 2
    ;; Validation
    :required t))
  :documentation "Represents bd dep remove command.
Removes a dependency relationship between two issues.
When executed with :json t, returns parsed JSON result.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-remove))
  "Return \"dep remove\" as the CLI subcommand name."
  "dep remove")

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

;; No custom parse needed for dep-remove - uses parent JSON parse

;;; Dep Tree Command

(beads-defcommand beads-command-dep-tree (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to show tree for (required positional argument)."
    ;; CLI properties
    :positional 1
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID"
    :transient-class transient-option
    :transient-argument "--issue-id="
    :transient-prompt "Issue ID: "
    :transient-reader beads-reader-dep-tree-issue-id
    :transient-group "Issue"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)
   (direction
    :initarg :direction
    :type (or null string)
    :initform nil
    :documentation "Tree direction (--direction).
Values: down (dependencies - default), up (dependents), both (full graph)."
    ;; CLI properties
    :long-option "--direction"
    :option-type :string
    ;; Transient properties
    :transient-key "-d"
    :transient-description "--direction"
    :transient-class transient-option
    :transient-argument "--direction="
    :transient-prompt "Direction: "
    :transient-choices '("down" "up" "both")
    :transient-group "Display"
    :transient-level 1
    :transient-order 1)
   (format
    :initarg :format
    :type (or null string)
    :initform nil
    :documentation "Output format (--format).
Value: mermaid for Mermaid.js flowchart."
    ;; CLI properties
    :long-option "--format"
    :option-type :string
    ;; Transient properties
    :transient-key "-f"
    :transient-description "--format"
    :transient-class transient-option
    :transient-argument "--format="
    :transient-prompt "Format: "
    :transient-choices '("mermaid")
    :transient-group "Display"
    :transient-level 2
    :transient-order 2)
   (max-depth
    :initarg :max-depth
    :type (or null integer)
    :initform nil
    :documentation "Maximum tree depth (-d, --max-depth).
Safety limit, default: 50."
    ;; CLI properties
    :long-option "--max-depth"
    :short-option "-d"
    :option-type :integer
    ;; Transient properties
    :transient-key "-D"
    :transient-description "--max-depth"
    :transient-class transient-option
    :transient-argument "--max-depth="
    :transient-prompt "Max depth: "
    :transient-group "Display"
    :transient-level 2
    :transient-order 3)
   (reverse
    :initarg :reverse
    :type boolean
    :initform nil
    :documentation "Show dependent tree instead of dependency tree (--reverse).
Deprecated: use --direction=up instead."
    ;; CLI properties
    :long-option "--reverse"
    :option-type :boolean
    ;; Transient properties - hidden since deprecated
    :transient-level 7)
   (show-all-paths
    :initarg :show-all-paths
    :type boolean
    :initform nil
    :documentation "Show all paths to nodes (--show-all-paths).
No deduplication for diamond dependencies."
    ;; CLI properties
    :long-option "--show-all-paths"
    :option-type :boolean
    ;; Transient properties
    :transient-key "-a"
    :transient-description "--show-all-paths"
    :transient-class transient-switch
    :transient-argument "--show-all-paths"
    :transient-group "Display"
    :transient-level 2
    :transient-order 4)
   (status
    :initarg :status
    :type (or null string)
    :initform nil
    :documentation "Filter to only show issues with this status (--status).
Values: open, in_progress, blocked, deferred, closed."
    ;; CLI properties
    :long-option "--status"
    :option-type :string
    ;; Transient properties
    :transient-key "-s"
    :transient-description "--status"
    :transient-class transient-option
    :transient-argument "--status="
    :transient-prompt "Status filter: "
    :transient-choices '("open" "in_progress" "blocked" "deferred" "closed")
    :transient-group "Filters"
    :transient-level 2
    :transient-order 1)
   (dep-type
    :initarg :dep-type
    :type (or null string)
    :initform nil
    :documentation "Filter to only show dependencies of this type (-t, --type).
Examples: tracks, blocks, parent-child."
    ;; CLI properties
    :long-option "--type"
    :short-option "-t"
    :option-type :string
    ;; Transient properties
    :transient-key "-t"
    :transient-description "--type"
    :transient-class transient-option
    :transient-argument "--type="
    :transient-prompt "Dependency type: "
    :transient-group "Filters"
    :transient-level 2
    :transient-order 2))
  :documentation "Represents bd dep tree command.
Shows dependency tree for an issue.
When executed with :json t, returns parsed JSON tree structure.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-tree))
  "Return \"dep tree\" as the CLI subcommand name."
  "dep tree")

(cl-defmethod beads-command-validate ((command beads-command-dep-tree))
  "Validate dep tree COMMAND.
Checks that issue ID is provided and validates option values.
Returns error string or nil if valid."
  (with-slots (issue-id direction max-depth status) command
    (or
     ;; Must have issue-id
     (and (or (null issue-id) (string-empty-p issue-id))
          "Must provide issue-id")
     ;; Validate direction value
     (and direction (not (member direction '("down" "up" "both")))
          "Direction must be one of: down, up, both")
     ;; Validate status value
     (and status (not (member status '("open" "in_progress" "blocked"
                                       "deferred" "closed")))
          "Status must be one of: open, in_progress, blocked, deferred, closed")
     ;; Validate max-depth if provided
     (and max-depth (< max-depth 1)
          "Max depth must be positive"))))

;; No custom parse needed for dep-tree - uses parent JSON parse

;;; Dep Cycles Command

(beads-defcommand beads-command-dep-cycles (beads-command-json)
  ()
  :documentation "Represents bd dep cycles command.
Detects dependency cycles in the issue database.
When executed with :json t, returns parsed JSON with cycle information.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-cycles))
  "Return \"dep cycles\" as the CLI subcommand name."
  "dep cycles")

(cl-defmethod beads-command-validate ((_command beads-command-dep-cycles))
  "Validate dep cycles COMMAND.
No required fields, returns nil (valid)."
  nil)

;; No custom parse needed for dep-cycles - uses parent JSON parse

;;; Dep List Command

(beads-defcommand beads-command-dep-list (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to list dependencies for (required positional)."
    :positional 1
    :required t)
   (direction
    :initarg :direction
    :type (or null string)
    :initform nil
    :documentation "Direction: down (dependencies) or up (dependents)."
    :long-option "--direction"
    :option-type :string)
   (dep-type
    :initarg :dep-type
    :type (or null string)
    :initform nil
    :documentation "Filter by dependency type (e.g., tracks, blocks)."
    :long-option "--type"
    :short-option "-t"
    :option-type :string))
  :documentation "Represents bd dep list command.
Lists dependencies or dependents of an issue.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-list))
  "Return \"dep list\" as the CLI subcommand name."
  "dep list")

(cl-defmethod beads-command-validate ((command beads-command-dep-list))
  "Validate dep list COMMAND."
  (with-slots (issue-id direction) command
    (cond
     ((or (null issue-id) (string-empty-p issue-id))
      "Issue ID is required")
     ((and direction (not (member direction '("down" "up"))))
      "Direction must be 'down' or 'up'")
     (t nil))))

;;; Dep Relate Command

(beads-defcommand beads-command-dep-relate (beads-command-json)
  ((id1
    :initarg :id1
    :type (or null string)
    :initform nil
    :documentation "First issue ID."
    :positional 1
    :required t)
   (id2
    :initarg :id2
    :type (or null string)
    :initform nil
    :documentation "Second issue ID."
    :positional 2
    :required t))
  :documentation "Represents bd dep relate command.
Creates a bidirectional relates_to link between two issues.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-relate))
  "Return \"dep relate\" as the CLI subcommand name."
  "dep relate")

(cl-defmethod beads-command-validate ((command beads-command-dep-relate))
  "Validate dep relate COMMAND."
  (with-slots (id1 id2) command
    (cond
     ((or (null id1) (string-empty-p id1))
      "First issue ID is required")
     ((or (null id2) (string-empty-p id2))
      "Second issue ID is required")
     ((string= id1 id2)
      "Cannot relate issue to itself")
     (t nil))))

;;; Dep Unrelate Command

(beads-defcommand beads-command-dep-unrelate (beads-command-json)
  ((id1
    :initarg :id1
    :type (or null string)
    :initform nil
    :documentation "First issue ID."
    :positional 1
    :required t)
   (id2
    :initarg :id2
    :type (or null string)
    :initform nil
    :documentation "Second issue ID."
    :positional 2
    :required t))
  :documentation "Represents bd dep unrelate command.
Removes a relates_to link between two issues.")

(cl-defmethod beads-command-subcommand ((_command beads-command-dep-unrelate))
  "Return \"dep unrelate\" as the CLI subcommand name."
  "dep unrelate")

(cl-defmethod beads-command-validate ((command beads-command-dep-unrelate))
  "Validate dep unrelate COMMAND."
  (with-slots (id1 id2) command
    (cond
     ((or (null id1) (string-empty-p id1))
      "First issue ID is required")
     ((or (null id2) (string-empty-p id2))
      "Second issue ID is required")
     (t nil))))

;;; Label List-All Command

(beads-defcommand beads-command-label-list-all (beads-command-json)
  ()
  :documentation "Represents bd label list-all command.
Shows all labels with usage counts.
When executed with :json t, returns parsed JSON label list.")

(cl-defmethod beads-command-subcommand ((_command beads-command-label-list-all))
  "Return \"label list-all\" as the CLI subcommand name."
  "label list-all")

(cl-defmethod beads-command-validate ((_command beads-command-label-list-all))
  "Validate label list-all COMMAND.
No required fields, returns nil (valid)."
  nil)

;; No custom parse needed for label-list-all - uses parent JSON parse

;;; Label Add Command

(beads-defcommand beads-command-label-add (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "Issue ID(s) (required)"
    ;; CLI properties
    :positional 1
    :option-type :list
    :option-separator nil
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID(s)"
    :transient-class transient-option
    :transient-argument "--issue-ids="
    :transient-reader beads-reader-label-issue-ids
    :transient-prompt "Issue ID(s) (comma-separated): "
    :transient-group "Add Label"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)
   (label
    :initarg :label
    :type (or null string)
    :initform nil
    :documentation "Label name (required)"
    ;; CLI properties
    :positional 2
    :option-type :string
    ;; Transient properties
    :transient-key "l"
    :transient-description "Label"
    :transient-class transient-option
    :transient-argument "--label="
    :transient-reader beads-reader-label-name
    :transient-prompt "Label name: "
    :transient-group "Add Label"
    :transient-level 1
    :transient-order 2
    ;; Validation
    :required t))
  :documentation "Represents bd label add command.
Adds a label to one or more issues.
When executed with :json t, returns parsed JSON result.")

(cl-defmethod beads-command-subcommand ((_command beads-command-label-add))
  "Return \"label add\" as the CLI subcommand name."
  "label add")

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

;; No custom parse needed for label-add - uses parent JSON parse

;;; Label Remove Command

(beads-defcommand beads-command-label-remove (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type (or null list)
    :initform nil
    :documentation "Issue ID(s) (required)"
    ;; CLI properties
    :positional 1
    :option-type :list
    :option-separator nil
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID(s)"
    :transient-class transient-option
    :transient-argument "--issue-ids="
    :transient-reader beads-reader-label-issue-ids
    :transient-prompt "Issue ID(s) (comma-separated): "
    :transient-group "Remove Label"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t)
   (label
    :initarg :label
    :type (or null string)
    :initform nil
    :documentation "Label name (required)"
    ;; CLI properties
    :positional 2
    :option-type :string
    ;; Transient properties
    :transient-key "l"
    :transient-description "Label"
    :transient-class transient-option
    :transient-argument "--label="
    :transient-reader beads-reader-label-name
    :transient-prompt "Label name: "
    :transient-group "Remove Label"
    :transient-level 1
    :transient-order 2
    ;; Validation
    :required t))
  :documentation "Represents bd label remove command.
Removes a label from one or more issues.
When executed with :json t, returns parsed JSON result.")

(cl-defmethod beads-command-subcommand ((_command beads-command-label-remove))
  "Return \"label remove\" as the CLI subcommand name."
  "label remove")

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

;; No custom parse needed for label-remove - uses parent JSON parse

;;; Label List Command

(beads-defcommand beads-command-label-list (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to list labels for (positional argument)."
    ;; CLI properties
    :positional 1
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue ID"
    :transient-class transient-option
    :transient-argument "--issue-id="
    :transient-prompt "Issue ID: "
    :transient-reader beads-reader-issue-id
    :transient-group "Issue"
    :transient-level 1
    :transient-order 1
    ;; Validation
    :required t))
  :documentation "Represents bd label list command.
Lists all labels for a specific issue.
When executed with :json t, returns parsed JSON result.")

(cl-defmethod beads-command-subcommand ((_command beads-command-label-list))
  "Return \"label list\" as the CLI subcommand name."
  "label list")

(cl-defmethod beads-command-validate ((command beads-command-label-list))
  "Validate label list COMMAND.
Checks that an issue ID is provided.
Returns error string or nil if valid."
  (with-slots (issue-id) command
    ;; Must have an issue ID
    (and (or (null issue-id) (string-empty-p issue-id))
         "Must provide an issue ID")))

;; No custom parse needed for label-list - uses parent JSON parse

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

(provide 'beads-command)
;;; beads-command.el ends here
