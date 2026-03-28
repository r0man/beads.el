;;; beads-command.el --- EIEIO command classes for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; This module defines EIEIO classes for all Beads CLI commands,
;; providing an object-oriented interface to bd command execution.
;;
;; The class hierarchy mirrors bd command structure:
;; - beads-command: Minimal abstract base (execution machinery, generics only)
;; - beads-command-global-options: Inherits beads-command, adds global flags
;;     (--actor, --db, --json, etc.)
;;     - beads-command-create: bd create command
;;     - beads-command-update: bd update command (future)
;;     - beads-command-list: bd list command
;;     - beads-command-init: bd init command
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
(declare-function beads--string-blank-p "beads")
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

;; These helpers are used at macro-expansion time by beads-defcommand,
;; so they must be available during compilation.
;; beads--extract-first-sentence is defined in beads-meta.el (loaded first).
(eval-and-compile
(defun beads--derive-transient-name (class-name)
  "Derive transient menu name from CLASS-NAME.
Strips \"-command-\" from class name to get the transient name.
Example: beads-command-close -> beads-close"
  (let ((name-str (symbol-name class-name)))
    (intern (replace-regexp-in-string "-command-" "-" name-str))))

(defun beads--extract-option (keyword options)
  "Extract value for KEYWORD from OPTIONS plist and return (VALUE . REST).
Returns (nil . OPTIONS) if KEYWORD is not found."
  (let ((pos (cl-position keyword options)))
    (if pos
        (let ((val (nth (1+ pos) options))
              (rest (append (cl-subseq options 0 pos)
                            (cl-subseq options (+ pos 2)))))
          (cons val rest))
      (cons nil options)))))

;; beads--current-feature-name is defined in beads-meta.el (loaded first)

(defmacro beads-defcommand (name superclasses slots &rest options)
  "Define a beads command class with all generated artifacts.

NAME is the class name (a symbol like `beads-command-foo').
SUPERCLASSES is the list of parent classes.
SLOTS is the list of slot definitions.
OPTIONS are additional class options like :documentation.

This macro generates:
1. Class definition wrapped in `eval-and-compile'
2. NAME! convenience function that executes the command
3. Transient menu (when :global-section is specified)
4. CLI subcommand method (when :cli-command is specified)
5. Parse method (when :parse-as is specified)

Custom keyword options (stripped before passing to defclass):
  :global-section SYM   - Generate a transient menu and include SYM
                          as a global options section.  The transient
                          name is derived by stripping \"-command-\"
                          from NAME (e.g., beads-command-close ->
                          beads-close).  The docstring uses the first
                          sentence of :documentation.
  :cli-command STR      - Explicit CLI subcommand string.  When not
                          specified, the subcommand is auto-derived
                          from the class name by the default method
                          on `beads-command'.
  :parse-as KEYWORD     - Auto-generate a `beads-command-parse' method.
                          :issue  - Parse JSON as single beads-issue
                                    (or list when multiple IDs).
                          :issues - Parse JSON as list of beads-issue.

When :global-section is specified, the macro generates the transient
menu automatically from slot metadata via `beads-meta-define-transient'.
Without it, only the class and bang function are generated.

When :global-section is specified, the macro also generates an
`autoload' form for the transient command, so that manual autoload
cookies are no longer needed.  The file name is derived from
`load-file-name' at macroexpansion time.

Example (with transient):
  ;;;###autoload
  (beads-defcommand beads-command-close (beads-command-global-options)
    ((issue-ids :initarg :issue-ids :key \"i\" :transient \"Issue ID\")
     (reason :initarg :reason :key \"r\" :transient \"Reason\"))
    :documentation \"Close issue.\"
    :cli-command \"close\"
    :parse-as :issue
    :global-section beads-option-global-section)

Example (without transient):
  (beads-defcommand beads-command-foo (beads-command-global-options)
    ((name :initarg :name))
    :documentation \"Foo command.\")"
  (declare (indent 2))
  ;; Extract custom keywords from options before passing to defclass
  (let* ((result-1 (beads--extract-option :global-section options))
         (global-section (car result-1))
         (options-2 (cdr result-1))
         (result-2 (beads--extract-option :cli-command options-2))
         (cli-command (car result-2))
         (options-3 (cdr result-2))
         (result-3 (beads--extract-option :parse-as options-3))
         (parse-as (car result-3))
         (defclass-options (cdr result-3))
         ;; Add cli-command as a class-allocated slot if specified
         (final-slots (if cli-command
                         (append slots
                                 `((cli-command
                                    :initform ,cli-command
                                    :allocation :class
                                    :documentation "CLI subcommand name.")))
                       slots))
         ;; Derived names
         (bang-fn (intern (concat (symbol-name name) "!")))
         ;; Transient-related names (only needed when generating a menu)
         (transient-name (when global-section
                           (beads--derive-transient-name name)))
         (transient-prefix (when transient-name
                             (symbol-name transient-name)))
         ;; Extract docstring for transient
         (doc-pos (cl-position :documentation defclass-options))
         (docstring (when doc-pos (nth (1+ doc-pos) defclass-options)))
         (short-doc (when global-section
                      (beads--extract-first-sentence docstring))))
    `(progn
       (eval-and-compile
         (defclass ,name ,superclasses ,final-slots ,@defclass-options))
       (defun ,bang-fn (&rest args)
         ,(format "Execute %s and return result.\n\nARGS are passed to the constructor.\nJSON output is enabled by default for data access.\nPass :json nil to disable." name)
         (let ((cmd (apply #',name args)))
           (unless (plist-member args :json)
             (oset cmd json t))
           (oref (beads-command-execute cmd) result)))
       ,@(when cli-command
           `((cl-defmethod beads-command-subcommand ((_command ,name))
               ,(format "Return %S as the CLI subcommand name." cli-command)
               ,cli-command)))
       ,@(when parse-as
           (beads--generate-parse-method name parse-as))
       ,@(when global-section
           `((beads-meta-define-transient ,name ,transient-prefix
               ,short-doc
               ,global-section))))))

(defun beads--generate-parse-method (class-name parse-as)
  "Generate a `beads-command-parse' method for CLASS-NAME.
PARSE-AS is :issue (single/multi depending on issue-ids count)
or :issues (always list)."
  (pcase parse-as
    (:issue
     `((cl-defmethod beads-command-parse ((command ,class-name) execution)
         ,(format "Parse %s output from EXECUTION into beads-issue objects." class-name)
         (with-slots (json) command
           (if (not json)
               (cl-call-next-method)
             (let ((parsed-json (cl-call-next-method)))
               (condition-case err
                   (if (eq (type-of parsed-json) 'vector)
                       (let ((issues (mapcar #'beads-issue-from-json
                                             (append parsed-json nil))))
                         (if (and (slot-exists-p command 'issue-ids)
                                  (slot-boundp command 'issue-ids)
                                  (oref command issue-ids)
                                  (= (length (oref command issue-ids)) 1))
                             (car issues)
                           issues))
                     ;; Single object response
                     (if (consp parsed-json)
                         (beads-issue-from-json parsed-json)
                       (signal 'beads-json-parse-error
                               (list ,(format "Unexpected JSON structure from %s"
                                              class-name)
                                     :exit-code (oref execution exit-code)
                                     :parsed-json parsed-json
                                     :stderr (oref execution stderr)))))
                 (beads-json-parse-error (signal (car err) (cdr err)))
                 (error
                  (signal 'beads-json-parse-error
                          (list (format "Failed to create beads-issue: %s"
                                        (error-message-string err))
                                :exit-code (oref execution exit-code)
                                :parsed-json parsed-json
                                :stderr (oref execution stderr)
                                :parse-error err))))))))))
    (:issues
     `((cl-defmethod beads-command-parse ((command ,class-name) execution)
         ,(format "Parse %s output from EXECUTION.\nReturn list of beads-issue objects." class-name)
         (with-slots (json) command
           (if (not json)
               (cl-call-next-method)
             (let ((parsed-json (cl-call-next-method)))
               (condition-case err
                   (if (eq (type-of parsed-json) 'vector)
                       (mapcar #'beads-issue-from-json
                               (append parsed-json nil))
                     (signal 'beads-json-parse-error
                             (list ,(format "Unexpected JSON structure from %s"
                                            class-name)
                                   :exit-code (oref execution exit-code)
                                   :parsed-json parsed-json
                                   :stderr (oref execution stderr))))
                 (beads-json-parse-error (signal (car err) (cdr err)))
                 (error
                  (signal 'beads-json-parse-error
                          (list (format "Failed to create beads-issue: %s"
                                        (error-message-string err))
                                :exit-code (oref execution exit-code)
                                :parsed-json parsed-json
                                :stderr (oref execution stderr)
                                :parse-error err))))))))))
    (_ (error "Invalid :parse-as value: %S (must be :issue or :issues)" parse-as))))

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
  ()
  :abstract t
  :documentation "Minimal abstract base class for all bd commands.
Provides execution machinery and generic method definitions only.
Global CLI options are provided by `beads-command-global-options'.
Execution results are returned in `beads-command-execution' objects,
not stored on the command itself (commands are immutable/reusable).")

;;; Global Options Class

(defclass beads-command-global-options (beads-command)
  ((actor
    :initarg :actor
    :type (or null string)
    :initform nil
    :documentation "Actor name for audit trail
Overrides $BD_ACTOR or $USER"
    :long-option "actor"
    :option-type :string)
   (allow-stale
    :initarg :allow-stale
    :type boolean
    :initform nil
    :documentation "Allow operations on potentially stale data
Skip staleness check"
    :long-option "allow-stale"
    :option-type :boolean)
   (db
    :initarg :db
    :type (or null string)
    :initform nil
    :documentation "Database path
Overrides auto-discovery of .beads/*.db"
    :long-option "db"
    :option-type :string)
   (dolt-auto-commit
    :initarg :dolt-auto-commit
    :type (or null string)
    :initform nil
    :documentation "Dolt auto-commit policy
Values: off, on, batch.  on: commit after each write.
batch: defer commits to bd dolt commit"
    :long-option "dolt-auto-commit"
    :option-type :string)
   (lock-timeout
    :initarg :lock-timeout
    :type (or null string)
    :initform nil
    :documentation "SQLite busy timeout
E.g., \"30s\". 0 means fail immediately if locked"
    :long-option "lock-timeout"
    :option-type :string)
   (no-auto-flush
    :initarg :no-auto-flush
    :type boolean
    :initform nil
    :documentation "Disable automatic JSONL sync
Prevents auto-export after CRUD operations"
    :long-option "no-auto-flush"
    :option-type :boolean)
   (no-auto-import
    :initarg :no-auto-import
    :type boolean
    :initform nil
    :documentation "Disable automatic JSONL import
Prevents auto-import when JSONL is newer than DB"
    :long-option "no-auto-import"
    :option-type :boolean)
   (no-daemon
    :initarg :no-daemon
    :type boolean
    :initform nil
    :documentation "Force direct storage mode
Bypass daemon if running"
    :long-option "no-daemon"
    :option-type :boolean)
   (no-db
    :initarg :no-db
    :type boolean
    :initform nil
    :documentation "Use no-db mode
Load from JSONL, no SQLite database"
    :long-option "no-db"
    :option-type :boolean)
   (profile
    :initarg :profile
    :type boolean
    :initform nil
    :documentation "Generate CPU profile
For performance analysis"
    :long-option "profile"
    :option-type :boolean)
   (quiet
    :initarg :quiet
    :type boolean
    :initform nil
    :documentation "Suppress non-essential output
Errors only"
    :long-option "quiet"
    :short-option "q"
    :option-type :boolean)
   (readonly
    :initarg :readonly
    :type boolean
    :initform nil
    :documentation "Read-only mode
Block write operations for worker sandboxes"
    :long-option "readonly"
    :option-type :boolean)
   (sandbox
    :initarg :sandbox
    :type boolean
    :initform nil
    :documentation "Sandbox mode
Disables daemon and auto-sync"
    :long-option "sandbox"
    :option-type :boolean)
   (verbose
    :initarg :verbose
    :type boolean
    :initform nil
    :documentation "Enable verbose output
Debug output"
    :long-option "verbose"
    :short-option "v"
    :option-type :boolean)
   (json
    :initarg :json
    :type boolean
    :initform nil
    :documentation "Output in JSON format
Enables machine-readable output.
Defaults to nil (human-readable terminal output).
Programmatic callers that need structured data must set this to t"
    :long-option "json"
    :option-type :boolean))
  :documentation "Class providing global bd CLI options.
Inherits `beads-command' base and adds all global flag slots.
These flags apply to all bd commands and are built using slot metadata.
All concrete command classes should inherit from this class.")

;;; Command Execution Result

(defclass beads-command-execution ()
  ((command
    :initarg :command
    :type beads-command
    :documentation "The command that was executed.")
   (exit-code
    :initarg :exit-code
    :type (or null integer)
    :initform nil
    :documentation "Exit code from command execution.
0 indicates success, non-zero indicates failure.")
   (stdout
    :initarg :stdout
    :type (or null string)
    :initform nil
    :documentation "Standard output from command execution.")
   (stderr
    :initarg :stderr
    :type (or null string)
    :initform nil
    :documentation "Standard error from command execution.")
   (result
    :initarg :result
    :initform nil
    :documentation "Parsed/processed result data after command execution.
For non-JSON commands: raw stdout string.
For JSON commands: parsed JSON data.
For subclass commands: domain objects (e.g., beads-issue instances).
Set by `beads-command-parse' after execution."))
  :documentation "Result of executing a beads-command.
Separates execution results (mutable, per-execution) from command
definition (immutable, reusable). Created by `beads-command-execute'
and `beads-command-execute-async'.")

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

Returns a `beads-command-execution' object with slots:
  - `command': The command that was executed
  - `exit-code': Process exit code (0 = success)
  - `stdout': Standard output string
  - `stderr': Standard error string
  - `result': Parsed result (see below)

The `result' slot contents depend on command type:

DEFAULT BEHAVIOR (:json t, the default):
    `result' contains domain objects:
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
    `result' contains raw stdout string

Signals errors:
  - beads-validation-error: Command validation failed
  - beads-command-error: Command execution failed (non-zero exit)
  - beads-json-parse-error: JSON parsing failed

Subclasses should not override this; the implementation is
provided for beads-command.")

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
Otherwise returns just global flags (for abstract classes).

Global options are built from `beads-command-global-options' slot metadata
via `beads-meta-build-global-options'."
  (let ((global-args (beads-meta-build-global-options command))
        (subcommand (beads-command-subcommand command)))
    ;; Build full command line
    (if subcommand
        ;; Use metadata-based building
        (append (split-string subcommand)
                global-args
                (beads-meta-build-command-line command))
      ;; No subcommand - just return global flags
      global-args)))

(cl-defmethod beads-command-validate ((_command beads-command))
  "Validate base COMMAND.
Default implementation returns nil (valid).
Subclasses override to add validation."
  nil)

(cl-defgeneric beads-command-subcommand (command)
  "Return the CLI subcommand name for COMMAND.
For example, \"create\", \"update\", \"doctor\", etc.

The default implementation auto-derives the subcommand from the class
name by stripping the \"beads-command-\" prefix and converting hyphens
in multi-word commands to spaces:
  beads-command-close     -> \"close\"
  beads-command-dep-add   -> \"dep add\"

Override this method only for non-standard subcommand names.
Returns nil for the abstract base class `beads-command'.")

(cl-defmethod beads-command-subcommand ((command beads-command))
  "Auto-derive subcommand name from COMMAND class name.
Strips \"beads-command-\" prefix and replaces hyphens with spaces.
For the abstract `beads-command' class itself, returns nil.
Checks for :cli-command class-allocated slot first."
  (let ((class-name (symbol-name (eieio-object-class command))))
    (unless (equal class-name "beads-command")
      (let ((cli-cmd (and (slot-exists-p command 'cli-command)
                          (slot-boundp command 'cli-command)
                          (with-no-warnings (oref command cli-command)))))
        (or cli-cmd
            (when (string-match "\\`beads-command-\\(.+\\)\\'" class-name)
              (replace-regexp-in-string
               "-" " " (match-string 1 class-name))))))))

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
Runs the command via the backend specified by `beads-terminal-backend'
from the beads project root.  The json slot defaults to nil so bd
outputs colored human-readable text without modification."
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

(cl-defgeneric beads-command-parse (command execution)
  "Parse EXECUTION output and return the parsed result.
COMMAND is the command object (used for method dispatch and configuration).
EXECUTION is the `beads-command-execution' object containing stdout/stderr.

This method is called by `beads-command-execute' after process execution.
It should:
1. Read from EXECUTION's stdout/stderr slots
2. Parse/transform the data as appropriate for COMMAND type
3. Return the parsed result (EIEIO objects, lists, etc.)

IMPORTANT: This method must NOT modify any slots.  The caller
\(`beads-command-execute' or `beads-command-execute-async') is responsible
for setting the EXECUTION's `result' slot to the returned value.

Dispatches based on json slot:
- When :json is t: Parses JSON from stdout, returns alist/vector
- When :json is nil (default): Returns raw stdout string
- Subclasses may override to transform parsed JSON into domain objects

Signals `beads-json-parse-error' if JSON parsing fails.")

(cl-defmethod beads-command-parse ((command beads-command) execution)
  "Parse COMMAND output from EXECUTION.
When json slot is t, parses JSON from stdout and returns it.
When json slot is nil, returns raw stdout string.
Does not modify any slots.
Signals `beads-json-parse-error' if JSON parsing fails."
  (with-slots (json) command
    (if (not json)
        (oref execution stdout)
      (let ((stdout (oref execution stdout)))
        (condition-case err
            (let* ((json-object-type 'alist)
                   (json-array-type 'vector)
                   (json-key-type 'symbol))
              (json-read-from-string stdout))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to parse JSON: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :stdout stdout
                         :stderr (oref execution stderr)
                         :parse-error err))))))))

;;; Process Environment Helpers

(defun beads-command--process-environment ()
  "Return a `process-environment' for bd command execution.
If `beads-dolt-port' is set, prepends BEADS_DOLT_PORT=<port> so
that bd connects to the specified Dolt server instead of
auto-starting its own instance."
  (if (and (boundp 'beads-dolt-port) beads-dolt-port)
      (cons (format "BEADS_DOLT_PORT=%d" beads-dolt-port)
            process-environment)
    process-environment))

;;; Base Command Execution - Non-JSON Commands

(cl-defmethod beads-command-execute ((command beads-command))
  "Execute COMMAND and return a `beads-command-execution' object.
Runs the bd CLI command, creates an execution object with results,
calls `beads-command-parse' to process output, and returns the execution.

The returned `beads-command-execution' object contains:
- `command': The command object that was executed
- `exit-code': Process exit code (0 = success)
- `stdout': Standard output string
- `stderr': Standard error string
- `result': Parsed result (raw stdout, parsed JSON, or domain objects)

For backward compatibility, command slots are also populated:
- `exit-code', `stdout', `stderr', `data' (deprecated, use execution object)

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
          (let* ((process-environment (beads-command--process-environment))
                 (proc-exit-code (apply #'process-file
                                        (car cmd) nil
                                        (list (current-buffer) stderr-file)
                                        nil (cdr cmd)))
                 (end-time (current-time))
                 (elapsed (float-time (time-subtract end-time start-time)))
                 (proc-stdout (buffer-string))
                 (proc-stderr (with-temp-buffer
                                (insert-file-contents stderr-file)
                                (buffer-string)))
                 ;; Create execution object
                 (execution (beads-command-execution
                             :command command
                             :exit-code proc-exit-code
                             :stdout proc-stdout
                             :stderr proc-stderr)))

            (when (fboundp 'beads--log)
              (beads--log 'info "Command completed in %.3fs" elapsed)
              (beads--log 'verbose "Exit code: %d" proc-exit-code)
              ;; Truncate output to avoid logging sensitive issue content
              ;; (verbose mode may expose passwords, PII, or API tokens
              ;; embedded in issue fields).
              (beads--log 'verbose "Stdout: %s"
                          (if (> (length proc-stdout) 500)
                              (concat (substring proc-stdout 0 500) "...[truncated]")
                            proc-stdout))
              (beads--log 'verbose "Stderr: %s"
                          (if (> (length proc-stderr) 500)
                              (concat (substring proc-stderr 0 500) "...[truncated]")
                            proc-stderr)))

            (if (zerop proc-exit-code)
                ;; Success: parse output and set result
                (let ((parsed (beads-command-parse command execution)))
                  (oset execution result parsed)
                  execution)
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

;;; Async Command Execution

(cl-defgeneric beads-command-execute-async (command &optional callback)
  "Execute COMMAND asynchronously without blocking Emacs.

CALLBACK is called with a `beads-command-execution' object when done.
The execution object contains the result data in its slots.

Signals `beads-validation-error' immediately if validation fails.

Return value: process object (use `delete-process' to cancel).

The `beads-command-execution' object passed to CALLBACK contains:
  - `command': The command that was executed
  - `exit-code': Process exit code (0 = success)
  - `stdout': Standard output as string
  - `stderr': Standard error as string
  - `result': Parsed result on success (nil on failure)

For backward compatibility, COMMAND slots are also populated:
  - `exit-code', `stdout', `stderr', `data' (deprecated)

Example usage:

  (beads-command-execute-async
   (beads-command-list :status \"open\")
   (lambda (exec)
     (if (oref exec result)
         (message \"Got %d issues\" (length (oref exec result)))
       (message \"Failed: %s\" (oref exec stderr)))))")

(cl-defmethod beads-command-execute-async ((command beads-command)
                                           &optional callback)
  "Execute non-JSON COMMAND asynchronously.
CALLBACK receives a `beads-command-execution' object when complete.
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
           :environment (beads-command--process-environment)
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
                                     (buffer-string)))
                      ;; Create execution object
                      (execution (beads-command-execution
                                  :command command
                                  :exit-code proc-exit-code
                                  :stdout proc-stdout
                                  :stderr proc-stderr)))

                 (when (fboundp 'beads--log)
                   (beads--log 'info "Async command completed in %.3fs" elapsed)
                   (beads--log 'verbose "Exit code: %d" proc-exit-code)
                   ;; Truncate output to avoid logging sensitive issue content
                   (beads--log 'verbose "Stdout: %s"
                               (if (> (length proc-stdout) 500)
                                   (concat (substring proc-stdout 0 500)
                                           "...[truncated]")
                                 proc-stdout))
                   (beads--log 'verbose "Stderr: %s"
                               (if (> (length proc-stderr) 500)
                                   (concat (substring proc-stderr 0 500)
                                           "...[truncated]")
                                 proc-stderr)))

                 ;; Clean up buffers (suppress kill queries for these temp buffers)
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer stdout-buffer)
                   (kill-buffer stderr-buffer))

                 ;; Parse output and set result slot.
                 ;; Wrap in condition-case: unhandled errors in
                 ;; process sentinels are silently suppressed by
                 ;; Emacs, which would cause callback to never fire.
                 (when (zerop proc-exit-code)
                   (condition-case parse-err
                       (oset execution result
                             (beads-command-parse command execution))
                     (error
                      (oset execution exit-code 1)
                      (oset execution stderr
                            (format "%s\nParse error: %s"
                                    proc-stderr
                                    (error-message-string parse-err))))))

                 ;; Call callback with execution object (always)
                 (when callback
                   (funcall callback execution)))))))
    process))


(provide 'beads-command)
;;; beads-command.el ends here
