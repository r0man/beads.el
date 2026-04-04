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
;; - Implements beads-command-execute method (returns parsed result directly)
;; - Provides beads-command-line for building full command line
;; - Supports validation via beads-command-validate
;;
;; Usage:
;;
;;   ;; Create and execute a command — returns parsed result directly
;;   (let ((cmd (beads-command-list :status "open" :json t)))
;;     (beads-command-execute cmd))  ;; => list of beads-issue objects
;;
;;   ;; Build full command line without execution
;;   (beads-command-line cmd)
;;   ;; => ("bd" "list" "--status" "open" "--json")

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
2. CLI subcommand method (when :cli-command is specified)
3. Symbol properties for :result, :json
4. Transient menu (controlled by :transient keyword)

Custom keyword options (stripped before passing to defclass):
  :cli-command STR      - Explicit CLI subcommand string.  When not
                          specified, the subcommand is auto-derived
                          from the class name by the default method
                          on `beads-command'.
  :result TYPE          - Declare the command's result type.  Stored
                          as symbol property `beads-result'.
                          Examples: `beads-issue', `(list-of beads-issue)'.
                          Used by Phase 3 auto-parsing infrastructure.
  :json BOOL            - Whether the command supports --json.
                          Stored as symbol property `beads-json'.
                          Default is t (most commands support --json).
                          Set to nil for commands like doctor, init.
  :transient VAL        - Control transient menu generation.
                          t or omitted: auto-generate transient menu
                            with `beads-option-global-section'.
                          nil: no transient at all.
                          :manual: skip auto-generation; a hand-written
                            `transient-define-prefix' follows in the
                            same file.

The transient name is derived by stripping \"-command-\" from NAME
\(e.g., beads-command-close -> beads-close).  The docstring uses
the first sentence of :documentation.

Example (with auto-generated transient):
  ;;;###autoload
  (beads-defcommand beads-command-close (beads-command-global-options)
    ((issue-ids :initarg :issue-ids :key \"i\" :transient \"Issue ID\")
     (reason :initarg :reason :key \"r\" :transient \"Reason\"))
    :documentation \"Close issue.\"
    :result (list-of beads-issue))

Example (no transient):
  (beads-defcommand beads-command-doctor (beads-command-global-options)
    ()
    :documentation \"Run bd doctor.\"
    :json nil
    :transient nil)

Example (hand-written transient):
  (beads-defcommand beads-command-list (beads-command-global-options)
    ((status ...))
    :documentation \"List issues.\"
    :result (list-of beads-issue)
    :transient :manual)"
  (declare (indent 2))
  ;; Extract custom keywords from options before passing to defclass
  (let* ((result-1 (beads--extract-option :cli-command options))
         (cli-command (car result-1))
         (options-2 (cdr result-1))
         (result-2 (beads--extract-option :result options-2))
         (result-type (car result-2))
         (options-3 (cdr result-2))
         (result-3 (beads--extract-option :json options-3))
         (json-val (car result-3))
         (json-specified (cl-position :json options-2))
         (options-4 (cdr result-3))
         (result-4 (beads--extract-option :transient options-4))
         (transient-val (car result-4))
         (transient-specified (cl-position :transient options-3))
         (defclass-options (cdr result-4))
         ;; Also strip legacy keywords for backward compatibility
         (result-5 (beads--extract-option :global-section defclass-options))
         (defclass-options (cdr result-5))
         (result-6 (beads--extract-option :parse-as defclass-options))
         (defclass-options (cdr result-6))
         ;; Normalize slots: infer :initarg, :type, :initform, :long-option
         (normalized-slots (mapcar #'beads-meta--normalize-slot-def slots))
         ;; Add cli-command as a class-allocated slot if specified
         (final-slots (if cli-command
                         (append normalized-slots
                                 `((cli-command
                                    :initform ,cli-command
                                    :allocation :class
                                    :documentation "CLI subcommand name.")))
                       normalized-slots))
         ;; Determine transient mode:
         ;; - not specified → auto-generate (t)
         ;; - t → auto-generate
         ;; - nil → no transient
         ;; - :manual → skip auto-generation
         (generate-transient (if transient-specified
                                 (eq transient-val t)
                               t))
         ;; Transient-related names (only needed when generating a menu)
         (transient-name (when generate-transient
                           (beads--derive-transient-name name)))
         (transient-prefix (when transient-name
                             (symbol-name transient-name)))
         ;; Extract docstring for transient
         (doc-pos (cl-position :documentation defclass-options))
         (docstring (when doc-pos (nth (1+ doc-pos) defclass-options)))
         (short-doc (when generate-transient
                      (beads--extract-first-sentence docstring))))
    ;; Defensive superclass check at macro-expansion time
    (dolist (super superclasses)
      (unless (or (find-class super nil)
                  ;; Allow forward references during compilation
                  (bound-and-true-p byte-compile-current-file))
        (lwarn 'beads :warning
               "beads-defcommand %s: superclass %s is not defined"
               name super)))
    (let ((primary-parent (car superclasses)))
      `(progn
         (eval-and-compile
           (defclass ,name ,superclasses ,final-slots ,@defclass-options))
         ;; Register class hierarchy for subcommand derivation (D13)
         ;; and transient menu generation (D14)
         ,@(when (and primary-parent
                      (not (eq primary-parent 'beads-command)))
             `((put ',name 'beads-parent ',primary-parent)
               (cl-pushnew ',name (get ',primary-parent 'beads-children))))
         ,@(when cli-command
             `((cl-defmethod beads-command-subcommand ((_command ,name))
                 ,(format "Return %S as the CLI subcommand name."
                          cli-command)
                 ,cli-command)))
         ,@(when result-type
             `((put ',name 'beads-result ',result-type)))
         ,@(when (and json-specified (not json-val))
             `((put ',name 'beads-json nil)))
         ,@(when (and transient-specified (eq transient-val :manual))
             `((put ',name 'beads-transient :manual)))
         ,@(when generate-transient
             `((beads-meta-define-transient ,name ,transient-prefix
                 ,short-doc
                 beads-option-global-section)))))))

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
Execution returns parsed results directly (domain objects, raw JSON,
or raw stdout).  Commands are immutable/reusable.")

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

;;; Condition Accessor Functions

(defun beads-command-error-exit-code (err)
  "Extract exit code from a beads-command-error condition ERR."
  (plist-get (cddr err) :exit-code))

(defun beads-command-error-stdout (err)
  "Extract stdout string from a beads-command-error condition ERR."
  (plist-get (cddr err) :stdout))

(defun beads-command-error-stderr (err)
  "Extract stderr string from a beads-command-error condition ERR."
  (plist-get (cddr err) :stderr))

;;; Helper Functions

(defun beads-command--format-validation-errors (errors)
  "Format ERRORS into a single string for display.
ERRORS may be a string, a list of strings, or nil."
  (cond
   ((null errors) nil)
   ((stringp errors) errors)
   ((and (listp errors) (= (length errors) 1))
    (car errors))
   ((listp errors)
    (mapconcat #'identity errors "; "))
   (t (format "%s" errors))))

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

Returns the parsed result directly:
  - With :json t: domain objects (beads-issue, etc.) or raw JSON alist
  - With :json nil: raw stdout string
  - Nil return is valid (e.g., empty list results)

If this function returns without signaling, the command succeeded.

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
  "Validate COMMAND and return errors or nil if valid.
Returns a list of error strings, a single error string, or nil.
The base method delegates to `beads-command-validate-slots'.
Override for cross-field validation rules.")

(cl-defgeneric beads-command-validate-slot (command slot-name value)
  "Validate VALUE for SLOT-NAME on COMMAND using slot metadata.
Returns an error string if validation fails, nil if valid.
The base method checks :required and :choices via metadata.
Override with (eql SLOT-NAME) specializer for custom per-slot rules.")

(cl-defgeneric beads-command-validate-slots (command)
  "Validate all slots on COMMAND using metadata.
Loops over command-option slots, calls `beads-command-validate-slot'
for each, and collects errors.  Returns a list of error strings,
or nil if all slots are valid.")

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

(cl-defmethod beads-command-validate-slot ((cmd beads-command)
                                            slot-name value)
  "Validate VALUE for SLOT-NAME on CMD using slot metadata.
Checks :required and :choices via `beads-meta-slot-property'.
Returns an error string if validation fails, nil if valid."
  (let ((class (eieio-object-class cmd)))
    (let ((required (beads-meta-slot-property class slot-name :required))
          (choices (beads-meta-slot-property class slot-name
                                             :transient-choices)))
      (cond
       ((and required
             (or (null value)
                 (and (stringp value) (string-empty-p value))
                 (and (listp value) (null value))))
        (format "%s is required" slot-name))
       ((and choices value (not (member value choices)))
        (format "%s must be one of: %s" slot-name
                (mapconcat (lambda (c) (format "%s" c))
                           choices ", ")))))))

(cl-defmethod beads-command-validate-slots ((cmd beads-command))
  "Validate all command-option slots on CMD using metadata.
Calls `beads-command-validate-slot' for each slot that has any
command-option metadata (:required, :choices, :long-option, etc.).
Returns a list of error strings, or nil if all slots are valid."
  (let ((class (eieio-object-class cmd))
        errors)
    (dolist (slot-name (beads-meta-command-slots class))
      (let ((props (beads-meta-slot-properties class slot-name)))
        ;; Only validate slots that have custom metadata
        (when props
          (let ((value (and (slot-boundp cmd slot-name)
                            (slot-value cmd slot-name))))
            (when-let ((err (beads-command-validate-slot
                             cmd slot-name value)))
              (push err errors))))))
    (nreverse errors)))

(cl-defmethod beads-command-validate ((_command beads-command))
  "Validate base COMMAND.
Default implementation delegates to `beads-command-validate-slots'.
Returns a list of error strings, or nil if valid.
Subclasses may override for cross-field validation rules."
  (beads-command-validate-slots _command))

(cl-defgeneric beads-command-subcommand (command)
  "Return the CLI subcommand name for COMMAND.
For example, \"close\", \"admin compact\", \"federation add-peer\".

Priority:
1. :cli-command class-allocated slot (explicit override)
2. Hierarchy-based derivation via `beads--collect-subcommand-segments'

The hierarchy-based approach walks from the concrete class up to
`beads-command-global-options', collecting name segments.  Each class
contributes its own suffix relative to its parent:
  beads-command-close               -> \"close\"
  beads-command-admin-compact
    inheriting beads-command-admin   -> \"admin compact\"
  beads-command-federation-add-peer
    inheriting beads-command-federation -> \"federation add-peer\"

Returns nil for `beads-command' and `beads-command-global-options'.")

(defun beads--collect-subcommand-segments (class)
  "Collect subcommand name segments by walking CLASS hierarchy.
Returns a list of strings from root to leaf, or nil for base classes.
Each class contributes a segment derived from its name relative to
its parent (strips the parent name prefix + hyphen).
Classes `beads-command' and `beads-command-global-options' are excluded."
  (when (and class
             (not (memq class '(beads-command beads-command-global-options))))
    (let* ((parent (get class 'beads-parent))
           (parent-segments (when parent
                              (beads--collect-subcommand-segments parent)))
           (class-name (symbol-name class)))
      (if (and parent
               (not (memq parent '(beads-command
                                   beads-command-global-options))))
          ;; Has a command parent — derive segment relative to parent
          (let* ((parent-name (symbol-name parent))
                 (prefix (concat parent-name "-"))
                 (segment (if (string-prefix-p prefix class-name)
                              (substring class-name (length prefix))
                            ;; Fallback: strip beads-command- prefix
                            (when (string-match
                                   "\\`beads-command-\\(.+\\)\\'" class-name)
                              (match-string 1 class-name)))))
            (when segment
              (append parent-segments (list segment))))
        ;; Direct child of global-options or beads-command
        (when (string-match "\\`beads-command-\\(.+\\)\\'" class-name)
          (list (match-string 1 class-name)))))))

(cl-defmethod beads-command-subcommand ((command beads-command))
  "Derive subcommand name from COMMAND class hierarchy.
Priority:
1. :cli-command class-allocated slot (explicit override)
2. Hierarchy-based derivation (when beads-parent is registered)
3. Legacy fallback: strip beads-command- prefix, hyphens to spaces
Returns nil for abstract base classes."
  (let* ((class (eieio-object-class command))
         (class-name (symbol-name class)))
    (unless (memq class '(beads-command beads-command-global-options))
      (let ((cli-cmd (and (slot-exists-p command 'cli-command)
                          (slot-boundp command 'cli-command)
                          (with-no-warnings (oref command cli-command)))))
        (or cli-cmd
            ;; Hierarchy-based: use when parent is a real command class
            ;; (not global-options, which is infrastructure)
            (let ((parent (get class 'beads-parent)))
              (when (and parent
                         (not (memq parent '(beads-command
                                             beads-command-global-options))))
                (let ((segments (beads--collect-subcommand-segments class)))
                  (when segments
                    (string-join segments " ")))))
            ;; Legacy fallback: strip prefix, hyphens to spaces
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

(cl-defgeneric beads-command-parse (command stdout)
  "Parse STDOUT and return the parsed result.
COMMAND is the command object (used for method dispatch and configuration).
STDOUT is the standard output string from the process.

This method is called by `beads-command-execute' after successful process
execution (exit code 0).  Errors are signaled before parse runs, so
STDOUT is always from a successful execution.

Dispatches based on json slot:
- When :json is t: Parses JSON from STDOUT, returns alist/vector
- When :json is nil (default): Returns raw STDOUT string
- Subclasses may override to transform parsed JSON into domain objects

Signals `beads-json-parse-error' if JSON parsing fails.")

(cl-defmethod beads-command-parse ((command beads-command) stdout)
  "Parse COMMAND output from STDOUT string.
When json slot is t, parses JSON from STDOUT and returns it.
When the command class has a `beads-result' symbol property,
delegates to `beads-coerce-json-value' for automatic domain
object construction.
When json slot is nil, returns raw STDOUT string.
Signals `beads-json-parse-error' if JSON parsing fails."
  (with-slots (json) command
    (if (not json)
        stdout
      (condition-case err
          (let* ((json-null nil)
                 (json-object-type 'alist)
                 (json-array-type 'vector)
                 (json-key-type 'symbol)
                 (parsed (json-read-from-string stdout))
                 (result-type (get (eieio-object-class command)
                                   'beads-result)))
            (if (null result-type)
                parsed
              (beads-coerce-json-value parsed result-type)))
        (error
         (signal 'beads-json-parse-error
                 (list (format "Failed to parse JSON: %s"
                               (error-message-string err))
                       :stdout stdout
                       :parse-error err)))))))

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
  "Execute COMMAND and return the parsed result directly.
Runs the bd CLI command, parses output via `beads-command-parse',
and returns the parsed result (domain objects, raw JSON, or raw stdout).

If `beads-command-execute' returns without signaling, the command
succeeded.  Nil return is valid (e.g., empty list results).

Signals `beads-validation-error' if command validation fails.
Signals `beads-command-error' if process exits with non-zero code.
Signals `beads-json-parse-error' if JSON parsing fails (for JSON commands)."
  ;; Validate first
  (when-let ((errors (beads-command-validate command)))
    (let ((error-msg (beads-command--format-validation-errors errors)))
      (signal 'beads-validation-error
              (list (format "Command validation failed: %s" error-msg)
                    :command command
                    :error errors))))

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
                                (buffer-string))))

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
                ;; Success: parse output and return result directly
                (beads-command-parse command proc-stdout)
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

(cl-defgeneric beads-command-execute-async (command on-success
                                                    &optional on-error)
  "Execute COMMAND asynchronously without blocking Emacs.

ON-SUCCESS receives the parsed result (same as sync return value).
ON-ERROR receives the error condition; nil means display via `beads--error'.

Signals `beads-validation-error' immediately if validation fails.

Return value: process object (use `delete-process' to cancel).

Example usage:

  (beads-command-execute-async
   (beads-command-list :status \"open\")
   (lambda (result)
     (message \"Got %d issues\" (length result)))
   (lambda (err)
     (message \"Failed: %s\" (cadr err))))")

(cl-defmethod beads-command-execute-async ((command beads-command)
                                           on-success
                                           &optional on-error)
  "Execute COMMAND asynchronously.
ON-SUCCESS receives the parsed result on success.
ON-ERROR receives the error condition; nil means display via `beads--error'.
Signals `beads-validation-error' immediately if validation fails.
Returns process object."
  ;; Validate first - raise error immediately
  (when-let ((errors (beads-command-validate command)))
    (let ((error-msg (beads-command--format-validation-errors errors)))
      (signal 'beads-validation-error
              (list (format "Command validation failed: %s" error-msg)
                    :command command
                    :error errors))))

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
                                     (buffer-string))))

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

                 (if (zerop proc-exit-code)
                     ;; Success: parse and call on-success
                     (condition-case parse-err
                         (let ((result (beads-command-parse
                                        command proc-stdout)))
                           (funcall on-success result))
                       (error
                        (let ((err (list (format "Parse error: %s"
                                                 (error-message-string
                                                  parse-err))
                                         :stdout proc-stdout
                                         :parse-error parse-err)))
                          (if on-error
                              (funcall on-error err)
                            (when (fboundp 'beads--error)
                              (beads--error "%s" (car err)))))))
                   ;; Failure: call on-error or display
                   (let ((err (list (format "Command failed with exit code %d"
                                            proc-exit-code)
                                    :command cmd-string
                                    :exit-code proc-exit-code
                                    :stdout proc-stdout
                                    :stderr proc-stderr)))
                     (if on-error
                         (funcall on-error err)
                       (when (fboundp 'beads--error)
                         (beads--error "%s" (car err)))))))))))
    process))


;;; Convenience Functions

(defun beads-execute (class &rest args)
  "Construct CLASS with ARGS and execute it.
Returns parsed result (domain objects, JSON, or raw stdout).
JSON output is enabled by default unless :json nil is passed.

Example:
  (beads-execute \\='beads-command-close
    :issue-ids \\='(\"bd-1\") :reason \"done\")
  ;; => beads-issue object"
  (unless (find-class class nil)
    (error "Not a beads command class: %S" class))
  (let ((cmd (apply #'make-instance class args)))
    (unless (plist-member args :json)
      (oset cmd json t))
    (beads-command-execute cmd)))

(defun beads-execute-async (class on-success &optional on-error
                                  &rest args)
  "Construct CLASS with ARGS and execute asynchronously.
ON-SUCCESS receives the parsed result.
ON-ERROR receives the condition; nil means display via `beads--error'.

Example:
  (beads-execute-async \\='beads-command-list
    (lambda (issues) (message \"Got %d\" (length issues)))
    nil
    :status \"open\")"
  (unless (find-class class nil)
    (error "Not a beads command class: %S" class))
  (let ((cmd (apply #'make-instance class args)))
    (unless (plist-member args :json)
      (oset cmd json t))
    (beads-command-execute-async cmd on-success on-error)))

(provide 'beads-command)
;;; beads-command.el ends here
