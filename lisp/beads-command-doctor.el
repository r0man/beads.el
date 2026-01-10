;;; beads-command-doctor.el --- Doctor command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-doctor' EIEIO class for the
;; `bd doctor' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd doctor command performs sanity checks on the beads installation:
;; - Database version and migration status
;; - Schema compatibility
;; - Daemon health
;; - Circular dependencies
;; - Git hooks status
;; - And more...

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; Doctor Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-doctor (beads-command-json)
  ((path
    :initarg :path
    :type (or null string)
    :initform nil
    :documentation "Repository path to check (positional argument).
Defaults to current directory."
    :positional 1)
   (check
    :initarg :check
    :type (or null string)
    :initform nil
    :documentation "Run specific check in detail (--check).
Available: pollution."
    :long-option "--check"
    :option-type :string
    :transient-key "c"
    :transient-description "Specific check"
    :transient-class transient-option
    :transient-argument "--check="
    :transient-prompt "Check (pollution): "
    :transient-choices ("pollution")
    :transient-group "Checks"
    :transient-level 2
    :transient-order 1)
   (check-health
    :initarg :check-health
    :type boolean
    :initform nil
    :documentation "Quick health check for git hooks (--check-health).
Silent on success."
    :long-option "--check-health"
    :option-type :boolean
    :transient-key "h"
    :transient-description "Quick health check"
    :transient-class transient-switch
    :transient-argument "--check-health"
    :transient-group "Checks"
    :transient-level 2
    :transient-order 2)
   (deep
    :initarg :deep
    :type boolean
    :initform nil
    :documentation "Validate full graph integrity (--deep).
May be slow on large databases."
    :long-option "--deep"
    :option-type :boolean
    :transient-key "d"
    :transient-description "Deep validation"
    :transient-class transient-switch
    :transient-argument "--deep"
    :transient-group "Checks"
    :transient-level 2
    :transient-order 3)
   (perf
    :initarg :perf
    :type boolean
    :initform nil
    :documentation "Run performance diagnostics (--perf).
Generates CPU profile."
    :long-option "--perf"
    :option-type :boolean
    :transient-key "p"
    :transient-description "Performance diagnostics"
    :transient-class transient-switch
    :transient-argument "--perf"
    :transient-group "Checks"
    :transient-level 2
    :transient-order 4)
   (fix
    :initarg :fix
    :type boolean
    :initform nil
    :documentation "Automatically fix issues where possible (--fix)."
    :long-option "--fix"
    :option-type :boolean
    :transient-key "f"
    :transient-description "Auto-fix issues"
    :transient-class transient-switch
    :transient-argument "--fix"
    :transient-group "Fix Options"
    :transient-level 3
    :transient-order 1)
   (dry-run
    :initarg :dry-run
    :type boolean
    :initform nil
    :documentation "Preview fixes without making changes (--dry-run)."
    :long-option "--dry-run"
    :option-type :boolean
    :transient-key "n"
    :transient-description "Dry run (preview)"
    :transient-class transient-switch
    :transient-argument "--dry-run"
    :transient-group "Fix Options"
    :transient-level 3
    :transient-order 2)
   (yes
    :initarg :yes
    :type boolean
    :initform nil
    :documentation "Skip confirmation prompts (--yes)."
    :long-option "--yes"
    :short-option "-y"
    :option-type :boolean
    :transient-key "y"
    :transient-description "Skip confirmation"
    :transient-class transient-switch
    :transient-argument "--yes"
    :transient-group "Fix Options"
    :transient-level 3
    :transient-order 3)
   (interactive-mode
    :initarg :interactive-mode
    :type boolean
    :initform nil
    :documentation "Confirm each fix individually (--interactive)."
    :long-option "--interactive"
    :short-option "-i"
    :option-type :boolean
    :transient-key "i"
    :transient-description "Interactive mode"
    :transient-class transient-switch
    :transient-argument "--interactive"
    :transient-group "Fix Options"
    :transient-level 3
    :transient-order 4)
   (fix-child-parent
    :initarg :fix-child-parent
    :type boolean
    :initform nil
    :documentation "Also fix child->parent deps (--fix-child-parent).
Opt-in, requires --fix."
    :long-option "--fix-child-parent"
    :option-type :boolean
    :transient-key "C"
    :transient-description "Fix child->parent deps"
    :transient-class transient-switch
    :transient-argument "--fix-child-parent"
    :transient-group "Fix Options"
    :transient-level 4
    :transient-order 5)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force repair even when database can't be opened (--force)."
    :long-option "--force"
    :option-type :boolean
    :transient-key "F"
    :transient-description "Force repair"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Fix Options"
    :transient-level 4
    :transient-order 6)
   (source
    :initarg :source
    :type (or null string)
    :initform nil
    :documentation "Source of truth for recovery (--source).
Values: auto (default), jsonl, db."
    :long-option "--source"
    :option-type :string
    :transient-key "s"
    :transient-description "Source of truth"
    :transient-class transient-option
    :transient-argument "--source="
    :transient-choices ("auto" "jsonl" "db")
    :transient-group "Fix Options"
    :transient-level 4
    :transient-order 7)
   (output
    :initarg :output
    :type (or null string)
    :initform nil
    :documentation "Export diagnostics to JSON file (--output)."
    :long-option "--output"
    :short-option "-o"
    :option-type :string
    :transient-key "o"
    :transient-description "Output file"
    :transient-class transient-option
    :transient-argument "--output="
    :transient-prompt "Output file: "
    :transient-group "Output"
    :transient-level 3
    :transient-order 1)
   (verbose
    :initarg :verbose
    :type boolean
    :initform nil
    :documentation "Show detailed output during fixes (--verbose)."
    :long-option "--verbose"
    :short-option "-v"
    :option-type :boolean
    :transient-key "v"
    :transient-description "Verbose output"
    :transient-class transient-switch
    :transient-argument "--verbose"
    :transient-group "Output"
    :transient-level 3
    :transient-order 2)
   (clean
    :initarg :clean
    :type boolean
    :initform nil
    :documentation "For pollution check: delete detected test issues (--clean)."
    :long-option "--clean"
    :option-type :boolean
    :transient-key "D"
    :transient-description "Clean (delete test issues)"
    :transient-class transient-switch
    :transient-argument "--clean"
    :transient-group "Checks"
    :transient-level 4
    :transient-order 5))
  :documentation "Represents bd doctor command.
Sanity checks the beads installation.
When executed with :json t, returns diagnostic results."))

(cl-defmethod beads-command-subcommand ((_command beads-command-doctor))
  "Return subcommand name for doctor command."
  "doctor")

(cl-defmethod beads-command-validate ((_command beads-command-doctor))
  "Validate doctor COMMAND.
No required fields, returns nil (valid)."
  nil)

(cl-defmethod beads-command-line ((command beads-command-doctor))
  "Build command arguments for doctor COMMAND (without executable).
Returns list: (\"doctor\" ...global-flags... [path] ...options...)."
  (with-slots (path check check-health deep perf fix dry-run yes
                    interactive-mode fix-child-parent force source
                    output verbose clean) command
    (let ((args (list "doctor"))
          (global-args (cl-call-next-method)))
      ;; Append global flags (includes --json if enabled)
      (setq args (append args global-args))

      ;; Positional argument
      (when path
        (setq args (append args (list path))))

      ;; String options
      (when check
        (setq args (append args (list "--check" check))))
      (when source
        (setq args (append args (list "--source" source))))
      (when output
        (setq args (append args (list "--output" output))))

      ;; Boolean flags
      (when check-health
        (setq args (append args (list "--check-health"))))
      (when deep
        (setq args (append args (list "--deep"))))
      (when perf
        (setq args (append args (list "--perf"))))
      (when fix
        (setq args (append args (list "--fix"))))
      (when dry-run
        (setq args (append args (list "--dry-run"))))
      (when yes
        (setq args (append args (list "--yes"))))
      (when interactive-mode
        (setq args (append args (list "--interactive"))))
      (when fix-child-parent
        (setq args (append args (list "--fix-child-parent"))))
      (when force
        (setq args (append args (list "--force"))))
      (when verbose
        (setq args (append args (list "--verbose"))))
      (when clean
        (setq args (append args (list "--clean"))))

      args)))

;; No custom parse needed for doctor - uses parent JSON parse

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-doctor))
  "Execute CMD in compilation buffer with human-readable output.
Disables JSON mode for interactive display with colors."
  ;; Set json to nil for human-readable colored output
  (oset cmd json nil)
  ;; Call the default implementation
  (cl-call-next-method))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-doctor "beads-command-doctor" nil t)
(beads-meta-define-transient beads-command-doctor "beads-doctor"
  "Run diagnostics on beads installation.

Checks database health, schema, daemon status, dependencies, and more.

Transient levels control which options are visible (cycle with C-x l):
  Level 2: Basic checks (specific check, health, deep, perf)
  Level 3: Fix options and output settings
  Level 4: Advanced fix options"
  beads-option-global-section)

(provide 'beads-command-doctor)
;;; beads-command-doctor.el ends here
