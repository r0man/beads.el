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

(beads-defcommand beads-command-doctor (beads-command-global-options)
  ((path
    :positional 1)
   (check
    :type (or null string)
    :short-option "c"
    :prompt "Check (pollution): "
    :choices ("pollution")
    :group "Checks"
    :level 2
    :order 1)
   (check-health
    :type boolean
    :short-option "h"
    :group "Checks"
    :level 2
    :order 2)
   (deep
    :type boolean
    :short-option "d"
    :group "Checks"
    :level 2
    :order 3)
   (perf
    :type boolean
    :short-option "p"
    :group "Checks"
    :level 2
    :order 4)
   (fix
    :type boolean
    :short-option "f"
    :group "Fix Options"
    :level 3
    :order 1)
   (dry-run
    :type boolean
    :short-option "n"
    :group "Fix Options"
    :level 3
    :order 2)
   (yes
    :short-option "y"
    :type boolean
    :group "Fix Options"
    :level 3
    :order 3)
   (interactive-mode
    :long-option "interactive"
    :short-option "i"
    :type boolean
    :group "Fix Options"
    :level 3
    :order 4)
   (fix-child-parent
    :type boolean
    :short-option "C"
    :group "Fix Options"
    :level 4
    :order 5)
   (force
    :type boolean
    :short-option "F"
    :group "Fix Options"
    :level 4
    :order 6)
   (source
    :type (or null string)
    :short-option "s"
    :choices ("auto" "jsonl" "db")
    :group "Fix Options"
    :level 4
    :order 7)
   (output
    :short-option "o"
    :type (or null string)
    :prompt "Output file: "
    :group "Output"
    :level 3
    :order 1)
   (verbose
    :short-option "v"
    :type boolean
    :group "Output"
    :level 3
    :order 2)
   (clean
    :type boolean
    :short-option "D"
    :group "Checks"
    :level 4
    :order 5))
  :documentation "Represents bd doctor command.
Sanity checks the beads installation.
When executed with :json t, returns diagnostic results.")


;; Validate override removed: base handles slot-level validation.

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
