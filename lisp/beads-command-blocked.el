;;; beads-command-blocked.el --- Blocked command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-blocked' EIEIO class for the
;; `bd blocked' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd blocked command shows blocked issues (issues with unresolved
;; blockers).  This is useful for understanding what work is stalled
;; waiting on other issues.
;;
;; Features:
;; - Filter by parent epic/bead
;;
;; Usage:
;;   (beads-command-execute (beads-command-blocked))
;;   (beads-command-blocked!)  ; convenience function

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'beads-types)

;;; Blocked Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-blocked (beads-command-json)
  ((parent
    :initarg :parent
    :type (or null string)
    :initform nil
    :documentation "Filter to descendants of this bead/epic (--parent)."
    ;; CLI properties
    :long-option "parent"
    :option-type :string
    ;; Transient properties
    :transient-key "P"
    :transient-description "--parent"
    :transient-class transient-option
    :transient-argument "--parent="
    :transient-prompt "Parent ID: "
    :transient-group "Scope"
    :transient-level 2
    :transient-order 1))
  :documentation "Represents bd blocked command.
Shows blocked issues (issues with unresolved blockers).
When executed with :json t, returns list of beads-blocked-issue instances."))

(cl-defmethod beads-command-subcommand ((_command beads-command-blocked))
  "Return \"blocked\" as the CLI subcommand name."
  "blocked")

(cl-defmethod beads-command-parse ((command beads-command-blocked) execution)
  "Parse blocked COMMAND output from EXECUTION.
Returns list of beads-blocked-issue instances.
When :json is nil, falls back to parent (returns raw stdout).
When :json is t, converts parsed JSON to beads-blocked-issue instances.
Does not modify any slots."
  (with-slots (json) command
    (if (not json)
        ;; If json is not enabled, use parent implementation
        (cl-call-next-method)
      ;; Call parent to parse JSON, then convert to beads-blocked-issue instances
      (let ((parsed-json (cl-call-next-method)))
        (condition-case err
            (mapcar #'beads-blocked-issue-from-json (append parsed-json nil))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to create beads-blocked-issue instances: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :parsed-json parsed-json
                         :stderr (oref execution stderr)
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-blocked))
  "Execute CMD in compilation buffer with human-readable output.
Disables JSON mode for interactive display with colors."
  ;; Set json to nil for human-readable colored output
  (oset cmd json nil)
  ;; Call the default implementation
  (cl-call-next-method))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-blocked-transient "beads-command-blocked" nil t)
(beads-meta-define-transient beads-command-blocked "beads-blocked-transient"
  "Show blocked issues (issues with unresolved blockers).

Blocked issues are those that have dependencies that are not yet closed.
This helps understand what work is stalled waiting on other issues.

Transient levels control which options are visible (cycle with C-x l):
  Level 2: Scope filter (parent)"
  beads-option-global-section)

(provide 'beads-command-blocked)
;;; beads-command-blocked.el ends here
