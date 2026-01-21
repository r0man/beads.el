;;; beads-command-init.el --- Init command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-init' EIEIO class for the
;; `bd init' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd init command initializes beads in the current directory by
;; creating a .beads/ directory and database file.  Optionally specify
;; a custom issue prefix.
;;
;; Features:
;; - Custom issue prefix
;; - Git branch configuration for beads commits
;; - Setup wizards for contributors and teams
;; - Stealth mode for invisible beads usage
;; - Force re-initialization
;;
;; Usage:
;;   (beads-command-execute (beads-command-init :prefix "myproject"))
;;   (beads-command-init!)  ; convenience function

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;; Forward declarations
(declare-function beads--sanitize-string "beads")
(declare-function beads--error "beads")
(defvar beads-executable)

;;; Init Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
(beads-defcommand beads-command-init (beads-command)
  ((branch
    :initarg :branch
    :type (or null string)
    :initform nil
    :documentation "Git branch for beads commits (-b, --branch).
Default: current branch."
    ;; CLI properties
    :long-option "branch"
    :short-option "b"
    :option-type :string
    ;; Transient properties
    :key "b"
    :transient "--branch"
    :class transient-option
    :argument "--branch="
    :prompt "Branch: "
    :transient-group "Basic Options"
    :level 1
    :order 2)
   (contributor
    :initarg :contributor
    :type boolean
    :initform nil
    :documentation "Run OSS contributor setup wizard (--contributor)."
    ;; CLI properties
    :long-option "contributor"
    :option-type :boolean
    ;; Transient properties
    :key "C"
    :transient "--contributor"
    :class transient-switch
    :argument "--contributor"
    :transient-group "Setup Wizards"
    :level 2
    :order 1)
   (force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Force re-initialization even if JSONL already has issues
(--force).  May cause data loss."
    ;; CLI properties
    :long-option "force"
    :option-type :boolean
    ;; Transient properties
    :key "f"
    :transient "--force (may lose data)"
    :class transient-switch
    :argument "--force"
    :transient-group "Advanced"
    :level 3
    :order 1)
   (from-jsonl
    :initarg :from-jsonl
    :type boolean
    :initform nil
    :documentation "Import from current .beads/issues.jsonl file instead
of git history (--from-jsonl).  Preserves manual cleanups."
    ;; CLI properties
    :long-option "from-jsonl"
    :option-type :boolean
    ;; Transient properties
    :key "j"
    :transient "--from-jsonl"
    :class transient-switch
    :argument "--from-jsonl"
    :transient-group "Advanced"
    :level 3
    :order 2)
   (prefix
    :initarg :prefix
    :type (or null string)
    :initform nil
    :documentation "Issue prefix (-p, --prefix).
Default: current directory name."
    ;; CLI properties
    :long-option "prefix"
    :short-option "p"
    :option-type :string
    ;; Transient properties
    :key "p"
    :transient "--prefix"
    :class transient-option
    :argument "--prefix="
    :prompt "Prefix: "
    :transient-group "Basic Options"
    :level 1
    :order 1)
   (quiet
    :initarg :quiet
    :type boolean
    :initform nil
    :documentation "Suppress output (-q, --quiet)."
    ;; CLI properties
    :long-option "quiet"
    :short-option "q"
    :option-type :boolean
    ;; Transient properties
    :key "q"
    :transient "--quiet"
    :class transient-switch
    :argument "--quiet"
    :transient-group "Other Options"
    :level 2
    :order 1)
   (setup-exclude
    :initarg :setup-exclude
    :type boolean
    :initform nil
    :documentation "Configure .git/info/exclude to keep beads files local
(--setup-exclude).  For forks."
    ;; CLI properties
    :long-option "setup-exclude"
    :option-type :boolean
    ;; Transient properties
    :key "e"
    :transient "--setup-exclude"
    :class transient-switch
    :argument "--setup-exclude"
    :transient-group "Advanced"
    :level 3
    :order 3)
   (skip-hooks
    :initarg :skip-hooks
    :type boolean
    :initform nil
    :documentation "Skip git hooks installation (--skip-hooks)."
    ;; CLI properties
    :long-option "skip-hooks"
    :option-type :boolean
    ;; Transient properties
    :key "H"
    :transient "--skip-hooks"
    :class transient-switch
    :argument "--skip-hooks"
    :transient-group "Other Options"
    :level 2
    :order 2)
   (skip-merge-driver
    :initarg :skip-merge-driver
    :type boolean
    :initform nil
    :documentation "Skip git merge driver setup (--skip-merge-driver).
Non-interactive mode."
    ;; CLI properties
    :long-option "skip-merge-driver"
    :option-type :boolean
    ;; Transient properties
    :key "M"
    :transient "--skip-merge-driver"
    :class transient-switch
    :argument "--skip-merge-driver"
    :transient-group "Other Options"
    :level 2
    :order 3)
   (stealth
    :initarg :stealth
    :type boolean
    :initform nil
    :documentation "Enable stealth mode (--stealth).
Global gitattributes and gitignore, no local repo tracking.
Perfect for personal use without affecting repo collaborators."
    ;; CLI properties
    :long-option "stealth"
    :option-type :boolean
    ;; Transient properties
    :key "s"
    :transient "--stealth"
    :class transient-switch
    :argument "--stealth"
    :transient-group "Advanced"
    :level 3
    :order 4)
   (team
    :initarg :team
    :type boolean
    :initform nil
    :documentation "Run team workflow setup wizard (--team)."
    ;; CLI properties
    :long-option "team"
    :option-type :boolean
    ;; Transient properties
    :key "T"
    :transient "--team"
    :class transient-switch
    :argument "--team"
    :transient-group "Setup Wizards"
    :level 2
    :order 2))
  :documentation "Represents bd init command.
Initializes bd in the current directory by creating .beads/ directory
and database file."))

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

(cl-defmethod beads-command-execute-interactive ((_cmd beads-command-init))
  "Execute CMD in terminal buffer.
Uses the terminal backend for interactive setup wizards."
  ;; Call the default implementation
  (cl-call-next-method))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-init-transient "beads-command-init" nil t)
(beads-meta-define-transient beads-command-init "beads-init-transient"
  "Initialize a new Beads project (auto-generated menu).

See `beads-init' for the full user-facing transient menu."
  beads-option-global-section)

;;; Interactive Init Workflow

(defun beads-init--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-init instance.
Returns a beads-command-init object populated with values from ARGS.

This uses transient's standard argument parsing with dash-style
flags."
  (let* ((prefix (beads--sanitize-string
                  (transient-arg-value "--prefix=" args)))
         (branch (beads--sanitize-string
                  (transient-arg-value "--branch=" args)))
         (db (beads--sanitize-string
              (transient-arg-value "--db=" args)))
         (contributor (and (member "--contributor" args) t))
         (quiet (and (member "--quiet" args) t))
         (skip-merge-driver (and (member "--skip-merge-driver" args) t))
         (team (and (member "--team" args) t)))
    (beads-command-init
     :prefix prefix
     :branch branch
     :db db
     :contributor contributor
     :quiet quiet
     :skip-merge-driver skip-merge-driver
     :team team)))

(defun beads-init--validate-all (cmd)
  "Validate all parameters from CMD beads-command-init instance.
Returns list of error messages, or nil if all valid."
  (let ((validation-error (beads-command-validate cmd)))
    (when validation-error
      (list validation-error))))

;;; Suffix Commands

(transient-define-suffix beads-init--execute ()
  "Execute the bd init command with current parameters."
  :key "x"
  :description "Initialize"
  (interactive)
  (when (y-or-n-p "Initialize Beads project in current directory? ")
    (let* ((args (transient-args 'beads-init))
           (cmd (beads-init--parse-transient-args args))
           (errors (beads-init--validate-all cmd)))
      (if errors
          (user-error "Validation failed: %s" (string-join errors "; "))
        (condition-case err
            (progn
              (beads-command-execute cmd)
              (message "Beads project initialized%s"
                       (if (oref cmd prefix)
                           (format " with prefix '%s'" (oref cmd prefix))
                         ""))
              ;; Return nil to close transient
              nil)
          (error
           (beads--error "Failed to initialize: %s"
                         (error-message-string err))))))))

(transient-define-suffix beads-init--preview ()
  "Preview the bd init command without executing it."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-init))
         (cmd (beads-init--parse-transient-args args))
         (cmd-line (beads-command-line cmd)))
    (message "Command: %s" (string-join cmd-line " "))))

(transient-define-suffix beads-init--reset ()
  "Reset all init parameters."
  :key "R"
  :description "Reset all fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload (autoload 'beads-init "beads-command-init" nil t)
(transient-define-prefix beads-init ()
  "Initialize a new Beads project.

This command provides a transient interface for initializing a
new Beads project in the current directory.  It allows setting
the issue ID prefix, git branch, database path, and various
setup options.

The menu displays all available init parameters organized into
logical groups.  After setting parameters, press \\='x\\=' to execute
the initialization."
  :value (lambda () nil)
  ["Initialization Parameters"
   :class transient-columns
   :description "Set initialization options"
   ["Basic Options"
    (beads-option-init-prefix)
    (beads-option-init-branch)
    (beads-option-init-db)]
   ["Setup Wizards"
    (beads-option-init-contributor)
    (beads-option-init-team)]
   ["Other Options"
    (beads-option-init-quiet)
    (beads-option-init-skip-merge-driver)]]
  ["Actions"
   (beads-init--execute)
   (beads-init--preview)
   (beads-init--reset)])

(provide 'beads-command-init)
;;; beads-command-init.el ends here
