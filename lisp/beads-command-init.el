;;; beads-command-init.el --- Init command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-init' EIEIO class for the
;; `bd init' command.  The class uses shorthand slot syntax with
;; `beads-defcommand'.  Init uses `:transient :manual' because the
;; auto-derived name (beads-init) differs from the hand-written
;; transient (beads-init-transient).  Bootstrap uses auto-generated
;; transient menus -- no separate `beads-meta-define-transient' call
;; is needed.
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
;;   (beads-execute 'beads-command-init)  ; convenience function

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

(beads-defcommand beads-command-init (beads-command-global-options)
  ((branch
    :short-option "b"
    :type (or null string)
    :group "Basic Options"
    :level 1
    :order 2)
   (contributor
    :type boolean
    :short-option "C"
    :group "Setup Wizards"
    :level 2
    :order 1)
   (force
    :type boolean
    :short-option "f"
    :group "Advanced"
    :level 3
    :order 1)
   (from-jsonl
    :type boolean
    :short-option "j"
    :group "Advanced"
    :level 3
    :order 2)
   (prefix
    :short-option "p"
    :type (or null string)
    :group "Basic Options"
    :level 1
    :order 1)
   (quiet
    :short-option "q"
    :type boolean
    :group "Other Options"
    :level 2
    :order 1)
   (server-host
    :type (or null string)
    :short-option "h"
    :prompt "Server host: "
    :group "Server Connection"
    :level 3
    :order 1)
   (server-port
    :type (or null string integer)
    :short-option "P"
    :prompt "Server port: "
    :group "Server Connection"
    :level 3
    :order 2)
   (server-user
    :type (or null string)
    :short-option "U"
    :prompt "Server user: "
    :group "Server Connection"
    :level 3
    :order 3)
   (setup-exclude
    :type boolean
    :short-option "e"
    :group "Advanced"
    :level 3
    :order 3)
   (skip-hooks
    :type boolean
    :short-option "H"
    :group "Other Options"
    :level 2
    :order 2)
   (stealth
    :type boolean
    :short-option "s"
    :group "Advanced"
    :level 3
    :order 4)
   (team
    :type boolean
    :short-option "T"
    :group "Setup Wizards"
    :level 2
    :order 2))
  :documentation "Represents bd init command.
Initializes bd in the current directory by creating .beads/ directory
and database file."
  :json nil
  :transient :manual)


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

;; Parse override removed: base method returns raw stdout when :json nil.

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
         (skip-hooks (and (member "--skip-hooks" args) t))
         (team (and (member "--team" args) t))
         (server-host (beads--sanitize-string
                       (transient-arg-value "--server-host=" args)))
         (server-port-str (transient-arg-value "--server-port=" args))
         (server-port (when server-port-str
                        (string-to-number server-port-str)))
         (server-user (beads--sanitize-string
                       (transient-arg-value "--server-user=" args))))
    (beads-command-init
     :prefix prefix
     :branch branch
     :db db
     :contributor contributor
     :quiet quiet
     :skip-hooks skip-hooks
     :team team
     :server-host server-host
     :server-port server-port
     :server-user server-user)))

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
    (beads-option-init-skip-hooks)]]
  ["Actions"
   (beads-init--execute)
   (beads-init--preview)
   (beads-init--reset)])

;;; Bootstrap Command

;;;###autoload (autoload 'beads-bootstrap "beads-command-init" nil t)
(beads-defcommand beads-command-bootstrap (beads-command-global-options)
  ((dry-run
    :type boolean
    :short-option "n"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd bootstrap command.
Non-destructive database setup for fresh clones and recovery.")

(provide 'beads-command-init)
;;; beads-command-init.el ends here
