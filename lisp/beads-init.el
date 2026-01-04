;;; beads-init.el --- Initialize beads project -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides a transient menu interface for initializing Beads projects.
;; This module uses the transient library to create an interactive menu
;; that allows users to set all parameters for `bd init` command.
;;
;; Usage:
;;   M-x beads-init RET
;;
;; The menu allows setting:
;; - Issue ID prefix (default: current directory name)
;; - Git branch for beads commits (default: current branch)
;; - Database path (optional)
;; - Setup wizards (--contributor or --team)
;; - Non-interactive options (--quiet, --skip-merge-driver)
;;
;; After setting parameters, the suffix command executes bd init
;; and initializes the beads project in the current directory.

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-option)
(require 'transient)

;;; Utility Functions

(defun beads-init--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-init instance.
Returns a beads-command-init object populated with values from ARGS.

This uses transient's standard argument parsing with dash-style
flags."
  (let* ((prefix (beads--sanitize-string (transient-arg-value "--prefix=" args)))
         (branch (beads--sanitize-string (transient-arg-value "--branch=" args)))
         (db (beads--sanitize-string (transient-arg-value "--db=" args)))
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
         (cmd-line (beads-command-line cmd))
         (full-cmd (cons beads-executable cmd-line)))
    (message "Command: %s" (string-join full-cmd " "))))

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

(transient-define-prefix beads-init ()
  "Initialize a new Beads project.

This command provides a transient interface for initializing a
new Beads project in the current directory.  It allows setting
the issue ID prefix, git branch, database path, and various
setup options.

The menu displays all available init parameters organized into
logical groups.  After setting parameters, press 'i' to execute
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

;;; Footer

(provide 'beads-init)
;;; beads-init.el ends here
