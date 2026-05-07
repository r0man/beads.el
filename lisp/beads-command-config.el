;;; beads-command-config.el --- Config command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd config' operations.
;; Config manages configuration settings for integrations and preferences.
;;
;; Each `beads-defcommand' form auto-generates a transient menu from
;; slot metadata -- no separate `beads-meta-define-transient' calls
;; are needed.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; Config Get

;;;###autoload (autoload 'beads-config-get "beads-command-config" nil t)
(beads-defcommand beads-command-config-get (beads-command-global-options)
  ((key
    :positional 1
    :required t))
  :documentation "Represents bd config get command.
Gets a configuration value.")

;; Validate override removed: the base method checks :required slots
;; automatically via beads-command-validate-slots.

;;; Config Set

;;;###autoload (autoload 'beads-config-set "beads-command-config" nil t)
(beads-defcommand beads-command-config-set (beads-command-global-options)
  ((key
    :positional 1
    :required t)
   (value
    :positional 2
    :required t)
   (force-git-tracked
    :type boolean
    :documentation "Allow writing secret keys to git-tracked config files (use with caution)"
    :group "Options"
    :level 3
    :order 1))
  :documentation "Represents bd config set command.
Sets a configuration value.")

;; Validate override removed: the base method checks :required slots
;; automatically via beads-command-validate-slots.

;;; Config List

;;;###autoload (autoload 'beads-config-list "beads-command-config" nil t)
(beads-defcommand beads-command-config-list (beads-command-global-options)
  ()
  :documentation "Represents bd config list command.
Lists all configuration values.")

;;; Config Unset

;;;###autoload (autoload 'beads-config-unset "beads-command-config" nil t)
(beads-defcommand beads-command-config-unset (beads-command-global-options)
  ((key
    :positional 1
    :required t))
  :documentation "Represents bd config unset command.
Deletes a configuration value.")

;; Validate override removed: the base method checks :required slots
;; automatically via beads-command-validate-slots.

;;; Config Set-Many

;;;###autoload (autoload 'beads-config-set-many "beads-command-config" nil t)
(beads-defcommand beads-command-config-set-many (beads-command-global-options)
  ((pairs
    :positional 1
    :type (list-of string)
    :separator " "
    :required t)
   (force-git-tracked
    :type boolean
    :documentation "Allow writing secret keys to git-tracked config files (use with caution)"
    :group "Options"
    :level 3
    :order 1))
  :documentation "Represents bd config set-many command.
Sets multiple configuration values at once with a single auto-commit.
Each argument must be in key=value format."
  :cli-command "config set-many")

;; Validate override removed: the base method checks :required slots
;; automatically via beads-command-validate-slots.

;;; Config Validate

;;;###autoload (autoload 'beads-config-validate "beads-command-config" nil t)
(beads-defcommand beads-command-config-validate (beads-command-global-options)
  ()
  :documentation "Represents bd config validate command.
Validates sync-related configuration settings.")

;;; Config Show

;;;###autoload (autoload 'beads-config-show "beads-command-config" nil t)
(beads-defcommand beads-command-config-show (beads-command-global-options)
  ((source
    :type (or null string)
    :long-option "source"
    :prompt "Filter by source (config.yaml, env, default, ...): "
    :documentation "Filter by source (e.g., config.yaml, env, default, metadata, database, git)"
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd config show command.
Displays a unified view of all effective configuration across all
sources with annotations showing where each value comes from.

Sources (by precedence for Viper-managed keys):
  - env          Environment variable (BD_* or BEADS_*)
  - config.yaml  Project config file (.beads/config.yaml)
  - default      Built-in default value

Additional sources:
  - metadata     Connection settings from .beads/metadata.json
  - database     Integration config stored in the Dolt database
  - git          Git config (e.g., beads.role)")

;;; Config Apply

;;;###autoload (autoload 'beads-config-apply "beads-command-config" nil t)
(beads-defcommand beads-command-config-apply (beads-command-global-options)
  ((dry-run
    :type boolean
    :group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd config apply command.
Reconciles actual system state to match declared configuration.

Runs drift detection and then fixes any mismatches it finds:
  - hooks     Reinstall git hooks if missing or outdated
  - remote    Add/update Dolt origin remote to match federation.remote
  - server    Start Dolt server if dolt.shared-server is enabled

This command is idempotent -- safe to run multiple times.  Use
--dry-run to preview what would change without making
modifications.")

;;; Config Drift

;;;###autoload (autoload 'beads-config-drift "beads-command-config" nil t)
(beads-defcommand beads-command-config-drift (beads-command-global-options)
  ()
  :documentation "Represents bd config drift command.
Detects drift between declared configuration and actual system state.

This is a read-only diagnostic that answers \"is my environment
consistent with my config?\" -- no mutations are performed.

Checks:
  - hooks     Git hooks installed and up-to-date
  - remote    Dolt remote matches federation.remote config
  - server    Server state matches dolt.shared-server config

Exit codes:
  0  No drift detected (all checks ok/info/skipped)
  1  Drift detected (at least one check has status \"drift\")")

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-config "beads-command-config" nil t)
(transient-define-prefix beads-config ()
  "Manage beads configuration.

Configuration is stored per-project in .beads/*.db.

Common namespaces:
  jira.*    - Jira integration
  linear.*  - Linear integration
  github.*  - GitHub integration
  status.*  - Issue status configuration"
  ["Config Commands"
   ("g" "Get value" beads-config-get)
   ("s" "Set value" beads-config-set)
   ("S" "Set many" beads-config-set-many)
   ("l" "List all" beads-config-list)
   ("u" "Unset value" beads-config-unset)
   ("w" "Show effective config" beads-config-show)
   ("a" "Apply (reconcile)" beads-config-apply)
   ("d" "Drift detection" beads-config-drift)
   ("v" "Validate config" beads-config-validate)]
  ["Quick Actions"
   ("q" "Quit" transient-quit-one)])

(provide 'beads-command-config)
;;; beads-command-config.el ends here
