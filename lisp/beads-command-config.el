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
    :required t))
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
    :required t))
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
   ("v" "Validate config" beads-config-validate)])

(provide 'beads-command-config)
;;; beads-command-config.el ends here
