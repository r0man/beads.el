;;; beads-command-config.el --- Config command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd config' operations.
;; Config manages configuration settings for integrations and preferences.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-config-get
;;; ============================================================

(beads-defcommand beads-command-config-get (beads-command-global-options)
  ((key
    :initarg :key
    :type (or null string)
    :initform nil
    :documentation "Configuration key to get."
    :positional 1))
  :documentation "Represents bd config get command.
Gets a configuration value.")


(cl-defmethod beads-command-validate ((command beads-command-config-get))
  "Validate config get COMMAND.  Requires key."
  (with-slots (key) command
    (cond
     ((not key) "Configuration key is required")
     ((string-empty-p key) "Configuration key cannot be empty")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-config-set
;;; ============================================================

(beads-defcommand beads-command-config-set (beads-command-global-options)
  ((key
    :initarg :key
    :type (or null string)
    :initform nil
    :documentation "Configuration key to set."
    :positional 1)
   (value
    :initarg :value
    :type (or null string)
    :initform nil
    :documentation "Value to set."
    :positional 2))
  :documentation "Represents bd config set command.
Sets a configuration value.")


(cl-defmethod beads-command-validate ((command beads-command-config-set))
  "Validate config set COMMAND.  Requires key and value."
  (with-slots (key value) command
    (cond
     ((not key) "Configuration key is required")
     ((string-empty-p key) "Configuration key cannot be empty")
     ((not value) "Value is required")
     (t nil))))

;;; ============================================================
;;; Command Class: beads-command-config-list
;;; ============================================================

(beads-defcommand beads-command-config-list (beads-command-global-options)
  ()
  :documentation "Represents bd config list command.
Lists all configuration values.")


;;; ============================================================
;;; Command Class: beads-command-config-unset
;;; ============================================================

(beads-defcommand beads-command-config-unset (beads-command-global-options)
  ((key
    :initarg :key
    :type (or null string)
    :initform nil
    :documentation "Configuration key to unset."
    :positional 1))
  :documentation "Represents bd config unset command.
Deletes a configuration value.")


(cl-defmethod beads-command-validate ((command beads-command-config-unset))
  "Validate config unset COMMAND.  Requires key."
  (with-slots (key) command
    (cond
     ((not key) "Configuration key is required")
     ((string-empty-p key) "Configuration key cannot be empty")
     (t nil))))

;;; Execute Interactive Methods





;;; Transient Menus

;;;###autoload (autoload 'beads-config-get "beads-command-config" nil t)
(beads-meta-define-transient beads-command-config-get "beads-config-get"
  "Get a configuration value."
  beads-option-global-section)

;;;###autoload (autoload 'beads-config-set "beads-command-config" nil t)
(beads-meta-define-transient beads-command-config-set "beads-config-set"
  "Set a configuration value."
  beads-option-global-section)

;;;###autoload (autoload 'beads-config-list "beads-command-config" nil t)
(beads-meta-define-transient beads-command-config-list "beads-config-list"
  "List all configuration values."
  beads-option-global-section)

;;;###autoload (autoload 'beads-config-unset "beads-command-config" nil t)
(beads-meta-define-transient beads-command-config-unset "beads-config-unset"
  "Delete a configuration value."
  beads-option-global-section)

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
   ("l" "List all" beads-config-list)
   ("u" "Unset value" beads-config-unset)])

(provide 'beads-command-config)
;;; beads-command-config.el ends here
