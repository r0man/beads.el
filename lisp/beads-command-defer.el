;;; beads-command-defer.el --- Defer command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd defer' and `bd undefer'.
;; Defer/undefer commands manage deferred status on issues.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;; Forward declarations
(declare-function beads-reader-issue-id "beads-reader")

;;; ============================================================
;;; Command Class: beads-command-defer
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-defer (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type list
    :initform nil
    :documentation "Issue IDs to defer."
    :positional-rest t
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue IDs"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue ID(s): "
    :transient-reader beads-reader-issue-id
    :transient-group "Defer"
    :transient-level 1
    :transient-order 1)
   (until
    :initarg :until
    :type (or null string)
    :initform nil
    :documentation "Defer until specific time (e.g., +1h, tomorrow, next monday)."
    :long-option "until"
    :option-type :string
    :transient-key "u"
    :transient-description "--until"
    :transient-class transient-option
    :transient-argument "--until="
    :transient-prompt "Until (e.g., +1h, tomorrow): "
    :transient-group "Options"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd defer command.
Defers one or more issues for later."))

(cl-defmethod beads-command-subcommand ((_command beads-command-defer))
  "Return \"defer\" as the CLI subcommand."
  "defer")

(cl-defmethod beads-command-validate ((command beads-command-defer))
  "Validate defer COMMAND.  Requires at least one issue ID."
  (with-slots (issue-ids) command
    (if (null issue-ids)
        "At least one issue ID is required"
      nil)))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-defer))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; ============================================================
;;; Command Class: beads-command-undefer
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-undefer (beads-command-json)
  ((issue-ids
    :initarg :issue-ids
    :type list
    :initform nil
    :documentation "Issue IDs to undefer."
    :positional-rest t
    ;; Transient properties
    :transient-key "i"
    :transient-description "Issue IDs"
    :transient-class transient-option
    :transient-argument "--id="
    :transient-prompt "Issue ID(s): "
    :transient-reader beads-reader-issue-id
    :transient-group "Undefer"
    :transient-level 1
    :transient-order 1))
  :documentation "Represents bd undefer command.
Undefers one or more issues (restores to open)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-undefer))
  "Return \"undefer\" as the CLI subcommand."
  "undefer")

(cl-defmethod beads-command-validate ((command beads-command-undefer))
  "Validate undefer COMMAND.  Requires at least one issue ID."
  (with-slots (issue-ids) command
    (if (null issue-ids)
        "At least one issue ID is required"
      nil)))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-undefer))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-defer "beads-command-defer" nil t)
(beads-meta-define-transient beads-command-defer "beads-defer"
  "Defer one or more issues for later.

Deferred issues are excluded from ready work lists.
Optionally specify a reason and/or until date."
  beads-option-global-section)

;;;###autoload (autoload 'beads-undefer "beads-command-defer" nil t)
(beads-meta-define-transient beads-command-undefer "beads-undefer"
  "Undefer one or more issues.

Restores deferred issues to open status."
  beads-option-global-section)

(provide 'beads-command-defer)
;;; beads-command-defer.el ends here
