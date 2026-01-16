;;; beads-command-hooks.el --- Hooks command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd hooks' operations.
;; Hooks manages git hooks for automatic bd sync.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-hooks-install
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-hooks-install (beads-command-json)
  ((force
    :initarg :force
    :type boolean
    :initform nil
    :documentation "Overwrite existing hooks."
    :long-option "force"
    :short-option "f"
    :option-type :boolean
    :transient-key "f"
    :transient-description "--force"
    :transient-class transient-switch
    :transient-argument "--force"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (chain
    :initarg :chain
    :type boolean
    :initform nil
    :documentation "Chain with existing hooks instead of replacing."
    :long-option "chain"
    :option-type :boolean
    :transient-key "c"
    :transient-description "--chain"
    :transient-class transient-switch
    :transient-argument "--chain"
    :transient-group "Options"
    :transient-level 1
    :transient-order 2)
   (shared
    :initarg :shared
    :type boolean
    :initform nil
    :documentation "Install for all repos using shared hooks directory."
    :long-option "shared"
    :option-type :boolean
    :transient-key "s"
    :transient-description "--shared"
    :transient-class transient-switch
    :transient-argument "--shared"
    :transient-group "Options"
    :transient-level 1
    :transient-order 3))
  :documentation "Represents bd hooks install command.
Installs bd git hooks."))

(cl-defmethod beads-command-subcommand ((_command beads-command-hooks-install))
  "Return \"hooks install\" as the CLI subcommand."
  "hooks install")

;;; ============================================================
;;; Command Class: beads-command-hooks-uninstall
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-hooks-uninstall (beads-command-json)
  ()
  :documentation "Represents bd hooks uninstall command.
Uninstalls bd git hooks."))

(cl-defmethod beads-command-subcommand ((_command beads-command-hooks-uninstall))
  "Return \"hooks uninstall\" as the CLI subcommand."
  "hooks uninstall")

;;; ============================================================
;;; Command Class: beads-command-hooks-list
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-hooks-list (beads-command-json)
  ()
  :documentation "Represents bd hooks list command.
Lists installed git hooks status."))

(cl-defmethod beads-command-subcommand ((_command beads-command-hooks-list))
  "Return \"hooks list\" as the CLI subcommand."
  "hooks list")

;;; ============================================================
;;; Command Class: beads-command-hooks-run
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-hooks-run (beads-command-json)
  ((hook-name
    :initarg :hook-name
    :type (or null string)
    :initform nil
    :documentation "Hook name to run."
    :positional 1))
  :documentation "Represents bd hooks run command.
Executes a git hook (called by thin shims)."))

(cl-defmethod beads-command-subcommand ((_command beads-command-hooks-run))
  "Return \"hooks run\" as the CLI subcommand."
  "hooks run")

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-hooks-install))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-hooks-uninstall))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-hooks-list))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-hooks-run))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-hooks-install "beads-command-hooks" nil t)
(beads-meta-define-transient beads-command-hooks-install "beads-hooks-install"
  "Install bd git hooks.

Hooks ensure automatic sync:
- pre-commit: Flush pending changes
- post-merge: Import updated JSONL
- pre-push: Prevent pushing stale JSONL
- post-checkout: Import after branch switch"
  beads-option-global-section)

;;;###autoload (autoload 'beads-hooks-uninstall "beads-command-hooks" nil t)
(beads-meta-define-transient beads-command-hooks-uninstall "beads-hooks-uninstall"
  "Uninstall bd git hooks."
  beads-option-global-section)

;;;###autoload (autoload 'beads-hooks-list "beads-command-hooks" nil t)
(beads-meta-define-transient beads-command-hooks-list "beads-hooks-list"
  "List installed git hooks status."
  beads-option-global-section)

;;;###autoload (autoload 'beads-hooks-run "beads-command-hooks" nil t)
(beads-meta-define-transient beads-command-hooks-run "beads-hooks-run"
  "Execute a git hook (internal use)."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-hooks "beads-command-hooks" nil t)
(transient-define-prefix beads-hooks ()
  "Manage bd git hooks.

Git hooks provide automatic sync on git operations."
  ["Hooks Commands"
   ("i" "Install hooks" beads-hooks-install)
   ("u" "Uninstall hooks" beads-hooks-uninstall)
   ("l" "List hooks" beads-hooks-list)])

(provide 'beads-command-hooks)
;;; beads-command-hooks.el ends here
