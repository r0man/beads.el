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

;;;###autoload (autoload 'beads-hooks-install "beads-command-hooks" nil t)
(beads-defcommand beads-command-hooks-install (beads-command-global-options)
  ((force
    :short-option "f"
    :type boolean
    :group "Options"
    :level 1
    :order 1)
   (chain
    :type boolean
    :short-option "c"
    :group "Options"
    :level 1
    :order 2)
   (shared
    :type boolean
    :short-option "s"
    :group "Options"
    :level 1
    :order 3)
   (beads
    :type boolean
    :short-option "b"
    :group "Options"
    :level 1
    :order 4))
  :documentation "Install bd git hooks.")


;;; ============================================================
;;; Command Class: beads-command-hooks-uninstall
;;; ============================================================

;;;###autoload (autoload 'beads-hooks-uninstall "beads-command-hooks" nil t)
(beads-defcommand beads-command-hooks-uninstall (beads-command-global-options)
  ()
  :documentation "Uninstall bd git hooks.")


;;; ============================================================
;;; Command Class: beads-command-hooks-list
;;; ============================================================

;;;###autoload (autoload 'beads-hooks-list "beads-command-hooks" nil t)
(beads-defcommand beads-command-hooks-list (beads-command-global-options)
  ()
  :documentation "List installed git hooks status.")


;;; ============================================================
;;; Command Class: beads-command-hooks-run
;;; ============================================================

;;;###autoload (autoload 'beads-hooks-run "beads-command-hooks" nil t)
(beads-defcommand beads-command-hooks-run (beads-command-global-options)
  ((hook-name
    :positional 1))
  :documentation "Execute a git hook (internal use).")


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
