;;; beads-command-graph.el --- Graph command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the EIEIO command class for `bd graph' operation.
;; Graph displays a visualization of issue dependency graphs.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-graph
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-graph (beads-command-json)
  ((issue-id
    :initarg :issue-id
    :type (or null string)
    :initform nil
    :documentation "Issue ID to show graph for."
    :positional 1)
   (all
    :initarg :all
    :type boolean
    :initform nil
    :documentation "Show graph for all open issues."
    :long-option "--all"
    :option-type :boolean
    :transient-key "-a"
    :transient-description "--all"
    :transient-class transient-switch
    :transient-argument "--all"
    :transient-group "Options"
    :transient-level 1
    :transient-order 1)
   (box
    :initarg :box
    :type boolean
    :initform t
    :documentation "ASCII boxes showing layers (default)."
    :long-option "--box"
    :option-type :boolean
    :transient-key "-b"
    :transient-description "--box"
    :transient-class transient-switch
    :transient-argument "--box"
    :transient-group "Display"
    :transient-level 1
    :transient-order 2)
   (compact
    :initarg :compact
    :type boolean
    :initform nil
    :documentation "Tree format, one line per issue."
    :long-option "--compact"
    :option-type :boolean
    :transient-key "-c"
    :transient-description "--compact"
    :transient-class transient-switch
    :transient-argument "--compact"
    :transient-group "Display"
    :transient-level 1
    :transient-order 3))
  :documentation "Represents bd graph command.
Displays issue dependency graph visualization."))

(cl-defmethod beads-command-subcommand ((_command beads-command-graph))
  "Return \"graph\" as the CLI subcommand."
  "graph")

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-graph))
  "Execute CMD in compilation buffer with human-readable output."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menu

;;;###autoload (autoload 'beads-graph "beads-command-graph" nil t)
(beads-meta-define-transient beads-command-graph "beads-graph"
  "Display issue dependency graph.

For epics, shows all children and their dependencies.
For regular issues, shows direct dependencies.
With --all, shows all open issues grouped by component.

Display formats:
  --box (default): ASCII boxes showing layers
  --compact: Tree format, one line per issue

Status icons: open in_progress blocked closed deferred"
  beads-option-global-section)

(provide 'beads-command-graph)
;;; beads-command-graph.el ends here
