;;; beads-command-diff.el --- Diff command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-diff' EIEIO class for the
;; `bd diff' command.  Shows differences between two commits or branches
;; in the Dolt-backed beads database.
;;
;; Usage:
;;   (beads-execute 'beads-command-diff :from-ref "HEAD~1" :to-ref "HEAD")
;;   (beads-diff)  ; invoke transient menu

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)

;;; Diff Command

(beads-defcommand beads-command-diff (beads-command-global-options)
  ((from-ref
    :positional 1
    :option-type :string
    :key "f"
    :argument "--from="
    :prompt "From ref: "
    :group "Diff"
    :level 1
    :order 1
    :required t)
   (to-ref
    :positional 2
    :option-type :string
    :key "t"
    :argument "--to="
    :prompt "To ref: "
    :group "Diff"
    :level 1
    :order 2
    :required t))
  :documentation "Show differences between two commits or branches.
Requires Dolt backend.")


(cl-defmethod beads-command-validate ((command beads-command-diff))
  "Validate diff COMMAND.
Both from-ref and to-ref are required."
  (with-slots (from-ref to-ref) command
    (cond
     ((or (null from-ref) (string-empty-p from-ref))
      "Must provide a from-ref")
     ((or (null to-ref) (string-empty-p to-ref))
      "Must provide a to-ref"))))


;;; Transient Menu

;;;###autoload (autoload 'beads-diff "beads-command-diff" nil t)
(beads-meta-define-transient beads-command-diff "beads-diff"
  "Show differences between two commits or branches.

Refs can be commit hashes, branch names, or special refs like
HEAD, HEAD~1.  Requires Dolt backend."
  beads-option-global-section)

(provide 'beads-command-diff)
;;; beads-command-diff.el ends here
