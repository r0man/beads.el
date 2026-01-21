;;; beads-command-merge-slot.el --- Merge-slot command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd merge-slot' operations.
;; Merge-slot gates serialize conflict resolution in the merge queue.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-merge-slot-create
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-merge-slot-create (beads-command-json)
  ()
  :documentation "Represents bd merge-slot create command.
Creates a merge slot bead for the current rig."))

(cl-defmethod beads-command-subcommand ((_command beads-command-merge-slot-create))
  "Return \"merge-slot create\" as the CLI subcommand."
  "merge-slot create")

;;; ============================================================
;;; Command Class: beads-command-merge-slot-check
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-merge-slot-check (beads-command-json)
  ()
  :documentation "Represents bd merge-slot check command.
Checks merge slot availability."))

(cl-defmethod beads-command-subcommand ((_command beads-command-merge-slot-check))
  "Return \"merge-slot check\" as the CLI subcommand."
  "merge-slot check")

;;; ============================================================
;;; Command Class: beads-command-merge-slot-acquire
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-merge-slot-acquire (beads-command-json)
  ((holder
    :initarg :holder
    :type (or null string)
    :initform nil
    :documentation "Agent ID requesting the slot."
    :long-option "holder"
    :option-type :string
    :key "h"
    :transient "--holder"
    :class transient-option
    :argument "--holder="
    :prompt "Holder agent: "
    :transient-group "Options"
    :level 1
    :order 1)
   (wait
    :initarg :wait
    :type boolean
    :initform nil
    :documentation "Wait for slot to become available."
    :long-option "wait"
    :option-type :boolean
    :key "w"
    :transient "--wait"
    :class transient-switch
    :argument "--wait"
    :transient-group "Options"
    :level 1
    :order 2))
  :documentation "Represents bd merge-slot acquire command.
Tries to acquire the merge slot."))

(cl-defmethod beads-command-subcommand ((_command beads-command-merge-slot-acquire))
  "Return \"merge-slot acquire\" as the CLI subcommand."
  "merge-slot acquire")

;;; ============================================================
;;; Command Class: beads-command-merge-slot-release
;;; ============================================================

(eval-and-compile
(beads-defcommand beads-command-merge-slot-release (beads-command-json)
  ((holder
    :initarg :holder
    :type (or null string)
    :initform nil
    :documentation "Agent ID releasing the slot."
    :long-option "holder"
    :option-type :string
    :key "h"
    :transient "--holder"
    :class transient-option
    :argument "--holder="
    :prompt "Holder agent: "
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd merge-slot release command.
Releases the merge slot."))

(cl-defmethod beads-command-subcommand ((_command beads-command-merge-slot-release))
  "Return \"merge-slot release\" as the CLI subcommand."
  "merge-slot release")

;;; Execute Interactive Methods

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-merge-slot-create))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-merge-slot-check))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-merge-slot-acquire))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

(cl-defmethod beads-command-execute-interactive ((cmd beads-command-merge-slot-release))
  "Execute CMD in compilation buffer."
  (oset cmd json nil)
  (cl-call-next-method))

;;; Transient Menus

;;;###autoload (autoload 'beads-merge-slot-create "beads-command-merge-slot" nil t)
(beads-meta-define-transient beads-command-merge-slot-create "beads-merge-slot-create"
  "Create merge slot bead for current rig."
  beads-option-global-section)

;;;###autoload (autoload 'beads-merge-slot-check "beads-command-merge-slot" nil t)
(beads-meta-define-transient beads-command-merge-slot-check "beads-merge-slot-check"
  "Check merge slot availability."
  beads-option-global-section)

;;;###autoload (autoload 'beads-merge-slot-acquire "beads-command-merge-slot" nil t)
(beads-meta-define-transient beads-command-merge-slot-acquire "beads-merge-slot-acquire"
  "Acquire the merge slot."
  beads-option-global-section)

;;;###autoload (autoload 'beads-merge-slot-release "beads-command-merge-slot" nil t)
(beads-meta-define-transient beads-command-merge-slot-release "beads-merge-slot-release"
  "Release the merge slot."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-merge-slot "beads-command-merge-slot" nil t)
(transient-define-prefix beads-merge-slot ()
  "Merge-slot gate management.

Merge slots serialize conflict resolution in merge queues."
  ["Merge Slot Commands"
   ("c" "Create slot" beads-merge-slot-create)
   ("C" "Check availability" beads-merge-slot-check)
   ("a" "Acquire slot" beads-merge-slot-acquire)
   ("r" "Release slot" beads-merge-slot-release)])

(provide 'beads-command-merge-slot)
;;; beads-command-merge-slot.el ends here
