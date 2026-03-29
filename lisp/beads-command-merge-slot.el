;;; beads-command-merge-slot.el --- Merge-slot command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd merge-slot' operations.
;; Merge-slot gates serialize conflict resolution in the merge queue,
;; preventing multiple agents from racing to resolve conflicts.
;;
;; A merge slot is an exclusive access primitive: only one agent can hold
;; it at a time.  The slot uses:
;;   - status=open:        slot is available
;;   - status=in_progress: slot is held
;;   - metadata.holder:    who currently holds the slot
;;   - metadata.waiters:   priority-ordered queue of waiters
;;
;; Usage:
;;   (beads-command-merge-slot-create!)    ; create the slot bead
;;   (beads-command-merge-slot-check!)     ; check availability
;;   (beads-command-merge-slot-acquire! :holder "furiosa" :wait t)
;;   (beads-command-merge-slot-release! :holder "furiosa")

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; ============================================================
;;; Command Class: beads-command-merge-slot-create
;;; ============================================================

(beads-defcommand beads-command-merge-slot-create (beads-command-global-options)
  ()
  :documentation "Represents bd merge-slot create command.
Creates a merge slot bead for serialized conflict resolution.
The slot ID is generated automatically from the beads prefix."
  :cli-command "merge-slot create")


(cl-defmethod beads-command-validate ((_command beads-command-merge-slot-create))
  "Validate merge-slot create COMMAND.  Always valid."
  nil)

;;;###autoload (autoload 'beads-merge-slot-create "beads-command-merge-slot" nil t)
(beads-meta-define-transient beads-command-merge-slot-create
  "beads-merge-slot-create"
  "Create a merge slot bead for the current rig.

The slot is created with status=open (available).
Run once per rig to enable merge serialization."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-merge-slot-check
;;; ============================================================

(beads-defcommand beads-command-merge-slot-check (beads-command-global-options)
  ()
  :documentation "Represents bd merge-slot check command.
Check if the merge slot is available or held.
Returns: available, held by <holder>, or not found."
  :cli-command "merge-slot check")


(cl-defmethod beads-command-validate ((_command beads-command-merge-slot-check))
  "Validate merge-slot check COMMAND.  Always valid."
  nil)

;;;###autoload (autoload 'beads-merge-slot-check "beads-command-merge-slot" nil t)
(beads-meta-define-transient beads-command-merge-slot-check
  "beads-merge-slot-check"
  "Check if the merge slot is available or held."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-merge-slot-acquire
;;; ============================================================

(beads-defcommand beads-command-merge-slot-acquire (beads-command-global-options)
  ((holder
    :initarg :holder
    :type (or null string)
    :initform nil
    :documentation "Who is acquiring the slot (--holder).
Defaults to BEADS_ACTOR environment variable."
    :long-option "holder"
    :option-type :string
    :key "h"
    :transient "Holder name"
    :class transient-option
    :argument "--holder="
    :prompt "Holder (default: BEADS_ACTOR): "
    :transient-group "Options"
    :level 1
    :order 1)
   (wait
    :initarg :wait
    :type boolean
    :initform nil
    :documentation "Add to waiters list if slot is held (--wait)."
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
Attempt to acquire the merge slot for exclusive access.
If slot is available, acquires it.  If held and --wait is set,
adds to the waiters queue."
  :cli-command "merge-slot acquire")


(cl-defmethod beads-command-validate ((_command beads-command-merge-slot-acquire))
  "Validate merge-slot acquire COMMAND.  Always valid."
  nil)

;;;###autoload (autoload 'beads-merge-slot-acquire "beads-command-merge-slot" nil t)
(beads-meta-define-transient beads-command-merge-slot-acquire
  "beads-merge-slot-acquire"
  "Acquire the merge slot for exclusive conflict resolution access.

If the slot is available, it will be acquired immediately.
If held, use --wait to join the waiters queue."
  beads-option-global-section)

;;; ============================================================
;;; Command Class: beads-command-merge-slot-release
;;; ============================================================

(beads-defcommand beads-command-merge-slot-release (beads-command-global-options)
  ((holder
    :initarg :holder
    :type (or null string)
    :initform nil
    :documentation "Who is releasing the slot, for verification (--holder)."
    :long-option "holder"
    :option-type :string
    :key "h"
    :transient "Holder name (for verification)"
    :class transient-option
    :argument "--holder="
    :prompt "Holder (for verification): "
    :transient-group "Options"
    :level 1
    :order 1))
  :documentation "Represents bd merge-slot release command.
Release the merge slot after conflict resolution is complete.
Sets status back to open and clears the holder field."
  :cli-command "merge-slot release")


(cl-defmethod beads-command-validate ((_command beads-command-merge-slot-release))
  "Validate merge-slot release COMMAND.  Always valid."
  nil)

;;;###autoload (autoload 'beads-merge-slot-release "beads-command-merge-slot" nil t)
(beads-meta-define-transient beads-command-merge-slot-release
  "beads-merge-slot-release"
  "Release the merge slot after conflict resolution is complete.

Sets status back to open and clears the holder field.
If there are waiters, the highest-priority waiter should then acquire."
  beads-option-global-section)

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-merge-slot "beads-command-merge-slot" nil t)
(transient-define-prefix beads-merge-slot ()
  "Merge-slot gate management for serialized conflict resolution.

Merge slots prevent monkey knife fights: only one agent can hold
the slot at a time, preventing cascading conflict storms."
  ["Merge Slot Commands"
   ("c" "Create slot" beads-merge-slot-create)
   ("k" "Check status" beads-merge-slot-check)
   ("a" "Acquire slot" beads-merge-slot-acquire)
   ("r" "Release slot" beads-merge-slot-release)])

(provide 'beads-command-merge-slot)
;;; beads-command-merge-slot.el ends here
