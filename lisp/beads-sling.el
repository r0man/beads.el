;;; beads-sling.el --- Sling target and dispatch ABI -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, agents

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The sling extension seam (design.md §4.5): a target value object
;; and the discovery/dispatch hooks that let downstream packages
;; contribute dispatch destinations to the beads UI.  This
;; foundation owns exactly `beads-sling-target',
;; `beads-sling-target-functions' and `beads-sling-dispatch'; the
;; target collector `beads-sling-targets' is the small consultation
;; function that makes the hook meaningful.  The remaining sling
;; surface (shape inference, validators, named backends) arrives with
;; the sling work items (WI-10/WI-11).
;;
;; Standalone behaviour: with `beads-sling-target-functions' empty,
;; `beads-sling-targets' returns nil, and the default
;; `beads-sling-dispatch' method starts a local agent through the
;; existing agent subsystem (REQ-021).

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'beads-agent)

;;; Sling Target

(defclass beads-sling-target ()
  ((name
    :initarg :name
    :type string
    :documentation "Stable, human-readable name of the target.")
   (kind
    :initarg :kind
    :type (or null symbol string)
    :documentation "Target kind (for example `agent' or `city').")
   (scope
    :initarg :scope
    :type (or null string)
    :documentation "Store or scope the target belongs to, or nil.")
   (backend
    :initarg :backend
    :initform nil
    :type (or null symbol string)
    :documentation "Dispatch backend name; nil or `local' means local.")
   (description
    :initarg :description
    :type (or null string)
    :documentation "One-line description shown in the target list.")
   (metadata
    :initarg :metadata
    :initform nil
    :type list
    :documentation "Free-form plist carried by the extension."))
  "A dispatchable sling target.
Extensions build these and return them from
`beads-sling-target-functions'.  `beads-sling-dispatch' specializes on
this class; a downstream package can subclass it to override dispatch
for its own targets without changing the core.")

;;; Target Discovery

(defvar beads-sling-target-functions nil
  "Hook of functions returning sling targets.
Each function is called with no arguments and returns a list of
`beads-sling-target' objects.  `beads-sling-targets' consults them in
order and dedupes by target name (first function wins).  An empty hook
\(the standalone default) yields no targets.

Downstream use: Gas City contributes its city and rig agents as
targets.")

(defun beads-sling-targets (&optional _bead)
  "Collect, dedupe and return the sling targets for BEAD.
Runs `beads-sling-target-functions', keeps only
`beads-sling-target' instances, and dedupes by `name' so a later
provider cannot shadow an earlier one.  BEAD is currently unused by
the foundation collector but is part of the signature so providers can
be specialized to a bead later.  An empty hook returns nil."
  (let ((seen (make-hash-table :test #'equal))
        (targets nil))
    (dolist (fn beads-sling-target-functions)
      (dolist (target (funcall fn))
        (when (and (eieio-object-p target)
                   (object-of-class-p target 'beads-sling-target))
          (let ((key (format "%s" (oref target name))))
            (unless (gethash key seen)
              (puthash key t seen)
              (push target targets))))))
    (nreverse targets)))

;;; Dispatch

(cl-defgeneric beads-sling-dispatch (target bead prompt)
  "Dispatch BEAD with PROMPT to TARGET and return the started session.
TARGET is a `beads-sling-target', BEAD is an issue id, and PROMPT is
the optional freeform instruction.  The default method starts a local
agent; extension packages specialize this generic on their own target
subclasses to reach their own backend.")

(cl-defmethod beads-sling-dispatch ((target beads-sling-target) bead prompt)
  "Default local method: start a local agent for BEAD with PROMPT.
A TARGET whose `backend' slot is a non-\"local\" string selects that
registered agent backend; a nil or \"local\" backend uses the
configured default.  Delegates to `beads-agent-start'."
  (let* ((backend (oref target backend))
         (backend-name (and (stringp backend)
                            (not (equal backend "local"))
                            backend)))
    (beads-agent-start bead backend-name prompt nil)))

(provide 'beads-sling)
;;; beads-sling.el ends here