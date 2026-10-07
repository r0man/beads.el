;;; beads-command-swarm.el --- Swarm command classes for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines EIEIO command classes for `bd swarm' operations.
;; Swarm manages parallel work coordination on epics.

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)
(require 'beads-prefix)

;;; ============================================================
;;; Command Class: beads-command-swarm-create
;;; ============================================================

;;;###autoload (autoload 'beads-swarm-create "beads-command-swarm" nil t)
(beads-defcommand beads-command-swarm-create (beads-command-global-options)
  ((epic-id
    :positional 1
    :required t)
   (coordinator
    :type (or null string)
    :short-option "c"
    :prompt "Coordinator agent: "
    :group "Options"
    :level 1
    :order 1)
   (force
    :type boolean
    :short-option "f"
    :group "Options"
    :level 1
    :order 2))
  :result beads-swarm-create-result
  :documentation "Creates a swarm molecule from an epic.")

;;; ============================================================
;;; Command Class: beads-command-swarm-list
;;; ============================================================

;;;###autoload (autoload 'beads-swarm-list "beads-command-swarm" nil t)
(beads-defcommand beads-command-swarm-list (beads-command-global-options)
  ()
  :result (list-of beads-swarm-list-item)
  :documentation "Lists all swarm molecules.")

(cl-defmethod beads-command-parse ((_command beads-command-swarm-list) stdout)
  "Parse `bd swarm list --json' STDOUT into swarm list items.
The CLI wraps the array in a `swarms' key; unwrap it and coerce each
entry through `beads-from-json' (no second parser)."
  (when (and stdout (not (string-empty-p (string-trim stdout))))
    (let* ((json-null nil)
           (json-object-type 'alist)
           (json-array-type 'list)
           (json-key-type 'symbol)
           (parsed (json-read-from-string stdout)))
      (mapcar (lambda (item)
                (beads-from-json 'beads-swarm-list-item item))
              (alist-get 'swarms parsed)))))

;;; ============================================================
;;; Command Class: beads-command-swarm-status
;;; ============================================================

;;;###autoload (autoload 'beads-swarm-status "beads-command-swarm" nil t)
;; This command's generated transient is also named `beads-swarm-status',
;; which collides with the EIEIO constructor of the `beads-swarm-status'
;; result type.  The transient is the intended definition; suppress the
;; redefinition warning it emits for the class constructor.
(with-no-warnings
  (beads-defcommand beads-command-swarm-status (beads-command-global-options)
    ((swarm-id
      :positional 1))
    :result beads-swarm-status
    :documentation "Shows current swarm status."))

;;; ============================================================
;;; Command Class: beads-command-swarm-validate
;;; ============================================================

;;;###autoload (autoload 'beads-swarm-validate "beads-command-swarm" nil t)
(beads-defcommand beads-command-swarm-validate (beads-command-global-options)
  ((epic-id
    :positional 1
    :required t))
  :result beads-swarm-analysis
  :documentation "Validates epic structure for swarming.")

(defun beads-swarm-domain-error-p (parsed)
  "Return the swarm domain-error string in PARSED, or nil.
PARSED is the value returned by `beads-command-execute' for a
`beads-command-swarm-create' or `beads-command-swarm-validate'.
Both `bd' commands exit 0 on these domain states, so the payload is
authoritative and the exit code is not (REQ-SF-098).

Recognised domain errors:
- `{error: \"swarm already exists\", ...}' from `swarm create'
- `{error: \"epic is not swarmable\", ...}' from `swarm create'/
  `swarm validate'
- `{swarmable: false, errors: [...]}' from `swarm validate'

PARSED may be the raw JSON alist (`json-key-type' symbol) or a typed
swarm result object.  Returns the error message for a domain error
and nil otherwise.  This is the single place that turns a domain
payload into a user-facing error."
  (let* ((alist (and (listp parsed) (consp parsed) parsed))
         (obj (and (eieio-object-p parsed) parsed))
         (error-string
          (cond
           (alist (alist-get 'error parsed))
           ((and obj (slot-exists-p obj 'error) (slot-boundp obj 'error))
            (oref obj error))))
         (swarmable 'beads--absent))
    (cond
     (alist
      (when (assq 'swarmable parsed)
        (setq swarmable (alist-get 'swarmable parsed))))
     ((and obj (slot-exists-p obj 'swarmable)
           (slot-boundp obj 'swarmable))
      (setq swarmable (oref obj swarmable))))
    (cond
     ((and (stringp error-string)
           (string-match-p "already exists" error-string))
      error-string)
     ((and (stringp error-string)
           (string-match-p "not swarmable" error-string))
      error-string)
     ((and (not (eq swarmable 'beads--absent))
           (or (null swarmable) (eq swarmable :json-false)))
      "swarmable=false")
     (t nil))))

;;; Parent Transient Menu

;;;###autoload (autoload 'beads-swarm "beads-command-swarm" nil t)
(beads-define-prefix beads-swarm ()
  "Swarm management for structured epics.

A swarm is parallel work coordination on an epic's DAG."
  ["Swarm Commands"
   ("c" "Create swarm" beads-swarm-create)
   ("l" "List swarms" beads-swarm-list)
   ("s" "Show status" beads-swarm-status)
   ("v" "Validate epic" beads-swarm-validate)])

(provide 'beads-command-swarm)
;;; beads-command-swarm.el ends here
