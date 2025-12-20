;;; beads-agent-type.el --- Agent type system for AI agents -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues, ai

;;; Commentary:

;; This module provides an agent type layer for beads.el that defines
;; different kinds of AI work (Task, Review, Plan, QA, Custom).  Each
;; type has a name, single-letter abbreviation, description, and
;; customizable behavior via EIEIO.
;;
;; Agent types are orthogonal to backends - a session combines an agent
;; type with a backend.  For example, a Task agent can run on claude-code
;; or claude-code-ide backend.
;;
;; Architecture:
;;   Session = Agent Type (Task/Review/Plan) + Backend (claude-code/claude-code-ide)
;;
;; Built-in agent types:
;;   - Task (T): Autonomous task completion
;;   - Review (R): Code review with customizable prompt
;;   - Plan (P): Planning agent requiring backend plan mode
;;   - QA (Q): Testing/quality assurance agent
;;   - Custom (C): User-provided prompt at runtime
;;
;; This module provides:
;;   - Abstract base class `beads-agent-type'
;;   - Generic functions for prompt building and validation
;;   - Type registry for registration and lookup
;;
;; Concrete type implementations are defined separately (e.g., in
;; beads-agent-types.el) and register themselves at load time.

;;; Code:

(require 'eieio)
(require 'cl-lib)

;;; EIEIO Classes

(defclass beads-agent-type ()
  ((name
    :initarg :name
    :type string
    :documentation "Human-readable type name (e.g., \"Task\", \"Review\").")
   (letter
    :initarg :letter
    :type string
    :documentation "Single-letter abbreviation for display (e.g., \"T\", \"R\").
Used in list columns and keybindings.")
   (description
    :initarg :description
    :type string
    :initform ""
    :documentation "Brief description of what this agent type does.
Shown in completion annotations to help users choose types.")
   (prompt-template
    :initarg :prompt-template
    :initform nil
    :documentation "Template string for building prompts, or nil.
When non-nil, this is combined with issue context to build the agent prompt.
When nil, the type must override `beads-agent-type-build-prompt'."))
  :abstract t
  :documentation "Abstract base class for AI agent types.
Subclasses define specific agent behaviors and can override generic methods.
Types must be registered with `beads-agent-type-register' to be available.")

;;; Generic Functions

(cl-defgeneric beads-agent-type-build-prompt (type issue)
  "Build the prompt string for TYPE working on ISSUE.
TYPE is a `beads-agent-type' object.
ISSUE is a beads issue object (plist or alist with :id, :title, :description).

Returns a string to send to the agent, or nil if the type uses special
mechanisms (e.g., Plan type uses --plan flag instead of prompt).

Default implementation uses `prompt-template' slot combined with issue context.
Subclasses may override for custom prompt building.")

(cl-defmethod beads-agent-type-build-prompt ((type beads-agent-type) issue)
  "Build prompt for TYPE using the prompt-template slot combined with ISSUE."
  (let ((template (oref type prompt-template))
        (issue-id (plist-get issue :id))
        (issue-title (plist-get issue :title))
        (issue-desc (or (plist-get issue :description) "")))
    (if template
        ;; Combine template with issue context
        (format "%s\n\n## Issue: %s\n\n**Title:** %s\n\n**Description:**\n%s"
                template issue-id issue-title issue-desc)
      ;; No template - subclass must override or type uses non-prompt mechanism
      nil)))

(cl-defgeneric beads-agent-type-validate-backend (type backend)
  "Validate that BACKEND is compatible with TYPE.
TYPE is a `beads-agent-type' object.
BACKEND is a `beads-agent-backend' object.

Returns non-nil if valid, or signals an error with a descriptive message
if the backend is incompatible.

Default implementation always returns t (all types work with all backends).
Subclasses may override for custom validation.")

(cl-defmethod beads-agent-type-validate-backend ((_type beads-agent-type)
                                                  _backend)
  "Validate BACKEND is compatible with TYPE.  Always return t."
  t)

(cl-defgeneric beads-agent-type-letter-display (type)
  "Return the single-letter display string for TYPE.
TYPE is a `beads-agent-type' object.
Returns a string suitable for display in list columns.")

(cl-defmethod beads-agent-type-letter-display ((type beads-agent-type))
  "Return the single-letter display string for TYPE from the letter slot."
  (oref type letter))

(cl-defgeneric beads-agent-type-name-display (type)
  "Return the display name for TYPE.
TYPE is a `beads-agent-type' object.
Returns a string suitable for display in UI elements.")

(cl-defmethod beads-agent-type-name-display ((type beads-agent-type))
  "Return the display name for TYPE from the name slot."
  (oref type name))

;;; Type Registry

(defvar beads-agent-type--registry nil
  "Hash table mapping type names (lowercase strings) to type instances.
Use `beads-agent-type-register', `beads-agent-type-get', and
`beads-agent-type-list' to access.")

(defun beads-agent-type--ensure-registry ()
  "Ensure the type registry exists."
  (unless beads-agent-type--registry
    (setq beads-agent-type--registry (make-hash-table :test #'equal))))

;;;###autoload
(defun beads-agent-type-register (type)
  "Register TYPE for use with beads-agent.
TYPE must be an instance of a `beads-agent-type' subclass.
Replaces any existing type with the same name (case-insensitive)."
  (unless (object-of-class-p type 'beads-agent-type)
    (error "Type must be a beads-agent-type instance"))
  (beads-agent-type--ensure-registry)
  (let ((name (downcase (oref type name))))
    (puthash name type beads-agent-type--registry)))

;;;###autoload
(defun beads-agent-type-get (name)
  "Get agent type by NAME (case-insensitive).
Returns the `beads-agent-type' instance, or nil if not found."
  (beads-agent-type--ensure-registry)
  (gethash (downcase name) beads-agent-type--registry))

;;;###autoload
(defun beads-agent-type-list ()
  "Return list of all registered agent types.
Types are returned as `beads-agent-type' instances, sorted by name."
  (beads-agent-type--ensure-registry)
  (let ((types nil))
    (maphash (lambda (_k v) (push v types)) beads-agent-type--registry)
    (sort types (lambda (a b)
                  (string< (oref a name) (oref b name))))))

(defun beads-agent-type-names ()
  "Return list of all registered agent type names.
Names are returned as lowercase strings, sorted alphabetically."
  (mapcar (lambda (type) (downcase (oref type name)))
          (beads-agent-type-list)))

(defun beads-agent-type--clear-registry ()
  "Clear the type registry.
This function is intended for testing purposes only."
  (setq beads-agent-type--registry nil))

;;; Completion Support

(defun beads-agent-type-completion-table ()
  "Return a completion table for agent type names.
Includes annotations showing description for each type."
  (let ((types (beads-agent-type-list)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function
             . ,(lambda (candidate)
                  (when-let ((type (beads-agent-type-get candidate)))
                    (concat " - " (oref type description)))))
            (category . beads-agent-type))
        (complete-with-action
         action
         (mapcar (lambda (type) (oref type name)) types)
         string pred)))))

;;;###autoload
(defun beads-agent-type-read (&optional prompt)
  "Read an agent type name with completion.
PROMPT is the prompt string, defaulting to \"Agent type: \".
Returns the selected type name as a string."
  (completing-read (or prompt "Agent type: ")
                   (beads-agent-type-completion-table)
                   nil t))

(provide 'beads-agent-type)

;;; beads-agent-type.el ends here
