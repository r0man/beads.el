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
;; Customizing Agent Types:
;;
;; To add a new agent type, subclass `beads-agent-type' and register it:
;;
;;   (defclass my-debug-type (beads-agent-type)
;;     ((name :initform "Debug")
;;      (letter :initform "D")
;;      (description :initform "Debugging agent")
;;      (prompt-template :initform "You are a debugging agent...")))
;;
;;   (beads-agent-type-register (my-debug-type))
;;
;; To replace a built-in type (e.g., customize the Review prompt):
;;
;;   (defclass my-review-type (beads-agent-type)
;;     ((name :initform "Review")
;;      (letter :initform "R")
;;      (description :initform "My custom review agent")
;;      (prompt-template :initform "Review this code for security...")))
;;
;;   ;; Re-registering with the same name replaces the existing type
;;   (beads-agent-type-register (my-review-type))
;;
;; For custom prompt logic, override `beads-agent-type-build-prompt':
;;
;;   (cl-defmethod beads-agent-type-build-prompt ((type my-debug-type) issue)
;;     (format "Debug issue %s: %s" (oref issue id) (oref issue title)))
;;
;; Letter uniqueness is enforced - each type must use a unique letter.
;; Re-registering a type with the same name frees its old letter.
;;
;; Concrete type implementations are defined in beads-agent-types.el
;; and register themselves at load time.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'beads-types)

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
ISSUE is a `beads-issue' EIEIO object with slots id, title, description, etc.

Returns a string to send to the agent, or nil if the type uses special
mechanisms (e.g., Plan type uses --plan flag instead of prompt).

Default implementation uses `prompt-template' slot combined with issue context.
Subclasses may override for custom prompt building.")

(cl-defmethod beads-agent-type-build-prompt ((type beads-agent-type) issue)
  "Build prompt for TYPE using the prompt-template slot combined with ISSUE.
ISSUE is a beads-issue EIEIO object."
  (let ((template (oref type prompt-template))
        (issue-id (oref issue id))
        (issue-title (oref issue title))
        (issue-desc (or (oref issue description) "")))
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

(cl-defgeneric beads-agent-type-preferred-backend (type)
  "Return the preferred backend name for TYPE, or nil to use default.
TYPE is a `beads-agent-type' object.

Returns a string naming the preferred backend for this agent type,
or nil to use the global default backend.

The backend selection order is:
1. Type-specific backend (returned by this method)
2. Global default (`beads-agent-default-backend')
3. First available backend

Default implementation returns nil (use global default).
Subclasses may override to return a type-specific preference.")

(cl-defmethod beads-agent-type-preferred-backend ((_type beads-agent-type))
  "Return preferred backend for TYPE, or nil by default."
  nil)

;;; Type Registry

(defvar beads-agent-type--registry nil
  "Hash table mapping type names (lowercase strings) to type instances.
Use `beads-agent-type-register', `beads-agent-type-get', and
`beads-agent-type-list' to access.")

(defvar beads-agent-type--letter-registry nil
  "Hash table mapping letters (uppercase strings) to type names.
Used to validate letter uniqueness during registration.")

(defun beads-agent-type--ensure-registry ()
  "Ensure the type registries exist."
  (unless beads-agent-type--registry
    (setq beads-agent-type--registry (make-hash-table :test #'equal)))
  (unless beads-agent-type--letter-registry
    (setq beads-agent-type--letter-registry (make-hash-table :test #'equal))))

(defun beads-agent-type--validate-letter (letter type-name)
  "Validate that LETTER is unique and well-formed for TYPE-NAME.
LETTER must be a single-character string.
Signals an error if the letter is already used by another type.
Returns t if validation passes."
  (unless (and (stringp letter) (= (length letter) 1))
    (error "Letter must be a single-character string, got: %S" letter))
  (beads-agent-type--ensure-registry)
  (let* ((upper-letter (upcase letter))
         (existing-name (gethash upper-letter beads-agent-type--letter-registry)))
    (when (and existing-name
               (not (string= (downcase existing-name) (downcase type-name))))
      (error "Letter %S is already used by type %S" upper-letter existing-name)))
  t)

;;;###autoload
(defun beads-agent-type-register (type)
  "Register TYPE for use with beads-agent.
TYPE must be an instance of a `beads-agent-type' subclass.
Validates that the type's letter is unique across all registered types.
Replaces any existing type with the same name (case-insensitive).
Returns TYPE for convenient chaining."
  (unless (object-of-class-p type 'beads-agent-type)
    (error "Type must be a beads-agent-type instance"))
  (beads-agent-type--ensure-registry)
  (let* ((name (oref type name))
         (letter (oref type letter))
         (lower-name (downcase name))
         (upper-letter (upcase letter)))
    ;; Validate letter uniqueness
    (beads-agent-type--validate-letter letter name)
    ;; If replacing existing type, unregister its letter first
    (when-let ((existing (gethash lower-name beads-agent-type--registry)))
      (remhash (upcase (oref existing letter)) beads-agent-type--letter-registry))
    ;; Register the type and letter
    (puthash lower-name type beads-agent-type--registry)
    (puthash upper-letter lower-name beads-agent-type--letter-registry)
    type))

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
  "Clear the type registries.
This function is intended for testing purposes only."
  (setq beads-agent-type--registry nil)
  (setq beads-agent-type--letter-registry nil))

(defun beads-agent-type--unregister (name)
  "Unregister the type with NAME from all registries.
NAME is case-insensitive.  Does nothing if type is not registered."
  (beads-agent-type--ensure-registry)
  (let ((lower-name (downcase name)))
    (when-let ((type (gethash lower-name beads-agent-type--registry)))
      (remhash (upcase (oref type letter)) beads-agent-type--letter-registry)
      (remhash lower-name beads-agent-type--registry))))

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

;;; Lookup by Letter

;;;###autoload
(defun beads-agent-type-get-by-letter (letter)
  "Get agent type by LETTER (case-insensitive).
Returns the `beads-agent-type' instance, or nil if not found."
  (beads-agent-type--ensure-registry)
  (when-let ((name (gethash (upcase letter) beads-agent-type--letter-registry)))
    (beads-agent-type-get name)))

;;;###autoload
(defun beads-agent-type-letter-used-p (letter)
  "Return non-nil if LETTER is used by any registered type.
LETTER is case-insensitive."
  (beads-agent-type--ensure-registry)
  (gethash (upcase letter) beads-agent-type--letter-registry))

(provide 'beads-agent-type)

;;; beads-agent-type.el ends here
