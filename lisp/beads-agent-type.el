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
;; For custom prompt logic, override `beads-agent-type-build-user-prompt':
;;
;;   (cl-defmethod beads-agent-type-build-user-prompt ((type my-debug-type) issue)
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

;;; Customization

(defcustom beads-agent-display-use-icons 'auto
  "Whether to use emoji icons for agent type indicators.

Controls what `beads-agent-type-icon-or-letter' returns: an emoji
icon (from the `icon' slot or a `beads-agent-display-type-icons' override)
or the single-letter abbreviation from the `letter' slot.

Possible values:
- `auto' (default): icons under GUI Emacs, letters in TTY frames
- t: always use icons regardless of frame type
- nil: always use single letters (T/R/P/Q/C)

Set this to nil when icons render as tofu (missing-glyph) on your
system, or override icons per-type via `beads-agent-display-type-icons'."
  :type '(choice (const :tag "Auto-detect by frame" auto)
                 (const :tag "Always icons" t)
                 (const :tag "Always letters" nil))
  :group 'beads-agent)

(defcustom beads-agent-display-type-icons nil
  "Per-type icon overrides for agent display.

Alist mapping lowercase type names to display strings.  Each entry
takes precedence over the `icon' slot of the corresponding
`beads-agent-type'.  When an entry is present with a nil value the
type's icon resolves to nil (and renderers fall back to the letter)
without consulting the slot.  Set the whole variable to nil to
disable all overrides and use only class slots.

Example:
  ((\"task\" . \"\\=🛠\") (\"review\" . \"\\=🔍\"))"
  :type '(alist :key-type (string :tag "Type name (lowercase)")
                :value-type (string :tag "Icon"))
  :group 'beads-agent)

(defcustom beads-agent-display-show-instance nil
  "When non-nil, append #N to icons in narrow display contexts.

By default the instance number is shown only in mode-line, the
agent list buffer, and help-echo tooltips, freeing space in the
issue list column and dashboard badges.  Set this to t to also
show #N in those tight surfaces."
  :type 'boolean
  :group 'beads-agent)

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
    :documentation "Template string for the user prompt, or nil.
When non-nil, this is combined with issue context to build the agent
user prompt.  When nil, the type must override
`beads-agent-type-build-user-prompt'.")
   (system-prompt
    :initarg :system-prompt
    :initform nil
    :documentation "Role/identity template for the system prompt, or nil.
A string used directly, or a symbol dereferenced with `symbol-value'
\(like `prompt-template').  Runs the same `<ISSUE-...>' substitution
via `beads-agent-type-system-prompt'.  nil means this type has no
distinct system prompt (builder types such as Custom, and the
default in Phase 1a-i where all slots are still nil).")
   (icon
    :initarg :icon
    :initform nil
    :documentation "Display icon string for this agent type, or nil.
A short string (typically a single emoji, two display columns wide) used
as the visual identifier across all UIs.  When nil, the letter slot is
used as fallback.  Users may override via `beads-agent-display-type-icons'."))
  :abstract t
  :documentation "Abstract base class for AI agent types.
Subclasses define specific agent behaviors and can override generic methods.
Types must be registered with `beads-agent-type-register' to be available.")

;;; Generic Functions

(cl-defgeneric beads-agent-type-build-user-prompt (type issue)
  "Build the user prompt string for TYPE working on ISSUE.
TYPE is a `beads-agent-type' object.
ISSUE is a `beads-issue' EIEIO object with slots id, title, description, etc.

Returns a string to send to the agent, or nil if the type uses special
mechanisms (e.g., Plan type uses --plan flag instead of prompt).

Default implementation uses `prompt-template' slot combined with issue context.
Subclasses may override for custom prompt building.")

(cl-defmethod beads-agent-type-build-user-prompt ((type beads-agent-type) issue)
  "Build user prompt for TYPE from the prompt-template slot and ISSUE.
ISSUE is a beads-issue EIEIO object.

The template can contain placeholders that are replaced with issue data:
  <ISSUE-ID>          - The issue ID (e.g., \"beads.el-123\")
  <ISSUE-TITLE>       - The issue title
  <ISSUE-DESCRIPTION> - The issue description (empty string if nil)

The prompt-template slot can be:
  - A string: used directly as the template
  - A symbol: dereferenced with `symbol-value' to get the template string
  - nil: returns nil (subclass must override or type uses non-prompt mechanism)"
  (let ((template-or-sym (oref type prompt-template)))
    (when template-or-sym
      (let ((template (if (symbolp template-or-sym)
                          (symbol-value template-or-sym)
                        template-or-sym))
            (issue-id (or (oref issue id) ""))
            (issue-title (or (oref issue title) ""))
            (issue-desc (or (oref issue description) "")))
        ;; Replace placeholders in template
        (thread-last template
          (string-replace "<ISSUE-ID>" issue-id)
          (string-replace "<ISSUE-TITLE>" issue-title)
          (string-replace "<ISSUE-DESCRIPTION>" issue-desc))))))

(cl-defgeneric beads-agent-type-system-prompt (type issue)
  "Build the system (role/identity) prompt for TYPE working on ISSUE.
TYPE is a `beads-agent-type' object.
ISSUE is a `beads-issue' EIEIO object, or nil.

Returns the role string with `<ISSUE-...>' placeholders substituted,
or nil when the type has no distinct system prompt (builder types
such as Custom, and every type in Phase 1a-i where the slot is still
nil).  Delivered via the backend's system-prompt channel.")

(cl-defmethod beads-agent-type-system-prompt ((type beads-agent-type) issue)
  "Build system prompt for TYPE from the `system-prompt' slot and ISSUE.
The slot may be a string (used directly), a symbol (dereferenced
with `symbol-value'), or nil (returns nil).  When ISSUE is non-nil,
the same `<ISSUE-...>' substitution as the user prompt is applied so
a role template may reference the issue.  Returns nil when the slot
is nil, so builder types and the Phase 1a-i frozen defaults yield
nil for every type."
  (let ((tmpl-or-sym (oref type system-prompt)))
    (when tmpl-or-sym
      (let ((template (if (symbolp tmpl-or-sym)
                          (symbol-value tmpl-or-sym)
                        tmpl-or-sym))
            (issue-id (or (and issue (oref issue id)) ""))
            (issue-title (or (and issue (oref issue title)) ""))
            (issue-desc (or (and issue (oref issue description)) "")))
        (thread-last template
          (string-replace "<ISSUE-ID>" issue-id)
          (string-replace "<ISSUE-TITLE>" issue-title)
          (string-replace "<ISSUE-DESCRIPTION>" issue-desc))))))

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

(cl-defgeneric beads-agent-type-icon (type)
  "Return the icon string for TYPE, or nil if no icon is configured.

Resolution order:
1. `beads-agent-display-type-icons' override (alist lookup by lowercase
   name).  A cons cell present in the alist wins even when its
   cdr is nil, so a user can explicitly clear the icon for a
   type without subclassing.
2. `icon' slot of TYPE.
3. nil.

Does NOT apply the terminal-supported-p gate; callers wanting the
user-visible identifier should use `beads-agent-type-icon-or-letter'.")

(cl-defmethod beads-agent-type-icon ((type beads-agent-type))
  "Return the configured icon string for TYPE, or nil.
Follows the resolution order documented on the generic."
  (let ((entry (assoc (downcase (oref type name)) beads-agent-display-type-icons)))
    (if entry
        (cdr entry)
      (and (slot-boundp type 'icon) (oref type icon)))))

(defun beads-agent--icons-supported-p ()
  "Return non-nil when emoji icons should be rendered.
Resolves `beads-agent-display-use-icons':
- `auto' (default): non-nil in GUI frames, nil in TTY
- t: always non-nil
- nil: always nil"
  (pcase beads-agent-display-use-icons
    ('auto (display-graphic-p))
    (val val)))

(defun beads-agent-type-icon-or-letter (type)
  "Return the user-visible identifier string for TYPE.

Returns the configured icon when icons are enabled by
`beads-agent-display-use-icons', supported by the frame, AND a
non-nil icon is configured for TYPE (via
`beads-agent-display-type-icons' override or the `icon' slot).  Otherwise
returns the single-letter abbreviation from TYPE's `letter' slot."
  (or (and (beads-agent--icons-supported-p)
           (beads-agent-type-icon type))
      (oref type letter)))

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
