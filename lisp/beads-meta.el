;;; beads-meta.el --- EIEIO slot property infrastructure for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module provides infrastructure for adding custom slot properties
;; to EIEIO classes, inspired by the closql approach.  By adding metadata
;; directly to slot definitions, we can:
;;
;; - Auto-generate CLI arguments from slot metadata
;; - Auto-generate transient infixes from slot metadata
;; - Auto-generate parse functions from slot metadata
;; - Ensure consistency between CLI and transient interfaces
;;
;; The implementation uses `define-advice' on EIEIO's internal functions
;; to preserve custom slot properties that would otherwise be discarded.
;;
;; Supported Custom Properties:
;;
;; CLI Properties:
;;   :long-option     - Long CLI option (e.g., "--title")
;;   :short-option    - Short CLI option (e.g., "-t")
;;   :option-type     - Serialization type (:string, :boolean, :integer, :list)
;;   :positional      - Position for positional args (integer 1, 2, 3... or nil)
;;   :option-separator - Separator for :list type (default ",")
;;
;; Transient Properties:
;;   :transient-key         - Key binding in transient menu
;;   :transient-description - Description in transient
;;   :transient-class       - Transient class (transient-option, etc.)
;;   :transient-reader      - Reader function for input
;;   :transient-choices     - Valid choices list
;;   :transient-prompt      - Input prompt string
;;   :transient-level       - Menu visibility level (1-7)
;;   :transient-group       - Group name for organization
;;   :transient-order       - Order within group (lower = first)
;;
;; Validation Properties:
;;   :required  - Is field required?
;;   :validator - Validation function
;;
;; Usage:
;;
;;   (defclass my-command (beads-command)
;;     ((title
;;       :initarg :title
;;       :type (or null string)
;;       :initform nil
;;       :documentation "Issue title"
;;       ;; CLI properties
;;       :positional 1
;;       ;; Transient properties
;;       :transient-key "t"
;;       :transient-description "Title (required)"
;;       :transient-class transient-option
;;       :transient-reader beads-reader-issue-title
;;       :transient-group "Required"
;;       :transient-level 1
;;       ;; Validation
;;       :required t)))
;;
;;   ;; Get a property for a slot
;;   (beads-meta-slot-property 'my-command 'title :transient-key)
;;   ;; => "t"
;;
;;   ;; Get all custom properties for a slot
;;   (beads-meta-slot-properties 'my-command 'title)
;;   ;; => ((:positional . 1) (:transient-key . "t") ...)
;;
;;   ;; Find all slots with a property
;;   (beads-meta-slots-with-property 'my-command :transient-key)
;;   ;; => (title ...)

;;; Code:

(require 'eieio)
(require 'eieio-core)
(require 'cl-lib)

;; Forward declarations for generic methods from beads-command.el
;; These are used in the generated suffix code
(declare-function beads-command-validate "beads-command")
(declare-function beads-command-execute-interactive "beads-command")
(declare-function beads-command-preview "beads-command")

;;; ============================================================
;;; Custom Slot Properties List
;;; ============================================================

(defconst beads-meta--slot-properties
  '(;; CLI properties
    :long-option
    :short-option
    :option-type
    :positional
    :option-separator
    ;; Transient properties
    :transient-key
    :transient-description
    :transient-class
    :transient-reader
    :transient-choices
    :transient-prompt
    :transient-argument
    :transient-field-name
    :transient-level
    :transient-group
    :transient-order
    ;; Validation properties
    :required
    :validator)
  "List of custom slot properties supported by beads-meta.
These properties are preserved in slot descriptors via advice on
`eieio-defclass-internal' and `eieio--slot-override'.")

;;; ============================================================
;;; EIEIO Advice for Custom Property Preservation
;;; ============================================================

(define-advice eieio-defclass-internal
    (:after (cname _superclasses slots _options) beads-meta)
  "Preserve custom slot properties for beads command classes.
This advice intercepts class definition and stores custom properties
in the slot descriptor's property alist.

CNAME is the class name being defined.
SLOTS is the list of slot specifications."
  (dolist (slot slots)
    (when (consp slot)
      (let* ((slot-name (car slot))
             (slot-options (cdr slot))
             (custom-props nil))
        ;; Extract custom properties from slot options
        (dolist (prop beads-meta--slot-properties)
          (let ((value (plist-get slot-options prop)))
            (when value
              (push (cons prop value) custom-props))))
        ;; Store custom properties in slot descriptor if any found
        (when custom-props
          (let* ((class (cl--find-class cname))
                 (slots-vec (and class (eieio--class-slots class))))
            ;; Find slot descriptor by name and update its properties
            (when slots-vec
              (cl-loop for slot-desc across slots-vec
                       when (eq (cl--slot-descriptor-name slot-desc) slot-name)
                       do (setf (cl--slot-descriptor-props slot-desc)
                                (append custom-props
                                        (cl--slot-descriptor-props slot-desc)))
                       and return nil))))))))

(define-advice eieio--slot-override
    (:after (old new _skipnil) beads-meta)
  "Preserve custom slot properties during inheritance.
When a child class overrides a parent slot, copy custom properties
from the parent slot descriptor to the child.

OLD is the parent slot descriptor.
NEW is the child slot descriptor being created."
  (let ((old-props (cl--slot-descriptor-props old))
        (new-props (cl--slot-descriptor-props new)))
    ;; Copy custom properties from old to new if not already present
    (dolist (prop beads-meta--slot-properties)
      (let ((old-value (alist-get prop old-props)))
        (when (and old-value (not (alist-get prop new-props)))
          (push (cons prop old-value) new-props))))
    ;; Update new slot's properties
    (when (cl-set-difference new-props (cl--slot-descriptor-props new))
      (setf (cl--slot-descriptor-props new) new-props))))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun beads-meta-slot-property (class slot-name property)
  "Get PROPERTY for SLOT-NAME in CLASS.
CLASS is a class name symbol.
SLOT-NAME is the slot name symbol.
PROPERTY is a keyword like :transient-key, :long-option, etc.

Returns the property value, or nil if not found."
  (let* ((class-obj (cl--find-class class))
         (slots-vec (and class-obj (eieio--class-slots class-obj))))
    (when slots-vec
      (cl-loop for slot-desc across slots-vec
               when (eq (cl--slot-descriptor-name slot-desc) slot-name)
               return (alist-get property
                                 (cl--slot-descriptor-props slot-desc))))))

(defun beads-meta-slot-properties (class slot-name)
  "Get all custom properties for SLOT-NAME in CLASS.
CLASS is a class name symbol.
SLOT-NAME is the slot name symbol.

Returns an alist of (PROPERTY . VALUE) pairs for custom properties
defined by `beads-meta--slot-properties'."
  (let* ((class-obj (cl--find-class class))
         (slots-vec (and class-obj (eieio--class-slots class-obj))))
    (when slots-vec
      (cl-loop for slot-desc across slots-vec
               when (eq (cl--slot-descriptor-name slot-desc) slot-name)
               return (cl-remove-if-not
                       (lambda (pair)
                         (memq (car pair) beads-meta--slot-properties))
                       (cl--slot-descriptor-props slot-desc))))))

(defun beads-meta-slots-with-property (class property)
  "Find all slots in CLASS that have PROPERTY defined.
CLASS is a class name symbol.
PROPERTY is a keyword like :transient-key, :long-option, etc.

Returns a list of (SLOT-NAME . VALUE) pairs for slots that have
the property defined."
  (let* ((class-obj (cl--find-class class))
         (slots-vec (and class-obj (eieio--class-slots class-obj)))
         result)
    (when slots-vec
      (cl-loop for slot-desc across slots-vec
               do (let ((value (alist-get property
                                          (cl--slot-descriptor-props slot-desc))))
                    (when value
                      (push (cons (cl--slot-descriptor-name slot-desc) value)
                            result))))
      (nreverse result))))

(defun beads-meta-command-slots (class)
  "Get all slots for command CLASS including inherited slots.
CLASS is a class name symbol.

Returns a list of slot name symbols."
  (let* ((class-obj (cl--find-class class))
         (slots-vec (and class-obj (eieio--class-slots class-obj))))
    (when slots-vec
      (cl-loop for slot-desc across slots-vec
               collect (cl--slot-descriptor-name slot-desc)))))

(defun beads-meta--slot-initarg (class-obj slot-name)
  "Get the initarg keyword for SLOT-NAME in CLASS-OBJ.
CLASS-OBJ is an EIEIO class object.
SLOT-NAME is the slot name symbol.

Returns the initarg keyword (e.g., :title) or nil if not found."
  (let ((tuples (eieio--class-initarg-tuples class-obj)))
    (car (cl-rassoc slot-name tuples))))

(defun beads-meta-slot-info (class slot-name)
  "Get complete slot information for SLOT-NAME in CLASS.
CLASS is a class name symbol.
SLOT-NAME is the slot name symbol.

Returns a plist with:
  :name        - Slot name
  :initarg     - Init argument keyword
  :type        - Slot type
  :initform    - Default value
  :custom-props - Alist of custom properties

Returns nil if slot not found."
  (let* ((class-obj (cl--find-class class))
         (slots-vec (and class-obj (eieio--class-slots class-obj))))
    (when slots-vec
      (cl-loop for slot-desc across slots-vec
               when (eq (cl--slot-descriptor-name slot-desc) slot-name)
               return (list
                       :name (cl--slot-descriptor-name slot-desc)
                       :initarg (beads-meta--slot-initarg class-obj slot-name)
                       :type (cl--slot-descriptor-type slot-desc)
                       :initform (cl--slot-descriptor-initform slot-desc)
                       :custom-props
                       (cl-remove-if-not
                        (lambda (pair)
                          (memq (car pair) beads-meta--slot-properties))
                        (cl--slot-descriptor-props slot-desc)))))))

(defun beads-meta-positional-slots (class)
  "Get all positional slots for CLASS, sorted by position.
CLASS is a class name symbol.

Returns a list of (SLOT-NAME . POSITION) pairs, sorted by position
in ascending order.  Only slots with :positional property are included."
  (let ((slots (beads-meta-slots-with-property class :positional)))
    (sort slots (lambda (a b) (< (cdr a) (cdr b))))))

(defun beads-meta-option-slots (class)
  "Get all named option slots for CLASS (non-positional).
CLASS is a class name symbol.

Returns a list of slot names that have :long-option or :short-option
but not :positional."
  (let* ((class-obj (cl--find-class class))
         (slots-vec (and class-obj (eieio--class-slots class-obj)))
         result)
    (when slots-vec
      (cl-loop for slot-desc across slots-vec
               do (let* ((props (cl--slot-descriptor-props slot-desc))
                         (has-option (or (alist-get :long-option props)
                                         (alist-get :short-option props)))
                         (is-positional (alist-get :positional props)))
                    (when (and has-option (not is-positional))
                      (push (cl--slot-descriptor-name slot-desc) result))))
      (nreverse result))))

(defun beads-meta-transient-slots (class)
  "Get all slots for CLASS that have transient properties.
CLASS is a class name symbol.

Returns a list of slot names that have :transient-key defined,
sorted by :transient-group and :transient-order."
  (let ((slots-with-key (beads-meta-slots-with-property class :transient-key)))
    (mapcar #'car
            (sort slots-with-key
                  (lambda (a b)
                    (let* ((a-name (car a))
                           (b-name (car b))
                           (a-group (or (beads-meta-slot-property class a-name
                                                                  :transient-group)
                                        ""))
                           (b-group (or (beads-meta-slot-property class b-name
                                                                  :transient-group)
                                        ""))
                           (a-order (or (beads-meta-slot-property class a-name
                                                                  :transient-order)
                                        999))
                           (b-order (or (beads-meta-slot-property class b-name
                                                                  :transient-order)
                                        999)))
                      (if (string= a-group b-group)
                          (< a-order b-order)
                        (string< a-group b-group))))))))

;;; ============================================================
;;; Command-Line Building from Slot Metadata
;;; ============================================================

(defun beads-meta--format-slot-value (value option-type separator)
  "Format VALUE for command line based on OPTION-TYPE.
OPTION-TYPE is one of :string, :boolean, :integer, :list.
SEPARATOR is used for :list type (default \",\").

Returns a string or nil if value should not be included."
  (pcase option-type
    (:boolean (if value t nil))  ; Return t for truthy, nil for falsy
    (:integer (when value (number-to-string value)))
    (:list (when (and value (listp value) (not (null value)))
             (mapconcat (lambda (v) (if (stringp v) v (format "%s" v)))
                        value
                        (or separator ","))))
    (_ (when (and value (stringp value) (not (string-empty-p value)))
         value))))

(defun beads-meta-build-command-line (command)
  "Build command-line arguments from slot metadata for COMMAND.
COMMAND is an EIEIO object instance.

Returns a list of strings representing CLI arguments.  Positional
arguments come first (sorted by :positional value), followed by
named options (--option value or --flag).

Slots without :long-option, :short-option, or :positional are skipped."
  (let* ((class-name (eieio-object-class command))
         (class-obj (cl--find-class class-name))
         (slots-vec (eieio--class-slots class-obj))
         (positional-args nil)
         (named-args nil))
    ;; Process each slot
    (cl-loop for slot-desc across slots-vec
             do (let* ((slot-name (cl--slot-descriptor-name slot-desc))
                       (props (cl--slot-descriptor-props slot-desc))
                       (long-opt (alist-get :long-option props))
                       (short-opt (alist-get :short-option props))
                       (option-type (or (alist-get :option-type props) :string))
                       (positional (alist-get :positional props))
                       (separator (alist-get :option-separator props))
                       (value (when (slot-boundp command slot-name)
                                (eieio-oref command slot-name))))
                  (cond
                   ;; Positional argument
                   (positional
                    (let ((formatted (beads-meta--format-slot-value
                                      value option-type separator)))
                      (when formatted
                        (push (cons positional
                                    (if (eq option-type :list)
                                        ;; Lists become multiple positional args
                                        (split-string formatted
                                                      (or separator ","))
                                      (list formatted)))
                              positional-args))))
                   ;; Named option with value
                   ((or long-opt short-opt)
                    (let* ((opt-name (or long-opt short-opt))
                           (formatted (beads-meta--format-slot-value
                                       value option-type separator)))
                      (cond
                       ;; Boolean flag
                       ((eq option-type :boolean)
                        (when formatted
                          (push (list opt-name) named-args)))
                       ;; Option with value
                       (formatted
                        (push (list opt-name formatted) named-args))))))))
    ;; Build final argument list
    (let ((result nil))
      ;; Sort positional args by position and add them
      (dolist (pos-pair (sort positional-args (lambda (a b) (< (car a) (car b)))))
        (setq result (append result (cdr pos-pair))))
      ;; Add named options (reverse to maintain declaration order)
      (dolist (opt (nreverse named-args))
        (setq result (append result opt)))
      result)))

(defun beads-meta-build-full-command-line (command subcommand)
  "Build complete command line for COMMAND with SUBCOMMAND.
COMMAND is an EIEIO object instance.
SUBCOMMAND is a string like \"create\", \"update\", etc.

Returns a list of strings: (SUBCOMMAND ...positional... ...options...).
This does NOT include global flags or executable - those are handled
by the base class's command-line method."
  (cons subcommand (beads-meta-build-command-line command)))

;;; ============================================================
;;; Transient Infix Generation from Slot Metadata
;;; ============================================================

(defun beads-meta-generate-infix-spec (class slot-name prefix)
  "Generate a transient infix specification for SLOT-NAME in CLASS.
PREFIX is a string like \"beads-create\" for naming the infix.

Returns a plist suitable for passing to `transient-define-infix',
or nil if the slot doesn't have transient metadata (no :transient-key)."
  (let* ((key (beads-meta-slot-property class slot-name :transient-key))
         (desc (beads-meta-slot-property class slot-name :transient-description))
         (trans-class (beads-meta-slot-property class slot-name :transient-class))
         (reader (beads-meta-slot-property class slot-name :transient-reader))
         (choices (beads-meta-slot-property class slot-name :transient-choices))
         (prompt (beads-meta-slot-property class slot-name :transient-prompt))
         (long-opt (beads-meta-slot-property class slot-name :long-option))
         (option-type (beads-meta-slot-property class slot-name :option-type)))
    (when key
      (let ((spec (list :name (intern (format "%s-infix-%s" prefix slot-name))
                        :key key)))
        ;; Description
        (when desc
          (setq spec (plist-put spec :description desc)))
        ;; Transient class (default based on option-type)
        (setq spec (plist-put spec :class
                              (or trans-class
                                  (if (eq option-type :boolean)
                                      'transient-switch
                                    'transient-option))))
        ;; Argument (from :long-option)
        (when long-opt
          (let ((arg (if (eq option-type :boolean)
                         long-opt
                       (concat long-opt "="))))
            (setq spec (plist-put spec :argument arg))))
        ;; Reader function
        (when reader
          (setq spec (plist-put spec :reader reader)))
        ;; Choices
        (when choices
          (setq spec (plist-put spec :choices choices)))
        ;; Prompt
        (when prompt
          (setq spec (plist-put spec :prompt prompt)))
        spec))))

(defun beads-meta-generate-infix-specs (class prefix)
  "Generate all infix specifications for CLASS.
PREFIX is a string like \"beads-create\" for naming infixes.

Returns a list of infix plists, sorted by :transient-group and
:transient-order."
  (let ((slots (beads-meta-transient-slots class))
        (specs nil))
    (dolist (slot-name slots)
      (let ((spec (beads-meta-generate-infix-spec class slot-name prefix)))
        (when spec
          ;; Add group/level/order info to spec for sorting
          (let ((group (beads-meta-slot-property class slot-name :transient-group))
                (level (beads-meta-slot-property class slot-name :transient-level))
                (order (beads-meta-slot-property class slot-name :transient-order)))
            (when group (setq spec (plist-put spec :group group)))
            (when level (setq spec (plist-put spec :level level)))
            (when order (setq spec (plist-put spec :order order))))
          (push spec specs))))
    (nreverse specs)))

(defun beads-meta-group-infix-specs (specs)
  "Group SPECS by their :group property.
Returns an alist of (GROUP-NAME . SPECS-IN-GROUP), sorted by minimum
:level in each group."
  (let ((groups (make-hash-table :test 'equal)))
    ;; Group specs by :group
    (dolist (spec specs)
      (let ((group (or (plist-get spec :group) "Options")))
        (puthash group (cons spec (gethash group groups)) groups)))
    ;; Convert to sorted alist
    (let ((group-list nil))
      (maphash (lambda (name group-specs)
                 (let ((min-level (apply #'min
                                         (mapcar (lambda (s)
                                                   (or (plist-get s :level) 5))
                                                 group-specs))))
                   (push (cons name (cons min-level (nreverse group-specs)))
                         group-list)))
               groups)
      ;; Sort by level
      (mapcar (lambda (g) (cons (car g) (cddr g)))
              (sort group-list (lambda (a b)
                                 (< (cadr a) (cadr b))))))))

(defmacro beads-meta-define-infix (spec)
  "Define a transient infix from SPEC plist.
SPEC should contain :name, :key, and optionally :class, :argument,
:description, :reader, :choices, :prompt."
  (let ((name (plist-get spec :name))
        (key (plist-get spec :key))
        (class (or (plist-get spec :class) 'transient-option))
        (arg (plist-get spec :argument))
        (desc (plist-get spec :description))
        (reader (plist-get spec :reader))
        (choices (plist-get spec :choices))
        (prompt (plist-get spec :prompt)))
    `(transient-define-infix ,name ()
       ,@(when desc (list desc))
       :class ',class
       :key ,key
       ,@(when arg (list :argument arg))
       ,@(when desc (list :description desc))
       ,@(when reader (list :reader `#',reader))
       ,@(when choices (list :choices `',choices))
       ,@(when prompt (list :prompt prompt)))))

(defmacro beads-meta-define-infixes (class prefix)
  "Define all transient infixes for CLASS using PREFIX.
This macro generates transient-define-infix forms for all slots
in CLASS that have :transient-key defined.

Example:
  (beads-meta-define-infixes beads-command-create \"beads-create\")

Expands to multiple transient-define-infix forms."
  (let ((specs (beads-meta-generate-infix-specs (eval class) (eval prefix))))
    `(progn
       ,@(mapcar (lambda (spec)
                   `(beads-meta-define-infix ,spec))
                 specs))))

;;; ============================================================
;;; Transient Group and Prefix Generation
;;; ============================================================

(defun beads-meta--normalize-group-name (group-name)
  "Normalize GROUP-NAME to a valid Lisp symbol suffix.
Converts spaces to dashes and lowercases the name."
  (downcase (replace-regexp-in-string "[ \t]+" "-" group-name)))

(defun beads-meta-generate-group-spec (group-name specs prefix)
  "Generate a transient group specification.
GROUP-NAME is the group display name (e.g., \"Issue attributes\").
SPECS is a list of infix specs belonging to this group.
PREFIX is a string like \"beads-create\".

Returns a plist with :name, :level, :description, and :infixes."
  (let* ((normalized (beads-meta--normalize-group-name group-name))
         (group-sym (intern (format "%s--%s-section" prefix normalized)))
         (min-level (apply #'min (mapcar (lambda (s)
                                           (or (plist-get s :level) 5))
                                         specs)))
         ;; Sort specs within group by :order
         (sorted-specs (sort (copy-sequence specs)
                             (lambda (a b)
                               (< (or (plist-get a :order) 999)
                                  (or (plist-get b :order) 999))))))
    (list :name group-sym
          :level min-level
          :description group-name
          :infixes (mapcar (lambda (s) (plist-get s :name)) sorted-specs))))

(defun beads-meta-generate-group-specs (class prefix)
  "Generate all group specifications for CLASS.
PREFIX is a string like \"beads-create\".

Returns a list of group specs, sorted by :level."
  (let* ((infix-specs (beads-meta-generate-infix-specs class prefix))
         (grouped (beads-meta-group-infix-specs infix-specs)))
    (mapcar (lambda (group-pair)
              (beads-meta-generate-group-spec
               (car group-pair) (cdr group-pair) prefix))
            grouped)))

(defmacro beads-meta-define-group (spec)
  "Define a transient group from SPEC plist.
SPEC should contain :name, :level, :description, and :infixes."
  (let ((name (plist-get spec :name))
        (level (plist-get spec :level))
        (desc (plist-get spec :description))
        (infixes (plist-get spec :infixes)))
    `(transient-define-group ,name
       [:level ,level ,desc
               ,@(mapcar (lambda (infix) `(,infix)) infixes)])))

(defmacro beads-meta-define-groups (class prefix)
  "Define all transient groups for CLASS using PREFIX.
This macro generates transient-define-group forms for each
distinct :transient-group value in CLASS slots.

Example:
  (beads-meta-define-groups beads-command-create \"beads-create\")

Expands to multiple transient-define-group forms."
  (let ((specs (beads-meta-generate-group-specs (eval class) (eval prefix))))
    `(progn
       ,@(mapcar (lambda (spec)
                   `(beads-meta-define-group ,spec))
                 specs))))

(defun beads-meta-generate-prefix-spec (class prefix &optional docstring)
  "Generate a transient prefix specification for CLASS.
PREFIX is a string like \"beads-create\".
DOCSTRING is an optional documentation string for the prefix.

Returns a plist with :name, :docstring, :groups, and group symbols."
  (let* ((group-specs (beads-meta-generate-group-specs class prefix))
         (prefix-sym (intern prefix)))
    (list :name prefix-sym
          :docstring (or docstring (format "Transient menu for %s." prefix))
          :groups (mapcar (lambda (gs) (plist-get gs :name)) group-specs))))

(defmacro beads-meta-define-prefix (class prefix &optional docstring
                                          actions global-section)
  "Define a complete transient prefix for CLASS.
PREFIX is a string like \"beads-create\".
DOCSTRING is an optional documentation string.
ACTIONS is an optional list of (KEY DESCRIPTION FUNCTION) for
the Actions group.  Defaults to standard execute/preview/reset.
GLOBAL-SECTION is an optional symbol for a global options section
to include (e.g., beads-option-global-section).

This macro defines:
1. All infixes from class slot metadata
2. All groups organized by :transient-group
3. The prefix combining all groups plus actions

Example:
  (beads-meta-define-prefix beads-command-create \"beads-create\"
    \"Create a new issue in Beads.\"
    ((\"x\" \"Create issue\" beads-create--execute)
     (\"P\" \"Preview command\" beads-create--preview)
     (\"R\" \"Reset all fields\" beads-create--reset))
    beads-option-global-section)"
  (let* ((class-val (eval class))
         (prefix-val (eval prefix))
         (group-specs (beads-meta-generate-group-specs class-val prefix-val))
         (prefix-sym (intern prefix-val))
         (actions-val (or (eval actions)
                          `(("x" "Execute" ,(intern (format "%s--execute"
                                                           prefix-val)))
                            ("P" "Preview" ,(intern (format "%s--preview"
                                                           prefix-val)))
                            ("R" "Reset" ,(intern (format "%s--reset"
                                                          prefix-val))))))
         (global-sym (eval global-section)))
    `(progn
       ;; Define infixes
       (beads-meta-define-infixes ,class ,prefix)
       ;; Define groups
       (beads-meta-define-groups ,class ,prefix)
       ;; Define prefix
       (transient-define-prefix ,prefix-sym ()
         ,(or (eval docstring)
              (format "Transient menu for %s." prefix-val))
         ,@(mapcar (lambda (gs) (plist-get gs :name)) group-specs)
         ,@(when global-sym (list global-sym))
         ["Actions"
          ,@(mapcar (lambda (a)
                      `(,(nth 0 a) ,(nth 1 a) ,(nth 2 a)))
                    actions-val)]))))

(defun beads-meta-generate-parse-function-name (prefix)
  "Generate the parse function name for PREFIX.
Returns a symbol like `beads-create--parse-transient-args'."
  (intern (format "%s--parse-transient-args" prefix)))

(defmacro beads-meta-define-standard-suffixes (class prefix)
  "Define standard execute/preview/reset suffixes for CLASS.
PREFIX is a string like \"beads-create\".

Generates three transient suffixes:
- PREFIX--execute: Validates and calls `beads-command-execute-interactive'
- PREFIX--preview: Validates and calls `beads-command-preview'
- PREFIX--reset: Calls `transient-reset'

These suffixes use the generic methods from beads-command.el,
which can be overridden per command class for custom behavior."
  (let* ((prefix-val (eval prefix))
         (prefix-sym (intern prefix-val))
         (execute-sym (intern (format "%s--execute" prefix-val)))
         (preview-sym (intern (format "%s--preview" prefix-val)))
         (reset-sym (intern (format "%s--reset" prefix-val)))
         (parse-fn-sym (beads-meta-generate-parse-function-name prefix-val)))
    `(progn
       ;; Define parse function first
       (beads-meta-define-parse-function ,class ,prefix)

       ;; Execute suffix
       (transient-define-suffix ,execute-sym ()
         "Execute the command with current parameters."
         :key "x"
         :description "Execute"
         (interactive)
         (let* ((args (transient-args ',prefix-sym))
                (cmd (,parse-fn-sym args))
                (error-msg (beads-command-validate cmd)))
           (if error-msg
               (user-error "Validation failed: %s" error-msg)
             (condition-case err
                 (beads-command-execute-interactive cmd)
               (error
                (message "Failed: %s" (error-message-string err)))))))

       ;; Preview suffix
       (transient-define-suffix ,preview-sym ()
         "Preview the command that will be executed."
         :key "P"
         :description "Preview"
         :transient t
         (interactive)
         (let* ((args (transient-args ',prefix-sym))
                (cmd (,parse-fn-sym args))
                (error-msg (beads-command-validate cmd)))
           (if error-msg
               (message "Validation errors: %s" error-msg)
             (beads-command-preview cmd))))

       ;; Reset suffix
       (transient-define-suffix ,reset-sym ()
         "Reset all parameters to their default values."
         :key "R"
         :description "Reset"
         :transient t
         (interactive)
         (when (y-or-n-p "Reset all fields? ")
           (transient-reset)
           (transient--redisplay)
           (message "All fields reset"))))))

(defmacro beads-meta-define-parse-function (class prefix)
  "Define a transient args parse function for CLASS.
PREFIX is a string like \"beads-create\".

The generated function takes transient args and returns a
populated instance of CLASS.

Example:
  (beads-meta-define-parse-function beads-command-create \"beads-create\")

Generates:
  (defun beads-create--parse-transient-args (args)
    \"Parse ARGS into a beads-command-create instance.\"
    ...)"
  (let* ((class-val (eval class))
         (prefix-val (eval prefix))
         (fn-name (beads-meta-generate-parse-function-name prefix-val))
         (infix-specs (beads-meta-generate-infix-specs class-val prefix-val)))
    `(defun ,fn-name (args)
       ,(format "Parse transient ARGS into a %s instance." class-val)
       (,class-val
        ,@(cl-mapcan
           (lambda (spec)
             (let* ((infix-name (plist-get spec :name))
                    ;; Extract slot name from infix name
                    (name-str (symbol-name infix-name))
                    (slot-name (intern
                                (replace-regexp-in-string
                                 (format "^%s-infix-" (regexp-quote prefix-val))
                                 "" name-str)))
                    (arg (plist-get spec :argument))
                    (trans-class (plist-get spec :class)))
               (when arg
                 (let* ((initarg (intern (format ":%s" slot-name)))
                        (is-switch (eq trans-class 'transient-switch)))
                   (list initarg
                         (if is-switch
                             `(transient-arg-value ,arg args)
                           `(transient-arg-value ,arg args)))))))
           infix-specs)))))

(defmacro beads-meta-define-transient (class prefix &optional docstring
                                             global-section)
  "Define a complete transient menu for CLASS with generated suffixes.
PREFIX is a string like \"beads-create\".
DOCSTRING is an optional documentation string.
GLOBAL-SECTION is an optional symbol for a global options section
to include (e.g., beads-option-global-section).

This macro generates everything needed for a transient menu:
1. All infixes from class slot metadata
2. All groups organized by :transient-group
3. Standard suffixes (execute, preview, reset) using generic methods
4. The prefix combining all groups plus actions

The generated suffixes call generic methods from beads-command.el:
- Execute calls `beads-command-execute-interactive' (override per class)
- Preview calls `beads-command-preview' (override per class)
- Reset calls `transient-reset'

IMPORTANT: Autoload cookies must be added manually.  Since this macro
generates a `transient-define-prefix' form, the autoload machinery
cannot automatically generate the autoload stub.  You MUST add an
explicit autoload cookie before the macro call:

  ;;;###autoload (autoload \\='beads-foo \"beads-foo\" nil t)
  (beads-meta-define-transient beads-command-foo \"beads-foo\"
    \"Do something with foo.\"
    beads-option-global-section)

The autoload form parameters are:
  - Symbol name (must match PREFIX)
  - File name (without .el extension)
  - nil (docstring will be loaded from the actual definition)
  - t (marks function as interactive for M-x)

Example (complete):
  ;;;###autoload (autoload \\='beads-create \"beads-create\" nil t)
  (beads-meta-define-transient beads-command-create \"beads-create\"
    \"Create a new issue.\"
    beads-option-global-section)

This replaces ~100 lines of manual infix/group/suffix definitions
with a single macro call."
  (let* ((class-val (eval class))
         (prefix-val (eval prefix))
         (group-specs (beads-meta-generate-group-specs class-val prefix-val))
         (prefix-sym (intern prefix-val))
         (execute-sym (intern (format "%s--execute" prefix-val)))
         (preview-sym (intern (format "%s--preview" prefix-val)))
         (reset-sym (intern (format "%s--reset" prefix-val))))
    `(progn
       ;; Define infixes from slot metadata
       (beads-meta-define-infixes ,class ,prefix)
       ;; Define groups
       (beads-meta-define-groups ,class ,prefix)
       ;; Define standard suffixes (execute, preview, reset)
       (beads-meta-define-standard-suffixes ,class ,prefix)
       ;; Define prefix
       (transient-define-prefix ,prefix-sym ()
         ,(or (eval docstring)
              (format "Transient menu for %s." prefix-val))
         ,@(mapcar (lambda (gs) (plist-get gs :name)) group-specs)
         ,@(when global-section (list global-section))
         ["Actions"
          (,execute-sym)
          (,preview-sym)
          (,reset-sym)]))))

(provide 'beads-meta)
;;; beads-meta.el ends here
