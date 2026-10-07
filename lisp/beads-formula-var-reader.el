;;; beads-formula-var-reader.el --- Typed formula variable readers -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The formula variable reader ABI (design.md §4.7).  It maps a
;; `beads-formula-var' declaration (type, enum, name convention) to a
;; transient reader kind, and reads one value with the matching Emacs
;; reader, so a var is read the same way in the formula browser, the
;; instantiate flow and the sling How stage.
;;
;; It lives in its own module so extension packages (and downstream
;; formula surfaces such as the cook preview) can depend on the ABI
;; without loading the whole browser, and so the reader stays the one
;; implementation instead of a per-surface fork (REQ-SF-014, REQ-SF-081).
;;
;; - `beads-formula-var-reader' generic returns the reader spec plist.
;; - `beads-formula-var-kind' is the `:kind' shortcut.
;; - `beads-formula-read-var' reads one value with the typed reader.
;; - `beads-formula-read-vars' reads every required var without a
;;   default from a formula.
;;
;; Downstream packages may specialize `beads-formula-var-reader' on
;; their own metadata class; `beads-formula-read-var' then follows.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'beads-types)

;;; Forward Declarations

(declare-function beads-sling-targets "beads-sling" (&optional bead))
(declare-function beads-formula-var-choices "beads-formula"
                  (var &optional formula))

;;; ============================================================
;;; Blank Helpers
;;; ============================================================

(defun beads-formula--blank-p (value)
  "Return non-nil when VALUE is nil or the empty string."
  (or (null value) (and (stringp value) (string-empty-p value))))

;;; ============================================================
;;; Variable Readers
;;; ============================================================

(cl-defgeneric beads-formula-var-reader (var &optional formula)
  "Return the transient reader spec for formula variable VAR.
FORMULA is the formula VAR belongs to, when known; it supplies the
methodology mapping `beads-formula-var-choices' consults for a
built-in enum.  The spec is a plist; `:kind' is one of `enum', `bool',
`numeric', `file', `directory', `agent' or `string'.  Optional keys are
`:name', `:prompt', `:required', `:default', `:pattern' and `:choices'.

The declared shape wins (an enum list, a boolean or integer `var-type'),
then naming conventions (`context_path'/`*_path' is a file,
`artifact_root' a directory, `*_target' an agent, numeric defaults or
`max_'/`_iterations' names are numeric).  Anything unrecognised fails
soft to plain string entry.  Downstream packages may specialize this
generic on their own metadata.")

(cl-defmethod beads-formula-var-reader ((var beads-formula-var)
                                        &optional formula)
  "Return the reader spec for VAR (see `beads-formula-var-reader')."
  (let ((name (or (oref var name) ""))
        (type (oref var var-type))
        (choices (if (fboundp 'beads-formula-var-choices)
                     (beads-formula-var-choices var formula)
                   (oref var enum)))
        (default (oref var default)))
    (list :kind (cond
                 (choices 'enum)
                 ((equal type "bool") 'bool)
                 ((equal type "int") 'numeric)
                 ((or (equal name "context_path")
                      (string-suffix-p "_path" name))
                  'file)
                 ((equal name "artifact_root") 'directory)
                 ((string-suffix-p "_target" name) 'agent)
                 ((or (and default (string-match-p "\\`[0-9]+\\'" default))
                      (string-prefix-p "max_" name)
                      (string-suffix-p "_iterations" name))
                  'numeric)
                 (t 'string))
          :name name
          :prompt (or (oref var description) name)
          :required (oref var required)
          :default default
          :pattern (oref var pattern)
          :choices choices)))

(defun beads-formula-var-kind (var &optional formula)
  "Return VAR's reader kind symbol in FORMULA.
A `beads-formula-var-reader' shortcut."
  (plist-get (beads-formula-var-reader var formula) :kind))

;;; ============================================================
;;; Reading One Value
;;; ============================================================

(defun beads-formula-read-var (var &optional formula)
  "Read one value for VAR using its `beads-formula-var-reader' spec.
FORMULA supplies the metadata enum mapping, when known.  Returns the
value as a string, or nil when the user leaves it blank."
  (let* ((spec (beads-formula-var-reader var formula))
         (kind (plist-get spec :kind))
         (prompt (concat (or (plist-get spec :prompt) "Value")
                         (when-let* ((default (plist-get spec :default)))
                           (format " [%s]" default))
                         ": ")))
    (pcase kind
      ('enum (completing-read prompt (plist-get spec :choices) nil t
                              nil nil (plist-get spec :default)))
      ('bool (if (y-or-n-p (concat prompt "yes/no? ")) "true" "false"))
      ('file (read-file-name prompt nil nil t nil))
      ('directory (read-directory-name prompt))
      ('agent (beads-formula--read-agent prompt))
      ('numeric (let ((n (read-number prompt
                                      (and (plist-get spec :default)
                                           (string-to-number
                                            (plist-get spec :default))))))
                 (and n (number-to-string n))))
      (_ (read-string prompt nil nil (plist-get spec :default))))))

(defun beads-formula--read-agent (prompt)
  "Read an agent target name with PROMPT.
Uses `beads-sling-targets' when sling is loaded, else plain entry."
  (if (fboundp 'beads-sling-targets)
      (let ((names (mapcar (lambda (target) (format "%s" (oref target name)))
                           (beads-sling-targets))))
        (if names
            (completing-read prompt names nil t)
          (read-string prompt)))
    (read-string prompt)))

(defun beads-formula-read-vars (formula)
  "Read values for FORMULA's required vars without a default.
Returns a (NAME . VALUE) alist; optional and defaulted vars are left to
the formula's own defaults."
  (let ((values nil))
    (dolist (var (oref formula vars))
      (when (and (oref var required)
                 (beads-formula--blank-p (oref var default)))
        (let ((value (beads-formula-read-var var)))
          (unless (beads-formula--blank-p value)
            (push (cons (oref var name) value) values)))))
    (nreverse values)))

(provide 'beads-formula-var-reader)
;;; beads-formula-var-reader.el ends here
