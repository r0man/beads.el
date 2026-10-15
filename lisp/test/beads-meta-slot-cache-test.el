;;; beads-meta-slot-cache-test.el --- Memoized slot metadata tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Regression tests for the per-class slot-metadata memo in
;; `beads-meta--class-slot-meta' (see `beads-meta--slot-meta-cache').
;; A 1000-issue store exposed `beads-from-json' as the dominant parse
;; hotspot because it re-derived every slot's JSON key, initarg and type
;; on every object; the memo makes that O(slots) per class instead of
;; O(slots^2) per object.  These tests pin the correctness properties
;; the optimisation depends on: identity reuse across calls, invalidation
;; on class redefinition (EIEIO reuses the class object in place), and
;; exact parity with the public slot accessors.

;;; Code:

(require 'ert)
(require 'beads-meta)
(require 'beads-types)

;;; Test Classes

(defclass beads-meta-slot-cache-test-item ()
  ((id
    :initarg :id
    :type (or null string)
    :initform nil
    :documentation "Identifier."
    :json-key id)
   (display-name
    :initarg :display-name
    :type (or null string)
    :initform nil
    :documentation "Human-readable name with an overridden JSON key."
    :json-key display_name)
   (count
    :initarg :count
    :type (or null integer)
    :initform nil
    :documentation "Integer, coerced from a JSON string.")
   (tags
    :initarg :tags
    :type (or null (list-of string))
    :initform nil
    :documentation "List of strings.")
   (enabled
    :initarg :enabled
    :type boolean
    :initform nil
    :documentation "Boolean flag.")))

;;; Memoization

(ert-deftest beads-meta-slot-cache-memoizes-per-class ()
  "The slot-metadata list is computed once and reused for a class."
  (let ((first (beads-meta--class-slot-meta 'beads-meta-slot-cache-test-item)))
    (should (equal (length first) 5))
    (should (eq first
                (beads-meta--class-slot-meta
                 'beads-meta-slot-cache-test-item)))))

(ert-deftest beads-meta-slot-cache-invalidated-on-redefinition ()
  "Redefining a class drops its cached metadata.
EIEIO reuses the class object across `defclass', so the memo must be
invalidated by the class-definition advice rather than by object
identity; otherwise a reloaded class would serve stale slot names."
  (defclass beads-meta-slot-cache-test-redef ()
    ((only-slot :initarg :only-slot)))
  (should (equal '(only-slot)
                 (beads-meta-command-slots
                  'beads-meta-slot-cache-test-redef)))
  ;; Populate the cache, then redefine with a different slot set.
  (beads-meta--class-slot-meta 'beads-meta-slot-cache-test-redef)
  (defclass beads-meta-slot-cache-test-redef ()
    ((renamed-slot :initarg :renamed-slot)))
  (should (equal '(renamed-slot)
                 (beads-meta-command-slots
                  'beads-meta-slot-cache-test-redef))))

;;; Parity With Public Accessors

(ert-deftest beads-meta-slot-meta-matches-accessors ()
  "The cached metadata agrees with the public slot accessors."
  (let* ((class 'beads-meta-slot-cache-test-item)
         (meta (beads-meta--slot-meta class 'display-name)))
    (should (eq (plist-get meta :json-key) 'display_name))
    (should (eq (beads-meta--slot-json-key class 'display-name) 'display_name))
    (should (eq (beads-meta-slot-initarg class 'display-name) :display-name))
    (should (equal (beads-meta-slot-type class 'count) '(or null integer)))
    (should (eq (beads-meta-slot-property class 'id :json-key) 'id))
    (should (equal (beads-meta-command-slots class)
                   (mapcar (lambda (m) (plist-get m :name))
                           (beads-meta--class-slot-meta class))))))

(ert-deftest beads-meta-from-json-constructs-via-cache ()
  "`beads-from-json' builds objects from the cached slot table."
  (let ((obj (beads-from-json
              'beads-meta-slot-cache-test-item
              '((id . "x1")
                (display_name . "Hello")
                (count . "7")
                (tags . ["a" "b"])
                (enabled . t)))))
    (should (equal (oref obj id) "x1"))
    (should (equal (oref obj display-name) "Hello"))
    (should (eql (oref obj count) 7))
    (should (equal (oref obj tags) '("a" "b")))
    (should (eq (oref obj enabled) t))))

;;; Edge Cases

(ert-deftest beads-meta-slot-cache-unknown-class ()
  "Unknown classes yield nil metadata rather than signalling."
  (should-not (beads-meta--class-slot-meta
               'beads-meta-slot-cache-test-missing))
  (should-not (beads-meta--slot-meta
               'beads-meta-slot-cache-test-item 'nope)))

(provide 'beads-meta-slot-cache-test)
;;; beads-meta-slot-cache-test.el ends here
