;;; beads-type-inference-test.el --- Tests for type-based serialization inference -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for the type-based serialization inference (D12):
;; - beads-meta--unwrap-nullable: robust (or ...) type normalization
;; - beads-meta--infer-option-type: infers serialization from :type
;; - Priority: list-of > integer > string

;;; Code:

(require 'ert)
(require 'beads-meta)

;;; ============================================================
;;; Tests for beads-meta--unwrap-nullable
;;; ============================================================

(ert-deftest beads-type-inference-test-unwrap-simple-nullable ()
  "Unwrap (or null string) to string."
  (should (eq (beads-meta--unwrap-nullable '(or null string)) 'string)))

(ert-deftest beads-type-inference-test-unwrap-nullable-integer ()
  "Unwrap (or null integer) to integer."
  (should (eq (beads-meta--unwrap-nullable '(or null integer)) 'integer)))

(ert-deftest beads-type-inference-test-unwrap-nullable-list-of ()
  "Unwrap (or null (list-of string)) to (list-of string)."
  (should (equal (beads-meta--unwrap-nullable '(or null (list-of string)))
                 '(list-of string))))

(ert-deftest beads-type-inference-test-unwrap-non-nullable ()
  "Non-or types pass through unchanged."
  (should (eq (beads-meta--unwrap-nullable 'string) 'string))
  (should (eq (beads-meta--unwrap-nullable 'boolean) 'boolean))
  (should (equal (beads-meta--unwrap-nullable '(list-of string))
                 '(list-of string))))

(ert-deftest beads-type-inference-test-unwrap-null-first ()
  "Handle null appearing first in (or null X)."
  (should (eq (beads-meta--unwrap-nullable '(or null string)) 'string)))

(ert-deftest beads-type-inference-test-unwrap-null-last ()
  "Handle null appearing last in (or X null)."
  (should (eq (beads-meta--unwrap-nullable '(or string null)) 'string)))

(ert-deftest beads-type-inference-test-unwrap-multiple-non-null ()
  "Multiple non-null types: use most-specific priority."
  ;; list-of > integer > string
  (should (eq (beads-meta--unwrap-nullable '(or null string integer))
              'integer))
  (should (equal (beads-meta--unwrap-nullable
                  '(or null string (list-of string)))
                 '(list-of string))))

(ert-deftest beads-type-inference-test-unwrap-only-null ()
  "(or null) returns nil."
  (should-not (beads-meta--unwrap-nullable '(or null))))

(ert-deftest beads-type-inference-test-unwrap-t ()
  "Type t passes through."
  (should (eq (beads-meta--unwrap-nullable t) t)))

(ert-deftest beads-type-inference-test-unwrap-nil ()
  "Nil passes through."
  (should-not (beads-meta--unwrap-nullable nil)))

;;; ============================================================
;;; Tests for beads-meta--infer-option-type (updated inference)
;;; ============================================================

;; Direct types
(ert-deftest beads-type-inference-test-boolean ()
  "Boolean type infers :boolean."
  (should (eq (beads-meta--infer-option-type '(:type boolean))
              :boolean)))

(ert-deftest beads-type-inference-test-integer ()
  "Integer type infers :integer."
  (should (eq (beads-meta--infer-option-type '(:type integer))
              :integer)))

(ert-deftest beads-type-inference-test-string ()
  "String type infers :string."
  (should (eq (beads-meta--infer-option-type '(:type string))
              :string)))

(ert-deftest beads-type-inference-test-number ()
  "Number type infers :integer."
  (should (eq (beads-meta--infer-option-type '(:type number))
              :integer)))

;; Nullable types
(ert-deftest beads-type-inference-test-nullable-string ()
  "(or null string) infers :string."
  (should (eq (beads-meta--infer-option-type '(:type (or null string)))
              :string)))

(ert-deftest beads-type-inference-test-nullable-integer ()
  "(or null integer) infers :integer."
  (should (eq (beads-meta--infer-option-type '(:type (or null integer)))
              :integer)))

(ert-deftest beads-type-inference-test-nullable-boolean ()
  "(or null boolean) infers :boolean."
  (should (eq (beads-meta--infer-option-type '(:type (or null boolean)))
              :boolean)))

;; list-of types
(ert-deftest beads-type-inference-test-list-of-string ()
  "(list-of string) infers :list."
  (should (eq (beads-meta--infer-option-type '(:type (list-of string)))
              :list)))

(ert-deftest beads-type-inference-test-list-of-integer ()
  "(list-of integer) infers :list."
  (should (eq (beads-meta--infer-option-type '(:type (list-of integer)))
              :list)))

(ert-deftest beads-type-inference-test-nullable-list-of ()
  "(or null (list-of string)) infers :list."
  (should (eq (beads-meta--infer-option-type
               '(:type (or null (list-of string))))
              :list)))

;; Bare list type
(ert-deftest beads-type-inference-test-bare-list ()
  "Bare list type infers :list."
  (should (eq (beads-meta--infer-option-type '(:type list))
              :list)))

(ert-deftest beads-type-inference-test-nullable-list ()
  "(or null list) infers :list."
  (should (eq (beads-meta--infer-option-type '(:type (or null list)))
              :list)))

;; Omitted/default
(ert-deftest beads-type-inference-test-type-t ()
  "Type t infers :string (default fallback)."
  (should (eq (beads-meta--infer-option-type '(:type t))
              :string)))

(ert-deftest beads-type-inference-test-no-type ()
  "No :type returns nil (no inference)."
  (should-not (beads-meta--infer-option-type '())))

;; Already set
(ert-deftest beads-type-inference-test-already-set ()
  "Returns nil when :option-type already set."
  (should-not (beads-meta--infer-option-type
               '(:option-type :boolean :type boolean))))

;; Multi-type (or ...) priority
(ert-deftest beads-type-inference-test-multi-type-priority ()
  "(or null string integer) prefers integer over string."
  (should (eq (beads-meta--infer-option-type
               '(:type (or null string integer)))
              :integer)))

(ert-deftest beads-type-inference-test-multi-type-list-of-wins ()
  "(or null string (list-of string)) prefers list-of."
  (should (eq (beads-meta--infer-option-type
               '(:type (or null string (list-of string))))
              :list)))

(provide 'beads-type-inference-test)

;;; beads-type-inference-test.el ends here
