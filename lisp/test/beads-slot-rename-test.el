;;; beads-slot-rename-test.el --- Tests for slot property rename infrastructure -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for the slot property rename infrastructure (bde-vta5):
;; - :separator alias for :option-separator
;; - :json-key support with hyphen → underscore default
;; - :short-option → :transient-key inference
;; - beads-meta--slot-json-key helper
;; - beads-meta-slot-initarg and beads-meta-slot-type accessors

;;; Code:

(require 'ert)
(require 'beads-command)
(require 'beads-meta)

;;; ============================================================
;;; Test Classes
;;; ============================================================

;; Test command using :separator shorthand
(beads-defcommand beads-command-test-rename (beads-command-global-options)
  ((labels
    :option-type :list
    :separator ","
    :key "l"
    :required t)
   ;; Slot with :short-option but no :key — should infer :key
   ;; (only when transient metadata like :group is present)
   (status
    :option-type :string
    :short-option "s"
    :group "Filters")
   ;; Slot with custom :json-key
   (issue-type
    :option-type :string
    :key "t"
    :json-key dep_type)
   ;; Slot with default json-key (hyphen → underscore)
   (created-at
    :option-type :string
    :key "c"))
  :documentation "Test command for rename infrastructure."
  :transient nil)

;;; ============================================================
;;; Tests for :separator alias
;;; ============================================================

(ert-deftest beads-slot-rename-test-separator-alias ()
  ":separator expands to :option-separator."
  (should (equal ","
                 (beads-meta-slot-property
                  'beads-command-test-rename 'labels :option-separator))))

(ert-deftest beads-slot-rename-test-separator-concise ()
  ":separator is also accessible by concise name."
  (should (equal ","
                 (beads-meta-slot-property
                  'beads-command-test-rename 'labels :separator))))

;;; ============================================================
;;; Tests for beads-meta--slot-json-key
;;; ============================================================

(ert-deftest beads-slot-rename-test-json-key-default ()
  "Default json-key converts hyphens to underscores."
  (should (eq 'created_at
              (beads-meta--slot-json-key
               'beads-command-test-rename 'created-at))))

(ert-deftest beads-slot-rename-test-json-key-custom ()
  "Custom :json-key overrides default convention."
  (should (eq 'dep_type
              (beads-meta--slot-json-key
               'beads-command-test-rename 'issue-type))))

(ert-deftest beads-slot-rename-test-json-key-no-hyphens ()
  "Slot name without hyphens maps to itself."
  (should (eq 'labels
              (beads-meta--slot-json-key
               'beads-command-test-rename 'labels))))

;;; ============================================================
;;; Tests for :short-option → :transient-key inference
;;; ============================================================

(ert-deftest beads-slot-rename-test-short-option-infers-key ()
  ":short-option infers :transient-key when :key is absent."
  (should (equal "s"
                 (beads-meta-slot-property
                  'beads-command-test-rename 'status :transient-key))))

(ert-deftest beads-slot-rename-test-explicit-key-not-overridden ()
  "Explicit :key is not overridden by :short-option."
  ;; labels has :key "l" explicitly set
  (should (equal "l"
                 (beads-meta-slot-property
                  'beads-command-test-rename 'labels :transient-key))))

;;; ============================================================
;;; Tests for beads-meta-slot-type and beads-meta-slot-initarg
;;; ============================================================

(ert-deftest beads-slot-rename-test-slot-type ()
  "beads-meta-slot-type returns EIEIO :type."
  (require 'beads-command-close)
  ;; issue-ids on close has type (or null list)
  (let ((typ (beads-meta-slot-type 'beads-command-close 'issue-ids)))
    (should typ)
    (should (listp typ))))

(ert-deftest beads-slot-rename-test-slot-initarg ()
  "beads-meta-slot-initarg returns initarg keyword."
  (require 'beads-command-close)
  (should (eq :reason
              (beads-meta-slot-initarg 'beads-command-close 'reason))))

(provide 'beads-slot-rename-test)

;;; beads-slot-rename-test.el ends here
