;;; beads-agent-display-test.el --- Tests for agent display accessors -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for the icon-resolution defcustoms and accessors added
;; in bde-npte.2:
;;   - `beads-agent-display-use-icons'
;;   - `beads-agent-type-icons'
;;   - `beads-agent-display-show-instance'
;;   - `beads-agent-type-icon' (generic + method)
;;   - `beads-agent--icons-supported-p'
;;   - `beads-agent-type-icon-or-letter'
;;
;; Tests cover the override > slot > nil resolution order, gating
;; via `beads-agent-display-use-icons' (`auto', t, nil), and letter
;; fallback for missing-icon and explicitly-nil-override cases.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-type)

;;; Mock Types

(defclass beads-agent-display-test--task (beads-agent-type)
  ((name :initform "Task")
   (letter :initform "T")
   (icon :initform "👷")
   (description :initform "Task agent for display tests"))
  :documentation "Mock Task type with both icon and letter set.")

(defclass beads-agent-display-test--iconless (beads-agent-type)
  ((name :initform "Plain")
   (letter :initform "X")
   (description :initform "Type with no icon slot value"))
  :documentation "Mock type whose `icon' slot is nil (default).")

;;; Defcustom Existence Tests

(ert-deftest beads-agent-display-test-defcustom-use-icons-defined ()
  "`beads-agent-display-use-icons' is defined and defaults to `auto'."
  (should (boundp 'beads-agent-display-use-icons))
  (should (eq (default-value 'beads-agent-display-use-icons) 'auto))
  (should (get 'beads-agent-display-use-icons 'custom-type)))

(ert-deftest beads-agent-display-test-defcustom-type-icons-defined ()
  "`beads-agent-type-icons' is defined and defaults to nil."
  (should (boundp 'beads-agent-type-icons))
  (should (null (default-value 'beads-agent-type-icons)))
  (should (get 'beads-agent-type-icons 'custom-type)))

(ert-deftest beads-agent-display-test-defcustom-show-instance-defined ()
  "`beads-agent-display-show-instance' is defined and defaults to nil."
  (should (boundp 'beads-agent-display-show-instance))
  (should (null (default-value 'beads-agent-display-show-instance)))
  (should (eq (get 'beads-agent-display-show-instance 'custom-type) 'boolean)))

;;; beads-agent-type-icon - resolution order

(ert-deftest beads-agent-display-test-icon-from-slot ()
  "Returns the `icon' slot value when no override is present."
  (let ((beads-agent-type-icons nil)
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon type) "👷"))))

(ert-deftest beads-agent-display-test-icon-override-wins-over-slot ()
  "An override entry in `beads-agent-type-icons' wins over the slot."
  (let ((beads-agent-type-icons '(("task" . "🛠")))
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon type) "🛠"))))

(ert-deftest beads-agent-display-test-icon-override-case-insensitive ()
  "Override lookup is case-insensitive on the type name."
  (let ((beads-agent-type-icons '(("task" . "🛠")))
        ;; Type name is registered as "Task" (mixed case).
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon type) "🛠"))))

(ert-deftest beads-agent-display-test-icon-override-explicit-nil ()
  "An override entry with a nil cdr returns nil — slot is NOT consulted."
  (let ((beads-agent-type-icons '(("task" . nil)))
        (type (beads-agent-display-test--task)))
    (should (null (beads-agent-type-icon type)))))

(ert-deftest beads-agent-display-test-icon-slot-nil-no-override ()
  "Returns nil when the slot is nil and no override is configured."
  (let ((beads-agent-type-icons nil)
        (type (beads-agent-display-test--iconless)))
    (should (null (beads-agent-type-icon type)))))

(ert-deftest beads-agent-display-test-icon-no-match-falls-to-slot ()
  "An override alist with unrelated entries falls through to the slot."
  (let ((beads-agent-type-icons '(("review" . "🔍")))
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon type) "👷"))))

;;; beads-agent--icons-supported-p - gating

(ert-deftest beads-agent-display-test-supported-p-always-t ()
  "`use-icons' = t returns non-nil regardless of frame."
  (let ((beads-agent-display-use-icons t))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (should (beads-agent--icons-supported-p)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (should (beads-agent--icons-supported-p)))))

(ert-deftest beads-agent-display-test-supported-p-always-nil ()
  "`use-icons' = nil returns nil regardless of frame."
  (let ((beads-agent-display-use-icons nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (should-not (beads-agent--icons-supported-p)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (should-not (beads-agent--icons-supported-p)))))

(ert-deftest beads-agent-display-test-supported-p-auto-gui ()
  "`use-icons' = `auto' returns t under a GUI frame."
  (let ((beads-agent-display-use-icons 'auto))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (should (beads-agent--icons-supported-p)))))

(ert-deftest beads-agent-display-test-supported-p-auto-tty ()
  "`use-icons' = `auto' returns nil under a TTY frame."
  (let ((beads-agent-display-use-icons 'auto))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (should-not (beads-agent--icons-supported-p)))))

;;; beads-agent-type-icon-or-letter - integration

(ert-deftest beads-agent-display-test-or-letter-icon-when-supported ()
  "Returns icon when icons are enabled and one is configured."
  (let ((beads-agent-display-use-icons t)
        (beads-agent-type-icons nil)
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon-or-letter type) "👷"))))

(ert-deftest beads-agent-display-test-or-letter-letter-when-disabled ()
  "Returns letter when icons are disabled, even if icon is configured."
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil)
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon-or-letter type) "T"))))

(ert-deftest beads-agent-display-test-or-letter-letter-when-slot-nil ()
  "Returns letter when icons are enabled but slot has no icon."
  (let ((beads-agent-display-use-icons t)
        (beads-agent-type-icons nil)
        (type (beads-agent-display-test--iconless)))
    (should (equal (beads-agent-type-icon-or-letter type) "X"))))

(ert-deftest beads-agent-display-test-or-letter-override-wins ()
  "Override icon wins when icons are enabled."
  (let ((beads-agent-display-use-icons t)
        (beads-agent-type-icons '(("task" . "🛠")))
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon-or-letter type) "🛠"))))

(ert-deftest beads-agent-display-test-or-letter-override-nil-falls-to-letter ()
  "Override explicitly nil falls back to letter (skips slot)."
  (let ((beads-agent-display-use-icons t)
        (beads-agent-type-icons '(("task" . nil)))
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon-or-letter type) "T"))))

(ert-deftest beads-agent-display-test-or-letter-auto-gui ()
  "`auto' under a GUI frame returns the icon."
  (let ((beads-agent-display-use-icons 'auto)
        (beads-agent-type-icons nil)
        (type (beads-agent-display-test--task)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (should (equal (beads-agent-type-icon-or-letter type) "👷")))))

(ert-deftest beads-agent-display-test-or-letter-auto-tty ()
  "`auto' under a TTY frame returns the letter."
  (let ((beads-agent-display-use-icons 'auto)
        (beads-agent-type-icons nil)
        (type (beads-agent-display-test--task)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (should (equal (beads-agent-type-icon-or-letter type) "T")))))

(provide 'beads-agent-display-test)

;;; beads-agent-display-test.el ends here
