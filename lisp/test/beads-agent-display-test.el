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
(require 'beads-agent-display)

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

;;; beads-agent-display-format-session - rendering matrix
;;
;; Sessions are mocked via `cl-letf' on the accessor functions
;; (`beads-agent-session-type-name', `beads-agent-session-instance-number')
;; so the tests don't need to load `beads-agent-backend.el' (and pull
;; sesman) — `beads-agent-display.el' declares both accessors via
;; `declare-function' for the byte-compiler.
;;
;; The Task agent type registered in `beads-agent-types.el' has letter
;; "T" and icon "👷"; tests assume that registration is in effect.

(defmacro beads-agent-display-test--with-session (type-name instance-n &rest body)
  "Stub session accessors to return TYPE-NAME and INSTANCE-N, then run BODY.
The mock session itself is a symbol, since the accessors are mocked."
  (declare (indent 2) (debug t))
  `(let ((--mock-session-- 'beads-agent-display-test--session))
     (cl-letf (((symbol-function 'beads-agent-session-type-name)
                (lambda (_) ,type-name))
               ((symbol-function 'beads-agent-session-instance-number)
                (lambda (_) ,instance-n)))
       (let ((session --mock-session--))
         ,@body))))

(defun beads-agent-display-test--ensure-task-registered ()
  "Ensure the built-in Task agent type is registered.
Required because tests run with the icon \"👷\" and letter \"T\"."
  (require 'beads-agent-types)
  (unless (beads-agent-type-get "task")
    (beads-agent-type-register (beads-agent-type-task))))

;;;; Running state (default — icons mode)

(ert-deftest beads-agent-display-test-format-running-icon ()
  "Running state in icons mode renders the icon with working face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons t)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session)))
        (should (equal (substring-no-properties result) "👷"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-working))
        (should (string-match-p "Task agent #1: focused"
                                (get-text-property 0 'help-echo result)))))))

(ert-deftest beads-agent-display-test-format-running-letter ()
  "Running state in letter mode renders the letter with working face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session)))
        (should (equal (substring-no-properties result) "T"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-working))))))

;;;; Touched state

(ert-deftest beads-agent-display-test-format-touched-icon ()
  "Touched state in icons mode renders the icon with shadow face and no `~'."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons t)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 2
      (let ((result (beads-agent-display-format-session session 'touched)))
        (should (equal (substring-no-properties result) "👷"))
        (should-not (string-match-p "~" result))
        (should (eq (get-text-property 0 'face result) 'shadow))
        (should (string-match-p "touched but focused elsewhere"
                                (get-text-property 0 'help-echo result)))
        (should (string-match-p "Task agent #2"
                                (get-text-property 0 'help-echo result)))))))

(ert-deftest beads-agent-display-test-format-touched-letter ()
  "Touched state in letter mode renders the letter with shadow face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session 'touched)))
        (should (equal (substring-no-properties result) "T"))
        (should-not (string-match-p "~" result))
        (should (eq (get-text-property 0 'face result) 'shadow))))))

;;;; Finished state

(ert-deftest beads-agent-display-test-format-finished-icon ()
  "Finished state in icons mode renders ✓ + icon with finished face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons t)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session 'finished)))
        (should (equal (substring-no-properties result) "✓👷"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-finished))
        ;; Face covers the entire string, including the role glyph.
        (should (eq (get-text-property (1- (length result)) 'face result)
                    'beads-list-agent-finished))
        (should (string-match-p "Task agent: finished"
                                (get-text-property 0 'help-echo result)))))))

(ert-deftest beads-agent-display-test-format-finished-letter ()
  "Finished state in letter mode renders ✓T with finished face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session 'finished)))
        (should (equal (substring-no-properties result) "✓T"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-finished))))))

;;;; Failed state

(ert-deftest beads-agent-display-test-format-failed-icon ()
  "Failed state in icons mode renders ✗ + icon with failed face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons t)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session 'failed)))
        (should (equal (substring-no-properties result) "✗👷"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-failed))
        (should (eq (get-text-property (1- (length result)) 'face result)
                    'beads-list-agent-failed))
        (should (string-match-p "Task agent: failed"
                                (get-text-property 0 'help-echo result)))))))

(ert-deftest beads-agent-display-test-format-failed-letter ()
  "Failed state in letter mode renders ✗T with failed face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session 'failed)))
        (should (equal (substring-no-properties result) "✗T"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-failed))))))

;;;; Outcome marks always present regardless of icon mode

(ert-deftest beads-agent-display-test-format-outcome-marks-in-both-modes ()
  "✓/✗ outcome marks render in finished/failed output regardless of icon mode."
  (beads-agent-display-test--ensure-task-registered)
  (dolist (use-icons '(t nil))
    (let ((beads-agent-display-use-icons use-icons)
          (beads-agent-type-icons nil))
      (beads-agent-display-test--with-session "Task" 1
        (should (string-match-p
                 "✓"
                 (beads-agent-display-format-session session 'finished)))
        (should (string-match-p
                 "✗"
                 (beads-agent-display-format-session session 'failed)))))))

;;;; #N instance suffix gating

(ert-deftest beads-agent-display-test-format-no-instance-suffix-by-default ()
  "No `#N' suffix when `beads-agent-display-show-instance' is nil."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 3
      (let ((result (beads-agent-display-format-session session)))
        (should-not (string-match-p "#" result))))))

(ert-deftest beads-agent-display-test-format-instance-suffix-when-enabled ()
  "`#N' suffix appears when `beads-agent-display-show-instance' is t."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance t))
    (beads-agent-display-test--with-session "Task" 3
      (let ((result (beads-agent-display-format-session session)))
        (should (string-match-p "T#3" (substring-no-properties result)))))))

(ert-deftest beads-agent-display-test-format-brief-overrides-show-instance ()
  "BRIEF non-nil forces `#N' off even when show-instance is t."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance t))
    (beads-agent-display-test--with-session "Task" 3
      (let ((result (beads-agent-display-format-session session nil t)))
        (should-not (string-match-p "#" result))))))

;;;; nil outcome treated as 'running

(ert-deftest beads-agent-display-test-format-nil-outcome-is-running ()
  "Passing nil for OUTCOME is equivalent to the running state."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons t)
        (beads-agent-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((nil-out (beads-agent-display-format-session session nil))
            (running-out (beads-agent-display-format-session session 'running)))
        (should (equal nil-out running-out))
        (should (eq (get-text-property 0 'face nil-out)
                    'beads-list-agent-working))))))

;;;; beads-agent-display-format-type-name — outcome-path helper

(ert-deftest beads-agent-display-test-format-type-name-finished-icon ()
  "Type-name façade renders ✓ + role icon in icons mode."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons t)
        (beads-agent-type-icons nil))
    (let ((result (beads-agent-display-format-type-name "Task" 'finished)))
      (should (equal (substring-no-properties result) "✓👷"))
      (should (eq (get-text-property 0 'face result)
                  'beads-list-agent-finished)))))

(ert-deftest beads-agent-display-test-format-type-name-failed-letter ()
  "Type-name façade renders ✗T in letter mode."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil))
    (let ((result (beads-agent-display-format-type-name "Task" 'failed)))
      (should (equal (substring-no-properties result) "✗T"))
      (should (eq (get-text-property 0 'face result)
                  'beads-list-agent-failed)))))

(ert-deftest beads-agent-display-test-format-type-name-nil-type ()
  "Type-name façade falls back to the `●' glyph when TYPE-NAME is nil."
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil))
    (let ((result (beads-agent-display-format-type-name nil 'finished)))
      (should (equal (substring-no-properties result) "✓●")))))

;;;; Latent collision regression — custom :letter must win

(defclass beads-agent-display-test--collision (beads-agent-type)
  ((name :initform "Test")
   (letter :initform "X")
   (description :initform "Type whose name starts with `T' but letter is `X'."))
  :documentation
  "Regression coverage for the historical bug where renderers ignored
the registered `:letter' slot and used `(substring NAME 0 1)' — so a
type named \"Test\" with letter \"X\" rendered as \"T\", colliding
with the built-in Task type.")

(ert-deftest beads-agent-display-test-letter-collision-bug-fixed ()
  "A registered type with name \"Test\" and :letter \"X\" must render as X.
This is the latent collision bug called out in the bde-npte epic: the
old `(substring type-name 0 1)' callsites would have rendered this
type as \"T\", colliding with Task."
  (require 'beads-agent-types)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-type-icons nil)
        (collision (beads-agent-display-test--collision)))
    (unwind-protect
        (progn
          (beads-agent-type-register collision)
          ;; Direct accessor returns the registered letter, NOT "T".
          (should (equal (beads-agent-type-icon-or-letter collision) "X"))
          ;; The display helper used by every UI surface also returns "X".
          (beads-agent-display-test--with-session "Test" 1
            (let ((result (beads-agent-display-format-session session nil t)))
              (should (equal (substring-no-properties result) "X")))))
      (beads-agent-type--unregister "Test"))))

(provide 'beads-agent-display-test)

;;; beads-agent-display-test.el ends here
