;;; beads-agent-display-test.el --- Tests for agent display accessors -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT tests for the icon-resolution defcustoms and accessors added
;; in bde-npte.2:
;;   - `beads-agent-display-use-icons'
;;   - `beads-agent-display-type-icons'
;;   - `beads-agent-display-show-instance'
;;   - `beads-agent-type-icon' (generic + method)
;;   - `beads-agent-icons-supported-p'
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
  "`beads-agent-display-type-icons' is defined and defaults to nil."
  (should (boundp 'beads-agent-display-type-icons))
  (should (null (default-value 'beads-agent-display-type-icons)))
  (should (get 'beads-agent-display-type-icons 'custom-type)))

(ert-deftest beads-agent-display-test-defcustom-show-instance-defined ()
  "`beads-agent-display-show-instance' is defined and defaults to nil."
  (should (boundp 'beads-agent-display-show-instance))
  (should (null (default-value 'beads-agent-display-show-instance)))
  (should (eq (get 'beads-agent-display-show-instance 'custom-type) 'boolean)))

;;; beads-agent-type-icon - resolution order

(ert-deftest beads-agent-display-test-icon-from-slot ()
  "Returns the `icon' slot value when no override is present."
  (let ((beads-agent-display-type-icons nil)
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon type) "👷"))))

(ert-deftest beads-agent-display-test-icon-override-wins-over-slot ()
  "An override entry in `beads-agent-display-type-icons' wins over the slot."
  (let ((beads-agent-display-type-icons '(("task" . "🛠")))
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon type) "🛠"))))

(ert-deftest beads-agent-display-test-icon-override-case-insensitive ()
  "Override lookup is case-insensitive on the type name."
  (let ((beads-agent-display-type-icons '(("task" . "🛠")))
        ;; Type name is registered as "Task" (mixed case).
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon type) "🛠"))))

(ert-deftest beads-agent-display-test-icon-override-explicit-nil ()
  "An override entry with a nil cdr returns nil — slot is NOT consulted."
  (let ((beads-agent-display-type-icons '(("task" . nil)))
        (type (beads-agent-display-test--task)))
    (should (null (beads-agent-type-icon type)))))

(ert-deftest beads-agent-display-test-icon-slot-nil-no-override ()
  "Returns nil when the slot is nil and no override is configured."
  (let ((beads-agent-display-type-icons nil)
        (type (beads-agent-display-test--iconless)))
    (should (null (beads-agent-type-icon type)))))

(ert-deftest beads-agent-display-test-icon-no-match-falls-to-slot ()
  "An override alist with unrelated entries falls through to the slot."
  (let ((beads-agent-display-type-icons '(("review" . "🔍")))
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon type) "👷"))))

;;; beads-agent-icons-supported-p - gating

(ert-deftest beads-agent-display-test-supported-p-always-t ()
  "`use-icons' = t returns non-nil regardless of frame."
  (let ((beads-agent-display-use-icons t))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (should (beads-agent-icons-supported-p)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (should (beads-agent-icons-supported-p)))))

(ert-deftest beads-agent-display-test-supported-p-always-nil ()
  "`use-icons' = nil returns nil regardless of frame."
  (let ((beads-agent-display-use-icons nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (should-not (beads-agent-icons-supported-p)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (should-not (beads-agent-icons-supported-p)))))

(ert-deftest beads-agent-display-test-supported-p-auto-gui ()
  "`use-icons' = `auto' returns t under a GUI frame."
  (let ((beads-agent-display-use-icons 'auto))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (should (beads-agent-icons-supported-p)))))

(ert-deftest beads-agent-display-test-supported-p-auto-tty ()
  "`use-icons' = `auto' returns nil under a TTY frame."
  (let ((beads-agent-display-use-icons 'auto))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (should-not (beads-agent-icons-supported-p)))))

;;; beads-agent-type-icon-or-letter - integration

(ert-deftest beads-agent-display-test-or-letter-icon-when-supported ()
  "Returns icon when icons are enabled and one is configured."
  (let ((beads-agent-display-use-icons t)
        (beads-agent-display-type-icons nil)
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon-or-letter type) "👷"))))

(ert-deftest beads-agent-display-test-or-letter-letter-when-disabled ()
  "Returns letter when icons are disabled, even if icon is configured."
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil)
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon-or-letter type) "T"))))

(ert-deftest beads-agent-display-test-or-letter-letter-when-slot-nil ()
  "Returns letter when icons are enabled but slot has no icon."
  (let ((beads-agent-display-use-icons t)
        (beads-agent-display-type-icons nil)
        (type (beads-agent-display-test--iconless)))
    (should (equal (beads-agent-type-icon-or-letter type) "X"))))

(ert-deftest beads-agent-display-test-or-letter-override-wins ()
  "Override icon wins when icons are enabled."
  (let ((beads-agent-display-use-icons t)
        (beads-agent-display-type-icons '(("task" . "🛠")))
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon-or-letter type) "🛠"))))

(ert-deftest beads-agent-display-test-or-letter-override-nil-falls-to-letter ()
  "Override explicitly nil falls back to letter (skips slot)."
  (let ((beads-agent-display-use-icons t)
        (beads-agent-display-type-icons '(("task" . nil)))
        (type (beads-agent-display-test--task)))
    (should (equal (beads-agent-type-icon-or-letter type) "T"))))

(ert-deftest beads-agent-display-test-or-letter-auto-gui ()
  "`auto' under a GUI frame returns the icon."
  (let ((beads-agent-display-use-icons 'auto)
        (beads-agent-display-type-icons nil)
        (type (beads-agent-display-test--task)))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (should (equal (beads-agent-type-icon-or-letter type) "👷")))))

(ert-deftest beads-agent-display-test-or-letter-auto-tty ()
  "`auto' under a TTY frame returns the letter."
  (let ((beads-agent-display-use-icons 'auto)
        (beads-agent-display-type-icons nil)
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
;; "T" and icon "🦅"; tests assume that registration is in effect.

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
  "Ensure the built-in `beads-agent-type-task' is registered as \"Task\".

Always (re-)registers the built-in type, regardless of what may be
currently registered under the name.  This protects tests that
assume the shipped icon (\"🦅\") and letter (\"T\") from earlier
tests that registered a *different* class under the same name (e.g.
the mock `beads-agent-display-test--task' with icon \"👷\") — the
naive `unless (beads-agent-type-get \"task\")' guard would silently
keep the mock, causing icon assertions to compare against the wrong
value.

`beads-agent-type-register' is idempotent for the same class: it
replaces an existing same-name entry and rewrites the letter
registry, so calling this in every test is safe and cheap."
  (require 'beads-agent-types)
  (beads-agent-type-register (beads-agent-type-task)))

;;;; Running state (default — icons mode)

(ert-deftest beads-agent-display-test-format-running-icon ()
  "Running state in icons mode renders the icon with working face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons t)
        (beads-agent-display-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session)))
        (should (equal (substring-no-properties result) "🦅"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-working))
        (should (string-match-p "Task agent #1: focused"
                                (get-text-property 0 'help-echo result)))))))

(ert-deftest beads-agent-display-test-format-running-letter ()
  "Running state in letter mode renders the letter with working face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil)
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
        (beads-agent-display-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 2
      (let ((result (beads-agent-display-format-session session 'touched)))
        (should (equal (substring-no-properties result) "🦅"))
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
        (beads-agent-display-type-icons nil)
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
        (beads-agent-display-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session 'finished)))
        (should (equal (substring-no-properties result) "✓🦅"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-finished))
        ;; Face covers the entire string, including the role glyph.
        (should (eq (get-text-property (1- (length result)) 'face result)
                    'beads-list-agent-finished))
        ;; Terminal states now carry the #N suffix in help-echo so two
        ;; sequential Task agents are disambiguated after they finish.
        (should (string-match-p "Task agent #1: finished"
                                (get-text-property 0 'help-echo result)))))))

(ert-deftest beads-agent-display-test-format-finished-letter ()
  "Finished state in letter mode renders ✓T with finished face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil)
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
        (beads-agent-display-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session 'failed)))
        (should (equal (substring-no-properties result) "✗🦅"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-failed))
        (should (eq (get-text-property (1- (length result)) 'face result)
                    'beads-list-agent-failed))
        ;; Terminal states now carry the #N suffix in help-echo.
        (should (string-match-p "Task agent #1: failed"
                                (get-text-property 0 'help-echo result)))))))

(ert-deftest beads-agent-display-test-format-failed-letter ()
  "Failed state in letter mode renders ✗T with failed face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session 'failed)))
        (should (equal (substring-no-properties result) "✗T"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-failed))))))

;;;; Stopped state — session terminated without a recorded outcome

(ert-deftest beads-agent-display-test-format-stopped-icon ()
  "Stopped state renders the icon with shadow face, no outcome prefix.
Distinguishes \"stopped\" from \"touched\" in help-echo wording."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons t)
        (beads-agent-display-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 4
      (let ((result (beads-agent-display-format-session session 'stopped)))
        (should (equal (substring-no-properties result) "🦅"))
        (should (eq (get-text-property 0 'face result) 'shadow))
        (let ((help (get-text-property 0 'help-echo result)))
          (should (string-match-p "Task agent #4: stopped" help))
          ;; Must NOT leak the touched wording into stopped.
          (should-not (string-match-p "touched" help)))))))

(ert-deftest beads-agent-display-test-format-stopped-letter ()
  "Stopped state in letter mode renders the letter with shadow face."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 1
      (let ((result (beads-agent-display-format-session session 'stopped)))
        (should (equal (substring-no-properties result) "T"))
        (should (eq (get-text-property 0 'face result) 'shadow))))))

;;;; Outcome marks always present regardless of icon mode

(ert-deftest beads-agent-display-test-format-outcome-marks-in-both-modes ()
  "✓/✗ outcome marks render in finished/failed output regardless of icon mode."
  (beads-agent-display-test--ensure-task-registered)
  (dolist (use-icons '(t nil))
    (let ((beads-agent-display-use-icons use-icons)
          (beads-agent-display-type-icons nil))
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
        (beads-agent-display-type-icons nil)
        (beads-agent-display-show-instance nil))
    (beads-agent-display-test--with-session "Task" 3
      (let ((result (beads-agent-display-format-session session)))
        (should-not (string-match-p "#" result))))))

(ert-deftest beads-agent-display-test-format-instance-suffix-when-enabled ()
  "`#N' suffix appears when `beads-agent-display-show-instance' is t."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil)
        (beads-agent-display-show-instance t))
    (beads-agent-display-test--with-session "Task" 3
      (let ((result (beads-agent-display-format-session session)))
        (should (string-match-p "T#3" (substring-no-properties result)))))))

(ert-deftest beads-agent-display-test-format-brief-overrides-show-instance ()
  "BRIEF non-nil forces `#N' off even when show-instance is t."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil)
        (beads-agent-display-show-instance t))
    (beads-agent-display-test--with-session "Task" 3
      (let ((result (beads-agent-display-format-session session nil t)))
        (should-not (string-match-p "#" result))))))

;;;; nil outcome treated as 'running

(ert-deftest beads-agent-display-test-format-nil-outcome-is-running ()
  "Passing nil for OUTCOME is equivalent to the running state."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons t)
        (beads-agent-display-type-icons nil)
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
        (beads-agent-display-type-icons nil))
    (let ((result (beads-agent-display-format-type-name "Task" 'finished)))
      (should (equal (substring-no-properties result) "✓🦅"))
      (should (eq (get-text-property 0 'face result)
                  'beads-list-agent-finished)))))

(ert-deftest beads-agent-display-test-format-type-name-failed-letter ()
  "Type-name façade renders ✗T in letter mode."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil))
    (let ((result (beads-agent-display-format-type-name "Task" 'failed)))
      (should (equal (substring-no-properties result) "✗T"))
      (should (eq (get-text-property 0 'face result)
                  'beads-list-agent-failed)))))

(ert-deftest beads-agent-display-test-format-type-name-nil-type ()
  "Type-name façade falls back to the `●' glyph when TYPE-NAME is nil."
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil))
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
        (beads-agent-display-type-icons nil)
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

;;;; beads-agent-display-format-issue-agents — shared issue-row badge group

(defmacro beads-agent-display-test--with-issue-agents
    (focused-list touched-list outcome &rest body)
  "Run BODY with mocked issue-session accessors.
FOCUSED-LIST is the list returned by
`beads-agent--get-sessions-focused-on-issue', TOUCHED-LIST the list
returned by `beads-agent--get-sessions-touching-issue', and OUTCOME
the value returned by `beads-agent--get-issue-outcome'.

`beads-agent--get-sessions-for-issue' (the legacy unsegmented
accessor) is stubbed to return nil here.  Tests that need to
exercise the `legacy-sessions' branch of
`beads-agent-display-format-issue-agents' should rebind it via a
nested `cl-letf' inside BODY (FOCUSED-LIST and TOUCHED-LIST should
both be nil in that case so the legacy branch is reached)."
  (declare (indent 3) (debug t))
  `(cl-letf (((symbol-function 'beads-agent--get-sessions-focused-on-issue)
              (lambda (_id) ,focused-list))
             ((symbol-function 'beads-agent--get-sessions-touching-issue)
              (lambda (_id) ,touched-list))
             ((symbol-function 'beads-agent--get-sessions-for-issue)
              (lambda (_id) nil))
             ((symbol-function 'beads-agent--get-issue-outcome)
              (lambda (_id) ,outcome)))
     ,@body))

(ert-deftest beads-agent-display-test-format-issue-agents-empty ()
  "Returns empty string when there are no focused / touched / outcome agents."
  (beads-agent-display-test--with-issue-agents nil nil nil
    (should (equal (beads-agent-display-format-issue-agents "bd-x") ""))))

(ert-deftest beads-agent-display-test-format-issue-agents-one-focused ()
  "One focused Task agent renders as the role glyph (letter mode = `T')."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil))
    (cl-letf (((symbol-function 'beads-agent-session-type-name)
               (lambda (_s) "Task"))
              ((symbol-function 'beads-agent-session-instance-number)
               (lambda (_s) 1)))
      (beads-agent-display-test--with-issue-agents '(sess1) nil nil
        (let ((result (beads-agent-display-format-issue-agents "bd-x")))
          (should (string= (substring-no-properties result) "T"))
          (should (string-match-p "1 focused agent, 0 touched"
                                  (get-text-property 0 'help-echo result))))))))

(ert-deftest beads-agent-display-test-format-issue-agents-multiple-focused ()
  "Multiple focused agents render joined by a space separator, no `#N'."
  (beads-agent-display-test--ensure-task-registered)
  (require 'beads-agent-types)
  (unless (beads-agent-type-get "review")
    (beads-agent-type-register (beads-agent-type-review)))
  (let* ((beads-agent-display-use-icons nil)
         (beads-agent-display-type-icons nil)
         (types '((s1 . "Task") (s2 . "Review"))))
    (cl-letf (((symbol-function 'beads-agent-session-type-name)
               (lambda (s) (cdr (assoc s types))))
              ((symbol-function 'beads-agent-session-instance-number)
               (lambda (_s) 1)))
      (beads-agent-display-test--with-issue-agents '(s1 s2) nil nil
        (let* ((result (beads-agent-display-format-issue-agents "bd-x"))
               (plain (substring-no-properties result)))
          (should (string= plain "T R"))
          (should-not (string-match-p "#" plain)))))))

(ert-deftest beads-agent-display-test-format-issue-agents-finished-outcome ()
  "Finished outcome with no live sessions renders `✓T' (letter mode)."
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil))
    (beads-agent-display-test--with-issue-agents nil nil '("Task" . finished)
      (let ((result (beads-agent-display-format-issue-agents "bd-x")))
        (should (string= (substring-no-properties result) "✓T"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-finished))))))

(ert-deftest beads-agent-display-test-format-issue-agents-failed-outcome ()
  "Failed outcome with no live sessions renders `✗R' (letter mode)."
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil))
    (beads-agent-display-test--with-issue-agents nil nil '("Review" . failed)
      (let ((result (beads-agent-display-format-issue-agents "bd-x")))
        (should (string= (substring-no-properties result) "✗R"))
        (should (eq (get-text-property 0 'face result)
                    'beads-list-agent-failed))))))

(ert-deftest beads-agent-display-test-format-issue-agents-legacy-sessions ()
  "Legacy (unsegmented) sessions render when no focused/touched sessions exist.

Covers the fallback branch in `beads-agent-display-format-issue-agents'
that fires when both `beads-agent--get-sessions-focused-on-issue' and
`beads-agent--get-sessions-touching-issue' return nil but
`beads-agent--get-sessions-for-issue' returns a non-empty list.  This
preserves backward compatibility with backends that have not adopted
the focused/touched session segmentation."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil))
    (cl-letf (((symbol-function 'beads-agent-session-type-name)
               (lambda (_s) "Task"))
              ((symbol-function 'beads-agent-session-instance-number)
               (lambda (_s) 1)))
      (beads-agent-display-test--with-issue-agents nil nil nil
        (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
                   (lambda (_id) '(legacy-sess1 legacy-sess2))))
          (let* ((result (beads-agent-display-format-issue-agents "bd-x"))
                 (plain (substring-no-properties result)))
            (should (string= plain "T T"))
            (should-not (string-match-p "#" plain))
            (should (string-match-p "2 agents working"
                                    (get-text-property 0 'help-echo result)))))))))

(ert-deftest beads-agent-display-test-format-issue-agents-legacy-singular ()
  "Legacy-sessions branch uses singular `agent working' for a single session."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil))
    (cl-letf (((symbol-function 'beads-agent-session-type-name)
               (lambda (_s) "Task"))
              ((symbol-function 'beads-agent-session-instance-number)
               (lambda (_s) 1)))
      (beads-agent-display-test--with-issue-agents nil nil nil
        (cl-letf (((symbol-function 'beads-agent--get-sessions-for-issue)
                   (lambda (_id) '(legacy-sess1))))
          (let ((result (beads-agent-display-format-issue-agents "bd-x")))
            (should (string= (substring-no-properties result) "T"))
            (should (string-match-p "1 agent working"
                                    (get-text-property 0 'help-echo result)))
            ;; Make sure plural form does not leak.
            (should-not (string-match-p "agents working"
                                        (get-text-property 0 'help-echo result)))))))))

(ert-deftest beads-agent-display-test-format-issue-agents-focused-wins-outcome ()
  "When both focused sessions and an outcome exist, focused agents win.
The outcome cell is reserved for terminated sessions; an active focused
session means the issue is still in flight."
  (beads-agent-display-test--ensure-task-registered)
  (let ((beads-agent-display-use-icons nil)
        (beads-agent-display-type-icons nil))
    (cl-letf (((symbol-function 'beads-agent-session-type-name)
               (lambda (_s) "Task"))
              ((symbol-function 'beads-agent-session-instance-number)
               (lambda (_s) 1)))
      (beads-agent-display-test--with-issue-agents
          '(sess1) nil '("Task" . finished)
        (let ((result (beads-agent-display-format-issue-agents "bd-x")))
          (should (string= (substring-no-properties result) "T"))
          (should-not (string-match-p "✓" result)))))))

(provide 'beads-agent-display-test)

;;; beads-agent-display-test.el ends here
