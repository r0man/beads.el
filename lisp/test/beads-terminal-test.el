;;; beads-terminal-test.el --- Tests for beads-terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Phase 1b (bde-xle9.3) acceptance tests for the `beads-terminal'
;; subsystem.  No test here spawns a real vterm/eat/ghostel, touches
;; bd, creates a .beads/ repo, or uses sleep/sit-for; the buffer-name
;; -ownership invariant uses `beads-terminal-fake' (spawns `cat'),
;; with the built-in `term'/`ansi-term' run for real (trivial `cat').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-terminal)
(require 'beads-test-helpers)
(require 'beads-meta)

;;; Registry isolation idiom

(ert-deftest beads-terminal-test-temp-registry-isolates ()
  "`beads-test-with-temp-registry' empties and restores the registry."
  (beads-terminal-register-builtin)
  (let ((outer (beads-terminal-list)))
    (should outer)
    (beads-test-with-temp-registry
      (should (null (beads-terminal-list)))
      (beads-terminal-register (beads-terminal-vterm))
      (should (= 1 (length (beads-terminal-list)))))
    ;; Restored.
    (should (equal (mapcar (lambda (x) (oref x name)) outer)
                   (mapcar (lambda (x) (oref x name))
                           (beads-terminal-list))))))

(ert-deftest beads-terminal-test-temp-registry-reentrant ()
  "Nested `beads-test-with-temp-registry' restores correctly.
Reentrancy must hold for hermeticity under `ert-randomize-tests'."
  (beads-terminal-register-builtin)
  (let ((baseline (length (beads-terminal-list))))
    (should (> baseline 0))
    (beads-test-with-temp-registry
      (beads-terminal-register (beads-terminal-term))
      (should (= 1 (length (beads-terminal-list))))
      (beads-test-with-temp-registry
        (should (null (beads-terminal-list)))
        (beads-terminal-register (beads-terminal-eat))
        (beads-terminal-register (beads-terminal-vterm))
        (should (= 2 (length (beads-terminal-list)))))
      ;; Inner restored the outer temp registry, not the baseline.
      (should (= 1 (length (beads-terminal-list))))
      (should (equal "term" (oref (car (beads-terminal-list)) name))))
    (should (= baseline (length (beads-terminal-list))))))

;;; Priorities

(ert-deftest beads-terminal-test-distinct-priorities ()
  "Built-in concretes have the distinct priorities 10/15/20/40/50.
`auto' is priority 0 and sorts first."
  (beads-test-with-temp-registry
    (beads-terminal-register-builtin)
    (let ((by-name (lambda (n) (oref (beads-terminal-get n) priority))))
      (should (= 0  (funcall by-name "auto")))
      (should (= 10 (funcall by-name "vterm")))
      (should (= 15 (funcall by-name "ghostel")))
      (should (= 20 (funcall by-name "eat")))
      (should (= 40 (funcall by-name "ansi-term")))
      (should (= 50 (funcall by-name "term")))
      ;; List is priority-sorted ascending.
      (let ((prios (mapcar (lambda (x) (oref x priority))
                           (beads-terminal-list))))
        (should (equal prios (sort (copy-sequence prios) #'<)))))))

;;; Abstract / slot typing

(ert-deftest beads-terminal-test-abstract-cannot-instantiate ()
  "The abstract base class constructor signals an error.
\(EIEIO enforces `:abstract' in the generated constructor, matching
the project's existing abstract-class test pattern.)"
  (should-error (beads-terminal) :type 'error))

(ert-deftest beads-terminal-test-classes-have-documentation ()
  "Every terminal class and base slot carries non-empty :documentation.
Slot docs are read through the project's `beads-meta' accessor (raw
EIEIO drops custom slot props per CLAUDE.md)."
  (dolist (cls '(beads-terminal beads-terminal-vterm beads-terminal-ghostel
                                beads-terminal-eat beads-terminal-ansi-term
                                beads-terminal-term beads-terminal-auto))
    (when (fboundp 'cl--class-docstring)
      (let ((cdoc (cl--class-docstring (cl--find-class cls))))
        (should (and cdoc (not (string-empty-p cdoc)))))))
  ;; Slot docs on the base class via the beads-meta accessor.
  (dolist (slot (mapcar #'eieio-slot-descriptor-name
                        (eieio-class-slots (cl--find-class 'beads-terminal))))
    (let ((doc (beads-meta-slot-property 'beads-terminal slot :documentation)))
      (should (and doc (not (string-empty-p doc)))))))

;;; symbol->class bridge

(ert-deftest beads-terminal-test-symbol->class ()
  "`beads-terminal--symbol->class' maps the legacy symbol vocabulary."
  (should (eq 'beads-terminal-auto  (beads-terminal--symbol->class nil)))
  (should (eq 'beads-terminal-vterm (beads-terminal--symbol->class 'vterm)))
  (should (eq 'beads-terminal-eat   (beads-terminal--symbol->class 'eat)))
  (should (eq 'beads-terminal-term  (beads-terminal--symbol->class 'term)))
  (should-error (beads-terminal--symbol->class 'bogus) :type 'error))

;;; auto gating

(ert-deftest beads-terminal-test-auto-available-when-builtin-present ()
  "`auto' is available because built-in term is always available."
  (beads-test-with-temp-registry
    (beads-terminal-register-builtin)
    (should (beads-terminal-available-p (beads-terminal-auto)))))

(ert-deftest beads-terminal-test-auto-all-unavailable-errors ()
  "`auto' spawn signals a clear error when no concrete is available."
  (beads-test-with-temp-registry
    ;; Register only auto and a never-available stub concrete.
    (beads-terminal-register (beads-terminal-auto))
    (cl-letf (((symbol-function 'beads-terminal--first-available)
               (lambda () nil)))
      (should-not (beads-terminal-available-p (beads-terminal-auto)))
      (should-error
       (beads-terminal-spawn (beads-terminal-auto) "*x*" '("cat") "/tmp" nil)
       :type 'error))))

(ert-deftest beads-terminal-test-auto-skips-unavailable-picks-lower ()
  "`auto' skips a registered-but-unavailable class for a lower one.
vterm (10) is stubbed unavailable; the fake (99) is available, so
`first-available' returns the fake."
  (beads-test-with-temp-registry
    (beads-terminal-register (beads-terminal-auto))
    (beads-terminal-register (beads-terminal-vterm))
    (beads-terminal-register (beads-terminal-fake))
    (cl-letf (((symbol-function 'beads-terminal-available-p)
               (lambda (term)
                 (cond
                  ((cl-typep term 'beads-terminal-vterm) nil)
                  ((cl-typep term 'beads-terminal-fake) t)
                  ((cl-typep term 'beads-terminal-auto)
                   (and (beads-terminal--first-available) t))
                  (t nil)))))
      (let ((picked (beads-terminal--first-available)))
        (should (cl-typep picked 'beads-terminal-fake))))))

;;; Buffer-name ownership — fake terminal (no real vterm/eat)

(ert-deftest beads-terminal-test-fake-owns-buffer-name ()
  "The fake terminal spawns into exactly the requested buffer name,
with a live process, and leaves no `<2>'/extra-`*' variant."
  (let* ((dir (make-temp-file "bde-term-" t))
         (name "*beads-agent[ownership]*")
         (process-environment process-environment)
         (default-directory dir)
         buf)
    (unwind-protect
        (progn
          (setq buf (beads-terminal-spawn (beads-terminal-fake)
                                          name '("cat") dir nil))
          (should (bufferp buf))
          (should (equal name (buffer-name buf)))
          (should (process-live-p (get-buffer-process buf)))
          ;; No collision-renamed variant got created.
          (should-not (get-buffer (concat name "<2>"))))
      (when (buffer-live-p buf)
        (when-let ((p (get-buffer-process buf))) (delete-process p))
        (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest beads-terminal-test-builtin-term-owns-name-vs-preexisting ()
  "Built-in `term' spawns the exact name even when it pre-exists.
Runs for real with a trivial `cat' (term is built in)."
  (let* ((dir (make-temp-file "bde-term-" t))
         (name "*beads-agent[builtinterm]*")
         (process-environment process-environment)
         (default-directory dir)
         (pre (get-buffer-create name))
         buf)
    (unwind-protect
        (progn
          (setq buf (beads-terminal-spawn (beads-terminal-term)
                                          name '("cat") dir nil))
          (should (equal name (buffer-name buf)))
          ;; Re-used the pre-existing buffer in place; no `<2>'.
          (should (eq buf pre))
          (should-not (get-buffer (concat name "<2>"))))
      (when (buffer-live-p buf)
        (when-let ((p (get-buffer-process buf))) (delete-process p))
        (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest beads-terminal-test-send-input-deferred ()
  "`beads-terminal-send-input' signals the documented deferral."
  (should-error
   (beads-terminal-send-input (beads-terminal-fake) (current-buffer) "x")
   :type 'error))

;;; ghostel strict availability

(ert-deftest beads-terminal-test-ghostel-unavailable-without-module ()
  "ghostel is unavailable and selecting it errors when the native
module / symbols are absent."
  (skip-unless (not (featurep 'ghostel)))
  (let ((g (beads-terminal-ghostel)))
    (should-not (beads-terminal-available-p g))
    (should-error
     (beads-terminal-spawn g "*x*" '("cat") "/tmp" nil)
     :type 'error)))

(ert-deftest beads-terminal-test-apply-env-drops-term ()
  "`beads-terminal--apply-env' adds entries but never sets TERM."
  (let ((env '(("FOO" . "bar") ("TERM" . "evil") ("BAZ" . "qux"))))
    (let ((pe (beads-terminal--apply-env env)))
      (should (member "FOO=bar" pe))
      (should (member "BAZ=qux" pe))
      (should-not (member "TERM=evil" pe)))))

(provide 'beads-terminal-test)
;;; beads-terminal-test.el ends here
