;;; beads-test-helpers.el --- Shared test fixtures -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; Shared fixtures for the terminal subsystem tests (Phase 1b,
;; bde-xle9.3):
;;
;; - `beads-test-with-temp-registry': isolates the
;;   `beads-terminal--registry' for the duration of BODY using the
;;   proven save-pointer / clear / restore idiom from
;;   beads-agent-type-test.el (the production code reallocates a
;;   fresh table lazily, so saving and restoring the defvar pointer
;;   is reentrant by construction — a shallow `copy-hash-table' would
;;   not survive in-place instance mutation, and a non-nesting
;;   counter cascades failures under `ert-randomize-tests').  Tests
;;   MUST register fresh instances, never `oset' a registered object.
;;
;; - `beads-terminal-fake': a real `beads-terminal' that spawns `cat'
;;   (a long-lived stdin reader) into a `get-buffer-create' buffer,
;;   so buffer-name-ownership invariants are exercised WITHOUT
;;   starting real vterm/eat/ghostel.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'beads-terminal)

;;; Registry isolation

(defmacro beads-test-with-temp-registry (&rest body)
  "Run BODY with an isolated, empty `beads-terminal--registry'.
Saves the current registry defvar value, clears it (production
reallocates lazily on next access), runs BODY, and restores the
saved value unconditionally.  Reentrant: nested uses each save and
restore their own snapshot, so this is safe under
`ert-randomize-tests' with no non-nesting guard."
  (declare (indent 0) (debug t))
  (let ((saved (gensym "saved-registry-"))
        (saved-builtin (gensym "saved-builtin-")))
    `(let ((,saved beads-terminal--registry)
           (,saved-builtin beads-terminal--builtin-registered))
       (unwind-protect
           (progn
             (beads-terminal--clear-registry)
             (setq beads-terminal--builtin-registered nil)
             ,@body)
         (setq beads-terminal--registry ,saved
               beads-terminal--builtin-registered ,saved-builtin)))))

;;; Fake terminal (spawns `cat'; no real vterm/eat/ghostel)

(defclass beads-terminal-fake (beads-terminal)
  ((name :initform "fake")
   (priority :initform 99))
  :documentation "Test-only terminal: spawns `cat' into the named
buffer so name ownership and long-lived-process invariants can be
exercised without a real terminal package.")

(cl-defmethod beads-terminal-available-p ((_t beads-terminal-fake))
  "The fake terminal is available whenever `cat' is on PATH."
  (and (executable-find "cat") t))

(cl-defmethod beads-terminal-spawn ((_t beads-terminal-fake)
                                    buffer-name argv working-dir env)
  "Create BUFFER-NAME and run `cat' in it (ignoring ARGV).
`cat' with no args reads stdin forever, giving a live process and a
buffer that survives.  Name ownership is via the pre-named
`get-buffer-create' buffer.  ENV/ARGV/WORKING-DIR are accepted for
signature parity; WORKING-DIR binds `default-directory'."
  (ignore argv)
  (let* ((default-directory working-dir)
         (process-environment (beads-terminal--apply-env env))
         (buf (get-buffer-create buffer-name)))
    (beads-terminal--kill-stale-process buf)
    (let ((proc (start-process buffer-name buf "cat")))
      (set-process-query-on-exit-flag proc nil))
    buf))

(provide 'beads-test-helpers)
;;; beads-test-helpers.el ends here
