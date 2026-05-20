;;; beads-agent-ralph-registry-test.el --- Tests for Ralph public controller registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: test

;;; Commentary:

;; ERT coverage for the public controller registry in
;; `beads-agent-ralph.el' (bde-deqx.2): the defvar
;; `beads-agent-ralph--controllers' and its accessors
;; `beads-agent-ralph-controllers' /
;; `beads-agent-ralph-controller-for-root' /
;; `beads-agent-ralph--register-controller' /
;; `beads-agent-ralph--unregister-controller'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-agent-ralph)

;;; Test fixtures

(defun beads-agent-ralph-registry-test--make-controller (root-id)
  "Build a minimal controller pinned to ROOT-ID."
  (beads-agent-ralph--controller
   :root-id root-id
   :root-kind 'issue
   :iteration 0
   :max-iterations 10
   :cumulative-cost-usd 0.0
   :started-at (current-time)
   :status 'idle))

(defmacro beads-agent-ralph-registry-test--with-clean-registry (&rest body)
  "Run BODY with the public registry reset to nil before and after."
  (declare (indent 0) (debug (body)))
  `(let ((beads-agent-ralph--controllers nil))
     (unwind-protect (progn ,@body)
       (setq beads-agent-ralph--controllers nil))))

;;; Tests

(ert-deftest beads-agent-ralph-registry-test-empty-by-default ()
  "An empty registry returns nil from the public accessors."
  (beads-agent-ralph-registry-test--with-clean-registry
    (should (null (beads-agent-ralph-controllers)))
    (should (null (beads-agent-ralph-controller-for-root "bde-42")))))

(ert-deftest beads-agent-ralph-registry-test-register-adds-controller ()
  "Registering a controller exposes it via the public accessors."
  (beads-agent-ralph-registry-test--with-clean-registry
    (let ((ctrl (beads-agent-ralph-registry-test--make-controller "bde-42")))
      (should (eq ctrl (beads-agent-ralph--register-controller ctrl)))
      (should (equal (list ctrl) (beads-agent-ralph-controllers)))
      (should (eq ctrl (beads-agent-ralph-controller-for-root "bde-42"))))))

(ert-deftest beads-agent-ralph-registry-test-register-is-idempotent ()
  "Registering the same root-id twice leaves only one entry, newest first."
  (beads-agent-ralph-registry-test--with-clean-registry
    (let ((c1 (beads-agent-ralph-registry-test--make-controller "bde-42"))
          (c2 (beads-agent-ralph-registry-test--make-controller "bde-42")))
      (beads-agent-ralph--register-controller c1)
      (beads-agent-ralph--register-controller c2)
      (let ((list (beads-agent-ralph-controllers)))
        (should (= 1 (length list)))
        (should (eq c2 (car list)))
        (should (eq c2 (beads-agent-ralph-controller-for-root "bde-42")))))))

(ert-deftest beads-agent-ralph-registry-test-register-orders-newest-first ()
  "Distinct root-ids land in the registry head-first."
  (beads-agent-ralph-registry-test--with-clean-registry
    (let ((c1 (beads-agent-ralph-registry-test--make-controller "bde-1"))
          (c2 (beads-agent-ralph-registry-test--make-controller "bde-2"))
          (c3 (beads-agent-ralph-registry-test--make-controller "bde-3")))
      (beads-agent-ralph--register-controller c1)
      (beads-agent-ralph--register-controller c2)
      (beads-agent-ralph--register-controller c3)
      (should (equal (list c3 c2 c1) (beads-agent-ralph-controllers))))))

(ert-deftest beads-agent-ralph-registry-test-unregister-removes-controller ()
  "Unregistering drops the controller from the public accessors."
  (beads-agent-ralph-registry-test--with-clean-registry
    (let ((ctrl (beads-agent-ralph-registry-test--make-controller "bde-42")))
      (beads-agent-ralph--register-controller ctrl)
      (should (eq ctrl (beads-agent-ralph--unregister-controller ctrl)))
      (should (null (beads-agent-ralph-controllers)))
      (should (null (beads-agent-ralph-controller-for-root "bde-42"))))))

(ert-deftest beads-agent-ralph-registry-test-unregister-missing-is-noop ()
  "Unregistering a controller that was never registered does not error."
  (beads-agent-ralph-registry-test--with-clean-registry
    (let ((ctrl (beads-agent-ralph-registry-test--make-controller "bde-42")))
      (should (eq ctrl (beads-agent-ralph--unregister-controller ctrl)))
      (should (null (beads-agent-ralph-controllers))))))

(ert-deftest beads-agent-ralph-registry-test-unregister-is-eq-not-root-id ()
  "Unregistering a stale OLD instance must not evict the live NEW one.
After a relaunch the registry holds the NEW controller for that root.
A late kill-buffer hook firing on the OLD dashboard buffer (which
still references the OLD controller) should be a no-op — evicting
the NEW one would break the live loop the user just launched."
  (beads-agent-ralph-registry-test--with-clean-registry
    (let ((old (beads-agent-ralph-registry-test--make-controller "bde-42"))
          (new (beads-agent-ralph-registry-test--make-controller "bde-42")))
      (beads-agent-ralph--register-controller old)
      (beads-agent-ralph--register-controller new)
      (should (eq new (beads-agent-ralph-controller-for-root "bde-42")))
      (beads-agent-ralph--unregister-controller old)
      (should (eq new (beads-agent-ralph-controller-for-root "bde-42")))
      (should (equal (list new) (beads-agent-ralph-controllers))))))

(ert-deftest beads-agent-ralph-registry-test-controllers-returns-copy ()
  "The public accessor returns a fresh copy; mutating it does not leak."
  (beads-agent-ralph-registry-test--with-clean-registry
    (let ((ctrl (beads-agent-ralph-registry-test--make-controller "bde-42")))
      (beads-agent-ralph--register-controller ctrl)
      (let ((view (beads-agent-ralph-controllers)))
        (setq view nil))
      (should (equal (list ctrl) (beads-agent-ralph-controllers))))))

(ert-deftest beads-agent-ralph-registry-test-lookup-by-root-id ()
  "`beads-agent-ralph-controller-for-root' finds the entry by root-id."
  (beads-agent-ralph-registry-test--with-clean-registry
    (let ((c-a (beads-agent-ralph-registry-test--make-controller "bde-aaa"))
          (c-b (beads-agent-ralph-registry-test--make-controller "bde-bbb")))
      (beads-agent-ralph--register-controller c-a)
      (beads-agent-ralph--register-controller c-b)
      (should (eq c-a (beads-agent-ralph-controller-for-root "bde-aaa")))
      (should (eq c-b (beads-agent-ralph-controller-for-root "bde-bbb")))
      (should (null (beads-agent-ralph-controller-for-root "bde-zzz"))))))

(ert-deftest beads-agent-ralph-registry-test-state-changes-preserve-order ()
  "Mode-line state-change handling must not reshuffle the public registry.
The public registry is ordered most-recently-*started* first; only
`beads-agent-ralph-start' (and explicit register/unregister) should
mutate it.  In particular, the mode-line subscriber reacting to
status transitions on an existing controller must not move it back
to the head."
  (require 'beads-agent-ralph-mode-line)
  (beads-agent-ralph-registry-test--with-clean-registry
    (let ((c-old (beads-agent-ralph-registry-test--make-controller "bde-old"))
          (c-new (beads-agent-ralph-registry-test--make-controller "bde-new")))
      (beads-agent-ralph--register-controller c-old)
      (beads-agent-ralph--register-controller c-new)
      (should (equal (list c-new c-old) (beads-agent-ralph-controllers)))
      ;; Drive the older controller through every status the mode-line
      ;; subscribes to: it must stay at the tail of the public registry.
      (unwind-protect
          (dolist (status '(running cooling-down auto-paused
                                    running done))
            (beads-agent-ralph--mode-line-on-state-change c-old status)
            (should (equal (list c-new c-old)
                           (beads-agent-ralph-controllers))))
        (beads-agent-ralph--mode-line-reset)))))

(provide 'beads-agent-ralph-registry-test)

;;; beads-agent-ralph-registry-test.el ends here
