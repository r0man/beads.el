;;; beads-status-test.el --- Tests for the beads-status compat shim -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for the `beads-status' compat shim.  The dashboard
;; itself is covered by `beads-dashboard-test.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-status)
;; `beads-status' forwards via `(require 'beads-dashboard)' then
;; `(beads-dashboard)'.  Load `beads-dashboard' HERE so that, in the
;; forwards-to-dashboard test, the forwarder's internal `require' is a
;; no-op and the `cl-letf' stub survives.  Without this, suite-order
;; dependent: when nothing loaded `beads-dashboard' earlier, the
;; forwarder's `require' loads beads-dashboard.el whose `defun
;; beads-dashboard' clobbers the stub and the real (bd-shelling) command
;; runs instead — a non-deterministic failure.
(require 'beads-dashboard)

(ert-deftest beads-status-test-shim-fboundp ()
  "`beads-status' remains callable as a compat shim."
  :tags '(:unit)
  (should (fboundp 'beads-status))
  (should (commandp 'beads-status)))

(ert-deftest beads-status-test-shim-forwards-to-dashboard ()
  "`beads-status' forwards to `beads-dashboard'."
  :tags '(:unit)
  (let ((dashboard-called nil))
    (cl-letf (((symbol-function 'beads-dashboard)
               (lambda () (setq dashboard-called t))))
      (beads-status))
    (should dashboard-called)))

(ert-deftest beads-status-test-shim-marked-obsolete ()
  "`beads-status' is registered as obsolete in favour of `beads-dashboard'."
  :tags '(:unit)
  ;; When beads-status.elc is byte-compiled, `make-obsolete' can record
  ;; the target as a symbol-with-position (printed `#<symbol
  ;; beads-dashboard at N>') rather than the bare symbol.  Such symbols
  ;; are not `eq'/`equal' to the plain symbol unless
  ;; `symbols-with-pos-enabled' is bound — which is exactly the
  ;; canonical Emacs (29+) mechanism for comparing values that may
  ;; carry compiler position metadata.  This is a compilation artifact,
  ;; not a contract change, so normalise before asserting.
  (let ((symbols-with-pos-enabled t))
    (should (equal (get 'beads-status 'byte-obsolete-info)
                   '(beads-dashboard nil "1.x")))))

(ert-deftest beads-status-test-section-hook-still-defined ()
  "`beads-status-sections-hook' is preserved as a public extension point."
  :tags '(:unit)
  (should (boundp 'beads-status-sections-hook)))

(provide 'beads-status-test)
;;; beads-status-test.el ends here
