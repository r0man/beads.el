;;; beads-status-test.el --- Tests for the beads-status compat shim -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for the `beads-status' compat shim.  The dashboard
;; itself is covered by `beads-dashboard-test.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-status)

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
  (should (equal (get 'beads-status 'byte-obsolete-info)
                 '(beads-dashboard nil "1.x"))))

(ert-deftest beads-status-test-section-hook-still-defined ()
  "`beads-status-sections-hook' is preserved as a public extension point."
  :tags '(:unit)
  (should (boundp 'beads-status-sections-hook)))

(provide 'beads-status-test)
;;; beads-status-test.el ends here
