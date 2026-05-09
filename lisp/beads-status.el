;;; beads-status.el --- Compat shim for beads-dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `beads-status' forwards to `beads-dashboard'.  The
;; `beads-status-sections-hook' extension point is preserved for
;; downstream packages.

;;; Code:

(require 'beads-section)
;; `beads-defcommand beads-command-status' auto-generates a `beads-status'
;; transient that would clobber our forwarder.  Loading it first lets the
;; `defun' below win the symbol slot.
(require 'beads-command-status)

(declare-function beads-dashboard "beads-dashboard")

;;;###autoload
(defun beads-status ()
  "Open the beads dashboard (deprecated alias for `beads-dashboard')."
  (interactive)
  (require 'beads-dashboard)
  (beads-dashboard))

(make-obsolete 'beads-status 'beads-dashboard "1.x")

(provide 'beads-status)
;;; beads-status.el ends here
