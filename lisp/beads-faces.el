;;; beads-faces.el --- Canonical face names for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Beads Contributors
;; Keywords: tools, faces

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Canonical face set for beads.el and its downstream extensions
;; (design.md §4.9).  Extensions do not register faces through a hook;
;; they derive their own faces with `:inherit', e.g.
;;
;;   (defface my-rig-face '((t (:inherit beads-face-header)))
;;     "Face for rig headers." :group 'my-package)
;;
;; so the exact symbol names below are part of the documented extension
;; ABI.  This module defines the palette only; migrating existing
;; rendering to these faces is the faces/consistency work item (WI-17).

;;; Code:

(defgroup beads-faces nil
  "Faces used by beads.el."
  :group 'beads
  :prefix "beads-face-")

(defface beads-face-header
  '((t (:inherit bold)))
  "Face for buffer and section titles."
  :group 'beads-faces)

(defface beads-face-section
  '((t (:inherit bold)))
  "Face for section header lines."
  :group 'beads-faces)

(defface beads-face-issue-line
  '((t (:inherit default)))
  "Face for issue rows in list, section and dashboard buffers."
  :group 'beads-faces)

(defface beads-face-id
  '((t (:inherit fixed-pitch)))
  "Face for bead ids."
  :group 'beads-faces)

(defface beads-face-key
  '((t (:inherit shadow)))
  "Face for key hints in footers and menus."
  :group 'beads-faces)

(defface beads-face-status-open
  '((t (:inherit success)))
  "Face for the open status glyph."
  :group 'beads-faces)

(defface beads-face-status-in-progress
  '((t (:inherit warning)))
  "Face for the in-progress status glyph."
  :group 'beads-faces)

(defface beads-face-status-blocked
  '((t (:inherit error)))
  "Face for the blocked status glyph."
  :group 'beads-faces)

(defface beads-face-status-closed
  '((t (:inherit shadow)))
  "Face for the closed status glyph."
  :group 'beads-faces)

(defface beads-face-priority-critical
  '((t (:inherit error)))
  "Face for critical (P0) priority."
  :group 'beads-faces)

(defface beads-face-priority-high
  '((t (:inherit warning)))
  "Face for high (P1) priority."
  :group 'beads-faces)

(defface beads-face-priority-medium
  '((t (:inherit default)))
  "Face for medium (P2) priority."
  :group 'beads-faces)

(defface beads-face-priority-low
  '((t (:inherit shadow)))
  "Face for low (P3/P4) priority."
  :group 'beads-faces)

(defface beads-face-agent-running
  '((t (:inherit success)))
  "Face for running agent state glyphs."
  :group 'beads-faces)

(defface beads-face-agent-idle
  '((t (:inherit shadow)))
  "Face for idle agent state glyphs."
  :group 'beads-faces)

(defface beads-face-agent-failed
  '((t (:inherit error)))
  "Face for failed agent state glyphs."
  :group 'beads-faces)

(defface beads-face-success
  '((t (:inherit success)))
  "Face for success footers and validation results."
  :group 'beads-faces)

(defface beads-face-warning
  '((t (:inherit warning)))
  "Face for warning footers and validation results."
  :group 'beads-faces)

(defface beads-face-error
  '((t (:inherit error)))
  "Face for error footers and validation results."
  :group 'beads-faces)

(provide 'beads-faces)
;;; beads-faces.el ends here