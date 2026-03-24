;;; beads-status.el --- Status buffer using vui.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides the main beads status buffer, modelled after
;; `magit-status'.  It renders a project overview using collapsible
;; vui.el components populated by `beads-status-sections-hook'.
;;
;; ## Entry Point
;;
;; - `beads-status' — open (or refresh) the status buffer for the
;;   current project.
;;
;; ## Mode
;;
;; `beads-status-mode' is derived from `beads-section-mode' and adds:
;;   g   — `beads-status-refresh' (re-mount with fresh data)
;;   q   — `quit-window'
;;   TAB — move to next widget (inherited from vui-mode)
;;   RET — visit issue at point (inherited from beads-section-mode)
;;
;; ## Buffer Layout
;;
;; A header vnode (project name + db path) is rendered above the
;; section vnodes produced by `beads-status-sections-hook'.
;;
;; ## Root Component
;;
;; `beads-status--root' is a `vui-defcomponent' that assembles the
;; header and section tree into one mounted component tree.  Calling
;; `beads-status--refresh-buffer' remounts this component so vui's
;; reconciler handles re-rendering efficiently.
;;
;; ## Auto-Refresh
;;
;; `revert-buffer-function' is set to `beads-status--revert' so that
;; `M-x revert-buffer' (or any package that calls it) re-renders the
;; buffer without prompting.
;;
;; ## Dependencies
;;
;; Requires `beads-section' (which in turn requires `vui').

;;; Code:

(require 'beads)
(require 'beads-section)

;;; Variables

(defvar beads-status--buffer-name "*beads-status*"
  "Default buffer name for beads status buffers.
When a project root is known a project-specific name is used instead.")

;;; Keymap

(defvar-keymap beads-status-mode-map
  :parent beads-section-mode-map
  "g" #'beads-status-refresh
  "q" #'quit-window)

;;; Mode

(define-derived-mode beads-status-mode beads-section-mode "Beads Status"
  "Major mode for the beads status buffer.

Displays a project overview as collapsible vui.el sections.
Sections are populated by running `beads-status-sections-hook'.

Key bindings:
  g   — Refresh (remount with fresh data)
  q   — Quit window
  TAB — Move to next widget (inherited)
  RET — Visit issue at point (inherited)"
  :interactive nil
  (setq-local revert-buffer-function #'beads-status--revert))

;;; Header Vnode

(defun beads-status--header-vnode ()
  "Return a vui vnode for the beads status buffer header.

Shows the project name derived from the git root and the database
path.  For Dolt server mode the path points to the dolt directory.
Falls back gracefully when no project or database is found."
  (let* ((root (beads-git-find-project-root))
         (db   (beads--get-database-path))
         (project-name
          (if root
              (file-name-nondirectory (directory-file-name root))
            "unknown"))
         (db-display (or db "not found")))
    (vui-hstack :spacing 0
      (vui-text "Beads" :face 'bold)
      (vui-text " \u2014 " :face 'shadow)
      (vui-text project-name :face 'font-lock-constant-face)
      (vui-text " (" :face 'shadow)
      (vui-text db-display :face 'font-lock-string-face)
      (vui-text ")" :face 'shadow))))

;;; Root Component

(vui-defcomponent beads-status--root ()
  "Root vui component for the beads status buffer.
Renders the header and all section vnodes from
`beads-status-sections-hook' into a single component tree."
  :render
  (vui-vstack :spacing 1
    (beads-status--header-vnode)
    (beads-section-build-vnode)))

;;; Refresh

(defun beads-status-refresh ()
  "Refresh the current beads status buffer.

Remounts the root component to re-fetch data and re-render."
  (interactive)
  (beads-status--refresh-buffer (current-buffer)))

(defun beads-status--revert (_ignore-auto _noconfirm)
  "Revert function for `beads-status-mode' buffers.
Called by `revert-buffer'; delegates to `beads-status-refresh'."
  (beads-status-refresh))

(defun beads-status--refresh-buffer (buffer)
  "Refresh beads status BUFFER by remounting the root vui component.

Calls `vui-mount' with `beads-status--root' which re-fetches data
from `beads-status-sections-hook' and reconciles the buffer."
  (with-current-buffer buffer
    (vui-mount
     (vui-component 'beads-status--root)
     (buffer-name))))

;;; Entry Point

;;;###autoload
(defun beads-status ()
  "Open the beads status buffer for the current project.

Reuses an existing buffer when one already exists for the project.
The buffer name is project-specific when a git root is found, or
falls back to `beads-status--buffer-name'."
  (interactive)
  (let* ((root (beads-git-find-project-root))
         (buf-name
          (if root
              (format "*beads-status<%s>*"
                      (file-name-nondirectory
                       (directory-file-name root)))
            beads-status--buffer-name))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'beads-status-mode)
        (beads-status-mode)))
    (beads-status--refresh-buffer buf)
    (pop-to-buffer buf)))

(provide 'beads-status)
;;; beads-status.el ends here
