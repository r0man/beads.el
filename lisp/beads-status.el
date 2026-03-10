;;; beads-status.el --- Status buffer with section hooks -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides the main beads status buffer, modelled after
;; `magit-status'.  It renders a project overview using collapsible
;; magit-section groups populated by `beads-status-sections-hook'.
;;
;; ## Entry Point
;;
;; - `beads-status' — open (or refresh) the status buffer for the
;;   current project.
;;
;; ## Mode
;;
;; `beads-status-mode' is derived from `beads-section-mode' and adds:
;;   g   — `beads-status-refresh' (re-run section hooks)
;;   q   — `quit-window'
;;   n/p — inherited navigation from `magit-section-mode'
;;   RET — inherited visit from `beads-section-mode' / `beads-section-issue-map'
;;
;; ## Buffer Layout
;;
;; The buffer always starts with a header line produced by
;; `beads-status--format-header'.  Below the header the functions in
;; `beads-status-sections-hook' insert collapsible sections via the
;; magit-section API.
;;
;; ## Auto-Refresh
;;
;; `revert-buffer-function' is set to `beads-status--revert' so that
;; `M-x revert-buffer' (or any package that calls it) re-renders the
;; buffer without prompting.
;;
;; ## Dependencies
;;
;; Requires `beads-section' (which in turn requires `magit-section').

;;; Code:

(require 'beads)
(require 'beads-section)

;;; Variables

(defvar beads-status--buffer-name "*beads-status*"
  "Default buffer name for beads status buffers.
When a project root is known a project-specific name is used instead.")

;;; Keymap

(defvar beads-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map beads-section-mode-map)
    (define-key map (kbd "g") #'beads-status-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `beads-status-mode'.
Inherits from `beads-section-mode-map'.  Adds:
  g — `beads-status-refresh'
  q — `quit-window'")

;;; Mode

(define-derived-mode beads-status-mode beads-section-mode "Beads Status"
  "Major mode for the beads status buffer.

Displays a project overview as collapsible magit-section groups.
Sections are populated by running `beads-status-sections-hook'.

Key bindings:
  g   — Refresh (re-run section hooks)
  q   — Quit window
  TAB — Toggle section visibility (inherited)
  n/p — Move to next / previous section (inherited)
  RET — Visit issue at point (inherited)"
  :interactive nil
  (setq-local revert-buffer-function #'beads-status--revert))

;;; Header

(defun beads-status--format-header ()
  "Return a propertized header string for the beads status buffer.

Shows the project name derived from the git root and the database
path.  Falls back gracefully when no project or database is found."
  (let* ((root (beads-git-find-project-root))
         (db   (beads--get-database-path))
         (project-name
          (if root
              (file-name-nondirectory (directory-file-name root))
            "unknown")))
    (concat
     (propertize "Beads" 'face 'bold)
     (propertize " \u2014 " 'face 'shadow)          ; em dash
     (propertize project-name 'face 'font-lock-constant-face)
     (propertize " (" 'face 'shadow)
     (propertize (or db "no database") 'face 'font-lock-string-face)
     (propertize ")" 'face 'shadow)
     "\n")))

;;; Refresh

(defun beads-status-refresh ()
  "Refresh the current beads status buffer.

Clears the buffer and re-runs `beads-status-sections-hook' to
re-populate it with up-to-date data."
  (interactive)
  (beads-status--refresh-buffer (current-buffer)))

(defun beads-status--revert (_ignore-auto _noconfirm)
  "Revert function for `beads-status-mode' buffers.
Called by `revert-buffer'; delegates to `beads-status-refresh'."
  (beads-status-refresh))

(defun beads-status--refresh-buffer (buffer)
  "Refresh beads status BUFFER by re-running all section hooks.

Erases the buffer, inserts the header, then calls each function in
`beads-status-sections-hook' inside a root magit-section so that
magit-section navigation and visibility toggling work correctly."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      ;; Header
      (insert (beads-status--format-header))
      (insert "\n")
      ;; Sections
      (magit-insert-section (magit-section)
        (run-hooks 'beads-status-sections-hook))
      ;; Restore point, clamped to valid range
      (goto-char (min pos (point-max))))))

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
