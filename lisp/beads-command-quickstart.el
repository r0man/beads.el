;;; beads-command-quickstart.el --- Quickstart command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-quickstart' EIEIO class for
;; the `bd quickstart' command.  The class includes full slot metadata
;; for automatic transient menu generation via
;; `beads-defcommand'.
;;
;; The bd quickstart command displays a quick start guide showing
;; common bd workflows and patterns.  It has no options.
;;
;; Usage:
;;   (beads-command-execute (beads-command-quickstart))
;;   (beads-execute 'beads-command-quickstart)  ; convenience function

;;; Code:

(require 'beads-util)
(require 'beads-buffer)
(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;;; Quickstart Command

(beads-defcommand beads-command-quickstart (beads-command-global-options)
  ()
  :documentation "Represents bd quickstart command.
Displays a quick start guide showing common bd workflows and patterns."
  :json nil
  :transient :manual)

;;; Transient Menu

;;;###autoload (autoload 'beads-quickstart-transient "beads-command-quickstart" nil t)
(beads-meta-define-transient beads-command-quickstart "beads-quickstart-transient"
  "Display a quick start guide for beads.

Shows common bd workflows and patterns to help you get started."
  beads-option-global-section)

;;; Interactive UI

(defun beads-quickstart--execute ()
  "Execute bd quickstart and display in buffer."
  (condition-case err
      (let* ((cmd (beads-command-quickstart))
             (output (beads-command-execute cmd))
             (buf-name (beads-buffer-name-utility "quickstart"))
             (buf (get-buffer-create buf-name)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert output)
            (goto-char (point-min))
            (special-mode)
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "g")
                           (lambda ()
                             (interactive)
                             (beads-quickstart--execute)))
            (visual-line-mode 1)
            (setq header-line-format
                  "Beads Quick Start Guide - Press 'q' to quit")))
        (display-buffer buf)
        (message "Beads quickstart guide")
        nil)
    (error
     (beads--error "Failed to get quickstart guide: %s"
                   (error-message-string err)))))

;;;###autoload
(defun beads-quickstart ()
  "Show Beads quickstart guide."
  (interactive)
  (beads-check-executable)
  (beads-quickstart--execute))

(provide 'beads-command-quickstart)
;;; beads-command-quickstart.el ends here
