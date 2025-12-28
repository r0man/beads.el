;;; beads-quickstart.el --- Quickstart guide for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides interface for displaying the Beads quickstart guide.
;;
;; The bd quickstart command displays a beginner-friendly guide for
;; getting started with Beads.  This module provides an Emacs interface
;; for viewing this guide in a buffer.
;;
;; Usage:
;;   M-x beads-quickstart

;;; Code:

(require 'beads)
(require 'beads-command)

(defun beads-quickstart--execute ()
  "Execute bd quickstart and display in buffer."
  (condition-case err
      (let* ((cmd (beads-command-quickstart))
             (_ (beads-command-execute cmd))
             (output (oref cmd data))
             (buf (get-buffer-create "*beads-quickstart*")))
        ;; execute signals error on non-zero exit, so we don't need to check
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
            ;; Make the buffer more readable
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

;;; Footer

(provide 'beads-quickstart)
;;; beads-quickstart.el ends here
