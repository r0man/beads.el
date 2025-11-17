;;; beads-misc.el --- Transient menus for misc bd commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides transient menu interfaces for miscellaneous Beads commands:
;; - bd close: Close issues with reason
;; - bd dep: Dependency management (add, remove, tree, list)
;; - bd stats: Project statistics
;; - bd init: Initialize beads project
;;
;; All menus follow the patterns established in beads-create.el and
;; beads-update.el.  They provide context-aware issue detection,
;; validation, and integration with beads-list and beads-show buffers.

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-option)
(require 'transient)

;;; Forward declarations
(declare-function beads-list--current-issue-id "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-show--issue-id "beads-show")
(declare-function beads-refresh-show "beads-show")

;;; ============================================================
;;; bd init
;;; ============================================================

;;; Utility Functions

(defun beads-init--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-init--parse-transient-args (args)
  "Parse transient ARGS and return plist with init parameters.
Returns (:prefix PREFIX :db-path DB-PATH)."
  (list :prefix (transient-arg-value "--prefix=" args)
        :db-path (transient-arg-value "--db=" args)))

;;; Suffix Commands

(defun beads-init--execute (prefix db-path)
  "Execute bd init with PREFIX and DB-PATH."
  (condition-case err
      (let (args)
        ;; Build args in reverse order for push/nreverse
        (when db-path
          (let ((trimmed (string-trim db-path)))
            (unless (string-empty-p trimmed)
              (push trimmed args)
              (push "--db" args))))
        (when prefix
          (let ((trimmed (string-trim prefix)))
            (unless (string-empty-p trimmed)
              (push trimmed args)
              (push "--prefix" args))))
        (push "init" args)
        (setq args (nreverse args))
        (with-temp-buffer
          (let ((exit-code (apply #'call-process
                                  beads-executable nil t nil args)))
            (unless (zerop exit-code)
              (error "Init failed: %s" (buffer-string)))))
        (message "Beads project initialized%s"
                 (if prefix (format " with prefix '%s'" prefix) ""))
        nil)
    (error
     (beads--error "Failed to initialize: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-init--execute-command ()
  "Execute the bd init command."
  :key "i"
  :description "Initialize project"
  (interactive)
  (when (y-or-n-p "Initialize Beads project in current directory? ")
    (let* ((args (transient-args 'beads-init--menu))
           (params (beads-init--parse-transient-args args))
           (prefix (plist-get params :prefix))
           (db-path (plist-get params :db-path)))
      (beads-init--execute prefix db-path))))

(transient-define-suffix beads-init--reset ()
  "Reset all init parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "Fields reset")))

;;; Main Transient Menu

(transient-define-prefix beads-init--menu ()
  "Transient menu for initializing a new Beads project."
  :value (lambda () nil)
  ["Initialization Parameters"
   (beads-option-init-prefix)
   (beads-option-init-db)]
  ["Actions"
   ("i" "Initialize" beads-init--execute-command)
   ("R" "Reset fields" beads-init--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-init ()
  "Initialize a new Beads project.

This command provides an interface for initializing a new
Beads project in the current directory.  It allows setting the
issue ID prefix and database path."
  (interactive)
  (beads-check-executable)
  (call-interactively #'beads-init--menu))

;;; Footer

(provide 'beads-misc)
;;; beads-misc.el ends here
