;;; beads-misc.el --- Transient menus for misc bd commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides transient menu interfaces for miscellaneous Beads commands:
;; - bd close: Close issues with reason
;; - bd dep: Dependency management (add, remove, tree, list)
;; - bd stats: Project statistics
;; - bd import: Import from JSONL
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
;;; bd import
;;; ============================================================

;;; Utility Functions

(defun beads-import--format-value (value)
  "Format VALUE for display in transient menu."
  (if value
      (propertize (format " [%s]" value) 'face 'transient-value)
    (propertize " [unset]" 'face 'transient-inactive-value)))

(defun beads-import--get-default-input ()
  "Get default input path (.beads/issues.jsonl)."
  (when-let* ((beads-dir (beads--find-beads-dir)))
    (expand-file-name "issues.jsonl" beads-dir)))

(defun beads-import--parse-transient-args (args)
  "Parse transient ARGS and return plist with import parameters.
Returns (:input INPUT :dry-run DRY-RUN :resolve-collisions RESOLVE-COLLISIONS)."
  (list :input (transient-arg-value "--input=" args)
        :dry-run (transient-arg-value "--dry-run" args)
        :resolve-collisions (transient-arg-value "--resolve-collisions" args)))

(defun beads-import--validate-input (input)
  "Validate that INPUT path is set.
Returns error message string if invalid, nil if valid."
  (when (or (null input)
            (string-empty-p (string-trim input)))
    "Input file path is required"))

(defun beads-import--validate-all (input)
  "Validate all parameters with INPUT.
Returns list of error messages, or nil if all valid."
  (delq nil (list (beads-import--validate-input input))))

;;; Suffix Commands

(defun beads-import--execute (input-path dry-run resolve-collisions)
  "Execute bd import from INPUT-PATH with flags.
DRY-RUN and RESOLVE-COLLISIONS control behavior."
  (condition-case err
      (let (args)
        ;; Build args in reverse order for push/nreverse
        (when resolve-collisions
          (push "--resolve-collisions" args))
        (when dry-run
          (push "--dry-run" args))
        (push input-path args)
        (push "-i" args)
        (push "import" args)
        (setq args (nreverse args))
        (let ((output (with-temp-buffer
                        (let ((exit-code
                               (apply #'call-process
                                      beads-executable nil t nil args)))
                          (unless (zerop exit-code)
                            (error "Import failed: %s" (buffer-string)))
                          (buffer-string)))))
          ;; Display output in buffer if dry-run or there are collisions
          (when (or dry-run
                    (string-match-p "collision" output))
            (let ((buf (get-buffer-create "*beads-import*")))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert output)
                  (goto-char (point-min))
                  (special-mode)
                  (local-set-key (kbd "q") 'quit-window)))
              (display-buffer buf)))
          (if dry-run
              (message "Dry run completed (see *beads-import* buffer)")
            (message "Import completed from: %s" input-path))
          nil))
    (error
     (beads--error "Failed to import: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-import--execute-command ()
  "Execute the bd import command."
  :key "i"
  :description "Import from JSONL"
  (interactive)
  (let* ((args (transient-args 'beads-import--menu))
         (params (beads-import--parse-transient-args args))
         (input (plist-get params :input))
         (dry-run (plist-get params :dry-run))
         (resolve-collisions (plist-get params :resolve-collisions))
         (errors (beads-import--validate-all input)))
    (if errors
        (user-error "Validation failed: %s" (string-join errors "; "))
      (beads-import--execute input dry-run resolve-collisions))))

(transient-define-suffix beads-import--reset ()
  "Reset all import parameters."
  :key "R"
  :description "Reset fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "Fields reset")))

;;; Main Transient Menu

(transient-define-prefix beads-import--menu ()
  "Transient menu for importing issues from JSONL."
  :value (lambda () nil)
  ["Import Parameters"
   (beads-option-import-input)]
  ["Options"
   (beads-option-import-dry-run)
   (beads-option-import-resolve-collisions)]
  ["Actions"
   ("i" "Import" beads-import--execute-command)
   ("R" "Reset fields" beads-import--reset)
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun beads-import ()
  "Import issues from JSONL.

This command provides an interface for importing issues from
JSONL format.  It supports dry-run mode for previewing changes and
automatic collision resolution for branch merges."
  (interactive)
  (beads-check-executable)
  (call-interactively #'beads-import--menu))

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
