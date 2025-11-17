;;; beads-import.el --- Import issues from JSONL -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; Provides interface for importing Beads issues from JSONL format.
;;
;; The bd import command imports issues from JSON Lines format
;; (one JSON object per line).  It supports:
;;   - Updating existing issues (by ID)
;;   - Creating new issues
;;   - Collision detection and reporting
;;   - Dry-run mode for previewing changes
;;   - Content duplicate detection
;;
;; NOTE: Import requires direct database access and automatically
;; uses --no-daemon.
;;
;; Usage:
;;   M-x beads-import

;;; Code:

(require 'beads)
(require 'beads-command)
(require 'beads-option)
(require 'transient)

;;; Utility Functions

(defun beads-import--get-default-input ()
  "Get default input path (.beads/issues.jsonl)."
  (when-let* ((beads-dir (beads--find-beads-dir)))
    (expand-file-name "issues.jsonl" beads-dir)))

(defun beads-import--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-import instance.
Returns a beads-command-import object populated with values from ARGS.

This uses transient's standard argument parsing with dash-style flags."
  (let* ((input (transient-arg-value "--input=" args))
         (dry-run (transient-arg-value "--dry-run" args))
         (skip-existing (transient-arg-value "--skip-existing" args))
         (clear-duplicate-external-refs
          (transient-arg-value "--clear-duplicate-external-refs" args))
         (dedupe-after (transient-arg-value "--dedupe-after" args))
         (rename-on-import (transient-arg-value "--rename-on-import" args))
         (strict (transient-arg-value "--strict" args))
         (orphan-handling (transient-arg-value "--orphan-handling=" args)))
    (beads-command-import
     :input input
     :dry-run dry-run
     :skip-existing skip-existing
     :clear-duplicate-external-refs clear-duplicate-external-refs
     :dedupe-after dedupe-after
     :rename-on-import rename-on-import
     :strict strict
     :orphan-handling orphan-handling)))

(defun beads-import--validate-input (input)
  "Validate that INPUT path is set.
Returns error message string if invalid, nil if valid."
  (when (or (null input)
            (string-empty-p (string-trim input)))
    "Input file path is required"))

(defun beads-import--validate-all (command)
  "Validate all parameters in COMMAND.
Returns list of error messages, or nil if all valid."
  (with-slots (input) command
    (delq nil (list (beads-import--validate-input input)
                    (beads-command-validate command)))))

;;; Suffix Commands

(defun beads-import--execute (command)
  "Execute bd import using COMMAND object.
Displays import output in *beads-import* buffer."
  (condition-case err
      (progn
        ;; Execute the import command (returns (EXIT-CODE STDOUT STDERR))
        (let* ((result (beads-command-execute command))
               (_exit-code (nth 0 result))  ; Unused, but part of result tuple
               (stdout (nth 1 result))
               (stderr (nth 2 result))
               ;; Combine stdout and stderr for display
               (output (concat (when (and stdout (not (string-empty-p stdout)))
                                 (concat stdout "\n"))
                               stderr)))
          (with-slots (input dry-run) command
            ;; Display output in buffer
            (when (and output (not (string-empty-p output)))
              (let ((buf (get-buffer-create "*beads-import*")))
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert output)
                    (goto-char (point-min))
                    (special-mode)
                    (local-set-key (kbd "q") 'quit-window)))
                (display-buffer buf)))
            ;; Show appropriate message
            (if dry-run
                (message "Dry run completed (see *beads-import* buffer)")
              (message "Import completed from: %s" input)))
          result))
    (error
     (beads--error "Failed to import: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-import--execute-command ()
  "Execute the bd import command."
  :key "x"
  :description "Import"
  (interactive)
  (let* ((args (transient-args 'beads-import))
         (command (beads-import--parse-transient-args args)))
    ;; Set default input if not provided
    (when (not (oref command input))
      (oset command input (beads-import--get-default-input)))
    ;; Validate
    (let ((errors (beads-import--validate-all command)))
      (if errors
          (user-error "Validation failed: %s" (string-join errors "; "))
        (beads-import--execute command)))))

(transient-define-suffix beads-import--preview ()
  "Preview the bd import command without executing."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-import))
         (command (beads-import--parse-transient-args args)))
    ;; Set default input if not provided
    (when (not (oref command input))
      (oset command input (beads-import--get-default-input)))
    (let ((cmd-line (beads-command-line command)))
      (message "Command: %s" (string-join cmd-line " ")))))

(transient-define-suffix beads-import--reset ()
  "Reset all import parameters."
  :key "R"
  :description "Reset all fields"
  :transient t
  (interactive)
  (when (y-or-n-p "Reset all fields? ")
    (transient-reset)
    (transient--redisplay)
    (message "Fields reset")))

;;; Main Transient Menu

;;;###autoload
(transient-define-prefix beads-import ()
  "Transient menu for importing issues from JSONL."
  :value (lambda () nil)
  ["Import Parameters"
   (beads-option-import-input)]
  ["Basic Options"
   (beads-option-import-dry-run)
   (beads-option-import-skip-existing)]
  ["Advanced Options"
   (beads-option-import-clear-duplicate-external-refs)
   (beads-option-import-dedupe-after)
   (beads-option-import-rename-on-import)
   (beads-option-import-strict)
   (beads-option-import-orphan-handling)]
  ["Actions"
   ("x" "Import" beads-import--execute-command)
   ("P" "Preview command" beads-import--preview)
   ("R" "Reset all fields" beads-import--reset)])

;;; Footer

(provide 'beads-import)
;;; beads-import.el ends here
