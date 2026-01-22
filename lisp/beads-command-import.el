;;; beads-command-import.el --- Import command class for beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module defines the `beads-command-import' EIEIO class for the
;; `bd import' command.  The class includes full slot metadata for
;; automatic transient menu generation via `beads-meta-define-transient'.
;;
;; The bd import command imports issues from JSON Lines format.
;; Existing issues are updated, new issues are created.
;;
;; Features:
;; - Import from stdin or file
;; - Collision detection and reporting
;; - Deduplication after import
;; - Dry-run preview mode
;; - Skip existing issues option
;; - Rename issues to match database prefix
;;
;; NOTE: Import requires direct database access and does not work with
;; daemon mode.  The command automatically uses --no-daemon.
;;
;; Usage:
;;   (beads-command-execute (beads-command-import :input "issues.jsonl"))
;;   (beads-command-import!)  ; convenience function

;;; Code:

(require 'beads-command)
(require 'beads-meta)
(require 'beads-option)
(require 'transient)

;; Forward declarations
(declare-function beads--sanitize-string "beads")
(declare-function beads--string-blank-p "beads")
(declare-function beads--find-beads-dir "beads")
(declare-function beads--error "beads")
(declare-function beads-buffer-name-utility "beads-buffer")
(defvar beads-executable)

;;; Import Command

;; Wrap in eval-and-compile so class is available at compile time for
;; beads-meta-define-transient macro
(eval-and-compile
  (beads-defcommand beads-command-import (beads-command-json)
    ((json
      :initarg :json
      :type boolean
      :initform nil
      :documentation "Output in JSON format (--json).
  Defaults to nil because import command does not produce JSON output.")
     (input
      :initarg :input
      :type (or null string)
      :initform nil
      :documentation "Input file (-i, --input).
  Default: stdin."
      ;; CLI properties
      :long-option "input"
      :short-option "i"
      :option-type :string
      ;; Transient properties
      :key "i"
      :transient "--input"
      :class transient-option
      :argument "--input="
      :prompt "Input file: "
      :reader transient-read-file
      :transient-group "Import Options"
      :level 1
      :order 1)
     (dry-run
      :initarg :dry-run
      :type boolean
      :initform nil
      :documentation "Preview collision detection without making changes
  (--dry-run)."
      ;; CLI properties
      :long-option "dry-run"
      :option-type :boolean
      ;; Transient properties
      :key "n"
      :transient "--dry-run"
      :class transient-switch
      :argument "--dry-run"
      :transient-group "Import Options"
      :level 1
      :order 2)
     (skip-existing
      :initarg :skip-existing
      :type boolean
      :initform nil
      :documentation "Skip existing issues instead of updating them
  (-s, --skip-existing)."
      ;; CLI properties
      :long-option "skip-existing"
      :short-option "s"
      :option-type :boolean
      ;; Transient properties
      :key "s"
      :transient "--skip-existing"
      :class transient-switch
      :argument "--skip-existing"
      :transient-group "Import Options"
      :level 1
      :order 3)
     (force
      :initarg :force
      :type boolean
      :initform nil
      :documentation "Force metadata update even when database is already
  in sync with JSONL (--force)."
      ;; CLI properties
      :long-option "force"
      :option-type :boolean
      ;; Transient properties
      :key "f"
      :transient "--force"
      :class transient-switch
      :argument "--force"
      :transient-group "Import Options"
      :level 1
      :order 4)
     (dedupe-after
      :initarg :dedupe-after
      :type boolean
      :initform nil
      :documentation "Detect and report content duplicates after import
  (--dedupe-after)."
      ;; CLI properties
      :long-option "dedupe-after"
      :option-type :boolean
      ;; Transient properties
      :key "d"
      :transient "--dedupe-after"
      :class transient-switch
      :argument "--dedupe-after"
      :transient-group "Import Options"
      :level 2
      :order 1)
     (strict
      :initarg :strict
      :type boolean
      :initform nil
      :documentation "Fail on dependency errors instead of treating them
  as warnings (--strict)."
      ;; CLI properties
      :long-option "strict"
      :option-type :boolean
      ;; Transient properties
      :key "S"
      :transient "--strict"
      :class transient-switch
      :argument "--strict"
      :transient-group "Import Options"
      :level 2
      :order 2)
     (rename-on-import
      :initarg :rename-on-import
      :type boolean
      :initform nil
      :documentation "Rename imported issues to match database prefix
  (--rename-on-import).  Updates all references."
      ;; CLI properties
      :long-option "rename-on-import"
      :option-type :boolean
      ;; Transient properties
      :key "R"
      :transient "--rename-on-import"
      :class transient-switch
      :argument "--rename-on-import"
      :transient-group "Import Options"
      :level 2
      :order 3)
     (orphan-handling
      :initarg :orphan-handling
      :type (or null string)
      :initform nil
      :documentation "How to handle missing parent issues (--orphan-handling).
  Values: strict, resurrect, skip, allow.
  Default: use config or 'allow'."
      ;; CLI properties
      :long-option "orphan-handling"
      :option-type :string
      ;; Transient properties
      :key "oh"
      :transient "--orphan-handling"
      :class transient-option
      :argument "--orphan-handling="
      :prompt "Orphan handling: "
      :transient-group "Advanced"
      :level 3
      :order 1)
     (clear-duplicate-external-refs
      :initarg :clear-duplicate-external-refs
      :type boolean
      :initform nil
      :documentation "Clear duplicate external_ref values
  (--clear-duplicate-external-refs).  Keeps first occurrence."
      ;; CLI properties
      :long-option "clear-duplicate-external-refs"
      :option-type :boolean
      ;; Transient properties
      :key "ce"
      :transient "--clear-duplicate-external-refs"
      :class transient-switch
      :argument "--clear-duplicate-external-refs"
      :transient-group "Advanced"
      :level 3
      :order 2)
     (no-git-history
      :initarg :no-git-history
      :type boolean
      :initform nil
      :documentation "Skip git history backfill for deletions
  (--no-git-history).  Passed by bd sync."
      ;; CLI properties
      :long-option "no-git-history"
      :option-type :boolean
      ;; Transient properties
      :key "ng"
      :transient "--no-git-history"
      :class transient-switch
      :argument "--no-git-history"
      :transient-group "Advanced"
      :level 3
      :order 3)
     (protect-left-snapshot
      :initarg :protect-left-snapshot
      :type boolean
      :initform nil
      :documentation "Protect issues in left snapshot from
  git-history-backfill (--protect-left-snapshot)."
      ;; CLI properties
      :long-option "protect-left-snapshot"
      :option-type :boolean
      ;; Transient properties
      :key "pl"
      :transient "--protect-left-snapshot"
      :class transient-switch
      :argument "--protect-left-snapshot"
      :transient-group "Advanced"
      :level 3
      :order 4))
    :documentation "Represents bd import command.
  Imports issues from JSON Lines format (one JSON object per line).
  When executed with :json t, returns import statistics."))

(cl-defmethod beads-command-subcommand ((_command beads-command-import))
  "Return \"import\" as the CLI subcommand name."
  "import")

(cl-defmethod beads-command-validate ((command beads-command-import))
  "Validate import COMMAND.
Returns error string or nil if valid."
  (with-slots (orphan-handling) command
    (cond
     ;; Validate orphan-handling if provided
     ((and orphan-handling
           (not (member orphan-handling
                        '("strict" "resurrect" "skip" "allow"))))
      (format "Invalid orphan-handling: %s (must be one of: strict, \
resurrect, skip, allow)"
              orphan-handling))
     ;; Otherwise valid
     (t nil))))

(cl-defmethod beads-command-execute :before ((command beads-command-import))
  "Set no-daemon flag for import COMMAND before execution.
Import always uses --no-daemon to avoid daemon issues."
  (oset command no-daemon t))

(cl-defmethod beads-command-parse ((command beads-command-import) execution)
  "Parse import COMMAND output from EXECUTION.
Unlike most commands, bd import writes stats to stderr, not stdout.
When :json is nil, returns raw stderr.
When :json is t, parses JSON from stderr.
Does not modify any slots."
  (with-slots (json) command
    (let ((stderr (oref execution stderr)))
      (if (not json)
          ;; No JSON parsing, return raw stderr
          stderr
        ;; Parse JSON from stderr (not stdout!)
        (condition-case err
            (let* ((json-object-type 'alist)
                   (json-array-type 'vector)
                   (json-key-type 'symbol))
              (json-read-from-string stderr))
          (error
           (signal 'beads-json-parse-error
                   (list (format "Failed to parse JSON from stderr: %s"
                                 (error-message-string err))
                         :exit-code (oref execution exit-code)
                         :stdout (oref execution stdout)
                         :stderr stderr
                         :parse-error err))))))))

(cl-defmethod beads-command-execute-interactive ((_cmd beads-command-import))
  "Execute CMD in terminal buffer with human-readable output."
  ;; Call the default implementation
  (cl-call-next-method))

;;; Transient Menu

;; Generate the complete transient menu from slot metadata
;;;###autoload (autoload 'beads-import-transient "beads-command-import" nil t)
(beads-meta-define-transient beads-command-import "beads-import-transient"
  "Import issues from JSON Lines format (auto-generated menu).

See `beads-import' for the full user-facing transient menu."
  beads-option-global-section)

;;; Interactive Import Workflow

(defun beads-import--get-default-input ()
  "Get default input path (.beads/issues.jsonl)."
  (when-let* ((beads-dir (beads--find-beads-dir)))
    (expand-file-name "issues.jsonl" beads-dir)))

(defun beads-import--parse-transient-args (args)
  "Parse transient ARGS list into a beads-command-import instance.
Returns a beads-command-import object populated with values from ARGS.

This uses transient's standard argument parsing with dash-style flags."
  ;; Note: transient 0.12.0 can return `t' instead of "" for empty option
  ;; values.  We use beads--sanitize-string to convert non-string values
  ;; to nil.
  (let* ((input (beads--sanitize-string
                 (transient-arg-value "--input=" args)))
         (dry-run (transient-arg-value "--dry-run" args))
         (skip-existing (transient-arg-value "--skip-existing" args))
         (clear-duplicate-external-refs
          (transient-arg-value "--clear-duplicate-external-refs" args))
         (dedupe-after (transient-arg-value "--dedupe-after" args))
         (rename-on-import (transient-arg-value "--rename-on-import" args))
         (strict (transient-arg-value "--strict" args))
         (orphan-handling (beads--sanitize-string
                           (transient-arg-value "--orphan-handling=" args))))
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
  (when (beads--string-blank-p input)
    "Input file path is required"))

(defun beads-import--validate-all (cmd)
  "Validate all parameters in CMD.
Returns list of error messages, or nil if all valid."
  (with-slots (input) cmd
    (delq nil (list (beads-import--validate-input input)
                    (beads-command-validate cmd)))))

;;; Suffix Commands

(defun beads-import--execute (cmd)
  "Execute bd import using CMD object.
Displays import output in *beads-import* buffer."
  (condition-case err
      (let* ((exec (beads-command-execute cmd))
             ;; Read from execution object
             (stdout (oref exec stdout))
             (stderr (oref exec stderr))
             ;; Combine stdout and stderr for display
             (output (concat (when (and stdout (not (string-empty-p stdout)))
                               (concat stdout "\n"))
                             stderr)))
        (with-slots (input dry-run) cmd
          ;; Display output in buffer
          (when (and output (not (string-empty-p output)))
            (let ((buf (get-buffer-create
                        (beads-buffer-name-utility "import"))))
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
        ;; Return the result slot value
        (oref exec result))
    (error
     (beads--error "Failed to import: %s"
                   (error-message-string err)))))

(transient-define-suffix beads-import--execute-command ()
  "Execute the bd import command."
  :key "x"
  :description "Import"
  (interactive)
  (let* ((args (transient-args 'beads-import))
         (cmd (beads-import--parse-transient-args args)))
    ;; Set default input if not provided
    (when (not (oref cmd input))
      (oset cmd input (beads-import--get-default-input)))
    ;; Validate
    (let ((errors (beads-import--validate-all cmd)))
      (if errors
          (user-error "Validation failed: %s" (string-join errors "; "))
        (beads-import--execute cmd)))))

(transient-define-suffix beads-import--preview ()
  "Preview the bd import command without executing."
  :key "P"
  :description "Preview command"
  :transient t
  (interactive)
  (let* ((args (transient-args 'beads-import))
         (cmd (beads-import--parse-transient-args args)))
    ;; Set default input if not provided
    (when (not (oref cmd input))
      (oset cmd input (beads-import--get-default-input)))
    (let ((cmd-line (beads-command-line cmd)))
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

;;;###autoload (autoload 'beads-import "beads-command-import" nil t)
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
   (beads-import--execute-command)
   (beads-import--preview)
   (beads-import--reset)])

(provide 'beads-command-import)
;;; beads-command-import.el ends here
