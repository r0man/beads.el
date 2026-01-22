;;; beads-error.el --- Error definitions for Beads -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is part of beads.el.

;;; Commentary:

;; This module defines all error conditions used throughout beads.el.
;; Centralizing error definitions makes it easier to:
;; - Maintain a consistent error hierarchy
;; - Document all possible errors in one place
;; - Avoid duplication across modules
;;
;; Error Hierarchy:
;;
;; error
;;   └─ beads-error (base error for all beads operations)
;;       ├─ beads-command-error (command execution failed)
;;       ├─ beads-json-parse-error (JSON parsing failed)
;;       └─ beads-validation-error (command validation failed)
;;
;; Usage:
;;
;;   ;; Signal a validation error
;;   (signal 'beads-validation-error
;;           (list "Title is required"))
;;
;;   ;; Catch beads-specific errors
;;   (condition-case err
;;       (beads-command-execute cmd)
;;     (beads-error
;;      (message "Beads operation failed: %s" (error-message-string err))))
;;
;;   ;; Catch specific error type
;;   (condition-case err
;;       (beads-command-execute cmd)
;;     (beads-validation-error
;;      (message "Validation failed: %s" (error-message-string err))))

;;; Code:

;;; Error Definitions

(define-error 'beads-error
  "Beads error"
  'error)

(define-error 'beads-command-error
  "Beads command execution error"
  'beads-error)

(define-error 'beads-json-parse-error
  "Beads JSON parse error"
  'beads-error)

(define-error 'beads-validation-error
  "Beads command validation error"
  'beads-error)

;;; Error Extraction Utilities

(defun beads-error-extract-message (err)
  "Extract user-friendly message from beads error ERR.
ERR is the error data from `condition-case'.

For `beads-command-error', this extracts the stderr message which
typically contains the actual error text from the bd CLI.
If stderr is empty, falls back to stdout, then to the generic message.

Returns a string suitable for display to the user."
  (let* ((err-data (cdr err))
         (message (car err-data))
         (plist (cdr err-data))
         (stderr (plist-get plist :stderr))
         (stdout (plist-get plist :stdout)))
    (cond
     ;; Prefer stderr if non-empty (typical for CLI errors)
     ((and stderr (not (string-empty-p (string-trim stderr))))
      (string-trim stderr))
     ;; Fall back to stdout if stderr is empty
     ((and stdout (not (string-empty-p (string-trim stdout))))
      (string-trim stdout))
     ;; Use the error message if nothing else
     (t (or message (error-message-string err))))))

(provide 'beads-error)
;;; beads-error.el ends here
