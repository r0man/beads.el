;;; beads-state.el --- Transient state variables for beads.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Keywords: tools, project, issues

;;; Commentary:

;; This module centralizes all transient state variables used across
;; beads.el command modules.  These are ephemeral runtime state
;; variables (not user configuration) that hold values during command
;; execution.
;;
;; By consolidating state variables in one place, we:
;;
;; - Eliminate circular dependencies between beads-option and
;;   beads-reader
;; - Provide a single source of truth for command state
;; - Make it easier to understand what state is used by which commands
;; - Separate runtime state from configuration (defvar vs defcustom)
;;
;; These variables are reset after command execution and should not
;; persist across Emacs sessions.

;;; Code:

;;; ============================================================
;;; Global state variables (shared across all commands)
;;; ============================================================

(defvar beads-global-actor nil
  "Global actor name override for audit trail.
When set, overrides $BD_ACTOR or $USER.")

(defvar beads-global-db nil
  "Global database path override.
When set, bypasses auto-discovery of .beads/*.db.")

(defvar beads-global-json nil
  "Global JSON output flag.
When non-nil, forces JSON output format.")

(defvar beads-global-dolt-auto-commit nil
  "Global dolt-auto-commit value.
Dolt auto-commit policy: off, on, or batch.")

(defvar beads-global-directory nil
  "Global working directory override.
When set, bd changes to this directory before running (like git -C).")

(defvar beads-global-global nil
  "Global shared-server database flag.
When non-nil, uses the global shared-server database (beads_global).")

(defvar beads-global-sandbox nil
  "Global sandbox flag.
When non-nil, enables sandbox mode: disables daemon and auto-sync.")

(defvar beads-global-profile nil
  "Global profile flag.
When non-nil, generates CPU profile for performance analysis.")

(defvar beads-global-quiet nil
  "Global quiet flag.
When non-nil, suppresses `non-essential' output (errors only).")

(defvar beads-global-readonly nil
  "Global readonly flag.
When non-nil, blocks write operations (for worker sandboxes).")

(defvar beads-global-verbose nil
  "Global verbose flag.
When non-nil, enables verbose/debug output.")

;;; ============================================================
;;; beads-update state variables
;;; ============================================================

(defvar beads-update--issue-id nil
  "Issue ID being updated.")

(defvar beads-update--original-data nil
  "Original issue data as fetched from bd.")

(defvar beads-update--status nil
  "New status for the issue.")

(defvar beads-update--priority nil
  "New priority for the issue.")

(defvar beads-update--type nil
  "New type for the issue.")

(defvar beads-update--title nil
  "New title for the issue.")


(defvar beads-update--assignee nil
  "New assignee for the issue.")

(defvar beads-update--external-ref nil
  "New external reference for the issue.")

;;; ============================================================
;;; beads-close state variables
;;; ============================================================

(defvar beads-close--issue-id nil
  "Issue ID to close.")


;;; ============================================================
;;; beads-reopen state variables
;;; ============================================================

(defvar beads-reopen--issue-id nil
  "Issue ID to reopen.")


;;; ============================================================
;;; beads-dep state variables
;;; ============================================================

(defvar beads-dep--from-issue nil
  "Source issue ID for dependency operations.")

(defvar beads-dep--to-issue nil
  "Target issue ID for dependency operations.")

(defvar beads-dep--dep-type nil
  "Dependency type (blocks, related, parent-child, discovered-from).")

;;; ============================================================
;;; beads-edit state variables
;;; ============================================================

(defvar beads-edit--issue-id nil
  "Issue ID to edit.")

(provide 'beads-state)
;;; beads-state.el ends here
