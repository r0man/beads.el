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

(defvar beads-global-no-auto-flush nil
  "Global no-auto-flush flag.
When non-nil, disables automatic JSONL sync after CRUD operations.")

(defvar beads-global-no-auto-import nil
  "Global no-auto-import flag.
When non-nil, disables automatic JSONL import when newer than DB.")

(defvar beads-global-no-daemon nil
  "Global no-daemon flag.
When non-nil, forces direct storage mode, bypassing daemon.")

(defvar beads-global-no-db nil
  "Global no-db flag.
When non-nil, uses no-db mode: load from JSONL only, no SQLite.")

(defvar beads-global-sandbox nil
  "Global sandbox flag.
When non-nil, enables sandbox mode: disables daemon and auto-sync.")

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

(defvar beads-update--description nil
  "New description for the issue.")

(defvar beads-update--acceptance-criteria nil
  "New acceptance criteria for the issue.")

(defvar beads-update--design nil
  "New design notes for the issue.")

(defvar beads-update--notes nil
  "New notes for the issue.")

(defvar beads-update--assignee nil
  "New assignee for the issue.")

(defvar beads-update--external-ref nil
  "New external reference for the issue.")

;;; ============================================================
;;; beads-close state variables
;;; ============================================================

(defvar beads-close--issue-id nil
  "Issue ID to close.")

(defvar beads-close--reason nil
  "Reason for closing the issue.")

;;; ============================================================
;;; beads-reopen state variables
;;; ============================================================

(defvar beads-reopen--issue-id nil
  "Issue ID to reopen.")

(defvar beads-reopen--reason nil
  "Reason for reopening the issue.")

;;; ============================================================
;;; beads-dep state variables
;;; ============================================================

(defvar beads-dep--from-issue nil
  "Source issue ID for dependency operations.")

(defvar beads-dep--to-issue nil
  "Target issue ID for dependency operations.")

(defvar beads-dep--dep-type nil
  "Dependency type (blocks, related, parent-child, discovered-from).")

(provide 'beads-state)
;;; beads-state.el ends here
